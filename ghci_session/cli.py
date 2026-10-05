"""The client: `ghci-session start|stop|restart|status|reload|eval|check|server|compose|mem|log|list|init`."""
import argparse
import json
import os
import signal
import socket
import subprocess
import sys
import time

from . import config, gc
from .daemon import Session, sock_path

INIT_TEMPLATE = {
    "default": "lib",
    "targets": {
        "lib": {
            "units": ["lib:yourpackage"],
            "watch": ["src"],
            "modules": [],
            "check": None,
            "hygiene": False,
        }
    },
}


def state(conf: dict, name: str) -> str:
    return os.path.join(conf["state_dir"], name)


def pid_of(conf: dict, name: str) -> int | None:
    try:
        with open(os.path.join(state(conf, name), "pid")) as fh:
            pid = int(fh.read().strip())
        os.kill(pid, 0)
        return pid
    except (OSError, ValueError):
        return None


def request(conf: dict, name: str, req: dict, timeout: float = 3600) -> dict:
    sp = sock_path(state(conf, name))
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    s.settimeout(timeout)
    try:
        s.connect(sp)
    except OSError:
        raise SystemExit(f"{name}: no session running (ghci-session start {name})")
    s.sendall(json.dumps(req).encode() + b"\n")
    buf = b""
    while not buf.endswith(b"\n"):
        chunk = s.recv(1 << 20)
        if not chunk:
            break
        buf += chunk
    s.close()
    return json.loads(buf.decode())


def pick(conf: dict, name: str | None) -> str:
    """The session a command is for: the one named, else the only one running, else the config's default."""
    if not name:
        up = [s for s in config.session_names(conf) if pid_of(conf, s)]
        name = up[0] if len(up) == 1 else conf["default"]
    if name not in config.session_names(conf):
        raise SystemExit(f"unknown session {name!r}; have {', '.join(config.session_names(conf))}")
    return name


def say(r: dict) -> int:
    if r.get("stale"):
        print(f"warning: STALE -- {len(r['stale'])} watched file(s) differ from the loaded code "
              f"(e.g. {os.path.basename(r['stale'][0])}); `ghci-session reload`", file=sys.stderr)
    print(r.get("out", ""))
    return 0 if r.get("ok") else 1


def cmd_start(conf, args) -> int:
    name = pick(conf, args.target)
    if pid_of(conf, name):
        print(f"{name}: already running (pid {pid_of(conf, name)})")
        return 0
    d = state(conf, name)
    os.makedirs(d, exist_ok=True)
    for f in ("status", "status.json"):
        try:
            os.unlink(os.path.join(d, f))
        except OSError:
            pass
    log = open(os.path.join(d, "daemon.out"), "ab")
    subprocess.Popen([sys.executable, "-m", "ghci_session", "--root", conf["root"], "_daemon", name,
                      *(["--no-check"] if getattr(args, "no_check", False) else [])],
                     cwd=os.path.dirname(os.path.dirname(os.path.abspath(__file__))), stdout=log, stderr=log,
                     stdin=subprocess.DEVNULL, start_new_session=True)
    print(f"{name}: booting (log: {os.path.relpath(os.path.join(d, 'daemon.log'))})", file=sys.stderr)
    t0 = time.time()
    limit = config.resolve(conf, name)["load_timeout"] + 120
    while time.time() - t0 < limit:
        time.sleep(0.1)
        try:
            with open(os.path.join(d, "status")) as fh:
                first = fh.readline().strip()
        except OSError:
            first = ""
        if first and first.split(") ", 1)[-1] != "starting":
            print(first)
            return 0 if first.startswith(("OK", "STALE")) else 1
        if not pid_of(conf, name) and time.time() - t0 > 3:
            print(f"{name}: the daemon died while booting; see {os.path.join(d, 'daemon.out')}", file=sys.stderr)
            return 1
    print(f"{name}: still booting after {limit}s", file=sys.stderr)
    return 1


def cmd_stop(conf, args) -> int:
    name = pick(conf, args.target)
    pid = pid_of(conf, name)
    if not pid:
        print(f"{name}: not running")
        return 0
    try:
        request(conf, name, {"op": "stop", "keep_servers": bool(getattr(args, "keep_servers", False))}, timeout=30)
    except (SystemExit, OSError):
        pass
    for _ in range(100):
        if not pid_of(conf, name):
            print(f"{name}: stopped")
            return 0
        time.sleep(0.2)
    os.kill(pid, signal.SIGKILL)
    print(f"{name}: killed")
    return 0


def cmd_status(conf, args) -> int:
    names = [pick(conf, args.target)] if args.target else [s for s in config.session_names(conf) if pid_of(conf, s)]
    if not names:
        print("no session running")
    if not args.target:   # say why a session that was running is not: it was stopped for being idle
        for name in config.session_names(conf):
            if not pid_of(conf, name):
                try:
                    with open(os.path.join(state(conf, name), "status")) as fh:
                        first = fh.readline().strip()
                except OSError:
                    continue
                if first.startswith("stopped") and first != "stopped":
                    print(f"{name}: {first}")
    left = gc.find(conf)
    k = len(left["daemons"]) + len(left["servers"]) + len(left["builds"])
    if k:
        print(f"warning: {k} leftover process(es) of this project that no session tracks "
              f"({len(left['daemons'])} daemon, {len(left['servers'])} server, {len(left['builds'])} build); `ghci-session gc`", file=sys.stderr)
    for name in names:
        if not pid_of(conf, name):
            print(f"{name}: not running")
            continue
        r = request(conf, name, {"op": "status"}, timeout=30)
        print(f"{name}: {r['out']}")
        if args.detail:
            try:
                print(open(os.path.join(state(conf, name), "status")).read().rstrip())
            except OSError:
                pass
    return 0


def cmd_reload(conf, args) -> int:
    name = pick(conf, args.target)
    t0 = time.time()
    rc = say(request(conf, name, {"op": "reload", "check": not args.no_check, "refork": not args.no_refork,
                                    "async_refork": True if args.async_refork else None}))
    # the verdict is in the status file: print it, not GHC's whole load log
    try:
        with open(os.path.join(state(conf, name), "status")) as fh:
            print(fh.read().rstrip())
    except OSError:
        pass
    print(f"({time.time() - t0:.1f}s)", file=sys.stderr)
    return rc if rc else (0 if _ok(conf, name) else 1)


def _ok(conf, name) -> bool:
    try:
        return json.load(open(os.path.join(state(conf, name), "status.json")))["ok"]
    except (OSError, ValueError, KeyError):
        return False


def cmd_eval(conf, args) -> int:
    name = pick(conf, args.target)
    return say(request(conf, name, {"op": "eval", "expr": args.expr, "timeout": args.timeout}))


def cmd_simple(op):
    def run(conf, args) -> int:
        return say(request(conf, pick(conf, args.target), {"op": op}))
    return run


def cmd_check(conf, args) -> int:
    name = pick(conf, args.target)
    rc = say(request(conf, name, {"op": "check", "member": args.member}))
    return rc if rc else (0 if _ok(conf, name) else 1)


def cmd_server(conf, args) -> int:
    name = pick(conf, args.session)
    r = request(conf, name, {"op": "server", "action": args.action, "member": args.member, "resume": args.resume})
    print(r["out"])
    return 0 if r["ok"] and "FAILED" not in r["out"] else 1


def cmd_compose(conf, args) -> int:
    """Set a composed session's members. A repl's package set is fixed when it boots, so a change restarts
    that repl -- but the servers already running are kept and adopted by the new one."""
    name = args.session
    if name not in conf["sessions"]:
        raise SystemExit(f"{name!r} is not a composed session; declare it under \"sessions\" "
                         f"(have: {', '.join(conf['sessions']) or 'none'})")
    cur = config.read_members(conf, name)
    if args.add or args.remove:
        new = [m for m in cur if m not in (args.remove or [])] + [m for m in (args.add or []) if m not in cur]
    elif args.members:
        new = list(dict.fromkeys(args.members))
    else:
        print(f"{name} members: {', '.join(cur) or '(none)'}")
        return cmd_status(conf, argparse.Namespace(target=name, detail=False))
    unknown = [m for m in new if m not in conf["targets"]]
    if unknown:
        raise SystemExit(f"unknown target(s): {', '.join(unknown)}; have {', '.join(conf['targets'])}")
    if new == cur and pid_of(conf, name):
        print(f"{name}: unchanged ({', '.join(new) or 'none'})")
        return 0
    config.write_members(conf, name, new)
    if pid_of(conf, name):
        cmd_stop(conf, argparse.Namespace(target=name, keep_servers=True))
    print(f"{name} members: {', '.join(new) or '(none)'}")
    return cmd_start(conf, argparse.Namespace(target=name, no_check=getattr(args, "no_check", False)))


def autostop_plan(infos: list[dict], max_mem_mb: float, idle_mins: float, include_serving: bool) -> tuple[float, list[dict], list[tuple[dict, str]]]:
    """Which sessions to stop: (total MB, to stop, [(session, why not)]). The longest-idle go first, until the
    total is back under the limit; with no limit (0), every eligible one goes."""
    total = sum(i["repl_mb"] + i["servers_mb"] for i in infos)
    stop, spared = [], []
    left = total
    for i in sorted(infos, key=lambda i: -i["idle_s"]):
        why = ("busy" if i["busy"] else f"used {i['idle_s'] / 60:.0f} min ago" if i["idle_s"] < idle_mins * 60
               else f"serving {', '.join(i['serving'])}" if i["serving"] and not include_serving else None)
        if why:
            spared.append((i, why))
        elif max_mem_mb > 0 and left <= max_mem_mb:
            spared.append((i, "memory is back within the limit"))
        else:
            stop.append(i)
            left -= i["repl_mb"] + i["servers_mb"]
    return total, stop, spared


def cmd_autostop(conf, args) -> int:
    """Stop sessions nobody is using. A session is idle from its last client command or source change; one that
    is busy, or serving, is left alone (a server is in use by whoever is connected to it)."""
    infos = []
    for name in config.session_names(conf):
        if pid_of(conf, name):
            try:
                infos.append(json.loads(request(conf, name, {"op": "info"}, timeout=60)["out"]))
            except (SystemExit, OSError, ValueError, KeyError):
                pass
    total, stop, spared = autostop_plan(infos, args.max_mem_mb, args.idle_mins, args.include_serving)
    limit = f"limit {args.max_mem_mb:.0f} MB" if args.max_mem_mb > 0 else "no memory limit: every idle session goes"
    print(f"autostop: {len(infos)} session(s) using {total:.0f} MB ({limit}; idle after {args.idle_mins:g} min)")
    if args.max_mem_mb > 0 and total <= args.max_mem_mb:
        print("autostop: within the limit -- nothing to stop")
        return 0
    for i, why in spared:
        print(f"autostop: keeping {i['session']} ({i['repl_mb'] + i['servers_mb']:.0f} MB): {why}")
    for i in stop:
        mb = i["repl_mb"] + i["servers_mb"]
        print(f"autostop: {'would stop' if args.dry_run else 'stopping'} {i['session']} (idle {i['idle_s'] / 60:.0f} min, {mb:.0f} MB)")
        if not args.dry_run:
            try:
                request(conf, i["session"], {"op": "stop", "reason": f"stopped by autostop after {i['idle_s'] / 60:.0f} min idle; `ghci-session start {i['session']}`"}, timeout=30)
            except (SystemExit, OSError):
                pass
    after = total - sum(i["repl_mb"] + i["servers_mb"] for i in stop)
    if args.max_mem_mb > 0 and after > args.max_mem_mb:
        print(f"autostop: still {after:.0f} MB, over the limit -- nothing else is eligible")
    return 0


def cmd_log(conf, args) -> int:
    name = pick(conf, args.target)
    path = os.path.join(state(conf, name), args.which)
    try:
        sys.stdout.write("".join(open(path).readlines()[-args.n:]))
    except OSError:
        print(f"no {args.which} for {name}", file=sys.stderr)
        return 1
    return 0


def cmd_list(conf, args) -> int:
    for name, t in conf["targets"].items():
        pid = pid_of(conf, name)
        extra = (f"  checks={len(t['checks']) or (1 if t['check'] else 0)}" + ("  server" if t["server"] else ""))
        print(f"{name}{' (default)' if name == conf['default'] else ''}: {'running pid ' + str(pid) if pid else 'stopped'}"
              f"  units={' '.join(t['units']) or '-'}  watch={','.join(t['watch'])}  hygiene={'on' if t['hygiene'] else 'off'}{extra}")
    for name in conf["sessions"]:
        pid = pid_of(conf, name)
        print(f"{name} [composed]: {'running pid ' + str(pid) if pid else 'stopped'}  members={', '.join(config.read_members(conf, name)) or '(none)'}")
    return 0


def cmd_init(args) -> int:
    path = os.path.join(os.getcwd(), config.CONFIG_NAME)
    if os.path.exists(path):
        print(f"{path} exists", file=sys.stderr)
        return 1
    with open(path, "w") as fh:
        json.dump(INIT_TEMPLATE, fh, indent=2)
        fh.write("\n")
    print(f"wrote {path}; edit \"units\", then `ghci-session start`")
    return 0


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(prog="ghci-session", description="a warm GHCi per project")
    ap.add_argument("--root", help="project root (default: the nearest dir with ghci-session.json)")
    sub = ap.add_subparsers(dest="cmd", required=True)

    def add(name, fn, help_, target=True):
        p = sub.add_parser(name, help=help_)
        if target:
            p.add_argument("target", nargs="?", help="session name (default: the one running, else the config's default)")
        p.set_defaults(fn=fn)
        return p

    p = add("start", cmd_start, "boot the target's repl in a background daemon")
    p.add_argument("--no-check", action="store_true", help="up at the compile verdict; run the checks later with `check`")
    add("stop", cmd_stop, "stop the daemon and its repl")
    add("restart", cmd_simple("restart"), "boot a fresh repl")
    p = add("status", cmd_status, "the last verdict")
    p.add_argument("-d", "--detail", action="store_true")
    p = add("reload", cmd_reload, ":reload, prune, and the check")
    p.add_argument("--no-check", action="store_true", help="stop at the compile verdict")
    p.add_argument("--no-refork", action="store_true", help="leave running servers on the old code")
    p.add_argument("--async-refork", action="store_true", help="return at the verdict; re-fork the servers in the background")
    p = add("check", cmd_check, "run the session's checks")
    p.add_argument("-m", "--member", help="only this member's")
    p = sub.add_parser("server", help="a session's forked servers: status | start | stop | restart")
    p.add_argument("action", nargs="?", default="status", choices=["status", "start", "stop", "restart"])
    p.add_argument("-m", "--member", help="only this member's server")
    p.add_argument("-s", "--session")
    p.add_argument("--resume", action="store_true", help="start from the state the last child left")
    p.set_defaults(fn=cmd_server)
    p = sub.add_parser("compose", help="show or set a composed session's members")
    p.add_argument("session")
    p.add_argument("members", nargs="*")
    p.add_argument("--add", action="append")
    p.add_argument("--remove", action="append")
    p.add_argument("--no-check", action="store_true", help="restart at the compile verdict")
    p.set_defaults(fn=cmd_compose)
    add("mem", cmd_simple("mem"), "the repl process tree's memory")
    p = sub.add_parser("eval", help="evaluate an expression in the warm repl")
    p.add_argument("expr")
    p.add_argument("-s", "-t", "--session", dest="target")
    p.add_argument("--timeout", type=float, default=0)
    p.set_defaults(fn=cmd_eval)
    p = sub.add_parser("log", help="tail a state file (daemon.log, reload.log, run.log, async.log, server-NAME.log)")
    p.add_argument("which", nargs="?", default="daemon.log")
    p.add_argument("-s", "--session", dest="target")
    p.add_argument("-n", type=int, default=40)
    p.set_defaults(fn=cmd_log)
    add("list", cmd_list, "the configured targets", target=False)
    p = sub.add_parser("autostop", help="stop idle sessions (all of them, or until memory is under --max-mem-mb)")
    p.add_argument("--max-mem-mb", type=float, default=0, help="only stop while the sessions' total is over this (0: no limit)")
    p.add_argument("--idle-mins", type=float, default=30, help="unused this long counts as idle (default 30)")
    p.add_argument("--include-serving", action="store_true", help="also stop sessions with a running server")
    p.add_argument("-n", "--dry-run", action="store_true")
    p.set_defaults(fn=cmd_autostop)
    p = sub.add_parser("gc", help="reap orphaned daemons, servers and build processes of THIS project; prune old state")
    p.add_argument("-n", "--dry-run", action="store_true")
    p.add_argument("--days", type=float, default=0, help="also prune state dirs of sessions idle longer than this")
    p.set_defaults(fn=lambda conf, args: 0 if gc.run(conf, args.dry_run, args.days) >= 0 else 1)
    sub.add_parser("init", help="write a ghci-session.json here").set_defaults(fn=None, cmd="init")
    p = sub.add_parser("_daemon")
    p.add_argument("target")
    p.add_argument("--no-check", action="store_true")

    args = ap.parse_args(argv)
    if args.cmd == "init":
        return cmd_init(args)
    try:
        root = args.root or config.find_root()
        conf = config.load(root)
    except config.ConfigError as e:
        print(f"ghci-session: {e}", file=sys.stderr)
        return 2
    if args.cmd == "_daemon":
        Session(conf, args.target, boot_check=not args.no_check).run()
        return 0
    return args.fn(conf, args)
