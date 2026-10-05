"""The client: `ghci-session start|stop|restart|status|reload|eval|check|mem|log|list|init`."""
import argparse
import json
import os
import signal
import socket
import subprocess
import sys
import time

from . import config
from .daemon import Session, sock_path

INIT_TEMPLATE = {
    "default": "lib",
    "targets": {
        "lib": {
            "cabal_args": "lib:yourpackage",
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
    name = name or conf["default"]
    if name not in conf["targets"]:
        raise SystemExit(f"unknown target {name!r}; have {', '.join(conf['targets'])}")
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
    subprocess.Popen([sys.executable, "-m", "ghci_session", "--root", conf["root"], "_daemon", name],
                     cwd=os.path.dirname(os.path.dirname(os.path.abspath(__file__))), stdout=log, stderr=log,
                     stdin=subprocess.DEVNULL, start_new_session=True)
    print(f"{name}: booting (log: {os.path.relpath(os.path.join(d, 'daemon.log'))})", file=sys.stderr)
    t0 = time.time()
    limit = conf["targets"][name]["load_timeout"] + 120
    while time.time() - t0 < limit:
        time.sleep(0.5)
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
        request(conf, name, {"op": "stop"}, timeout=30)
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
    names = [pick(conf, args.target)] if args.target else list(conf["targets"])
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
    rc = say(request(conf, name, {"op": "reload", "check": not args.no_check}))
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
        print(f"{name}{' (default)' if name == conf['default'] else ''}: {'running pid ' + str(pid) if pid else 'stopped'}"
              f"  watch={','.join(t['watch'])}  hygiene={'on' if t['hygiene'] else 'off'}")
    return 0


def cmd_init(args) -> int:
    path = os.path.join(os.getcwd(), config.CONFIG_NAME)
    if os.path.exists(path):
        print(f"{path} exists", file=sys.stderr)
        return 1
    with open(path, "w") as fh:
        json.dump(INIT_TEMPLATE, fh, indent=2)
        fh.write("\n")
    print(f"wrote {path}; edit cabal_args, then `ghci-session start`")
    return 0


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(prog="ghci-session", description="a warm GHCi per project")
    ap.add_argument("--root", help="project root (default: the nearest dir with ghci-session.json)")
    sub = ap.add_subparsers(dest="cmd", required=True)

    def add(name, fn, help_, target=True):
        p = sub.add_parser(name, help=help_)
        if target:
            p.add_argument("target", nargs="?", help="target name (default: the config's default)")
        p.set_defaults(fn=fn)
        return p

    add("start", cmd_start, "boot the target's repl in a background daemon")
    add("stop", cmd_stop, "stop the daemon and its repl")
    add("restart", cmd_simple("restart"), "boot a fresh repl")
    p = add("status", cmd_status, "the last verdict")
    p.add_argument("-d", "--detail", action="store_true")
    p = add("reload", cmd_reload, ":reload, prune, and the check")
    p.add_argument("--no-check", action="store_true", help="stop at the compile verdict")
    add("check", cmd_simple("check"), "run the target's check expression")
    add("mem", cmd_simple("mem"), "the repl process tree's memory")
    p = sub.add_parser("eval", help="evaluate an expression in the warm repl")
    p.add_argument("expr")
    p.add_argument("-t", "--target")
    p.add_argument("--timeout", type=float, default=0)
    p.set_defaults(fn=cmd_eval)
    p = add("log", cmd_log, "tail a state file (daemon.log, reload.log, run.log, async.log)")
    p.add_argument("which", nargs="?", default="daemon.log")
    p.add_argument("-n", type=int, default=40)
    # `ghci-session log [TARGET] [FILE]` is ambiguous with one optional positional pair; keep FILE second
    add("list", cmd_list, "the configured targets", target=False)
    sub.add_parser("init", help="write a ghci-session.json here").set_defaults(fn=None, cmd="init")
    p = sub.add_parser("_daemon")
    p.add_argument("target")

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
        Session(conf, args.target).run()
        return 0
    return args.fn(conf, args)
