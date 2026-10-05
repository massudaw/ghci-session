"""Finding and reaping what a session left behind.

Three kinds of leftover, each invisible to `status` and unkillable by `stop` because nothing records it:

  * a DAEMON whose pid file is gone or names another process (a second start that raced, a pruned state dir);
  * a SERVER still running after its session's daemon died (a detached child, or a daemon killed -9);
  * a BUILD process -- the `ghc --interactive` a `cabal repl` exec'd -- reparented to init after its daemon
    went. It holds a lock on dist-newstyle, so every later cabal command in the project queues behind it.

Attribution is by ABSOLUTE PATH, never by name: several checkouts of one project are routinely live at once,
and a needle like "ghc" or "cabal repl" would match a sibling checkout's healthy session. A daemon carries
`--root <abs>` on its command line; a build process carries this project's dist-newstyle or state dir.
"""
import os
import re
import shutil
import signal
import subprocess
import time

from . import config
from .daemon import pid_alive


def list_processes() -> list[tuple[int, int, str]]:
    out = subprocess.run(["ps", "-axo", "pid=,ppid=,command="], capture_output=True, text=True).stdout
    procs = []
    for line in out.splitlines():
        parts = line.strip().split(None, 2)
        if len(parts) == 3 and parts[0].isdigit() and parts[1].isdigit():
            procs.append((int(parts[0]), int(parts[1]), parts[2]))
    return procs


def descendants(procs, pid: int) -> list[int]:
    """Every process under `pid`, transitively: a session is daemon -> cabal -> ghc, and killing the daemon
    and its direct children leaves the GHC grandchild holding the build lock."""
    kids: dict[int, list[int]] = {}
    for c, pp, _ in procs:
        kids.setdefault(pp, []).append(c)
    out, stack = [], list(kids.get(pid, []))
    while stack:
        c = stack.pop()
        if c in out or c == pid:
            continue
        out.append(c)
        stack.extend(kids.get(c, []))
    return out


def kill_tree(pid: int, children: list[int]) -> None:
    for sig in (signal.SIGTERM, signal.SIGKILL):
        for p in [pid, *children]:
            try:
                os.kill(p, sig)
            except OSError:
                pass
        deadline = time.time() + (3.0 if sig is signal.SIGTERM else 1.0)
        while time.time() < deadline and (pid_alive(pid) or any(pid_alive(c) for c in children)):
            time.sleep(0.1)
        if not pid_alive(pid) and not any(pid_alive(c) for c in children):
            return


def _read_int(path: str) -> int | None:
    try:
        with open(path) as fh:
            return int(fh.read().strip())
    except (OSError, ValueError):
        return None


def session_dirs(conf: dict) -> list[str]:
    """State dirs that are sessions (not clib, not a members file)."""
    sd = conf["state_dir"]
    if not os.path.isdir(sd):
        return []
    return [e for e in sorted(os.listdir(sd))
            if os.path.isdir(os.path.join(sd, e)) and any(os.path.exists(os.path.join(sd, e, f)) for f in ("status", "pid", "daemon.log"))]


def daemon_pid(conf: dict, name: str) -> int | None:
    pid = _read_int(os.path.join(conf["state_dir"], name, "pid"))
    return pid if pid and pid_alive(pid) else None


def find(conf: dict, procs=None) -> dict:
    """What is left over in this project: {"daemons": [(session, pid)], "servers": [(session, member, pid)],
    "builds": [(pid, cmd)]}."""
    procs = procs if procs is not None else list_processes()
    root = os.path.realpath(conf["root"])
    names = sorted(set(config.session_names(conf)) | set(session_dirs(conf)))
    live = {n: daemon_pid(conf, n) for n in names}
    daemon_re = re.compile(r"ghci_session\s+--root\s+(\S+)\s+_daemon\s+(\S+)")

    daemons = []
    for pid, _pp, cmd in procs:
        m = daemon_re.search(cmd)
        if m and os.path.realpath(m.group(1)) == root and live.get(m.group(2)) != pid:
            daemons.append((m.group(2), pid))

    servers, tracked = [], set()
    for n in session_dirs(conf):
        d = os.path.join(conf["state_dir"], n)
        for f in sorted(os.listdir(d)):
            if f.startswith("server-") and f.endswith(".pid"):
                pid = _read_int(os.path.join(d, f))
                if pid and pid_alive(pid):
                    tracked.add(pid)
                    if not live.get(n):
                        servers.append((n, f[len("server-"):-len(".pid")], pid))

    # everything under a live daemon, or under an orphan we already list, is accounted for
    owned = set(tracked)
    for pid in [p for p in live.values() if p] + [p for _, p in daemons]:
        owned.add(pid)
        owned.update(descendants(procs, pid))
    needles = (os.path.join(root, "dist-newstyle"), os.path.realpath(conf["state_dir"]))
    builds = [(pid, cmd) for pid, pp, cmd in procs
              if pp == 1 and pid not in owned and any(n in cmd for n in needles)
              and re.search(r"\bghc\b|ghc-\d|\bcabal\b|ghc-iserv", cmd)]
    return {"daemons": daemons, "servers": servers, "builds": builds}


def describe_build(cmd: str) -> str:
    units = re.findall(r"multi-out-\d+/([A-Za-z0-9.-]+?)-\d", cmd)
    return ", ".join(dict.fromkeys(units)) or cmd.split()[0].rsplit("/", 1)[-1]


def du_mb(path: str) -> float:
    total = 0
    for r, _d, files in os.walk(path):
        for f in files:
            try:
                total += os.path.getsize(os.path.join(r, f))
            except OSError:
                pass
    return total / 1e6


def run(conf: dict, dry_run: bool = False, days: float = 0.0, out=print) -> int:
    """Reap the leftovers; with `days` > 0 also prune state dirs of sessions idle longer than that. Returns
    how many things were (or would be) reaped."""
    procs = list_processes()
    found = find(conf, procs)
    verb = "would reap" if dry_run else "reaping"
    n = 0
    for name, pid in found["daemons"]:
        out(f"gc: {verb} orphaned daemon pid {pid} (session {name}: not the one its state dir records)")
        if not dry_run:
            kill_tree(pid, descendants(procs, pid))
        n += 1
    for name, member, pid in found["servers"]:
        out(f"gc: {verb} server {member} pid {pid}: its session {name} is not running")
        if not dry_run:
            kill_tree(pid, descendants(procs, pid))
            try:
                os.unlink(os.path.join(conf["state_dir"], name, f"server-{member}.pid"))
            except OSError:
                pass
        n += 1
    if not dry_run:
        procs = list_processes()
        found = find(conf, procs)
    for pid, cmd in found["builds"]:
        out(f"gc: {verb} orphaned build process {pid} ({describe_build(cmd)}) -- it holds a lock on dist-newstyle")
        if not dry_run:
            kill_tree(pid, descendants(procs, pid))
        n += 1
    if days > 0:
        cutoff = time.time() - days * 86400
        for name in session_dirs(conf):
            d = os.path.join(conf["state_dir"], name)
            if daemon_pid(conf, name):
                continue
            try:
                mtime = os.path.getmtime(os.path.join(d, "status"))
            except OSError:
                mtime = os.path.getmtime(d)
            if mtime >= cutoff:
                continue
            age, size = (time.time() - mtime) / 86400, du_mb(d)
            if dry_run:
                out(f"gc: would prune {name} (idle {age:.1f}d, {size:.0f} MB)")
            else:
                shutil.rmtree(d, ignore_errors=True)
                out(f"gc: pruned {name} (idle {age:.1f}d, freed {size:.0f} MB)")
    if not n:
        out("gc: no orphaned daemons, servers or build processes")
    return n
