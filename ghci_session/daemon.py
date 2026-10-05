"""The per-target daemon: owns one repl, serves reload/eval/check/status on a unix socket, watches the sources."""
import hashlib
import json
import os
import re
import socket
import subprocess
import sys
import threading
import time

from .repl import Repl, ReplDied, ReplTimeout

PKG_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# GHC's own verdict lines, used to decide whether a load succeeded.
RE_OK = re.compile(r"^Ok, (?:\d+|one|two|three|no) modules? (?:loaded|reloaded)\.", re.M)
RE_FAILED = re.compile(r"^Failed, ", re.M)
# Not anchored on a source location: GHC also emits `<no location info>: error:` for link/IO failures.
RE_ERROR = re.compile(r"^.*?: error:", re.M)
RE_NOMODULE = re.compile(r"Could not find module|not in scope|Not in scope|Variable not in scope", re.I)


def write_atomic(path: str, text: str) -> None:
    tmp = path + ".tmp"
    with open(tmp, "w") as fh:
        fh.write(text)
    os.replace(tmp, path)


def sock_path(state_dir: str) -> str:
    """A short, collision-free unix socket path (macOS caps sun_path near 104 bytes, which a nested worktree
    easily exceeds), unique per absolute state dir."""
    base = f"/tmp/ghci-session-{os.getuid()}"
    os.makedirs(base, exist_ok=True, mode=0o700)
    return os.path.join(base, hashlib.sha1(os.path.abspath(state_dir).encode()).hexdigest()[:16] + ".sock")


def scan(root: str, dirs: list[str], exts: tuple) -> dict[str, float]:
    """mtimes of every watched source. Polling beats a watcher dependency: ~600 stats is under 10 ms."""
    seen: dict[str, float] = {}
    for d in dirs:
        top = os.path.join(root, d)
        if os.path.isfile(top):
            try:
                seen[top] = os.stat(top).st_mtime
            except OSError:
                pass
            continue
        for dirpath, dirnames, filenames in os.walk(top):
            dirnames[:] = [x for x in dirnames if not x.startswith(".") and x != "dist-newstyle"]
            for fn in filenames:
                if fn.endswith(exts):
                    p = os.path.join(dirpath, fn)
                    try:
                        seen[p] = os.stat(p).st_mtime
                    except OSError:
                        pass
    return seen


def process_table() -> list[tuple[int, int, int]]:
    """(pid, ppid, rss_kb) of every process."""
    try:
        out = subprocess.run(["ps", "-axo", "pid=,ppid=,rss="], capture_output=True, text=True, timeout=10).stdout
    except (OSError, subprocess.TimeoutExpired):
        return []
    rows = []
    for line in out.splitlines():
        p = line.split()
        if len(p) == 3 and all(x.isdigit() for x in p):
            rows.append((int(p[0]), int(p[1]), int(p[2])))
    return rows


def tree_rss_mb(pid: int) -> float:
    """RSS of a process and its descendants. Under memory pressure macOS compresses a process's pages and RSS
    collapses, so on macOS the physical footprint is preferred when `footprint` is available."""
    rows = process_table()
    kids = {pid}
    grew = True
    while grew:
        grew = False
        for p, pp, _ in rows:
            if pp in kids and p not in kids:
                kids.add(p)
                grew = True
    if sys.platform == "darwin":
        total = 0.0
        ok = True
        for p in sorted(kids):
            try:
                out = subprocess.run(["footprint", "-p", str(p), "--noCategories"], capture_output=True, text=True, timeout=10).stdout
                m = re.search(r"Footprint:\s*([\d.]+)\s*(KB|MB|GB)", out)
                if m:
                    total += float(m.group(1)) * {"KB": 1 / 1024, "MB": 1, "GB": 1024}[m.group(2)]
            except (OSError, subprocess.TimeoutExpired):
                ok = False
                break
        if ok and total > 0:
            return total
    return sum(r for p, _, r in rows if p in kids) / 1024


class Session:
    def __init__(self, conf: dict, name: str):
        self.conf = conf
        self.name = name
        self.cfg = conf["targets"][name]
        self.root = conf["root"]
        self.dir = os.path.join(conf["state_dir"], name)
        os.makedirs(self.dir, exist_ok=True)
        self.exts = tuple(self.cfg["watch_ext"])
        # build files at the root are watched too: a changed .cabal means a new package set, which a reload cannot adopt
        rootfiles = sorted(f for f in os.listdir(self.root) if f.endswith(".cabal") or f.startswith("cabal.project"))
        self.cfg = dict(self.cfg, watch=list(self.cfg["watch"]) + [f for f in rootfiles if f not in self.cfg["watch"]])
        self.repl: Repl | None = None
        self.last_status = "starting"
        self.last_json: dict = {}
        self.loaded_sig: dict[str, float] = {}
        self.pending_sig: dict[str, float] = {}
        self.stopping = threading.Event()
        self._work = threading.Lock()  # the watcher and a client both drive the repl; never at once
        self.hygiene_on = bool(self.cfg["hygiene"])

    # -- logging and status --

    def log(self, msg: str) -> None:
        line = f"[{time.strftime('%H:%M:%S')}] {msg}\n"
        with open(os.path.join(self.dir, "daemon.log"), "a") as fh:
            fh.write(line)

    def async_out(self, text: str) -> None:
        with open(os.path.join(self.dir, "async.log"), "a") as fh:
            fh.write(text)

    def stale_files(self) -> list[str]:
        """Watched sources that differ from what the loaded code was built from."""
        now = scan(self.root, self.cfg["watch"], self.exts)
        return sorted(p for p in set(now) | set(self.loaded_sig) if now.get(p) != self.loaded_sig.get(p))

    def set_status(self, text: str, detail: list[str] | None = None, **facts) -> None:
        self.last_status = text
        stale = [] if text == "starting" else self.stale_files()
        head = f"STALE({len(stale)}) {text}" if stale else text
        stamp = lambda t: time.strftime("%H:%M:%S", time.localtime(t)) if t else "-"  # noqa: E731
        lines = [head, f"target={self.name} loaded={stamp(self.loaded_at)} checked={stamp(self.checked_at)}"]
        if stale:
            lines.append("stale: " + ", ".join(os.path.relpath(p, self.root) for p in stale[:6]))
        lines += detail or []
        write_atomic(os.path.join(self.dir, "status"), "\n".join(lines) + "\n")
        kind = text.split(":")[0].split(" ")[0].strip("-") or "?"
        j = {"target": self.name, "kind": kind, "ok": text.startswith("OK"), "stale": stale, "verdict": text,
             "failing": detail or [], "at": time.time(), **facts}
        self.last_json = j
        write_atomic(os.path.join(self.dir, "status.json"), json.dumps(j, indent=1))

    loaded_at: float = 0.0
    checked_at: float = 0.0

    # -- the repl --

    def repl_command(self) -> str:
        cfg = self.cfg
        if cfg["repl"]:
            return cfg["repl"]
        cmd = "cabal repl"
        if cfg["rts_flags"] not in ("", "none"):
            cmd += f" --with-repl={os.path.join(PKG_DIR, 'bin', 'ghci-rts.sh')}"
        cmd += " --repl-options=-fdiagnostics-color=never"
        if cfg["hygiene"]:
            cmd += " --repl-options=-fobject-code"   # CAFs of interpreted code are not prunable by address
        return f"{cmd} {cfg['cabal_args']}".strip()

    def build_hygiene(self) -> None:
        script = os.path.join(PKG_DIR, "hygiene", "build.sh")
        clib = os.path.join(self.conf["state_dir"], "clib")
        r = subprocess.run([script, clib], capture_output=True, text=True, cwd=self.root)
        self.log("hygiene build: " + (r.stderr.strip() or r.stdout.strip() or "ok").replace("\n", "; "))

    def post_load(self, repl: Repl) -> None:
        repl.post_load_basics()
        for expr in self.cfg["preload"]:
            repl.command(expr, timeout=120)
        for m in self.cfg["modules"]:
            repl.command(f":module + {m}", timeout=60)
        if self.hygiene_on:
            out = repl.command(":module + GHC.Hygiene GHC.Stats", timeout=60)
            if out.strip() and RE_NOMODULE.search(out):
                self.log("hygiene disabled: the ghci-hygiene package is not in scope in this repl "
                         "(add it to build-depends); no pruning, no memory report from the RTS")
                self.hygiene_on = False

    def verdict_of(self, out: str) -> tuple[str, list[str]]:
        errs = [l for l in out.splitlines() if RE_ERROR.match(l)]
        if RE_FAILED.search(out) or errs:
            return f"COMPILE-ERROR: {len(errs)} error(s)", errs[:20]
        if RE_OK.search(out):
            return "OK", []
        return "COMPILE-ERROR: no GHC verdict in the load output", out.strip().splitlines()[-5:]

    def after_load(self, out: str, do_check: bool = True, t0: float | None = None) -> None:
        self.loaded_sig = self.pending_sig or scan(self.root, self.cfg["watch"], self.exts)
        self.loaded_at = time.time()
        write_atomic(os.path.join(self.dir, "load.log"), out)
        v, detail = self.verdict_of(out)
        if v != "OK":
            self.set_status(v, detail, duration_s=round(time.time() - (t0 or self.loaded_at), 2))
            return
        if not do_check or not self.cfg["check"]:
            why = "--no-check" if not do_check else "no check configured"
            self.checked_at = self.loaded_at
            self.set_status(f"OK -- CHECK SKIPPED ({why}): this is a COMPILE verdict only",
                            duration_s=round(time.time() - (t0 or self.loaded_at), 2))
            return
        self.run_check(t0=t0)

    def run_check(self, t0: float | None = None) -> str:
        chk = self.cfg["check"]
        if not chk:
            return "no check configured"
        t = time.time()
        try:
            out = self.repl.command(chk["expr"], timeout=chk.get("timeout") or None)
        except (ReplDied, ReplTimeout) as e:
            self.set_status(f"DEAD: {e}")
            return str(e)
        write_atomic(os.path.join(self.dir, "run.log"), out)
        self.checked_at = time.time()
        fail_re = chk.get("fail")
        pass_re = chk.get("pass")
        failing = [l for l in out.splitlines() if fail_re and re.search(fail_re, l)]
        took = round(time.time() - (t0 or t), 2)
        if failing:
            self.set_status(f"CHECK-FAIL: {len(failing)} failing", failing[:20], duration_s=took)
        elif pass_re and not re.search(pass_re, out):
            self.set_status("CHECK-FAIL: the pass marker never appeared (did the check run?)", out.strip().splitlines()[-5:], duration_s=took)
        else:
            self.set_status(f"OK -- CHECK-PASS ({took:.1f}s)", duration_s=took)
        return out

    # -- memory --

    def repl_mb(self) -> float:
        return tree_rss_mb(self.repl.pid) if self.repl and self.repl.pid else 0.0

    def prune_cafs(self) -> None:
        """Unlink the CAFs this reload superseded, then log the session's memory. Best effort: never fails a reload."""
        if not self.hygiene_on:
            return
        try:
            before = self.repl_mb()
            t0 = time.time()
            out = self.repl.command(
                'GHC.Hygiene.pruneCafs >>= \\k -> GHC.Stats.getRTSStatsEnabled >>= \\e -> '
                '(if e then GHC.Stats.getRTSStats >>= \\s -> return (show (GHC.Stats.gcdetails_live_bytes (GHC.Stats.gc s) `div` 1000000)) else return "?") >>= \\l -> '
                'putStrLn ("prune_cafs=" ++ show k ++ " live_mb=" ++ l)', timeout=300)
            k = next((l.split("=", 1)[1].split()[0] for l in out.splitlines() if l.startswith("prune_cafs=")), "?")
            live = next((l.split("live_mb=", 1)[1].strip() for l in out.splitlines() if "live_mb=" in l), "?")
            line = f"[mem] prune_cafs: {k} unlinked in {time.time() - t0:.1f}s; repl {before:.0f} -> {self.repl_mb():.0f} MB, live heap {live} MB"
            self.log(line)
            with open(os.path.join(self.dir, "reload.log"), "a") as fh:
                fh.write("\n" + line + "\n")
        except Exception as e:  # noqa: BLE001
            self.log(f"prune_cafs failed: {e}")

    # -- lifecycle --

    def boot(self) -> None:
        if self.cfg["hygiene"]:
            self.build_hygiene()
        env = dict(self.cfg["env"])
        if self.cfg["rts_flags"] not in ("", "none"):
            env.setdefault("GHS_RTS_FLAGS", self.cfg["rts_flags"])
        env.setdefault("GHS_DIR", os.path.join(self.conf["state_dir"], "clib"))
        env["GHCI_SESSION"] = self.name
        self.repl = Repl(self.repl_command(), self.root, env, self.cfg["load_timeout"], self.cfg["eval_timeout"],
                         log=self.log, on_async=self.async_out)
        self.set_status("starting")
        t0 = time.time()
        self.pending_sig = scan(self.root, self.cfg["watch"], self.exts)
        try:
            out = self.repl.start(self.post_load)
        except (ReplDied, ReplTimeout) as e:
            self.set_status(f"DEAD: {e}", str(e).splitlines()[-8:])
            raise
        self.after_load(out, t0=t0)

    def restart(self) -> str:
        self.log("restart")
        self.repl.stop()
        self.hygiene_on = bool(self.cfg["hygiene"])
        self.boot()
        return self.last_status

    def reload(self, do_check: bool = True) -> str:
        budget = float(os.environ.get("GHS_REPL_BUDGET_MB", self.cfg["repl_budget_mb"]))
        rss = self.repl_mb()
        if budget > 0 and rss > budget:
            self.log(f"reload: repl at {rss:.0f} MB > budget {budget:.0f} MB -- restarting instead")
            out = self.restart()
            self.set_status(self.last_status + f"  [repl RESTARTED instead of reloaded: it had grown to {rss:.0f} MB, "
                            f"over the {budget:.0f} MB budget; now {self.repl_mb():.0f} MB]")
            return out
        t0 = time.time()
        self.pending_sig = scan(self.root, self.cfg["watch"], self.exts)
        try:
            out = self.repl.command(":reload", timeout=self.cfg["load_timeout"])
        except (ReplDied, ReplTimeout) as e:
            self.set_status(f"DEAD: {e}")
            return str(e)
        write_atomic(os.path.join(self.dir, "reload.log"), out)
        try:
            self.post_load(self.repl)
        except Exception as e:  # noqa: BLE001
            self.log(f"post_load after reload failed: {e}")
        self.prune_cafs()
        self.after_load(out, do_check=do_check, t0=t0)
        return out

    # -- the watcher --

    def watch_loop(self) -> None:
        last = scan(self.root, self.cfg["watch"], self.exts)
        while not self.stopping.wait(0.5):
            now = scan(self.root, self.cfg["watch"], self.exts)
            if now == last:
                continue
            # debounce: an editor's save is several writes
            time.sleep(self.cfg["debounce"])
            now = scan(self.root, self.cfg["watch"], self.exts)
            last = now
            if not self.cfg["auto_reload"] or now == self.loaded_sig:
                continue
            with self._work:
                if self.stopping.is_set():
                    return
                changed = [p for p in now if now.get(p) != self.loaded_sig.get(p)]
                if any(p.endswith((".c", ".h", ".cabal")) for p in changed):
                    self.log("a .c/.h/.cabal changed: restarting the repl (a loaded C object cannot be replaced)")
                    try:
                        self.restart()
                    except Exception as e:  # noqa: BLE001
                        self.log(f"restart failed: {e}")
                else:
                    self.log(f"watch: {len(changed)} file(s) changed -- reload")
                    self.reload()

    # -- the socket --

    def serve(self) -> None:
        sp = sock_path(self.dir)
        if os.path.exists(sp):
            os.unlink(sp)
        link = os.path.join(self.dir, "sock")
        try:
            if os.path.islink(link) or os.path.exists(link):
                os.unlink(link)
            os.symlink(sp, link)
        except OSError:
            pass
        srv = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        srv.bind(sp)
        srv.listen(8)
        srv.settimeout(0.5)
        while not self.stopping.is_set():
            try:
                conn, _ = srv.accept()
            except socket.timeout:
                continue
            threading.Thread(target=self.handle, args=(conn,), daemon=True).start()
        srv.close()
        for p in (sp, link):
            try:
                os.unlink(p)
            except OSError:
                pass

    def handle(self, conn: socket.socket) -> None:
        def reply(ok: bool, out: str) -> None:
            conn.sendall(json.dumps({"ok": ok, "out": out, "stale": self.stale_files()[:6],
                                     "status": self.last_json}).encode() + b"\n")

        with conn:
            buf = b""
            while not buf.endswith(b"\n"):
                chunk = conn.recv(65536)
                if not chunk:
                    return
                buf += chunk
            try:
                req = json.loads(buf.decode())
                op = req.get("op")
                if op == "status":      # reads only the last verdict: must answer while a reload holds the repl
                    stale = self.stale_files()
                    reply(True, (f"STALE({len(stale)}) " if stale else "") + self.last_status)
                elif op == "stop":
                    self.stopping.set()
                    reply(True, "stopping")
                else:
                    with self._work:   # eval is inside the lock too: its answer must not straddle a reload
                        if op == "eval":
                            out = self.repl.command(req["expr"], timeout=req.get("timeout") or None)
                        elif op == "reload":
                            out = self.reload(do_check=req.get("check", True))
                        elif op == "check":
                            out = self.run_check()
                        elif op == "restart":
                            out = self.restart()
                        elif op == "mem":
                            out = f"repl tree {self.repl_mb():.0f} MB (budget {self.cfg['repl_budget_mb']})"
                        else:
                            reply(False, f"unknown op {op!r}")
                            return
                    reply(True, out)
            except Exception as e:  # a broken eval must not kill the daemon
                try:
                    reply(False, f"{type(e).__name__}: {e}")
                except OSError:
                    pass

    def run(self) -> None:
        write_atomic(os.path.join(self.dir, "pid"), str(os.getpid()))
        try:
            self.boot()
        except Exception as e:  # noqa: BLE001
            self.log(f"boot failed: {e}")
            if self.repl:
                self.repl.stop()
            try:
                os.unlink(os.path.join(self.dir, "pid"))
            except OSError:
                pass
            raise
        threading.Thread(target=self.watch_loop, daemon=True).start()
        try:
            self.serve()
        finally:
            self.repl.stop()
            self.set_status("stopped")
            try:
                os.unlink(os.path.join(self.dir, "pid"))
            except OSError:
                pass
