"""The per-target daemon: owns one repl, serves reload/eval/check/status on a unix socket, watches the sources."""
import glob
import hashlib
import json
import os
import re
import signal
import socket
import subprocess
import sys
import threading
import time

from . import config
from .repl import Repl, ReplDied, ReplTimeout, strip_ansi

PKG_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# GHC's own verdict lines, used to decide whether a load succeeded.
RE_OK = re.compile(r"^Ok, (?:\d+|one|two|three|no) modules? (?:loaded|reloaded)\.", re.M)
RE_FAILED = re.compile(r"^Failed, ", re.M)
# Not anchored on a source location: GHC also emits `<no location info>: error:` for link/IO failures.
RE_ERROR = re.compile(r"^.*?: error:", re.M)
RE_NOMODULE = re.compile(r"Could not (find|load) module|not in scope|is not loaded|hidden package|no such module", re.I)


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


def footprint_mb(pids: list[int]) -> float | None:
    """Physical footprint (MB) on macOS, or None where `footprint` is not there. `ps rss` is NOT this number:
    under memory pressure macOS compresses and swaps a process's pages and rss drops to almost nothing (a
    21 GB repl read 150 MB), so a budget on rss never fires exactly when it matters."""
    if sys.platform != "darwin" or not pids:
        return None
    try:
        out = subprocess.run(["footprint", *[str(p) for p in pids]], capture_output=True, text=True, timeout=30).stdout
    except (OSError, subprocess.SubprocessError):
        return None
    units = {"KB": 1 / 1024.0, "MB": 1.0, "GB": 1024.0, "TB": 1024.0 * 1024.0}
    total, seen = 0.0, False
    for l in out.splitlines():
        m = re.match(r"\s*phys_footprint:\s+([\d.]+)\s*([KMGT]B)\s*$", l)
        if m:
            total += float(m.group(1)) * units[m.group(2)]
            seen = True
    return total if seen else None


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
    fp = footprint_mb(sorted(kids))
    if fp is not None:
        return fp
    return sum(r for p, _, r in rows if p in kids) / 1024


def port_listener(port: int) -> int | None:
    """PID of whoever LISTENS on `port`, or None (also None when lsof is not there to ask)."""
    try:
        r = subprocess.run(["lsof", "-nP", f"-iTCP:{port}", "-sTCP:LISTEN", "-Fp"], capture_output=True, text=True, timeout=10)
    except Exception:  # noqa: BLE001
        return None
    for ln in r.stdout.splitlines():
        if ln.startswith("p") and ln[1:].isdigit():
            return int(ln[1:])
    return None


def pid_alive(pid: int) -> bool:
    """Running, and not a zombie (a forked child that died is a zombie until the repl waits on it)."""
    try:
        os.kill(pid, 0)
    except OSError:
        return False
    try:
        st = subprocess.run(["ps", "-o", "stat=", "-p", str(pid)], capture_output=True, text=True, timeout=10).stdout.strip()
        return bool(st) and not st.startswith("Z")
    except Exception:  # noqa: BLE001
        return True


def ghc_version() -> tuple:
    try:
        v = subprocess.run(["ghc", "--numeric-version"], capture_output=True, text=True, timeout=20).stdout.strip()
        return tuple(int(x) for x in v.split(".")[:2])
    except Exception:  # noqa: BLE001
        return (0, 0)


class Session:
    def __init__(self, conf: dict, name: str):
        self.conf = conf
        self.name = name
        self.cfg = config.resolve(conf, name)
        self.root = conf["root"]
        self.dir = os.path.join(conf["state_dir"], name)
        os.makedirs(self.dir, exist_ok=True)
        self.exts = tuple(self.cfg["watch_ext"])
        # build files at the root are watched too: a changed .cabal means a new package set, which a reload cannot adopt
        rootfiles = sorted(f for f in os.listdir(self.root) if f.endswith(".cabal") or f.startswith("cabal.project"))
        self.cfg["watch"] = list(self.cfg["watch"]) + [f for f in rootfiles if f not in self.cfg["watch"]]
        self.obj_rel = os.path.join(conf["state_rel"], name, "obj")
        self.repl: Repl | None = None
        self.last_status = "starting"
        self.last_json: dict = {}
        self.loaded_sig: dict[str, float] = {}
        self.pending_sig: dict[str, float] = {}
        self.loaded_at = 0.0
        self.checked_at = 0.0
        self.stopping = threading.Event()
        self.keep_servers = False
        self.owed: set[str] = set()     # servers a failed load could not re-fork; the next good one does
        self._work = threading.Lock()   # the watcher and a client both drive the repl; never at once
        self.hygiene_on = bool(self.cfg["hygiene"])
        self.zygote_on = bool(self.cfg["servers"])
        self.hm, self.zm = self.cfg["hygiene_module"], self.cfg["zygote_module"]
        self.generation = 0
        self.cwd = self.root            # the repl's own working directory, asked at boot
        self.last_used = time.time()    # the last client command or source change: what "idle" is measured from
        self.busy = 0                   # >0 while a command, a reload or a check holds the repl
        self._refork_thread: threading.Thread | None = None
        self._refork_pending: threading.Thread | None = None
        self._hold = 0                  # >0: an operation is in progress; its status is published once, at its end

    # -- logging and status --

    def log(self, msg: str) -> None:
        with open(os.path.join(self.dir, "daemon.log"), "a") as fh:
            fh.write(f"[{time.strftime('%H:%M:%S')}] {msg}\n")

    def async_out(self, text: str) -> None:
        with open(os.path.join(self.dir, "async.log"), "a") as fh:
            fh.write(text)

    def stale_files(self) -> list[str]:
        """Watched sources that differ from what the loaded code was built from."""
        now = scan(self.root, self.cfg["watch"], self.exts)
        return sorted(p for p in set(now) | set(self.loaded_sig) if now.get(p) != self.loaded_sig.get(p))

    def set_status(self, text: str, detail: list[str] | None = None, **facts) -> None:
        # A failure in a session that has NEVER passed is probably the target's, not the edit just made: say so.
        lastpass = os.path.join(self.dir, "last-pass")
        if text.startswith("OK") and "CHECK SKIPPED" not in text:
            if not os.path.exists(lastpass):
                write_atomic(lastpass, time.strftime("%Y-%m-%d %H:%M:%S") + "\n")
        elif text.startswith("CHECK-FAIL") and "[NEVER-PASSED" not in text and not os.path.exists(lastpass):
            text += "  [NEVER-PASSED: no check has been green in this state dir -- suspect the target as much as your edit]"
        self.last_status = text
        stale = [] if text == "starting" else self.stale_files()
        head = f"STALE({len(stale)}) {text}" if stale else text
        stamp = lambda t: time.strftime("%H:%M:%S", time.localtime(t)) if t else "-"  # noqa: E731
        lines = [head, f"session={self.name} gen={self.generation} at={time.strftime('%Y-%m-%d %H:%M:%S')} "
                       f"loaded={stamp(self.loaded_at)} checked={stamp(self.checked_at)}"]
        if stale:
            lines.append("stale: " + ", ".join(os.path.relpath(p, self.root) for p in stale[:6]))
        lines += detail or []
        self._status_text = "\n".join(lines) + "\n"
        kind = next((k for k in ("DEAD", "stopped", "starting", "PREBUILD-ERROR", "COMPILE-ERROR") if text.startswith(k)), None) \
            or next((k for k in ("CHECK-FAIL", "CHECK-PASS") if k in text), "OK")
        warn = re.search(r"\((\d+) warning\(s\)\)", text)
        j = dict(self.last_json) if facts.pop("_keep", False) else {}
        j.update({"session": self.name, "target": self.name, "kind": kind, "ok": kind in ("OK", "CHECK-PASS"),
                  "stale": len(stale), "stale_files": [os.path.relpath(p, self.root) for p in stale],
                  "warnings": int(warn.group(1)) if warn else 0, "verdict": text, "text": head,
                  "failing": len(detail or []) if kind == "CHECK-FAIL" else 0, "detail": list(detail or [])[:30],
                  "generation": self.generation, "at": time.time()})
        j.setdefault("members", [])
        j.setdefault("servers", [])
        j.update(facts)
        self.last_json = j
        if not self._hold:
            self._publish()
        self.push(head, lines[1])

    def push(self, verdict: str, detail: str = "") -> None:
        """POST a verdict to `status_url`, if one is configured: what lets a dashboard be event-driven instead
        of polling (a check that takes a second begins and ends between two polls, and a session busy
        building is the one too busy to answer a probe). Sent for every status, the intermediate ones too.
        Best effort, 0.25 s: an observer must never slow a session down or fail it."""
        url = self.cfg["status_url"]
        if not url:
            return
        try:
            import urllib.request
            body = json.dumps({"session": self.name, "target": self.name, "verdict": verdict, "detail": detail,
                               "alive": "1", "kind": self.last_json.get("kind"), "ok": self.last_json.get("ok")}).encode()
            urllib.request.urlopen(urllib.request.Request(url, data=body, headers={"Content-Type": "application/json"}),
                                   timeout=0.25).close()
        except Exception:  # noqa: BLE001
            pass

    def _publish(self) -> None:
        # status first, status.json second: a reader that finds the JSON can trust the text beside it
        write_atomic(os.path.join(self.dir, "status"), self._status_text)
        write_atomic(os.path.join(self.dir, "status.json"), json.dumps(self.last_json, indent=1))

    def one_verdict(self):
        """A reload is a verdict AND what then happened to the servers: published once, when both are known,
        so a reader never sees the first half as if it were the whole."""
        sess = self

        class _Held:
            def __enter__(self):
                sess._hold += 1

            def __exit__(self, *exc):
                sess._hold -= 1
                if not sess._hold:
                    sess._publish()
                return False

        return _Held()

    def note(self, suffix: str, **facts) -> None:
        """Append to the verdict (what happened to the servers, a restart) without losing its facts."""
        self.set_status(self.last_status + "  " + suffix, self.last_json.get("detail"), _keep=True, **facts)

    # -- the repl --

    def repl_command(self) -> str:
        cfg = self.cfg
        if cfg["repl"]:
            return cfg["repl"]
        cmd = "cabal repl"
        if len(cfg["units"]) > 1 or cfg["servers"]:
            cmd += " --enable-multi-repl"
        if cfg["rts_flags"] not in ("", "none"):
            cmd += f" --with-repl={os.path.join(PKG_DIR, 'bin', 'ghci-rts.sh')}"
        cmd += " --repl-options=-fdiagnostics-color=never"
        if cfg["hygiene"] or cfg["servers"]:
            # object code: CAFs of interpreted code are not prunable by address, and a server's code is its objects.
            # Its own -odir (relative: under each unit's package dir) so `cabal build` is not disturbed.
            cmd += f" --repl-options=-fobject-code --repl-options=-odir={self.obj_rel} --repl-options=-hidir={self.obj_rel}"
            if ghc_version() >= (9, 12):
                # without it GHCi's recompile of IDENTICAL source gives a different .o, and every reload
                # would look like a change to a running server
                cmd += " --repl-options=-fobject-determinism"
        return " ".join(x for x in (cmd, cfg["cabal_args"], " ".join(cfg["units"])) if x)

    def build_hygiene(self) -> None:
        script = os.path.join(PKG_DIR, "hygiene", "build.sh")
        r = subprocess.run([script, os.path.join(self.conf["state_dir"], "clib")], capture_output=True, text=True, cwd=self.root)
        self.log("hygiene build: " + (r.stderr.strip() or r.stdout.strip() or "ok").replace("\n", "; "))

    def post_load(self, repl: Repl) -> None:
        repl.post_load_basics()
        if self.cfg["capabilities"]:
            repl.command(f"GHC.Conc.setNumCapabilities {int(self.cfg['capabilities'])}", timeout=60)
        for expr in self.cfg["preload"]:
            repl.command(expr, timeout=120)
        for m in self.cfg["modules"]:
            repl.command(f":module + {m}", timeout=60)
        if self.cfg["hygiene"]:
            out = repl.command(f":module + {self.hm} GHC.Stats", timeout=60)
            self.hygiene_on = not RE_NOMODULE.search(out)
            if not self.hygiene_on:
                self.log(f"hygiene OFF: {self.hm} is not in scope in this repl (add ghci-hygiene to build-depends)")
        if self.cfg["servers"]:
            out = repl.command(f":module + {self.zm}", timeout=60)
            self.zygote_on = not RE_NOMODULE.search(out)
            if not self.zygote_on:
                self.log(f"servers OFF: {self.zm} is not in scope in this repl (add ghci-hygiene to build-depends)")

    def write_loaded_sources(self) -> None:
        """The signature the loaded code was built from, as a file the loaded code can read:
        `<mtime_ns>\t<path relative to the root>` per watched source. A cache keyed by SOURCE can trust a
        source only while it still carries this stamp (an object's age cannot say it: GHC leaves an
        unchanged module's .o alone however new its file)."""
        try:
            write_atomic(os.path.join(self.dir, "loaded_sources.tsv"),
                         "".join(f"{int(round(m * 1e9))}\t{os.path.relpath(p, self.root)}\n" for p, m in sorted(self.loaded_sig.items())))
        except OSError as e:
            self.log(f"loaded_sources: {e}")

    def verdict_of(self, out: str) -> tuple[str, list[str]]:
        errs = [l for l in out.splitlines() if RE_ERROR.match(l)]
        if RE_FAILED.search(out) or errs:
            return f"COMPILE-ERROR: {len(errs)} error(s)", errs[:20]
        if RE_OK.search(out):
            return "OK", []
        return "COMPILE-ERROR: no GHC verdict in the load output", out.strip().splitlines()[-5:]

    def compiled(self) -> bool:
        return not self.last_status.startswith(("COMPILE-ERROR", "DEAD"))

    def after_load(self, out: str, do_check: bool = True, t0: float | None = None) -> None:
        self.loaded_sig = self.pending_sig or scan(self.root, self.cfg["watch"], self.exts)
        self.loaded_at = time.time()
        self.generation += 1
        self.write_loaded_sources()
        write_atomic(os.path.join(self.dir, "load.log"), out)
        v, detail = self.verdict_of(out)
        warns = out.count(": warning:")
        self.ok_prefix = "OK" + (f" ({warns} warning(s))" if warns else "")
        took = lambda: round(time.time() - (t0 or self.loaded_at), 2)  # noqa: E731
        if v != "OK":
            self.set_status(v, detail, duration_s=took())
        elif not do_check or not self.cfg["checks"]:
            why = "--no-check" if not do_check else "no check configured"
            self.set_status(f"{self.ok_prefix} -- CHECK SKIPPED ({why}): this is a COMPILE verdict only", duration_s=took())
        else:
            self.run_check(t0=t0)

    # -- checks: per member, never merged --

    def _check_one(self, e: dict) -> dict:
        # Remember how old the check's log is, so a check that never ran cannot be scored against the PREVIOUS
        # run's file: a link failure leaves the log untouched and would read as a pass.
        logp = os.path.join(self.cwd, e["log"]) if e.get("log") else None
        before = -1.0
        if logp:
            try:
                before = os.path.getmtime(logp)
            except OSError:
                pass
        t0 = time.time()
        try:
            out = self.repl.command(e["expr"], timeout=e.get("timeout") or None)
        except ReplTimeout as ex:
            return {"member": e["member"], "kind": "TIMEOUT", "failing": [str(ex)], "body": str(ex), "duration_s": time.time() - t0}
        except ReplDied as ex:
            return {"member": e["member"], "kind": "DEAD", "failing": [str(ex)], "body": str(ex), "duration_s": time.time() - t0}
        dt = time.time() - t0
        body = out
        if logp:
            try:
                if os.path.getmtime(logp) <= before:
                    body = out + f"\n[session] {e['log']} was NOT rewritten by this run: the check did not get as far as producing output"
                else:
                    with open(logp) as fh:
                        body = fh.read()
            except OSError:
                body = out + f"\n[session] {e['log']} unreadable"
        fails = [l.strip() for l in body.splitlines() if e.get("fail") and re.search(e["fail"], l)]
        if fails:
            kind = "FAIL"
        elif e.get("pass") and not re.search(e["pass"], body, re.M):
            kind, fails = "INCOMPLETE", ["the pass marker never appeared (did the check run?)"] + body.strip().splitlines()[-3:]
        else:
            kind = "PASS"
        return {"member": e["member"], "kind": kind, "failing": fails, "body": body, "duration_s": round(dt, 2)}

    def run_check(self, t0: float | None = None, member: str | None = None) -> str:
        entries = [e for e in self.cfg["checks"] if member in (None, e["member"], e["member"].split(":")[0])]
        if not entries:
            return "no check configured" + (f" for {member!r}" if member else "")
        t = t0 or time.time()
        self.push(f"{getattr(self, 'ok_prefix', 'OK')} -- running check")
        results = [self._check_one(e) for e in entries]
        self.checked_at = time.time()
        write_atomic(os.path.join(self.dir, "run.log"), "\n".join(f"===== {r['member']} =====\n{r['body']}" for r in results) + "\n")
        took = round(time.time() - t, 2)
        bad = [r for r in results if r["kind"] != "PASS"]
        times = ", ".join(f"{r['member']} {r['duration_s']:.1f}s" for r in results)
        tag = f" [{len(results)} members: {times}]" if len(results) > 1 else ""
        facts = [{"member": r["member"], "name": r["member"], "kind": r["kind"], "duration_s": r["duration_s"],
                  "failing": len(r["failing"]), "detail": r["failing"][:12]} for r in results]
        if any(r["kind"] == "DEAD" for r in results):
            self.set_status("DEAD: the repl died during a check", [f"{r['member']}: {r['body']}" for r in bad][:20], members=facts, duration_s=took)
        elif bad:
            n = sum(len(r["failing"]) or 1 for r in bad)
            detail = [f"{r['member']}: {l}" for r in bad for l in (r["failing"] or [r["kind"]])]
            self.set_status(f"CHECK-FAIL: {n} failing in {', '.join(r['member'] for r in bad)}{tag}", detail[:30], members=facts, duration_s=took)
        else:
            self.set_status(f"{getattr(self, 'ok_prefix', 'OK')} -- CHECK-PASS ({took:.1f}s){tag}", members=facts, duration_s=took)
        return "\n".join(r["body"] for r in results)

    # -- memory --

    def repl_mb(self) -> float:
        """The repl's own memory: its process tree less the servers forked from it."""
        if not (self.repl and self.repl.pid):
            return 0.0
        return max(0.0, tree_rss_mb(self.repl.pid) - self.servers_mb(only_children_of=self.repl.pid))

    def servers_mb(self, only_children_of: int | None = None) -> float:
        total = 0.0
        rows = {p: pp for p, pp, _ in process_table()} if only_children_of else {}
        for label in self.server_labels():
            pid = self.server_running(label)
            if not pid:
                continue
            if only_children_of:   # a detached child is still in the repl's tree by ppid until the repl goes
                up, hops = pid, 0
                while up in rows and up != only_children_of and hops < 20:
                    up, hops = rows[up], hops + 1
                if up != only_children_of:
                    continue
            total += tree_rss_mb(pid)
        return total

    def prune_cafs(self) -> None:
        """Unlink the CAFs this reload superseded, then log the session's memory. Best effort: never fails a reload."""
        if not self.hygiene_on:
            return
        try:
            before = self.repl_mb()
            t0 = time.time()
            out = self.repl.command(
                self.hm + '.pruneCafs >>= \\k -> GHC.Stats.getRTSStatsEnabled >>= \\e -> '
                '(if e then GHC.Stats.getRTSStats >>= \\s -> return (show (GHC.Stats.gcdetails_live_bytes (GHC.Stats.gc s) `div` 1000000)) else return "?") >>= \\l -> '
                'putStrLn ("prune_cafs=" ++ show k ++ " live_mb=" ++ l)', timeout=300)
            k = next((l.split("=", 1)[1].split()[0] for l in out.splitlines() if l.startswith("prune_cafs=")), "?")
            live = next((l.split("live_mb=", 1)[1].strip() for l in out.splitlines() if "live_mb=" in l), "?")
            line = (f"[mem] prune_cafs: {k} unlinked in {time.time() - t0:.1f}s; repl {before:.0f} -> {self.repl_mb():.0f} MB, "
                    f"live heap {live} MB, servers {self.servers_mb():.0f} MB")
            self.log(line)
            with open(os.path.join(self.dir, "reload.log"), "a") as fh:
                fh.write("\n" + line + "\n")
        except Exception as e:  # noqa: BLE001
            self.log(f"prune_cafs failed: {e}")

    # -- servers: forked children of the repl (GHC.Hygiene.Zygote) --

    def server_labels(self) -> list[str]:
        return [z["member"] for z in self.cfg["servers"]]

    def server_spec(self, label: str) -> dict | None:
        return next((z for z in self.cfg["servers"] if z["member"] == label), None)

    def _sfile(self, label: str, what: str) -> str:
        return os.path.join(self.dir, f"server-{label}.{what}")

    def server_running(self, label: str) -> int | None:
        try:
            with open(self._sfile(label, "pid")) as fh:
                pid = int(fh.read().strip())
        except (OSError, ValueError):
            return None
        return pid if pid_alive(pid) else None

    def server_detach(self) -> bool:
        """A composed session's children outlive its repl: changing the member set restarts the repl, and
        taking every running server down to add one member is not a trade worth making. They are adopted
        back on boot and stopped explicitly at shutdown."""
        return self.cfg["composed"]

    def server_prefork(self, spec: dict) -> None:
        """Work the child must INHERIT: it is a fork without an exec, so anything not fork-safe (and anything
        slow) is done here in the parent. On a re-fork this runs BEFORE the old server is stopped, so the
        outage is only the stop and the fork."""
        if not spec.get("prefork"):
            return
        try:
            t0 = time.time()
            self.repl.command(spec["prefork"])
            self.log(f"server[{spec['member']}]: prefork done in {time.time() - t0:.1f}s")
        except Exception as e:  # noqa: BLE001
            self.log(f"server[{spec['member']}]: prefork failed ({e}); forking cold")

    def server_action_ok(self, spec: dict) -> str | None:
        """None when the server's action typechecks in the loaded code, else GHC's complaint. Asked BEFORE an
        old server is stopped: a fork that cannot happen must not cost the server that is running."""
        if not self.zygote_on:
            return f"{self.zm} is not in scope in this repl (add ghci-hygiene to build-depends)"
        try:
            out = self.repl.command(f":type ({spec['action']}) :: IO ()", timeout=120)
        except Exception as e:  # noqa: BLE001
            return str(e)
        return " ".join(out.split())[:300] if "error" in out else None

    def server_fork(self, spec: dict, handover: bool = False, preforked: bool = False) -> int | None:
        """Fork one server out of the code the repl holds RIGHT NOW.

        `handover` says whether this continues a server that was just stopped (carry its state in) or is a cold
        start. The child never decides that for itself: an envelope on disk may simply be stale.
        """
        label = spec["member"]
        if not self.zygote_on:
            self.log(f"server[{label}]: cannot fork, {self.zm} is not in scope")
            return None
        env = {k: str(v) for k, v in spec.get("env", {}).items()}
        hpath = self._sfile(label, "handover")
        env[self.cfg["handover_env"][0]] = hpath
        if handover and os.path.exists(hpath):
            env[self.cfg["handover_env"][1]] = hpath
        env_lit = "[" + ",".join("(%s,%s)" % (json.dumps(k), json.dumps(v)) for k, v in env.items()) + "]"
        # positional, not record update: GHC rejects a qualified record update on a field it also sees as a selector
        sp = self.zm + ".zygoteSpec %s %s %s %s" % (json.dumps(label), json.dumps(self._sfile(label, "log")),
                                                          env_lit, "True" if self.server_detach() else "False")
        if not preforked:
            self.server_prefork(spec)
        try:
            out = self.repl.command("fmap %s.zcPid (%s.zygoteFork (%s) (%s))" % (self.zm, self.zm, sp, spec["action"]))
        except Exception as e:  # noqa: BLE001
            self.log(f"server[{label}]: fork failed: {e}")
            return None
        # A pid is a whole line of digits, never a number lifted out of prose: a type error's
        # `<interactive>:16:120:` would otherwise be reported as pid 16.
        pid = next((int(l.strip()) for l in strip_ansi(out).splitlines() if l.strip().isdigit()), None)
        if pid is None:
            self.log(f"server[{label}]: fork produced no pid: " + " ".join(out.split())[:300])
            return None
        self.log(f"server[{label}]: forked pid {pid} -> {self._sfile(label, 'log')}")
        # Verified BEFORE the pid file is written: a fork that dies on a busy port must not overwrite the
        # record of the healthy child it collided with.
        if not self.server_verify(label, pid, spec):
            return None
        write_atomic(self._sfile(label, "pid"), f"{pid}\n")
        fp = self.code_fingerprint(spec)
        if fp:
            write_atomic(self._sfile(label, "code"), fp + "\n")
        else:
            self._unlink(self._sfile(label, "code"))
        return pid

    def server_verify(self, label: str, pid: int, spec: dict) -> bool:
        """Did the fork produce a SERVER, or just a process? A dead child is a failure; so is the port held by
        a different process (the child does not die, it just never listens). Alive with the port still free
        is not: a server may build its state before it listens, so a timeout only warns."""
        port = spec.get("port")
        if not port:
            time.sleep(0.3)
            if pid_alive(pid):
                return True
            self.log(f"server[{label}]: pid {pid} died at once -- {self._log_tail(label)}")
            self.server_stop_pid(label, pid)
            return False
        deadline = time.time() + float(spec.get("verify_timeout", 60))
        conflict_after = time.time() + 3.0   # the old child's socket can outlive its exit by a moment
        while time.time() < deadline:
            if not pid_alive(pid):
                self.log(f"server[{label}]: pid {pid} DIED before taking port {port} -- {self._log_tail(label)}")
                self.server_stop_pid(label, pid)   # reap it: GHCi never waits on a child
                return False
            holder = port_listener(port)
            if holder == pid:
                self.log(f"server[{label}]: pid {pid} serving port {port}")
                return True
            if holder and time.time() > conflict_after:
                self.log(f"server[{label}]: port {port} is held by pid {holder}, not our child {pid} -- stopping it")
                self.server_stop_pid(label, pid)
                return False
            time.sleep(0.25)
        self.log(f"server[{label}]: pid {pid} alive but port {port} not taken after {spec.get('verify_timeout', 60)}s -- leaving it to finish booting")
        return True

    def _log_tail(self, label: str) -> str:
        try:
            with open(self._sfile(label, "log")) as fh:
                return " | ".join(fh.read().splitlines()[-3:])[:300]
        except OSError:
            return ""

    @staticmethod
    def _unlink(path: str) -> None:
        try:
            os.unlink(path)
        except OSError:
            pass

    def server_stop_pid(self, label: str, pid: int) -> None:
        """Stop one pid THROUGH the repl, which is what reaps it: GHCi installs no SIGCHLD handling, so a child
        killed from outside stays a zombie for the life of the session. Falls back to signals if the repl
        cannot take a command."""
        expr = "%s.zygoteStop (%s.ZygoteChild %d %s %s) 30" % (
            self.zm, self.zm, pid, json.dumps(label), json.dumps(self._sfile(label, "log")))
        try:
            if not (self.repl and self.repl.alive and self.zygote_on):
                raise ReplDied("no repl")
            self.repl.command(expr, timeout=30)
            if pid_alive(pid):
                raise ReplDied("still alive")
            self.log(f"server[{label}]: stopped pid {pid}")
        except Exception as e:  # noqa: BLE001
            self.log(f"server[{label}]: stopping pid {pid} by signal ({e})")
            for sig, wait in ((signal.SIGTERM, 3.0), (signal.SIGKILL, 2.0)):
                try:
                    os.kill(pid, sig)
                except OSError:
                    break
                t = time.time() + wait
                while time.time() < t and pid_alive(pid):
                    time.sleep(0.1)
                if not pid_alive(pid):
                    break

    def server_stop(self, label: str) -> None:
        pid = self.server_running(label)
        if pid is not None:
            self.server_stop_pid(label, pid)
        self._unlink(self._sfile(label, "pid"))

    def server_orphans(self) -> list[tuple[str, int]]:
        """Live children this session no longer declares. Dropping a member does not stop its server, and the
        child keeps its PORT, so the running set is read from the pid files, not from the member list."""
        declared = set(self.server_labels())
        out = []
        for f in sorted(glob.glob(os.path.join(self.dir, "server-*.pid"))):
            label = os.path.basename(f)[len("server-"):-len(".pid")]
            if label in declared:
                continue
            pid = self.server_running(label)
            if pid is None:
                self._unlink(f)
            else:
                out.append((label, pid))
        return out

    def servers_boot(self) -> list[str]:
        """After a boot: stop what is no longer a member, ADOPT what is still running, and start what must
        always serve. Loading a target is not serving it: a server starts when asked, or when it declares
        `serve_on_load`."""
        out = []
        for label, pid in self.server_orphans():
            self.log(f"server[{label}]: no longer a member -- stopping pid {pid}")
            self.server_stop(label)
            out.append(f"{label}: dropped")
        for z in self.cfg["servers"]:
            label = z["member"]
            pid = self.server_running(label)
            if pid:
                self.log(f"server[{label}]: adopted running pid {pid}")
                out.append(f"{label}: adopted pid {pid}")
            elif z.get("serve_on_load") and self.compiled():
                pid = self.server_fork(z)
                out.append(f"{label}: {'pid %d' % pid if pid else 'FAILED'}")
        return out

    def servers_stop_all(self) -> None:
        for label in self.server_labels() + [l for l, _ in self.server_orphans()]:
            self.server_stop(label)

    # Is the running server's CODE still the code? A child needs replacing exactly when the code it would run
    # differs from the code it was forked from: the object files of its units and of every in-session unit
    # they depend on (cabal's per-unit argument files list both), the declared extra files, and its spec.

    def _unit_files(self) -> dict[str, dict]:
        dirs = sorted(glob.glob(os.path.join(self.root, "dist-newstyle", "multi-out-*")), key=os.path.getmtime)
        if not dirs:
            return {}
        units: dict[str, dict] = {}
        modre = re.compile(r"^[A-Z][A-Za-z0-9_']*(\.[A-Z][A-Za-z0-9_']*)*$")
        for f in glob.glob(os.path.join(dirs[-1], "*")):
            if not os.path.isfile(f):
                continue
            try:
                with open(f) as fh:
                    args = fh.read().split("\n")
            except OSError:
                continue
            uid = pkg = None
            wd = self.root
            deps, mods, maybe = [], [], []
            i = 0
            while i < len(args):
                a = args[i].strip()
                nxt = args[i + 1].strip() if i + 1 < len(args) else ""
                if a in ("-this-unit-id", "-this-package-name", "-working-dir", "-package-id"):
                    if a == "-this-unit-id":
                        uid = nxt
                    elif a == "-this-package-name":
                        pkg = nxt
                    elif a == "-working-dir":
                        wd = nxt
                    else:
                        deps.append(nxt)
                    i += 2
                    continue
                if not a.startswith("-") and modre.match(a):
                    # A capitalised word right after a flag may be that flag's VALUE (`-framework Accelerate`),
                    # not a module: remember it, and let the missing object decide.
                    prev = args[i - 1].strip() if i else ""
                    (maybe if prev.startswith("-") else mods).append(a)
                i += 1
            if uid:
                odir = os.path.join(wd, self.obj_rel)
                mods += [m for m in maybe if os.path.exists(os.path.join(odir, *m.split(".")) + ".o")]
                units[uid] = {"pkg": pkg, "modules": mods, "deps": deps, "wd": wd}
        return units

    def code_fingerprint(self, spec: dict) -> str | None:
        """A hash of everything a child forked now would run, or None when that cannot be said (then the
        child is always re-forked)."""
        want = [u.split(":", 1) for u in spec.get("units") or [] if ":" in u]
        units = self._unit_files()
        if not units:
            return self._fingerprint_all_objects(spec)
        if not want:
            return None
        roots = []
        for kind, comp in want:
            ids = [uid for uid, u in units.items()
                   if (kind == "lib" and u["pkg"] == comp and uid.endswith("-inplace"))
                   or (kind != "lib" and uid.endswith("-inplace-" + comp))]
            if not ids:
                return None
            roots += ids
        seen, todo = set(), list(roots)
        while todo:
            uid = todo.pop()
            if uid in seen or uid not in units:
                continue
            seen.add(uid)
            todo += [d for d in units[uid]["deps"] if d in units]
        # -odir is relative and each unit compiles in its own package directory
        objs = sorted({(m, os.path.join(units[uid]["wd"], self.obj_rel)) for uid in seen for m in units[uid]["modules"]})
        h = hashlib.sha256()
        try:
            for m, odir in objs:
                with open(os.path.join(odir, *m.split(".")) + ".o", "rb") as fh:
                    h.update(m.encode() + b"\0" + hashlib.sha256(fh.read()).digest())
            for extra in self.cfg["fingerprint_files"]:
                with open(os.path.join(self.root, extra), "rb") as fh:
                    h.update(extra.encode() + b"\0" + hashlib.sha256(fh.read()).digest())
        except OSError:
            return None   # something we cannot see: make no claim
        h.update(json.dumps(spec, sort_keys=True).encode())
        return h.hexdigest()

    def _fingerprint_all_objects(self, spec: dict) -> str | None:
        """Without cabal's per-unit files (a single-unit repl has none): every object this session compiled.
        Over-inclusive, which is the safe side -- it can only re-fork more often, never less."""
        objs = []
        for dirpath, dirnames, filenames in os.walk(self.root):
            dirnames[:] = [d for d in dirnames if d != "dist-newstyle" and d != ".git"]
            if os.path.join(dirpath, "").endswith(os.path.join(self.obj_rel, "")) or (os.sep + self.obj_rel + os.sep) in dirpath + os.sep:
                objs += [os.path.join(dirpath, f) for f in filenames if f.endswith(".o")]
        if not objs:
            return None
        h = hashlib.sha256()
        try:
            for p in sorted(objs):
                with open(p, "rb") as fh:
                    h.update(os.path.relpath(p, self.root).encode() + b"\0" + hashlib.sha256(fh.read()).digest())
            for extra in self.cfg["fingerprint_files"]:
                with open(os.path.join(self.root, extra), "rb") as fh:
                    h.update(extra.encode() + b"\0" + hashlib.sha256(fh.read()).digest())
        except OSError:
            return None
        h.update(json.dumps(spec, sort_keys=True).encode())
        return h.hexdigest()

    def code_unchanged(self, label: str) -> bool:
        spec = self.server_spec(label)
        try:
            with open(self._sfile(label, "code")) as fh:
                was = fh.read().strip()
        except OSError:
            return False
        return bool(spec) and bool(was) and self.code_fingerprint(spec) == was

    PENDING = "  [servers: re-fork running in the background -- the old server serves until the new one is up]"

    def refork_join(self) -> None:
        """Wait for a background re-fork: anything that touches the servers or reloads must not overlap one."""
        t = self._refork_thread
        if t is not None and t.is_alive() and t is not threading.current_thread():
            self.log("waiting for the background re-fork")
            t.join()

    def refork_async(self, was_running: set[str]) -> None:
        """The re-fork in a thread, so a reload returns at its verdict. The repl is one process: a command
        sent during the prefork queues behind it, but the client is not held. The thread is started by
        'reload' once the verdict (with its "running in the background" note) has been published."""
        self.set_status(self.last_status + self.PENDING, self.last_json.get("detail"), _keep=True, servers_pending=True)

        def go():
            try:
                self.refork(was_running)
            except Exception as e:  # noqa: BLE001
                self.log(f"background re-fork: {type(e).__name__}: {e}")
                self.set_status(self.last_status.replace(self.PENDING, "") + f"  [servers: background re-fork FAILED: {e}]",
                                self.last_json.get("detail"), _keep=True, servers_pending=False)

        self._refork_pending = threading.Thread(target=go, daemon=True)

    def refork(self, was_running: set[str]) -> None:
        """Bring the servers that were running onto the code now loaded: keep those whose code did not change,
        stop and fork the rest. Prefork first, with the old servers still serving."""
        was_running = set(was_running) | self.owed
        self.owed = set()
        if not was_running:
            return
        kept = {l for l in was_running if self.server_running(l) and self.code_unchanged(l)}
        todo = [z for z in self.cfg["servers"] if z["member"] in was_running - kept]
        broken = {}
        for z in todo:
            why = self.server_action_ok(z)
            if why:
                broken[z["member"]] = why
                self.log(f"server[{z['member']}]: action does not typecheck, not re-forked: {why}")
            else:
                self.server_prefork(z)
        res = []
        for z in todo:
            label = z["member"]
            if label in broken:
                if not self.server_running(label):
                    self.owed.add(label)
                continue
            carried = bool(self.server_running(label))
            self.server_stop(label)   # it writes its state on the way out
            pid = self.server_fork(z, handover=carried, preforked=True)
            res.append((label, pid, carried and os.path.exists(self._sfile(label, "handover"))))
        for l in sorted(kept):
            self.log(f"server[{l}]: code unchanged -- kept pid {self.server_running(l)}")
        bad = [l for l, pid, _ in res if not pid]
        parts = []
        ok = [f"{l}:{pid}{'+state' if st else ''}" for l, pid, st in res if pid]
        if ok:
            parts.append(f"re-forked {len(ok)} server(s) ({', '.join(ok)}), now running the NEW code")
        if kept:
            parts.append(f"kept {len(kept)} ({', '.join(f'{l}:{self.server_running(l)}' for l in sorted(kept))}): their code did not change")
        if bad:
            parts.append(f"RE-FORK FAILED for {', '.join(bad)} -- not running")
        if broken:
            still = [l for l in broken if self.server_running(l)]
            parts.append(f"NOT re-forked: {', '.join(broken)} (the action no longer typechecks: see daemon.log)"
                         + (f"; {', '.join(still)} still on the OLD code" if still else ""))
        facts = ([{"member": l, "action": "re-forked" if pid else "failed", "pid": pid} for l, pid, _ in res]
                 + [{"member": l, "action": "kept", "pid": self.server_running(l)} for l in sorted(kept)]
                 + [{"member": l, "action": "broken", "pid": self.server_running(l)} for l in broken])
        self.last_status = self.last_status.replace(self.PENDING, "")
        self.note("[servers: " + "; ".join(parts) + "]", servers=facts, servers_pending=False)

    def server_op(self, action: str, member: str | None = None, resume: bool = False) -> str:
        specs = [z for z in self.cfg["servers"] if member in (None, z["member"])]
        if not specs:
            return f"no server {member!r} in this session" if member else "this session declares no server"
        lines = []
        for z in specs:
            label = z["member"]
            if action == "stop":
                self.server_stop(label)
                lines.append(f"{label}: stopped")
            elif action in ("start", "restart"):
                if action == "start" and self.server_running(label):
                    lines.append(f"{label}: already running pid {self.server_running(label)}")
                    continue
                # a restart is a cut-over and carries the state the old child just wrote; a start brings up
                # something that was NOT running, for an unknown time, so it is cold unless asked (--resume)
                why = self.server_action_ok(z)
                if why:
                    lines.append(f"{label}: FAILED, the action does not typecheck ({why})"
                                 + ("; the running server was left alone" if self.server_running(label) else ""))
                    continue
                carry = resume or (action == "restart" and bool(self.server_running(label)))
                self.server_stop(label)
                pid = self.server_fork(z, handover=carry)
                took = carry and os.path.exists(self._sfile(label, "handover"))
                lines.append(f"{label}: {'pid %d' % pid if pid else 'FAILED (see daemon.log)'}{'+state' if pid and took else ''}")
            else:
                pid = self.server_running(label)
                port = f" port {z['port']}" if z.get("port") else ""
                code = "" if not pid else (" (current code)" if self.code_unchanged(label) else " (code differs from what is loaded, or unknown)")
                lines.append(f"{label}: {'running pid %d' % pid if pid else 'not running'}{port}{code}")
        return "\n".join(lines)

    # -- lifecycle --

    def boot(self) -> None:
        if self.cfg["hygiene"] and self.cfg["hygiene_build"]:
            self.build_hygiene()
        if self.cfg["prebuild"]:
            t0 = time.time()
            r = subprocess.run(self.cfg["prebuild"], shell=True, cwd=self.root, capture_output=True, text=True)
            write_atomic(os.path.join(self.dir, "prebuild.log"), r.stdout + r.stderr)
            self.log(f"prebuild: exit {r.returncode} in {time.time() - t0:.1f}s")
            if r.returncode != 0:
                self.set_status("PREBUILD-ERROR: see prebuild.log", (r.stdout + r.stderr).strip().splitlines()[-8:])
                raise RuntimeError("prebuild failed")
        env = dict(self.cfg["env"])
        if self.cfg["rts_flags"] not in ("", "none"):
            env.setdefault("GHS_RTS_FLAGS", self.cfg["rts_flags"])
        env.setdefault("GHS_DIR", os.path.join(self.conf["state_dir"], "clib"))
        env["GHCI_SESSION"] = self.name
        self.repl = Repl(self.repl_command(), self.root, env, self.cfg["load_timeout"], self.cfg["eval_timeout"],
                         log=self.log, on_async=self.async_out)
        hold, self._hold = self._hold, 0
        self.set_status("starting")      # always visible at once: `start` waits on it
        self._hold = hold
        t0 = time.time()
        self.pending_sig = scan(self.root, self.cfg["watch"], self.exts)
        try:
            out = self.repl.start(self.post_load)
        except (ReplDied, ReplTimeout) as e:
            self.set_status(f"DEAD: {e}", str(e).splitlines()[-8:])
            raise
        try:   # `cabal repl` may chdir into the package: a check that writes a relative path lands there
            self.cwd = self.repl.command(":!pwd", timeout=60).strip().splitlines()[-1] or self.root
        except Exception:  # noqa: BLE001
            self.cwd = self.root
        self.after_load(out, t0=t0)

    def restart(self, refork: bool = True) -> str:
        """A fresh repl. The servers that were running come back on the new code (a plain session's children
        die with its repl; a composed session's are kept if their code did not change)."""
        self.refork_join()
        with self.one_verdict():
            return self._restart(refork)

    def _restart(self, refork: bool) -> str:
        self.log("restart")
        was_running = {l for l in self.server_labels() if self.server_running(l)}
        self.repl.stop()
        self.boot()
        if self.cfg["servers"]:
            if self.compiled() and refork:
                self.refork(was_running)
            elif was_running:
                self.owed |= was_running
        return self.last_status

    def reload(self, do_check: bool = True, refork: bool = True, async_refork: bool | None = None) -> str:
        self.refork_join()
        if async_refork is None:
            async_refork = bool(self.cfg["async_refork"]) or os.environ.get("GHS_ASYNC_REFORK") == "1"
        self._refork_pending = None
        with self.one_verdict():
            out = self._reload(do_check, refork, async_refork)
        if self._refork_pending is not None:   # only now: the verdict it amends is on disk
            self._refork_thread, self._refork_pending = self._refork_pending, None
            self._refork_thread.start()
        return out

    def _reload(self, do_check: bool, refork: bool, async_refork: bool = False) -> str:
        budget = float(os.environ.get("GHS_REPL_BUDGET_MB", self.cfg["repl_budget_mb"]))
        rss = self.repl_mb()
        if budget > 0 and rss > budget:
            self.log(f"reload: repl at {rss:.0f} MB > budget {budget:.0f} MB -- restarting instead")
            out = self.restart()
            self.note(f"[repl RESTARTED instead of reloaded: it had grown to {rss:.0f} MB, over the {budget:.0f} MB budget; now {self.repl_mb():.0f} MB]")
            return out
        t0 = time.time()
        self.pending_sig = scan(self.root, self.cfg["watch"], self.exts)
        self.push("reloading")
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
        running = {l for l in self.server_labels() if self.server_running(l)}
        if running or self.owed:
            # A reload updates the code the repl HOLDS, not the code a running child IS.
            if not self.compiled():
                self.note("[servers: NOT re-forked (compile error) -- they still run the OLD code]")
            elif not refork:
                self.note("[servers: NOT re-forked -- they still run the OLD code; `reload` cuts them over]")
            elif async_refork:
                self.refork_async(running)
            else:
                self.refork(running)
        return out

    def head_commit(self) -> str:
        try:
            r = subprocess.run(["git", "rev-parse", "HEAD"], cwd=self.root, capture_output=True, text=True, timeout=5)
            return r.stdout.strip() if r.returncode == 0 else ""
        except (OSError, subprocess.SubprocessError):
            return ""

    # -- idleness --

    def info(self) -> dict:
        """What an eviction decision needs, answered without the repl: so it works while a reload holds it."""
        running = [l for l in self.server_labels() if self.server_running(l)]
        t = self._refork_thread
        return {"session": self.name, "idle_s": round(time.time() - self.last_used, 1),
                "busy": bool(self.busy) or bool(t is not None and t.is_alive()),
                "repl_mb": round(self.repl_mb()), "servers_mb": round(self.servers_mb()), "serving": running,
                "verdict": self.last_status}

    def idle_stop_due(self) -> bool:
        """This session's own rule (`idle_stop_mins`): unused that long, not busy, and serving nothing --
        a server is in use by whoever is connected to it, which this daemon cannot see."""
        mins = float(self.cfg["idle_stop_mins"])
        if mins <= 0 or self.busy or time.time() - self.last_used < mins * 60:
            return False
        return not any(self.server_running(l) for l in self.server_labels())

    # -- the watcher --

    def watch_loop(self) -> None:
        last = scan(self.root, self.cfg["watch"], self.exts)
        on_commit = self.cfg["reload_on_commit"]
        head, polls = (self.head_commit() if on_commit else ""), 0
        while not self.stopping.wait(0.5):
            polls += 1
            if on_commit and polls % 4 == 0:
                now_head = self.head_commit()
                if now_head and head and now_head != head:
                    # a commit is when everything catches up, whatever a save does: checks and servers too
                    self.log(f"commit {now_head[:10]}: full reload (check, re-fork)")
                    self.last_used = time.time()
                    with self._work:
                        self.busy += 1
                        try:
                            last = scan(self.root, self.cfg["watch"], self.exts)
                            self.reload(do_check=True, refork=True)
                        except Exception as e:  # noqa: BLE001
                            self.log(f"commit reload: {type(e).__name__}: {e}")
                        finally:
                            self.busy -= 1
                head = now_head or head
            now = scan(self.root, self.cfg["watch"], self.exts)
            if now == last:
                if self.idle_stop_due():
                    self.log(f"idle for {self.cfg['idle_stop_mins']} min -- stopping (idle_stop_mins)")
                    self.stop_reason = f"stopped: idle for {self.cfg['idle_stop_mins']:g} min (idle_stop_mins); `ghci-session start {self.name}`"
                    self.stopping.set()
                continue
            self.last_used = time.time()   # someone is editing
            time.sleep(self.cfg["debounce"])   # an editor's save is several writes
            now = scan(self.root, self.cfg["watch"], self.exts)
            last = now
            if not self.cfg["auto_reload"] or now == self.loaded_sig:
                continue
            with self._work:
                if self.stopping.is_set():
                    return
                self.busy += 1
                changed = [p for p in set(now) | set(self.loaded_sig) if now.get(p) != self.loaded_sig.get(p)]
                if not changed:
                    continue
                try:
                    if any(p.endswith((".c", ".h", ".cabal")) or os.path.basename(p).startswith("cabal.project") for p in changed):
                        self.log("a .c/.h/.cabal changed: restarting the repl (a loaded C object, or a package set, cannot be replaced)")
                        self.restart()
                    else:
                        self.log(f"watch: {len(changed)} file(s) changed -- reload")
                        self.reload(do_check=self.cfg["watch_check"], refork=self.cfg["watch_refork"])
                except Exception as e:  # noqa: BLE001
                    self.log(f"watch: {type(e).__name__}: {e}")
                finally:
                    self.busy -= 1
                    self.last_used = time.time()

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
            self._unlink(p)

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
                elif op == "info":
                    reply(True, json.dumps(self.info()))
                elif op == "stop":
                    if req.get("reason"):
                        self.stop_reason = str(req["reason"])
                    self.keep_servers = bool(req.get("keep_servers"))
                    self.stopping.set()
                    reply(True, "stopping")
                else:
                    self.last_used = time.time()
                    self.busy += 1
                    try:
                        out = self.dispatch(op, req)
                    finally:
                        self.busy -= 1
                        self.last_used = time.time()
                    if out is None:
                        reply(False, f"unknown op {op!r}")
                    else:
                        reply(True, out)
            except Exception as e:  # a broken eval must not kill the daemon
                try:
                    reply(False, f"{type(e).__name__}: {e}")
                except OSError:
                    pass

    def dispatch(self, op: str, req: dict) -> str | None:
        with self._work:   # eval is inside the lock too: its answer must not straddle a reload
            if op == "eval":
                out = self.repl.command(req["expr"], timeout=req.get("timeout") or None)
            elif op == "reload":
                out = self.reload(do_check=req.get("check", True), refork=req.get("refork", True),
                                  async_refork=req.get("async_refork"))
            elif op == "check":
                out = self.run_check(member=req.get("member"))
            elif op == "restart":
                out = self.restart()
            elif op in ("server", "zygote"):   # "zygote" with fork/refork: the names an older client of this protocol used
                req = dict(req, action={"fork": "start", "refork": "restart"}.get(req.get("action"), req.get("action", "status")))
                self.refork_join()
                out = self.server_op(req.get("action", "status"), req.get("member"), bool(req.get("resume")))
            elif op == "mem":
                out = (f"repl {self.repl_mb():.0f} MB (budget {self.cfg['repl_budget_mb']}), "
                       f"servers {self.servers_mb():.0f} MB")
            else:
                return None
        return out

    def run(self) -> None:
        write_atomic(os.path.join(self.dir, "pid"), str(os.getpid()))
        try:
            with self.one_verdict():
                self.boot()
                started = self.servers_boot()
                if started:
                    self.note("[servers: " + "; ".join(started) + "]")
        except Exception as e:  # noqa: BLE001
            self.log(f"boot failed: {e}")
            if self.repl:
                self.repl.stop()
            self._unlink(os.path.join(self.dir, "pid"))
            raise
        threading.Thread(target=self.watch_loop, daemon=True).start()
        try:
            self.serve()
        finally:
            self.refork_join()
            if not self.keep_servers:
                self.servers_stop_all()
            self.repl.stop()
            self.set_status(getattr(self, "stop_reason", "stopped"))
            self._unlink(os.path.join(self.dir, "pid"))
