#!/usr/bin/env python3
"""A tour of ghci-session: every feature exercised on `examples/hello`, each step checked and timed.

    python3 examples/tour.py                  # the whole tour (~4 min), a table of steps and times
    python3 examples/tour.py --only servers   # one group (see --list)
    python3 examples/tour.py --json out.json  # the result as data
    python3 examples/tour.py --compare out.json   # ... and a later run against it: what got slower
    python3 examples/tour.py --keep           # leave the working copy behind and say where it is

It works on a COPY of the example in a temporary directory (the tour edits sources, kills daemons and commits
to git there), so the checkout is not touched. Exit status 1 if any step's outcome was not the expected one.
Read it as documentation too: each step is the command a person would type and what it must answer.

The groups: boot, eval, reload, watch, stale, compose, servers, census, leak, budget, hooks, idle, gc.
"""
import argparse
import http.server
import json
import os
import shutil
import signal
import subprocess
import sys
import tempfile
import threading
import time

HERE = os.path.dirname(os.path.abspath(__file__))
PKG = os.path.dirname(HERE)

LIVE_MB = ("System.Mem.performMajorGC >> GHC.Stats.getRTSStats >>= \\s -> "
           "putStrLn (\"live_mb=\" ++ show (GHC.Stats.gcdetails_live_bytes (GHC.Stats.gc s) `div` 1000000))")


def config(push_port: int) -> dict:
    """The tour's ghci-session.json: the example's two packages, declared as the targets the tour needs."""
    hello = {"units": ["lib:hello"], "watch": ["src"], "modules": ["Hello"],
             "check": {"expr": "Hello.selfTest", "pass": r"\[PASS\] table"}}
    extra = {"units": ["lib:extra"], "watch": ["extra/src"], "modules": ["Extra"],
             "check": {"expr": "Extra.selfTest", "pass": r"\[PASS\] shout"}}
    return {
        "default": "hello",
        "hygiene": True,
        "status_url": f"http://127.0.0.1:{push_port}/push",
        "targets": {
            # the main one: a check, a server with something slow to do before it forks, a prebuild, a placeholder
            "hello": {**hello, "env": {"HELLO_SESSION": "{session}"},
                      "prebuild": "date > {state}/prebuilt.txt",
                      "server": {"action": "Hello.serve", "env": {"HELLO_OUT": "{state}/hello.out"},
                                 "prefork": "Control.Concurrent.threadDelay 2000000"}},
            "extra": extra,
            # no watcher: so a changed source stays unloaded and the verdict must say STALE
            "manual": {**extra, "auto_reload": False, "hygiene": False},
            # the leak, with and without the pruner; reloads are explicit so the two are measured alike
            "tidy": {**hello, "auto_reload": False},
            "leaky": {**hello, "auto_reload": False, "hygiene": False, "cabal_args": "--repl-options=-fobject-code"},
            # a save only compiles; a commit runs the check
            "oncommit": {**extra, "watch_check": False, "reload_on_commit": True, "hygiene": False},
            # a check that has never been green
            "broken": {**extra, "hygiene": False, "auto_reload": False,
                       "check": {"expr": "putStrLn \"[FAIL] this target never passes\""}},
            # stops itself when nobody uses it
            "sleepy": {**extra, "hygiene": False, "idle_stop_mins": 0.05},
        },
        "sessions": {"dev": ["hello", "extra"]},
    }


class Tour:
    def __init__(self, keep: bool):
        self.keep = keep
        self.steps: list[dict] = []
        self.group = ""
        self.pushes: list[dict] = []
        self.dir = tempfile.mkdtemp(prefix="ghci-session-tour-")
        self.proj = os.path.join(self.dir, "examples", "hello")
        ig = shutil.ignore_patterns(".ghci-session", "dist-newstyle", "__pycache__")
        shutil.copytree(os.path.join(PKG, "examples", "hello"), self.proj, ignore=ig)
        for d in ("hygiene", "bin", "ghci_session"):
            shutil.copytree(os.path.join(PKG, d), os.path.join(self.dir, d), ignore=ig)
        self.cli_path = os.path.join(self.dir, "bin", "ghci-session")
        self.state = os.path.join(self.proj, ".ghci-session")
        self.hs = os.path.join(self.proj, "src", "Hello.hs")
        self.ex = os.path.join(self.proj, "extra", "src", "Extra.hs")
        self.hs0, self.ex0 = self.read(self.hs), self.read(self.ex)
        self.push_port = self.listen()
        self.conf = config(self.push_port)
        self.write_conf()
        for cmd in (["git", "init", "-q"], ["git", "add", "-A"],
                    ["git", "-c", "user.name=tour", "-c", "user.email=tour@example.org", "commit", "-qm", "start"]):
            subprocess.run(cmd, cwd=self.proj, capture_output=True)

    # -- plumbing --

    @staticmethod
    def read(path: str) -> str:
        with open(path) as fh:
            return fh.read()

    @staticmethod
    def write(path: str, text: str) -> None:
        with open(path, "w") as fh:
            fh.write(text)

    def write_conf(self) -> None:
        self.write(os.path.join(self.proj, "ghci-session.json"), json.dumps(self.conf, indent=1))

    def listen(self) -> int:
        """A tiny listener for `status_url`: counts what the sessions push."""
        tour = self

        class H(http.server.BaseHTTPRequestHandler):
            def do_POST(self):
                try:
                    tour.pushes.append(json.loads(self.rfile.read(int(self.headers.get("Content-Length", 0)))))
                except ValueError:
                    pass
                self.send_response(200)
                self.end_headers()

            def log_message(self, *a):
                pass

        srv = http.server.ThreadingHTTPServer(("127.0.0.1", 0), H)
        threading.Thread(target=srv.serve_forever, daemon=True).start()
        return srv.server_address[1]

    def run(self, *argv, env=None, timeout=900):
        t0 = time.time()
        p = subprocess.run([self.cli_path, *argv], cwd=self.proj, capture_output=True, text=True, timeout=timeout,
                           env={**os.environ, **(env or {})})
        return p.returncode, p.stdout, p.stderr, time.time() - t0

    def status(self, session: str) -> dict:
        with open(os.path.join(self.state, session, "status.json")) as fh:
            return json.load(fh)

    def at(self, session: str) -> float:
        try:
            return self.status(session)["at"]
        except (OSError, ValueError, KeyError):
            return 0.0

    def wait(self, session: str, pred, since: float, secs: float = 120):
        """Wait for a status newer than `since` that satisfies `pred`; (status or None, seconds waited)."""
        t0 = time.time()
        while time.time() - t0 < secs:
            try:
                j = self.status(session)
                if j["at"] > since and not j.get("servers_pending") and pred(j):
                    return j, time.time() - t0
            except (OSError, ValueError, KeyError):
                pass
            time.sleep(0.05)
        return None, time.time() - t0

    def served(self, session: str = "dev"):
        try:
            word, n = self.read(os.path.join(self.state, "hello.out")).split()
            return word, int(n)
        except (OSError, ValueError):
            return None, -1

    def server_pid(self, session: str):
        out = self.run("server", "-s", session)[1]
        return int(out.split("pid ")[1].split()[0]) if "running pid" in out else None

    def live_mb(self, session: str) -> int:
        out = self.run("eval", LIVE_MB, "-s", session)[1]
        return next((int(l.split("=")[1]) for l in out.splitlines() if l.startswith("live_mb=")), -1)

    # -- recording --

    def step(self, name: str, seconds: float, ok: bool, note: str = "") -> bool:
        self.steps.append({"group": self.group, "name": name, "seconds": round(seconds, 2), "ok": bool(ok), "note": note})
        flag = "" if ok else "   <-- UNEXPECTED"
        print(f"  {name:58s} {seconds:7.2f} s  {note}{flag}", flush=True)
        return ok

    def cmd(self, name: str, *argv, expect: str | None = None, rc: int | None = 0, env=None, note=None) -> str:
        """One client command as a step: its exit status and, if given, a string its output must contain."""
        code, out, err, dt = self.run(*argv, env=env)
        ok = (rc is None or code == rc) and (expect is None or expect in out + err)
        last = (out.strip().splitlines() or [""])[-1][:70]
        if argv and argv[0] == "reload":   # a reload prints GHC's log and the status file: the verdict is what matters
            last = next((l for l in out.splitlines() if l.startswith(("OK", "STALE", "CHECK-", "COMPILE-"))), last)[:70]
        self.step(name, dt, ok, note if note is not None else (last if ok else f"rc={code} {(out + err).strip()[-160:]!r}"))
        return out + err

    def save(self, name: str, session: str, path: str, text: str, pred, note_of=lambda j: j["verdict"][:60]) -> dict | None:
        """A SAVE as a step: write the file and time how long until the watcher's verdict."""
        since = self.at(session)
        self.write(path, text)
        j, dt = self.wait(session, pred, since)
        self.step(name, dt, j is not None, note_of(j) if j else f"timed out: {self.status(session).get('verdict', '?')[:90]}")
        return j

    def stop_all(self) -> None:
        for s in list(self.conf["targets"]) + list(self.conf["sessions"]):
            self.run("stop", s)

    # -- the groups --

    def g_boot(self):
        """Booting a session, cold and warm; the verdict; what a boot leaves in the state directory."""
        self.cmd("list (nothing running)", "list", expect="dev [composed]: stopped")
        out = self.cmd("start hello (COLD: cabal builds the dependencies)", "start", "hello", expect="CHECK-PASS")
        self.step("  prebuild ran before the boot", 0, os.path.exists(os.path.join(self.state, "prebuilt.txt")), "prebuilt.txt")
        self.step("  loaded_sources.tsv names the sources", 0,
                  "src/Hello.hs" in self.read(os.path.join(self.state, "hello", "loaded_sources.tsv")))
        self.cmd("  {session} placeholder reached the repl's env", "eval", 'System.Environment.getEnv "HELLO_SESSION"', expect='"hello"')
        self.cmd("status", "status", expect="hello: OK")
        self.cmd("status -d", "status", "-d", "hello", expect="loaded=")
        self.cmd("start again (already running)", "start", "hello", expect="already running")
        self.cmd("stop", "stop", "hello", expect="stopped")
        self.cmd("start hello (WARM: objects on disk)", "start", "hello", expect="CHECK-PASS")
        del out

    def g_eval(self):
        """Evaluating against the loaded code."""
        self.cmd("eval, first", "eval", "Hello.greeting", expect='"hello"')
        t0 = time.time()
        ok = all(self.run("eval", "length Hello.greeting")[1].strip() == "5" for _ in range(10))
        self.step("eval, 10 more (each)", (time.time() - t0) / 10, ok, "the client's start-up is most of it")
        self.cmd("eval, a multi-line expression", "eval", "let x = 20\n    y = 22\nin x + y", expect="42")
        self.cmd("eval, an error is an answer, not a hang", "eval", "Hello.nope", expect="ot in scope", rc=None)
        self.cmd("check", "check", expect="[PASS] table")
        self.cmd("mem", "mem", expect="repl ")
        self.cmd("log", "log", "daemon.log", "-n", "3", expect="[")

    def g_reload(self):
        """An explicit reload: with nothing changed, without the check, and what the status carries."""
        self.cmd("reload (nothing changed)", "reload", expect="CHECK-PASS")
        self.cmd("reload --no-check (a compile verdict, and it says so)", "reload", "--no-check", expect="CHECK SKIPPED")
        j = self.status("hello")
        self.step("  status.json: kind OK, members and counts as data", 0,
                  j["kind"] == "OK" and j["ok"] and j["stale"] == 0 and isinstance(j["members"], list), f"gen={j['generation']}")
        self.cmd("reload (the check runs again)", "reload", expect="CHECK-PASS")
        self.step("  status.json: kind CHECK-PASS, one entry per member", 0,
                  self.status("hello")["kind"] == "CHECK-PASS" and [m["member"] for m in self.status("hello")["members"]] == ["hello"])

    def g_watch(self):
        """Saving a file: the watcher reloads, checks, and reports -- timed from the save to the verdict."""
        good = lambda j: j["kind"] == "CHECK-PASS"  # noqa: E731
        self.save("save: a comment", "hello", self.hs, self.hs0 + "-- a comment\n", good)
        self.save("save: a real change", "hello", self.hs, self.hs0.replace('"hello"', '"bonjour"'), good)
        self.cmd("  the new code answers", "eval", "Hello.greeting", expect='"bonjour"')
        j = self.save("save: a compile error", "hello", self.hs, self.hs0 + "\ngreeting = oops\n",
                      lambda j: j["kind"] == "COMPILE-ERROR")
        self.step("  the error's location is in the status", 0, bool(j and any("Hello.hs" in d for d in j["detail"])),
                  (j["detail"][0][:60] if j and j["detail"] else ""))
        self.save("save: the fix", "hello", self.hs, self.hs0, good)
        self.cmd("stop", "stop", "hello", expect="stopped")

    def g_stale(self):
        """Without the watcher, a changed source is NOT loaded -- and every verdict and answer says so."""
        self.cmd("start manual (auto_reload off)", "start", "manual", expect="CHECK-PASS")
        self.write(self.ex, self.ex0.replace('"!"', '"?!"'))
        time.sleep(0.2)
        self.cmd("status after an edit: STALE(1)", "status", "manual", expect="STALE(1)")
        code, out, err, dt = self.run("eval", "Extra.shout", "-s", "manual")
        self.step("eval answers from the OLD code, and warns", dt, out.strip() == '"HELLO!"' and "STALE" in err, err.strip()[:60])
        self.cmd("reload clears it", "reload", "manual", expect="CHECK-PASS")
        self.cmd("  and the new code answers", "eval", "Extra.shout", "-s", "manual", expect='"HELLO?!"')
        self.step("  status is no longer stale", 0, "STALE" not in self.run("status", "manual")[1])
        self.write(self.ex, self.ex0)
        self.cmd("stop", "stop", "manual", expect="stopped")

    def g_compose(self):
        """A composed session: two packages in one repl, a check per member."""
        good = lambda j: j["kind"] == "CHECK-PASS"  # noqa: E731
        self.cmd("start dev (hello + extra in one repl)", "start", "dev", expect="[2 members: hello")
        self.cmd("eval reaches both members", "eval", "(Hello.greeting, Extra.shout)", expect='("hello","HELLO!")')
        self.cmd("check -m extra (one member's)", "check", "-m", "extra", expect="[PASS] shout")
        j = self.save("save: break extra's check", "dev", self.ex, self.ex0.replace("last shout == '!'", "last shout == '?'"),
                      lambda j: j["kind"] == "CHECK-FAIL")
        self.step("  the verdict names the member; the other passed", 0,
                  bool(j) and {m["member"]: m["kind"] for m in j["members"]} == {"hello": "PASS", "extra": "FAIL"},
                  j["verdict"][:60] if j else "")
        self.save("save: fix it", "dev", self.ex, self.ex0, good)
        self.save("save: edit hello -- extra, which imports it, follows", "dev", self.hs, self.hs0.replace('"hello"', '"hej"'), good)
        self.cmd("  the dependent member sees it", "eval", "Extra.shout", expect='"HEJ!"')
        self.save("save: back", "dev", self.hs, self.hs0, good)

    def g_servers(self):
        """A server forked from the repl: kept, re-forked with its state, protected, in the background."""
        good = lambda j: j["kind"] == "CHECK-PASS"  # noqa: E731
        if not self.status_ok("dev"):
            self.cmd("start dev", "start", "dev", expect="CHECK-PASS")
        self.cmd("server (loading is not serving)", "server", expect="hello: not running")
        self.cmd("server start (prefork 2 s, then the fork)", "server", "start", expect="hello: pid")
        pid = self.server_pid("dev")
        time.sleep(0.6)
        self.step("  it serves the loaded code", 0, self.served()[0] == "hello", " ".join(map(str, self.served())))
        j = self.save("save: a comment -- the server is KEPT", "dev", self.hs, self.hs0 + "-- c\n", good,
                      lambda j: j["verdict"].split("[servers:")[-1][:60])
        self.step("  same pid", 0, self.server_pid("dev") == pid and bool(j) and j["servers"][0]["action"] == "kept")
        ticks = self.served()[1]
        j = self.save("save: a real change -- RE-FORKED, state carried", "dev", self.hs, self.hs0.replace('"hello"', '"bonjour"'),
                      good, lambda j: j["verdict"].split("[servers:")[-1][:60])
        time.sleep(0.6)
        word, n = self.served()
        self.step("  new pid, new code, the tick count continued", 0,
                  self.server_pid("dev") != pid and word == "bonjour" and n > ticks, f"{word} {n} (was {ticks})")
        pid = self.server_pid("dev")
        j = self.save("save: a compile error -- the server keeps serving", "dev", self.hs, self.hs0 + "\ngreeting = oops\n",
                      lambda j: j["kind"] == "COMPILE-ERROR", lambda j: j["verdict"].split("[servers:")[-1][:60])
        self.step("  same pid, still answering", 0, self.server_pid("dev") == pid)
        broken = self.hs0.replace("serve :: IO ()", "serve :: Int -> IO ()").replace("serve = do", "serve _ = do")
        j = self.save("save: the action no longer typechecks -- NOT stopped", "dev", self.hs, broken, good,
                      lambda j: j["verdict"].split("[servers:")[-1][:60])
        self.step("  same pid", 0, self.server_pid("dev") == pid and bool(j) and j["servers"][0]["action"] == "broken")
        self.save("save: back to the original", "dev", self.hs, self.hs0, good, lambda j: j["verdict"].split("[servers:")[-1][:60])
        self.cmd("server restart (a cut-over: state carried)", "server", "restart", expect="+state")
        self.cmd("server stop", "server", "stop", expect="hello: stopped")
        ticks = self.served()[1]
        self.cmd("server start --resume (from the state it left)", "server", "start", "--resume", expect="+state")
        time.sleep(0.6)
        self.step("  the count went on", 0, self.served()[1] > ticks, f"{self.served()[1]} > {ticks}")
        self.cmd("server stop; server start (cold: no --resume)", "server", "stop", expect="stopped")
        self.cmd("  start", "server", "start", expect="hello: pid", note="")
        time.sleep(0.5)
        self.step("  the count started again", 0, 0 <= self.served()[1] < ticks, str(self.served()[1]))
        # membership: the repl restarts, the server does not
        pid = self.server_pid("dev")
        self.cmd("compose --remove extra (repl restarts, server ADOPTED)", "compose", "dev", "--remove", "extra", expect="adopted")
        self.step("  same pid", 0, self.server_pid("dev") == pid)
        self.cmd("  extra is gone from the repl", "eval", "Extra.shout", expect="Extra", rc=None, note="not in scope")
        self.cmd("compose --add extra", "compose", "dev", "--add", "extra", expect="[2 members")
        self.step("  same pid", 0, self.server_pid("dev") == pid)
        self.cmd("stop dev (its servers stop with it)", "stop", "dev", expect="stopped")
        time.sleep(0.6)
        a = self.served()
        time.sleep(0.5)
        self.step("  nothing is serving", 0, self.served() == a)
        # the background re-fork: the verdict is published with the 2 s prefork still to run
        self.cmd("start hello with GHS_ASYNC_REFORK=1", "start", "hello", expect="CHECK-PASS", env={"GHS_ASYNC_REFORK": "1"})
        self.cmd("  server start", "server", "start", "-s", "hello", expect="hello: pid", note="")
        pid = self.server_pid("hello")
        since, t0 = self.at("hello"), time.time()
        self.write(self.hs, self.hs0.replace('"hello"', '"hola"'))
        first = None
        while time.time() - t0 < 60 and first is None:
            try:
                j = self.status("hello")
                if j["at"] > since and j["kind"] == "CHECK-PASS":
                    first = (time.time() - t0, bool(j.get("servers_pending")))
            except (OSError, ValueError, KeyError):
                pass
            time.sleep(0.02)
        self.step("save: the verdict, with the re-fork still in the background", first[0] if first else 60,
                  bool(first and first[1]), "servers_pending: true")
        j, _ = self.wait("hello", lambda j: any(x["action"] == "re-forked" for x in j.get("servers", [])), since)
        self.step("  ... and the server is up on the new code", time.time() - t0, j is not None and self.server_pid("hello") != pid)
        self.save("save: back", "hello", self.hs, self.hs0, good)
        self.cmd("stop hello", "stop", "hello", expect="stopped")

    def status_ok(self, session: str) -> bool:
        return "OK" in self.run("status", session)[1]

    def g_census(self):
        """What is holding the memory: the C heap census, from inside the session."""
        self.cmd("start hello", "start", "hello", expect="CHECK-PASS")
        self.cmd("census: every CAF, by what it retains", "eval", "GHC.Hygiene.Census.cafReport 3 100000000", expect="by constructor")
        self.cmd("census: the Strings among it", "eval", "GHC.Hygiene.Census.cafStrings 3 100000000", expect="string groups")
        self.cmd("census: ONE value, alone", "eval", 'GHC.Hygiene.Census.censusOf "bigTable" Hello.bigTable', expect="bigTable:")
        self.cmd("bench: an action (wall, GC, allocation, live heap)", "eval", 'GHC.Hygiene.Census.benchOf "selfTest" Hello.selfTest', expect="[bench] selfTest")
        self.cmd("keep a value, then report the kept ones", "eval",
                 'GHC.Hygiene.Census.keep "greeting" (replicate 100000 Hello.greeting) >> GHC.Hygiene.Census.keptReport 100000000',
                 expect="greeting")
        self.cmd("memNow: the session as a whole", "eval", "GHC.Hygiene.Census.memNow", expect="CAFs retain")
        self.cmd("loaderStats: what the RTS linker holds", "eval", "GHC.Hygiene.loaderStats", rc=None, note="(to the session's stderr)")
        self.cmd("stop", "stop", "hello", expect="stopped")

    def g_leak(self):
        """The reason for the pruner: the same five reloads with it and without, live heap after each."""
        results = {}
        for name in ("leaky", "tidy"):
            self.cmd(f"start {name}", "start", name, expect="CHECK-PASS")
            series = [self.live_mb(name)]
            t0 = time.time()
            for i in range(5):
                self.write(self.hs, self.hs0 + f"-- reload {i} for {name}\n")
                self.run("reload", name)
                series.append(self.live_mb(name))
            results[name] = series
            grew = series[-1] - series[0]
            self.step(f"  {name}: 5 edit+reload+check, live MB after each", (time.time() - t0) / 5,
                      series[0] > 0, f"{' '.join(map(str, series))}  (+{grew} MB)")
            self.write(self.hs, self.hs0)
            self.cmd(f"stop {name}", "stop", name, expect="stopped")
        leak, tidy = results["leaky"][-1] - results["leaky"][0], results["tidy"][-1] - results["tidy"][0]
        self.step("the pruner holds the heap flat where plain GHCi grows", 0, leak > 20 and tidy < leak / 3,
                  f"without: +{leak} MB over 5 reloads; with: +{tidy} MB")

    def g_budget(self):
        """Past the memory budget a reload is a restart: GHCi never gives memory back."""
        self.cmd("start tidy with a 1 MB budget", "start", "tidy", expect="CHECK-PASS", env={"GHS_REPL_BUDGET_MB": "1"})
        self.cmd("mem (the budget is checked against the last reading)", "mem", "tidy", expect="budget 6144")
        self.cmd("reload: over budget, so the repl is RESTARTED", "reload", "tidy", expect="RESTARTED instead of reloaded")
        self.cmd("  and it still answers", "eval", "Hello.greeting", "-s", "tidy", expect='"hello"')
        self.cmd("stop", "stop", "tidy", expect="stopped")

    def g_hooks(self):
        """What a project hangs on a session: the status feed, a save that only compiles, a commit that checks."""
        n = len(self.pushes)
        kinds = {p.get("verdict", "").split(" ")[0] for p in self.pushes}
        self.step("status_url: every verdict was POSTed to the listener", 0, n > 10 and "reloading" in kinds,
                  f"{n} pushes so far, e.g. {sorted(k for k in kinds if k)[:5]}")
        self.cmd("start oncommit (watch_check off, reload_on_commit on)", "start", "oncommit", expect="CHECK-PASS")
        self.save("save: it only COMPILES (no check)", "oncommit", self.ex, self.ex0 + "-- c\n",
                  lambda j: j["kind"] == "OK" and "CHECK SKIPPED" in j["verdict"])
        since = self.at("oncommit")
        subprocess.run(["git", "-c", "user.name=tour", "-c", "user.email=tour@example.org", "commit", "-qam", "a commit"],
                       cwd=self.proj, capture_output=True)
        j, dt = self.wait("oncommit", lambda j: j["kind"] == "CHECK-PASS", since)
        self.step("git commit: a full reload, the check runs", dt, j is not None, j["verdict"][:50] if j else "timed out")
        self.write(self.ex, self.ex0)
        self.cmd("stop", "stop", "oncommit", expect="stopped")
        out = self.cmd("start broken: a check that has never passed", "start", "broken", expect="NEVER-PASSED", rc=1,
                       note="CHECK-FAIL ... [NEVER-PASSED]")
        self.cmd("stop", "stop", "broken", expect="stopped")
        del out

    def g_idle(self):
        """Giving memory back: a session that stops itself, and autostop for the rest."""
        self.cmd("start sleepy (idle_stop_mins = 3 s)", "start", "sleepy", expect="CHECK-PASS")
        t0 = time.time()
        while time.time() - t0 < 40 and "sleepy: stopped: idle for" not in self.run("status")[1]:
            time.sleep(0.2)
        self.step("sleepy stopped itself, and status says why", time.time() - t0, "sleepy: stopped: idle for" in self.run("status")[1],
                  "3 s unused, then the stop")
        self.cmd("start extra", "start", "extra", expect="CHECK-PASS")
        self.cmd("autostop -n --idle-mins 60: nothing is idle yet", "autostop", "-n", "--idle-mins", "60", expect="keeping extra")
        self.cmd("autostop --max-mem-mb 100000: within the limit", "autostop", "--idle-mins", "0", "--max-mem-mb", "100000",
                 expect="within the limit")
        self.cmd("autostop --idle-mins 0: stops what is idle", "autostop", "--idle-mins", "0", expect="stopping extra")
        time.sleep(1.5)
        self.cmd("  status says who stopped it", "status", expect="stopped by autostop")

    def g_gc(self):
        """What a killed daemon leaves behind, and reaping it."""
        self.cmd("start dev", "start", "dev", expect="CHECK-PASS")
        self.cmd("server start", "server", "start", expect="hello: pid")
        srv = self.server_pid("dev")
        os.kill(int(self.read(os.path.join(self.state, "dev", "pid"))), signal.SIGKILL)
        time.sleep(1.5)
        code, out, err, dt = self.run("status")
        self.step("kill -9 the daemon: status warns of a leftover", dt, "leftover" in err, err.strip()[:70])
        self.cmd("gc -n (look first)", "gc", "-n", expect=f"would reap server hello pid {srv}")
        self.cmd("gc", "gc", expect=f"reaping server hello pid {srv}")
        self.cmd("gc again: nothing left", "gc", expect="no orphaned")
        old = os.path.join(self.state, "manual", "status")
        if os.path.exists(old):
            os.utime(old, (time.time() - 30 * 86400,) * 2)
            self.cmd("gc --days 7 prunes a session's old state", "gc", "--days", "7", expect="pruned manual")

    GROUPS = ("boot", "eval", "reload", "watch", "stale", "compose", "servers", "census", "leak", "budget", "hooks", "idle", "gc")
    NEEDS_HELLO = ("eval", "reload", "watch")   # run inside the session `boot` leaves up

    def main(self, only: list[str]) -> int:
        t0 = time.time()
        try:
            for g in self.GROUPS:
                if only and g not in only:
                    continue
                self.group = g
                fn = getattr(self, "g_" + g)
                print(f"\n== {g}: {fn.__doc__.strip().splitlines()[0]}", flush=True)
                if g in self.NEEDS_HELLO and not self.status_ok("hello"):
                    self.cmd("start hello", "start", "hello", expect="CHECK-PASS")
                try:
                    fn()
                except Exception as e:  # noqa: BLE001
                    self.step(f"{g}: the group stopped", 0, False, f"{type(e).__name__}: {e}")
                    self.write(self.hs, self.hs0)
                    self.write(self.ex, self.ex0)
                    self.stop_all()
                if only and g in self.NEEDS_HELLO:
                    self.run("stop", "hello")
        finally:
            self.stop_all()
            self.run("gc")
            if self.keep:
                print(f"\nkept: {self.proj}")
            else:
                shutil.rmtree(self.dir, ignore_errors=True)
        bad = [s for s in self.steps if not s["ok"]]
        timed = [s for s in self.steps if s["seconds"] > 0]
        print(f"\n== {len(self.steps)} steps, {len(timed)} timed, {time.time() - t0:.0f} s in all: "
              f"{'all as expected' if not bad else str(len(bad)) + ' UNEXPECTED'}")
        for s in bad:
            print(f"   {s['group']}/{s['name'].strip()}: {s['note']}")
        return 1 if bad else 0


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--only", action="append", help="run only this group (repeatable)")
    ap.add_argument("--list", action="store_true", help="list the groups")
    ap.add_argument("--json", help="write the steps to this file")
    ap.add_argument("--compare", help="a file an earlier run wrote with --json: list the steps that got slower")
    ap.add_argument("--keep", action="store_true", help="keep the working copy")
    a = ap.parse_args()
    if a.list:
        for g in Tour.GROUPS:
            print(f"{g:9s} {getattr(Tour, 'g_' + g).__doc__.strip().splitlines()[0]}")
        return 0
    if not shutil.which("cabal"):
        print("the tour needs cabal and GHC on PATH", file=sys.stderr)
        return 2
    t = Tour(a.keep)
    rc = t.main(a.only or [])
    if a.compare:
        with open(a.compare) as fh:
            old = {(s["group"], s["name"]): s["seconds"] for s in json.load(fh)["steps"]}
        slower = [(s, old[(s["group"], s["name"])]) for s in t.steps
                  if (s["group"], s["name"]) in old and s["seconds"] > 1.5 * old[(s["group"], s["name"])] and s["seconds"] > old[(s["group"], s["name"])] + 0.5]
        print(f"== against {a.compare}: {len(slower)} step(s) more than 1.5x and 0.5 s slower")
        for s, o in slower:
            print(f"   {s['group']}/{s['name'].strip()}: {s['seconds']:.2f} s, was {o:.2f} s")
    if a.json:
        ghc = subprocess.run(["ghc", "--numeric-version"], capture_output=True, text=True).stdout.strip()
        with open(a.json, "w") as fh:
            json.dump({"at": time.strftime("%Y-%m-%dT%H:%M:%S"), "ghc": ghc, "platform": sys.platform, "steps": t.steps}, fh, indent=1)
    return rc


if __name__ == "__main__":
    sys.exit(main())
