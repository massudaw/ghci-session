"""End to end against examples/hello: a composed session of two packages, per-member checks, a forked server
kept / re-forked with its state / protected from a broken action, a compile error and recovery, adoption across
a member change, prune and census. Needs cabal and GHC (and macOS for the pruner); run with GHS_E2E=1. ~2 min."""
import json
import os
import shutil
import subprocess
import sys
import tempfile
import time
import unittest

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CLI = os.path.join(HERE, "bin", "ghci-session")


@unittest.skipUnless(os.environ.get("GHS_E2E") == "1" and shutil.which("cabal"), "set GHS_E2E=1 (needs cabal)")
class EndToEnd(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.dir = tempfile.mkdtemp(prefix="ghs-e2e-")
        cls.proj = os.path.join(cls.dir, "examples", "hello")
        shutil.copytree(os.path.join(HERE, "examples", "hello"), cls.proj,
                        ignore=shutil.ignore_patterns(".ghci-session", "dist-newstyle"))
        for d in ("hygiene", "bin", "app", "cbits"):
            shutil.copytree(os.path.join(HERE, d), os.path.join(cls.dir, d), ignore=shutil.ignore_patterns("dist-newstyle", ".obj", "clib"))
        for f in ("ghci-session.cabal", "README.md"):
            shutil.copy(os.path.join(HERE, f), os.path.join(cls.dir, f))
        cls.cli = os.environ.get("GHCI_SESSION_BIN") or os.path.join(HERE, "bin", "ghci-session")

    @classmethod
    def tearDownClass(cls):
        run(cls.cli, "stop", "dev", cwd=cls.proj)
        shutil.rmtree(cls.dir, ignore_errors=True)

    def status(self, session="dev"):
        with open(os.path.join(self.proj, ".ghci-session", session, "status.json")) as fh:
            return json.load(fh)

    def wait_for(self, pred, secs=90):
        t0 = time.time()
        while time.time() - t0 < secs:
            try:
                if pred(self.status()):
                    return self.status()
            except (OSError, ValueError):
                pass
            time.sleep(0.5)
        self.fail(f"timed out; status={self.status()}")

    def edit(self, path, text):
        """Write a source and wait for the verdict the watcher's reload produces."""
        t = self.status()["at"]
        with open(path, "w") as fh:
            fh.write(text)
        return lambda pred: self.wait_for(lambda j: j["at"] > t and pred(j))

    def cli_(self, *argv):
        return run(self.cli, *argv, cwd=self.proj)

    def served(self):
        with open(os.path.join(self.proj, ".ghci-session", "hello.out")) as fh:
            word, n = fh.read().split()
        return word, int(n)

    def server_pid(self):
        out = self.cli_("server").stdout
        return int(out.split("pid ")[1].split()[0]) if "running pid" in out else None

    def test_lifecycle(self):
        hs = os.path.join(self.proj, "src", "Hello.hs")
        with open(hs) as fh:
            orig = fh.read()
        good = lambda j: j["ok"] and "CHECK-PASS" in j["verdict"]  # noqa: E731

        # a composed session: two packages in one repl, a check per member
        r = self.cli_("start", "dev")
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertIn("CHECK-PASS", r.stdout)
        self.assertEqual([m["member"] for m in self.status()["members"]], ["hello", "extra"])
        self.assertEqual(self.cli_("eval", "Hello.greeting").stdout.strip(), '"hello"')
        self.assertEqual(self.cli_("eval", "Extra.shout").stdout.strip(), '"HELLO!"')

        # loading is not serving; starting is an action
        self.assertIsNone(self.server_pid())
        self.assertEqual(self.cli_("server", "start").returncode, 0)
        pid = self.server_pid()
        self.assertIsNotNone(pid)
        time.sleep(1.0)
        self.assertEqual(self.served()[0], "hello")

        # an edit that does not change the object code keeps the server
        j = self.edit(hs, orig + "-- a comment\n")(good)
        self.assertEqual([(s["member"], s["action"]) for s in j["servers"]], [("hello", "kept")])
        self.assertEqual(self.server_pid(), pid)

        # a real edit reloads both members, re-forks the server onto the new code, and carries its state
        ticks = self.served()[1]
        j = self.edit(hs, orig.replace('"hello"', '"bonjour"'))(good)
        self.assertEqual(j["servers"][0]["action"], "re-forked")
        self.assertNotEqual(self.server_pid(), pid)
        pid = self.server_pid()
        time.sleep(1.0)
        word, n = self.served()
        self.assertEqual(word, "bonjour")
        self.assertGreater(n, ticks)                      # the tick count was handed over, not reset
        self.assertEqual(self.cli_("eval", "Extra.shout").stdout.strip(), '"BONJOUR!"')   # the dependent member too

        # a compile error is a verdict, and the server keeps running the old code
        self.edit(hs, orig + "\ngreeting = oops\n")(lambda j: j["verdict"].startswith("COMPILE-ERROR"))
        self.assertEqual(self.server_pid(), pid)

        # an action that no longer typechecks does not cost the running server
        j = self.edit(hs, orig.replace("serve :: IO ()", "serve :: Int -> IO ()").replace("serve = do", "serve _ = do"))(good)
        self.assertEqual(j["servers"][0]["action"], "broken")
        self.assertEqual(self.server_pid(), pid)

        # back to the original: the server follows
        self.edit(hs, orig)(good)
        time.sleep(1.0)
        self.assertEqual(self.served()[0], "hello")
        pid = self.server_pid()

        # one member's check failing names that member, and the other still passes
        ex = os.path.join(self.proj, "extra", "src", "Extra.hs")
        with open(ex) as fh:
            eorig = fh.read()
        j = self.edit(ex, eorig.replace("last shout == '!'", "last shout == '?'"))(lambda j: j["kind"] == "CHECK-FAIL")
        self.assertIn("extra", j["verdict"])
        self.assertEqual({m["member"]: m["kind"] for m in j["members"]}, {"hello": "PASS", "extra": "FAIL"})
        self.assertEqual(self.server_pid(), pid)          # extra's edit is not the server's code
        self.edit(ex, eorig)(good)

        # dropping a member restarts the repl but ADOPTS the running server
        self.assertEqual(self.cli_("compose", "dev", "--remove", "extra").returncode, 0)
        self.assertEqual(self.server_pid(), pid)
        self.assertEqual([m["member"] for m in self.status()["members"]], ["hello"])
        self.assertIn("Extra", self.cli_("eval", "Extra.shout").stdout)        # ... and Extra is gone from scope

        # hygiene ran, and the census answers
        with open(os.path.join(self.proj, ".ghci-session", "dev", "daemon.log")) as fh:
            self.assertIn("unlink_cafs:", fh.read())
        self.assertIn("closures", self.cli_("eval", "GHC.Hygiene.Census.cafReport 2 100000000").stdout)

        # stopping the session stops its server
        self.cli_("stop", "dev")
        time.sleep(1.0)
        a = self.served()
        time.sleep(1.0)
        self.assertEqual(self.served(), a)

        # a background re-fork: the reload returns at its verdict, with the (3 s) prefork still to run
        # (GHS_ASYNC_REFORK makes it what the watcher does too: an explicit `reload --async-refork` after a
        # save would race the watcher, which sees the save within milliseconds)
        r = subprocess.run([self.cli, "start", "dev"], cwd=self.proj, capture_output=True, text=True,
                           env={**os.environ, "GHS_ASYNC_REFORK": "1"})
        self.assertEqual(r.returncode, 0)
        self.assertEqual(self.cli_("server", "start").returncode, 0)
        old = self.server_pid()
        t0 = time.time()
        since = self.status()["at"]
        with open(hs, "w") as fh:
            fh.write(orig.replace('"hello"', '"hola"'))
        saw_pending = False
        while time.time() - t0 < 60 and not saw_pending:
            j = self.status()
            saw_pending = j["at"] > since and bool(j.get("servers_pending"))
            if j["at"] > since and j.get("servers") and not j.get("servers_pending"):
                break
            time.sleep(0.02)
        self.assertTrue(saw_pending, "the verdict was not published before the re-fork finished")
        j = self.wait_for(lambda j: j.get("servers_pending") is False and j.get("servers"))
        self.assertEqual(j["servers"][0]["action"], "re-forked")
        self.assertNotEqual(self.server_pid(), old)
        time.sleep(1.0)
        self.assertEqual(self.served()[0], "hola")
        with open(hs, "w") as fh:
            fh.write(orig)
        self.wait_for(lambda j: j["at"] > t0 and good(j) and j.get("servers_pending") is not True and not j["stale"])

        # gc: a daemon killed outright leaves its (detached) server running; gc finds and reaps it
        srv = self.server_pid()
        with open(os.path.join(self.proj, ".ghci-session", "dev", "pid")) as fh:
            os.kill(int(fh.read()), 9)
        time.sleep(1.5)
        self.assertIn("leftover", self.cli_("status").stderr)
        self.assertIn(f"would reap server hello pid {srv}", self.cli_("gc", "-n").stdout)
        self.assertIn(f"reaping server hello pid {srv}", self.cli_("gc").stdout)
        self.assertIn("no orphaned", self.cli_("gc").stdout)
        a = self.served()
        time.sleep(1.0)
        self.assertEqual(self.served(), a)

        # idle: `autostop` stops what nobody is using, and a session with `idle_stop_mins` stops itself
        self.assertEqual(self.cli_("start", "dev").returncode, 0)
        out = self.cli_("autostop", "--idle-mins", "60").stdout
        self.assertIn("keeping dev", out)                       # just started: not idle
        self.assertIsNotNone(self.status())
        out = self.cli_("autostop", "--idle-mins", "0", "-n").stdout
        self.assertIn("would stop dev", out)
        self.assertIn("stopping dev", self.cli_("autostop", "--idle-mins", "0").stdout)
        time.sleep(2.0)
        self.assertIn("stopped by autostop", self.cli_("status").stdout)
        cfgp = os.path.join(self.proj, "ghci-session.json")
        with open(cfgp) as fh:
            c = json.load(fh)
        c["targets"]["extra"]["idle_stop_mins"] = 0.05           # 3 s
        with open(cfgp, "w") as fh:
            json.dump(c, fh)
        self.assertEqual(self.cli_("start", "extra").returncode, 0)
        self.assertEqual(self.cli_("eval", "Extra.shout", "-s", "extra").stdout.strip(), '"HELLO!"')
        deadline = time.time() + 30
        while time.time() < deadline and "idle for" not in self.cli_("status").stdout:
            time.sleep(0.5)
        self.assertIn("extra: stopped: idle for", self.cli_("status").stdout)


@unittest.skipUnless(os.environ.get("GHS_E2E") == "1" and shutil.which("cabal") and sys.platform == "darwin",
                     "set GHS_E2E=1 (needs cabal, and the pruner: macOS)")
class YoungCafRepro(unittest.TestCase):
    """hygiene/repro: unlinking a superseded CAF whose value is young kills GHCi; the pruner's check prevents it."""

    def go(self, *args):
        script = os.path.join(HERE, "hygiene", "repro", "run.sh")
        return subprocess.run([script, *args], capture_output=True, text=True, timeout=600).stdout

    def test_guarded_survives_and_unguarded_dies(self):
        out = self.go()
        self.assertIn("old f, after the major GC: 500501", out, out)
        self.assertIn("(exit 0)", out)
        bad = self.go("unsafe")
        # the value was freed under the CAF: reading it kills the process, or -- while the freed memory has
        # not been used again -- answers with something else (1, seen one run in four). Either is the repro;
        # the RIGHT answer would mean it is no longer one.
        self.assertNotIn("old f, after the major GC: 500501", bad, "the unguarded pruner still answers right: is the repro still a repro?")


@unittest.skipUnless(os.environ.get("GHS_E2E") == "1" and shutil.which("cabal"), "set GHS_E2E=1 (needs cabal)")
class KeepLinked(unittest.TestCase):
    """A reload relinks what changed and what depends on it, and nothing else: an untouched module keeps its
    code -- and so the values its CAFs hold -- while a module whose dependency changed runs the new code."""

    FILES = {
        "kl.cabal": "cabal-version: 2.4\nname: kl\nversion: 0.1\nlibrary\n  hs-source-dirs: src\n"
                    "  exposed-modules: A, B, C\n  build-depends: base\n  default-language: Haskell2010\n",
        "cabal.project": "packages: .\n",
        "src/A.hs": "module A (f) where\nf :: Int\nf = 1\n",
        # B is NOT recompiled when A's body changes (its interface is the same), but its code calls A's
        "src/B.hs": "module B (g) where\nimport A\ng :: Int\ng = f + 1\n",
        # C depends on neither: a CAF that says when it was computed
        "src/C.hs": "module C (stamp) where\nimport Data.IORef\nimport System.IO.Unsafe\n"
                    "{-# NOINLINE counter #-}\ncounter :: IORef Int\ncounter = unsafePerformIO (newIORef 0)\n"
                    "stamp :: IO Int\nstamp = atomicModifyIORef' counter (\\n -> (n + 1, n + 1))\n",
        "ghci-session.json": json.dumps({"targets": {"kl": {"units": ["lib:kl"], "watch": ["src"], "modules": ["A", "B", "C"],
                                                            "hygiene": True}}}),
    }

    def test_untouched_module_keeps_its_state(self):
        d = tempfile.mkdtemp(prefix="ghs-kl-")
        cli = os.environ.get("GHCI_SESSION_BIN") or CLI
        try:
            for name, text in self.FILES.items():
                os.makedirs(os.path.dirname(os.path.join(d, name)), exist_ok=True)
                with open(os.path.join(d, name), "w") as fh:
                    fh.write(text)
            self.assertIn("OK", run(cli, "start", "kl", cwd=d).stdout)
            ev = lambda e: run(cli, "eval", "-s", "kl", e, cwd=d).stdout.strip()  # noqa: E731
            self.assertEqual(ev("B.g"), "2")
            self.assertEqual(ev("C.stamp"), "1")
            self.assertEqual(ev("C.stamp"), "2")
            with open(os.path.join(d, "src", "A.hs"), "w") as fh:
                fh.write("module A (f) where\nf :: Int\nf = 10\n")
            self.assertIn("OK", run(cli, "reload", "kl", cwd=d).stdout)
            self.assertEqual(ev("B.g"), "11", "B was not recompiled, but must run A's new code")
            self.assertEqual(ev("C.stamp"), "3", "C did not change: its counter must not have been reset by the reload")
            with open(os.path.join(d, ".ghci-session", "kl", "daemon.log")) as fh:
                self.assertIn("1 module(s) stay linked, 2 to link again (A B)", fh.read())
            # a NEW module, listed in the .cabal: the build tool is asked what changed, and as it is only a
            # module the session is not restarted -- C's counter goes on counting
            with open(os.path.join(d, "src", "E.hs"), "w") as fh:
                fh.write("module E (e) where\ne :: Int\ne = 7\n")
            with open(os.path.join(d, "kl.cabal"), "w") as fh:
                fh.write(self.FILES["kl.cabal"].replace("A, B, C", "A, B, C, E"))
            log = os.path.join(d, ".ghci-session", "kl", "daemon.log")
            t0 = time.time()
            while "the session stays up" not in open(log).read() and time.time() - t0 < 60:
                time.sleep(0.2)
            self.assertIn("differs only by 1 module(s) added (E): the session stays up", open(log).read())
            t0 = time.time()
            while ev("E.e") != "7" and time.time() - t0 < 30:
                time.sleep(0.3)
            self.assertEqual(ev("E.e"), "7")
            self.assertEqual(ev("C.stamp"), "4", "adding a module must not have restarted the session")
        finally:
            run(cli, "stop", "kl", cwd=d)
            shutil.rmtree(d, ignore_errors=True)


def run(*argv, cwd):
    return subprocess.run(list(argv), cwd=cwd, capture_output=True, text=True, timeout=900)


if __name__ == "__main__":
    unittest.main()
