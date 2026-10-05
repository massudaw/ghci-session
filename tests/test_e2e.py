"""End to end against examples/hello: boot, eval, edit-triggered reload, compile error and recovery, prune,
census. Needs cabal and GHC (and macOS for the pruner); run with GHS_E2E=1. ~1.5 min."""
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
        shutil.copytree(os.path.join(HERE, "hygiene"), os.path.join(cls.dir, "hygiene"),
                        ignore=shutil.ignore_patterns("dist-newstyle"))
        # keep the example's `../../hygiene` pointing at the copy; the daemon needs the package's bin/ and hygiene/
        shutil.copytree(os.path.join(HERE, "bin"), os.path.join(cls.dir, "bin"))
        shutil.copytree(os.path.join(HERE, "ghci_session"), os.path.join(cls.dir, "ghci_session"))
        cls.cli = os.path.join(cls.dir, "bin", "ghci-session")

    @classmethod
    def tearDownClass(cls):
        run(cls.cli, "stop", cwd=cls.proj)
        shutil.rmtree(cls.dir, ignore_errors=True)

    def status(self):
        with open(os.path.join(self.proj, ".ghci-session", "lib", "status.json")) as fh:
            return json.load(fh)

    def wait_for(self, pred, secs=90):
        t0 = time.time()
        while time.time() - t0 < secs:
            try:
                if pred(self.status()):
                    return
            except (OSError, ValueError):
                pass
            time.sleep(0.5)
        self.fail(f"timed out; status={self.status()}")

    def test_lifecycle(self):
        r = run(self.cli, "start", cwd=self.proj)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertIn("CHECK-PASS", r.stdout)
        self.assertEqual(run(self.cli, "eval", "Hello.greeting", cwd=self.proj).stdout.strip(), '"hello"')
        hs = os.path.join(self.proj, "src", "Hello.hs")
        orig = open(hs).read()
        # an edit is picked up by the watcher
        t = self.status()["at"]
        open(hs, "w").write(orig.replace('"hello"', '"bonjour"'))
        self.wait_for(lambda j: j["at"] > t and j["ok"] and "CHECK-PASS" in j["verdict"])
        self.assertEqual(run(self.cli, "eval", "Hello.greeting", cwd=self.proj).stdout.strip(), '"bonjour"')
        # a compile error is a verdict, not a hang
        t = self.status()["at"]
        open(hs, "w").write(orig + "\ngreeting = oops\n")
        self.wait_for(lambda j: j["verdict"].startswith("COMPILE-ERROR"))
        # and the next good edit recovers
        t = self.status()["at"]
        open(hs, "w").write(orig)
        self.wait_for(lambda j: j["at"] > t and j["ok"] and "CHECK-PASS" in j["verdict"])
        # the pruner ran after reloads
        log = open(os.path.join(self.proj, ".ghci-session", "lib", "daemon.log")).read()
        self.assertIn("prune_cafs:", log)
        out = run(self.cli, "eval", "GHC.Hygiene.Census.cafReport 2 100000000", cwd=self.proj).stdout
        self.assertIn("closures", out)


def run(*argv, cwd):
    return subprocess.run(list(argv), cwd=cwd, capture_output=True, text=True, timeout=900)


if __name__ == "__main__":
    unittest.main()
