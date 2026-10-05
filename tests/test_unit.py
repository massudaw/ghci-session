"""Pure-logic tests: no GHC needed. Run: python3 -m unittest discover -s tests"""
import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from ghci_session import config  # noqa: E402
from ghci_session.daemon import Session, scan, sock_path  # noqa: E402


def make(tmp, targets=None, **top):
    with open(os.path.join(tmp, "ghci-session.json"), "w") as fh:
        json.dump({"targets": targets or {"lib": {"cabal_args": "lib:x"}}, **top}, fh)
    return config.load(tmp)


class ConfigTests(unittest.TestCase):
    def test_defaults_and_default_target(self):
        with tempfile.TemporaryDirectory() as t:
            conf = make(t, {"a": {}, "b": {"hygiene": True}})
            self.assertEqual(conf["default"], "a")
            self.assertFalse(conf["targets"]["a"]["hygiene"])
            self.assertTrue(conf["targets"]["b"]["hygiene"])

    def test_top_level_keys_are_shared_but_overridable(self):
        with tempfile.TemporaryDirectory() as t:
            conf = make(t, {"a": {}, "b": {"rts_flags": "none"}}, rts_flags="-c -A64m", default="b")
            self.assertEqual(conf["targets"]["a"]["rts_flags"], "-c -A64m")
            self.assertEqual(conf["targets"]["b"]["rts_flags"], "none")
            self.assertEqual(conf["default"], "b")

    def test_unknown_key_is_refused(self):
        with tempfile.TemporaryDirectory() as t:
            with self.assertRaises(config.ConfigError):
                make(t, {"a": {"chek": {}}})

    def test_find_root_walks_up(self):
        with tempfile.TemporaryDirectory() as t:
            make(t)
            sub = os.path.join(t, "a", "b")
            os.makedirs(sub)
            self.assertEqual(config.find_root(sub), os.path.abspath(t))


class VerdictTests(unittest.TestCase):
    def session(self, t):
        return Session(make(t), "lib")

    def test_verdicts(self):
        with tempfile.TemporaryDirectory() as t:
            s = self.session(t)
            self.assertEqual(s.verdict_of("[1 of 1] Compiling M\nOk, one module loaded.")[0], "OK")
            self.assertEqual(s.verdict_of("Ok, 12 modules reloaded.")[0], "OK")
            v, d = s.verdict_of("src/M.hs:3:1: error: [GHC-1]\n  oops\nFailed, no modules loaded.")
            self.assertEqual(v, "COMPILE-ERROR: 1 error(s)")
            self.assertEqual(len(d), 1)
            # a link failure has no source location but is still an error
            self.assertTrue(s.verdict_of("<no location info>: error:\n  symbol not found")[0].startswith("COMPILE-ERROR"))
            # no verdict at all must not read as success
            self.assertTrue(s.verdict_of("???")[0].startswith("COMPILE-ERROR"))

    def test_stale_detection_and_status_files(self):
        with tempfile.TemporaryDirectory() as t:
            os.makedirs(os.path.join(t, "src"))
            src = os.path.join(t, "src", "M.hs")
            open(src, "w").write("module M where\n")
            s = self.session(t)
            s.pending_sig = scan(t, s.cfg["watch"], s.exts)
            s.loaded_sig = dict(s.pending_sig)
            self.assertEqual(s.stale_files(), [])
            os.utime(src, (1, 1))
            self.assertEqual(s.stale_files(), [src])
            s.set_status("OK -- CHECK-PASS")
            first = open(os.path.join(s.dir, "status")).readline()
            self.assertTrue(first.startswith("STALE(1) OK"), first)
            j = json.load(open(os.path.join(s.dir, "status.json")))
            self.assertTrue(j["ok"])
            self.assertEqual(j["stale"], [src])

    def test_root_cabal_files_are_watched(self):
        with tempfile.TemporaryDirectory() as t:
            open(os.path.join(t, "x.cabal"), "w").write("")
            open(os.path.join(t, "cabal.project"), "w").write("")
            s = self.session(t)
            self.assertIn("x.cabal", s.cfg["watch"])
            self.assertIn("cabal.project", s.cfg["watch"])

    def test_socket_path_is_short_and_stable(self):
        p = sock_path("/a/very/" + "long/" * 40 + "state")
        self.assertLess(len(p), 100)
        self.assertEqual(p, sock_path("/a/very/" + "long/" * 40 + "state"))


if __name__ == "__main__":
    unittest.main()
