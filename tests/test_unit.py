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
            self.assertEqual(config.resolve(conf, "a")["rts_flags"], "-c -A64m")
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


class ComposeTests(unittest.TestCase):
    TARGETS = {
        "a": {"units": "lib:a", "watch": ["a/src"], "modules": ["A"], "env": {"A_PORT": 1},
              "check": {"expr": "A.t"}, "server": {"action": "A.serve", "port": 1, "env": {"X": "child"}}},
        "b": {"units": ["lib:b"], "watch": ["b/src"], "modules": ["B", "A"], "hygiene": True, "load_timeout": 2000,
              "checks": [{"expr": "B.t"}, {"expr": "B.u", "name": "slow", "fail": "BAD"}]},
    }

    def test_plain_session_is_its_target(self):
        with tempfile.TemporaryDirectory() as t:
            cfg = config.resolve(make(t, self.TARGETS), "a")
            self.assertEqual(cfg["units"], ["lib:a"])
            self.assertEqual([c["member"] for c in cfg["checks"]], ["a"])
            self.assertFalse(cfg["composed"])
            self.assertEqual(cfg["servers"][0]["env"], {"A_PORT": 1, "X": "child"})

    def test_composed_unions_and_keeps_checks_per_member(self):
        with tempfile.TemporaryDirectory() as t:
            conf = make(t, self.TARGETS, sessions={"dev": ["a", "b"]})
            cfg = config.resolve(conf, "dev")
            self.assertEqual(cfg["units"], ["lib:a", "lib:b"])
            self.assertEqual(cfg["modules"], ["A", "B"])
            self.assertEqual(cfg["watch"], ["a/src", "b/src"])
            self.assertEqual([c["member"] for c in cfg["checks"]], ["a", "b", "b:slow"])
            self.assertEqual(cfg["checks"][2]["fail"], "BAD")
            self.assertTrue(cfg["hygiene"])
            self.assertEqual(cfg["load_timeout"], 2000)
            self.assertEqual([s["member"] for s in cfg["servers"]], ["a"])
            self.assertEqual(cfg["servers"][0]["units"], ["lib:a"])

    def test_members_are_remembered_and_override_the_config(self):
        with tempfile.TemporaryDirectory() as t:
            conf = make(t, self.TARGETS, sessions={"dev": ["a", "b"]})
            self.assertEqual(config.read_members(conf, "dev"), ["a", "b"])
            config.write_members(conf, "dev", ["b"])
            self.assertEqual(config.resolve(conf, "dev")["members"], ["b"])
            self.assertEqual(config.resolve(conf, "dev")["servers"], [])

    def test_bad_sessions_are_refused(self):
        with tempfile.TemporaryDirectory() as t:
            with self.assertRaises(config.ConfigError):
                make(t, self.TARGETS, sessions={"dev": ["nope"]})
            with self.assertRaises(config.ConfigError):
                make(t, self.TARGETS, sessions={"a": ["b"]})
            with self.assertRaises(config.ConfigError):
                config.resolve(make(t, self.TARGETS), "dev")

    def test_repl_command(self):
        with tempfile.TemporaryDirectory() as t:
            conf = make(t, {**self.TARGETS, "plain": {"units": "lib:p", "rts_flags": "none"}}, sessions={"dev": ["a", "b"]})
            cmd = Session(conf, "dev").repl_command()
            self.assertIn("--enable-multi-repl", cmd)
            self.assertTrue(cmd.endswith("lib:a lib:b"), cmd)
            self.assertIn("-odir=.ghci-session/dev/obj", cmd)
            plain = Session(conf, "plain").repl_command()
            self.assertEqual(plain, "cabal repl --repl-options=-fdiagnostics-color=never lib:p")


class GcTests(unittest.TestCase):
    def test_find_attributes_by_absolute_path(self):
        from ghci_session import gc
        with tempfile.TemporaryDirectory() as t:
            conf = make(t, {"a": {"units": "lib:a"}})
            root = os.path.realpath(t)
            sd = os.path.join(t, ".ghci-session", "a")
            os.makedirs(sd)
            open(os.path.join(sd, "status"), "w").write("stopped\n")
            me = os.getpid()
            open(os.path.join(sd, "server-a.pid"), "w").write(str(me))   # a live "server" whose session is down
            procs = [
                (100, 1, f"python -m ghci_session --root {root} _daemon a"),          # ours, untracked: orphan
                (101, 100, "cabal repl lib:a"),
                (102, 101, f"ghc --interactive -outputdir {root}/dist-newstyle/x"),   # under the orphan: not listed twice
                (200, 1, "python -m ghci_session --root /some/other/checkout _daemon a"),   # a sibling checkout's
                (300, 1, f"/bin/ghc-9.14 --interactive @{root}/dist-newstyle/multi-out-3/a-0.1-inplace"),  # stray build
                (301, 1, "/bin/ghc-9.14 --interactive @/some/other/checkout/dist-newstyle/multi-out-1/a"),
                (302, 1, f"vim {root}/dist-newstyle/notes"),                          # names the path, is not a build
            ]
            got = gc.find(conf, procs)
            self.assertEqual(got["daemons"], [("a", 100)])
            self.assertEqual(got["servers"], [("a", "a", me)])
            self.assertEqual([p for p, _ in got["builds"]], [300])
            self.assertEqual(gc.describe_build(procs[4][2]), "a")
            self.assertEqual(sorted(gc.descendants(procs, 100)), [101, 102])
            # dry run reports and touches nothing
            lines = []
            self.assertEqual(gc.run(conf, dry_run=True, out=lines.append), 1 + len(gc.find(conf)["builds"]))
            self.assertTrue(os.path.exists(os.path.join(sd, "server-a.pid")))

    def test_prune_only_idle_stopped_sessions(self):
        from ghci_session import gc
        with tempfile.TemporaryDirectory() as t:
            conf = make(t, {"a": {}, "b": {}})
            for n, age in (("a", 10), ("b", 1)):
                d = os.path.join(t, ".ghci-session", n)
                os.makedirs(d)
                p = os.path.join(d, "status")
                open(p, "w").write("stopped\n")
                when = __import__("time").time() - age * 86400
                os.utime(p, (when, when))
            gc.run(conf, days=7, out=lambda _s: None)
            self.assertFalse(os.path.exists(os.path.join(t, ".ghci-session", "a")))
            self.assertTrue(os.path.exists(os.path.join(t, ".ghci-session", "b")))


class IdleTests(unittest.TestCase):
    def info(self, name, idle_min, mb, busy=False, serving=()):
        return {"session": name, "idle_s": idle_min * 60, "busy": busy, "repl_mb": mb, "servers_mb": 0, "serving": list(serving)}

    def test_plan_stops_longest_idle_until_under_the_limit(self):
        from ghci_session.cli import autostop_plan
        infos = [self.info("a", 40, 1000), self.info("b", 90, 2000), self.info("c", 5, 500),
                 self.info("d", 300, 800, busy=True), self.info("e", 200, 700, serving=["e"])]
        total, stop, spared = autostop_plan(infos, 3500, 30, False)
        self.assertEqual(total, 5000)
        self.assertEqual([i["session"] for i in stop], ["b"])            # 5000 -> 3000: enough
        why = {i["session"]: w for i, w in spared}
        self.assertEqual(why["d"], "busy")
        self.assertIn("serving", why["e"])
        self.assertIn("min ago", why["c"])
        self.assertIn("within the limit", why["a"])
        # no limit: every eligible one
        self.assertEqual([i["session"] for i in autostop_plan(infos, 0, 30, False)[1]], ["b", "a"])
        self.assertEqual([i["session"] for i in autostop_plan(infos, 0, 30, True)[1]], ["e", "b", "a"])

    def test_a_session_idles_out_only_when_unused_and_not_serving(self):
        import time
        with tempfile.TemporaryDirectory() as t:
            conf = make(t, {"a": {"idle_stop_mins": 1, "server": {"action": "A.s"}}, "b": {}})
            s = Session(conf, "a")
            self.assertFalse(s.idle_stop_due())
            s.last_used = time.time() - 120
            self.assertTrue(s.idle_stop_due())
            s.busy = 1
            self.assertFalse(s.idle_stop_due())
            s.busy = 0
            open(os.path.join(s.dir, "server-a.pid"), "w").write(str(os.getpid()))   # "serving"
            self.assertFalse(s.idle_stop_due())
            off = Session(conf, "b")
            off.last_used = 0
            self.assertFalse(off.idle_stop_due())

    def test_composed_idle_rule_needs_every_member(self):
        with tempfile.TemporaryDirectory() as t:
            ts = {"a": {"idle_stop_mins": 10}, "b": {"idle_stop_mins": 30}, "c": {}}
            conf = make(t, ts, sessions={"ab": ["a", "b"], "ac": ["a", "c"]})
            self.assertEqual(config.resolve(conf, "ab")["idle_stop_mins"], 30)
            self.assertEqual(config.resolve(conf, "ac")["idle_stop_mins"], 0)


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

    def test_port_listener_finds_this_process(self):
        import socket
        from ghci_session.daemon import pid_alive, port_listener
        srv = socket.socket()
        srv.bind(("127.0.0.1", 0))
        srv.listen(1)
        try:
            got = port_listener(srv.getsockname()[1])
            self.assertIn(got, (os.getpid(), None))   # None only where lsof is not installed
        finally:
            srv.close()
        self.assertTrue(pid_alive(os.getpid()))
        self.assertFalse(pid_alive(2 ** 22 + 12345))

    def test_socket_path_is_short_and_stable(self):
        p = sock_path("/a/very/" + "long/" * 40 + "state")
        self.assertLess(len(p), 100)
        self.assertEqual(p, sock_path("/a/very/" + "long/" * 40 + "state"))


if __name__ == "__main__":
    unittest.main()
