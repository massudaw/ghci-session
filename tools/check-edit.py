#!/usr/bin/env python3
"""What an agent does with a session, live: an edit and what is asked straight after it, a bench at another level.
tools/fake-claude.py stands for the `claude` command, so the tools are the chat's own, through its tool server.

    tools/check-edit.py [-v] [-n ROUNDS]

In a project made for it (a module of a few thousand generated definitions, so that a reload is not instant):

  edit, then eval  a result is changed by an `edit` and asked for by an `eval` the moment the edit answers, ROUNDS
                   times, alone and then with another client typechecking all the while (a second agent): the eval
                   must always answer from the new code, never the code before the edit.
  a slow sibling   `bench` at -O1, a timeout of one second: the sibling session is started and compiles on; the
                   answer says so, and that nothing was measured. The same asked again gets its number.
  a sibling that   `bench` with a unit that does not exist: the answer says the session did not start, and why.
  cannot boot

Exit status 0 when all hold. A minute or two.
"""
import json, os, re, shutil, subprocess, sys, tempfile, threading, time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tuicheck

DEFS = 3000


def big(val):
    """src/Big.hs: DEFS definitions, and `val`, the result that is changed."""
    lines = ["module Big (val, total) where", "", "val :: Int", "val = %d" % val, ""]
    for i in range(DEFS):
        lines += ["f%d :: Int -> Int" % i, "f%d x = x * %d + (x `mod` %d) + length (show (x + %d))" % (i, i + 3, i % 7 + 2, i), ""]
    lines += ["total :: Int -> Int", "total x = " + " + ".join("f%d x" % i for i in range(0, DEFS, 50))]
    return "\n".join(lines) + "\n"


CABAL = "cabal-version: 2.4\nname:          demo\nversion:       0.1\nlibrary\n  hs-source-dirs:   src\n  exposed-modules:  Demo, Big\n  build-depends:    base\n  default-language: Haskell2010\n"


def wait_for(path, text, secs=30, count=1):
    end = time.time() + secs
    while time.time() < end:
        try:
            if open(path, errors="replace").read().count(text) >= count:
                return True
        except OSError:
            pass
        time.sleep(0.1)
    return False


def main():
    verbose = "-v" in sys.argv
    rounds = int(sys.argv[sys.argv.index("-n") + 1]) if "-n" in sys.argv else 12
    tuicheck.build()
    d = tempfile.mkdtemp(prefix="ghs-edit-")
    proj, session = tuicheck.project(d, session=True, extra={"modules": ["Demo", "Big"]}, more={"demo.cabal": CABAL, "src/Big.hs": big(1000)})
    fakebin = os.path.join(d, "bin")
    os.makedirs(fakebin)
    os.symlink(os.path.join(tuicheck.HERE, "tools", "fake-claude.py"), os.path.join(fakebin, "claude"))
    env = dict(os.environ, PATH=fakebin + os.pathsep + os.environ["PATH"], GHS_PROVIDER="claude", FAKE_CLAUDE_LOG=os.path.join(d, "claude.log"))
    out, fifo = os.path.join(d, "chat.out"), os.path.join(d, "chat.in")
    os.mkfifo(fifo)
    checks = tuicheck.Checks("check-edit")
    check = checks.check
    read = lambda: open(out, errors="replace").read()
    p = None
    try:
        keep = os.open(fifo, os.O_RDWR)
        p = subprocess.Popen([tuicheck.CLI, "chat", "-s", session, "--settle", "0"], cwd=proj, env=env, stdin=keep, stdout=open(out, "w"), stderr=subprocess.STDOUT)
        send = lambda line: os.write(keep, (line + "\n").encode())
        turns = [0]

        def turn(line, secs=300):
            """One turn typed, waited for until its cost is said; what it printed."""
            time.sleep(float(os.environ.get("GAP", "0")))
            text = read()
            send(line)
            ok = wait_for(out, "[turn: ", secs, count=text.count("[turn: ") + 1)
            return ok, read()[len(text):]

        def steps(a, b):
            return " ;; ".join(
                'tool edit %s ;; tool eval %s' % (
                    json.dumps({"path": "src/Big.hs", "old": "val = %d\n" % k, "new": "val = %d\n" % (k + 1)}),
                    json.dumps({"expr": "\"v=\" ++ show Big.val ++ \"=\" ++ show (Big.total 1 > 0)"}))
                for k in range(a, b))

        # a warm-up: the first reload is the slowest
        ok, t = turn('tool eval {"expr": "Big.val"}')
        check("the session is up with the generated module loaded (Big.val is 1000)", ok and "1000" in t, t[-400:])
        t0 = time.time()
        ok, t = turn(steps(1000, 1001))
        first = time.time() - t0
        print("     one edit and its eval took %.1f s" % first)

        def check_round(label, a, b, secs):
            ok, t = turn(steps(a, b), secs)
            got = [int(x) for x in re.findall(r'v=(\d+)=', t)]
            want = list(range(a + 1, b + 1))
            check("%s: each of %d evals answers from the code its edit saved (never the code before it)" % (label, b - a), ok and got == want,
                  (ok, got, want))
            return t

        check_round("edit then eval, alone", 1001, 1001 + rounds, 60 * rounds)

        # another client typechecking all the while
        stop = threading.Event()
        calls = [0]

        def poll():
            while not stop.is_set():
                subprocess.run([tuicheck.CLI, "typecheck", session], cwd=proj, capture_output=True)
                calls[0] += 1

        th = threading.Thread(target=poll)
        if not os.environ.get("NOPOLL"):
            th.start()
        try:
            check_round("edit then eval, with a second client typechecking", 1001 + rounds, 1001 + 2 * rounds, 60 * rounds)
        finally:
            stop.set()
            if th.is_alive():
                th.join()
        print("     the other client made %d typecheck calls" % calls[0])
        if verbose:
            print(read()[-3000:])
        # bench at another level, on a sibling that is still compiling
        t0 = time.time()
        ok, t = turn('tool bench {"expr": "print Big.val", "opt": 1, "timeout": 1}', 120)
        first = t.split("> bench")[-1]
        check("bench at -O1 with a timeout of 1 s, the sibling not up: it says it is still compiling, asks to ask again, and that nothing was measured",
              ok and "FIRST compile" in first and "Nothing was measured" in first and "ask the same again" in first and time.time() - t0 < 60, first[-700:])
        ok, t = turn('tool bench {"expr": "print Big.val", "opt": 1, "timeout": 600}', 900)
        second = t.split("> bench")[-1]
        check("and asked again it waits for the compile and gets the number", ok and "wall" in second and "1000" not in first.split("measured")[0] and re.search(r"v?\d{4}", second) is not None
              and "still compiling" not in second, second[-700:])
        # a sibling that cannot boot
        ok, t = turn('tool bench {"expr": "print 1", "opt": 1, "unit": "exe:nonesuch"}', 300)
        third = t.split("> bench")[-1]
        check("bench with a unit that does not exist: the sibling does not start, and the answer says why", ok and ("did not start" in third or "does not load" in third) and "nonesuch" in third, third[-900:])
        if verbose:
            print(read()[-3000:])
    finally:
        if p is not None and p.poll() is None:
            p.kill()
        sdir = os.path.join(proj, ".ghci-session")
        if os.path.isdir(sdir):
            for n in sorted(os.listdir(sdir)):
                if os.path.isdir(os.path.join(sdir, n)) and n != "history":
                    tuicheck.stop(proj, n)
        shutil.rmtree(d, ignore_errors=True)
    return checks.done()


if __name__ == "__main__":
    sys.exit(main())
