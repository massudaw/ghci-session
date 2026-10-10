#!/usr/bin/env python3
"""The chat's subscription path, without a subscription: tools/fake-claude.py stands for the `claude` command.

    tools/check-cli.py [-v]

In a project made for it, with a session of its own:

  a turn        the chat starts the program, serves it its tools, shows what it says; each model call is in the
                usage ledger as it ends, with what it read from the cache.
  a restart     `chat --restart` while a tool call is running, and again while the model "thinks": the chat
                becomes the executable on disk and the turn goes on -- the same chat process, the same program
                (it is started once), the call that was running answered, the calls after it made.
  a turn gone   the chat is killed in the middle of a turn; `chat --continue` goes on with it from the history:
  on with       the new program is given the turn's log, and what it did before is in it.

  a rollover    a turn whose context grows past `--rollover` tokens ends its run after a tool call and goes on in a
                fresh one, from its log.

  a limit       the subscription's limit reached in the middle of a turn: the chat waits for it to reset, and the
                turn goes on from its log.

Exit status 0 when all hold. About a minute and a half.
"""
import json, os, shutil, signal, subprocess, sys, tempfile, time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tuicheck


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
    tuicheck.build()
    d = tempfile.mkdtemp(prefix="ghs-cli-")
    proj, session = tuicheck.project(d, session=True)
    fakebin = os.path.join(d, "bin")
    os.makedirs(fakebin)
    os.symlink(os.path.join(tuicheck.HERE, "tools", "fake-claude.py"), os.path.join(fakebin, "claude"))
    log = os.path.join(d, "claude.log")
    env = dict(os.environ, PATH=fakebin + os.pathsep + os.environ["PATH"], GHS_PROVIDER="claude", FAKE_CLAUDE_LOG=log)
    out = os.path.join(d, "chat.out")
    fifo = os.path.join(d, "chat.in")
    os.mkfifo(fifo)
    checks = tuicheck.Checks("check-cli")
    check = checks.check
    chat = lambda *more: [tuicheck.CLI, "chat", "-s", session, "--settle", "0", "--usage", *more]
    read = lambda: open(out, errors="replace").read()
    try:
        keep = os.open(fifo, os.O_RDWR)      # (the pipe stays open: the chat reads lines as they are sent)
        p = subprocess.Popen(chat(), cwd=proj, env=env, stdin=keep, stdout=open(out, "w"), stderr=subprocess.STDOUT)
        send = lambda line: os.write(keep, (line + "\n").encode())
        # a turn
        send('tool sh {"cmd":"echo one-$((6*7))"} ;; say the first turn is done')
        ok = wait_for(out, "[turn: ", 30)
        t = read()
        check("a turn through the program: its tool call is run by the chat and shown, what it says is shown, the turn's cost said",
              ok and "> sh " in t and "one-42" in t and "the first turn is done" in t, t[-400:])
        ledger = [json.loads(l) for l in open(os.path.join(proj, ".ghci-session", session, "usage.jsonl"))]
        check("each model call is in the ledger as it ends, with what it read from the cache",
              len(ledger) == 2 and all(r["who"] == "chat" and r["in"] == 1000 and r["cached"] == 900 and r["out"] == 20 for r in ledger), ledger)
        # a restart while a tool call runs, and one while the model thinks
        send('tool sh {"cmd":"sleep 3; echo first-done"} ;; pause 4 ;; tool sh {"cmd":"echo second-done"} ;; say all of it is done')
        wait_for(out, "sleep 3; echo first-done", 20)
        time.sleep(0.5)
        subprocess.run(chat("--restart")[:4] + ["--restart"], cwd=proj, env=env, capture_output=True)
        took = wait_for(out, "the turn goes on, with the claude program it was in", 30)
        first = wait_for(out, "first-done", 20, count=2)      # (in the command, and in its answer)
        time.sleep(1.0)                                       # (now in the pause)
        subprocess.run(chat("--restart")[:4] + ["--restart"], cwd=proj, env=env, capture_output=True)
        again = wait_for(out, "the turn goes on, with the claude program it was in", 30, count=2)
        ended = wait_for(out, "all of it is done", 40)
        wait_for(out, "[turn: ", 20, count=2)
        t = read()
        check("a restart while a tool call runs: the call is finished and answered, then the chat is the program on disk, with the turn",
              took and first and t.index("first-done") < t.index("the turn goes on"), t[-900:])
        check("a restart while the model thinks: the turn goes on, and the calls after it are made and answered",
              again and ended and t.count("second-done") >= 2 and "all of it is done" in t, t[-600:])
        check("it is the same chat process throughout, and the program was started once for each turn (not again at a restart)",
              p.poll() is None and open(log).read().count("pid ") == 2, open(log).read())
        # a chat killed in the middle of a turn, and the turn gone on with from its log
        send('tool sh {"cmd":"echo step-one"} ;; pause 60 ;; say never said')
        wait_for(out, "step-one", 20, count=2)
        time.sleep(0.5)
        p.send_signal(signal.SIGKILL); p.wait()
        out2 = os.path.join(d, "chat2.out")
        p2 = subprocess.Popen(chat("--continue"), cwd=proj, env=env, stdin=keep, stdout=open(out2, "w"), stderr=subprocess.STDOUT)
        ok = wait_for(out2, "resumed from the log", 40)
        wait_for(out2, "[turn: ", 20)
        t2 = open(out2, errors="replace").read()
        check("a chat killed in a turn, and `chat --continue`: the turn's log is given to a new program, which finds in it what was done",
              ok and "going on with the turn of message" in t2 and "1 tool call(s)" in t2 and "step-one" in t2 and "never said" not in t2.split("resumed from the log")[-1], t2[-700:])
        send("say and a line after it")
        check("and the chat goes on taking lines after it", wait_for(out2, "and a line after it", 20, count=1) and p2.poll() is None, open(out2, errors="replace").read()[-300:])
        wait_for(out2, "[turn: ", 20, count=2)
        time.sleep(0.5)                                       # (the turn's end is noted just after its cost is said)
        p2.send_signal(signal.SIGTERM); p2.wait()
        # --continue, and the last turn ended: there is nothing to go on with
        out3 = os.path.join(d, "chat3.out")
        p3 = subprocess.Popen(chat("--continue"), cwd=proj, env=env, stdin=keep, stdout=open(out3, "w"), stderr=subprocess.STDOUT)
        check("`chat --continue` when the last turn ended starts none", wait_for(out3, "nothing to go on with: the last turn ended", 20) and "going on with the turn" not in open(out3).read(), open(out3).read()[-300:])
        p3.send_signal(signal.SIGTERM); p3.wait()
        # a turn whose context grows past the rollover's tokens goes on in a fresh call, from its log
        out4 = os.path.join(d, "chat4.out")
        started = open(log).read().count("pid ")
        p4 = subprocess.Popen(chat("--rollover", "2000"), cwd=proj, env=dict(env, FAKE_CLAUDE_CTX="1000,500"), stdin=keep, stdout=open(out4, "w"), stderr=subprocess.STDOUT)
        send("first of all ;; " + " ;; ".join('tool sh {"cmd":"echo roll-%d"}' % k for k in range(1, 7)) + " ;; say said by the first run")
        ok = wait_for(out4, "resumed from the log", 40)
        wait_for(out4, "[turn: ", 20)
        t4 = open(out4, errors="replace").read()
        rolled_at = [k for k in range(1, 7) if ("roll-%d" % k) in t4.split("the turn's context is at")[0]] if "the turn's context is at" in t4 else []
        check("a turn whose context passes the rollover's tokens ends its run after a tool call and goes on in a fresh one, from its log -- what it did is in it, and no call is made twice",
              ok and rolled_at and max(rolled_at) < 6 and "said by the first run" not in t4 and ("%d tool call(s)" % len(rolled_at)) in t4
              and open(log).read().count("pid ") == started + 2 and t4.count("[turn: ") == 1, (rolled_at, t4[-600:]))
        check("the turn's message is still the one it began with (what was typed first), and its cost is the whole turn's",
              "first of all" in t4.split("the turn's context is at")[0] and ("%d tool call" % len(rolled_at)) in t4.split("[turn: ")[-1]
              and ("%d model calls" % (len(rolled_at) + 1)) in t4.split("[turn: ")[-1], t4.split("[turn: ")[-1][:200])
        p4.send_signal(signal.SIGTERM); p4.wait()
        # the subscription's limit reached in the middle of a turn: waited out, and the turn goes on
        out5 = os.path.join(d, "chat5.out")
        started = open(log).read().count("pid ")
        p5 = subprocess.Popen(chat(), cwd=proj, env=dict(env, FAKE_CLAUDE_LIMIT="2,3"), stdin=keep, stdout=open(out5, "w"), stderr=subprocess.STDOUT)
        send('tool sh {"cmd":"echo before-the-limit"} ;; tool sh {"cmd":"echo never-run"} ;; say never said')
        waits = wait_for(out5, "the turn waits for the limit to reset", 30)
        ok = wait_for(out5, "[turn: ", 60)       # (the wait is the seconds to the reset and twenty more)
        t5 = open(out5, errors="replace").read()
        ok = ok and "resumed from the log: 1 tool call(s)" in t5.split("the turn waits")[-1]
        check("the subscription's limit reached in a turn: the turn waits for it to reset and then goes on from its log, by itself",
              waits and ok and "before-the-limit" in t5 and "echo never-run" not in t5.split("the turn waits")[0].split("tool sh")[-1] and "1 tool call(s)" in t5
              and open(log).read().count("pid ") == started + 2 and t5.count("[turn: ") == 1, t5[-700:])
        p5.send_signal(signal.SIGTERM)
        if verbose:
            print(read()); print(open(out2, errors="replace").read())
    finally:
        for q in ("p", "p2", "p3", "p4", "p5"):
            if q in locals() and locals()[q].poll() is None:
                locals()[q].kill()
        tuicheck.stop(proj, session)
        shutil.rmtree(d, ignore_errors=True)
    return checks.done()


if __name__ == "__main__":
    sys.exit(main())
