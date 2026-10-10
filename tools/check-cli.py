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
    hook = os.path.join(d, "hook.txt")       # (what the on_turn_end command is told of a turn ends up here)
    proj, session = tuicheck.project(d, session=True, extra={"on_turn_end": "printf '%s|%s|' \"$GHS_SESSION\" \"$GHS_TURN_TOOL_CALLS\" > " + hook + "; cat >> " + hook})
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
        check("each ledger line keeps the plan's windows as last seen (name, used, reset), whether the call was on usage credits, and what it wrote to the cache",
              all(r.get("windows", {}).get("five_hour", {}).get("u", 0) >= 0.11 and r["windows"]["seven_day"]["u"] == 0.3 and r["windows"]["five_hour"].get("reset", 0) > 1e9
                  and r.get("credits") is False and r.get("new") == 100 and r.get("wr") == 0 for r in ledger), ledger)
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
        p5.send_signal(signal.SIGTERM); p5.wait()
        # a task sent and waited for: its last words and summary, the exit status, the command of on_turn_end
        out6 = os.path.join(d, "chat6.out")
        p6 = subprocess.Popen(chat(), cwd=proj, env=env, stdin=keep, stdout=open(out6, "w"), stderr=subprocess.STDOUT)
        cli = lambda *a: subprocess.run([tuicheck.CLI, "chat", "-s", session, *a], cwd=proj, env=env, capture_output=True, text=True, timeout=90)
        for _ in range(40):                    # (until the chat is there to be sent to)
            r = cli("--send", 'tool sh {"cmd":"echo wait-one"} ;; say the waited task is done', "--wait", "60")
            if r.returncode != 1:
                break
            time.sleep(0.5)
        time.sleep(1.0)                        # (the command of on_turn_end runs just after the turn's end)
        told = open(hook).read() if os.path.exists(hook) else ""
        check("chat --send MSG --wait: waits for the turn that took the line, prints its last words whole and its summary, exits 0; on_turn_end is told the session, the tool calls and the last words",
              r.returncode == 0 and "the waited task is done" in r.stdout and "[turn: " in r.stdout and "1 tool call" in r.stdout
              and told.startswith(session + "|1|") and "the waited task is done" in told, (r.returncode, r.stdout, r.stderr, told))
        r = cli("--send", 'pause 8 ;; tool sh {"cmd":"echo x"} ;; say the late words', "--wait", "2")
        check("chat --send --wait SECS: when SECS pass first it says the turn is still running, and exits 3", r.returncode == 3 and "still running" in r.stdout, (r.returncode, r.stdout, r.stderr))
        r = cli("--wait", "60")
        check("chat --wait alone: waits for the turn under way, and prints its last words", r.returncode == 0 and "the late words" in r.stdout and "[turn: " in r.stdout, (r.returncode, r.stdout, r.stderr))
        t0 = time.time(); r = cli("--wait")
        check("chat --wait with the chat at rest: at once, the last turn's last words", r.returncode == 0 and "the late words" in r.stdout and time.time() - t0 < 5, (r.returncode, r.stdout, r.stderr))
        # a line sent while a turn runs, and left for the turn after it (the first one has no call to feed it between)
        r0 = cli("--send", "pause 4 ;; say first of two")
        time.sleep(1.0)
        r = cli("--send", "say second of two", "--wait", "60")
        check("chat --send --wait while a turn runs that does not take the line: it waits for the turn the line began, and not the one that ended meanwhile",
              r0.returncode == 0 and r.returncode == 0 and "second of two" in r.stdout and "first of two" not in r.stdout, (r0.returncode, r.returncode, r.stdout, r.stderr))
        # the chat dies in the middle of a turn: turn.json says running for good; a waiter leaves at once
        cli("--send", "pause 30 ;; say never")
        time.sleep(1.5)
        waiter = subprocess.Popen([tuicheck.CLI, "chat", "-s", session, "--wait", "60"], cwd=proj, env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        time.sleep(1.0)
        t0 = time.time(); p6.send_signal(signal.SIGKILL); p6.wait()
        try:
            waiter.wait(timeout=15)
        except subprocess.TimeoutExpired:
            waiter.kill(); waiter.wait()
        check("chat --wait: the chat dies in the turn waited for: exit 1 within seconds (not SECS)",
              waiter.returncode == 1 and time.time() - t0 < 10, (waiter.returncode, time.time() - t0))
        t0 = time.time(); r = cli("--wait", "60")
        check("chat --wait when the chat died in a turn (turn.json still says it runs): exit 1 at once", r.returncode == 1 and time.time() - t0 < 5, (r.returncode, r.stdout, r.stderr, time.time() - t0))
        r = cli("--wait", "5")
        check("chat --wait when no chat is running: exit 1", r.returncode == 1, (r.returncode, r.stdout, r.stderr))
        # the hook at a Claude Code turn's end: with the session's daemon not running, `import --go` leaves at once
        subprocess.run([tuicheck.CLI, "stop", session], cwd=proj, env=env, capture_output=True, text=True, timeout=60)   # (the check's own throwaway session)
        t0 = time.time(); r = subprocess.run([tuicheck.CLI, "import", "--go", "-s", session], cwd=proj, env=env, capture_output=True, text=True, timeout=60)
        check("import --go with no daemon running: exit 0 at once, one line saying so, no build started, nothing written",
              r.returncode == 0 and time.time() - t0 < 5 and len(r.stdout.strip().splitlines()) == 1 and "no session running" in r.stdout
              and not os.path.exists(os.path.join(proj, ".ghci-session", session, "history", "imported.json")), (r.returncode, r.stdout, r.stderr, time.time() - t0))
        # a sibling whose boot failed leaves its state dir (status "loaded=-", no history): gc -n lists it, gc removes it
        ghost = os.path.join(proj, ".ghci-session", "ghost-O2"); fresh = os.path.join(proj, ".ghci-session", "booting-O2")
        built = os.path.join(proj, ".ghci-session", "built-O2")      # (timed out in the middle of a first compile: it holds what it compiled)
        for g, age in ((ghost, 3600), (fresh, 0), (built, 3600)):
            os.makedirs(g, exist_ok=True); sf = os.path.join(g, "status")
            open(sf, "w").write("STALE(3) DEAD: timed out after 900s\nsession=x gen=0 loaded=- checked=-\n")
            os.utime(sf, (time.time() - age, time.time() - age))
        os.makedirs(os.path.join(built, "objs", "Demo"), exist_ok=True)
        open(os.path.join(built, "objs", "Demo", "A.o"), "w").write("x")
        gc = lambda *a: subprocess.run([tuicheck.CLI, "gc", *a], cwd=proj, env=env, capture_output=True, text=True, timeout=60)
        r1 = gc("-n"); there1 = os.path.isdir(ghost)
        r2 = gc(); there2 = os.path.isdir(ghost)
        check("gc -n lists a dead session that never loaded and keeps it; gc removes it; one still young, or holding compiled modules, is left",
              "would reap session ghost-O2" in r1.stdout and "booting-O2" not in r1.stdout and "built-O2" not in r1.stdout and there1
              and "reaping session ghost-O2" in r2.stdout and not there2 and os.path.isdir(fresh)
              and os.path.isfile(os.path.join(built, "objs", "Demo", "A.o")), (r1.stdout, r2.stdout, there1, there2))
        if verbose:
            print(read()); print(open(out2, errors="replace").read())
    finally:
        for q in ("p", "p2", "p3", "p4", "p5", "p6"):
            if q in locals() and locals()[q].poll() is None:
                locals()[q].kill()
        tuicheck.stop(proj, session)
        shutil.rmtree(d, ignore_errors=True)
    return checks.done()


if __name__ == "__main__":
    sys.exit(main())
