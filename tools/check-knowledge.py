#!/usr/bin/env python3
"""What the sessions establish, kept by subject ("GhciSession.Know"), end to end with a stand-in for the model.

    tools/check-knowledge.py [-v]

In a project made for it, with "knowledge": true and tools/fake-summarize.py as the compactor's command, and
the facts kept in a directory of the check's own ($GHS_KNOWLEDGE):

  a rule said          a message of the user's is asked for its facts; the fact is stored under its subject and
                       appended to the session's history as a `known` line.
  a rule changed       a later message that contradicts it: the new fact replaces the old, which is kept, marked.
  a tool's arguments   an argument a tool is called with for the first time is a fact, with no model asked.
  the view             begins with the subjects' block -- what holds, not what was replaced -- and the block is
                       the same text after more is learned: the new fact is a line of the view, not a rewrite.
  forget               a fact is forgotten by its id.

Exit status 0 when all hold. Half a minute.
"""
import json, os, shutil, subprocess, sys, tempfile, time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tuicheck

CLI = tuicheck.CLI


def main():
    verbose = "-v" in sys.argv
    tuicheck.build()
    d = tempfile.mkdtemp(prefix="ghs-know-")
    know, slog = os.path.join(d, "know"), os.path.join(d, "summarize.log")
    os.environ.update(GHS_KNOWLEDGE=know, FAKE_SUMMARIZE_LOG=slog)
    proj, session = tuicheck.project(d, session=True)
    checks = tuicheck.Checks("check-knowledge")
    run = lambda *a: subprocess.run([CLI] + list(a), cwd=proj, capture_output=True, text=True)
    try:
        # the session again, with the knowledge kept
        tuicheck.stop(proj, session)
        cfg = json.load(open(os.path.join(proj, "ghci-session.json")))
        cfg.update(knowledge=True, summarize_cmd=sys.executable + " " + os.path.join(tuicheck.HERE, "tools", "fake-summarize.py"))
        json.dump(cfg, open(os.path.join(proj, "ghci-session.json"), "w"))
        run("start", session)

        def until(f, secs=30):
            t = time.time()
            while time.time() - t < secs:
                if f():
                    return True
                time.sleep(0.3)
            return False
        facts = lambda *a: run("knowledge", *a).stdout
        hist = lambda: run("history", "-n", "400", "--full").stdout
        pad = " (said at some length, so that the message is one worth asking for its facts and not a word in passing)"

        run("history", "--kind", "user", "RULE-A: run the routine check with cabal test" + pad)
        ok = until(lambda: "cabal test through sh" in facts("--subject", "user/rules"))
        checks.check("a rule the user gives is a fact under its subject, with the message it came from",
                     ok and "proj/demo:" in facts("--subject", "user/rules"), facts("--subject", "user/rules"))
        checks.check("and a `known` line in the session's history", until(lambda: "known: user/rules: The routine check is run with cabal test" in hist()), hist()[-400:])

        run("history", "--kind", "user", "RULE-B: use the session's test tool for the routine check, not cabal" + pad)
        ok = until(lambda: "session's test tool" in facts("--subject", "user/rules"))
        cur, everything = facts("--subject", "user/rules"), facts("--subject", "user/rules", "--all")
        checks.check("a later rule that contradicts it replaces it: one fact holds, the old one is kept and marked",
                     ok and "cabal test through sh" not in cur and "cabal test through sh" in everything and "replaced by" in everything, (cur, everything))
        checks.check("the `known` line says what it replaces", until(lambda: "(THIS REPLACES: The routine check is run with cabal test through sh.)" in hist()), hist()[-500:])

        run("history", "--kind", "tool", 'bench {"expr": "Demo.greeting", "opt": 2}')
        ok = until(lambda: '"opt"' in facts("--subject", "tool/usage"))
        before = open(slog).read().count("reconcile") if os.path.exists(slog) else 0
        checks.check("an argument a tool is first called with is a fact, and no model was asked for it",
                     ok and '"expr"' in facts("--subject", "tool/usage") and "e.g. bench {" in facts("--subject", "tool/usage"), facts("--subject", "tool/usage"))

        view1 = run("view").stdout
        block1 = view1.split("</subjects>")[0]
        checks.check("the view begins with the subjects: what holds, not what was replaced; the tool's arguments on a line",
                     view1.startswith("<subjects>") and "## user/rules (1 facts)" in block1 and "session's test tool" in block1 and "cabal test through sh" not in block1
                     and 'bench is called with: ' in block1 and "<chat>" in view1.split("</subjects>")[1], block1)

        run("history", "--kind", "user", "RULE-C: do not commit data files in this project" + pad)
        ok = until(lambda: "No data files" in facts("--subject", "proj/rules"))
        view2 = run("view").stdout
        checks.check("what is learned after is under the project's subject, and a line of the view: the block before it is the text it was",
                     ok and view2.split("</subjects>")[0] == block1 and "known: proj/rules: No data files are committed" in view2.split("</subjects>")[1], view2[-600:])
        checks.check("a fact with nothing near it is stored with no model asked to place it", open(slog).read().count("reconcile") == before, open(slog).read().split())

        fid = facts("--subject", "proj/rules").split()[0]
        r = run("knowledge", "forget", fid)
        checks.check("a fact is forgotten by its id", r.returncode == 0 and "nothing under" in facts("--subject", "proj/rules") and run("knowledge", "forget", "nosuch").returncode == 1, facts())
        listing = facts()
        checks.check("the subjects and what each holds are listed", "user/rules" in listing and "tool/usage" in listing and "1 replaced" in listing, listing)
        if verbose:
            print(view2)
    finally:
        tuicheck.stop(proj, session)
        shutil.rmtree(d, ignore_errors=True)
    sys.exit(checks.done())


if __name__ == "__main__":
    main()
