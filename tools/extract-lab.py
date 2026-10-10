#!/usr/bin/env python3
"""A lab for the EXTRACTION of facts ("GhciSession.Know"): pieces of real logs with what each should yield, the
real prompt and the real parser, a small model, and a score that is counted, not judged.

    tools/extract-lab.py [-n RUNS] [-v] [CASES.json]

A change to the extraction prompt, or to where a fact is filed, is tried here before a store is read again
from its histories: each case (tools/extract-lab/cases.json) is a piece -- a user's note, an agent's reply,
a stretch of summaries -- with the facts it should give (`expect`: a pattern its sentence holds, and one its
subject matches), the ones it must not (`forbid`), and how many at most (`max`). The prompt is the daemon's
(`ghci-session knowledge extract-prompt`), the answer is read as the daemon reads it (`extract-parse`: the
scope rules, the refiling), the model is the compactor's (Haiku through the claude command), RUNS times a case
(3). Counted: expected facts found, forbidden ones given, cases over their maximum.

Exit status 0 always: it is a measure, not a check (a model's answers vary).
"""
import json, os, re, subprocess, sys
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CLI = os.path.join(HERE, ".bin", "ghci-session")


def facts(case):
    prompt = subprocess.run([CLI, "knowledge", "extract-prompt", case["project"]], input=case["piece"], capture_output=True, text=True).stdout
    system, _, user = prompt.partition("\n<chat>\n")
    try:
        r = subprocess.run(["claude", "-p", "--output-format", "json", "--setting-sources", "", "--strict-mcp-config", "--disable-slash-commands", "--no-session-persistence",
                            "--model", "claude-haiku-5-5", "--tools", "", "--system-prompt", system], input="<chat>\n" + user, capture_output=True, text=True, timeout=300, cwd="/")
        answer = json.loads(r.stdout).get("result", "")
    except Exception as e:                            # (said: a call that failed is not a piece with no facts)
        print("extract-lab: the model could not be asked for %r (%s): this run counts no facts" % (case["name"], e), file=sys.stderr)
        return []
    out = subprocess.run([CLI, "knowledge", "extract-parse", case["project"]], input=answer, capture_output=True, text=True).stdout
    return [dict(zip(("subject", "topic", "fact"), l.split("\t", 2))) for l in out.splitlines() if l.count("\t") >= 2]


def hit(spec, f):
    return bool(re.search(spec["text"], f["fact"], re.I)) and bool(re.search(spec.get("subject", "."), f["subject"]))


def main():
    a = sys.argv[1:]
    verbose = "-v" in a
    runs = int(a[a.index("-n") + 1]) if "-n" in a and a.index("-n") + 1 < len(a) else 3
    files = [x for k, x in enumerate(a) if not x.startswith("-") and (k == 0 or a[k - 1] != "-n")]
    cases = [dict(dict(expect=[], forbid=[], max=3), **c) for c in json.load(open(files[0] if files else os.path.join(HERE, "tools", "extract-lab", "cases.json")))]
    jobs = [(k, r) for k in range(len(cases)) for r in range(runs)]
    with ThreadPoolExecutor(6) as ex:
        got = list(ex.map(lambda j: facts(cases[j[0]]), jobs))
    tot = dict(expect=0, found=0, forbidden=0, over=0, facts=0)
    print("%-62s %9s %9s %5s %s" % ("case (%d runs each)" % runs, "expected", "forbidden", "over", "facts"))
    for k, c in enumerate(cases):
        rs = [g for (kk, _), g in zip(jobs, got) if kk == k]
        found = sum(1 for fs in rs for e in c["expect"] if any(hit(e, f) for f in fs))
        forb = sum(1 for fs in rs for f in fs if any(hit(e, f) for e in c["forbid"]))
        over = sum(1 for fs in rs if len(fs) > c["max"])
        n = sum(len(fs) for fs in rs)
        tot["expect"] += runs * len(c["expect"]); tot["found"] += found; tot["forbidden"] += forb; tot["over"] += over; tot["facts"] += n
        print("%-62s %4d/%-4d %9d %5d %5d" % (c["name"][:62], found, runs * len(c["expect"]), forb, over, n))
        if verbose:
            for fs in rs[:1]:
                for f in fs:
                    print("      %-22s %s" % (f["subject"], f["fact"][:150]))
    print("%-62s %4d/%-4d %9d %5d %5d" % ("total", tot["found"], tot["expect"], tot["forbidden"], tot["over"], tot["facts"]))


if __name__ == "__main__":
    main()
