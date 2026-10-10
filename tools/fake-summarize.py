#!/usr/bin/env python3
"""A stand-in for `ghci-session summarize`, for tools/check-knowledge.py: it answers the three prompts a daemon
sends -- a line to compress or merge, a piece of the log to extract facts from, new facts to set against the
ones that hold -- by rule, from marks in the text (RULE-A, RULE-B, RULE-C), and notes each prompt's kind in
$FAKE_SUMMARIZE_LOG."""
import json, os, re, sys
p = sys.stdin.read()
def note(kind):
    f = os.environ.get("FAKE_SUMMARIZE_LOG")
    if f:
        with open(f, "a") as h: h.write(kind + "\n")
FACTS = {"RULE-A": ("user", "user/rules", "routine check command", "The routine check is run with cabal test through sh."),
         "RULE-B": ("user", "user/rules", "routine check command", "The routine check is run with the session's test tool, not with cabal through sh."),
         "RULE-C": ("project", "rules", "commit of data files", "No data files are committed in this project.")}
if "knowledge base built from the log" in p:
    note("extract")
    body = p.split("<chat>", 1)[1]
    print(json.dumps({"facts": [dict(scope=s, subject=sub, topic=t, fact=f) for k, (s, sub, t, f) in FACTS.items() if k in body]}))
elif "You keep a knowledge base of facts" in p:
    note("reconcile")
    existing = dict(re.findall(r"^#(\d+) \[[^\]]*\] \([^)]*\) (.*)$", p, re.M))
    new = re.findall(r"^N(\d+) \[[^\]]*\] \([^)]*\) (.*)$", p, re.M)
    ds = []
    for n, text in new:
        old = [int(k) for k, t in existing.items() if "routine check" in t and "routine check" in text and t != text]
        same = [int(k) for k, t in existing.items() if t == text]
        ds.append({"n": int(n), "action": "same" if same else "replace" if old else "add", "ids": same or old})
    print("```json")                      # (a fence, as a model sometimes puts: the daemon reads the object in it)
    print(json.dumps({"decisions": ds}))
else:
    note("summary")
    print("a summary line")
