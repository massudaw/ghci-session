#!/usr/bin/env python3
"""A round's changes read by a reviewer that did not write them and has nothing else in mind.

    tools/review-diff.py BASE [HEAD] [--model M] [-- PATH...]        findings on standard output, as markdown

The agent that made a change checks it with what it believed while making it: a day of rounds that each passed
every suite still held nine real faults, one of them a feature that had never been on (a default merged into
every configuration made a ratio "always set"), found only when the whole diff was read again from nothing.
This is that reading made routine, for one round: `git diff BASE..HEAD` (HEAD: the work tree when not given),
with wide context, the source files only, given a file or a few at a time to a model with no tools and no
history, and asked for what a maintainer would stop a merge for. Run it before a round is pushed; give its
findings to the agent to prove or dismiss -- it reads the diff, not the program, and is wrong some of the time.

The model is Sonnet through the claude command (--model for another); a call for every 150 KB of diff.
"""
import json, re, subprocess, sys
from concurrent.futures import ThreadPoolExecutor

PROMPT = """You review a change to a Haskell program (a daemon and command line that keep a warm GHCi session, an agent's chat loop, its memory) and its Python check scripts, as a maintainer who did not write it. Below is a diff with wide context.
Report only what you would stop a merge for, or want proven before it:
- real bugs: a race, a partial function on input that can occur, an off-by-one, a file written without care for a crash or a second writer, a lock not released on an exception, a process, thread or fd left behind, an unbounded read of what can be huge;
- behaviour that contradicts the comment or the message beside it, or a default that makes a branch unreachable;
- an error path that swallows what the user needs to know;
- a test that asserts nothing, or tests its fixture and not the code;
- dead code and leftovers of an approach that was abandoned.
Not style, not naming, not what you would have done differently. If you cannot tell without code the diff does not show, say what would have to be looked at. Be specific: the line, the input, the consequence.
Reply with JSON only: {"findings": [{"file": "...", "line": <number in the new file, or 0>, "severity": "high|medium|low", "sure": "yes|likely|unsure", "what": "<the fault and its consequence, two sentences at most>", "show": "<how it could be shown: an input, a test>"}]} -- an empty list when there is nothing."""


def main():
    a = sys.argv[1:]
    if not a or a[0].startswith("-"):
        sys.exit(__doc__)
    paths = []
    if "--" in a:
        paths = a[a.index("--") + 1:]
        a = a[:a.index("--")]
    model = "claude-sonnet-5-5"
    if "--model" in a:
        model = a.pop(a.index("--model") + 1)
        a.remove("--model")
    rng = [a[0] + ".." + a[1]] if len(a) > 1 else [a[0]]
    g = subprocess.run(["git", "diff", "-U25"] + rng + ["--"] + (paths or ["app", "engine", "tui/src", "hygiene/src", "tools", "cbits", "*.sh"]), capture_output=True, text=True)
    if g.returncode != 0:
        sys.exit("review-diff: git diff failed: " + g.stderr.strip())
    diff = g.stdout
    files = [f for f in re.split(r"(?m)^(?=diff --git )", diff) if f.strip() and not re.search(r"^diff --git a/\S+\.(md|json|txt)\b", f)]
    if not files:
        print("nothing to review in " + " ".join(rng))
        return
    chunks, cur = [], ""
    for f in files:                                   # a few files a call, a big one alone (cut at 150 KB a piece)
        lines, parts, part = f.splitlines(True), [], ""
        for l in lines:                               # (cut at a line's end, and each piece says which file it is of)
            if part and len(part) + len(l) > 150000:
                parts.append(part)
                part = lines[0] + "(... the same file, continued)\n"
            part += l
        parts.append(part)
        for part in parts:
            if cur and len(cur) + len(part) > 150000:
                chunks.append(cur)
                cur = ""
            cur += part
    chunks.append(cur)

    def one(c):
        r = subprocess.run(["claude", "-p", "--output-format", "json", "--setting-sources", "", "--strict-mcp-config", "--disable-slash-commands", "--no-session-persistence",
                            "--model", model, "--tools", "", "--system-prompt", PROMPT], input=c, capture_output=True, text=True, timeout=900, cwd="/")
        try:
            t = json.loads(r.stdout).get("result", "")
            return json.loads(t[t.find("{"):t.rfind("}") + 1]).get("findings", [])
        except Exception:
            return [dict(file="?", line=0, severity="low", sure="unsure", what="the reviewer's answer for a part of the diff could not be read (%d bytes of diff)" % len(c), show="run again")]
    with ThreadPoolExecutor(4) as ex:
        found = [f for fs in ex.map(one, chunks) for f in fs if isinstance(f, dict)]
    rank = {"high": 0, "medium": 1, "low": 2}
    found.sort(key=lambda f: (rank.get(f.get("severity"), 3), {"yes": 0, "likely": 1}.get(f.get("sure"), 2)))
    print("# Review of %s: %d file(s), %d KB of diff, %d finding(s)\n" % (" ".join(rng), len(files), len(diff) // 1000, len(found)))
    for f in found:
        print("- **%s** (%s) `%s:%s` -- %s\n  *shown by:* %s" % (f.get("severity", "?"), f.get("sure", "?"), f.get("file", "?"), f.get("line", 0), f.get("what", ""), f.get("show", "")))


if __name__ == "__main__":
    main()
