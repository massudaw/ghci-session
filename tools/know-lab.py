#!/usr/bin/env python3
"""A lab for the subjects' block ("GhciSession.Know"): the real store of facts, a policy as a few arguments, and
two scores -- one that asks no model and takes a second, one that asks a small model and takes two minutes.

    tools/know-lab.py score  [--project P] [--questions FILE] [POLICY...]     no model: which questions have a
                                                                             fact that answers them IN the block
    tools/know-lab.py ask    [--project P] [--questions FILE] [--dir PROJECT-DIR] [POLICY...]
                                                                             the questions asked of a model that
                                                                             reads the block (and the session's view)
    tools/know-lab.py block  [--project P] POLICY                             the block a policy gives

A change to how the block is made -- its size, its shares, its order, what is left out of it -- is tried here
before it is built: an agent's round, a store read again from two histories and two daemons restarted gave a
number in an hour, and the number's cause was one fact at the cut. This gives it in a second (`score`), and
`ask` says whether the second's number meant anything (cutting facts to 140 characters kept them "in the block"
and lost the answers: 41 of 48 against 45).

A POLICY is `name` or `name:key=value,...` with: budget=BYTES (16000), tiers=A/B/C (45/40/15: the tool's and
the user's subjects, the project's, the others'), cut=CHARS (a fact cut to so many), scope=1 (general facts
that speak of one project left out: what `ghci-session knowledge refile` does to the store), drop=SUBJECT.
With none: `now` and `scope:scope=1`.

The questions (--questions, default tools/know-lab/dxf-questions.json): [{"q", "current", "outdated"}], what
holds now and what was once said. `score` needs to know which facts answer each: a model is asked once per
question (the claude command, Sonnet) and its answer kept beside the store (lab-gold.json), by the questions'
and the store's content. `ask` runs each question three times on Haiku and has Sonnet grade twice; the same
policy asked again differs by a point or two of 48.
"""
import hashlib, json, math, os, re, subprocess, sys, time
from collections import Counter, defaultdict
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CLI = os.path.join(HERE, ".bin", "ghci-session")
KDIR = os.environ.get("GHS_KNOWLEDGE") or os.path.join(os.environ.get("XDG_STATE_HOME") or os.path.expanduser("~/.local/state"), "ghci-session", "knowledge")
NOW = time.time()


def ask(system, prompt, model="claude-haiku-5-5", timeout=400):
    r = subprocess.run(["claude", "-p", "--output-format", "json", "--setting-sources", "", "--strict-mcp-config", "--disable-slash-commands",
                        "--no-session-persistence", "--model", model, "--tools", "", "--system-prompt", system],
                       input=prompt, capture_output=True, text=True, timeout=timeout, cwd="/")
    try:
        return json.loads(r.stdout).get("result", "")
    except Exception:
        return ""


def js(t):
    a, b = t.find("{"), t.rfind("}")
    try:
        return json.loads(t[a:b + 1])
    except Exception:
        return {}


def load():
    """The store as the daemon reads it: the facts in order, their marks applied."""
    facts, order = {}, []
    for l in open(os.path.join(KDIR, "facts.jsonl"), "rb").read().split(b"\n"):
        try:
            j = json.loads(l)
        except Exception:
            continue
        m = j.get("mark")
        if not m:
            if j["id"] not in facts:
                facts[j["id"]] = dict(j, by=None, seen=0)
                order.append(j["id"])
        elif j.get("id") in facts:
            f = facts[j["id"]]
            if m == "by":
                f["by"] = j.get("by")
            elif m == "seen":
                f["last"] = max(f["last"], j.get("at", 0))
                f["seen"] += 1
            elif m == "forget":
                facts.pop(j["id"])
    return [facts[i] for i in order if i in facts]


def score(f):
    half = 180 if (f["subject"].startswith("user/") or f["subject"].endswith("/rules")) else 30
    return (1 + f["seen"]) * 2 ** (-((NOW - f["last"]) / 86400) / half)


iscall = lambda f: bool(re.match(r"\w+ call argument \w+$", f.get("topic", "")))
SPEAKS = re.compile(r"this repository|this repo\b|this project|this codebase", re.I)


def policy(spec):
    name, _, args = spec.partition(":")
    kw = dict(budget=16000, tiers=(.45, .40, .15), cut=0, scope=False, drop=["tool/operator"])
    for a in filter(None, args.split(",")):
        k, _, v = a.partition("=")
        if k == "tiers":
            kw[k] = tuple(float(x) / 100 for x in v.split("/"))
        elif k == "drop":
            kw[k].append(v)
        elif k == "scope":
            kw[k] = v not in ("0", "")
        else:
            kw[k] = int(v)
    return name, kw


def render(facts, project, budget, tiers, cut, scope, drop):
    def keep(f):
        general = f["subject"].split("/")[0] in ("tool", "user")
        return not (scope and general and SPEAKS.search(f["fact"]))
    text = lambda f: f["fact"] if not cut or len(f["fact"]) <= cut else f["fact"][:cut].rsplit(" ", 1)[0] + " ..."
    by = defaultdict(list)
    for f in facts:
        if not f["by"] and not iscall(f) and f["subject"] not in drop and keep(f):
            by[f["subject"]].append(f)
    subs = sorted(by, key=lambda s: -max(x["last"] for x in by[s]))
    t1 = [s for s in subs if s.split("/")[0] in ("tool", "user")]
    t2 = [s for s in subs if s.startswith(project + "/")]
    t3 = [s for s in subs if s not in t1 and s not in t2]
    shown, out = set(), ""
    for tier, frac in zip((t1, t2, t3), tiers):
        for s in tier:
            share, used = max(200, int(budget * frac / max(1, len(tier)))), 0
            out += "## %s (%d facts)\n" % (s, len(by[s]))
            for f in sorted(by[s], key=lambda f: -score(f)):
                line = "- [%s] %s" % (time.strftime("%Y-%m-%d", time.gmtime(f["first"])), text(f))
                if used + len(line.encode()) + 1 > share:
                    out += "  (more)\n"
                    break
                out += line + "\n"
                used += len(line.encode()) + 1
                shown.add(f["id"])
    return out, shown


def gold(facts, questions):
    """Which facts answer each question, and which mislead: asked once, kept beside the store."""
    cur = [f for f in facts if not f["by"] and not iscall(f)]
    key = hashlib.sha1(json.dumps([questions, sorted(f["id"] for f in cur)]).encode()).hexdigest()[:16]
    path = os.path.join(KDIR, "lab-gold.json")
    kept = json.load(open(path)) if os.path.exists(path) else {}
    if kept.get("key") == key:
        return kept["gold"]
    tok = lambda s: set(re.findall(r"[a-z0-9_]{3,}", s.lower()))
    df = Counter(w for f in cur for w in tok(f["fact"] + " " + f.get("topic", "")))
    sysp = ('You label facts of a knowledge base against a question about what holds NOW. For the question, with its current answer and the outdated one, say of the numbered facts: '
            '"answers": the facts that by themselves let a reader give the CURRENT answer; "misleads": the facts that would push a reader to the outdated answer or a wrong one '
            '(an outdated statement, or a caution about another project or one moment). Most are neither. Reply with JSON only: {"answers": [n, ...], "misleads": [n, ...]}')

    def one(q):
        qt = tok(q["q"] + " " + q["current"])
        c = [f for s, f in sorted(((sum(math.log(1 + len(cur) / df[w]) for w in qt & tok(f["fact"] + " " + f.get("topic", ""))), f) for f in cur), key=lambda x: -x[0])[:24] if s > 0]
        j = js(ask(sysp, "question: %s\ncurrent answer: %s\noutdated answer: %s\n\n" % (q["q"], q["current"], q["outdated"])
                   + "\n".join("%d. (%s) %s" % (i + 1, f["subject"], f["fact"]) for i, f in enumerate(c)), model="claude-sonnet-5-5"))
        pick = lambda k: [c[n - 1]["id"] for n in j.get(k, []) if isinstance(n, int) and 1 <= n <= len(c)]
        return dict(q, answers=pick("answers"), misleads=pick("misleads"))
    with ThreadPoolExecutor(6) as ex:
        g = list(ex.map(one, questions))
    json.dump(dict(key=key, gold=g), open(path, "w"), indent=1)
    return g


SYS = """You are an agent that works on code through a warm GHCi session. Below is your memory. <subjects> is what is known by subject, each fact dated. <chat>, when there is one, is the history of the session as one-line summaries, oldest first. Tools and settings changed over time: when statements conflict, the latest is the one that holds now.
Answer the question you are asked from the memory alone, as what holds NOW. Reply with JSON only: {"answer": "<one or two sentences; say you do not know if the memory does not say>"}

"""
JUDGE = 'You grade answers about what holds NOW. For each line label "given" as one of: "current" (states the current truth, without asserting the outdated way as valid now), "outdated" (asserts the outdated way as what holds now), "mixed" (asserts both, or hedges between them), "unknown" (says it does not know, or states neither). Reply with JSON only: {"labels": {"<n>": "<label>", ...}}'


def main():
    a = sys.argv[1:]
    if not a or a[0] not in ("score", "ask", "block"):
        sys.exit(__doc__)
    def opt(k, d):
        if k not in a:
            return d
        i = a.index(k)
        a.pop(i)
        return a.pop(i)
    cmd, project = a.pop(0), opt("--project", "dxf")
    qfile, pdir = opt("--questions", os.path.join(HERE, "tools", "know-lab", "dxf-questions.json")), opt("--dir", None)
    specs = a or ["now", "scope:scope=1"]
    facts = load()
    if cmd == "block":
        print(render(facts, project, **policy(specs[0])[1])[0], end="")
        return
    questions = json.load(open(qfile))
    blocks = {}
    for s in specs:
        n, kw = policy(s)
        blocks[n] = render(facts, project, **kw)
    if cmd == "score":
        g = gold(facts, questions)
        print("%-34s %6s %5s %10s %8s   (of %d questions)" % ("policy", "bytes", "facts", "answerable", "misled", len(g)))
        for n, (b, shown) in blocks.items():
            print("%-34s %6d %5d %10d %8d" % (n, len(b.encode()), len(shown), sum(1 for x in g if set(x["answers"]) & shown), sum(1 for x in g if set(x["misleads"]) & shown)))
        return
    chat = ""
    if pdir:
        v = subprocess.run([CLI, "view"], cwd=pdir, capture_output=True, text=True).stdout
        chat = v.split("</subjects>\n", 1)[-1]
    L = {n: "<subjects>\n" + b + "</subjects>\n" + chat for n, (b, _) in blocks.items()}
    jobs = [(n, k) for n in L for k in range(len(questions)) for _ in range(3)]
    one = lambda j: dict(layout=j[0], k=j[1], got=js(ask(SYS + L[j[0]], questions[j[1]]["q"])).get("answer", ""))
    with ThreadPoolExecutor(len(L)) as ex:          # (one of each first: the others read its prompt from the cache)
        res = list(ex.map(one, [(n, 0) for n in L]))
    with ThreadPoolExecutor(6) as ex:
        res += list(ex.map(one, [j for n in L for j in [x for x in jobs if x[0] == n][1:]]))
    by = defaultdict(list)
    for r in res:
        by[r["layout"]].append(r)

    def judge(n):
        body = "\n".join(json.dumps({"n": i, "question": questions[r["k"]]["q"], "current_truth": questions[r["k"]]["current"], "outdated": questions[r["k"]]["outdated"], "given": r["got"]}) for i, r in enumerate(by[n]))
        labs = [js(ask(JUDGE, body, model="claude-sonnet-5-5")).get("labels", {}) for _ in range(2)]
        for i, r in enumerate(by[n]):
            x, y = labs[0].get(str(i), "?"), labs[1].get(str(i), "?")
            r["label"] = x if x == y else "split"
    with ThreadPoolExecutor(len(L)) as ex:
        list(ex.map(judge, list(by)))
    print("%-34s current outdated mixed unknown split   of %d%s" % ("policy", 3 * len(questions), " (with the view of " + pdir + ")" if pdir else " (the block alone)"))
    for n, rs in by.items():
        c = Counter(r["label"] for r in rs)
        print("%-34s %7d %8d %5d %7d %5d" % (n, c["current"], c["outdated"], c["mixed"], c["unknown"], c["split"]))


if __name__ == "__main__":
    main()
