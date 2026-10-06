#!/usr/bin/env python3
"""An endless chat with an agent that works on a ghci-session as its sandbox and remembers through the
session's history (OptChat's turn loop, over the daemon's log, tree and view).

    python3 tools/chat.py                      # from the project: the chat, on the default session
    python3 tools/chat.py -s dev               # a session
    python3 tools/chat.py --once 'what was tried on Raster.depth last week?'
    python3 tools/chat.py --instructions AGENTS.md

Each message starts a FRESH model call: no conversation is carried over. The call sees the system prompt
(MASTER, VIEW_DOC, your instructions file), then the view -- the whole history as one-line summaries,
rendered by the daemon before the new message is logged -- then the message. Its tools are the session's
operations (eval, status, typecheck, reload, test, doc, census, bench, mem: the daemon logs each with its
answer), the memory's (zoom, date), and the agent's hands on the files (read, write, edit, ls, sh), which
this harness logs. Replies are logged as talk; thoughts are shown and never logged. A line typed while the
agent works reaches it between tool calls. The summaries are the daemon's business (summarize_cmd); a turn
waits for the view to settle first.

The model is an OpenAI-compatible chat endpoint: DeepSeek by default, its flash model (DEEPSEEK_API_KEY;
DEEPSEEK_MODEL / DEEPSEEK_BASE_URL override; OPENAI_API_KEY / OPENAI_BASE_URL / OPENAI_MODEL for another).
DeepSeek caches the prompt's prefix on its own, so the stable layout above -- system, then the view whose
start does not change from turn to turn, then the message -- is what makes a turn cheap.
"""
import argparse
import json
import os
import queue
import socket
import subprocess
import sys
import threading
import time

from openai import OpenAI

CAP = 30000          # characters of a tool result kept (head and tail), as the spec logs them
SAVE_WAIT = 45.0     # seconds a write or edit waits for the verdict of the reload it causes
MODEL = os.environ.get("DEEPSEEK_MODEL") or os.environ.get("OPENAI_MODEL") or "deepseek-v4-flash"
BASE = os.environ.get("DEEPSEEK_BASE_URL") or os.environ.get("OPENAI_BASE_URL") or "https://api.deepseek.com"
KEY = os.environ.get("DEEPSEEK_API_KEY") or os.environ.get("OPENAI_API_KEY")


# the session -----------------------------------------------------------------------------

def find_root(start):
    d = os.path.abspath(start)
    while True:
        if os.path.exists(os.path.join(d, "ghci-session.json")):
            return d
        parent = os.path.dirname(d)
        if parent == d:
            sys.exit("chat: no ghci-session.json at or above " + start)
        d = parent


class Session:
    """The daemon's socket protocol: one JSON line out, one back."""

    def __init__(self, root, name):
        self.root = root
        conf = json.load(open(os.path.join(root, "ghci-session.json")))
        self.state = os.path.join(root, conf.get("state_dir", ".ghci-session"))
        self.agent = conf.get("agent", "Agent")
        names = list(conf.get("targets", {})) + list(conf.get("sessions", {}))
        if name is None:
            up = [n for n in names if os.path.exists(os.path.join(self.state, n, "sock"))]
            name = up[0] if len(up) == 1 else conf.get("default", names[0])
        self.name = name
        self.sock = os.path.join(self.state, name, "sock")
        # what the daemon watches (its targets' dirs, and the build files): a save of one of these has a verdict
        members = conf.get("sessions", {}).get(name, [name])
        self.watched = [d for m in members for d in conf.get("targets", {}).get(m, {}).get("watch", [])]

    def watches(self, path):
        p = os.path.normpath(path)
        return (p.endswith(".cabal") or os.path.basename(p) == "cabal.project"
                or any(p == d or p.startswith(os.path.normpath(d) + os.sep) for d in self.watched))

    def verdict_at(self):
        """(the verdict's time stamp, its line, what is behind it) now -- None when no session answers. Behind a
        COMPILE-ERROR are the compiler's diagnostics, file:line:col and the message whole; behind a CHECK-FAIL
        the failing lines; behind an OK nothing."""
        r = self.request("status")
        j = r.get("status") or {}
        if r.get("ok") is None or not j:
            return None
        head = (r.get("out") or "").split("\n")[0]
        if j.get("kind") == "COMPILE-ERROR":
            ds = [d for d in j.get("diagnostics") or [] if isinstance(d, dict)]
            errs = [d for d in ds if d.get("severity") == "error"] or ds
            behind = [f"{d.get('file')}:{d.get('line')}:{d.get('col')}: {d.get('severity')}: {d.get('message', '').strip()}" for d in errs[:12]]
            if len(errs) > 12:
                behind.append(f"[... {len(errs) - 12} more]")
            behind = behind or [str(x) for x in j.get("detail") or []]
        elif j.get("kind") == "CHECK-FAIL":
            behind = [str(x) for x in j.get("detail") or []]
        else:
            behind = []
        return (j.get("at"), head, behind)

    def verdict_after(self, before, secs):
        """The verdict the session gives AFTER the one at `before` (a save's), waiting up to secs; else why not."""
        if before is None:
            return None
        t0 = time.time()
        while time.time() - t0 < secs:
            time.sleep(0.25)
            now = self.verdict_at()
            if now and now[0] is not None and now[0] > before[0]:
                return now
        return None

    def request(self, op, **args):
        req = {"op": op, **{k: v for k, v in args.items() if v is not None}}
        try:
            s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            s.connect(os.path.realpath(self.sock))
        except OSError:
            return {"ok": False, "out": f"{self.name}: no session running (ghci-session start {self.name})"}
        s.sendall((json.dumps(req) + "\n").encode())
        buf = b""
        while not buf.endswith(b"\n"):
            chunk = s.recv(1 << 16)
            if not chunk:
                break
            buf += chunk
        s.close()
        try:
            return json.loads(buf.decode("utf-8", "replace"))
        except json.JSONDecodeError:
            return {"ok": False, "out": "bad reply from the session"}

    def text(self, op, **args):
        """A request's answer as text, with the stale warning a client is given."""
        r = self.request(op, **args)
        stale = r.get("stale") or []
        warn = (f"[STALE: {len(stale)} watched file(s) differ from the loaded code (e.g. {stale[0]}): this answer "
                "is from the code before them; the session reloads a save by itself, see status]\n") if stale else ""
        return r.get("ok", False), warn + (r.get("out") or "")

    def log(self, kind, text):
        self.request("log", kind=kind, text=text)

    def view(self, wait):
        r = self.request("view", json=True, wait=wait)
        try:
            return json.loads(r.get("out") or "{}")
        except json.JSONDecodeError:
            return {"view": r.get("out", ""), "settled": False}


# the tools --------------------------------------------------------------------------------

def cap(text):
    if len(text) <= CAP:
        return text
    h = CAP // 2
    return text[:h] + f"\n[... {len(text) - 2 * h} characters cut ...]\n" + text[-h:]


def tool(name, desc, props, required=()):
    return {"type": "function", "function": {"name": name, "description": desc,
            "parameters": {"type": "object", "properties": props, "required": list(required)}}}


S = lambda d: {"type": "string", "description": d}       # noqa: E731
N = lambda d: {"type": "number", "description": d}       # noqa: E731
B = lambda d: {"type": "boolean", "description": d}      # noqa: E731

TOOLS = [
    tool("eval", "Evaluate a Haskell expression, or run a GHCi command (:t, :i, :browse, import M), against the LOADED code. The answer is what GHCi printed. ONE expression, command or declaration group per call: several lines are one GHCi block (:{ :}), so an import or a let on its own line fails to parse -- make a separate call for an import, and write `let a = 1; b = 2 in ...` on one line.",
         {"expr": S("the expression or command"), "timeout": N("seconds; a hung evaluation is interrupted (default 600)")}, ["expr"]),
    tool("status", "The session's verdict: OK -- CHECK-PASS, COMPILE-ERROR: n error(s), CHECK-FAIL: n failing; STALE(n) when watched sources differ from the loaded code.", {}),
    tool("typecheck", "Do the sources on disk typecheck? Nothing is loaded; the errors are listed.", {}),
    tool("reload", "Compile and load the sources on disk (a save does this by itself), then the tests unless test is false.", {"test": B("run the tests after a good load (default true)")}),
    tool("test", "Run the project's tests on the loaded code.", {"member": S("one member of a composed session")}),
    tool("doc", "Find a definition by name (a typo, a prefix or initials are fine), qualified, or by words of its type or comment: the signature, the comment above it, file:line.", {"query": S("the name or words"), "n": N("how many answers")}, ["query"]),
    tool("census", "What the heap holds: every CAF by what it retains (default), the Strings (mode strings), what a reload cannot drop (kept), sharing missed (dups), or one value alone (expr).", {"mode": S("cafs | strings | kept | dups | mem"), "expr": S("one value"), "top": N("how many entries")}),
    tool("bench", "Time an IO action in the session: wall, GC, allocation.", {"expr": S("the action")}, ["expr"]),
    tool("mem", "The repl's memory and its servers'.", {}),
    tool("zoom", "Open the line id+n of the view into the two lines of n/2 under it; n = 1 gives the message whole.", {"id": N("the line's first message"), "n": N("how many messages it covers")}, ["id", "n"]),
    tool("date", "The date and time of message id.", {"id": N("the message")}, ["id"]),
    tool("read", "A file of the project, with line numbers.", {"path": S("relative to the project"), "start": N("first line (default 1)"), "lines": N("how many (default 200)")}, ["path"]),
    tool("write", "Write a file of the project whole. A watched source (or a .cabal) is reloaded by the session itself and the answer carries the verdict of that reload: no status call is needed after it.", {"path": S("relative to the project"), "content": S("the whole content")}, ["path", "content"]),
    tool("edit", "Replace one exact, unique occurrence of a text in a file of the project. A watched source (or a .cabal) is reloaded by the session itself and the answer carries the verdict of that reload: no status call is needed after it.", {"path": S("relative to the project"), "old": S("the text as it is, unique in the file"), "new": S("its replacement")}, ["path", "old", "new"]),
    tool("ls", "List a directory of the project.", {"path": S("relative to the project (default: the root)")}),
    tool("sh", "Run a shell command in the project's directory: its output and status.", {"cmd": S("the command"), "timeout": N("seconds (default 120)")}, ["cmd"]),
]

SESSION_TOOLS = {"eval", "status", "typecheck", "reload", "test", "doc", "census", "bench", "mem"}   # the daemon logs these itself


# what a model calls an argument when it does not call it by its name
ALIASES = {"cmd": ["command", "shell", "script"], "path": ["file", "filename", "file_path", "filepath"], "content": ["text", "contents", "data"],
           "old": ["old_string", "old_text", "from", "search"], "new": ["new_string", "new_text", "to", "replace"], "expr": ["expression", "code", "command"],
           "query": ["q", "name", "words"], "lines": ["count", "n", "limit"], "start": ["from_line", "line", "offset"]}
REQUIRED = {t["function"]["name"]: t["function"]["parameters"]["required"] for t in TOOLS}


def arguments(name, a):
    """The call's arguments by their names (an alias is renamed), or what is missing."""
    a = dict(a)
    for k, als in ALIASES.items():
        if k not in a:
            for al in als:
                if al in a:
                    a[k] = a.pop(al)
                    break
    missing = [k for k in REQUIRED.get(name, []) if k not in a]
    return a, missing


def run_tool(sess, name, a):
    """(ok, text) of one tool call."""
    root = sess.root
    a, missing = arguments(name, a)
    if missing:
        return False, f"{name}: missing argument(s) {', '.join(missing)}; it takes {', '.join(REQUIRED.get(name, []))}"

    def inside(p):
        full = os.path.realpath(os.path.join(root, p or "."))
        if full != root and not full.startswith(root + os.sep):
            raise ValueError(f"{p}: outside the project")
        return full

    if name == "eval":
        # leading import lines are each their own command (in one block with the expression they do not parse)
        lines = [l for l in a.get("expr", "").strip().split("\n")]
        heads = []
        while len(lines) > 1 and (lines[0].startswith("import ") or lines[0].startswith(":")):
            heads.append(lines.pop(0))
        outs = []
        for h in heads:
            ok, out = sess.text("eval", expr=h, timeout=a.get("timeout"))
            if not ok or "error" in out:
                outs.append(h + "\n" + out)
        expr = "\n".join(lines)
        ok, out = sess.text("eval", expr=expr, timeout=a.get("timeout"))
        if "\n" in expr.strip() and "parse error" in out:
            out += "\n[hint: a multi-line eval is ONE GHCi block (:{ :}); a let on its own line does not parse there -- write `let a = 1; b = 2 in ...` on one line, or one declaration group per call]"
        return ok, "\n".join(outs + [out])
    if name == "status":
        return sess.text("status")
    if name == "typecheck":
        return sess.text("typecheck")
    if name == "reload":
        return sess.text("reload", check=a.get("test", True) is not False)
    if name == "test":
        return sess.text("check", member=a.get("member"))
    if name == "doc":
        return sess.text("doc", words=str(a.get("query", "")).split(), n=a.get("n"))
    if name == "census":
        mode = "value" if a.get("expr") else a.get("mode", "cafs")
        return sess.text("census", mode=mode, expr=a.get("expr"), top=a.get("top"))
    if name == "bench":
        return sess.text("bench", expr=a.get("expr", ""))
    if name == "mem":
        return sess.text("mem")
    if name == "zoom":
        return sess.text("zoom", id=int(a.get("id", -1)), n=int(a.get("n", 1)))
    if name == "date":
        return sess.text("date", id=int(a.get("id", -1)))
    try:
        if name == "read":
            p = inside(a["path"])
            with open(p, encoding="utf-8", errors="replace") as fh:
                ls = fh.read().split("\n")
            start = max(1, int(a.get("start", 1)))
            n = int(a.get("lines", 200))
            return True, "\n".join(f"{i + 1:5d}  {l}" for i, l in enumerate(ls) if start <= i + 1 < start + n) or "(empty)"
        if name == "write":
            p = inside(a["path"])
            before = sess.verdict_at() if sess.watches(a["path"]) else None
            os.makedirs(os.path.dirname(p), exist_ok=True)
            with open(p, "w", encoding="utf-8") as fh:
                fh.write(a["content"])
            return saved(sess, f"wrote {a['path']} ({len(a['content'])} characters)", before)
        if name == "edit":
            p = inside(a["path"])
            with open(p, encoding="utf-8") as fh:
                t = fh.read()
            k = t.count(a["old"])
            if k != 1:
                return False, f"{a['path']}: the text occurs {k} times; it must occur exactly once"
            before = sess.verdict_at() if sess.watches(a["path"]) else None
            with open(p, "w", encoding="utf-8") as fh:
                fh.write(t.replace(a["old"], a["new"], 1))
            return saved(sess, f"edited {a['path']}", before)
        if name == "ls":
            p = inside(a.get("path"))
            return True, "\n".join(sorted(e + ("/" if os.path.isdir(os.path.join(p, e)) else "") for e in os.listdir(p) if not e.startswith(".")))
        if name == "sh":
            r = subprocess.run(a["cmd"], shell=True, cwd=root, capture_output=True, text=True, timeout=float(a.get("timeout", 120)))
            out = r.stdout + (("\n[stderr]\n" + r.stderr) if r.stderr else "")
            return r.returncode == 0, (out.strip() or "(no output)") + (f"\n[exit {r.returncode}]" if r.returncode else "")
    except subprocess.TimeoutExpired:
        return False, "timed out"
    except KeyError as e:
        return False, f"{name}: missing argument {e}"
    except (OSError, ValueError) as e:
        return False, f"{type(e).__name__}: {e}"
    return False, f"unknown tool {name}"


def saved(sess, what, before):
    """A save's answer: what was written, and the verdict of the reload it caused (waited for), when the file
    is one the session watches. A verdict that is not there within the wait is said to be pending (a long
    compile): status has it later. The save itself is good either way."""
    if before is None:
        return True, what
    v = sess.verdict_after(before, SAVE_WAIT)
    if v is None:
        return True, what + f"\n[the session has not finished reloading this save after {SAVE_WAIT:.0f}s: status will have its verdict]"
    _, head, behind = v
    good = "ERROR" not in head and "FAIL" not in head
    # a bad verdict brings what is behind it (the errors, the failing tests): no status call is needed to see them
    return good, what + "\nverdict: " + head + "".join("\n" + l for l in behind)


# the prompts ---------------------------------------------------------------------------

def master(who):
    return f"""You are {who}, an AI agent that works for one user in a single chat that
never ends, on a Haskell project whose code is loaded in a warm GHCi session.
Do the user's tasks yourself, with your tools, following the user's
instructions at the end of this prompt: they say who the user is, how
their files are organized and how they want work done.

The session is your sandbox: eval runs against the loaded code in
milliseconds; a file you write or edit is reloaded by the session itself
and the write's answer carries that reload's verdict (COMPILE-ERROR,
CHECK-FAIL, or OK -- CHECK-PASS); status repeats the latest verdict
(STALE when the loaded code is behind the disk). Prefer an evaluation to
a guess, and the verdict to a belief that an edit is right. Fix a
COMPILE-ERROR before anything else: the session answers from the last
code that compiled until you do.

You keep no memory between turns. Each turn starts with the view below,
followed by the user's new message. Summaries keep little of tool
output, so say in your reply what you learned that will matter later.
Messages the user sends while you work reach you between tool calls."""


def view_doc(who):
    return f"""The view: the whole history of this session -- the chat between {who} and the
user, and everything done to the code by hand -- oldest first, inside
<chat> tags, as one-line summaries. Each line is

  id+n|text   the n messages from id on, summarized (newlines shown as spaces)

A summary tags each item with its kind: user (the user's words), talk
({who}'s replies), tool (a request: an evaluation, a reload, a save of a
file), echo (its result: the output, the verdict), note (memories from
before this chat). A short message is its own line, word for word. Recent
lines cover one message each; the older the messages, the more a line
covers. A message not summarized yet shows as "(not summarized yet: zoom
it)". No message appears in full, not even the last ones.

Navigating: zoom(id, n) opens line id+n into the two lines of n/2
messages it was made from; zoom(id, 1) gives message id in full. Zoom
whenever a summary only mentions something you need, such as what your
last reply said, a decision, a past attempt or where a file is, before
you act, guess or ask. date(id) gives the date and time of message id."""


# the turn loop --------------------------------------------------------------------------

def turn(client, sess, system, texts, pending, args):
    """One fresh call: the view, then the message; its tools until it ends."""
    v = sess.view(wait=args.settle)
    if not v.get("settled", True):
        print(f"[view: {v.get('parts')} lines, not all summarized yet; going on]", file=sys.stderr)
    for t in texts:
        sess.log("user", t)
    messages = [{"role": "system", "content": system},
                {"role": "user", "content": v.get("view", "") + "\n\n" + "\n\n".join(texts)}]
    for step in range(args.max_steps):
        t0 = time.time()
        r = client.chat.completions.create(model=args.model, max_tokens=args.max_tokens, messages=messages, tools=TOOLS, tool_choice="auto")
        m = r.choices[0].message
        u = r.usage
        if u is not None and args.usage:
            hit = getattr(u, "prompt_cache_hit_tokens", None)
            print(f"[usage: in {u.prompt_tokens}, out {u.completion_tokens}" + (f", cached {hit}" if hit is not None else "") + f", {time.time() - t0:.1f}s]", file=sys.stderr)
        thought = getattr(m, "reasoning_content", None)
        if thought:
            print("\n[thinking] " + thought.strip() + "\n", file=sys.stderr)
        if m.content and m.content.strip():
            print(m.content.strip() + "\n", flush=True)
            sess.log("talk", m.content.strip())
        messages.append({"role": "assistant", "content": m.content or "", **({"tool_calls": [tc.model_dump() for tc in m.tool_calls]} if m.tool_calls else {})})
        if not m.tool_calls:
            return
        for tc in m.tool_calls:
            name = tc.function.name
            try:
                a = json.loads(tc.function.arguments or "{}")
            except json.JSONDecodeError:
                a = {}
            print(f"> {name} {json.dumps(a, ensure_ascii=False)[:300]}", flush=True)
            if name not in SESSION_TOOLS:
                sess.log("tool", f"{name} {json.dumps(a, ensure_ascii=False)}")
            t1 = time.time()
            ok, out = run_tool(sess, name, a)
            out = cap(out)
            if args.usage:
                print(f"[tool: {name} {time.time() - t1:.1f}s]", file=sys.stderr)
            if name not in SESSION_TOOLS:
                sess.log("echo", ("" if ok else "ERROR: ") + out)
            print("  " + out[:600].replace("\n", "\n  ") + ("..." if len(out) > 600 else ""), flush=True)
            messages.append({"role": "tool", "tool_call_id": tc.id, "content": ("" if ok else "ERROR: ") + out})
        mid = drain(pending)
        if mid:
            for t in mid:
                sess.log("user", t)
            messages.append({"role": "user", "content": "\n\n".join(mid)})
    print("[the turn reached its step limit; stopping]", file=sys.stderr)


def drain(q):
    out = []
    while True:
        try:
            out.append(q.get_nowait())
        except queue.Empty:
            return out


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("-s", "--session")
    ap.add_argument("--root", default=".")
    ap.add_argument("--instructions", help="a file of the user's own instructions (an AGENTS.md), appended to the system prompt")
    ap.add_argument("--model", default=MODEL)
    ap.add_argument("--base-url", default=BASE)
    ap.add_argument("--max-tokens", type=int, default=8000)
    ap.add_argument("--max-steps", type=int, default=60, help="tool calls per turn")
    ap.add_argument("--settle", type=float, default=120, help="seconds to wait for the view's last lines to be summarized")
    ap.add_argument("--once", help="one message, then exit")
    ap.add_argument("--print-view", action="store_true", help="print the view and exit")
    ap.add_argument("--usage", action="store_true", help="print each step's token usage")
    args = ap.parse_args()

    root = find_root(args.root)
    sess = Session(root, args.session)
    if args.print_view:
        print(sess.view(wait=0).get("view", ""))
        return
    if not KEY:
        sys.exit("chat: no DEEPSEEK_API_KEY (or OPENAI_API_KEY) in the environment")
    client = OpenAI(api_key=KEY, base_url=args.base_url)
    who = sess.agent
    system = master(who) + "\n\n" + view_doc(who)
    if args.instructions:
        with open(args.instructions, encoding="utf-8") as fh:
            system += "\n\n" + fh.read().strip()

    v = sess.view(wait=0)
    print(v.get("view", ""))
    print(f"[{sess.name}: {v.get('messages')} messages, {v.get('parts')} lines; model {args.model}]\n")

    pending = queue.Queue()
    if args.once:
        turn(client, sess, system, [args.once], pending, args)
        return

    def reader():
        for line in sys.stdin:
            pending.put(line.rstrip("\n"))
        pending.put(None)
    threading.Thread(target=reader, daemon=True).start()
    while True:
        first = pending.get()
        if first is None:
            return
        texts = [first] + [t for t in drain(pending) if t is not None]
        texts = [t for t in texts if t.strip()]
        if texts:
            turn(client, sess, system, texts, pending, args)
        print("> ", end="", flush=True)


if __name__ == "__main__":
    main()
