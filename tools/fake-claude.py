#!/usr/bin/env python3
"""A stand-in for the `claude` command, as `ghci-session chat` runs it (GHS_PROVIDER=claude): the same
arguments, the same stream of JSON lines in and out, the chat's tools called through the tool server it is
told to start -- and no model. Put it on the PATH as `claude` (tools/check-cli.py does) to try the chat's
subscription path, a restart in the middle of a turn, a turn gone on with from its log, without a sign-in.

What it does is in the last line of the message it is given, steps separated by ` ;; `:

    tool NAME {JSON}    call a tool of the chat's, and take its answer
    say TEXT            say TEXT
    pause SECONDS       do nothing for a while (a model that thinks)

and then it ends the turn, saying what the last step gave. A message that holds a <recent> block (a turn gone
on with from its log) is answered with what it found there. FAKE_CLAUDE_LOG: a file it notes its process id and
each message in. FAKE_CLAUDE_CTX=BASE,STEP: the context it says each model call has -- BASE tokens and STEP more
with every call (default 1000,0), 90% of it read from the cache: a context that grows, to see a turn roll over.
FAKE_CLAUDE_LIMIT=N,SECONDS: its Nth model call is refused -- the subscription's limit, which resets in SECONDS.
"""
import json, os, re, subprocess, sys, time

def out(obj):
    sys.stdout.write(json.dumps(obj) + "\n"); sys.stdout.flush()

def note(text):
    path = os.environ.get("FAKE_CLAUDE_LOG")
    if path:
        with open(path, "a") as f:
            f.write(text + "\n")

class Tools:
    """The chat's tool server: the command of --mcp-config, spoken to a line of JSON at a time."""
    def __init__(self, conf):
        srv = next(iter(json.loads(conf)["mcpServers"].values()))
        self.p = subprocess.Popen([srv["command"]] + srv["args"], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
        self.n = 0
        self.ask("initialize", {"protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": {"name": "fake-claude", "version": "0"}})
        self.names = [t["name"] for t in self.ask("tools/list", {}).get("tools", [])]
    def ask(self, method, params):
        self.n += 1
        self.p.stdin.write(json.dumps({"jsonrpc": "2.0", "id": self.n, "method": method, "params": params}) + "\n"); self.p.stdin.flush()
        line = self.p.stdout.readline()
        return (json.loads(line).get("result") or {}) if line else {}
    def call(self, name, args):
        r = self.ask("tools/call", {"name": name, "arguments": args})
        return "".join(c.get("text", "") for c in r.get("content", []) if c.get("type") == "text")

def main():
    argv = sys.argv[1:]
    arg = lambda k: argv[argv.index(k) + 1] if k in argv else None
    tools = Tools(arg("--mcp-config")) if arg("--mcp-config") else None
    note("pid %d" % os.getpid())
    note("system prompt ends: " + json.dumps((arg("--system-prompt") or "")[-300:]))      # (where a project's instructions are)
    note("tools offered: " + " ".join(tools.names if tools else []))
    out({"type": "system", "subtype": "init", "model": arg("--model"), "tools": tools.names if tools else []})
    calls = 0
    base, grow = (int(x) for x in os.environ.get("FAKE_CLAUDE_CTX", "1000,0").split(","))
    limit = [int(x) for x in os.environ["FAKE_CLAUDE_LIMIT"].split(",")] if os.environ.get("FAKE_CLAUDE_LIMIT") else None
    wr5, wr1 = (int(x) for x in os.environ.get("FAKE_CLAUDE_WRITE", "0,0").split(","))      # what each call writes to the cache, for five minutes and for an hour
    r5, r7 = int(time.time()) + 5 * 3600, int(time.time()) + 7 * 86400
    for raw in sys.stdin:
        try:
            m = json.loads(raw)
        except ValueError:
            continue
        content = m.get("message", {}).get("content", [])
        text = content if isinstance(content, str) else "".join(b.get("text", "") for b in content if b.get("type") == "text")
        note("message: %d characters" % len(text))
        if "<recent>" in text:      # a turn gone on with from its log: say what the log holds
            recent = text[text.index("<recent>"):text.index("</recent>")]
            kinds = re.findall(r"^\d+\|(\w+): ", recent, re.M)
            echoes = re.findall(r"^\d+\|echo: (.*)$", recent, re.M)
            steps = ["say resumed from the log: %d tool call(s), the last answer %r" % (kinds.count("tool"), (echoes[-1] if echoes else "")[:60])]
        else:
            line = next((l.strip() for l in reversed(text.splitlines()) if l.strip()), "")
            steps = [s.strip() for s in line.split(";;") if s.strip()]
        last = ""
        for step in steps:
            calls += 1
            if limit and calls == limit[0]:      # the subscription's limit: said, and the turn ends as an error
                out({"type": "rate_limit_event", "rate_limit_info": {"status": "rejected", "rateLimitType": "five_hour", "resetsAt": time.time() + limit[1]}})
                out({"type": "result", "subtype": "error_during_execution", "is_error": True, "result": "the usage limit is reached", "num_turns": calls, "usage": {}})
                return
            ctx = base + grow * calls
            out({"type": "rate_limit_event", "rate_limit_info": {"status": "allowed", "rateLimitType": "five_hour", "resetsAt": r5, "isUsingOverage": False,
                 "unifiedWindows": {"five_hour": {"utilization": 0.10 + 0.01 * calls, "resetsAt": r5}, "seven_day": {"utilization": 0.30, "resetsAt": r7}}}})
            out({"type": "stream_event", "event": {"type": "message_start", "message": {"usage": {"input_tokens": ctx - ctx * 9 // 10 - wr5 - wr1, "cache_read_input_tokens": ctx * 9 // 10, "cache_creation_input_tokens": wr5 + wr1,
                 "cache_creation": {"ephemeral_5m_input_tokens": wr5, "ephemeral_1h_input_tokens": wr1}}}}})
            mt = re.match(r"tool\s+(\w+)\s*(\{.*\})?$", step)
            if mt and tools:
                out({"type": "assistant", "message": {"content": [{"type": "tool_use", "id": "t%d" % calls, "name": "mcp__ghs__" + mt.group(1), "input": json.loads(mt.group(2) or "{}")}]}})
                last = tools.call(mt.group(1), json.loads(mt.group(2) or "{}"))
            elif step.startswith("pause "):
                time.sleep(float(step[6:]))
            elif step.startswith("say "):
                last = step[4:]
                out({"type": "assistant", "message": {"content": [{"type": "text", "text": last}]}})
            else:
                last = "fake claude: you said %r" % step[:200]
                out({"type": "assistant", "message": {"content": [{"type": "text", "text": last}]}})
            out({"type": "stream_event", "event": {"type": "message_delta", "delta": {}, "usage": {"output_tokens": 20}}})
        out({"type": "result", "subtype": "success", "is_error": False, "result": last, "num_turns": max(1, len(steps)),
             "usage": {"input_tokens": 100 * len(steps), "cache_read_input_tokens": 900 * len(steps), "cache_creation_input_tokens": 0, "output_tokens": 20 * len(steps)}})

if __name__ == "__main__":
    main()
