#!/usr/bin/env python3
"""A fake OpenAI-compatible chat endpoint, for trying `ghci-session chat` and the summary compactor without a key.

    tools/fake-llm.py [PORT]           # default 8799; stdlib only
    export OPENAI_API_KEY=fake OPENAI_BASE_URL=http://127.0.0.1:8799 OPENAI_MODEL=fake
    ghci-session chat --tui            # or `top`, tab 7

Chat turns (a request that carries tools):
  - a line starting `eval EXPR` is answered with a call to the eval tool on EXPR;
  - a line starting `tool NAME [JSON]` calls any tool the request offers (`tool status`, `tool doc {"query": "grep"}`);
  - after a tool's result, it says what the tool answered;
  - anything else is echoed.
Compactions (a request without tools): one line, the head of the <input>. Every reply carries `usage`, so the
ledger (`ghci-session usage`) has something to sum. POST /chat/completions or /v1/chat/completions; GET / says it is up.
"""
import json, re, sys, time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

def text_of(m):
    c = m.get("content") or ""
    return c if isinstance(c, str) else " ".join(p.get("text", "") for p in c if isinstance(p, dict))

def reply(body):
    msgs, tools = body.get("messages", []), body.get("tools") or []
    last = msgs[-1] if msgs else {}
    if not tools:                                   # a compaction: one line from the <input>
        t = text_of(last)
        m = re.search(r"<input>(.*?)</input>", t, re.S)
        src = (m.group(1) if m else t).strip()
        return {"content": "fake summary: " + re.sub(r"\s+", " ", src)[:120]}
    if last.get("role") == "tool":
        return {"content": "The tool answered: " + text_of(last)[:300]}
    t = text_of(last).strip()
    # the user's line is the last line of a turn's message, after the view the first turn carries
    line = next((l.strip() for l in reversed(t.splitlines()) if l.strip()), "")
    names = [x.get("function", {}).get("name") for x in tools]
    m = re.match(r"eval\s+(.+)", line)
    if m and "eval" in names:
        return call("eval", {"expr": m.group(1)})
    m = re.match(r"tool\s+(\w+)\s*(\{.*\})?$", line)
    if m:
        if m.group(1) not in names:
            return {"content": "fake: no tool %r (offered: %s)" % (m.group(1), ", ".join(n for n in names if n))}
        return call(m.group(1), json.loads(m.group(2) or "{}"))
    return {"content": "fake: you said %r (try `eval 1 + 1` or `tool status`)" % line[:200]}

def call(name, args):
    return {"content": None, "tool_calls": [{"id": "call_%d" % int(time.time() * 1000), "type": "function",
                                              "function": {"name": name, "arguments": json.dumps(args)}}]}

class H(BaseHTTPRequestHandler):
    def _send(self, code, obj):
        b = json.dumps(obj).encode()
        self.send_response(code); self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(b))); self.end_headers(); self.wfile.write(b)

    def do_GET(self):
        self._send(200, {"ok": True, "fake": "llm"})

    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers.get("Content-Length", 0))) or b"{}")
        if not self.path.rstrip("/").endswith("/chat/completions"):
            return self._send(404, {"error": {"message": "no such route: " + self.path}})
        m = reply(body)
        n_in = len(json.dumps(body.get("messages", []))) // 4
        msg = {"role": "assistant", **m}
        out = {"id": "fake-%d" % int(time.time() * 1000), "object": "chat.completion", "model": body.get("model", "fake"),
               "choices": [{"index": 0, "message": msg, "finish_reason": "tool_calls" if m.get("tool_calls") else "stop"}],
               "usage": {"prompt_tokens": n_in, "completion_tokens": len(json.dumps(m)) // 4, "prompt_cache_hit_tokens": n_in // 2}}
        sys.stderr.write("%s -> %s\n" % (self.path, "tool call" if m.get("tool_calls") else m["content"][:70])); sys.stderr.flush()
        self._send(200, out)

    def log_message(self, *a): pass

if __name__ == "__main__":
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8799
    print("fake llm on http://127.0.0.1:%d" % port, file=sys.stderr)
    ThreadingHTTPServer(("127.0.0.1", port), H).serve_forever()
