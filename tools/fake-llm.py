#!/usr/bin/env python3
"""A fake model endpoint, for trying `ghci-session chat` and the summary compactor without a key. It speaks both
protocols the tool does: OpenAI's chat completions and Anthropic's Messages API.

    tools/fake-llm.py [PORT]           # default 8799; stdlib only
    export OPENAI_API_KEY=fake OPENAI_BASE_URL=http://127.0.0.1:8799 OPENAI_MODEL=fake
    # or: export GHS_PROVIDER=anthropic ANTHROPIC_API_KEY=fake ANTHROPIC_BASE_URL=http://127.0.0.1:8799 ANTHROPIC_MODEL=fake
    ghci-session chat --tui            # or `top`, tab 7

Chat turns (a request that carries tools):
  - a line starting `eval EXPR` is answered with a call to the eval tool on EXPR;
  - a line starting `tool NAME [JSON]` calls any tool the request offers (`tool status`, `tool doc {"query": "grep"}`);
  - after a tool's result, it says what the tool answered;
  - anything else is echoed.
Compactions (a request without tools): one line, the head of the <input>. Every reply carries `usage`, so the
ledger (`ghci-session usage`) has something to sum. POST /chat/completions or /v1/chat/completions; GET / says it is up.

POST /v1/messages is the Anthropic side: the same behaviour, as content blocks (a `thinking` block with a signature
first, then `text` or `tool_use`). It is strict where the real API is -- a request it would answer 400 to is
answered 400 here, in the API's own error shape: roles that do not alternate, an empty text block, a tool_result
that is not at the head of its message or answers no tool_use of the turn before, more than four cache_control
marks, and a thinking block sent back changed (it remembers the ones it gave). FAKE_PREFIX_CACHE=0 turns off its
imitation of the prompt cache (usage then reports nothing read from it). FAKE_BUSY=N answers the first N
requests 429 with a Retry-After of one second, on either side.
"""
import hashlib, os
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

# the Anthropic side --------------------------------------------------------------------------

GIVEN = {}      # signature -> the thinking block as it was given: one sent back has to be the same
CACHED = set()  # hashes of prefixes that ended at a cache_control mark (the imitation of the prompt cache)

class Bad(Exception):
    pass

def a_text(content):
    if isinstance(content, str):
        return content
    return "".join(b.get("text", "") for b in content if isinstance(b, dict) and b.get("type") == "text")

def a_check(body):
    """What the real API would refuse, refused."""
    msgs = body.get("messages")
    if not isinstance(msgs, list) or not msgs:
        raise Bad("messages: at least one message is required")
    if not isinstance(body.get("max_tokens"), int) or body["max_tokens"] < 1:
        raise Bad("max_tokens: required")
    if msgs[0].get("role") != "user":
        raise Bad("messages: first message must use the user role")
    marks = 1 if body.get("cache_control") else 0
    for b in body.get("system") or [] if isinstance(body.get("system"), list) else []:
        marks += 1 if b.get("cache_control") else 0
    prev_role, prev_uses = None, set()
    for k, m in enumerate(msgs):
        role, content = m.get("role"), m.get("content")
        if role not in ("user", "assistant"):
            raise Bad("messages.%d.role: user or assistant" % k)
        if role == prev_role:
            raise Bad("messages.%d: roles must alternate (two %s messages in a row)" % (k, role))
        blocks = [{"type": "text", "text": content}] if isinstance(content, str) else content
        if not blocks:
            raise Bad("messages.%d.content: must not be empty" % k)
        seen_other, uses, results = False, set(), set()
        for b in blocks:
            t = b.get("type")
            marks += 1 if b.get("cache_control") else 0
            if t == "text":
                if not b.get("text", "").strip():
                    raise Bad("messages.%d: text content blocks must contain non-whitespace text" % k)
                seen_other = True
            elif t == "tool_result":
                if role != "user":
                    raise Bad("messages.%d: tool_result blocks belong in a user message" % k)
                if seen_other:
                    raise Bad("messages.%d: tool_result blocks must come before any other content" % k)
                if b.get("tool_use_id") not in prev_uses:
                    raise Bad("messages.%d: tool_result for %r, which is no tool_use of the previous message" % (k, b.get("tool_use_id")))
                results.add(b.get("tool_use_id"))
            elif t == "tool_use":
                if role != "assistant":
                    raise Bad("messages.%d: tool_use blocks belong in an assistant message" % k)
                if not isinstance(b.get("input"), dict):
                    raise Bad("messages.%d: tool_use.input must be an object" % k)
                uses.add(b.get("id"))
            elif t == "thinking":
                was = GIVEN.get(b.get("signature"))
                if was is None or was != b:
                    raise Bad("messages.%d: a thinking block was modified, or is not one this endpoint gave" % k)
            else:
                raise Bad("messages.%d: unknown block type %r" % (k, t))
        if role == "user" and prev_uses and results != prev_uses:
            raise Bad("messages.%d: tool_use ids %s have no tool_result in the next message" % (k, sorted(prev_uses - results)))
        prev_role, prev_uses = role, uses
    if marks > 4:
        raise Bad("a maximum of 4 blocks with cache_control may be provided (%d)" % marks)
    for t in body.get("tools") or []:
        if not t.get("name") or not isinstance(t.get("input_schema"), dict):
            raise Bad("tools: each needs a name and an input_schema")
    if body.get("temperature") is not None and str(body.get("model", "")).startswith("claude-opus"):
        raise Bad("temperature: not supported on this model")

def a_cache(body):
    """Tokens of the prompt read from the 'cache': the longest prefix that ended at a mark in an earlier request."""
    if os.environ.get("FAKE_PREFIX_CACHE") == "0":
        return 0
    h, read, size = hashlib.sha256(), 0, 0
    def feed(x, marked):
        nonlocal read, size
        raw = json.dumps({k: v for k, v in x.items() if k != "cache_control"}, sort_keys=True).encode()
        h.update(raw); size += len(raw) // 4
        key = h.hexdigest()
        if key in CACHED:
            read = size
        if marked:
            CACHED.add(key)
    for t in body.get("tools") or []:
        feed(t, bool(t.get("cache_control")))
    for b in body.get("system") or [] if isinstance(body.get("system"), list) else []:
        feed(b, bool(b.get("cache_control")))
    msgs = body.get("messages", [])
    for k, m in enumerate(msgs):
        blocks = [{"type": "text", "text": m["content"]}] if isinstance(m["content"], str) else m["content"]
        for i, b in enumerate(blocks):
            last = k == len(msgs) - 1 and i == len(blocks) - 1
            feed({"role": m["role"], **b}, bool(b.get("cache_control")) or (last and bool(body.get("cache_control"))))
    return read

def a_reply(body):
    msgs, tools = body["messages"], body.get("tools") or []
    last = msgs[-1]
    blocks = [{"type": "text", "text": last["content"]}] if isinstance(last["content"], str) else last["content"]
    sig = "sig_%d_%d" % (len(GIVEN), int(time.time() * 1000))
    thought = {"type": "thinking", "thinking": "fake thinking about %d message(s)" % len(msgs), "signature": sig}
    GIVEN[sig] = thought
    names = [t.get("name") for t in tools]
    results = [b for b in blocks if b.get("type") == "tool_result"]
    said = a_text(last["content"]).strip()
    if not tools:
        m = re.search(r"<input>(.*?)</input>", said, re.S)
        return [thought, {"type": "text", "text": "fake summary: " + re.sub(r"\s+", " ", (m.group(1) if m else said).strip())[:120]}], "end_turn"
    if results and not said:
        c = results[-1].get("content")
        return [thought, {"type": "text", "text": "The tool answered: " + (c if isinstance(c, str) else a_text(c))[:300]}], "end_turn"
    line = next((l.strip() for l in reversed(said.splitlines()) if l.strip()), "")
    use = lambda name, args: ([thought, {"type": "tool_use", "id": "toolu_%d" % int(time.time() * 1000000), "name": name, "input": args}], "tool_use")
    m = re.match(r"eval\s+(.+)", line)
    if m and "eval" in names:
        return use("eval", {"expr": m.group(1)})
    m = re.match(r"tool\s+(\w+)\s*(\{.*\})?$", line)
    if m and m.group(1) in names:
        return use(m.group(1), json.loads(m.group(2) or "{}"))
    if line == "refuse":
        return [], "refusal"
    return [thought, {"type": "text", "text": "fake: you said %r (try `eval 1 + 1` or `tool status`)" % line[:200]}], "end_turn"

BUSY = int(os.environ.get("FAKE_BUSY", "0"))

class H(BaseHTTPRequestHandler):
    def _send(self, code, obj):
        b = json.dumps(obj).encode()
        self.send_response(code); self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(b))); self.end_headers(); self.wfile.write(b)

    def do_GET(self):
        self._send(200, {"ok": True, "fake": "llm"})

    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers.get("Content-Length", 0))) or b"{}")
        global BUSY
        if BUSY > 0:        # FAKE_BUSY=N: the first N requests are answered 429, with a Retry-After of a second
            BUSY -= 1
            sys.stderr.write("%s -> 429 (busy: %d more)\n" % (self.path, BUSY)); sys.stderr.flush()
            b = json.dumps({"type": "error", "error": {"type": "rate_limit_error", "message": "busy (fake)"}}).encode()
            self.send_response(429); self.send_header("Content-Type", "application/json"); self.send_header("Retry-After", "1")
            self.send_header("Content-Length", str(len(b))); self.end_headers(); self.wfile.write(b)
            return
        if self.path.rstrip("/").endswith("/v1/messages"):
            if not (self.headers.get("x-api-key") or self.headers.get("Authorization")) or self.headers.get("anthropic-version") != "2023-06-01":
                return self._send(401, {"type": "error", "error": {"type": "authentication_error", "message": "x-api-key (or a bearer token) and anthropic-version: 2023-06-01 are required"}})
            try:
                a_check(body)
            except Bad as e:
                sys.stderr.write("%s -> 400 %s\n" % (self.path, e)); sys.stderr.flush()
                return self._send(400, {"type": "error", "error": {"type": "invalid_request_error", "message": str(e)}})
            read = a_cache(body)
            content, stop = a_reply(body)
            total = len(json.dumps([body.get("tools"), body.get("system"), body["messages"]])) // 4
            usage = {"input_tokens": max(0, total - read), "cache_read_input_tokens": read, "cache_creation_input_tokens": 0, "output_tokens": len(json.dumps(content)) // 4}
            out = {"id": "msg_fake_%d" % int(time.time() * 1000), "type": "message", "role": "assistant", "model": body.get("model", "fake"),
                   "content": content, "stop_reason": stop, "stop_sequence": None, "usage": usage}
            if stop == "refusal":
                out["stop_details"] = {"type": "refusal", "category": "fake", "explanation": "asked to"}
            marks = sum(1 for m in body["messages"] if not isinstance(m["content"], str) for b in m["content"] if b.get("cache_control"))
            sys.stderr.write("%s -> %s; %d message(s), %d mark(s) in them, %d of %d tokens from the cache\n" % (self.path, stop, len(body["messages"]), marks, read, total)); sys.stderr.flush()
            return self._send(200, out)
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
