#!/usr/bin/env python3
"""The compactor, for `summarize_cmd`: one summary line from the prompt the daemon writes on standard input.

    "summarize_cmd": "python3 tools/summarize.py"

The daemon's prompt is the instructions, then the context (`<chat>` ... `</chat>`), then the step (the
message to compress, or the two lines to merge, with a line of exactly 512 bytes for scale); a retry
appends the earlier answer and where the limit cut it. Everything before the first `<chat>` line is sent as
the system prompt and the rest as the user message, so the context -- the same prefix for every node of a
stretch -- is cached by the provider. The answer is the model's first non-empty line.

The model is an OpenAI-compatible chat endpoint. DeepSeek by default (its flash model; DEEPSEEK_API_KEY),
DEEPSEEK_MODEL / DEEPSEEK_BASE_URL override; OPENAI_API_KEY / OPENAI_BASE_URL / OPENAI_MODEL for another.
Medium effort: the spec found low effort overshoots the size far more often.
"""
import os
import sys

from openai import OpenAI

MODEL = os.environ.get("DEEPSEEK_MODEL") or os.environ.get("OPENAI_MODEL") or "deepseek-v4-flash"
BASE = os.environ.get("DEEPSEEK_BASE_URL") or os.environ.get("OPENAI_BASE_URL") or "https://api.deepseek.com"
KEY = os.environ.get("DEEPSEEK_API_KEY") or os.environ.get("OPENAI_API_KEY")


def split(prompt: str):
    """(system, user): the instructions, and the context with the step."""
    lines = prompt.split("\n")
    for i, l in enumerate(lines):
        if l.strip() == "<chat>":
            return "\n".join(lines[:i]).strip(), "\n".join(lines[i:]).strip()
    return "", prompt.strip()


def main():
    prompt = sys.stdin.read()
    system, user = split(prompt)
    if "--dry" in sys.argv:
        print("system:", len(system), "bytes; user:", len(user), "bytes; model:", MODEL, "at", BASE)
        return 0
    if not KEY:
        print("summarize: no DEEPSEEK_API_KEY (or OPENAI_API_KEY) in the environment", file=sys.stderr)
        return 2
    client = OpenAI(api_key=KEY, base_url=BASE)
    r = client.chat.completions.create(
        model=MODEL, max_tokens=400, temperature=0.3,
        messages=([{"role": "system", "content": system}] if system else []) + [{"role": "user", "content": user}])
    text = (r.choices[0].message.content or "").strip()
    line = next((l.strip() for l in text.split("\n") if l.strip()), "")
    if not line:
        print("summarize: the model answered nothing", file=sys.stderr)
        return 1
    print(line)
    return 0


if __name__ == "__main__":
    sys.exit(main())
