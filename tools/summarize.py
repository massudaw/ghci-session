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

It is asked WITHOUT thinking. A reasoning model spends the answer's budget on its thoughts first, and at the
flash model's default effort a summary's budget (400 tokens) was most often all thoughts and no line: the
call ended on `length` with an empty answer, the daemon logged a failed node and asked again ten seconds
later, and each try was paid for. A summary is a compression, not a problem: without thinking the line
comes back in a second, and the daemon asks again anyway when it is too long. SUMMARIZE_EFFORT=low|high|max
turns thinking on at that effort, with a budget to match; whatever the setting, an answer cut off before
any text is asked again, without thinking and with four times the budget, before it is given up.
"""
import os
import sys

from openai import OpenAI

MODEL = os.environ.get("DEEPSEEK_MODEL") or os.environ.get("OPENAI_MODEL") or "deepseek-v4-flash"
BASE = os.environ.get("DEEPSEEK_BASE_URL") or os.environ.get("OPENAI_BASE_URL") or "https://api.deepseek.com"
KEY = os.environ.get("DEEPSEEK_API_KEY") or os.environ.get("OPENAI_API_KEY")
EFFORT = os.environ.get("SUMMARIZE_EFFORT", "none")     # none: no thinking; low | high | max: reasoning at that effort
DEEPSEEK = "deepseek" in BASE                             # (the thinking switch is DeepSeek's own request field)
TRIES = 3


def split(prompt: str):
    """(system, user): the instructions, and the context with the step."""
    lines = prompt.split("\n")
    for i, l in enumerate(lines):
        if l.strip() == "<chat>":
            return "\n".join(lines[:i]).strip(), "\n".join(lines[i:]).strip()
    return "", prompt.strip()


def ask(client, system, user, budget, think):
    """(the answer's text, why the model stopped) of one call: thinking on or off, within budget tokens."""
    extra = {}
    if think and EFFORT != "none":
        extra["reasoning_effort"] = EFFORT
    if DEEPSEEK:
        extra["extra_body"] = {"thinking": {"type": "enabled" if think else "disabled"}}
    r = client.chat.completions.create(
        model=MODEL, max_tokens=budget, temperature=0.3,
        messages=([{"role": "system", "content": system}] if system else []) + [{"role": "user", "content": user}],
        **extra)
    return (r.choices[0].message.content or "").strip(), r.choices[0].finish_reason


def main():
    prompt = sys.stdin.read()
    system, user = split(prompt)
    think = EFFORT != "none"
    budget = 4000 if think else 400
    if "--dry" in sys.argv:
        print("system:", len(system), "bytes; user:", len(user), "bytes; model:", MODEL, "at", BASE,
              "thinking:", EFFORT if think else "off", "budget:", budget)
        return 0
    if not KEY:
        print("summarize: no DEEPSEEK_API_KEY (or OPENAI_API_KEY) in the environment", file=sys.stderr)
        return 2
    client = OpenAI(api_key=KEY, base_url=BASE)
    text, why = "", ""
    for _ in range(TRIES):
        text, why = ask(client, system, user, budget, think)
        if text or why != "length":
            break
        # cut off before any text: the budget went to thoughts -- ask again without them, and with more
        think, budget = False, budget * 4
    line = next((l.strip() for l in text.split("\n") if l.strip()), "")
    if not line:
        print(f"summarize: the model answered nothing (finish_reason {why})", file=sys.stderr)
        return 1
    print(line)
    return 0


if __name__ == "__main__":
    sys.exit(main())
