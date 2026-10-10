#!/usr/bin/env python3
"""The screens, run and typed at: `chat --tui` and `top`, checked by what a terminal shows of them.

    tools/check-tui.py [-v] [--keep] [chat] [top]

A project is made for it with a session of its own (a package of one module; cabal builds it, a few seconds),
and tools/fake-llm.py answers as the model. Each screen is run on a pseudo-terminal and typed at from a script
(tools/tui-capture.py); what it wrote is replayed into Ghostty's terminal (tools/vt-replay.c) and the screen
that terminal holds is checked at each step -- its text, where the cursor is, the styles of cells.

  chat  the header and the line to type on; the line's keys (Left, Ctrl-A/E/W/U, recall with Up and Down); a
        line sent and its answer, labelled; markdown as what it marks up; a write's diff in its colors and a
        source's edit with the session's verdict; scrolling back and following; a turn stopped with Esc, in a model call, in a command and in an evaluation; subagents started,
        one's report answered, the other stopped; a resize; leaving.
  top   the header and the tabs, each one's content; the history's cursor, a message opened and closed; the
        chat in a pane, of the pane's size, typed at through the monitor, and what it did in the history; a
        shell in a pane; a test asked for; a resize, of a pane too; leaving.

About twenty seconds (the scripts wait for what a screen shows, not for a time). -v: every screen (they are kept in a file when a check fails); --keep: the project
is left (its path is said). Exit status 0 when all hold. tools/check-images.py is the same for the pictures a chat draws.
"""
import json, os, shutil, subprocess, sys, tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tuicheck

PORT = 8796
FILLER = "\\\\n".join("line %d of what was said before" % i for i in range(1, 61))

CHAT = r"""
until waiting for a line
gone no session running
mark start
type hello wrld
wait 0.2
mark typed
key left
key left
key left
type o
wait 0.2
mark fixed
key ctrl-a
wait 0.2
mark home
key ctrl-e
key ctrl-w
wait 0.2
mark word
key ctrl-u
wait 0.2
mark cleared
type first line\r
until 1 turn;
mark sent
key up
wait 0.2
mark recalled
key down
wait 0.2
mark back
type say FILLER\r
until 2 turns;
type say # Title\\n**bold** and *it* and `code`\\n- item\\n| a | b |\\n|---|---|\\n| 1 | 2 |\r
until 3 turns;
mark markdown
type tool write {"path":"notes.txt","content":"one\\ntwo\\n"}\r
until 4 turns;
type tool edit {"path":"notes.txt","old":"two","new":"2"}\r
until 5 turns;
mark diff
type tool edit {"path":"src/Demo.hs","old":"hello","new":"howdy"}\r
until:30 6 turns;
mark source
key pgup
until lines back
mark scrolled
key end
gone lines back
mark followed
type wait 30\r
until model call 1 of the turn
mark working
key esc
until waiting for a line
mark stopped
type tool sh {"command":"sleep 31"}\r
until running sh
mark running
key esc
until waiting for a line
type tool eval {"expr":"Control.Concurrent.threadDelay 40000000"}\r
until running eval
mark evaluating
key esc
until waiting for a line
type tool eval {"expr":"6 * 7"}\r
until 7 turns;
mark evaluated
type say after the stop\r
until 8 turns;
mark after
type tool spawn {"tasks":["say the first is done","wait 33"]}\r
until 10 turns;
mark spawned
key esc
until subagent(s) stopped
mark substopped
type tool edit {"path":"notes.txt","old":"one","new":"1"}\r
until 11 turns;
type say the end\\n2\\n3\\n4\\n5\\n6\\n7\\nlast of it\r
until 12 turns;
resize 80x24
wait 1
mark resized
key ctrl-c
wait 1
mark left
""".replace("FILLER", FILLER)

TOP = r"""
until  user:
wait 0.6
mark start
type 2
type g
until memory  messages
mark view
type 3
until [time] boot
mark log
type 4
until generation
mark verdict
type 5
until demo chat
mark usage
type 6
until resident memory
mark heap
type 9
until facts hold
mark known
type 1
type g
until scrolled: f to follow
mark first
type G
type p
wait 0.3
mark cursor
type n
key enter
wait 0.3
mark opened
key enter
wait 0.3
mark closed
type T
until CHECK-PASS
mark tested
type 7
until waiting for a line
mark pane
type pane line\r
until fake: you said 'pane line'
mark panesent
resize 100x30
wait 1.5
mark paneresized
key ctrl-a
type 8
wait 1
type echo tui-$((6*7))\r
until tui-42
mark shell
key ctrl-a
type 1
type G
until  user: pane line
mark history
type i
type from the monitor
until to the chat> from the monitor
mark writing
type \r
until fake: you said 'from the monitor'
mark written
type 5
until chat: at rest
mark chatstate
key ctrl-c
wait 1
mark left
"""


def screens(name, rec):
    out = []
    for label, s in rec.at.items():
        out.append("---- %s: %s  cursor=%s %dx%d" % (name, label, s.cursor, s.cols, s.rows))
        out += [l for l in s.show().splitlines() if l[3:].strip()]
    return "\n".join(out) + "\n"


def sleeping():
    """Is the command a stopped turn was running still there?"""
    return subprocess.run(["pgrep", "-f", "sleep 31"], capture_output=True).returncode == 0


def check_chat(check, rec):
    at = rec.at
    s = at["start"]
    check("chat: the header names the screen, the session and the model, with the session's verdict in its color",
          "ghci-session chat demo" in s.lines[0] and "CHECK-PASS" in s.lines[0] and s.style(0, 1) == {"b"} and "fg2" in s.style_of("OK -- CHECK-PASS"), s.lines[0])
    check("chat: it is on the alternate screen, the view it read above, a line to type on at the bottom with the cursor on it",
          s.alternate and s.has("0+1|tool: start demo") and s.lines[s.rows - 1].startswith(">") and "b" in s.style(s.rows - 1, 0) and "waiting for a line" in s.lines[s.rows - 2] and s.cursor == (2, s.rows - 1), (s.cursor, s.lines[-2:]))
    last = lambda s: s.lines[s.rows - 1]
    check("chat: what is typed is on the line, the cursor after it", last(at["typed"]) == "> hello wrld" and at["typed"].cursor[0] == 12, (last(at["typed"]), at["typed"].cursor))
    check("chat: Left moves in the line and a letter goes in where the cursor is", last(at["fixed"]) == "> hello world" and at["fixed"].cursor[0] == 10, (last(at["fixed"]), at["fixed"].cursor))
    check("chat: Ctrl-A is the line's start", at["home"].cursor[0] == 2 and last(at["home"]) == "> hello world", at["home"].cursor)
    check("chat: Ctrl-E the end and Ctrl-W takes the word before", last(at["word"]).rstrip() == "> hello" and at["word"].cursor[0] == 8, (last(at["word"]), at["word"].cursor))
    check("chat: Ctrl-U empties it", last(at["cleared"]).rstrip() == ">" and at["cleared"].cursor[0] == 2, last(at["cleared"]))
    s = at["sent"]
    u, t = s.find(" user:  first line"), s.find(" talk:  fake: you said 'first line'")
    check("chat: a line sent is in the transcript with the answer under it, each labelled in its color; the line is empty again",
          u is not None and t is not None and u[0] < t[0] and s.style(u[0], 1) == {"b", "fg6"} and s.style(t[0], 1) == {"b", "fg2"} and last(s).rstrip() == ">", (u, t))
    check("chat: the header counts the turn and says what it cost", s.lines[1].strip().startswith("1 turn; the last: turn: 1 model call"), s.lines[1])
    check("chat: Up recalls the line sent, Down gives back the one being typed", last(at["recalled"]) == "> first line" and last(at["back"]).rstrip() == ">", (last(at["recalled"]), last(at["back"])))
    s = at["markdown"]
    top = s.row(" talk:  Title") or 0
    check("chat: markdown is shown as what it marks up -- a heading, bold, italic, code, an item, a table",
          s.style_of("Title", top) == {"b", "u"} and s.style_of("bold and", top) == {"b"} and s.style_of("it and code", top) == {"i"} and s.style_of("code", top + 1) == {"fg3"}
          and s.has("• item") and s.has("│ a │ b │") and s.has("├───┼───┤") and s.has("│ 1 │ 2 │") and not s.has("**bold**", top), s.lines[top:top + 6])
    s = at["diff"]
    e = s.row(" echo:  edited notes.txt") or 0
    check("chat: a tool's call and its answer are shown, and an edit's diff in its colors",
          s.has(' tool:  edit {"path": "notes.txt"') and s.style_of("--- a/notes.txt", e) == {"b"} and s.style_of("@@ -1,2 +1,2 @@", e) == {"fg6"}
          and s.style_of("-two", e) == {"fg1"} and s.style_of("+2", e) == {"fg2"} and s.style_of("tool:  edit") == {"b", "fg4"}, s.lines[e:e + 7])
    s = at["source"]
    check("chat: an edit of a source is answered with the session's verdict on it, and its diff",
          s.has("echo:  edited src/Demo.hs") and s.has("verdict: OK") and s.style_of('-greeting = "hello"') == {"fg1"} and s.style_of('+greeting = "howdy"') == {"fg2"}, [l for l in s.lines if "verdict" in l])
    s, f = at["scrolled"], at["source"]
    check("chat: PgUp scrolls the transcript back (the status line says how far), the header and the line staying",
          "lines back; End follows" in s.lines[s.rows - 2] and s.lines[2:s.rows - 2] != f.lines[2:f.rows - 2] and s.lines[0] == f.lines[0] and last(s).startswith(">")
          and f.has('+greeting = "howdy"') and not s.has('+greeting = "howdy"'), s.lines[s.rows - 2])
    s = at["followed"]
    check("chat: End follows the end again", "lines back" not in s.lines[s.rows - 2] and s.lines[2:s.rows - 2] == f.lines[2:f.rows - 2], s.lines[s.rows - 2])
    s, w = at["stopped"], at["working"]
    check("chat: while the model is asked, the status line says so and that Esc stops the turn; Esc does -- said, and a line is waited for again",
          "Esc stops the turn" in w.lines[w.rows - 2] and "model call" in w.lines[w.rows - 2] and s.has("the turn was stopped") and "waiting for a line" in s.lines[s.rows - 2]
          and not s.has("waited 30 seconds"), (w.lines[w.rows - 2], s.lines[s.rows - 2]))
    s, w = at["after"], at["running"]
    check("chat: a turn stopped while a tool runs takes the command with it, and the chat goes on: the next line is answered",
          "running sh" in w.lines[w.rows - 2] and s.has(" talk:  after the stop") and not sleeping(), (w.lines[w.rows - 2], sleeping()))
    s, w = at["evaluated"], at["evaluating"]
    e = s.find(' tool:  eval {"expr": "6 * 7"}')
    check("chat: a turn stopped while the session evaluates interrupts the evaluation: the session answers the next one at once",
          "running eval" in w.lines[w.rows - 2] and e is not None and s.has(" echo:  42", e[0]), e)
    s = at["spawned"]
    check("chat: spawn starts a subagent a task and answers at once with their names; what each does is noted as it does it",
          s.has("Started Sub-1, Sub-2.") and s.has("Sub-1: started") and s.has("Sub-2: started"), [l for l in s.lines if "Sub-" in l][:6])
    r = s.find("[Sub-1] report")
    check("chat: a subagent's last reply reaches the agent as a message of its own, and the agent answers it; the other is still at work",
          r is not None and s.has("the first is done", r[0]) and s.has(" talk:  fake: you said 'the first is done'", r[0]) and not s.has("[Sub-2] report"), r)
    s = at["substopped"]
    check("chat: Esc with the agent waiting stops the subagents still at work", s.has("1 subagent(s) stopped") and not s.has("[Sub-2] report"), [l for l in s.lines if "stopped" in l])
    s = at["resized"]
    check("chat: a smaller terminal is drawn for its size -- the header at its top, the line at its bottom, the end of the transcript between",
          (s.cols, s.rows) == (80, 24) and s.lines[0].startswith(" ghci-session chat demo") and s.lines[23].startswith(">") and "waiting for a line" in s.lines[22]
          and s.has("last of it") and s.cursor == (2, 23) and all(len(l) <= 80 for l in s.lines), (s.cursor, s.lines[0], s.lines[-2:]))
    s = at["left"]
    check("chat: Ctrl-C leaves, with the terminal as it was found", rec.status == "0" and not s.alternate and s.state["cursor_visible"], (rec.status, s.alternate))


def check_top(check, rec):
    at = rec.at
    tabs = lambda s: s.lines[2]
    lit = lambda s, name: "r" in s.style(2, tabs(s).find(name))
    s = at["start"]
    check("top: the header names the screen and the session, with its verdict; under it what the session uses, and the tabs",
          s.lines[0].startswith(" ghci-session top demo") and "OK" in s.lines[0] and "repl " in s.lines[1] and " MB" in s.lines[1] and "gen " in s.lines[1]
          and all(t in tabs(s) for t in ["1 history", "2 view", "3 log", "4 verdict", "5 usage", "6 heap", "7 chat", "8 shell"]), s.lines[:3])
    check("top: it opens on the history, at its end: what the chat did, numbered, with its time and kind",
          lit(s, "1 history") and not lit(s, "2 view") and s.has(" talk: The tool answered: edited notes.txt") and any(l.startswith("#") and " user: " in l for l in s.lines), tabs(s))
    w = s.find(" work: [Sub-1] report")
    check("top: a subagent's report is in the history as work, in its color, and its own doings are not",
          w is not None and "fg3" in s.style(w[0], w[1] + 1) and not s.has("Sub-1: started"), w)
    check("top: a diff in the history is in its colors, and a long message is cut and says so",
          {"fg1"} in s.styles_of("-one") and {"fg6"} in s.styles_of("@@ -1,2 +1,2 @@") and s.has("more lines; Enter opens"), s.styles_of("-one"))
    s = at["view"]
    check("top: 2 is the view the model reads, with the memory's numbers at its head", lit(s, "2 view") and not lit(s, "1 history") and s.has(" memory  messages ") and s.has("0+1|tool: start demo") and s.has("<chat>"), s.lines[2:5])
    s = at["log"]
    check("top: 3 is the daemon's log", lit(s, "3 log") and s.has("build: cabal repl") and s.has("[time] boot"), s.lines[3:6])
    s = at["verdict"]
    check("top: 4 is the verdict and what is behind it", lit(s, "4 verdict") and s.lines[3].startswith("OK -- ") and s.has("generation ") and s.has("members"), s.lines[3:5])
    s = at["usage"]
    check("top: 5 is what the model calls cost", lit(s, "5 usage") and s.has("calls") and s.has("demo chat") and s.has("total"), s.lines[3:6])
    s = at["heap"]
    check("top: 6 is the memory over time", lit(s, "6 heap") and s.lines[3].strip().startswith("resident memory  repl ") and s.has("█"), s.lines[3:5])
    s = at["known"]
    check("top: 9 is what is known by subject: the tool's and the user's first, then the project's", lit(s, "9 known") and "2 facts hold, 1 replaced" in s.lines[3] and s.has("## user/rules (1 fact)")
          and s.has("Use the test tool for the routine check.") and s.has("## proj/rules (1 fact)") and not s.has("Run cabal test."), s.lines[3:10])
    s = at["first"]
    check("top: g goes to the history's start, and the tabs' line says it no longer follows", s.lines[3].startswith("#0 ") and "tool: start demo" in s.lines[3] and "scrolled" in tabs(s), (s.lines[3], tabs(s)))
    s = at["cursor"]
    cur = [y for y in range(3, s.rows - 1) if s.lines[y].startswith("#") and "r" in s.style(y, 0)]
    check("top: G follows the end again, and p moves the cursor to a message before (one is marked, not the last)",
          len(cur) == 1 and any(s.lines[y].startswith("#") for y in range(cur[0] + 1, s.rows - 1)), cur)
    o, c = at["opened"], at["closed"]
    whole = lambda s: any(l.strip() == "last of it" for l in s.lines)
    check("top: n is the next message, and Enter opens the one under the cursor (it was cut: its last line is shown), Enter again closes it",
          s.has("more lines; Enter opens") and not whole(s) and whole(o) and not whole(c) and c.has("more lines; Enter opens"),
          [l for l in o.lines if "more lines" in l or "last of it" in l])
    s = at["tested"]
    check("top: T asks the session for its test, and the verdict in the header is the test's", "CHECK-PASS" in s.lines[0] and "fg2" in s.style_of("OK -- CHECK-PASS"), s.lines[0])
    s = at["pane"]
    check("top: 7 is the chat, running in a pane of the pane's size -- its header under the tabs, its line at the bottom, the monitor's keys said",
          lit(s, "7 chat") and s.lines[3].startswith(" ghci-session chat demo") and s.lines[s.rows - 2].startswith(">") and "waiting for a line" in s.lines[s.rows - 3]
          and "Ctrl-a" in s.lines[s.rows - 1] and s.cursor == (2, s.rows - 2), (s.cursor, s.lines[3], s.lines[-3:]))
    s = at["panesent"]
    check("top: keys go to the program in the pane: a line typed there is sent, and answered", s.has(" user:  pane line") and s.has(" talk:  fake: you said 'pane line'"), None)
    s = at["paneresized"]
    check("top: a smaller terminal: the monitor is drawn for it and the pane's program for the pane -- its line at the new bottom",
          (s.cols, s.rows) == (100, 30) and s.lines[0].startswith(" ghci-session top demo") and s.lines[3].startswith(" ghci-session chat demo") and s.lines[28].startswith(">")
          and "waiting for a line" in s.lines[27] and s.cursor == (2, 28) and all(len(l) <= 100 for l in s.lines), (s.cursor, s.lines[26:]))
    s = at["shell"]
    check("top: Ctrl-a then 8 is a shell in a pane: a command typed is run", lit(s, "8 shell") and any(l.strip() == "tui-42" for l in s.lines), s.lines[3:7])
    s = at["history"]
    check("top: Ctrl-a then 1 is the history again, with what the chat in the pane did", lit(s, "1 history") and s.has(" user: pane line") and s.has(" talk: fake: you said 'pane line'"), s.lines[-4:])
    s, t = at["writing"], at["written"]
    check("top: i writes a line for the session's chat on the bottom line; Enter sends it to the chat that is running, which answers it",
          s.lines[s.rows - 1].startswith(" to the chat> from the monitor") and t.has(" user: from the monitor") and t.has(" talk: fake: you said 'from the monitor'"), (s.lines[-1], t.lines[-4:]))
    s = at["usage"]
    check("top: 5 says whether a chat runs (none, here: it was left) and its last turn's tool calls", s.has("chat: not running (the last turn: ") and s.has("tool call"), s.lines[3:6])
    s = at["chatstate"]
    check("top: 5 shows the running chat at rest, its last turn's tool calls and its last words, and the rollover controller's state",
          lit(s, "5 usage") and s.has("chat: at rest; the last turn: ") and s.has("said: fake: you said 'from the monitor'") and s.has("rollover: at ") and s.has("40k") and s.has("1500 tokens a call") and s.has("20% learned again")
          and s.has("the cache lasts at least 15 and at most 55 minutes") and s.has("rot: 10% of the last 20 reads were repeats"), s.lines[3:9])
    s = at["left"]
    check("top: Ctrl-C leaves, with the terminal as it was found", rec.status == "0" and not s.alternate and s.state["cursor_visible"], (rec.status, s.alternate))


def main():
    args = sys.argv[1:]
    verbose, keep = "-v" in args, "--keep" in args
    which = [a for a in args if not a.startswith("-")] or ["chat", "top"]
    tuicheck.build()
    d = tempfile.mkdtemp(prefix="ghs-tui-")
    proj, session = tuicheck.project(d, session=True)
    fake, env, unset = tuicheck.fake_llm(PORT)
    # (what is known is read from a directory of the check's own, not the person's)
    know = os.path.join(d, "know")
    os.makedirs(know)
    with open(os.path.join(know, "facts.jsonl"), "w") as f:
        for i, sub, text in (("a", "user/rules", "Run cabal test."), ("b", "user/rules", "Use the test tool for the routine check."), ("c", "proj/rules", "No data files are committed.")):
            f.write(json.dumps({"id": i, "subject": sub, "topic": "t", "fact": text, "first": 1e9, "last": 1e9, "src": "proj/demo:1+1", "replaces": []}) + "\n")
        f.write(json.dumps({"mark": "by", "id": "a", "by": "b"}) + "\n")
    env = dict(env, TERM="xterm-256color", SHELL="/bin/sh", PS1="$ ", GHS_KNOWLEDGE=know)
    checks = tuicheck.Checks("check-tui")
    seen = ""
    try:
        # (the monitor shows what the chat did: the chat is run first either way)
        rec = tuicheck.run([tuicheck.CLI, "chat", "--tui", "-s", session, "--settle", "0"], CHAT, proj, env, unset)
        if "chat" in which:
            seen += screens("chat", rec)
            check_chat(checks.check, rec)
        if "top" in which:
            # (the model here is the fake one, which the controller learns nothing from: its state is given, as the chat keeps it)
            hist = os.path.join(proj, ".ghci-session", session, "history")
            os.makedirs(hist, exist_ok=True)
            with open(os.path.join(hist, "roll.json"), "w") as f:
                json.dump({"S": 40000, "g": 1500, "relearn": 0.2, "said": 100000, "lo": 900, "hi": 3300, "relid": 0, "rot": 0.1}, f)
            rec = tuicheck.run([tuicheck.CLI, "top", session], TOP, proj, env, unset)
            seen += screens("top", rec)
            check_top(checks.check, rec)
    finally:
        fake.terminate()
        tuicheck.stop(proj, session)
        if keep:
            print("kept: " + d)
        else:
            shutil.rmtree(d, ignore_errors=True)
    if verbose:
        print(seen, end="")
    elif checks.failed:
        # (what was on the screens, kept: a check that fails once in many runs is looked at there)
        with tempfile.NamedTemporaryFile("w", prefix="check-tui-screens-", suffix=".txt", delete=False) as f:
            f.write(seen)
        print("the screens at each step: " + f.name)
    return checks.done()


if __name__ == "__main__":
    sys.exit(main())
