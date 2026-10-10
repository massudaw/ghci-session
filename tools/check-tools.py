#!/usr/bin/env python3
"""A project's own tools, proved live: tools/fake-claude.py stands for the `claude` command.

    tools/check-tools.py [-v]

In a project made for it, whose target declares `tools`, `builtin_tools`, `write_paths` and `instructions`:

  declared tools   the chat offers them beside its own, `ghci-session mcp` serves them (the default session's);
                   a call is an eval underneath, its arguments literals of their types -- a string holding quotes,
                   backslashes, a newline and a brace stays one string, a negative number is parenthesised, an
                   optional parameter not given is Nothing, a missing required one is refused -- and the history
                   has the eval, with the tool named.
  a picture        a tool whose answer ends with the path of an image inside the project shows the image to the
                   model (the history holds its marker); a path outside the project shows nothing.
  built-in tools   only those the target names are offered: no sh, no eval; a call of one that is not is refused.
  write boundary   write, edit and edits refuse a path outside `write_paths` -- by `..`, by a symlink -- and say
                   where writing is allowed; inside, they write.
  instructions     the target's file is in the system prompt when the chat is started without --instructions; the
                   flag wins; a restarted chat keeps them.

Exit status 0 when all hold. About a minute.
"""
import json, os, shutil, subprocess, sys, tempfile, time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tuicheck

DEMO = '''module Demo (greeting, selfTest, look, pic) where

greeting :: String
greeting = "hello"

selfTest :: IO ()
selfTest = putStrLn (if length greeting == 5 then "[PASS] greeting" else "[FAIL] greeting")

-- | What a declared tool is given, as the tool's answer.
look :: String -> Int -> Maybe Bool -> String
look s n b = "LOOK " ++ show (length s, n, b)

-- | The path it is given (the answer of a tool that draws).
pic :: String -> IO String
pic = pure
'''

TOOLS = [
    {"name": "look", "description": "Look at a view.", "expr": "Demo.look {view} {box} {flag}", "required": ["view", "box"],
     "params": {"view": {"type": "string", "description": "which view"}, "box": {"type": "integer", "description": "which box"},
                "flag": {"type": "boolean", "description": "optional"}}},
    {"name": "pic", "description": "A picture, by its path.", "expr": "Demo.pic {path}", "required": ["path"],
     "params": {"path": {"type": "string", "description": "where"}}},
]


def wait_for(path, text, secs=30, count=1):
    end = time.time() + secs
    while time.time() < end:
        try:
            if open(path, errors="replace").read().count(text) >= count:
                return True
        except OSError:
            pass
        time.sleep(0.1)
    return False


def mcp_session(proj, env):
    """The tool server of `ghci-session mcp`, spoken to a line of JSON at a time."""
    p = subprocess.Popen([tuicheck.CLI, "mcp"], cwd=proj, env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    n = [0]

    def ask(method, params):
        n[0] += 1
        p.stdin.write(json.dumps({"jsonrpc": "2.0", "id": n[0], "method": method, "params": params}) + "\n"); p.stdin.flush()
        return json.loads(p.stdout.readline()).get("result") or {}
    ask("initialize", {"protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": {"name": "check-tools", "version": "0"}})
    return p, ask


def main():
    verbose = "-v" in sys.argv
    tuicheck.build()
    d = tempfile.mkdtemp(prefix="ghs-tools-")
    extra = {"tools": TOOLS, "builtin_tools": ["read", "write", "edit", "edits", "ls", "status", "doc"], "write_paths": ["notes", "plan.md"],
             "instructions": "INSTR.md"}
    proj, session = tuicheck.project(d, session=True, extra=extra, more={"src/Demo.hs": DEMO, "INSTR.md": "PROJECT-RULE-ALPHA: say hello first.\n", "FLAG.md": "FLAG-RULE-BETA: say hi.\n"})
    os.makedirs(os.path.join(proj, "notes")); os.makedirs(os.path.join(proj, "src2"))
    os.symlink(os.path.join(proj, "src2"), os.path.join(proj, "notes", "link"))        # (a way out of notes)
    tuicheck.png(40, 30, os.path.join(proj, "pic.png"))
    tuicheck.png(40, 30, os.path.join(d, "outside.png"))
    fakebin = os.path.join(d, "bin")
    os.makedirs(fakebin)
    os.symlink(os.path.join(tuicheck.HERE, "tools", "fake-claude.py"), os.path.join(fakebin, "claude"))
    log = os.path.join(d, "claude.log")
    env = dict(os.environ, PATH=fakebin + os.pathsep + os.environ["PATH"], GHS_PROVIDER="claude", FAKE_CLAUDE_LOG=log)
    out, fifo = os.path.join(d, "chat.out"), os.path.join(d, "chat.in")
    os.mkfifo(fifo)
    checks = tuicheck.Checks("check-tools")
    check = checks.check
    chat = lambda *more: [tuicheck.CLI, "chat", "-s", session, "--settle", "0", "--usage", *more]
    read = lambda: open(out, errors="replace").read()
    hist = lambda: subprocess.run([tuicheck.CLI, "history", "-n", "400", "--full"], cwd=proj, env=env, capture_output=True, text=True).stdout
    p = p2 = mp = None
    try:
        # the tool server of the project (`ghci-session mcp`)
        mp, ask = mcp_session(proj, env)
        listed = {t["name"]: t for t in ask("tools/list", {}).get("tools", [])}
        check("mcp: the default session's declared tools are listed with their parameters, beside only the built-in tools its target names",
              set(listed) == {"look", "pic", "status", "doc"}
              and set(listed["look"]["inputSchema"]["properties"]) == {"view", "box", "flag", "session"} and listed["look"]["inputSchema"]["required"] == ["view", "box"]
              and listed["look"]["inputSchema"]["properties"]["box"]["type"] == "integer", sorted(listed))
        r = ask("tools/call", {"name": "look", "arguments": {"view": 'a"b\\c\n}', "box": -3}})
        txt = "".join(c.get("text", "") for c in r.get("content", []))
        check("mcp: a call is an eval, its arguments literals: quotes, a backslash, a newline and a brace stay one string, a negative number is parenthesised, an optional one not given is Nothing",
              "LOOK (7,-3,Nothing)" in txt and not r.get("isError"), r)
        r = ask("tools/call", {"name": "look", "arguments": {"box": 1}})
        check("mcp: a required parameter missing is refused, naming it", r.get("isError") and "view" in json.dumps(r), r)
        r = ask("tools/call", {"name": "look", "arguments": {"view": "v", "box": 1, "nope": 2}})
        check("mcp: a parameter that is not declared is refused", r.get("isError") and "nope" in json.dumps(r), r)
        r = ask("tools/call", {"name": "sh", "arguments": {"cmd": "echo shell-ran-$((1+1))"}})
        check("mcp: a built-in tool the target does not name is refused", "shell-ran-2" not in json.dumps(r), r)
        mp.kill(); mp = None

        # the chat, through the program
        keep = os.open(fifo, os.O_RDWR)
        p = subprocess.Popen(chat(), cwd=proj, env=env, stdin=keep, stdout=open(out, "w"), stderr=subprocess.STDOUT)
        send = lambda line: os.write(keep, (line + "\n").encode())
        done = [0]

        def turns(_n=None):      # (the next turn's end: one line is sent, and its turn awaited, at a time)
            done[0] += 1
            return wait_for(out, "[turn: ", 40, count=done[0])

        send('tool look {"view":"a\\"b\\\\c\\n}","box":-3,"flag":true} ;; say looked')
        check("chat: a declared tool is called and answered", turns(1) and "LOOK (7,-3,Just True)" in read(), read()[-500:])
        h = hist()
        check("the history holds the eval the tool made, with the tool's name (the audit)",
              '"tool":"look"' in h.replace(" ", "") and "Demo.look" in h and "(-3)" in h and "(Just True)" in h, h[-600:])
        check("the tools the program was offered: the project's own and the built-in ones named, no sh, no eval",
              any(l.startswith("tools offered:") and " mcp__" not in l and "look" in l and "pic" in l and "write" in l and " sh" not in l and " eval" not in l
                  for l in open(log).read().splitlines()), open(log).read())

        send('tool pic {"path":"pic.png"} ;; say pictured')
        turns(2)
        h = hist()
        check("a picture: the answer ends with the path of an image in the project; the model is shown the image (its marker is in the history), the path said",
              h.count("[image ") >= 1 and "pic.png" in h, h[-500:])
        n1 = hist().count("[image ")
        send('tool pic {"path":"%s"} ;; say outside' % os.path.join(d, "outside.png"))
        turns(3)
        check("a path outside the project shows no image", hist().count("[image ") == n1 and "outside" in read(), read()[-400:])

        send('tool sh {"cmd":"echo shell-ran-$((1+1))"} ;; say tried')
        turns(4)
        check("a built-in tool that is not offered is refused when called", "shell-ran-2" not in read(), read()[-400:])

        # the write boundary
        for line in ('tool write {"path":"src/x.txt","content":"no"} ;; say w1',
                     'tool write {"path":"notes/../src/z.txt","content":"no"} ;; say w2',
                     'tool write {"path":"notes/link/y.txt","content":"no"} ;; say w3',
                     'tool write {"path":"notes/a.txt","content":"yes"} ;; say w4',
                     'tool write {"path":"plan.md","content":"plan"} ;; say w5',
                     'tool edit {"path":"src/Demo.hs","old":"hello","new":"hullo"} ;; say w6',
                     'tool edits {"edits":[{"path":"plan.md","old":"plan","new":"plan2"},{"path":"src/Demo.hs","old":"hello","new":"hullo"}]} ;; say w7'):
            send(line); turns()
        t = read()
        files = lambda *p: os.path.join(proj, *p)
        check("write: outside write_paths is refused (a directory not named, a `..` out, a symlink out), and says where writing is allowed",
              not os.path.exists(files("src", "x.txt")) and not os.path.exists(files("src", "z.txt")) and not os.path.exists(files("src2", "y.txt"))
              and t.count("outside write_paths -- write, edit and edits may write only in notes, plan.md") >= 3, t[-900:])
        check("write: inside, a directory or a file named, it writes", open(files("notes", "a.txt")).read() == "yes" and open(files("plan.md")).read() == "plan", None)
        check("edit and edits: outside is refused, and edits writes nothing when any one of its files is refused",
              "hello" in open(files("src", "Demo.hs")).read() and open(files("plan.md")).read() == "plan", open(files("plan.md")).read())

        # instructions
        slog = open(log).read()
        check("instructions: the target's file is in the system prompt, the chat having been started without the flag",
              "PROJECT-RULE-ALPHA" in slog, slog[-400:])
        subprocess.run(chat("--restart"), cwd=proj, env=env, capture_output=True)
        time.sleep(2.0)
        send("say after the restart")
        turns(12)
        check("a restarted chat keeps them (the next turn's program has them again)", open(log).read().count("PROJECT-RULE-ALPHA") >= 2, open(log).read()[-400:])
        p.kill(); p.wait()
        log2 = os.path.join(d, "claude2.log")
        env2 = dict(env, FAKE_CLAUDE_LOG=log2)
        out2 = os.path.join(d, "chat2.out")
        p2 = subprocess.Popen(chat("--instructions", os.path.join(proj, "FLAG.md")), cwd=proj, env=env2, stdin=keep, stdout=open(out2, "w"), stderr=subprocess.STDOUT)
        send("say flagged")
        wait_for(out2, "[turn: ", 40)
        s2 = open(log2).read() if os.path.exists(log2) else ""
        check("instructions: the flag wins over the target's file", "FLAG-RULE-BETA" in s2 and "PROJECT-RULE-ALPHA" not in s2, s2[-400:])
        if verbose:
            print(read()); print(h)
    finally:
        for q in (p, p2, mp):
            if q is not None and q.poll() is None:
                q.kill()
        tuicheck.stop(proj, session)
        shutil.rmtree(d, ignore_errors=True)
    return checks.done()


if __name__ == "__main__":
    sys.exit(main())
