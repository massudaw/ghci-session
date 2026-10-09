#!/usr/bin/env python3
"""Run a program on a terminal of its own, type at it from a script, and record what it writes.

    tools/tui-capture.py [--size 120x45] [--cell 9x18] [--env K=V]... [--unset K]... [--cwd DIR]
                         --out RECORDING [--script FILE] -- COMMAND ARG...

The program gets a pseudo-terminal of that size (columns x rows; the cell's pixels are what a terminal says a
cell measures, which a program that draws pictures asks). The script -- FILE, or standard input -- is a line a
step:

    wait SECONDS        let it run, recording
    type TEXT           send TEXT (\\r Enter, \\e Escape, \\t, \\xHH, \\\\ as written)
    key NAME            enter up down left right pgup pgdn home end esc tab backspace ctrl-up ctrl-down ctrl-X
    mark LABEL          note how many bytes it has written so far
    resize COLSxROWS    the terminal changes size (the program is told, as a window's would tell it)

Every byte written is in RECORDING; the marks are printed, a line each: OFFSET<TAB>LABEL -- and with them
OFFSET<TAB>@resize COLSxROWS where the size changed, `end` at the end, and last @exit STATUS (or @exit running:
the program had not ended, and was stopped).
tools/vt-replay.c says what a terminal holds at an offset of a recording.
"""
import fcntl, os, pty, select, struct, sys, termios, time

KEYS = {"enter": b"\r", "up": b"\x1b[A", "down": b"\x1b[B", "right": b"\x1b[C", "left": b"\x1b[D", "pgup": b"\x1b[5~", "pgdn": b"\x1b[6~",
        "home": b"\x1b[H", "end": b"\x1b[F", "esc": b"\x1b", "tab": b"\t", "backspace": b"\x7f", "ctrl-up": b"\x1b[1;5A", "ctrl-down": b"\x1b[1;5B"}

def key(name):
    if name in KEYS:
        return KEYS[name]
    if name.startswith("ctrl-") and len(name) == 6:
        return bytes([ord(name[5].lower()) & 0x1f])
    sys.exit("tui-capture: no key %r" % name)

def text(s):
    out, i = bytearray(), 0
    while i < len(s):
        c = s[i]
        if c == "\\" and i + 1 < len(s):
            n = s[i + 1]
            if n == "x" and i + 3 < len(s):
                out.append(int(s[i + 2:i + 4], 16)); i += 4; continue
            out += {"r": b"\r", "n": b"\n", "t": b"\t", "e": b"\x1b", "\\": b"\\"}.get(n, ("\\" + n).encode()); i += 2; continue
        out += c.encode(); i += 1
    return bytes(out)

def main(argv):
    size, cell, env, unset, cwd, out, script, cmd = (120, 45), (9, 18), {}, [], None, None, None, []
    i = 0
    while i < len(argv):
        a = argv[i]
        if a == "--":
            cmd = argv[i + 1:]; break
        v = argv[i + 1] if i + 1 < len(argv) else sys.exit("tui-capture: %s needs a value" % a)
        if a == "--size": size = tuple(int(x) for x in v.split("x"))
        elif a == "--cell": cell = tuple(int(x) for x in v.split("x"))
        elif a == "--env": env.update([v.split("=", 1)])
        elif a == "--unset": unset.append(v)
        elif a == "--cwd": cwd = v
        elif a == "--out": out = v
        elif a == "--script": script = v
        else: sys.exit(__doc__)
        i += 2
    if not cmd or not out:
        sys.exit(__doc__)
    steps = [l.strip() for l in (open(script) if script else sys.stdin) if l.strip() and not l.lstrip().startswith("#")]
    cols, rows = size
    pid, fd = pty.fork()
    if pid == 0:
        for k in unset: os.environ.pop(k, None)
        os.environ.update(env)
        if cwd: os.chdir(cwd)
        os.execvp(cmd[0], cmd)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, cols * cell[0], rows * cell[1]))
    raw, alive = bytearray(), [True]
    def pump(secs):
        end = time.time() + secs
        while alive[0] and time.time() < end:
            r, _, _ = select.select([fd], [], [], 0.05)
            if r:
                try:
                    d = os.read(fd, 65536)
                except OSError:
                    d = b""
                if not d:
                    alive[0] = False
                raw.extend(d)
    def send(b):
        try:
            os.write(fd, b)
        except OSError:
            alive[0] = False
    for step in steps:
        op, _, arg = step.partition(" ")
        if op == "wait": pump(float(arg))
        elif op == "type": send(text(arg)); pump(0.05)
        elif op == "key": send(key(arg.strip())); pump(0.05)
        elif op == "mark": print("%d\t%s" % (len(raw), arg.strip())); sys.stdout.flush()
        elif op == "resize":
            cols, rows = (int(x) for x in arg.strip().split("x"))
            print("%d\t@resize %dx%d" % (len(raw), cols, rows))
            fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, cols * cell[0], rows * cell[1]))
            pump(0.05)
        else: sys.exit("tui-capture: no step %r" % op)
    pump(0.2)
    print("%d\tend" % len(raw))
    done, status = os.waitpid(pid, os.WNOHANG)
    if done == 0:
        try:
            os.kill(pid, 15)
        except OSError:
            pass
        print("%d\t@exit running" % len(raw))
    else:
        print("%d\t@exit %d" % (len(raw), os.waitstatus_to_exitcode(status)))
    open(out, "wb").write(bytes(raw))

if __name__ == "__main__":
    main(sys.argv[1:])
