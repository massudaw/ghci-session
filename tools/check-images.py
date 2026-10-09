#!/usr/bin/env python3
"""Does `chat --tui` draw the images a message names? Asked of Ghostty's own terminal, without a window.

    tools/check-images.py [-v] [--keep]

The chat's screen is run on a pseudo-terminal that says it is Ghostty (tools/tui-capture.py), against
tools/fake-llm.py, in a directory made for it: the agent reads a small PNG, a line typed names a large one, the
screen is scrolled, then left. What the chat wrote is replayed into libghostty-vt (tools/vt-replay.c, built here
when it is older than its source), and what that terminal then holds is checked at each step: each image stored
and decoded, placed in the cells it should take, every placeholder cell saying its row and column, a picture cut
at the screen's edge when scrolled, nothing left behind at the end. Then the same on a terminal that shows no
pictures: nothing sent, the line alone.

Needs .bin/ghci-session (./build.sh), .bin/libghostty-vt.* (tools/libghostty-vt.sh), a C compiler and zlib.
What it does not see is the drawing itself: that is the application's, from what is checked here.
-v: the screen at each step; --keep: the directory is left (its path is said). Exit status 0 when all hold.
"""
import json, os, shutil, struct, subprocess, sys, tempfile, time, zlib

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BIN = os.path.join(HERE, ".bin")
COLS, ROWS, CELL = 120, 45, (9, 18)
PORT = 8797

def png(w, h, path):
    """A gradient of w x h pixels."""
    rows = b"".join(b"\x00" + bytes(v for x in range(w) for v in (x * 255 // w, y * 255 // h, 128)) for y in range(h))
    chunk = lambda t, d: struct.pack(">I", len(d)) + t + d + struct.pack(">I", zlib.crc32(t + d))
    with open(path, "wb") as f:
        f.write(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0)) + chunk(b"IDAT", zlib.compress(rows, 6)) + chunk(b"IEND", b""))

def build():
    src, exe = os.path.join(HERE, "tools", "vt-replay.c"), os.path.join(BIN, "vt-replay")
    if not os.path.exists(exe) or os.path.getmtime(exe) < os.path.getmtime(src):
        r = subprocess.run([os.environ.get("CC", "cc"), "-O1", "-o", exe, src, "-I" + os.path.join(HERE, "ghostty-vt", "include"), "-L" + BIN, "-lghostty-vt", "-lz", "-Wl,-rpath," + BIN],
                           capture_output=True, text=True)
        if r.returncode != 0:
            sys.exit("check-images: vt-replay does not build (is .bin/libghostty-vt there? tools/libghostty-vt.sh):\n" + r.stderr)
    return exe

# (lines enough before the pictures that the screen has somewhere to scroll back to)
FILLER = "\\\\n".join("line %d of what was said before" % i for i in range(1, 61))

SCRIPT = """
wait 3
type say """ + FILLER + """\\r
wait 4
type tool read {"path":"small.png"}\\r
wait 5
mark small
type what is in big\\\\ shot.png ?\\r
wait 6
mark both
key ctrl-up
key ctrl-up
key ctrl-up
key ctrl-up
key ctrl-up
key ctrl-up
wait 1.5
mark scrolled
key pgup
wait 1.5
mark paged
key ctrl-c
wait 1.5
mark left
"""

def record(proj, term, out):
    r = subprocess.run([sys.executable, os.path.join(HERE, "tools", "tui-capture.py"), "--size", "%dx%d" % (COLS, ROWS), "--cell", "%dx%d" % CELL, "--cwd", proj, "--out", out,
                        "--env", "TERM=" + term, "--env", "GHS_PROVIDER=anthropic", "--env", "ANTHROPIC_API_KEY=fake", "--env", "ANTHROPIC_BASE_URL=http://127.0.0.1:%d" % PORT,
                        "--unset", "ANTHROPIC_AUTH_TOKEN", "--unset", "ANTHROPIC_MODEL", "--unset", "GHS_IMAGES",
                        "--", os.path.join(BIN, "ghci-session"), "chat", "--tui", "-s", "x", "--settle", "0"], input=SCRIPT, capture_output=True, text=True)
    if r.returncode != 0:
        sys.exit("check-images: the recording failed:\n" + r.stdout + r.stderr)
    return [(int(o), label) for o, label in (l.split("\t") for l in r.stdout.splitlines())]

def replay(exe, out, marks):
    args = [exe, out, str(COLS), str(ROWS), str(CELL[0]), str(CELL[1])]
    for o, _ in marks:
        args += ["--at", str(o)]
    r = subprocess.run(args, capture_output=True, text=True)
    if r.returncode != 0:
        sys.exit("check-images: vt-replay failed:\n" + r.stderr)
    return {label: json.loads(l) for (_, label), l in zip(marks, r.stdout.splitlines())}

def main():
    verbose, keep = "-v" in sys.argv, "--keep" in sys.argv
    if not os.path.exists(os.path.join(BIN, "ghci-session")):
        sys.exit("check-images: no .bin/ghci-session (./build.sh)")
    exe = build()
    d = tempfile.mkdtemp(prefix="ghs-images-")
    proj = os.path.join(d, "proj")
    os.makedirs(os.path.join(proj, ".ghci-session", "x"))
    with open(os.path.join(proj, "ghci-session.json"), "w") as f:
        json.dump({"targets": {"x": {"units": ["lib:x"], "watch": ["src"]}}}, f)
    png(64, 48, os.path.join(proj, "small.png"))
    png(2400, 1200, os.path.join(proj, "big shot.png"))
    fake = subprocess.Popen([sys.executable, os.path.join(HERE, "tools", "fake-llm.py"), str(PORT)], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    failed = []
    def check(what, ok, got=None):
        print("%s %s%s" % ("ok  " if ok else "FAIL", what, "" if ok else "\n       got: %s" % (got,)))
        if not ok:
            failed.append(what)
    try:
        time.sleep(1)
        # a terminal that shows pictures
        rec = os.path.join(d, "ghostty.bin")
        at = replay(exe, rec, record(proj, "xterm-ghostty", rec))
        if verbose:
            for label, s in at.items():
                print("---- %s" % label); print("\n".join(s["screen"]))
        img = lambda s, i: next((x for x in s["images"] if x["id"] == i), None)
        ph = lambda s, i: next((x for x in s["placeholders"] if x["id"] == i), None)
        s = at["small"]
        check("the image the agent read is stored by the terminal, decoded (64x48), as a placement of cells, at its own size (8x3)",
              img(s, 1) == {"id": 1, "stored": True, "width": 64, "height": 48, "virtual": True, "columns": 8, "rows": 3}, s["images"])
        p = ph(s, 1)
        check("its cells are on the screen under its line: 8x3 of them, each saying its row and column",
              p is not None and (p["cells"], p["right"] - p["left"], p["bottom"] - p["top"], p["mark_rows"], p["mark_columns"], p["unreadable"]) == (24, 7, 2, [0, 2], [0, 7], 0)
              and any("image " in l for l in s["screen"][p["top"] - 1:p["top"]]), p)
        s = at["both"]
        big = img(s, 2)
        check("the image a typed line names is stored too, from a smaller copy (1200 pixels wide), in the largest cells of its shape that fit (80x20)",
              big is not None and (big["stored"], big["virtual"], big["columns"], big["rows"]) == (True, True, 80, 20) and big["width"] <= 1200 and big["width"] == 2 * big["height"], s["images"])
        p = ph(s, 2)
        check("its cells: 80x20, every row and column said", p is not None and (p["cells"], p["mark_rows"], p["mark_columns"], p["unreadable"]) == (1600, [0, 19], [0, 79], 0), p)
        check("the first picture is still there above it", ph(s, 1) is not None and ph(s, 1)["cells"] == 24 and ph(s, 1)["bottom"] < p["top"], ph(s, 1))
        before, s = p, at["scrolled"]
        p = ph(s, 2)
        check("scrolled back six lines, the picture is six rows lower and cut at the screen's edge (its first rows, not squeezed)",
              p is not None and p["top"] == before["top"] + 6 and p["mark_rows"][0] == 0 and p["mark_rows"][1] < 19 and p["cells"] == 80 * (p["mark_rows"][1] + 1) and p["unreadable"] == 0, p)
        s = at["paged"]
        check("scrolled back a page, no picture is on the screen and both are still the terminal's", s["placeholders"] == [] and len(s["images"]) == 2, (s["placeholders"], s["images"]))
        s = at["left"]
        check("the screen left, the terminal holds no image", s["images"] == [] and s["placeholders"] == [], (s["images"], s["placeholders"]))
        # one that does not (what a pane of top says it is)
        rec = os.path.join(d, "plain.bin")
        marks = record(proj, "xterm-256color", rec)
        at = replay(exe, rec, marks)
        s = at["both"]
        raw = open(rec, "rb").read()
        check("a terminal that shows no pictures is sent none: the line that names the image, and no more",
              s["images"] == [] and s["placeholders"] == [] and b"\x1b_G" not in raw and sum("image " in l for l in s["screen"]) >= 2, (s["images"], s["placeholders"]))
    finally:
        fake.terminate()
        if keep:
            print("kept: " + d)
        else:
            shutil.rmtree(d, ignore_errors=True)
    print("check-images: %s" % ("all hold" if not failed else "%d FAILED" % len(failed)))
    return 1 if failed else 0

if __name__ == "__main__":
    sys.exit(main())
