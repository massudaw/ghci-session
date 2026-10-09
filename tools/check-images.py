#!/usr/bin/env python3
"""Does `chat --tui` draw the images a message names? Asked of Ghostty's own terminal, without a window.

    tools/check-images.py [-v] [--keep]

The chat's screen is run on a pseudo-terminal that says it is Ghostty (tools/tui-capture.py), against
tools/fake-llm.py, in a directory made for it (tools/tuicheck.py): the agent reads a small PNG, a line typed names a large one, the
screen is scrolled, then left. What the chat wrote is replayed into libghostty-vt (tools/vt-replay.c, built here
when it is older than its source), and what that terminal then holds is checked at each step: each image stored
and decoded, placed in the cells it should take, every placeholder cell saying its row and column, a picture cut
at the screen's edge when scrolled, nothing left behind at the end. Then the same on a terminal that shows no
pictures: nothing sent, the line alone.

Needs .bin/ghci-session (./build.sh), .bin/libghostty-vt.* (tools/libghostty-vt.sh), a C compiler and zlib.
What it does not see is the drawing itself: that is the application's, from what is checked here.
-v: the screen at each step; --keep: the directory is left (its path is said). Exit status 0 when all hold.
"""
import os, shutil, sys, tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import tuicheck
from tuicheck import png

PORT = 8797

# (lines enough before the pictures that the screen has somewhere to scroll back to)
FILLER = "\\\\n".join("line %d of what was said before" % i for i in range(1, 61))

SCRIPT = """
until waiting for a line
type say """ + FILLER + """\\r
until 1 turn;
type tool read {"path":"small.png"}\\r
until 2 turns;
mark small
type what is in big\\\\ shot.png ?\\r
until 3 turns;
mark both
key ctrl-up
key ctrl-up
key ctrl-up
key ctrl-up
key ctrl-up
key ctrl-up
until 6 lines back
mark scrolled
key pgup
wait 0.6
mark paged
key ctrl-c
wait 1
mark left
"""

def main():
    verbose, keep = "-v" in sys.argv, "--keep" in sys.argv
    tuicheck.build()
    d = tempfile.mkdtemp(prefix="ghs-images-")
    proj, session = tuicheck.project(d, session=False)
    png(64, 48, os.path.join(proj, "small.png"))
    png(2400, 1200, os.path.join(proj, "big shot.png"))
    fake, env, unset = tuicheck.fake_llm(PORT)
    chat = [tuicheck.CLI, "chat", "--tui", "-s", session, "--settle", "0"]
    checks = tuicheck.Checks("check-images")
    check = checks.check
    try:
        # a terminal that shows pictures
        at = tuicheck.run(chat, SCRIPT, proj, dict(env, TERM="xterm-ghostty"), unset).at
        if verbose:
            for label, s in at.items():
                print("---- %s" % label); print(s.show())
        img = lambda s, i: next((x for x in s.images if x["id"] == i), None)
        ph = lambda s, i: next((x for x in s.placeholders if x["id"] == i), None)
        s = at["small"]
        check("the image the agent read is stored by the terminal, decoded (64x48), as a placement of cells, at its own size (8x3)",
              img(s, 1) == {"id": 1, "stored": True, "width": 64, "height": 48, "virtual": True, "columns": 8, "rows": 3}, s.images)
        p = ph(s, 1)
        check("its cells are on the screen under its line: 8x3 of them, each saying its row and column",
              p is not None and (p["cells"], p["right"] - p["left"], p["bottom"] - p["top"], p["mark_rows"], p["mark_columns"], p["unreadable"]) == (24, 7, 2, [0, 2], [0, 7], 0)
              and any("image " in l for l in s.lines[p["top"] - 1:p["top"]]), p)
        s = at["both"]
        big = img(s, 2)
        check("the image a typed line names is stored too, from a smaller copy (1200 pixels wide), in the largest cells of its shape that fit (80x20)",
              big is not None and (big["stored"], big["virtual"], big["columns"], big["rows"]) == (True, True, 80, 20) and big["width"] <= 1200 and big["width"] == 2 * big["height"], s.images)
        p = ph(s, 2)
        check("its cells: 80x20, every row and column said", p is not None and (p["cells"], p["mark_rows"], p["mark_columns"], p["unreadable"]) == (1600, [0, 19], [0, 79], 0), p)
        check("the first picture is still there above it", ph(s, 1) is not None and ph(s, 1)["cells"] == 24 and ph(s, 1)["bottom"] < p["top"], ph(s, 1))
        before, s = p, at["scrolled"]
        p = ph(s, 2)
        check("scrolled back six lines, the picture is six rows lower and cut at the screen's edge (its first rows, not squeezed)",
              p is not None and p["top"] == before["top"] + 6 and p["mark_rows"][0] == 0 and p["mark_rows"][1] < 19 and p["cells"] == 80 * (p["mark_rows"][1] + 1) and p["unreadable"] == 0, p)
        s = at["paged"]
        check("scrolled back a page, no picture is on the screen and both are still the terminal's", s.placeholders == [] and len(s.images) == 2, (s.placeholders, s.images))
        s = at["left"]
        check("the screen left, the terminal holds no image", s.images == [] and s.placeholders == [], (s.images, s.placeholders))
        # one that does not (what a pane of top says it is)
        plain = tuicheck.run(chat, SCRIPT, proj, dict(env, TERM="xterm-256color"), unset)
        s, raw = plain.at["both"], plain.raw
        check("a terminal that shows no pictures is sent none: the line that names the image, and no more",
              s.images == [] and s.placeholders == [] and b"\x1b_G" not in raw and sum("image " in l for l in s.lines) >= 2, (s.images, s.placeholders))
    finally:
        fake.terminate()
        if keep:
            print("kept: " + d)
        else:
            shutil.rmtree(d, ignore_errors=True)
    return checks.done()

if __name__ == "__main__":
    sys.exit(main())
