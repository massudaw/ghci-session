"""What tools/check-tui.py and tools/check-images.py share: a screen run on a pseudo-terminal and typed at
(tools/tui-capture.py), what it wrote replayed into Ghostty's terminal (tools/vt-replay.c, built here when it is
older than its source), and the terminal's state at each mark as a `Screen` to ask things of."""
import json, os, shutil, struct, subprocess, sys, tempfile, time, zlib

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BIN = os.path.join(HERE, ".bin")
CLI = os.path.join(BIN, "ghci-session")


def build():
    """.bin/vt-replay, built if it is not there or is older than its source."""
    src, exe = os.path.join(HERE, "tools", "vt-replay.c"), os.path.join(BIN, "vt-replay")
    if not os.path.exists(CLI):
        sys.exit("no .bin/ghci-session (./build.sh)")
    if not os.path.exists(exe) or os.path.getmtime(exe) < os.path.getmtime(src):
        r = subprocess.run([os.environ.get("CC", "cc"), "-O1", "-o", exe, src, "-I" + os.path.join(HERE, "ghostty-vt", "include"), "-L" + BIN, "-lghostty-vt", "-lz", "-Wl,-rpath," + BIN],
                           capture_output=True, text=True)
        if r.returncode != 0:
            sys.exit("vt-replay does not build (is .bin/libghostty-vt there? tools/libghostty-vt.sh):\n" + r.stderr)
    return exe


class Screen:
    """A terminal's state at a mark: `lines` (its rows' text), `cursor` (column, row), `alternate`, `images`,
    `placeholders`, and the style of a cell."""

    def __init__(self, state):
        self.state, self.lines = state, state["screen"]
        self.cols, self.rows = state["cols"], state["rows"]
        self.cursor, self.alternate = tuple(state["cursor"]), state["alternate"]
        self.images, self.placeholders = state["images"], state["placeholders"]

    def find(self, text, top=0, bottom=None):
        """Where a text first is, (row, column), from a row on; None when it is nowhere."""
        for y in range(top, len(self.lines) if bottom is None else bottom):
            x = self.lines[y].find(text)
            if x >= 0:
                return (y, x)
        return None

    def has(self, text, top=0):
        return self.find(text, top) is not None

    def row(self, text):
        at = self.find(text)
        return None if at is None else at[0]

    def style(self, row, col):
        """The style of a cell, as a set: b d i u s r, fgN, bgN, fg#RRGGBB ... (empty: plain)."""
        for x, n, st in self.state["styles"][row]:
            if x <= col < x + n:
                return set(st.split())
        return set()

    def style_of(self, text, top=0):
        """The style of the first cell of a text (None when it is nowhere)."""
        at = self.find(text, top)
        return None if at is None else self.style(*at)

    def styles_of(self, text):
        """The styles of the first cell of a text, wherever it is."""
        return [self.style(y, l.find(text)) for y, l in enumerate(self.lines) if text in l]

    def show(self):
        return "\n".join("%2d|%s" % (i, l) for i, l in enumerate(self.lines))


class Recording:
    def __init__(self, at, raw, status, marks):
        self.at, self.raw, self.status, self.marks = at, raw, status, marks


def run(cmd, script, cwd, env=None, unset=(), size=(120, 45), cell=(9, 18), out=None):
    """Run a command on a terminal of that size, typed at by the script; the terminal's state at each mark."""
    exe = build()
    tmp = None
    if out is None:
        tmp = tempfile.NamedTemporaryFile(suffix=".bin", delete=False)
        tmp.close()
        out = tmp.name
    args = [sys.executable, os.path.join(HERE, "tools", "tui-capture.py"), "--size", "%dx%d" % size, "--cell", "%dx%d" % cell, "--cwd", cwd, "--out", out, "--replay", exe]
    for k, v in (env or {}).items():
        args += ["--env", "%s=%s" % (k, v)]
    for k in unset:
        args += ["--unset", k]
    r = subprocess.run(args + ["--"] + list(cmd), input=script, capture_output=True, text=True)
    if r.returncode != 0:
        sys.exit("the recording failed:\n" + r.stdout + r.stderr)
    if r.stderr.strip():        # (a wait that was not met: said, and the checks after it say the rest)
        sys.stderr.write(r.stderr)
    marks = [(int(o), label) for o, label in (l.split("\t", 1) for l in r.stdout.splitlines())]
    status = next((label.split(" ", 1)[1] for _, label in marks if label.startswith("@exit ")), "running")
    replay = [exe, out, str(size[0]), str(size[1]), str(cell[0]), str(cell[1])]
    labels = []
    for o, label in marks:
        if label.startswith("@resize "):
            replay += ["--resize", str(o), label.split(" ", 1)[1]]
        elif not label.startswith("@"):
            replay += ["--at", str(o)]
            labels.append(label)
    p = subprocess.run(replay, capture_output=True, text=True)
    if p.returncode != 0:
        sys.exit("vt-replay failed:\n" + p.stderr)
    raw = open(out, "rb").read()
    if tmp is not None:
        os.unlink(out)
    return Recording({label: Screen(json.loads(l)) for label, l in zip(labels, p.stdout.splitlines())}, raw, status, marks)


class Checks:
    """The checks made, said as they are made; `done` is the exit status."""

    def __init__(self, name):
        self.name, self.failed, self.count = name, [], 0

    def check(self, what, ok, got=None):
        self.count += 1
        print("%s %s%s" % ("ok  " if ok else "FAIL", what, "" if ok or got is None else "\n       got: %s" % (got,)))
        if not ok:
            self.failed.append(what)
        return ok

    def done(self):
        print("%s: %s" % (self.name, "all %d hold" % self.count if not self.failed else "%d of %d FAILED" % (len(self.failed), self.count)))
        return 1 if self.failed else 0


def png(w, h, path):
    """A gradient of w x h pixels, as a PNG."""
    rows = b"".join(b"\x00" + bytes(v for x in range(w) for v in (x * 255 // w, y * 255 // h, 128)) for y in range(h))
    chunk = lambda t, d: struct.pack(">I", len(d)) + t + d + struct.pack(">I", zlib.crc32(t + d))
    with open(path, "wb") as f:
        f.write(b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0)) + chunk(b"IDAT", zlib.compress(rows, 6)) + chunk(b"IEND", b""))


def fake_llm(port):
    """tools/fake-llm.py on a port, and the environment that sends a chat to it."""
    p = subprocess.Popen([sys.executable, os.path.join(HERE, "tools", "fake-llm.py"), str(port)], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    time.sleep(1)
    return p, {"GHS_PROVIDER": "anthropic", "ANTHROPIC_API_KEY": "fake", "ANTHROPIC_BASE_URL": "http://127.0.0.1:%d" % port}, ["ANTHROPIC_AUTH_TOKEN", "ANTHROPIC_MODEL", "GHS_IMAGES", "GHS_CLAUDE_MODEL"]


def project(d, session):
    """A project made for a check: with `session`, a package of one module and its session `demo` started (a
    few seconds: cabal builds it); without, only the directory a chat needs (session `x`, never started)."""
    proj = os.path.join(d, "proj")
    if not session:
        os.makedirs(os.path.join(proj, ".ghci-session", "x"))
        with open(os.path.join(proj, "ghci-session.json"), "w") as f:
            json.dump({"targets": {"x": {"units": ["lib:x"], "watch": ["src"]}}}, f)
        return proj, "x"
    os.makedirs(os.path.join(proj, "src"))
    files = {
        "demo.cabal": "cabal-version: 2.4\nname:          demo\nversion:       0.1\nlibrary\n  hs-source-dirs:   src\n  exposed-modules:  Demo\n  build-depends:    base\n  default-language: Haskell2010\n",
        "cabal.project": "packages: .\n",
        "src/Demo.hs": "module Demo (greeting, selfTest) where\n\ngreeting :: String\ngreeting = \"hello\"\n\nselfTest :: IO ()\nselfTest = putStrLn (if length greeting == 5 then \"[PASS] greeting\" else \"[FAIL] greeting\")\n",
        "ghci-session.json": json.dumps({"default": "demo", "hygiene": False, "targets": {"demo": {"units": ["lib:demo"], "watch": ["src"], "modules": ["Demo"],
                                                                                         "test": {"expr": "Demo.selfTest", "pass": "\\[PASS\\] greeting", "fail": "\\[FAIL\\]"}}}}),
    }
    for name, text in files.items():
        with open(os.path.join(proj, name), "w") as f:
            f.write(text)
    r = subprocess.run([CLI, "start", "demo"], cwd=proj, capture_output=True, text=True)
    if "CHECK-PASS" not in r.stdout:
        sys.exit("the session of the check's project did not start (it needs cabal and ghc):\n" + r.stdout + r.stderr)
    return proj, "demo"


def stop(proj, session):
    subprocess.run([CLI, "stop", session], cwd=proj, capture_output=True, text=True)
