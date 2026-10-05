"""A GHCi behind a pty, framed by a sentinel prompt.

Every command written yields exactly one sentinel, so reading until the sentinel is a complete reply. The
reader runs on its own thread so a command that prints megabytes cannot deadlock on a full pty buffer.
"""
import errno
import fcntl
import os
import pty
import re
import select
import signal
import struct
import subprocess
import termios
import threading
import time

SENTINEL = "GHS_READY"
# Exactly ONE command, so exactly one sentinel is produced by the handshake (two would desynchronise every
# later reply by one command).
PROMPT_CMD = f':set prompt "\\n{SENTINEL}\\n"\n'

RE_ANSI = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")


def strip_ansi(s: str) -> str:
    return RE_ANSI.sub("", s)


class ReplDied(Exception):
    pass


class ReplTimeout(Exception):
    def __init__(self, secs: float):
        super().__init__(f"timed out after {secs:g}s")


class Repl:
    def __init__(self, command: str, cwd: str, env: dict, load_timeout: float, eval_timeout: float,
                 log=lambda _s: None, on_async=lambda _s: None):
        self.command_line = command
        self.cwd = cwd
        self.env = env
        self.load_timeout = load_timeout
        self.eval_timeout = eval_timeout
        self.log = log
        self.on_async = on_async
        self.proc: subprocess.Popen | None = None
        self.master = -1
        self._buf = ""
        self._cond = threading.Condition()
        self._dead = False
        self._io = threading.Lock()  # serialises whole command round-trips

    # -- lifecycle --

    def start(self, post_load) -> str:
        """Spawn, handshake, run `post_load(self)`; returns the load log."""
        master, slave = pty.openpty()
        attrs = termios.tcgetattr(slave)
        attrs[3] &= ~termios.ECHO  # otherwise every command we write comes back in the output
        termios.tcsetattr(slave, termios.TCSANOW, attrs)
        # A wide terminal keeps GHC from hard-wrapping diagnostics at 80 columns.
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 200, 400, 0, 0))
        env = dict(os.environ)
        env.update({k: str(v) for k, v in self.env.items()})
        env["TERM"] = "dumb"
        self.log(f"spawn: {self.command_line}")
        self.proc = subprocess.Popen(self.command_line, shell=True, cwd=self.cwd, env=env, stdin=slave,
                                     stdout=slave, stderr=slave, close_fds=True, start_new_session=True)
        os.close(slave)
        self.master = master
        self._dead = False
        threading.Thread(target=self._reader, daemon=True).start()
        # Written before GHCi is listening; the pty buffers it. Everything up to the FIRST sentinel is the
        # load log (including GHCi's own default prompt, which we discard).
        os.write(self.master, PROMPT_CMD.encode())
        load = self._await_sentinel(self.load_timeout)
        post_load(self)
        return load

    def stop(self) -> None:
        """Stop the repl AND everything it forked.

        The direct child is cabal; the process that matters is the `ghc --interactive` it execs, and anything
        that process forked lives inside it. When cabal exits first `proc.wait()` returns happily and a stale
        GHCi is reparented to init, still holding its ports. So signal the process GROUP and then verify the
        group is actually empty.
        """
        pgid = None
        if self.proc and self.proc.poll() is None:
            try:
                pgid = os.getpgid(self.proc.pid)
                os.killpg(pgid, signal.SIGTERM)
            except OSError:
                pass
            try:
                self.proc.wait(timeout=10)
            except subprocess.TimeoutExpired:
                if pgid is not None:
                    try:
                        os.killpg(pgid, signal.SIGKILL)
                    except OSError:
                        pass
        if pgid is not None:
            self._reap_group(pgid)
        if self.master >= 0:
            try:
                os.close(self.master)
            except OSError:
                pass
            self.master = -1

    def _reap_group(self, pgid: int) -> None:
        def empty() -> bool:
            try:
                os.killpg(pgid, 0)
                return False
            except OSError:
                return True

        for escalate in (False, True):
            if escalate:
                try:
                    os.killpg(pgid, signal.SIGKILL)
                except OSError:
                    pass
            deadline = time.time() + (5.0 if escalate else 3.0)
            while time.time() < deadline:
                if empty():
                    return
                time.sleep(0.1)
        self.log(f"WARNING: process group {pgid} still alive after SIGKILL")

    @property
    def alive(self) -> bool:
        return self.proc is not None and self.proc.poll() is None

    @property
    def pid(self) -> int | None:
        return self.proc.pid if self.proc else None

    # -- io --

    def _reader(self) -> None:
        while True:
            try:
                r, _, _ = select.select([self.master], [], [], 0.25)
            except (OSError, ValueError):
                break
            if not r:
                if not self.alive:
                    break
                continue
            try:
                chunk = os.read(self.master, 65536)
            except OSError as e:
                chunk = b"" if e.errno == errno.EIO else None
                if chunk is None:
                    break
            if not chunk:
                break
            with self._cond:
                self._buf += chunk.decode("utf-8", "replace")
                self._cond.notify_all()
        with self._cond:
            self._dead = True
            self._cond.notify_all()

    def _await_sentinel(self, timeout: float) -> str:
        deadline = time.time() + timeout
        with self._cond:
            while True:
                idx = self._buf.find(SENTINEL)
                if idx >= 0:
                    out = self._buf[:idx]
                    self._buf = self._buf[idx + len(SENTINEL):].lstrip("\r\n")
                    return strip_ansi(out)
                if self._dead:
                    out, self._buf = self._buf, ""
                    raise ReplDied(strip_ansi(out))
                left = deadline - time.time()
                if left <= 0:
                    raise ReplTimeout(timeout)
                self._cond.wait(min(left, 0.5))

    def command(self, expr: str, timeout: float | None = None) -> str:
        """Run one GHCi command and return its output (sentinel stripped)."""
        timeout = timeout or self.eval_timeout
        with self._io:
            if not self.alive:
                raise ReplDied("repl is not running")
            # Anything already buffered was written by a BACKGROUND thread (a server forked in the session,
            # say) between commands: park it rather than letting it masquerade as this command's answer.
            with self._cond:
                if self._buf.strip():
                    self.on_async(strip_ansi(self._buf))
                self._buf = ""
            e = expr.strip()
            payload = (":{\n" + e + "\n:}\n") if "\n" in e else (e + "\n")
            os.write(self.master, payload.encode())
            out = self._await_sentinel(timeout)
        return out.strip("\r\n")

    def post_load_basics(self) -> None:
        """Re-establish what a load or :reload resets.

        The buffering line is load-bearing, not hygiene: GHCi puts stdout in NoBuffering, so a forkIO'd thread
        sharing that handle interleaves with the prompt one CHARACTER at a time and shreds the sentinel -- the
        reply then never frames and every command times out. LineBuffering keeps the sentinel line intact.
        """
        self.command(':set prompt-cont ""', timeout=60)
        self.command(":module + System.IO", timeout=60)
        self.command("hSetBuffering stdout LineBuffering", timeout=60)
