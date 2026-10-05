"""Knowing WHEN a source may have changed, without waiting for the next poll.

The truth about what changed is always the mtime scan (`daemon.scan`); a waiter only says "look now". With
kernel events a save is noticed in milliseconds and its burst of writes is over when the events stop, so the
watcher need not sleep a fixed poll and a fixed debounce (0.2 s each) before every reload.

    kqueue    macOS/BSD (stdlib `select.kqueue`): one descriptor per watched file and directory
    inotify   Linux (ctypes onto libc): one watch per directory
    poll      anywhere, and the fallback whenever the others cannot be set up

Every waiter is allowed to miss an event: the daemon still scans every couple of seconds.
"""
import ctypes
import ctypes.util
import os
import select
import sys
import time


class PollWaiter:
    kind = "poll"

    def __init__(self, interval: float, debounce: float):
        self.interval, self.debounce = interval, debounce

    def update(self, paths) -> None:
        pass

    def wait(self, timeout: float) -> bool:
        time.sleep(min(timeout, self.interval))
        return True          # cannot know: scan

    def settle(self) -> None:
        time.sleep(self.debounce)   # an editor's save is several writes

    def close(self) -> None:
        pass


class _EventWaiter:
    QUIET = 0.05     # a burst is over when nothing has happened for this long ...

    def __init__(self, debounce: float):
        self.limit = max(debounce, 0.3)   # ... but never wait longer than this for it to end

    def settle(self) -> None:
        t0 = time.time()
        while time.time() - t0 < self.limit and self.wait(self.QUIET):
            pass


class KqueueWaiter(_EventWaiter):
    kind = "kqueue"

    def __init__(self, debounce: float):
        super().__init__(debounce)
        self.kq = select.kqueue()
        self.fds: dict[str, tuple[int, int]] = {}   # path -> (descriptor, inode)
        self.flags = getattr(os, "O_EVTONLY", os.O_RDONLY)
        self.notes = (select.KQ_NOTE_WRITE | select.KQ_NOTE_EXTEND | select.KQ_NOTE_DELETE
                      | select.KQ_NOTE_RENAME | select.KQ_NOTE_ATTRIB)
        try:   # a descriptor per file: lift the soft limit as far as it goes
            import resource
            soft, hard = resource.getrlimit(resource.RLIMIT_NOFILE)
            want = 16384 if hard == resource.RLIM_INFINITY else min(hard, 16384)
            if soft < want:
                resource.setrlimit(resource.RLIMIT_NOFILE, (want, hard))
            self.budget = resource.getrlimit(resource.RLIMIT_NOFILE)[0] - 256
        except Exception:  # noqa: BLE001
            self.budget = 0

    def update(self, paths) -> None:
        """Watch these files and their directories (a new file is an event on its directory). A file an
        editor saved by rename is a NEW inode: the old descriptor watches a file that is gone."""
        want = set(paths)
        want |= {os.path.dirname(p) for p in want if not os.path.isdir(p)}
        if len(want) > self.budget:
            raise OSError(f"{len(want)} paths to watch, {self.budget} descriptors to do it with")
        for p in [p for p in self.fds if p not in want]:
            os.close(self.fds.pop(p)[0])
        for p in want:
            try:
                ino = os.stat(p).st_ino
            except OSError:
                continue
            have = self.fds.get(p)
            if have and have[1] == ino:
                continue
            if have:
                os.close(have[0])
                del self.fds[p]
            try:
                fd = os.open(p, self.flags)
            except OSError:
                continue
            self.kq.control([select.kevent(fd, select.KQ_FILTER_VNODE, select.KQ_EV_ADD | select.KQ_EV_CLEAR, self.notes)], 0, 0)
            self.fds[p] = (fd, ino)

    def wait(self, timeout: float) -> bool:
        return bool(self.kq.control(None, 64, timeout))

    def close(self) -> None:
        for fd, _ in self.fds.values():
            try:
                os.close(fd)
            except OSError:
                pass
        self.fds = {}
        self.kq.close()


class InotifyWaiter(_EventWaiter):
    kind = "inotify"
    MASK = 0x2 | 0x4 | 0x8 | 0x40 | 0x80 | 0x100 | 0x200 | 0x400 | 0x800   # modify attrib close_write moved_* create delete *_self

    def __init__(self, debounce: float):
        super().__init__(debounce)
        self.libc = ctypes.CDLL(ctypes.util.find_library("c") or "libc.so.6", use_errno=True)
        self.fd = self.libc.inotify_init1(os.O_NONBLOCK | os.O_CLOEXEC)
        if self.fd < 0:
            raise OSError(ctypes.get_errno(), "inotify_init1")
        self.dirs: dict[str, int] = {}

    def update(self, paths) -> None:
        want = {p if os.path.isdir(p) else os.path.dirname(p) for p in paths}
        for d in [d for d in self.dirs if d not in want]:
            self.libc.inotify_rm_watch(self.fd, self.dirs.pop(d))
        for d in want - set(self.dirs):
            wd = self.libc.inotify_add_watch(self.fd, os.fsencode(d), self.MASK)
            if wd < 0:
                raise OSError(ctypes.get_errno(), f"inotify_add_watch {d}")
            self.dirs[d] = wd

    def wait(self, timeout: float) -> bool:
        r, _, _ = select.select([self.fd], [], [], timeout)
        if not r:
            return False
        try:
            while os.read(self.fd, 65536):   # drain: what happened is the scan's to say
                pass
        except BlockingIOError:
            pass
        return True

    def close(self) -> None:
        os.close(self.fd)


def make(kind: str, interval: float, debounce: float, paths, log=lambda _s: None):
    """The best waiter this platform offers (`kind` "auto"), or the polling one ("poll", or on any failure)."""
    if kind != "poll":
        try:
            w = None
            if hasattr(select, "kqueue"):
                w = KqueueWaiter(debounce)
            elif sys.platform.startswith("linux"):
                w = InotifyWaiter(debounce)
            if w is not None:
                w.update(paths)
                return w
        except Exception as e:  # noqa: BLE001
            log(f"watch: no kernel events ({type(e).__name__}: {e}) -- polling every {interval:g}s")
    return PollWaiter(interval, debounce)

