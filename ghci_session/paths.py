"""The one thing the client and the daemon must agree on, importable without the daemon's dependencies."""
import hashlib
import os


def sock_path(state_dir: str) -> str:
    """A short, collision-free unix socket path (macOS caps sun_path near 104 bytes, which a nested worktree
    easily exceeds), unique per absolute state dir."""
    base = f"/tmp/ghci-session-{os.getuid()}"
    os.makedirs(base, exist_ok=True, mode=0o700)
    return os.path.join(base, hashlib.sha1(os.path.abspath(state_dir).encode()).hexdigest()[:16] + ".sock")
