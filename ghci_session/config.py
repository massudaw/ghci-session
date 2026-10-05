"""Finding and reading ghci-session.json, and turning targets into a session's configuration.

A TARGET is a definition: what to load, what to check, what to serve. A SESSION is one repl. A plain session
holds one target and has its name; a COMPOSED session (declared under "sessions") holds whichever targets you
choose (`ghci-session compose NAME a b`), and its members are remembered in the state directory.
"""
import json
import os

CONFIG_NAME = "ghci-session.json"

DEFAULTS = {
    "repl": None,            # full command; default: `cabal repl` built from `units` and `cabal_args`
    "units": [],             # cabal components this target loads ("lib:mylib", "exe:server"); a composed
                             # session loads the union of its members' with --enable-multi-repl
    "cabal_args": "",        # extra arguments for the default command
    "watch": ["src"],        # dirs (relative to the project root) polled for source changes
    "modules": [],           # `:module +` after every load
    "prebuild": None,        # a shell command run before every boot of the repl (build a C bundle, generate code)
    "preload": [],           # GHCi expressions run BEFORE the module imports (dlopen a C bundle, set capabilities)
    "warm": [],              # expressions evaluated in the background after a reload that ran no check: they make
                             # GHCi link the reloaded code (name something from your modules: "My.thing `seq` ()")
    "check": None,           # {"expr": ..., "pass": regex, "fail": regex, "log": file the check writes, "name": label}
    "checks": [],            # several of them
    "server": None,          # {"action": IO (), "port": n, "env": {}, "prefork": IO (), "serve_on_load": bool,
                             #  "verify_timeout": 60}: served as a forked child of the repl (GHC.Hygiene.Zygote)
    "env": {},
    "load_timeout": 900,
    "eval_timeout": 600,
    "repl_budget_mb": 6144,  # past this a reload is a restart (0 disables); GHCi never gives memory back
    "rts_flags": "-c",       # GHCi's own RTS flags ("" or "none" turns the wrapper off)
    "ghc_jobs": 0,           # -jN for GHCi's own compiles (a reload that recompiles many modules); 0: GHC's default, one
    "capabilities": 0,       # setNumCapabilities in the repl (0: leave GHCi's single one)
    "hygiene_module": "GHC.Hygiene",        # where pruneCafs is, if your project re-exports or carries its own
    "zygote_module": "GHC.Hygiene.Zygote",  # likewise zygoteSpec / zygoteFork / zygoteStop / ZygoteChild / zcPid
    "hygiene_build": True,                  # build the C libraries (hygiene/build.sh) before boot
    "handover_env": ["GHS_HANDOVER_OUT", "GHS_HANDOVER_IN"],   # what a server's handover paths are called
    "unlink_after": "eval",  # when a reload's unlink happens: after the first evaluation ("eval"), or at once ("reload")
    "prune_gc_idle_s": 0,    # 0: the GC that frees what was unlinked runs at once, with the unlink. > 0 defers it
                             # until the session has been idle that long -- faster, and it has CRASHED the repl
                             # (see daemon.unlink_cafs); negative: unlink only, leave the GC to the RTS (as unsafe)
    "hygiene": False,        # prune CAFs after each reload (needs the ghci-hygiene package in the repl's scope)
    "auto_reload": True,     # reload when a watched file changes
    "watch_check": True,     # ... and run the checks (off: a save only compiles; `reload`/`check` still run them)
    "watch_refork": True,    # ... and bring running servers onto the new code (off: only an explicit reload does)
    "reload_on_commit": False,  # a new git HEAD is a full reload (checks, re-fork) whatever the two above say
    "watch_ext": [".hs", ".hs-boot", ".c", ".h", ".cabal"],
    "watcher": "auto",       # kernel file events where the platform has them (kqueue, inotify), else "poll"
    "poll_interval": 0.2,    # how often the watcher looks (a scan of ~600 sources is under 10 ms)
    "debounce": 0.2,         # ... and how long it lets a burst of writes settle before reloading
    "status_url": None,      # POST every verdict here as JSON (a dashboard's event feed); best effort
    "idle_stop_mins": 0,     # the session stops itself after this long unused (0: never); not while it serves
    "async_refork": False,   # a reload returns at its verdict and re-forks the servers in the background
    "fingerprint_files": [], # extra files whose content is part of a server's code (a C bundle, say)
}


class ConfigError(Exception):
    pass


def find_root(start: str | None = None) -> str:
    """The nearest directory at or above `start` holding ghci-session.json."""
    d = os.path.abspath(start or os.getcwd())
    while True:
        if os.path.isfile(os.path.join(d, CONFIG_NAME)):
            return d
        parent = os.path.dirname(d)
        if parent == d:
            raise ConfigError(f"no {CONFIG_NAME} in {start or os.getcwd()} or above (see `ghci-session init`)")
        d = parent


def load(root: str) -> dict:
    with open(os.path.join(root, CONFIG_NAME)) as fh:
        raw = json.load(fh)
    if "targets" not in raw or not raw["targets"]:
        raise ConfigError(f"{CONFIG_NAME}: no \"targets\"")
    reserved = ("targets", "state_dir", "default", "sessions")
    common = {k: v for k, v in raw.items() if k not in reserved}
    targets = {}
    for name, t in raw["targets"].items():
        cfg = dict(DEFAULTS)
        cfg.update(common)
        cfg.update(t)
        unknown = set(cfg) - set(DEFAULTS)
        if unknown:
            raise ConfigError(f"target {name!r}: unknown key(s) {sorted(unknown)}")
        if isinstance(cfg["units"], str):
            cfg["units"] = cfg["units"].split()
        if isinstance(cfg["warm"], str):
            cfg["warm"] = [cfg["warm"]]
        targets[name] = cfg
    sessions = raw.get("sessions") or {}
    for s, members in sessions.items():
        if s in targets:
            raise ConfigError(f"session {s!r} has the name of a target")
        for m in members:
            if m not in targets:
                raise ConfigError(f"session {s!r}: unknown member {m!r}")
    root = os.path.abspath(root)
    return {"root": root, "state_dir": os.path.join(root, raw.get("state_dir", ".ghci-session")),
            "state_rel": raw.get("state_dir", ".ghci-session"),
            "default": raw.get("default") or next(iter(targets)), "targets": targets, "sessions": sessions}


# -- composed sessions --

def members_file(conf: dict, session: str) -> str:
    return os.path.join(conf["state_dir"], session + ".members")


def read_members(conf: dict, session: str) -> list[str]:
    """A composed session's members: what was last chosen, else what the config declares."""
    try:
        with open(members_file(conf, session)) as fh:
            ms = json.load(fh)
        return [m for m in ms if m in conf["targets"]]
    except (OSError, ValueError):
        return list(conf["sessions"].get(session, []))


def write_members(conf: dict, session: str, members: list[str]) -> None:
    os.makedirs(conf["state_dir"], exist_ok=True)
    with open(members_file(conf, session), "w") as fh:
        json.dump(members, fh)


def session_names(conf: dict) -> list[str]:
    return list(conf["targets"]) + list(conf["sessions"])


def _checks_of(member: str, t: dict) -> list[dict]:
    """A member's checks, each labelled: the member's name, or `member:name` for a second one."""
    raw = list(t["checks"]) or ([t["check"]] if t["check"] else [])
    out = []
    for c in raw:
        if "expr" not in c:
            raise ConfigError(f"target {member!r}: a check needs \"expr\"")
        out.append({"member": f"{member}:{c['name']}" if c.get("name") else member, "expr": c["expr"],
                    "pass": c.get("pass"), "fail": c.get("fail", r"^\[FAIL\]"), "log": c.get("log"),
                    "timeout": c.get("timeout")})
    return out


def resolve(conf: dict, session: str) -> dict:
    """One session's configuration: a target's own, or the union of a composed session's members.

    Checks and servers stay PER MEMBER: each carries its own log, patterns and port, and merging those would
    lose exactly what a verdict is made of.
    """
    composed = session in conf["sessions"]
    if composed:
        members = read_members(conf, session)
    elif session in conf["targets"]:
        members = [session]
    else:
        raise ConfigError(f"unknown session {session!r}; have {', '.join(session_names(conf))}")
    ts = [conf["targets"][m] for m in members]

    def union(key):
        seen = []
        for t in ts:
            for x in t[key]:
                if x not in seen:
                    seen.append(x)
        return seen

    cfg = dict(DEFAULTS)
    if ts:
        cfg.update({k: ts[0][k] for k in ("repl", "cabal_args", "rts_flags", "debounce", "poll_interval", "watcher", "prebuild", "status_url", "hygiene_module",
                                          "zygote_module", "hygiene_build", "handover_env")})
    for key in ("units", "watch", "modules", "preload", "watch_ext", "fingerprint_files", "warm"):
        cfg[key] = union(key) if ts else list(DEFAULTS[key])
    cfg["env"] = {}
    for t in ts:
        cfg["env"].update(t["env"])   # a member's env reaches the repl (its check reads it) AND its own server
    cfg["prune_gc_idle_s"] = ts[0]["prune_gc_idle_s"] if ts else DEFAULTS["prune_gc_idle_s"]
    cfg["unlink_after"] = ts[0]["unlink_after"] if ts else DEFAULTS["unlink_after"]
    for key in ("load_timeout", "eval_timeout", "repl_budget_mb", "capabilities", "ghc_jobs"):
        cfg[key] = max([t[key] for t in ts] or [DEFAULTS[key]])
    cfg["hygiene"] = any(t["hygiene"] for t in ts)
    cfg["auto_reload"] = any(t["auto_reload"] for t in ts) if ts else True
    for key in ("watch_check", "watch_refork"):
        cfg[key] = all(t[key] for t in ts)
    cfg["reload_on_commit"] = any(t["reload_on_commit"] for t in ts)
    # a composed session idles out only if every member agrees to, at the longest of their waits
    mins = [t["idle_stop_mins"] for t in ts]
    cfg["idle_stop_mins"] = max(mins) if mins and all(m > 0 for m in mins) else 0
    cfg["async_refork"] = any(t["async_refork"] for t in ts)
    cfg["checks"] = [c for m, t in zip(members, ts) for c in _checks_of(m, t)]
    servers = []
    for m, t in zip(members, ts):
        if t["server"]:
            s = dict(t["server"])
            if "action" not in s:
                raise ConfigError(f"target {m!r}: a server needs \"action\"")
            s["member"] = m
            s["units"] = list(t["units"])
            s["env"] = {**t["env"], **(s.get("env") or {})}
            servers.append(s)
    cfg["servers"] = servers
    cfg["members"] = members
    cfg["composed"] = composed
    if composed and len(ts) > 1 and any(t["repl"] for t in ts):
        raise ConfigError(f"session {session!r}: a member with its own \"repl\" command cannot be composed "
                          "(give it \"units\" instead)")
    del cfg["check"], cfg["server"]
    return expand(cfg, {"session": session, "root": conf["root"], "state": conf["state_dir"],
                        "dylib": "dylib" if __import__("sys").platform == "darwin" else "so"})


def expand(x, vals: dict):
    """`{session}`, `{root}`, `{state}`, `{dylib}` in any string of a session's configuration."""
    if isinstance(x, str):
        for k, v in vals.items():
            x = x.replace("{" + k + "}", v)
        return x
    if isinstance(x, list):
        return [expand(v, vals) for v in x]
    if isinstance(x, dict):
        return {k: expand(v, vals) for k, v in x.items()}
    return x
