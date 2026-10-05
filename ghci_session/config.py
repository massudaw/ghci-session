"""Finding and reading ghci-session.json."""
import json
import os

CONFIG_NAME = "ghci-session.json"

DEFAULTS = {
    "repl": None,            # full command; default: `cabal repl` + the target's `cabal_args`
    "cabal_args": "",        # e.g. "lib:mylib" or "--enable-multi-repl a b"
    "watch": ["src"],        # dirs (relative to the project root) polled for .hs/.hs-boot/.c/.h changes
    "modules": [],           # `:module +` after every load
    "preload": [],           # GHCi expressions run BEFORE the module imports (dlopen a C bundle, set capabilities)
    "check": None,           # {"expr": ..., "pass": regex, "fail": regex}
    "env": {},
    "load_timeout": 900,
    "eval_timeout": 600,
    "repl_budget_mb": 6144,  # past this a reload is a restart (0 disables); GHCi never gives memory back
    "rts_flags": "-c",       # GHCi's own RTS flags ("" or "none" turns the wrapper off)
    "hygiene": False,        # call GHC.Hygiene.pruneCafs after each reload (needs the ghci-hygiene package in scope)
    "auto_reload": True,     # reload when a watched file changes
    "watch_ext": [".hs", ".hs-boot", ".c", ".h", ".cabal"],
    "debounce": 0.4,
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
    common = {k: v for k, v in raw.items() if k not in ("targets", "state_dir", "default")}
    targets = {}
    for name, t in raw["targets"].items():
        cfg = dict(DEFAULTS)
        cfg.update(common)
        cfg.update(t)
        unknown = set(cfg) - set(DEFAULTS)
        if unknown:
            raise ConfigError(f"target {name!r}: unknown key(s) {sorted(unknown)}")
        targets[name] = cfg
    return {"root": root, "state_dir": os.path.join(root, raw.get("state_dir", ".ghci-session")),
            "default": raw.get("default") or next(iter(targets)), "targets": targets}
