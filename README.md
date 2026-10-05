# ghci-session

A warm GHCi per project, behind a small daemon, with the two things a long GHCi session needs and plain
`cabal repl` does not give you: **a verdict you can trust** and **memory that does not grow with every edit**.

Extracted from the `tools/msq` tooling of this repository, with nothing project-specific in it. (`tools/msq` is
untouched and still what the sprinkler models use; moving it onto this tool is the next step, see *Status*.)

```
ghci-session start          # boot once, leave it running
# edit src/Foo.hs           # the daemon sees it, reloads, runs your check
ghci-session status         # one line: OK -- CHECK-PASS | COMPILE-ERROR: n error(s) | CHECK-FAIL: n failing
ghci-session eval 'Foo.bar 3'   # evaluate against the ALREADY LOADED code, in well under a second
```

## Why not just `cabal repl`?

| Problem | What this does |
|---|---|
| An edit means rebuild, link, run (tens of seconds) | one repl stays loaded; an edit is a `:reload` (and your check) |
| A verdict can describe OLD code if a reload was missed | every verdict is prefixed `STALE(n)` when a watched source differs from what the loaded code was built from, and `status.json` carries the same as data |
| GHCi never gives memory back: every reload keeps the old code and the RTS keeps every CAF of every superseded module as a GC root | `ghci-hygiene` unlinks the superseded CAFs after each reload; past `repl_budget_mb` a reload becomes a restart |
| Memory is hard to attribute in a session | a C heap census: what each CAF retains, by constructor, with the Strings among it |
| Killing the repl leaves the `ghc` it exec'd (and anything it forked) holding ports | the whole process *group* is signalled and verified empty |
| A forked thread's output interleaves with the prompt and shreds the framing | stdout is line-buffered after every load |

## Install

Nothing to install: `bin/ghci-session` runs from this directory (Python 3.10+, no packages). Put `bin/` on `PATH`,
or call it by path from any directory below a `ghci-session.json`.

## Configure

`ghci-session init` writes a starting `ghci-session.json`:

```json
{
  "default": "lib",
  "targets": {
    "lib": {
      "cabal_args": "lib:mypackage",
      "watch": ["src"],
      "modules": ["MyModule"],
      "check": { "expr": "MyModule.selfTest", "pass": "\\[PASS\\]", "fail": "\\[FAIL\\]" },
      "hygiene": true
    }
  }
}
```

Keys at the top level (other than `targets`, `default`, `state_dir`) are shared by every target and overridable per target.

| key | default | |
|---|---|---|
| `repl` | `cabal repl <cabal_args>` | the full command, if you need `--enable-multi-repl`, flags, a different tool |
| `cabal_args` | `""` | appended to the default command |
| `watch` | `["src"]` | dirs polled for `.hs/.hs-boot/.c/.h/.cabal`; root-level `*.cabal` and `cabal.project*` are always watched |
| `modules` | `[]` | `:module +` after every load |
| `preload` | `[]` | GHCi expressions run *before* the imports (e.g. `dlopen` a C bundle: importing an `-fobject-code` module links its objects there and then) |
| `check` | none | `expr` to run after a good load; `fail` regex marks failing lines, `pass` regex must appear |
| `env` | `{}` | environment of the repl |
| `repl_budget_mb` | `6144` | past this, a reload is a restart; `0` disables. Env `GHS_REPL_BUDGET_MB` overrides |
| `rts_flags` | `-c` | GHCi's own RTS flags, via `--with-repl=bin/ghci-rts.sh` (`-c`: compacting old generation; ~3x less heap than the copying GC for a long session); `none` turns it off |
| `hygiene` | `false` | build the C libraries, prune CAFs after each reload, report memory. Needs the `ghci-hygiene` package in the repl's scope |
| `auto_reload` | `true` | reload when a watched file changes (a `.c`, `.h` or `.cabal` change restarts instead: a loaded C object cannot be replaced) |
| `load_timeout`, `eval_timeout` | 900, 600 | seconds |

State lives in `.ghci-session/<target>/`: `status` (the verdict, then the failing lines), `status.json`, `load.log`/`reload.log`,
`run.log` (the check), `daemon.log`, `async.log` (output a background thread printed between commands).

## Commands

`start`, `stop`, `restart`, `status [-d]`, `reload [--no-check]`, `check`, `eval EXPR [-t TARGET]`, `mem`, `log [FILE]`, `list`, `init`.
Every command takes an optional target; the default is the config's `default`.

`reload --no-check` stops at the compile verdict, and says so (`CHECK SKIPPED`), so a compile-only verdict is never
mistaken for a check that passed.

## ghci-hygiene (`hygiene/`)

A Haskell package plus three small C libraries built against *your* GHC's RTS (`hygiene/build.sh`, run by the daemon).

```haskell
GHC.Hygiene.pruneCafs :: IO Int          -- CAFs unlinked, then a major GC; -1 unknown RTS layout, -2 no library
GHC.Hygiene.loaderStats :: IO Int        -- what the RTS linker holds, to stderr

GHC.Hygiene.Census.cafReport 10 100000000    -- what every CAF retains, by CAF and by constructor
GHC.Hygiene.Census.cafStrings 10 100000000   -- the Strings among it (24 bytes a character)
GHC.Hygiene.Census.censusOf "x" Mod.value    -- ONE value, alone
GHC.Hygiene.Census.keep "name" v >> keptReport 100000000   -- values you hold on to across reloads
GHC.Hygiene.Census.benchOf "label" action    -- wall, GC, allocation, live heap
GHC.Hygiene.Census.memNow
```

**Why the leak exists** (a GHC behaviour, not yours): with a dynamically linked GHC every `:reload` links the recompiled
modules into a *new* temporary shared library and never unloads the old one, and the RTS keeps every CAF in those
libraries as a root, so the evaluated value of every CAF of every superseded module stays live. In the project this was
extracted from it was ~240 MB per edit. `pruneCafs` unlinks the CAFs of a library that is wholly superseded (every
exported symbol now resolves to a newer one), plus exported CAFs whose name resolves elsewhere.

`examples/hello` shows it working: six CAFs unlinked per reload, the repl flat at ~505 MB over repeated edits.

**Safety.** The C reads the RTS's private symbol offsets from the dylib's symbol table and checks four exported symbols;
on an RTS whose layout it does not recognise it builds nothing / returns `-1` and the session just does not prune.
Tested on GHC 9.14.1, macOS arm64. `keepCAFs = 0` is *not* an option (SIGBUS: interpreted code refers to CAFs by raw
address, GHC #23182).

## Layout

```
bin/ghci-session        the client (python -m ghci_session)
bin/ghci-rts.sh         cabal repl --with-repl wrapper giving GHCi its own RTS flags
ghci_session/           config, repl (pty + sentinel framing), daemon (watch, reload, budget, socket), cli
hygiene/                ghci-hygiene.cabal, src/GHC/Hygiene*.hs, c/*.c, build.sh
examples/hello/         a package with a CAF that leaks without pruning
tests/                  test_unit.py (no GHC); test_e2e.py (GHS_E2E=1: boot, edit, error, recover, prune, census)
```

## Status

Working: single-target sessions, auto-reload, verdicts and staleness, memory budget, pruner, census, e2e test.
Not yet carried over from `tools/msq`: composed sessions (several packages' checks in one repl), forked-server
serving (`zygote`: serve a model from a forked child, re-fork on reload, keep a server whose object code did not
change), orphan collection (`gc`), the static-interpreter experiment. Linux: the C builds are skipped (the offsets
come from a Mach-O dylib); the session itself runs. Then: move `tools/msq` onto this and delete the copy.
