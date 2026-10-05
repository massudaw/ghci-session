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
| Serving from a thread of the repl means a restart to serve new code | a server is a forked CHILD of the repl: a reload re-forks it onto the new code, carrying its state, and keeps it if its object code did not change |
| One repl per package means the shared library compiled and held N times | a composed session loads several targets into ONE repl, each with its own check |
| A forked thread's output interleaves with the prompt and shreds the framing | stdout is line-buffered after every load |

## The tour: every feature, timed

`python3 examples/tour.py` runs the whole tool against a copy of `examples/hello` -- 115 steps in 13 groups (`--list`),
each one the command a person would type and what it must answer -- and prints what each took. It is the example to
read, the acceptance test to run after a change (exit 1 if any outcome is not the expected one), and the benchmark:
`--json out.json` saves a run and `--compare out.json` lists what got slower.

What a step costs on this machine (GHC 9.14.1, macOS arm64, an M4; the example is two small packages, so these are
the tool's own costs with almost no compile time in them):

| | seconds |
|---|---|
| boot, cold (cabal configures and compiles) / warm (objects on disk) | 11.2 / 2.1 |
| `eval` | 0.09 (the Python client's start-up is most of it) |
| `reload`, nothing changed / `--no-check` | 1.4 / 0.7 |
| save to verdict (the watcher: poll, debounce, reload, prune, check): a comment / a real change / a compile error | 1.6 / 2.1 / 1.5 |
| composed session of two packages, boot | 7.2 |
| `server start` with a 2 s prefork | 2.4 |
| save to verdict with a server: kept (comment) / re-forked with its state (2 s prefork) | 2.9 / 5.4 |
| `reload --async-refork` returns | 2.3 |
| `compose --remove`: the repl restarts, the server is adopted | 2.3 |
| census: every CAF / one value | 0.7 / 0.4 |
| a reload that is over the memory budget (a restart) | 2.2 |
| a commit to a full reload (`reload_on_commit`) | 1.6 |
| `gc` | 0.1-0.3 |

And the reason for the pruner, measured by the same tour -- live heap (MB) after each of five edit-reload-check
rounds of a module holding one 200,000-entry `Map`:

| | start | 1 | 2 | 3 | 4 | 5 | grew |
|---|---|---|---|---|---|---|---|
| object code, no pruning | 80 | 129 | 177 | 226 | 275 | 323 | +243 MB |
| `hygiene: true` | 98 | 147 | 147 | 147 | 147 | 147 | +49 MB |

One copy of the table per reload without it; with it, the first reload's copy stays (the initial library also holds
the modules that never change, so it is never wholly superseded) and every later one is freed.

## Install

Nothing to install: `bin/ghci-session` runs from this directory (Python 3.10+, no packages). Put `bin/` on `PATH`,
or call it by path from any directory below a `ghci-session.json`.

## Configure

`ghci-session init` writes a starting `ghci-session.json`. A fuller one (this is `examples/hello`):

```json
{
  "default": "hello",
  "hygiene": true,
  "targets": {
    "hello": {
      "units": ["lib:hello"],
      "watch": ["src"],
      "modules": ["Hello"],
      "check": { "expr": "Hello.selfTest", "pass": "\\[PASS\\] table" },
      "server": { "action": "Hello.serve", "env": { "HELLO_OUT": ".ghci-session/hello.out" },
                  "prefork": "Control.Concurrent.threadDelay 3000000" }
    },
    "extra": {
      "units": ["lib:extra"],
      "watch": ["extra/src"],
      "modules": ["Extra"],
      "check": { "expr": "Extra.selfTest", "pass": "\\[PASS\\] shout" }
    }
  },
  "sessions": { "dev": ["hello", "extra"] }
}
```

A **target** is a definition: what to load, what to check, what to serve. A **session** is one repl. `ghci-session start hello`
is a session holding that one target; `dev` is a *composed* session holding whichever targets you choose.
Keys at the top level (other than `targets`, `sessions`, `default`, `state_dir`) are shared by every target and overridable per target.
`{session}`, `{root}`, `{state}` and `{dylib}` (`dylib` or `so`) are replaced in any string.

| key | default | |
|---|---|---|
| `units` | `[]` | the cabal components the target loads (`lib:x`, `exe:y`). More than one, or a server, means `--enable-multi-repl` |
| `repl` | built from `units` | the full command instead, if you need something else (not composable) |
| `cabal_args` | `""` | extra arguments for the default command |
| `watch` | `["src"]` | dirs polled for `.hs/.hs-boot/.c/.h/.cabal`; root-level `*.cabal` and `cabal.project*` are always watched |
| `modules` | `[]` | `:module +` after every load |
| `prebuild` | none | a shell command run before every boot of the repl (build a C bundle, generate code); a failure is `PREBUILD-ERROR` |
| `preload` | `[]` | GHCi expressions run *before* the imports (e.g. `dlopen` a C bundle: importing an `-fobject-code` module links its objects there and then) |
| `check` / `checks` | none | `expr` to run after a good load; lines matching `fail` (default `^\[FAIL\]`) fail it, `pass` must appear; `log`: a file the check writes its real output to; `name` labels a second check |
| `server` | none | see *Servers* |
| `env` | `{}` | environment of the repl, and of the target's server |
| `repl_budget_mb` | `6144` | past this, a reload is a restart; `0` disables. Env `GHS_REPL_BUDGET_MB` overrides |
| `rts_flags` | `-c` | GHCi's own RTS flags, via `--with-repl=bin/ghci-rts.sh` (`-c`: compacting old generation; ~3x less heap than the copying GC for a long session); `none` turns it off |
| `capabilities` | `0` | `setNumCapabilities` in the repl (GHCi evaluates on one; more buys the parallel GC) |
| `hygiene` | `false` | build the C libraries, prune CAFs after each reload, report memory. Needs the `ghci-hygiene` package in the repl's scope |
| `auto_reload` | `true` | reload when a watched file changes (a `.c`, `.h` or `.cabal` change restarts instead: a loaded C object, or a package set, cannot be replaced) |
| `idle_stop_mins` | `0` | the session stops itself after this long unused (never while it serves). A composed session idles out only if every member sets it, at the longest |
| `async_refork` | `false` | a reload returns at its verdict and re-forks the servers in the background (also `reload --async-refork`, env `GHS_ASYNC_REFORK=1`) |
| `watch_check`, `watch_refork` | `true` | what a SAVE does beyond compiling: run the checks, cut the servers over. Off, an explicit `reload` (or a commit, below) does them |
| `reload_on_commit` | `false` | a new git HEAD is a full reload -- checks and re-fork -- whatever the two above say |
| `status_url` | none | POST every verdict there as JSON, the intermediate ones too (`reloading`, `running check`): a dashboard's event feed. Best effort, 0.25 s |
| `hygiene_module`, `zygote_module`, `hygiene_build`, `handover_env` | `GHC.Hygiene`, `GHC.Hygiene.Zygote`, `true`, `GHS_HANDOVER_OUT/IN` | for a project that carries its own copies of these modules |
| `fingerprint_files` | `[]` | extra files that are part of a server's code (a C bundle) |
| `load_timeout`, `eval_timeout` | 900, 600 | seconds |

State lives in `.ghci-session/<session>/`: `status` (the verdict, then the failing lines), `status.json`, `load.log`/`reload.log`,
`run.log` (the checks), `daemon.log`, `async.log` (output a background thread printed between commands), `server-<member>.log`.
`loaded_sources.tsv` is the signature the loaded code was built from (`<mtime ns>\t<path>`), for a cache in the loaded
code that is keyed by source. A reload publishes its status ONCE, when the verdict and what happened to the servers
are both known. A failing check in a state dir where none has ever passed is marked `[NEVER-PASSED]`: suspect the
target as much as the edit.

## Commands

```
start|stop|restart|status [-d] [SESSION]
reload [--no-check] [--no-refork] [--async-refork] [SESSION]
check [-m MEMBER] [SESSION]
eval EXPR [-s SESSION]
compose SESSION [MEMBERS...] [--add M] [--remove M]
server [status|start|stop|restart] [-m MEMBER] [-s SESSION] [--resume]
gc [-n] [--days N]
autostop [--max-mem-mb N] [--idle-mins M] [--include-serving] [-n]
mem | log [FILE] [-s SESSION] | list | init
```

With no session named, a command goes to the one that is running (else the config's `default`).
`reload --no-check` stops at the compile verdict, and says so (`CHECK SKIPPED`), so a compile-only verdict is never
mistaken for a check that passed.

## Composed sessions

```
ghci-session start dev                 # hello + extra in one repl
ghci-session compose dev --remove extra   # restarts the repl with the new set; running servers are adopted
```

```
OK -- CHECK-PASS (2.0s) [2 members: hello 0.5s, extra 0.4s]
CHECK-FAIL: 1 failing in extra [2 members: hello 0.5s, extra 0.4s]
```

Checks are reported per member, never merged, and `status.json` has one entry per member. The one constraint a shared
repl adds is a single namespace: **qualify the names in a check** (`Hello.selfTest`, not `selfTest`).

## Servers

A target's `server` is run as a forked child of the repl (`GHC.Hygiene.Zygote`), with the session's loaded code:

```json
"server": { "action": "My.Server.main", "port": 8080, "env": {"PORT": "8080"},
            "prefork": "My.Server.warmCaches", "serve_on_load": false, "verify_timeout": 60 }
```

- **Loading is not serving.** `ghci-session server start` starts it (or `serve_on_load`).
- **A reload re-forks what was running**: prefork (old server still serving), stop, fork. With a `port`, the fork is
  verified to be the process listening on it before it is recorded.
- **The re-fork can run in the background** (`--async-refork`): the reload returns at its verdict, marked
  `[servers: re-fork running in the background ...]` (`"servers_pending": true` in `status.json`), and the status is
  amended when the new server is up. The repl is one process, so a command sent during the prefork queues behind it;
  the next reload, restart or `server` command waits for the re-fork to finish.
- **It is kept when its code did not change**: the object files of its units and of the in-session units they depend
  on are hashed at fork (GHC >= 9.12 for `-fobject-determinism`; older GHCs re-fork every time). An edit to another
  member, or a comment, keeps the server: `[servers: kept 1 (hello:67878): their code did not change]`.
- **It is not stopped for a fork that cannot happen**: the action is type-checked first, and a compile error leaves
  the old server running the old code (and says so).
- **State carries over** if the server registers an exporter: `setHandoverExporter` at startup, `handoverInPath` to
  resume. The dying child writes its state on SIGTERM; the parent decides cold-or-resume, so a stale file is never
  silently resumed (`server start` is cold unless `--resume`).
- **The child is a fork without an exec.** On macOS, libraries that are not fork-safe (Accelerate/LAPACK, GSL) crash
  in it silently: do that work in `prefork`, in the parent, and let the child serve the result.
- A composed session's servers outlive a member change (the new repl adopts them); every session stops its servers when it stops.

## Idle sessions

A warm repl is hundreds of MB to several GB held for as long as you leave it. Two ways to give it back:

- `"idle_stop_mins": 60` -- the session stops itself, and `status` then says why
  (`stopped: idle for 60 min (idle_stop_mins); ghci-session start lib`).
- `ghci-session autostop [--max-mem-mb N] [--idle-mins 30]` -- stop the idle sessions of this project, longest idle
  first: all of them, or only until their total is under `N`. Meant for a cron job or a pre-build hook; `-n` shows the plan.

Idle is measured from the last client command or source change, not from the last verdict. A session that is busy
(a reload, a check, a background re-fork) is never stopped, and one with a running server is kept unless
`--include-serving`: a server is in use by whoever is connected to it, which the session cannot see.

## Leftovers: `gc`

Three things can outlive a session with nothing recording them: a daemon its state dir no longer names, a server
whose session's daemon died, and the `ghc --interactive` a `cabal repl` exec'd, reparented to init and still holding
the lock on `dist-newstyle` (every later cabal command then queues behind it). `ghci-session status` warns when it
sees any; `ghci-session gc` reaps them (`-n` to look first), and `--days N` also prunes the state of sessions idle
longer than that. Attribution is by absolute path -- the daemon's `--root`, this project's `dist-newstyle` -- never
by name, so a sibling checkout's healthy session is not touched.

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

GHC.Hygiene.Zygote.zygoteFork / zygoteStop   -- what `server` drives
GHC.Hygiene.Zygote.setHandoverExporter, handoverInPath
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
examples/hello/         two packages, a CAF that leaks without pruning, a server with state to hand over
examples/tour.py        every feature on a copy of it, each step checked and timed (the benchmark)
tests/                  test_unit.py (no GHC); test_e2e.py (GHS_E2E=1, ~1 min: a composed session, per-member checks,
                        a server kept / re-forked with its state / protected from a broken action, adoption, prune, census)
```

## Status

Working: plain and composed sessions, per-member checks, auto-reload, verdicts and staleness, memory budget, pruner,
census, forked servers (keep / re-fork, also in the background / handover / adoption), `gc`, idle stop, unit and
end-to-end tests. Not carried over from `tools/msq`: the static-interpreter experiment, and its project-specific commands. Linux: the C builds
are skipped (the offsets come from a Mach-O dylib); the session itself should run but is untested there. Port
verification needs `lsof`.

This repository's own sessions run on it: `tools/msq` is a front end (`tools/model_session/msq.py`) that keeps
`ghci-session.json` generated from `tools/model_session/targets.json` and adds the project's commands. The project
still carries its own copies of the hygiene and fork modules (`Solver.GhciHygiene`, `Solver.DES.Zygote`,
`tools/ghci_cafs/`), named through `hygiene_module` / `zygote_module`; replacing them with the `ghci-hygiene`
package is a dependency change to the core library and is not done.
