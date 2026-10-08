# ghci-session

A warm GHCi per project, behind a small daemon. It gives a long GHCi session the two things plain `cabal repl`
does not: **a verdict you can trust** and **memory that does not grow with every edit**.

```
ghci-session start              # boot once, leave it running
# edit src/Foo.hs               # the daemon sees the save, typechecks it, reloads what changed
ghci-session status             # one line: OK -- CHECK-PASS | COMPILE-ERROR: n error(s) | CHECK-FAIL: n failing
ghci-session eval 'Foo.bar 3'   # evaluate against the already loaded code, in milliseconds
```

It needs **GHC 9.14.1, 9.10.3 or 9.6.7** (the engine is that compiler's own GHCi front end, vendored per version)
and runs on Linux and macOS. It started in the tooling of a large Haskell modelling project (hundreds of modules,
servers forked from the repl, days-long sessions) and contains nothing of that project.

## What it does

- **Keeps one repl loaded.** An edit is a reload of what changed, not a rebuild and relink.
- **Gives verdicts that say what they describe.** A save is typechecked first and, if that fails, the session keeps
  the last code that compiled. Every verdict is prefixed `STALE(n)` when a watched source differs from what the
  loaded code was built from, and a compile-only verdict says `CHECK SKIPPED` so it is never mistaken for a passing
  check. The compiler's diagnostics are data in `status.json` (file, line, column, severity, code, message).
- **Runs your check.** A target names an expression to run after a good load (`Foo.selfTest`); lines matching a
  failure pattern fail it, a hang is interrupted and reported as `CHECK-HANG`. By default a save only compiles; set
  `watch_check` to run the check on every save, or run it with `reload` / `test`.
- **Stops the memory growth.** With a dynamically linked GHC every `:reload` links the recompiled modules into a new
  temporary shared library and never unloads the old one, and the RTS keeps every CAF of every superseded module
  alive. With `hygiene` on, the session unlinks the superseded CAFs after each reload and collects, so the repl stays
  flat over repeated edits; past a configured budget a reload becomes a restart. A heap census shows what each CAF
  retains, by constructor, with the Strings among it.
- **Serves from the repl.** A target can name a server action; it runs as a forked child of the repl, and a reload
  re-forks it onto the new code (or keeps it if its object code did not change), optionally carrying its state over.
- **Composes targets.** Several targets can share one repl, each with its own check, reported per member.
- **Remembers what was done to it.** The daemon logs every request, answer, save and verdict, keeps a tree of
  one-line summaries over the log, and serves it (and the session's operations) to an agent as MCP tools.

## Why not just `cabal repl`?

| Problem | What this does |
|---|---|
| An edit means rebuild, link, run | one repl stays loaded; an edit is a reload (and your check) |
| A verdict can describe old code if a reload was missed | verdicts carry `STALE(n)`; `status.json` has the same as data |
| GHCi never gives memory back: the RTS keeps every CAF of every superseded module | superseded CAFs are unlinked after each reload; a memory budget turns a reload into a restart |
| Memory is hard to attribute | a C heap census by CAF and by constructor |
| Killing the repl leaves the `ghc` it exec'd holding ports | the whole process group is signalled and checked empty |
| Serving from a thread of the repl means a restart for new code | a server is a forked child, re-forked on a reload |
| One repl per package compiles and holds a shared library N times | a composed session loads several targets into one repl |

## How it is built

`ghci-session.cabal` is the whole tool, in three parts:

- **`ghci-session-engine`** is GHCi, the compiler's interactive front end, with a socket where its terminal was.
  It is built against the same `ghc` library as the compiler it targets, so every command behaves as it does in
  `ghci`. A request is either a GHCi command (fed to its standard input, with the output and the compiler's
  diagnostics as records coming back) or a query answered by the engine itself in GHCi's monad: the loaded state,
  a typecheck of an expression, a fork, an unlink of superseded CAFs, the heap census.
- **`ghci-session`** is the command and the daemon, one binary. The daemon owns one engine per session, watches the
  sources (kqueue or inotify, with an mtime scan deciding what changed), publishes verdicts, keeps the servers and
  the history.
- **`ghci-hygiene`** (`hygiene/`) is a small library for your project's own code: state and a memo that outlive a
  reload, a heap census, and handing a server's state to its replacement. It depends on nothing of the compiler's,
  and a session needs none of it.

The daemon starts the engine itself. The build tool is run once with the engine as its repl program; it records the
arguments, directory and environment the repl would have had, and the daemon then runs the engine with them. There
is no `cabal repl` between the daemon and GHCi for the life of the session, stopping it is closing its socket, and
a restart that changes nothing cabal decides does not run cabal again.

The engine's per-compiler parts are small. GHCi's own sources are vendored per version (`vendor/ghc-X.Y.Z`, fetched
with `vendor/fetch.sh VERSION`); the modules that reach into the compiler's session are shared in `engine/`, and what
differs between compilers lives in `engine/ghc-X.Y/GhsCompat.hs`.

It depends only on GHC's boot packages. C (`cbits/`) covers what those lack: unix sockets, file events, POSIX regex,
hashing and the process table.

## Commands

```
start [--no-test] [--fast] | stop | restart [--fast] | status [-d] [SESSION]
reload [--no-test] [--no-refork] [--async-refork] [SESSION]
typecheck [SESSION]             # do the sources typecheck, without touching what is loaded
test [-m MEMBER] [SESSION]      # run the check
eval EXPR [-s SESSION]
doc WORDS...                    # find a definition: signature, comment, file:line
compose SESSION [MEMBERS...] [--add M] [--remove M]
server [status|start|stop|restart] [-m MEMBER] [-s SESSION] [--resume]
mem [--heap] | census | store | bench EXPR | profile
history | view | zoom ID | date ID      # the session's log and its summaries
mcp                             # the session and its memory as an agent's tools (MCP on stdin/stdout)
gc [-n] [--days N] | autostop [--max-mem-mb N] [--idle-mins M] | log | list | init
```

With no session named, a command goes to the one that is running (else the config's `default`).

- **`doc`** answers from an index of the session's own watched sources (a scanner of top-level declarations, not the
  compiler), so it works on a module that does not compile right now and does not wait for a reload.
- **`census`** lists every CAF by what it retains, the heap by constructor, the Strings among it, and the values you
  hold on to across reloads. **`bench`** reports wall time, GC and allocation of an IO action. **`profile`** runs the
  steps a target lists (an `eval`, a tool command or a shell command, each with a budget and an expected output) and
  compares the run with the one before.
- **`reload --no-test`** stops at the compile verdict and says so.

## Configuration

`ghci-session init` writes a starting `ghci-session.json`. This is `examples/hello`, abridged:

```json
{
  "default": "hello",
  "hygiene": true,
  "targets": {
    "hello": {
      "units": ["lib:hello"],
      "watch": ["src"],
      "modules": ["Hello"],
      "test": { "expr": "Hello.selfTest", "pass": "\\[PASS\\] table" },
      "server": { "action": "Hello.serve", "env": { "HELLO_OUT": ".ghci-session/hello.out" } }
    },
    "extra": {
      "units": ["lib:extra"],
      "watch": ["extra/src"],
      "modules": ["Extra"],
      "test": { "expr": "Extra.selfTest", "pass": "\\[PASS\\] shout" }
    }
  },
  "sessions": { "dev": ["hello", "extra"] }
}
```

A **target** is a definition: what to load, what to check, what to serve. A **session** is one repl: `start hello`
is a session holding that one target, `dev` a composed one. Top-level keys other than `targets`, `sessions`, `default`
and `state_dir` are shared by every target and can be overridden per target.

| key | meaning |
|---|---|
| `units` | the cabal components to load (`lib:x`, `exe:y`); more than one, or a server, means multi-repl |
| `repl` | the full repl command instead, with `{engine}` where the engine goes |
| `watch` | directories watched for `.hs`, `.hs-boot`, `.c`, `.h`, `.cabal`; root `*.cabal` and `cabal.project*` always are |
| `modules` | brought into scope after every load |
| `prebuild`, `preload`, `warm` | a command before each boot; expressions run before the imports; expressions run in the background after a reload |
| `test` / `tests` | the expression to run after a good load, with a `pass` pattern, a `fail` pattern and a `log` file |
| `watch_check` | run the check on every save (default off: a save compiles) |
| `watch_typecheck`, `watch_refork` | typecheck a save before reloading it (on); re-fork servers on a save (on) |
| `reload_on_commit` | a new git HEAD is a full reload, with checks and re-fork |
| `server` | `action`, optional `port`, `prefork`, `env`, `serve_on_load`, `verify_timeout` |
| `hygiene` | unlink superseded CAFs after each reload and report memory |
| `repl_budget_mb` | past this, a reload is a restart (default 6144; `0` disables) |
| `rts_flags` | the repl's RTS flags (default `-c -Fd0.5`) |
| `idle_stop_mins` | stop the session after this long unused; never while it serves |
| `load_timeout`, `eval_timeout` | seconds; a command past its timeout is interrupted, not abandoned |
| `history`, `summarize_cmd` | keep the session history; the command that writes its summary lines |

State lives in `.ghci-session/<session>/`: `status`, `status.json`, `load.log`, `reload.log`, `run.log`,
`daemon.log`, `server-<member>.log`, the history, and `loaded_sources.tsv`, the signature the loaded code was built from.
A failing check in a state directory where none has ever passed is marked `[NEVER-PASSED]`: suspect the target as
much as the edit.

## Composed sessions

```
ghci-session start dev                    # hello + extra in one repl
ghci-session compose dev --remove extra   # restarts the repl with the new set; running servers are adopted
```

Checks are reported per member, never merged. The one constraint a shared repl adds is a single namespace: qualify
the names in a check (`Hello.selfTest`, not `selfTest`). Two components of the same package cannot be members of one
session, because they would overwrite each other's object files; give them a session each.

## Servers

A target's `server` runs as a forked child of the repl, with the session's loaded code.

- Loading is not serving: `server start` starts it, or `serve_on_load` does.
- A reload re-forks what was running: `prefork` runs while the old server still serves, then the old one stops and
  the new one forks. With a `port`, the fork is verified to be the process listening on it.
- A server is kept when its code did not change: the object files of its units, and of the in-session units they
  depend on, are hashed at fork. This relies on `-fobject-determinism` (GHC 9.12 and later); older compilers
  re-fork on any save that touches it, even a comment.
- A server is not stopped for a fork that cannot happen: the action is typechecked first.
- State carries over if the server registers an exporter (`setHandoverExporter`, `handoverInPath`): the dying child
  writes its state on SIGTERM and the parent decides cold or resume.
- The child is a fork without an exec, so libraries that are not fork-safe must do their work in `prefork`.

## Memory and `ghci-hygiene`

With `hygiene` on, the engine finds the RTS's private lists (`dyn_caf_list`, `sm_mutex`, `loaded_objects`) by name in
the symbol table of the RTS image the process has mapped, and unlinks the CAFs of every library that is wholly
superseded, plus exported CAFs whose name now resolves elsewhere. Where it cannot read the RTS it prunes nothing and
says so once in `daemon.log`. The unlink waits for the check or the first `eval`, when the code that replaced the old
generation is linked.

The library gives your own code the same facilities:

```haskell
GHC.Hygiene.pruneCafs                          -- unlink the superseded CAFs, then a major GC
GHC.Hygiene.Census.cafReport / cafStrings      -- what every CAF retains; the Strings among it
GHC.Hygiene.Census.keep "name" v               -- values you hold on to across reloads
GHC.Hygiene.Store.storeRef name initial        -- an IORef that outlives a reload, by name
GHC.Hygiene.Kept.kept slot inputs outHash v    -- a stage remembered across reloads under a hash of what it reads
GHC.Hygiene.Kept.why                           -- what ran again since last asked, and the input that moved
GHC.Hygiene.Zygote.setHandoverExporter         -- a server's state, out on SIGTERM and in at start
```

`census`, `bench` and `mem --heap` are answered by the engine itself, so they work in any session without the project
depending on this library.

## Idle sessions and leftovers

A warm repl holds hundreds of MB to several GB for as long as it is left. `idle_stop_mins` stops a session after it
has been unused that long, and `autostop` stops the idle sessions of a project (longest idle first, optionally until
their total is under a limit). Three things can outlive a session with nothing recording them: a daemon its state
directory no longer names, a server whose daemon died, and the `ghc --interactive` that a `cabal repl` exec'd, which
keeps the lock on `dist-newstyle`. `status` warns when it sees any, and `gc` reaps them. Attribution is by absolute
path, never by name, so a sibling checkout's healthy session is not touched.

## History and agents

The daemon sees everything done to a session: client requests and their answers, saves with the verdict they compiled
to (and their diffs), commits, restarts. It writes each as a line of `.ghci-session/<session>/history/`, and keeps a
binary tree of one-line summaries over the log (written by a model through `summarize_cmd`, or left for an outside
compactor). `view` renders that tree as a bounded list of summaries, oldest first, and `zoom` opens any line into the
messages it was made from.

`ghci-session mcp` serves the session's operations and this memory as tools over the Model Context Protocol
(`claude mcp add ghci -- ghci-session mcp`, from the project's directory). `ghci-session chat` is a turn loop over
the same history for an OpenAI-compatible endpoint.

## Install

```
cabal install exe:ghci-session        # or run from a checkout: bin/ghci-session builds into .bin/ when stale
./build.sh                            # builds both executables into .bin/
```

The engine must sit beside `ghci-session` and must have been built with the compiler that is first on `PATH`
(it says so if not). Search can use `libfff` when it can be loaded; that is off by default and built with
`GHS_FFF=1 ./build.sh` or `cabal build -f+fff`. A project adds `ghci-hygiene` to its `build-depends` only to call the
library from its own code.

## Supported compilers and platforms

| GHC | Linux x86_64 | macOS arm64 |
|---|---|---|
| 9.14.1 | yes | yes (developed here) |
| 9.10.3 | yes | not tried (the pruner is off before 9.14 there) |
| 9.6.7 | yes | not tried (the pruner is off before 9.14 there) |

CI builds the engine and runs the whole tour on each Linux compiler, and builds the engine on macOS with 9.14.1.

Before 9.14 the GHCi front end has no interactive home units, which shows in three places:

- a composed session sees one unit at the prompt (the one that sees the most), so a package and one that imports it are
  both in scope, but of three packages in a chain one is not;
- adding a package to a running session (`compose --add`) restarts the repl;
- a save that changes only a comment re-forks a server, before GHC 9.12.

Not supported: a compiler other than these three (another needs its GHCi sources vendored with `vendor/fetch.sh` and a
stanza in the cabal file), and a deferred GC after an unlink (it crashed a large session; the GC is immediate).
Template Haskell projects work; with a splice in the module graph each save rescans the modules rather than reusing
the previous graph, which costs on the order of a quarter of a millisecond per module.

## Layout

```
ghci-session.cabal      the package: the engine and the command (hygiene/ghci-hygiene.cabal: the library)
app/GhciSession/        Json, Config, Sys (FFI), Repl (starting the engine, its protocol), Watch, Daemon, Gc, Cli, History, ...
engine/, vendor/        the engine (GhsEngine.hs, Main.hs, per-compiler GhsCompat) and GHCi's sources per compiler
cbits/                  sockets, file events, regex, hashing, processes; the executable's entry point
hygiene/                c/*.c (the pruner and the census, compiled into the engine), src/ (the library), repro/
bin/ghci-session        run from a checkout (builds if stale)
bin/ghci-history        save a session's history to a git branch and load it back
ghci-session.json       the sessions this package runs on itself
examples/hello/         two packages, a CAF that leaks without pruning, a server with state to hand over, Template Haskell splices
examples/tour.py        every feature on a copy of hello, each step checked and timed
tests/test_e2e.py       the lifecycle end to end (GHS_E2E=1)
```

`ghci-session selftest` runs the built-in self-tests from the binary.
