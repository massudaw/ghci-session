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

## One Haskell package

`ghci-session.cabal` is the whole tool, in three parts:

- **`ghci-session-engine`** is GHCi -- the compiler's own interactive front end, vendored -- with a socket where its
  terminal was and the session's operations built in (below);
- **`ghci-session`** is the command and the daemon (one binary): it owns one engine per session, watches the
  sources, publishes verdicts, keeps the servers;
- **the library** (`GHC.Hygiene`, `.Census`, `.Zygote`) is for your project's own code: a heap census, and handing a
  server's state to its replacement. A session needs none of it.

It depends only on GHC's boot packages; C (`cbits/`) covers what those lack -- unix sockets, kqueue/inotify, POSIX
regex, hashing, the process table -- and the executable's `main`, which picks the runtime's options per command.

It began as a Python daemon (removed; the command line, the state files and the socket protocol are unchanged), and
what the rewrite changed is the client and the tool's own overhead:

| | Python | Haskell |
|---|---|---|
| `eval` | 90 ms | 10 ms |
| `list` / `status` / `mem` | 260 / 150 / 220 ms | 10 / 20 / 10 ms |
| `gc -n` / `autostop -n` | 160 / 210 ms | 30 / 20 ms |
| `server stop` | 130 ms | 30 ms |
| a reload, a save's verdict, a boot | the same: they are GHC, cabal and your check |
| the daemon | ~25 MB | 23 MB resident, 1 MB of live heap |

How it got there is in `ghci-session selfbench` (the hot paths on realistic inputs) and was found with the tool
itself -- this package has a `ghci-session.json`, and a save here is a compile and 65 self-tests in a few seconds (target `tool`; `engine` is the engine's own session, a compile verdict in 0.3 s)
(`ghci-session selftest` runs them from the binary):

- **Processes are asked of the kernel.** Spawning `ps` to ask "is this pid alive" was 20 ms, several times a reload
  and once per session in every client command; `footprint` could hang. One libproc call now gives liveness, parent,
  resident size and physical footprint for every process in 0.8 ms.
- **Replies are `Text` and bytes end to end.** As `String`, a 0.5 MB load log was 20 MB allocated to decode, 14 MB to
  find the verdict in, 16 MB to encode as JSON: 15 ms. Now 0.7 ms and 1 MB.
- **Paths are bytes.** The watched-source signature of a 526-file project was 1 MB of cons cells and 5.6 MB a scan.
- **The runtime is configured per command.** 15 of a client command's 21 ms were the Haskell runtime starting and
  stopping: its interval timer (the exit waited out a tick) and the reservation of a terabyte of address space. The
  client runs with `-V0 -xr1g`; the daemon keeps the timer, two capabilities and `GHC.Stats`.
- **A server's code is hashed fast, and only where it changed.** A byte-at-a-time FNV did 1 GB/s: 70 ms for the 109
  object files (34 MB) of one server, on every reload. Four 64-bit lanes over 32-byte stripes do ~6 GB/s (6.5 ms),
  and a file whose size and modification time stand is not read again at all.
- HEAD is read from `.git` instead of spawning `git` every two seconds.

## The engine: GHCi with a socket where its terminal was

`ghci-session-engine` IS GHCi: the `ghc` executable's interactive front end, vendored unchanged
(`vendor/ghc-9.14.1`, 7,600 lines; `vendor/fetch.sh VERSION` gets another, plus a stanza in the cabal file) and built
against the same `ghc` library, so every command behaves exactly as it does there. A session's GHCi is always this
one; there is no second way to drive a stock `ghci` through a terminal (there was: a pseudo-terminal, a prompt
chosen so it could be found in the output, and every question to GHCi asked as a command whose printed answer was
read back).

It needs no change to GHCi's loop. GHCi calls a prompt function before it reads each command; the engine's is the
turn (`engine/GhsEngine.hs`): reply to what just ran, wait for the next request. A request is one of two things.

- **A GHCi command** (`:reload`, an expression): fed to GHCi's standard input, which is a pipe the process holds
  the other end of. The reply is everything it wrote -- standard output and error are a pipe the engine drains
  itself -- and **the compiler's diagnostics as records** (a hook on GHC's logger): file, line, column, severity,
  code, message.
- **A query**, answered by the engine itself, in GHCi's own monad, without going through the command line:

| query | answer | it replaced |
|---|---|---|
| `state` | the directory, how many modules of the graph are loaded, and each unit's dependencies and object files | `:show paths`; looking for `Ok, N modules loaded.`; parsing cabal's per-unit argument files to guess which objects a server runs |
| `typecheck` | does this expression have type `IO ()` | `:type` and looking for "error" in what it printed |
| `fork` | compile the expression, fork this process running it, wait on the child | a `zygoteFork` call built as a string, which needed this package's library in the project's `build-depends`; a second command to reap the child |
| `prune` | unlink the CAFs the last reload superseded, collect, report the live heap | the same through a project-side module, against C libraries the daemon compiled at boot from `nm` of the RTS |
| `capabilities` | `setNumCapabilities` | a command |

So the verdict of a load is data: `COMPILE-ERROR` when the compiler logged an error or a module of the graph is not
loaded, and `status.json` carries `diagnostics` for a tool to read. (One thing is still read from the output: an
error GHCi printed without logging it -- a link failure thrown as an exception.)

**The daemon starts the engine itself.** The build tool is run once with the engine as its repl program and
`GHS_CAPTURE` set: cabal builds what the repl needs and starts "the repl", which writes down its arguments,
directory and environment (`.ghci-session/<session>/launch/`; a multi-unit repl's per-unit argument files are
copied, because cabal deletes them) and exits, and cabal with it. Then the daemon runs the engine with those
arguments, its socket as the engine's standard input. Three things follow:

- the process under the daemon IS GHCi. There is no `cabal repl` between them for the life of the session, no
  wrapper script; if GHCi dies, its exit status or signal is in the verdict;
- stopping it is closing its socket;
- **a restart that changes nothing cabal decides does not run cabal**: a restart for memory (`repl_budget_mb`)
  reuses the recorded start (`restart --fast` by hand). A changed `.c`, `.h`, `.cabal` or `cabal.project`, and a
  plain `restart`, ask cabal again: it is cabal that compiles a package's C before the repl starts.

The hygiene C (the pruner, the census) is compiled into the engine by cabal like any other source. It finds the
RTS's private lists by name in the symbol table of the RTS image the process has mapped (`hygiene/c/rts_syms.h`),
so there is nothing to build per session and nothing tied to one build of the RTS's addresses.

A target with its own `"repl"` command says where the engine goes with `{engine}`:
`"repl": "cabal repl --with-repl={engine} exe:foo"`.

Two things found on the way: started with its standard descriptors closed, a process's first pipe IS descriptor 0,
and "duplicate onto 0, then close the original" closes what it just installed; and GHCi leaves standard output
unbuffered, which is a system call per character -- 4.8 s for a megabyte -- so the engine line-buffers it.

## What each operation costs, end to end

Measured on this package's own session (11 modules; macOS arm64, GHC 9.14.1), from the shell prompt to the answer.

| operation | before | now | what changed |
|---|---|---|---|
| `eval EXPR` | 13 ms | 8 ms | answered in C before the Haskell runtime starts (`cbits/ghs_fast.c`: the session named, or the only one running); 6 of the 8 are the executable being mapped |
| `status` | 23 ms | 13 ms | the leftover scan asked for the ARGUMENTS of every child of launchd (324 of 402 processes); now only of those named like ours |
| `gc -n` | 30 ms | 14 ms | the same |
| save of a leaf module, with its check | 1.2 s | 0.4 s | the reload no longer costs a re-link of everything (below) |
| reload with nothing changed, then the check | 1.0 s | 0.25 s | the same, and the check's own waits |
| first `eval` after an edit | answer + 0.15 s | answer | the unlink and its GC run after the reply is out |
| `stop` | 0.55 s | 0.03 s | the daemon's accept loop looked at its stop flag between connections, and sat out its half-second wait first |
| `start`, nothing to build (the example) | 2.0 s | 1.3 s | cabal is not run when its answer still stands (below), and GHCi starts without ten trips through `xcrun` |
| `start` after a `cabal build` elsewhere (this package) | 5.7 s | 1.5 s | the same: cabal alone was 3.4 s of re-planning to arrive at the same arguments |
| a composed session changing members | 3.2 s | 1.8 s | one recorded start per member set |
| a commit, to its full reload starting | up to 2 s | at once | HEAD and the branch's ref are watched, not looked at every two seconds |
| the self-test (the check) | 0.55 s | 0.22 s | it waited for things that had already happened |

**A reload relinks what changed, not everything.** GHC's `load` forgets every object it had linked -- the driver
no longer works out which modules are stable -- so after ANY reload, even one that recompiled nothing, the next
evaluation links every loaded module again into a new temporary library. That is time proportional to the session
instead of the edit, a fresh copy of every CAF of every module (the leak the pruner exists for), and every value a
module had computed and kept, computed again. The engine remembers what was linked before a command and, after it,
puts back in the loader's table every module whose object file did not change and that depends, transitively, only
on such modules (`keepLinked` in `engine/GhsEngine.hs`). So after editing `A`: `A` and what imports it are linked
again; a module that does not depend on `A` keeps its code, **and a CAF in it keeps its value** -- a table that took
3 s to build is still there after an unrelated edit. `daemon.log` says `reload: 9 module(s) stay linked, 2 to link
again (A B)`. `GHS_KEEP_LINKED=0` in a target's `env` turns it off. (`tests/test_e2e.py`, `KeepLinked`.)

**Loading a new library costs a quarter of a second on macOS, whatever its size, and it is not GHC.** After an
edit, the first evaluation that needs the recompiled code links it into a temporary dylib and `dlopen`s it. The link
is 60 ms. The `dlopen` is 250 ms -- for a 17 KB library as for a 1.5 MB one, and 0.3 ms the second time: every new
executable file is scanned by XProtect when it is first loaded (twelve fresh dylibs: 3.2 s of wall, 2.2 s of
`XprotectService` CPU). It is most of what a save costs once the compile is short, in any GHCi, and nothing in this
tool can avoid it. macOS can: **System Settings -> Privacy & Security -> Developer Tools**, add your terminal
(`sudo spctl developer-mode enable-terminal` makes the list appear) -- processes started from it are then not
scanned. Not measured here: it needs an administrator.

**Cabal is asked how to start the repl once per command, not once per start.** Its answer is recorded
(`launch/<hash of the command>/`) with what it depends on: the command, the engine, every build file and
non-Haskell source the session watches, and -- found in cabal's own `plan.json` and each package's `.cabal` -- the
sources of every local package the repl uses without loading. While none of that has changed, a `start` goes
straight to GHCi; when a dependency's source has, cabal runs and rebuilds it. A `restart` by hand always asks
cabal (`restart --fast` to not), and so does a changed `.c`, `.h` or `.cabal`. If cabal's plan does not say where a
local dependency's source is, the session cannot see it and cabal is always asked, unless told otherwise
(`start --fast`, `"fast_start": true`).

**GHCi asks `gcc` where each system library is, ten times as it starts**, and on macOS `gcc` is a shim that asks
`xcrun` which compiler to run: 30 ms a time, 0.2 s of a 0.45 s start, and again for every library linked. The daemon
passes the compiler itself (`-pgml`, `-pgmc`, with the SDK the shim would have named in `SDKROOT`), when the `gcc`
on PATH is that shim.

Smaller things: GHC follows each such link with two `otool`s and an `install_name_tool` to add rpaths the library
does not need (everything it names is already loaded): `-fno-use-rpaths` in the repl's options, 0.08 s. The pruner
left a superseded CAF whose value was still young for the next reload's pass -- and the CAF the check has just
evaluated is always that one; two minor collections before the pass age it, and the heap of a session that reloads
is flat instead of one generation behind.

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
| boot, cold (cabal configures and compiles) / warm (objects on disk) | 10.8 / 1.8 |
| `eval` | 0.09 (the Python client's start-up is most of it) |
| `reload`, nothing changed / `--no-check` | 0.6 / 0.1 |
| save to verdict (the watcher: file event, reload, check): a comment / a real change / a compile error | 0.9 / 0.6 / 0.17 |
| composed session of two packages, boot | 7.5 |
| `server start` with a 2 s prefork | 2.3 |
| save to verdict with a server: kept (comment) / re-forked with its state (2 s prefork) | 1.1 / 3.8 |
| the same re-fork in the background (`async_refork`): the verdict / the server up | 0.85 / 3.2 |
| `compose --remove`: the repl restarts, the server is adopted | 2.2 |
| census: every CAF / one value | 0.7 / 0.4 |
| a reload that is over the memory budget (a restart) | 1.8 |
| a commit to a full reload (`reload_on_commit`) | 1.8 |
| `gc` | 0.1-0.3 |

**Where a step's time goes** is in the daemon's log and in `status.json` (`phases_s`), for every boot, reload, restart
and server command:

```
[time] boot 2.31s: load 0.77, check 0.56, build 0.43, other 0.55
[time] reload 0.75s: check 0.49, prune 0.17, ghci_reload 0.08, other 0.01
[time] reload 3.50s: prefork 2.01, check 0.88, prune 0.20, fork_verify 0.18, server_stop 0.11, ghci_reload 0.09, ...
```

`load` is cabal and GHC; `check` and `prefork` are yours. Everything the tool adds to a reload is under 50 ms. It
was not always, and the breakdown is how each of these was found:

- 0.4-0.8 s of every reload went to asking the OS for the repl's memory footprint, three times, inline. It is now
  sampled once, after the reload returns, and reused by the next reload's budget check.
- 0.65 s of every save was the watcher's poll and debounce. Now a kernel file event and a 50 ms quiet period.
- Every reload re-issued the session's `:module +` imports (40 of them, 0.1 s): now only after a reload that failed,
  which is the only kind that drops them.
- The script that prepares the pruner ran `nm` on the RTS library once per symbol, sixteen times, before looking at
  whether anything was stale: 12 s of every boot of a project that used it. One listing per run: 0.45 s.
- The pruner's cost is not the pruning. Finding and unlinking the superseded CAFs is microseconds (5,549 of them
  in 0.02 s on a 98-module session); the major GC that follows is all of it (0.5 s at 250 MB live). It runs only
  when something was unlinked, and it shows in the log, not in `[time]`, when it follows an `eval`.

**Do not defer that GC.** Running it later, when the session is idle, takes it off the reload path -- and crashed
the repl. On a 98-module session, a sequence of reloads and evaluations that read values kept across reloads
killed GHCi every time the GC ran later than the unlink (three runs of three: an RTS internal error
`scavenge_mark_stack: unimplemented/strange closure type 0`, or a death in the next evaluation) and never when it
ran at once (three of three, with either unlink timing). One cause is found and fixed -- a superseded CAF whose value
is still young must stay on the list (`hygiene/repro`, a 10 s reproduction that kills GHCi without the check) -- but
with that fixed the big session still dies when the GC is deferred, so there is a second cause, not yet reproduced
in the small. `prune_gc_idle_s` stays at `0`; the deferred mode is still there for whoever wants to find out.

And the reason for the pruner, measured by the same tour -- live heap (MB) after each of five edit-reload-check
rounds of a module holding one 200,000-entry `Map`:

| | start | 1 | 2 | 3 | 4 | 5 | grew |
|---|---|---|---|---|---|---|---|
| object code, no pruning | 80 | 129 | 177 | 226 | 275 | 323 | +243 MB |
| `hygiene: true` | 98 | 98 | 98 | 98 | 98 | 98 | +0 MB |

One copy of the table per reload without it. (An earlier version of the pruner settled one copy higher, at 147 MB:
it unlinked right after `:reload`, but GHCi links a reloaded module into its new library only on the first
evaluation that needs it, so the generation just replaced did not yet look superseded and survived until the next
reload. The unlink now waits for the check, or the first `eval`.)

## Install

`cabal install exe:ghci-session` from this directory, or run it from the checkout: `bin/ghci-session` builds the
executable into `.bin/` when it is missing or older than its sources, then runs it (`./build.sh` does the build).
`./build.sh` puts both executables in `.bin/`; the engine must sit beside `ghci-session`, and must have been built
with the compiler on PATH (it says so if not). A project adds this package to its `build-depends` only to call the
library from its own code (the census, a server's state handover: see `examples/hello`).

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
| `warm` | `[]` | expressions evaluated in the background after a reload that ran no check (`--no-check`, `watch_check` off), e.g. `"My.thing `seq` ()"`: GHCi links the reloaded code, and the unlink and its GC run, while you read the verdict rather than on your next command |
| `ghc_jobs` | `0` | `-jN` for GHCi's compiles. It helps only a reload that recompiles many modules; on a 98-module session an interface change recompiled two (GHC's recompilation avoidance) and `-j8` changed nothing |
| `check` / `checks` | none | `expr` to run after a good load; lines matching `fail` (default `^\[FAIL\]`) fail it, `pass` must appear; `log`: a file the check writes its real output to; `name` labels a second check |
| `server` | none | see *Servers* |
| `env` | `{}` | environment of the repl, and of the target's server |
| `repl_budget_mb` | `6144` | past this, a reload is a restart; `0` disables. Env `GHS_REPL_BUDGET_MB` overrides |
| `rts_flags` | `-c` | GHCi's own RTS flags (the daemon starts it: `+RTS ... -RTS`); `none` for none. `-c` is the compacting old generation: on a 98-module session with 250 MB live, the repl's footprint was 1,251 MB copying, 1,094 MB with `-c`, 920 MB with `-c -F1.5` (and its forked server 569 / 412 / 385 MB), for 9.5 / 17.1 / 25.2 s of GC over a 100 s scenario. The non-moving collector is refused with `hygiene` (the pruner edits lists it reads concurrently: the repl died) |
| `capabilities` | `0` | `setNumCapabilities` in the repl (GHCi evaluates on one; more buys the parallel GC) |
| `hygiene` | `false` | unlink superseded CAFs after each reload, report memory |
| `unlink_after` | `eval` | when a reload's unlink happens: after the first evaluation (the check, or an `eval`), when the code that replaced it is linked; `reload` is at once, which reaches one generation less |
| `prune_gc_idle_s` | `0` | `0`: the GC that frees what was unlinked runs at once. A positive value defers it to an idle moment and HAS CRASHED the repl (see the tour section); leave it |
| `auto_reload` | `true` | reload when a watched file changes (a `.c`, `.h` or `.cabal` change restarts instead: a loaded C object, or a package set, cannot be replaced) |
| `idle_stop_mins` | `0` | the session stops itself after this long unused (never while it serves). A composed session idles out only if every member sets it, at the longest |
| `async_refork` | `false` | a reload returns at its verdict and re-forks the servers in the background (also `reload --async-refork`, env `GHS_ASYNC_REFORK=1`) |
| `watch_check`, `watch_refork` | `true` | what a SAVE does beyond compiling: run the checks, cut the servers over. Off, an explicit `reload` (or a commit, below) does them |
| `reload_on_commit` | `false` | a new git HEAD is a full reload -- checks and re-fork -- whatever the two above say |
| `status_url` | none | POST every verdict there as JSON, the intermediate ones too (`reloading`, `running check`): a dashboard's event feed. Best effort, 0.25 s |
| `handover_env` | `GHS_HANDOVER_OUT`, `GHS_HANDOVER_IN` | the two variables a forked server finds its state paths in (a project with its own copy of `GHC.Hygiene.Zygote` may name others) |
| `fast_start` | `false` | a start skips the build tool by itself when it can see everything the tool would build (see *What each operation costs*). This says to skip it also when it cannot -- a local dependency whose source cabal's plan does not place; then building that is yours (`start --fast` once) |
| `fingerprint_files` | `[]` | extra files that are part of a server's code (a C bundle) |
| `watcher` | `auto` | kernel file events where the platform has them (kqueue on macOS/BSD, inotify on Linux), else `poll`. The mtime scan still decides what changed and still runs every 2 s: an event only says "look now" |
| `poll_interval`, `debounce` | 0.2, 0.2 | when polling: how often the watcher looks, and how long it lets a burst of writes settle (with events a burst is over when they stop for 50 ms) |
| `load_timeout`, `eval_timeout` | 900, 600 | seconds |

State lives in `.ghci-session/<session>/`: `status` (the verdict, then the failing lines), `status.json`, `load.log`/`reload.log`,
`run.log` (the checks), `daemon.log`, `async.log` (output a background thread printed between commands), `server-<member>.log`.
`loaded_sources.tsv` is the signature the loaded code was built from (`<mtime ns>\t<path>`), for a cache in the loaded
code that is keyed by source. A reload publishes its status ONCE, when the verdict and what happened to the servers
are both known. A failing check in a state dir where none has ever passed is marked `[NEVER-PASSED]`: suspect the
target as much as the edit.

## Commands

```
start [--no-check] | stop | restart | status [-d] [SESSION]
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

Two components of the SAME package (a library and its executable, two executables) cannot be members of one
session: the session gives every unit one object directory relative to its package, and they would overwrite each
other's `Main.o`. Give them a session each (this package does: `tool` and `engine`).

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

The C is part of the engine. The library is a Haskell front for it, for a project's own code: it looks the C up in
the running process, so in the session's GHCi it works and anywhere else it says there is nothing to find.

```haskell
GHC.Hygiene.pruneCafs :: IO Int          -- unlink the superseded CAFs, then a major GC if any: -1 an RTS it cannot read, -2 not the engine
GHC.Hygiene.unlinkCafs :: IO Int         -- the unlink alone. A GC that comes LATER has crashed GHCi: use pruneCafs
GHC.Hygiene.loaderStats :: IO Int        -- what the RTS linker holds, to stderr

GHC.Hygiene.Census.cafReport 10 100000000    -- what every CAF retains, by CAF and by constructor
GHC.Hygiene.Census.cafStrings 10 100000000   -- the Strings among it (24 bytes a character)
GHC.Hygiene.Census.censusOf "x" Mod.value    -- ONE value, alone
GHC.Hygiene.Census.keep "name" v >> keptReport 100000000   -- values you hold on to across reloads
GHC.Hygiene.Census.benchOf "label" action    -- wall, GC, allocation, live heap
GHC.Hygiene.Census.memNow

GHC.Hygiene.Zygote.setHandoverExporter, handoverInPath   -- a server's state, out on SIGTERM and in at start
GHC.Hygiene.Zygote.zygoteFork / zygoteStop               -- the fork the engine does, for use by hand
```

**Why the leak exists** (a GHC behaviour, not yours): with a dynamically linked GHC every `:reload` links the recompiled
modules into a *new* temporary shared library and never unloads the old one, and the RTS keeps every CAF in those
libraries as a root, so the evaluated value of every CAF of every superseded module stays live. In the project this was
extracted from it was ~240 MB per edit. `pruneCafs` unlinks the CAFs of a library that is wholly superseded (every
exported symbol now resolves to a newer one), plus exported CAFs whose name resolves elsewhere.

`examples/hello` shows it working: six CAFs unlinked per reload, the repl flat at ~505 MB over repeated edits.

**Safety.** The C finds the RTS's private lists by name in the mapped RTS image's symbol table; where they are not
there it returns `-1` and the session just does not prune (and says so once, in `daemon.log`). The object and
closure layouts it then reads are GHC 9.14.1's, which is the compiler the engine is built for.
Tested on GHC 9.14.1, macOS arm64. `keepCAFs = 0` is *not* an option (SIGBUS: interpreted code refers to CAFs by raw
address, GHC #23182).

## Layout

```
ghci-session.cabal      the package: the engine, the command, the library
app/GhciSession/        Json, Config, Sys (the FFI), Repl (starting the engine, its protocol), Watch, Daemon, Gc, Cli, SelfTest, SelfBench
engine/, vendor/        the engine: GhsEngine.hs and Main.hs (ours), GHCi's own sources per compiler version
cbits/                  ghs_sys.c (sockets, file events, regex, hashing, processes), ghs_main.c (the entry point)
hygiene/                c/*.c (the pruner, the census: compiled into the engine), src/GHC/Hygiene*.hs (the library),
                        repro/ (why a superseded CAF with a young value must stay listed)
bin/ghci-session        run from a checkout (builds if stale)
ghci-session.json       the session this package runs on itself
examples/hello/         two packages, a CAF that leaks without pruning, a server with state to hand over
examples/tour.py        every feature on a copy of it, each step checked and timed (the benchmark)
tests/test_e2e.py       GHS_E2E=1: the lifecycle end to end, and the CAF reproduction
```

## Status

Working: plain and composed sessions, per-member checks, auto-reload, verdicts and staleness, memory budget, pruner,
census, forked servers (keep / re-fork, also in the background / handover / adoption), `gc`, idle stop; 65 self-tests,
the tour (123 steps) and the end-to-end tests. This repository's own sessions run on it (`tools/msq` is a thin front
end: it keeps `ghci-session.json` generated from `tools/model_session/targets.json` and adds the project's commands).

Not here: the deferred GC after an unlink (it crashed a large session; the GC is immediate). A compiler
other than GHC 9.14.1: the engine is that compiler's front end, so another needs its sources vendored and has not
been tried. Linux: the C has `/proc` and inotify code paths that have not been run, and the pruner reads a Mach-O
symbol table (on ELF it finds nothing and the session runs without pruning). Port verification needs `lsof`.
