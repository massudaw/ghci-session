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
and runs on Linux and macOS. It is built for large projects: hundreds of modules, servers forked from the repl,
days-long sessions.

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

What a save does depends on where it is:

| a save in | does |
|---|---|
| a loaded package, `.hs` | a reload |
| a loaded package, `.c` / `.h` | the C is compiled and taken by the running repl, no restart (GHC 9.14; a restart where that cannot be done) |
| a `.cabal` or `cabal.project` | the build tool is asked what it changes; a restart if it is a new package set |
| a local package the repl uses without loading (a library the executable depends on), `.hs` / `.c` / `.h` | that package is built and the repl restarted: it is object code, so it cannot be reloaded |

The sources of those packages are found from the build tool's plan and watched without being listed in `watch`.

A loaded C object cannot be replaced, but it can be superseded, as a reloaded module is. The changed source is
compiled with the build tool's own command, the object linked as the newest, and the modules that call into it
(its unit's, and whatever imports them) linked again when next needed, bound to the new C. A header is every C
source of its unit. The command is found at each start, without the build tool: put together from the unit's
flags and the `.cabal`'s options for C, and kept only if compiling an unchanged source with it makes the build
tool's own object, byte for byte, for every source of the unit. Where no candidate does, the build tool is asked once, at the unit's first
change (which takes what a build takes). Either way it is kept with the start it belongs to, and a C save is a
few tenths of a second. C that does not compile is a `COMPILE-ERROR` with the compiler's words, and the
session goes on running what it had. Two things to know: the library with the old C stays mapped, so a pointer
into it stays good, and **what the C keeps in its own variables starts again from nothing** in the new object --
a handle it opened, a table it filled. A restart is what it was: a source of no loaded unit, a build file changed
with it, a session with a `repl` command of its own.

To make an edit to such a package a reload instead, load it: name it in `units` beside the others
(`"units": ["lib:app", "lib:its-library"]`). The build tool's multi-unit repl takes libraries, not executables, so a
package that is only an executable has to be a library with a thin executable over it first -- as this one is
(`ghci-session.json` here loads `lib:ghci-session` with the three libraries it uses, and a save in any of them
reloads in a few tenths of a second). Units with C work: the engine is given every unit's C objects itself, since
GHCi links only one unit's.

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
import [--tools] [--all] [--since DATE] [--go]   # chats had in Claude Code and Codex, into the history
top [SESSION]                   # a screen that follows a session: verdict, history, heap, a chat and a shell
chat [-s SESSION] [--tui] [--once TEXT]   # an agent working on the session, with the history as its memory
knowledge [--subject S] [--all]           # what the sessions established, by subject
usage [SESSION] [--since DAYS] [--json]   # what the model calls cost
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
| `optimize` | load the session's code compiled and optimised (`-fobject-code -O1`) instead of interpreted: an evaluation of it, and what `bench` measures, run at the speed of the built code (default off: a reload compiles faster) |
| `optimize` level | `"optimize": 2` loads at `-O2` (`true` is 1) |
| `restart_stuck` | an evaluation that ran out of time and cannot be interrupted (a loop that does not allocate) is ended by restarting the repl, and its answer says so (default on; off: the session runs it to its end before answering anything) |
| `watch_typecheck`, `watch_refork` | typecheck a save before reloading it (on); re-fork servers on a save (on) |
| `reload_on_commit` | a new git HEAD is a full reload, with checks and re-fork |
| `server` | `action`, optional `port`, `prefork`, `env`, `serve_on_load`, `verify_timeout` |
| `hygiene` | unlink superseded CAFs after each reload and report memory |
| `repl_budget_mb` | past this, a reload is a restart (default 6144; `0` disables) |
| `line_budget` | a project's rule for the lines of a file: the chat's `read`, `write` and `edit` then say a file's count against it (`[OVER BUDGET: 260/250 lines!]`). Absent: nothing is said of lines. The `vfs` tool keeps its own `budget` argument (default 250) |
| `rollover_ratio` | what a token written to the provider's cache costs over one read from it (12.5): what `chat --rollover auto` weighs a cold call by (default 12.5) |
| `on_turn_end` | a shell command the chat runs when a turn ends (told `GHS_SESSION`, `GHS_TURN_SECONDS`, `GHS_TURN_TOOL_CALLS`, and the last words on stdin); see "A line for a chat started somewhere else" (default none) |
| `rts_flags` | the repl's RTS flags (default `-c -Fd0.5`) |
| `idle_stop_mins` | stop the session after this long unused; never while it serves |
| `load_timeout`, `eval_timeout` | seconds; a command past its timeout is interrupted, not abandoned |
| `history`, `summarize_cmd` | keep the session history; the command that writes its summary lines |
| `summarize_jobs`, `compact_prompt` | compactions run at once (64; fewer for an endpoint with a low rate limit, or a model on this machine); `"shared"` gives them the turns' one system prompt |
| `prices` | the model's rates per million tokens, so `usage` can say money |

State lives in `.ghci-session/<session>/`: `status`, `status.json`, `load.log`, `reload.log`, `run.log`,
`daemon.log`, `server-<member>.log`, the history, and `loaded_sources.tsv`, the signature the loaded code was built from.
A failing check in a state directory where none has ever passed is marked `[NEVER-PASSED]`: suspect the target as
much as the edit.

**Measuring at an optimisation level: `bench --opt N [--unit COMPONENT]`.** `bench` times an IO action in the
session; with `--opt 2` (the tool's `opt`) it is timed in a session of its own that has the same code at that level
-- `NAME-O2`, beside the session, started on the first call, reloaded with what changed on each, stopped after half
an hour unused -- and with `--unit exe:NAME` (or `bench:`, `test:`) a component of the build is loaded with it, so
its own code can be run (`System.Environment.withArgs [..] Main.main`). What a built executable at `-O2` measures,
without building one: the first call compiles (half a minute for a small library), the next takes as long as the
action.

**What a value holds, by where it was allocated: `census EXPR --sites`.** `census EXPR` says what a value is made
of, by constructor, in a fraction of a second. With `--sites` (the tool's `sites`) each line is a constructor AT A
PLACE -- `DXF.Entity.Poly.Vertex @ src/DXF/Entity/Fast.hs:63:28-29 (ent)`: the file, the span, the binding -- and a
thunk or a function is named by where it is defined. It is asked in a session of its own beside the session
(`NAME-O1s`; `--opt N` for another level), whose code is compiled with its info tables mapped to the source and a
constructor's table apart for each place it is built at (`-finfo-table-map -fdistinct-constructor-tables`; `"sites":
true` in the configuration has the session itself so). No profiling build: the first call compiles, the next takes
as long as the walk. It tells what is LIVE; what was allocated and is garbage already it does not see. (GHC 9.14.)

A session of SEVERAL units is given its optimisation's flags for the session itself, not only for each unit: the
build tool puts a unit's flags in the unit's file and none on the command line, and the code then ran as if it had
not been optimised (a 19 MB drawing printed in 2.0 s and 6.4 GB of allocation; 0.10 s and 140 MB with the flags, or
as a built executable). The objects of sessions from before are compiled again once.

`ghci-session.json` is read again while the session runs: a second after it is saved the session restarts its repl on
it (a check set, a unit or a watched directory added, an option changed), and so does `restart`. What only a new
daemon takes -- the history and its compactor -- needs `stop` and `start`. A file that does not parse is said in
the log, and the configuration is kept as it was.

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
path, never by name, so a sibling checkout's healthy session is not touched. `gc` also removes the state directory of
a session that never loaded (a sibling such as `tool-O2` whose boot failed or timed out: no daemon, status `loaded=-`,
no history, idle over ten minutes); `gc -n` lists what it would do.

## History and agents

The daemon sees everything done to a session: client requests and their answers, saves with the verdict they compiled
to (and their diffs), commits, restarts. It writes each as a line of `.ghci-session/<session>/history/main/YYYY-MM-DD.jsonl`
and never edits one: `tool` (the request, as one line), `echo` (the answer: an evaluation's output whole, a verdict with its failing lines, the `STALE` warning the client
was given),
and, from a harness, `user`, `talk`, `work` and `note`. A line is written with one write and an fsync, and a torn line
is skipped at load. `status` and `info` are not logged: a tool polls them. The daemon keeps of each line where it is on
disk, its kind and its size -- not its text, which is read when a `zoom`, a compaction or the view asks for it: a
history of 60,000 messages, 112 MB of files, is 35 MB of the daemon's memory. A text over 30,000 characters is
several messages in a row, cut at a line's end, each saying which part it is (`[part 2 of 7 of message 461]`) and
where it goes on: nothing is dropped, and the middle of a long test run is a `zoom` away like the rest. (Past a
million characters the head and tail are kept.) A save's line carries the diff of each
changed file against the copy taken when it was last seen, so the log says what was edited, not only that a file was.

```
history                       # the log: every request and its answer, every save and its verdict
view --wait 10                # the whole history as one-line summaries, oldest first
zoom 2184 8                   # open line 2184+8 of the view into the two lines it was made from
zoom 2187 1                   # message 2187 whole
date 2187                     # when it was written
```

### Chats had elsewhere: `import`

```
ghci-session import                       # the plan: what would be imported, and what its summaries cost
ghci-session import --go                  # do it (with the session's daemon not running: one line, exit 0, nothing read or built)
ghci-session import --tools --since 2026-09-01 --go
```

What was done to this project in Claude Code (`~/.claude/projects/<the project's path>`) and in Codex
(`~/.codex/sessions`, the ones of this directory) is read from their session files into the history: a `note`
saying what each session is and when it began, then the user's words as `user` and the other agent's as `ai` --
not `talk`, which is this agent's own -- with their dates; `--tools` takes its tool calls and their results too
(`tool`, `echo`: several times the messages, and mostly long). What a program put into a chat and nobody said is
left out: a command's echo, a reminder, its own bookkeeping lines.

Two things decide what is taken. Whose session it is: a program that drives the model through the SDK leaves a
file for every call -- of this repository's own 1,135, all but three were a compactor's, a prompt and a line each
-- so a session an SDK started, or one of a single exchange, is passed over (`--all` takes those too). And what
was taken before: each session's last imported message is written down (`history/imported.json`), so a second
import takes what is newer and an import stopped half way goes on. (A session imported without its tools does
not get them later.)

Nothing is written without `--go`: the plan lists the sessions and says how many messages are over a line's size
-- each of those is a model call when there is a `summarize_cmd`, and about as many again for the merges above.
The messages go at the end of the log, where their summaries are made like any others; while that backlog
stands, the compactor also takes the newest messages, so a turn's own last lines are not left waiting behind it.
`--claude DIR` and `--codex DIR` read sessions from somewhere else.

### The summary tree

Over the log the daemon keeps a binary tree of one-line summaries, so that a model can start each turn from a bounded
view of everything that happened instead of from nothing. Node `(l, i)` covers messages `[i*2^l, (i+1)*2^l)`; a line
is at most 512 bytes; a parent is made from its two children; a message or a pair that already fits *is* its node with
no model call, so a routine verdict (`OK -- CHECK-PASS (0.4s)`) costs nothing. The **view** is the list of nodes
tiling the whole log, oldest first, one line each as `id+n|text`. It holds only summaries, never a message whole, and
a line not summarized yet renders as `(not summarized yet: zoom it)`. Two rules decide it (`GhciSession.History`, both
covered by self-tests):

- **Which lines merge.** Detail fades in proportion to age: old lines stay put and new ones churn, like the carries
  of a binary counter. For sibling lines at level `l`, with `T` messages in the log, `due = (T - last) / 2^l`, where
  `last` is the pair's last message. The most due pair whose parent is built merges, the oldest of equal pairs first.
- **When they merge.** In batches. A message appends its line and nothing else changes; once the view passes 128,000
  bytes one batch merges it down to 64,000. So between two batches a call's view is the one before and a few lines
  more, which is the prefix a provider caches. A batch merges only pairs whose parent is built.

The view is saved at every message (`history/view.json`) and loaded at start, never rebuilt from the log: a rebuilt
view differs from the live one and every cache entry would die. A history without one is folded once.

### The compactor

Summaries are written in *compactions*: a system prompt, the compaction's own view, then its task. The view is the
chat's view merged further, to 16,000-32,000 bytes with the same sawtooth, with its ids, up to the node being built
and to the first line not yet built, so no call sees a placeholder or half a message. The task says which message or
which two lines, the size, and shows it as a ruler of 512 dashes (a model cannot count bytes); the input is in
`<input>` tags. A line over 512 bytes is asked again, up to five times, and the shortest kept as it is (one over 1,024
is cut at its last word); an answer that is no line (the task said back, a tag alone, a code fence, nothing) is not kept and not asked for again. A
message's node starts once fewer than `summarize_jobs` (64) messages before it are unbuilt, a merge once both halves
are built, that many at once, from queues kept as the tree changes. When no compaction has been answered in four minutes, or the view was merged since, one call goes first and the rest wait for its answer: they then read the prompt from the provider's cache instead of each paying for it. A command that fails or times out is tried again after ten seconds, and while it fails no call is started for 5 s, then 10, 20, ... up to five minutes; one whose answer is no line leaves the node as its input, cut at the size, so the merges above it are not held up.

The daemon runs a compaction through `"summarize_cmd"`: a shell command given the system prompt, the view and the task
on its standard input, answering one line on its standard output. `"summarize_cmd": "ghci-session summarize"` is the
tool's own compactor, which sends them to an OpenAI-compatible chat endpoint over HTTPS (libcurl, found at run time by
`cbits/ghs_http.c`, so nothing outside the boot packages is linked). It defaults to DeepSeek's flash model
(`DEEPSEEK_API_KEY`; `DEEPSEEK_MODEL`, `DEEPSEEK_BASE_URL` or `OPENAI_*` override), without thinking, since a
reasoning model spends a summary's whole budget on thoughts (`SUMMARIZE_EFFORT=low|high|max` turns it on). A cut-off
answer is asked again with room for a line; a model that calls a tool is asked again without them. Any server that
speaks `/chat/completions` works (`OPENAI_BASE_URL=http://localhost:11434/v1` for a local one, with `OPENAI_MODEL`)
with a context of 20,000 tokens or more. Without a command the log and the free nodes are kept and the tree waits for
an outside compactor: the `pending` operation answers the nodes ready to build and `tree_put` takes a line. The tree
is stored in `history/tree/` and never recomputed. `"history": false` turns all of it off.

The system prompt is split by default (`turnPrompt` for a turn, `compactPrompt` for a compaction, which says who is
writing and goes without tools), because a small model given one prompt takes a compaction for a turn.
`"compact_prompt": "shared"` makes it one prompt, with the turns' tools, so a compaction reads them from the turns'
cache entry (`SUMMARIZE_TOOLS=0` for a model that takes no tools).

How long a line is ASKED to be is steered by how the lines come back. A line is taken up to 512 bytes, and one over
that is asked for again -- a second call for the same line; a model asked for 512 wrote 900, most times, 1.7 calls a
line. So of each first answer the compactor keeps its length over what was asked (a moving mean and spread) and
whether it was over the limit, and asks the next at the limit divided by that ratio with room to spare -- within a
third of the limit and the limit itself -- and in stronger words the more of them miss. A model that writes long is
asked for less; one that writes short is given the room back. Compressing a message and merging two lines are
steered apart. The daemon's log says each time what is asked changes.

### What is known, by subject, across sessions

A history is a chain in time: its newest lines are fine, its old ones coarse, and a rule the user gave once is
summarized away with the lines around it. Asked "what holds now", a model reading two real histories joined in
time order answered with the outdated way as often as with the current one (13 and 13 of 42); in the wrong order
it mostly did not know. So what the sessions ESTABLISH is kept apart from any one of them, for the person:

```json
"knowledge": true
```

(in `ghci-session.json`, with a `summarize_cmd`: the compactor's command is the one asked.) A fact is a record --
a subject, a topic, one sentence, when it was first learned and last confirmed, the message it came from -- and is
never rewritten. The subjects are `tool/usage` and `tool/config` (true of this tool on any project), `user/rules`
(how the user wants work done anywhere) and the project's own: `P/architecture`, `P/performance`, `P/testing`,
`P/status`, `P/rules`, where P is the project directory's name.

- **Extracted** by the daemon as the log grows: a message of the user's, the agent's or a note is asked for its
  facts (three at most, usually none); the tools' traffic is asked 128 messages at a time, as the lines the
  compactor made of it. An argument a tool is called with for the first time is a fact with no model asked: a
  way of working that changed without anyone saying so.
- **Reconciled**: a new fact is set against the nearest that hold (by the words they share), and the model says
  only whether it is new, restates one (which is then confirmed), replaces one (which is kept, marked), or is
  not worth keeping. Its subject's latest five are always among them (a new "last commit" and the old share no
  word). With nothing to set it against, it is stored unasked.
- **Folded**: a subject whose facts pass 6,000 characters has the half least recently confirmed written again
  as at most five; the folded ones are kept, marked as replaced.
- **Read** as a block before the view (`<subjects>`, 16 KB, taken from the view's budget): the tool's and the
  user's subjects first (45%), the project's (40%), the others' by last use (15%); in a subject the facts most
  recently confirmed first, cut where its share ends. What the tools' own schemas already say is left out of it
  (a call-argument fact whose argument a tool's schema describes) and so is `tool/operator`, what only a person at
  the command line does (`import --go`, `chat --send`, the `i` key in `top`, `knowledge search`); both stay in the
  store, in `knowledge --subject` and in `top`'s tab. The extraction prompt is given the tools' one-line
  descriptions (before the log, so a provider caches it) and told not to extract what they say.
- **Appended, not rewritten**: the block is written when the view is rewritten (a batch merged its lines, so the
  provider's cache of the prompt is lost from there anyway) and is the same text in between. What is learned
  meanwhile is a `known` line in the session's history -- `known: user/rules: ... (THIS REPLACES: ...)` -- which a
  turn reads at the view's end.

```
ghci-session knowledge                        # the subjects, and how many facts each holds
ghci-session knowledge --subject user/rules   # its facts, newest confirmed first, with their ids and sources
ghci-session knowledge --subject S --all      # the replaced ones too
ghci-session knowledge --block PROJECT        # the block a session of that project would read
ghci-session knowledge --candidates           # tool/ and user/ facts confirmed on 3+ days or from 2+ projects, most confirmed first (for the system prompt; lists only)
ghci-session knowledge search WORDS            # the facts that hold the words, the best first
ghci-session knowledge forget ID
ghci-session history --search WORDS            # the messages of a session's log that hold them
```

An agent has both as one tool, `recall`: the facts known (from every project's sessions) and this session's
messages that hold given words. Keyword search finds what the view's summaries have dropped: on forty questions
about details of two real histories, the message that answers was among the ten found for 33, and a model
reading the view picked the line that covers it for 14 -- and then had five zooms to go. In `top`, `9` is what
is known, by subject.

They are in `$GHS_KNOWLEDGE`, or `$XDG_STATE_HOME/ghci-session/knowledge` (`~/.local/state/...`): one
`facts.jsonl`, only appended to, shared by every session of every project. Measured on the same two histories
with this in place (40 KB of view and the block): 30 of 42 answers current and none outdated, whichever order
the sessions came in; about one model call more in 26 messages. Not there yet: two facts worded with no word in
common, in different subjects, are not seen as the same (the search is by words, not by meaning).
`tools/check-knowledge.py` runs it end to end with a stand-in for the model.

### Two ways in for an agent

- **`ghci-session mcp`** serves the session's operations and its memory as tools over the Model Context Protocol
  (`claude mcp add ghci -- ghci-session mcp`, from the project's directory): `eval`, `status`, `typecheck`, `reload`,
  `test`, `doc`, `census`, `bench`, `mem`, and the memory's `view`, `zoom`, `date`, `history` and `remember` (a finding
  kept for later turns). Each call is a request to the daemon, so it is in the history like any other; the agent's
  edits are seen as saves, with their diffs.
- **`ghci-session chat`** is a turn loop over the same history: a fresh model call per message, whose input is the
  system prompt, the view and the message, with the session's tools plus `read`, `write`, `edit`, `edits`, `ls` and
  `sh`. Replies are logged as `talk`, thoughts shown and not logged, and a line typed while the agent works is
  delivered between tool calls. `-s SESSION`, `--once 'what was tried on X?'`, `--instructions FILE`, `--usage`;
  `--tui` puts it on a screen of its own (below). `--context view` builds every call from the log (the view up to a
  boundary, then the log after it word for word) instead of carrying a turn's steps as a conversation.
  `chat --restart` has a running chat run itself again as the executable on disk now, in the middle of a turn, with
  the provider's cache intact.
  `tools/fake-llm.py` is a stand-in endpoint for trying it without a key (`OPENAI_API_KEY=fake
  OPENAI_BASE_URL=http://127.0.0.1:8799`, or `GHS_PROVIDER=anthropic ANTHROPIC_API_KEY=fake ANTHROPIC_BASE_URL=...`): `eval EXPR` and `tool NAME [JSON]` make it call a tool, anything else is
  echoed, and a request without tools (a compaction) gets a one-line summary.

What the harness does for an agent that works on a session: every reply carries `stale`; a command past its timeout
is interrupted, not abandoned; a check that hangs is interrupted at five times the median of its last passing runs
(`CHECK-HANG`, with the last line it printed); a save answers once the code compiles, and the check's verdict rides
on the next tool result -- or, sooner, once the sources typecheck clean (a fraction of a second; the daemon's
`typecheck_cached`): the answer says the reload is under way, and an `eval` or `test` asked then queues behind it
(the watcher holds the work lock from before the typecheck to the end of the reload), so none runs on the old code; a
type error is answered at once, as before; `typecheck` answers at once when nothing changed; a read of lines the context already holds
answers with a pointer instead of the text; a session that is down is waited for (`GHS_CHAT_DOWN_WAIT`).

The agent's `read` (and `grep` with a `path`) shows a line as its number, the bar `│` and then the line exactly as
the file has it (`grep` puts `>` before the number of a match): the indentation is all the spaces after the bar. The
bar is no whitespace and not an ASCII `|` (a guard starts a line with that), so it cannot be counted among the
spaces; two spaces after the number, as it was, were: 15 of 60 edits of an agent's rounds were parse errors from
a new text indented one or two spaces too far.

An `edit` whose old text matches only with its spacing squeezed is applied when the indentation of its lines differs
from the file's by one constant (the new text's lines after the first are shifted by it, and the answer says so); when
the lines differ by more than that, or in their number, or a new line has no spaces to give, it is not applied and
the answer shows the file's lines as they are, to copy from. (All six squeezed matches of three rounds, applied as
written, were compile errors.)

**Anthropic's API** is spoken too (`GhciSession.Anthropic`: the Messages API, natively, not through a compatible
endpoint). With `ANTHROPIC_API_KEY` (or `ANTHROPIC_AUTH_TOKEN`, sent as a bearer token) and no key for the other
protocol -- or `GHS_PROVIDER=anthropic` with both -- the chat and `ghci-session summarize` use it:
`ANTHROPIC_MODEL` (default `claude-opus-5-5`), `ANTHROPIC_BASE_URL` (default `https://api.anthropic.com`; another
provider's endpoint that speaks the protocol works, and is sent only the conversation). What is its own:

- **The cache is asked for**, where the other protocol's endpoints cache a prefix by themselves. The system prompt
  is one marked block, with the tools before it; the view is sent as blocks of four lines with the last whole one
  marked, so a turn reads the view of the turn before from the cache as far as that block; the conversation after
  it is cached by the request's own mark, which the API moves along. Three of the four marks a request may have.
- **Thinking is the model's.** On the current models it cannot be turned off: a turn runs at effort `high` (or
  `--effort`), a compaction at `low`, both said in the request since the default differs between models. A reply's
  blocks are sent back as they came, its thinking with them, so the conversation is only appended to: superseded
  reads are not rewritten as stubs here.
- **The web, when asked for.** `chat --web N` (or `GHS_WEB_SEARCH=N`) lets the model search it, at most N times
  a call: the API's own tool, run by the API and charged by it for each search, so never on unless asked. A search
  and what it found are shown and logged as a tool's call and answer are; a turn the API stops in the middle of
  one (`pause_turn`) is sent back as it is and goes on.
- **Images.** `read` on an image (png, jpeg, gif, webp) shows it to the model, and so does a line typed that
  names an image's file (in the project or anywhere; a path dragged onto the terminal will do). The image is kept
  once in the session's state (`images/`, under a name made of its bytes) and what stands for it in the text is a
  line `[image NAME]`: the history and its summaries hold only the name, and the image goes with the text
  wherever a message is given whole -- when it is new, or when the agent opens it again (`zoom(id, 1)`). One
  larger than a model makes use of (a side over 1568 pixels, or over a megabyte) is sent as a smaller copy made
  by `sips` or ImageMagick. This is for the two backends here and the subscription's; an OpenAI-compatible
  endpoint is sent the line only. `chat --tui` draws the picture under the line in a terminal that shows them
  (below); `top` shows the line (`▣ image NAME`).
- **A request declined** (`stop_reason: refusal`) is said as such, and on Anthropic's API another model is asked
  in the same call (`fallbacks: default`).

A reply is read as it comes (the API's stream of events): the call is held to five minutes of silence, not to a
time in all, and the chat's status line says what the reply is doing -- thinking, writing, calling a tool -- and how
much of it has come. The events make the message the API would have sent whole, which is read as that one is; a
stream that breaks off is asked again. `GHS_STREAM=0` asks for the reply in one piece. (The other protocol's
replies are still one piece.) A call to either is asked again while the service is busy -- 408, 409, 429, 5xx, a
lost connection -- up to eight times in a turn, after the seconds the server asks for or else 2, 4, 8, ...; what
asking again cannot mend, a request refused, ends the call at once. The compactor uses the model the environment names, like a turn: a smaller one is `ANTHROPIC_MODEL` in the
daemon's environment. `tools/fake-llm.py` speaks this protocol too (`POST /v1/messages`), and answers 400 where
the API would: roles that do not alternate, a result that is not at the head of its message, a thinking block
sent back changed.

**A subscription** is used through the `claude` command (Claude Code), which is where its sign-in is: there is
no key, so no request to make, only that program to run (`GhciSession.ClaudeCli`). `GHS_PROVIDER=claude` asks for
it -- it is never taken by default -- with `GHS_CLAUDE_MODEL` (default `claude-opus-5-5`) and, for the compactor,
`GHS_CLAUDE_SUMMARIZE_MODEL` (a small one: every compaction spends the subscription's limits, and there are many).

```
claude --version                 # Claude Code, installed and signed in (`claude` once, to sign in)
GHS_PROVIDER=claude GHS_CLAUDE_MODEL=claude-sonnet-5-5 ghci-session chat
# the compactor too, in ghci-session.json (settings before the command are taken as a shell takes them):
#   "summarize_cmd": "GHS_PROVIDER=claude GHS_CLAUDE_SUMMARIZE_MODEL=claude-haiku-5-5 ghci-session summarize"
```

That program runs the tools itself, so a turn is its and the chat is what it calls: for the length of a turn the
chat serves its tools on a socket, as `ghci-session mcp` serves the session's, and the program is told to start
`ghci-session mcp-relay` on that socket as its tool server -- it has no tool of its own (`--tools ""`), reads none
of the user's settings or tool servers, and ours are the only ones allowed (no permission is waived). So a call is
run by the chat as in any turn: shown, logged, answered with the reload's verdict and the diff; a line typed
meanwhile reaches it between two calls; a turn that ends red is told so once. What does not apply is what
belonged to a conversation the chat kept: a read is not checked against earlier reads, there is no step limit,
and `--context view` and `--restart` in the middle of a turn are for the other two. Its environment is cleared of
`ANTHROPIC_*` and `CLAUDE_CODE_*` first -- a base URL and a token set for another tool would send it to another
provider on another account -- and what it adds to a prompt (memory files, git instructions) is turned off. When
the subscription's limits are reached, or nearly, the chat says so and until when. `--web 1` leaves it its own web
tools. A compaction is the same program with no tools at all, one prompt and its answer, a process each: a few
seconds and a few hundred megabytes, so `summarize_jobs` is better at 4 or 8 here than at 64. On a Haiku it runs
with thinking off (`MAX_THINKING_TOKENS=0`): left on, a line of seventy words was four thousand tokens of thought
and half a minute; off it is under two hundred tokens and three seconds. The larger current models cannot have it
off and are asked for the lightest effort.

**What the model calls cost** is kept: each call of the chat and the compactor appends a line to
`<state>/<session>/usage.jsonl` (when, who asked, model, tokens in and cached, tokens out, seconds). A call of the
chat through the `claude` command also keeps what a subscription meters: `new` (tokens neither read from the cache
nor written to it), `wr` (written to it; `wr5m` of them for five minutes, `wr1h` for an hour, as the provider says),
`windows` (every window of the plan the command's `rate_limit_event` names, by name, as last seen: `u` the share
used, `reset` when it resets) and `credits` (whether the event says the call was on usage credits, not within the plan).
`usage` and the monitor's `5` tab end with the windows of the latest call that has them.
`ghci-session usage --quota` fits, per window, by least squares, how much its use rose between two calls of a session
against the call's uncached input, cache-read, cache-written and output tokens, and says the weights relative to an
uncached input token, with the number of pairs and R². Pairs are two neighbouring calls of one session that saw the
window with the same reset, the use not falling, and no call of another session's ledger ending between them (a call
of another machine or of Claude Code itself shows in no ledger here, and only adds noise, which R² shows). It says
plainly when there are too few pairs, and what write/read ratio a trusted fit implies beside the one in use; it
never switches to it by itself.
`ghci-session usage [SESSION] [--since DAYS] [--json]` sums the ledger by who asked and by day, in money too when
`"prices"` gives the model's rates per million tokens
(`{"deepseek-v4-flash": {"input": .., "input_cached": .., "output": ..}}`).

### A line for a chat started somewhere else

`ghci-session chat --send 'a line' [-s SESSION]` leaves a line for the session's running chat, as if it had been
typed at it: read between two tool calls when a turn is running, the next turn when none is. `top` does the same
with `i`. The chat that takes it is the last one started on the session (`chat.pid`), whatever its standard input
is -- a terminal, a pipe, a screen of its own; the lines wait in `<state>/<session>/chat-inbox`, a file each.

**Sending a task and waiting for it.** `chat --send 'a task' --wait [SECS] [-s SESSION]` leaves the line, waits until
the chat has taken it and the turn that took it has ended (a line taken in the middle of a running turn is that
turn's: it is the one waited for), and prints that turn's last words, whole, and its summary line. Exit status 0
when it ended; 3 when SECS passed first (it says the turn is still running and how many tool calls it has made);
1 when no chat is running, or it died meanwhile. `chat --wait [SECS]` alone is the same for the turn under way, and
at once, with the last turn's last words, when the chat is at rest. What it reads is `<state>/<session>/turn.json`
(where the turn began, `tools` so far, `last` words, `summary`, `done`).

`"on_turn_end": "COMMAND"` in `ghci-session.json` (a target's key, or common to all) runs COMMAND, through the shell
in the project's directory, whenever a turn of the chat ends or is stopped -- for a notification. It is told
`GHS_SESSION`, `GHS_TURN_SECONDS` and `GHS_TURN_TOOL_CALLS` in its environment and the turn's last words on its
standard input; it runs apart from the turn (60 s at most), and its failure is a note in the chat, never the turn's.

### A chat restarted, and a turn gone on with

`ghci-session chat --restart` makes the session's running chat the executable now on disk (build it first), in the
middle of its turn, which goes on:

- with a turn the chat keeps itself (the two API backends), it writes the conversation out before its next model call
  and reads it back;
- with a turn through the `claude` command (a subscription), the turn is that program's -- so the chat hands the
  PROGRAM over: it waits for the tool call under way to be answered, keeps open the pipes to the program and the
  socket its tools are served on, writes down what it had read from them and not yet used, and becomes the new
  executable, which takes them up. It is the same process still, the program is its child still, and the model sees
  nothing happen: a call it makes meanwhile waits in the socket. (Not while subagents are at work: they are threads of
  the chat, and would end with it.)

`ghci-session chat --continue` is for a turn whose chat is gone -- stopped, killed, a machine that went down: it goes
on with the last turn of the history from its LOG. The new turn reads the view, then `<recent>`: the turn's message
and its last messages word for word, as far as `--tail` bytes go (what is between is in the view, as summaries), and
is told to go on from where it stops. The agent has every call it made and what came of it; what it had in mind
between the steps it has not. On any backend, and from one to another.

What the chat shows of a tool's answer (the screen, and `chat.out`) is cut at 600 characters with `...`; `--show BYTES`
changes that (`0`: whole), so a log can be kept whole. The model always gets the answer itself.

A turn through the `claude` command also ROLLS OVER by itself: the conversation is the program's, it only grows, and
every model call reads all of it -- a turn of a few hundred tool calls was at 210,000 tokens a call and rising. So
once a call's context is past `--rollover` tokens the run is ended after the tool call under
way -- its answer in the log, not given to the program -- and a fresh one goes on with the turn from the view as it
is now and the turn's log, as `--continue` does. The turn's first message and what the user said during it are kept
whole, the last six messages of the log too, and the older ones cut to 1500 characters with their number to zoom
(a third of `--tail` for a rollover); its cost is counted across the runs. `chat --replay-rollover [--ratio R] FILE...` runs the controller over the usage lines of recorded chat logs and
says the resets it would have made and what they cost against a fixed 150,000 and 110,000 (PERF.md has its table).
`chat --carry N` says what each of the last
N fresh calls was given, part by part (no model call).

The default, `--rollover auto`, is a controller: a fresh call reads its whole context uncached, at `rollover_ratio`
times (12.5) the price of a read from the cache, and the context then grows a few thousand tokens a call; the cost of
a call is least at `S + sqrt(2 * r * S * (1 + relearn) * g)` (S: the context of a run's first call, g: what a call
adds, both running estimates from the calls' usage and kept in `history/roll.json`; relearn: the share of what a
reset makes the agent read again), within 80,000 and 200,000. With `--usage` a move of more than 5,000 is said. A
number is a fixed threshold, `0` never.

The ratio is not guessed when the calls say what they wrote: the `claude` program reports its cache writes as
`ephemeral_5m` and `ephemeral_1h` tokens. One-hour writes (a subscription within the plan's usage) cost twice the
input price against a read's tenth, so the ratio is 20 and the cache's upper bound starts at 3600 s; five-minute writes
(usage credits, an API key) 1.25 and a tenth, 12.5 and 300 s; none seen, 12.5 and 3300 s. A change of kind starts the
cache's bounds over. `rollover_ratio` in the configuration still fixes the ratio whatever the writes say. `usage
--quota` says, beside the ratio in use, the one its fit implies when the fit is worth trusting (it never switches to it).

A cold cache is the other reason to roll over: the controller keeps bounds on how long the provider keeps its cache
(a call after a pause of G seconds that came back mostly cached: it lasts at least G; the same context after G, mostly
not: at most G; 3300 s to begin with, kept in `roll.json`), and when a tool call has taken the time since the last
model call past that, and the context is over 1.3 times a fresh call's, the run ends after the tool's answer instead
of paying for the whole context uncached (a bench of fifteen minutes can do this).

A fresh call is also given `<files>`, under a kilobyte: the files the turn has read and written, the latest first, with
the lines it read last (`chat --carry N` shows its bytes). And after each reset the chat counts how many of the next ten
calls read again a file read before it; that share (logged as a harness echo, kept in `roll.json`) is the controller's
relearn share, which makes a reset dearer and the threshold higher when the agent has to read much again.

A rotting context is the third: when over 15% of the last twenty `read` calls of a run read lines that overlap lines
the run had read already (and not written since), the agent is not holding what it has in front of it, and the
threshold comes down by a tenth (never under 80,000; said once a run).

With `auto` the run does not end in the middle of an edit: from 0.85 of the threshold it ends after a `git commit` that
went through or a green `test` / `reload`; at 1.15 of it, after whatever tool call comes next. (A fixed number ends the
run at the first call past it.)

`tools/check-cli.py` checks these without a subscription: `tools/fake-claude.py` stands for the `claude` command (the
same arguments and stream, the chat's tools called through its tool server, no model). On the subscription path
each model call is now in the usage ledger as it ends, with what it read from the cache -- a turn of hours was one
line, at its end.

### Subagents

The chat's agent can give work away. `spawn` starts a subagent a task, all at once -- each a turn of its own with the
agent's tools on the same session and the same files -- and answers at once with their names (`Sub-1`, `Sub-2`, ...;
eight at work at most). The agent goes on, and each subagent's last reply reaches it as a message, `[Sub-1] report`
and the text: between two of its tool calls if it is at work, or as a turn of its own if it is waiting for a line.
`tell` sends a subagent a message: one at work reads it between its tool calls; one that has finished goes on from
where it was, and its answer comes as `[Sub-1] reply`.

A subagent reads the view, then its own chat: what it is, its task, every call it made and what it was told, kept in
`<state>/<session>/agents/Sub-N.jsonl`. So the history holds a subagent's report -- as `work`, the kind the view has
for it -- and not its doings, which the chat shows as notes while they happen (`[Sub-1: read {...}]`). Stopping a
turn stops its subagents; with the agent waiting, Esc (Ctrl-C on the streams) stops the ones still at work. They
are threads of the chat: one that is restarted or left takes them with it, and their files stay for `tell`. On every
backend, the subscription's too.

## Watching a session, and working in it: `top`

```
ghci-session top [SESSION]
```

A screen that follows the session as it works. The header is its verdict, memory and servers, idle time, generation
and stale and warning counts, half a second behind. Below, a tab at a time:

| tab | shows |
|---|---|
| `1` history | every request and its answer, a save with its diff, the chat's words. `f` follows the end; `j`/`k`/`PgUp`/`PgDn`/`g`/`G` scroll; a message is cut at six lines and says how many more it has; `n`/`p` move the cursor, `Enter` opens or closes the message under it, `a` all of them; `i` writes a line for the session's running chat (Enter sends it, Esc drops it) |
| `2` view | the view the model reads, with the memory's numbers: lines, built nodes, settled or not, the compactor's jobs |
| `3` log | the daemon's log |
| `4` verdict | the verdict with what is behind it: the compiler's diagnostics, the failing lines, the members and the servers |
| `5` usage | the chat's turn (whether it runs, its tool calls so far or in the last turn, its last words; from `turn.json`), the rollover controller's state (`history/roll.json`: threshold, fresh call, growth, write price, share learned again, the cache's bounds, rot), and what the model calls cost |
| `6` heap | the repl's resident memory graphed, a column each half second (the axis starts near the lowest value so a change of a few per cent shows; the servers' below it), and a report of the heap taken on a key, since each is a major collection with the session paused: `M` figures, `C` CAFs and the heap by constructor, `S` strings, `K` kept values, `D` missed sharing |
| `7` chat | `ghci-session chat --tui` on this session |
| `8` shell | a shell in the project |
| `9` known | what is known by subject, across sessions: the tool's and the user's subjects, the project's, the others' |

`R` sends a reload, `T` the tests, `q` quits. In a pane every key goes to the program; `Ctrl-a` first makes the next
key the monitor's (`Ctrl-a 1`, `Ctrl-a q`; `Ctrl-a a` sends a `Ctrl-a`). A session that is not running shows as such
and is picked up when it starts.

`ghci-session chat --tui` is the chat on a screen of its own: the transcript labelled by kind, a status line saying what
the turn is doing now, the session's verdict in the header and a line to type on (Enter sends, Up recalls,
PgUp/PgDn scroll, Ctrl-C leaves). Esc stops the turn the agent is in -- in the middle of a model call, which is
given up, of a command, which ends with it, or of an evaluation, which the session interrupts -- and the chat goes on: what the turn did is in the history, and the
next line starts from there (on the standard streams, Ctrl-C during a turn does the same; with none, it leaves). The turn loop prints nothing itself: it tells a `Ui` what happened, and the streams
or the screen show it. A screen that ends leaves the terminal as it found it.

Both screens show what the agent writes as markdown -- headings, lists, quotes, rules, fenced code, a table laid out
when it fits, and in a line bold, italic, code, struck text and links -- and a unified diff in a tool's answer in its
colors (so a `write` or an `edit` is its words, then the change in green and red). Nothing is re-flowed, and what does
not parse is shown as it was written.

An image a message names is drawn in `chat --tui`, under its line, where the terminal shows pictures among its
cells: Ghostty and kitty, by the graphics protocol's placeholders (`GHS_IMAGES=0` for never, `1` for a terminal not
known by its name). It is sent to the terminal once, when it first comes onto the screen, at its own size or the
largest of its shape that fits 80 columns and 24 rows, and scrolls with the lines around it; a JPEG, GIF or WebP, or
a PNG over 1200 pixels a side, is shown from a PNG copy made by `sips` or ImageMagick (with neither, the line
alone). In a pane of `top` the line is what is shown: a pane's cells are drawn again by `top`, and a picture is not
among them.

The screens have checks that run them: a screen is run on a pseudo-terminal and typed at from a script, what it
wrote is replayed into libghostty-vt -- Ghostty's own terminal, without a window -- and what that terminal then
shows is checked: its text, where the cursor is, the styles of cells, the images it holds.

| | |
|---|---|
| `tools/check-tui.py` | `chat --tui` and `top`, in a project made for it with a session of its own: the line's keys, a line sent and answered, markdown and a diff's colors, a source's edit and its verdict, scrolling, a resize, leaving; every tab of `top`, the history's cursor and a message opened, the chat and a shell in panes of the pane's size, a test asked for. Twenty seconds; needs cabal |
| `tools/check-images.py` | the pictures of `chat --tui`: each image stored and decoded by the terminal, in the cells it should take, every placeholder saying its row and column, a picture cut at the edge when scrolled, nothing left when the screen is left -- and none sent to a terminal that shows none. Seven seconds; no session |

Both answer as the model with `tools/fake-llm.py` and exit 0 when all holds (`-v`: every screen). Their parts are
of use alone: `tools/tui-capture.py` runs any program on a terminal of a given size, types at it from a script
(`type`, `key`, `until` the screen shows a text, `mark`, `resize`) and records what it writes; `tools/vt-replay.c` says, as JSON, what
Ghostty's terminal holds at an offset of such a recording; `tools/tuicheck.py` is the two put together for a check
to ask a screen things. What they do not see is the drawing itself.

`top` and the chat's screen are built on two packages of this repository, usable without them:

- **`ghostty-vt`** (`ghostty-vt/`) binds **libghostty-vt**, Ghostty's terminal emulation as a C library. `Ghostty.Vt`:
  a `Terminal` of a size; `write` it the bytes a program prints and read its `screen` as a value (every cell's text,
  colors and attributes, the cursor, what changed since the last read), its title and working directory, the
  scrollback viewport; `onWrite` for what the terminal answers to the program; a `KeyEncoder` that writes a key as the
  program in that terminal expects it, by the modes it set (application cursor keys, the kitty keyboard protocol,
  modifyOtherKeys). `Ghostty.Vt.Pty`: a program on a pseudo-terminal. The library is found at run time
  (`Ghostty.Vt.load`), so a program builds and runs without it and can say what is missing. The headers it is built
  against are in `ghostty-vt/include` (MIT, at the commit in `COMMIT`). Boot packages only.
- **`ghostty-tui`** (`tui/`) is a small terminal UI library on it. `Tui.Buffer`: a frame of styled cells and the bytes
  that turn the last frame into the next, only the cells that changed, so nothing flickers; wide characters take two
  columns. `Tui.Terminal`: raw mode, the size, keys decoded with their modifiers. `Tui.App`: the loop (draw, wait for a
  key, a resize, a tick or a wake, handle, draw again). `Tui.Pane`: a program on a pseudo-terminal whose screen
  libghostty-vt keeps, as a part of the frame, so a full-screen program in a pane and the UI around it do not fight.
  A key typed into a pane is decoded and encoded again for the modes the program set, not forwarded as the outer
  terminal sent it.

Without libghostty-vt the other tabs work and the pane tabs say what is missing. `tools/libghostty-vt.sh` builds it
into `.bin/` (a ghostty checkout at the headers' commit and zig 0.16, downloaded if absent); or put a
`libghostty-vt.so`/`.dylib` beside the executable, or name one in `GHS_LIBGHOSTTY`.

To link it in instead, so the executable needs no library beside it: `tools/libghostty-vt.sh` also leaves
`.bin/libghostty-vt-static.a`, and `GHS_STATIC_VT=1 ./build.sh` builds with it (the `ghostty-vt` package's `static`
flag; about 1.3 MB more, and libc++ is linked).

## Install

```
cabal install exe:ghci-session        # or run from a checkout: bin/ghci-session builds into .bin/ when stale
./build.sh                            # builds both executables into .bin/
```

The engine must sit beside `ghci-session` and must have been built with the compiler that is first on `PATH`
(it says so if not). Search can use `libfff` when it can be loaded; that is off by default and built with
`GHS_FFF=1 ./build.sh` or `cabal build -f+fff`. `top`'s terminal panes need libghostty-vt, built by `tools/libghostty-vt.sh` (see `top`). A project adds `ghci-hygiene` to its `build-depends` only to call the
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
app/GhciSession/        the command and the daemon, as a library: Json, Config, Sys (FFI), Repl (starting the engine, its protocol), Watch, Daemon, Gc, Cli, History, ...
exe/Main.hs             the executable's Haskell side (its `main` is C: cbits/ghs_main.c)
engine/, vendor/        the engine (GhsEngine.hs, Main.hs, per-compiler GhsCompat) and GHCi's sources per compiler
cbits/                  sockets, file events, regex, hashing, processes; the executable's entry point
ghostty-vt/, tui/       libghostty-vt bindings and a terminal UI library on them (`top`, `chat --tui`)
tools/libghostty-vt.sh  builds libghostty-vt into .bin/
hygiene/                c/*.c (the pruner and the census, compiled into the engine), src/ (the library), repro/
bin/ghci-session        run from a checkout (builds if stale); bin/ghs is a symlink to it, for short
bin/ghci-history        save a session's history to a git branch and load it back
ghci-session.json       the sessions this package runs on itself
examples/hello/         two packages, a CAF that leaks without pruning, a server with state to hand over, Template Haskell splices
examples/tour.py        every feature on a copy of hello, each step checked and timed
tests/test_e2e.py       the lifecycle end to end (GHS_E2E=1)
```

`ghci-session selftest` runs the built-in self-tests from the binary.
