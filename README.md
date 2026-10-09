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
top [SESSION]                   # a screen that follows a session: verdict, history, heap, a chat and a shell
chat [-s SESSION] [--tui] [--once TEXT]   # an agent working on the session, with the history as its memory
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
| `watch_typecheck`, `watch_refork` | typecheck a save before reloading it (on); re-fork servers on a save (on) |
| `reload_on_commit` | a new git HEAD is a full reload, with checks and re-fork |
| `server` | `action`, optional `port`, `prefork`, `env`, `serve_on_load`, `verify_timeout` |
| `hygiene` | unlink superseded CAFs after each reload and report memory |
| `repl_budget_mb` | past this, a reload is a restart (default 6144; `0` disables) |
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
to (and their diffs), commits, restarts. It writes each as a line of `.ghci-session/<session>/history/main/YYYY-MM-DD.jsonl`
and never edits one: `tool` (the request, as one line), `echo` (the answer: an evaluation's output whole, a verdict with its failing lines, the `STALE` warning the client
was given),
and, from a harness, `user`, `talk`, `work` and `note`. A line is written with one write and an fsync, and a torn line
is skipped at load. `status` and `info` are not logged: a tool polls them. A text over 30,000 characters is
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
on the next tool result; `typecheck` answers at once when nothing changed; a read of lines the context already holds
answers with a pointer instead of the text; a session that is down is waited for (`GHS_CHAT_DOWN_WAIT`).

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
- **A request declined** (`stop_reason: refusal`) is said as such, and on Anthropic's API another model is asked
  in the same call (`fallbacks: default`).

Replies are not streamed (the transport is one request, one reply), so a call's `max_tokens` is what one reply may
be. The compactor uses the model the environment names, like a turn: a smaller one is `ANTHROPIC_MODEL` in the
daemon's environment. `tools/fake-llm.py` speaks this protocol too (`POST /v1/messages`), and answers 400 where
the API would: roles that do not alternate, a result that is not at the head of its message, a thinking block
sent back changed.

**What the model calls cost** is kept: each call of the chat and the compactor appends a line to
`<state>/<session>/usage.jsonl` (when, who asked, model, tokens in and cached, tokens out, seconds).
`ghci-session usage [SESSION] [--since DAYS] [--json]` sums the ledger by who asked and by day, in money too when
`"prices"` gives the model's rates per million tokens
(`{"deepseek-v4-flash": {"input": .., "input_cached": .., "output": ..}}`).

## Watching a session, and working in it: `top`

```
ghci-session top [SESSION]
```

A screen that follows the session as it works. The header is its verdict, memory and servers, idle time, generation
and stale and warning counts, half a second behind. Below, a tab at a time:

| tab | shows |
|---|---|
| `1` history | every request and its answer, a save with its diff, the chat's words. `f` follows the end; `j`/`k`/`PgUp`/`PgDn`/`g`/`G` scroll; a message is cut at six lines and says how many more it has; `n`/`p` move the cursor, `Enter` opens or closes the message under it, `a` all of them |
| `2` view | the view the model reads, with the memory's numbers: lines, built nodes, settled or not, the compactor's jobs |
| `3` log | the daemon's log |
| `4` verdict | the verdict with what is behind it: the compiler's diagnostics, the failing lines, the members and the servers |
| `5` usage | what the model calls cost |
| `6` heap | the repl's resident memory graphed, a column each half second (the axis starts near the lowest value so a change of a few per cent shows; the servers' below it), and a report of the heap taken on a key, since each is a major collection with the session paused: `M` figures, `C` CAFs and the heap by constructor, `S` strings, `K` kept values, `D` missed sharing |
| `7` chat | `ghci-session chat --tui` on this session |
| `8` shell | a shell in the project |

`R` sends a reload, `T` the tests, `q` quits. In a pane every key goes to the program; `Ctrl-a` first makes the next
key the monitor's (`Ctrl-a 1`, `Ctrl-a q`; `Ctrl-a a` sends a `Ctrl-a`). A session that is not running shows as such
and is picked up when it starts.

`ghci-session chat --tui` is the chat on a screen of its own: the transcript labelled by kind, a status line saying what
the turn is doing now, the session's verdict in the header and a line to type on (Enter sends, Up recalls,
PgUp/PgDn scroll, Ctrl-C leaves). The turn loop prints nothing itself: it tells a `Ui` what happened, and the streams
or the screen show it. A screen that ends leaves the terminal as it found it.

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
