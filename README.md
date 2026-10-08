# ghci-session

A warm GHCi per project, behind a small daemon, with the two things a long GHCi session needs and plain
`cabal repl` does not give you: **a verdict you can trust** and **memory that does not grow with every edit**.

It grew out of the session tooling of a large Haskell modelling project (several hundred modules, servers forked
from the repl, days-long sessions) and has nothing of that project in it. **It needs GHC 9.14.1, 9.10.3 or 9.6.7;
it is developed on macOS (arm64) with 9.14.1, and runs on Linux with each**: see *Status* (before 9.14 there are a
few known differences).

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
- **the library** is its own small package, `ghci-hygiene` (`hygiene/ghci-hygiene.cabal`: `GHC.Hygiene`, `.Store`,
  `.Kept`, `.Census`, `.Zygote`), for your project's own code: state and a memo that outlive a reload, a heap census,
  handing a server's state to its replacement. It depends on nothing of the compiler's. A session needs none of it.

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
itself -- this package has a `ghci-session.json`, and a save here is a compile and 82 self-tests in a few seconds (target `tool`; `engine` is the engine's own session, a compile verdict in 0.3 s)
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
(`vendor/ghc-9.14.1`, 7,600 lines, `vendor/ghc-9.10.3` and `vendor/ghc-9.6.7`; `vendor/fetch.sh VERSION` gets another, plus a stanza in the
cabal file, and the engine's few modules that reach into the compiler's session are per version, `engine/ghc-X.Y`) and built
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

## Finding a definition: `doc`

```
ghci-session doc quickDivergnces        # a typo is fine
ghci-session doc qe                     # initials: quickEdit
ghci-session doc Daemon.boot            # qualified
ghci-session doc raster depth           # words: of the name, the type or the comment
ghci-session doc codeFingerprint -n 1   # one answer, its whole comment
```

Each answer is a declaration of the session: its signature as written, the comment above it, and `file:line`.
`--json` gives the same as data (`module`, `name`, `kind`, `signature`, `doc`, `file`, `line`, `score`).

The index is the session's own watched Haskell sources and nothing else -- no dependencies. It is read by a
scanner of top-level declarations (`app/GhciSession/Doc.hs`), not asked of the compiler, so it has what a module
does not export, record fields, the documentation without compiling anything with `-haddock`, and a module that
does not compile right now. It lives in the daemon: built on the first query, and after that only a file that
changed is read again. On a session of 102 source files that is 2,485 declarations, 29 ms to build and 1 ms a
query; the daemon answers without the repl, so it works while a reload is running.

A name is matched exactly, then as a prefix, then by the initials of a camelCase or snake_case name, then as a
substring, a subsequence, and a near miss; any other word must appear in the module name, the signature or the
comment. Several words are first looked for TOGETHER, in one declaration (`raster depth`); if no declaration has
them all, each word is answered on its own, best answers in turn -- two names typed together are two questions --
and a word that finds nothing is named. What the scanner does not see: instances, class methods, constructors
without fields, local definitions, and anything behind CPP it cannot follow.

## The history: what was done to the session, as the memory of whoever works on it

```
ghci-session history                 # the log: every request and its answer, every save and its verdict
ghci-session history --kind user 'keep the painted views across reloads'   # a harness logs the user's words (and its agent's replies: talk)
ghci-session view --wait 10          # the whole history as one-line summaries, oldest first: what a model reads at the start of a turn
ghci-session zoom 2184 8             # open line 2184+8 of the view into the two lines of 4 it was made from; zoom 2187 1 is message 2187 whole
ghci-session date 2187               # when it was written
```

The daemon is the one process that sees everything done to a session -- a client's request (`eval`, `reload`,
`test`, `doc`, `census`, ...) and what it answered, a save with the verdict it compiled to, a commit, a restart, a
stop -- so it writes each as a line of `.ghci-session/<session>/history/main/YYYY-MM-DD.jsonl`: `tool` (the
request, as one line: `eval Hello.greeting`, `save: src/Hello.hs`), `echo` (the answer: an evaluation's output cut
to 30,000 characters with its head and tail kept, a verdict with its failing lines, the STALE warning the client
was given), and, from a harness, `user`, `talk` and `note`. A line is written with one write and an fsync, and
never edited; a torn line is skipped at load. `status` and `info` are not logged: a tool polls them.

Over the log the daemon keeps a binary tree of one-line summaries, the design of OptChat (Victor Taelin): node
`(l, i)` covers messages `[i*2^l, (i+1)*2^l)`, a line is asked for in 512 bytes (and taken up to 1,024), a parent is made from its two
children, and a message or a pair that already fits IS its node, with no model call -- so a routine verdict
(`OK -- CHECK-PASS (0.4s)`) costs nothing, and a session that saves two hundred times a day costs the merges
above it. The view is the list of nodes tiling the whole log, oldest first, kept under 128,000 bytes: a new
message is appended, and while the view is over budget the most due adjacent pair (the oldest relative to its
size, whose parent is built) is replaced by its parent. A merged part is never split again, so the start of the
view is the same from one turn to the next (what lets a model cache it) and the distant past fades in resolution
instead of being dropped. A line not summarized yet renders as `(not summarized yet: zoom it)`; `view --wait N`
waits for the compactor first, as a turn should. Rendered: `<chat>`, one line a part, `id+n|text`
(`GhciSession.History`; the fold and the schedule are self-tested). A save's `tool` line carries the diff of
each changed file against the copy taken when it was last seen (`history/loaded/`, seeded at boot), so the
log says what was edited, not only that a file was.

The summaries are written by a cheap model through `"summarize_cmd"`: a shell command the daemon runs with the
instructions (OptChat's compactor prompt, `compactPrompt` there, with `"agent"` as the agent's name), the view's
lines before the node (bare -- no ids, which a model copies into its answer), and the step -- the message
whole, or the two lines to merge -- on its standard input, reading one line from its standard output:
`"summarize_cmd": "ghci-session summarize"` runs the tool's own compactor (`GhciSession.Chat.summarizeMain`; the
daemon runs its own executable, wherever that is), which sends the instructions as the system prompt and the rest
as the user message to an OpenAI-compatible chat endpoint -- DeepSeek's flash model by default
(`DEEPSEEK_API_KEY`; `DEEPSEEK_MODEL`, `DEEPSEEK_BASE_URL`, or `OPENAI_*`, override) -- over HTTPS through
libcurl, found at run time (`cbits/ghs_http.c` `dlopen`s `libcurl.so.4` / `libcurl.4.dylib`: no headers, no link
name, and the package still depends on nothing outside GHC's boot packages), asked without thinking: a reasoning model spends the answer's budget on its thoughts first, and a
summary's budget was often all thoughts and no line (`SUMMARIZE_EFFORT=low|high|max` turns thinking on; an answer
cut off before any text is asked again with a larger budget). Nodes are built one message at a time, in order,
with merges of finished parts alongside, `summarize_jobs` (8) at once, and no call sees a line that is not a
summary. The context a call sees is the last 32-64 KB of those lines, cut at the front with hysteresis (dropped
to 32 KB once over 64 KB, then left alone until it is over again), so the prefix the provider caches stays the
same for a stretch of calls: the whole view went with every call before, and the compactor's calls were 70% of
a session's tokens. A line is taken up to 1,024 bytes, twice what is asked
for: a model asked for 512 writes 600-900, and asked again (up to five times, as it was) it made three calls a
line, 41% of the lines kept still over 512. A line over 1,024 is asked again with the line cut where the limit
falls, up to three tries, and the shortest is kept, cut at its last word within the limit. (Replayed over one
round's 291 messages: 415 calls instead of 917, 111k tokens out instead of 239k, 13 minutes of compactor time
instead of 30; the lines are 13% longer on average, so a view of the same budget holds that many fewer.) a failed node is tried again after ten seconds, for ever, and only its first failure is
logged. Without a command the log and the free nodes are kept and the tree waits for a compactor outside the
daemon: the `pending` operation answers the nodes ready to build, each with its prompt, and `tree_put` takes a
line. The tree is stored (`history/tree/`) and never recomputed. `"history": false` turns all of it off.

What it is for: an agent that works on the session as a sandbox -- an evaluation against the loaded code in
milliseconds, a verdict it can trust, `doc` for a definition, a save the watcher reloads -- starts each turn from
the view instead of from nothing, and its memory is what was actually done to this code, by it or by hand:
which evaluations answered what, which edits failed and why, what a census said. Two things an agent's loop
needs are in the protocol: every reply carries `stale` (the echo says when an answer came from code that is no
longer on disk), and a command that runs past its timeout is interrupted, not abandoned -- the engine is sent a
SIGINT, GHCi turns it into `UserInterrupt` and is back at its prompt -- so a probe that hangs costs its timeout
and nothing after it (it used to leave the next request queued behind it). A check that HANGS costs less than
its timeout: the daemon keeps the seconds of each member's last five passing checks, and a run past five times
their median (at least 15 s, never past the check's own `timeout`) is interrupted with the verdict `CHECK-HANG`
and the last line it printed -- which test it hung in. (An emulator's frame loop that never ended cost an
agent ten minutes a save, four times in an hour, while the check it guards takes three seconds.) The chat's
`eval` and `bench` default to two minutes for the same reason, and say how to ask for more. Two ways in for an agent:

- **`ghci-session mcp`** serves the session's operations and its memory to any agent client as tools, over
  the Model Context Protocol on standard input and output (`claude mcp add ghci -- ghci-session mcp` from
  the project's directory): `eval`, `status`, `typecheck`, `reload`, `test`, `doc`, `census`, `bench`,
  `mem`, and the memory's `view`, `zoom`, `date`, `history` and `remember` (a finding kept for later turns,
  logged as the agent's words). Each call is a request to the daemon, so it is in the history like any other.
  The agent's edits are not: the session sees them as saves, with their diffs.
- **`ghci-session chat`** is the endless chat itself (`GhciSession.Chat`), OptChat's turn loop over this
  history: a fresh model call per message, whose input is the system prompt, the view (rendered before the
  message is logged) and the message; the tools above (the MCP server's own definitions) plus the agent's hands
  on the files (`read`, `write`, `edit`, `ls`, `sh`), which the harness logs; replies logged as `talk`, thoughts
  shown and never logged; a line typed while the agent works delivered between tool calls. It waits for the view
  to settle before each turn. The model is an OpenAI-compatible endpoint (`GhciSession.Llm`, over the same
  libcurl), DeepSeek's flash model by default, which caches the prompt's prefix on its own (`ghci-session chat
  -s dev`, `--once 'what was tried on X?'`, `--instructions AGENTS.md`, `--usage` for each call's tokens and
  seconds). What letting it build an emulator taught the harness: a save answers with the verdict of the reload
  it caused, and behind a bad verdict the compiler's diagnostics or the failing tests, so no `status` call
  follows a write (it used to see STALE and reload by hand); an eval of several lines runs its leading imports
  as their own commands (in one GHCi block they do not parse); an argument the model calls by another name
  (`command` for `cmd`) is taken by its name, and a missing one is said; a reply cut off at the output limit
  (the thinking ran on) is asked to go on in smaller steps, not taken as the end of the turn; a shell command
  that edits a watched source answers with the reload's verdict too; an edit whose text occurs nowhere as
  written but exactly once with its spacing squeezed is applied there, and says so (else it points at the
  nearest line); a turn that changed the files and is about to end with the verdict red is told so once, so it
  fixes it or says plainly that it stops red; `remember` keeps a finding for later turns as the agent's own
  words. Faster, because tools took 3.5 times the model's time in an agent's rounds (249 minutes to 70), most of
  it saves waiting out a check of 20-100 s once per edit: a save answers once the code COMPILES (a compile error
  at once, with its diagnostics; a check still running after six more seconds is not waited for), and the
  check's verdict rides on whichever tool result comes after it is in -- the daemon's every reply carries a
  `checking` record while a check runs, with when its reload began, and a save's verdict is one whose reload
  began after the file was written, not the end of a check already running; `edits` applies several
  replacements, across files, checked together before any is written, with one reload (a replacement
  without a path is in the file of the one before it); `test` takes an
  expression -- one group of the tests, run alone, scored by the check's own fail and pass patterns, the
  session's verdict left as it is; and `typecheck` answers at once, without waiting behind a running check,
  when no source changed since it was last asked (four of an agent's waited 226 s each). With tools out of the
  way the model is the time, and a turn's conversation grows with every tool result, most of it files read
  (17 reads in 42 calls of one round, 128k tokens carried into each call after): so every read's answer is
  numbered; a read of lines the context already holds, unchanged, answers with a pointer to that read instead
  of the text again; and a read supersedes the earlier reads of the lines it covers, which are rewritten as
  one-line stubs naming it once 40,000 characters of them have piled up -- in a batch, because rewriting a
  message ends the provider's cache of the prompt from there on (`GHS_CHAT_TRIM_AT` sets the threshold). A shell
  command's output is kept to 8,000 characters, head and tail, the cut said with how to narrow it. A session
  tool that finds the session down (stopped, or restarting) waits up to two minutes for it to come back
  (`GHS_CHAT_DOWN_WAIT`) and sends the call again, rather than answer at once -- an agent told "no session
  running" loaded the project in a GHCi of its own through `sh`, cold, 14 times for 374 s; and a `sh` that does
  start a GHCi of its own is told the session has the project loaded. The pure parts of all this are in the self-tests (`ghci-session selftest`). It began as
  `tools/chat.py` (removed), Python because a model client is not a boot package: `dlopen` made it one binary.
- **`--context view` builds every model call from the log**, not only each turn's first. OptChat starts a
  turn fresh from the view and carries the turn's own steps as a conversation: a round of a hundred tool calls
  grew to 195k tokens and never met its own memory. With `--context view` a call is: the system prompt, the
  view up to a boundary, the turn's message, a line saying where the turn's own work starts in the log, and
  `<recent>` -- the log after the boundary, word for word (`id|kind: text`). Once `<recent>` is over `--tail`
  bytes (96,000) the boundary moves on to leave a third of it and the view is taken again up to there: in a
  batch, so between two moves each call is the one before and a bit more, and the provider's cache holds.
  `--tail 0` is the view alone, every call waiting for the compactor to summarize the step before. For the
  log to be the prompt, the chat logs every tool call as the agent made it and every answer as the agent saw
  it (with the notes the harness adds), and asks the session's own operations `quiet`ly so they are not
  logged twice; the harness's own notices are logged too. (Without the line saying where its work starts, a
  fresh call took the message for new: with a tail of 0 an agent ran the same evaluation 16 times, its answer
  already in the view's last lines.)
- **The harness upgrades in the middle of a turn.** `ghci-session chat --restart [-s SESSION]` (or `kill -HUP`
  the chat; its pid is `<state>/<session>/chat.pid`) has the running chat run itself again as the executable on
  disk now -- build first, and `bin/ghci-session` builds on its own. In a turn it waits for the step's tools to
  finish, writes the turn out (the conversation word for word, the step, the reads, what it has spent, the lines
  typed and not yet taken) and `exec`s itself with `--resume FILE`: the same process, output and standard input,
  and the turn goes on at the same step with the provider's cache of the prompt intact (the first call after
  one restart: 3,456 of 3,621 tokens cached). Between turns it restarts at once. A restart that cannot `exec`
  goes on as it was.
- **What the model calls cost** is kept: every call of the chat and of the compactor appends a line to
  `<state>/<session>/usage.jsonl` (when, who asked, the model, tokens in and of them cached, tokens out,
  seconds), each turn ends with its summary on standard error (`[turn: 12 model calls, 1.2M tokens in (99%
  cached), 8.4k out, 15 tool calls, 94s]`), and `ghci-session usage [SESSION] [--since DAYS] [--json]` sums
  the ledger by who asked and by day -- in money too when `"prices"` in `ghci-session.json` gives the model's
  rates per million tokens (`{"deepseek-v4-flash": {"input": .., "input_cached": .., "output": ..}}`; the
  cached and the uncached part of a prompt are priced apart, and with a stable prefix the cached part is most
  of it).

## What the heap holds, what an action costs, and the session's own scenario

```
ghci-session mem --heap               # the live heap, and what the CAFs and the kept values retain
ghci-session census [--top N]         # every CAF by what it retains, and the heap by constructor
ghci-session census --strings         # the Strings among it (24 bytes a character), by their first characters
ghci-session census 'My.Module.table' # ONE value, alone: bytes, closures, constructors
ghci-session census --kept            # what a reload cannot drop: each slot of GHC.Hygiene.Store, each value given to Census.keep
ghci-session store [--drop NAME]      # the named slots that outlive a reload: list them, or forget one
ghci-session bench 'My.Module.rebuild'  # an IO action: wall, GC, allocation, live heap before and after
ghci-session profile                  # the target's "profile" steps, timed
```

`census`, `bench` and `mem --heap` are answered by the engine itself (it runs "GHC.Hygiene.Census"), so they work
in any session: the project does not have to depend on this package's library.

`profile` runs the steps a target lists, in order, against the running session:

```json
"profile": [
  { "name": "eval: the greeting", "eval": "Hello.greeting", "budget": 1, "expect": "hello" },
  { "name": "edit a source",      "sh": "echo '-- probe' >> src/Hello.hs" },
  { "name": "reload after it",    "cmd": "reload hello --no-test", "budget": 10 },
  { "name": "revert",             "sh": "sed -i.bak '/^-- probe$/d' src/Hello.hs && rm -f src/Hello.hs.bak" }
]
```

A step is an expression in the session (`eval`), one of this tool's own commands (`cmd`) or a shell command
(`sh`: an edit, its revert), with an optional `budget` in seconds and an `expect` its output must match. Each run
is saved under `<state>/<session>/profile/` and compared with the one before: a step more than 1.5 times and 1 s
slower is marked a REGRESSION, and the command fails when a step fails, misses what it expected or goes over its
budget. `--only NAME` runs some of them, `--no-save` does not record the run. `examples/hello` has one.

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

**Objects another session compiled.** A session's objects are in a directory of its own (two sessions running must
not write one file), so one started after work in another compiled that work again. Before the engine starts, a
module whose interface is missing or older than its source takes the interface and the object of a sibling session
-- the same unit's directory under another session's name -- when that one is newer (a pair written in the last two
seconds is left alone). Nothing is trusted: the compiler checks an interface against the source, the flags and its
imports before it uses the object, and compiles what does not fit. `objects: N module(s) taken from ...` in
`daemon.log`.

**What is not asked twice.** A typecheck keeps its own module graph and the daemon says which files changed since
the last one (no scan of every module: 0.13 -> 0.07 s on a hundred modules), answers from its last answer when none
did, and runs once in the background after a reload or a start so its interfaces never lag the sources. A reload
with every watched source as it was loaded is not sent to GHCi at all -- its last answer stands; the checks and the
servers follow as ever. A member added live uses the build tool's recorded answer for that member set when nothing
the tool reads has changed.

**A member added to a session that is running** (`compose SESSION --add M`): GHCi has no command to add a package --
its home units are the `-unit` arguments it starts with -- but a session IS a graph of home units, and the engine
inserts one (`engine/GhsAddUnits.hs`: the unit's flags parsed over the session's flags from before its units, its
package state, the two interactive units made again, its sources as targets). The daemon asks the build tool what the
new set is started with (the question a start asks: 2-8 s), checks that the units already loaded would be started
exactly as they were, hands the engine the new ones, and becomes the daemon of the new set: launch record, the new
members' environment, imports, watched sources, checks and servers. What was linked stays linked. It restarts
instead, saying why, when a member is removed, when the session has ONE unit (the build tool then writes no unit
file), when the new set wants other RTS flags, prebuild or environment values, when the loaded units' flags would
change, or when a new unit is a package the loaded units already use BUILT (it cannot be both: the compiler's module
graph panics -- everything that uses it has to be set up again). On the example: a third package into a two-package session in 2.2 s, 2 of it the build tool. (The tour's
`compose` group.)

**A library then holds only some modules, and a name must be found where it is current.** `dlsym` on a library's
handle goes on through the libraries it was linked against. While every reload linked everything that never mattered;
with modules kept, the newest library depends on the older ones it uses and NOT on a newer one it does not use, so a
module it lacks was found two generations back, past its current copy. Two things asked that way and both were wrong:
GHCi's own lookup of a name (after a reload it ran the copy from before an edit) and the pruner (it unlinked the
current copy's CAFs as superseded: their values were freed under running code, and the session died in a later
collection or in the forked server). So on macOS the library the daemon inserts (`libghsmem.dylib`) opens GHCi's
temporary libraries with `RTLD_FIRST` -- a handle answers for its own image -- and the pruner counts an answer only
when it lies in the library asked. The engine keeps modules linked ONLY when that library is in place (elsewhere, or
with it missing, every reload links everything, as GHCi does; `GHS_KEEP_LINKED=1` insists). The tour's `partial`
group is the case: three modules, two libraries that do not depend on each other.

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

**Three questions, three commands.** `typecheck`: do the sources on disk typecheck? `reload`: compile and load
them. `test`: run the target's tests on what is loaded. (`test` was called `check`, which said neither; the old
command, `--no-check` and the `check`/`checks`/`watch_check` keys still work, and the verdict words in the status
files -- `CHECK-PASS`, `CHECK-FAIL`, `CHECK SKIPPED` -- are unchanged, because programs read them.)

`typecheck` generates no code and does not touch what is loaded: the engine runs a `load` of the same targets in a
copy of the session whose units have no backend and keep their interfaces in a directory of their own, then puts
the session back (`typecheck` in `engine/GhsEngine.hs`). It costs what changed since the last reload: on a
session of 87 modules, an error in a leaf module is reported in 0.1 s, and an edit to a module in the middle of
the graph is answered in 0.35 s where the reload that follows takes 6.8 s to compile it and what imports it. The
first call after a start typechecks everything (5.8 s there, against 31 s to compile it). It answers
`OK -- TYPECHECK` or `TYPE-ERROR: n error(s)` and the errors, exits non-zero on an error, and leaves the session's
verdict alone -- it is about the sources, not the loaded code.

**A save is typechecked before it is reloaded** (`watch_typecheck`, on by default). The watcher asks the same
question first, and when the answer is a type error it stops there: the verdict is the error, a fraction of a
second after the save (0.1-0.3 s here), and the reload is not done. A reload would only fail the same way later
-- and a failed load takes the modules it could not compile, and the prompt's imports, out of the session. So
with an error on disk the session goes on answering from the last code that compiled; `status` reads
`STALE(1) COMPILE-ERROR: 1 error(s)  [typecheck: NOT reloaded ...]` and `status.json` has `typecheck_only`. When the
types are right the reload follows as before; the typecheck it repeats is the small part of it. An explicit
`reload` always reloads.

**A new module is not a restart.** A new file that a loaded module imports is simply found by the reload. Listing
it in the `.cabal` used to cost the session -- any change to a build file restarted the repl. Now the build tool
is asked again (its few seconds are unavoidable: only it can read the file) and its answer compared with the one
the engine is running on: if the two differ only by module names added, the session stays up, with everything
linked and every value computed still there, and a module nothing imports yet is added as a target of the unit
that lists it -- by the engine, not GHCi's `:add`: a GHCi 9.14 started from a unit file is a multi-unit session
whether or not `-unit` was said (cabal passes one component as a bare `@file`), and there `:add` gives the file to
the interactive unit, which compiles it too, into the same object file as the real unit -- the two builds
overwrite each other and the next link fails with an undefined symbol (`hello-inplace_Stack_selfTest_closure`),
until a restart. Anything else -- a dependency, a flag, a module removed -- is a restart, on the answer just had,
so cabal is not run twice. A changed `.c` or `.h` still restarts.

**GHCi asks `gcc` where each system library is, ten times as it starts**, and on macOS `gcc` is a shim that asks
`xcrun` which compiler to run: 30 ms a time, 0.2 s of a 0.45 s start, and again for every library linked. The daemon
passes the compiler itself (`-pgml`, `-pgmc`, with the SDK the shim would have named in `SDKROOT`), when the `gcc`
on PATH is that shim.

**An optimisation pragma in a module does nothing in GHCi by default.** `{-# OPTIONS_GHC -O1 #-}` on the one
module everything else calls is the obvious way to keep a session's compiles fast and its hot code fast. In GHCi it
is accepted and has almost no effect: the session starts at `-O0`, which also means "ignore the pragmas in
interface files", so the libraries' unfoldings are never read and the "optimised" module still calls `+` and `*`
through class dictionaries (a numeric loop: 97 MB allocated with the pragma, 128 MB without, 0.2 MB when it really
is optimised). Saying `-fno-ignore-interface-pragmas` in the pragma is too late; it has to be on GHCi's command
line, and the daemon now puts it there. On a session of 87 modules whose projection engine carries the pragma:
measured with and without the flag from empty object directories, the cold compile 33 -> 42 s, the first check
33 -> 25 s, the full report with every view recomputed 19.5 -> 11.9 s, the repl 857 -> 913 MB. An object compiled before the flag was there is not recompiled because of
it (GHCi ignores optimisation changes): touch the module, or delete its object.

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
| `reload`, nothing changed / `--no-test` | 0.6 / 0.1 |
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
- **Several files, one reload: `hold` / `release`.** The watcher reloads once the events stop for 50 ms (at most
  0.3 s into a burst), so files written further apart than that are a reload each: ten files 400 ms apart were ten
  reloads, 1.8 s in all, and ten at 100 ms were five. `ghci-session hold` makes the watcher leave the sources
  alone; write the files; `ghci-session release` reloads once and answers with that verdict (the same ten files:
  one reload of 1.0 s). A hold ends by itself after `--timeout` seconds (30, at most 600), so a writer that died
  does not leave the session deaf to saves; the watcher then reloads what was saved. The MCP server has both as
  tools (`hold`, `release`). **The chat does it by itself:** writes (`write`, `edit`, `edits`) that come one after
  the other in one model reply are a batch -- the session is held, the writes are made at once (those on the same
  file stay in order), one release reloads, and the verdict comes with the last result. Ten `write`s in a reply:
  ten sequential saves were 10 reloads and a 3 s turn, the batch is one reload and 1 s. (Calls in between, an
  `eval` or a `read`, end a batch: they may depend on what was written.)
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
with the compiler on PATH (it says so if not). A project adds `ghci-hygiene` (the package in `hygiene/`:
list that directory in its `cabal.project`) to its `build-depends` only to call the library from its own code.

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
| `warm` | `[]` | expressions evaluated in the background after a reload that ran no test (`--no-test`, `watch_test` off), e.g. `"My.thing `seq` ()"`: GHCi links the reloaded code, and the unlink and its GC run, while you read the verdict rather than on your next command |
| `ghc_jobs` | `-1` | `-jN` for GHCi's compiles: `-1` is bare `-j` (one per processor), `0` is off. It helps a reload that recompiles many modules (53 of 53 on a 4-core machine: 11.3 s -> 6.9 s) and nothing else: one file is 0.3 s either way, and on a 98-module session an interface change recompiled two (GHC's recompilation avoidance) and `-j8` changed nothing |
| `test` / `tests` | none | (`check` / `checks` is the name they had, still read) `expr` to run after a good load; lines matching `fail` (default `^\[FAIL\]`) fail it, `pass` must appear; `log`: a file the check writes its real output to; `name` labels a second check |
| `server` | none | see *Servers* |
| `env` | `{}` | environment of the repl, and of the target's server |
| `repl_budget_mb` | `6144` | past this, a reload is a restart; `0` disables. Env `GHS_REPL_BUDGET_MB` overrides |
| `rts_flags` | `-c -Fd0.5` | GHCi's own RTS flags (the daemon starts it: `+RTS ... -RTS`); `none` for none. `-c` is the compacting old generation: on a 98-module session with 250 MB live, the repl's footprint was 1,251 MB copying, 1,094 MB with `-c`, 920 MB with `-c -F1.5` (and its forked server 569 / 412 / 385 MB), for 9.5 / 17.1 / 25.2 s of GC over a 100 s scenario. The non-moving collector is refused with `hygiene` (the pruner edits lists it reads concurrently: the repl died) |
| `capabilities` | `0` | `setNumCapabilities` in the repl (GHCi evaluates on one; more buys the parallel GC) |
| `hygiene` | `false` | unlink superseded CAFs after each reload, report memory |
| `unlink_after` | `eval` | when a reload's unlink happens: after the first evaluation (the check, or an `eval`), when the code that replaced it is linked; `reload` is at once, which reaches one generation less |
| `heap_auto` | `false` | keep the RTS's `-H` (allocation area up to the largest heap it has needed): more memory, fewer collections |
| `mem_return` | `true` | macOS: memory the RTS frees leaves the footprint (`libghsmem.dylib` is inserted either way: it is also what makes a kept module's name resolve to its current copy) |
| `prune_gc` | `copying` | the collection after a reload's unlink: `copying` (fast, parallel, more footprint) or `compact` (the RTS's own under `-c`) |
| `prune_gc_idle_s` | `0` | `0`: the GC that frees what was unlinked runs at once. A positive value defers it to an idle moment and HAS CRASHED the repl (see the tour section); leave it |
| `auto_reload` | `true` | reload when a watched file changes (a `.c` or `.h` change restarts instead, and so does a `.cabal` change that is more than modules added: a loaded C object, or a package set, cannot be replaced) |
| `idle_stop_mins` | `0` | the session stops itself after this long unused (never while it serves). A composed session idles out only if every member sets it, at the longest |
| `async_refork` | `false` | a reload returns at its verdict and re-forks the servers in the background (also `reload --async-refork`, env `GHS_ASYNC_REFORK=1`) |
| `watch_typecheck` | `true` | a save is typechecked first, and not reloaded if that fails: the session keeps the last code that compiled |
| `watch_test`, `watch_refork` | `true` | what a SAVE does beyond compiling: run the tests, cut the servers over. Off, an explicit `reload` (or a commit, below) does them |
| `reload_on_commit` | `false` | a new git HEAD is a full reload -- checks and re-fork -- whatever the two above say |
| `status_url` | none | POST every verdict there as JSON, the intermediate ones too (`reloading`, `running check`): a dashboard's event feed. Best effort, 0.25 s |
| `handover_env` | `GHS_HANDOVER_OUT`, `GHS_HANDOVER_IN` | the two variables a forked server finds its state paths in (a project with its own copy of `GHC.Hygiene.Zygote` may name others) |
| `fast_start` | `false` | a start skips the build tool by itself when it can see everything the tool would build (see *What each operation costs*). This says to skip it also when it cannot -- a local dependency whose source cabal's plan does not place; then building that is yours (`start --fast` once) |
| `fingerprint_files` | `[]` | extra files that are part of a server's code (a C bundle) |
| `watcher` | `auto` | kernel file events where the platform has them (kqueue on macOS/BSD, inotify on Linux), else `poll`. The mtime scan still decides what changed and still runs every 2 s: an event only says "look now" |
| `poll_interval`, `debounce` | 0.2, 0.2 | when polling: how often the watcher looks, and how long it lets a burst of writes settle (with events a burst is over when they stop for 50 ms) |
| `load_timeout`, `eval_timeout` | 900, 600 | seconds. A command past `eval_timeout` is interrupted (SIGINT to the engine) |
| `history` | `true` | keep the session's history (`<state>/<session>/history/`): every request and verdict, and the summary tree over it (see *The history*) |
| `summarize_cmd` | none | the command that writes the tree's lines (stdin: the instructions, the context, the step; stdout: the line). None: the log is kept and the tree waits for an outside compactor |
| `summarize_jobs` | `8` | how many of those run at once |
| `agent` | `Agent` | the agent's name in the compactor's instructions |

State lives in `.ghci-session/<session>/`: `status` (the verdict, then the failing lines), `status.json`, `load.log`/`reload.log`,
`run.log` (the checks), `daemon.log`, `async.log` (output a background thread printed between commands), `server-<member>.log`.
`loaded_sources.tsv` is the signature the loaded code was built from (`<mtime ns>\t<path>`), for a cache in the loaded
code that is keyed by source. A reload publishes its status ONCE, when the verdict and what happened to the servers
are both known. A failing check in a state dir where none has ever passed is marked `[NEVER-PASSED]`: suspect the
target as much as the edit.

## Commands

```
start [--no-test] [--fast] | stop | restart [--fast] | status [-d] [SESSION]
reload [--no-test] [--no-refork] [--async-refork] [SESSION]
hold [--timeout SECS] [SESSION] | release [SESSION]
typecheck [SESSION]
test [-m MEMBER] [SESSION]
eval EXPR [-s SESSION]
compose SESSION [MEMBERS...] [--add M] [--remove M]
server [status|start|stop|restart] [-m MEMBER] [-s SESSION] [--resume]
gc [-n] [--days N]
autostop [--max-mem-mb N] [--idle-mins M] [--include-serving] [-n]
mem | log [FILE] [-s SESSION] | list | init
history [-n N] [--since ID] [--full] [--json] | history --kind user|talk|note TEXT
view [--wait SECS] [--json] | zoom ID [N] | date ID
mcp                                      # the session and its memory as an agent's tools (MCP on stdin/stdout)
```

With no session named, a command goes to the one that is running (else the config's `default`).
`reload --no-test` stops at the compile verdict, and says so (`CHECK SKIPPED`), so a compile-only verdict is never
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

### What a session's memory was, and three things that halved it

Measured on a 101-module session (158 MB live when fresh), same scenario before and after: the repl's footprint
went from 1,218 MB to 597 MB at the same wall time (91 s), and a fresh session from ~500 MB to ~410.

- **`-H` is off** (`"heap_auto": false`, the default). The engine is built as GHC's own executable is, with
  `-H`: after each major collection the RTS takes the largest heap it has needed and spends the difference on
  allocation area. Fewer collections, for memory that is never live: the RTS held 807 MB where it now holds ~530,
  and the run took exactly as long (more, smaller collections: 27 s of GC against 22, and 5 s less mutator).
- **Memory the RTS returns is returned** (`"mem_return": true`, macOS). The RTS decommits free megablocks with
  `madvise(MADV_FREE)`, which on macOS is a hint: the pages stay dirty and in the footprint until the system is
  short. A session whose RTS reported 551 MB had 830 MB of heap pages counted, and the copying collection after a
  reload looked 200 MB dearer than it is. `libghsmem.dylib` (`hygiene/c/mem_return.c`, built by `build.sh`, inserted
  by the daemon when it starts the engine -- dyld honours an interposer only from a library) turns a `MADV_FREE`
  inside the RTS's own heap reservation into a fresh anonymous mapping of the range, which drops the pages at once;
  `GHC.Hygiene.memReturned` says how much (2.3 GB over that run). The footprint now tracks what the RTS holds.
- **Nothing is left behind by an edit any more.** A session used to step up after its first edit and stay there
  (296 -> 356 MB live on that session), for two reasons. GHCi links a reloaded module when a command first NEEDS it,
  and the unlink ran once, after the reload's first evaluation: a module linked by a later command superseded its old
  copy after the pruner had been -- one stale generation of every late-linked module, for good. The engine now says
  how many libraries it has linked with every reply, and another unlink follows whenever that has grown. And a
  superseded module's LOCAL CAFs (the compiler's floated constants, which have no name to look up) were only dropped
  with a library that was superseded whole; a library holding other, still current, modules kept them. They are now
  found by their neighbours: the linker lays a library's data out object by object, so a closure between two
  exported closures of one module is that module's, and goes when those resolve elsewhere
  (`GHS_CAF_WHOLE_ONLY=1` is the old rule). Eight edits of that session: 313 -> 316 MB live, flat, and a footprint
  that ends BELOW where it started (617 -> 519 MB).
- **A census gives its tables back.** The heap walk's visited set is 8 bytes a slot and doubles as it fills: 128 MB
  for a few million closures, and it stayed allocated for the life of the session after the first `mem` or
  `census` -- so measuring the memory added to it.

### A reload without the compiler's scan (`GhsFastLoad`)

GHCi's `:reload` is the compiler's `depanal` and then its `load'`. The first rebuilds the module graph from
nothing -- every module's source opened, read and hashed to learn that it did not change -- and only then is the
edited module compiled. The daemon already knows which watched files differ from what is loaded, and says so before
each reload; the engine then takes the last load's graph, summarises only those files again (the compiler's own
`summariseFile`), reads the object and interface dates of the others again, and calls `load'` with it. On a
101-module session: the graph in 4-6 ms against 36-40 ms for the scan (up to 100 ms on the first), a leaf edit's
reload 0.40 s against 0.43.

It falls back to the scan whenever the last graph is not known to be enough: no change list or an empty one, a file
added or removed, a changed file that is not in the graph, one whose IMPORTS changed (the graph's shape), or the
session's first load, whose graph carries no source hashes (kept, every module would look changed). What only the
scan does: the warnings about home modules missing from a `.cabal` and unused packages appear on a scanned load.

`GHS_FAST_GRAPH=0` always scans. `GHS_FAST_GRAPH=verify` builds the graph both ways, says in the load's output
(and in the file `GHS_FAST_GRAPH_LOG` names) whether they are identical, module by module -- source hash, imports,
the four dates -- and loads the scanned one: 26 random edits over 26 modules of that session, single and in pairs,
were identical every time. It is the one edit to the vendored `GHCi/UI.hs` (`vendor/fetch.sh` makes it).

### When a start asks the build tool, and what

`cabal repl` takes 4-8 s on a project of any size even when nothing needs building: it configures the session's units
every time (a plain `cabal build` that finds everything up to date is 0.5 s). So a start asks it only for what only
it can say, and what is recorded (`<state>/<session>/launch/<command hash>/`) is in two parts:

- **the answer** -- how the engine is to be started: each unit's flags and modules. It depends on the command and on
  the build files: the project files, every `.cabal` under the watched directories, and the `.cabal` of each package
  the repl loads. A change there runs the repl command again (`build` in `[time] boot`).
- **the dependencies** -- the sources of local packages the repl uses without loading. A change there changes nothing
  in the answer; those packages have to be built: `cabal build --only-dependencies <units>` (`build_deps`), which is
  the compile and no more. (With a `repl` command of your own the tool does not know how to ask that, and runs it.)

Neither depends on the engine's binary, so rebuilding the tool does not send every session back to the build tool.
`restart` by hand still asks; `restart --fast` and a plain `start` decide as above.

### What the library and its commands cost

On `examples/hello` (a 90 MB repl, which is GHCi itself): `store` and `why` 25 ms, `bench ACTION` 34 ms,
`census EXPR` 26 ms, `census --kept` 30 ms, `mem --heap` 0.32 s, `census` (every CAF: 2 M closures) 0.36 s,
`census --strings` 1 s. A major collection of even that heap is 0.17 s, and it used to be the floor of every one of
these: `bench` forced two (now only with `--live`, for the live heap's change), a value's census one (a walk from a
root reaches only what is live, so a minor collection -- for the indirections a just-evaluated thunk leaves -- gives
the same numbers), `mem --heap` three (now one). In the session's own code: `storeRef` of an existing slot 1.5 us
whatever the number of slots (a hash table), `kept` 2 us a decision, `sourceHash` of an unchanged file 5 us.

### Where a reload's time goes inside GHCi, and `prune_gc`

Sampling the engine through six saves of a one-module project: the compile is 22 ms; **0.22 s is `dlopen`** of the
new temporary library (`fcntl` in `dyld`: macOS validating a file it has not seen; the Developer Tools setting
avoids it -- System Settings, Privacy & Security, Developer Tools, the terminal app, then quit and reopen that app:
measured, the save's reload went 0.45 -> 0.20 s); and **0.23 s is the major collection after the unlink**, most of it the compacting collector a session's
`-c` selects, which is single-threaded. `"prune_gc": "copying"` (`GHS_PRUNE_GC` in the daemon's
environment overrides) makes that one collection a copying one, using every capability: measured on a 370 MB session with 3
capabilities **0.6 s -> 0.1 s after every reload** (0.31 -> 0.20 s on the one-capability example). It is the default
since 2026-10-06, by choice of speed over memory: the same session ends at ~1,200 MB of footprint against ~1,000
under `"prune_gc": "compact"` (the RTS's own compacting collection). It runs twice: the first copies into fresh space
and the next major collection is what gives the old space back -- with the default `-Fd0.5` in `rts_flags` the RTS
then reports LESS in use than under compaction (806 MB against 831), but macOS goes on counting those pages in the
process's footprint, with or without `--disable-delayed-os-memory-return`: that part is real only under memory
pressure. (`-Fd0` does not mean "at once": it switches returning off.) `GHC.Hygiene.majorGC False` is that collection for your own code. (It
sets the old generation's `mark`/`compact` for the collection that follows at once: the flag alone takes effect a
collection late. The generation's layout depends on the RTS's way, so `gc_once.c` is compiled as the threaded RTS's
and checks what it reads before it writes.)

### Sharing that is missed: `census --dups`

```bash
ghci-session census --dups [--top N]         # over every CAF
ghci-session census --dups --kept            # over the store's slots
ghci-session census --dups 'Mod.value'       # within one value
```

Every data closure gets a hash of what it IS, bottom-up -- its constructor (by name), its plain words, the hashes
of what it points to; a byte array, its bytes -- and closures with one hash are the same value built twice. The
report: how many bytes maximal sharing would give back, by constructor, by the root that holds the copies (roots
are walked in order: a copy belongs to the later one), and the largest repeated values, shown, with what their
extra copies cost. A thunk, a function or a mutable cell is only ever itself; a value repeated only because the
value holding it is repeated is not listed again; a part two copies physically share is not counted, and an extra
copy that many of them share is counted ONCE (a stray copy of a title that 222 one-cell lists point at is one
string: charged to each list it read as 0.15 MB where 5 KB was lost). On a
101-module session: 4.9 M closures in 2.6 s, 55 of 192 MB duplicated -- 86% of the boxed `Double`s, and the same
4.4 MB painted view held four times by a memo. It runs as ONE foreign call: it keys closures by address, and the
collector must not move anything under it (walking a root a call crashed, and reported terabytes).

### State that outlives a reload: `GHC.Hygiene.Store`

A reload reverts every CAF of the modules it links again, so a cache a module keeps in a top-level `IORef` starts
empty after every edit. The engine is not reloaded, and holds named slots (`hygiene/c/store.c`):

```haskell
{-# NOINLINE cache #-}
cache :: IORef (Map Key Value)
cache = unsafePerformIO (storeRef "myproject.cache.v1" Map.empty)
```

`storeRef` hands back the same ref under a name for the life of the process. It is the idea of the `foreign-store`
package with names instead of numbers, in the engine instead of a dependency -- so the tool can list the slots
(`ghci-session store`), say what each retains (`census --kept`) and forget one (`store --drop NAME`: its owner starts
from its initial value the next time it is linked). A project that does not want the library in its `build-depends`
can look `ghs_store_get` / `ghs_store_put_new` up with `dlsym`, as any loaded code may. A slot is untyped: put a
version in its name and change it with the type; and store evaluated values, since a thunk holds the code of the
generation that built it. Outside the engine `storeRef` is a ref per name for the life of the process.

### A memo that outlives a reload, and `why`: `GHC.Hygiene.Kept`

A long computation is a chain of stages of which an edit changes few, and a reload throws all of their values away.
A stage is remembered under a hash of the VALUES it reads:

```haskell
paint = hValue (kept "paint front" [("code", code), ("view", viewHash v), ("boxes", boxesHash)] edgesHash (render sc v))
  where code = unsafePerformIO (sourceHash ["src/Render.hs"])
```

Equal values are held once: a stage whose output hash is that of a value already in the table, at the same type
(`Typeable`: an entry is also only ever handed back at the type it was stored at), keeps that value -- so the output
hash must identify the value. `census --dups` found one 4.4 MB painted view held four times; `why` says
`(same value as <slot>: shared)`. "The same type" means the same LAYOUT: a `TypeRep` is a type's name, and a type of the
program being edited keeps its name when its definition changes -- a value built before the change, handed to code
compiled after it, is read at the wrong offsets (an edit that unpacked a record's fields and its revert killed a
session). So a type that mentions any type of a package built in place is shared only between stages with the same
input labelled `"code"` (what lays the value out; without one, only with its own slot's earlier key); a type made of
installed packages' types alone -- bytes, text, numbers -- is shared across everything.

The scheduler is laziness (a stage runs when its value is asked for; there is no graph to declare), a stage hands
its own output hash to the stages after it (so one whose output did not change stops the recomputation there), and
a slot keeps its last two keys (an edit and its revert both hit). `ghci-session why` prints what ran again since it
was last asked, the input that moved and the seconds of the stage's own work -- plus the time spent reading inputs
that are no stage, and anything wrapped in `timed`: the top line of a slow command is what to make a stage next.
`KEPT_VERIFY=1` (set in the session: `System.Environment.setEnv`) recomputes on every hit and reports a kept value
whose hash differs, which is how a key that misses an input is found; `KEPT_SKIP` bypasses the memo. The code a
stage runs, and the modules defining its types, are an input like any other (`sourceHash`). A value that reads
everything gains nothing here: it wants a faster search, which `why` followed by `bench` on its inputs will show.

The engine compiles its own copy of the hygiene modules into itself rather than depending on this package: a
session whose project depends on `ghci-hygiene` loads its own build, and the two share only the C store, by name.

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
GHC.Hygiene.Census.benchQuick "label" action  -- wall, GC, allocation (`ghci-session bench`)
GHC.Hygiene.Census.benchOf "label" action    -- the same and the live heap before and after: two collections (`bench --live`)
GHC.Hygiene.Census.memNow

GHC.Hygiene.Store.storeRef name initial                  -- an IORef that outlives a reload, by name
GHC.Hygiene.Kept.kept slot inputs outHash value          -- a stage remembered across reloads under a hash of what it reads
GHC.Hygiene.Kept.why                                     -- what ran again, the input that moved, the seconds (`ghci-session why`)
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
closure layouts it then reads are the compiler's the engine is built for: the closures through that compiler's own
`Rts.h`; the few offsets written in the C (`ObjectCode`'s type, file name and links, `Section`, `StgIndStatic`'s
static link) were measured with `offsetof` against GHC 9.14.1's, 9.10.3's and 9.6.7's headers and are the same on
x86_64. Tested on GHC 9.14.1 (macOS arm64, Linux x86_64), 9.10.3 and 9.6.7 (Linux x86_64; on macOS the pruner is off before 9.14,
the dlopen handle it reads there having been measured on 9.14 only). `keepCAFs = 0` is *not* an option (SIGBUS: interpreted code refers to CAFs by raw
address, GHC #23182).

## Layout

```
ghci-session.cabal      the package: the engine and the command (hygiene/ghci-hygiene.cabal: the library)
app/GhciSession/        Json, Config, Sys (the FFI), Repl (starting the engine, its protocol), Watch, Daemon, Gc, Cli, SelfTest, SelfBench
engine/, vendor/        the engine: GhsEngine.hs and Main.hs (ours), GHCi's own sources per compiler version
cbits/                  ghs_sys.c (sockets, file events, regex, hashing, processes), ghs_main.c (the entry point)
hygiene/                c/*.c (the pruner, the census: compiled into the engine), src/GHC/Hygiene*.hs (the library),
                        repro/ (why a superseded CAF with a young value must stay listed)
bin/ghci-session        run from a checkout (builds if stale)
bin/ghci-history        save a session's history (log, tree, usage) to a git branch, and load it back: `save`, `load`, `status` (`--help`)
ghci-session.json       the session this package runs on itself
examples/hello/         two packages, a CAF that leaks without pruning, a server with state to hand over
examples/tour.py        every feature on a copy of it, each step checked and timed (the benchmark)
tests/test_e2e.py       GHS_E2E=1: the lifecycle end to end, and the CAF reproduction
```

## Status

Working: plain and composed sessions, per-member checks, auto-reload, verdicts and staleness, memory budget, pruner,
census, forked servers (keep / re-fork, also in the background / handover / adoption), `gc`, idle stop; 82 self-tests,
the tour (123 steps) and the end-to-end tests. This package's own two sessions (`tool`, `engine`) run on it.

Not here: the deferred GC after an unlink (it crashed a large session; the GC is immediate). A compiler
other than GHC 9.14.1, 9.10.3 and 9.6.7: the engine is that compiler's front end, so another needs its sources vendored
(`vendor/fetch.sh`) and has not been tried. Port verification needs `lsof`.

GHC 9.6.7 and 9.10.3 (Linux x86_64, cabal 3.18): both executables build and the tour passes, 142 steps with 3 known
to differ (the tour reads the compiler and checks what it does instead), in 97 s and 104 s; 9.14.1 is 145 of 145
there. The pruner holds the `leak` group flat (+0 MB over five reloads, against +242 MB without it) and `partial`
passes; the census, servers, budget, hooks, idle stop and `gc` are as on 9.14. What differs before 9.14, all from
its GHCi (9.10's engine modules, `engine/ghc-9.10`, are 9.6's with 9.14's load, which takes a diagnostic wrapper):
- **A composed session sees one unit at the prompt.** Before 9.14, GHCi has no interactive units: the prompt resolves
  a module through one home unit, which finds its own modules and its direct home dependencies'. The engine makes
  that unit the one that sees the most (`promptUnit`, `engine/ghc-9.6/GhsCompat.hs`), so a package and one that
  imports it are both in scope; of three packages in a chain, one is not.
- **A package added to a running session restarts the repl** (`compose --add`): its GHCi takes units at the start.
- **A save that changes only a comment re-forks a server** instead of keeping it (with its state): there is no
  `-fobject-determinism` before 9.12, so an identical source compiles to a different object.
- On 9.6, its reload libraries are `libghc_N`, loaded with a plain `dlopen` that the RTS keeps no record of; the
  pruner finds them through the dynamic linker instead (`ghci_cafs.c`).
- On 9.6, the client's start-up skips `-xr` (GHC 9.10's): the older runtime refuses an option it does not know.

Linux (x86_64, GHC 9.14.1 and cabal 3.18 from ghcup, Ubuntu 24.04): both executables build and the tour passes,
145 of 145 steps. The pruner works there too: the tour's `leak` group holds the live heap at 78 MB over five reloads
where plain GHCi grows 243 MB, its `partial` group passes, and `hygiene/repro/run.sh` passes (and `unsafe` dies,
as on macOS). The ELF half of `hygiene/c/ghci_cafs.c` finds the libraries with `dl_iterate_phdr` and each one's
exported symbols in its file's `.dynsym`; a temporary library's dlopen handle comes from `dlopen(RTLD_NOLOAD)`,
since the ObjectCode offset measured on arm64 does not hold on x86_64 (the pruner checks the handle names the
file, and touches nothing if one does not). The format-free parts are `caf_common.h` and `caf_modules.h`, which
the Mach-O half includes where it had them. `compose --add` takes a package into the running repl there as on
macOS: cabal 3.18 on Linux starts the repl with ONE response file holding every argument (the flags, then a
`-unit @file` per unit), which the live path now opens up, and a unit is known by the `-this-unit-id` its file
declares, not by the file's name (which carries the build tool's numbering of that run, moved by a unit added
before it). The history, the view and the compactor are platform-free.
The heap census works on Linux: the engine exports its `ghs_*` functions to code loaded into it
(`--export-dynamic-symbol=ghs_*`; Linux shows `dlsym` none of an executable's own symbols otherwise, and every
`census`, the store and the major GC answered "not the engine"), and the RTS's private lists (`dyn_caf_list`,
`sm_mutex`, `loaded_objects`) are read from the full symbol table of the RTS shared object's FILE
(`hygiene/c/rts_syms.h`: only the exported table is mapped; a ghcup GHC's RTS is not stripped). On both platforms
a census row is named `unit:Module.name` (the symbol z-decoded), and a CAF the compiler made "a local CAF of
Module", by the module of the exported closure below it -- not dladdr's nearest symbol, which is another value's
name. `build.sh` touches the C sources
when a header is newer than the last build: cabal recompiles a C file for its own changes, not its headers'.
