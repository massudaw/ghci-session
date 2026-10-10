# Performance of ghci-session itself

Measured on 2026-10-10 on the project's own tool session (GHC 9.14, the session's own build flags), on a copy of
its history (`.ghci-session/tool/history`: 1,465 messages, 2,922 tree nodes) and of the knowledge store (985 facts,
275 KB). Nothing here ran at -O2 (the sibling -O2 session does not boot on this repository); numbers are the plain
`bench` of the loaded session, "wall" is the median of the runs, and the command line was timed as a process
(`.bin/ghci-session`, built by `build.sh` at -O1, as users run it).

## Census

### (a) What the daemon logs of itself (`[time]` lines, seconds; the dxf log read only)

| operation | how measured | time | where the time goes |
|---|---|---|---|
| boot (tool) | 29 `[time] boot` lines | mean 15.9, max 46.7 | `build` 9-21 on a rebuild, `load` up to 36.7 (GHC); a warm boot is about 2 (load 1.2, check 0.6) |
| restart (tool) | 17 lines | mean 11.7 | the same: cabal/GHC |
| reload (tool) | 54 lines | mean 2.6, max 20.2 | `ghci_reload` (GHC compiling the changed module and what depends on it) |
| typecheck (tool) | 124 lines | mean 0.28 | GHC |
| boot / restart / reload / typecheck (dxf) | 14 / 7 / 137 / 282 lines | 13.9 / 10.7 / 2.2 (max 38.6) / 0.09 | GHC, a larger library |

Nothing in these is ghci-session's own code: they are the compiler. The history ops the daemon serves are not
logged with `[time]`; they are in (b).

### (b) The pure hot paths (`bench`, in the session)

| operation | how measured | time | allocated | note |
|---|---|---|---|---|
| `Know.loadFacts` (985 facts) | bench x10 | 13.9 ms | 17 MB | parse 3.4 ms of it; the rest the fold over marks |
| `Know.render 16000` after loadFacts | bench x20 | 12.5 ms (render itself ~0) | 25 MB | |
| `Know.rank` over 1,264 messages (823 KB) | bench x20 | 34 ms | 155 MB | what `recall` and `history --search` run |
| `H.messages 0 n` (all 1,465 messages) | bench x10 | 72 ms | 72 MB | reads every line from disk |
| messages + rank (the recall op) | bench x10 | 93 ms | 227 MB | |
| `H.openHistory` | bench x5 | 35 ms | 93 MB | |
| `H.snapshot` + `renderView` | bench x20 | 0.77 ms | 4 MB | |
| `H.treeTexts` | bench x10 | 1.15 ms | 0 | |
| `H.appendMsg` | bench x20 | 1.0 ms | 0 | an fsync |
| `H.messages mm 1000 1` | bench x200 | 0.02 ms | 0 | |
| Json parse, 862 KB reply | bench x10 | 12.3 ms | 13 MB | |
| Json encode, same reply | bench x10 | 3.96 ms | 14 MB | |
| Json parse of the 985 facts / 590 log lines | bench x20 | 3.4 ms each | 12.5 MB | |
| Top's layout of all 1,465 messages at width 120 | bench x5 | 416 ms | 1,964 MB | only the last 400 are laid out, once, then cached (3 ms a key) |
| `Search.grep "recall"` over the project (fallback) | bench x5 | 64.6 ms | 223 MB | what the agent's `grep` runs, built without libfff |
| `Search.searchFiles "Know"` (fallback) | bench x5 | 13.2 ms | 25 MB | |
| SelfBench: scan 0.21, compare 0.22, repl decode 0.01, verdict 0.17, hash 64 MB 11, process table 0.95, pidAlive 0.00 | `SelfBench.run` | ms | | all small |

### (c) The command line as a process (`timeit`, median of 10-20 runs; `/usr/bin/true` is 2.5 ms)

| command | median |
|---|---|
| `help` | 9.9 ms |
| `history -n 1` / `-n 5` | 11.3 / 12.2 ms |
| `zoom 100 1` | 11.9 ms |
| `status` | 15.4 ms |
| `view` | 15.4 ms |
| `memory` | 19.1 ms |
| `eval 1+1` | 13.7 ms |
| `knowledge` | 25.4 ms |
| `doc rank` | 29.1 ms |
| `history --search daemon` | 84.2 ms |
| `find Know` | 115.5 ms |
| `grep recall` | 134.0 ms |
| `import` (the Claude Code Stop hook, every turn: 26 MB of session files) | 530 ms |

## What costs the most (frequency x time)

1. `grep` / `find` of the agent: every agent turn makes several (a call in three is a search); the fallback decodes
   every file of the tree to Text (223 MB a call) and the CLI adds the start: 64-134 ms a call.
2. `import`, run at the end of each Claude Code turn: 530 ms, nearly all of it parsing 26 MB of session files that
   did not change.
3. `rank`, behind `recall` and `history --search`: 34 ms of scoring over 155 MB of allocation, 93 ms with the
   messages read.

The boots and reloads are the compiler's; a start of the command line is 10-20 ms and not worth more.

## Optimisations

### 1. The fallback `grep`: do not decode what cannot match (`Search.fallbackGrep`)

Before, every file of the tree was read whole and decoded to Text, to be searched for a line; a binary file was read
whole (a 28 MB runner) to be passed over. Now the first 8000 bytes are read first (a NUL: passed over, the rest
not read), and a file whose bytes do not hold the query's UTF-8 bytes is not decoded. (One difference: a query with
a space in it no longer matches across a byte that was not UTF-8 and was replaced by a space.)

| call (bench, same expression before and after) | before | after |
|---|---|---|
| `S.grep root "recall" 30` (hits come early) | 64.6 ms, 223 MB | 35.5 ms, 45 MB |
| `S.grep root "zzqxnomatch" 30` (the whole tree) | 61.9 ms, 252 MB | 29.2 ms, 34 MB |

As a process (`.bin/ghci-session grep recall`, built at -O1, 10 runs): 134.0 ms median before, 76.3 ms after. (`find Know`,
whose code did not change, read 115.5 ms then and 68.5 ms now: the machine was busier at the first run, so the
census numbers of (c) are +-40% and only the code-path ratios above are to be trusted.)

Self-tests: 357 pass.

### 2. `recall` / `history --search`: read the messages in runs, count words where they stand (`History.messages`, `Know.rank`)

Two costs under one call. `H.messages` opened, seeked and read the file once for each of the messages (1,465 of
them: 72 ms); now the messages that follow one another in a file are read with one read of the span (the same
messages: tested equal to reading them one at a time, whole and in a part). `Know.rank` split every text into words
to count the few that are a query's; now it finds each query word in the lowered text and checks the characters next
to it (the same scores: compared with the old function on six queries over the 1,264 documents, scores to 1e-9 and the
same set).

| call (bench, same expression before and after) | before | after |
|---|---|---|
| `H.messages mm 0 n` (all of them) | 72.4 ms, 72 MB | 12.6 ms, 43 MB |
| `K.rank "daemon knowledge fold"` over prepared documents | 34.3 ms, 155 MB | 16.2 ms, 75 MB |
| the recall op: messages + documents + rank | 92.9 ms, 227 MB | 31.6 ms, 119 MB |
| `H.messages mm 1000 1` (one message) | 0.02 ms | 0.02 ms |

Not measured as a process: `ghci-session history --search` asks the running daemon, which still runs the code of
before (it is not restarted here); it read 84 ms at the census and 106 ms now, on a history that has grown, and
that is the number the daemon's restart will move. `tools/check-knowledge.py`: all 14 hold. Self-tests: 357 pass.

### 3. `import` (the Claude Code Stop hook, every turn): no tool entries unless asked, timestamps by hand (`Import.readSession`, `isoSeconds`)

Reading the 37 session files (26 MB, 9,892 lines) made the entries of every tool call and result, the largest texts
of a file, to drop them in `wanted` unless `--tools` was asked for, built the file's entries with a lazy fold, and
parsed each line's timestamp with `parseTimeM` (10 us a line, 0.1 s of the import). Now `readSession` takes whether
tools are wanted and makes no entry for them otherwise, the fold is strict, and `isoSeconds` reads the one shape the
programs write by hand (anything else still goes to `parseTimeM`: 5,600 timestamps over leap years, month ends and
fraction lengths compared with it, all equal). One difference: a session with nothing but tool entries is not counted
among the files "with messages" when `--tools` is not given.

| call | before | after |
|---|---|---|
| `.bin/ghci-session import` (5 runs, built at -O1, nothing new to import) | 530.6 ms median | 275.0 ms median |
| `mapM readSession files` in the session (bench x5; its GC is the session's own heap) | 685 ms, 732 MB | 553 ms, 834 MB |

Self-tests: 357 pass.

### 4. `import`: a file as it was at the last import is not read (`ImportStamp`, `imported-files.json`)

What was left of an `import` was the parse of every line of 26 MB, nearly all of it of files that had not changed.
Now the size and the modification time (to the microsecond, a whole number, so that it is written and read back as it
was) of each file that was decided on -- taken, or passed over as nobody's, or with nothing new -- are kept beside
`imported.json`, and a file whose two numbers are the same is not opened. A file that is picked but not yet imported
(a plan without `--go`) keeps its old stamp, so is read again. Only the plain import uses and writes them (`--tools`,
`--all`, `--since` ask another question of the same file); with no `imported.json` nothing is skipped. The stamp is
taken before the file is read, so a line added while it is read is read the next time.

| call | before | after |
|---|---|---|
| `.bin/ghci-session import` (10 runs, built at -O1, nothing new; 32 files) | 274.3 ms median | 12.6 ms median (13.4 on a second set) |

The file of the turn itself has changed, so it is read whole: a session's file is 18 MB at the end of a long
round and costs what it did (about 0.2 s) in the hook of that turn; reading only what was added (from a byte
offset, with the session's state kept) would make it free too, and is not done. Self-tests: 359 pass (two new:
the stamps' decision, and their file written and read back, a grown file, a missing one).

### 5. `Json.parseJsonBS`: strings joined once, numbers without `read`, keys without a `Text`

Three things in the parser were slow: a string with escapes made a `Text` of each piece and each escape and joined
them (a reply of 8,000 lines is 8,000 `\n`); a number went through `String` and `reads` (a fact's `first` and `last`
are `1.791556110883025e9`); a key was decoded to `Text` and unpacked; and `skip` asked `isSpace` of each byte. Now
`rawString` finds the closing quote with `memchr` (a string with no backslash is a slice of the input), joins the
pieces of one with escapes as bytes and decodes once; a key of ASCII bytes is unpacked straight from them; a number
whose mantissa is a whole number up to 2^53 and whose power of ten is up to 10^22 (the Clinger case: one exact
multiplication or division, rounded as `read` rounds) is made without `String`, any other goes the old way; `skip`
tests the six bytes. Same results: 300,000 generated numbers against `reads` (sign, digits, point, exponent, -0),
and all 1,575 lines of the facts and of a day of the history parse and print back to themselves.

| bench (`pure () >>= \_ -> evaluate (size of parsed)`, x30) | before | after |
|---|---|---|
| 862 KB reply (8,000 escaped lines) | 11.4 ms, 13.5 MB | 7.5 ms, 11.0 MB |
| facts file, 985 lines (274 KB) | 9.5 ms, 20.8 MB | 5.9 ms, 8.3 MB |
| a day of history, 590 lines (410 KB) | 7.6 ms, 20.8 MB | 4.0 ms, 4.4 MB |

The reply's 7.5 ms is mostly the copy of 800 KB into a `Text` and the collector's pass over it (3.8 ms GC); a parser
that makes no `Either` of each value would take the facts and history further. Self-tests: 363 pass (four new,
of numbers, string escapes, and strings that do not end).

### 6. `grep` and `find`: the file list is git's, not a walk

Chosen over a list kept by the daemon: `grep` and `find` run in the CLI's own process and in the chat as well as
the daemon, so a daemon's list needs a call to it (and fails when it is down), and a list cached in a file needs a
validity check that is itself a walk of the directories. A git work tree has the cache already, with the check: `git
ls-files --cached --others --exclude-standard` (14 ms, 193 files) lists what is tracked or not ignored, as libfff
does, so the 2,000 files of ignored build trees (gba/nes runners, libfff) are not read either. Not a work tree, or no
`git`: the walk, as before. Dot names and `dist-newstyle` stay out.

| bench (-O1 in-session, x10) | before | after |
|---|---|---|
| grep, no match | 29.2 ms, 34 MB | 25.5 ms, 17 MB |
| grep "recall" | 35.5 ms, 45 MB | 26.5 ms, 25 MB |
| find "Know" | 13.2 ms, 25 MB | 13.8 ms, 4 MB |

A small gain in time (the process spawn costs what the walk did, so `find` is as it was) and a half of the memory;
the change of meaning (ignored files are not searched) is the point as much as the time. Self-tests: 363 pass.

### 7. `bench` with `opt`/`unit` (harness): the timeout bounds the wait; no objects across flags

No number: a first `-O2` compile of this repository takes over 15 minutes and was not run to test this. The call's
`timeout` (default 30 s) now bounds the wait for the sibling session to be started and reloaded (`System.Timeout`
around the start and the reload request; the `start` process and the daemon are detached, so they go on). When it
runs out the answer's first line says `[in tool-O2: the FIRST compile of the code at -O2 is under way, N s so far ...]`
(or `reloading`), that nothing was measured, that asking again later picks it up, and the status line and the last
line of the sibling's log. A daemon that is up but not listening yet is the same wait, not an error. A boot that
fails says its status line and why. Tested by the types and a self-test of the flags rule, not by a run.

The log line `objects: 45 module(s) taken from tool`: a sibling's objects were copied into the -O2 session whatever
their flags. Not right: the interface carries the optimisation flags and GHC compiles a module again when they differ
(`ghc -O1 -c M.hs` then `--make -O2`: `[Flags changed]`), so the copy cost its time and saved nothing. `seedObjects`
now takes only from sessions with the same flags (`Config.sameFlags`: the same level and info-table mapping among the
measuring sessions; configuration sessions among themselves). A base session that is itself `-O1` could feed a `-O1`
measuring one; not done, since the base's real level is not known from its name. Self-tests: 365 pass.

## The fresh call after a rollover (what a reset carries, in the model's tokens)

`chat --carry N` (no model call) builds, for each of the last N rollovers of this chat, the first message of the call
that went on from it, with the code the chat builds it with (`Carry.carrySplit` and friends), and says what each part
is in bytes (messages or lines in brackets). `GHS_CARRY_DUMP=DIR` writes the messages too; the tokens below are the
model's own count (sonnet, the claude command with no tools and a one-line system prompt: 2 tokens of its own, 532 for
`say ok`), so tokens/byte is measured, not assumed: 2.1 bytes a token on this view/log format (124 KB of `--print-view`
is 57.7k tokens). The view column is today's view cut where each log began; the summaries were not the same then.

| part (bytes)               | #2800, old rule | #2968, old rule | #3303, old rule | #2800, now | #2968, now | #3303, now |
|----------------------------|----------------:|----------------:|----------------:|-----------:|-----------:|-----------:|
| system prompt              |            6347 |            6347 |            6347 |       6347 |       6347 |       6347 |
| tools (16 KB of schemas)   |           16083 |           16083 |           16083 |      16083 |      16083 |      16083 |
| view: summary lines        |      17041 (44) |      20586 (54) |     36835 (105) |  17793 (46) |  21138 (56) | 46063 (141) |
| log: the turn's message    |              67 |              67 |            3973 |         67 |         67 |       3973 |
| log: what the user said    |            5356 |            5356 |              55 |       5356 |       5356 |         90 |
| log: the tail, whole       |      94276 (97) |      93431 (98) |      88730 (81) |  31686 (50) |  30954 (60) |  31247 (35) |
| gap line, note             |             628 |             628 |             627 |        628 |        628 |        628 |
| (not given) subjects block |           16173 |           16173 |           16173 |      16173 |      16173 |      16173 |
| given, bytes               |          139798 |          142498 |          152650 |      77960 |      80573 |     104431 |
| message + system, tokens   |           53166 |           55491 |           59294 |      26226 |      26414 |      38554 |

The tools (about 5-6k tokens) come on top in the real call. The first call's tokens in the log for the old rule, as
the usage lines have them: 66.1k (#2612), 43.7k (#2800: the view then was smaller), the sixth 56.4k with 98% cached
(the same view and system as the call before it, still in the provider's cache).

What was waste, and what is done: **the tail given whole** was 93 KB, 60% of the call (old rule: the whole `--tail`,
96000 characters, for a rollover as for a restart). A rollover now carries a third of it (12000 characters minimum),
and the messages older than the last six are cut to 1500 characters with their number to zoom (`Carry.capOld`): a
read of two hundred lines that was answered and acted on is for the summaries, and the same bytes now hold 50-60
messages instead of 80-100 whole ones. A call is 26-39k tokens, not 53-59k: about half. What the user said stays
whole, however old.

What is left, and why: the view (18-46 KB, a quarter to a half of the call) is the memory of the whole chat (for a long turn,
the lines before the cut include summaries of the turn's own earlier messages: not measured how many); it is not cut, because it is what stops a fresh call from starting over -- the controller's relearn
share (item 5) is what says if it must be. The tools' schemas (16 KB, 20% of a cold call, a tenth of that price on
every call after it) hold `session` on thirty tools; dropping tools from the agent's reach is a change of what it
can do, not waste. The subjects block (16 KB) is not given to a fresh call (the view before the log has none).
The system prompt (6 KB) is the agent's rules.

Item 5, the files: a fresh call is given `<files>` (`Carry.carryFiles`, under 900 bytes): what the turn has read and
written, latest first, with the lines read last. On this chat's last three rollovers it is 363-686 bytes (6-11 files),
about 100-170 tokens: under 0.5% of a call. Whether it helps is counted, not assumed: after each reset the next ten
calls are counted for reads of a file read before the reset (`Carry.relearned`, from the log alone), logged as a harness
echo, and fed to the controller as its relearn share (`Roll.seeRelearn`, a running estimate, once a reset). It starts at
0.28 (measured by hand on this chat and dxf, before the list); comparing the logged shares from now on with that is how
to see whether the list helped. Not yet seen live.

## The replay of the controller (item 7)

`chat --replay-rollover [--ratio R] FILE...` (`Replay.hs`) runs the controller over the `[usage: ...]` lines of recorded
chat logs, no session, no model call. It is a model, not a run: the growth of a call is what the log had (not what an
agent that had been reset would have done), a reset's next call costs the log's median fresh call (written) plus the
relearn share (0.28) of it, and boundaries, the cold cache and the rot signal are not replayed (they need the tool
calls). List prices: read 0.30, write R x read, output 15 dollars a million tokens. Logs of 2026-10-10:

| log (calls) | R | policy | resets | dollars | mean context |
|---|---|---|---|---|---|
| this chat (654, fresh 56k) | 12.5 | fixed 150k | 11 | 33.78 | 103k |
| | | fixed 110k | 18 | 31.59 | 81k |
| | | auto | 19 | 32.08 | 83k |
| | 20 | fixed 150k | 11 | 37.88 | 103k |
| | | fixed 110k | 18 | 36.93 | 81k |
| | | auto | 14 | 36.67 | 88k |
| dxf (391, fresh 81k) | 12.5 | fixed 150k | 10 | 26.37 | 115k |
| | | fixed 110k | 22 | 28.52 | 97k |
| | | auto | 10 | 26.26 | 114k |
| | 20 | fixed 150k | 10 | 30.53 | 115k |
| | | fixed 110k | 22 | 35.40 | 97k |
| | | auto | 8 | 30.41 | 124k |

Auto against fixed 150k: -5.0% / -0.4% (R 12.5; this chat / dxf), -3.2% / -0.4% (R 20). Against 110k: +1.5% / -7.9%
(R 12.5), -0.7% / -14.1% (R 20). So the threshold alone is worth little, as measured before (about 5%): auto is never
worse than 150k here, and it avoids 110k's cost on dxf, where a fresh call is large (81k) and a reset dear. What I would
not trust: the replay keeps the growth the log had after a reset, takes the fresh-call size as the log's median (so it
knows what auto has to learn), and counts no benefit of a shorter context (fewer repeat reads, less rot) -- the one
thing that would favour a lower threshold; the saving on the cold cache and on the carry (items 1, 3, 5) is not in it.

## Not done, and next

- A file list kept by the daemon would save the 14 ms of `git ls-files` (see 6); not done, see there for why.
- `Top`'s layout of a whole history (416 ms, 2 GB) is paid only for the last 400 messages, once; if the history
  opens whole, it is the cost to look at.
- Boots and restarts (12-16 s, up to 47) are cabal and GHC; the watchdog's and the compactor's costs are in model
  calls, not CPU.
