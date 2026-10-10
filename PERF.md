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

Self-tests: 357 pass.

