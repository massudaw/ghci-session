# Review of `8af91d927..HEAD` (42 commits, 32 files, +2,868/-228)

Read as a maintainer who did not write it: every hunk of the files under `app/` with the code around it;
`Know.hs`, `Roll.hs`, `Carry.hs`, `Replay.hs`, `Inbox.hs`, `Quota.hs`, `QuotaFit.hs`, `ImportStamp.hs`, `SelfRound.hs`
entire; the new `tools/check-*.py`, `fake-claude.py`, the README hunks. Findings by seriousness. "Sure" = shown by
running it or by a path that cannot be otherwise; "likely" = a path I can describe but did not make fail.
Nine fixes are committed locally (`review 1` .. `review 9`), nothing pushed; after them the self-test is 498 passing,
`check-cli.py` 22/22, `check-tui.py` 46/46, `check-edit.py` 6/6.

## Fixed

### 1. `rollover_ratio` was always "set": the one-hour price ratio of 20 never applied  (HIGH, sure) -- c0ebdad8e

`Config.hs:95` had `("rollover_ratio", JNum 12.5)` in `defaults`, and `loadConf` merges `defaults` into every target. So
`rolloverRatioSet` -- "the ratio the configuration fixes, if it does" -- was `Just 12.5` for every session whose file
never mentions the key (shown with `eval` on this repository: `Just 12.5`; its `roll.json` has `"kind": 2`, one-hour
writes seen, yet the controller priced at 12.5). So `rFixed` was always True: metering item 3 (0bac6edcd) did nothing
live; `top`'s tab 5 said "ratio fixed by rollover_ratio" everywhere; `usage --quota` reported 12.5 "in use"; the
README row ("default 12.5") and the paragraph that says the writes decide contradicted each other. The auto threshold
was lower than the design says (the figures of today's `roll.json`: 119k with 20, 106k with 12.5).
Why the tests did not see it: `SelfRound.hs` built `emptyRollWith (Just 15)` -- a fixture -- and nothing went through
`loadConf`. Now the default is `null` (as `line_budget`'s), and `SelfTest` goes through `loadConf` (3 checks failed
before, pass after). README row rewritten.

### 2. `gc` reaped a session holding 30 compiled modules of a first -O2 build  (MEDIUM, sure; a rule of the user's) -- b83ef2165

`Gc.deadSessions` removed any session with no daemon, status `loaded=-`, none of `history`/`loaded_sources.tsv`/
`turn.json`/`usage.jsonl`/`chat.pid`, idle over 600 s. `gc -n` said "would reap session tool-O2 (17.4 MB)": the `-O2`
sibling whose boot timed out at 900 s -- with 30 `.o` files, fifteen minutes of compile the next `bench opt:2` resumes.
The rule recorded for this project: gc never removes tool-O2. The README and `check-cli.py` ("ghost-O2" removed)
enshrined the opposite. Now a session that holds a compiled module (`.o`/`.hi` under `objs`) is kept; `gc -n` on this
repository says "no orphaned ..."; self-test over a state directory (failed with the old rule: it reaped `built-O2`),
and `check-cli.py` keeps a `built-O2` beside the removed `ghost-O2`.

### 3. `usage --quota` could never reach a trusted fit on the real ledger  (MEDIUM, sure; leftover "item 3") -- 1ec21d352

The code that prints "the write/read ratio this implies" was there (`QuotaFit.hs`, `quotaLines`) -- and unreachable.
The plan's use comes in steps of 0.01 and a call raises it by about a tenth of a step; `pairsOf` paired neighbouring
calls, so every rise was 0 or one step of noise. Today's ledger (30 calls with windows): R squared 0.19 / 0.00, "NOT to
be trusted", and the untrusted fit still said `1% of the window is about 4 uncached input tokens`. The self-test used
a continuous use (`scanl`), the fixture again. Now: a span is as many calls as make the use rise 20 steps on the
average (`spanCalls`; **not** cut where each span's rise reaches a number: tried first, every rise comes out equal
and R squared is meaningless); a window's size is said only for a trusted fit; a use with no steps is fitted by
neighbours as before. Self-test on a use rounded to a hundredth: neighbours give no trusted fit (R squared 0.07), spans do
(R squared 0.83, ratio 20.2 from weights made up for 20).
**What it means on the real data**: five_hour needs 268 calls a span, seven_day 2,720; 30 spans (the minimum) are
some 8,000 calls with the windows in the ledger for the first, never in practice for the second. So the ratio will
be printed after about a week of this chat's use, and the 7-day window not at all. A decision for the user: whether
the 7-day fit is worth showing, or the trust threshold should be lower for the 5-hour one.

### 4. `check-tui.py chat` lost 20 s three times, about one run in ten  (MEDIUM for the check, sure; leftover) -- d5b96e699

`until 10 turns;`, `until 11 turns;`, `until 12 turns;` after `spawn`. Reproduced: 2 runs of 22 printed the three
`tui-capture: until '10 turns;': not in 20 s` ...; the `-v` run caught the screen: header `9 turns` at the mark where `10`
was awaited, `[Sub-1] report` printed BEFORE the tool's echo -- the first subagent's report arrived before the spawning
turn drained its queue (`drain pending` after the `spawn` call), was fed into that turn, and the count stayed one
less from then on. No check asserts a count, so nothing failed. The script now waits for the text the turn shows in
either order (`you said 'the first is done'`, the edit's diff, `talk:  the end`), then for rest: 0 of 24 runs warn.

### 5. Smaller, sure, fixed

* `Know.hs` comments (module header, `render`) said facts are listed most recently confirmed first; they are ordered
  by `factScore` since a6a434e33 -- e3e23f9c7.
* `chat --replay-rollover` read a log it could not open as `[]` and said "0 calls: too few to replay", exit 0. Now it says
  the file cannot be read and exits 1 -- 1e318338d (self-test).
* `Carry.resumeLog`'s tail budget is said in bytes and counted in characters; a tail of non-ASCII messages (the
  read's bar is three bytes a line) held up to four times its budget -- 88ee058a6 (self-test).
* A self-test named "a faster growth is sooner" asserted the opposite (the threshold `S + sqrt(2 R g)` is later in
  tokens for a faster growth) -- 2f1508993.
* The chat's inbox (`Inbox.hs`, today's) read a message with the locale's encoding and REMOVED the file before looking at
  whether it had been read: a message that is not valid UTF-8 (shown: `hGetContents: invalid argument (cannot decode
  byte sequence starting from 255)`) was dropped without a word, and under an ASCII locale (shown) a non-ASCII message
  could not even be written. A message is UTF-8 bytes now, a bad byte the replacement character, and a file that cannot be
  read is set aside as `.unread-NAME`; `drainInbox` is the pass, self-tested -- 617e54fa5.

## Left, with why

### 6. The save's early answer trusts any client's typecheck  (LOW-MEDIUM, likely, unproven)

`Chat.hs:377` `typecheckEarly` answers "the reload is under way ... an eval or test now waits for it" once
`typecheck_cached` is clean. `typecheckCached` (`Daemon.hs:1518`) says yes whenever the last typecheck of ANYONE matches
the sources on disk -- including a plain `typecheck` of a second client that ran between the save and the moment the
watcher (poll 0.2 s + debounce 0.2 s) took the work lock; in that window the agent's next `eval` can win the lock and run
on the old code (the reply carries `stale`, which limits the harm). `check-edit.py -n 30` with a second client
typechecking all the while (6,360 calls) passed 30 + 30 rounds: the window was not made to open. The fix is a decision: the daemon would have to say whether the *watcher's* typecheck made the
cache (a field beside `vTcLast`).

### 7. The forked write of a fresh call's first message is not waited for  (LOW, likely)

`Chat.hs:1655` writes it from a `forkIO` (the 64 KB hang fix) and `Chat.hs:1971` closes the fd when the run ends. A run
that ends while the write still waits (`threadWaitWrite`) leaves a thread blocked on a closed descriptor (a leak of the
message), and, if the wait wakes, writes to a number that may be re-used. Needs a child that stops reading its stdin to
show; the fix keeps the thread in the run and kills it before the close.

### 8. The cached subjects block and its sidecars are written in place  (LOW, likely)

`Daemon.hs:2216`: `subjects.txt`, `subjects-view.json`, `subjects-at` written with `writeFileUtf8` (truncate, write) one
after the other while `view` requests are served concurrently (the chat and `top`). A reader between truncation and the end
of the write finds a non-empty, cut block and serves it as the cached one until the view is next rewritten; a crash does
the same for good. `Sys.writeAtomic` exists. Same for `know.json` (`Daemon.hs:2343`): a torn file restarts the
extraction from message 0 (a model call a piece). A race: no deterministic test.

### 9. Smaller

* `Replay.hs:81`: `go` carries a third argument (`0`, "the run's first context (not kept...)") nothing reads: a leftover.
* `Cli.hs:132`: the plan mode of `import` (no `--go`) writes `imported-files.json` while it says "nothing was written".
  A cache, harmless.
* `ImportStamp.hs:41`, `Roll.hs:223`, `Chat.hs:792` (`turnNote`), `Cli.hs` `doneFile`: a fixed `.new`/`.tmp` name. Two writers
  at once (two Claude Code hooks ending turns together) can rename a half file or make the second `renameFile` fail; in
  `cmdImportUp` that is an uncaught exception, so the hook exits non-zero.
* `Chat.hs:780` `turnBegan` is not atomic and not under `turnLock`; a `turnNote` that read the file just before it can write
  the old turn's state over the new turn's start. Waiters tolerate a half file; not this.
* `Chat.hs:821` `turnHook`: on the 60 s timeout only the shell is terminated, not what it started.
* `Know.hs:158` `withLock`: a lock older than 120 s is "taken" by `release >> take' 0`; two waiters that both saw it old
  can each remove the other's fresh lock.
* `Inbox.running` trusts `chat.pid`: a recycled pid keeps `chat --wait` (no SECS) waiting.
* Other file I/O of the project still goes by the locale (`Chat.carryReport`'s `GHS_CARRY_DUMP` file, `Config`'s
  `readFile`): the same failure under a C locale, in places that matter less.
* `Mcp.hs` `toolBrief` cuts a description at the first ". ": "e.g. ..." ends it. Only for a prompt.

### 10. Matters of taste or for the user

* The fit of #3 is for one machine's own ledger: the plan's windows are shared with Claude Code itself and other
  machines, which show in no ledger here; it can stay untrusted for good.
* A `known` line before the block's id is hidden from the view (item 5), but a fact the block's 16 KB budget cut is then in
  neither: it is only in the store ("more: recall"). By design; a decision whether hiding should wait for the fact to be in
  the block.
* `SelfRound.hs` is named for a round, not a subject; its checks (know, roll, replay, gc, quota) belong with their modules'.
* `gc` is now more careful (#2) at the cost of keeping a failed sibling's few MB; a session left by a boot that died after
  compiling something is never reaped by `gc` -- by hand.

## Hot path

Nothing of today's is slow where it matters. The costs I found, all small: `saveRoll` (tmp+rename) at every model call's
start; `turnNote` (read, parse, write, rename) at every talk and tool message; `typecheckEarly` polls `typecheck_cached`
every 50 ms for up to 10 s per save when nothing answers sooner (one call is 1.6 ms measured: a scan of the watched sources
and the RPC); `subjectsFor` reads three files a `view` where it read two. `History.messages` is faster (one read per run
of messages), `isoSeconds` agrees with `parseTimeM` on 24,616 dates 1899-2120 (checked), the number fast path was already
checked against `read` on 300k.

## Not reviewed well, and why

* `Chat.hs` outside the diff (the `turn`/`goOn` machinery, the tui): I read the hunks with the code around them, not the
  file. Races between the chat's threads (subagents, the stream reader, the inbox watcher, `turnNote`) are judged from the
  hunks, not traced.
* `Daemon.hs` watcher/reload locking: #6 rests on reading `typecheckCached` and the README's claim, not on a trace.
* `PERF.md` (292 lines) was not re-measured. `tools/check-edit.py`, `check-knowledge.py` were read, not changed.
* The real `claude` stream: the windows and the write kinds are read from one day of ledger lines (30 with windows), not
  from documentation; `windowsOf` has fallbacks for shapes I have not seen.
