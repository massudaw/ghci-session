# Why a superseded CAF must not be unlinked while its value is young

`./run.sh` (about 10 s) passes; `./run.sh unsafe` kills GHCi every time (`exit code -10`, a bus error). The only
difference is one check in the pruner (`../c/ghci_cafs.c`, `value_is_old`), which `unsafe` turns off.

**The rule it shows.** When GHCi first enters a CAF, `newCAF` puts it on the RTS's retained list (`dyn_caf_list`)
*instead of* on the mutable list. So the only thing that keeps the CAF's freshly computed value alive through minor
collections is `markCAFs` walking that list at every GC. Take the CAF off the list while its value is still in a
young generation and the next minor GC frees the value -- while any live closure whose code refers to the CAF still
leads to it. Only a major GC follows those references (SRTs), and by then the pointer dangles.

**How a superseded CAF comes to have a young value.** Old code that is still run. A value kept across reloads (a
cache held by a `StablePtr`) is made of closures of the generation that built it; forcing it after a reload runs
that old code, which enters the old generation's CAFs for the first time. The script does exactly that:

1. `R.stash` keeps generation 1's `f` (whose code refers to the CAF `table`) by a StablePtr. `table` is not evaluated.
2. Edit, `:reload`, evaluate something: generation 2 is linked and generation 1 is wholly superseded.
3. `R.callStashed` calls the kept `f`: generation 1's `table` is entered now, so its value is in the nursery.
4. `unlinkCafs` (no GC). Without the check it unlinks that CAF.
5. Allocate, so minor collections run. Then call the kept `f` again.

```
guarded:  old f, first call: 500501 / unlinked: 2 / old f, after minor GCs: 500501 / major GC done / ... 500501
unsafe:   old f, first call: 500501 / unlinked: 3 / old f, after minor GCs: <GHCi dies>
```

**What hid it.** A major GC run *at once* after the unlink, before any minor one, promotes the value (the kept
closure still reaches the CAF), and from then on nothing is wrong. That is what the pruner always did. With the
check, a CAF whose value is not yet in the oldest generation simply stays on the list; a later pass takes it.

**What this does NOT explain.** This was found while chasing a crash on a 98-module session that appeared when the
GC after the unlink was deferred to an idle moment (`internal error: scavenge_mark_stack: unimplemented/strange
closure type` in the compacting collector, or a death in the next evaluation that read a kept value). The check
fixes this reproduction and does not fix that session: with the check in place it still dies with the GC deferred
(3 runs of 3) and still survives with the GC at once (4 iterations). So there is a second way for a deferred GC to
go wrong, not reproduced in the small, and the GC stays immediate (`prune_gc_idle_s: 0`).
