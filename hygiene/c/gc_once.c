/* One major collection, compacting or copying, whatever the RTS flags say -- and only this one.
 *
 * A session runs with -c (the compacting old generation: a third less memory), and compaction is
 * single-threaded and slow: the collection after every reload's unlink was 0.42 s of a 250 MB heap,
 * against 0.06 s for a copying one using every capability. The RTS decides how the old generation is
 * collected at the END of the previous collection (oldest_gen->mark / ->compact), so setting the flag
 * alone takes effect a collection late: the generation's own fields are set here, for the collection
 * that follows at once, and the RTS decides again for the next from its flags. Returns whether it was
 * going to compact. */
/* The generation's layout depends on the RTS's way: the engine links the threaded RTS, and a file compiled
 * without THREADED_RTS would write these two fields at the wrong offsets (it did: the first version of this
 * changed nothing and scribbled on its neighbours). So it is defined here, and the layout is CHECKED against
 * what must hold of the oldest generation before anything is written; if not, an ordinary collection. */
#define THREADED_RTS 1
#include "Rts.h"
int ghs_major_gc(int compact) {
  if (oldest_gen == NULL || oldest_gen->no != RtsFlags.GcFlags.generations - 1
      || (oldest_gen->mark != 0 && oldest_gen->mark != 1) || (oldest_gen->compact != 0 && oldest_gen->compact != 1)
      || oldest_gen->mark != oldest_gen->compact) { performMajorGC(); return -1; }
  int was = oldest_gen->compact;
  oldest_gen->mark = compact ? 1 : 0;
  oldest_gen->compact = compact ? 1 : 0;
  performMajorGC();
  return was;
}

/* How fast the RTS hands unused memory back to the system after a major collection (-Fd): 0 at once, the
 * default 4 over several collections. Returns what it was. */
double ghs_return_decay(double f) {
  double was = RtsFlags.GcFlags.returnDecayFactor;
  if (f >= 0) RtsFlags.GcFlags.returnDecayFactor = f;
  return was;
}

/* GHC's own executable is built with -H: after each major collection the RTS takes the largest heap it
 * has needed as its "suggested" size and spends the difference on allocation area -- fewer collections,
 * for memory that is never live (a 158 MB session held 375). 0 turns that off and clears the suggestion
 * (the allocation area is then -A per capability); 1 turns it back on. Returns what it was. */
int ghs_heap_auto(int on) {
  int was = RtsFlags.GcFlags.heapSizeSuggestionAuto ? 1 : 0;
  if (on >= 0) {
    RtsFlags.GcFlags.heapSizeSuggestionAuto = on ? 1 : 0;
    if (!on) RtsFlags.GcFlags.heapSizeSuggestion = 0;
  }
  return was;
}
