/* The executable's entry point, in C so the Haskell runtime can be configured PER COMMAND.
 *
 * The client and the daemon are one binary, and they want opposite runtimes. A client command lives for a few
 * milliseconds: with the default runtime 15 of its 21 ms were the runtime starting and stopping -- the interval
 * timer (-V0 turns it off: the exit no longer waits out a tick) and the reservation of a terabyte of address
 * space (-xr1g). The daemon lives for hours, runs several threads and wants the timer, two capabilities and
 * GHC.Stats. `-with-rtsopts` can only say one thing; this says the right one for each.
 *
 *   ghci-session --help            21.7 ms -> 7.8 ms
 */
#include <string.h>
#include "Rts.h"

extern int ghsMain(void);
extern int ghs_fast_client(int argc, char **argv);   /* ghs_fast.c: `eval` without the runtime */

int main(int argc, char **argv) {
  int fast = ghs_fast_client(argc, argv);
  if (fast >= 0) return fast;
  int daemon = 0, stats = 0;
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "_daemon")) daemon = 1;
    if (!strcmp(argv[i], "selfbench") || !strcmp(argv[i], "selftest")) stats = 1;
    if (!strcmp(argv[i], "chat")) stats = 1;     /* long-lived, threaded (a reader on stdin, the model call): the timer, not the client's -V0 */
  }
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  /* (-xr is GHC 9.10's: an older runtime refuses to start on an option it does not know) */
#if __GLASGOW_HASKELL__ >= 910
  conf.rts_opts = daemon ? "-N2 -A2m -T" : stats ? "-N2 -A2m -T" : "-V0 -xr1g -A1m";
#else
  conf.rts_opts = daemon ? "-N2 -A2m -T" : stats ? "-N2 -A2m -T" : "-V0 -A1m";
#endif
  hs_init_ghc(&argc, &argv, conf);
  int rc = ghsMain();
  hs_exit();
  return rc;
}
