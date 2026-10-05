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

int main(int argc, char **argv) {
  int daemon = 0, stats = 0;
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "_daemon")) daemon = 1;
    if (!strcmp(argv[i], "selfbench") || !strcmp(argv[i], "selftest")) stats = 1;
  }
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  conf.rts_opts = daemon ? "-N2 -A2m -T" : stats ? "-N2 -A2m -T" : "-V0 -xr1g -A1m";
  hs_init_ghc(&argc, &argv, conf);
  int rc = ghsMain();
  hs_exit();
  return rc;
}
