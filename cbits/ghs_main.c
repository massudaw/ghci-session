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
#include <signal.h>
#include <termios.h>
#include <unistd.h>
#include "Rts.h"

extern int ghsMain(void);
extern int ghs_fast_client(int argc, char **argv);   /* ghs_fast.c: `eval` without the runtime */

static struct termios orig_termios;
static int orig_termios_saved = 0;

/* Put the terminal back as it was found -- but only when it was left changed (a screen that died in raw
 * mode): every command passes here on its way out, and one whose terminal is as it was writes nothing. The
 * escapes (attributes off, cursor shown, the main screen) go to standard output only when that is the
 * terminal too: `ghci-session status > file` from a terminal got them in the file, with the bytes after the
 * string's end (its length was given as 20; it is 18). */
static void restore_terminal(void) {
  static const char reset[] = "\033[0m\033[?25h\033[?1049l";
  struct termios now;
  if (!orig_termios_saved) return;
  if (tcgetattr(0, &now) != 0) return;
  if (now.c_lflag == orig_termios.c_lflag && now.c_iflag == orig_termios.c_iflag && now.c_oflag == orig_termios.c_oflag) return;
  if (isatty(1)) (void)write(1, reset, sizeof(reset) - 1);
  (void)tcsetattr(0, TCSANOW, &orig_termios);
}

static void term_signal_handler(int sig) {
  restore_terminal();
  signal(sig, SIG_DFL);
  raise(sig);
}

int main(int argc, char **argv) {
  if (isatty(0)) {
    if (tcgetattr(0, &orig_termios) == 0) {
      orig_termios_saved = 1;
      atexit(restore_terminal);
      struct sigaction sa;
      memset(&sa, 0, sizeof(sa));
      sa.sa_handler = term_signal_handler;
      sigaction(SIGTERM, &sa, NULL);
      sigaction(SIGHUP, &sa, NULL);
      sigaction(SIGQUIT, &sa, NULL);
    }
  }

  int fast = ghs_fast_client(argc, argv);
  if (fast >= 0) return fast;
  int daemon = 0, stats = 0;
  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "_daemon")) daemon = 1;
    if (!strcmp(argv[i], "selfbench") || !strcmp(argv[i], "selftest")) stats = 1;
    if (!strcmp(argv[i], "chat") || !strcmp(argv[i], "top")) stats = 1;     /* long-lived, threaded (a reader on stdin, the model call, a screen): the timer, not the client's -V0 */
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
