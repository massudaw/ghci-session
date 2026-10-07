/* What the Haskell side cannot ask the OS for through the unix package. */
#include <signal.h>
#include <sys/ioctl.h>
#include <unistd.h>

/* The terminal's size (standard output): 0, or -1 when it is not a terminal. */
int tui_term_size(int *rows, int *cols) {
  struct winsize w;
  if (ioctl(1, TIOCGWINSZ, &w) != 0 || w.ws_row == 0) return -1;
  *rows = w.ws_row; *cols = w.ws_col;
  return 0;
}

int tui_sigwinch(void) { return SIGWINCH; }
