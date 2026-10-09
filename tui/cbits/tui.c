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

/* The pixels of a cell, where the terminal says its size in them. */
int tui_cell_pixels(int *w, int *h) {
  struct winsize ws;
  if (ioctl(1, TIOCGWINSZ, &ws) != 0 || ws.ws_row == 0 || ws.ws_col == 0 || ws.ws_xpixel == 0 || ws.ws_ypixel == 0) return -1;
  *w = ws.ws_xpixel / ws.ws_col; *h = ws.ws_ypixel / ws.ws_row;
  return (*w > 0 && *h > 0) ? 0 : -1;
}

int tui_sigwinch(void) { return SIGWINCH; }
