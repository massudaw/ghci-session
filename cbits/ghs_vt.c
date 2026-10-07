/* Terminal panes for `ghci-session top`, through libghostty-vt -- Ghostty's terminal emulation as a C library --
 * found at run time with dlopen (as libcurl is for the model client, ghs_http.c), so the package builds and runs
 * without it and the panes say what is missing. The headers are vendored (vendor/ghostty-vt/include, MIT) for
 * the prototypes and the sized structs; the library's ABI is versioned by those structs' `size` fields.
 *
 * A pane is a pseudo-terminal running a program (the chat, a shell) whose output is fed to a GhosttyTerminal,
 * and a render state read back as rows of cells: `ghs_vt_render` writes them as text with true-color SGR runs,
 * one line a row, which the monitor places in its own frame. The real terminal never sees the program's own
 * escape sequences; libghostty-vt interprets them, scrollback, wrapping, the alternate screen and all. */
#define _XOPEN_SOURCE 700
#define _DEFAULT_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

#include <ghostty/vt.h>

static void *lib;
static char err[512];

#define SYMS(X) \
  X(ghostty_terminal_new) X(ghostty_terminal_free) X(ghostty_terminal_vt_write) X(ghostty_terminal_resize) X(ghostty_terminal_set) \
  X(ghostty_render_state_new) X(ghostty_render_state_free) X(ghostty_render_state_update) X(ghostty_render_state_get) \
  X(ghostty_render_state_row_iterator_new) X(ghostty_render_state_row_iterator_free) X(ghostty_render_state_row_iterator_next) \
  X(ghostty_render_state_row_get) X(ghostty_render_state_row_cells_new) X(ghostty_render_state_row_cells_free) \
  X(ghostty_render_state_row_cells_next) X(ghostty_render_state_row_cells_get) X(ghostty_cell_get)
#define DECL(n) static __typeof__(n) *p_##n;
SYMS(DECL)

const char *ghs_vt_error(void) { return err; }

/* Load the library: `path` first (may be ""), then the names the system linker knows. 0, or -1 with the reason. */
int ghs_vt_open(const char *path) {
  if (lib) return 0;
  const char *names[] = { path, "libghostty-vt.so.0", "libghostty-vt.so", "libghostty-vt.0.dylib", "libghostty-vt.dylib", NULL };
  for (int i = 0; names[i]; i++) {
    if (!names[i][0]) continue;
    lib = dlopen(names[i], RTLD_NOW | RTLD_LOCAL);
    if (lib) break;
  }
  if (!lib) {
    snprintf(err, sizeof err, "libghostty-vt not found (%s): put libghostty-vt.so beside ghci-session or name it in GHS_LIBGHOSTTY", path && path[0] ? path : "no path given");
    return -1;
  }
#define LOAD(n) p_##n = (__typeof__(n) *)dlsym(lib, #n); if (!p_##n) { snprintf(err, sizeof err, "libghostty-vt lacks %s: an older build?", #n); dlclose(lib); lib = NULL; return -1; }
  SYMS(LOAD)
  return 0;
}

typedef struct {
  GhosttyTerminal t;
  GhosttyRenderState rs;
  GhosttyRenderStateRowIterator rows;
  GhosttyRenderStateRowCells cells;
  int fd;
} GhsVt;

/* What the terminal answers to the program (a device attributes query, a cursor position report) goes back
 * down the pty. */
static void write_pty(GhosttyTerminal t, void *ud, const uint8_t *data, size_t len) {
  (void)t;
  GhsVt *v = ud;
  if (v->fd < 0) return;
  while (len > 0) {
    ssize_t n = write(v->fd, data, len);
    if (n < 0) { if (errno == EINTR) continue; return; }
    data += n; len -= (size_t)n;
  }
}

void *ghs_vt_new(int cols, int rows, int fd) {
  if (!lib) return NULL;
  GhsVt *v = calloc(1, sizeof *v);
  if (!v) return NULL;
  v->fd = fd;
  if (p_ghostty_terminal_new(NULL, &v->t, (uint16_t)cols, (uint16_t)rows) != GHOSTTY_SUCCESS) { free(v); return NULL; }
  p_ghostty_terminal_set(v->t, GHOSTTY_TERMINAL_OPT_USERDATA, v);
  GhosttyTerminalWritePtyFn wp = write_pty;
  p_ghostty_terminal_set(v->t, GHOSTTY_TERMINAL_OPT_WRITE_PTY, &wp);
  if (p_ghostty_render_state_new(NULL, &v->rs) != GHOSTTY_SUCCESS
      || p_ghostty_render_state_row_iterator_new(NULL, &v->rows) != GHOSTTY_SUCCESS
      || p_ghostty_render_state_row_cells_new(NULL, &v->cells) != GHOSTTY_SUCCESS) {
    p_ghostty_terminal_free(v->t); free(v); return NULL;
  }
  return v;
}

void ghs_vt_free(void *vp) {
  GhsVt *v = vp;
  if (!v) return;
  p_ghostty_render_state_row_cells_free(v->cells);
  p_ghostty_render_state_row_iterator_free(v->rows);
  p_ghostty_render_state_free(v->rs);
  p_ghostty_terminal_free(v->t);
  free(v);
}

void ghs_vt_write(void *vp, const uint8_t *buf, size_t n) { GhsVt *v = vp; if (v) p_ghostty_terminal_vt_write(v->t, buf, n); }

int ghs_vt_resize(void *vp, int cols, int rows) {
  GhsVt *v = vp;
  if (!v) return -1;
  return p_ghostty_terminal_resize(v->t, (uint16_t)cols, (uint16_t)rows, 8, 16) == GHOSTTY_SUCCESS ? 0 : -1;
}

typedef struct { char *p; size_t n, cap; int over; } Out;
static void put(Out *o, const char *s, size_t k) { if (o->n + k > o->cap) { o->over = 1; return; } memcpy(o->p + o->n, s, k); o->n += k; }
static void puts_(Out *o, const char *s) { put(o, s, strlen(s)); }

static GhosttyColorRgb resolve(GhosttyStyleColor c, const GhosttyRenderStateColors *cs, GhosttyColorRgb dflt, int *is_default) {
  switch (c.tag) {
    case GHOSTTY_STYLE_COLOR_RGB: *is_default = 0; return c.value.rgb;
    case GHOSTTY_STYLE_COLOR_PALETTE: *is_default = 0; return cs->palette[c.value.palette];
    default: *is_default = 1; return dflt;
  }
}

/* The viewport as text: `rows` lines separated by '\n', each of `cols` cells with true-color SGR runs (the
 * terminal's defaults as 39/49, so the pane takes the real terminal's colors) and a reset at the end; the
 * cursor's place and visibility. The bytes written, or -1 when `cap` is too small (ask again with more). */
int ghs_vt_render(void *vp, char *out, size_t cap, int *cx, int *cy, int *cvis) {
  GhsVt *v = vp;
  if (!v) return -1;
  Out o = { out, 0, cap, 0 };
  if (p_ghostty_render_state_update(v->rs, v->t) != GHOSTTY_SUCCESS) return -1;
  GhosttyRenderStateColors colors = GHOSTTY_INIT_SIZED(GhosttyRenderStateColors);
  p_ghostty_render_state_get(v->rs, GHOSTTY_RENDER_STATE_DATA_COLORS, &colors);
  GhosttyRenderStateCursor cur = GHOSTTY_INIT_SIZED(GhosttyRenderStateCursor);
  p_ghostty_render_state_get(v->rs, GHOSTTY_RENDER_STATE_DATA_CURSOR, &cur);
  *cx = cur.viewport_x; *cy = cur.viewport_y; *cvis = cur.visible && cur.viewport_has_value;
  if (p_ghostty_render_state_get(v->rs, GHOSTTY_RENDER_STATE_DATA_ROW_ITERATOR, &v->rows) != GHOSTTY_SUCCESS) return -1;
  int first = 1;
  while (p_ghostty_render_state_row_iterator_next(v->rows)) {
    if (!first) put(&o, "\n", 1);
    first = 0;
    if (p_ghostty_render_state_row_get(v->rows, GHOSTTY_RENDER_STATE_ROW_DATA_CELLS, &v->cells) != GHOSTTY_SUCCESS) continue;
    char last[128] = "";
    while (p_ghostty_render_state_row_cells_next(v->cells)) {
      GhosttyCell raw = 0;
      GhosttyCellWide wide = GHOSTTY_CELL_WIDE_NARROW;
      if (p_ghostty_render_state_row_cells_get(v->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_RAW, &raw) == GHOSTTY_SUCCESS)
        p_ghostty_cell_get(raw, GHOSTTY_CELL_DATA_WIDE, &wide);
      if (wide == GHOSTTY_CELL_WIDE_SPACER_TAIL || wide == GHOSTTY_CELL_WIDE_SPACER_HEAD) continue;   /* the wide character before it took this column */
      GhosttyStyle st = GHOSTTY_INIT_SIZED(GhosttyStyle);
      p_ghostty_render_state_row_cells_get(v->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_STYLE, &st);
      int fgd, bgd;
      GhosttyColorRgb fg = resolve(st.fg_color, &colors, colors.foreground, &fgd);
      GhosttyColorRgb bg = resolve(st.bg_color, &colors, colors.background, &bgd);
      if (st.inverse) { GhosttyColorRgb t = fg; fg = bg; bg = t; int d = fgd; fgd = bgd; bgd = d; }
      char sgr[128];
      int k = snprintf(sgr, sizeof sgr, "\033[0%s%s%s%s%s", st.bold ? ";1" : "", st.faint ? ";2" : "", st.italic ? ";3" : "", st.underline ? ";4" : "", st.strikethrough ? ";9" : "");
      if (fgd && !st.inverse) k += snprintf(sgr + k, sizeof sgr - k, ";39"); else k += snprintf(sgr + k, sizeof sgr - k, ";38;2;%u;%u;%u", fg.r, fg.g, fg.b);
      if (bgd && !st.inverse) k += snprintf(sgr + k, sizeof sgr - k, ";49"); else k += snprintf(sgr + k, sizeof sgr - k, ";48;2;%u;%u;%u", bg.r, bg.g, bg.b);
      snprintf(sgr + k, sizeof sgr - k, "m");
      if (strcmp(sgr, last)) { puts_(&o, sgr); strcpy(last, sgr); }
      uint32_t glen = 0;
      p_ghostty_render_state_row_cells_get(v->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_GRAPHEMES_LEN, &glen);
      if (glen == 0 || st.invisible) { put(&o, " ", 1); continue; }
      uint8_t ub[64];
      GhosttyBuffer b = { ub, sizeof ub, 0 };
      if (p_ghostty_render_state_row_cells_get(v->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_GRAPHEMES_UTF8, &b) == GHOSTTY_SUCCESS && b.len > 0)
        put(&o, (const char *)ub, b.len);
      else put(&o, " ", 1);
    }
    puts_(&o, "\033[0m");
  }
  if (o.over) return -1;
  return (int)o.n;
}

/* ---- the pseudo-terminal ---------------------------------------------------------------------------- */

/* Run `cmd` under /bin/sh in `cwd` on a new pseudo-terminal of cols x rows: the master descriptor, and the
 * child's pid; -1 when it cannot. The child gets TERM=xterm-256color (what libghostty-vt emulates well enough
 * for every program) and COLORTERM=truecolor. */
int ghs_pty_spawn(const char *cmd, const char *cwd, int cols, int rows, int *pid_out) {
  int m = posix_openpt(O_RDWR | O_NOCTTY);
  if (m < 0) return -1;
  if (grantpt(m) < 0 || unlockpt(m) < 0) { close(m); return -1; }
  const char *sn = ptsname(m);
  if (!sn) { close(m); return -1; }
  char slave[256];
  snprintf(slave, sizeof slave, "%s", sn);
  struct winsize ws; memset(&ws, 0, sizeof ws); ws.ws_row = (unsigned short)rows; ws.ws_col = (unsigned short)cols;
  ioctl(m, TIOCSWINSZ, &ws);
  pid_t pid = fork();
  if (pid < 0) { close(m); return -1; }
  if (pid == 0) {
    setsid();
    int s = open(slave, O_RDWR);
    if (s < 0) _exit(127);
    ioctl(s, TIOCSCTTY, 0);
    dup2(s, 0); dup2(s, 1); dup2(s, 2);
    if (s > 2) close(s);
    close(m);
    if (cwd && cwd[0]) { if (chdir(cwd) != 0) { /* then where we are */ } }
    setenv("TERM", "xterm-256color", 1);
    setenv("COLORTERM", "truecolor", 1);
    signal(SIGINT, SIG_DFL); signal(SIGQUIT, SIG_DFL); signal(SIGTSTP, SIG_DFL); signal(SIGPIPE, SIG_DFL);
    sigset_t none; sigemptyset(&none); sigprocmask(SIG_SETMASK, &none, NULL);
    execl("/bin/sh", "sh", "-c", cmd, (char *)NULL);
    _exit(127);
  }
  fcntl(m, F_SETFD, FD_CLOEXEC);
  *pid_out = (int)pid;
  return m;
}

int ghs_pty_resize(int fd, int cols, int rows) {
  struct winsize ws; memset(&ws, 0, sizeof ws); ws.ws_row = (unsigned short)rows; ws.ws_col = (unsigned short)cols;
  return ioctl(fd, TIOCSWINSZ, &ws);
}

/* Has the child ended? -1 while it runs; else its exit status (128 + the signal when killed by one). */
int ghs_pty_wait(int pid) {
  int st = 0;
  pid_t r = waitpid((pid_t)pid, &st, WNOHANG);
  if (r <= 0) return -1;
  if (WIFEXITED(st)) return WEXITSTATUS(st);
  if (WIFSIGNALED(st)) return 128 + WTERMSIG(st);
  return -1;
}

int ghs_sigwinch(void) { return SIGWINCH; }
