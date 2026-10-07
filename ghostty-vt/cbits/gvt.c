/* libghostty-vt, found at run time with dlopen, behind flat functions a Haskell binding can call: no structs
 * cross the boundary, every sized struct is filled here, and a terminal's write-back callback is a plain
 * function pointer with a context. gvt_load() first; everything answers an error until it has. */
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ghostty/vt.h>

static void *lib;
static char err[512];

#define SYMS(X) \
  X(ghostty_terminal_new) X(ghostty_terminal_free) X(ghostty_terminal_vt_write) X(ghostty_terminal_resize) X(ghostty_terminal_set) \
  X(ghostty_terminal_get) X(ghostty_terminal_scroll_viewport) \
  X(ghostty_render_state_new) X(ghostty_render_state_free) X(ghostty_render_state_update) X(ghostty_render_state_get) X(ghostty_render_state_clean) \
  X(ghostty_render_state_row_iterator_new) X(ghostty_render_state_row_iterator_free) X(ghostty_render_state_row_iterator_next) \
  X(ghostty_render_state_row_get) X(ghostty_render_state_row_cells_new) X(ghostty_render_state_row_cells_free) \
  X(ghostty_render_state_row_cells_next) X(ghostty_render_state_row_cells_get) X(ghostty_cell_get)
#define DECL(n) static __typeof__(n) *p_##n;
SYMS(DECL)

const char *gvt_error(void) { return err; }
int gvt_loaded(void) { return lib != NULL; }

/* `path` first (may be empty), then the names the system linker knows. 0, or -1 with gvt_error(). */
int gvt_load(const char *path) {
  if (lib) return 0;
  const char *names[] = { path, "libghostty-vt.so.0", "libghostty-vt.so", "libghostty-vt.0.dylib", "libghostty-vt.dylib", NULL };
  for (int i = 0; names[i]; i++) {
    if (!names[i][0]) continue;
    lib = dlopen(names[i], RTLD_NOW | RTLD_LOCAL);
    if (lib) break;
  }
  if (!lib) { snprintf(err, sizeof err, "libghostty-vt not found%s%s", path && path[0] ? ": tried " : "", path && path[0] ? path : ""); return -1; }
#define LOAD(n) p_##n = (__typeof__(n) *)dlsym(lib, #n); if (!p_##n) { snprintf(err, sizeof err, "libghostty-vt lacks %s (an older build?)", #n); dlclose(lib); lib = NULL; return -1; }
  SYMS(LOAD)
  return 0;
}

/* ---- the terminal ---------------------------------------------------------------------- */

typedef void (*gvt_write_fn)(void *ud, const uint8_t *data, size_t len);
typedef struct { GhosttyTerminal t; gvt_write_fn fn; void *ud; } Term;

static void write_pty(GhosttyTerminal t, void *ud, const uint8_t *data, size_t len) {
  (void)t;
  Term *tm = ud;
  if (tm->fn) tm->fn(tm->ud, data, len);
}

void *gvt_terminal_new(int cols, int rows) {
  if (!lib) return NULL;
  Term *tm = calloc(1, sizeof *tm);
  if (!tm) return NULL;
  if (p_ghostty_terminal_new(NULL, &tm->t, (uint16_t)cols, (uint16_t)rows) != GHOSTTY_SUCCESS) { free(tm); return NULL; }
  p_ghostty_terminal_set(tm->t, GHOSTTY_TERMINAL_OPT_USERDATA, tm);
  GhosttyTerminalWritePtyFn wp = write_pty;
  p_ghostty_terminal_set(tm->t, GHOSTTY_TERMINAL_OPT_WRITE_PTY, &wp);
  return tm;
}

void gvt_terminal_free(void *tp) { Term *tm = tp; if (!tm) return; p_ghostty_terminal_free(tm->t); free(tm); }
void gvt_terminal_write(void *tp, const uint8_t *buf, size_t n) { Term *tm = tp; if (tm) p_ghostty_terminal_vt_write(tm->t, buf, n); }
int gvt_terminal_resize(void *tp, int cols, int rows) { Term *tm = tp; return tm && p_ghostty_terminal_resize(tm->t, (uint16_t)cols, (uint16_t)rows, 8, 16) == GHOSTTY_SUCCESS ? 0 : -1; }
void gvt_terminal_on_write(void *tp, gvt_write_fn fn, void *ud) { Term *tm = tp; if (tm) { tm->fn = fn; tm->ud = ud; } }

/* 0 cols, 1 rows, 2 cursor x, 3 cursor y, 4 cursor visible, 5 on the alternate screen, 6 scrollback rows,
 * 7 total rows, 8 cursor pending wrap; -1 when it cannot be asked */
long gvt_terminal_int(void *tp, int what) {
  Term *tm = tp;
  if (!tm) return -1;
  GhosttyTerminalData keys[] = { GHOSTTY_TERMINAL_DATA_COLS, GHOSTTY_TERMINAL_DATA_ROWS, GHOSTTY_TERMINAL_DATA_CURSOR_X, GHOSTTY_TERMINAL_DATA_CURSOR_Y,
                                 GHOSTTY_TERMINAL_DATA_CURSOR_VISIBLE, GHOSTTY_TERMINAL_DATA_ACTIVE_SCREEN, GHOSTTY_TERMINAL_DATA_SCROLLBACK_ROWS,
                                 GHOSTTY_TERMINAL_DATA_TOTAL_ROWS, GHOSTTY_TERMINAL_DATA_CURSOR_PENDING_WRAP };
  if (what < 0 || what >= (int)(sizeof keys / sizeof *keys)) return -1;
  switch (what) {
    case 0: case 1: case 2: case 3: { uint16_t v = 0; if (p_ghostty_terminal_get(tm->t, keys[what], &v) != GHOSTTY_SUCCESS) return -1; return v; }
    case 4: case 8: { bool v = false; if (p_ghostty_terminal_get(tm->t, keys[what], &v) != GHOSTTY_SUCCESS) return -1; return v; }
    case 5: { GhosttyTerminalScreen v = 0; if (p_ghostty_terminal_get(tm->t, keys[what], &v) != GHOSTTY_SUCCESS) return -1; return v == GHOSTTY_TERMINAL_SCREEN_ALTERNATE; }
    default: { size_t v = 0; if (p_ghostty_terminal_get(tm->t, keys[what], &v) != GHOSTTY_SUCCESS) return -1; return (long)v; }
  }
}

/* 0 the title, 1 the working directory the program reported: the bytes copied into buf, their length, or -1 */
int gvt_terminal_string(void *tp, int what, char *buf, size_t cap) {
  Term *tm = tp;
  if (!tm) return -1;
  GhosttyString s = { NULL, 0 };
  if (p_ghostty_terminal_get(tm->t, what == 0 ? GHOSTTY_TERMINAL_DATA_TITLE : GHOSTTY_TERMINAL_DATA_PWD, &s) != GHOSTTY_SUCCESS) return -1;
  size_t n = s.len < cap ? s.len : cap;
  if (s.ptr && n) memcpy(buf, s.ptr, n);
  return (int)n;
}

/* 0 to the top, 1 to the bottom, 2 by delta rows, 3 to a row */
void gvt_terminal_scroll(void *tp, int tag, long value) {
  Term *tm = tp;
  if (!tm) return;
  GhosttyTerminalScrollViewport sv;
  memset(&sv, 0, sizeof sv);
  switch (tag) {
    case 0: sv.tag = GHOSTTY_SCROLL_VIEWPORT_TOP; break;
    case 1: sv.tag = GHOSTTY_SCROLL_VIEWPORT_BOTTOM; break;
    case 2: sv.tag = GHOSTTY_SCROLL_VIEWPORT_DELTA; sv.value.delta = (intptr_t)value; break;
    default: sv.tag = GHOSTTY_SCROLL_VIEWPORT_ROW; sv.value.row = (size_t)value; break;
  }
  p_ghostty_terminal_scroll_viewport(tm->t, sv);
}

/* ---- the screen, read through a render state ---------------------------------------------- */

typedef struct {
  GhosttyRenderState rs;
  GhosttyRenderStateRowIterator rows;
  GhosttyRenderStateRowCells cells;
  GhosttyRenderStateColors colors;
  int have_cells;
} Render;

void *gvt_render_new(void) {
  if (!lib) return NULL;
  Render *r = calloc(1, sizeof *r);
  if (!r) return NULL;
  if (p_ghostty_render_state_new(NULL, &r->rs) != GHOSTTY_SUCCESS
      || p_ghostty_render_state_row_iterator_new(NULL, &r->rows) != GHOSTTY_SUCCESS
      || p_ghostty_render_state_row_cells_new(NULL, &r->cells) != GHOSTTY_SUCCESS) { free(r); return NULL; }
  return r;
}

void gvt_render_free(void *rp) {
  Render *r = rp;
  if (!r) return;
  p_ghostty_render_state_row_cells_free(r->cells);
  p_ghostty_render_state_row_iterator_free(r->rows);
  p_ghostty_render_state_free(r->rs);
  free(r);
}

/* Take the terminal's state; -1 when it cannot. */
int gvt_render_update(void *rp, void *tp) {
  Render *r = rp; Term *tm = tp;
  if (!r || !tm) return -1;
  if (p_ghostty_render_state_update(r->rs, tm->t) != GHOSTTY_SUCCESS) return -1;
  r->colors = (GhosttyRenderStateColors)GHOSTTY_INIT_SIZED(GhosttyRenderStateColors);
  p_ghostty_render_state_get(r->rs, GHOSTTY_RENDER_STATE_DATA_COLORS, &r->colors);
  return 0;
}

int gvt_render_clean(void *rp) { Render *r = rp; return r && p_ghostty_render_state_clean(r->rs) == GHOSTTY_SUCCESS ? 0 : -1; }

/* 0 cols, 1 rows, 2 dirty (0 no, 1 some rows, 2 everything), 3 cursor x, 4 cursor y, 5 cursor visible,
 * 6 cursor style (0 bar, 1 block, 2 underline, 3 hollow block), 7 cursor in the viewport; -1 otherwise */
long gvt_render_int(void *rp, int what) {
  Render *r = rp;
  if (!r) return -1;
  switch (what) {
    case 0: case 1: { uint16_t v = 0; if (p_ghostty_render_state_get(r->rs, what == 0 ? GHOSTTY_RENDER_STATE_DATA_COLS : GHOSTTY_RENDER_STATE_DATA_ROWS, &v) != GHOSTTY_SUCCESS) return -1; return v; }
    case 2: { GhosttyRenderStateDirty d = 0; if (p_ghostty_render_state_get(r->rs, GHOSTTY_RENDER_STATE_DATA_DIRTY, &d) != GHOSTTY_SUCCESS) return -1; return d; }
    default: {
      GhosttyRenderStateCursor c = GHOSTTY_INIT_SIZED(GhosttyRenderStateCursor);
      if (p_ghostty_render_state_get(r->rs, GHOSTTY_RENDER_STATE_DATA_CURSOR, &c) != GHOSTTY_SUCCESS) return -1;
      switch (what) {
        case 3: return c.viewport_x;
        case 4: return c.viewport_y;
        case 5: return c.visible;
        case 6: return c.visual_style;
        case 7: return c.viewport_has_value;
        default: return -1;
      }
    }
  }
}

/* The default foreground and background: r g b, r g b. */
int gvt_render_colors(void *rp, uint8_t out[6]) {
  Render *r = rp;
  if (!r) return -1;
  out[0] = r->colors.foreground.r; out[1] = r->colors.foreground.g; out[2] = r->colors.foreground.b;
  out[3] = r->colors.background.r; out[4] = r->colors.background.g; out[5] = r->colors.background.b;
  return 0;
}

/* Start over the rows, top to bottom. */
int gvt_render_rows_begin(void *rp) {
  Render *r = rp;
  if (!r) return -1;
  r->have_cells = 0;
  return p_ghostty_render_state_get(r->rs, GHOSTTY_RENDER_STATE_DATA_ROW_ITERATOR, &r->rows) == GHOSTTY_SUCCESS ? 0 : -1;
}

/* The next row: its y and whether it changed since the last clean; 0 when there are no more. */
int gvt_render_row_next(void *rp, int *y, int *dirty) {
  Render *r = rp;
  if (!r || !p_ghostty_render_state_row_iterator_next(r->rows)) return 0;
  int32_t vy = 0; bool d = true;
  p_ghostty_render_state_row_get(r->rows, GHOSTTY_RENDER_STATE_ROW_DATA_VIEWPORT_Y, &vy);
  p_ghostty_render_state_row_get(r->rows, GHOSTTY_RENDER_STATE_ROW_DATA_DIRTY, &d);
  *y = vy; *dirty = d;
  r->have_cells = p_ghostty_render_state_row_get(r->rows, GHOSTTY_RENDER_STATE_ROW_DATA_CELLS, &r->cells) == GHOSTTY_SUCCESS;
  return 1;
}

static void resolve(GhosttyStyleColor c, const GhosttyRenderStateColors *cs, int *set, uint8_t out[3]) {
  GhosttyColorRgb v;
  switch (c.tag) {
    case GHOSTTY_STYLE_COLOR_RGB: v = c.value.rgb; *set = 1; break;
    case GHOSTTY_STYLE_COLOR_PALETTE: v = cs->palette[c.value.palette]; *set = 1; break;
    default: *set = 0; v.r = v.g = v.b = 0; break;
  }
  out[0] = v.r; out[1] = v.g; out[2] = v.b;
}

/* The next cell of the current row: -1 when there are no more, else the length of its text (UTF-8 into
 * `utf8`, 0 for an empty cell). `wide`: 0 narrow, 1 wide (two columns), 2 the spacer after a wide character
 * (nothing to draw), 3 the spacer at the end of a wrapped line. `flags`: 1 bold, 2 faint, 4 italic, 8
 * underline, 16 inverse, 32 strikethrough, 64 invisible, 128 blink. The colors as set (a palette entry
 * resolved) with `fgset`/`bgset` 0 when the terminal's default applies. */
int gvt_render_cell_next(void *rp, int *wide, int *flags, int *fgset, uint8_t fg[3], int *bgset, uint8_t bg[3], uint8_t *utf8, int cap) {
  Render *r = rp;
  if (!r || !r->have_cells || !p_ghostty_render_state_row_cells_next(r->cells)) return -1;
  GhosttyCell raw = 0;
  GhosttyCellWide w = GHOSTTY_CELL_WIDE_NARROW;
  if (p_ghostty_render_state_row_cells_get(r->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_RAW, &raw) == GHOSTTY_SUCCESS)
    p_ghostty_cell_get(raw, GHOSTTY_CELL_DATA_WIDE, &w);
  *wide = (int)w;
  GhosttyStyle st = GHOSTTY_INIT_SIZED(GhosttyStyle);
  p_ghostty_render_state_row_cells_get(r->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_STYLE, &st);
  *flags = (st.bold ? 1 : 0) | (st.faint ? 2 : 0) | (st.italic ? 4 : 0) | (st.underline ? 8 : 0) | (st.inverse ? 16 : 0)
         | (st.strikethrough ? 32 : 0) | (st.invisible ? 64 : 0) | (st.blink ? 128 : 0);
  resolve(st.fg_color, &r->colors, fgset, fg);
  resolve(st.bg_color, &r->colors, bgset, bg);
  if (w == GHOSTTY_CELL_WIDE_SPACER_TAIL || w == GHOSTTY_CELL_WIDE_SPACER_HEAD) return 0;
  uint32_t glen = 0;
  p_ghostty_render_state_row_cells_get(r->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_GRAPHEMES_LEN, &glen);
  if (glen == 0) return 0;
  GhosttyBuffer b = { utf8, (size_t)cap, 0 };
  if (p_ghostty_render_state_row_cells_get(r->cells, GHOSTTY_RENDER_STATE_ROW_CELLS_DATA_GRAPHEMES_UTF8, &b) != GHOSTTY_SUCCESS) return 0;
  return (int)b.len;
}
