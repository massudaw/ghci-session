/* vt-replay: what Ghostty's terminal would hold after the bytes a program wrote to it.
 *
 * The bytes (a recording: tools/tui-capture.py makes one) are fed to libghostty-vt -- Ghostty's own terminal,
 * as a library -- and at each offset asked for, a line of JSON says what it holds then: the screen's text, the
 * images it was sent by the kitty graphics protocol (stored, and decoded: a PNG it could not read is not
 * there), their placements, and the cells that are picture placeholders, by image, with the rows and columns
 * their marks say. So a screen that draws pictures is checked without a window: by the terminal that would
 * draw them, short of the drawing.
 *
 *   cc -o .bin/vt-replay tools/vt-replay.c -Ighostty-vt/include -L.bin -lghostty-vt -lz -Wl,-rpath,$PWD/.bin
 *   .bin/vt-replay RECORDING COLS ROWS [CELLW CELLH] [--at OFFSET]...      (no --at: at the end)
 *
 * tools/check-images.py builds and runs it.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>
#include <ghostty/vt.h>

/* ---- a PNG's pixels (the library leaves the decoding to its host) ---------------------------------------
 * What a screen is sent here: 8 or 16 bits a sample, any color type, not interlaced. */
static uint32_t be32(const uint8_t *p) { return (uint32_t)p[0] << 24 | (uint32_t)p[1] << 16 | (uint32_t)p[2] << 8 | p[3]; }

static bool decode_png(void *ud, const GhosttyAllocator *al, const uint8_t *d, size_t len, GhosttySysImage *out) {
  (void)ud;
  if (len < 33 || memcmp(d, "\x89PNG\r\n\x1a\n", 8) != 0) return false;
  uint32_t w = 0, h = 0; int depth = 0, ctype = 0, interlace = 0;
  uint8_t pal[256][4]; memset(pal, 255, sizeof pal);
  int trns_gray = -1, trns_rgb[3] = { -1, -1, -1 };
  uint8_t *idat = malloc(len), *raw = NULL; size_t nidat = 0; bool ok = false;
  if (!idat) return false;
  for (size_t o = 8; o + 12 <= len;) {
    uint32_t n = be32(d + o); const uint8_t *ty = d + o + 4, *b = d + o + 8;
    if (o + 12 + (size_t)n > len) break;
    if (!memcmp(ty, "IHDR", 4) && n >= 13) { w = be32(b); h = be32(b + 4); depth = b[8]; ctype = b[9]; interlace = b[12]; }
    else if (!memcmp(ty, "PLTE", 4)) { for (uint32_t i = 0; i < n / 3 && i < 256; i++) memcpy(pal[i], b + 3 * i, 3); }
    else if (!memcmp(ty, "tRNS", 4)) {
      if (ctype == 3) { for (uint32_t i = 0; i < n && i < 256; i++) pal[i][3] = b[i]; }
      else if (ctype == 0 && n >= 2) trns_gray = depth == 16 ? b[0] : b[1];
      else if (ctype == 2 && n >= 6) { for (int i = 0; i < 3; i++) trns_rgb[i] = depth == 16 ? b[2 * i] : b[2 * i + 1]; }
    }
    else if (!memcmp(ty, "IDAT", 4)) { memcpy(idat + nidat, b, n); nidat += n; }
    else if (!memcmp(ty, "IEND", 4)) break;
    o += 12 + (size_t)n;
  }
  int ch = ctype == 0 ? 1 : ctype == 2 ? 3 : ctype == 3 ? 1 : ctype == 4 ? 2 : ctype == 6 ? 4 : 0;
  if (!w || !h || !ch || interlace || (depth != 8 && depth != 16) || (ctype == 3 && depth != 8) || w > 16384 || h > 16384) goto done;
  {
    size_t bpp = (size_t)ch * (depth / 8), stride = (size_t)w * bpp, need = (stride + 1) * h;
    uLongf got = need;
    raw = malloc(need);
    if (!raw || uncompress(raw, &got, idat, nidat) != Z_OK || got != need) goto done;
    for (uint32_t y = 0; y < h; y++) {                   /* each line's filter undone, in place */
      uint8_t *row = raw + (stride + 1) * y + 1, *up = y ? row - stride - 1 : NULL; int f = row[-1];
      for (size_t x = 0; x < stride; x++) {
        int a = x >= bpp ? row[x - bpp] : 0, b = up ? up[x] : 0, c = up && x >= bpp ? up[x - bpp] : 0, v = row[x];
        if (f == 1) v += a;
        else if (f == 2) v += b;
        else if (f == 3) v += (a + b) / 2;
        else if (f == 4) { int p = a + b - c, pa = abs(p - a), pb = abs(p - b), pc = abs(p - c); v += pa <= pb && pa <= pc ? a : pb <= pc ? b : c; }
        row[x] = (uint8_t)v;
      }
    }
    size_t n = (size_t)w * h * 4, step = depth / 8;
    uint8_t *px = ghostty_alloc(al, n);
    if (!px) goto done;
    for (uint32_t y = 0; y < h; y++) for (uint32_t x = 0; x < w; x++) {
      const uint8_t *s = raw + (stride + 1) * y + 1 + (size_t)x * bpp; uint8_t *p = px + ((size_t)y * w + x) * 4;
      switch (ctype) {
        case 0: p[0] = p[1] = p[2] = s[0]; p[3] = s[0] == trns_gray ? 0 : 255; break;
        case 2: p[0] = s[0]; p[1] = s[step]; p[2] = s[2 * step]; p[3] = (p[0] == trns_rgb[0] && p[1] == trns_rgb[1] && p[2] == trns_rgb[2]) ? 0 : 255; break;
        case 3: memcpy(p, pal[s[0]], 4); break;
        case 4: p[0] = p[1] = p[2] = s[0]; p[3] = s[step]; break;
        default: p[0] = s[0]; p[1] = s[step]; p[2] = s[2 * step]; p[3] = s[3 * step]; break;
      }
    }
    out->width = w; out->height = h; out->data = px; out->data_len = n; ok = true;
  }
done:
  free(idat); free(raw);
  return ok;
}

/* ---- what the terminal holds ----------------------------------------------------------------------------- */

/* (the protocol's marks, its first 128: the n-th says n) */
static const uint32_t marks[] = { 0x0305,0x030D,0x030E,0x0310,0x0312,0x033D,0x033E,0x033F,0x0346,0x034A,0x034B,0x034C,0x0350,0x0351,0x0352,0x0357,0x035B,0x0363,0x0364,0x0365,0x0366,0x0367,0x0368,0x0369,0x036A,0x036B,0x036C,0x036D,0x036E,0x036F,0x0483,0x0484,0x0485,0x0486,0x0487,0x0592,0x0593,0x0594,0x0595,0x0597,0x0598,0x0599,0x059C,0x059D,0x059E,0x059F,0x05A0,0x05A1,0x05A8,0x05A9,0x05AB,0x05AC,0x05AF,0x05C4,0x0610,0x0611,0x0612,0x0613,0x0614,0x0615,0x0616,0x0617,0x0657,0x0658,0x0659,0x065A,0x065B,0x065D,0x065E,0x06D6,0x06D7,0x06D8,0x06D9,0x06DA,0x06DB,0x06DC,0x06DF,0x06E0,0x06E1,0x06E2,0x06E4,0x06E7,0x06E8,0x06EB,0x06EC,0x0730,0x0732,0x0733,0x0735,0x0736,0x073A,0x073D,0x073F,0x0740,0x0741,0x0743,0x0745,0x0747,0x0749,0x074A,0x07EB,0x07EC,0x07ED,0x07EE,0x07EF,0x07F0,0x07F1,0x07F3,0x0816,0x0817,0x0818,0x0819,0x081B,0x081C,0x081D,0x081E,0x081F,0x0820,0x0821,0x0822,0x0823,0x0825,0x0826,0x0827,0x0829,0x082A,0x082B,0x082C };

static int mark_of(uint32_t cp) { for (int m = 0; m < 128; m++) if (marks[m] == cp) return m; return -1; }

static void put_json_cp(uint32_t c) {
  if (c == '"' || c == '\\') printf("\\%c", (int)c);
  else if (c < 0x20) printf("\\u%04x", c);
  else if (c < 0x80) putchar((int)c);
  else if (c < 0x800) printf("%c%c", 0xC0 | (c >> 6), 0x80 | (c & 63));
  else if (c < 0x10000) printf("%c%c%c", 0xE0 | (c >> 12), 0x80 | ((c >> 6) & 63), 0x80 | (c & 63));
  else printf("%c%c%c%c", 0xF0 | (c >> 18), 0x80 | ((c >> 12) & 63), 0x80 | ((c >> 6) & 63), 0x80 | (c & 63));
}

static void report(GhosttyTerminal t, long at, int cols, int rows) {
  printf("{\"at\": %ld, \"images\": [", at);
  GhosttyKittyGraphics g = NULL;
  if (ghostty_terminal_get(t, GHOSTTY_TERMINAL_DATA_KITTY_GRAPHICS, &g) == GHOSTTY_SUCCESS && g) {
    GhosttyKittyGraphicsPlacementIterator it;
    if (ghostty_kitty_graphics_placement_iterator_new(NULL, &it) == GHOSTTY_SUCCESS) {
      ghostty_kitty_graphics_get(g, GHOSTTY_KITTY_GRAPHICS_DATA_PLACEMENT_ITERATOR, &it);
      for (int k = 0; ghostty_kitty_graphics_placement_next(it); k++) {
        uint32_t id = 0, c = 0, r = 0, w = 0, h = 0; bool virt = false;
        ghostty_kitty_graphics_placement_get(it, GHOSTTY_KITTY_GRAPHICS_PLACEMENT_DATA_IMAGE_ID, &id);
        ghostty_kitty_graphics_placement_get(it, GHOSTTY_KITTY_GRAPHICS_PLACEMENT_DATA_IS_VIRTUAL, &virt);
        ghostty_kitty_graphics_placement_get(it, GHOSTTY_KITTY_GRAPHICS_PLACEMENT_DATA_COLUMNS, &c);
        ghostty_kitty_graphics_placement_get(it, GHOSTTY_KITTY_GRAPHICS_PLACEMENT_DATA_ROWS, &r);
        GhosttyKittyGraphicsImage im = ghostty_kitty_graphics_image(g, id);
        if (im) { ghostty_kitty_graphics_image_get(im, GHOSTTY_KITTY_IMAGE_DATA_WIDTH, &w); ghostty_kitty_graphics_image_get(im, GHOSTTY_KITTY_IMAGE_DATA_HEIGHT, &h); }
        printf("%s{\"id\": %u, \"stored\": %s, \"width\": %u, \"height\": %u, \"virtual\": %s, \"columns\": %u, \"rows\": %u}",
               k ? ", " : "", id, im ? "true" : "false", w, h, virt ? "true" : "false", c, r);
      }
      ghostty_kitty_graphics_placement_iterator_free(it);
    }
  }
  printf("], \"placeholders\": [");
  /* by image (a placeholder's color is its image): how many cells, where, and what their marks say */
  static int cells[256], bad[256], top[256], bottom[256], left[256], right[256], r0[256], r1[256], c0[256], c1[256];
  memset(cells, 0, sizeof cells); memset(bad, 0, sizeof bad);
  uint32_t *text = calloc((size_t)cols * rows, sizeof *text);
  for (int y = 0; y < rows; y++) for (int x = 0; x < cols; x++) {
    GhosttyPoint p; memset(&p, 0, sizeof p); p.tag = GHOSTTY_POINT_TAG_ACTIVE; p.value.coordinate.x = (uint16_t)x; p.value.coordinate.y = (uint32_t)y;
    GhosttyGridRef ref; memset(&ref, 0, sizeof ref); ref.size = sizeof ref;
    uint32_t cps[8]; size_t k = 0;
    if (ghostty_terminal_grid_ref(t, p, &ref) != GHOSTTY_SUCCESS || ghostty_grid_ref_graphemes(&ref, cps, 8, &k) != GHOSTTY_SUCCESS || k == 0) continue;
    text[y * cols + x] = cps[0];
    if (cps[0] != 0x10EEEE) continue;
    GhosttyStyle st; memset(&st, 0, sizeof st); st.size = sizeof st;
    ghostty_grid_ref_style(&ref, &st);
    int id = st.fg_color.tag == GHOSTTY_STYLE_COLOR_PALETTE ? st.fg_color.value.palette : 0;
    int r = k > 1 ? mark_of(cps[1]) : -1, c = k > 2 ? mark_of(cps[2]) : -1;
    if (!cells[id]++) { top[id] = bottom[id] = y; left[id] = right[id] = x; r0[id] = r1[id] = r; c0[id] = c1[id] = c; }
    if (r < 0 || c < 0) { bad[id]++; continue; }
    if (y > bottom[id]) bottom[id] = y;
    if (x < left[id]) left[id] = x;
    if (x > right[id]) right[id] = x;
    if (r < r0[id]) r0[id] = r;
    if (r > r1[id]) r1[id] = r;
    if (c < c0[id]) c0[id] = c;
    if (c > c1[id]) c1[id] = c;
  }
  for (int i = 0, k = 0; i < 256; i++) if (cells[i])
    printf("%s{\"id\": %d, \"cells\": %d, \"left\": %d, \"right\": %d, \"top\": %d, \"bottom\": %d, \"mark_rows\": [%d, %d], \"mark_columns\": [%d, %d], \"unreadable\": %d}",
           k++ ? ", " : "", i, cells[i], left[i], right[i], top[i], bottom[i], r0[i], r1[i], c0[i], c1[i], bad[i]);
  printf("], \"screen\": [");
  for (int y = 0; y < rows; y++) {
    int end = cols;
    while (end > 0 && (text[y * cols + end - 1] == 0 || text[y * cols + end - 1] == ' ')) end--;
    printf("%s\"", y ? ", " : "");
    for (int x = 0; x < end; x++) { uint32_t c = text[y * cols + x]; put_json_cp(c == 0 ? ' ' : c == 0x10EEEE ? 0x2592 : c); }   /* (a placeholder: shown as a shade) */
    printf("\"");
  }
  printf("]}\n");
  free(text);
}

int main(int argc, char **argv) {
  if (argc < 4) { fprintf(stderr, "usage: vt-replay RECORDING COLS ROWS [CELLW CELLH] [--at OFFSET]...\n"); return 2; }
  int cols = atoi(argv[2]), rows = atoi(argv[3]), cw = 9, chh = 18, a = 4;
  if (argc > 5 && argv[4][0] != '-') { cw = atoi(argv[4]); chh = atoi(argv[5]); a = 6; }
  FILE *f = fopen(argv[1], "rb");
  if (!f) { perror(argv[1]); return 2; }
  fseek(f, 0, SEEK_END); long n = ftell(f); rewind(f);
  uint8_t *bytes = malloc(n > 0 ? (size_t)n : 1);
  if (fread(bytes, 1, (size_t)n, f) != (size_t)n) { perror(argv[1]); return 2; }
  fclose(f);
  GhosttySysDecodePngFn fn = decode_png;
  ghostty_sys_set(GHOSTTY_SYS_OPT_DECODE_PNG, fn);
  GhosttyTerminal t;
  if (ghostty_terminal_new(NULL, &t, (uint16_t)cols, (uint16_t)rows) != GHOSTTY_SUCCESS) { fprintf(stderr, "vt-replay: no terminal\n"); return 1; }
  uint64_t limit = 320ull << 20;
  ghostty_terminal_set(t, GHOSTTY_TERMINAL_OPT_KITTY_IMAGE_STORAGE_LIMIT, &limit);
  ghostty_terminal_resize(t, (uint16_t)cols, (uint16_t)rows, (uint32_t)cw, (uint32_t)chh);
  long done = 0; int asked = 0;
  for (; a + 1 < argc; a += 2) {
    if (strcmp(argv[a], "--at") != 0) continue;
    long upto = atol(argv[a + 1]);
    if (upto > n) upto = n;
    if (upto > done) { ghostty_terminal_vt_write(t, bytes + done, (size_t)(upto - done)); done = upto; }
    report(t, done, cols, rows);
    asked++;
  }
  if (!asked) { ghostty_terminal_vt_write(t, bytes, (size_t)n); report(t, n, cols, rows); }
  ghostty_terminal_free(t);
  free(bytes);
  return 0;
}
