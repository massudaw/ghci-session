/* `eval`, without the Haskell runtime.
 *
 * An evaluation is the command a tool issues in a loop, and of its ~13 ms only ~2 are the session: the rest is
 * this (large, static) executable being mapped and the Haskell runtime starting and stopping. Everything an
 * `eval` needs is a connect, a line out and a line back, so it is done here, before the runtime is started:
 * ~6 ms. It handles exactly the plain case -- `eval [-s|-t|--session NAME] EXPR [--timeout N]` with the session
 * named, or exactly one running -- and returns -1 for anything else (an unknown flag, no socket, a
 * reply it does not understand), which then takes the ordinary path and gets the ordinary messages.
 *
 * It finds the session the way the Haskell side does: the nearest ghci-session.json at or above the working
 * directory, its "state_dir" (default .ghci-session), and <state>/<session>/sock, a link the daemon leaves to
 * its socket (whose own path is short: sun_path is ~104 bytes).
 */
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>

typedef struct { char *p; size_t n, cap; } Buf;
static int put(Buf *b, const char *s, size_t k) {
  if (b->n + k + 1 > b->cap) { size_t c = (b->cap ? b->cap * 2 : 4096) + k; char *q = realloc(b->p, c); if (!q) return -1; b->p = q; b->cap = c; }
  memcpy(b->p + b->n, s, k); b->n += k; b->p[b->n] = 0; return 0;
}
static int putc1(Buf *b, char c) { return put(b, &c, 1); }

static int json_string(Buf *b, const char *s) {       /* s is UTF-8: only quotes, backslashes and controls need escaping */
  putc1(b, '"');
  for (const unsigned char *c = (const unsigned char *)s; *c; c++) {
    if (*c == '"' || *c == '\\') { putc1(b, '\\'); putc1(b, (char)*c); }
    else if (*c < 0x20) { char u[8]; snprintf(u, sizeof u, "\\u%04x", *c); put(b, u, 6); }
    else putc1(b, (char)*c);
  }
  return putc1(b, '"');
}

static void utf8(Buf *b, unsigned cp) {
  char o[4];
  if (cp < 0x80) { o[0] = (char)cp; put(b, o, 1); }
  else if (cp < 0x800) { o[0] = (char)(0xC0 | (cp >> 6)); o[1] = (char)(0x80 | (cp & 63)); put(b, o, 2); }
  else if (cp < 0x10000) { o[0] = (char)(0xE0 | (cp >> 12)); o[1] = (char)(0x80 | ((cp >> 6) & 63)); o[2] = (char)(0x80 | (cp & 63)); put(b, o, 3); }
  else { o[0] = (char)(0xF0 | (cp >> 18)); o[1] = (char)(0x80 | ((cp >> 12) & 63)); o[2] = (char)(0x80 | ((cp >> 6) & 63)); o[3] = (char)(0x80 | (cp & 63)); put(b, o, 4); }
}
static int hex4(const char *p, unsigned *v) {
  *v = 0;
  for (int i = 0; i < 4; i++) {
    char c = p[i]; unsigned d;
    if (c >= '0' && c <= '9') d = (unsigned)(c - '0'); else if (c >= 'a' && c <= 'f') d = (unsigned)(c - 'a' + 10);
    else if (c >= 'A' && c <= 'F') d = (unsigned)(c - 'A' + 10); else return -1;
    *v = *v * 16 + d;
  }
  return 0;
}
/* a JSON string at *pp (on its opening quote), decoded into out (or skipped when out is NULL); 0 or -1 */
static int rd_string(const char **pp, const char *end, Buf *out) {
  const char *p = *pp;
  if (p >= end || *p != '"') return -1;
  for (p++; p < end && *p != '"'; p++) {
    if (*p != '\\') { if (out) putc1(out, *p); continue; }
    if (++p >= end) return -1;
    char c = *p;
    if (c == 'u') {
      unsigned cp, lo;
      if (p + 4 >= end || hex4(p + 1, &cp) < 0) return -1;
      p += 4;
      if (cp >= 0xD800 && cp < 0xDC00 && p + 6 < end && p[1] == '\\' && p[2] == 'u' && hex4(p + 3, &lo) == 0 && lo >= 0xDC00 && lo < 0xE000) {
        cp = 0x10000 + ((cp - 0xD800) << 10) + (lo - 0xDC00); p += 6;
      }
      if (out) utf8(out, cp);
    } else if (out) {
      char d = c == 'n' ? '\n' : c == 't' ? '\t' : c == 'r' ? '\r' : c == 'b' ? '\b' : c == 'f' ? '\f' : c;
      putc1(out, d);
    }
  }
  if (p >= end) return -1;
  *pp = p + 1;
  return 0;
}
static void ws(const char **pp, const char *end) { while (*pp < end && (**pp == ' ' || **pp == '\n' || **pp == '\t' || **pp == '\r')) (*pp)++; }
/* skip any JSON value */
static int skip(const char **pp, const char *end) {
  ws(pp, end);
  if (*pp >= end) return -1;
  char c = **pp;
  if (c == '"') return rd_string(pp, end, NULL);
  if (c == '{' || c == '[') {
    char close = c == '{' ? '}' : ']';
    (*pp)++; ws(pp, end);
    if (*pp < end && **pp == close) { (*pp)++; return 0; }
    for (;;) {
      if (c == '{') { ws(pp, end); if (rd_string(pp, end, NULL) < 0) return -1; ws(pp, end); if (*pp >= end || **pp != ':') return -1; (*pp)++; }
      if (skip(pp, end) < 0) return -1;
      ws(pp, end);
      if (*pp >= end) return -1;
      if (**pp == ',') { (*pp)++; continue; }
      if (**pp == close) { (*pp)++; return 0; }
      return -1;
    }
  }
  while (*pp < end && **pp != ',' && **pp != '}' && **pp != ']') (*pp)++;      /* a number, true, false, null */
  return 0;
}

static char *slurp(const char *path, size_t *n) {
  int fd = open(path, O_RDONLY);
  if (fd < 0) return NULL;
  struct stat st;
  if (fstat(fd, &st) < 0 || st.st_size > (1 << 22)) { close(fd); return NULL; }
  char *b = malloc((size_t)st.st_size + 1);
  ssize_t got = b ? read(fd, b, (size_t)st.st_size) : -1;
  close(fd);
  if (got < 0) { free(b); return NULL; }
  b[got] = 0; *n = (size_t)got;
  return b;
}

/* a connection to a session's daemon, through the link it leaves in its state directory; -1 if it is not up */
static int session_socket(const char *sdir, const char *session) {
  char link[6000], sock[256];
  snprintf(link, sizeof link, "%s/%s/sock", sdir, session);
  ssize_t ln = readlink(link, sock, sizeof sock - 1);
  if (ln <= 0) return -1;
  sock[ln] = 0;
  struct sockaddr_un sa; memset(&sa, 0, sizeof sa); sa.sun_family = AF_UNIX;
  if ((size_t)ln >= sizeof sa.sun_path) return -1;
  memcpy(sa.sun_path, sock, (size_t)ln + 1);
  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (fd < 0) return -1;
  if (connect(fd, (struct sockaddr *)&sa, sizeof sa) < 0) { close(fd); return -1; }
  return fd;
}

/* -1: not handled here; otherwise the exit status */
int ghs_fast_client(int argc, char **argv) {
  if (argc < 3 || strcmp(argv[1], "eval")) return -1;
  const char *session = NULL, *expr = NULL, *timeout = NULL;
  for (int i = 2; i < argc; i++) {
    const char *a = argv[i];
    if (!strcmp(a, "-s") || !strcmp(a, "-t") || !strcmp(a, "--session")) { if (++i >= argc) return -1; session = argv[i]; }
    else if (!strcmp(a, "--timeout")) { if (++i >= argc) return -1; timeout = argv[i]; }
    else if (a[0] == '-' && a[1]) return -1;               /* a flag we do not know, or a negative number: the ordinary path decides */
    else if (!expr) expr = a;
    else return -1;
  }
  if (!expr || (session && (!*session || strchr(session, '/')))) return -1;
  if (timeout) { char *e; strtod(timeout, &e); if (*e || e == timeout) return -1; }

  char dir[4096], path[4400];
  if (!getcwd(dir, sizeof dir)) return -1;
  char *conf = NULL; size_t cn = 0;
  for (;;) {
    snprintf(path, sizeof path, "%s/ghci-session.json", dir);
    if ((conf = slurp(path, &cn))) break;
    char *sl = strrchr(dir, '/');
    if (!sl || sl == dir) return -1;
    *sl = 0;
  }
  /* "state_dir": a top-level string; it is the only place the key can be (a target with it is refused) */
  char state[1024] = ".ghci-session";
  const char *k = strstr(conf, "\"state_dir\"");
  if (k) {
    const char *p = k + 11, *end = conf + cn;
    ws(&p, end);
    if (p >= end || *p != ':') { free(conf); return -1; }
    p++; ws(&p, end);
    Buf sb = {0};
    if (rd_string(&p, end, &sb) < 0 || !sb.p || sb.n >= sizeof state) { free(sb.p); free(conf); return -1; }
    memcpy(state, sb.p, sb.n + 1); free(sb.p);
  }
  free(conf);
  char sdir[5200];
  if (state[0] == '/') snprintf(sdir, sizeof sdir, "%s", state);
  else snprintf(sdir, sizeof sdir, "%s/%s", dir, state);
  int fd = -1;
  if (session) fd = session_socket(sdir, session);
  else {
    /* no session named: the only one running (more than one, or none, is the ordinary path's to decide) */
    DIR *d = opendir(sdir);
    if (!d) return -1;
    struct dirent *e; int n = 0;
    while ((e = readdir(d))) {
      if (e->d_name[0] == '.') continue;
      int c = session_socket(sdir, e->d_name);
      if (c < 0) continue;
      if (++n > 1) { close(c); break; }
      fd = c;
    }
    closedir(d);
    if (n != 1) { if (fd >= 0) close(fd); return -1; }
  }
  if (fd < 0) return -1;

  Buf req = {0};
  put(&req, "{\"op\":\"eval\",\"expr\":", 20);
  json_string(&req, expr);
  if (timeout) { put(&req, ",\"timeout\":", 11); put(&req, timeout, strlen(timeout)); }
  put(&req, "}\n", 2);
  for (size_t off = 0; off < req.n;) {
    ssize_t w = write(fd, req.p + off, req.n - off);
    if (w < 0) { if (errno == EINTR) continue; close(fd); free(req.p); return -1; }   /* nothing was run: the ordinary path may try */
    off += (size_t)w;
  }
  free(req.p);
  /* from here the request is the session's: whatever happens, do not hand it to the ordinary path to run again */
  Buf rep = {0}; char chunk[65536];
  for (;;) {
    ssize_t r = read(fd, chunk, sizeof chunk);
    if (r < 0 && errno == EINTR) continue;
    if (r <= 0) break;
    put(&rep, chunk, (size_t)r);
    if (memchr(chunk, '\n', (size_t)r)) break;
  }
  close(fd);
  if (!rep.p) { fprintf(stderr, "bad reply from the session: nothing came back\n"); return 2; }
  const char *p = rep.p, *end = rep.p + rep.n;
  int ok = 0, nstale = 0, bad = 0;
  Buf out = {0}, first = {0};
  ws(&p, end);
  if (p >= end || *p != '{') bad = 1; else p++;
  while (!bad) {
    ws(&p, end);
    if (p < end && *p == '}') break;
    Buf key = {0};
    if (rd_string(&p, end, &key) < 0) { free(key.p); bad = 1; break; }
    ws(&p, end);
    if (p >= end || *p != ':') { free(key.p); bad = 1; break; }
    p++; ws(&p, end);
    const char *kk = key.p ? key.p : "";
    if (!strcmp(kk, "ok")) { ok = p + 4 <= end && !strncmp(p, "true", 4); if (skip(&p, end) < 0) bad = 1; }
    else if (!strcmp(kk, "out") && p < end && *p == '"') { if (rd_string(&p, end, &out) < 0) bad = 1; }
    else if (!strcmp(kk, "stale") && p < end && *p == '[') {
      p++;
      for (;;) {
        ws(&p, end);
        if (p < end && *p == ']') { p++; break; }
        if (rd_string(&p, end, nstale == 0 ? &first : NULL) < 0) { bad = 1; break; }
        nstale++;
        ws(&p, end);
        if (p < end && *p == ',') p++;
      }
    }
    else if (skip(&p, end) < 0) bad = 1;
    free(key.p);
    ws(&p, end);
    if (p < end && *p == ',') p++;
  }
  if (bad) { fprintf(stderr, "bad reply from the session\n"); return 2; }
  if (nstale > 0) {
    const char *f = first.p ? first.p : "", *base = strrchr(f, '/');
    fprintf(stderr, "warning: STALE -- %d watched file(s) differ from the loaded code (e.g. %s); `ghci-session reload`\n", nstale, base ? base + 1 : f);
  }
  if (out.p) fwrite(out.p, 1, out.n, stdout);
  fputc('\n', stdout);
  fflush(stdout);
  return ok ? 0 : 1;
}
