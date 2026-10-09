/* HTTPS for the model client (GhciSession.Llm), through libcurl -- found at run time with dlopen, so the
 * build needs neither its headers nor its link name (a machine has libcurl.so.4 or libcurl.4.dylib because
 * `curl` is there; the -dev package and the libcurl.so symlink it often has not), and the package still
 * depends on nothing outside GHC's boot packages. The easy API's few functions and option numbers are
 * libcurl's stable ABI, declared here as curl.h declares them. libcurl reads the proxy from the
 * environment by itself (https_proxy); the CA bundle is passed in (CURL_CA_BUNDLE / SSL_CERT_FILE are the
 * curl TOOL's variables, not the library's). */
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>

typedef void CURL;
typedef int CURLcode;
struct curl_slist;
#define CURLOPT_WRITEDATA        10001
#define CURLOPT_URL              10002
#define CURLOPT_ERRORBUFFER      10010
#define CURLOPT_POSTFIELDS       10015
#define CURLOPT_HTTPHEADER       10023
#define CURLOPT_WRITEFUNCTION    20011
#define CURLOPT_TIMEOUT          13
#define CURLOPT_POSTFIELDSIZE    60
#define CURLOPT_FOLLOWLOCATION   52
#define CURLOPT_NOSIGNAL         99
#define CURLOPT_CAINFO           10065
#define CURLOPT_ACCEPT_ENCODING  10102
#define CURLOPT_HEADERFUNCTION   20079
#define CURLOPT_HEADERDATA       10029
#define CURLOPT_NOPROGRESS       43
#define CURLOPT_XFERINFOFUNCTION 20219
#define CURLOPT_XFERINFODATA     10057
#define CURLOPT_LOW_SPEED_LIMIT  19
#define CURLOPT_LOW_SPEED_TIME   20
#define CURLINFO_RESPONSE_CODE   0x200002
#define CURL_GLOBAL_DEFAULT      3

static void *lib;
static CURLcode (*f_global_init)(long);
static CURL *(*f_init)(void);
static CURLcode (*f_setopt)(CURL *, int, ...);
static CURLcode (*f_perform)(CURL *);
static void (*f_cleanup)(CURL *);
static CURLcode (*f_getinfo)(CURL *, int, ...);
static struct curl_slist *(*f_slist_append)(struct curl_slist *, const char *);
static void (*f_slist_free_all)(struct curl_slist *);
static const char *(*f_strerror)(CURLcode);

/* 0 when libcurl is loaded (once), else a message in err. */
static int load(char *err, size_t errlen) {
  if (lib) return 0;
  static const char *names[] = { "libcurl.so.4", "libcurl.4.dylib", "libcurl.so", "libcurl.dylib", "libcurl-gnutls.so.4", "libcurl-nss.so.4", NULL };
  void *h = NULL;
  for (int i = 0; names[i] && !h; i++) h = dlopen(names[i], RTLD_NOW | RTLD_LOCAL);
  if (!h) { snprintf(err, errlen, "libcurl not found (tried libcurl.so.4, libcurl.4.dylib, ...): %s", dlerror() ? dlerror() : "no such library"); return 1; }
#define SYM(var, name) do { *(void **)&var = dlsym(h, name); if (!var) { snprintf(err, errlen, "libcurl without %s", name); dlclose(h); return 1; } } while (0)
  SYM(f_global_init, "curl_global_init"); SYM(f_init, "curl_easy_init"); SYM(f_setopt, "curl_easy_setopt");
  SYM(f_perform, "curl_easy_perform"); SYM(f_cleanup, "curl_easy_cleanup"); SYM(f_getinfo, "curl_easy_getinfo");
  SYM(f_slist_append, "curl_slist_append"); SYM(f_slist_free_all, "curl_slist_free_all"); SYM(f_strerror, "curl_easy_strerror");
#undef SYM
  f_global_init(CURL_GLOBAL_DEFAULT);
  lib = h;
  return 0;
}

struct buf { char *p; size_t n, cap; };

static size_t on_write(char *d, size_t s, size_t n, void *u) {
  struct buf *b = u;
  size_t k = s * n;
  if (b->n + k + 1 > b->cap) {
    size_t cap = b->cap ? b->cap : 65536;
    while (b->n + k + 1 > cap) cap *= 2;
    char *p = realloc(b->p, cap);
    if (!p) return 0;
    b->p = p; b->cap = cap;
  }
  memcpy(b->p + b->n, d, k);
  b->n += k;
  b->p[b->n] = 0;
  return k;
}

/* The reply's body to a descriptor as it arrives (a reply that is a stream: each event is read while the
 * next is still being written). */
static size_t on_write_fd(char *d, size_t s, size_t n, void *u) {
  int fd = *(int *)u;
  size_t k = s * n, done = 0;
  while (done < k) {
    ssize_t w = write(fd, d + done, k - done);
    if (w <= 0) return 0;            /* (the reader is gone: the transfer ends) */
    done += (size_t)w;
  }
  return k;
}

/* The one header read: how long the server asks to be left alone (Retry-After, in seconds). */
static size_t on_header(char *d, size_t s, size_t n, void *u) {
  size_t k = s * n;
  if (k > 12 && !strncasecmp(d, "retry-after:", 12)) {
    char tmp[32]; size_t m = k - 12 < sizeof tmp - 1 ? k - 12 : sizeof tmp - 1;
    memcpy(tmp, d + 12, m); tmp[m] = 0;
    long v = strtol(tmp, NULL, 10);
    if (v > 0) *(long *)u = v;
  }
  return k;
}

/* Giving up the exchanges under way: each notes the count when it begins, and ends itself (at its next look,
 * within a second) once the count has moved. So a turn that is stopped does not leave a reply being read --
 * and paid for -- behind it. */
static volatile long cancels = 0;
void ghs_https_cancel(void) { __sync_fetch_and_add(&cancels, 1); }
static int on_progress(void *u, long long dt, long long dn, long long ut, long long un) {
  (void)dt; (void)dn; (void)ut; (void)un;
  return *(long *)u != cancels;
}

/* POST body to url with the headers (one per line in `headers`). 0 and the reply with its status, or 1 and why
 * in err (errlen >= 256). The reply's body: with fd < 0, all of it (malloc'd: ghs_https_free); else written to
 * fd as it arrives, *out empty. timeout_s: the whole exchange (0: no limit). idle_s: this long with nothing
 * received ends it (0: no limit) -- what a stream is held to, which may rightly go on for longer than any
 * limit on the whole. *retry_after: the seconds the server asked for, 0 if it did not. */
int ghs_https_request(const char *url, const char *headers, const char *body, size_t body_len, long timeout_s, long idle_s, const char *cainfo,
                      int fd, char **out, size_t *out_len, long *status, long *retry_after, char *err, size_t errlen) {
  *out = NULL; *out_len = 0; *status = 0; *retry_after = 0; err[0] = 0;
  if (load(err, errlen)) return 1;
  CURL *c = f_init();
  if (!c) { snprintf(err, errlen, "curl_easy_init failed"); return 1; }
  struct curl_slist *hs = NULL;
  char *hcopy = strdup(headers ? headers : "");
  for (char *line = strtok(hcopy, "\n"); line; line = strtok(NULL, "\n")) if (*line) hs = f_slist_append(hs, line);
  struct buf b = { NULL, 0, 0 };
  char cerr[256]; cerr[0] = 0;
  f_setopt(c, CURLOPT_URL, url);
  f_setopt(c, CURLOPT_HTTPHEADER, hs);
  f_setopt(c, CURLOPT_POSTFIELDS, body);
  f_setopt(c, CURLOPT_POSTFIELDSIZE, (long)body_len);
  if (fd >= 0) { f_setopt(c, CURLOPT_WRITEFUNCTION, on_write_fd); f_setopt(c, CURLOPT_WRITEDATA, &fd); }
  else { f_setopt(c, CURLOPT_WRITEFUNCTION, on_write); f_setopt(c, CURLOPT_WRITEDATA, &b); }
  f_setopt(c, CURLOPT_HEADERFUNCTION, on_header);
  f_setopt(c, CURLOPT_HEADERDATA, retry_after);
  f_setopt(c, CURLOPT_TIMEOUT, timeout_s);
  if (idle_s > 0) { f_setopt(c, CURLOPT_LOW_SPEED_LIMIT, 1L); f_setopt(c, CURLOPT_LOW_SPEED_TIME, idle_s); }
  long began = cancels;
  f_setopt(c, CURLOPT_XFERINFOFUNCTION, on_progress);
  f_setopt(c, CURLOPT_XFERINFODATA, &began);
  f_setopt(c, CURLOPT_NOPROGRESS, 0L);
  f_setopt(c, CURLOPT_NOSIGNAL, 1L);
  f_setopt(c, CURLOPT_FOLLOWLOCATION, 1L);
  f_setopt(c, CURLOPT_ACCEPT_ENCODING, "");
  f_setopt(c, CURLOPT_ERRORBUFFER, cerr);
  if (cainfo && *cainfo) f_setopt(c, CURLOPT_CAINFO, cainfo);
  CURLcode rc = f_perform(c);
  f_getinfo(c, CURLINFO_RESPONSE_CODE, status);
  f_slist_free_all(hs);
  f_cleanup(c);
  free(hcopy);
  if (rc != 0) {
    snprintf(err, errlen, "%s%s%s", f_strerror(rc), cerr[0] ? ": " : "", cerr);
    free(b.p);
    return 1;
  }
  *out = b.p ? b.p : calloc(1, 1);
  *out_len = b.n;
  return 0;
}

int ghs_https_post(const char *url, const char *headers, const char *body, size_t body_len, long timeout_s, const char *cainfo,
                   char **out, size_t *out_len, long *status, char *err, size_t errlen) {
  long after = 0;
  return ghs_https_request(url, headers, body, body_len, timeout_s, 0, cainfo, -1, out, out_len, status, &after, err, errlen);
}

void ghs_https_free(char *p) { free(p); }
