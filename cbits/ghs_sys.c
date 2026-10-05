/* The few things the session tool needs from the OS that GHC's boot libraries do not give it: unix-domain
 * sockets (no `network` dependency), kernel file events, POSIX regular expressions, a fast content hash,
 * and a best-effort HTTP POST. Everything returns -1 (or NULL) on failure. */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <poll.h>
#include <regex.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

/* ---- unix sockets ---- */

static int unix_addr(struct sockaddr_un *a, const char *path) {
  memset(a, 0, sizeof *a);
  a->sun_family = AF_UNIX;
  if (strlen(path) >= sizeof a->sun_path) return -1;
  strcpy(a->sun_path, path);
  return 0;
}

int ghs_unix_listen(const char *path, int backlog) {
  struct sockaddr_un a;
  if (unix_addr(&a, path) < 0) return -1;
  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (fd < 0) return -1;
  fcntl(fd, F_SETFD, FD_CLOEXEC);
  unlink(path);
  if (bind(fd, (struct sockaddr *)&a, sizeof a) < 0 || listen(fd, backlog) < 0) { close(fd); return -1; }
  return fd;
}

/* a connection, -1 on timeout or error */
int ghs_unix_accept(int fd, int timeout_ms) {
  struct pollfd p = { fd, POLLIN, 0 };
  if (poll(&p, 1, timeout_ms) <= 0) return -1;
  int c = accept(fd, NULL, NULL);
  if (c >= 0) fcntl(c, F_SETFD, FD_CLOEXEC);
  return c;
}

int ghs_unix_connect(const char *path) {
  struct sockaddr_un a;
  if (unix_addr(&a, path) < 0) return -1;
  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (fd < 0) return -1;
  if (connect(fd, (struct sockaddr *)&a, sizeof a) < 0) { close(fd); return -1; }
  return fd;
}

/* A connected pair: the daemon keeps fds[0] (closed on exec) and gives fds[1] to the engine as its standard input. */
int ghs_socketpair(int *fds) {
  if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds) < 0) return -1;
  fcntl(fds[0], F_SETFD, FD_CLOEXEC);
  return 0;
}

/* ---- regular expressions (POSIX extended; on macOS with the enhanced syntax, so \s \d \b work) ---- */

void *ghs_regex_compile(const char *pat) {
  regex_t *re = malloc(sizeof *re);
  int flags = REG_EXTENDED | REG_NOSUB | REG_NEWLINE;
#ifdef REG_ENHANCED
  flags |= REG_ENHANCED;
#endif
  if (!re || regcomp(re, pat, flags) != 0) { free(re); return NULL; }
  return re;
}
int ghs_regex_match(void *re, const char *s) { return regexec((regex_t *)re, s, 0, NULL, 0) == 0; }
void ghs_regex_free(void *re) { if (re) { regfree((regex_t *)re); free(re); } }

/* ---- hashing: change detection, not security ----
 *
 * Small inputs (a path, a string): FNV-1a, a byte at a time. Files: four independent 64-bit lanes over 32-byte
 * stripes, a multiply and a rotate per 8 bytes, folded with the length at the end. The byte-at-a-time loop did
 * ~1 GB/s -- 70 ms for the 109 object files (70 MB) a server's code is, on every reload; this is ~10x that,
 * and the reads are the rest. */

uint64_t ghs_hash_bytes(const unsigned char *p, size_t n, uint64_t h) {
  if (!h) h = 1469598103934665603ULL;
  for (size_t i = 0; i < n; i++) { h ^= p[i]; h *= 1099511628211ULL; }
  return h;
}

#define GHS_K1 0x9E3779B97F4A7C15ULL
#define GHS_K2 0xC2B2AE3D27D4EB4FULL
static inline uint64_t ghs_rd64(const unsigned char *p) { uint64_t w; memcpy(&w, p, 8); return w; }
static inline uint64_t ghs_round(uint64_t acc, uint64_t w) { acc += w * GHS_K2; acc = (acc << 31) | (acc >> 33); return acc * GHS_K1; }
static inline uint64_t ghs_avalanche(uint64_t h) { h ^= h >> 33; h *= GHS_K2; h ^= h >> 29; h *= GHS_K1; h ^= h >> 32; return h; }

typedef struct { uint64_t v[4]; uint64_t len; unsigned char tail[32]; size_t ntail; } ghs_hstate;

static void ghs_hinit(ghs_hstate *st, uint64_t seed) {
  st->v[0] = seed + GHS_K1 + GHS_K2; st->v[1] = seed + GHS_K2; st->v[2] = seed; st->v[3] = seed - GHS_K1;
  st->len = 0; st->ntail = 0;
}
static void ghs_hupdate(ghs_hstate *st, const unsigned char *p, size_t n) {
  st->len += n;
  if (st->ntail) {                                   /* finish a stripe left over from the last block */
    size_t take = 32 - st->ntail; if (take > n) take = n;
    memcpy(st->tail + st->ntail, p, take); st->ntail += take; p += take; n -= take;
    if (st->ntail < 32) return;
    for (int k = 0; k < 4; k++) st->v[k] = ghs_round(st->v[k], ghs_rd64(st->tail + 8 * k));
    st->ntail = 0;
  }
  uint64_t a = st->v[0], b = st->v[1], c = st->v[2], d = st->v[3];
  while (n >= 32) {
    a = ghs_round(a, ghs_rd64(p)); b = ghs_round(b, ghs_rd64(p + 8)); c = ghs_round(c, ghs_rd64(p + 16)); d = ghs_round(d, ghs_rd64(p + 24));
    p += 32; n -= 32;
  }
  st->v[0] = a; st->v[1] = b; st->v[2] = c; st->v[3] = d;
  memcpy(st->tail, p, n); st->ntail = n;
}
static uint64_t ghs_hfinal(ghs_hstate *st) {
  uint64_t h = ((st->v[0] << 1) | (st->v[0] >> 63)) + ((st->v[1] << 7) | (st->v[1] >> 57))
             + ((st->v[2] << 12) | (st->v[2] >> 52)) + ((st->v[3] << 18) | (st->v[3] >> 46));
  h ^= ghs_hash_bytes(st->tail, st->ntail, st->len + 1);   /* the last partial stripe, and the length */
  h = ghs_avalanche(h + st->len);
  return h ? h : 1;
}

/* 0 when the file cannot be read (a hash is never 0) */
uint64_t ghs_hash_file(const char *path, uint64_t seed) {
  int fd = open(path, O_RDONLY);
  if (fd < 0) return 0;
  static __thread unsigned char buf[1 << 18];
  ghs_hstate st; ghs_hinit(&st, seed);
  ssize_t n;
  while ((n = read(fd, buf, sizeof buf)) > 0) ghs_hupdate(&st, buf, (size_t)n);
  close(fd);
  return n < 0 ? 0 : ghs_hfinal(&st);
}

/* ---- kernel file events ---- */

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#include <sys/event.h>
#include <sys/resource.h>
#ifndef O_EVTONLY
#define O_EVTONLY O_RDONLY
#endif
int ghs_watch_kind(void) { return 1; }                      /* 1: a descriptor per file and directory */
int ghs_watch_new(void) {
  struct rlimit r;                                          /* a descriptor per file: lift the soft limit */
  if (getrlimit(RLIMIT_NOFILE, &r) == 0) {
    rlim_t want = 16384;
    if (r.rlim_max != RLIM_INFINITY && r.rlim_max < want) want = r.rlim_max;
    if (r.rlim_cur < want) { r.rlim_cur = want; setrlimit(RLIMIT_NOFILE, &r); }
  }
  return kqueue();
}
int ghs_watch_budget(void) {
  struct rlimit r;
  return getrlimit(RLIMIT_NOFILE, &r) == 0 ? (int)(r.rlim_cur > 100000 ? 100000 : r.rlim_cur) - 256 : 0;
}
/* watch one path: the descriptor (close it to stop), or -1 */
int ghs_watch_add(int kq, const char *path) {
  int fd = open(path, O_EVTONLY | O_CLOEXEC);
  if (fd < 0) return -1;
  struct kevent ev;
  EV_SET(&ev, fd, EVFILT_VNODE, EV_ADD | EV_CLEAR, NOTE_WRITE | NOTE_EXTEND | NOTE_DELETE | NOTE_RENAME | NOTE_ATTRIB, 0, NULL);
  if (kevent(kq, &ev, 1, NULL, 0, NULL) < 0) { close(fd); return -1; }
  return fd;
}
int ghs_watch_rm(int kq, int wd) { (void)kq; return close(wd); }
/* 1 if anything happened within the timeout */
int ghs_watch_wait(int kq, int timeout_ms) {
  struct kevent ev[64];
  struct timespec ts = { timeout_ms / 1000, (long)(timeout_ms % 1000) * 1000000L };
  return kevent(kq, NULL, 0, ev, 64, &ts) > 0;
}
#elif defined(__linux__)
#include <sys/inotify.h>
int ghs_watch_kind(void) { return 2; }                      /* 2: a watch per directory */
int ghs_watch_new(void) { return inotify_init1(IN_NONBLOCK | IN_CLOEXEC); }
int ghs_watch_budget(void) { return 1 << 20; }
int ghs_watch_add(int fd, const char *dir) {
  return inotify_add_watch(fd, dir, IN_MODIFY | IN_ATTRIB | IN_CLOSE_WRITE | IN_MOVED_FROM | IN_MOVED_TO | IN_CREATE | IN_DELETE | IN_DELETE_SELF | IN_MOVE_SELF);
}
int ghs_watch_rm(int fd, int wd) { return inotify_rm_watch(fd, wd); }
int ghs_watch_wait(int fd, int timeout_ms) {
  struct pollfd p = { fd, POLLIN, 0 };
  if (poll(&p, 1, timeout_ms) <= 0) return 0;
  char buf[1 << 16];
  while (read(fd, buf, sizeof buf) > 0) {}                  /* drain: what happened is the scan's to say */
  return 1;
}
#else
int ghs_watch_kind(void) { return 0; }
int ghs_watch_new(void) { return -1; }
int ghs_watch_budget(void) { return 0; }
int ghs_watch_add(int k, const char *p) { (void)k; (void)p; return -1; }
int ghs_watch_rm(int k, int w) { (void)k; (void)w; return -1; }
int ghs_watch_wait(int k, int t) { (void)k; (void)t; return 0; }
#endif

/* ---- a best-effort HTTP POST (plain http, an observer's feed): 0 if the request was written ---- */

int ghs_http_post(const char *host, const char *port, const char *path, const char *body, int timeout_ms) {
  struct addrinfo hints, *res = NULL;
  memset(&hints, 0, sizeof hints);
  hints.ai_socktype = SOCK_STREAM;
  if (getaddrinfo(host, port, &hints, &res) != 0 || !res) return -1;
  int fd = socket(res->ai_family, res->ai_socktype, res->ai_protocol), rc = -1;
  if (fd >= 0) {
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) | O_NONBLOCK);
    int c = connect(fd, res->ai_addr, res->ai_addrlen);
    struct pollfd p = { fd, POLLOUT, 0 };
    if (c == 0 || (errno == EINPROGRESS && poll(&p, 1, timeout_ms) > 0)) {
      int err = 0; socklen_t el = sizeof err;
      getsockopt(fd, SOL_SOCKET, SO_ERROR, &err, &el);
      if (!err) {
        size_t bl = strlen(body), cap = bl + strlen(path) + strlen(host) + 256;
        char *req = malloc(cap);
        if (req) {
          int n = snprintf(req, cap, "POST %s HTTP/1.0\r\nHost: %s\r\nContent-Type: application/json\r\nContent-Length: %zu\r\nConnection: close\r\n\r\n%s", path, host, bl, body);
          fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) & ~O_NONBLOCK);
          struct timeval tv = { timeout_ms / 1000, (timeout_ms % 1000) * 1000 };
          setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof tv);
          if (write(fd, req, (size_t)n) == n) rc = 0;
          free(req);
        }
      }
    }
    close(fd);
  }
  freeaddrinfo(res);
  return rc;
}

/* ---- processes: liveness and memory without spawning `ps` (20 ms a time) or `footprint` (which can hang) ---- */

/* 0: no such process; 1: running; 2: a zombie (exited, not yet waited on) */
#if defined(__APPLE__)
#include <libproc.h>
#include <signal.h>
#include <sys/proc.h>
int ghs_pid_state(int pid) {
  struct proc_bsdinfo bi;
  if (proc_pidinfo(pid, PROC_PIDTBSDINFO, 0, &bi, sizeof bi) == (int)sizeof bi) return bi.pbi_status == SZOMB ? 2 : 1;
  /* not ours to inspect (another user's), or gone: kill(0) tells which */
  return (kill(pid, 0) == 0 || errno == EPERM) ? 1 : 0;
}
/* every process: pid, parent, resident KB, physical-footprint KB (0 where it cannot be read). Returns how many
 * (at most cap). The footprint is what macOS's memory pressure is about: RSS collapses when pages are compressed. */
int ghs_proc_table(int *pids, int *ppids, int64_t *rss_kb, int64_t *foot_kb, int cap) {
  int n = proc_listallpids(NULL, 0);
  if (n <= 0) return 0;
  pid_t *all = malloc((size_t)(n + 64) * sizeof *all);
  if (!all) return 0;
  n = proc_listallpids(all, (n + 64) * (int)sizeof *all);
  int k = 0;
  for (int i = 0; i < n && k < cap; i++) {
    struct proc_bsdinfo bi;
    if (all[i] <= 0 || proc_pidinfo(all[i], PROC_PIDTBSDINFO, 0, &bi, sizeof bi) != (int)sizeof bi) continue;
    struct rusage_info_v2 ri;
    int ok = proc_pid_rusage(all[i], RUSAGE_INFO_V2, (rusage_info_t *)&ri) == 0;
    pids[k] = all[i]; ppids[k] = (int)bi.pbi_ppid;
    rss_kb[k] = ok ? (int64_t)(ri.ri_resident_size / 1024) : 0;
    foot_kb[k] = ok ? (int64_t)(ri.ri_phys_footprint / 1024) : 0;
    k++;
  }
  free(all);
  return k;
}
#elif defined(__linux__)
#include <dirent.h>
#include <signal.h>
static int read_stat(int pid, int *ppid, char *state, long *rss_pages) {
  char path[64], buf[1024];
  snprintf(path, sizeof path, "/proc/%d/stat", pid);
  int fd = open(path, O_RDONLY);
  if (fd < 0) return -1;
  ssize_t n = read(fd, buf, sizeof buf - 1);
  close(fd);
  if (n <= 0) return -1;
  buf[n] = 0;
  char *p = strrchr(buf, ')');          /* the command may contain spaces and parentheses */
  if (!p) return -1;
  long rss = 0;
  /* after ") ": state ppid pgrp session tty tpgid flags minflt cminflt majflt cmajflt utime stime cutime cstime
   * priority nice threads itrealvalue starttime vsize rss */
  if (sscanf(p + 2, "%c %d %*d %*d %*d %*d %*u %*u %*u %*u %*u %*u %*u %*d %*d %*d %*d %*d %*d %*u %*u %ld", state, ppid, &rss) < 2) return -1;
  *rss_pages = rss;
  return 0;
}
int ghs_pid_state(int pid) {
  int pp; char st; long r;
  if (read_stat(pid, &pp, &st, &r) < 0) return (kill(pid, 0) == 0 || errno == EPERM) ? 1 : 0;
  return st == 'Z' ? 2 : 1;
}
int ghs_proc_table(int *pids, int *ppids, int64_t *rss_kb, int64_t *foot_kb, int cap) {
  DIR *d = opendir("/proc");
  if (!d) return 0;
  long page = sysconf(_SC_PAGESIZE) / 1024;
  struct dirent *e; int k = 0;
  while ((e = readdir(d)) && k < cap) {
    if (e->d_name[0] < '0' || e->d_name[0] > '9') continue;
    int pid = atoi(e->d_name), pp; char st; long r;
    if (read_stat(pid, &pp, &st, &r) < 0) continue;
    pids[k] = pid; ppids[k] = pp; rss_kb[k] = (int64_t)r * page; foot_kb[k] = 0; k++;
  }
  closedir(d);
  return k;
}
#else
#include <signal.h>
int ghs_pid_state(int pid) { return (kill(pid, 0) == 0 || errno == EPERM) ? 1 : 0; }
int ghs_proc_table(int *a, int *b, int64_t *c, int64_t *d, int cap) { (void)a; (void)b; (void)c; (void)d; (void)cap; return 0; }
#endif

/* ---- a process's command line, for the few we must attribute (orphans): no `ps -o command` (40-60 ms) ---- */

#if defined(__APPLE__)
#include <sys/sysctl.h>
/* the arguments joined by spaces into buf; the length, or -1 */
int ghs_proc_args(int pid, char *buf, int cap) {
  int mib[3] = { CTL_KERN, KERN_PROCARGS2, pid };
  size_t size = 0;
  if (sysctl(mib, 3, NULL, &size, NULL, 0) < 0 || size < sizeof(int)) return -1;
  char *raw = malloc(size);
  if (!raw) return -1;
  if (sysctl(mib, 3, raw, &size, NULL, 0) < 0) { free(raw); return -1; }
  int argc; memcpy(&argc, raw, sizeof argc);
  char *p = raw + sizeof argc, *end = raw + size;
  while (p < end && *p) p++;                 /* the executable's path */
  while (p < end && !*p) p++;                /* padding */
  int n = 0;
  for (int i = 0; i < argc && p < end; i++) {
    size_t l = strnlen(p, (size_t)(end - p));
    if (n + (int)l + 2 > cap) break;
    if (i) buf[n++] = ' ';
    memcpy(buf + n, p, l); n += (int)l;
    p += l + 1;
  }
  buf[n] = 0;
  free(raw);
  return n;
}
#elif defined(__linux__)
int ghs_proc_args(int pid, char *buf, int cap) {
  char path[64];
  snprintf(path, sizeof path, "/proc/%d/cmdline", pid);
  int fd = open(path, O_RDONLY);
  if (fd < 0) return -1;
  ssize_t n = read(fd, buf, (size_t)cap - 1);
  close(fd);
  if (n < 0) return -1;
  for (ssize_t i = 0; i + 1 < n; i++) if (!buf[i]) buf[i] = ' ';
  buf[n] = 0;
  return (int)strlen(buf);
}
#else
int ghs_proc_args(int pid, char *buf, int cap) { (void)pid; (void)buf; (void)cap; return -1; }
#endif

/* ---- hang up a socket another thread is blocked reading: it sees end of file, and so does the peer ---- */
int ghs_shutdown(int fd) { return shutdown(fd, SHUT_RDWR); }
