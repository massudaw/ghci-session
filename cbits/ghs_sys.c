/* The few things the session tool needs from the OS that GHC's boot libraries do not give it: unix-domain
 * sockets (no `network` dependency), kernel file events, POSIX regular expressions, a fast content hash,
 * a terminal's window size, and a best-effort HTTP POST. Everything returns -1 (or NULL) on failure. */
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
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <termios.h>
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

/* ---- terminal ---- */

int ghs_set_winsize(int fd, int rows, int cols) {
  struct winsize w = { (unsigned short)rows, (unsigned short)cols, 0, 0 };
  return ioctl(fd, TIOCSWINSZ, &w);
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

/* ---- hashing: FNV-1a 64. Change detection, not security. ---- */

uint64_t ghs_hash_bytes(const unsigned char *p, size_t n, uint64_t h) {
  if (!h) h = 1469598103934665603ULL;
  for (size_t i = 0; i < n; i++) { h ^= p[i]; h *= 1099511628211ULL; }
  return h;
}

/* 0 when the file cannot be read (no real content hashes to 0: the seed is odd and so is the prime) */
uint64_t ghs_hash_file(const char *path, uint64_t h) {
  int fd = open(path, O_RDONLY);
  if (fd < 0) return 0;
  unsigned char buf[1 << 16];
  ssize_t n;
  if (!h) h = 1469598103934665603ULL;
  while ((n = read(fd, buf, sizeof buf)) > 0) h = ghs_hash_bytes(buf, (size_t)n, h);
  close(fd);
  return n < 0 ? 0 : (h ? h : 1);
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
