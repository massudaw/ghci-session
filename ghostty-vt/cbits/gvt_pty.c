/* A program on a pseudo-terminal, for "Ghostty.Vt.Pty". */
#define _XOPEN_SOURCE 700
#define _DEFAULT_SOURCE
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

/* Run `cmd` under /bin/sh in `cwd` on a new pseudo-terminal of cols x rows, with TERM set to `term`: the
 * master descriptor (close-on-exec) and the child's pid; -1 when it cannot. */
int gvt_pty_spawn(const char *cmd, const char *cwd, const char *term, int cols, int rows, int *pid_out) {
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
    if (cwd && cwd[0] && chdir(cwd) != 0) { /* then where we are */ }
    setenv("TERM", term && term[0] ? term : "xterm-256color", 1);
    setenv("COLORTERM", "truecolor", 1);
    signal(SIGINT, SIG_DFL); signal(SIGQUIT, SIG_DFL); signal(SIGTSTP, SIG_DFL); signal(SIGPIPE, SIG_DFL); signal(SIGHUP, SIG_DFL);
    sigset_t none; sigemptyset(&none); sigprocmask(SIG_SETMASK, &none, NULL);
    execl("/bin/sh", "sh", "-c", cmd, (char *)NULL);
    _exit(127);
  }
  fcntl(m, F_SETFD, FD_CLOEXEC);
  *pid_out = (int)pid;
  return m;
}

int gvt_pty_resize(int fd, int cols, int rows) {
  struct winsize ws; memset(&ws, 0, sizeof ws); ws.ws_row = (unsigned short)rows; ws.ws_col = (unsigned short)cols;
  return ioctl(fd, TIOCSWINSZ, &ws);
}

/* -1 while the child runs; else its exit status, 128 + the signal when one killed it. */
int gvt_pty_wait(int pid) {
  int st = 0;
  pid_t r = waitpid((pid_t)pid, &st, WNOHANG);
  if (r <= 0) return -1;
  if (WIFEXITED(st)) return WEXITSTATUS(st);
  if (WIFSIGNALED(st)) return 128 + WTERMSIG(st);
  return -1;
}
