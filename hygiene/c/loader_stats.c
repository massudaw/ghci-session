/* ghs_loader_stats -- what the RTS linker is holding, read from inside the process.
 * A diagnostic for the reload leak: how many objects are loaded (by status and
 * kind), how many bytes of object code they hold, and how many CAFs are rooted on
 * each list. Offsets are from `nm` of the RTS the process runs (the threaded RTS
 * dylib in a dynamic GHCi, the ghc-iserv executable in a static one), so the
 * build script takes the binary as an argument. Prints to stderr, returns the
 * object count (-1: offsets do not match this process). */
#define _DARWIN_C_SOURCE
#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef GHS_OFF_KEEPCAFS
#error "build with offsets: see build.sh"
#endif
#define OC_STATUS 0
#define OC_TYPE 32
#define OC_N_SECTIONS 96
#define OC_SECTIONS 104
#define OC_NEXT 128
#define OC_NEXT_LOADED 144
#define SEC_SIZE 8
#define SEC_STRIDE 56
#define CAF_STATIC_LINK 16
#define LIST_END 3u

int ghs_loader_stats(void) {
  void *k = dlsym(RTLD_DEFAULT, "keepCAFs"), *h = dlsym(RTLD_DEFAULT, "highMemDynamic"), *u = dlsym(RTLD_DEFAULT, "unloadObj");
  if (!k || !h || !u) { fprintf(stderr, "loader_stats: RTS symbols not visible\n"); return -1; }
  uintptr_t b = (uintptr_t)k - GHS_OFF_KEEPCAFS;
  if ((uintptr_t)h != b + GHS_OFF_HIGHMEMDYNAMIC || (uintptr_t)u != b + GHS_OFF_UNLOADOBJ) { fprintf(stderr, "loader_stats: offsets do not match\n"); return -1; }
  char **objects = (char **)(b + GHS_OFF_OBJECTS), **loaded = (char **)(b + GHS_OFF_LOADED_OBJECTS);
  long n_unl = *(long *)(b + GHS_OFF_N_UNLOADED);
  uintptr_t *dyn = (uintptr_t *)(b + GHS_OFF_DYN_CAF_LIST), *rev = (uintptr_t *)(b + GHS_OFF_REVERTIBLE);
  int n = 0, st[8] = {0}, ty[2] = {0}, nloaded = 0;
  double mb_static = 0, mb_static_unl = 0;
  for (char *oc = *objects; oc; oc = *(char **)(oc + OC_NEXT)) {
    n++;
    int s = *(int *)(oc + OC_STATUS), t = *(int *)(oc + OC_TYPE);
    if (s >= 0 && s < 8) st[s]++;
    if (t >= 0 && t < 2) ty[t]++;
    if (t == 0) {
      int ns = *(int *)(oc + OC_N_SECTIONS); char *secs = *(char **)(oc + OC_SECTIONS);
      double bytes = 0;
      for (int i = 0; secs && i < ns; i++) bytes += (double)*(uintptr_t *)(secs + (size_t)i * SEC_STRIDE + SEC_SIZE);
      mb_static += bytes / 1048576.0;
      if (s == 4) mb_static_unl += bytes / 1048576.0;
    }
  }
  if (getenv("GHS_LS_LIST"))
    for (char *oc = *objects; oc; oc = *(char **)(oc + OC_NEXT))
      if (*(int *)(oc + OC_STATUS) == 4) { const char *fn = *(const char **)(oc + 8); fprintf(stderr, "  pending-unload: %s\n", fn ? (strrchr(fn, '/') ? strrchr(fn, '/') + 1 : fn) : "?"); }
  for (char *oc = *loaded; oc; oc = *(char **)(oc + OC_NEXT_LOADED)) nloaded++;
  int nd = 0, nr = 0;
  for (uintptr_t c = *dyn; c != LIST_END; c = *(uintptr_t *)((c & ~(uintptr_t)3) + CAF_STATIC_LINK)) nd++;
  for (uintptr_t c = *rev; c != LIST_END; c = *(uintptr_t *)((c & ~(uintptr_t)3) + CAF_STATIC_LINK)) nr++;
  /* who owns the CAFs on dyn_caf_list? bucket by the owning object's status and file-name kind */
  {
    typedef struct { uintptr_t lo, hi; char *oc; } R;
    size_t nr = 0, cap = 4096; R *rs = malloc(cap * sizeof *rs);
    for (char *oc = *objects; rs && oc; oc = *(char **)(oc + OC_NEXT)) {
      if (*(int *)(oc + OC_TYPE) != 0) continue;
      int ns = *(int *)(oc + OC_N_SECTIONS); char *secs = *(char **)(oc + OC_SECTIONS);
      for (int i = 0; secs && i < ns; i++) {
        uintptr_t st = *(uintptr_t *)(secs + (size_t)i * SEC_STRIDE), z = *(uintptr_t *)(secs + (size_t)i * SEC_STRIDE + SEC_SIZE);
        if (!st || !z) continue;
        if (nr == cap) { cap *= 2; R *t = realloc(rs, cap * sizeof *rs); if (!t) { free(rs); rs = NULL; break; } rs = t; }
        rs[nr].lo = st; rs[nr].hi = st + z; rs[nr].oc = oc; nr++;
      }
    }
    if (rs) {
      long by_status[8] = {0}, archive = 0, objdir = 0, none = 0, unl_objdir = 0;
      for (uintptr_t c = *dyn; c != LIST_END; c = *(uintptr_t *)((c & ~(uintptr_t)3) + CAF_STATIC_LINK)) {
        uintptr_t a = c & ~(uintptr_t)3; char *own = NULL;
        for (size_t i = 0; i < nr; i++) if (a >= rs[i].lo && a < rs[i].hi) { own = rs[i].oc; break; }
        if (!own) { none++; continue; }
        int s = *(int *)(own + OC_STATUS); if (s >= 0 && s < 8) by_status[s]++;
        const char *fn = *(const char **)(own + 8);
        if (fn && strstr(fn, ".a(")) archive++; else if (fn && strstr(fn, ".sprk/")) { objdir++; if (s == 4) unl_objdir++; }
      }
      fprintf(stderr, "loader_stats: dyn_caf_list owners: no static owner %ld; by status [ready %ld, unloaded %ld, other %ld]; from archives %ld, from .sprk obj dir %ld (of it unloaded %ld)\n",
              none, by_status[3], by_status[4], by_status[0] + by_status[1] + by_status[2], archive, objdir, unl_objdir);
      free(rs);
    }
  }
  fprintf(stderr, "loader_stats: %d objects (static %d, dynamic %d; ready %d, unloaded-pending %d, other %d), %d in loaded_objects, "
          "static object code %.0f MB (of it pending-unload %.0f MB), n_unloaded_objects %ld, CAFs: dyn_caf_list %d, revertible %d\n",
          n, ty[0], ty[1], st[3], st[4], n - st[3] - st[4], nloaded, mb_static, mb_static_unl, n_unl, nd, nr);
  return n;
}
