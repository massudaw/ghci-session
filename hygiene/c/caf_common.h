/* The parts of the pruner that do not depend on the object format (ghci_cafs.c includes this once, in its
 * Mach-O or its ELF section): the images already judged wholly superseded, and where a name is defined now. */
#ifndef GHS_CAF_COMMON_H
#define GHS_CAF_COMMON_H

/* Images already judged wholly superseded, kept between calls: once true it stays
 * true (a newer definition never goes away), and the judgement is the expensive part. */
#define MAX_DEAD 4096
static uintptr_t dead_lo[MAX_DEAD], dead_hi[MAX_DEAD];
static size_t n_dead_img = 0;
static int in_dead_image(uintptr_t c) {
  for (size_t i = 0; i < n_dead_img; i++) if (c >= dead_lo[i] && c < dead_hi[i]) return 1;
  return 0;
}
static void remember_dead(uintptr_t lo, uintptr_t hi) {
  if (n_dead_img < MAX_DEAD) { dead_lo[n_dead_img] = lo; dead_hi[n_dead_img] = hi; n_dead_img++; }
}

/* Where a name is defined NOW: in the newest temporary library that itself defines it.
 *
 * dlsym(handle) does not stop at the handle's image: it goes on through the libraries that image was
 * linked against. A library holding only some modules (every one, since unchanged modules stay linked)
 * depends on the older libraries it uses and not on a newer one it does not, so asking the newest
 * library for a name it lacks answers with a STALE copy in one of its dependencies while the current
 * copy sits in a library in between -- whose CAFs were then unlinked as superseded, and their values
 * freed under running code. So an answer counts only when it lies in the image asked (g_tmp_lo/hi, the
 * ranges of the handles, set by ghs_prune_cafs_stats). */
static uintptr_t *g_tmp_lo = NULL, *g_tmp_hi = NULL;
static void *lookup_tmp(void **tmp, size_t nt, const char *name) {
  for (size_t k = 0; k < nt; k++) {
    void *v = dlsym(tmp[k], name);
    if (!v) continue;
    static int through = -1;                  /* GHS_CAF_THROUGH_DEPS=1: the old answer, to see the tour's `partial` fail */
    if (through < 0) through = getenv("GHS_CAF_THROUGH_DEPS") != NULL;
    if (!through && g_tmp_hi && g_tmp_hi[k] && ((uintptr_t)v < g_tmp_lo[k] || (uintptr_t)v >= g_tmp_hi[k])) continue;   /* a dependency's */
    return v;
  }
  return NULL;
}

static int cmp_ptr(const void *a, const void *b) {
  uintptr_t x = *(const uintptr_t *)a, y = *(const uintptr_t *)b;
  return x < y ? -1 : x > y;
}
#endif
