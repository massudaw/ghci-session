/* What the engine changes under the RTS, as a library inserted at its start (dyld interposing only
 * works from one): memory the RTS gives back is given back, and a temporary library answers for its
 * own names only (ghs_dlopen, below).
 *
 *
 * With its two-step allocator the RTS "decommits" free megablocks with madvise(MADV_FREE). On macOS
 * that is a hint: the pages stay dirty, and stay in the process's footprint, until the system is short
 * of memory -- a session whose RTS reported 551 MB in use had 830 MB of heap pages counted. (Linux
 * frees them at once with MADV_DONTNEED, which --disable-delayed-os-memory-return selects; on macOS
 * that flag changes nothing.)
 *
 * So the engine interposes madvise: a MADV_FREE of a range inside the RTS's own heap reservation is
 * done as a fresh anonymous mapping over the same range, which drops the pages now. The RTS commits a
 * range again the same way (mmap MAP_FIXED, read-write) and does not rely on what a decommitted range
 * holds. Anything else -- another advice, a range outside the heap -- goes to madvise unchanged.
 * ghs_mem_return(0) turns it off; ghs_mem_returned says how much it has returned. */
#if defined(__APPLE__)
#include <dlfcn.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

struct mblock_range { uintptr_t begin, end; };
static struct mblock_range *heap_range = 0;
static int looked = 0, enabled = -1;      /* -1: not read yet (GHS_MEM_RETURN=0 turns it off) */
static _Atomic long long calls = 0, bytes = 0;

int ghs_madvise(void *addr, size_t len, int advice) {
  if (enabled < 0) { const char *e = getenv("GHS_MEM_RETURN"); enabled = !(e && e[0] == '0'); }
  if (advice == MADV_FREE && enabled) {
    if (!looked) { heap_range = (struct mblock_range *)dlsym(RTLD_DEFAULT, "mblock_address_space"); looked = 1; }
    uintptr_t a = (uintptr_t)addr;
    if (heap_range && heap_range->begin && a >= heap_range->begin && a + len <= heap_range->end) {
      void *p = mmap(addr, len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON | MAP_FIXED, -1, 0);
      if (p == addr) { calls++; bytes += (long long)len; return 0; }
    }
  }
  return madvise(addr, len, advice);
}

/* A temporary library is asked for what IT defines.
 *
 * GHCi finds a name of loaded code by asking each library it has opened, newest first (the RTS's
 * internal_dlsym), and dlsym on a handle goes on through the libraries that one was linked against.
 * While every reload linked every module again that never mattered: the newest library had everything.
 * With unchanged modules kept linked a library holds a few modules, depends on the older libraries it
 * uses and NOT on a newer one it does not use -- so the newest library answered for a module it lacks
 * with a stale copy two generations back, past the current one. RTLD_FIRST makes a handle answer for
 * its own image only, which is what "newest first" was written to mean. Only for GHCi's temporary
 * libraries; ghs_dlopen_first says how many were opened so (the engine keeps modules linked only when
 * this is in place). */
static _Atomic int firsts = 0;
static void *ghs_dlopen(const char *path, int mode) {
  /* (libghc_tmp_N on GHC 9.14, libghc_N on 9.6: see is_tmp_lib in ghci_cafs.c) */
  const char *base = path ? strrchr(path, '/') : NULL;
  base = base ? base + 1 : path;
  if (base && !strncmp(base, "libghc_", 7) && (!strncmp(base + 7, "tmp_", 4) || (base[7] >= '0' && base[7] <= '9'))) { mode |= RTLD_FIRST; firsts++; }
  return dlopen(path, mode);
}
int ghs_dlopen_first(void) { return firsts; }

__attribute__((used)) static struct { const void *replacement; const void *replacee; } ghs_interpose[]
  __attribute__((section("__DATA,__interpose"))) = { { (const void *)ghs_madvise, (const void *)madvise }
                                                   , { (const void *)ghs_dlopen, (const void *)dlopen } };

int ghs_mem_return(int on) { int was = enabled != 0; if (on >= 0) enabled = on ? 1 : 0; return was; }
long long ghs_mem_returned(long long *ncalls) { if (ncalls) *ncalls = calls; return bytes; }
#else
int ghs_mem_return(int on) { (void)on; return -1; }
long long ghs_mem_returned(long long *ncalls) { if (ncalls) *ncalls = 0; return 0; }
#endif
