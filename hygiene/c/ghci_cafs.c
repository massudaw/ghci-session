/* ghs_prune_cafs -- the GHCi reload leak, mitigated from outside the RTS.
 *
 * What leaks. With a dynamically linked GHC (ghcup's, macOS) every `:reload`
 * links the modules it recompiled -- and their dependents -- into a NEW
 * temporary shared library (libghc_tmp_N.dylib) and never unloads the old one:
 * GHC.Linker.Loader.unload_wkr, "We don't do any cleanup when linking objects
 * with the dynamic linker". Worse, the RTS chains every CAF of every such
 * library onto `dyn_caf_list`, and `markCAFs` treats that whole list as GC
 * roots (evacuating each CAF's value) for the life of the process. So the
 * evaluated value of every CAF of every superseded module -- here the projected
 * views, element lists, tables: ~200 MB per edit -- stays reachable forever.
 *
 * What this does. A CAF in a libghc_tmp_* library whose name now resolves, by
 * the RTS's own lookup order (loaded objects, newest first), to a DIFFERENT
 * address belongs to a superseded module: no code that can still run refers to
 * it by name (a module whose dependency was recompiled is itself relinked), and
 * a live closure that does reach it through an SRT is handled by the ordinary
 * static-object scavenging once the CAF is off the list. So unlink it from
 * `dyn_caf_list` and reset its static_link to NULL -- what revertCAFs does, for
 * the same reason (#16842: a stale static_link makes a major GC skip it). The
 * next major GC then frees the values.
 *
 * That names only the exported CAFs. The compiler also floats constant
 * sub-expressions into LOCAL CAFs, which cannot be looked up by name -- and
 * those hold the rest (a floated [1..n] list kept 72 of 100 MB per reload in a
 * test). So a whole temporary library is dropped when it is wholly superseded:
 * every exported symbol in its symbol table (read from the mapped image)
 * resolves, newest first, to some OTHER image. Each edit's library is
 * superseded by the next edit's, so the leak stops growing after the first
 * generation (the very first library also holds the modules that never change,
 * so it is never wholly superseded).
 *
 * Not touched: CAFs of package libraries (libHS*), of the current generation,
 * and any whose symbol cannot be resolved (kept).
 *
 * The RTS keeps `dyn_caf_list` and `loaded_objects` private, so they are
 * located by their offset from exported symbols in THIS RTS build (GHS_OFF_*,
 * from `nm` of the RTS dylib: build.sh), checked against four exported symbols
 * before anything is read. The ObjectCode / StgIndStatic layouts are GHC 9.14.1
 * arm64's, measured against its headers.
 */
#define _DARWIN_C_SOURCE
#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifndef GHS_OFF_KEEPCAFS
#error "build with the offsets of the RTS dylib: see build.sh"
#endif

#define OC_TYPE 32
#define OC_NEXT_LOADED 144
#define OC_DLOPEN_HANDLE 272
#define DYNAMIC_OBJECT 1
#define CAF_STATIC_LINK 16
#define LIST_END 3u

static uintptr_t rts_base(void) {
  void *k = dlsym(RTLD_DEFAULT, "keepCAFs");
  void *h = dlsym(RTLD_DEFAULT, "highMemDynamic");
  void *u = dlsym(RTLD_DEFAULT, "unloadObj");
  void *l = dlsym(RTLD_DEFAULT, "lookupSymbol");
  if (!k || !h || !u || !l) return 0;
  uintptr_t b = (uintptr_t)k - GHS_OFF_KEEPCAFS;
  if ((uintptr_t)h != b + GHS_OFF_HIGHMEMDYNAMIC || (uintptr_t)u != b + GHS_OFF_UNLOADOBJ
      || (uintptr_t)l != b + GHS_OFF_LOOKUPSYMBOL) return 0;
  return b;
}


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

/* ---- Mach-O image helpers --------------------------------------------- */
typedef struct { uintptr_t lo, hi; int dead; } Img;

static const struct mach_header_64 *image_by_base(const void *base) {
  uint32_t n = _dyld_image_count();
  for (uint32_t i = 0; i < n; i++)
    if ((const void *)_dyld_get_image_header(i) == base) return (const struct mach_header_64 *)base;
  return NULL;
}

/* [lo,hi) of the image's segments, and its symbol table */
static int image_info(const struct mach_header_64 *h, uintptr_t *lo, uintptr_t *hi,
                      const struct nlist_64 **syms, uint32_t *nsyms, const char **strs) {
  const struct load_command *lc = (const struct load_command *)(h + 1);
  const struct segment_command_64 *text = NULL, *linkedit = NULL;
  const struct symtab_command *st = NULL;
  uintptr_t mn = (uintptr_t)-1, mx = 0;
  for (uint32_t i = 0; i < h->ncmds; i++, lc = (const struct load_command *)((const char *)lc + lc->cmdsize)) {
    if (lc->cmd == LC_SEGMENT_64) {
      const struct segment_command_64 *sg = (const struct segment_command_64 *)lc;
      if (!strcmp(sg->segname, "__TEXT")) text = sg;
      else if (!strcmp(sg->segname, "__LINKEDIT")) linkedit = sg;
    } else if (lc->cmd == LC_SYMTAB) st = (const struct symtab_command *)lc;
  }
  if (!text || !linkedit || !st) return 0;
  uintptr_t slide = (uintptr_t)h - text->vmaddr;
  lc = (const struct load_command *)(h + 1);
  for (uint32_t i = 0; i < h->ncmds; i++, lc = (const struct load_command *)((const char *)lc + lc->cmdsize)) {
    if (lc->cmd != LC_SEGMENT_64) continue;
    const struct segment_command_64 *sg = (const struct segment_command_64 *)lc;
    if (!sg->vmsize || !strcmp(sg->segname, "__PAGEZERO")) continue;
    if (slide + sg->vmaddr < mn) mn = slide + sg->vmaddr;
    if (slide + sg->vmaddr + sg->vmsize > mx) mx = slide + sg->vmaddr + sg->vmsize;
  }
  uintptr_t le = slide + linkedit->vmaddr - linkedit->fileoff;
  *lo = mn; *hi = mx;
  *syms = (const struct nlist_64 *)(le + st->symoff);
  *nsyms = st->nsyms;
  *strs = (const char *)(le + st->stroff);
  return 1;
}

/* Is every exported symbol of this image resolved, newest first, to ANOTHER
 * image? (Stops at the first one that is still its own.) */
static void *lookup_tmp(void **tmp, size_t nt, const char *name) {
  for (size_t k = 0; k < nt; k++) { void *v = dlsym(tmp[k], name); if (v) return v; }
  return NULL;
}

static int wholly_superseded(const struct mach_header_64 *h, void **tmp, size_t nt) {
  uintptr_t lo, hi; const struct nlist_64 *sy; uint32_t ns; const char *str;
  if (!image_info(h, &lo, &hi, &sy, &ns, &str) || !nt) return 0;
  int tested = 0;
  /* a quick look first, every 61st symbol: an image that still defines something
   * current (most of them) is rejected after a few lookups, not a full scan */
  for (uint32_t i = 0; i < ns; i += 61) {
    if (!(sy[i].n_type & N_EXT) || (sy[i].n_type & N_TYPE) != N_SECT) continue;
    const char *nm = str + sy[i].n_un.n_strx;
    if (nm[0] != '_' || !nm[1]) continue;
    void *now = lookup_tmp(tmp, nt, nm + 1);
    if (!now || ((uintptr_t)now >= lo && (uintptr_t)now < hi)) return 0;
  }
  for (uint32_t i = 0; i < ns; i++) {
    if (!(sy[i].n_type & N_EXT) || (sy[i].n_type & N_TYPE) != N_SECT) continue;
    const char *nm = str + sy[i].n_un.n_strx;
    if (nm[0] != '_' || !nm[1]) continue;
    /* dlsym(handle) searches ONE image on macOS, so walk the temp libraries newest
     * first (the RTS's own order); a symbol found inside this image is still current */
    void *now = lookup_tmp(tmp, nt, nm + 1);
    if (!now) return 0;                         /* cannot resolve: be conservative */
    if ((uintptr_t)now >= lo && (uintptr_t)now < hi) return 0;
    tested++;
  }
  return tested > 0;
}

static int cmp_ptr(const void *a, const void *b) {
  uintptr_t x = *(const uintptr_t *)a, y = *(const uintptr_t *)b;
  return x < y ? -1 : x > y;
}

typedef struct { uintptr_t addr; const char *name; } Exp;
typedef struct {
  uintptr_t lo, hi;
  const struct mach_header_64 *h;
  int dead;
  Exp *exps; size_t nexp;      /* exported symbols by address, built lazily */
  const char *label;
} Image;

static int cmp_exp(const void *a, const void *b) {
  uintptr_t x = ((const Exp *)a)->addr, y = ((const Exp *)b)->addr;
  return x < y ? -1 : x > y;
}

static void build_exports(Image *im) {
  uintptr_t lo, hi; const struct nlist_64 *sy; uint32_t ns; const char *str;
  if (im->exps || !image_info(im->h, &lo, &hi, &sy, &ns, &str)) return;
  uintptr_t slide = im->lo - lo + lo;  /* addresses in nlist are unslid: see below */
  (void)slide;
  size_t n = 0;
  Exp *e = malloc((size_t)ns * sizeof *e);
  if (!e) return;
  /* n_value is the link-time address; the image's slide is (header address - __TEXT vmaddr) */
  const struct load_command *lc = (const struct load_command *)(im->h + 1);
  uintptr_t tva = 0;
  for (uint32_t i = 0; i < im->h->ncmds; i++, lc = (const struct load_command *)((const char *)lc + lc->cmdsize))
    if (lc->cmd == LC_SEGMENT_64 && !strcmp(((const struct segment_command_64 *)lc)->segname, "__TEXT")) tva = ((const struct segment_command_64 *)lc)->vmaddr;
  uintptr_t sl = (uintptr_t)im->h - tva;
  for (uint32_t i = 0; i < ns; i++) {
    if (!(sy[i].n_type & N_EXT) || (sy[i].n_type & N_TYPE) != N_SECT) continue;
    const char *nm = str + sy[i].n_un.n_strx;
    if (nm[0] != '_' || !nm[1]) continue;
    e[n].addr = sy[i].n_value + sl; e[n].name = nm + 1; n++;
  }
  qsort(e, n, sizeof *e, cmp_exp);
  im->exps = e; im->nexp = n;
}

static const char *export_at(Image *im, uintptr_t a) {
  build_exports(im);
  size_t lo = 0, hi = im->nexp;
  while (lo < hi) { size_t m = (lo + hi) / 2; if (im->exps[m].addr < a) lo = m + 1; else hi = m; }
  return (lo < im->nexp && im->exps[lo].addr == a) ? im->exps[lo].name : NULL;
}

/* Returns the number of CAFs unlinked, or -1 when this is not the RTS build the
 * offsets were taken from (nothing is touched then). */
int ghs_prune_cafs_stats(int *seen, int *tmpcount) {
  uintptr_t b = rts_base();
  if (!b) return -1;
  pthread_mutex_t *sm = (pthread_mutex_t *)(b + GHS_OFF_SM_MUTEX);
  uintptr_t *dyn = (uintptr_t *)(b + GHS_OFF_DYN_CAF_LIST);
  char **loaded = (char **)(b + GHS_OFF_LOADED_OBJECTS);

  /* the temp libraries' handles, newest first (the RTS's own lookup order) */
  void **tmps = NULL; size_t nt = 0, tcap = 0;
  for (char *oc = *loaded; oc; oc = *(char **)(oc + OC_NEXT_LOADED)) {
    if (*(int *)(oc + OC_TYPE) != DYNAMIC_OBJECT) continue;
    void *h = *(void **)(oc + OC_DLOPEN_HANDLE);
    const char *fn = *(const char **)(oc + 8);   /* ObjectCode.fileName */
    if (!h || !fn || !strstr(fn, "libghc_tmp_")) continue;
    if (nt == tcap) { tcap = tcap ? tcap * 2 : 64; void **t = realloc(tmps, tcap * sizeof *tmps); if (!t) { free(tmps); return -1; } tmps = t; }
    tmps[nt++] = h;
  }
  /* and their images (address ranges), by walking dyld's list once: no dladdr */
  Image *ims = NULL; size_t ni = 0, icap = 0;
  uint32_t nimg = _dyld_image_count();
  for (uint32_t i = 0; i < nimg; i++) {
    const char *nm = _dyld_get_image_name(i);
    if (!nm || !strstr(nm, "libghc_tmp_")) continue;
    const struct mach_header_64 *h = (const struct mach_header_64 *)_dyld_get_image_header(i);
    uintptr_t lo, hi; const struct nlist_64 *sy; uint32_t ns; const char *str;
    if (!h || !image_info(h, &lo, &hi, &sy, &ns, &str)) continue;
    if (ni == icap) { icap = icap ? icap * 2 : 64; Image *t = realloc(ims, icap * sizeof *ims); if (!t) { free(tmps); free(ims); return -1; } ims = t; }
    ims[ni].lo = lo; ims[ni].hi = hi; ims[ni].h = h; ims[ni].exps = NULL; ims[ni].nexp = 0;
    ims[ni].dead = in_dead_image(lo);
    ims[ni].label = strrchr(nm, '/') ? strrchr(nm, '/') + 1 : nm;
    ni++;
  }
  int dbg = getenv("GHS_CAF_DEBUG") != NULL;
  /* judge each image once per call: wholly superseded? */
  for (size_t k = 0; k < ni; k++) {
    if (!ims[k].dead) {
      ims[k].dead = wholly_superseded(ims[k].h, tmps, nt);
      if (ims[k].dead) remember_dead(ims[k].lo, ims[k].hi);
    }
    if (dbg) fprintf(stderr, "  image %s: %s\n", ims[k].label, ims[k].dead ? "WHOLLY SUPERSEDED" : "still current");
  }

  size_t cap = 1024, nd = 0;
  uintptr_t *dead = malloc(cap * sizeof *dead);
  if (!dead) { free(tmps); free(ims); return -1; }
  int total = 0, intmp = 0;
  for (uintptr_t cur = *dyn; cur != LIST_END; cur = *(uintptr_t *)((cur & ~(uintptr_t)3) + CAF_STATIC_LINK)) {
    uintptr_t c = cur & ~(uintptr_t)3;
    total++;
    Image *im = NULL;
    for (size_t k = 0; k < ni; k++) if (c >= ims[k].lo && c < ims[k].hi) { im = &ims[k]; break; }
    if (!im) continue;
    intmp++;
    int kill = im->dead;
    if (!kill) {                       /* an exported CAF whose name now resolves elsewhere */
      const char *nm = export_at(im, c);
      if (nm) { void *now = lookup_tmp(tmps, nt, nm); if (now && (uintptr_t)now != c) kill = 1; }
    }
    if (!kill) continue;
    if (nd == cap) { cap *= 2; uintptr_t *t = realloc(dead, cap * sizeof *dead); if (!t) { free(tmps); free(ims); free(dead); return -1; } dead = t; }
    dead[nd++] = c;
  }
  for (size_t k = 0; k < ni; k++) free(ims[k].exps);
  free(ims); free(tmps);
  if (seen) *seen = total;
  if (tmpcount) *tmpcount = intmp;

  int removed = 0;
  if (nd) {
    qsort(dead, nd, sizeof *dead, cmp_ptr);
    pthread_mutex_lock(sm);
    uintptr_t *prev = dyn;
    uintptr_t cur = *dyn;
    while (cur != LIST_END) {
      uintptr_t c = cur & ~(uintptr_t)3;
      uintptr_t *link = (uintptr_t *)(c + CAF_STATIC_LINK);
      uintptr_t next = *link;
      if (bsearch(&c, dead, nd, sizeof *dead, cmp_ptr)) { *prev = next; *link = 0; removed++; }
      else prev = link;
      cur = next;
    }
    pthread_mutex_unlock(sm);
  }
  free(dead);
  return removed;
}

int ghs_prune_cafs(void) { return ghs_prune_cafs_stats(NULL, NULL); }

/* Every CAF the RTS roots, with the nearest symbol dladdr knows (for a heap census by owner:
 * Examples.MMHeap.cafReport). Fills up to `cap` entries; returns how many CAFs there are
 * (-1: not the RTS build the offsets were taken from). The addresses are static closures. */
int ghs_caf_list(uintptr_t *addrs, const char **names, int cap) {
  uintptr_t b = rts_base();
  if (!b) return -1;
  uintptr_t *dyn = (uintptr_t *)(b + GHS_OFF_DYN_CAF_LIST);
  pthread_mutex_t *sm = (pthread_mutex_t *)(b + GHS_OFF_SM_MUTEX);
  int n = 0;
  pthread_mutex_lock(sm);
  for (uintptr_t cur = *dyn; cur != LIST_END; cur = *(uintptr_t *)((cur & ~(uintptr_t)3) + CAF_STATIC_LINK)) {
    uintptr_t c = cur & ~(uintptr_t)3;
    if (n < cap) {
      Dl_info di; (void)di;
      addrs[n] = c;
      /* a name costs a dladdr (~1 ms on macOS): NULL `names` skips it, and ghs_caf_name asks for one */
      if (names) names[n] = dladdr((void *)c, &di) && di.dli_sname ? di.dli_sname : "?";
    }
    n++;
  }
  pthread_mutex_unlock(sm);
  return n;
}

/* One CAF's nearest symbol (for the few a report prints). */
const char *ghs_caf_name(uintptr_t c) {
  Dl_info di;
  return (dladdr((void *)c, &di) && di.dli_sname) ? di.dli_sname : "?";
}
