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
 * The RTS keeps `dyn_caf_list` and `loaded_objects` private, so they are found
 * in the symbol table of the RTS image this process runs (rts_syms.h). The
 * ObjectCode / StgIndStatic layouts are GHC 9.14.1 arm64's, measured against
 * its headers.
 */
#define _DARWIN_C_SOURCE
#include "Rts.h"
#include <dlfcn.h>
#if defined(__APPLE__)
#include <mach-o/dyld.h>
#include <mach-o/loader.h>
#include <mach-o/nlist.h>
#endif
#include <pthread.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "rts_syms.h"

#define OC_TYPE 32
#define OC_NEXT_LOADED 144
#define OC_DLOPEN_HANDLE 272
#define DYNAMIC_OBJECT 1
#define CAF_STATIC_LINK 16
#define LIST_END 3u

#if defined(__APPLE__)
/* Is this CAF's value out of reach of a MINOR collection?
 *
 * A CAF the RTS retains (dyn_caf_list) is NOT put on the mutable list when it is first entered -- newCAF does
 * one or the other -- so what keeps its freshly computed value alive through minor GCs is markCAFs walking
 * that list at every collection. Take such a CAF off the list while its value is still in a young generation
 * and the next minor GC frees the value, though a live closure's SRT may still lead to the CAF: a major GC
 * (the only kind that follows SRTs) then walks a dangling pointer -- "scavenge_mark_stack: strange closure
 * type" -- or the next evaluation that enters the CAF dies. It needs old code that is still run (a thunk of
 * a superseded module, kept alive across reloads, forced after the reload) to enter a superseded CAF shortly
 * before the unlink; a major GC run at once, before any minor one, hides it by promoting the value.
 *
 * So a CAF whose value is not in the oldest generation is left on the list: a later pass takes it. */
static int value_is_old(uintptr_t c) {
  StgClosure *p = UNTAG_CLOSURE(((StgIndStatic *)c)->indirectee);
  if (!p || !HEAP_ALLOCED(p)) return 1;                 /* a static closure: nothing to free */
  return Bdescr((StgPtr)p)->gen_no == RtsFlags.GcFlags.generations - 1;
}
#endif

/* The three private things this reads, or 0 when this RTS does not have them by these names. */
static pthread_mutex_t *rts_sm; static uintptr_t *rts_dyn; static char **rts_loaded;
static int rts_found(void) {
  if (!rts_dyn) {
    rts_sm = (pthread_mutex_t *)ghs_rts_sym("sm_mutex");
    rts_loaded = (char **)ghs_rts_sym("loaded_objects");
    rts_dyn = (uintptr_t *)ghs_rts_sym("dyn_caf_list");
  }
  return rts_sm && rts_loaded && rts_dyn;
}

#if defined(__APPLE__)
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

static int image_info(const struct mach_header_64 *h, uintptr_t *lo, uintptr_t *hi,
                      const struct nlist_64 **syms, uint32_t *nsyms, const char **strs) {
  return ghs_image_info(h, lo, hi, syms, nsyms, strs, NULL);
}

/* Is every exported symbol of this image resolved, newest first, to ANOTHER
 * image? (Stops at the first one that is still its own.) */
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
    /* the temp libraries newest first (the RTS's own order), each asked for what IT defines
     * ('lookup_tmp'); a symbol found inside this image is still current */
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

/* ---- a LOCAL CAF of a superseded module, in a library that is not wholly superseded -------------
 *
 * The session's first library holds every module, so it is never wholly superseded, and the local CAFs
 * of the modules an edit relinks stayed in it for good: the first edit of a session cost their values
 * a second time (60 MB on a 101-module session), and so did the first edit of any module after it.
 *
 * A local CAF has no name to look up, but it has neighbours. The linker lays a library's data out
 * object by object, in order, so a closure that lies BETWEEN two exported closures of the same module
 * is that module's. A module is relinked whole, so it is superseded when its exported closures resolve
 * to another image. Hence: the nearest exported closure below and the nearest above, both of one module
 * (a symbol is unit_Module_name_closure; `_` inside a name is z-encoded, so the second `_` ends the
 * module), both now resolving elsewhere. A CAF at a module's edge, with a neighbour of another module,
 * is left alone. */
static size_t module_prefix(const char *nm) {
  const char *a = strchr(nm, '_');
  const char *b = a ? strchr(a + 1, '_') : NULL;
  return b ? (size_t)(b - nm) : 0;
}

static int is_closure(const char *nm) {
  size_t n = strlen(nm);
  return n > 8 && !strcmp(nm + n - 8, "_closure");
}

#define MAX_MODS 16384
typedef struct { const Image *im; const char *nm; size_t len; int superseded; } ModVerdict;   /* a module IN an image */

static int module_superseded(ModVerdict *mv, size_t *nmv, Image *im, const Exp *e, void **tmps, size_t nt) {
  size_t len = module_prefix(e->name);
  if (!len) return 0;
  for (size_t i = 0; i < *nmv; i++) if (mv[i].im == im && mv[i].len == len && !strncmp(mv[i].nm, e->name, len)) return mv[i].superseded;
  void *now = lookup_tmp(tmps, nt, e->name);
  int sup = now && !((uintptr_t)now >= im->lo && (uintptr_t)now < im->hi);
  if (*nmv < MAX_MODS) { mv[*nmv].im = im; mv[*nmv].nm = e->name; mv[*nmv].len = len; mv[*nmv].superseded = sup; (*nmv)++; }
  return sup;
}

static int local_of_superseded(Image *im, uintptr_t c, ModVerdict *mv, size_t *nmv, void **tmps, size_t nt) {
  build_exports(im);
  if (!im->exps) return 0;
  size_t lo = 0, hi = im->nexp;
  while (lo < hi) { size_t m = (lo + hi) / 2; if (im->exps[m].addr <= c) lo = m + 1; else hi = m; }
  /* lo: the first export above c. The nearest CLOSURES on each side: */
  size_t up = lo; while (up < im->nexp && !is_closure(im->exps[up].name)) up++;
  size_t dn = lo; while (dn > 0 && !is_closure(im->exps[dn - 1].name)) dn--;
  if (up >= im->nexp || dn == 0) return 0;
  const Exp *a = &im->exps[dn - 1], *b = &im->exps[up];
  size_t la = module_prefix(a->name), lb = module_prefix(b->name);
  { const char *want = getenv("GHS_CAF_DEBUG_LOCAL");       /* a local CAF's dladdr name, e.g. LQ4eC_closure */
    if (want) { Dl_info di; if (dladdr((void *)c, &di) && di.dli_sname && strstr(di.dli_sname, want))
      fprintf(stderr, "  local %s %p in %s: below %.60s (+%ld) | above %.60s (-%ld)%s\n", di.dli_sname, (void *)c, im->label, a->name + (la > 24 ? la - 24 : 0), (long)(c - a->addr), b->name + (lb > 24 ? lb - 24 : 0), (long)(b->addr - c),
              (!la || la != lb || strncmp(a->name, b->name, la)) ? " EDGE" : (module_superseded(mv, nmv, im, a, tmps, nt) ? " superseded" : " current")); } }
  if (!la || la != lb || strncmp(a->name, b->name, la)) return 0;       /* an edge, or no module */
  return module_superseded(mv, nmv, im, a, tmps, nt) && module_superseded(mv, nmv, im, b, tmps, nt);
}

/* The MODULE a closure of a temporary library belongs to, by the nearest exported closure below it (see
 * above: a library's data is laid out object by object) -- for a report that would otherwise name a
 * compiler-made local CAF "LQ4eC". Writes "Examples.Mod" (z-decoded as far as dots go); 0 if unknown. */
int ghs_caf_owner(uintptr_t c, char *out, size_t cap) {
  uint32_t nimg = _dyld_image_count();
  for (uint32_t i = 0; i < nimg; i++) {
    const struct mach_header_64 *h = (const struct mach_header_64 *)_dyld_get_image_header(i);
    uintptr_t lo, hi; const struct nlist_64 *sy; uint32_t ns; const char *str;
    if (!h || !image_info(h, &lo, &hi, &sy, &ns, &str) || c < lo || c >= hi) continue;
    Image im; memset(&im, 0, sizeof im); im.lo = lo; im.hi = hi; im.h = h;
    build_exports(&im);
    if (!im.exps) return 0;
    size_t a = 0, b = im.nexp;
    while (a < b) { size_t m = (a + b) / 2; if (im.exps[m].addr <= c) a = m + 1; else b = m; }
    while (a > 0 && !is_closure(im.exps[a - 1].name)) a--;
    int ok = 0;
    if (a > 0) {
      const char *nm = im.exps[a - 1].name; const char *u = strchr(nm, '_'); size_t len = module_prefix(nm);
      if (u && len > (size_t)(u - nm) + 1) {
        size_t k = 0;
        for (const char *q = u + 1; q < nm + len && k + 1 < cap; q++) { if (q[0] == 'z' && q[1] == 'i') { out[k++] = '.'; q++; } else out[k++] = *q; }
        out[k] = 0; ok = 1;
      }
    }
    free(im.exps);
    return ok;
  }
  return 0;
}

/* Returns the number of CAFs unlinked, or -1 when this RTS does not have the lists
 * by the names we know (nothing is touched then). */
int ghs_prune_cafs_stats(int *seen, int *tmpcount) {
  if (!rts_found()) return -1;
  pthread_mutex_t *sm = rts_sm;
  uintptr_t *dyn = rts_dyn;
  char **loaded = rts_loaded;

  /* the temp libraries' handles, newest first (the RTS's own lookup order) */
  void **tmps = NULL; size_t nt = 0, tcap = 0;
  const char **tfn = NULL;
  for (char *oc = *loaded; oc; oc = *(char **)(oc + OC_NEXT_LOADED)) {
    if (*(int *)(oc + OC_TYPE) != DYNAMIC_OBJECT) continue;
    void *h = *(void **)(oc + OC_DLOPEN_HANDLE);
    const char *fn = *(const char **)(oc + 8);   /* ObjectCode.fileName */
    if (!h || !fn || !strstr(fn, "libghc_tmp_")) continue;
    if (nt == tcap) { tcap = tcap ? tcap * 2 : 64; void **t = realloc(tmps, tcap * sizeof *tmps); if (!t) { free(tmps); free(tfn); return -1; } tmps = t;
                      const char **u = realloc(tfn, tcap * sizeof *tfn); if (!u) { free(tmps); free(tfn); return -1; } tfn = u; }
    tfn[nt] = fn;
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
    if (ni == icap) { icap = icap ? icap * 2 : 64; Image *t = realloc(ims, icap * sizeof *ims); if (!t) { free(tmps); free(tfn); free(g_tmp_lo); free(g_tmp_hi); g_tmp_lo = g_tmp_hi = NULL; free(ims); return -1; } ims = t; }
    ims[ni].lo = lo; ims[ni].hi = hi; ims[ni].h = h; ims[ni].exps = NULL; ims[ni].nexp = 0;
    ims[ni].dead = in_dead_image(lo);
    ims[ni].label = strrchr(nm, '/') ? strrchr(nm, '/') + 1 : nm;
    ni++;
  }
  /* each handle's own range ('lookup_tmp'), by the library's file name */
  g_tmp_lo = calloc(nt ? nt : 1, sizeof *g_tmp_lo); g_tmp_hi = calloc(nt ? nt : 1, sizeof *g_tmp_hi);
  if (!g_tmp_lo || !g_tmp_hi) { free(tmps); free(tfn); free(ims); free(g_tmp_lo); free(g_tmp_hi); g_tmp_lo = g_tmp_hi = NULL; return -1; }
  for (size_t k = 0; k < nt; k++) {
    const char *b = strrchr(tfn[k], '/') ? strrchr(tfn[k], '/') + 1 : tfn[k];
    for (size_t j = 0; j < ni; j++) if (!strcmp(b, ims[j].label)) { g_tmp_lo[k] = ims[j].lo; g_tmp_hi[k] = ims[j].hi; break; }
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
  if (!dead) { free(tmps); free(tfn); free(g_tmp_lo); free(g_tmp_hi); g_tmp_lo = g_tmp_hi = NULL; free(ims); return -1; }
  int total = 0, intmp = 0, young = 0, locals = 0;
  ModVerdict *mv = malloc(MAX_MODS * sizeof *mv); size_t nmv = 0;
  int per_module = mv && !getenv("GHS_CAF_WHOLE_ONLY");
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
      else if (per_module && local_of_superseded(im, c, mv, &nmv, tmps, nt)) { kill = 1; locals++; }
    }
    { const char *want = getenv("GHS_CAF_DEBUG_NAME");
      if (want) { const char *nm = export_at(im, c); if (nm && strstr(nm, want)) { void *now = lookup_tmp(tmps, nt, nm);
        fprintf(stderr, "  caf %s at %p in %s (image %s): resolves to %p -> %s\n", nm, (void *)c, im->label, im->dead ? "dead" : "current", now, kill ? "UNLINK" : "keep"); } } }
    if (!kill) continue;
    if (!value_is_old(c)) { young++; if (!getenv("GHS_CAF_UNSAFE_YOUNG")) continue; }   /* the variable: for repro/run.sh only */
    if (nd == cap) { cap *= 2; uintptr_t *t = realloc(dead, cap * sizeof *dead); if (!t) { free(tmps); free(tfn); free(g_tmp_lo); free(g_tmp_hi); g_tmp_lo = g_tmp_hi = NULL; free(ims); free(dead); free(mv); return -1; } dead = t; }
    dead[nd++] = c;
  }
  for (size_t k = 0; k < ni; k++) free(ims[k].exps);
  free(ims); free(tmps); free(tfn); free(g_tmp_lo); free(g_tmp_hi); g_tmp_lo = g_tmp_hi = NULL; free(mv);
  if (dbg) fprintf(stderr, "prune_cafs: %d local CAFs of superseded modules in libraries still partly current\n", locals);
  if (dbg || getenv("GHS_CAF_YOUNG")) fprintf(stderr, "prune_cafs: %d superseded CAFs left on the list (value still young)\n", young);
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

#else
/* ELF: the walk of the loaded images and their symbol tables is not written yet (rts_syms.h finds nothing
 * either), so the pruner says it cannot read this RTS and the daemon turns hygiene off; the census still
 * works for everything that does not need a CAF's module. */
int ghs_caf_owner(uintptr_t c, char *out, size_t cap) { (void)c; (void)out; (void)cap; return 0; }
int ghs_prune_cafs_stats(int *seen, int *tmpcount) { (void)seen; (void)tmpcount; return -1; }
#endif

int ghs_prune_cafs(void) { return ghs_prune_cafs_stats(NULL, NULL); }

/* Every CAF the RTS roots, with the nearest symbol dladdr knows (for a heap census by owner:
 * GHC.Hygiene.Census.cafReport). Fills up to `cap` entries; returns how many CAFs there are
 * (-1: an RTS we cannot read). The addresses are static closures. */
int ghs_caf_list(uintptr_t *addrs, const char **names, int cap) {
  if (!rts_found()) return -1;
  uintptr_t *dyn = rts_dyn;
  pthread_mutex_t *sm = rts_sm;
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
