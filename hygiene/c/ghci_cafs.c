#define _GNU_SOURCE   /* dladdr and Dl_info on glibc (rts_syms.h) */
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

#if defined(__APPLE__) || defined(__ELF__)
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
#if !defined(HEAP_ALLOCED)
/* GHC 9.6 keeps HeapAlloc.h among the RTS's private headers. Its test on a 64-bit RTS (USE_LARGE_ADDRESS_SPACE)
 * is a range check against `mblock_address_space`, which is private too: found by name like the lists below.
 * 1 or 0, or -1 when the range is not there to read. */
static int heap_alloced(const void *p) {
  static const W_ *range = NULL;                       /* struct mblock_address_range: begin, end, padding */
  if (!range) range = (const W_ *)ghs_rts_sym("mblock_address_space");
  if (!range) return -1;
  return (W_)p >= range[0] && (W_)p < range[1];
}
#else
static int heap_alloced(const void *p) { return HEAP_ALLOCED(p) ? 1 : 0; }
#endif

static int value_is_old(uintptr_t c) {
  StgClosure *p = UNTAG_CLOSURE(((StgIndStatic *)c)->indirectee);
  if (!p) return 1;
  int h = heap_alloced(p);
  if (h < 0) return 0;                                  /* cannot tell: left on the list, as a young value is */
  if (!h) return 1;                                     /* a static closure: nothing to free */
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
#include "caf_common.h"   /* the dead-image memo and lookup_tmp: shared with the ELF pruner below */

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

#include "caf_modules.h"   /* export_at, and a local CAF of a superseded module: shared with the ELF pruner below */

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
#if __GLASGOW_HASKELL__ < 914
  /* OC_DLOPEN_HANDLE was measured on GHC 9.14 (arm64); an older ObjectCode has it elsewhere (9.6: 24 bytes
   * earlier on x86_64), and a wrong handle here would be dlsym'd. Not measured on macOS: the pruner is off. */
  (void)seen; (void)tmpcount; return -1;
#endif
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

#elif defined(__ELF__)
/* ---- ELF: the same pruner on Linux ----------------------------------------
 *
 * What differs from Mach-O is only how the images are found and read: the loaded libraries and their address
 * ranges come from dl_iterate_phdr, and a library's exported symbols from its FILE's .dynsym (a mapped ELF
 * image has its dynamic symbol table, but not the section header that says how long it is). A loaded library
 * is never unloaded, so each is read once and kept, with its file mapped (the names point into it).
 *
 * The ObjectCode offsets above were measured on macOS arm64. The type, the file name and the link of the
 * loaded list hold on x86_64 Linux (loader_stats reads them back sensibly); the dlopen handle does not sit at
 * OC_DLOPEN_HANDLE there, and is not read: a library already loaded gives its handle to dlopen(RTLD_NOLOAD),
 * which loads nothing (its dlclose right after only gives back the reference that took). The handle is
 * then checked to name that very file; a temporary library without one leaves everything as it is (-1). */
#include <elf.h>
#include <fcntl.h>
#include <link.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "caf_common.h"

typedef struct { uintptr_t addr; const char *name; } Exp;
typedef struct {
  uintptr_t lo, hi, bias;
  const char *label;              /* the file's own name, within path */
  char path[1024];
  int dead, tried;
  Exp *exps; size_t nexp;         /* defined global symbols, by address; names point into `file` */
  const unsigned char *file; size_t flen;
} Image;

static Image *g_img = NULL; static size_t g_nimg = 0, g_imgcap = 0;

static int cmp_exp(const void *a, const void *b) {
  uintptr_t x = ((const Exp *)a)->addr, y = ((const Exp *)b)->addr;
  return x < y ? -1 : x > y;
}

/* every loaded image, once each, by its first loadable address */
static int note_image(struct dl_phdr_info *info, size_t size, void *data) {
  (void)size; (void)data;
  uintptr_t lo = (uintptr_t)-1, hi = 0;
  for (int i = 0; i < info->dlpi_phnum; i++) {
    const ElfW(Phdr) *ph = &info->dlpi_phdr[i];
    if (ph->p_type != PT_LOAD || !ph->p_memsz) continue;
    uintptr_t a = info->dlpi_addr + ph->p_vaddr, b = a + ph->p_memsz;
    if (a < lo) lo = a;
    if (b > hi) hi = b;
  }
  if (lo >= hi || !info->dlpi_name || !info->dlpi_name[0]) return 0;
  for (size_t k = 0; k < g_nimg; k++) if (g_img[k].lo == lo && !strcmp(g_img[k].path, info->dlpi_name)) return 0;
  if (g_nimg == g_imgcap) {
    size_t cap = g_imgcap ? g_imgcap * 2 : 256;
    Image *t = realloc(g_img, cap * sizeof *t);
    if (!t) return 1;
    g_img = t; g_imgcap = cap;
  }
  Image *im = &g_img[g_nimg];
  memset(im, 0, sizeof *im);
  im->lo = lo; im->hi = hi; im->bias = info->dlpi_addr;
  snprintf(im->path, sizeof im->path, "%s", info->dlpi_name);
  g_nimg++;
  return 0;
}

static void scan_images(void) {
  dl_iterate_phdr(note_image, NULL);
  for (size_t k = 0; k < g_nimg; k++) {          /* (labels after the array has stopped moving) */
    const char *b = strrchr(g_img[k].path, '/');
    g_img[k].label = b ? b + 1 : g_img[k].path;
  }
}

/* the defined global symbols of the image's .dynsym, from its file, by address */
static void build_exports(Image *im) {
  if (im->exps || im->tried) return;
  im->tried = 1;
  int fd = open(im->path, O_RDONLY | O_CLOEXEC);
  if (fd < 0) return;
  struct stat st;
  void *m = MAP_FAILED;
  if (fstat(fd, &st) == 0 && (size_t)st.st_size > sizeof(ElfW(Ehdr))) m = mmap(NULL, (size_t)st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  close(fd);
  if (m == MAP_FAILED) return;
  const unsigned char *f = m; size_t flen = (size_t)st.st_size;
  const ElfW(Ehdr) *eh = (const ElfW(Ehdr) *)f;
  if (memcmp(eh->e_ident, ELFMAG, SELFMAG) || eh->e_shoff + (size_t)eh->e_shnum * sizeof(ElfW(Shdr)) > flen) { munmap(m, flen); return; }
  const ElfW(Shdr) *sh = (const ElfW(Shdr) *)(f + eh->e_shoff);
  for (int i = 0; i < eh->e_shnum; i++) {
    if (sh[i].sh_type != SHT_DYNSYM || sh[i].sh_link >= eh->e_shnum) continue;
    const ElfW(Shdr) *strh = &sh[sh[i].sh_link];
    if (sh[i].sh_offset + sh[i].sh_size > flen || strh->sh_offset + strh->sh_size > flen) break;
    const ElfW(Sym) *sy = (const ElfW(Sym) *)(f + sh[i].sh_offset);
    const char *str = (const char *)(f + strh->sh_offset);
    size_t ns = sh[i].sh_size / sizeof(ElfW(Sym)), n = 0;
    Exp *e = malloc((ns ? ns : 1) * sizeof *e);
    if (!e) break;
    for (size_t j = 0; j < ns; j++) {
      int bind = ELF64_ST_BIND(sy[j].st_info), ty = ELF64_ST_TYPE(sy[j].st_info);
      if (sy[j].st_shndx == SHN_UNDEF || (bind != STB_GLOBAL && bind != STB_WEAK)) continue;
      if (ty != STT_OBJECT && ty != STT_FUNC && ty != STT_NOTYPE) continue;
      if (sy[j].st_name >= strh->sh_size) continue;
      const char *nm = str + sy[j].st_name;
      if (!nm[0] || nm[0] == '_') continue;         /* the linker's own (_init, _end, __bss_start): not Haskell's */
      e[n].addr = im->bias + sy[j].st_value; e[n].name = nm; n++;
    }
    qsort(e, n, sizeof *e, cmp_exp);
    im->exps = e; im->nexp = n; im->file = f; im->flen = flen;
    return;
  }
  munmap(m, flen);
}

#include "caf_modules.h"

/* Is every exported symbol of this image resolved, newest first, to ANOTHER image? */
static int wholly_superseded(Image *im, void **tmp, size_t nt) {
  build_exports(im);
  if (!im->exps || !im->nexp || !nt) return 0;
  for (size_t i = 0; i < im->nexp; i += 61) {      /* a quick look first: most images are still current */
    void *now = lookup_tmp(tmp, nt, im->exps[i].name);
    if (!now || ((uintptr_t)now >= im->lo && (uintptr_t)now < im->hi)) return 0;
  }
  for (size_t i = 0; i < im->nexp; i++) {
    void *now = lookup_tmp(tmp, nt, im->exps[i].name);
    if (!now) return 0;                            /* cannot resolve: be conservative */
    if ((uintptr_t)now >= im->lo && (uintptr_t)now < im->hi) return 0;
  }
  return 1;
}

/* The MODULE a closure belongs to, by the nearest exported closure below it in its image: "Examples.Mod". */
int ghs_caf_owner(uintptr_t c, char *out, size_t cap) {
  scan_images();
  for (size_t k = 0; k < g_nimg; k++) {
    Image *im = &g_img[k];
    if (c < im->lo || c >= im->hi) continue;
    build_exports(im);
    if (!im->exps) return 0;
    size_t a = 0, b = im->nexp;
    while (a < b) { size_t m = (a + b) / 2; if (im->exps[m].addr <= c) a = m + 1; else b = m; }
    while (a > 0 && !is_closure(im->exps[a - 1].name)) a--;
    if (a == 0) return 0;
    const char *nm = im->exps[a - 1].name; const char *u = strchr(nm, '_'); size_t len = module_prefix(nm);
    if (!u || len <= (size_t)(u - nm) + 1) return 0;
    size_t n = 0;
    for (const char *q = u + 1; q < nm + len && n + 1 < cap; q++) { if (q[0] == 'z' && q[1] == 'i') { out[n++] = '.'; q++; } else out[n++] = *q; }
    out[n] = 0;
    return 1;
  }
  return 0;
}

int ghs_prune_cafs_stats(int *seen, int *tmpcount) {
  if (!rts_found()) return -1;
  pthread_mutex_t *sm = rts_sm;
  uintptr_t *dyn = rts_dyn;
  char **loaded = rts_loaded;
  int dbg = getenv("GHS_CAF_DEBUG") != NULL;

  /* the temp libraries' handles, newest first (the RTS's own lookup order), each checked against its file */
  void **tmps = NULL; const char **tfn = NULL; size_t nt = 0, tcap = 0;
  for (char *oc = *loaded; oc; oc = *(char **)(oc + OC_NEXT_LOADED)) {
    if (*(int *)(oc + OC_TYPE) != DYNAMIC_OBJECT) continue;
    const char *fn = *(const char **)(oc + 8);    /* ObjectCode.fileName */
    if (!fn || !strstr(fn, "libghc_tmp_")) continue;
    void *h = dlopen(fn, RTLD_NOLOAD | RTLD_LAZY);
    if (h) dlclose(h);                            /* (the reference this took: GHC's own keeps it loaded) */
    struct link_map *lm = NULL;
    const char *fb = strrchr(fn, '/') ? strrchr(fn, '/') + 1 : fn;
    if (!h || dlinfo(h, RTLD_DI_LINKMAP, &lm) != 0 || !lm || !lm->l_name || strcmp(strrchr(lm->l_name, '/') ? strrchr(lm->l_name, '/') + 1 : lm->l_name, fb)) {
      if (dbg) fprintf(stderr, "prune_cafs: %s is not loaded under its own name: nothing unlinked\n", fb);
      free(tmps); free(tfn); return -1;
    }
    if (nt == tcap) { tcap = tcap ? tcap * 2 : 64; void **t = realloc(tmps, tcap * sizeof *tmps); const char **u = t ? realloc(tfn, tcap * sizeof *tfn) : NULL;
                      if (!t || !u) { free(t ? t : tmps); free(tfn); return -1; } tmps = t; tfn = u; }
    tfn[nt] = fn; tmps[nt++] = h;
  }
  scan_images();
  /* the temp libraries among the images, and each handle's own range ('lookup_tmp') */
  Image **ims = malloc((g_nimg ? g_nimg : 1) * sizeof *ims); size_t ni = 0;
  g_tmp_lo = calloc(nt ? nt : 1, sizeof *g_tmp_lo); g_tmp_hi = calloc(nt ? nt : 1, sizeof *g_tmp_hi);
  if (!ims || !g_tmp_lo || !g_tmp_hi) { free(ims); free(tmps); free(tfn); free(g_tmp_lo); free(g_tmp_hi); g_tmp_lo = g_tmp_hi = NULL; return -1; }
  for (size_t k = 0; k < g_nimg; k++) if (strstr(g_img[k].label, "libghc_tmp_")) { g_img[k].dead = g_img[k].dead || in_dead_image(g_img[k].lo); ims[ni++] = &g_img[k]; }
  for (size_t k = 0; k < nt; k++) {
    const char *b = strrchr(tfn[k], '/') ? strrchr(tfn[k], '/') + 1 : tfn[k];
    for (size_t j = 0; j < ni; j++) if (!strcmp(b, ims[j]->label)) { g_tmp_lo[k] = ims[j]->lo; g_tmp_hi[k] = ims[j]->hi; break; }
  }
  for (size_t k = 0; k < ni; k++) {
    if (!ims[k]->dead) { ims[k]->dead = wholly_superseded(ims[k], tmps, nt); if (ims[k]->dead) remember_dead(ims[k]->lo, ims[k]->hi); }
    if (dbg) fprintf(stderr, "  image %s: %s\n", ims[k]->label, ims[k]->dead ? "WHOLLY SUPERSEDED" : "still current");
  }

  size_t cap = 1024, nd = 0;
  uintptr_t *dead = malloc(cap * sizeof *dead);
  ModVerdict *mv = malloc(MAX_MODS * sizeof *mv); size_t nmv = 0;
  if (!dead) { free(ims); free(tmps); free(tfn); free(mv); free(g_tmp_lo); free(g_tmp_hi); g_tmp_lo = g_tmp_hi = NULL; return -1; }
  int total = 0, intmp = 0, young = 0, locals = 0;
  int per_module = mv && !getenv("GHS_CAF_WHOLE_ONLY");
  for (uintptr_t cur = *dyn; cur != LIST_END; cur = *(uintptr_t *)((cur & ~(uintptr_t)3) + CAF_STATIC_LINK)) {
    uintptr_t c = cur & ~(uintptr_t)3;
    total++;
    Image *im = NULL;
    for (size_t k = 0; k < ni; k++) if (c >= ims[k]->lo && c < ims[k]->hi) { im = ims[k]; break; }
    if (!im) continue;
    intmp++;
    int kill = im->dead;
    if (!kill) {                       /* an exported CAF whose name now resolves elsewhere */
      const char *nm = export_at(im, c);
      if (nm) { void *now = lookup_tmp(tmps, nt, nm); if (now && (uintptr_t)now != c) kill = 1; }
      else if (per_module && local_of_superseded(im, c, mv, &nmv, tmps, nt)) { kill = 1; locals++; }
    }
    if (!kill) continue;
    if (!value_is_old(c)) { young++; if (!getenv("GHS_CAF_UNSAFE_YOUNG")) continue; }   /* the variable: for repro/run.sh only */
    if (nd == cap) { cap *= 2; uintptr_t *t = realloc(dead, cap * sizeof *dead); if (!t) { free(dead); free(ims); free(tmps); free(tfn); free(mv); free(g_tmp_lo); free(g_tmp_hi); g_tmp_lo = g_tmp_hi = NULL; return -1; } dead = t; }
    dead[nd++] = c;
  }
  free(ims); free(tmps); free(tfn); free(mv); free(g_tmp_lo); free(g_tmp_hi); g_tmp_lo = g_tmp_hi = NULL;
  if (dbg) fprintf(stderr, "prune_cafs: %d CAFs, %d in temporary libraries, %d to unlink (%d local), %d left (value still young)\n", total, intmp, (int)nd, locals, young);
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

/* One CAF's name, for the few a report prints: its own symbol when it has one; else, for a local CAF the
 * compiler made (a floated constant), "a local CAF of Module" by the module of the exported closure below it
 * (ghs_caf_owner) -- not dladdr's answer, which is the NEAREST symbol below and so another value's name. */
const char *ghs_caf_name(uintptr_t c) {
  static __thread char buf[320];
  Dl_info di;
  if (dladdr((void *)c, &di) && di.dli_sname && (uintptr_t)di.dli_saddr == c) return di.dli_sname;
  char mod[256];
  if (ghs_caf_owner(c, mod, sizeof mod)) { snprintf(buf, sizeof buf, "a local CAF of %s", mod); return buf; }
  if (di.dli_fname) { const char *b = strrchr(di.dli_fname, '/'); snprintf(buf, sizeof buf, "a local CAF in %s", b ? b + 1 : di.dli_fname); return buf; }
  return "?";
}
