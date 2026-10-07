/* The parts of the pruner that work on an image's exported symbols, whatever the object format: the symbol
 * at an address, and whether a LOCAL CAF belongs to a superseded module. ghci_cafs.c includes this once, after
 * its format's `Image`, `Exp` and `build_exports` (Mach-O or ELF). */
#ifndef GHS_CAF_MODULES_H
#define GHS_CAF_MODULES_H

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
#endif
