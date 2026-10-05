/* The RTS's private symbols, found at run time.
 *
 * The RTS keeps `dyn_caf_list`, `loaded_objects` and their like out of its export list, so `dlsym` cannot
 * give them. Their names are still in the symbol table of the RTS image this process has mapped (a dylib's
 * __LINKEDIT is mapped whole), so they are read from there: no offsets taken from `nm` at build time, and
 * nothing to rebuild for another build of the RTS. The image is the one that holds `keepCAFs` (exported).
 * NULL when the symbol, or the table, is not there: every caller then does nothing. */
#ifndef GHS_RTS_SYMS_H
#define GHS_RTS_SYMS_H
#include <dlfcn.h>
#include <stdint.h>
#include <string.h>

#if defined(__APPLE__)
#include <mach-o/loader.h>
#include <mach-o/nlist.h>

/* [lo,hi) of the image's segments, its symbol table, and its slide (a symbol is at n_value + slide) */
static int ghs_image_info(const struct mach_header_64 *h, uintptr_t *lo, uintptr_t *hi,
                          const struct nlist_64 **syms, uint32_t *nsyms, const char **strs, uintptr_t *slide_out) {
  const struct load_command *lc = (const struct load_command *)(h + 1);
  const struct segment_command_64 *text = NULL, *linkedit = NULL;
  const struct symtab_command *st = NULL;
  uintptr_t mn = (uintptr_t)-1, mx = 0;
  if (h->magic != MH_MAGIC_64) return 0;
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
  if (lo) *lo = mn;
  if (hi) *hi = mx;
  *syms = (const struct nlist_64 *)(le + st->symoff);
  *nsyms = st->nsyms;
  *strs = (const char *)(le + st->stroff);
  if (slide_out) *slide_out = slide;
  return 1;
}

static void *ghs_rts_sym(const char *name) {
  void *exported = dlsym(RTLD_DEFAULT, name);
  if (exported) return exported;
  Dl_info di;
  void *k = dlsym(RTLD_DEFAULT, "keepCAFs");
  if (!k || !dladdr(k, &di) || !di.dli_fbase) return NULL;
  const struct nlist_64 *sy; uint32_t ns; const char *str; uintptr_t slide;
  if (!ghs_image_info((const struct mach_header_64 *)di.dli_fbase, NULL, NULL, &sy, &ns, &str, &slide)) return NULL;
  for (uint32_t i = 0; i < ns; i++) {
    if ((sy[i].n_type & N_STAB) || (sy[i].n_type & N_TYPE) != N_SECT) continue;
    const char *nm = str + sy[i].n_un.n_strx;
    if (nm[0] == '_' && !strcmp(nm + 1, name)) return (void *)(sy[i].n_value + slide);
  }
  return NULL;
}
#else
static void *ghs_rts_sym(const char *name) { (void)name; return NULL; }   /* ELF: not written yet */
#endif
#endif
