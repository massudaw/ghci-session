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
#elif defined(__ELF__)
/* ELF: the RTS shared object's full symbol table (.symtab) is in the FILE, not in the mapped image (only the
 * exported .dynsym is mapped), so the file is mapped once, read-only, and its local symbols read from there.
 * The library a ghcup GHC ships is not stripped. A symbol is at its st_value plus the load bias: where the
 * image is mapped minus the vaddr of its first loadable segment. */
#include <elf.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

static void *ghs_rts_sym(const char *name) {
  void *exported = dlsym(RTLD_DEFAULT, name);
  if (exported) return exported;
  static const unsigned char *file = NULL; static size_t flen = 0; static uintptr_t bias = 0; static int tried = 0;
  if (!tried) {
    tried = 1;
    Dl_info di;
    void *k = dlsym(RTLD_DEFAULT, "keepCAFs");
    if (!k || !dladdr(k, &di) || !di.dli_fname || !di.dli_fbase) return NULL;
    int fd = open(di.dli_fname, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return NULL;
    struct stat st;
    if (fstat(fd, &st) == 0 && (size_t)st.st_size > sizeof(Elf64_Ehdr)) {
      void *m = mmap(NULL, (size_t)st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
      if (m != MAP_FAILED) { file = (const unsigned char *)m; flen = (size_t)st.st_size; }
    }
    close(fd);
    if (!file) return NULL;
    const Elf64_Ehdr *eh = (const Elf64_Ehdr *)file;
    if (memcmp(eh->e_ident, ELFMAG, SELFMAG) || eh->e_ident[EI_CLASS] != ELFCLASS64 || eh->e_phoff + (size_t)eh->e_phnum * sizeof(Elf64_Phdr) > flen) { file = NULL; return NULL; }
    const Elf64_Phdr *ph = (const Elf64_Phdr *)(file + eh->e_phoff);
    uintptr_t first = 0; int any = 0;
    for (int i = 0; i < eh->e_phnum; i++) if (ph[i].p_type == PT_LOAD) { if (!any || ph[i].p_vaddr < first) first = ph[i].p_vaddr; any = 1; }
    bias = (uintptr_t)di.dli_fbase - (first & ~(uintptr_t)0xfff);
  }
  if (!file) return NULL;
  const Elf64_Ehdr *eh = (const Elf64_Ehdr *)file;
  if (eh->e_shoff + (size_t)eh->e_shnum * sizeof(Elf64_Shdr) > flen) return NULL;
  const Elf64_Shdr *sh = (const Elf64_Shdr *)(file + eh->e_shoff);
  for (int pass = 0; pass < 2; pass++) {               /* the full table first, the exported one after */
    for (int i = 0; i < eh->e_shnum; i++) {
      if (sh[i].sh_type != (pass == 0 ? SHT_SYMTAB : SHT_DYNSYM) || sh[i].sh_link >= eh->e_shnum) continue;
      const Elf64_Shdr *strh = &sh[sh[i].sh_link];
      if (sh[i].sh_offset + sh[i].sh_size > flen || strh->sh_offset + strh->sh_size > flen) continue;
      const Elf64_Sym *sy = (const Elf64_Sym *)(file + sh[i].sh_offset);
      const char *str = (const char *)(file + strh->sh_offset);
      size_t n = sh[i].sh_size / sizeof(Elf64_Sym);
      for (size_t j = 0; j < n; j++) {
        if (sy[j].st_shndx == SHN_UNDEF || sy[j].st_name >= strh->sh_size) continue;
        int ty = ELF64_ST_TYPE(sy[j].st_info);
        if (ty != STT_OBJECT && ty != STT_NOTYPE && ty != STT_FUNC) continue;
        if (!strcmp(str + sy[j].st_name, name)) return (void *)(bias + sy[j].st_value);
      }
    }
  }
  return NULL;
}
#else
static void *ghs_rts_sym(const char *name) { (void)name; return NULL; }
#endif
#endif
