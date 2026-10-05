/* Everything in this directory that code LOADED INTO the engine may look up by name (GHC.Hygiene and
 * GHC.Hygiene.Census do, with dlsym): referenced here so the linker keeps what the engine itself never calls. */
#include <stdint.h>
extern int ghs_prune_cafs(void), ghs_prune_cafs_stats(int *, int *), ghs_caf_list(uintptr_t *, const char **, int), ghs_loader_stats(void);
extern const char *ghs_caf_name(uintptr_t);
extern void ghs_cen_reset(void);
extern int ghs_cen_root(), ghs_cen_stable(), ghs_cen_nroots(void), ghs_cen_root_row(), ghs_cen_ninfo(void), ghs_cen_info_row(), ghs_cen_nstr(void), ghs_cen_str_row();
extern int64_t ghs_cen_visited(void);
void *ghs_exports[] = {
  (void *)ghs_prune_cafs, (void *)ghs_prune_cafs_stats, (void *)ghs_caf_list, (void *)ghs_caf_name, (void *)ghs_loader_stats,
  (void *)ghs_cen_reset, (void *)ghs_cen_root, (void *)ghs_cen_stable, (void *)ghs_cen_nroots, (void *)ghs_cen_root_row,
  (void *)ghs_cen_ninfo, (void *)ghs_cen_info_row, (void *)ghs_cen_nstr, (void *)ghs_cen_str_row, (void *)ghs_cen_visited, 0 };
int ghs_exports_count(void) { int n = 0; while (ghs_exports[n]) n++; return n; }
