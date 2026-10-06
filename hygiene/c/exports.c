/* Everything in this directory that code LOADED INTO the engine may look up by name (GHC.Hygiene and
 * GHC.Hygiene.Census do, with dlsym): referenced here so the linker keeps what the engine itself never calls. */
#include <stdint.h>
extern int ghs_prune_cafs(void), ghs_prune_cafs_stats(int *, int *), ghs_caf_list(uintptr_t *, const char **, int), ghs_loader_stats(void);
extern const char *ghs_caf_name(uintptr_t);
extern void ghs_cen_reset(void), ghs_cen_done(void);
extern int ghs_cen_root(), ghs_cen_stable(), ghs_cen_nroots(void), ghs_cen_root_row(), ghs_cen_ninfo(void), ghs_cen_info_row(), ghs_cen_nstr(void), ghs_cen_str_row();
extern int64_t ghs_cen_visited(void);
extern void *ghs_store_get(const char *), *ghs_store_put_new(const char *, void *), *ghs_store_take(const char *), *ghs_store_ptr(int);
extern int ghs_store_count(void), ghs_major_gc(int), ghs_heap_auto(int);
extern double ghs_return_decay(double);
extern char *ghs_dup_all();
extern const char *ghs_store_name(int);
void *ghs_exports[] = {
  (void *)ghs_prune_cafs, (void *)ghs_prune_cafs_stats, (void *)ghs_caf_list, (void *)ghs_caf_name, (void *)ghs_loader_stats,
  (void *)ghs_cen_reset, (void *)ghs_cen_root, (void *)ghs_cen_stable, (void *)ghs_cen_nroots, (void *)ghs_cen_root_row,
  (void *)ghs_cen_ninfo, (void *)ghs_cen_info_row, (void *)ghs_cen_nstr, (void *)ghs_cen_str_row, (void *)ghs_cen_visited,
  (void *)ghs_store_get, (void *)ghs_store_put_new, (void *)ghs_store_take, (void *)ghs_store_count, (void *)ghs_store_name, (void *)ghs_store_ptr, (void *)ghs_major_gc, (void *)ghs_return_decay, (void *)ghs_heap_auto, (void *)ghs_cen_done,
  (void *)ghs_dup_all, 0 };
int ghs_exports_count(void) { int n = 0; while (ghs_exports[n]) n++; return n; }
