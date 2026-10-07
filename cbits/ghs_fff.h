#ifndef GHS_FFF_H
#define GHS_FFF_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Check if the FFF dynamic library is loaded and operational */
int ghs_fff_available(void);

/* Search file contents (grep) within base_dir.
   Returns 1 on success (JSON written to out_json), 0 on failure. */
int ghs_fff_grep(const char *base_dir, const char *query, int max_results, char *out_json, size_t out_max);

/* Fuzzy search file names within base_dir.
   Returns 1 on success (JSON written to out_json), 0 on failure. */
int ghs_fff_search_files(const char *base_dir, const char *query, int max_results, char *out_json, size_t out_max);

/* Shutdown/cleanup any cached handles */
void ghs_fff_cleanup(void);

#ifdef __cplusplus
}
#endif

#endif /* GHS_FFF_H */
