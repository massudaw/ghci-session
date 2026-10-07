#include "ghs_fff.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/stat.h>
#include <limits.h>

typedef struct FffResult {
  bool success;
  char *error;
  void *handle;
  int64_t int_value;
} FffResult;

typedef struct FffFileItem {
  char *relative_path;
  char *file_name;
  char *git_status;
  uint64_t size;
  uint64_t modified;
  int64_t access_frecency_score;
  int64_t modification_frecency_score;
  int64_t total_frecency_score;
  bool is_binary;
} FffFileItem;

typedef struct FffSearchResult {
  struct FffFileItem *items;
  void *scores;
  uint32_t count;
  uint32_t total_matched;
  uint32_t total_files;
} FffSearchResult;

typedef struct FffGrepMatch {
  char *relative_path;
  char *file_name;
  char *git_status;
  char *line_content;
  void *match_ranges;
  char **context_before;
  char **context_after;
  uint64_t size;
  uint64_t modified;
  int64_t total_frecency_score;
  int64_t access_frecency_score;
  int64_t modification_frecency_score;
  uint64_t line_number;
  uint64_t byte_offset;
  uint32_t col;
  uint32_t match_ranges_count;
  uint32_t context_before_count;
  uint32_t context_after_count;
  uint16_t fuzzy_score;
  bool has_fuzzy_score;
  bool is_binary;
  bool is_definition;
} FffGrepMatch;

typedef struct FffGrepResult {
  struct FffGrepMatch *items;
  uint32_t count;
  uint32_t total_matched;
  uint32_t total_files_searched;
  uint32_t total_files;
  uint32_t filtered_file_count;
  uint32_t next_file_offset;
  char *regex_fallback_error;
} FffGrepResult;

typedef FffResult *(*fn_create)(const char *, const char *, const char *, bool, bool, bool, bool, bool);
typedef void (*fn_destroy)(void *);
typedef FffResult *(*fn_wait)(void *, uint64_t);
typedef FffResult *(*fn_search)(void *, const char *, const char *, uint32_t, uint32_t, uint32_t, int32_t, uint32_t);
typedef FffResult *(*fn_grep)(void *, const char *, uint8_t, uint64_t, uint32_t, bool, uint32_t, uint32_t, uint64_t, uint32_t, uint32_t, bool);
typedef void (*fn_free_res)(FffResult *);

static void *s_lib = NULL;
static int s_loaded = -1; /* -1 = untested, 0 = failed, 1 = loaded */

static fn_create s_fn_create = NULL;
static fn_destroy s_fn_destroy = NULL;
static fn_wait s_fn_wait = NULL;
static fn_search s_fn_search = NULL;
static fn_grep s_fn_grep = NULL;
static fn_free_res s_fn_free_res = NULL;

static char s_cached_dir[PATH_MAX] = {0};
static void *s_cached_instance = NULL;

static void try_load_lib(void) {
    if (s_loaded != -1) return;

    const char *paths[] = {
        ".bin/libfff.dylib",
        "lib/libfff.dylib",
        "/Users/wesleymassuda/.local/lib/libfff.dylib",
        "/opt/homebrew/lib/libfff.dylib",
        "libfff.dylib",
        ".bin/libfff.so",
        "lib/libfff.so",
        "libfff.so",
        NULL
    };

    for (int i = 0; paths[i] != NULL; i++) {
        s_lib = dlopen(paths[i], RTLD_NOW | RTLD_LOCAL);
        if (s_lib) break;
    }

    if (!s_lib) {
        s_loaded = 0;
        return;
    }

    s_fn_create = (fn_create)dlsym(s_lib, "fff_create_instance");
    s_fn_destroy = (fn_destroy)dlsym(s_lib, "fff_destroy");
    s_fn_wait = (fn_wait)dlsym(s_lib, "fff_wait_for_scan");
    s_fn_search = (fn_search)dlsym(s_lib, "fff_search");
    s_fn_grep = (fn_grep)dlsym(s_lib, "fff_live_grep");
    s_fn_free_res = (fn_free_res)dlsym(s_lib, "fff_free_result");

    if (s_fn_create && s_fn_destroy && s_fn_wait && s_fn_search && s_fn_grep && s_fn_free_res) {
        s_loaded = 1;
    } else {
        dlclose(s_lib);
        s_lib = NULL;
        s_loaded = 0;
    }
}

int ghs_fff_available(void) {
    try_load_lib();
    return s_loaded == 1 ? 1 : 0;
}

static void *get_or_create_instance(const char *dir) {
    if (!ghs_fff_available()) return NULL;
    if (!dir || strlen(dir) == 0) dir = ".";

    char real_path[PATH_MAX];
    if (!realpath(dir, real_path)) {
        strncpy(real_path, dir, sizeof(real_path) - 1);
        real_path[sizeof(real_path) - 1] = '\0';
    }

    if (s_cached_instance && strcmp(s_cached_dir, real_path) == 0) {
        return s_cached_instance;
    }

    if (s_cached_instance) {
        s_fn_destroy(s_cached_instance);
        s_cached_instance = NULL;
        s_cached_dir[0] = '\0';
    }

    FffResult *r = s_fn_create(real_path, NULL, NULL, true, true, false, true, false);
    if (!r || !r->success || !r->handle) {
        if (r) s_fn_free_res(r);
        return NULL;
    }

    void *inst = r->handle;
    s_fn_free_res(r);

    FffResult *wr = s_fn_wait(inst, 5000);
    if (wr) s_fn_free_res(wr);

    s_cached_instance = inst;
    strncpy(s_cached_dir, real_path, sizeof(s_cached_dir) - 1);
    return s_cached_instance;
}

static void json_escape(const char *src, char *dst, size_t dst_max) {
    size_t d = 0;
    for (size_t s = 0; src && src[s] != '\0' && d + 6 < dst_max; s++) {
        unsigned char c = (unsigned char)src[s];
        if (c == '"') { dst[d++] = '\\'; dst[d++] = '"'; }
        else if (c == '\\') { dst[d++] = '\\'; dst[d++] = '\\'; }
        else if (c == '\n') { dst[d++] = '\\'; dst[d++] = 'n'; }
        else if (c == '\r') { dst[d++] = '\\'; dst[d++] = 'r'; }
        else if (c == '\t') { dst[d++] = '\\'; dst[d++] = 't'; }
        else if (c < 32) {
            d += snprintf(dst + d, dst_max - d, "\\u%04x", c);
        } else {
            dst[d++] = (char)c;
        }
    }
    dst[d] = '\0';
}

int ghs_fff_grep(const char *base_dir, const char *query, int max_results, char *out_json, size_t out_max) {
    if (!out_json || out_max == 0) return 0;
    if (!query) {
        snprintf(out_json, out_max, "{\"ok\":false,\"error\":\"empty query\"}");
        return 0;
    }

    void *inst = get_or_create_instance(base_dir);
    if (!inst) {
        snprintf(out_json, out_max, "{\"ok\":false,\"error\":\"fff library not available or failed to index directory\"}");
        return 0;
    }

    if (max_results <= 0) max_results = 30;

    FffResult *gr = s_fn_grep(inst, query, 0, 0, 0, true, 0, (uint32_t)max_results, 0, 0, 0, false);
    if (!gr || !gr->success || !gr->handle) {
        char err_esc[256] = {0};
        json_escape(gr && gr->error ? gr->error : "unknown grep error", err_esc, sizeof(err_esc));
        snprintf(out_json, out_max, "{\"ok\":false,\"error\":\"%s\"}", err_esc);
        if (gr) s_fn_free_res(gr);
        return 0;
    }

    FffGrepResult *gres = (FffGrepResult *)gr->handle;
    char q_esc[256];
    json_escape(query, q_esc, sizeof(q_esc));

    size_t pos = snprintf(out_json, out_max,
        "{\"ok\":true,\"query\":\"%s\",\"mode\":\"grep\",\"count\":%u,\"total_matched\":%u,\"files_searched\":%u,\"items\":[",
        q_esc, gres->count, gres->total_matched, gres->total_files_searched);

    for (uint32_t i = 0; i < gres->count && pos + 256 < out_max; i++) {
        FffGrepMatch *m = &gres->items[i];
        char p_esc[512], c_esc[1024];
        json_escape(m->relative_path ? m->relative_path : "", p_esc, sizeof(p_esc));
        json_escape(m->line_content ? m->line_content : "", c_esc, sizeof(c_esc));

        pos += snprintf(out_json + pos, out_max - pos,
            "%s{\"path\":\"%s\",\"line\":%llu,\"content\":\"%s\",\"git_status\":\"%s\",\"frecency\":%lld}",
            (i > 0 ? "," : ""),
            p_esc,
            (unsigned long long)m->line_number,
            c_esc,
            (m->git_status ? m->git_status : "clean"),
            (long long)m->total_frecency_score);
    }

    if (pos + 3 < out_max) {
        strcat(out_json, "]}");
    }

    s_fn_free_res(gr);
    return 1;
}

int ghs_fff_search_files(const char *base_dir, const char *query, int max_results, char *out_json, size_t out_max) {
    if (!out_json || out_max == 0) return 0;
    if (!query) {
        snprintf(out_json, out_max, "{\"ok\":false,\"error\":\"empty query\"}");
        return 0;
    }

    void *inst = get_or_create_instance(base_dir);
    if (!inst) {
        snprintf(out_json, out_max, "{\"ok\":false,\"error\":\"fff library not available or failed to index directory\"}");
        return 0;
    }

    if (max_results <= 0) max_results = 20;

    FffResult *sr = s_fn_search(inst, query, NULL, 0, 0, (uint32_t)max_results, 0, 0);
    if (!sr || !sr->success || !sr->handle) {
        char err_esc[256] = {0};
        json_escape(sr && sr->error ? sr->error : "unknown search error", err_esc, sizeof(err_esc));
        snprintf(out_json, out_max, "{\"ok\":false,\"error\":\"%s\"}", err_esc);
        if (sr) s_fn_free_res(sr);
        return 0;
    }

    FffSearchResult *sres = (FffSearchResult *)sr->handle;
    char q_esc[256];
    json_escape(query, q_esc, sizeof(q_esc));

    size_t pos = snprintf(out_json, out_max,
        "{\"ok\":true,\"query\":\"%s\",\"mode\":\"files\",\"count\":%u,\"total_matched\":%u,\"total_files\":%u,\"items\":[",
        q_esc, sres->count, sres->total_matched, sres->total_files);

    for (uint32_t i = 0; i < sres->count && pos + 256 < out_max; i++) {
        FffFileItem *it = &sres->items[i];
        char p_esc[512];
        json_escape(it->relative_path ? it->relative_path : "", p_esc, sizeof(p_esc));

        pos += snprintf(out_json + pos, out_max - pos,
            "%s{\"path\":\"%s\",\"git_status\":\"%s\",\"frecency\":%lld,\"size\":%llu}",
            (i > 0 ? "," : ""),
            p_esc,
            (it->git_status ? it->git_status : "clean"),
            (long long)it->total_frecency_score,
            (unsigned long long)it->size);
    }

    if (pos + 3 < out_max) {
        strcat(out_json, "]}");
    }

    s_fn_free_res(sr);
    return 1;
}

void ghs_fff_cleanup(void) {
    if (s_cached_instance && s_fn_destroy) {
        s_fn_destroy(s_cached_instance);
        s_cached_instance = NULL;
        s_cached_dir[0] = '\0';
    }
    if (s_lib) {
        dlclose(s_lib);
        s_lib = NULL;
        s_loaded = -1;
    }
}
