/* A heap census in C: what a value (or every CAF, or every StablePtr-held value) retains, by
 * constructor, and the Strings among it -- GHC.Hygiene.Census.cafReport / keptReport.
 *
 * Why C. The same walk in Haskell (GHC.Exts.Heap) costs ~3 us and ~3 KB of allocation a closure,
 * a StableName a closure (the RTS table is scanned by every major GC), and took minutes over the
 * repl's heap. Here a closure costs a hash probe and a switch.
 *
 * Why it is safe to hold addresses: the entry points are called as `unsafe` foreign calls, which
 * keep the capability, so no collection runs while a walk is in progress and nothing moves. The
 * visited set is keyed by address. Never call these from a `safe` call.
 *
 * What is traversed: the pointer fields of constructors, functions, thunks, selector thunks,
 * indirections (a CAF's root is one), boxed arrays, mutable variables, MVars and TVars. NOT: PAP/AP
 * argument payloads (only the function), stacks, TSOs, weak pointers, BCOs, SRTs -- the code a
 * closure refers to is not data. So a number here is a LOWER bound for a structure built from
 * partial applications, exact for plain data.
 *
 * Built by build.sh against the RTS headers of the GHC in use (libghscensus.dylib); optional. */
#include "Rts.h"
#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ---- the visited set: addresses, open addressing ---- */
static uintptr_t *vs_keys; static size_t vs_cap, vs_n;
static size_t vs_hash(uintptr_t k, size_t cap) { return (size_t)(((k >> 3) * 0x9E3779B97F4A7C15ULL) >> 20) & (cap - 1); }
static int vs_grow(void) {
  size_t ncap = vs_cap ? vs_cap * 2 : (1u << 20);
  uintptr_t *nk = calloc(ncap, sizeof *nk);
  if (!nk) return 0;
  for (size_t i = 0; i < vs_cap; i++) if (vs_keys[i]) {
    size_t h = vs_hash(vs_keys[i], ncap);
    while (nk[h]) h = (h + 1) & (ncap - 1);
    nk[h] = vs_keys[i];
  }
  free(vs_keys); vs_keys = nk; vs_cap = ncap;
  return 1;
}
/* 1 if k was new */
static int vs_add(uintptr_t k) {
  if ((vs_n + 1) * 2 > vs_cap && !vs_grow()) return 0;
  size_t h = vs_hash(k, vs_cap);
  while (vs_keys[h]) { if (vs_keys[h] == k) return 0; h = (h + 1) & (vs_cap - 1); }
  vs_keys[h] = k; vs_n++;
  return 1;
}

/* ---- per info table: bytes and count ---- */
typedef struct { uintptr_t info; uint64_t bytes, count; char label[96]; int kind; } Info;   /* kind: 0 other, 1 cons, 2 Char */
#define MAX_INFO 8192
static Info infos[MAX_INFO]; static int n_info;
static const char *type_name(int t) {
  switch (t) {
    case ARR_WORDS: return "ARR_WORDS"; case IND: return "IND"; case IND_STATIC: return "IND_STATIC";
    case BLACKHOLE: return "BLACKHOLE"; case THUNK: return "THUNK"; case THUNK_1_0: return "THUNK_1_0";
    case THUNK_0_1: return "THUNK_0_1"; case THUNK_2_0: return "THUNK_2_0"; case THUNK_1_1: return "THUNK_1_1";
    case THUNK_0_2: return "THUNK_0_2"; case THUNK_SELECTOR: return "THUNK_SELECTOR"; case FUN: return "FUN";
    case FUN_1_0: return "FUN_1_0"; case FUN_0_1: return "FUN_0_1"; case FUN_2_0: return "FUN_2_0";
    case FUN_1_1: return "FUN_1_1"; case FUN_0_2: return "FUN_0_2"; case FUN_STATIC: return "FUN_STATIC";
    case PAP: return "PAP"; case AP: return "AP"; case AP_STACK: return "AP_STACK";
    case MUT_ARR_PTRS_CLEAN: case MUT_ARR_PTRS_DIRTY: case MUT_ARR_PTRS_FROZEN_CLEAN: case MUT_ARR_PTRS_FROZEN_DIRTY: return "MUT_ARR_PTRS";
    case SMALL_MUT_ARR_PTRS_CLEAN: case SMALL_MUT_ARR_PTRS_DIRTY: case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN: case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY: return "SMALL_MUT_ARR_PTRS";
    case MUT_VAR_CLEAN: case MUT_VAR_DIRTY: return "MUT_VAR"; case MVAR_CLEAN: case MVAR_DIRTY: return "MVAR";
    case TVAR: return "TVAR"; case WEAK: return "WEAK"; case BCO: return "BCO"; case STACK: return "STACK"; case TSO: return "TSO";
    default: return NULL;
  }
}
static Info *info_of(StgClosure *p) {
  const StgInfoTable *it = get_itbl(p);
  uintptr_t key = (uintptr_t)p->header.info;
  size_t h = (size_t)((key >> 3) * 0x9E3779B97F4A7C15ULL >> 40) & (MAX_INFO - 1);
  for (int probe = 0; probe < MAX_INFO; probe++, h = (h + 1) & (MAX_INFO - 1)) {
    Info *e = &infos[h];
    if (e->info == key) return e;
    if (!e->info) {
      e->info = key; n_info++;
      int t = it->type;
      if (t >= CONSTR && t <= CONSTR_NOCAF) {
        const char *d = GET_CON_DESC(get_con_itbl(p));
        snprintf(e->label, sizeof e->label, "%s", d ? d : "?");
        size_t n = strlen(e->label);
        /* the descriptor is "package:Module.Name": the list cell is GHC.Internal.Types.: and the
         * character box GHC.Internal.Types.C# (GHC.Types before ghc-internal) */
        if (n >= 7 && strcmp(e->label + n - 7, "Types.:") == 0) e->kind = 1;
        else if (n >= 8 && strcmp(e->label + n - 8, "Types.C#") == 0) e->kind = 2;
      } else {
        const char *tn = type_name(t);
        if (tn) snprintf(e->label, sizeof e->label, "%s", tn); else snprintf(e->label, sizeof e->label, "TYPE_%d", t);
      }
      return e;
    }
  }
  return &infos[0];
}

/* ---- the strings found: grouped by their first 40 characters ---- */
typedef struct { char key[44]; uint64_t chars, count; int root; } Str;
#define MAX_STR 262144
static Str strs[MAX_STR]; static int n_str; static Str str_other;   /* what did not fit: one bucket, never a scan */
static void str_add(const char *key, int klen, uint64_t len, int root) {
  char k[44]; memset(k, 0, sizeof k); memcpy(k, key, klen < 40 ? klen : 40);
  size_t h = 5381; for (int i = 0; i < 40; i++) h = h * 33 + (unsigned char)k[i];
  h &= MAX_STR - 1;
  for (int probe = 0; probe < 32; probe++, h = (h + 1) & (MAX_STR - 1)) {
    Str *s = &strs[h];
    if (s->count == 0) { memcpy(s->key, k, sizeof k); s->chars = len; s->count = 1; s->root = root; n_str++; return; }
    if (memcmp(s->key, k, 40) == 0) { s->chars += len; s->count++; return; }
  }
  if (!str_other.count) { strcpy(str_other.key, "(other: table full)"); str_other.root = root; }
  str_other.chars += len; str_other.count++;
}

/* ---- roots ---- */
typedef struct { char label[128]; uint64_t bytes, count, cons_bytes, arr_bytes; int stopped; } Root;
#define MAX_ROOT 65536
static Root roots[MAX_ROOT]; static int n_root;

/* ---- the work stack ---- */
static uintptr_t *stk; static size_t stk_cap, stk_n;
static void push(StgClosure *c) {
  uintptr_t u = (uintptr_t)c & ~(uintptr_t)7;
  if (u < 4096) return;
  if (stk_n == stk_cap) { size_t nc = stk_cap ? stk_cap * 2 : (1u << 16); uintptr_t *ns = realloc(stk, nc * sizeof *ns); if (!ns) return; stk = ns; stk_cap = nc; }
  __builtin_prefetch((const void *)u);
  stk[stk_n++] = u;
}

static int is_static_box(StgClosure *p) {
  uintptr_t a = (uintptr_t)p;
  return (a >= (uintptr_t)&stg_CHARLIKE_closure[0] && a < (uintptr_t)&stg_CHARLIKE_closure[MAX_CHARLIKE - MIN_CHARLIKE + 1])
      || (a >= (uintptr_t)&stg_INTLIKE_closure[0] && a < (uintptr_t)&stg_INTLIKE_closure[MAX_INTLIKE - MIN_INTLIKE + 1]);
}

static StgClosure *through(StgClosure *c) {   /* follow indirections */
  for (int i = 0; i < 64; i++) {
    c = (StgClosure *)((uintptr_t)c & ~(uintptr_t)7);
    if ((uintptr_t)c < 4096) return c;
    int t = get_itbl(c)->type;
    if (t == IND || t == IND_STATIC || t == BLACKHOLE) c = ((StgInd *)c)->indirectee; else break;
  }
  return c;
}

void ghs_cen_reset(void) {
  if (vs_keys) memset(vs_keys, 0, vs_cap * sizeof *vs_keys);
  vs_n = 0; memset(infos, 0, sizeof infos); n_info = 0; memset(strs, 0, sizeof strs); memset(&str_other, 0, sizeof str_other); n_str = 0; n_root = 0; stk_n = 0;
}

/* Walk what `root` retains and has not been counted by an earlier root. Returns the root's index. */
int ghs_cen_root(StgClosure *root, const char *label, int64_t cap) {
  if (n_root >= MAX_ROOT) return -1;
  int ri = n_root++;
  Root *r = &roots[ri]; memset(r, 0, sizeof *r); snprintf(r->label, sizeof r->label, "%s", label ? label : "?");
  stk_n = 0; push(root);
  while (stk_n) {
    StgClosure *p = (StgClosure *)stk[--stk_n];
    if (!vs_add((uintptr_t)p)) continue;
    if ((int64_t)r->count >= cap) { r->stopped = 1; break; }
    const StgInfoTable *info = get_itbl(p);
    Info *e = info_of(p);
    int t = info->type;
    uint64_t bytes = is_static_box(p) ? 0 : (uint64_t)closure_sizeW(p) * 8;
    e->bytes += bytes; e->count++; r->bytes += bytes; r->count++;
    if (e->kind == 1) r->cons_bytes += bytes;
    if (t == ARR_WORDS) r->arr_bytes += bytes;
    switch (t) {
      case CONSTR: case CONSTR_1_0: case CONSTR_0_1: case CONSTR_2_0: case CONSTR_1_1: case CONSTR_0_2: case CONSTR_NOCAF: {
        if (e->kind == 1) {   /* a cons: is it a String? */
          StgClosure *h = through(((StgClosure **)p->payload)[0]);
          if ((uintptr_t)h >= 4096 && info_of(h)->kind == 2) {
            char key[40]; int kl = 0; uint64_t len = 0; StgClosure *q = p; uint64_t cb = 0;
            for (;;) {
              StgClosure *hd = through(((StgClosure **)q->payload)[0]);
              if ((uintptr_t)hd < 4096 || info_of(hd)->kind != 2) break;
              if (len > 0 && !vs_add((uintptr_t)q)) break;
              if (kl < 40) { StgWord ch = (StgWord)hd->payload[0]; key[kl++] = ch < 128 && ch >= 32 ? (char)ch : '?'; }
              len++; cb += 24;
              StgClosure *tl = through(((StgClosure **)q->payload)[1]);
              if ((uintptr_t)tl < 4096) { q = NULL; break; }
              if (info_of(tl)->kind != 1) { push(tl); q = NULL; break; }
              q = tl;
            }
            if (q) push(q);
            e->bytes += cb - 24 * (len ? 1 : 0); e->count += len ? len - 1 : 0; r->bytes += cb - 24; r->count += len ? len - 1 : 0; r->cons_bytes += cb - 24;
            str_add(key, kl, len, ri);
            break;
          }
        }
        for (uint32_t i = 0; i < info->layout.payload.ptrs; i++) push(((StgClosure **)p->payload)[i]);
        break;
      }
      case FUN: case FUN_1_0: case FUN_0_1: case FUN_2_0: case FUN_1_1: case FUN_0_2:
        for (uint32_t i = 0; i < info->layout.payload.ptrs; i++) push(((StgClosure **)p->payload)[i]);
        break;
      case THUNK: case THUNK_1_0: case THUNK_0_1: case THUNK_2_0: case THUNK_1_1: case THUNK_0_2:
        for (uint32_t i = 0; i < info->layout.payload.ptrs; i++) push(((StgThunk *)p)->payload[i]);
        break;
      case THUNK_SELECTOR: push(((StgSelector *)p)->selectee); break;
      case IND: case IND_STATIC: case BLACKHOLE: push(((StgInd *)p)->indirectee); break;
      case AP: push(((StgAP *)p)->fun); break;
      case PAP: push(((StgPAP *)p)->fun); break;
      case AP_STACK: push(((StgAP_STACK *)p)->fun); break;
      case MUT_ARR_PTRS_CLEAN: case MUT_ARR_PTRS_DIRTY: case MUT_ARR_PTRS_FROZEN_CLEAN: case MUT_ARR_PTRS_FROZEN_DIRTY: {
        StgMutArrPtrs *a = (StgMutArrPtrs *)p;
        for (StgWord i = 0; i < a->ptrs; i++) push(a->payload[i]);
        break;
      }
      case SMALL_MUT_ARR_PTRS_CLEAN: case SMALL_MUT_ARR_PTRS_DIRTY: case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN: case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY: {
        StgSmallMutArrPtrs *a = (StgSmallMutArrPtrs *)p;
        for (StgWord i = 0; i < a->ptrs; i++) push(a->payload[i]);
        break;
      }
      case MUT_VAR_CLEAN: case MUT_VAR_DIRTY: push(((StgMutVar *)p)->var); break;
      case MVAR_CLEAN: case MVAR_DIRTY: push((StgClosure *)((StgMVar *)p)->value); break;
      case TVAR: push(((StgTVar *)p)->current_value); break;
      default: break;
    }
  }
  return ri;
}

/* A StablePtr-held value (GHC.Hygiene.Census.keep). */
int ghs_cen_stable(void *sp, const char *label, int64_t cap) {
  StgClosure *c = (StgClosure *)deRefStablePtr((StgStablePtr)sp);
  return ghs_cen_root(c, label, cap);
}

/* ---- results ---- */
int ghs_cen_nroots(void) { return n_root; }
int ghs_cen_root_row(int i, char *label, int64_t *out) {
  if (i < 0 || i >= n_root) return 0;
  Root *r = &roots[i]; snprintf(label, 128, "%s", r->label);
  out[0] = r->bytes; out[1] = r->count; out[2] = r->cons_bytes; out[3] = r->arr_bytes; out[4] = r->stopped;
  return 1;
}
int ghs_cen_ninfo(void) { return MAX_INFO; }
int ghs_cen_info_row(int i, char *label, int64_t *out) {   /* i over 0..MAX_INFO: 0 for an empty slot */
  if (i < 0 || i >= MAX_INFO || !infos[i].info) return 0;
  snprintf(label, 96, "%s", infos[i].label); out[0] = infos[i].bytes; out[1] = infos[i].count;
  return 1;
}
int ghs_cen_nstr(void) { return MAX_STR + 1; }
int ghs_cen_str_row(int i, char *key, char *rootlabel, int64_t *out) {
  const Str *s = i == MAX_STR ? &str_other : (i >= 0 && i < MAX_STR ? &strs[i] : NULL);
  if (!s || !s->count) return 0;
  memcpy(key, s->key, 44); snprintf(rootlabel, 128, "%s", s->root >= 0 && s->root < n_root ? roots[s->root].label : "?");
  out[0] = s->chars; out[1] = s->count;
  return 1;
}
int64_t ghs_cen_visited(void) { return (int64_t)vs_n; }

/* Give back what a census allocated to do its walk: the visited set is 8 bytes a slot and doubles as it
 * fills -- 128 MB for a few million closures -- and it stayed allocated for the life of the session after
 * the first `mem` or `census`. The rows already gathered (roots, constructors, strings) are kept. */
void ghs_cen_done(void) {
  free(vs_keys); vs_keys = 0; vs_cap = 0; vs_n = 0;
  free(stk); stk = 0; stk_cap = 0; stk_n = 0;
}
