/* Sharing that is missed: closures that are structurally EQUAL to one already in the heap.
 *
 * Every data closure gets a hash of what it IS, bottom-up: its constructor (by NAME, so the same
 * constructor of two generations of a reloaded module is one), its non-pointer words, and the hashes of
 * what its pointer fields lead to; a byte array, its length and bytes. Two closures with one hash are
 * the same value built twice. What cannot be compared structurally -- a thunk, a function, a partial
 * application, anything mutable -- is only ever equal to itself.
 *
 * So for a set of roots this says how many bytes maximal sharing would give back (every closure but one
 * of each class), which constructors they are, which ROOT holds copies of what already exists elsewhere
 * (roots are walked in order: a closure's class belongs to the first root that reaches it), and the
 * largest repeated values, shown.
 *
 * The walk is post-order over an explicit stack (a list is as deep as it is long). A cycle through data
 * (a knot) is cut where it closes: the back edge counts as identity, so two equal cyclic structures are
 * not found equal -- an undercount, never a false match. Hash collisions (64 bits) are ignored.
 *
 * As heap_census.c: called from `unsafe` foreign calls, so nothing moves while it runs. */
#include "Rts.h"
#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIX(h, w) (((h) ^ (uint64_t)(w)) * 0x100000001b3ULL)
static uint64_t fin(uint64_t h) { h ^= h >> 33; h *= 0xff51afd7ed558ccdULL; h ^= h >> 33; return h ? h : 1; }

/* ---- a closure's record: its hash and the size of the value below it, unshared ---- */
typedef struct { uintptr_t addr; uint64_t hash; uint64_t tree; uint64_t xt; } Node;   /* hash 0: in progress. xt: the bytes below (and of) this closure that are EXTRA copies */
static Node *nodes; static size_t n_cap, n_n;
static size_t nhash(uintptr_t k, size_t cap) { return (size_t)(((k >> 3) * 0x9E3779B97F4A7C15ULL) >> 18) & (cap - 1); }
static int ngrow(void) {
  size_t nc = n_cap ? n_cap * 2 : (1u << 20);
  Node *nn = calloc(nc, sizeof *nn);
  if (!nn) return 0;
  for (size_t i = 0; i < n_cap; i++) if (nodes[i].addr) { size_t h = nhash(nodes[i].addr, nc); while (nn[h].addr) h = (h + 1) & (nc - 1); nn[h] = nodes[i]; }
  free(nodes); nodes = nn; n_cap = nc;
  return 1;
}
static Node *nfind(uintptr_t a) {
  if (!n_cap) return NULL;
  for (size_t h = nhash(a, n_cap); nodes[h].addr; h = (h + 1) & (n_cap - 1)) if (nodes[h].addr == a) return &nodes[h];
  return NULL;
}
static Node *nadd(uintptr_t a) {
  if ((n_n + 1) * 2 > n_cap && !ngrow()) return NULL;
  size_t h = nhash(a, n_cap);
  while (nodes[h].addr) h = (h + 1) & (n_cap - 1);
  nodes[h].addr = a; nodes[h].hash = 0; nodes[h].tree = 0; nodes[h].xt = 0; n_n++;
  return &nodes[h];
}

/* (the top bit of `tree`: this closure is an EXTRA copy -- not the first of its class) */
#define EXTRA (1ULL << 63)
/* (the next: an extra copy whose bytes a parent has taken into its own `xt` -- each is charged once) */
#define CLAIMED (1ULL << 62)
#define FLAGS (EXTRA | CLAIMED)

/* ---- a class of equal closures ---- */
typedef struct { uint64_t hash; uint64_t count, own, tree, xsum; uintptr_t rep, other; int root, info; } Class;   /* xsum: what its extra members add, in all */
typedef struct { uint64_t hash; uint64_t count, own, tree_unused; } ClassOld_unused;   /* other: a member that is NOT the first */
static Class *classes; static size_t c_cap, c_n;
static int cgrow(void) {
  size_t nc = c_cap ? c_cap * 2 : (1u << 20);
  Class *nn = calloc(nc, sizeof *nn);
  if (!nn) return 0;
  for (size_t i = 0; i < c_cap; i++) if (classes[i].hash) { size_t h = (size_t)(classes[i].hash >> 20) & (nc - 1); while (nn[h].hash) h = (h + 1) & (nc - 1); nn[h] = classes[i]; }
  free(classes); classes = nn; c_cap = nc;
  return 1;
}
static Class *cget(uint64_t hash, int *fresh) {
  if ((c_n + 1) * 2 > c_cap && !cgrow()) return NULL;
  size_t h = (size_t)(hash >> 20) & (c_cap - 1);
  while (classes[h].hash) { if (classes[h].hash == hash) { *fresh = 0; return &classes[h]; } h = (h + 1) & (c_cap - 1); }
  classes[h].hash = hash; c_n++; *fresh = 1;
  return &classes[h];
}

/* ---- per constructor, per root ---- */
typedef struct { uintptr_t info; uint64_t name_hash; uint64_t count, bytes, dups, dup_bytes; char label[96]; int kind; } DInfo;   /* kind 1 cons, 2 C#, 3 D#, 4 I#/W# */
#define D_MAX_INFO 8192
static DInfo dinfos[D_MAX_INFO];
typedef struct { char label[128]; uint64_t count, bytes, dups, dup_bytes; } DRoot;
#define D_MAX_ROOT 65536
static DRoot droots[D_MAX_ROOT]; static int d_nroot;
static uint64_t t_count, t_bytes, t_dups, t_dup_bytes, t_opaque, t_opaque_bytes;

static int dinfo_of(StgClosure *p) {
  uintptr_t key = (uintptr_t)p->header.info;
  size_t h = (size_t)((key >> 3) * 0x9E3779B97F4A7C15ULL >> 40) & (D_MAX_INFO - 1);
  for (int probe = 0; probe < D_MAX_INFO; probe++, h = (h + 1) & (D_MAX_INFO - 1)) {
    DInfo *e = &dinfos[h];
    if (e->info == key) return (int)h;
    if (!e->info) {
      e->info = key;
      int t = get_itbl(p)->type;
      if (t >= CONSTR && t <= CONSTR_NOCAF) {
        const char *d = GET_CON_DESC(get_con_itbl(p));
        snprintf(e->label, sizeof e->label, "%s", d ? d : "?");
        size_t n = strlen(e->label);
        if (n >= 7 && !strcmp(e->label + n - 7, "Types.:")) e->kind = 1;
        else if (n >= 8 && !strcmp(e->label + n - 8, "Types.C#")) e->kind = 2;
        else if (n >= 8 && !strcmp(e->label + n - 8, "Types.D#")) e->kind = 3;
        else if (n >= 8 && (!strcmp(e->label + n - 8, "Types.I#") || !strcmp(e->label + n - 8, "Types.W#"))) e->kind = 4;
      } else if (t == ARR_WORDS) snprintf(e->label, sizeof e->label, "ARR_WORDS");
      else snprintf(e->label, sizeof e->label, "(not data: type %d)", t);
      uint64_t nh = 0xcbf29ce484222325ULL;
      for (const char *c = e->label; *c; c++) nh = MIX(nh, (unsigned char)*c);
      e->name_hash = nh;
      return (int)h;
    }
  }
  return 0;
}

static StgClosure *thru(StgClosure *c) {
  for (int i = 0; i < 64; i++) {
    c = (StgClosure *)((uintptr_t)c & ~(uintptr_t)7);
    if ((uintptr_t)c < 4096) return c;
    int t = get_itbl(c)->type;
    if (t == IND || t == IND_STATIC || t == BLACKHOLE) c = ((StgInd *)c)->indirectee; else break;
  }
  return c;
}
static int is_box(StgClosure *p) {
  uintptr_t a = (uintptr_t)p;
  return (a >= (uintptr_t)&stg_CHARLIKE_closure[0] && a < (uintptr_t)&stg_CHARLIKE_closure[MAX_CHARLIKE - MIN_CHARLIKE + 1])
      || (a >= (uintptr_t)&stg_INTLIKE_closure[0] && a < (uintptr_t)&stg_INTLIKE_closure[MAX_INTLIKE - MIN_INTLIKE + 1]);
}
static int is_data(int t) { return (t >= CONSTR && t <= CONSTR_NOCAF) || t == ARR_WORDS; }

/* ---- the walk ---- */
static uintptr_t *dstk; static size_t ds_cap, ds_n;
static int dpush(uintptr_t u) {
  if (ds_n == ds_cap) { size_t nc = ds_cap ? ds_cap * 2 : (1u << 16); uintptr_t *ns = realloc(dstk, nc * sizeof *ns); if (!ns) return 0; dstk = ns; ds_cap = nc; }
  dstk[ds_n++] = u;
  return 1;
}

void ghs_dup_reset(void) {
  free(nodes); nodes = 0; n_cap = n_n = 0;
  free(classes); classes = 0; c_cap = c_n = 0;
  free(dstk); dstk = 0; ds_cap = ds_n = 0;
  memset(dinfos, 0, sizeof dinfos); d_nroot = 0;
  t_count = t_bytes = t_dups = t_dup_bytes = t_opaque = t_opaque_bytes = 0;
}

/* a finished closure: its hash from its fields (all finished, or in progress = a back edge) */
static void finish(StgClosure *p, Node *me, int ri) {
  const StgInfoTable *info = get_itbl(p);
  int t = info->type, di = dinfo_of(p);
  DInfo *e = &dinfos[di];
  uint64_t own = is_box(p) ? 0 : (uint64_t)closure_sizeW(p) * 8, tree = own, xt = 0, h;
  if (t == ARR_WORDS) {
    StgArrBytes *a = (StgArrBytes *)p;
    h = MIX(0x9E3779B97F4A7C15ULL, a->bytes);
    const unsigned char *b = (const unsigned char *)a->payload;
    size_t n = a->bytes, i = 0;
    for (; i + 8 <= n; i += 8) { uint64_t w; memcpy(&w, b + i, 8); h = MIX(h, w); }
    for (; i < n; i++) h = MIX(h, b[i]);
  } else {   /* a constructor */
    uint32_t np = info->layout.payload.ptrs, nn = info->layout.payload.nptrs;
    h = MIX(e->name_hash, np);
    for (uint32_t i = 0; i < np; i++) {
      StgClosure *c = thru(((StgClosure **)p->payload)[i]);
      Node *cn = (uintptr_t)c < 4096 ? NULL : nfind((uintptr_t)c);
      if (cn && cn->hash) { h = MIX(h, cn->hash); tree += cn->tree & ~FLAGS; }
      else h = MIX(h, (uintptr_t)c);                       /* a back edge, or nothing: identity */
    }
    for (uint32_t i = 0; i < nn; i++) h = MIX(h, (StgWord)p->payload[np + i]);
  }
  h = fin(h);
  me->hash = h; me->tree = tree & ~FLAGS; me->xt = xt;
  e->count++; e->bytes += own; t_count++; t_bytes += own;
  droots[ri].count++; droots[ri].bytes += own;
  int fresh = 0;
  Class *c = cget(h, &fresh);
  if (!c) return;
  if (fresh) { c->rep = (uintptr_t)p; c->root = ri; c->info = di; c->own = own; c->tree = tree; c->xsum = 0; c->count = 1; }
  else {
    /* an extra copy: what it adds is itself and the extra copies below it that no other parent has
     * counted yet. (A stray copy of a string that 200 one-cell lists all point at is ONE extra string:
     * taken by each list in turn it made 200 lists of 24 bytes read as 200 x 700.) */
    if (t != ARR_WORDS) {
      uint32_t np = info->layout.payload.ptrs;
      for (uint32_t i = 0; i < np; i++) {
        StgClosure *ch = thru(((StgClosure **)p->payload)[i]);
        Node *cn = (uintptr_t)ch < 4096 ? NULL : nfind((uintptr_t)ch);
        if (cn && cn->hash && (cn->tree & EXTRA) && !(cn->tree & CLAIMED)) { me->xt += cn->xt; cn->tree |= CLAIMED; }
      }
    }
    c->count++; c->other = (uintptr_t)p; me->tree |= EXTRA; me->xt += own; c->xsum += me->xt; e->dups++; e->dup_bytes += own; t_dups++; t_dup_bytes += own; droots[ri].dups++; droots[ri].dup_bytes += own; }
}

int ghs_dup_root(StgClosure *root, const char *label) {
  if (d_nroot >= D_MAX_ROOT) return -1;
  int ri = d_nroot++;
  memset(&droots[ri], 0, sizeof droots[ri]); snprintf(droots[ri].label, sizeof droots[ri].label, "%s", label ? label : "?");
  ds_n = 0;
  StgClosure *r0 = thru(root);
  if ((uintptr_t)r0 < 4096) return ri;
  dpush((uintptr_t)r0);
  while (ds_n) {
    uintptr_t top = dstk[ds_n - 1];
    int second = (int)(top & 1);
    StgClosure *p = (StgClosure *)(top & ~(uintptr_t)7);
    if (second) { ds_n--; Node *me = nfind((uintptr_t)p); if (me && !me->hash) finish(p, me, ri); continue; }
    if (nfind((uintptr_t)p)) { ds_n--; continue; }          /* done, or in progress further down */
    const StgInfoTable *info = get_itbl(p);
    int t = info->type;
    Node *me = nadd((uintptr_t)p);
    if (!me) { ds_n--; continue; }
    if (!is_data(t)) {
      /* not comparable: itself only. What it holds is walked (it may hold data), but is not part of it. */
      uint64_t own = (uint64_t)closure_sizeW(p) * 8;
      me->hash = fin(MIX(0x1234567ULL, (uintptr_t)p)); me->tree = own;
      t_opaque++; t_opaque_bytes += own; droots[ri].count++; droots[ri].bytes += own;
      ds_n--;
      switch (t) {
        case FUN: case FUN_1_0: case FUN_0_1: case FUN_2_0: case FUN_1_1: case FUN_0_2:
          for (uint32_t i = 0; i < info->layout.payload.ptrs; i++) { StgClosure *c = thru(((StgClosure **)p->payload)[i]); if ((uintptr_t)c >= 4096) dpush((uintptr_t)c); }
          break;
        case THUNK: case THUNK_1_0: case THUNK_0_1: case THUNK_2_0: case THUNK_1_1: case THUNK_0_2:
          for (uint32_t i = 0; i < info->layout.payload.ptrs; i++) { StgClosure *c = thru(((StgThunk *)p)->payload[i]); if ((uintptr_t)c >= 4096) dpush((uintptr_t)c); }
          break;
        case THUNK_SELECTOR: { StgClosure *c = thru(((StgSelector *)p)->selectee); if ((uintptr_t)c >= 4096) dpush((uintptr_t)c); break; }
        case MUT_ARR_PTRS_CLEAN: case MUT_ARR_PTRS_DIRTY: case MUT_ARR_PTRS_FROZEN_CLEAN: case MUT_ARR_PTRS_FROZEN_DIRTY: {
          StgMutArrPtrs *a = (StgMutArrPtrs *)p;
          for (StgWord i = 0; i < a->ptrs; i++) { StgClosure *c = thru(a->payload[i]); if ((uintptr_t)c >= 4096) dpush((uintptr_t)c); }
          break; }
        case SMALL_MUT_ARR_PTRS_CLEAN: case SMALL_MUT_ARR_PTRS_DIRTY: case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN: case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY: {
          StgSmallMutArrPtrs *a = (StgSmallMutArrPtrs *)p;
          for (StgWord i = 0; i < a->ptrs; i++) { StgClosure *c = thru(a->payload[i]); if ((uintptr_t)c >= 4096) dpush((uintptr_t)c); }
          break; }
        case MUT_VAR_CLEAN: case MUT_VAR_DIRTY: { StgClosure *c = thru(((StgMutVar *)p)->var); if ((uintptr_t)c >= 4096) dpush((uintptr_t)c); break; }
        case MVAR_CLEAN: case MVAR_DIRTY: { StgClosure *c = thru((StgClosure *)((StgMVar *)p)->value); if ((uintptr_t)c >= 4096) dpush((uintptr_t)c); break; }
        case TVAR: { StgClosure *c = thru(((StgTVar *)p)->current_value); if ((uintptr_t)c >= 4096) dpush((uintptr_t)c); break; }
        default: break;
      }
      continue;
    }
    dstk[ds_n - 1] = top | 1;                               /* come back when the fields are done */
    if (t != ARR_WORDS)
      for (uint32_t i = 0; i < info->layout.payload.ptrs; i++) {
        StgClosure *c = thru(((StgClosure **)p->payload)[i]);
        if ((uintptr_t)c >= 4096 && !nfind((uintptr_t)c)) dpush((uintptr_t)c);
      }
  }
  return ri;
}

int ghs_dup_stable(void *sp, const char *label) { return ghs_dup_root((StgClosure *)deRefStablePtr((StgStablePtr)sp), label); }

/* ---- showing a value ---- */
static void show(StgClosure *p, char *out, size_t cap, int depth) {
  p = thru(p);
  if ((uintptr_t)p < 4096) { snprintf(out, cap, "_"); return; }
  int t = get_itbl(p)->type;
  if (t == ARR_WORDS) {
    StgArrBytes *a = (StgArrBytes *)p; size_t n = a->bytes, k = 0;
    k += snprintf(out, cap, "bytes[%zu] \"", n);
    for (size_t i = 0; i < n && i < 36 && k + 2 < cap; i++) { unsigned char c = ((unsigned char *)a->payload)[i]; out[k++] = c >= 32 && c < 127 ? (char)c : '.'; }
    if (k + 2 < cap) { out[k++] = '"'; out[k] = 0; }
    return;
  }
  if (!is_data(t)) { snprintf(out, cap, "<thunk or function>"); return; }
  DInfo *e = &dinfos[dinfo_of(p)];
  if (e->kind == 3) { double d; memcpy(&d, &p->payload[0], 8); snprintf(out, cap, "%.6g", d); return; }
  if (e->kind == 4) { snprintf(out, cap, "%ld", (long)(StgWord)p->payload[0]); return; }
  if (e->kind == 2) { StgWord c = (StgWord)p->payload[0]; snprintf(out, cap, "'%c'", c >= 32 && c < 127 ? (char)c : '?'); return; }
  if (e->kind == 1) {
    StgClosure *hd = thru(((StgClosure **)p->payload)[0]);
    if ((uintptr_t)hd >= 4096 && is_data(get_itbl(hd)->type) && dinfos[dinfo_of(hd)].kind == 2) {      /* a String */
      size_t k = 0; uint64_t len = 0; StgClosure *q = p;
      if (cap > 4) out[k++] = '"';
      while ((uintptr_t)q >= 4096 && is_data(get_itbl(q)->type) && dinfos[dinfo_of(q)].kind == 1 && len < 100000) {
        StgClosure *h2 = thru(((StgClosure **)q->payload)[0]);
        if ((uintptr_t)h2 < 4096 || !is_data(get_itbl(h2)->type) || dinfos[dinfo_of(h2)].kind != 2) break;
        StgWord c = (StgWord)h2->payload[0];
        if (len < 44 && k + 3 < cap) out[k++] = c >= 32 && c < 127 ? (char)c : '?';
        len++; q = thru(((StgClosure **)q->payload)[1]);
      }
      snprintf(out + k, cap - k, "\"%s (%llu chars)", len > 44 ? "..." : "", (unsigned long long)len);
      return;
    }
    uint64_t len = 0; StgClosure *q = p;                    /* another list: its length and its head */
    /* (bounded: a cyclic list never ends, and this runs where nothing can interrupt it) */
    while ((uintptr_t)q >= 4096 && is_data(get_itbl(q)->type) && dinfos[dinfo_of(q)].kind == 1 && len < 200000) { len++; q = thru(((StgClosure **)q->payload)[1]); }
    char hb[80]; if (depth < 2) show(hd, hb, sizeof hb, depth + 1); else snprintf(hb, sizeof hb, "..");
    snprintf(out, cap, "[%s, ..] (%llu%s elements)", hb, (unsigned long long)len, len >= 200000 ? "+" : "");
    return;
  }
  const char *nm = strrchr(e->label, '.'); nm = nm ? nm + 1 : e->label;
  const StgInfoTable *info = get_itbl(p);
  size_t k = (size_t)snprintf(out, cap, "%s", nm);
  for (uint32_t i = 0; i < info->layout.payload.ptrs && i < 4 && depth < 2 && k + 8 < cap; i++) {
    char fb[64]; show(((StgClosure **)p->payload)[i], fb, sizeof fb, depth + 1);
    k += (size_t)snprintf(out + k, cap - k, " %s%s%s", strchr(fb, ' ') ? "(" : "", fb, strchr(fb, ' ') ? ")" : "");
  }
}

/* a root's name: a CAF is labelled "@address" (a name is a dladdr, ~1 ms: only the few printed get one) */
extern int ghs_caf_owner(uintptr_t c, char *out, size_t cap);
static const char *rootname(const char *label) {
  static char buf[4][260]; static int turn = 0;
  if (label[0] != '@') return label;
  Dl_info di; uintptr_t a = (uintptr_t)strtoull(label + 1, NULL, 10);
  if (!(dladdr((void *)a, &di) && di.dli_sname)) return label;
  /* a compiler-made local CAF has a name that says nothing: say whose module's it is */
  char own[160];
  if (di.dli_sname[0] == 'L' && ghs_caf_owner(a, own, sizeof own)) {
    char *b = buf[turn++ & 3]; snprintf(b, sizeof buf[0], "%s (in %s)", di.dli_sname, own); return b;
  }
  return di.dli_sname;
}

static Class *cfind(uint64_t hash) {
  if (!c_cap) return NULL;
  for (size_t h = (size_t)(hash >> 20) & (c_cap - 1); classes[h].hash; h = (h + 1) & (c_cap - 1)) if (classes[h].hash == hash) return &classes[h];
  return NULL;
}

/* What an EXTRA copy of a repeated value adds is known when the walk ends: `xt`, summed bottom-up over
 * the closures below it that are themselves extra copies and that no other parent has counted (a part the
 * copies physically share is the first of its class, or the only one, and counts for nobody; an extra copy
 * they share counts once). A class's cost is the sum over its extra members. It was first measured by walking each repeated
 * value again: a sub-structure held by many of them was walked once for each, and a report hung. */

static int cmp_u64_desc(const void *a, const void *b) { uint64_t x = ((const uint64_t *)a)[0], y = ((const uint64_t *)b)[0]; return x < y ? 1 : x > y ? -1 : 0; }

/* The report, into `out`. NOT to stdout: this runs inside an unsafe foreign call, where the thread that
 * drains the engine's output pipe cannot run -- a report over the pipe's 64 KB blocked in write() for good. */
static void dup_report(FILE *out, int top) {
  double mb = 1e6;
  fprintf(out, "%llu data closures, %.1f MB (and %llu thunks, functions and mutable cells, %.1f MB, which are only ever themselves)\n",
         (unsigned long long)t_count, t_bytes / mb, (unsigned long long)t_opaque, t_opaque_bytes / mb);
  fprintf(out, "%llu are a value that already exists: %.1f MB that maximal sharing would give back (%.0f%%)\n",
         (unsigned long long)t_dups, t_dup_bytes / mb, t_bytes ? 100.0 * t_dup_bytes / t_bytes : 0.0);
  /* by constructor */
  uint64_t (*rows)[2] = malloc(sizeof(uint64_t[2]) * D_MAX_INFO); int nr = 0;
  for (int i = 0; i < D_MAX_INFO; i++) if (dinfos[i].info && dinfos[i].dup_bytes) { rows[nr][0] = dinfos[i].dup_bytes; rows[nr][1] = (uint64_t)i; nr++; }
  qsort(rows, (size_t)nr, sizeof rows[0], cmp_u64_desc);
  fprintf(out, "by constructor (duplicated MB of its total, copies of all):\n");
  for (int k = 0; k < nr && k < top; k++) { DInfo *e = &dinfos[rows[k][1]];
    fprintf(out, "  %8.1f of %7.1f MB  %9llu of %9llu  %s\n", e->dup_bytes / mb, e->bytes / mb, (unsigned long long)e->dups, (unsigned long long)e->count, e->label); }
  /* by root */
  nr = 0;
  uint64_t (*rr)[2] = malloc(sizeof(uint64_t[2]) * (size_t)(d_nroot ? d_nroot : 1));
  for (int i = 0; i < d_nroot; i++) if (droots[i].dup_bytes) { rr[nr][0] = droots[i].dup_bytes; rr[nr][1] = (uint64_t)i; nr++; }
  qsort(rr, (size_t)nr, sizeof rr[0], cmp_u64_desc);
  if (d_nroot > 1) {
    fprintf(out, "by root (MB of it that is a copy of something reached earlier, or repeated within it):\n");
    for (int k = 0; k < nr && k < top; k++) { DRoot *r = &droots[rr[k][1]];
      fprintf(out, "  %8.1f of %7.1f MB  %s\n", r->dup_bytes / mb, r->bytes / mb, rootname(r->label)); }
  }
  /* The largest repeated values. A value repeated because the value HOLDING it is repeated is not news:
   * a class is left out when some parent of one of its members is in a class repeated at least as often.
   * What remains are the tops of the repeated structures; each is measured once (dag_bytes). */
  int dbg = getenv("GHS_DUP_DEBUG") != NULL;
  if (dbg) fprintf(stderr, "dups: under pass, %zu nodes %zu classes\n", n_n, c_n);
  unsigned char *under = calloc(c_cap ? c_cap : 1, 1);
  for (size_t i = 0; i < n_cap && under; i++) {
    if (!nodes[i].addr || !nodes[i].hash) continue;
    StgClosure *p = (StgClosure *)nodes[i].addr;
    int t = get_itbl(p)->type;
    if (!(t >= CONSTR && t <= CONSTR_NOCAF)) continue;
    Class *c = cfind(nodes[i].hash);
    if (!c || c->count < 2) continue;
    for (uint32_t k = 0; k < get_itbl(p)->layout.payload.ptrs; k++) {
      StgClosure *ch = thru(((StgClosure **)p->payload)[k]);
      Node *cn = (uintptr_t)ch < 4096 ? NULL : nfind((uintptr_t)ch);
      Class *cc = cn && cn->hash ? cfind(cn->hash) : NULL;
      if (cc && cc->count >= 2 && cc->count <= c->count) under[cc - classes] = 1;
    }
  }
  size_t nc = 0;
  uint64_t (*cs)[2] = malloc(sizeof(uint64_t[2]) * (c_n ? c_n : 1));
  for (size_t i = 0; i < c_cap && under; i++) {
    Class *c = &classes[i];
    if (!c->hash || c->count < 2 || under[i]) continue;
    cs[nc][0] = c->xsum; cs[nc][1] = (uint64_t)i; nc++;
  }
  free(under);
  if (dbg) fprintf(stderr, "dups: %zu candidates, sorting\n", nc);
  qsort(cs, nc, sizeof cs[0], cmp_u64_desc);
  fprintf(out, "the largest repeated values (what the extra copies add, each shared part counted once; how many there are x the size of one; where the first was reached):\n");
  for (size_t k = 0; k < nc && (int)k < top; k++) {
    Class *c = &classes[cs[k][1]];
    if (!cs[k][0]) break;
    if (dbg) fprintf(stderr, "dups: showing %p\n", (void *)c->rep);
    char buf[200]; show((StgClosure *)c->rep, buf, sizeof buf, 0);
    char sz[24]; if (c->tree >= 10000) snprintf(sz, sizeof sz, "%8.1f KB", c->tree / 1e3); else snprintf(sz, sizeof sz, "%8llu B ", (unsigned long long)c->tree);
    fprintf(out, "  %8.2f MB  %7llu x %s  %-.110s   <- %s\n", cs[k][0] / mb, (unsigned long long)c->count, sz, buf,
           c->root >= 0 && c->root < d_nroot ? rootname(droots[c->root].label) : "?");
  }
  void *seen = NULL;
  free(seen); free(cs); free(rr); free(rows);
}

void ghs_dup_done(void) { ghs_dup_reset(); }

/* The whole analysis in ONE call: the roots, then the report. It has to be one: a closure's record is
 * keyed by its address, and between two foreign calls the collector may run and move it (the first
 * version walked a root a call and reported in another: stale addresses, a crash and figures in the
 * terabytes). `stable`: the roots are StablePtrs, else static closures. A NULL label is "@address".
 * Returns the report (malloc'd). */
char *ghs_dup_all(void **rs, const char **labels, int n, int stable, int top) {
  char *buf = NULL; size_t len = 0;
  FILE *out = open_memstream(&buf, &len);
  if (!out) return NULL;
  ghs_dup_reset();
  for (int i = 0; i < n; i++) {
    char lab[40];
    const char *l = labels && labels[i] ? labels[i] : (snprintf(lab, sizeof lab, "@%llu", (unsigned long long)(uintptr_t)rs[i]), lab);
    if (stable) ghs_dup_stable(rs[i], l); else ghs_dup_root((StgClosure *)rs[i], l);
  }
  dup_report(out, top);
  ghs_dup_reset();
  fclose(out);
  return buf;                                   /* the caller prints it and frees it */
}
