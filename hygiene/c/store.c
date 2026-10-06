/* Named slots that outlive a :reload.
 *
 * A reload reverts every CAF of the modules it links again, so a cache held by loaded code is gone with
 * it. The engine is not reloaded: a slot here holds one pointer (a StablePtr, made by the code that
 * owns the value) under a name, and the next generation of that code asks for it by the same name.
 * GHC.Hygiene.Store is the Haskell side; any loaded code may also look these up with dlsym.
 *
 * Nothing here knows what a slot holds: a value must only be read back by code compiled against the
 * same layout of its type, which is the owner's to ensure (put a version in the name). */
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

typedef struct { char *name; void *ptr; } slot_t;
static slot_t *slots = 0;          /* in the order they were made (the listing's order) */
static int nslots = 0, cap = 0;
static int *table = 0;             /* open addressing over slot indices + 1 (0: empty); rebuilt when it grows or a slot goes */
static int tcap = 0;
static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

static unsigned long hash(const char *s) {
  unsigned long h = 1469598103934665603UL;
  for (; *s; s++) h = (h ^ (unsigned char)*s) * 1099511628211UL;
  return h;
}

static void rebuild(int want) {
  free(table);
  tcap = 64; while (tcap < 2 * want) tcap *= 2;
  table = calloc(tcap, sizeof(int));
  for (int i = 0; i < nslots; i++) {
    unsigned long h = hash(slots[i].name) & (tcap - 1);
    while (table[h]) h = (h + 1) & (tcap - 1);
    table[h] = i + 1;
  }
}

static int find(const char *name) {
  if (!tcap) return -1;
  for (unsigned long h = hash(name) & (tcap - 1); table[h]; h = (h + 1) & (tcap - 1))
    if (strcmp(slots[table[h] - 1].name, name) == 0) return table[h] - 1;
  return -1;
}

/* The pointer under a name, or 0. */
void *ghs_store_get(const char *name) {
  pthread_mutex_lock(&lock);
  int i = find(name);
  void *p = i < 0 ? 0 : slots[i].ptr;
  pthread_mutex_unlock(&lock);
  return p;
}

/* Put a pointer under a name UNLESS one is there: returns the pointer the slot holds afterwards, so two
 * threads asking for the same new slot agree on one (the loser frees its own). */
void *ghs_store_put_new(const char *name, void *p) {
  pthread_mutex_lock(&lock);
  int i = find(name);
  if (i < 0) {
    if (nslots == cap) { cap = cap ? 2 * cap : 16; slots = realloc(slots, cap * sizeof(slot_t)); }
    slots[nslots].name = strdup(name); slots[nslots].ptr = p; nslots++;
    if (2 * nslots > tcap) rebuild(nslots);
    else { unsigned long h = hash(name) & (tcap - 1); while (table[h]) h = (h + 1) & (tcap - 1); table[h] = nslots; }
  } else p = slots[i].ptr;
  pthread_mutex_unlock(&lock);
  return p;
}

/* Remove a slot: the pointer it held (for its owner to free), or 0. */
void *ghs_store_take(const char *name) {
  pthread_mutex_lock(&lock);
  int i = find(name);
  void *p = 0;
  if (i >= 0) {
    p = slots[i].ptr; free(slots[i].name);
    for (int k = i; k < nslots - 1; k++) slots[k] = slots[k + 1];
    nslots--;
    rebuild(nslots);
  }
  pthread_mutex_unlock(&lock);
  return p;
}

int ghs_store_count(void) { return nslots; }

/* Slot i's name and pointer (0 past the end). Not for use while another thread adds or removes. */
const char *ghs_store_name(int i) { return i >= 0 && i < nslots ? slots[i].name : 0; }
void *ghs_store_ptr(int i) { return i >= 0 && i < nslots ? slots[i].ptr : 0; }
