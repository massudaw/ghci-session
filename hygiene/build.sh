#!/usr/bin/env bash
# Build the hygiene libraries against THIS GHC's RTS into OUT_DIR (default .ghci-session/clib):
#   libghscafs     the CAF pruner       (c/ghci_cafs.c)    -- needs the RTS dylib's private symbol offsets
#   libghscensus   the heap census      (c/heap_census.c)  -- needs the RTS headers
#   libghsloader   loader diagnostics   (c/loader_stats.c)
# Every one is optional: a failure leaves no library and the session simply does not prune/census.
# Usage: build.sh [OUT_DIR]      Env: GHC (default ghc)
set -uo pipefail
HERE=$(cd "$(dirname "$0")" && pwd)
mkdir -p "${1:-.ghci-session/clib}"
OUT=$(cd "${1:-.ghci-session/clib}" && pwd)
GHC=${GHC:-ghc}
[ "$(uname -s)" = Darwin ] || { echo "ghci-hygiene: only built on macOS (the RTS offsets are read from a Mach-O dylib)" >&2; exit 0; }
LIBDIR=$($GHC --print-libdir 2>/dev/null) || exit 0
RTS=$(find "$LIBDIR/.." -name 'libHSrts-*_thr-ghc*.dylib' 2>/dev/null | head -1)
[ -n "$RTS" ] || { echo "ghci-hygiene: no threaded RTS dylib under $LIBDIR (a statically linked GHC?)" >&2; exit 0; }
fresh() { [ -f "$1" ] && [ "$1" -nt "$2" ] && [ "$1" -nt "$RTS" ]; }
off() { nm "$RTS" 2>/dev/null | awk -v s="_$1" '$3==s {print "0x"$1}' | head -1; }

build_pruner() {
  local D="$OUT/libghscafs.dylib" S="$HERE/c/ghci_cafs.c" s
  fresh "$D" "$S" && return 0
  for s in keepCAFs highMemDynamic unloadObj lookupSymbol sm_mutex dyn_caf_list loaded_objects; do
    [ -n "$(off $s)" ] || { echo "ghci-hygiene: no symbol $s in $RTS -- pruner not built" >&2; return 0; }
  done
  clang -O1 -dynamiclib -undefined dynamic_lookup -o "$D" "$S" \
    -DGHS_OFF_KEEPCAFS=$(off keepCAFs) -DGHS_OFF_HIGHMEMDYNAMIC=$(off highMemDynamic) \
    -DGHS_OFF_UNLOADOBJ=$(off unloadObj) -DGHS_OFF_LOOKUPSYMBOL=$(off lookupSymbol) \
    -DGHS_OFF_SM_MUTEX=$(off sm_mutex) -DGHS_OFF_DYN_CAF_LIST=$(off dyn_caf_list) \
    -DGHS_OFF_LOADED_OBJECTS=$(off loaded_objects) 2>&1 || { rm -f "$D"; echo "ghci-hygiene: pruner compile failed" >&2; }
}
build_census() {
  local D="$OUT/libghscensus.dylib" S="$HERE/c/heap_census.c" INC
  fresh "$D" "$S" && return 0
  INC=$(dirname "$(find "$LIBDIR/.." -name Rts.h 2>/dev/null | head -1)")
  [ -f "$INC/Rts.h" ] || { echo "ghci-hygiene: no Rts.h under $LIBDIR -- census not built" >&2; return 0; }
  clang -O2 -dynamiclib -undefined dynamic_lookup -I"$INC" -o "$D" "$S" 2>&1 | head -5
  [ -f "$D" ] || echo "ghci-hygiene: census compile failed" >&2
}
build_loader() {
  local D="$OUT/libghsloader.dylib" S="$HERE/c/loader_stats.c" s
  fresh "$D" "$S" && return 0
  for s in keepCAFs highMemDynamic unloadObj objects loaded_objects n_unloaded_objects dyn_caf_list revertible_caf_list; do [ -n "$(off $s)" ] || return 0; done
  clang -O1 -dynamiclib -undefined dynamic_lookup -o "$D" "$S" \
    -DGHS_OFF_KEEPCAFS=$(off keepCAFs) -DGHS_OFF_HIGHMEMDYNAMIC=$(off highMemDynamic) -DGHS_OFF_UNLOADOBJ=$(off unloadObj) \
    -DGHS_OFF_OBJECTS=$(off objects) -DGHS_OFF_LOADED_OBJECTS=$(off loaded_objects) -DGHS_OFF_N_UNLOADED=$(off n_unloaded_objects) \
    -DGHS_OFF_DYN_CAF_LIST=$(off dyn_caf_list) -DGHS_OFF_REVERTIBLE=$(off revertible_caf_list) 2>&1 || rm -f "$D"
}
build_pruner; build_census; build_loader
ls "$OUT" | sed 's/^/ghci-hygiene: built /'
exit 0
