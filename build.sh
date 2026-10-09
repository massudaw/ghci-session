#!/bin/sh
# Build the two executables into .bin/ (what bin/ghci-session and a project's wrappers run): the command, and
# beside it the engine -- GHCi itself, which a session needs. Replaced by rename, so a daemon running the old
# copy keeps running it.
set -e
cd "$(dirname "$0")"
# GHS_FFF=1: built with libfff (the package's fff flag, off by default: search is then a plain scan), and the
# library fetched below
fff_flag=; [ "${GHS_FFF:-0}" = 1 ] && fff_flag=--flags=+fff
# GHS_STATIC_VT=1: libghostty-vt linked in (.bin/libghostty-vt-static.a, left by tools/libghostty-vt.sh) instead of
# found at run time
vt_flag=; [ "${GHS_STATIC_VT:-0}" = 1 ] && vt_flag="--constraint=ghostty-vt+static --extra-lib-dirs=$PWD/.bin"
cabal build -v0 $fff_flag $vt_flag exe:ghci-session exe:ghci-session-engine
mkdir -p .bin
for x in ghci-session ghci-session-engine; do
  cp "$(cabal list-bin -v0 $fff_flag $vt_flag exe:$x)" .bin/$x.new
  mv .bin/$x.new .bin/$x
done
# macOS: the library that makes the RTS's memory returns real (hygiene/c/mem_return.c). It has to be a
# library of its own -- dyld applies an interposer from an inserted library, not from the executable -- and
# the daemon inserts it when it starts the engine. Optional: without it the engine runs as before.
if [ "$(uname)" = Darwin ]; then
  if [ ! -f .bin/libghsmem.dylib ] || [ hygiene/c/mem_return.c -nt .bin/libghsmem.dylib ]; then
    cc -dynamiclib -O2 -o .bin/libghsmem.dylib.new hygiene/c/mem_return.c && mv .bin/libghsmem.dylib.new .bin/libghsmem.dylib
  fi
fi
# libfff (https://github.com/dmtrKovalenko/fff), the file finder behind grep / find (cbits/ghs_fff.c dlopens it),
# with GHS_FFF=1 only. Optional, like the macOS library above: without it the search falls back to a plain scan.
# Fetched from the latest release into lib/ (git-ignored) when neither lib/ nor .bin/ has one; FFF_URL overrides it.
case "$(uname -s)-$(uname -m)" in
  Darwin-arm64)             fff_target=aarch64-apple-darwin;      fff_ext=dylib ;;
  Darwin-x86_64)            fff_target=x86_64-apple-darwin;       fff_ext=dylib ;;
  Linux-x86_64)             fff_target=x86_64-unknown-linux-gnu;  fff_ext=so ;;
  Linux-aarch64|Linux-arm64) fff_target=aarch64-unknown-linux-gnu; fff_ext=so ;;
  *)                        fff_target=;                          fff_ext= ;;
esac
if [ -n "$fff_flag" ] && [ -n "$fff_target" ] && [ ! -f "lib/libfff.$fff_ext" ] && [ ! -f ".bin/libfff.$fff_ext" ]; then
  mkdir -p lib
  fff_url="${FFF_URL:-https://github.com/dmtrKovalenko/fff/releases/latest/download/c-lib-$fff_target.$fff_ext}"
  if curl -fsSL --retry 2 -o "lib/libfff.$fff_ext.new" "$fff_url"; then
    mv "lib/libfff.$fff_ext.new" "lib/libfff.$fff_ext"
  else
    rm -f "lib/libfff.$fff_ext.new"
    echo "build.sh: could not download libfff from $fff_url (search falls back to a plain scan)" >&2
  fi
fi
for ext in dylib so; do
  if [ -n "$fff_flag" ] && [ -f "lib/libfff.$ext" ] && [ ! -f ".bin/libfff.$ext" ]; then
    cp "lib/libfff.$ext" ".bin/libfff.$ext"
  fi
done
