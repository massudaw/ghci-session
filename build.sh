#!/bin/sh
# Build the two executables into .bin/ (what bin/ghci-session and a project's wrappers run): the command, and
# beside it the engine -- GHCi itself, which a session needs. Replaced by rename, so a daemon running the old
# copy keeps running it.
set -e
cd "$(dirname "$0")"
# cabal recompiles a C source when the .c changes, not when a header it includes does (and it does not even
# look while no source has changed): a header newer than the last build touches the C sources, so a change
# to hygiene/c/rts_syms.h (how the RTS's private symbols are found) is built in. Their contents stay as they are.
newest=$(ls -t hygiene/c/*.h cbits/*.h 2>/dev/null | head -1)
if [ -n "$newest" ] && [ -f .bin/ghci-session-engine ] && [ "$newest" -nt .bin/ghci-session-engine ]; then
  touch hygiene/c/*.c cbits/*.c
fi
cabal build -v0 exe:ghci-session exe:ghci-session-engine
mkdir -p .bin
for x in ghci-session ghci-session-engine; do
  cp "$(cabal list-bin -v0 exe:$x)" .bin/$x.new
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
