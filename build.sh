#!/bin/sh
# Build the two executables into .bin/ (what bin/ghci-session and a project's wrappers run): the command, and
# beside it the engine -- GHCi itself, which a session needs. Replaced by rename, so a daemon running the old
# copy keeps running it.
set -e
cd "$(dirname "$0")"
cabal build -v0 exe:ghci-session exe:ghci-session-engine
mkdir -p .bin
for x in ghci-session ghci-session-engine; do
  cp "$(cabal list-bin -v0 exe:$x)" .bin/$x.new
  mv .bin/$x.new .bin/$x
done
