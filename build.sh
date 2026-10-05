#!/bin/sh
# Build the executable and put it at .bin/ghci-session (what bin/ghci-session and a project's wrappers run).
# Replaced by rename, so a daemon running the old copy keeps running it.
set -e
cd "$(dirname "$0")"
cabal build -v0 exe:ghci-session
mkdir -p .bin
cp "$(cabal list-bin -v0 exe:ghci-session)" .bin/ghci-session.new
mv .bin/ghci-session.new .bin/ghci-session
# the vendored GHCi (only for the compiler it was vendored for: see ghci-session.cabal)
if cabal build -v0 exe:ghci-session-engine 2>/dev/null; then
  cp "$(cabal list-bin -v0 exe:ghci-session-engine)" .bin/ghci-session-engine.new
  mv .bin/ghci-session-engine.new .bin/ghci-session-engine
else
  rm -f .bin/ghci-session-engine
fi
