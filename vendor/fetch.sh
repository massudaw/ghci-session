#!/bin/sh
# Vendor GHCi's front end -- the `ghc` executable's own sources -- for one compiler version, UNCHANGED except
# for the two mechanical edits to Main.hs below (each asserted to apply exactly once). They must match the
# compiler exactly: they are built against its `ghc` library.
#   vendor/fetch.sh 9.14.1
set -e
V=${1:?a GHC version, e.g. 9.14.1}
D="$(cd "$(dirname "$0")" && pwd)/ghc-$V"
BASE="https://gitlab.haskell.org/ghc/ghc/-/raw/ghc-$V-release"
mkdir -p "$D"
for f in Main.hs GHCi/UI.hs GHCi/UI/Monad.hs GHCi/UI/Info.hs GHCi/UI/Print.hs GHCi/UI/Exception.hs GHCi/Leak.hs GHCi/Util.hs \
         GHC/Driver/Session/Lint.hs GHC/Driver/Session/Mode.hs; do
  mkdir -p "$D/$(dirname $f)"
  curl -fsS -m 60 -o "$D/$f" "$BASE/ghc/$f"
done
curl -fsS -m 60 -o "$D/LICENSE" "$BASE/LICENSE"
# Main.hs becomes a module we can call, and takes its GHCi settings from the engine (the prompt hook)
# and lets it hook the logger before the first load.
python3 - "$D" <<'PY'
import sys, os
d = sys.argv[1]
src = open(os.path.join(d, "Main.hs")).read()
def once(s, a, b):
    assert s.count(a) == 1, (a, s.count(a))
    return s.replace(a, b)
src = once(src, "module Main (main) where", "module GhcMain (main) where\n\nimport GhsEngine (engineHook, engineSettings)")
src = once(src, "interactiveUI defaultGhciSettings hs_srcs maybe_expr", "engineHook\n  interactiveUI (engineSettings defaultGhciSettings) hs_srcs maybe_expr")
os.makedirs(os.path.join(d, "patched"), exist_ok=True)
open(os.path.join(d, "patched", "GhcMain.hs"), "w").write(src)
os.remove(os.path.join(d, "Main.hs"))
PY
wc -l $(find "$D" -name '*.hs') | tail -1
