#!/bin/sh
# Vendor GHCi's front end -- the `ghc` executable's own sources -- for one compiler version, UNCHANGED except
# for the mechanical edits below (four to Main.hs, three to GHCi/UI.hs; each asserted to apply exactly once). They must match the
# compiler exactly: they are built against its `ghc` library.
#   vendor/fetch.sh 9.14.1
#   vendor/fetch.sh 9.6.7
#   vendor/fetch.sh 9.10.3
set -e
V=${1:?a GHC version, e.g. 9.14.1}
D="$(cd "$(dirname "$0")" && pwd)/ghc-$V"
BASE="https://gitlab.haskell.org/ghc/ghc/-/raw/ghc-$V-release"
mkdir -p "$D"
# (the front end's modules moved between versions: 9.6 has GHCi.UI.Tags and keeps the mode and lint code in Main)
case "$V" in
  9.6.*) FILES="Main.hs GHCi/UI.hs GHCi/UI/Monad.hs GHCi/UI/Info.hs GHCi/UI/Tags.hs GHCi/Leak.hs GHCi/Util.hs" ;;
  9.10.*) FILES="Main.hs GHCi/UI.hs GHCi/UI/Monad.hs GHCi/UI/Info.hs GHCi/UI/Exception.hs GHCi/Leak.hs GHCi/Util.hs" ;;
  *)     FILES="Main.hs GHCi/UI.hs GHCi/UI/Monad.hs GHCi/UI/Info.hs GHCi/UI/Print.hs GHCi/UI/Exception.hs GHCi/Leak.hs GHCi/Util.hs
                GHC/Driver/Session/Lint.hs GHC/Driver/Session/Mode.hs" ;;
esac
for f in $FILES; do
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
# ... and keeps the session's flags as they were before its units' (GhsAddUnits: a unit added later is parsed over them)
src = once(src, "import GhsEngine (engineHook, engineSettings)", "import GhsEngine (engineHook, engineSettings)\nimport GhsAddUnits (rememberInitial)")
src = once(src, "ghciUI units srcs maybe_expr = do\n  hs_srcs <- case NE.nonEmpty units of", "ghciUI units srcs maybe_expr = do\n  rememberInitial\n  hs_srcs <- case NE.nonEmpty units of")
os.makedirs(os.path.join(d, "patched"), exist_ok=True)
open(os.path.join(d, "patched", "GhcMain.hs"), "w").write(src)
os.remove(os.path.join(d, "Main.hs"))
PY
# GHCi/UI.hs: its one call of the compiler's load goes through the engine (GhsFastLoad: a reload without
# the scan of every module when the daemon knows what changed).
python3 - "$D" <<'PY'
import sys, os
p = os.path.join(sys.argv[1], "GHCi", "UI.hs")
src = open(p).read()
def once(s, a, b):
    assert s.count(a) == 1, (a, s.count(a))
    return s.replace(a, b)
src = once(src, "ok <- trySuccess $ GHC.loadWithCache (Just hmis)", "ok <- trySuccess $ GhsFastLoad.loadWith (Just hmis)")
src = once(src, "import GHC.Driver.Make ( newIfaceCache, ModIfaceCache(..) )", "import GHC.Driver.Make ( newIfaceCache, ModIfaceCache(..) )\nimport qualified GhsFastLoad")
# ... and it exports how the two interactive units are made (GhsAddUnits makes them again when a unit is added;
# a GHCi before them -- 9.6 -- has none, and adds no unit to a running session)
if "installInteractiveHomeUnits ::" in src:
    src = once(src, "module GHCi.UI (\n        interactiveUI,", "module GHCi.UI (\n        installInteractiveHomeUnits,\n        interactiveUI,")
open(p, "w").write(src)
PY
wc -l $(find "$D" -name '*.hs') | tail -1
