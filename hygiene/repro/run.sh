#!/bin/bash
# usage: run.sh [unsafe]     -- "unsafe" turns the pruner's young-value guard off (GHS_CAF_UNSAFE_YOUNG=1)
# Needs the hygiene libraries: ../build.sh ./clib   (done here)
cd "$(dirname "$0")"
../build.sh "$PWD/clib" >/dev/null 2>&1
export GHS_DIR="$PWD/clib"
[ "${1:-}" = unsafe ] && export GHS_CAF_UNSAFE_YOUNG=1
sed -i.bak '/^-- edit [0-9]*$/d' src/R.hs && rm -f src/R.hs.bak
{
  echo ':module + R GHC.Hygiene'
  echo 'R.stash'                                   # keep generation 1's f; its table is NOT evaluated
  echo ':! echo "-- edit 1" >> src/R.hs'
  echo ':reload'
  echo ':module + R GHC.Hygiene'
  echo 'print R.generation'                        # an evaluation: generation 2 is linked, 1 is now superseded
  echo 'R.callStashed 1 >>= \x -> putStrLn ("old f, first call: " ++ show x)'      # enters generation 1 table: its value is YOUNG
  echo 'GHC.Hygiene.unlinkCafs >>= \k -> putStrLn ("unlinked: " ++ show k)'        # no GC
  echo 'print (sum [1 .. 30000000 :: Int])'        # minor collections
  echo 'R.callStashed 1 >>= \x -> putStrLn ("old f, after minor GCs: " ++ show x)'
  echo 'System.Mem.performMajorGC >> putStrLn "major GC done"'
  echo 'R.callStashed 1 >>= \x -> putStrLn ("old f, after the major GC: " ++ show x)'
} | cabal repl -v0 lib:caf-repro --repl-options=-fobject-code --repl-options=-odir=.obj --repl-options=-hidir=.obj 2>&1 \
  | grep -v "^ld: warning\|^$"
echo "(exit ${PIPESTATUS[1]})"
sed -i.bak '/^-- edit [0-9]*$/d' src/R.hs && rm -f src/R.hs.bak
