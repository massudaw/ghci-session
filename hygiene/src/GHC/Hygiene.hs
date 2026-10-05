{-# LANGUAGE ForeignFunctionInterface #-}

-- | __Free what a GHCi @:reload@ leaves behind__ (see @hygiene/c/ghci_cafs.c@).
--
-- A dynamically linked GHCi never unloads a superseded module: each reload
-- links the recompiled modules into a new temporary shared library, keeps the
-- old one, and the RTS keeps every CAF of every such library as a GC root. The
-- evaluated value of each superseded CAF -- the projected views, the element
-- lists -- stays live for the life of the process (a 100-reload day reached
-- 21 GB). 'pruneCafs' unlinks the superseded ones from the RTS's CAF list and
-- runs a major GC (when it unlinked anything).
--
-- The C half is optional ('hygiene/build.sh' builds it, and only against an RTS whose layout it knows): with
-- no library, or a different RTS, this does nothing and says so.
module GHC.Hygiene (pruneCafs, loaderStats) where

import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Control.Monad (when)
import System.Mem (performMajorGC)
import System.Posix.DynamicLinker (RTLDFlags (RTLD_LOCAL, RTLD_NOW), dlopen, dlsym)

foreign import ccall "dynamic" callInt :: FunPtr (IO CInt) -> IO CInt

-- | CAFs unlinked (then a major GC), or -1 when the library does not know this
-- RTS, or -2 when there is no library to call. For a dynamic GHCi: the CAFs of wholly
-- superseded temporary libraries ('ghci_cafs.c').
pruneCafs :: IO Int
pruneCafs = do
  dir <- maybe ".ghci-session/clib" id <$> lookupEnv "GHS_DIR"
  let path = dir ++ "/libghscafs.dylib"
  there <- doesFileExist path
  if not there then pure (-2) else do
    h <- dlopen path [RTLD_NOW, RTLD_LOCAL]
    f <- dlsym h "ghs_prune_cafs"
    r <- callInt f
    -- the GC is what frees what was unlinked: with nothing unlinked it is a major collection of the whole
    -- heap for nothing (0.2 s at 100 MB live, 1.5 s at 700 MB), on every reload that changed no code
    when (r > 0) performMajorGC
    pure (fromIntegral r)

-- | What the RTS linker is holding, printed to stderr: objects by status and
-- kind, bytes of object code, CAFs rooted on each list (hygiene/c/loader_stats.c).
-- -1 when the library is not there or its offsets do not match this process.
loaderStats :: IO Int
loaderStats = go ["libghsloader.dylib"]
  where
    go [] = pure (-1)
    go (n : ns) = do
      dir <- maybe ".ghci-session/clib" id <$> lookupEnv "GHS_DIR"
      let path = dir ++ "/" ++ n
      there <- doesFileExist path
      if not there then go ns else do
        h <- dlopen path [RTLD_NOW, RTLD_LOCAL]
        f <- dlsym h "ghs_loader_stats"
        r <- fromIntegral <$> callInt f
        if r >= 0 then pure r else go ns
