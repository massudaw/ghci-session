{-# LANGUAGE ImplicitPrelude #-}
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
-- The C half is part of the session's engine (@ghci-session-engine@, which is GHCi): these look it up in the
-- running process. In any other GHCi there is nothing to find, and they do nothing and say so.
module GHC.Hygiene (pruneCafs, unlinkCafs, loaderStats, engineSymbol, majorGC, returnDecay, heapAuto, memReturned) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Foreign.C.Types (CInt (..), CLLong (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr)
import System.Mem (performMajorGC)
import System.Posix.DynamicLinker (DL (Default), dlsym)

foreign import ccall "dynamic" callInt :: FunPtr (IO CInt) -> IO CInt
foreign import ccall safe "dynamic" callIntInt :: FunPtr (CInt -> IO CInt) -> CInt -> IO CInt

foreign import ccall unsafe "dynamic" callDD :: FunPtr (Double -> IO Double) -> Double -> IO Double

-- | Set how fast the RTS returns unused memory after a major collection (@-Fd@; 0: at once), and say what
-- it was. A negative number only asks.
returnDecay :: Double -> IO Double
returnDecay f = engineSymbol "ghs_return_decay" >>= maybe (pure (-1)) (\g -> callDD g f)

-- | Turn the RTS's automatic heap size (@-H@: allocation area up to the largest heap it has needed) on or
-- off, and say whether it was on. A negative number only asks; -1 back: not the engine.
heapAuto :: Int -> IO Int
heapAuto on = engineSymbol "ghs_heap_auto" >>= maybe (pure (-1)) (\g -> fromIntegral <$> callIntInt g (fromIntegral on))

foreign import ccall unsafe "dynamic" callLL :: FunPtr (Ptr CLLong -> IO CLLong) -> Ptr CLLong -> IO CLLong

-- | What the engine has really handed back to the system of the memory the RTS freed (macOS: see
-- @hygiene/c/mem_return.c@): megabytes, and how many ranges.
memReturned :: IO (Int, Int)
memReturned = engineSymbol "ghs_mem_returned" >>= maybe (pure (0, 0)) (\g -> alloca (\p -> do { b <- callLL g p; n <- peek p; pure (fromIntegral (b `div` 1000000), fromIntegral n) }))

-- | One major collection, compacting (@True@) or copying, whatever the session's RTS flags say (it runs
-- with @-c@, whose compaction is single-threaded; a copying collection uses every capability). Outside
-- the engine: the RTS's own 'performMajorGC'.
majorGC :: Bool -> IO ()
majorGC compact = do
  f <- engineSymbol "ghs_major_gc"
  case f of
    Just g -> () <$ callIntInt g (if compact then 1 else 0)
    Nothing -> performMajorGC

-- | A function of the engine's C half, if this process is the engine.
engineSymbol :: String -> IO (Maybe (FunPtr a))
engineSymbol name = do
  r <- try (dlsym Default name) :: IO (Either SomeException (FunPtr a))
  pure (case r of { Right f | f /= nullFunPtr -> Just f; _ -> Nothing })

-- | Unlink the superseded CAFs from the RTS's root list, and nothing else: the number unlinked, or -1 when
-- this RTS cannot be read, or -2 when this process is not the engine. This is what makes their
-- values RECLAIMABLE, and it costs microseconds; the memory comes back at the next major GC, whenever
-- that is. Call it once the code that replaced them has been linked -- GHCi links a reloaded module on the
-- first evaluation that needs it, so right after a @:reload@ the generation just replaced does not yet
-- look superseded.
unlinkCafs :: IO Int
unlinkCafs = engineSymbol "ghs_prune_cafs" >>= maybe (pure (-2)) (fmap fromIntegral . callInt)

-- | 'unlinkCafs', then a major GC if it unlinked anything: the memory back NOW, for the price of a
-- collection of the whole heap (0.2 s at 100 MB live, ~0.9 s at 500 MB).
pruneCafs :: IO Int
pruneCafs = do
  r <- unlinkCafs
  when (r > 0) performMajorGC
  pure r

-- | What the RTS linker is holding, printed to stderr: objects by status and
-- kind, bytes of object code, CAFs rooted on each list (hygiene/c/loader_stats.c).
-- -1 when the RTS cannot be read, -2 outside the engine.
loaderStats :: IO Int
loaderStats = engineSymbol "ghs_loader_stats" >>= maybe (pure (-2)) (fmap fromIntegral . callInt)
