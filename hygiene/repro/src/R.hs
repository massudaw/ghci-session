-- | The smallest thing that shows why a superseded CAF must not be unlinked while its value is young
-- (see ../c/ghci_cafs.c, value_is_old, and ./README.md).
module R (table, f, stash, callStashed, generation) where

import Foreign.Ptr (intPtrToPtr, ptrToIntPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, newStablePtr)
import System.Environment (getEnv, setEnv)

-- | A CAF. Nothing evaluates it until 'f' is called.
table :: [Int]
table = [1 .. 1000]
{-# NOINLINE table #-}

-- | A function whose code refers to the CAF.
f :: Int -> Int
f n = sum table + n
{-# NOINLINE f #-}

-- | Keep THIS generation's 'f' alive across reloads, the way a cache keeps a value built by old code: a
-- StablePtr, its address in the environment (a Haskell-side variable would be reset by the reload).
stash :: IO ()
stash = do
  sp <- newStablePtr f
  setEnv "CAF_REPRO_F" (show (toInteger (ptrToIntPtr (castStablePtrToPtr sp))))

-- | Call the stashed 'f': after a reload this runs the OLD generation's code, which enters the OLD
-- generation's 'table'.
callStashed :: Int -> IO Int
callStashed n = do
  a <- read <$> getEnv "CAF_REPRO_F"
  g <- deRefStablePtr (castPtrToStablePtr (intPtrToPtr (fromInteger a)))
  pure ((g :: Int -> Int) n)

generation :: Int
generation = 1
