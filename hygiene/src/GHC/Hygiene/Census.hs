{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | __A heap census for a GHCi session__, done in C (@hygiene/c/heap_census.c@): what every CAF, and every
-- value you 'keep', retains, by constructor, with the Strings among it. A full CAF census is seconds, not
-- minutes. 'censusOf' and 'benchOf' answer one question about one value or action without a restart.
--
-- The C is part of the session's engine; in any other GHCi every entry point says so and does nothing.
module GHC.Hygiene.Census
  ( cafReport, cafStrings, keptReport, keptStrings, keep
  , censusOf, benchOf, memNow, censusBench
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import qualified Data.IntMap.Strict as IMs
import Data.Int (Int64)
import Data.List (isPrefixOf, sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as Tx
import Foreign.C.String (CString, peekCAString, withCAString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, intPtrToPtr, nullFunPtr, nullPtr, ptrToWordPtr, wordPtrToPtr)
import Foreign.StablePtr (castStablePtrToPtr, freeStablePtr, newStablePtr)
import Foreign.Storable (peekElemOff)
import GHC.Clock (getMonotonicTime)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import System.Environment (getEnvironment, setEnv)
import System.Mem (performMajorGC)
import GHC.Hygiene (engineSymbol)
import Text.Printf (printf)

foreign import ccall "dynamic" callCafList :: FunPtr (Ptr () -> Ptr () -> CInt -> IO CInt) -> Ptr () -> Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "dynamic" cenReset :: FunPtr (IO ()) -> IO ()
foreign import ccall unsafe "dynamic" cenRoot :: FunPtr (Ptr () -> CString -> Int64 -> IO CInt) -> Ptr () -> CString -> Int64 -> IO CInt
foreign import ccall unsafe "dynamic" cenN :: FunPtr (IO CInt) -> IO CInt
foreign import ccall unsafe "dynamic" cenRootRow :: FunPtr (CInt -> Ptr () -> Ptr Int64 -> IO CInt) -> CInt -> Ptr () -> Ptr Int64 -> IO CInt
foreign import ccall unsafe "dynamic" cenStrRow :: FunPtr (CInt -> Ptr () -> Ptr () -> Ptr Int64 -> IO CInt) -> CInt -> Ptr () -> Ptr () -> Ptr Int64 -> IO CInt

data Census = Census { cReset :: IO (), cRoot :: Ptr () -> String -> Int -> IO (), cStable :: Ptr () -> String -> Int -> IO (), cRows :: IO [(String, [Int64])], cInfos :: IO [(String, [Int64])], cStrs :: IO [(String, String, [Int64])] }

-- | The engine's census entry points, or say this process is not the engine.
withCensus :: (Census -> IO ()) -> IO ()
withCensus k = do
  let names = ["ghs_cen_reset", "ghs_cen_root", "ghs_cen_stable", "ghs_cen_nroots", "ghs_cen_root_row", "ghs_cen_ninfo", "ghs_cen_info_row", "ghs_cen_nstr", "ghs_cen_str_row"]
  found <- mapM engineSymbol names :: IO [Maybe (FunPtr ())]
  case sequence found of
    Nothing -> putStrLn "no heap census in this process: it is part of the session's engine (ghci-session-engine)"
    Just fs -> withFuns fs
  where
   withFuns fs = do
    let at i = castFunPtr (fs !! i)
        reset = at 0; root = at 1; stable = at 2; nroots = at 3; rootRow = at 4; ninfo = at 5; infoRow = at 6; nstr = at 7; strRow = at 8
    let rows nI getRow = do
          n <- fromIntegral <$> cenN nI
          fmap concat $ forM [0 .. n - 1] $ \i -> allocaBytes 256 $ \lab -> allocaBytes (8 * 8) $ \out -> do
            ok <- getRow (fromIntegral i) lab out
            if ok == 0 then pure [] else do
              l <- peekCAString (castPtr lab)
              vs <- mapM (peekElemOff out) [0 .. 4]
              pure [(l, vs)]
    cens <- pure Census
      { cReset = cenReset reset
      , cRoot = \a l cap -> withCAString l (\cs -> () <$ cenRoot root a cs (fromIntegral cap))
      , cStable = \a l cap -> withCAString l (\cs -> () <$ cenRoot stable a cs (fromIntegral cap))
      , cRows = rows nroots (\i lab out -> cenRootRow rootRow i (castPtr lab) out)
      , cInfos = rows ninfo (\i lab out -> cenRootRow infoRow i (castPtr lab) out)
      , cStrs = do
          n <- fromIntegral <$> cenN nstr
          fmap concat $ forM [0 .. n - 1] $ \i -> allocaBytes 64 $ \key -> allocaBytes 256 $ \rl -> allocaBytes 64 $ \out -> do
            ok <- cenStrRow strRow (fromIntegral i) (castPtr key) (castPtr rl) out
            if ok == 0 then pure [] else do
              kk <- peekCAString (castPtr key); rr <- peekCAString (castPtr rl)
              vs <- mapM (peekElemOff out) [0, 1]
              pure [(kk, rr, vs)]
      }
    k cens

-- | What the roots retain, by root and by constructor, and the Strings among it.
censusPrint :: Census -> (String -> IO String) -> Int -> Bool -> IO ()
censusPrint c namer top strings = do
  rs <- cRows c
  is <- cInfos c
  let tot f = sum [ f v | (_, v) <- rs ]
      mb x = fromIntegral x / 1e6 :: Double
  printf "%d roots, %d closures, %.1f MB (list cells %.1f MB, byte arrays %.1f MB)\n" (length rs) (tot (!! 1)) (mb (tot (!! 0))) (mb (tot (!! 2))) (mb (tot (!! 3)))
  if strings
    then do
      ss <- cStrs c
      let chars = sum [ v !! 0 | (_, _, v) <- ss ]
      printf "%d distinct string groups, %.2f M chars (%.1f MB as cons cells)\n" (length ss) (fromIntegral chars / 1e6 :: Double) (fromIntegral chars * 24 / 1e6 :: Double)
      forM_ (take top (sortOn (\(_, _, v) -> Down (v !! 0)) ss)) $ \(k, r0, v) -> do
        r <- namer r0
        printf "%9d chars x%-6d %-42s <- %s\n" (v !! 0) (v !! 1) (show k) (take 50 r)
    else do
      forM_ (take top (sortOn (Down . (!! 0) . snd) rs)) $ \(l, v) -> do
        nm <- namer l
        printf "%8.1f MB %9d closures  (cons %.1f MB, arrays %.1f MB)%s  %s\n" (mb (v !! 0)) (v !! 1) (mb (v !! 2)) (mb (v !! 3)) (if v !! 4 /= 0 then " STOPPED" else "") (take 90 nm)
      putStrLn "by constructor:"
      forM_ (take 14 (sortOn (Down . (!! 0) . snd) is)) $ \(l, v) ->
        printf "%8.1f MB %10d  %s\n" (mb (v !! 0)) (v !! 1) l

-- | Every CAF the RTS roots ('ghs_caf_list'), walked in list order (newest first): what each retains that
-- no earlier one did. @top@ roots shown; @cap@ closures at most a root.
cafRoots :: Census -> Int -> IO ()
cafRoots c cap = do
  f <- maybe nullFunPtr id <$> engineSymbol "ghs_caf_list"
  let n = 200000
  performMajorGC
  allocaArray n $ \addrs -> do
    -- no names here: `dladdr` is ~1 ms a CAF, 10 s for 8k of them; roots are labelled by address
    -- and the few that get printed are named afterwards ('cafName')
    got <- fromIntegral <$> callCafList f (castPtr addrs) nullPtr (fromIntegral n)
    as <- peekArray (min got n) (addrs :: Ptr (Ptr ()))
    cReset c
    forM_ as $ \a -> cRoot c a (show (ptrToIntPtr' a)) cap
  where ptrToIntPtr' a = fromIntegral (ptrToWordPtr a) :: Integer

-- | A CAF root's label (its address, from 'cafRoots') as its symbol.
cafName :: String -> IO String
cafName lab = do
  f <- maybe nullFunPtr id <$> engineSymbol "ghs_caf_name"
  case reads lab :: [(Integer, String)] of
    [(a, "")] -> do p <- callName f (wordPtrToPtr (fromIntegral a)); if p == nullPtr then pure lab else peekCAString p
    _ -> pure lab

foreign import ccall unsafe "dynamic" callName :: FunPtr (Ptr () -> IO CString) -> Ptr () -> IO CString

-- | The values registered with 'keep' (held by a 'StablePtr', so a reload cannot drop them).
keptRoots :: Census -> Int -> IO ()
keptRoots c cap = do
  env <- getEnvironment
  performMajorGC
  cReset c
  forM_ [ (k, v) | (k, v) <- env, "GHS_KEEP_" `isPrefixOf` k ] $ \(k, v) -> do
    let addr = case break (== '|') v of { (_, '|' : a) -> a; (a, _) -> a }
    case reads addr :: [(Integer, String)] of
      [(n, "")] -> cStable c (intPtrToPtr (fromInteger n)) (drop 9 k) cap
      _ -> pure ()

-- | What every CAF retains, by CAF and by constructor (C, ~seconds). @top@ CAFs shown.
cafReport :: Int -> Int -> IO ()
cafReport top cap = withCensus $ \c -> cafRoots c cap >> censusPrint c cafName top False

-- | The Strings under every CAF, by their first 40 characters.
cafStrings :: Int -> Int -> IO ()
cafStrings top cap = withCensus $ \c -> cafRoots c cap >> censusPrint c cafName top True

-- | What each 'StablePtr'-held value retains.
keptReport :: Int -> IO ()
keptReport cap = withCensus $ \c -> keptRoots c cap >> censusPrint c pure 12 False

-- | The Strings under the 'StablePtr'-held values.
keptStrings :: Int -> Int -> IO ()
keptStrings top cap = withCensus $ \c -> keptRoots c cap >> censusPrint c pure top True

-- | The C census on structures of KNOWN size: does it count right, and how fast (closures a second)?
-- Each value is built and forced first; the expected bytes are what the heap layout says
-- (cons 24, boxed Int/Char/Double 16, Text 32, tuple 24+8n, a byte array 16 + its length).
censusBench :: IO ()
censusBench = withCensus $ \c -> do
  let run label expected frc x = do
        _ <- evaluate (frc x)
        performMajorGC
        sp <- newStablePtr x
        cReset c
        t0 <- getMonotonicTime
        cStable c (castStablePtrToPtr sp) label 1000000000
        t1 <- getMonotonicTime
        rs <- cRows c
        freeStablePtr sp
        case rs of
          ((_, v) : _) -> printf "%-36s %9.1f MB (expect %7.1f)  %9d closures  %8.1f ms  %6.1f M/s\n" label (fromIntegral (v !! 0) / 1e6 :: Double) (expected / 1e6 :: Double) (v !! 1) ((t1 - t0) * 1000) (fromIntegral (v !! 1) / (t1 - t0) / 1e6 :: Double)
          _ -> putStrLn (label ++ ": no row")
      n = 2000000 :: Int
  run "[Int] 2M (cons + boxed Int)" (fromIntegral n * 40) sum (map (+ 1000) [1 .. n])
  run "[String] 200k, 6 chars each" (200000 * 6 * 24 + 200000 * 24) (sum . map length) (map show [100000 .. 299999 :: Int])
  let shared = replicate 1000 'x'
  run "100k refs to ONE 1000-char string" (100000 * 24 + 1000 * 24) (\xs -> length shared + length xs) (replicate 100000 shared)
  run "IntMap Int Int 500k" 0 IMs.size (IMs.fromList [ (i, i + 1000) | i <- [1 .. 500000] ])
  let buf = Tx.replicate 10000000 (Tx.pack "a")
  run "1M Text slices of one 10 MB buffer" (1000000 * (32 + 24) + 10000000) (sum . map Tx.length) [ Tx.take 5 (Tx.drop i buf) | i <- [0, 7 .. 7000000] ]
  run "tuples (Int, Double) 1M" (1000000 * (24 + 24 + 16 + 16)) (\xs -> sum (map fst xs) + sum (map (truncate . snd) xs)) [ (i + 1000, fromIntegral i + 0.5 :: Double) | i <- [1 .. 1000000 :: Int] ]

-- | The running session's memory in about three seconds, with no restart: the live heap and what the
-- RTS holds (after a forced major GC), and the census's total over every CAF and every kept value.
-- A restart ('measure') is only for the RTS's own overhead; for "did this edit shrink anything" this is enough.
memNow :: IO ()
memNow = do
  performMajorGC
  s <- getRTSStats
  let g = gc s
  printf "live %d MB, RTS holds %d MB, major GCs so far %d\n" (gcdetails_live_bytes g `div` 1000000) (gcdetails_mem_in_use_bytes g `div` 1000000) (major_gcs s)
  withCensus $ \c -> do
    cafRoots c 1000000000
    cs <- cRows c
    let tot i = sum [ v !! i | (_, v) <- cs ]
    printf "CAFs retain %.1f MB (list cells %.1f, byte arrays %.1f)\n" (fromIntegral (tot 0) / 1e6 :: Double) (fromIntegral (tot 2) / 1e6 :: Double) (fromIntegral (tot 3) / 1e6 :: Double)
    keptRoots c 1000000000
    ks <- cRows c
    let totk i = sum [ v !! i | (_, v) <- ks ]
    printf "kept values retain %.1f MB (byte arrays %.1f)\n" (fromIntegral (totk 0) / 1e6 :: Double) (fromIntegral (totk 3) / 1e6 :: Double)

-- | The big MM values, each measured ALONE (a fresh census per value, so nothing is attributed to an
-- earlier root), with the constructors it is made of.


-- | What ONE value retains (already evaluated or not), alone: bytes, closures and its constructors.
-- @GHC.Hygiene.Census.censusOf "idx" (My.Module.index v)@
censusOf :: String -> a -> IO ()
censusOf label x = withCensus $ \c -> do
  _ <- evaluate x
  performMajorGC
  sp <- newStablePtr x
  cReset c
  cStable c (castStablePtrToPtr sp) label 1000000000
  rs <- cRows c
  is <- cInfos c
  freeStablePtr sp
  case rs of
    ((_, r) : _) -> do
      printf "%s: %.2f MB in %d closures\n" label (fromIntegral (r !! 0) / 1e6 :: Double) (r !! 1)
      forM_ (take 5 (sortOn (Down . (!! 0) . snd) is)) $ \(l, v) -> printf "    %7.2f MB %9d  %s\n" (fromIntegral (v !! 0) / 1e6 :: Double) (v !! 1) (reverse (take 44 (reverse l)))
    _ -> pure ()

-- | Time an action in the session: wall, GC and allocation, and the live heap before and after. The tool
-- for "is this slow, and is it the GC or the work?" without a profiling build.
-- @GHC.Hygiene.Census.benchOf "rebuild" My.Module.rebuild@
benchOf :: String -> IO a -> IO a
benchOf label act = do
  performMajorGC
  s0 <- getRTSStats
  t0 <- getMonotonicTime
  r <- act
  t1 <- getMonotonicTime
  s1 <- getRTSStats
  performMajorGC
  s2 <- getRTSStats
  let mb x = fromIntegral x / 1e6 :: Double
      g = gc s2
  printf "[bench] %s: %.2f s wall, %.2f s GC (%d major, %d minor), %.0f MB allocated, live %.0f -> %.0f MB\n" label (t1 - t0)
    (fromIntegral (gc_elapsed_ns s1 - gc_elapsed_ns s0) / 1e9 :: Double) (major_gcs s1 - major_gcs s0) (gcs s1 - gcs s0 - (major_gcs s1 - major_gcs s0))
    (mb (allocated_bytes s1 - allocated_bytes s0)) (mb (gcdetails_live_bytes (gc s0))) (mb (gcdetails_live_bytes g))
  pure r

-- | Register a value for 'keptReport' / 'keptStrings'. The address lives in an environment variable, because a
-- Haskell-side registry would be a CAF and a @:reload@ resets those.
keep :: String -> a -> IO ()
keep name x = do
  sp <- newStablePtr x
  setEnv ("GHS_KEEP_" ++ name) (show (fromIntegral (ptrToWordPtr (castStablePtrToPtr sp)) :: Integer))
