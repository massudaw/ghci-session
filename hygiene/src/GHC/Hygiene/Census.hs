{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | __A heap census for a GHCi session__, done in C (@hygiene/c/heap_census.c@): what every CAF, and every
-- value you 'keep', retains, by constructor, with the Strings among it. A full CAF census is seconds, not
-- minutes. 'censusOf' and 'benchOf' answer one question about one value or action without a restart.
--
-- The C is part of the session's engine; in any other GHCi every entry point says so and does nothing.
module GHC.Hygiene.Census
  ( cafReport, cafStrings, keptReport, keptStrings, keep
  , censusOf, benchOf, benchQuick, memNow, censusBench
  , dupsCafs, dupsKept, dupsOf
  , readSymbol, zdecode
  ) where

import GHC.Hygiene.Store (storeRoots)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, when)
import Data.Maybe (isNothing)
import qualified Data.IntMap.Strict as IMs
import Data.Int (Int64)
import Data.List (isPrefixOf, sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as Tx
import Foreign.C.String (CString, newCAString, peekCAString, withCAString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes, free)
import Foreign.Marshal.Array (allocaArray, peekArray, withArray)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr, intPtrToPtr, nullFunPtr, nullPtr, ptrToWordPtr, wordPtrToPtr)
import Foreign.StablePtr (castStablePtrToPtr, freeStablePtr, newStablePtr)
import System.IO (hFlush, stdout)
import Foreign.Storable (peekElemOff)
import GHC.Clock (getMonotonicTime)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import System.Environment (getEnvironment, setEnv)
import System.Mem (performMajorGC, performMinorGC)
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
    -- the walk's own tables go back: 128 MB of them stayed for the life of a session after one census
    engineSymbol "ghs_cen_done" >>= maybe (pure ()) (cenReset . castFunPtr)

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

-- ---------------------------------------------------------------------------------------------------
-- Sharing that is missed (@hygiene/c/heap_dups.c@)

foreign import ccall unsafe "dynamic" dupAll :: FunPtr (Ptr (Ptr ()) -> Ptr CString -> CInt -> CInt -> CInt -> IO CString) -> Ptr (Ptr ()) -> Ptr CString -> CInt -> CInt -> CInt -> IO CString

-- | Run a duplicate analysis over roots -- static closures (CAFs: no label, the C names them) or
-- 'StablePtr'-held values with their labels -- and print the report, @top@ lines a section. One foreign
-- call for the whole of it: the analysis keys closures by address, and the collector must not run inside.
runDups :: Bool -> [(Ptr (), Maybe String)] -> Int -> IO ()
runDups stable roots top = do
  f <- engineSymbol "ghs_dup_all"
  case f of
    Nothing -> putStrLn "no duplicate analysis in this process: it is part of the session's engine (ghci-session-engine)"
    Just g -> do
      hFlush stdout
      let n = length roots
      labels <- mapM (maybe (pure nullPtr) newCAString . snd) roots
      -- the report comes back as text and is printed HERE: written from inside the call it filled the
      -- engine's output pipe, which nothing can drain while an unsafe call runs
      text <- withArray (map fst roots) $ \ps -> withArray labels $ \ls ->
        dupAll (castFunPtr g) ps (if all isNothing (map snd roots) then nullPtr else ls) (fromIntegral n) (if stable then 1 else 0) (fromIntegral top)
      when (text /= nullPtr) (peekCAString text >>= putStr >> free text)
      mapM_ (\l -> when (l /= nullPtr) (free l)) labels

-- | __Sharing that is missed, over every CAF__: the closures that are structurally equal to one already
-- reached -- how many bytes maximal sharing would give back, by constructor, by the CAF that holds the
-- copies, and the largest repeated values.
dupsCafs :: Int -> IO ()
dupsCafs top = do
  performMajorGC
  f <- maybe nullFunPtr id <$> engineSymbol "ghs_caf_list"
  let n = 200000
  as <- allocaArray n $ \addrs -> do
    got <- fromIntegral <$> callCafList f (castPtr addrs) nullPtr (fromIntegral n)
    peekArray (min got n) (addrs :: Ptr (Ptr ()))
  runDups False [ (a, Nothing) | a <- as ] top

-- | The same over the values a reload cannot drop (the slots of "GHC.Hygiene.Store", what 'keep' registered).
dupsKept :: Int -> IO ()
dupsKept top = do
  performMinorGC
  env <- getEnvironment
  let kept = [ (intPtrToPtr (fromInteger n), Just (drop 9 k)) | (k, v) <- env, "GHS_KEEP_" `isPrefixOf` k
             , [(n, "")] <- [reads (case break (== '|') v of { (_, '|' : a) -> a; (a, _) -> a }) :: [(Integer, String)]] ]
  slots <- storeRoots
  runDups True (kept ++ [ (castPtr p, Just k) | (k, p) <- slots ]) top

-- | The same within ONE value (as far as it has been evaluated: a thunk is only ever itself).
dupsOf :: String -> a -> Int -> IO ()
dupsOf label x top = do
  _ <- evaluate x
  performMinorGC
  sp <- newStablePtr x
  runDups True [(castStablePtrToPtr sp, Just label)] top
  freeStablePtr sp

-- | Every CAF the RTS roots ('ghs_caf_list'), walked in list order (newest first): what each retains that
-- no earlier one did. @top@ roots shown; @cap@ closures at most a root.
cafRoots :: Census -> Int -> IO ()
cafRoots c cap = performMajorGC >> cafRootsNow c cap

-- | 'cafRoots' on the heap as it stands (the caller has just collected).
cafRootsNow :: Census -> Int -> IO ()
cafRootsNow c cap = do
  f <- maybe nullFunPtr id <$> engineSymbol "ghs_caf_list"
  let n = 200000
  allocaArray n $ \addrs -> do
    -- no names here: `dladdr` is ~1 ms a CAF, 10 s for 8k of them; roots are labelled by address
    -- and the few that get printed are named afterwards ('cafName')
    got <- fromIntegral <$> callCafList f (castPtr addrs) nullPtr (fromIntegral n)
    as <- peekArray (min got n) (addrs :: Ptr (Ptr ()))
    cReset c
    forM_ as $ \a -> cRoot c a (show (ptrToIntPtr' a)) cap
  where ptrToIntPtr' a = fromIntegral (ptrToWordPtr a) :: Integer

-- | A CAF root's label (its address, from 'cafRoots') as its name: @unit:Module.name@, or "a local CAF of
-- Module" for one the compiler made.
cafName :: String -> IO String
cafName lab = do
  f <- maybe nullFunPtr id <$> engineSymbol "ghs_caf_name"
  case reads lab :: [(Integer, String)] of
    [(a, "")] -> do p <- callName f (wordPtrToPtr (fromIntegral a)); if p == nullPtr then pure lab else readSymbol <$> peekCAString p
    _ -> pure lab

-- | A symbol as GHC writes it, read back: @hellozm0zi1zi0zi0zminplace_Hello_bigTable_closure@ is
-- @hello-0.1.0.0-inplace:Hello.bigTable@ (the unit, the module and the name, each z-encoded, joined by @_@;
-- an @_@ inside any of them is encoded). Anything else is left as it is.
readSymbol :: String -> String
readSymbol s = case parts (dropSuffix "_closure" s) of
  [u, m, n] | all (not . null) [u, m, n] -> zdecode u ++ ":" ++ zdecode m ++ "." ++ zdecode n
  _ -> s
  where
    dropSuffix suf x = if reverse suf == take (length suf) (reverse x) then take (length x - length suf) x else x
    parts x = case break (== '_') x of { (a, '_' : r) -> a : parts r; (a, _) -> [a] }

-- | GHC's z-encoding undone (GHC.Utils.Encoding: @zi@ is @.@, @zm@ @-@, @zu@ @_@, @ZC@ @:@, ...). A code
-- it does not know stays as written.
zdecode :: String -> String
zdecode ('z' : c : r) | Just d <- lookup c lower = d : zdecode r
zdecode ('Z' : c : r) | Just d <- lookup c upper = d : zdecode r
zdecode (c : r) = c : zdecode r
zdecode [] = []

lower, upper :: [(Char, Char)]
lower = [ ('z', 'z'), ('a', '&'), ('b', '|'), ('c', '^'), ('d', '$'), ('e', '='), ('g', '>'), ('h', '#'), ('i', '.'), ('l', '<')
        , ('m', '-'), ('n', '!'), ('p', '+'), ('q', '\''), ('r', '\\'), ('s', '/'), ('t', '*'), ('u', '_'), ('v', '%') ]
upper = [ ('Z', 'Z'), ('L', '('), ('R', ')'), ('M', '['), ('N', ']'), ('C', ':') ]

foreign import ccall unsafe "dynamic" callName :: FunPtr (Ptr () -> IO CString) -> Ptr () -> IO CString

-- | The values a reload cannot drop: those registered with 'keep', and the slots of "GHC.Hygiene.Store".
keptRoots :: Census -> Int -> IO ()
keptRoots c cap = do
  env <- getEnvironment
  -- a walk from a root reaches only what is live, collected or not: the collection is for the
  -- indirections a just-evaluated thunk leaves, and those are young -- a minor one removes them (a major
  -- one is 0.2 s of even a small session, and was all of this command's time)
  performMinorGC
  cReset c
  forM_ [ (k, v) | (k, v) <- env, "GHS_KEEP_" `isPrefixOf` k ] $ \(k, v) -> do
    let addr = case break (== '|') v of { (_, '|' : a) -> a; (a, _) -> a }
    case reads addr :: [(Integer, String)] of
      [(n, "")] -> cStable c (intPtrToPtr (fromInteger n)) (drop 9 k) cap
      _ -> pure ()
  -- and every slot of the engine's store ("GHC.Hygiene.Store")
  slots <- storeRoots
  forM_ slots $ \(k, p) -> cStable c (castPtr p) k cap

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
    cafRootsNow c 1000000000
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
  performMinorGC   -- (see 'keptRoots')
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
  (r, t, s1) <- timedAct act
  performMajorGC
  s2 <- getRTSStats
  printf "[bench] %s: %s, live %.0f -> %.0f MB\n" label (benchLine t s0 s1) (mb (gcdetails_live_bytes (gc s0))) (mb (gcdetails_live_bytes (gc s2)))
  pure r

-- | 'benchOf' without its two forced collections: the action's wall time, GC and allocation, and nothing
-- about the live heap. The collections are 0.2 s each in even a small session, which was nearly all of a
-- quick action's bench; without one first, the action's GC time can include a little of what was garbage
-- before it ran.
benchQuick :: String -> IO a -> IO a
benchQuick label act = do
  s0 <- getRTSStats
  (r, t, s1) <- timedAct act
  printf "[bench] %s: %s\n" label (benchLine t s0 s1)
  pure r

timedAct :: IO a -> IO (a, Double, RTSStats)
timedAct act = do
  t0 <- getMonotonicTime
  r <- act
  t1 <- getMonotonicTime
  s1 <- getRTSStats
  pure (r, t1 - t0, s1)

benchLine :: Double -> RTSStats -> RTSStats -> String
benchLine t s0 s1 = printf "%.2f s wall, %.2f s GC (%d major, %d minor), %.0f MB allocated%s" t
  (fromIntegral (gc_elapsed_ns s1 - gc_elapsed_ns s0) / 1e9 :: Double) (major_gcs s1 - major_gcs s0) (gcs s1 - gcs s0 - (major_gcs s1 - major_gcs s0))
  (mb (allocated_bytes s1 - allocated_bytes s0)) peak
  where
    -- The most that was live at a major collection (what `+RTS -s` calls the maximum residency): the process's
    -- own, since it began -- so it is the action's when the action raised it, and otherwise only a bound.
    peak :: String
    peak | major_gcs s1 == major_gcs s0 = ""
         | max_live_bytes s1 > max_live_bytes s0 = printf ", %.0f MB live at most (a new most for this process)" (mb (max_live_bytes s1))
         | otherwise = printf ", under %.0f MB live (the most this process has had)" (mb (max_live_bytes s1))

mb :: Integral a => a -> Double
mb x = fromIntegral x / 1e6

-- | Register a value for 'keptReport' / 'keptStrings'. The address lives in an environment variable, because a
-- Haskell-side registry would be a CAF and a @:reload@ resets those.
keep :: String -> a -> IO ()
keep name x = do
  sp <- newStablePtr x
  setEnv ("GHS_KEEP_" ++ name) (show (fromIntegral (ptrToWordPtr (castStablePtrToPtr sp)) :: Integer))
