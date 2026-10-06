{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | __A memo for the stages of a computation that outlives a @:reload@, and the answer to "why did that
-- run again?"__
--
-- A reload throws away every value of the modules it links again. A long computation is a chain of
-- stages of which an edit changes few; a stage here is remembered under a hash of the VALUES it reads:
--
-- > paint = kept "paint front" [("code", code), ("view", viewHash v), ("boxes", boxesHash)] edgesHash (render sc v)
--
-- * __The scheduler is laziness__: a stage runs when something asks for its value. There is no task
--   monad and no graph to declare; the graph is the inputs each stage names.
-- * __A stage's value carries its own hash__ ('Hashed'), so the next stage's key is a combination of
--   the hashes handed to it -- and a stage whose output did not change stops the recomputation there
--   (early cutoff).
-- * __Equal values are held once__: a stage whose output hash is that of a value already in the table,
--   at the same type, keeps THAT value (so the output hash must identify the value).
-- * __A slot keeps its last two keys__, so an edit and its revert both hit ('keptN' to choose).
-- * __Every decision is logged__ with the input that moved: 'why' (@ghci-session why@) prints what ran
--   again since it was last asked, why, and what it cost; 'timed' puts an action that is no stage in
--   the same account.
-- * __@KEPT_VERIFY=1@__ recomputes on every hit and says if the kept value's hash differs: a key that
--   misses an input is found by running the project's tests once with it set. @KEPT_SKIP@ (@all@, or
--   parts of slot names) bypasses the memo. Both are read from the process's environment at each
--   decision: set them in the session (@System.Environment.setEnv@).
--
-- The table is a slot of "GHC.Hygiene.Store". What it holds is untyped, so the rules are the caller's:
-- a slot names ONE value of ONE type; __the code that computes a stage and defines its types is an
-- input like any other__ ('sourceHash' of the files, under a label such as @"code"@) -- a kept value
-- must never be read by code compiled against another layout; and a kept value must be FORCED (its
-- output hash walks it), because a thunk in it holds the code of the generation that built it.
module GHC.Hygiene.Kept
  ( -- * Stages
    Hash, Hashed (..), hashed
  , kept, keptN, keptOpaque, sourceHash, forget
    -- * Hashing
  , hStart, hWord, hInt, hDouble, hDoubles, hString, hStrings, hList, hMaybe, hBytes, hCombine
    -- * Why did it run
  , why, whyAll, noteDecision, timed
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isSuffixOf, sortOn)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Word (Word64)
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, splitTyConApp, tyConPackage, typeRep, typeRepFingerprint)
import GHC.Fingerprint (Fingerprint (..))
import GHC.Clock (getMonotonicTime)
import GHC.Exts (Any)
import GHC.Float (castDoubleToWord64)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

import System.Directory (getFileSize, getModificationTime)
import Data.Time.Clock (UTCTime)
import Control.Exception (SomeException, try)

import GHC.Hygiene.Store (storeRef)

type Hash = Word64

-- | A value and the hash of it the next stage keys on.
data Hashed a = Hashed { hValue :: a, hHash :: Hash }

-- | A value that is no stage's output (data read as it is), hashed.
hashed :: (a -> Hash) -> a -> Hashed a
hashed f x = Hashed x (f x)

-- | How many keys a slot remembers (the newest first): two, so an edit and its revert both hit.
entriesKept :: Int
entriesKept = 2
type Key = [(String, Hash)]

-- | A value's type, as the table knows it (its 'TypeRep''s fingerprint): an entry is only ever handed back
-- at the type it was stored at, and only values of one type are compared for sharing.
type TypeKey = (Word64, Word64)

typeKey :: forall a. Typeable a => Proxy a -> TypeKey
typeKey p = case typeRepFingerprint (typeRep p) of Fingerprint a b -> (a, b)

-- | The type AND the code its values were built by, for a type a reload can change.
--
-- A 'TypeRep' is a type's NAME. A type of the program being edited keeps its name when its definition
-- changes, and a value built before the change is laid out the old way: handed to code compiled after
-- it (two stages with one output hash, "the same value"), its fields are read at the wrong places --
-- an edit that unpacked a record's fields and its revert killed the session. So for a type that
-- mentions any constructor of a package built in place (or of no package: GHCi's own modules), the key
-- also carries the stage's input labelled @"code"@ -- what its values' representation depends on -- and
-- with no such input, all its inputs (it is then shared with nothing but its own earlier key). A type
-- made only of installed packages' types (bytes, text, numbers, containers of them) is laid out one way
-- for the life of the process and is shared across everything.
layoutKey :: forall a. Typeable a => Proxy a -> Key -> TypeKey
layoutKey p key
  | fixed (typeRep p) = (a, b)
  | otherwise = (a, b `xor` (code * 0x9E3779B97F4A7C15 + 1))
  where
    (a, b) = typeKey p
    code = maybe (keyHash key) id (lookup "code" key)
    fixed r = let (c, args) = splitTyConApp r; pkg = tyConPackage c
              in not ("-inplace" `isSuffixOf` pkg) && pkg /= "main" && not ("interactive" `isInfixOf` pkg) && all fixed args

data Table = Table
  { tSlots :: !(M.Map String [(Key, Any, Hash, TypeKey)])
  , tLog :: ![Decision]      -- newest first, capped
  , tSeq :: !Int
  , tMark :: !Int            -- the last decision 'why' showed
  }

data Decision = Decision { dSeq :: !Int, dSlot :: !String, dHit :: !Bool, dWhy :: !String, dSecs :: !Double
                         , dKeySecs :: !Double   -- ^ reading its inputs: the stages before it, and whatever of them is no stage
                         }

{-# NOINLINE table #-}
table :: IORef Table
table = unsafePerformIO (storeRef "ghs.kept.v3" (Table M.empty [] 0 0))

-- | Record a decision made elsewhere (the element cache on disk), so 'why' is the one place to look:
-- slot, hit, why not, the seconds computing it, the seconds deciding (its key).
noteDecision :: String -> Bool -> String -> Double -> Double -> IO ()
noteDecision = note

note :: String -> Bool -> String -> Double -> Double -> IO ()
note slot hit reason secs ksecs = atomicModifyIORef' table $ \t ->
  (t { tLog = take 4000 (Decision (tSeq t + 1) slot hit reason secs ksecs : tLog t), tSeq = tSeq t + 1 }, ())

-- | __A stage__: @value@ is remembered under @slot@ for as long as its labelled @inputs@ are what they
-- were. @outHash@ is the value's hash for the stages after it, and must walk everything of the value
-- that is kept (it is what forces it).
kept :: forall a. Typeable a => String -> [(String, Hash)] -> (a -> Hash) -> a -> Hashed a
kept = keptN entriesKept

-- | 'kept' remembering @n@ keys: one for a value of megabytes.
keptN :: forall a. Typeable a => Int -> String -> [(String, Hash)] -> (a -> Hash) -> a -> Hashed a
keptN n slot inputs outHash v = unsafePerformIO (keptIO True n slot inputs (const outHash) v)
{-# NOINLINE keptN #-}

-- | A stage whose value has no cheap hash (a raster): @force@ walks it, and its hash for the stages
-- after it is its KEY's -- sound, but with no early cutoff: when its inputs move, so do its readers.
-- It keeps one key.
keptOpaque :: forall a. Typeable a => String -> [(String, Hash)] -> (a -> ()) -> a -> Hashed a
keptOpaque slot inputs force v = unsafePerformIO (keptIO False 1 slot inputs (\k x -> force x `seq` k) v)
{-# NOINLINE keptOpaque #-}

-- | The contents of source files as one hash: the code a stage runs, as an input. A file is read
-- again only when its size or modification time changed; a missing one hashes as missing.
sourceHash :: [FilePath] -> IO Hash
sourceHash fs = do
  hs <- mapM one fs
  pure (hCombine hs)
  where
    one f = do
      st <- try ((,) <$> getModificationTime f <*> getFileSize f) :: IO (Either SomeException (UTCTime, Integer))
      case st of
        Left _ -> pure (hString "missing" (hString f hStart))
        Right stamp -> do
          m <- readIORef fileHashes
          case M.lookup f m of
            Just (st', h) | st' == stamp -> pure h
            _ -> do
              b <- BS.readFile f
              let h = hBytes b hStart
              atomicModifyIORef' fileHashes (\x -> (M.insert f (stamp, h) x, ()))
              pure h

{-# NOINLINE fileHashes #-}
fileHashes :: IORef (M.Map FilePath ((UTCTime, Integer), Hash))
fileHashes = unsafePerformIO (storeRef "ghs.kept.files.v2" M.empty)

keyHash :: Key -> Hash
keyHash = L.foldl' (\h (l, x) -> hWord x (hString l h)) hStart

-- | Empty the table: every stage is computed again when next asked. What a value held only by the table
-- cost is the live heap before this and after (a value some code still holds stays).
forget :: IO ()
forget = atomicModifyIORef' table (\t -> (t { tSlots = M.empty }, ()))

keptIO :: forall a. Typeable a => Bool -> Int -> String -> Key -> (Hash -> a -> Hash) -> a -> IO (Hashed a)
keptIO share nKeep slot inputs outHash v = do
  skip <- maybe [] words <$> lookupEnv "KEPT_SKIP"
  if any (`isInfixOf` slot) skip || skip == ["all"] then pure (Hashed v (outHash (keyHash inputs) v)) else keptIO' share nKeep slot inputs outHash v

keptIO' :: forall a. Typeable a => Bool -> Int -> String -> Key -> (Hash -> a -> Hash) -> a -> IO (Hashed a)
keptIO' share nKeep slot inputs outHash v = do
  let key = inputs
      ty = layoutKey (Proxy :: Proxy a) key
  -- its inputs are read here: the stages before it run inside this, and are taken out of its time ('own')
  (_, ksecs) <- own (evaluate (sum (map snd key)))
  t <- readIORef table
  let have = M.findWithDefault [] slot (tSlots t)
  case [ (x, h) | (k, x, h, ty') <- have, k == key, ty' == ty ] of
    ((x, h) : _) -> do
      note slot True "" 0 ksecs
      verify <- (== Just "1") <$> lookupEnv "KEPT_VERIFY"
      when verify $ do
        h' <- evaluate (outHash (keyHash key) v)
        when (h' /= h) $ do
          let msg = "KEPT_VERIFY: " ++ slot ++ " MISMATCH: recomputed under the same key, the value differs -- the key misses an input"
          hPutStrLn stderr msg
          note slot False msg 0 0
      pure (Hashed (unsafeCoerce x) h)
    [] -> do
      let reason = case have of
            [] -> "first time"
            ((k, _, _, _) : _)
              | map fst k /= map fst key -> "its inputs are other inputs"
              | otherwise -> "changed: " ++ unwords [ l | ((l, a), (_, b)) <- zip key k, a /= b ]
      (h, secs) <- own (evaluate (outHash (keyHash key) v))
      -- The same value may already be held, under another slot or another key of this one (a view painted
      -- for two purposes, an edit and its revert): its output hash is what the stages after it compare, so
      -- an entry of the SAME TYPE with the same hash is the same value, and that one is kept instead of a
      -- second copy (a 4.4 MB painted view was held four times). "The same type" includes the code that
      -- lays it out ('layoutKey').
      t' <- readIORef table
      let twin = if share then listToMaybe [ (s', x) | (s', es) <- M.toList (tSlots t'), (_, x, h', ty') <- es, h' == h, ty' == ty ] else Nothing
          held = maybe (unsafeCoerce v :: Any) snd twin
      atomicModifyIORef' table $ \tb ->
        (tb { tSlots = M.insert slot (take nKeep ((key, held, h, ty) : [ e | e@(k, _, _, _) <- M.findWithDefault [] slot (tSlots tb), k /= key ])) (tSlots tb) }, ())
      note slot False (reason ++ maybe "" (\(s', _) -> if s' == slot then " (the value it had before: shared)" else " (same value as " ++ s' ++ ": shared)") twin) secs ksecs
      pure (Hashed (unsafeCoerce held) h)

-- | An action's OWN seconds: the stages that ran inside it (asked for by its laziness) taken out.
own :: IO a -> IO (a, Double)
own act = do
  outer <- atomicModifyIORef' inner (\x -> (0, x))
  t0 <- getMonotonicTime
  x <- act
  t1 <- getMonotonicTime
  mine <- atomicModifyIORef' inner (\nested -> (outer + (t1 - t0), nested))
  pure (x, max 0 (t1 - t0 - mine))

{-# NOINLINE inner #-}
inner :: IORef Double
inner = unsafePerformIO (newIORef 0)

-- | An action that is NOT kept, in the log with its own seconds (the stages it asked for taken out): a
-- golden's run, a command. So 'why' accounts for the whole of a command, and what it shows as "not a
-- stage" is what to make one next.
timed :: String -> IO a -> IO a
timed name act = do
  (x, secs) <- own act
  note name False "not a stage: runs every time" secs 0
  pure x

-- | What ran again since 'why' was last asked: each stage that missed, the input that moved, and the
-- seconds of its OWN it took (the stages it asked for are their own lines).
why :: IO ()
why = do
  t <- atomicModifyIORef' table (\tb -> (tb { tMark = tSeq tb }, tb))
  report [ d | d <- tLog t, dSeq d > tMark t ]

-- | Every decision still in the log (the last 4,000).
whyAll :: IO ()
whyAll = readIORef table >>= report . tLog

report :: [Decision] -> IO ()
report ds = do
  let misses = [ d | d <- reverse ds, not (dHit d) ]
      hits = length [ () | d <- ds, dHit d ]
      -- a slot decided more than once: its seconds summed, its last reason
      bySlot = M.fromListWith (\(n, a, _) (m, b, d) -> (n + m, a + b, d)) [ (dSlot d, (1 :: Int, dSecs d, d)) | d <- reverse misses ]
      unstaged = M.fromListWith (+) [ (dSlot d, dKeySecs d) | d <- ds, dKeySecs d >= 0.05 ]
  if null ds then putStrLn "kept: nothing decided since the last why" else do
    printf "kept: %d stage(s) recomputed in %.1f s of their own, %d reused; %.1f s reading inputs that are no stage\n"
      (M.size bySlot) (sum [ a | (_, a, _) <- M.elems bySlot ]) hits (sum (map dKeySecs ds))
    forM_ (sortOn (\(_, (_, a, _)) -> negate a) (M.toList bySlot)) $ \(s, (n, a, d)) ->
      printf "  %6.2f s  %-44s %s%s\n" a s (dWhy d) (if n > 1 then printf "  (x%d)" n else "" :: String)
    -- work that is no stage shows as the time a stage took to READ its inputs (the stages before it
    -- taken out): what to make a stage next
    when (not (M.null unstaged)) $ do
      putStrLn "  inputs that are no stage, by the stage that read them first:"
      forM_ (sortOn (negate . snd) (M.toList unstaged)) $ \(s, k) -> printf "  %6.2f s  read by %s\n" k s

-- ---------------------------------------------------------------------------
-- Hashing (FNV-1a over 64-bit words; every list is hashed with its length)

hStart :: Hash
hStart = 14695981039346656037

hWord :: Word64 -> Hash -> Hash
hWord w h = (h `xor` w) * 1099511628211
{-# INLINE hWord #-}

hInt :: Int -> Hash -> Hash
hInt i = hWord (fromIntegral i)
{-# INLINE hInt #-}

hDouble :: Double -> Hash -> Hash
hDouble x = hWord (castDoubleToWord64 x)
{-# INLINE hDouble #-}

hDoubles :: [Double] -> Hash -> Hash
hDoubles xs h = L.foldl' (flip hDouble) (hInt (length xs) h) xs

hString :: String -> Hash -> Hash
hString s h = L.foldl' (\a c -> hInt (fromEnum c) a) (hWord 7919 h) s

hStrings :: [String] -> Hash -> Hash
hStrings = hList hString

hList :: (a -> Hash -> Hash) -> [a] -> Hash -> Hash
hList f xs h = L.foldl' (flip f) (hInt (length xs) h) xs

hMaybe :: (a -> Hash -> Hash) -> Maybe a -> Hash -> Hash
hMaybe f m h = maybe (hInt 0 h) (\x -> f x (hInt 1 h)) m

hBytes :: BS.ByteString -> Hash -> Hash
hBytes b h = BS.foldl' (\a w -> hWord (fromIntegral w) a) (hInt (BS.length b) h) b

-- | Hashes of other stages, in order, as one.
hCombine :: [Hash] -> Hash
hCombine = L.foldl' (flip hWord) hStart
