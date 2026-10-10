-- | __A replay of the rollover controller__ over recorded chat logs (@chat --replay-rollover FILE...@): the usage lines
-- of a log ("[usage: in N, out M, cached C (P%), Ts]") are the calls; the context grows as it grew there, a reset
-- is taken where the policy takes it, and what a reset costs is what a fresh call costs -- so a policy can be judged
-- before it is trusted. It is a model, not a run: the growth a call is what the log had (not what an agent that had
-- been reset would have done), boundaries, the cold cache and the rot signal are not replayed (they need the tool
-- calls' times and answers), and the relearn share is the start's. Prices are list prices of a model
-- (read 0.30, output 15 dollars a million tokens; a write is the ratio times a read). The parts are pure.
module GhciSession.Replay
  ( Call (..), parseCalls, growths, freshSize, freshCached, Policy (..), Sim (..), simulate, replayReport, replayMain
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.List (isPrefixOf, sort, stripPrefix)
import Text.Printf (printf)

import GhciSession.Roll

-- | A model call as the log has it: its context, what of it was read from the cache, and what it wrote.
data Call = Call { cIn :: Int, cCached :: Int, cOut :: Int }
  deriving (Eq, Show)

-- | The calls of a log, in order.
parseCalls :: String -> [Call]
parseCalls s = [ c | l <- lines s, "[usage: in " `isPrefixOf` l, Just c <- [usage l] ]
  where
    usage l = do
      r <- stripPrefix "[usage: in " l
      [(i, r1)] <- Just (reads r)
      r2 <- stripPrefix ", out " r1
      [(o, r3)] <- Just (reads r2)
      r4 <- stripPrefix ", cached " r3
      [(c, _)] <- Just (reads r4)
      pure (Call i c o)

median :: [Int] -> Maybe Int
median [] = Nothing
median xs = Just (sort xs !! (length xs `div` 2))

-- | What a call added to the context, for each call (the first: nothing): as the log had it where it grew by less
-- than 20k; where it did not (a reset, or an image) the median growth.
growths :: [Call] -> [Int]
growths cs = 0 : [ if b > a && b - a <= 20000 then b - a else fallback | (Call a _ _, Call b _ _) <- zip cs (drop 1 cs) ]
  where fallback = maybe 1500 id (median [ b - a | (Call a _ _, Call b _ _) <- zip cs (drop 1 cs), b > a, b - a <= 20000 ])

-- | What a fresh call's context was in the log: the median of the calls that began a context under 70% of the one
-- before (50k if there are none).
freshSize :: [Call] -> Int
freshSize cs = maybe 50000 id (median [ b | (Call a _ _, Call b _ _) <- zip cs (drop 1 cs), b * 10 < a * 7 ])

-- | What a fresh call read from the cache in the log: the median of those calls' cached tokens (7.7k, a system
-- prompt's and tools', if there are none).
freshCached :: [Call] -> Int
freshCached cs = maybe 7700 id (median [ cCached b | (Call a _ _, b) <- zip cs (drop 1 cs), cIn b * 10 < a * 7 ])

-- | 'Auto' prices a reset at what it writes (the context less what a fresh call has cached); 'AutoWhole' at the
-- whole context, as the controller did before.
data Policy = Fixed Int | Auto | AutoWhole
  deriving (Eq, Show)

-- | What a policy did: the contexts at which it reset, the dollars, the calls, the mean context.
data Sim = Sim { smResets :: [Int], smCost :: Double, smCalls :: Int, smMean :: Double }
  deriving (Eq, Show)

-- | @simulate ratio fresh cached policy calls@: the calls with their growths, a reset after a call whose context is over
-- the policy's limit (the controller's, learnt from the calls as they come, or a fixed one). A reset's next call
-- starts at @fresh@ tokens, all written, and the agent reads again a part of what it had read (the controller's
-- relearn share of a fresh call, written too).
simulate :: Double -> Int -> Int -> Policy -> [Call] -> Sim
simulate ratio fresh cached pol calls = go (zip calls (growths calls)) 0 0 True False ((emptyRoll ratio) { rP = if pol == Auto then fromIntegral cached else 0 }) [] 0 0 0
  where
    pr = 0.30e-6
    pw = ratio * pr
    po = 15e-6
    n = case pol of { Fixed k -> k; _ -> -1 }
    relearn = rRelearn (emptyRoll ratio)
    -- the calls left, the context of the call before (0: none), the run's first context (not kept: a fresh call is
    -- the start), is this call the first of a run, did a reset just come, the controller, resets, cost, sum of contexts, calls
    go [] _ _ _ _ _ rs cost sumC k = Sim (reverse rs) cost k (if k == 0 then 0 else sumC / fromIntegral k)
    go ((c, d) : rest) prev _ firstCall reset ro rs cost sumC k =
      let isFresh = firstCall || reset
          ctx | firstCall = cIn c
              | reset = fresh
              | otherwise = prev + d
          cachedPart | reset = cached
                     | otherwise = cCached c
          spent | isFresh = fromIntegral (ctx - min ctx cachedPart) * pw + fromIntegral (min ctx cachedPart) * pr + (if reset then relearn * fromIntegral fresh * pw else 0)
                | otherwise = fromIntegral prev * pr + fromIntegral (ctx - prev) * pw
          ro' = (if pol == Auto then seeCached isFresh cachedPart else id) (seeCall isFresh prev ctx ro)
          over = maybe False (ctx >) (limitOf n ro')
      in go rest ctx 0 False over ro' (if over then ctx : rs else rs) (cost + spent + fromIntegral (cOut c) * po) (sumC + fromIntegral ctx) (k + 1)

-- | The report on the logs, as text: for each, the policies side by side.
replayReport :: Double -> [(String, [Call])] -> String
replayReport ratio logs = unlines (concatMap one logs)
  where
    one (name, cs)
      | length cs < 20 = [name ++ ": " ++ show (length cs) ++ " calls: too few to replay"]
      | otherwise =
          [ printf "%s: %d calls, a fresh call %dk (the log's median), %d tokens growth a call (median), write/read ratio %.1f" name (length cs) (freshSize cs `div` 1000) (maybe 0 id (median (drop 1 (growths cs)))) ratio
          , "| policy       | resets | dollars | $ a call | mean context | contexts at the resets |"
          , "|--------------|--------|---------|----------|--------------|------------------------|" ]
          ++ [ row label (simulate ratio (freshSize cs) (freshCached cs) p cs) | (label, p) <- policies ]
          ++ [ printf "auto (reset priced net of the cached %dk) against 150k: %+.1f%%, against 110k: %+.1f%%, against auto priced whole: %+.1f%%" (freshCached cs `div` 1000) (rel auto150 (cost Auto)) (rel auto110 (cost Auto)) (rel (cost AutoWhole) (cost Auto)) ]
      where
        policies = [("fixed 150000", Fixed 150000), ("fixed 110000", Fixed 110000), ("auto, whole", AutoWhole), ("auto", Auto)]
        cost p = smCost (simulate ratio (freshSize cs) (freshCached cs) p cs)
        auto150 = cost (Fixed 150000)
        auto110 = cost (Fixed 110000)
        rel base x = if base == 0 then 0 else 100 * (x - base) / base :: Double
    row :: String -> Sim -> String
    row label s = printf "| %-12s | %6d | %7.2f | %8.4f | %11.0fk | %s |" label (length (smResets s)) (smCost s) (smCost s / fromIntegral (max 1 (smCalls s))) (smMean s / 1000) (showResets (smResets s))
    showResets rs | null rs = "-"
                  | length rs <= 8 = unwords [ show (r `div` 1000) ++ "k" | r <- rs ]
                  | otherwise = unwords [ show (r `div` 1000) ++ "k" | r <- take 8 rs ] ++ " ... (" ++ show (length rs) ++ ")"

-- | @chat --replay-rollover [--ratio R] FILE...@: no session, no model call.
replayMain :: [String] -> IO Int
replayMain args = do
  let (ratio, files) = case args of
        ("--ratio" : v : r) | [(x, "")] <- reads v -> (x, r)
        _ -> (defaultRatio, args)
  logs <- mapM (\f -> do
                  r <- try (B.readFile f) :: IO (Either IOException B.ByteString)
                  pure (f, either (Left . show) (Right . parseCalls . B8.unpack) r)) files
  -- (a log that cannot be read is said so, and is not taken for a short one)
  forM_ [ (f, why) | (f, Left why) <- logs ] $ \(f, why) -> hPutStrLn stderr ("replay: " ++ f ++ ": cannot be read: " ++ why)
  putStr (replayReport ratio [ (f, cs) | (f, Right cs) <- logs ])
  pure (if null files then 2 else if any (either (const True) (const False) . snd) logs then 1 else 0)