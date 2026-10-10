-- | __What a plan's window weighs a token at__: from the ledger (@usage.jsonl@), a least-squares fit, per window,
-- of how much its utilization rose between two calls against the tokens of the call, in four classes: uncached
-- input, read from the cache, written to it, output. The weights are said relative to an uncached input token.
-- The docs do not publish them; this is an estimate from this machine's own calls, and says how good it is.
--
-- How the pairs are made. The calls of ONE session that have windows, in time order; two neighbours make a pair when
-- both saw the window with the same reset (one period), its use did not fall, and no call of ANOTHER session's ledger
-- ended between the start of the earlier call and the end of the later one (that call's tokens would be in the rise
-- and not in the pair). A chat of another machine, or Claude Code itself, shows in no ledger here: its calls cannot be
-- told apart, and only add noise (the fit's quality says so). The event of a call is seen at its start, so the rise
-- between two calls may be the earlier call's tokens (lag 1) or the later's (lag 0): both are fitted, and the better kept.
module GhciSession.QuotaFit
  ( Obs (..), obsOf, Pair (..), pairsOf, Fit (..), fitOf, solve, trusted, impliedRatio, quotaLines, minPairs
  ) where

import Data.List (sortOn, nub, transpose)
import Data.Maybe (fromMaybe, catMaybes)
import Text.Printf (printf)

import GhciSession.Json
import GhciSession.Quota (Window (..), windowsFrom)
import GhciSession.Llm (human)

-- | A call that has windows: its session, when it ended, how long it took, its windows, and its tokens:
-- uncached input, read from the cache, written to it, output.
data Obs = Obs { oSess :: String, oT :: Double, oSecs :: Double, oWins :: [Window], oTok :: [Double] }

obsOf :: (String, Json) -> Maybe Obs
obsOf (s, r)
  | null ws = Nothing
  | otherwise = Just (Obs s (n "t") (n "secs") ws [fromMaybe (n "in" - n "cached") (lookupNum "new" r), n "cached", n "wr", n "out"])
  where ws = windowsFrom r
        n k = fromMaybe 0 (lookupNum k r)

-- | Two calls seen in one period of one window: the rise of its use, and the tokens of the earlier and the later call.
data Pair = Pair { pWin :: String, pRise :: Double, pPrev :: [Double], pNext :: [Double] }

pairsOf :: [Obs] -> [Pair]
pairsOf obs =
  [ Pair (wName wa) (wUsed wb - wUsed wa) (oTok a) (oTok b)
  | s <- nub (map oSess obs), let sq = sortOn oT [ o | o <- obs, oSess o == s ], (a, b) <- zip sq (drop 1 sq)
  , not (crossed a b), wa <- oWins a, wb <- oWins b, wName wa == wName wb, wUsed wb >= wUsed wa, period wa == period wb ]
  where
    crossed a b = any (\o -> oSess o /= oSess a && oT o > oT a - oSecs a && oT o <= oT b) obs
    period w = fmap (\t -> round (t / 60) :: Int) (wReset w)

-- | A fit: the weight of each class (Nothing: never seen in the pairs), the pairs, how much of the rise's variance
-- it explains (R squared), and which lag it is.
data Fit = Fit { fWeights :: [Maybe Double], fN :: Int, fR2 :: Double, fLag :: Int }

-- | The fewest pairs for a fit, with k classes seen: 3 a class, and 8.
minPairs :: Int -> Int
minPairs k = max 8 (3 * k)

-- | The least-squares fit of (rise, tokens) with no intercept; Nothing when there are too few or it is singular.
fitOf :: Int -> [(Double, [Double])] -> Maybe Fit
fitOf lag ps
  | n < minPairs (length cols) || null cols = Nothing
  | otherwise = do
      co <- solve [ [ sum (zipWith (*) ci cj) | cj <- xs ] | ci <- xs ] [ sum (zipWith (*) ci ys) | ci <- xs ]
      let pred' = [ sum (zipWith (*) co row) | row <- transpose xs ]
          sse = sum [ (y - p) ^ (2 :: Int) | (y, p) <- zip ys pred' ]
          mean = sum ys / fromIntegral n
          sst = sum [ (y - mean) ^ (2 :: Int) | y <- ys ]
          back = [ (c, w / 1000) | (c, w) <- zip cols co ]
      pure (Fit [ lookup c back | c <- [0 .. 3] ] n (if sst <= 0 then 0 else 1 - sse / sst) lag)
  where
    n = length ps
    ys = map fst ps
    cols = [ c | c <- [0 .. 3], any (\(_, t) -> t !! c /= 0) ps ]
    xs = [ [ (t !! c) / 1000 | (_, t) <- ps ] | c <- cols ]

-- | Solve A x = b by elimination with a pivot; Nothing when singular.
solve :: [[Double]] -> [Double] -> Maybe [Double]
solve a b = go (zipWith (\r y -> r ++ [y]) a b)
  where
    go [] = Just []
    go rows = do
      let m = snd (maximum [ (abs (head r), i) | (r, i) <- zip rows [0 :: Int ..] ])
          p = rows !! m
          rest = [ r | (r, i) <- zip rows [0 ..], i /= m ]
      if abs (head p) <= 1e-9 * (1 + maximum (map abs (init p))) then Nothing else do
        xs <- go [ drop 1 (zipWith (-) r (map (* (head r / head p)) p)) | r <- rest ]
        pure ((last p - sum (zipWith (*) (drop 1 (init p)) xs)) / head p : xs)

-- | A fit worth trusting: 30 pairs, R squared 0.8, every seen weight positive, and the uncached input one seen.
trusted :: Fit -> Bool
trusted f = fN f >= 30 && fR2 f >= 0.8 && all (maybe True (> 0)) (fWeights f) && maybe False (> 0) (head (fWeights f))

-- | The write/read price ratio a trusted fit implies: what a token written weighs over one read.
impliedRatio :: Fit -> Maybe Double
impliedRatio f
  | trusted f, [_, Just r, Just w, _] <- fWeights f, r > 0 = Just (w / r)
  | otherwise = Nothing

-- | The report: per window, the weights relative to an uncached input token, the calls, the fit, and the ratio it
-- implies beside the one in use. Says plainly when there is too little data.
quotaLines :: Double -> [(String, Json)] -> [String]
quotaLines ratioInUse rows
  | null obs = [ "quota: no call in the ledger has the plan's windows yet (they come with the claude command's calls): nothing to fit" ]
  | otherwise = concatMap one (nub ([ pWin p | p <- ps ] ++ [ wName w | o <- obs, w <- oWins o ]))
  where
    obs = catMaybes (map obsOf rows)
    ps = pairsOf obs
    one w =
      let mine = [ p | p <- ps, pWin p == w ]
          fits = catMaybes [ fitOf 1 [ (pRise p, pPrev p) | p <- mine ], fitOf 0 [ (pRise p, pNext p) | p <- mine ] ]
      in case sortOn (negate . fR2) fits of
        [] -> [ printf "quota, window %s: %d usable pairs of calls: too little data to fit (at least %d, and the classes must vary)" w (length mine) (minPairs 4) ]
        f : _ -> report w f
    report w f =
      [ printf "quota, window %s: %d pairs of calls, R squared %.2f, the rise taken to be the %s call's tokens%s" w (fN f) (fR2 f) (if fLag f == 1 then "earlier" else "later" :: String)
          (if trusted f then "" else " -- NOT to be trusted (needs 30 pairs, R squared 0.8, positive weights)" :: String)
      , "  weight of a token, uncached input = 1: " ++ unwords [ printf "%s %s" nm (maybe "(not seen)" (rel f) wt) | (nm, wt) <- zip ["uncached", "cache-read", "cache-written", "output"] (fWeights f) :: [(String, Maybe Double)] ] ]
      ++ [ printf "  1%% of the window is about %s uncached input tokens" (human (round (0.01 / a))) | Just a <- [head (fWeights f)], a > 0 ]
      ++ [ case impliedRatio f of
             Just r -> printf "  the write/read price ratio this implies: %.1f (the controller's: %.1f; not switched to by itself)" r ratioInUse
             Nothing -> printf "  no write/read ratio implied (the controller's: %.1f)" ratioInUse | True ]
    rel f x = case head (fWeights f) of
      Just a | a > 0 -> printf "%.3g" (x / a) :: String
      _ -> printf "%.3g (per 1000 tokens, of the window)" (x * 1000)
