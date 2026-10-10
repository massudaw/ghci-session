-- | __What a plan's window weighs a token at__: from the ledger (@usage.jsonl@), a least-squares fit, per window,
-- of how much its utilization rose between two calls against the tokens of the call, in four classes: uncached
-- input, read from the cache, written to it, output. The weights are said relative to an uncached input token.
-- The docs do not publish them; this is an estimate from this machine's own calls, and says how good it is.
--
-- How the pairs are made. The calls of ONE session that have windows, in time order; a SPAN of them is a pair when
-- they all saw the window with the same reset (one period), its use did not fall, and no call of ANOTHER session's
-- ledger ended between the start of the span's first call and the end of its last (that call's tokens would be in the
-- rise and not in the pair). A chat of another machine, or Claude Code itself, shows in no ledger here: its calls cannot
-- be told apart, and only add noise (the fit's quality says so). The span is as short as will do: the plan's use comes
-- in steps (a hundredth), and a call raises it by a tenth of a step, so two neighbouring calls differ by a step or by
-- nothing, which is noise, not a rise; a span is as many calls as make the use rise by a few steps on the average
-- ('spanCalls', 'spanSteps'), and the tokens are those of all its calls. The event of a call is seen at its start, so the rise over a span may be the
-- tokens of all its calls but the last (lag 1) or all but the first (lag 0): both are fitted, and the better kept.
module GhciSession.QuotaFit
  ( Obs (..), obsOf, Pair (..), pairsOf, quantumOf, spanSteps, spanCalls, Fit (..), fitOf, solve, trusted, impliedRatio, quotaLines, minPairs
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

-- | A span of calls seen in one period of one window: the rise of its use from its first call to its last, and the
-- tokens of its calls but the last (@pPrev@: the use is read at a call's start) and but the first (@pNext@: at its end).
-- Two neighbouring calls are a span of two: @pPrev@ the earlier's tokens, @pNext@ the later's.
data Pair = Pair { pWin :: String, pRise :: Double, pPrev :: [Double], pNext :: [Double] }

-- | How many steps of a window's use a span should rise over, as a rule, to be worth a fit. Reading a use to a step is
-- a noise of a third of a step at each end, whatever the span: the tokens' own spread over a span must stand out of it
-- for R squared to reach 0.8, and it grows with the square root of the calls and so of the rise (measured on made-up
-- calls: 6 steps, R squared 0.4; 10, 0.64; 20, 0.83).
spanSteps :: Double
spanSteps = 20

-- | The step a window's use comes in (0.01 for the plan's: two decimals): the smallest rise between calls that is over
-- a millionth, when every use seen is a whole number of such steps; 0 where none is seen, or the use is continuous.
quantumOf :: String -> [Obs] -> Double
quantumOf name obs = case [ d | (a, b) <- zip us (drop 1 us), let d = b - a, d > 1e-6 ] of
  [] -> 0
  ds -> let q = minimum ds in if all (onGrid q) us then q else 0
  where
    us = [ wUsed w | o <- sortOn oT obs, w <- oWins o, wName w == name ]
    onGrid q x = let k = x / q in abs (k - fromIntegral (round k :: Integer)) < 1e-4

-- | The runs of one window: calls of a session that saw it with one reset, its use not falling, in time order.
windowRuns :: String -> [Obs] -> [[(Obs, Window)]]
windowRuns name obs = [ run | s <- nub (map oSess obs), run <- runsOf [ (o, w) | o <- sortOn oT [ o | o <- obs, oSess o == s ], w <- oWins o, wName w == name ] ]
  where
    period w = fmap (\t -> round (t / 60) :: Int) (wReset w)
    runsOf [] = []
    runsOf (x : xs) = let (a, b) = more x xs in (x : a) : runsOf b
    more p (y : ys) | wUsed (snd y) >= wUsed (snd p) && period (snd y) == period (snd p) = let (a, b) = more y ys in (y : a, b)
    more _ ys = ([], ys)

-- | How many calls a span of this window runs over, so that its rise is 'spanSteps' steps of the use on the average
-- (at least 1: the neighbours). The length is a function of the window's use over the whole ledger, not of each span's
-- own rise: a span cut where its rise reaches a number has that number for its rise, whatever the tokens were, and
-- nothing is left to fit. A window whose use has no steps (continuous) is fitted by its neighbours: 1; one whose use
-- has not risen has no span worth making: a number of calls that no run reaches.
spanCalls :: Double -> String -> [Obs] -> Int
spanCalls steps name obs
  | q <= 0 = 1
  | rises <= 0 = maxBound `div` 4
  | otherwise = max 1 (ceiling (steps * q / (rises / fromIntegral calls)))
  where
    q = quantumOf name obs
    runs = windowRuns name obs
    rises = sum [ wUsed (snd (last r)) - wUsed (snd (head r)) | r <- runs ]
    calls = max 1 (sum [ length r - 1 | r <- runs ]) :: Int

-- | The spans of the calls, @pairsOf calls obs@, each window's runs cut into consecutive spans of @calls name@ calls
-- (the next span starts where the last ended; what is left of a run, short of one, is dropped), those with no other
-- session's call ending inside them.
pairsOf :: (String -> Int) -> [Obs] -> [Pair]
pairsOf calls obs =
  [ Pair name (u (last sp) - u (head sp)) (toks (init sp)) (toks (drop 1 sp))
  | name <- nub [ wName w | o <- obs, w <- oWins o ], run <- windowRuns name obs
  , sp <- spans (max 1 (calls name)) run, not (crossed (fst (head sp)) (fst (last sp))) ]
  where
    u (_, w) = wUsed w
    toks = foldr (zipWith (+) . oTok . fst) [0, 0, 0, 0]
    crossed a b = any (\o -> oSess o /= oSess a && oT o > oT a - oSecs a && oT o <= oT b) obs
    spans k run = case splitAt (k + 1) run of
      (sp, _) | length sp == k + 1 -> sp : spans k (drop k run)
      _ -> []

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

-- | The report: per window, the weights relative to an uncached input token, the spans of calls, the fit, and the
-- ratio it implies beside the one in use. Says plainly when there is too little data, and gives no figure of a window's
-- size from a fit that is not to be trusted.
quotaLines :: Double -> [(String, Json)] -> [String]
quotaLines ratioInUse rows
  | null obs = [ "quota: no call in the ledger has the plan's windows yet (they come with the claude command's calls): nothing to fit" ]
  | otherwise = concatMap one (nub ([ pWin p | p <- ps ] ++ [ wName w | o <- obs, w <- oWins o ]))
  where
    obs = catMaybes (map obsOf rows)
    ps = pairsOf (\w -> spanCalls spanSteps w obs) obs
    one w =
      let mine = [ p | p <- ps, pWin p == w ]
          fits = catMaybes [ fitOf 1 [ (pRise p, pPrev p) | p <- mine ], fitOf 0 [ (pRise p, pNext p) | p <- mine ] ]
      in case sortOn (negate . fR2) fits of
        [] -> [ printf "quota, window %s: %d usable spans of %d calls: too little data to fit (at least %d, and the classes must vary)" w (length mine) (spanCalls spanSteps w obs) (minPairs 4) ]
        f : _ -> report w f
    report w f =
      [ printf "quota, window %s: %d spans of %d calls, R squared %.2f, the rise taken to be the tokens of the span's calls but the %s%s" w (fN f) (spanCalls spanSteps w obs) (fR2 f) (if fLag f == 1 then "last" else "first" :: String)
          (if trusted f then "" else " -- NOT to be trusted (needs 30 spans, R squared 0.8, positive weights)" :: String)
      , "  weight of a token, uncached input = 1: " ++ unwords [ printf "%s %s" nm (maybe "(not seen)" (rel f) wt) | (nm, wt) <- zip ["uncached", "cache-read", "cache-written", "output"] (fWeights f) :: [(String, Maybe Double)] ] ]
      ++ [ printf "  1%% of the window is about %s uncached input tokens" (human (round (0.01 / a))) | trusted f, Just a <- [head (fWeights f)], a > 0 ]
      ++ [ case impliedRatio f of
             Just r -> printf "  the write/read price ratio this implies: %.1f (the controller's: %.1f; not switched to by itself)" r ratioInUse
             Nothing -> printf "  no write/read ratio implied (the controller's: %.1f)" ratioInUse | True ]
    rel f x = case head (fWeights f) of
      Just a | a > 0 -> printf "%.3g" (x / a) :: String
      _ -> printf "%.3g (per 1000 tokens, of the window)" (x * 1000)
