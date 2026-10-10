-- | The checks of the plan-metering round (the plan's windows, the quota fit, the price ratio, ...): a list of
-- (what is checked, whether it holds), run by "GhciSession.SelfTest" with the rest.
module GhciSession.SelfRound (checks) where

import GhciSession.Json
import GhciSession.Quota
import GhciSession.QuotaFit
import GhciSession.Roll
import GhciSession.Gc (orphanBuilds, isPidFile)
import qualified GhciSession.Know as K
import qualified Data.Text as T
import Data.List (isInfixOf)

-- (a fact: subject, text, days since the newest, times said again)
kf :: String -> String -> Double -> Int -> K.Fact
kf sub txt ago seen = K.Fact txt (T.pack sub) (T.pack "t") (T.pack txt) (1e9 - ago * 86400) (1e9 - ago * 86400) "p/s:1+1" [] Nothing seen

checks :: [(String, Bool)]
checks =
  [ ("know: a rule said once 90 days ago is worth 0.71, a fact of another kind 0.125, one said three times 40 days ago beats one said today"
    , let sc = K.factScore 1e9; near x y = abs (x - y) < 0.01
      in near (sc (kf "user/rules" "R" 90 0)) 0.707 && near (sc (kf "dxf/status" "R" 90 0)) 0.125
         && sc (kf "dxf/status" "R" 40 3) > sc (kf "dxf/status" "T" 0 0) && sc (kf "dxf/status" "R" 90 3) < sc (kf "dxf/status" "T" 0 0))
  , ("know: a fact confirmed as often and as lately is never worth less, rules included"
    , let sc = K.factScore 1e9
      in and [ sc (kf s "a" a n) >= sc (kf s "b" b 0) | s <- ["user/rules", "dxf/rules", "dxf/status"], a <- [0, 30, 400], b <- [a, a + 1, a + 200], n <- [0, 2] ]
         && K.isRuleSubject (T.pack "user/style") && K.isRuleSubject (T.pack "nes/rules") && not (K.isRuleSubject (T.pack "nes/status")))
  , ("know: the block lists a fact said often before newer said-once ones, where the last-confirmation order cut it"
    , let facts = [ (kf "dxf/status" "KEPT-OFTEN" 40 3) ] ++ [ kf "dxf/status" ("TRIVIA-" ++ show k ++ replicate 60 'x') (fromIntegral k) 0 | k <- [0 .. 30 :: Int] ]
          block = T.unpack (K.render 700 "dxf" facts)
      in "KEPT-OFTEN" `isInfixOf` block && not ("TRIVIA-30" `isInfixOf` block))
  , ("gc: a live chat's command line holds ghc and the build directory, yet it is no orphaned build once its chat.pid owns it"
    , let chat = (79661, 1, "/r/dist-newstyle/build/ghci-session/ghci-session chat -s tool"); ghc = (500, 1, "ghc --interactive -i/r/dist-newstyle/x")
          other = (600, 1, "vim /r/dist-newstyle/x")
      in (map fst (orphanBuilds [chat, ghc, other] [] ["/r/dist-newstyle"]), map fst (orphanBuilds [chat, ghc, other] [79661] ["/r/dist-newstyle"]))
         == ([79661, 500], [500]) && all isPidFile ["pid", "chat.pid", "server-O2.pid"] && not (any isPidFile ["status", "pidx", "history"]))
  , ("price ratio: one-hour writes make it 20 and the cache's upper bound 3600 s, five-minute ones 12.5 and 300 s, none seen 12.5 and 3300"
    , let k w5 w1 = seeWrites w5 w1 (emptyRollWith Nothing)
          f r = (rKind r, rRatio r, rHi r, lifetime r)
      in (f (k 0 300), f (k 100 0), f (k 0 0), f (k 100 300)) == ((2, 20, 3600, 3300), (1, 12.5, 300, 300), (0, 12.5, 3300, 3300), (2, 20, 3600, 3300)))
  , ("price ratio: rollover_ratio fixes the ratio whatever the writes say, but the cache's bounds follow the writes"
    , let r = seeWrites 0 5 (emptyRollWith (Just 15)) in (rRatio r, rKind r, rHi r) == (15, 2, 3600))
  , ("price ratio: a change of kind starts the bounds over; the same kind keeps what was learned"
    , let learned = (seeWrites 0 5 (emptyRollWith Nothing)) { rLo = 1800, rHi = 3000 }
      in ((rLo (seeWrites 0 9 learned), rHi (seeWrites 0 9 learned)), (rLo (seeWrites 9 0 learned), rHi (seeWrites 9 0 learned))) == ((1800, 3000), (0, 300)))
  , ("price ratio: it is kept in the state's file, the kind too, and the dearer write rolls over later"
    , let r = seeWrites 0 5 (emptyRollWith Nothing); b = seeWrites 5 0 (emptyRollWith Nothing)
      in rollFrom Nothing (rollJson r) == r && rollFrom (Just 15) (rollJson r) == (seeWrites 0 5 (emptyRollWith (Just 15))) && threshold r > threshold b)
  , ("quota: the windows of an event are its unifiedWindows, by name, each with its use and reset"
    , windowsOf ev == [Window "five_hour" 0.34 (Just 1760000000), Window "seven_day" 0.8 (Just 1760500000)])
  , ("quota: an event with no windows but a utilization says the one window it is about; with neither, none"
    , (windowsOf (JObj [("rateLimitType", JStr "five_hour"), ("utilization", JNum 0.5), ("resetsAt", JNum 7)]), windowsOf (JObj [("status", JStr "allowed")]))
      == ([Window "five_hour" 0.5 (Just 7)], []))
  , ("quota: a window with no reset of its own has the event's, when it is the window the event is about"
    , windowsOf (JObj [("rateLimitType", JStr "five_hour"), ("resetsAt", JNum 9), ("unifiedWindows", JObj [("five_hour", JObj [("utilization", JNum 0.1)]), ("other", JObj [("utilization", JNum 0.2)])])])
      == [Window "five_hour" 0.1 (Just 9), Window "other" 0.2 Nothing])
  , ("quota: usage credits are what isUsingOverage says, or nothing"
    , map creditsOf [JObj [("isUsingOverage", JBool True)], JObj [("isUsingOverage", JBool False)], JObj []] == [Just True, Just False, Nothing])
  , ("quota: a newer event replaces a window by name and leaves the others"
    , mergeWindows [Window "a" 0.1 Nothing, Window "b" 0.2 Nothing] [Window "b" 0.3 (Just 1)] == [Window "a" 0.1 Nothing, Window "b" 0.3 (Just 1)])
  , ("quota: a ledger line's windows come back as they were put in"
    , let ws = [Window "five_hour" 0.34 (Just 1760000000), Window "x" 0.5 Nothing] in windowsFrom (JObj [("windows", windowsJson ws)]) == ws)
  , ("quota: a call's cache writes are the total and, where the provider says, what was for five minutes and what for an hour"
    , (writesOf (JObj [("cache_creation_input_tokens", JNum 300), ("cache_creation", JObj [("ephemeral_5m_input_tokens", JNum 100), ("ephemeral_1h_input_tokens", JNum 200)])]), writesOf (JObj [("cache_creation_input_tokens", JNum 50)]), writesOf (JObj []))
      == ((300, 100, 200), (50, 0, 0), (0, 0, 0)))
  , ("quota fit: elimination solves a small system, and says Nothing for a singular one"
    , (solve [[2, 1], [1, 3]] [5, 10], solve [[1, 2], [2, 4]] [1, 2]) == (Just [1, 3], Nothing))
  , ("quota fit: weights made up (read 0.1, written 2, output 5 of an uncached input token) come back from 60 calls, trusted, ratio 20"
    , case fit1 of
        Just f -> fLag f == 1 && fN f == 59 && fR2 f > 0.999 && trusted f && near [1, 0.1, 2, 5] (relative f) && maybe False (\r -> abs (r - 20) < 0.5) (impliedRatio f)
        Nothing -> False)
  , ("quota fit: the same data with the rise seen a call late (the later call's tokens) is told by its lag"
    , fmap fLag (fitOf 0 [ (pRise p, pNext p) | p <- pairsOf (obsFrom (series 1 60)) ]) == Just 0 && fmap fLag fit1 == Just 1)
  , ("quota fit: with 5 calls the report says too little data; with none that has windows, that there is nothing to fit"
    , let ls = quotaLines 12.5 (series 1 5) in (any ("too little data" `isInfixOf`) ls, any ("nothing to fit" `isInfixOf`) (quotaLines 12.5 [("s", JObj [("t", JNum 1)])])) == (True, True))
  , ("quota fit: a call of another session's ledger that ended between two calls takes that pair out"
    , length (pairsOf (obsFrom (series 1 60 ++ [("other", row 0 (10 * 10 + 0.5) 1 (0.5) [1, 1, 1, 1])])) ) == 58)
  , ("quota fit: two calls in different periods of a window (another reset) make no pair"
    , length (pairsOf (obsFrom (series 1 30 ++ [ (s, setReset 5e9 r) | (s, r) <- series 31 30 ]))) == 58)
  , ("quota fit: a rise that has nothing to do with the tokens is not to be trusted, and the report says so"
    , let ls = quotaLines 12.5 (noise 60) in any ("NOT to be trusted" `isInfixOf`) ls)
  , ("quota fit: a fit that is not trusted implies no ratio; a trusted one says it beside the controller's"
    , let ls = quotaLines 12.5 (series 1 60) in (any ("implies: 20." `isInfixOf`) ls, any ("controller's: 12.5" `isInfixOf`) ls) == (True, True))
  ]
  where
    ev = JObj [ ("status", JStr "allowed"), ("rateLimitType", JStr "five_hour"), ("unifiedWindows", JObj
          [ ("five_hour", JObj [("utilization", JNum 0.34), ("resetsAt", JNum 1760000000)])
          , ("seven_day", JObj [("utilization", JNum 0.8), ("resetsAt", JNum 1760500000)]) ]) ]

    -- made-up calls of one session: tokens from a generator, and the window's use as it is seen at the START of each call, the sum
    -- of the cost of the calls before it, at the weights (uncached 1, read 0.1, written 2, output 5) times 2e-7
    wts = [1, 0.1, 2, 5]
    lcg x = (x * 1103515245 + 12345) `mod` 2147483648 :: Integer
    rnd = map (\x -> fromIntegral (x `div` 65536)) (tail (iterate lcg 7)) :: [Double]
    toks k = [ [ 500 + r1 * 3000 / 32768, 20000 + r2 * 50000 / 32768, r3 * 4000 / 32768, 100 + r4 * 1500 / 32768 ] | (r1, r2, r3, r4) <- take k (quads rnd) ]
    quads (a : b : c : d : rest) = (a, b, c, d) : quads rest
    quads _ = []
    cost t = 2e-7 * sum (zipWith (*) wts t)
    row :: Double -> Double -> Double -> Double -> [Double] -> Json
    row u t secs reset [n, c, w, o] = JObj [ ("t", JNum t), ("secs", JNum secs), ("in", JNum (n + c + w)), ("cached", JNum c), ("new", JNum n), ("wr", JNum w), ("out", JNum o)
                                           , ("windows", windowsJson [Window "five_hour" u (Just (1e9 + reset))]) ]
    row _ _ _ _ _ = JObj []
    -- calls i .. i+n-1 of the series (call i at time 10 i, taking 1 s)
    series :: Int -> Int -> [(String, Json)]
    series i n = [ ("s", row u (10 * fromIntegral k) 1 0 t) | (k, u, t) <- take n (drop (i - 1) (zip3 [1 :: Int ..] us ts)) ]
    ts = toks 70
    us = scanl (+) 0.01 (map cost ts)
    setReset r j = case windowsFrom j of { [w] -> JObj ([ kv | kv@(k, _) <- objOf j, k /= "windows" ] ++ [("windows", windowsJson [w { wReset = Just r }])]); _ -> j }
    objOf (JObj kvs) = kvs
    objOf _ = []
    noise n = [ ("s", row (fromIntegral (floor (r * 1000 / 32768) :: Int) / 1000) (10 * fromIntegral k) 1 0 t) | (k, r, t) <- take n (zip3 [1 :: Int ..] (drop 400 rnd) (toks n)) ]
    obsFrom rs = [ o | Just o <- map obsOf rs ]
    fit1 = fitOf 1 [ (pRise p, pPrev p) | p <- pairsOf (obsFrom (series 1 60)) ]
    relative f = case fWeights f of { ws@(Just a : _) -> [ maybe 0 (/ a) w | w <- ws ]; _ -> [] }
    near xs ys = length xs == length ys && and (zipWith (\x y -> abs (x - y) < 0.02 * abs x + 1e-9) xs ys)
