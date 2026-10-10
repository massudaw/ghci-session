-- | __When a turn goes on in a fresh call__ (a rollover). A call of the claude command reads its whole context,
-- most of it from the provider's cache at a tenth of the price; a fresh call after a reset reads the restart's
-- context (@S@ tokens) uncached, at the write price (@r@ times the read's, 12.5 by default), and the context then
-- grows by @g@ tokens a call. A cycle from @S@ to @T@ is @(T-S)/g@ calls; a call costs, in reads, the mean
-- context @(S+T)/2@, and the reset @r*S@ over the cycle: @r*S*g/(T-S) + (S+T)/2 + r*g@ a call, which is least at
-- @T = S + sqrt(2 r S g)@. That is 'threshold', with the share of what the reset makes the agent learn again
-- counted in the restart's price (@1 + relearn@ times). What a reset writes is not the whole of @S@: a fresh call reads the
-- system prompt and the tools (@P@, some 7.7k tokens) from the cache as the call before it did, so its price is
-- @r (S - P) + P@ -- while the context starts again at the whole of @S@ and grows from there. This module is pure, but
-- for the file its state is kept in, beside the session's history: what the controller has learned (the
-- restart's size, the growth a call) survives the chat.
module GhciSession.Roll
  ( Roll (..), emptyRoll, threshold, resetCost, seeCall, seeCached, limitOf, moved, rollJson, rollFrom, loadRoll, saveRoll, defaultRatio
  , seeGap, lifetime, coldDue, rollAt, boundaryTool, seeRelearn, rotLimit, rotOver, rollLines
  , emptyRollWith, seeWrites, ratioFor, startHi
  ) where

import Control.Exception (IOException, try)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.ByteString as B
import System.Directory (renameFile)
import Text.Printf (printf)

import GhciSession.Json

-- | What is learned. The restart's size and the growth are running estimates from each call's usage.
data Roll = Roll
  { rRatio :: Double      -- ^ the write price over the read price ("rollover_ratio")
  , rS :: Double          -- ^ tokens of a fresh run's first call
  , rG :: Double          -- ^ tokens a call adds to the context
  , rRelearn :: Double    -- ^ the share of what a reset makes the agent learn again (counted against a reset)
  , rSaid :: Int          -- ^ the threshold last said (a harness note when it moves)
  , rLo :: Double         -- ^ the provider's cache lasted at least this many seconds (a call after a pause this long came back cached)
  , rHi :: Double         -- ^ and lasted at most this many (the same context, after a pause this long, was read uncached)
  , rRelId :: Int         -- ^ the reset (its message's number) whose relearning was last counted
  , rRot :: Double        -- ^ the share of repeats among the last twenty reads, at the last call (-1: none seen); for the screen
  , rFixed :: Bool        -- ^ the ratio is the configuration's ("rollover_ratio"): what the writes say does not change it
  , rP :: Double          -- ^ tokens of a fresh call that are read from the cache (the system prompt's and the tools': the reset does not write them)
  , rKind :: Int          -- ^ what the calls' cache writes were: 0 not seen, 1 five-minute ones, 2 one-hour ones (a subscription's)
  } deriving (Eq, Show)

-- | The ratio of the prices when the configuration says none.
defaultRatio :: Double
defaultRatio = 12.5

-- | Before anything is seen: the measured values of this chat and another (a restart 45k, growth 1590 a call, 28%).
emptyRoll :: Double -> Roll
emptyRoll r = Roll r 45000 1590 0.28 0 0 3300 0 (-1) False 7700 0

-- | The start where the configuration may fix the ratio (@Just@) or leave it to what the writes say.
emptyRollWith :: Maybe Double -> Roll
emptyRollWith m = (emptyRoll (fromMaybe defaultRatio m)) { rFixed = isJust m }

-- | The write price over the read price by the kind of cache write seen: a subscription's one-hour writes cost twice
-- the input price and a read a tenth, 20; five-minute writes 1.25 and a tenth, 12.5 (as does an unknown one).
ratioFor :: Int -> Double
ratioFor 2 = 20
ratioFor _ = defaultRatio

-- | How long the cache is first taken to last at most, seconds, by the kind of write: an hour, five minutes, or (not
-- seen) what an earlier chat found of the claude command's cache.
startHi :: Int -> Double
startHi 2 = 3600
startHi 1 = 300
startHi _ = 3300

-- | A call's cache writes, @seeWrites five-minute one-hour@: tokens written for an hour mean one-hour writes, else
-- tokens written mean five-minute ones; none, nothing. A change of kind starts the cache's bounds and the ratio over
-- (the ratio only where the configuration has not fixed it).
seeWrites :: Int -> Int -> Roll -> Roll
seeWrites w5 w1 ro
  | k == 0 || k == rKind ro = ro
  | otherwise = ro { rKind = k, rLo = 0, rHi = startHi k, rRatio = if rFixed ro then rRatio ro else ratioFor k }
  where k | w1 > 0 = 2
          | w5 > 0 = 1
          | otherwise = 0

-- | What a reset costs, in reads: a fresh call writes its context but the part that comes from the cache anyway (the
-- system prompt and the tools, 'rP': the same prefix as the call before it) -- @r (S - P) + P@.
resetCost :: Roll -> Double
resetCost ro = rRatio ro * (rS ro - p) + p where p = max 0 (min (rS ro) (rP ro))

-- | The context at which a turn rolls over: @S + sqrt(2 (R + relearn r S) g)@ with @R@ the 'resetCost' (that is
-- @r S@ if nothing of a fresh call is cached; what is learned again is written, whole), within 80k and 200k. The restart is where the context starts again,
-- the whole of @S@; only what the reset writes is its price.
threshold :: Roll -> Int
threshold ro = max 80000 (min 200000 (round (rS ro + sqrt (2 * (resetCost ro + rRelearn ro * rRatio ro * rS ro) * rG ro))))

-- | A call's usage: @seeCall first prev ctx@, the first of a run or not, the context of the call before it (0:
-- none) and this one's. A run's first call is a restart's size; one after it is a growth (a call that shrank the
-- context, or grew it by more than 20k at once, is not a rate: the sample is cut).
seeCall :: Bool -> Int -> Int -> Roll -> Roll
seeCall first prev ctx ro
  | ctx <= 0 = ro
  | first = ro { rS = mix 0.5 (rS ro) (fromIntegral ctx) }
  | prev > 0 && ctx > prev = ro { rG = mix 0.05 (rG ro) (fromIntegral (min 20000 (ctx - prev))) }
  | otherwise = ro
  where mix a old new = (1 - a) * old + a * new

-- | A run's first call's cache read, @seeCached first cached@: what a fresh call has cached already (a third of the
-- way a call, as a reset after a pause that lost the cache reads none).
seeCached :: Bool -> Int -> Roll -> Roll
seeCached first cached ro
  | first && cached >= 0 = ro { rP = 0.7 * rP ro + 0.3 * fromIntegral cached }
  | otherwise = ro

-- | The context past which a run rolls over, by the setting of @--rollover@: a number is that many tokens
-- (0: never), a negative one is the controller's.
limitOf :: Int -> Roll -> Maybe Int
limitOf n ro | n == 0 = Nothing
             | n < 0 = Just (threshold ro)
             | otherwise = Just n

-- | Has a threshold moved enough, from the one last said, to say it again (5k)?
moved :: Int -> Int -> Bool
moved said now' = abs (now' - said) > 5000

-- | What a reset was seen to make the agent learn again: @seeRelearn reset share@, the reset's message number and the
-- share of the look-ups after it (0..1) that read again a file read before it. A running estimate (a third a reset),
-- counted once a reset.
seeRelearn :: Int -> Double -> Roll -> Roll
seeRelearn reset share ro
  | reset <= rRelId ro = ro
  | otherwise = ro { rRelearn = max 0 (min 1 (0.7 * rRelearn ro + 0.3 * share)), rRelId = reset }

-- | Is the agent reading again what it has in front of it (the share of the last twenty reads that were repeats, over
-- 15%)? Then its context is rotting: it is not holding what it has read.
rotOver :: Maybe Double -> Bool
rotOver = maybe False (> 0.15)

-- | A threshold under a rotting context: a tenth lower, never under 80k (a limit already under it is left).
rotLimit :: Bool -> Int -> Int
rotLimit rot lim | rot = min lim (max 80000 (lim - lim `div` 10))
                 | otherwise = lim

-- the boundary -------------------------------------------------------------------------------

-- | Does a context past the limit end the run now? A fixed limit: past it. The controller's: at 1.15 of it, whatever
-- the agent is doing; from 0.85 of it at a natural boundary ('boundaryTool'), not in the middle of an edit.
rollAt :: Bool -> Int -> Int -> Bool -> Bool
rollAt auto lim ctx boundary
  | not auto = ctx > lim
  | otherwise = ctx * 100 >= lim * 115 || (boundary && ctx * 100 >= lim * 85)

-- | Is a tool call that was answered a natural place to end a run: a commit that went through, or a test that is green?
boundaryTool :: String -> Json -> Bool -> String -> Bool
boundaryTool name args ok out = ok && case name of
  "sh" -> "git commit" `isInfixOf` cmd
          && "[exit 0 " `isInfixOf` out && not ("nothing to commit" `isInfixOf` out || "nothing added to commit" `isInfixOf` out)
  "test" -> green
  "reload" -> green
  _ -> False
  where cmd = maybe "" id (lookupStr "cmd" args)
        green = not ("FAIL" `isInfixOf` out || "COMPILE-ERROR" `isInfixOf` out || "CHECK-HANG" `isInfixOf` out)
                && ("CHECK-PASS" `isInfixOf` out || ("all " `isInfixOf` out && " passed" `isInfixOf` out))

-- the cache ----------------------------------------------------------------------------------

-- | What a call after a pause says of how long the provider keeps its cache: @seeGap pause prev ctx cached@, the seconds
-- since the call before it ended, that call's context, this one's, and how much of this one was read from the cache.
-- Mostly cached (7 tenths of what the call before had): it lasted at least the pause. The same context, grown by
-- under 3 tenths, mostly not (under 3 tenths cached): it lasted at most the pause. Pauses under two minutes say
-- nothing, and the bounds do not cross (the later word wins).
seeGap :: Double -> Int -> Int -> Int -> Roll -> Roll
seeGap gap prev ctx cached ro
  | gap < 120 || prev <= 0 || ctx <= 0 = ro
  | cached * 10 >= prev * 7 = let lo = max (rLo ro) gap in ro { rLo = lo, rHi = max (rHi ro) lo }
  | cached * 10 < ctx * 3 && ctx * 10 <= prev * 13 = let hi = min (rHi ro) gap in ro { rHi = hi, rLo = min (rLo ro) hi }
  | otherwise = ro

-- | How long the cache is taken to last, seconds: 3300 where what is seen allows it, else the bound it breaks.
lifetime :: Roll -> Double
lifetime ro = min (rHi ro) (max (rLo ro) 3300)

-- | Is the next call's context, so long after the last call ended, to be read uncached whole? Then it is over 1.3 times
-- a restart's: a fresh call costs less than the old context cold.
coldDue :: Roll -> Double -> Int -> Bool
coldDue ro since ctx = since > lifetime ro && fromIntegral ctx * 10 > 13 * rS ro

-- the file ------------------------------------------------------------------------------------

rollJson :: Roll -> Json
rollJson ro = JObj [("S", JNum (rS ro)), ("g", JNum (rG ro)), ("relearn", JNum (rRelearn ro)), ("said", JNum (fromIntegral (rSaid ro))), ("lo", JNum (rLo ro)), ("hi", JNum (rHi ro)), ("relid", JNum (fromIntegral (rRelId ro))), ("rot", JNum (rRot ro)), ("kind", JNum (fromIntegral (rKind ro))), ("P", JNum (rP ro))]

-- | What the file says, for a screen: the threshold and what it comes from, the cache bounds, the rot.
rollLines :: Maybe Double -> Json -> [String]
rollLines ratio j =
  [ printf "rollover: at %dk tokens (a fresh call %dk, %dk of it cached, %d tokens a call, a write %.1f reads, %.0f%% learned again)" (threshold ro `div` 1000) (round (rS ro) `div` 1000 :: Int) (round (rP ro) `div` 1000 :: Int) (round (rG ro) :: Int) (rRatio ro) (100 * rRelearn ro)
  , printf "  the cache lasts at least %d and at most %d minutes (%s); %s" (mins (rLo ro)) (mins (rHi ro)) writes rot ]
  where
    ro = rollFrom ratio j
    rotS = fromMaybe (-1) (lookupNum "rot" j)
    mins s = round (s / 60) :: Int
    writes = case (rKind ro, rFixed ro) of
      (2, False) -> "one-hour writes: ratio 20" :: String
      (1, False) -> "five-minute writes"
      (_, True) -> "ratio fixed by rollover_ratio"
      _ -> "writes not seen"
    rot | rotS < 0 = "no rot seen"
        | rotOver (Just rotS) = printf "rot: %.0f%% of the last 20 reads were repeats (the threshold a tenth lower)" (100 * rotS)
        | otherwise = printf "rot: %.0f%% of the last 20 reads were repeats" (100 * rotS)

-- | A state from its file's JSON: what it does not have, or has not well, is what the start has.
rollFrom :: Maybe Double -> Json -> Roll
rollFrom ratio j = ro0 { rKind = kind, rRatio = if rFixed ro0 then rRatio ro0 else ratioFor kind, rS = pick "S" (\x -> x >= 5000 && x <= 400000) (rS ro0), rG = pick "g" (\x -> x >= 50 && x <= 20000) (rG ro0)
                       , rRelearn = pick "relearn" (\x -> x >= 0 && x <= 2) (rRelearn ro0), rSaid = round (pick "said" (>= 0) 0)
                       , rLo = pick "lo" (>= 0) 0, rHi = pick "hi" (>= 0) (startHi kind), rRelId = round (pick "relid" (>= 0) 0), rP = pick "P" (\x -> x >= 0 && x <= 100000) (rP ro0) }
  where ro0 = emptyRollWith ratio
        kind = round (pick "kind" (\x -> x >= 0 && x <= 2) 0) :: Int
        pick k ok d = case lookupNum k j of { Just x | ok x -> x; _ -> d }

-- | The state kept in a file (the start where there is none).
loadRoll :: FilePath -> Maybe Double -> IO Roll
loadRoll f ratio = do
  b <- try (B.readFile f) :: IO (Either IOException B.ByteString)
  pure (either (const (emptyRollWith ratio)) (either (const (emptyRollWith ratio)) (rollFrom ratio) . parseJsonBS) b)

-- | Keep the state (written beside, then renamed: a chat killed in the middle leaves the old one).
saveRoll :: FilePath -> Roll -> IO ()
saveRoll f ro = do
  r <- try (B.writeFile (f ++ ".tmp") (encodeBS (rollJson ro)) >> renameFile (f ++ ".tmp") f) :: IO (Either IOException ())
  either (const (pure ())) pure r
