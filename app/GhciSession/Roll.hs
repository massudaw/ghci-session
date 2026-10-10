-- | __When a turn goes on in a fresh call__ (a rollover). A call of the claude command reads its whole context,
-- most of it from the provider's cache at a tenth of the price; a fresh call after a reset reads the restart's
-- context (@S@ tokens) uncached, at the write price (@r@ times the read's, 12.5 by default), and the context then
-- grows by @g@ tokens a call. A cycle from @S@ to @T@ is @(T-S)/g@ calls; a call costs, in reads, the mean
-- context @(S+T)/2@, and the reset @r*S@ over the cycle: @r*S*g/(T-S) + (S+T)/2 + r*g@ a call, which is least at
-- @T = S + sqrt(2 r S g)@. That is 'threshold', with the share of what the reset makes the agent learn again
-- counted in the restart's price (@S@ times @1 + relearn@). This module is pure, but
-- for the file its state is kept in, beside the session's history: what the controller has learned (the
-- restart's size, the growth a call) survives the chat.
module GhciSession.Roll
  ( Roll (..), emptyRoll, threshold, seeCall, limitOf, moved, rollJson, rollFrom, loadRoll, saveRoll, defaultRatio
  , seeGap, lifetime, coldDue, rollAt, boundaryTool, seeRelearn, rotLimit, rotOver, rollLines
  ) where

import Control.Exception (IOException, try)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
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
  } deriving (Eq, Show)

-- | The ratio of the prices when the configuration says none.
defaultRatio :: Double
defaultRatio = 12.5

-- | Before anything is seen: the measured values of this chat and another (a restart 45k, growth 1590 a call, 28%).
emptyRoll :: Double -> Roll
emptyRoll r = Roll r 45000 1590 0.28 0 0 3300 0 (-1)

-- | The context at which a turn rolls over: @S + sqrt(2 r S (1 + relearn) g)@, within 80k and 200k.
threshold :: Roll -> Int
threshold ro = max 80000 (min 200000 (round (rS ro + sqrt (2 * rRatio ro * rS ro * (1 + rRelearn ro) * rG ro))))

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
rollJson ro = JObj [("S", JNum (rS ro)), ("g", JNum (rG ro)), ("relearn", JNum (rRelearn ro)), ("said", JNum (fromIntegral (rSaid ro))), ("lo", JNum (rLo ro)), ("hi", JNum (rHi ro)), ("relid", JNum (fromIntegral (rRelId ro))), ("rot", JNum (rRot ro))]

-- | What the file says, for a screen: the threshold and what it comes from, the cache bounds, the rot.
rollLines :: Double -> Json -> [String]
rollLines ratio j =
  [ printf "rollover: at %dk tokens (a fresh call %dk, %d tokens a call, a write %.1f reads, %.0f%% learned again)" (threshold ro `div` 1000) (round (rS ro) `div` 1000 :: Int) (round (rG ro) :: Int) (rRatio ro) (100 * rRelearn ro)
  , printf "  the cache lasts at least %d and at most %d minutes; %s" (mins (rLo ro)) (mins (rHi ro)) rot ]
  where
    ro = rollFrom ratio j
    rotS = fromMaybe (-1) (lookupNum "rot" j)
    mins s = round (s / 60) :: Int
    rot | rotS < 0 = "no rot seen"
        | rotOver (Just rotS) = printf "rot: %.0f%% of the last 20 reads were repeats (the threshold a tenth lower)" (100 * rotS)
        | otherwise = printf "rot: %.0f%% of the last 20 reads were repeats" (100 * rotS)

-- | A state from its file's JSON: what it does not have, or has not well, is what the start has.
rollFrom :: Double -> Json -> Roll
rollFrom ratio j = ro0 { rS = pick "S" (\x -> x >= 5000 && x <= 400000) (rS ro0), rG = pick "g" (\x -> x >= 50 && x <= 20000) (rG ro0)
                       , rRelearn = pick "relearn" (\x -> x >= 0 && x <= 2) (rRelearn ro0), rSaid = round (pick "said" (>= 0) 0)
                       , rLo = pick "lo" (>= 0) 0, rHi = pick "hi" (>= 0) (rHi ro0), rRelId = round (pick "relid" (>= 0) 0) }
  where ro0 = emptyRoll ratio
        pick k ok d = case lookupNum k j of { Just x | ok x -> x; _ -> d }

-- | The state kept in a file (the start where there is none).
loadRoll :: FilePath -> Double -> IO Roll
loadRoll f ratio = do
  b <- try (B.readFile f) :: IO (Either IOException B.ByteString)
  pure (either (const (emptyRoll ratio)) (either (const (emptyRoll ratio)) (rollFrom ratio) . parseJsonBS) b)

-- | Keep the state (written beside, then renamed: a chat killed in the middle leaves the old one).
saveRoll :: FilePath -> Roll -> IO ()
saveRoll f ro = do
  r <- try (B.writeFile (f ++ ".tmp") (encodeBS (rollJson ro)) >> renameFile (f ++ ".tmp") f) :: IO (Either IOException ())
  either (const (pure ())) pure r
