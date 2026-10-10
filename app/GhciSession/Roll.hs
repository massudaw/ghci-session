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
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as B
import System.Directory (renameFile)

import GhciSession.Json

-- | What is learned. The restart's size and the growth are running estimates from each call's usage.
data Roll = Roll
  { rRatio :: Double      -- ^ the write price over the read price ("rollover_ratio")
  , rS :: Double          -- ^ tokens of a fresh run's first call
  , rG :: Double          -- ^ tokens a call adds to the context
  , rRelearn :: Double    -- ^ the share of what a reset makes the agent learn again (counted against a reset)
  , rSaid :: Int          -- ^ the threshold last said (a harness note when it moves)
  } deriving (Eq, Show)

-- | The ratio of the prices when the configuration says none.
defaultRatio :: Double
defaultRatio = 12.5

-- | Before anything is seen: the measured values of this chat and another (a restart 45k, growth 1590 a call, 28%).
emptyRoll :: Double -> Roll
emptyRoll r = Roll r 45000 1590 0.28 0

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

-- the file ------------------------------------------------------------------------------------

rollJson :: Roll -> Json
rollJson ro = JObj [("S", JNum (rS ro)), ("g", JNum (rG ro)), ("relearn", JNum (rRelearn ro)), ("said", JNum (fromIntegral (rSaid ro)))]

-- | A state from its file's JSON: what it does not have, or has not well, is what the start has.
rollFrom :: Double -> Json -> Roll
rollFrom ratio j = ro0 { rS = pick "S" (\x -> x >= 5000 && x <= 400000) (rS ro0), rG = pick "g" (\x -> x >= 50 && x <= 20000) (rG ro0)
                       , rRelearn = pick "relearn" (\x -> x >= 0 && x <= 2) (rRelearn ro0), rSaid = round (pick "said" (>= 0) 0) }
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
