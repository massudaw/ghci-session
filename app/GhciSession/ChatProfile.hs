-- | __The chat's profile__: what the turns of this chat cost in all -- model calls, tokens, tool calls,
-- seconds -- kept by the screen ('GhciSession.ChatTui') and shown in a panel (Ctrl-P).
module GhciSession.ChatProfile (Profile (..), noProfile, addTurn, profileLines, spark) where

import Text.Printf (printf)

import GhciSession.ChatUi (Spent (..))
import GhciSession.Llm (human)

data Profile = Profile
  { pTurns, pCalls, pIn, pCached, pOut, pTools :: !Int
  , pSecs, pSlow :: !Double
  , pRecent :: [Double]          -- ^ seconds of the last turns, newest first
  } deriving (Eq, Show)

noProfile :: Profile
noProfile = Profile 0 0 0 0 0 0 0 0 []

-- | The profile after a turn that cost @s@ and took @secs@ seconds.
addTurn :: Spent -> Double -> Profile -> Profile
addTurn s secs p = p
  { pTurns = pTurns p + 1, pCalls = pCalls p + sCalls s, pIn = pIn p + sIn s, pCached = pCached p + sCached s
  , pOut = pOut p + sOut s, pTools = pTools p + sTools s, pSecs = pSecs p + secs, pSlow = max (pSlow p) secs
  , pRecent = take 40 (secs : pRecent p) }

-- | A bar per value, scaled to the largest, oldest first.
spark :: [Double] -> String
spark xs = [ bars !! min 7 (floor (8 * x / top)) | x <- reverse xs ]
  where bars = "▁▂▃▄▅▆▇█"; top = max 1e-9 (maximum (0 : xs)) * 1.0001

-- | The panel's lines.
profileLines :: Profile -> [String]
profileLines p =
  [ printf " profile: %d turn%s, %.0fs in all (%.1fs a turn, slowest %.0fs)" (pTurns p) (if pTurns p == 1 then "" else "s" :: String) (pSecs p) avg (pSlow p)
  , printf "  model: %d call%s, %s tokens in (%d%% cached), %s out" (pCalls p) (if pCalls p == 1 then "" else "s" :: String) (human (pIn p)) pct (human (pOut p))
  , printf "  tools: %d call%s (%.1f a turn)   turns: %s" (pTools p) (if pTools p == 1 then "" else "s" :: String) perTurn (spark (pRecent p))
  ]
  where
    n = max 1 (pTurns p)
    avg = pSecs p / fromIntegral n :: Double
    perTurn = fromIntegral (pTools p) / fromIntegral n :: Double
    pct = if pIn p == 0 then 0 else (100 * pCached p) `div` pIn p :: Int
