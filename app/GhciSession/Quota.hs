-- | __What a subscription meters__: the windows the @claude@ command reports (@rate_limit_event@, its
-- @unifiedWindows@: a name, how much of the window is used, when it resets), kept with each model call's line in
-- the ledger (@usage.jsonl@), and the cache writes of a call (how many tokens, of them how many for an hour).
-- Pure, but for 'windowLines', which says a reset in the zone of the machine.
module GhciSession.Quota
  ( Window (..), windowsOf, creditsOf, mergeWindows, windowsJson, windowsFrom, writesOf, windowLines
  ) where

import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Time (defaultTimeLocale, formatTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Text.Printf (printf)

import GhciSession.Json

-- | A window of the plan: its name (@five_hour@, @seven_day@, ...), the share of it used (0..1), and when it
-- resets (seconds since the epoch), when the event says.
data Window = Window { wName :: String, wUsed :: Double, wReset :: Maybe Double }
  deriving (Eq, Show)

-- | The windows a @rate_limit_event@'s info says. Its @unifiedWindows@ are an object by name, each with a
-- @utilization@ and a reset (@resetsAt@); an event that has none says the one window it is about
-- (@rateLimitType@, @utilization@), if it has a utilization.
windowsOf :: Json -> [Window]
windowsOf info = case lookupObj "unifiedWindows" info of
  [] -> [ Window kind u (lookupNum "resetsAt" info) | Just u <- [lookupNum "utilization" info] ]
  ws -> [ Window k u (resetOf k w) | (k, w) <- ws, Just u <- [lookupNum "utilization" w] ]
  where
    kind = fromMaybe "usage" (lookupStr "rateLimitType" info)
    resetOf k w = listToMaybe (mapMaybe (`lookupNum` w) ["resetsAt", "reset", "resets_at"])
                  `orElse` (if Just k == lookupStr "rateLimitType" info then lookupNum "resetsAt" info else Nothing)
    orElse a b = maybe b Just a

-- | Whether the event says the calls are on usage credits (not within the plan): @isUsingOverage@.
creditsOf :: Json -> Maybe Bool
creditsOf info = case (lookupBool "isUsingOverage" info, lookupBool "usingOverage" info) of
  (Just b, _) -> Just b
  (_, b) -> b

-- | The windows as last seen, with what a newer event says (by name: a window the event does not mention stays).
mergeWindows :: [Window] -> [Window] -> [Window]
mergeWindows old new = [ w | w <- old, wName w `notElem` map wName new ] ++ new

-- | A ledger line's windows: @{"five_hour": {"u": 0.34, "reset": 1760000000}, ...}@.
windowsJson :: [Window] -> Json
windowsJson ws = JObj [ (wName w, JObj (("u", JNum (wUsed w)) : [ ("reset", JNum r) | Just r <- [wReset w] ])) | w <- ws ]

-- | The windows of a ledger line.
windowsFrom :: Json -> [Window]
windowsFrom row = [ Window k u (lookupNum "reset" w) | (k, w) <- lookupObj "windows" row, Just u <- [lookupNum "u" w] ]

-- | What a call wrote to the provider's cache, from the stream's usage: @(all, for five minutes, for an hour)@. The
-- provider says the two apart in @cache_creation@ (@ephemeral_5m_input_tokens@, @ephemeral_1h_input_tokens@); a
-- reply that does not has the total only, and the two are 0.
writesOf :: Json -> (Int, Int, Int)
writesOf u = (n "cache_creation_input_tokens" u, n "ephemeral_5m_input_tokens" cc, n "ephemeral_1h_input_tokens" cc)
  where cc = u .: "cache_creation"
        n k j = maybe 0 round (lookupNum k j) :: Int

-- | The windows of the latest line that has any, for a screen: what is used, when it resets, whether the call was on
-- usage credits, as of when.
windowLines :: [Json] -> IO [String]
windowLines rows = case reverse (sortOn (fromMaybe 0 . lookupNum "t") [ r | r <- rows, not (null (windowsFrom r)) ]) of
  [] -> pure ["  the plan's windows: none seen yet (the claude command reports them in its stream; the ledger keeps them with each call)"]
  r : _ -> do
    seen <- at (fromMaybe 0 (lookupNum "t" r))
    ls <- mapM line (windowsFrom r)
    pure (printf "  the plan's windows, as of %s (%s):" seen credit : ls)
    where credit = case lookupBool "credits" r of
            Just True -> "that call was on usage credits" :: String
            Just False -> "that call was within the plan"
            Nothing -> "whether it was on usage credits is not said"
  where
    at t = formatTime defaultTimeLocale "%H:%M:%S" <$> utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac t))
    line w = do
      rs <- maybe (pure "resets: not said") (\t -> ("resets " ++) . formatTime defaultTimeLocale "%H:%M on %b %e" <$> utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac t))) (wReset w)
      pure (printf "    %-12s %5.1f%% used, %s" (wName w) (100 * wUsed w) rs)
