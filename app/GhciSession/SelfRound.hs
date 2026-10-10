-- | The checks of the plan-metering round (the plan's windows, the quota fit, the price ratio, ...): a list of
-- (what is checked, whether it holds), run by "GhciSession.SelfTest" with the rest.
module GhciSession.SelfRound (checks) where

import GhciSession.Json
import GhciSession.Quota

checks :: [(String, Bool)]
checks =
  [ ("quota: the windows of an event are its unifiedWindows, by name, each with its use and reset"
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
  ]
  where
    ev = JObj [ ("status", JStr "allowed"), ("rateLimitType", JStr "five_hour"), ("unifiedWindows", JObj
          [ ("five_hour", JObj [("utilization", JNum 0.34), ("resetsAt", JNum 1760000000)])
          , ("seven_day", JObj [("utilization", JNum 0.8), ("resetsAt", JNum 1760500000)]) ]) ]
