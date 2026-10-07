-- | __What the model calls cost__, as a table: the ledger's rows (@<state>/<session>/usage.jsonl@, one a
-- call: when, who asked, the model, tokens in and of them cached, tokens out, seconds) summed by who asked
-- and by day, in money when @"prices"@ in the config prices the model. Shared by `ghci-session usage` and
-- the monitor (`top`).
module GhciSession.Usage (usageRows, usageTable) where

import Control.Monad (forM)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import System.FilePath ((</>))
import Text.Printf (printf)

import GhciSession.Config
import GhciSession.Json
import GhciSession.Llm (human)
import GhciSession.Sys (readFileMaybe)

-- | The ledger's rows of these sessions, from a time on: @(session, row)@.
usageRows :: Conf -> [String] -> Double -> IO [(String, Json)]
usageRows conf names since = fmap concat $ forM names $ \n -> do
  t <- readFileMaybe (cStateDir conf </> n </> "usage.jsonl")
  pure [ (n, j) | l <- maybe [] lines t, Right j <- [parseJson l], fromMaybe 0 (lookupNum "t" j) >= since ]

-- | The table: a header, a line per session and asker, the total, a line per day, and a note on the
-- models that have no price.
usageTable :: Conf -> [(String, Json)] -> [String]
usageTable conf rows =
  [ printf "  %-24s %6s %9s %9s %9s %9s  %s" ("" :: String) ("calls" :: String) ("in" :: String) ("cached" :: String) ("out" :: String) ("secs" :: String) (if null unpriced then "cost" else "" :: String) ]
  ++ [ line k js | (k, js) <- grouped (\n j -> n ++ " " ++ fromMaybe "?" (lookupStr "who" j)) ]
  ++ [ line "total" (map snd rows), "" ]
  ++ [ line d js | (d, js) <- grouped (\_ j -> take 10 (fromMaybe "" (lookupStr "date" j))) ]
  ++ [ "\n  no prices for " ++ unwordsC unpriced ++ ": \"prices\": {\"" ++ head unpriced ++ "\": {\"input\": .., \"input_cached\": .., \"output\": ..}} in ghci-session.json, in money per million tokens" | not (null unpriced) ]
  where
    price j = lookup (fromMaybe "" (lookupStr "model" j)) (cPrices conf)
    cost j = do
      p <- price j
      i <- lookupNum "input" p
      c <- lookupNum "input_cached" p
      o <- lookupNum "output" p
      let n k = fromMaybe 0 (lookupNum k j)
      pure ((n "in" - n "cached") * i / 1e6 + n "cached" * c / 1e6 + n "out" * o / 1e6)
    sumOf k js = round (sum [ fromMaybe 0 (lookupNum k j) | j <- js ]) :: Int
    secsOf js = sum [ fromMaybe 0 (lookupNum "secs" j) | j <- js ] :: Double
    money js = maybe "" (\cs -> printf "$%.3f" (sum cs)) (mapM cost js) :: String
    line :: String -> [Json] -> String
    line label js = printf "  %-24s %6d %9s %9s %9s %8.0fs  %s" label (length js) (human (sumOf "in" js)) (human (sumOf "cached" js)) (human (sumOf "out" js)) (secsOf js) (money js)
    grouped key = M.toList (M.fromListWith (flip (++)) [ (key n j, [j]) | (n, j) <- rows ])
    unpriced = nub [ m | (_, j) <- rows, Just m <- [lookupStr "model" j], isNothing (price j) ]
    unwordsC = foldr1 (\a b -> a ++ ", " ++ b)
