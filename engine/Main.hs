-- | GHCi, driven by the session daemon (see "GhsEngine").
module Main (main) where

import Prelude

import qualified GhcMain
import GhsEngine (engineInit)

main :: IO ()
main = engineInit >> GhcMain.main
