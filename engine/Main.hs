-- | GHCi, driven by the session daemon (see "GhsEngine").
module Main (main) where

import Prelude

import qualified GhcMain
import GhsEngine (engineInit, libdirArgs)
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  engineInit
  args <- getArgs
  extra <- libdirArgs args
  withArgs (extra ++ args) GhcMain.main
