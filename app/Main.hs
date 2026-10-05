module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import GhciSession.Cli (cliMain)
import qualified GhciSession.SelfBench as SelfBench
import qualified GhciSession.SelfTest as SelfTest

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("selftest" : _) -> SelfTest.run >>= \ok -> if ok then pure () else exitFailure
    ("selfbench" : rest) -> SelfBench.run rest
    _ -> cliMain
