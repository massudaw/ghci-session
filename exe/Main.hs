{-# LANGUAGE ForeignFunctionInterface #-}

-- | The executable. Its @main@ is C ('cbits/ghs_main.c': it picks the runtime's options per command and then
-- calls 'ghsMain'); the Haskell @main@ here is what a repl on this package runs.
module Main (main, ghsMain) where

import Control.Exception (SomeException, catch, displayException, fromException)
import Foreign.C.Types (CInt (..))
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import GhciSession.Cli (cliMain)
import qualified GhciSession.SelfBench as SelfBench
import qualified GhciSession.SelfTest as SelfTest

foreign export ccall ghsMain :: IO CInt

run :: IO ()
run = do
  args <- getArgs
  case args of
    ("selftest" : _) -> SelfTest.run >>= \ok -> if ok then pure () else exitWith (ExitFailure 1)
    ("selfbench" : rest) -> SelfBench.run rest
    _ -> cliMain

-- | 'run', with its exit status as a value: an exception cannot cross back into C.
ghsMain :: IO CInt
ghsMain = (run >> done 0) `catch` \(e :: SomeException) -> case fromException e of
  Just ExitSuccess -> done 0
  Just (ExitFailure n) -> done (fromIntegral n)
  Nothing -> hPutStrLn stderr ("ghci-session: " ++ displayException e) >> done 1
  where done n = hFlush stdout >> hFlush stderr >> pure n

main :: IO ()
main = run
