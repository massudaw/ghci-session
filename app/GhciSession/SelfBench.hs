-- | The tool's hot paths on inputs of a realistic size, timed, with what each allocates:
-- @ghci-session selfbench [ROOT DIR...]@ (the directories are scanned as a project's sources would be; default:
-- this package's). In the session this package runs on itself, @GhciSession.SelfBench.run []@ -- but that is
-- unoptimised code: the binary's numbers are the ones to quote.
module GhciSession.SelfBench (run) where

import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM_, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Directory (getCurrentDirectory)
import Text.Printf (printf)

import GhciSession.Daemon (verdictOf)
import GhciSession.Json
import GhciSession.Repl (decode, frameChunks, sentinel)
import GhciSession.Sys
import GhciSession.Watch

bench :: String -> Int -> IO a -> IO ()
bench name n act = do
  on <- getRTSStatsEnabled
  a0 <- if on then allocated_bytes <$> getRTSStats else pure 0
  t0 <- now
  replicateM_ n (void act)
  t1 <- now
  a1 <- if on then allocated_bytes <$> getRTSStats else pure 0
  printf "%-46s %9.2f ms %10.2f MB allocated\n" name ((t1 - t0) * 1000 / fromIntegral n) (fromIntegral (a1 - a0) / 1e6 / fromIntegral n :: Double)

run :: [String] -> IO ()
run args = do
  cwd <- getCurrentDirectory
  let (root, dirs) = case args of { (r : ds@(_ : _)) -> (r, ds); [r] -> (r, ["src"]); [] -> (cwd, ["app", "hygiene/src"]) }
      exts = [".hs", ".hs-boot", ".c", ".h", ".cabal"]
  sig <- scan root dirs exts
  printf "scanning %s %s: %d sources\n" root (show dirs) (M.size sig)
  bench "scan" 20 (scan root dirs exts >>= evaluate . M.size)
  bench "scan and compare with the loaded signature" 20 (scan root dirs exts >>= \cur -> evaluate (length [ p | p <- M.keys (M.union cur sig), M.lookup p cur /= M.lookup p sig ]))

  -- a load log: 4000 modules compiling, a few warnings, the verdict at the end
  let logLines = concat [ [ "[" ++ show i ++ " of 4000] Compiling Some.Module.Name" ++ show i ++ " ( src/Some/Module/Name" ++ show i ++ ".hs, dist/obj/Some/Module/Name" ++ show i ++ ".o ) [Source file changed]" ]
                          ++ [ "src/Some/Module/Name" ++ show i ++ ".hs:12:3: warning: [GHC-40910] [-Wunused-top-binds]\n    Defined but not used: x" | i `mod` 50 == 0 ]
                        | i <- [1 .. 4000 :: Int] ] ++ ["Ok, 4000 modules loaded."]
      logText = unlines logLines
      raw = BC.pack (concatMap (\c -> if c == '\n' then "\r\n" else [c]) logText) `B.append` BC.pack "\r\n" `B.append` sentinel `B.append` BC.pack "\r\n"
  printf "a load log of %d lines, %.1f MB\n" (length logLines) (fromIntegral (B.length raw) / 1e6 :: Double)
  bench "repl: frame the reply as 64 KB chunks arrive" 5 (evaluate (sum (map B.length (fst (frameChunks (chunks 65536 raw))))))
  bench "repl: decode the reply" 5 (evaluate (T.length (decode raw)))
  let logT = decode raw
  _ <- evaluate (T.length logT)
  bench "verdict of the load log" 5 (evaluate (length (show (verdictOf logT))))
  let reply = JObj [("ok", JBool True), ("out", JText logT), ("stale", JArr []), ("status", JObj [("kind", JStr "OK")])]
  bench "json: encode a reply carrying it" 5 (evaluate (B.length (encodeBS reply)))
  let wire = encodeBS reply
  _ <- evaluate (B.length wire)
  bench "json: parse that reply (the client)" 5 (evaluate (either length (maybe 0 T.length . lookupText "out") (parseJsonBS wire)))

  -- hashing: what a server's code is (its object files)
  let blob = B.replicate (64 * 1024 * 1024) 120
  B.writeFile "/tmp/ghci-session-selfbench.blob" blob
  bench "hash a 64 MB file" 5 (hashFile "/tmp/ghci-session-selfbench.blob" 1)

  -- a process table
  bench "the process table (was: ps, 20 ms)" 20 (processTable >>= evaluate . length)
  bench "pidAlive (was: ps, 20 ms)" 20 (pidAlive 1)
  where
    chunks n b = if B.null b then [] else let (x, r) = B.splitAt n b in x : chunks n r
