module Hello (greeting, selfTest, bigTable, serve) where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import GHC.Hygiene.Zygote (handoverInPath, setHandoverExporter)
import System.Environment (lookupEnv)

greeting :: String
greeting = "hello"

-- a CAF that leaks one copy per reload in a plain GHCi session
bigTable :: M.Map Int String
bigTable = M.fromList [ (i, show i) | i <- [1 .. 200000] ]

selfTest :: IO ()
selfTest = do
  putStrLn ("greeting = " ++ greeting)
  putStrLn (if M.size bigTable == 200000 then "[PASS] table" else "[FAIL] table")

-- | A stand-in for a server: every 0.2 s it writes "<greeting> <ticks>" to $HELLO_OUT. Its state (the tick
-- count) is handed to its replacement when the session re-forks it, so the count carries across a reload.
serve :: IO ()
serve = do
  out <- maybe "hello.out" id <$> lookupEnv "HELLO_OUT"
  from <- handoverInPath
  n0 <- maybe (pure 0) (fmap (read . BL.unpack) . BL.readFile) from
  ticks <- newIORef (n0 :: Int)
  setHandoverExporter (BL.pack . show <$> readIORef ticks)
  putStrLn ("serving " ++ show greeting ++ " from tick " ++ show n0)
  let loop = do
        n <- readIORef ticks
        writeFile out (greeting ++ " " ++ show n ++ "\n")
        writeIORef ticks (n + 1)
        threadDelay 200000
        loop
  loop
