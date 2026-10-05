module Hello (greeting, selfTest, bigTable) where

import qualified Data.Map.Strict as M

greeting :: String
greeting = "hello"

-- a CAF that leaks one copy per reload in a plain GHCi session
bigTable :: M.Map Int String
bigTable = M.fromList [ (i, show i) | i <- [1 .. 200000] ]

selfTest :: IO ()
selfTest = do
  putStrLn ("greeting = " ++ greeting)
  putStrLn (if M.size bigTable == 200000 then "[PASS] table" else "[FAIL] table")
-- e1
-- e2
-- e3
