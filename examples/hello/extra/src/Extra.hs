module Extra (shout, selfTest) where

import Data.Char (toUpper)
import Hello (greeting)

shout :: String
shout = map toUpper greeting ++ "!"

selfTest :: IO ()
selfTest = putStrLn (if last shout == '!' then "[PASS] shout = " ++ shout else "[FAIL] shout")
