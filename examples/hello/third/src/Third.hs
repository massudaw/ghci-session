-- | A third package, over Extra: what the tour adds to a session that is already running.
module Third (echo, selfTest) where

import Extra (shout)

echo :: String
echo = shout ++ " " ++ shout

selfTest :: IO ()
selfTest = putStrLn (if length (words echo) == 2 then "[PASS] echo = " ++ echo else "[FAIL] echo")
