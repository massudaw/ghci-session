module Part.Mid (mid, midTable) where

import Part.Base (base)

mid :: String
mid = "m0" ++ base

-- | A value big enough to be among the largest in a census: the tour looks for the CAF holding it.
midTable :: [Int]
midTable = [1 .. 100000]
