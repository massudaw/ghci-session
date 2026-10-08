{-# LANGUAGE TemplateHaskell #-}
-- | What the splices of "Splice.Use" run (the tour's @th@ group edits this file, that one, and the quotation).
module Splice.Gen (scaled, mkGreeting) where

import Language.Haskell.TH

-- | A number, times what this code says: an expression splice's code.
scaled :: Integer -> Q Exp
scaled n = litE (integerL (n * 10))

-- | A declaration splice: @greeting :: String@, bound to the string given.
mkGreeting :: String -> Q [Dec]
mkGreeting value = sequence [sigD n [t| String |], valD (varP n) (normalB (litE (stringL value))) []]
  where n = mkName "greeting"
