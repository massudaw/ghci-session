{-# LANGUAGE ImplicitPrelude #-}
-- | A package added to a running session, before GHC 9.14 (see "GhsAddUnits"): its GHCi has no interactive
-- units to make again over a grown unit graph (the prompt is one of the home units), so a unit is not added
-- to a running session -- the daemon restarts the repl with the new set, as for anything it cannot add live.
module GhsUnits (addUnitsLive) where

import GHC (GhcMonad)
import GHC.Driver.Session (DynFlags)

addUnitsLive :: GhcMonad m => DynFlags -> [FilePath] -> m (Either String [String])
addUnitsLive _ _ = pure (Left "a GHCi before 9.14 takes its units at the start only")
