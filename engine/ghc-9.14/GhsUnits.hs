{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- | A package added to a running session, on GHC 9.14 (see "GhsAddUnits"): the unit's flags parsed over the
-- session's from before its units, its package state with the home units that exist counted as home, the two
-- interactive units made again (they depend on every home unit: it is how the prompt sees them), its sources
-- as targets. The only engine module that needs the front end ("GHCi.UI"), so it is apart from "GhsCompat",
-- which the front end's own load goes through.
module GhsUnits (addUnitsLive) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (partition)
import qualified Data.Set as S
import System.FilePath (isRelative, (</>))

import qualified GHC
import GHC (GhcMonad, getSession, parseTargetFiles)
import GHC.Driver.Monad (modifySession)
import GHC.Data.Graph.Directed (SCC (..))
import GHC.Driver.DynFlags (DynFlags (..), homeUnitId_)
import GHC.Driver.Env (hscUpdateHUG, hsc_HUG, hsc_all_home_unit_ids, hsc_logger)
import GHC.Driver.Phases (isHaskellishTarget)
import GHC.Driver.Session (parseDynamicFlagsCmdLine, updatePlatformConstants)
import GHC.ResponseFile (expandResponse)
import GHC.Types.SrcLoc (mkGeneralLocated, unLoc)
import GHC.Unit.Env (HomeUnitEnv (..))
import qualified GHC.Unit.Home.Graph as HUG
import GHC.Unit.Home.PackageTable (emptyHomePackageTable)
import GHC.Unit.State (initUnits)
import GHC.Unit.Types (unitIdString)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import qualified Data.Foldable as F
import Data.Maybe (catMaybes)

import GHCi.UI (installInteractiveHomeUnits)

-- | Add the units these files describe, parsed over @base@ (the session's flags before its units). The unit ids
-- added, or why nothing was (the graph is then as it was).
addUnitsLive :: GhcMonad m => DynFlags -> [FilePath] -> m (Either String [String])
addUnitsLive base files = do
  hsc <- getSession
  let logger = hsc_logger hsc
  do
    do
      parsed <- forM files $ \f -> do
        args <- liftIO (expandResponse ['@' : f])
        (d2, fileish, _warns) <- parseDynamicFlagsCmdLine logger base (map (mkGeneralLocated f) (removeRTS args))
        let (d3, srcs, _objs) = parseTargetFiles d2 (map unLoc fileish)
        pure (offsetDynFlags d3, fst (partition isHaskellishTarget srcs))
      let have = hsc_all_home_unit_ids hsc
          new = [ homeUnitId_ d | (d, _) <- parsed ]
          dup = [ u | u <- new, S.member u have ] ++ [ u | (u, k) <- zip new [0 :: Int ..], u `elem` take k new ]
      if not (null dup) then pure (Left ("already a unit of this session: " ++ unwords (map unitIdString dup))) else do
        let allIds = S.union have (S.fromList new)
            -- the package databases the units there have read: not read again
            cached = concat (catMaybes (map homeUnitEnv_unit_dbs (F.toList (hsc_HUG hsc))))
        envs <- forM parsed $ \(d, _) -> liftIO $ do
          (dbs, us, hu, mconstants) <- initUnits logger d (Just cached) allIds
          d' <- updatePlatformConstants d mconstants
          hpt <- emptyHomePackageTable
          pure (homeUnitId_ d, HomeUnitEnv { homeUnitEnv_units = us, homeUnitEnv_unit_dbs = Just dbs, homeUnitEnv_dflags = d'
                                           , homeUnitEnv_hpt = hpt, homeUnitEnv_home_unit = Just hu })
        let grown g = foldr (\(u, e) -> HUG.unitEnv_insert u e) g envs
            cycles = [ us | CyclicSCC us <- HUG.hugSCCs (grown (hsc_HUG hsc)) ]
        if not (null cycles) then pure (Left ("the units would form a cycle: " ++ unwords (map (showSDocUnsafe . ppr) (concat cycles)))) else do
          modifySession (hscUpdateHUG grown)
          installInteractiveHomeUnits
          forM_ parsed $ \(d, srcs) -> forM_ srcs $ \(file, phase) ->
            GHC.guessTarget file (Just (homeUnitId_ d)) phase >>= GHC.addTarget
          pure (Right (map unitIdString new))

-- (the two below are GHC.Driver.Session.Units' own, which it does not export)

-- Strip out any ["+RTS", ..., "-RTS"] sequences in the command string list.
removeRTS :: [String] -> [String]
removeRTS ("+RTS" : xs) = case dropWhile (/= "-RTS") xs of { [] -> []; (_ : ys) -> removeRTS ys }
removeRTS (y : ys) = y : removeRTS ys
removeRTS [] = []

offsetDynFlags :: DynFlags -> DynFlags
offsetDynFlags dflags = dflags { hiDir = c hiDir, objectDir = c objectDir, stubDir = c stubDir, hieDir = c hieDir, dumpDir = c dumpDir }
  where
    c f = fmap augment (f dflags)
    augment f | isRelative f, Just offset <- workingDirectory dflags = offset </> f
              | otherwise = f
