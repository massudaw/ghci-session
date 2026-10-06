{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- | __A package added to a session that is running.__
--
-- GHCi has no command for it: the home units are the @-unit \@file@ arguments it was started with
-- (@GHC.Driver.Session.Units.initMulti@, which REPLACES the session's unit graph). But a GHCi session is a
-- graph of home units from the start (Note [Multiple Home Units aware GHCi]), and each of them is made by
-- three calls -- parse the unit's flags, 'initUnits', a 'HomeUnitEnv' -- which is what 'addUnits' does for
-- a unit file given later, INSERTING into the graph that is there:
--
-- 1. the unit's flags, parsed over the flags the session had BEFORE its units ('rememberInitial': a unit's
--    file says everything about it, and must not inherit another unit's);
-- 2. its package state, with the home units that exist counted as home;
-- 3. the two interactive units made again (they depend on every home unit: it is how the prompt sees them);
-- 4. its source files as targets. Nothing is compiled here: the next load does that, like any reload.
--
-- What is already loaded and linked is not touched. A unit must be NEW (a unit whose flags changed is a
-- restart: its modules are compiled against the old ones) and may depend only on what is there.
module GhsAddUnits (rememberInitial, addUnits) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (partition)
import qualified Data.Set as S
import System.FilePath (isRelative, (</>))
import System.IO.Unsafe (unsafePerformIO)

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

-- | The session's flags before any unit's were read (set once, by the front end, before @initMulti@).
{-# NOINLINE initial #-}
initial :: IORef (Maybe DynFlags)
initial = unsafePerformIO (newIORef Nothing)

rememberInitial :: GhcMonad m => m ()
rememberInitial = GHC.getSessionDynFlags >>= liftIO . writeIORef initial . Just

-- | Add the units these files describe (each as cabal writes it: the flags, then the modules). The unit
-- ids added, or why nothing was (the graph is then as it was).
addUnits :: GhcMonad m => [FilePath] -> m (Either String [String])
addUnits files = do
  hsc <- getSession
  let logger = hsc_logger hsc
  mbase <- liftIO (readIORef initial)
  case mbase of
    Nothing -> pure (Left "the session's initial flags were not kept (not started with units)")
    Just base -> do
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
