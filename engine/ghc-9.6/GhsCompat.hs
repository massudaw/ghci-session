{-# LANGUAGE ImplicitPrelude #-}
-- | The few places the engine reads or rewrites the compiler's session where GHC versions differ: the home
-- unit graph (its module and its lookup), a module's direct dependencies, and how a session is given another
-- module graph. One copy per compiler (engine/ghc-X.Y); "GhsEngine" is the same for all.
--
-- GHC 9.6: the graph is "GHC.Unit.Env"'s, its lookup is pure, a dependency has no import level, and the
-- session's module graph is a plain field.
module GhsCompat
  ( homeUnits, mapUnitFlags, withModuleGraph, homeObject, promptUnit
  , Diag, noDiag, loadScan, depanalScan, loadGraph, mapSummaries, importKeys
  , Messager, batchMsg, batchMultiMsg, summariseFile
  ) where

import qualified Data.Set as S

import Control.Monad (forM)
import GHC (GhcMonad, LoadHowMuch, ModSummary (..), SuccessFlag, getSession)
import qualified GHC
import GHC.Driver.Main (Messager, batchMsg, batchMultiMsg)
import GHC.Driver.Make (ModIfaceCache, depanalE, load', summariseFile)
import GHC.Types.SrcLoc (unLoc)
import GHC.Unit.Module.Graph (ModuleGraphNode (..), mgModSummaries', mkModuleGraph)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import GHC.Driver.Env (HscEnv (..), hscActiveUnitId, hscSetActiveUnitId, hscUpdateHUG, hsc_HUG)
import GHC.Driver.Monad (modifySession)
import GHC.Driver.Session (DynFlags)
import GHC.Linker.Types (Linkable)
import GHC.Unit.Env (HomeUnitEnv (..), hugElts, lookupHugByModule, unitEnv_adjust)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), homeModInfoObject)
import GHC.Unit.Module.Deps (dep_direct_mods)
import GHC.Unit.Module.Graph (ModuleGraph)
import GHC.Unit.Module.ModIface (mi_deps)
import GHC.Unit.State (homeUnitDepends)
import GHC.Unit.Types (Definite (..), GenUnit (..), GenWithIsBoot (..), Module, UnitId, mkModule, unitIdString)

-- | Each home unit: its id, its flags, and the home units it depends on.
homeUnits :: HscEnv -> [(UnitId, DynFlags, [UnitId])]
homeUnits hsc = [ (uid, homeUnitEnv_dflags ue, homeUnitDepends (homeUnitEnv_units ue)) | (uid, ue) <- hugElts (hsc_HUG hsc) ]

-- | Every home unit's flags, changed.
mapUnitFlags :: (UnitId -> DynFlags -> DynFlags) -> HscEnv -> HscEnv
mapUnitFlags f hsc = foldl (\h (uid, _) -> hscUpdateHUG (unitEnv_adjust (\ue -> ue { homeUnitEnv_dflags = f uid (homeUnitEnv_dflags ue) }) uid) h) hsc (hugElts (hsc_HUG hsc))

withModuleGraph :: ModuleGraph -> HscEnv -> HscEnv
withModuleGraph g hsc = hsc { hsc_mod_graph = g }

-- | A home module's object as the session has it now, and the home modules it imports directly.
homeObject :: HscEnv -> Module -> IO (Maybe (Linkable, [Module]))
homeObject hsc m =
  pure $ case lookupHugByModule m (hsc_HUG hsc) of
    Just hmi | Just ln <- homeModInfoObject hmi ->
      Just (ln, [ mkModule (RealUnit (Definite u)) (gwib_mod n) | (u, n) <- S.toList (dep_direct_mods (mi_deps (hm_iface hmi))) ])
    _ -> Nothing

-- | The unit the prompt works in: the one from which the most home units are in scope.
--
-- GHCi 9.6 has no interactive units. The prompt resolves a module through the ACTIVE home unit, which finds
-- its own modules and those of the home units it depends on DIRECTLY (GHC.Unit.Finder: the active unit's
-- homeUnitDepends) -- and the active unit is simply the first one the build tool listed. In a composed
-- session (a package and one that imports it) that can be the one underneath, and the other package's
-- modules are then "not in scope" at the prompt and in its checks. So the active unit is made the one that
-- sees the most units (itself and its direct home dependencies), the build tool's first on a tie. When no unit
-- sees them all -- three packages in a chain, or two that do not depend on each other -- what the chosen one
-- cannot see is out of scope on this compiler. The unit chosen, when it changed.
promptUnit :: GhcMonad m => m (Maybe String)
promptUnit = do
  hsc <- getSession
  let units = [ (u, ds) | (u, _, ds) <- homeUnits hsc ]
      ids = S.fromList (map fst units)
      sees (u, ds) = S.size (S.intersection ids (S.fromList (u : ds)))
      active = hscActiveUnitId hsc
      activeSees = maybe 0 (\ds -> sees (active, ds)) (lookup active units)
      best = foldl (\b x -> if sees x > maybe activeSees sees b then Just x else b) Nothing units
  case best of
    Just (u, _) | u /= active -> do
      modifySession (hscSetActiveUnitId u)
      pure (Just (unitIdString u ++ " (" ++ show (sees (u, maybe [] id (lookup u units))) ++ " of " ++ show (S.size ids) ++ " units in scope)"))
    _ -> pure Nothing

-- | How a load reports its diagnostics: the front end's wrapper (on 9.6 there is none, and it is @()@).
-- What follows is the load "GhsFastLoad" makes: the scan of every module, the graph's own load, and a summary
-- of each module mapped over a graph.
type Diag = ()

noDiag :: Diag
noDiag = ()

loadScan :: GhcMonad m => Maybe ModIfaceCache -> Diag -> LoadHowMuch -> m SuccessFlag
loadScan cache () how = GHC.loadWithCache cache how

depanalScan :: GhcMonad m => Diag -> Messager -> m ModuleGraph
depanalScan _ _ = snd <$> depanalE [] False

loadGraph :: GhcMonad m => Maybe ModIfaceCache -> LoadHowMuch -> Diag -> Messager -> ModuleGraph -> m SuccessFlag
loadGraph cache how () msg = load' cache how (Just msg)

mapSummaries :: (ModSummary -> IO ModSummary) -> ModuleGraph -> IO ModuleGraph
mapSummaries f g = mkModuleGraph <$> forM (mgModSummaries' g) (\node -> case node of
  ModuleNode deps s -> ModuleNode deps <$> f s
  other -> pure other)

-- | What a module imports, as the graph's edges see it.
importKeys :: ModSummary -> ([String], [String])
importKeys s = ( [ showSDocUnsafe (ppr q) ++ " " ++ showSDocUnsafe (ppr (unLoc m)) | (q, m) <- ms_textual_imps s ]
               , [ showSDocUnsafe (ppr (unLoc m)) | (_, m) <- ms_srcimps s ] )
