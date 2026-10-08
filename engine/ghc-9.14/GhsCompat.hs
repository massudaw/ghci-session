{-# LANGUAGE ImplicitPrelude #-}
-- | The few places the engine reads or rewrites the compiler's session where GHC versions differ: the home
-- unit graph (its module and its lookup), a module's direct dependencies, and how a session is given another
-- module graph. One copy per compiler (engine/ghc-X.Y); "GhsEngine" is the same for all.
module GhsCompat
  ( homeUnits, mapUnitFlags, withModuleGraph, homeObject, promptUnit
  , Diag, noDiag, loadScan, depanalScan, loadGraph, mapSummaries, importKeys
  , Messager, batchMsg, batchMultiMsg, summariseFile
  ) where

import qualified Data.Set as S

import GHC (GhcMonad, LoadHowMuch, ModSummary (..), SuccessFlag)
import qualified GHC
import GHC.Driver.Downsweep (summariseFile)
import GHC.Driver.Errors.Types (AnyGhcDiagnostic, GhcMessage)
import GHC.Driver.Main (batchMsg, batchMultiMsg)
import GHC.Driver.Make (ModIfaceCache, depanalE, load')
import GHC.Driver.Messager (Messager)
import GHC.Types.Error (mkUnknownDiagnostic)
import GHC.Types.SrcLoc (unLoc)
import GHC.Unit.Module.Graph (ModuleNodeInfo (..), mgMapM)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import GHC.Driver.Env (HscEnv, hscUpdateHUG, hsc_HUG, setModuleGraph)
import GHC.Driver.Session (DynFlags)
import GHC.Linker.Types (Linkable)
import GHC.Unit.Home.Graph (homeUnitEnv_dflags, homeUnitEnv_units, lookupHugByModule, unitEnv_assocs, updateUnitFlags)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), homeModInfoObject)
import GHC.Unit.Module.Deps (dep_direct_mods)
import GHC.Unit.Module.Graph (ModuleGraph)
import GHC.Unit.Module.ModIface (mi_deps)
import GHC.Unit.State (homeUnitDepends)
import GHC.Unit.Types (Definite (..), GenUnit (..), GenWithIsBoot (..), Module, UnitId, mkModule)

-- | Each home unit: its id, its flags, and the home units it depends on.
homeUnits :: HscEnv -> [(UnitId, DynFlags, [UnitId])]
homeUnits hsc = [ (uid, homeUnitEnv_dflags ue, homeUnitDepends (homeUnitEnv_units ue)) | (uid, ue) <- unitEnv_assocs (hsc_HUG hsc) ]

-- | Every home unit's flags, changed.
mapUnitFlags :: (UnitId -> DynFlags -> DynFlags) -> HscEnv -> HscEnv
mapUnitFlags f hsc = foldl (\h (uid, _) -> hscUpdateHUG (updateUnitFlags uid (f uid)) h) hsc (unitEnv_assocs (hsc_HUG hsc))

withModuleGraph :: ModuleGraph -> HscEnv -> HscEnv
withModuleGraph = setModuleGraph

-- | A home module's object as the session has it now, and the home modules it imports directly.
homeObject :: HscEnv -> Module -> IO (Maybe (Linkable, [Module]))
homeObject hsc m = do
  mi <- lookupHugByModule m (hsc_HUG hsc)
  pure $ case mi of
    Just hmi | Just ln <- homeModInfoObject hmi ->
      Just (ln, [ mkModule (RealUnit (Definite u)) (gwib_mod n) | (_, u, n) <- S.toList (dep_direct_mods (mi_deps (hm_iface hmi))) ])
    _ -> Nothing

-- | The unit the prompt works in. Nothing to choose here: GHCi 9.14's interactive units depend on every home
-- unit, so every one is in scope at the prompt.
promptUnit :: GhcMonad m => m (Maybe String)
promptUnit = pure Nothing

-- | How a load reports its diagnostics: the front end's wrapper (on 9.6 there is none, and it is @()@).
-- What follows is the load "GhsFastLoad" makes: the scan of every module, the graph's own load, and a summary
-- of each module mapped over a graph.
type Diag = GhcMessage -> AnyGhcDiagnostic

noDiag :: Diag
noDiag = mkUnknownDiagnostic

loadScan :: GhcMonad m => Maybe ModIfaceCache -> Diag -> LoadHowMuch -> m SuccessFlag
loadScan = GHC.loadWithCache

depanalScan :: GhcMonad m => Diag -> Messager -> m ModuleGraph
depanalScan diag msg = snd <$> depanalE diag (Just msg) [] False

loadGraph :: GhcMonad m => Maybe ModIfaceCache -> LoadHowMuch -> Diag -> Messager -> ModuleGraph -> m SuccessFlag
loadGraph cache how diag msg = load' cache how diag (Just msg)

mapSummaries :: (ModSummary -> IO ModSummary) -> ModuleGraph -> IO ModuleGraph
mapSummaries f = mgMapM (\info -> case info of { ModuleNodeCompile s -> ModuleNodeCompile <$> f s; other -> pure other })

-- | What a module imports, as the graph's edges see it (an import has a level on 9.14).
importKeys :: ModSummary -> ([String], [String])
importKeys s = ( [ show lvl ++ " " ++ showSDocUnsafe (ppr q) ++ " " ++ showSDocUnsafe (ppr (unLoc m)) | (lvl, q, m) <- ms_textual_imps s ]
               , [ showSDocUnsafe (ppr (unLoc m)) | m <- ms_srcimps s ] )
