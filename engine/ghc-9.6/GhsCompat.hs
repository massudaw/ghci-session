{-# LANGUAGE ImplicitPrelude #-}
-- | The few places the engine reads or rewrites the compiler's session where GHC versions differ: the home
-- unit graph (its module and its lookup), a module's direct dependencies, and how a session is given another
-- module graph. One copy per compiler (engine/ghc-X.Y); "GhsEngine" is the same for all.
--
-- GHC 9.6: the graph is "GHC.Unit.Env"'s, its lookup is pure, a dependency has no import level, and the
-- session's module graph is a plain field.
module GhsCompat (homeUnits, mapUnitFlags, withModuleGraph, homeObject) where

import qualified Data.Set as S

import GHC.Driver.Env (HscEnv (..), hscUpdateHUG, hsc_HUG)
import GHC.Driver.Session (DynFlags)
import GHC.Linker.Types (Linkable)
import GHC.Unit.Env (HomeUnitEnv (..), hugElts, lookupHugByModule, unitEnv_adjust)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), homeModInfoObject)
import GHC.Unit.Module.Deps (dep_direct_mods)
import GHC.Unit.Module.Graph (ModuleGraph)
import GHC.Unit.Module.ModIface (mi_deps)
import GHC.Unit.State (homeUnitDepends)
import GHC.Unit.Types (Definite (..), GenUnit (..), GenWithIsBoot (..), Module, UnitId, mkModule)

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
