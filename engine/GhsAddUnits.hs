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
--
-- The adding itself is per compiler ("GhsUnits"): GHCi 9.14 has interactive units to make again; an older
-- one does not, takes its units at the start only, and the daemon restarts the repl with the new set.
module GhsAddUnits (rememberInitial, addUnits, addTargets) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (isPrefixOf)
import System.Directory (getCurrentDirectory)
import System.FilePath (isAbsolute, makeRelative, normalise, (</>))
import System.IO.Unsafe (unsafePerformIO)

import qualified GHC
import GHC (GhcMonad, getSession)
import GHC.Driver.Session (DynFlags (..))
import GHC.Unit.Types (unitIdString)

import GhsCompat (homeUnits)
import GhsUnits (addUnitsLive)

-- | The session's flags before any unit's were read (set once, by the front end, before @initMulti@).
{-# NOINLINE initial #-}
initial :: IORef (Maybe DynFlags)
initial = unsafePerformIO (newIORef Nothing)

rememberInitial :: GhcMonad m => m ()
rememberInitial = GHC.getSessionDynFlags >>= liftIO . writeIORef initial . Just

-- | Add the units these files describe (each as cabal writes it: the flags, then the modules), parsed over the
-- session's flags from before its units ('rememberInitial'). The unit ids added, or why nothing was (the graph
-- is then as it was).
addUnits :: GhcMonad m => [FilePath] -> m (Either String [String])
addUnits files = do
  mbase <- liftIO (readIORef initial)
  case mbase of
    Nothing -> pure (Left "the session's initial flags were not kept (not started with units)")
    Just base -> addUnitsLive base files

-- | __A module added to a unit that is running__ (a name listed in its @.cabal@ since the start). GHCi's own
-- @:add FILE@ gives the file to the unit that is current at the prompt, and in a multi-unit session that is the
-- interactive one -- and a GHCi 9.14 started from a unit file is multi-unit whether or not @-unit@ was said
-- (cabal passes one component as a bare @\@file@). The module is then compiled THERE too, into the same object
-- directory as the unit that lists it: two objects, each with its unit's symbols, take turns at one path, and
-- the next link of the real unit fails with an undefined symbol. Here each file goes to the unit whose import
-- paths hold it (the only unit, when there is one): the next load compiles it there, once. A module name
-- (when its file was not found) is given the same way. What was added as @(file, unit)@, or why one could not be.
addTargets :: GhcMonad m => [FilePath] -> m (Either String [(FilePath, String)])
addTargets files = do
  hsc <- getSession
  cwd <- liftIO getCurrentDirectory
  let units = [ (uid, d) | (uid, d, _) <- homeUnits hsc, not ("interactive" `isPrefixOf` unitIdString uid) ]
      roots d = [ normalise (wd </> p) | p <- importPaths d, let wd = maybe cwd (cwd </>) (workingDirectory d) ]
      under r f = not (isAbsolute (makeRelative r f))      -- (a path not under r comes back as it was: absolute)
      owner f = case [ uid | (uid, d) <- units, any (`under` normalise (cwd </> f)) (roots d) ] of
        (u : _) -> Right u
        [] -> case units of
          [(u, _)] -> Right u
          _ -> Left (f ++ ": under the import paths of none of " ++ unwords (map (unitIdString . fst) units))
      picked = [ (f, owner f) | f <- files ]
  case [ why | (_, Left why) <- picked ] of
    (why : _) -> pure (Left why)
    [] -> do
      forM_ [ (f, u) | (f, Right u) <- picked ] $ \(f, u) -> GHC.guessTarget f (Just u) Nothing >>= GHC.addTarget
      pure (Right [ (f, unitIdString u) | (f, Right u) <- picked ])
