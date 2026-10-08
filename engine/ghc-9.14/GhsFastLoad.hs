{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | __A reload without the compiler's scan of every module.__
--
-- GHCi's @:reload@ is @depanal@ then @load'@. The first rebuilds the module graph from nothing: every
-- module's source is opened, read and hashed to learn that it did not change (50-130 ms for a hundred
-- modules and 5 MB of source), and only then is the one edited module compiled. The daemon already knows
-- which watched files differ from what is loaded, so it says so ('setChanged') and 'loadWith' builds the
-- graph from the last one instead:
--
-- * the changed files are summarised again with the compiler's own 'summariseFile' (source hash,
--   imports, preprocessing);
-- * every other module keeps its summary, with the dates of its object and interface files read again
--   (a module recompiled as a dependent by the last load has newer ones than its summary says);
-- * if a changed module's IMPORTS are what they were, the graph's shape is the same: its nodes are
--   replaced in place and the compiler's @load'@ is called with it.
--
-- Anything else is the ordinary scan: no change list, a file that is not in the graph (a new module), a
-- file that cannot be read, an import list that moved, anything but a load of all targets. What the scan
-- also does and this does not: it warns about home modules missing from a @.cabal@ and about unused
-- packages, so those warnings appear on a scanned load only.
--
-- @GHS_FAST_GRAPH@: @0@ always scans; @verify@ builds the graph both ways, says where they differ
-- (stderr, so the load's log) and loads the SCANNED one; anything else, or unset, is on.
module GhsFastLoad (loadWith, loadAll, setChanged) where

import Control.Monad (forM, unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import System.Directory (makeAbsolute)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Clock (getMonotonicTime)
import GHC.Fingerprint (fingerprint0)
import Text.Printf (printf)

import qualified GHC
import GHC (GhcMonad, LoadHowMuch (..), ModSummary (..), SuccessFlag, getSession, ms_mod_name)
import GHC.Driver.Downsweep (summariseFile)
import GHC.Driver.Env (HscEnv, hscSetActiveUnitId, hsc_mod_graph, hsc_unit_env)
import GHC.Driver.Errors.Types (GhcMessage)
import GHC.Driver.Make (ModIfaceCache, depanalE, load')
import GHC.Driver.Main (batchMsg, batchMultiMsg)
import GHC.Driver.Messager (Messager)
import GHC.Driver.Env (hsc_all_home_unit_ids)
import qualified Data.Set as S
import GHC.Types.Error (UnknownDiagnostic, mkUnknownDiagnostic)
import GHC.Driver.Errors.Types (AnyGhcDiagnostic)
import GHC.Types.SrcLoc (unLoc)
import GHC.Unit.Env (ue_unitHomeUnit)
import GHC.Unit.Module.Graph (ModuleGraph, ModuleNodeInfo (..), mgMapM, mgModSummaries)
import GHC.Unit.Module.Location (ml_dyn_obj_file, ml_hi_file, ml_hie_file, ml_hs_file, ml_obj_file)
import GHC.Unit.Module.ModSummary (ms_unitid)
import GHC.Utils.Misc (modificationTimeIfExists)
import GHC.Utils.Outputable (showSDocUnsafe, ppr)

-- | The watched files that differ from what is loaded, for the NEXT load only (absolute paths).
{-# NOINLINE changed #-}
changed :: IORef (Maybe [FilePath])
changed = unsafePerformIO (newIORef Nothing)

setChanged :: Maybe [FilePath] -> IO ()
setChanged = writeIORef changed

-- | @GHC.loadWithCache@, with the graph taken from the last load when that is known to be enough.
loadWith :: GhcMonad m => Maybe ModIfaceCache -> (GhcMessage -> AnyGhcDiagnostic) -> LoadHowMuch -> m SuccessFlag
loadWith cache diag how = do
  files <- liftIO (atomicModifyIORef' changed (\c -> (Nothing, c)))
  mode <- liftIO (lookupEnv "GHS_FAST_GRAPH")
  hsc <- getSession
  let scan = GHC.loadWithCache cache diag how
  case (files, how, mode) of
    (_, _, Just "0") -> scan
    (Just fs@(_ : _), LoadAllTargets, _) | not (null (mgModSummaries (hsc_mod_graph hsc))) -> do
      t0 <- liftIO getMonotonicTime
      fast <- liftIO (fastGraph hsc fs)
      t1 <- liftIO getMonotonicTime
      case fast of
        Left why -> liftIO (say ("fast graph: not used (" ++ why ++ "): scanning")) >> scan
        Right g | mode == Just "verify" -> do
          (_, scanned) <- depanalE diag (Just (mkBatchMsg hsc)) [] False
          t2 <- liftIO getMonotonicTime
          let ds = differences g scanned
          liftIO (say (printf "fast graph: verify, %d module(s) from %d changed file(s): built in %.1f ms, the scan %.1f ms; %s"
                         (length (mgModSummaries g)) (length fs) ((t1 - t0) * 1000) ((t2 - t1) * 1000)
                         (if null ds then "IDENTICAL to the scan" else "DIFFERS from the scan in " ++ show (length ds) ++ ": " ++ unwords (take 6 ds))))
          load' cache how diag (Just (mkBatchMsg hsc)) scanned
        Right g -> do
          liftIO (say (printf "fast graph: %d module(s) from %d changed file(s) in %.1f ms (no scan)" (length (mgModSummaries g)) (length fs) ((t1 - t0) * 1000)))
          load' cache how diag (Just (mkBatchMsg hsc)) g
    _ -> scan
  where say m = hPutStrLn stderr m >> (lookupEnv "GHS_FAST_GRAPH_LOG" >>= maybe (pure ()) (\f -> appendFile f (m ++ "\n")))

-- | Everything, with no interface cache: the engine's typecheck.
loadAll :: GhcMonad m => m SuccessFlag
loadAll = loadWith Nothing mkUnknownDiagnostic LoadAllTargets

-- | The compiler's own choice of progress messages (@GHC.Driver.Make.mkBatchMsg@, which it does not export).
mkBatchMsg :: HscEnv -> Messager
mkBatchMsg hsc = if S.size (hsc_all_home_unit_ids hsc) > 1 then batchMultiMsg else batchMsg

-- | The last load's graph with the changed files summarised again, or why it cannot be used.
fastGraph :: HscEnv -> [FilePath] -> IO (Either String ModuleGraph)
fastGraph hsc files = do
  let old = hsc_mod_graph hsc
      sums = mgModSummaries old
  abs' <- mapM makeAbsolute files
  paths <- forM sums (\s -> maybe (pure Nothing) (fmap Just . makeAbsolute) (ml_hs_file (ms_location s)))
  let byPath = M.fromListWith (++) [ (p, [s]) | (Just p, s) <- zip paths sums ]
      hs = [ f | f <- abs', isSource f ]
      unknown = [ f | f <- hs, not (M.member f byPath) ]
      -- the graph of a session's FIRST load has no source hashes (all zero): kept, every module would look
      -- changed to the recompilation check. One scanned load fills them in.
      unhashed = length [ () | s <- sums, ms_hs_hash s == fingerprint0 ]
  if unhashed > 0 then pure (Left (show unhashed ++ " module(s) of the last graph have no source hash (the first load)")) else
   if not (null unknown) then pure (Left ("not in the graph: " ++ unwords (take 3 unknown))) else do
    -- each changed file, in each unit that has it, by the compiler's own summary
    news <- forM [ (f, s) | f <- hs, s <- M.findWithDefault [] f byPath ] $ \(f, s) -> do
      let uid = ms_unitid s
          hsc' = hscSetActiveUnitId uid hsc
          oldMap = M.fromList [ ((ms_unitid x, p), x) | (Just p, x) <- [(ml_hs_file (ms_location s), s)] ]
      r <- summariseFile hsc' (ue_unitHomeUnit uid (hsc_unit_env hsc')) oldMap (maybe f id (ml_hs_file (ms_location s))) Nothing Nothing
      pure (f, s, r)
    case [ f | (f, _, Left _) <- news ] of
      (f : _) -> pure (Left ("cannot be summarised: " ++ f))
      [] -> do
        let moved = [ f | (f, s, Right n) <- news, imports s /= imports n || ms_mod s /= ms_mod n ]
        if not (null moved) then pure (Left ("imports changed: " ++ unwords (take 3 moved))) else do
          let fresh = M.fromList [ ((ms_unitid n, ms_mod n), n) | (_, _, Right n) <- news ]
          Right <$> mgMapM (\info -> case info of
            ModuleNodeCompile s -> case M.lookup (ms_unitid s, ms_mod s) fresh of
              Just n -> pure (ModuleNodeCompile n)
              Nothing -> ModuleNodeCompile <$> redate s
            other -> pure other) old
  where
    isSource f = any (`isSuffix` f) [".hs", ".lhs", ".hs-boot", ".lhs-boot", ".hsig"]
    isSuffix suf s = drop (length s - length suf) s == suf

-- | What a module imports, as the graph's edges see it.
imports :: ModSummary -> ([String], [String])
imports s = ( sort [ show lvl ++ " " ++ showSDocUnsafe (ppr q) ++ " " ++ showSDocUnsafe (ppr (unLoc m)) | (lvl, q, m) <- ms_textual_imps s ]
            , sort [ showSDocUnsafe (ppr (unLoc m)) | m <- ms_srcimps s ] )

-- | An unchanged module's summary with the dates of what was built from it read again (what the scan
-- does for a module whose source hash is what it was).
redate :: ModSummary -> IO ModSummary
redate s = do
  let loc = ms_location s
  o <- modificationTimeIfExists (ml_obj_file loc)
  d <- modificationTimeIfExists (ml_dyn_obj_file loc)
  i <- modificationTimeIfExists (ml_hi_file loc)
  h <- modificationTimeIfExists (ml_hie_file loc)
  pure s { ms_obj_date = o, ms_dyn_obj_date = d, ms_iface_date = i, ms_hie_date = h }

-- | Where two graphs differ, module by module: the source hash, the imports and the dates.
differences :: ModuleGraph -> ModuleGraph -> [String]
differences a b =
  [ "count " ++ show (M.size ma) ++ "/" ++ show (M.size mb) | M.size ma /= M.size mb ]
  ++ [ name k ++ ":" ++ what | (k, x) <- M.toList ma, Just y <- [M.lookup k mb]
     , what <- [ "hash(" ++ show (ms_hs_hash x) ++ " scan " ++ show (ms_hs_hash y) ++ ")" | ms_hs_hash x /= ms_hs_hash y ] ++ [ "imports" | imports x /= imports y ]
             ++ [ "obj-date" | ms_obj_date x /= ms_obj_date y ] ++ [ "dyn-obj-date" | ms_dyn_obj_date x /= ms_dyn_obj_date y ]
             ++ [ "iface-date" | ms_iface_date x /= ms_iface_date y ] ++ [ "hie-date" | ms_hie_date x /= ms_hie_date y ]
             ++ [ "file" | ml_hs_file (ms_location x) /= ml_hs_file (ms_location y) ] ++ [ "hspp" | ms_hspp_file x /= ms_hspp_file y && isJust (ml_hs_file (ms_location x)) && False ] ]
  ++ [ name k ++ ":missing" | k <- M.keys ma, not (M.member k mb) ]
  where
    key s = (showSDocUnsafe (ppr (ms_unitid s)), showSDocUnsafe (ppr (ms_mod_name s)), show (ms_hsc_src s))
    name (_, m, _) = m
    ma = M.fromList [ (key s, s) | s <- mgModSummaries a ]
    mb = M.fromList [ (key s, s) | s <- mgModSummaries b ]
