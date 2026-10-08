{-# LANGUAGE ForeignFunctionInterface #-}

-- | __GHCi with a socket where its terminal was.__
--
-- This executable is GHCi: the same source as the @ghc@ binary's interactive front end (@vendor/ghc-X.Y.Z@,
-- unchanged), so every command behaves as it does there. What differs is where commands come from, where
-- their output goes, and that it can be ASKED things.
--
-- It needs no change to GHCi's loop. GHCi calls a prompt function before it reads each command; ours is the
-- turn. It sends the reply to the command that just finished -- everything the command wrote, and the
-- compiler's diagnostics as records -- then waits for the next request. A request is either a GHCi command,
-- which the turn feeds to GHCi's standard input (a pipe this process holds the other end of), or a QUERY,
-- which the turn answers itself, in GHCi's own monad, and waits again: what is loaded and from which objects,
-- does this expression typecheck, fork a server out of this process, unlink the CAFs a reload superseded.
-- Those were once GHCi commands whose printed output the session daemon read back; here they are functions
-- of the session.
--
-- Three ways to start it:
--
-- * with @GHS_CAPTURE=dir@ and @--interactive@ among its arguments (how @cabal repl --with-repl=@ starts it):
--   it writes down its arguments, directory and environment, and exits. The daemon then starts it itself, so
--   cabal does not stay as a process between them, and a restart need not ask cabal again;
-- * with @GHS_CONTROL=stdin@: standard input is the daemon's socket;
-- * with neither: an ordinary GHCi.
module GhsEngine (engineInit, libdirArgs, engineSettings, engineHook) where

import Prelude

import Control.Concurrent (forkIO, setNumCapabilities, threadWaitRead)
import Control.Concurrent.MVar
import Control.Exception (IOException, SomeException, displayException, try)
import Control.Monad (filterM, forM, unless, void, when)
import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Class (liftIO)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import Foreign.Ptr (FunPtr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.Types (CInt (..))
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Data.Time (UTCTime)
import System.Directory (copyFile, createDirectoryIfMissing, getCurrentDirectory, getModificationTime, renameFile)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, takeDirectory, takeFileName, (</>))
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performMajorGC, performMinorGC)
import System.Posix.IO
import qualified System.Posix.IO.ByteString as PB
import System.Posix.Process (ProcessStatus, exitImmediately, getProcessStatus)
import System.Posix.Types (Fd (..))
import System.Process (readProcess)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import Unsafe.Coerce (unsafeCoerce)

import qualified GHC
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Backend (noBackend)
import GHC.Driver.Session (DynFlags (..), GeneralFlag (..), GhcLink (..), gopt_set)
import GHC.Driver.Env (hscInterp, hsc_mod_graph)
import GHC.Types.Basic (SuccessFlag (..))
import GHC.Linker.Types (Linkable, Loader (..), LoaderState (..), linkableObjs)
import GHC.Runtime.Interpreter.Types (interpLoader)
import GHC.Unit.Module.Env (extendModuleEnv, moduleEnvToList)
import GHC.Unit.Types (Module, isInteractiveModule)
import GHC.Driver.Session (thisPackageName, workingDirectory)
import GHC.Tc.Module (TcRnExprMode (..))
import GHC.Types.Error (MessageClass (..), Severity (..))
import GHC.Types.SrcLoc
import GHC.Unit.Module.Graph (ModuleGraph, emptyMG, mgModSummaries)
import GHC.Unit.Module.Location (ml_obj_file)
import GHC.Unit.Module.ModSummary (ms_location, ms_mod, ms_unitid)
import GHC.Unit.Types (unitIdString)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Utils.Logger (LogAction, log_default_user_context, pushLogHook)
import GHC.Utils.Outputable (empty, ppr, renderWithContext, showSDocUnsafe)
import GHCi.UI (GhciSettings (..))
import GHCi.UI.Monad (GHCi)

import GHC.Exts (Any)
import GHC.Hygiene (engineSymbol, heapAuto, majorGC)
import GhsAddUnits (addTargets, addUnits)
import GhsCompat (homeObject, homeUnits, mapUnitFlags, withModuleGraph)
import GhsFastLoad (loadAll, setChanged)
import GHC.Hygiene.Store (storeDrop, storeNames)
import GHC.Hygiene.Census (dupsCafs, dupsKept, dupsOf, benchQuick, benchOf, cafReport, cafStrings, censusOf, keptReport, keptStrings, memNow)
import GHC.Hygiene.Zygote (ZygoteChild (..), ZygoteSpec (..), zygoteFork)
import GhciSession.Json

foreign import ccall safe "ghs_prune_cafs" c_prune :: IO CInt
foreign import ccall unsafe "ghs_exports_count" c_exports :: IO CInt

data Engine = Engine
  { eControl :: Handle      -- ^ requests in, replies out
  , eControlFd :: Fd
  , eStdin :: Fd            -- ^ the write end of the pipe GHCi reads its commands from
  , eCapture :: Fd          -- ^ the read end of the pipe standard output and error are
  , eOut :: MVar [B.ByteString]   -- ^ what has been read from it since the last turn, newest first
  }

{-# NOINLINE engine #-}
engine :: IORef (Maybe Engine)
engine = unsafePerformIO (newIORef Nothing)

-- | The compiler's diagnostics since the last reply, newest first, and how many there were (the list is capped).
{-# NOINLINE diagnostics #-}
diagnostics :: IORef ([Json], Int, Int)
diagnostics = unsafePerformIO (newIORef ([], 0, 0))

-- | Before GHC starts: write down how we were started and leave (@GHS_CAPTURE@), or take the daemon's socket
-- from standard input and put pipes where the standard three were (@GHS_CONTROL@).
engineInit :: IO ()
engineInit = do
  args <- getArgs
  cap <- lookupEnv "GHS_CAPTURE"
  ctl <- lookupEnv "GHS_CONTROL"
  interactive <- argsInteractive args
  case cap of
    Just dir | interactive -> capture dir args >> exitImmediately ExitSuccess
    _ -> do
      -- GHS_HEAP_AUTO=0: without the -H this executable is built with, as GHC's is (the RTS then spends
      -- the largest heap it has needed on allocation area: memory that is never live)
      ha <- lookupEnv "GHS_HEAP_AUTO"
      when (ha == Just "0") (void (heapAuto 0))
      when (ctl == Just "stdin") takeControl

-- | The @-B@ this run needs, if it was given none. GHC finds its library directory beside its own executable
-- (@../lib@ of it), which is right for the @ghc@ binary and wrong for this one, built elsewhere: the daemon
-- passes @-B@ when it starts the engine, but the build tool probes its repl program (@--info@) without one
-- and the macOS layout happened to answer. So: the compiler on PATH is asked (@GHS_LIBDIR@ says it without
-- a process), unless a @lib/settings@ really is beside us.
libdirArgs :: [String] -> IO [String]
libdirArgs args
  | any ("-B" `isPrefixOf`) args = pure []
  | otherwise = do
      exe <- getExecutablePath
      own <- doesFileExist (takeDirectory exe </> ".." </> "lib" </> "settings")
      if own then pure [] else do
        env <- lookupEnv "GHS_LIBDIR"
        l <- case env of
          Just d | not (null d) -> pure d
          _ -> either (\(_ :: IOException) -> "") (takeWhile (/= '\n')) <$> try (readProcess "ghc" ["--print-libdir"] "")
        pure [ "-B" ++ l | not (null l) ]

-- | Is this a @--interactive@ start? Cabal may put every argument in ONE response file (@\@file@, one
-- argument a line: cabal 3.18 on Linux does), so those are read too.
argsInteractive :: [String] -> IO Bool
argsInteractive args
  | "--interactive" `elem` args = pure True
  | otherwise = or <$> forM [ f | '@' : f <- args ] (\f -> do
      t <- try (readFile f >>= \x -> length x `seq` pure x) :: IO (Either IOException String)
      pure (either (const False) (("--interactive" `elem`) . map (filter (/= '\r')) . lines) t))

-- | What @cabal repl@ would have run, as files: @args@ (the directory, then each argument, NUL-separated)
-- and @env@. Cabal deletes the per-unit argument files of a multi-unit repl when the repl exits, and a
-- response file it wrote in a temporary directory, so they are copied and the arguments point at the
-- copies -- a response file's own @\@file@ lines too.
capture :: FilePath -> [String] -> IO ()
capture dir args = do
  createDirectoryIfMissing True dir
  cwd <- getCurrentDirectory
  let abs' f = if isAbsolute f then f else cwd </> f
  args' <- forM (zip [0 :: Int ..] args) $ \(i, a) -> case a of
    '@' : f -> do
      let to = dir </> ("unit-" ++ show i ++ "-" ++ takeFileName f)
      copyFile (abs' f) to
      ls <- lines <$> readFile to
      length ls `seq` pure ()
      ls' <- forM (zip [0 :: Int ..] ls) $ \(k, l) -> case l of
        '@' : g -> do
          let to' = dir </> ("unit-" ++ show i ++ "-" ++ show k ++ "-" ++ takeFileName g)
          copyFile (abs' g) to'
          pure ('@' : to')
        _ -> pure l
      when (ls' /= ls) (writeFile (to ++ ".new") (unlines ls') >> renameFile (to ++ ".new") to)
      pure ('@' : to)
    _ -> pure a
  env <- getEnvironment
  let nul = B.intercalate (B.singleton 0) . map (TE.encodeUtf8 . T.pack)
  B.writeFile (dir </> "env") (nul [ k ++ "=" ++ v | (k, v) <- env, k /= "GHS_CAPTURE" ])
  B.writeFile (dir </> "args.new") (nul (cwd : args'))
  renameFile (dir </> "args.new") (dir </> "args")

takeControl :: IO ()
takeControl = do
  -- descriptors above 2 first: "duplicate onto 0, then close the original" would otherwise close what it installed
  c <- dup stdInput >>= above2
  setFdOption c CloseOnExec True
  h <- fdToHandle c
  hSetBinaryMode h True
  hSetBuffering h (BlockBuffering Nothing)
  (r0, w0) <- createPipe
  r <- above2 r0
  w <- above2 w0
  _ <- dupTo r stdInput
  closeFd r
  setFdOption w CloseOnExec True
  -- Standard output and error are a PIPE this process reads itself (a file was 7x slower than the terminal
  -- it replaced: 3.4 s for 20,000 lines). A thread drains it as it fills -- a pipe holds 64 KB, and the
  -- writer would otherwise block -- and a turn drains what is left before it replies.
  (cr0, cw0) <- createPipe
  cr <- above2 cr0
  cw <- above2 cw0
  setFdOption cr NonBlockingRead True
  setFdOption cr CloseOnExec True
  _ <- dupTo cw stdOutput
  _ <- dupTo cw stdError
  closeFd cw
  out <- newMVar []
  let e = Engine h c w cr out
  _ <- forkIO (drainLoop e)
  _ <- c_exports          -- (keeps the C that loaded code looks up by name in this executable)
  writeIORef engine (Just e)

-- | The same open file on a descriptor that is not one of the standard three.
above2 :: Fd -> IO Fd
above2 fd
  | fd > 2 = pure fd
  | otherwise = do
      fd' <- dup fd >>= above2
      closeFd fd
      pure fd'

-- | Before the first load: every diagnostic the compiler logs is also kept as a record, for the next reply.
engineHook :: GHC.GhcMonad m => m ()
engineHook = do
  on <- liftIO (readIORef engine)
  case on of
    Nothing -> pure ()
    Just _ -> GHC.modifyLogger (pushLogHook record)

record :: LogAction -> LogAction
record next flags cls sp doc = do
  case cls of
    MCDiagnostic sev _ code | sev /= SevIgnore -> do
      let isErr = sev == SevError
          msg = renderWithContext (log_default_user_context flags) doc
          place = case sp of
            RealSrcSpan s _ -> [ ("file", JStr (unpackFS (srcSpanFile s))), ("line", n (srcSpanStartLine s)), ("col", n (srcSpanStartCol s))
                               , ("end_line", n (srcSpanEndLine s)), ("end_col", n (srcSpanEndCol s)) ]
            _ -> []
          n = JNum . fromIntegral
          j = JObj ([ ("severity", JStr (if isErr then "error" else "warning")) ] ++ place
                    ++ [ ("code", JStr (showSDocUnsafe (ppr c))) | Just c <- [code] ] ++ [ ("message", JStr msg) ])
      atomicModifyIORef' diagnostics (\(js, e, w) ->
        ((if length js < 200 then j : js else js, if isErr then e + 1 else e, if isErr then w else w + 1), ()))
    _ -> pure ()
  next flags cls sp doc

-- | GHCi's settings with the turn as its prompt (and no continuation prompt: a multi-line request arrives whole).
engineSettings :: GhciSettings -> GhciSettings
engineSettings s = s
  { defPrompt = \mods line -> do
      me <- liftIO (readIORef engine)
      case me of
        Nothing -> defPrompt s mods line
        Just e -> turn e >> pure empty
  , defPromptCont = \mods line -> do
      me <- liftIO (readIORef engine)
      case me of
        Nothing -> defPromptCont s mods line
        Just _ -> pure empty
  }

-- | One turn: reply to what just ran (the first time, that is the load), then answer queries until a GHCi
-- command arrives, and hand that to GHCi.
turn :: Engine -> GHCi ()
turn e = do
  (kept, dropped, names) <- keepLinked
  -- how many libraries have been linked so far: GHCi links a reloaded module when a command first needs
  -- it, so code superseded by a LATER command than the one the unlink followed is found by this growing
  lv <- loaderVar
  links <- liftIO (maybe 0 (length . temp_sos) <$> readMVar lv)
  (ds, errs, warns) <- liftIO (atomicModifyIORef' diagnostics (\d -> (([], 0, 0), d)))
  liftIO (reply e (JObj [ ("errors", JNum (fromIntegral errs)), ("warnings", JNum (fromIntegral warns)), ("diagnostics", JArr (reverse ds))
                        , ("kept_linked", JNum (fromIntegral kept)), ("relink", JNum (fromIntegral dropped)), ("relink_modules", JArr (map JStr (take 12 names))), ("links", JNum (fromIntegral links)) ]))
  wait
  where
    wait = do
      req <- liftIO (recvFrame (eControl e))
      case req of
        Nothing -> liftIO (exitImmediately ExitSuccess)        -- the daemon is gone: so are we
        Just b -> case BC.uncons b of
          Just ('Q', q) -> do
            r <- MC.try (either (pure . failed) (query e) (parseJsonBS q))
            liftIO (reply e (either (\(x :: SomeException) -> failed (displayException x)) id r))
            wait
          Just ('C', cmd) -> rememberLinked >> liftIO (do
            -- GHCi leaves standard output unbuffered, which is a system call per CHARACTER written: a megabyte of
            -- output was 4.8 s. A line at a time keeps output and errors in the order they were written, and the
            -- turn flushes whatever is left.
            hSetBuffering stdout LineBuffering
            -- exactly ONE line end: a blank line is a command to GHCi too, and would be answered with a turn of its own
            void (forkIO (writeAll (eStdin e) (BC.dropWhileEnd (== '\n') cmd <> BC.pack "\n"))))   -- (a thread: a pipe holds 64 KB)
          _ -> liftIO (reply e (failed "a request is C<command> or Q<json>")) >> wait

-- what a reload must not cost -----------------------------------------------------------------------
--
-- GHC's `load` forgets every object it had linked ("unload everything": the driver no longer works out which
-- modules are stable), so after ANY reload -- one that recompiled nothing, too -- the next evaluation links
-- every loaded module again, into a new temporary library. That is time proportional to the whole session
-- rather than to the edit, a fresh copy of every CAF of every module (the values the old copies held are what
-- the pruner then has to free), and every cached value in an untouched module computed again.
--
-- A module's linked code is still right when its object did not change AND neither did any module it depends
-- on, transitively: its code is bound to theirs. So what was linked before a command is remembered, and after
-- it those modules are put back in the loader's table; the loader then finds them already loaded and links
-- only the rest.
--
-- "Did not change" is asked of the object FILE (its modification time before the command and after), not of
-- the loader's own record: a module compiled in this session is recorded with the time it was compiled, and
-- the same module found up to date by the next reload with the time of its file, so the two never agree on
-- the first reload after a compile. What is put back is the module's linkable as it is NOW, which is the one
-- the loader will compare with.

{-# NOINLINE linkedBefore #-}
linkedBefore :: IORef (Maybe [(Module, [(FilePath, UTCTime)])])
linkedBefore = unsafePerformIO (newIORef Nothing)

{-# NOINLINE keepLinkedOn #-}
keepLinkedOn :: Bool
keepLinkedOn = unsafePerformIO $ do
  -- Kept modules make a temporary library PARTIAL, and a name is then only found where it is current if
  -- each library answers for its own names alone (hygiene/c/mem_return.c, ghs_dlopen: the library the
  -- daemon inserts on macOS). Without that -- another system, the library missing -- every reload links
  -- everything again, as GHCi does; GHS_KEEP_LINKED=1 insists.
  e <- lookupEnv "GHS_KEEP_LINKED"
  first <- (engineSymbol "ghs_dlopen_first" :: IO (Maybe (FunPtr ())))
  let on = e /= Just "0" && (e == Just "1" || isJust first)
  when (e /= Just "0" && not on) (hPutStrLn stderr "ghci-session-engine: modules are not kept linked across a reload (temporary libraries are not opened to answer for their own names only)")
  pure on

loaderVar :: GHCi (MVar (Maybe LoaderState))
loaderVar = loader_state . interpLoader . hscInterp <$> GHC.getSession

rememberLinked :: GHCi ()
rememberLinked = when keepLinkedOn $ do
  v <- loaderVar
  liftIO $ do
    st <- readMVar v
    was <- forM (maybe [] (moduleEnvToList . objs_loaded) st) (\(m, l) -> (,) m <$> objTimes l)
    writeIORef linkedBefore (Just was)

-- | A linkable's object files and when each was written ([] if any cannot be seen: then nothing is claimed).
objTimes :: Linkable -> IO [(FilePath, UTCTime)]
objTimes l = do
  r <- try (forM (linkableObjs l) (\f -> (,) f <$> getModificationTime f)) :: IO (Either IOException [(FilePath, UTCTime)])
  pure (either (const []) id r)

-- | Put back what the command just run made the loader forget and is still right: how many were kept, and
-- how many were linked before and must be linked again.
keepLinked :: GHCi (Int, Int, [String])
keepLinked = do
  before <- liftIO (atomicModifyIORef' linkedBefore (\b -> (Nothing, b)))
  case before of
    Nothing -> pure (0, 0, [])
    Just old -> do
      hsc <- GHC.getSession
      v <- loaderVar
      now <- liftIO (maybe [] (map fst . moduleEnvToList . objs_loaded) <$> readMVar v)
      let nowSet = S.fromList now
          lost = [ ml | ml@(m, _) <- old, not (S.member m nowSet), not (isInteractiveModule m) ]   -- (a statement typed at the prompt is not a module of the program)
      if null lost then pure (0, 0, []) else do
        -- each lost module as it is loaded now: its object's time, and the home modules it imports
        infos <- liftIO $ forM lost $ \(m, was) -> do
          mi <- homeObject hsc m
          case mi of
            Just (ln, deps) -> do
              is <- objTimes ln
              pure (m, (ln, if not (null was) && is == was then Just deps else Nothing))
            Nothing -> pure (m, (undefinedLinkable, Nothing))
        let table = M.fromList infos
            -- a module stands if its object is unchanged and every home module it imports stands (one already
            -- in the loader's table, or outside what was lost, is not ours to judge: it stands)
            stands = go (M.keysSet (M.filter (\(_, d) -> d /= Nothing) table))
            go ok = let ok' = S.filter (\m -> case M.lookup m table of
                                                Just (_, Just deps) -> all (\d -> S.member d ok || not (M.member d table)) deps
                                                _ -> False) ok
                    in if S.size ok' == S.size ok then ok else go ok'
            back = [ (m, l) | (m, (l, _)) <- infos, S.member m stands ]
        liftIO $ modifyMVar_ v $ \st -> pure (fmap (\pls -> pls { objs_loaded = foldl (\env (m, l) -> extendModuleEnv env m l) (objs_loaded pls) back }) st)
        pure (length back, length lost - length back, [ GHC.moduleNameString (GHC.moduleName m) | (m, _) <- infos, not (S.member m stands) ])

undefinedLinkable :: Linkable
undefinedLinkable = error "a module that is not loaded has no linkable"      -- (never put back: its entry says so)

failed :: String -> Json
failed why = JObj [ ("error", JStr why) ]

-- | A reply: the facts as JSON, then everything written to standard output and error since the last one.
reply :: Engine -> Json -> IO ()
reply e facts = do
  hFlush stdout
  hFlush stderr
  out <- modifyMVar (eOut e) (\acc -> do { more <- drainNow e; pure ([], B.concat (reverse (more ++ acc))) })
  let j = encodeBS facts
  sendFrame (eControl e) (frameLen (B.length j) <> j <> out)

-- queries ----------------------------------------------------------------------------------------

query :: Engine -> Json -> GHCi Json
query e q = case fromMaybe "" (lookupStr "q" q) of
  "state" -> state
  -- the watched files that differ from what is loaded: the next load need not scan every module to
  -- find them ("GhsFastLoad")
  "changed" -> liftIO (setChanged (Just [ f | JStr f <- lookupArr "files" q ])) >> pure (JObj [])
  -- units added to the running session (GhsAddUnits); the daemon reloads after it
  "add_units" -> do
    r <- addUnits [ f | JStr f <- lookupArr "files" q ]
    pure (case r of
      Left why -> failed why
      Right us -> JObj [("ok", JBool True), ("units", JArr (map JStr us))])
  -- modules added to a unit that is running, each as a target of the unit whose import paths hold its file
  -- (GhsAddUnits.addTargets; GHCi's :add would put it in the interactive unit); the daemon reloads after it
  "add_targets" -> do
    r <- addTargets [ f | JStr f <- lookupArr "files" q ]
    pure (case r of
      Left why -> failed why
      Right ts -> JObj [("ok", JBool True), ("targets", JArr [ JObj [("file", JStr f), ("unit", JStr u)] | (f, u) <- ts ])])
  "typecheck" -> typecheck (arg "dir") (if lookupBool "since" q == Just True then Just [ f | JStr f <- lookupArr "files" q ] else Nothing)
  "typecheck_expr" -> do
    r <- MC.try (GHC.exprType TM_Inst (ioUnit (arg "expr")))
    pure $ case r of
      Left (x :: SomeException) -> JObj [ ("ok", JBool False), ("error", JStr (show x)) ]
      Right _ -> JObj [ ("ok", JBool True) ]
  "fork" -> do
    hv <- GHC.compileExpr (ioUnit (arg "action"))
    let act = unsafeCoerce hv :: IO ()
        spec = ZygoteSpec { zsName = arg "label", zsLogFile = arg "log"
                          , zsEnv = [ (k, v) | JArr [JStr k, JStr v] <- lookupArr "env" q ]
                          , zsDetach = fromMaybe False (lookupBool "detach" q)
                          , zsCloseFds = [eControlFd e, eStdin e, eCapture e] }
    liftIO $ do
      child <- zygoteFork spec act
      -- GHCi waits on no child, and a child killed from outside would stay a zombie -- still "alive" to
      -- whoever asks -- for the life of the session: so someone waits on each
      _ <- forkIO (void (try (getProcessStatus True False (zcPid child)) :: IO (Either SomeException (Maybe ProcessStatus))))
      pure (JObj [ ("pid", JNum (fromIntegral (zcPid child))) ])
  "prune" -> liftIO $ do
    -- The pruner leaves a superseded CAF whose value is still in the young generation (unlinking it there is
    -- the crash in hygiene/repro) -- and the CAF evaluated a moment ago, by the check that just ran, is
    -- exactly that one: it stayed until the NEXT reload's pass, one generation of every such value late.
    -- Two minor collections age everything live into the old generation first; they cost what is young.
    performMinorGC >> performMinorGC
    k <- fromIntegral <$> c_prune :: IO Int
    -- at once (see the README on why not later). `prune_gc: "copying"` makes this one collection a copying
    -- one whatever the RTS flags say: the compacting collection a session's -c gives is single-threaded and
    -- most of this command (0.42 s of a 250 MB heap against 0.06 s) -- but the RTS then holds the copy's
    -- space as far as macOS's footprint goes (measured: ~+200 MB on a 370 MB heap). The daemon's default.
    -- (twice: the first copies into fresh space, and it is the NEXT major collection that hands the old
    -- space back -- with a return-decay under 1, see the daemon's rts_flags; 0.07 s each against 0.46)
    when (k > 0) (if lookupStr "gc" q == Just "copying" then majorGC False >> majorGC False else performMajorGC)
    on <- getRTSStatsEnabled
    live <- if on then Just . gcdetails_live_bytes . gc <$> getRTSStats else pure Nothing
    pure (JObj ([ ("unlinked", JNum (fromIntegral k)) ] ++ [ ("live_mb", JNum (fromIntegral (l `div` 1000000))) | Just l <- [live] ]))
  -- what the heap holds, and what an action costs: "GHC.Hygiene.Census", run HERE -- so a session has them
  -- whether or not its project depends on that library. What they print is the answer.
  "census" -> do
    let top = maybe 15 round (lookupNum "top" q) :: Int
        cap = 1000000000
    case arg "mode" of
      "strings" -> liftIO (cafStrings top cap)
      "kept" -> liftIO (keptReport cap)
      "kept-strings" -> liftIO (keptStrings top cap)
      "mem" -> liftIO memNow
      -- sharing that is missed: closures structurally equal to one already in the heap
      "dups" -> liftIO (dupsCafs top)
      "dups-kept" -> liftIO (dupsKept top)
      "dups-value" -> do
        hv <- GHC.compileExpr ("(" ++ arg "expr" ++ ")")
        liftIO (dupsOf (arg "expr") (unsafeCoerce hv :: Any) top)
      -- the named slots that outlive a reload ("GHC.Hygiene.Store"): which there are, or forget one
      "store" -> liftIO (storeNames >>= \ns -> if null ns then putStrLn "no slots" else mapM_ putStrLn ns)
      "store-drop" -> liftIO (storeDrop (arg "expr") >>= \ok -> putStrLn (if ok then "dropped " ++ arg "expr" ++ ": its owner starts again when it is next linked" else "no slot " ++ arg "expr"))
      "value" -> do
        hv <- GHC.compileExpr ("(" ++ arg "expr" ++ ")")
        liftIO (censusOf (arg "expr") (unsafeCoerce hv :: Any))
      _ -> liftIO (cafReport top cap)
    pure (JObj [])
  "bench" -> do
    -- (typed: an action of any monad -- `pure ()` -- compiles to a function of its dictionary, and running
    -- that as an IO action took the process down)
    hv <- GHC.compileExpr ("((" ++ arg "expr" ++ ") Prelude.>> Prelude.return ()) :: Prelude.IO ()")
    -- (--live: with a collection before and after, for the live heap's change; 0.2 s each)
    liftIO ((if lookupBool "live" q == Just True then benchOf else benchQuick) (arg "expr") (unsafeCoerce hv :: IO ()))
    pure (JObj [])
  "heap_auto" -> liftIO (heapAuto (if lookupBool "on" q == Just True then 1 else 0)) >> pure (JObj [])
  "capabilities" -> liftIO (setNumCapabilities (max 1 (round (fromMaybe 1 (lookupNum "n" q)))) >> pure (JObj []))
  other -> pure (failed ("unknown query " ++ show other))
  where
    arg k = fromMaybe "" (lookupStr k q)
    ioUnit x = "(" ++ x ++ ") :: Prelude.IO ()"

-- | Do the sources, AS THEY ARE ON DISK NOW, typecheck? Without generating code, and without touching what is
-- loaded: the question a reload answers only after it has compiled everything the edit reaches.
--
-- It is a `load` of the same targets in a copy of the session whose every unit generates no code and keeps
-- its interface files in a directory of its own (@dir@, per unit) -- so the next time, only what changed since
-- the last typecheck is typechecked again. Then the session that was there is put back, and with it what was
-- linked (a load forgets that: see 'keepLinked').
--
-- The copy's module graph is kept from one typecheck to the next ('tcGraph'), and @since@ -- the files that
-- changed since the last one, when the daemon can say -- makes this a load WITHOUT the scan of every module
-- ('GhsFastLoad', as for a reload): the scan was half of a typecheck after a one-file save.
typecheck :: FilePath -> Maybe [FilePath] -> GHCi Json
typecheck dir since = do
  saved <- GHC.getSession
  rememberLinked
  liftIO (writeIORef diagnostics ([], 0, 0))
  let quiet uid df = (df { backend = noBackend, ghcLink = NoLink, hiDir = Just (dir </> unitIdString uid) }) `gopt_set` Opt_WriteInterface
      -- (with NO module graph: a module whose source has not changed would otherwise keep the summary it has,
      -- and with it the flags it was summarised under -- and if it then needed compiling, it was compiled for
      -- real, into the session's own object directory)
      noCode g hsc = withModuleGraph g (mapUnitFlags quiet hsc)
  kept <- liftIO (readIORef tcGraph)
  -- (the files that changed are only meaningful against the graph of the typecheck before this one)
  liftIO (setChanged (case (kept, since) of { (Just _, Just fs) -> Just fs; _ -> Nothing }))
  r <- MC.try (GHC.setSession (noCode (fromMaybe emptyMG kept) saved) >> loadAll)
  -- its graph is the next one's start -- unless it threw (a source that does not parse: the graph is partial)
  after <- hsc_mod_graph <$> GHC.getSession
  liftIO (setChanged Nothing)
  liftIO (writeIORef tcGraph (case r of { Right _ | not (null (mgModSummaries after)) -> Just after; _ -> Nothing }))
  GHC.setSession saved
  _ <- keepLinked
  (ds, errs, warns) <- liftIO (atomicModifyIORef' diagnostics (\d -> (([], 0, 0), d)))
  let (ok, thrown) = case r of
        Right Succeeded -> (errs == 0, [])
        Right Failed -> (False, [])
        Left (x :: SomeException) -> (False, [ ("thrown", JStr (show x)) ])
  pure (JObj ([ ("ok", JBool ok), ("errors", JNum (fromIntegral errs)), ("warnings", JNum (fromIntegral warns)), ("diagnostics", JArr (reverse ds)) ] ++ thrown))

{-# NOINLINE tcGraph #-}
tcGraph :: IORef (Maybe ModuleGraph)
tcGraph = unsafePerformIO (newIORef Nothing)

-- | What is loaded: the directory, whether every module of the graph is, and each unit's objects -- which is
-- what a server forked now would run.
state :: GHCi Json
state = do
  hsc <- GHC.getSession
  cwd <- liftIO getCurrentDirectory
  let sums = mgModSummaries (hsc_mod_graph hsc)
  loaded <- filterM (\x -> GHC.isLoadedModule (ms_unitid x) (GHC.ms_mod_name x)) sums
  let units =
        [ JObj [ ("id", JStr (unitIdString uid)), ("package", JStr (fromMaybe "" (thisPackageName df)))
               , ("deps", JArr (map (JStr . unitIdString) deps))
               , ("objects", JArr [ JStr (if isAbsolute o then o else wd </> o)
                                  | s <- sums, ms_unitid s == uid, let o = ml_obj_file (ms_location s) ]) ]
        | (uid, df, deps) <- homeUnits hsc
        , let wd = maybe cwd (\d -> if isAbsolute d then d else cwd </> d) (workingDirectory df)
        , not ("interactive" `isPrefixOf` unitIdString uid) ]
  pure (JObj [ ("cwd", JStr cwd), ("modules", JNum (fromIntegral (length sums))), ("loaded", JNum (fromIntegral (length loaded)))
             , ("units", JArr units) ])

-- the pipes ---------------------------------------------------------------------------------------

-- | Everything in the pipe right now, newest first (the read end does not block).
drainNow :: Engine -> IO [B.ByteString]
drainNow e = go []
  where go acc = do
          r <- try (PB.fdRead (eCapture e) 65536) :: IO (Either IOException B.ByteString)
          case r of
            Right b | not (B.null b) -> go (b : acc)
            _ -> pure acc

drainLoop :: Engine -> IO ()
drainLoop e = do
  threadWaitRead (eCapture e)
  modifyMVar_ (eOut e) (\acc -> (++ acc) <$> drainNow e)
  drainLoop e

frameLen :: Int -> B.ByteString
frameLen n = B.pack [ fromIntegral ((n `shiftR` s) .&. 255) | s <- [24, 16, 8, 0] ]

sendFrame :: Handle -> B.ByteString -> IO ()
sendFrame h b = B.hPut h (frameLen (B.length b)) >> B.hPut h b >> hFlush h

recvFrame :: Handle -> IO (Maybe B.ByteString)
recvFrame h = do
  r <- try (B.hGet h 4) :: IO (Either IOException B.ByteString)
  case r of
    Right hd | B.length hd == 4 -> do
      let n = foldl (\a w -> (a `shiftL` 8) .|. fromIntegral w) (0 :: Int) (B.unpack hd)
      b <- B.hGet h n
      pure (if B.length b == n then Just b else Nothing)
    _ -> pure Nothing

writeAll :: Fd -> B.ByteString -> IO ()
writeAll fd b = unless (B.null b) $ do
  n <- PB.fdWrite fd b
  writeAll fd (B.drop (fromIntegral n) b)
