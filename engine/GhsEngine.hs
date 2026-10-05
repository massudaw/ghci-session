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
module GhsEngine (engineInit, engineSettings, engineHook) where

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
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.Types (CInt (..))
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Directory (copyFile, createDirectoryIfMissing, getCurrentDirectory, renameFile)
import System.Environment (getArgs, getEnvironment, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, takeFileName, (</>))
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performMajorGC)
import System.Posix.IO
import qualified System.Posix.IO.ByteString as PB
import System.Posix.Process (ProcessStatus, exitImmediately, getProcessStatus)
import System.Posix.Types (Fd (..))
import Unsafe.Coerce (unsafeCoerce)

import qualified GHC
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Env (hsc_HUG, hsc_mod_graph)
import GHC.Driver.Session (thisPackageName, workingDirectory)
import GHC.Tc.Module (TcRnExprMode (..))
import GHC.Types.Error (MessageClass (..), Severity (..))
import GHC.Types.SrcLoc
import GHC.Unit.Home.Graph (homeUnitEnv_dflags, homeUnitEnv_units, unitEnv_assocs)
import GHC.Unit.Module.Graph (mgModSummaries)
import GHC.Unit.Module.Location (ml_obj_file)
import GHC.Unit.Module.ModSummary (ms_location, ms_mod, ms_unitid)
import GHC.Unit.State (homeUnitDepends)
import GHC.Unit.Types (unitIdString)
import GHC.Utils.Logger (LogAction, log_default_user_context, pushLogHook)
import GHC.Utils.Outputable (empty, ppr, renderWithContext, showSDocUnsafe)
import GHCi.UI (GhciSettings (..))
import GHCi.UI.Monad (GHCi)

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
  case cap of
    Just dir | "--interactive" `elem` args -> capture dir args >> exitImmediately ExitSuccess
    _ -> when (ctl == Just "stdin") takeControl

-- | What @cabal repl@ would have run, as files: @args@ (the directory, then each argument, NUL-separated)
-- and @env@. Cabal deletes the per-unit argument files of a multi-unit repl when the repl exits, so they are
-- copied and the arguments point at the copies.
capture :: FilePath -> [String] -> IO ()
capture dir args = do
  createDirectoryIfMissing True dir
  cwd <- getCurrentDirectory
  args' <- forM (zip [0 :: Int ..] args) $ \(i, a) -> case a of
    '@' : f -> do
      let to = dir </> ("unit-" ++ show i ++ "-" ++ takeFileName f)
      copyFile (if isAbsolute f then f else cwd </> f) to
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
  (ds, errs, warns) <- liftIO (atomicModifyIORef' diagnostics (\d -> (([], 0, 0), d)))
  liftIO (reply e (JObj [ ("errors", JNum (fromIntegral errs)), ("warnings", JNum (fromIntegral warns)), ("diagnostics", JArr (reverse ds)) ]))
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
          Just ('C', cmd) -> liftIO $ do
            -- GHCi leaves standard output unbuffered, which is a system call per CHARACTER written: a megabyte of
            -- output was 4.8 s. A line at a time keeps output and errors in the order they were written, and the
            -- turn flushes whatever is left.
            hSetBuffering stdout LineBuffering
            -- exactly ONE line end: a blank line is a command to GHCi too, and would be answered with a turn of its own
            void (forkIO (writeAll (eStdin e) (BC.dropWhileEnd (== '\n') cmd <> BC.pack "\n")))   -- (a thread: a pipe holds 64 KB)
          _ -> liftIO (reply e (failed "a request is C<command> or Q<json>")) >> wait

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
  "typecheck" -> do
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
    k <- fromIntegral <$> c_prune :: IO Int
    when (k > 0) performMajorGC        -- at once: see the README on why not later
    on <- getRTSStatsEnabled
    live <- if on then Just . gcdetails_live_bytes . gc <$> getRTSStats else pure Nothing
    pure (JObj ([ ("unlinked", JNum (fromIntegral k)) ] ++ [ ("live_mb", JNum (fromIntegral (l `div` 1000000))) | Just l <- [live] ]))
  "capabilities" -> liftIO (setNumCapabilities (max 1 (round (fromMaybe 1 (lookupNum "n" q)))) >> pure (JObj []))
  other -> pure (failed ("unknown query " ++ show other))
  where
    arg k = fromMaybe "" (lookupStr k q)
    ioUnit x = "(" ++ x ++ ") :: Prelude.IO ()"

-- | What is loaded: the directory, whether every module of the graph is, and each unit's objects -- which is
-- what a server forked now would run.
state :: GHCi Json
state = do
  hsc <- GHC.getSession
  cwd <- liftIO getCurrentDirectory
  let sums = mgModSummaries (hsc_mod_graph hsc)
  loaded <- filterM (GHC.isLoadedHomeModule . ms_mod) sums
  let units =
        [ JObj [ ("id", JStr (unitIdString uid)), ("package", JStr (fromMaybe "" (thisPackageName df)))
               , ("deps", JArr (map (JStr . unitIdString) (homeUnitDepends (homeUnitEnv_units ue))))
               , ("objects", JArr [ JStr (if isAbsolute o then o else wd </> o)
                                  | s <- sums, ms_unitid s == uid, let o = ml_obj_file (ms_location s) ]) ]
        | (uid, ue) <- unitEnv_assocs (hsc_HUG hsc)
        , let df = homeUnitEnv_dflags ue
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
