{-# OPTIONS_GHC -O2 #-}  -- decoding a reply: -O2 is a third faster here (0.47 -> 0.32 ms for 0.5 MB) and nowhere else
-- | The session's GHCi: the engine (@engine/GhsEngine.hs@), started by us and met on a socket.
--
-- A request is a length and a payload -- @C@ and a GHCi command, or @Q@ and a query as JSON -- and a reply is
-- a length, the engine's facts as JSON, and everything written while it ran. Nothing to recognise in the
-- output, no terminal.
--
-- The engine is started in two steps. The build tool is run once with the engine as its repl program and
-- @GHS_CAPTURE@ set: the engine writes down how it was started and exits, and the build tool with it
-- ('captureLaunch'). Then we start the engine ourselves with those arguments ('startRepl'). So the process
-- under the daemon IS GHCi -- its exit status is GHCi's, stopping it is closing its socket -- and a restart
-- that changes nothing the build tool decides can skip the build tool.
module GhciSession.Repl
  ( Repl, ReplError (..), Launch (..), Reply (..)
  , captureLaunch, readLaunch, startRepl, stopRepl, replBusy, replRun, replCommand, replQuery, replQueryOut, replAlive, replPid
  , decode
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, isEmptyMVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (Exception, IOException, SomeException, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.Directory (removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (BufferMode (..), Handle, IOMode (..), hClose, hFlush, hSetBinaryMode, hSetBuffering, openFile)
import System.Posix.IO (fdToHandle)
import System.Posix.Signals (nullSignal, sigINT, sigKILL, sigTERM, signalProcess, signalProcessGroup)
import System.Posix.Types (CPid (..))
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, getProcessExitCode, proc, shell, waitForProcess)
import System.Timeout (timeout)
import GHC.Clock (getMonotonicTime)

import GhciSession.Json
import GhciSession.Sys (socketPair, socketShutdown)

-- | 'ReplTimeout': the seconds, whether the interrupt stopped the command, and what it had printed.
data ReplError = ReplDied String | ReplTimeout Double Bool T.Text
instance Show ReplError where
  show (ReplDied s) = if null s then "repl is not running" else s
  show (ReplTimeout t stopped _) = "timed out after " ++ show (round t :: Int) ++ "s"
    ++ (if stopped then "" else " -- and it did not stop when interrupted: the session runs it to its end before it answers anything else (a loop that does not allocate cannot be interrupted); `restart` ends it")
instance Exception ReplError

-- | How to start the engine: what the build tool would have run.
data Launch = Launch { lCwd :: FilePath, lArgs :: [String], lEnv :: [(String, String)] }

-- | What a request came back with: the engine's facts, and what was written.
data Reply = Reply { rFacts :: Json, rOut :: T.Text }

data Repl = Repl
  { rProc :: ProcessHandle
  , rSend :: B.ByteString -> IO ()
  , rHangUp :: IO ()                 -- ^ the engine exits when its socket closes
  , rReplies :: TQueue B.ByteString
  , rDead :: TVar Bool
  , rIO :: MVar ()                   -- ^ serialises whole round-trips
  , rEvalTimeout :: Double
  , rPgid :: IORef (Maybe CPid)
  , rOwed :: IORef Int               -- ^ replies still to come for commands that timed out and did not stop: discarded as they arrive
  }

-- | A reply's output as text: UTF-8, leniently.
decode :: B.ByteString -> T.Text
decode = TE.decodeUtf8With TE.lenientDecode

-- | Run the build tool's repl command with the engine in its capture mode: it builds what the repl needs,
-- starts "the repl", which writes its own start down in @dir@ and exits. The command's output goes to @out@.
-- 'Left': it failed, with that output.
captureLaunch :: String -> FilePath -> [(String, String)] -> FilePath -> FilePath -> Double -> (String -> IO ()) -> IO (Either String Launch)
captureLaunch cmd cwd' extraEnv dir out secs logF = do
  void (try (removeFile (dir </> "args")) :: IO (Either IOException ()))
  base <- getEnvironment
  let mine = extraEnv ++ [("GHS_CAPTURE", dir)]
      env' = [ kv | kv@(k, _) <- base, k `notElem` map fst mine ] ++ mine
  oh <- openFile out WriteMode
  devnull <- openFile "/dev/null" ReadMode
  logF ("build: " ++ cmd)
  (_, _, _, ph) <- createProcess (shell cmd)
    { cwd = Just cwd', env = Just env', std_in = UseHandle devnull, std_out = UseHandle oh, std_err = UseHandle oh
    , close_fds = True, new_session = True }
  done <- timeout (round (min secs 2.0e9 * 1e6)) (waitForProcess ph)
  said <- either (\(_ :: IOException) -> "") id <$> try (readFile out >>= \x -> length x `seq` pure x)
  case done of
    Nothing -> do
      pg <- getPid ph
      mapM_ (\p -> try (signalProcessGroup sigKILL (CPid (fromIntegral p))) :: IO (Either IOException ())) pg
      pure (Left (said ++ "\n[session] the build timed out after " ++ show (round secs :: Int) ++ "s"))
    Just code -> do
      l <- readLaunch dir
      pure $ case l of
        Just x | code == ExitSuccess -> Right x
        _ -> Left (said ++ (if code == ExitSuccess then "\n[session] the repl command did not start the engine: it must run {engine} as its GHCi" else ""))

-- | A start written down earlier, if there is one.
readLaunch :: FilePath -> IO (Maybe Launch)
readLaunch dir = do
  a <- try (B.readFile (dir </> "args")) :: IO (Either IOException B.ByteString)
  e <- try (B.readFile (dir </> "env")) :: IO (Either IOException B.ByteString)
  pure $ case (a, e) of
    (Right ab, Right eb) -> case map (T.unpack . decode) (B.split 0 ab) of
      (c : as) | not (null c) -> Just (Launch c as [ (k, drop 1 v) | kv <- B.split 0 eb, let (k, v) = break (== '=') (T.unpack (decode kv)), not (null k) ])
      _ -> Nothing
    _ -> Nothing

-- | Start the engine and wait for its first reply, which is the load.
startRepl :: FilePath -> [String] -> Launch -> [(String, String)] -> FilePath -> Double -> Double -> (String -> IO ()) -> IO (Repl, Reply)
startRepl exe pre l extraEnv out loadTimeout evalTimeout logF = do
  (ours, theirs) <- socketPair >>= maybe (throwIO (ReplDied "cannot make a socket pair")) pure
  let mine = extraEnv ++ [("GHS_CONTROL", "stdin")]
      env' = [ kv | kv@(k, _) <- lEnv l, k `notElem` map fst mine ] ++ mine
  oh <- openFile out AppendMode       -- anything the engine says before it has taken its standard output over
  th <- fdToHandle theirs
  logF ("start: " ++ exe ++ " (" ++ show (length (lArgs l)) ++ " arguments from the build) in " ++ lCwd l)
  (_, _, _, ph) <- createProcess (proc exe (pre ++ lArgs l))
    { cwd = Just (lCwd l), env = Just env', std_in = UseHandle th, std_out = UseHandle oh, std_err = UseHandle oh
    , close_fds = True, new_session = True }
  void (try (hClose th) :: IO (Either IOException ()))
  h <- fdToHandle ours
  hSetBinaryMode h True
  hSetBuffering h (BlockBuffering Nothing)
  replies <- newTQueueIO
  dead <- newTVarIO False
  io <- newMVar ()
  pg <- getPid ph >>= newIORef . fmap (CPid . fromIntegral)
  owed <- newIORef 0
  let r = Repl ph (\b -> B.hPut h (frameLen (B.length b)) >> B.hPut h b >> hFlush h) (socketShutdown ours) replies dead io evalTimeout pg owed
  _ <- forkIO (recvLoop h replies dead)
  _ <- forkIO (waitForProcess ph >> atomically (writeTVar dead True))
  load <- await r out loadTimeout
  pure (r, load)

recvLoop :: Handle -> TQueue B.ByteString -> TVar Bool -> IO ()
recvLoop h replies dead = do
  hd <- try (B.hGet h 4) :: IO (Either SomeException B.ByteString)
  case hd of
    Right x | B.length x == 4 -> do
      b <- B.hGet h (be32 x)
      atomically (writeTQueue replies b)
      recvLoop h replies dead
    _ -> atomically (writeTVar dead True)

be32 :: B.ByteString -> Int
be32 = B.foldl' (\a w -> a * 256 + fromIntegral w) 0 . B.take 4

frameLen :: Int -> B.ByteString
frameLen n = B.pack [ fromIntegral (n `div` 16777216), fromIntegral (n `div` 65536 `mod` 256), fromIntegral (n `div` 256 `mod` 256), fromIntegral (n `mod` 256) ]

-- | The next reply. If the engine dies first, the error carries how it ended and the tail of what it said
-- outside the protocol (@out@: its standard error before it took that over, a crash report).
await :: Repl -> FilePath -> Double -> IO Reply
await r out secs = do
  tv <- registerDelay (round (min secs 2.0e9 * 1e6))
  res <- atomically $ do
    m <- tryReadTQueue (rReplies r)
    case m of
      Just b -> pure (Right b)
      Nothing -> do
        d <- readTVar (rDead r)
        late <- readTVar tv
        if d then pure (Left Nothing) else if late then pure (Left (Just secs)) else retry
  case res of
    Right b -> do
      let n = be32 b
          (j, o) = B.splitAt n (B.drop 4 b)
      pure (Reply (either (const (JObj [])) id (parseJsonBS j)) (decode o))
    Left (Just t) -> throwIO (ReplTimeout t True T.empty)
    Left Nothing -> do
      code <- timeout 2000000 (waitForProcess (rProc r))
      said <- if null out then pure "" else either (\(_ :: IOException) -> "") id <$> try (readFile out >>= \x -> length x `seq` pure x)
      let how = case code of
            Just (ExitFailure n) | n < 0 -> "the repl was killed by signal " ++ show (negate n)
            Just (ExitFailure n) -> "the repl exited with status " ++ show n
            Just ExitSuccess -> "the repl exited"
            Nothing -> "the repl closed its socket"
      throwIO (ReplDied (unlines (how : lastN 12 (lines said))))
  where lastN n xs = drop (length xs - n) xs

-- | Is a request running right now? (Whoever asks must not then queue behind it: a stop, say.)
replBusy :: Repl -> IO Bool
replBusy r = isEmptyMVar (rIO r)

replAlive :: Repl -> IO Bool
replAlive r = (== Nothing) <$> getProcessExitCode (rProc r)

replPid :: Repl -> IO (Maybe Int)
replPid r = fmap fromIntegral <$> getPid (rProc r)

roundTrip :: Repl -> Maybe Double -> B.ByteString -> IO Reply
roundTrip r mt payload = withMVar (rIO r) $ \_ -> do
  alive <- replAlive r
  unless alive (throwIO (ReplDied ""))
  rSend r payload
  res <- try (own (fromMaybe (rEvalTimeout r) mt))
  case res of
    Right rep -> pure rep
    -- A command that runs past its time is INTERRUPTED, not abandoned: GHCi turns a SIGINT into
    -- `UserInterrupt` in the running command, prints "Interrupted." and is back at its prompt, where the
    -- turn replies -- so the next request does not queue behind the one that hung (a check of 30 s had to
    -- be killed; an agent's probe would hang its session). The caller still gets the timeout. A command
    -- that cannot be interrupted (a foreign call that does not return) leaves its reply for 'drain'.
    Left (ReplTimeout t _ _) -> do
      mp <- getPid (rProc r)
      forM_ mp $ \p -> try (signalProcess sigINT (CPid (fromIntegral p))) :: IO (Either IOException ())
      -- (the interrupted command's output comes with the reply the interrupt frees: it says where it hung)
      said <- try (own 5) :: IO (Either ReplError Reply)
      case said of
        Right rep -> throwIO (ReplTimeout t True (rOut rep))
        -- it did not stop: its reply comes later, and must not be taken for the next request's (it was,
        -- and every answer after it belonged to the request before: an agent took the session for stuck)
        Left _ -> modifyIORef' (rOwed r) (+ 1) >> throwIO (ReplTimeout t False T.empty)
    Left e -> throwIO e
  where
    -- this request's reply: the engine answers in order, so the replies still owed to commands that timed
    -- out come first, and are discarded; the time allowed covers them too
    own secs = do
      t0 <- getMonotonicTime
      let go = do
            el <- subtract t0 <$> getMonotonicTime
            rep <- await r "" (max 0.05 (secs - el))
            owed <- readIORef (rOwed r)
            if owed > 0 then writeIORef (rOwed r) (owed - 1) >> go else pure rep
      go

-- | Run one GHCi command: its output, and the diagnostics the compiler logged while it ran. 'Nothing' for the
-- session's default timeout. Throws 'ReplError'.
replRun :: Repl -> Maybe Double -> String -> IO Reply
replRun r mt expr = do
  let e = trim expr
      stripped = stripBlock e
      payload = if '\n' `elem` stripped then ":{\n" ++ stripped ++ "\n:}" else stripped
  Reply f out <- roundTrip r mt (BC.cons 'C' (TE.encodeUtf8 (T.pack payload)))
  pure (Reply f (T.dropWhileEnd (== '\n') (T.dropWhile (== '\n') out)))
  where
    trim = dropWhileEnd' (`elem` " \n\r\t") . dropWhile (`elem` " \n\r\t")
    dropWhileEnd' p = reverse . dropWhile p . reverse
    stripBlock s =
      let ls = lines s
          isL0 = case ls of (l:_) -> trim l == ":{"; _ -> False
          isLn = case reverse ls of (l:_) -> trim l == ":}"; _ -> False
      in if isL0 && isLn && length ls >= 2
           then unlines (init (tail ls))
           else s

replCommand :: Repl -> Maybe Double -> String -> IO T.Text
replCommand r mt expr = rOut <$> replRun r mt expr

-- | Ask the engine something (@q@ and its arguments): the answer, which has @error@ when it could not.
replQuery :: Repl -> Maybe Double -> String -> [(String, Json)] -> IO Json
replQuery r mt q args = rFacts <$> replQueryOut r mt q args

-- | ... with whatever it wrote while answering (a diagnostic the C half prints, say).
replQueryOut :: Repl -> Maybe Double -> String -> [(String, Json)] -> IO Reply
replQueryOut r mt q args = roundTrip r mt (BC.cons 'Q' (encodeBS (JObj (("q", JStr q) : args))))

-- | Stop the engine AND everything it started.
--
-- It leaves as soon as its socket closes. Anything the loaded code started with it is in its process group,
-- so the group is signalled and then checked to be empty. (A server forked for a composed session has its
-- own group: those are stopped by whoever owns them.)
stopRepl :: Repl -> (String -> IO ()) -> IO ()
stopRepl r logF = do
  pg <- readIORef (rPgid r)
  rHangUp r
  alive0 <- replAlive r
  when alive0 (void (timeout 300000 (waitForProcess (rProc r))))
  alive <- replAlive r
  when alive $ do
    mapM_ (\g -> try (signalProcessGroup sigTERM g) :: IO (Either IOException ())) pg
    done <- timeout 10000000 (waitForProcess (rProc r))
    when (done == Nothing) (mapM_ (\g -> void (try (signalProcessGroup sigKILL g) :: IO (Either IOException ()))) pg)
  mapM_ reap pg
  writeIORef (rPgid r) Nothing
  where
    gone g = either (\e -> const True (e :: IOException)) (const False) <$> try (signalProcessGroup nullSignal g)
    waitGone g n = do
      e <- gone g
      if e || n <= (0 :: Int) then pure e else threadDelay 10000 >> waitGone g (n - 1)
    reap g = do
      ok <- waitGone g 300
      unless ok $ do
        void (try (signalProcessGroup sigKILL g) :: IO (Either IOException ()))
        ok' <- waitGone g 500
        unless ok' (logF ("WARNING: process group " ++ show g ++ " still alive after SIGKILL"))
