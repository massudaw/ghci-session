-- | A GHCi behind a pty, framed by a sentinel prompt.
--
-- Every command written yields exactly one sentinel, so reading until the sentinel is a complete reply. The
-- reader runs on its own thread so a command that prints megabytes cannot deadlock on a full pty buffer.
module GhciSession.Repl
  ( Repl, ReplError (..)
  , startRepl, stopRepl, replCommand, replAlive, replPid, postLoadBasics, stripAnsi
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (Exception, IOException, SomeException, throwIO, try)
import Control.Monad (unless, void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.Environment (getEnvironment)
import System.IO (hClose)
import System.Posix.IO (closeFd, dup, fdToHandle)
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Signals (nullSignal, sigKILL, sigTERM, signalProcessGroup)
import System.Posix.Terminal (TerminalMode (EnableEcho), TerminalState (Immediately), getTerminalAttributes, openPseudoTerminal, setTerminalAttributes, withoutMode)
import System.Posix.Types (CPid (..), Fd)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, getProcessExitCode, shell, waitForProcess)
import System.Timeout (timeout)

import GhciSession.Sys (setWinsize)

sentinel :: B.ByteString
sentinel = BC.pack "GHS_READY"

data ReplError = ReplDied String | ReplTimeout Double
instance Show ReplError where
  show (ReplDied s) = if null s then "repl is not running" else s
  show (ReplTimeout t) = "timed out after " ++ show (round t :: Int) ++ "s"
instance Exception ReplError

data Repl = Repl
  { rProc :: ProcessHandle
  , rMaster :: Fd
  , rBuf :: TVar B.ByteString
  , rDead :: TVar Bool
  , rIO :: MVar ()              -- ^ serialises whole command round-trips
  , rEvalTimeout :: Double
  , rOnAsync :: String -> IO ()
  , rPgid :: IORef (Maybe CPid)
  }

stripAnsi :: String -> String
stripAnsi ('\ESC' : '[' : r) = stripAnsi (drop 1 (dropWhile (\c -> c `elem` "0123456789;") r))
stripAnsi (c : r) = c : stripAnsi r
stripAnsi [] = []

decode :: B.ByteString -> String
decode = filter (/= '\r') . stripAnsi . T.unpack . TE.decodeUtf8With TE.lenientDecode   -- a pty ends lines with CR LF

-- | Spawn the command, handshake, run the post-load action; returns the repl and the load log.
startRepl :: String -> FilePath -> [(String, String)] -> Double -> Double -> (String -> IO ()) -> (String -> IO ())
          -> (Repl -> IO ()) -> IO (Repl, String)
startRepl cmd cwd' extraEnv loadTimeout evalTimeout logF onAsync postLoad = do
  (master, slave) <- openPseudoTerminal
  attrs <- getTerminalAttributes slave
  setTerminalAttributes slave (withoutMode attrs EnableEcho) Immediately   -- else every command we write comes back
  setWinsize slave 200 400          -- a wide terminal keeps GHC from hard-wrapping diagnostics at 80 columns
  base <- getEnvironment
  let env' = [ kv | kv@(k, _) <- base, k `notElem` ("TERM" : map fst extraEnv) ] ++ extraEnv ++ [("TERM", "dumb")]
  hs <- mapM (\_ -> dup slave >>= fdToHandle) [1 :: Int, 2, 3]
  logF ("spawn: " ++ cmd)
  (_, _, _, ph) <- createProcess (shell cmd)
    { cwd = Just cwd', env = Just env', std_in = UseHandle (hs !! 0), std_out = UseHandle (hs !! 1)
    , std_err = UseHandle (hs !! 2), close_fds = True, new_session = True }
  mapM_ (\h -> void (try (hClose h) :: IO (Either IOException ()))) hs
  closeFd slave
  buf <- newTVarIO B.empty
  dead <- newTVarIO False
  io <- newMVar ()
  pg <- getPid ph >>= newIORef . fmap (CPid . fromIntegral)
  let r = Repl ph master buf dead io evalTimeout onAsync pg
  _ <- forkIO (reader r)
  -- Written before GHCi is listening; the pty buffers it. Exactly ONE command, so exactly one sentinel is
  -- produced by the handshake (two would desynchronise every later reply by one command). Everything up to
  -- the FIRST sentinel is the load log.
  void (fdWrite master (BC.pack ":set prompt \"\\nGHS_READY\\n\"\n"))
  load <- awaitSentinel r loadTimeout
  postLoad r
  pure (r, load)

reader :: Repl -> IO ()
reader r = loop
  where
    loop = do
      got <- try (fdRead (rMaster r) 65536) :: IO (Either SomeException B.ByteString)
      case got of
        Right chunk | not (B.null chunk) -> atomically (modifyTVar' (rBuf r) (`B.append` chunk)) >> loop
        _ -> atomically (writeTVar (rDead r) True)

awaitSentinel :: Repl -> Double -> IO String
awaitSentinel r secs = do
  tv <- registerDelay (round (min secs 2.0e9 * 1e6))
  res <- atomically $ do
    b <- readTVar (rBuf r)
    let (pre, post) = B.breakSubstring sentinel b
    if not (B.null post)
      then do
        writeTVar (rBuf r) (BC.dropWhile (`elem` "\r\n") (B.drop (B.length sentinel) post))
        pure (Right pre)
      else do
        d <- readTVar (rDead r)
        if d then writeTVar (rBuf r) B.empty >> pure (Left (ReplDied (decode b)))
          else do
            late <- readTVar tv
            if late then pure (Left (ReplTimeout secs)) else retry
  either throwIO (pure . decode) res

replAlive :: Repl -> IO Bool
replAlive r = (== Nothing) <$> getProcessExitCode (rProc r)

replPid :: Repl -> IO (Maybe Int)
replPid r = fmap fromIntegral <$> getPid (rProc r)

-- | Run one GHCi command and return its output (sentinel stripped). 'Nothing' for the session's default
-- timeout. Throws 'ReplError'.
replCommand :: Repl -> Maybe Double -> String -> IO String
replCommand r mt expr = withMVar (rIO r) $ \_ -> do
  alive <- replAlive r
  unless alive (throwIO (ReplDied ""))
  -- Anything already buffered was written by a BACKGROUND thread (a server forked in the session, say)
  -- between commands: park it rather than letting it masquerade as this command's answer.
  stray <- atomically (swapTVar (rBuf r) B.empty)
  unless (BC.all (`elem` " \r\n\t") stray) (rOnAsync r (decode stray))
  let e = trim expr
      payload = if '\n' `elem` e then ":{\n" ++ e ++ "\n:}\n" else e ++ "\n"
  writeAll (rMaster r) (TE.encodeUtf8 (T.pack payload))
  out <- awaitSentinel r (maybe (rEvalTimeout r) id mt)
  pure (dropWhileEnd' (== '\n') (dropWhile (== '\n') out))
  where
    trim = dropWhileEnd' (`elem` " \n\r\t") . dropWhile (`elem` " \n\r\t")
    dropWhileEnd' p = reverse . dropWhile p . reverse

writeAll :: Fd -> B.ByteString -> IO ()
writeAll fd b = unless (B.null b) $ do
  n <- fdWrite fd b
  writeAll fd (B.drop (fromIntegral n) b)

-- | Re-establish what a load resets.
--
-- The buffering line is load-bearing, not hygiene: GHCi puts stdout in NoBuffering, so a forkIO'd thread
-- sharing that handle interleaves with the prompt one CHARACTER at a time and shreds the sentinel -- the reply
-- then never frames and every command times out. LineBuffering keeps the sentinel line intact.
postLoadBasics :: Repl -> IO ()
postLoadBasics r = do
  void (replCommand r (Just 60) ":set prompt-cont \"\"")
  void (replCommand r (Just 60) ":module + System.IO")
  void (replCommand r (Just 60) "hSetBuffering stdout LineBuffering")

-- | Stop the repl AND everything it forked.
--
-- The direct child is cabal; the process that matters is the @ghc --interactive@ it execs, and anything that
-- process forked lives inside it. When cabal exits first, waiting on it returns happily and a stale GHCi is
-- reparented to init, still holding its ports. So signal the process GROUP and then verify the group is empty.
stopRepl :: Repl -> (String -> IO ()) -> IO ()
stopRepl r logF = do
  pg <- readIORef (rPgid r)
  alive <- replAlive r
  when alive $ do
    mapM_ (\g -> try (signalProcessGroup sigTERM g) :: IO (Either IOException ())) pg
    done <- timeout 10000000 (waitForProcess (rProc r))
    when (done == Nothing) (mapM_ (\g -> void (try (signalProcessGroup sigKILL g) :: IO (Either IOException ()))) pg)
  mapM_ reap pg
  writeIORef (rPgid r) Nothing
  void (try (closeFd (rMaster r)) :: IO (Either IOException ()))
  where
    empty g = either (\e -> const True (e :: IOException)) (const False) <$> try (signalProcessGroup nullSignal g)
    waitEmpty g n = do
      e <- empty g
      if e || n <= (0 :: Int) then pure e else threadDelay 100000 >> waitEmpty g (n - 1)
    reap g = do
      gone <- waitEmpty g 30
      unless gone $ do
        void (try (signalProcessGroup sigKILL g) :: IO (Either IOException ()))
        gone' <- waitEmpty g 50
        unless gone' (logF ("WARNING: process group " ++ show g ++ " still alive after SIGKILL"))
