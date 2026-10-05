-- | A GHCi behind a pty, framed by a sentinel prompt.
--
-- Every command written yields exactly one sentinel, so reading until the sentinel is a complete reply. The
-- reader runs on its own thread so a command that prints megabytes cannot deadlock on a full pty buffer.
module GhciSession.Repl
  ( Repl, ReplError (..)
  , startRepl, stopRepl, replCommand, replAlive, replPid, postLoadBasics, stripAnsi
  , decode, sentinel
  , frameChunks
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
  , rReplies :: TQueue B.ByteString   -- ^ complete replies: everything up to a sentinel
  , rPending :: TVar [B.ByteString]   -- ^ what has arrived since the last sentinel, newest chunk first
  , rDead :: TVar Bool
  , rIO :: MVar ()              -- ^ serialises whole command round-trips
  , rEvalTimeout :: Double
  , rOnAsync :: T.Text -> IO ()
  , rPgid :: IORef (Maybe CPid)
  }

-- | Drop terminal colour sequences (@ESC [ ... letter@). Nearly every reply has none: that case is one scan.
stripAnsi :: T.Text -> T.Text
stripAnsi t
  | not (T.any (== '\ESC') t) = t
  | otherwise = case T.breakOn (T.pack "\ESC[") t of
      (a, b) | T.null b -> a
             | otherwise -> a <> stripAnsi (T.drop 1 (T.dropWhile (\c -> c == ';' || (c >= '0' && c <= '9')) (T.drop 2 b)))

-- | A reply as text: UTF-8 (leniently), without the CRs a pty adds to every line end, without colours.
decode :: B.ByteString -> T.Text
decode = stripAnsi . T.filter (/= '\r') . TE.decodeUtf8With TE.lenientDecode

-- | Spawn the command, handshake, run the post-load action; returns the repl and the load log.
startRepl :: String -> FilePath -> [(String, String)] -> Double -> Double -> (String -> IO ()) -> (T.Text -> IO ())
          -> (Repl -> IO ()) -> IO (Repl, T.Text)
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
  replies <- newTQueueIO
  pending <- newTVarIO []
  dead <- newTVarIO False
  io <- newMVar ()
  pg <- getPid ph >>= newIORef . fmap (CPid . fromIntegral)
  let r = Repl ph master replies pending dead io evalTimeout onAsync pg
  _ <- forkIO (reader r)
  -- Written before GHCi is listening; the pty buffers it. Exactly ONE command, so exactly one sentinel is
  -- produced by the handshake (two would desynchronise every later reply by one command). Everything up to
  -- the FIRST sentinel is the load log.
  void (fdWrite master (BC.pack ":set prompt \"\\nGHS_READY\\n\"\n"))
  load <- awaitSentinel r loadTimeout
  postLoad r
  pure (r, load)

-- | The reader does the framing. A reply arrives in chunks, and looking for the sentinel in the whole buffer
-- each time one arrives (and re-copying the buffer to append it) is quadratic in the reply: for a 0.5 MB load
-- log that was 6 ms and 2.7 MB of copying, and it grows with the square. So each chunk is searched once,
-- with the last few bytes of the one before it (a sentinel may straddle two), and the chunks are joined only
-- when a reply is complete.
reader :: Repl -> IO ()
reader r = loop B.empty
  where
    keep = B.length sentinel - 1
    loop carry = do
      got <- try (fdRead (rMaster r) 65536) :: IO (Either SomeException B.ByteString)
      case got of
        Right chunk | not (B.null chunk) -> frame carry chunk >>= loop
        _ -> atomically (writeTVar (rDead r) True)
    -- returns the carry for the next chunk
    frame carry chunk
      | B.null (snd (B.breakSubstring sentinel (carry <> chunk))) = do
          atomically (modifyTVar' (rPending r) (chunk :))
          pure (B.takeEnd keep (carry <> chunk))
      | otherwise = do
          old <- atomically (swapTVar (rPending r) [])
          let (pre, post) = B.breakSubstring sentinel (B.concat (reverse (chunk : old)))
              rest = BC.dropWhile (\c -> c == '\r' || c == '\n') (B.drop (B.length sentinel) post)
          -- the line end after a sentinel may arrive in a later chunk: drop it from the front of the reply it
          -- would otherwise begin
          atomically (writeTQueue (rReplies r) (BC.dropWhile (\c -> c == '\r' || c == '\n') pre))
          if B.null rest then pure B.empty else frame B.empty rest     -- (two sentinels in one chunk: not expected, but handled)

awaitSentinel :: Repl -> Double -> IO T.Text
awaitSentinel r secs = do
  tv <- registerDelay (round (min secs 2.0e9 * 1e6))
  res <- atomically $ do
    m <- tryReadTQueue (rReplies r)
    case m of
      Just pre -> pure (Right pre)
      Nothing -> do
        d <- readTVar (rDead r)
        if d then do
            left <- swapTVar (rPending r) []
            pure (Left (ReplDied (T.unpack (decode (B.concat (reverse left))))))
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
replCommand :: Repl -> Maybe Double -> String -> IO T.Text
replCommand r mt expr = withMVar (rIO r) $ \_ -> do
  alive <- replAlive r
  unless alive (throwIO (ReplDied ""))
  -- Anything already buffered was written by a BACKGROUND thread (a server forked in the session, say)
  -- between commands: park it rather than letting it masquerade as this command's answer.
  stray <- B.concat . reverse <$> atomically (swapTVar (rPending r) [])
  unless (BC.all (`elem` " \r\n\t") stray) (rOnAsync r (decode stray))
  let e = trim expr
      payload = if '\n' `elem` e then ":{\n" ++ e ++ "\n:}\n" else e ++ "\n"
  writeAll (rMaster r) (TE.encodeUtf8 (T.pack payload))
  out <- awaitSentinel r (maybe (rEvalTimeout r) id mt)
  pure (T.dropWhileEnd (== '\n') (T.dropWhile (== '\n') out))
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

-- | The framing rule on its own (what 'reader' does, for the benchmark and the tests): feed it chunks, get the
-- complete replies and whatever is still pending.
frameChunks :: [B.ByteString] -> ([B.ByteString], B.ByteString)
frameChunks = go [] [] B.empty
  where
    keep = B.length sentinel - 1
    go done pend _ [] = (reverse done, B.concat (reverse pend))
    go done pend carry (chunk : cs)
      | B.null (snd (B.breakSubstring sentinel (carry <> chunk))) = go done (chunk : pend) (B.takeEnd keep (carry <> chunk)) cs
      | otherwise =
          let (pre, post) = B.breakSubstring sentinel (B.concat (reverse (chunk : pend)))
              rest = BC.dropWhile (\c -> c == '\r' || c == '\n') (B.drop (B.length sentinel) post)
          in go (BC.dropWhile (\c -> c == '\r' || c == '\n') pre : done) [] B.empty ([ rest | not (B.null rest) ] ++ cs)
