{-# LANGUAGE ForeignFunctionInterface #-}

-- | __GHCi with a socket where its terminal was.__
--
-- This executable is GHCi: the same source as the @ghc@ binary's interactive front end (@vendor/ghc-X.Y.Z@,
-- unchanged), so every command behaves as it does there. What differs is where commands come from and where
-- their output goes. Started with @GHS_CONTROL@ set, it connects to that unix socket and then, for each
-- request the session daemon sends, runs it as one GHCi command and sends back everything the command wrote.
--
-- Driving a stock GHCi means a pseudo-terminal, a prompt chosen so it can be recognised in the output, and
-- finding that prompt in the bytes that come back. Here a request is a length and a string, and a reply is a
-- length and the output: nothing to recognise, nothing an evaluated program can print that looks like the
-- end of its own reply, no terminal settings to re-establish after a load.
--
-- It needs NO change to GHCi's loop. GHCi calls a prompt function before it reads each command; ours is the
-- turn: it sends the reply to the command that just finished, waits for the next request, and feeds it to
-- GHCi's standard input, which is a pipe this process holds the other end of. Standard output and error are
-- a file between turns, read back and emptied at each one.
--
-- Without @GHS_CONTROL@ it is an ordinary GHCi.
module GhsEngine (engineInit, engineSettings) where

import Prelude

import Control.Concurrent (forkIO, threadWaitRead)
import Control.Concurrent.MVar
import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO
import System.Posix.Process (exitImmediately)
import qualified System.Posix.IO.ByteString as PB
import System.Posix.Types (Fd (..))

import GHC.Utils.Outputable (empty)
import GHCi.UI (GhciSettings (..))

foreign import ccall safe "ghs_unix_connect" c_connect :: CString -> IO CInt

data Engine = Engine
  { eControl :: Handle      -- ^ requests in, replies out
  , eStdin :: Fd            -- ^ the write end of the pipe GHCi reads its commands from
  , eCapture :: Fd          -- ^ the read end of the pipe standard output and error are
  , eOut :: MVar [B.ByteString]   -- ^ what has been read from it since the last turn, newest first
  }

{-# NOINLINE engine #-}
engine :: IORef (Maybe Engine)
engine = unsafePerformIO (newIORef Nothing)

-- | Before GHC starts: if a session daemon is waiting (@GHS_CONTROL@), connect to it and take over this
-- process's standard input, output and error.
engineInit :: IO ()
engineInit = do
  mc <- lookupEnv "GHS_CONTROL"
  case mc of
    Nothing -> pure ()
    Just path -> do
      fd <- withCString path c_connect
      when (fd < 0) $ do
        hPutStrLn stderr ("ghci-session engine: cannot connect to " ++ path)
        exitImmediately (ExitFailure 2)
      h <- fdToHandle (Fd fd)
      hSetBinaryMode h True
      hSetBuffering h (BlockBuffering Nothing)
      -- This process may have been started with standard descriptors CLOSED, in which case the pipe or the
      -- file below can itself come back as descriptor 0, 1 or 2 -- and "duplicate onto 0, then close the
      -- original" would close the very descriptor just installed. So: descriptors above 2 first.
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
      let e = Engine h w cr out
      _ <- forkIO (drainLoop e)
      writeIORef engine (Just e)

-- | The same open file on a descriptor that is not one of the standard three.
above2 :: Fd -> IO Fd
above2 fd
  | fd > 2 = pure fd
  | otherwise = do
      fd' <- dup fd >>= above2
      closeFd fd
      pure fd'

-- | GHCi's settings with the turn as its prompt (and no continuation prompt: a multi-line request arrives whole).
engineSettings :: GhciSettings -> GhciSettings
engineSettings s = s
  { defPrompt = \mods line -> do
      me <- liftIO (readIORef engine)
      case me of
        Nothing -> defPrompt s mods line
        Just e -> liftIO (turn e) >> pure empty
  , defPromptCont = \mods line -> do
      me <- liftIO (readIORef engine)
      case me of
        Nothing -> defPromptCont s mods line
        Just _ -> pure empty
  }

-- | One turn: reply to what just ran (the first time, that is the load), then wait for the next request and
-- hand it to GHCi.
turn :: Engine -> IO ()
turn e = do
  hFlush stdout
  hFlush stderr
  out <- modifyMVar (eOut e) (\acc -> do { more <- drainNow e; pure ([], B.concat (reverse (more ++ acc))) })
  sendFrame (eControl e) out
  req <- recvFrame (eControl e)
  case req of
    Nothing -> exitImmediately ExitSuccess        -- the daemon is gone: so are we
    -- exactly ONE line end: a blank line is a command to GHCi too, and would be answered with a turn of its own
    Just cmd -> do
      -- GHCi leaves standard output unbuffered, which is a system call per CHARACTER written: a megabyte of
      -- output was 4.8 s. A line at a time keeps output and errors in the order they were written, and the
      -- turn flushes whatever is left.
      hSetBuffering stdout LineBuffering
      void (forkIO (writeAll (eStdin e) (BC.dropWhileEnd (== '\n') cmd <> BC.pack "\n")))   -- (a thread: a pipe holds 64 KB)

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

sendFrame :: Handle -> B.ByteString -> IO ()
sendFrame h b = do
  let n = B.length b
  B.hPut h (B.pack [ fromIntegral ((n `shiftR` s) .&. 255) | s <- [24, 16, 8, 0] ])
  B.hPut h b
  hFlush h

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
