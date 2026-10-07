{-# LANGUAGE ScopedTypeVariables #-}
-- | A program on a pseudo-terminal: what a "Ghostty.Vt" terminal is usually fed from, and where its answers
-- and the user's keys go.
module Ghostty.Vt.Pty
  ( Pty, ptyPid, ptyCommand, spawn, readSome, send, resize, status, hangup, terminate, close
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Control.Monad (void)
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (peek)
import System.IO (Handle, hClose, hSetBinaryMode)
import System.Posix.IO (fdToHandle, fdWriteBuf)
import System.Posix.Signals (sigHUP, sigTERM, signalProcess)
import System.Posix.Types (ByteCount, Fd (..))

import Ghostty.Vt.Raw

data Pty = Pty { ptyFd :: Fd, ptyPid :: Int, ptyRead :: Handle, ptyCommand :: String }

-- | @cmd@ under @/bin/sh@ in @cwd@, on a terminal of @cols@ x @rows@, with @TERM@ set to @term@
-- (@xterm-256color@ when empty: what libghostty-vt answers to well enough for every program).
spawn :: String -> FilePath -> String -> (Int, Int) -> IO (Either String Pty)
spawn cmd cwd term (cols, rows) = do
  (fd, pid) <- withCString cmd $ \c -> withCString cwd $ \d -> withCString term $ \tm -> alloca $ \pp -> do
    fd <- c_pty_spawn c d tm (fromIntegral cols) (fromIntegral rows) pp
    pid <- if fd < 0 then pure 0 else peek pp
    pure (fd, fromIntegral pid)
  if fd < 0 then pure (Left "cannot open a pseudo-terminal") else do
    h <- fdToHandle (Fd fd)
    hSetBinaryMode h True
    pure (Right (Pty (Fd fd) pid h cmd))

-- | Some of what the program printed, blocking until there is any; empty once it is gone.
readSome :: Pty -> IO B.ByteString
readSome p = either (\(_ :: IOException) -> B.empty) id <$> try (B.hGetSome (ptyRead p) 65536)

-- | Bytes for the program (the user's keys, the terminal's answers): to the descriptor itself, so a read
-- in progress is not waited for.
send :: Pty -> B.ByteString -> IO ()
send p b = BU.unsafeUseAsCStringLen b $ \(ptr, n) -> do
  let Fd fd = ptyFd p
      go off | off >= n = pure ()
             | otherwise = do
                 r <- try (fdWriteBuf (Fd fd) (castPtr ptr `plusPtr` off) (fromIntegral (n - off))) :: IO (Either IOException ByteCount)
                 case r of
                   Right k | k > 0 -> go (off + fromIntegral k)
                   _ -> pure ()
  go 0

resize :: Pty -> (Int, Int) -> IO ()
resize p (cols, rows) = let Fd fd = ptyFd p in void (c_pty_resize fd (fromIntegral cols) (fromIntegral rows))

-- | 'Nothing' while the program runs; its exit status once it ended (128 + the signal when one killed it).
status :: Pty -> IO (Maybe Int)
status p = (\c -> if c < 0 then Nothing else Just (fromIntegral c)) <$> c_pty_wait (fromIntegral (ptyPid p))

-- | Tell the program its terminal went away.
hangup :: Pty -> IO ()
hangup p = void (try (signalProcess sigHUP (fromIntegral (ptyPid p))) :: IO (Either IOException ()))

terminate :: Pty -> IO ()
terminate p = void (try (signalProcess sigTERM (fromIntegral (ptyPid p))) :: IO (Either IOException ()))

-- | Close the master side (the program then gets a hangup).
close :: Pty -> IO ()
close p = void (try (hClose (ptyRead p)) :: IO (Either IOException ()))
