-- | __A descriptor read a line at a time, with nothing read ahead that is not kept in sight.__
--
-- A buffered handle reads ahead: bytes it has taken and not yet given are in a buffer of the runtime's, and
-- are gone when the process becomes another program. The chat hands the connections of a turn under way to
-- the program it restarts as ("GhciSession.Chat"), so these are read here: what was read and not yet given is
-- a value ('wRest'), handed over with the descriptor. And a wait for a line can be asked to end ('Asked'),
-- which a read of a handle cannot: that is how every reader comes to rest before the hand-over.
module GhciSession.Wire
  ( Wire, wFd, newWire, wRest, Got (..), nextLine, fdPut, keepOnExec
  ) where

import Control.Concurrent (threadWaitRead, threadWaitWrite)
import Control.Exception (IOException, try)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.IORef
import Foreign.Ptr (castPtr, plusPtr)
import System.Posix.IO (FdOption (..), fdReadBuf, fdWriteBuf, setFdOption)
import System.Posix.Types (ByteCount, Fd)
import System.Timeout (timeout)

-- | A descriptor, and what was read from it and not yet given.
data Wire = Wire { wFd :: Fd, wBuf :: IORef B.ByteString }

newWire :: Fd -> B.ByteString -> IO Wire
newWire fd rest = Wire fd <$> newIORef rest

-- | What was read and not yet given: with the descriptor, all there is to go on reading it elsewhere.
wRest :: Wire -> IO B.ByteString
wRest = readIORef . wBuf

-- | A line (without its end), the end of the input (what was left, if anything, was given as a line), or
-- the wait given up because it was asked to be.
data Got = Line B.ByteString | End | Asked
  deriving (Eq, Show)

-- | The next line. While none is there, @asked@ is looked at five times a second: True ends the wait.
nextLine :: Wire -> IO Bool -> IO Got
nextLine w asked = go
  where
    go = do
      buf <- readIORef (wBuf w)
      case B.elemIndex 10 buf of
        Just i -> writeIORef (wBuf w) (B.drop (i + 1) buf) >> pure (Line (B.take i buf))
        Nothing -> do
          stop <- asked
          if stop then pure Asked else do
            ready <- timeout 200000 (threadWaitRead (wFd w))
            case ready of
              Nothing -> go
              Just () -> do
                r <- try (BI.createUptoN 65536 (\p -> fromIntegral <$> fdReadBuf (wFd w) p 65536)) :: IO (Either IOException B.ByteString)
                case r of
                  Right b | not (B.null b) -> modifyIORef' (wBuf w) (<> b) >> go
                  -- (nothing, on a descriptor said to be ready: its end. An error that is "try again" is not)
                  Right _ -> finish buf
                  Left e | again e -> go
                  Left _ -> finish buf
    finish buf = if B.null buf then pure End else writeIORef (wBuf w) B.empty >> pure (Line buf)
    again e = any (`isIn` show e) ["resource exhausted", "Resource temporarily unavailable", "interrupted"]
    isIn needle hay = any (\k -> take (length needle) (drop k hay) == needle) [0 .. length hay - length needle]

-- | All of the bytes, written (a descriptor that would block is waited for).
fdPut :: Fd -> B.ByteString -> IO ()
fdPut fd bs = BU.unsafeUseAsCStringLen bs (\(p, n) -> go (castPtr p) n)
  where
    go _ 0 = pure ()
    go p n = do
      r <- try (fdWriteBuf fd p (fromIntegral n)) :: IO (Either IOException ByteCount)
      case r of
        Right k -> go (p `plusPtr` fromIntegral k) (n - fromIntegral k)
        Left e | "exhausted" `elem` words (show e) || "unavailable" `elem` words (show e) -> threadWaitWrite fd >> go p n
        Left e -> ioError e

-- | The descriptor stays open in the program this process becomes.
keepOnExec :: Fd -> IO ()
keepOnExec fd = setFdOption fd CloseOnExec False
