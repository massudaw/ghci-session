{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The real terminal: raw mode for the life of an action, its size, keys as they are typed, and the events
-- a loop waits on.
module Tui.Terminal
  ( withRawTerminal, termSize
  , Key (..), decodeKey, Event (..), Events (..), startEvents, wake
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (IOException, finally, try)
import qualified Data.ByteString.Char8 as BC
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.IO
import System.Posix.IO (stdInput)
import System.Posix.Signals (Handler (..), installHandler)
import System.Posix.Terminal

foreign import ccall unsafe "tui_term_size" c_term_size :: Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "tui_sigwinch" c_sigwinch :: IO CInt

-- | Columns and rows of the terminal on standard output, if it is one.
termSize :: IO (Maybe (Int, Int))
termSize = alloca $ \pr -> alloca $ \pc -> do
  rc <- c_term_size pr pc
  if rc /= 0 then pure Nothing else (\r c -> Just (fromIntegral c, fromIntegral r)) <$> peek pr <*> peek pc

-- | Raw mode (no echo, no line discipline, no ^C), the alternate screen, the cursor hidden; everything put
-- back when the action ends, however it ends.
withRawTerminal :: IO a -> IO a
withRawTerminal act = do
  old <- getTerminalAttributes stdInput
  let raw = (foldl withoutMode old [EnableEcho, ProcessInput, KeyboardInterrupts, StartStopOutput, ExtendedFunctions, MapCRtoLF]) `withMinInput` 1 `withTime` 0
  setTerminalAttributes stdInput raw Immediately
  hSetBuffering stdout (BlockBuffering (Just 65536))
  hPutStr stdout "\ESC[?1049h\ESC[?25l\ESC[2J"
  hFlush stdout
  act `finally` do
    hPutStr stdout "\ESC[0m\ESC[?25h\ESC[?1049l"
    hFlush stdout
    setTerminalAttributes stdInput old Immediately

data Key = KChar Char | KUp | KDown | KLeft | KRight | KPgUp | KPgDn | KHome | KEnd | KEsc | KEnter | KBackspace | KDelete | KTab | KFn Int
  deriving (Eq, Show)

-- | A key from the bytes a terminal sends for it; a sequence it does not know is 'KEsc'.
decodeKey :: String -> Key
decodeKey s = case s of
  "\ESC[A" -> KUp
  "\ESC[B" -> KDown
  "\ESC[C" -> KRight
  "\ESC[D" -> KLeft
  "\ESCOA" -> KUp
  "\ESCOB" -> KDown
  "\ESCOC" -> KRight
  "\ESCOD" -> KLeft
  "\ESC[5~" -> KPgUp
  "\ESC[6~" -> KPgDn
  "\ESC[H" -> KHome
  "\ESC[1~" -> KHome
  "\ESCOH" -> KHome
  "\ESC[F" -> KEnd
  "\ESC[4~" -> KEnd
  "\ESCOF" -> KEnd
  "\ESC[3~" -> KDelete
  "\ESCOP" -> KFn 1
  "\ESCOQ" -> KFn 2
  "\ESCOR" -> KFn 3
  "\ESCOS" -> KFn 4
  "\ESC" -> KEsc
  "\r" -> KEnter
  "\n" -> KEnter
  "\t" -> KTab
  "\DEL" -> KBackspace
  "\b" -> KBackspace
  [c] -> KChar c
  _ -> KEsc

-- | What a loop sees: a key with the bytes that made it, the terminal resized, a tick, or a wake from
-- another thread.
data Event = EvKey BC.ByteString Key | EvResize | EvTick | EvWake
  deriving (Eq, Show)

data Events = Events { evQueue :: TQueue Event, evWakes :: TVar Int }

-- | Keys from standard input (an escape followed within a few milliseconds by more bytes is one sequence)
-- and the window's size changes, as events.
startEvents :: IO Events
startEvents = do
  q <- newTQueueIO
  wakes <- newTVarIO 0
  winch <- c_sigwinch
  _ <- installHandler winch (Catch (atomically (writeTQueue q EvResize))) Nothing
  hSetBinaryMode stdin True
  hSetBuffering stdin NoBuffering
  _ <- forkIO (reader q)
  pure (Events q wakes)

-- | Another thread has something to show.
wake :: Events -> IO ()
wake ev = atomically (modifyTVar' (evWakes ev) (+ 1))

reader :: TQueue Event -> IO ()
reader q = loop
  where
    loop = do
      r <- try (hGetChar stdin) :: IO (Either IOException Char)
      case r of
        Left _ -> pure ()
        Right '\ESC' -> do
          more <- hWaitForInput stdin 40
          if not more then push "\ESC" else do
            c2 <- hGetChar stdin
            if c2 `elem` "[O" then do
              rest <- final
              push ('\ESC' : c2 : rest)
            else push "\ESC" >> push [c2]
          loop
        Right c -> push [c] >> loop
    final = do
      c <- hGetChar stdin
      if c >= '@' && c <= '~' then pure [c] else (c :) <$> final
    push s = atomically (writeTQueue q (EvKey (BC.pack s) (decodeKey s)))
