{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The real terminal: raw mode for the life of an action, its size, keys as they are typed, and the events
-- a loop waits on.
module Tui.Terminal
  ( withRawTerminal, termSize
  , Key (..), Mod (..), KeyPress (..), decodeKey, decodeKeyPress, Event (..), Events (..), startEvents, stopEvents, wake
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
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
  hSetBinaryMode stdin True
  hSetBuffering stdin NoBuffering
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

data Key = KChar Char | KUp | KDown | KLeft | KRight | KPgUp | KPgDn | KHome | KEnd | KEsc | KEnter | KBackspace | KDelete | KInsert | KTab | KFn Int
  deriving (Eq, Show)

data Mod = Shift | Ctrl | Alt
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | What was typed: the key, the modifiers the terminal reported with it, and the bytes it sent.
data KeyPress = KeyPress { kKey :: Key, kMods :: [Mod], kBytes :: BC.ByteString }
  deriving (Eq, Show)

-- | A key from the bytes a terminal sends for it; a sequence it does not know is 'KEsc'.
decodeKey :: String -> Key
decodeKey = kKey . decodeKeyPress

-- | A key and its modifiers from the bytes: the xterm forms (@ESC [ 1 ; m A@, @ESC [ 3 ; m ~@, where
-- @m - 1@ is a bitmask of shift 1, alt 2, ctrl 4), a control character as Ctrl and its letter, an escape
-- before a character as Alt and the character.
decodeKeyPress :: String -> KeyPress
decodeKeyPress s = case s of
  '\ESC' : '[' : rest | Just (params, final) <- csi rest -> let (k, ms) = csiKey params final in KeyPress k ms bytes
  '\ESC' : 'O' : [c] -> KeyPress (ss3 c) [] bytes
  ['\ESC', c] | c /= '\ESC' -> let KeyPress k ms _ = decodeKeyPress [c] in KeyPress k (Alt : ms) bytes
  _ -> KeyPress (plainKey s) (mods s) bytes
  where
    bytes = BC.pack s
    csi r = let (ps, f) = span (\c -> c < '@' || c > '~') r in case f of { [c] -> Just (ps, c); _ -> Nothing }
    nums ps = map (\x -> case reads x of { [(n, "")] -> n; _ -> 0 :: Int }) (splitOn ';' ps)
    splitOn c str = case break (== c) str of { (x, _ : y) -> x : splitOn c y; (x, []) -> [x] }
    xmods n = [ Shift | n' `mod` 2 == 1 ] ++ [ Alt | n' `div` 2 `mod` 2 == 1 ] ++ [ Ctrl | n' `div` 4 `mod` 2 == 1 ] where n' = max 0 (n - 1)
    csiKey params final =
      let ns = nums params
          m = case ns of { (_ : x : _) -> xmods x; _ -> [] }
          byNum n = case n of { 1 -> KHome; 2 -> KInsert; 3 -> KDelete; 4 -> KEnd; 5 -> KPgUp; 6 -> KPgDn; 7 -> KHome; 8 -> KEnd
                              ; 11 -> KFn 1; 12 -> KFn 2; 13 -> KFn 3; 14 -> KFn 4; 15 -> KFn 5; 17 -> KFn 6; 18 -> KFn 7; 19 -> KFn 8
                              ; 20 -> KFn 9; 21 -> KFn 10; 23 -> KFn 11; 24 -> KFn 12; _ -> KEsc }
      in case final of
           'A' -> (KUp, m); 'B' -> (KDown, m); 'C' -> (KRight, m); 'D' -> (KLeft, m); 'H' -> (KHome, m); 'F' -> (KEnd, m)
           'P' -> (KFn 1, m); 'Q' -> (KFn 2, m); 'R' -> (KFn 3, m); 'S' -> (KFn 4, m)
           'Z' -> (KTab, Shift : m)
           '~' -> (byNum (case ns of { (x : _) -> x; [] -> 0 }), m)
           _ -> (KEsc, [])
    ss3 c = case c of { 'A' -> KUp; 'B' -> KDown; 'C' -> KRight; 'D' -> KLeft; 'H' -> KHome; 'F' -> KEnd; 'P' -> KFn 1; 'Q' -> KFn 2; 'R' -> KFn 3; 'S' -> KFn 4; _ -> KEsc }
    plainKey str = case str of
      "\ESC" -> KEsc
      "\r" -> KEnter
      "\n" -> KEnter
      "\t" -> KTab
      "\DEL" -> KBackspace
      "\b" -> KBackspace
      [c] | c < ' ' -> KChar (toEnum (fromEnum c + 96))     -- ^A..^Z as the letter, with Ctrl
      [c] -> KChar c
      _ -> KEsc
    mods str = case str of
      [c] | c < ' ' && c `notElem` "\ESC\r\n\t\b" -> [Ctrl]
      _ -> []

-- | What a loop sees: a key with the bytes that made it, the terminal resized, a tick, or a wake from
-- another thread.
data Event = EvKey KeyPress | EvResize | EvTick | EvWake
  deriving (Eq, Show)

data Events = Events { evQueue :: TQueue Event, evWakes :: TVar Int, evReader :: !ThreadId }

-- | Keys from standard input (an escape followed within a few milliseconds by more bytes is one sequence)
-- and the window's size changes, as events.
startEvents :: IO Events
startEvents = do
  q <- newTQueueIO
  wakes <- newTVarIO 0
  winch <- c_sigwinch
  _ <- installHandler winch (Catch (atomically (writeTQueue q EvResize))) Nothing
  tid <- forkIO (reader q)
  pure (Events q wakes tid)

-- | Stop reading events and terminate the reader thread.
stopEvents :: Events -> IO ()
stopEvents ev = killThread (evReader ev)

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
            else push ['\ESC', c2]          -- (alt and the key)
          loop
        Right c -> push [c] >> loop
    final = do
      c <- hGetChar stdin
      if c >= '@' && c <= '~' then pure [c] else (c :) <$> final
    push s = atomically (writeTQueue q (EvKey (decodeKeyPress s)))
