{-# LANGUAGE ScopedTypeVariables #-}
-- | __Ghostty's terminal emulation, from Haskell.__ A 'Terminal' is what a program writes to: feed it the
-- bytes a program prints ('write'), and read its 'screen' -- every cell's text, colors and attributes, the
-- cursor -- as a value. What the terminal answers to the program (a cursor position report, device
-- attributes) comes back through 'onWrite'. "Ghostty.Vt.Pty" runs a program on a pseudo-terminal, which is
-- where those bytes usually come from and go to.
--
-- The library is libghostty-vt, found at run time: 'load' first, and a program without it can say so
-- instead of failing to start. A terminal is not thread-safe in the library; every operation here takes
-- the terminal's lock, so one may be written by one thread and read by another.
module Ghostty.Vt
  ( -- * The library
    load, loaded, libraryError
    -- * A terminal
  , Terminal, newTerminal, freeTerminal, withTerminal, write, resize, onWrite, size, cursor, title, workingDirectory
  , Scroll (..), scroll
    -- * Its screen
  , Screen (..), Row, Cell (..), Wide (..), RGB (..), CursorStyle (..), screen, screenText
    -- * Keys for the program it runs
  , KeyEncoder, newKeyEncoder, freeKeyEncoder, KeyEvent (..), KeyAction (..), Mod (..), encodeKey
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracket)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Word (Word8)
import Foreign.C.String (withCStringLen)
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (FunPtr, Ptr, castPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (peek)

import Ghostty.Vt.Raw

-- | Load libghostty-vt: the path given first (or @""@), then the names the system linker knows
-- (@libghostty-vt.so.0@, @libghostty-vt.dylib@, ...). The reason when it cannot. Idempotent.
load :: FilePath -> IO (Either String ())
load path = do
  rc <- withCString path c_load
  if rc == 0 then pure (Right ()) else Left <$> libraryError

loaded :: IO Bool
loaded = (/= 0) <$> c_loaded

libraryError :: IO String
libraryError = c_error >>= peekCString

-- | A terminal of some size: the emulation's state (screens, scrollback, modes) and a render state to read
-- it through.
data Terminal = Terminal
  { tPtr :: Ptr ()
  , tRender :: Ptr ()
  , tLock :: MVar ()
  , tWriteFn :: IORef (Maybe (FunPtr WriteFn))
  }

-- | A terminal of @cols@ x @rows@. 'Left' when the library is not loaded or refuses.
newTerminal :: Int -> Int -> IO (Either String Terminal)
newTerminal cols rows = do
  ok <- loaded
  if not ok then Left <$> libraryError else do
    p <- c_terminal_new (fromIntegral cols) (fromIntegral rows)
    if p == nullPtr then pure (Left "libghostty-vt could not make a terminal") else do
      r <- c_render_new
      if r == nullPtr then c_terminal_free p >> pure (Left "libghostty-vt could not make a render state") else
        Right <$> (Terminal p r <$> newMVar () <*> newIORef Nothing)

freeTerminal :: Terminal -> IO ()
freeTerminal t = withMVar (tLock t) $ \_ -> do
  c_terminal_on_write (tPtr t) nullFunPtr nullPtr
  readIORef (tWriteFn t) >>= mapM_ freeHaskellFunPtr
  writeIORef (tWriteFn t) Nothing
  c_render_free (tRender t)
  c_terminal_free (tPtr t)

withTerminal :: Int -> Int -> (Terminal -> IO a) -> IO (Either String a)
withTerminal cols rows act = do
  r <- newTerminal cols rows
  case r of
    Left e -> pure (Left e)
    Right t -> Right <$> bracket (pure t) freeTerminal act

-- | Bytes the program printed.
write :: Terminal -> B.ByteString -> IO ()
write t b = withMVar (tLock t) $ \_ -> BU.unsafeUseAsCStringLen b $ \(p, n) -> c_terminal_write (tPtr t) (castPtr p) (fromIntegral n)

resize :: Terminal -> Int -> Int -> IO ()
resize t cols rows = withMVar (tLock t) $ \_ -> () <$ c_terminal_resize (tPtr t) (fromIntegral cols) (fromIntegral rows)

-- | What the terminal writes back to the program (answers to its queries): usually sent down the pty. The
-- action runs during a 'write', so it must not 'write' to this terminal itself.
onWrite :: Terminal -> (B.ByteString -> IO ()) -> IO ()
onWrite t act = withMVar (tLock t) $ \_ -> do
  fn <- mkWriteFn (\_ p n -> B.packCStringLen (castPtr p, fromIntegral n) >>= act)
  readIORef (tWriteFn t) >>= mapM_ freeHaskellFunPtr
  writeIORef (tWriteFn t) (Just fn)
  c_terminal_on_write (tPtr t) fn nullPtr

-- | Columns and rows.
size :: Terminal -> IO (Int, Int)
size t = withMVar (tLock t) $ \_ -> (,) <$> int 0 <*> int 1
  where int k = fromIntegral <$> c_terminal_int (tPtr t) k

-- | The cursor's column and row, and whether it is shown.
cursor :: Terminal -> IO ((Int, Int), Bool)
cursor t = withMVar (tLock t) $ \_ -> do
  x <- c_terminal_int (tPtr t) 2
  y <- c_terminal_int (tPtr t) 3
  v <- c_terminal_int (tPtr t) 4
  pure ((fromIntegral x, fromIntegral y), v > 0)

-- | The title the program set (OSC 0 / 2), empty when none.
title :: Terminal -> IO T.Text
title t = str t 0

-- | The working directory the program reported (OSC 7), empty when none.
workingDirectory :: Terminal -> IO T.Text
workingDirectory t = str t 1

str :: Terminal -> Int -> IO T.Text
str t what = withMVar (tLock t) $ \_ -> allocaBytes 4096 $ \buf -> do
  n <- c_terminal_string (tPtr t) (fromIntegral what) buf 4096
  if n <= 0 then pure T.empty else TE.decodeUtf8With TE.lenientDecode <$> B.packCStringLen (buf, fromIntegral n)

data Scroll = ScrollTop | ScrollBottom | ScrollBy Int | ScrollToRow Int
  deriving (Eq, Show)

-- | Move the viewport over the scrollback.
scroll :: Terminal -> Scroll -> IO ()
scroll t s = withMVar (tLock t) $ \_ -> case s of
  ScrollTop -> c_terminal_scroll (tPtr t) 0 0
  ScrollBottom -> c_terminal_scroll (tPtr t) 1 0
  ScrollBy d -> c_terminal_scroll (tPtr t) 2 (fromIntegral d)
  ScrollToRow r -> c_terminal_scroll (tPtr t) 3 (fromIntegral r)

-- the screen ---------------------------------------------------------------------------------

data RGB = RGB !Word8 !Word8 !Word8
  deriving (Eq, Ord, Show)

-- | How many columns a cell takes.
data Wide = Narrow | WideChar | SpacerTail | SpacerHead
  deriving (Eq, Show, Enum)

data CursorStyle = CursorBar | CursorBlock | CursorUnderline | CursorHollow
  deriving (Eq, Show, Enum)

data Cell = Cell
  { cText :: !T.Text          -- ^ the grapheme; empty for a blank cell or a spacer
  , cWide :: !Wide
  , cFg :: !(Maybe RGB)        -- ^ 'Nothing': the terminal's default
  , cBg :: !(Maybe RGB)
  , cBold, cFaint, cItalic, cUnderline, cInverse, cStrike, cInvisible, cBlink :: !Bool
  } deriving (Eq, Show)

type Row = [Cell]

data Screen = Screen
  { sCols, sRows :: !Int
  , sCells :: [Row]                      -- ^ top to bottom, each of 'sCols' cells
  , sCursor :: !(Maybe (Int, Int))       -- ^ column and row, when in the viewport
  , sCursorVisible :: !Bool
  , sCursorStyle :: !CursorStyle
  , sForeground, sBackground :: !RGB     -- ^ the defaults
  , sDirty :: !Int                       -- ^ 0: nothing changed since the last 'screen'; 1: some rows; 2: everything
  , sDirtyRows :: [Bool]                 -- ^ per row, changed since the last 'screen'
  } deriving (Show)

-- | The screen now. Reading it marks it clean, so the next one's dirty flags say what changed since.
screen :: Terminal -> IO Screen
screen t = withMVar (tLock t) $ \_ -> do
  let r = tRender t
  _ <- c_render_update r (tPtr t)
  cols <- int r 0
  rows <- int r 1
  dirty <- int r 2
  cx <- int r 3
  cy <- int r 4
  cvis <- int r 5
  cst <- int r 6
  chas <- int r 7
  (fgc, bgc) <- allocaBytes 6 $ \p -> c_render_colors r p >> rgb2 <$> peekArray 6 p
  _ <- c_render_rows_begin r
  rowsOut <- readRows r
  _ <- c_render_clean r
  pure Screen { sCols = cols, sRows = rows, sCells = map snd rowsOut
              , sCursor = if chas > 0 then Just (cx, cy) else Nothing, sCursorVisible = cvis > 0
              , sCursorStyle = toEnum (max 0 (min 3 cst)), sForeground = fgc, sBackground = bgc
              , sDirty = dirty, sDirtyRows = map fst rowsOut }
  where
    int r k = fromIntegral <$> c_render_int r k :: IO Int
    readRows r = alloca $ \py -> alloca $ \pd -> do
      more <- c_render_row_next r py pd
      if more == 0 then pure [] else do
        d <- peek pd
        cells <- readCells r
        ((d /= 0, cells) :) <$> readRows r
    readCells r = alloca $ \pw -> alloca $ \pf -> alloca $ \pfs -> allocaBytes 3 $ \pfg -> alloca $ \pbs -> allocaBytes 3 $ \pbg -> allocaBytes 256 $ \pu -> do
      let go = do
            n <- c_render_cell_next r pw pf pfs pfg pbs pbg pu 256
            if n < 0 then pure [] else do
              w <- peek pw
              f <- peek pf
              fs <- peek pfs
              bs <- peek pbs
              RGB a b c <- rgb1 <$> peekArray 3 pfg
              RGB d e g <- rgb1 <$> peekArray 3 pbg
              txt <- if n == 0 then pure T.empty else TE.decodeUtf8With TE.lenientDecode <$> B.packCStringLen (castPtr pu, fromIntegral n)
              let bit k = (fromIntegral f :: Int) `div` k `mod` 2 == 1
                  cell = Cell txt (toEnum (max 0 (min 3 (fromIntegral w)))) (if fs /= 0 then Just (RGB a b c) else Nothing) (if bs /= 0 then Just (RGB d e g) else Nothing)
                                (bit 1) (bit 2) (bit 4) (bit 8) (bit 16) (bit 32) (bit 64) (bit 128)
              (cell :) <$> go
      go

rgb1 :: [Word8] -> RGB
rgb1 ws = case ws of { (a : b : c : _) -> RGB a b c; _ -> RGB 0 0 0 }

rgb2 :: [Word8] -> (RGB, RGB)
rgb2 ws = (rgb1 ws, rgb1 (drop 3 ws))

-- | The screen as plain lines (a wide character once, its spacer skipped), trailing blanks cut.
screenText :: Screen -> [T.Text]
screenText s = [ T.stripEnd (T.concat [ if T.null (cText c) then T.singleton ' ' else cText c | c <- row, cWide c /= SpacerTail, cWide c /= SpacerHead ]) | row <- sCells s ]

-- keys ----------------------------------------------------------------------------------------

-- | Encodes keys as the program in a terminal expects them, by the modes that terminal is in.
newtype KeyEncoder = KeyEncoder (Ptr ())

newKeyEncoder :: IO (Either String KeyEncoder)
newKeyEncoder = do
  p <- c_key_encoder_new
  if p == nullPtr then Left <$> libraryError else pure (Right (KeyEncoder p))

freeKeyEncoder :: KeyEncoder -> IO ()
freeKeyEncoder (KeyEncoder p) = c_key_encoder_free p

data KeyAction = Press | Release | Repeat
  deriving (Eq, Show, Enum)

data Mod = Shift | Ctrl | Alt | Super
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A key as a keyboard reports it: the physical key by name (@"a"@, @"digit_1"@, @"arrow_up"@, @"enter"@,
-- @"f5"@, @"unidentified"@ ...), the modifiers held, the text it types under the layout (empty when none),
-- and the key's unshifted character.
data KeyEvent = KeyEvent
  { keAction :: KeyAction
  , keKey :: String
  , keMods :: [Mod]
  , keText :: T.Text
  , keUnshifted :: Maybe Char
  } deriving (Eq, Show)

-- | The bytes the program in the terminal should receive for this key, encoded for the modes the terminal
-- is in now (application cursor keys, the kitty keyboard protocol, modifyOtherKeys, ...). Empty when the
-- key has no encoding (a lone modifier); 'Left' for a key name the encoder does not know.
encodeKey :: KeyEncoder -> Terminal -> KeyEvent -> IO (Either String B.ByteString)
encodeKey (KeyEncoder enc) t ev = withMVar (tLock t) $ \_ ->
  withCString (keKey ev) $ \name -> withCStringLen (T.unpack (keText ev)) $ \(utf8, ulen) -> do
    let mods = sum [ case m of { Shift -> 1; Ctrl -> 2; Alt -> 4; Super -> 8 } | m <- keMods ev ] :: Int
        action = case keAction ev of { Press -> 1; Release -> 0; Repeat -> 2 }
        unshifted = maybe 0 (fromIntegral . fromEnum) (keUnshifted ev)
        go cap = allocaBytes cap $ \out -> do
          n <- c_key_encode enc (tPtr t) action name (fromIntegral mods) utf8 (fromIntegral ulen) unshifted out (fromIntegral cap)
          if n >= 0 then Right <$> B.packCStringLen (out, fromIntegral n)
          else if n == -1 then go (cap * 4)
          else if n == -2 then pure (Left ("unknown key " ++ show (keKey ev)))
          else pure (Left "libghostty-vt could not encode the key")
    go 256
