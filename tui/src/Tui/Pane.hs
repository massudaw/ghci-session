{-# LANGUAGE ScopedTypeVariables #-}
-- | A terminal pane: a program on a pseudo-terminal, its screen kept by libghostty-vt, drawn as a part of
-- the frame. The real terminal never sees the program's escape sequences.
module Tui.Pane
  ( Pane, paneCommand, newPane, paneFrame, paneInput, paneKey, keyEventFor, paneResize, paneStatus, paneTitle, paneHangup, freePane
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Text as T

import Data.Char (isAlpha, isAsciiLower, isAsciiUpper, isDigit, toLower)
import qualified Ghostty.Vt as Vt
import qualified Ghostty.Vt.Pty as Pty
import Tui.Buffer
import Tui.Terminal (Key (..), KeyPress (..), Mod (..))
import Tui.Types

data Pane = Pane
  { pTerm :: Vt.Terminal
  , pPty :: Pty.Pty
  , pSize :: IORef (Int, Int)
  , pDead :: IORef (Maybe Int)
  , pEncoder :: Maybe Vt.KeyEncoder     -- ^ keys encoded for the program's modes; without it the bytes go as typed
  }

paneCommand :: Pane -> String
paneCommand = Pty.ptyCommand . pPty

-- | Start @cmd@ in @cwd@ on a terminal of this size; @wake@ is called whenever the program printed
-- something or ended, so the frame is drawn again.
newPane :: String -> FilePath -> (Int, Int) -> IO () -> IO (Either String Pane)
newPane cmd cwd size wake = do
  t <- Vt.newTerminal (fst size) (snd size)
  case t of
    Left e -> pure (Left e)
    Right term -> do
      p <- Pty.spawn cmd cwd "xterm-256color" size
      case p of
        Left e -> Vt.freeTerminal term >> pure (Left e)
        Right pty -> do
          Vt.onWrite term (Pty.send pty)
          enc <- either (const Nothing) Just <$> Vt.newKeyEncoder
          pane <- Pane term pty <$> newIORef size <*> newIORef Nothing <*> pure enc
          _ <- forkIO $ do
            let loop = do
                  b <- Pty.readSome pty
                  if B.null b
                    then do
                      st <- Pty.status pty
                      writeIORef (pDead pane) (Just (maybe 0 id st))
                      wake
                    else Vt.write term b >> wake >> loop
            void (try loop :: IO (Either SomeException ()))
          pure (Right pane)

-- | The pane's screen as puts at a rectangle, and where its cursor is (in frame coordinates) when shown.
paneFrame :: Pane -> Rect -> IO ([Put], Maybe (Int, Int))
paneFrame pane (Rect x y w h) = do
  s <- Vt.screen (pTerm pane)
  let rows = take h (Vt.sCells s)
      cells row = [ (cell c, width c) | c <- take w' row ]
        where w' = w
      cell c = Cell (style c) (Vt.cText c) False
      width c = case Vt.cWide c of { Vt.WideChar -> 2; Vt.Narrow -> 1; _ -> 0 }
      style c = Style { sFg = color (Vt.cFg c), sBg = color (Vt.cBg c), sBold = Vt.cBold c, sFaint = Vt.cFaint c, sItalic = Vt.cItalic c
                      , sUnderline = Vt.cUnderline c, sInverse = Vt.cInverse c, sStrike = Vt.cStrike c }
      color = maybe Default (\(Vt.RGB r g b) -> Rgb r g b)
      puts = [ PutCells x (y + i) (cells row) | (i, row) <- zip [0 ..] rows ]
      cur = case Vt.sCursor s of
        Just (cx, cy) | Vt.sCursorVisible s && cx < w && cy < h -> Just (x + cx, y + cy)
        _ -> Nothing
  pure (puts, cur)

-- | Keys for the program.
paneInput :: Pane -> B.ByteString -> IO ()
paneInput pane b = do
  dead <- readIORef (pDead pane)
  case dead of
    Nothing -> Pty.send (pPty pane) b
    Just _ -> pure ()

-- | A key for the program, encoded as the program expects it: the pane's terminal says what modes the
-- program set (application cursor keys, the kitty keyboard protocol, ...), and libghostty-vt's key encoder
-- writes the key for them. A key the encoder does not know, or without the library, goes as the user's
-- terminal sent it.
paneKey :: Pane -> KeyPress -> IO ()
paneKey pane kp = case (pEncoder pane, keyEventFor kp) of
  (Just enc, Just ev) -> do
    r <- Vt.encodeKey enc (pTerm pane) ev
    case r of
      Right b | not (B.null b) -> paneInput pane b
      _ -> paneInput pane (kBytes kp)
  _ -> paneInput pane (kBytes kp)

-- | The key event a key press is, on a US layout: the physical key, its modifiers, the text it types.
keyEventFor :: KeyPress -> Maybe Vt.KeyEvent
keyEventFor (KeyPress k ms _) = case k of
  KChar c -> charEvent c
  KUp -> special "arrow_up"
  KDown -> special "arrow_down"
  KLeft -> special "arrow_left"
  KRight -> special "arrow_right"
  KPgUp -> special "page_up"
  KPgDn -> special "page_down"
  KHome -> special "home"
  KEnd -> special "end"
  KEsc -> special "escape"
  KEnter -> special "enter"
  KBackspace -> special "backspace"
  KDelete -> special "delete"
  KInsert -> special "insert"
  KTab -> special "tab"
  KFn n | n >= 1 && n <= 12 -> special ("f" ++ show n)
  _ -> Nothing
  where
    mods = [ case m of { Shift -> Vt.Shift; Ctrl -> Vt.Ctrl; Alt -> Vt.Alt } | m <- ms ]
    special name = Just (Vt.KeyEvent Vt.Press name mods T.empty Nothing)
    charEvent c
      | Ctrl `elem` ms = Just (Vt.KeyEvent Vt.Press (keyName (toLower c)) mods T.empty (Just (toLower c)))
      | c == ' ' = Just (Vt.KeyEvent Vt.Press "space" mods (T.singleton ' ') (Just ' '))
      | isAsciiUpper c = Just (Vt.KeyEvent Vt.Press [toLower c] (Vt.Shift : mods) (T.singleton c) (Just (toLower c)))
      | isAsciiLower c || isDigit c = Just (Vt.KeyEvent Vt.Press (keyName c) mods (T.singleton c) (Just c))
      | Just (name, unshifted, shifted) <- lookup c punctuation = Just (Vt.KeyEvent Vt.Press name (if shifted then Vt.Shift : mods else mods) (T.singleton c) (Just unshifted))
      | c < ' ' = Nothing
      | otherwise = Just (Vt.KeyEvent Vt.Press "unidentified" mods (T.singleton c) (if isAlpha c then Just (toLower c) else Just c))
    keyName c | isDigit c = "digit_" ++ [c]
              | isAsciiLower c = [c]
              | Just (name, _, _) <- lookup c punctuation = name
              | otherwise = "unidentified"
    punctuation =
      [ ('`', ("backquote", '`', False)), ('~', ("backquote", '`', True)), ('-', ("minus", '-', False)), ('_', ("minus", '-', True))
      , ('=', ("equal", '=', False)), ('+', ("equal", '=', True)), ('[', ("bracket_left", '[', False)), ('{', ("bracket_left", '[', True))
      , (']', ("bracket_right", ']', False)), ('}', ("bracket_right", ']', True)), ('\\', ("backslash", '\\', False)), ('|', ("backslash", '\\', True))
      , (';', ("semicolon", ';', False)), (':', ("semicolon", ';', True)), ('\'', ("quote", '\'', False)), ('"', ("quote", '\'', True))
      , (',', ("comma", ',', False)), ('<', ("comma", ',', True)), ('.', ("period", '.', False)), ('>', ("period", '.', True))
      , ('/', ("slash", '/', False)), ('?', ("slash", '/', True))
      , (')', ("digit_0", '0', True)), ('!', ("digit_1", '1', True)), ('@', ("digit_2", '2', True)), ('#', ("digit_3", '3', True)), ('$', ("digit_4", '4', True))
      , ('%', ("digit_5", '5', True)), ('^', ("digit_6", '6', True)), ('&', ("digit_7", '7', True)), ('*', ("digit_8", '8', True)), ('(', ("digit_9", '9', True)) ]

paneResize :: Pane -> (Int, Int) -> IO ()
paneResize pane sz@(cols, rows) = do
  old <- readIORef (pSize pane)
  if old == sz || cols <= 0 || rows <= 0 then pure () else do
    writeIORef (pSize pane) sz
    Vt.resize (pTerm pane) cols rows
    Pty.resize (pPty pane) sz

-- | 'Nothing' while the program runs, its exit status once it ended.
paneStatus :: Pane -> IO (Maybe Int)
paneStatus = readIORef . pDead

paneTitle :: Pane -> IO T.Text
paneTitle = Vt.title . pTerm

paneHangup :: Pane -> IO ()
paneHangup = Pty.hangup . pPty

freePane :: Pane -> IO ()
freePane pane = do
  mapM_ Vt.freeKeyEncoder (pEncoder pane)
  Pty.hangup (pPty pane)
  Pty.close (pPty pane)
  Vt.freeTerminal (pTerm pane)
