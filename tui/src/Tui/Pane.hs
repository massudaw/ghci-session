{-# LANGUAGE ScopedTypeVariables #-}
-- | A terminal pane: a program on a pseudo-terminal, its screen kept by libghostty-vt, drawn as a part of
-- the frame. The real terminal never sees the program's escape sequences.
module Tui.Pane
  ( Pane, paneCommand, newPane, paneFrame, paneInput, paneResize, paneStatus, paneTitle, paneHangup, freePane
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Text as T

import qualified Ghostty.Vt as Vt
import qualified Ghostty.Vt.Pty as Pty
import Tui.Buffer
import Tui.Types

data Pane = Pane
  { pTerm :: Vt.Terminal
  , pPty :: Pty.Pty
  , pSize :: IORef (Int, Int)
  , pDead :: IORef (Maybe Int)
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
          pane <- Pane term pty <$> newIORef size <*> newIORef Nothing
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
  Pty.hangup (pPty pane)
  Pty.close (pPty pane)
  Vt.freeTerminal (pTerm pane)
