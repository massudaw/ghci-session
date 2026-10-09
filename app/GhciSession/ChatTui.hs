{-# LANGUAGE ScopedTypeVariables #-}
-- | __The chat on a screen of its own__: @ghci-session chat --tui@. The transcript above -- what was typed,
-- what the agent said and thought, every tool call and its answer, the harness's notes, the view the first
-- turn read -- each line labelled by its kind as @top@'s history shows them; a status line saying what the
-- turn is doing now (which model call, which tool), or that a line is waited for; a line to type on at the
-- bottom. A line typed while the agent works reaches it between tool calls, as on the standard streams. The
-- session's verdict is in the header, read from its status file every two seconds.
--
-- Keys: Enter sends the line; Up and Down recall lines sent before; PgUp and PgDn scroll the transcript,
-- Ctrl-Up and Ctrl-Down by a line, End comes back to the end (which the screen follows until scrolled);
-- the line is edited with Left, Right, Home, End, Backspace, Delete, Ctrl-A/E/U/K/W; Esc clears it, or while
-- the agent works stops the turn (the chat goes on: what the turn did is in the history);
-- Ctrl-C leaves (Ctrl-D on an empty line too), in the middle of a turn as well -- what the turn did is
-- in the history already.
--
-- An image a message names (@[image NAME]@, "GhciSession.Image") is drawn under its line where the terminal
-- shows pictures among its cells ("Tui.Graphics": Ghostty, kitty; @GHS_IMAGES=0@ for never, @1@ for anyway) --
-- sent to it once, when it first comes onto the screen, and scrolling with the lines around it. Elsewhere, and
-- in a pane of @top@, the line is what is shown.
--
-- The chat runs on a thread of its own and tells the screen what happens through a 'Ui'; the screen is
-- the 'Tui.App' loop on the main thread, woken for each thing to show.
module GhciSession.ChatTui
  ( chatTui
  -- (the pure parts, for the self-tests)
  , Editor (..), editor, editText, editKey, Entry (..), entryLines, entryLinesWith, scrollLines
  ) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, tryReadMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (foldM, forM_, unless, when)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import System.Environment (lookupEnv)
import Data.Char (isSpace)
import Data.IORef
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.IO
import System.Posix.IO (stdInput)
import System.Posix.Terminal (TerminalAttributes, TerminalState (..), getTerminalAttributes, setTerminalAttributes)
import Text.Printf (printf)

import Tui
import GhciSession.ChatUi
import GhciSession.Json
import qualified GhciSession.Image as Img
import GhciSession.Md (imageLine, imageOf, mdLines, outputLines)
import GhciSession.Sys (now, readFileMaybe)
import GhciSession.Top (Span, spansLine, wrapSpans, kindStyle, verdictStyle, stBold, stDim, stYellow, stHi)

-- the transcript ------------------------------------------------------------------------------

-- | A thing shown: its kind, as the history labels it (@user@, @talk@, @tool@, @echo@, @note@, and @think@
-- and @view@ which the history has not), and its text.
data Entry = Entry { enKind :: String, enText :: T.Text }
  deriving (Eq, Show)

-- | An entry as lines of at most @w@ columns: the kind as a label on the first, the text's lines each
-- wrapped under it, and a blank line after what was said.
entryLines :: Int -> Entry -> [[Span]]
entryLines = entryLinesWith (const Nothing)

-- | The same, with the pictures there are: the line that stands for an image is followed by its rows (the
-- image's number at the terminal, its columns and rows), where they fit the width.
entryLinesWith :: (String -> Maybe (Int, Int, Int)) -> Int -> Entry -> [[Span]]
entryLinesWith pic w (Entry kind text) = concat [ wrapSpans w 7 (lbl i ++ l) | (i, l) <- zip [0 :: Int ..] (concatMap pictured body) ] ++ [ [] | kind `elem` ["user", "talk"] ]
  where
    pictured l = case imageOf l >>= pic of
      Just (n, cols, rows) | 7 + cols <= w -> l : [ [(placeholderStyle n, placeholderRow r cols)] | r <- [0 .. rows - 1] ]
      _ -> [l]
    -- what the agent said is markdown, and is shown as what it marks up; what a tool answered may hold a diff,
    -- shown in its colors; the rest is its lines as they are
    ls = case kind of
      "talk" -> mdLines (w - 7) (T.unpack text)
      "echo" -> outputLines (T.unpack text)
      _ -> [ maybe [(style, T.unpack l)] imageLine (Img.isMarker l) | l <- T.lines text ]
    body = if null ls then [[(style, "")]] else ls
    lbl i = [ (kindStyle kind, if i == 0 then take 7 (kind ++ ":      ") else "       ") ]
    style = case kind of { "think" -> stDim; "view" -> stDim; "note" -> stDim; _ -> plain }

-- | The lines of the transcript to show on @rows@ rows, @back@ lines before the end (as many as there are:
-- the number actually scrolled back is returned); the entries newest first, only as many as it takes.
scrollLines :: Int -> Int -> Int -> [Entry] -> (Int, [[Span]])
scrollLines = scrollLinesWith (const Nothing)

scrollLinesWith :: (String -> Maybe (Int, Int, Int)) -> Int -> Int -> Int -> [Entry] -> (Int, [[Span]])
scrollLinesWith pic w rows back entries = (back', drop (length gathered - back' - rows) (take (length gathered - back') gathered))
  where
    wanted = back + rows
    gathered = gather 0 entries []
    gather n (e : es) acc | n < wanted = let ls = entryLinesWith pic w e in gather (n + length ls) es (ls ++ acc)
    gather _ _ acc = acc
    back' = max 0 (min back (length gathered - rows))

-- the line ------------------------------------------------------------------------------

-- | The line being typed: what is before the cursor (reversed) and what is after it.
data Editor = Editor { edBefore :: String, edAfter :: String }
  deriving (Eq, Show)

editor :: Editor
editor = Editor "" ""

editText :: Editor -> String
editText (Editor b a) = reverse b ++ a

fromText :: String -> Editor
fromText t = Editor (reverse t) ""

-- | The line after a key, when the key edits it.
editKey :: KeyPress -> Editor -> Maybe Editor
editKey (KeyPress k mods _) (Editor b a) = case (k, mods) of
  (KChar c, []) | c >= ' ' -> Just (Editor (c : b) a)
  (KChar c, [Shift]) | c >= ' ' -> Just (Editor (c : b) a)
  (KBackspace, _) -> Just (Editor (drop 1 b) a)
  (KChar 'h', [Ctrl]) -> Just (Editor (drop 1 b) a)
  (KDelete, _) -> Just (Editor b (drop 1 a))
  (KLeft, []) -> Just (case b of { c : r -> Editor r (c : a); [] -> Editor b a })
  (KRight, []) -> Just (case a of { c : r -> Editor (c : b) r; [] -> Editor b a })
  (KHome, _) -> Just (Editor "" (reverse b ++ a))
  (KChar 'a', [Ctrl]) -> Just (Editor "" (reverse b ++ a))
  (KEnd, _) -> Just (Editor (reverse a ++ b) "")
  (KChar 'e', [Ctrl]) -> Just (Editor (reverse a ++ b) "")
  (KChar 'u', [Ctrl]) -> Just (Editor "" a)
  (KChar 'k', [Ctrl]) -> Just (Editor b "")
  (KChar 'w', [Ctrl]) -> Just (Editor (dropWhile (not . isSpace) (dropWhile isSpace b)) a)
  (KLeft, [Alt]) -> Just (let (wd, r) = span (not . isSpace) (dropWhile isSpace b) in Editor r (reverse (takeWhile isSpace b) ++ reverse wd ++ a))
  (KRight, [Alt]) -> Just (let (sp, r) = span isSpace a; (wd, r') = span (not . isSpace) r in Editor (reverse wd ++ reverse sp ++ b) r')
  _ -> Nothing

-- the screen ------------------------------------------------------------------------------

-- | What the chat's thread tells the screen.
data Msg = MEntry Entry | MBusy (Maybe String) | MSpent Spent Double | MDone

-- | A picture as the terminal has it, or will: its number there, its columns and rows, and its bytes (a PNG in
-- base64) to send.
data Pic = Pic { pcId, pcCols, pcRows :: !Int, pcData :: !BC.ByteString }

-- | What of the pictures is not the screen's state: whether the terminal shows any, where the images are kept,
-- which it has been sent (forgotten at a resize: they are sent again as they are shown), and all it was sent.
data Gfx = Gfx { gOn :: Bool, gDir :: FilePath, gSent :: IORef (Set.Set Int), gAll :: IORef (Set.Set Int) }

picOf :: St -> String -> Maybe (Int, Int, Int)
picOf st n = (\p -> (pcId p, pcCols p, pcRows p)) <$> (M.lookup n (sPics st) >>= id)

-- | The pictures of an entry made ready: each image it names read, sized for the screen as it is now and
-- numbered -- once (one that could not be is not tried again), and only while there are numbers.
picture :: Gfx -> St -> Entry -> IO St
picture g st e
  | not (gOn g) = pure st
  | otherwise = foldM one st [ n | n <- Img.namesIn (enText e), not (M.member n (sPics st)) ]
  where
    one s n = do
      (w, h) <- fromMaybe (80, 24) <$> termSize
      cell <- fromMaybe (8, 17) <$> cellPixels
      let next = 1 + length [ () | Just _ <- M.elems (sPics s) ]
          room = (min 80 (w - 10), min 24 (h - 8))
      r <- if next > 255 || fst room < 2 || snd room < 1 then pure Nothing else Img.viewPng (gDir g) n
      pure s { sPics = M.insert n (fmap (\(dat, size) -> let (c, rows) = cellsFor cell size room in Pic next c rows dat) r) (sPics s) }

data St = St
  { sEntries :: [Entry]                 -- ^ newest first
  , sPics :: M.Map String (Maybe Pic)   -- ^ the images named so far: a picture, or none to show
  , sBack :: Int                        -- ^ lines scrolled back from the end (0: following)
  , sEdit :: Editor
  , sSent :: [String], sRecall :: Maybe (Int, String)   -- ^ lines sent, newest first; which is recalled, and the line it replaced
  , sBusy :: Maybe String
  , sSpent :: Maybe (Spent, Double)
  , sTurns :: Int
  , sStatus :: Json, sStatusAt :: Double
  , sDone :: Bool
  , sTicks :: Int
  }

-- | The chat on the screen: @chat@ is run on a thread with a 'Ui' that draws here and the queue the lines
-- typed go on; the answer is the chat's exit code, or 0 when the screen was left first.
chatTui :: String -> String -> FilePath -> (Ui -> TQueue (Maybe T.Text) -> IO Int) -> IO Int
chatTui name model dir chat = do
  pending <- newTQueueIO
  inbox <- newTQueueIO
  wakeR <- newIORef (pure ())
  codeR <- newIORef Nothing
  old <- getTerminalAttributes stdInput
  chatTidVar <- newEmptyMVar
  stopR <- newIORef (pure False)
  wanted <- lookupEnv "GHS_IMAGES"
  byName <- graphicsTerm
  gfx <- Gfx (case wanted of { Just "0" -> False; Just "1" -> True; _ -> byName }) (dir </> "images") <$> newIORef Set.empty <*> newIORef Set.empty
  let post m = atomically (writeTQueue inbox m) >> (readIORef wakeR >>= id)
      entry k t = post (MEntry (Entry k t))
      ui = Ui { uiView = entry "view", uiTalk = entry "talk", uiThought = entry "think"
              , uiCall = \n args -> entry "tool" (T.pack (n ++ " " ++ args)), uiAnswer = entry "echo"
              , uiNote = entry "note" . T.pack . unbracket, uiBusy = post . MBusy, uiSpent = \s secs -> post (MSpent s secs)
              , uiDone = post MDone, uiLeave = forgetAll gfx >> leave old, uiOnStop = writeIORef stopR }
      start ev = do
        writeIORef wakeR (wake ev)
        tid <- forkIO $ do
          r <- try (chat ui pending)
          case r of
            Right code -> writeIORef codeR (Just code)
            Left (err :: SomeException) -> entry "note" (T.pack ("chat: " ++ show err))
          post MDone
        putMVar chatTidVar tid
        t <- now
        pure St { sEntries = [], sPics = M.empty, sBack = 0, sEdit = editor, sSent = [], sRecall = Nothing, sBusy = Nothing, sSpent = Nothing, sTurns = 0
                , sStatus = JObj [], sStatusAt = t - 10, sDone = False, sTicks = 0 }
  _ <- runApp App { appTick = 0.5, appDraw = draw gfx name model, appEvent = \ev st -> event gfx (readIORef stopR >>= id) dir pending inbox ev st >>= maybe (forgetAll gfx >> pure Nothing) (pure . Just) } start
  atomically (writeTQueue pending Nothing)
  chatTid <- tryReadMVar chatTidVar
  maybe (pure ()) killThread chatTid
  fromMaybe 0 <$> readIORef codeR
  where
    unbracket s = case (s, reverse s) of { ('[' : r, ']' : _) | '\n' `notElem` s -> init r; _ -> s }
    -- (the terminal is not left holding what it was sent)
    forgetAll g = do
      ids <- readIORef (gAll g)
      unless (Set.null ids) (hPutStr stdout (concatMap forget (Set.toList ids)) >> hFlush stdout)
      writeIORef (gAll g) Set.empty >> writeIORef (gSent g) Set.empty
    leave :: TerminalAttributes -> IO ()
    leave old = do
      hPutStr stdout "\ESC[0m\ESC[?25h\ESC[?1049l"
      hFlush stdout
      setTerminalAttributes stdInput old Immediately

event :: Gfx -> IO Bool -> FilePath -> TQueue (Maybe T.Text) -> TQueue Msg -> Event -> St -> IO (Maybe St)
event gfx stop dir pending inbox ev st = case ev of
  EvWake -> do
    ms <- atomically (flush inbox)
    Just <$> foldM (picture gfx) (foldl apply st ms) [ e | MEntry e <- ms ]
  EvTick -> do
    t <- now
    -- (what was said from this thread itself -- a key's own doing, as a stop's note is -- woke nobody: it is taken here)
    ms <- atomically (flush inbox)
    st0 <- foldM (picture gfx) (foldl apply st ms) [ e | MEntry e <- ms ]
    st' <- if t - sStatusAt st < 2 then pure st0 else do
      status <- maybe (JObj []) (either (const (JObj [])) id . parseJson) <$> readFileMaybe (dir </> "status.json")
      pure st0 { sStatus = status, sStatusAt = t }
    pure (Just st' { sTicks = sTicks st + 1 })
  EvResize -> writeIORef (gSent gfx) Set.empty >> pure (Just st)
  EvKey kp -> key kp
  where
    flush q = do
      m <- tryReadTQueue q
      case m of { Just x -> (x :) <$> flush q; Nothing -> pure [] }
    apply s m = case m of
      MEntry e -> s { sEntries = e : take 4000 (sEntries s) }
      MBusy b -> s { sBusy = b }
      MSpent sp secs -> s { sSpent = Just (sp, secs), sTurns = sTurns s + 1 }
      MDone -> s { sDone = True, sBusy = Nothing }
    -- scrolled by @d@ lines, no further back than the transcript goes on this screen
    scroll d = do
      (w, h) <- fromMaybe (80, 24) <$> termSize
      let (back, _) = scrollLinesWith (picOf st) (w - 1) (max 1 (h - 4)) (max 0 (sBack st + d)) (sEntries st)
      pure (Just st { sBack = back })
    page = do
      (_, h) <- fromMaybe (80, 24) <$> termSize
      pure (max 1 (h - 5))
    key kp = case (kKey kp, kMods kp) of
      (KChar 'c', [Ctrl]) -> atomically (writeTQueue pending Nothing) >> pure Nothing
      (KChar 'd', [Ctrl]) | null (editText (sEdit st)) -> atomically (writeTQueue pending Nothing) >> pure Nothing
      (KEnter, _) -> do
        let line = dropWhileEnd isSpace (editText (sEdit st))
        if all isSpace line || sDone st then pure (Just st) else do
          atomically (writeTQueue pending (Just (T.pack line)))
          pure (Just st { sEntries = Entry "user" (T.pack line) : sEntries st, sEdit = editor, sSent = line : sSent st, sRecall = Nothing, sBack = 0 })
      (KUp, []) -> pure (Just (recall 1))
      (KDown, []) -> pure (Just (recall (-1)))
      (KPgUp, _) -> page >>= scroll
      (KPgDn, _) -> page >>= scroll . negate
      (KUp, [Ctrl]) -> scroll 1
      (KDown, [Ctrl]) -> scroll (-1)
      (KEnd, _) | null (editText (sEdit st)) -> pure (Just st { sBack = 0 })
      -- (Esc: while the agent works, the turn is stopped; else the line is cleared)
      (KEsc, _) | Just _ <- sBusy st -> stop >> pure (Just st)
      -- (with nothing typed, the subagents still at work when the agent is not are what it stops)
      (KEsc, _) | null (editText (sEdit st)) -> stop >> pure (Just st)
      (KEsc, _) -> pure (Just st { sEdit = editor, sRecall = Nothing })
      _ -> pure (Just (maybe st (\e -> st { sEdit = e, sRecall = Nothing }) (editKey kp (sEdit st))))
    -- a line sent before, in place of the one being typed (which comes back below the oldest)
    recall d = case sRecall st of
      Nothing | d > 0, l : _ <- sSent st -> st { sEdit = fromText l, sRecall = Just (0, editText (sEdit st)) }
      Just (i, typed) | i + d < 0 -> st { sEdit = fromText typed, sRecall = Nothing }
                      | i + d < length (sSent st) -> st { sEdit = fromText (sSent st !! (i + d)), sRecall = Just (i + d, typed) }
      _ -> st

draw :: Gfx -> String -> String -> (Int, Int) -> St -> IO ([Put], Maybe (Int, Int))
draw gfx name model (w, h) st = do
  -- a picture that comes onto the screen is sent to the terminal first, if it has not been
  sent <- readIORef (gSent gfx)
  let onScreen = Set.fromList [ n | l <- shown, (Style { sFg = Ansi n }, t) <- l, isPlaceholder t ]
  forM_ [ p | Just p <- M.elems (sPics st), pcId p `Set.member` onScreen, not (pcId p `Set.member` sent) ] $ \p -> do
    hPutStr stdout (transmit (pcId p) (pcCols p, pcRows p) (pcData p))
    modifyIORef' (gSent gfx) (Set.insert (pcId p)) >> modifyIORef' (gAll gfx) (Set.insert (pcId p))
  pure (top ++ body ++ bottom, Just (cx, h - 1))
  where
    verdict = fromMaybe "no session running (ghci-session start)" (lookupStr "text" (sStatus st))
    facts = case sSpent st of
      Nothing -> printf "%d turns" (sTurns st)
      Just (sp, secs) -> printf "%d turn%s; the last: %s" (sTurns st) (if sTurns st == 1 then "" else "s" :: String) (drop 1 (init (spentLine sp secs)))
    top = spansLine w 0 [ (stBold, " ghci-session chat "), (plain, name ++ "  "), (stDim, model ++ "  "), (verdictStyle verdict, verdict) ]
       ++ spansLine w 1 [ (stDim, " " ++ facts) ]
    rows = max 1 (h - 4)
    (_, shown) = scrollLinesWith (picOf st) (w - 1) rows (sBack st) (sEntries st)
    body = concat [ spansLine w (2 + i) ((plain, " ") : l) | (i, l) <- zip [0 ..] shown ]
    spin = "-\\|/" !! (sTicks st `mod` 4)
    status
      | sDone st = [ (stYellow, " the chat ended; Ctrl-C leaves") ]
      | Just b <- sBusy st = [ (stYellow, ' ' : spin : ' ' : b), (stDim, "  (Esc stops the turn; a line typed now reaches the agent between tool calls)") ]
      | otherwise = [ (stDim, " waiting for a line; Enter sends, Up recalls, PgUp/PgDn scroll, Ctrl-C leaves") ]
    scrolled = if sBack st > 0 then [ (stHi, printf " %d lines back; End follows " (sBack st)) ] else []
    bottom = spansLine w (h - 2) (scrolled ++ status) ++ spansLine w (h - 1) [ (stBold, "> "), (plain, window) ]
    before = reverse (edBefore (sEdit st))
    text = editText (sEdit st)
    room = max 1 (w - 3)
    off = max 0 (length before - room)
    window = take room (drop off text)
    cx = 2 + length before - off
