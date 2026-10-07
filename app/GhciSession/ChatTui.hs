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
-- the line is edited with Left, Right, Home, End, Backspace, Delete, Ctrl-A/E/U/K/W; Esc clears it;
-- Ctrl-C leaves (Ctrl-D on an empty line too), in the middle of a turn as well -- what the turn did is
-- in the history already.
--
-- The chat runs on a thread of its own and tells the screen what happens through a 'Ui'; the screen is
-- the 'Tui.App' loop on the main thread, woken for each thing to show.
module GhciSession.ChatTui
  ( chatTui
  -- (the pure parts, for the self-tests)
  , Editor (..), editor, editText, editKey, Entry (..), entryLines, scrollLines
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
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
entryLines w (Entry kind text) = concat [ wrapSpans w 7 (lbl i ++ [(style, l)]) | (i, l) <- zip [0 :: Int ..] body ] ++ [ [] | kind `elem` ["user", "talk"] ]
  where
    ls = map T.unpack (T.lines text)
    body = if null ls then [""] else ls
    lbl i = [ (kindStyle kind, if i == 0 then take 7 (kind ++ ":      ") else "       ") ]
    style = case kind of { "think" -> stDim; "view" -> stDim; "note" -> stDim; _ -> plain }

-- | The lines of the transcript to show on @rows@ rows, @back@ lines before the end (as many as there are:
-- the number actually scrolled back is returned); the entries newest first, only as many as it takes.
scrollLines :: Int -> Int -> Int -> [Entry] -> (Int, [[Span]])
scrollLines w rows back entries = (back', drop (length gathered - back' - rows) (take (length gathered - back') gathered))
  where
    wanted = back + rows
    gathered = gather 0 entries []
    gather n (e : es) acc | n < wanted = let ls = entryLines w e in gather (n + length ls) es (ls ++ acc)
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

data St = St
  { sEntries :: [Entry]                 -- ^ newest first
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
  let post m = atomically (writeTQueue inbox m) >> (readIORef wakeR >>= id)
      entry k t = post (MEntry (Entry k t))
      ui = Ui { uiView = entry "view", uiTalk = entry "talk", uiThought = entry "think"
              , uiCall = \n args -> entry "tool" (T.pack (n ++ " " ++ args)), uiAnswer = entry "echo"
              , uiNote = entry "note" . T.pack . unbracket, uiBusy = post . MBusy, uiSpent = \s secs -> post (MSpent s secs)
              , uiDone = post MDone, uiLeave = leave old }
      start ev = do
        writeIORef wakeR (wake ev)
        _ <- forkIO $ do
          r <- try (chat ui pending)
          case r of
            Right code -> writeIORef codeR (Just code)
            Left (err :: SomeException) -> entry "note" (T.pack ("chat: " ++ show err))
          post MDone
        t <- now
        pure St { sEntries = [], sBack = 0, sEdit = editor, sSent = [], sRecall = Nothing, sBusy = Nothing, sSpent = Nothing, sTurns = 0
                , sStatus = JObj [], sStatusAt = t - 10, sDone = False, sTicks = 0 }
  _ <- runApp App { appTick = 0.5, appDraw = draw name model, appEvent = event dir pending inbox } start
  fromMaybe 0 <$> readIORef codeR
  where
    unbracket s = case (s, reverse s) of { ('[' : r, ']' : _) -> init r; _ -> s }
    leave :: TerminalAttributes -> IO ()
    leave old = do
      hPutStr stdout "\ESC[0m\ESC[?25h\ESC[?1049l"
      hFlush stdout
      setTerminalAttributes stdInput old Immediately

event :: FilePath -> TQueue (Maybe T.Text) -> TQueue Msg -> Event -> St -> IO (Maybe St)
event dir pending inbox ev st = case ev of
  EvWake -> do
    ms <- atomically (flush inbox)
    pure (Just (foldl apply st ms))
  EvTick -> do
    t <- now
    st' <- if t - sStatusAt st < 2 then pure st else do
      status <- maybe (JObj []) (either (const (JObj [])) id . parseJson) <$> readFileMaybe (dir </> "status.json")
      pure st { sStatus = status, sStatusAt = t }
    pure (Just st' { sTicks = sTicks st + 1 })
  EvResize -> pure (Just st)
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
      let (back, _) = scrollLines (w - 1) (max 1 (h - 4)) (max 0 (sBack st + d)) (sEntries st)
      pure (Just st { sBack = back })
    page = do
      (_, h) <- fromMaybe (80, 24) <$> termSize
      pure (max 1 (h - 5))
    key kp = case (kKey kp, kMods kp) of
      (KChar 'c', [Ctrl]) -> pure Nothing
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
      (KEsc, _) -> pure (Just st { sEdit = editor, sRecall = Nothing })
      _ -> pure (Just (maybe st (\e -> st { sEdit = e, sRecall = Nothing }) (editKey kp (sEdit st))))
    -- a line sent before, in place of the one being typed (which comes back below the oldest)
    recall d = case sRecall st of
      Nothing | d > 0, l : _ <- sSent st -> st { sEdit = fromText l, sRecall = Just (0, editText (sEdit st)) }
      Just (i, typed) | i + d < 0 -> st { sEdit = fromText typed, sRecall = Nothing }
                      | i + d < length (sSent st) -> st { sEdit = fromText (sSent st !! (i + d)), sRecall = Just (i + d, typed) }
      _ -> st

draw :: String -> String -> (Int, Int) -> St -> IO ([Put], Maybe (Int, Int))
draw name model (w, h) st = pure (top ++ body ++ bottom, Just (cx, h - 1))
  where
    verdict = fromMaybe "no session running (ghci-session start)" (lookupStr "text" (sStatus st))
    facts = case sSpent st of
      Nothing -> printf "%d turns" (sTurns st)
      Just (sp, secs) -> printf "%d turn%s; the last: %s" (sTurns st) (if sTurns st == 1 then "" else "s" :: String) (drop 1 (init (spentLine sp secs)))
    top = spansLine w 0 [ (stBold, " ghci-session chat "), (plain, name ++ "  "), (stDim, model ++ "  "), (verdictStyle verdict, verdict) ]
       ++ spansLine w 1 [ (stDim, " " ++ facts) ]
    rows = max 1 (h - 4)
    (_, shown) = scrollLines (w - 1) rows (sBack st) (sEntries st)
    body = concat [ spansLine w (2 + i) ((plain, " ") : l) | (i, l) <- zip [0 ..] shown ]
    spin = "-\\|/" !! (sTicks st `mod` 4)
    status
      | sDone st = [ (stYellow, " the chat ended; Ctrl-C leaves") ]
      | Just b <- sBusy st = [ (stYellow, ' ' : spin : ' ' : b), (stDim, "  (a line typed now reaches the agent between tool calls)") ]
      | otherwise = [ (stDim, " waiting for a line; Enter sends, Up recalls, PgUp/PgDn scroll, Ctrl-C leaves") ]
    scrolled = if sBack st > 0 then [ (stHi, printf " %d lines back; End follows " (sBack st)) ] else []
    bottom = spansLine w (h - 2) (scrolled ++ status) ++ spansLine w (h - 1) [ (stBold, "> "), (plain, window) ]
    before = reverse (edBefore (sEdit st))
    text = editText (sEdit st)
    room = max 1 (w - 3)
    off = max 0 (length before - room)
    window = take room (drop off text)
    cx = 2 + length before - off
