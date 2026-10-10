{-# LANGUAGE ScopedTypeVariables #-}
-- | __Watching a session, and working in it__: @ghci-session top [SESSION]@, a screen that follows a session as
-- it works -- its verdict, memory and servers in the header; below, a tab at a time: the history as the daemon
-- writes it (every request and verdict, a save and its diff, the chat's words), the view the model reads with
-- the memory's numbers, the daemon's log, the verdict with what is behind it (the compiler's diagnostics, the
-- failing tests, the members and servers), what the model calls cost -- and two terminal PANES, the chat and a
-- shell, running inside the monitor on their own pseudo-terminals. Half a second between looks; a session that
-- is not running shows as such and is picked up when it starts.
--
-- It is an application of the @ghostty-tui@ library ("Tui"): the frame is cells drawn by difference, the
-- panes are "Tui.Pane" -- a program on a pseudo-terminal whose screen libghostty-vt keeps ("Ghostty.Vt"),
-- loaded at run time; without the library the other tabs work and the pane tabs say what is missing.
--
-- Keys: @1@-@7@ or @h v l d u c s@ for a tab, @j@ @k@ and the arrows, @PgUp@ @PgDn@ @g@ @G@ to scroll, @f@ to
-- follow new lines again, @R@ reload, @T@ test, @r@ look now, @q@ quit. In a pane every key goes to the
-- program; @Ctrl-a@ first makes the next key the monitor's (@Ctrl-a 1@: the history, @Ctrl-a q@: quit,
-- @Ctrl-a a@: a Ctrl-a for the program).
module GhciSession.Top (topMain, Span, spansLine, wrapSpans, visible, kindStyle, verdictStyle, stBold, stDim, stYellow, stHi) where

import Control.Concurrent (forkIO)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (forM, forM_, unless, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit)
import Data.IORef
import Data.List (dropWhileEnd, intercalate, isInfixOf, isPrefixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified GhciSession.Inbox as Inbox
import qualified GhciSession.Know as K
import qualified GhciSession.Roll as Roll
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (exitWith, ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO
import Text.Printf (printf)

import qualified Ghostty.Vt as Vt
import Tui
import GhciSession.Config
import GhciSession.Json
import GhciSession.Md (mdLines, outputLines)
import qualified GhciSession.Mcp as Mcp
import GhciSession.Sys (now, readFileMaybe)
import GhciSession.Usage (usageRows, usageTable)
import GhciSession.Quota (windowLines)

-- | A run of text with its style.
type Span = (Style, String)

visible :: [Span] -> Int
visible = sum . map (putWidth . snd)

-- | A line of spans at a row, the frame's width wide.
spansLine :: Int -> Int -> [Span] -> [Put]
spansLine w y = textLine 0 y w

-- | A long line as several of at most @w@ columns, the continuation lines indented.
wrapSpans :: Int -> Int -> [Span] -> [[Span]]
wrapSpans w indent spans
  | visible spans <= w || w <= indent + 1 = [spans]
  | otherwise = let (a, b) = splitAt' w spans in a : map (\l -> ((plain, replicate indent ' ') : l)) (wrapSpans (w - indent) 0 b)
  where
    splitAt' _ [] = ([], [])
    splitAt' n ((st, t) : r)
      | putWidth t <= n = let (a, b) = splitAt' (n - putWidth t) r in ((st, t) : a, b)
      | otherwise = let (h, rest) = takeCols n t in ([(st, h)], (st, rest) : r)
    takeCols n t = go 0 t where go _ [] = ([], [])
                                go used (c : cs) | used + charWidth c > n = ([], c : cs)
                                                 | otherwise = let (a, b) = go (used + charWidth c) cs in (c : a, b)

-- styles -----------------------------------------------------------------------------

stBold, stDim, stGreen, stRed, stYellow, stBlue, stCyan, stMagenta, stHi :: Style
stBold = bold plain
stDim = dim plain
stGreen = bold (withFg (Ansi 2) plain)
stRed = bold (withFg (Ansi 1) plain)
stYellow = bold (withFg (Ansi 3) plain)
stBlue = bold (withFg (Ansi 4) plain)
stCyan = bold (withFg (Ansi 6) plain)
stMagenta = bold (withFg (Ansi 5) plain)
stHi = inverse plain

-- the state ----------------------------------------------------------------------------

data Tab = THistory | TView | TLog | TVerdict | TUsage | THeap | TChat | TShell | TKnow
  deriving (Eq, Ord, Show, Enum, Bounded)

tabName :: Tab -> String
tabName t = case t of { THistory -> "1 history"; TView -> "2 view"; TLog -> "3 log"; TVerdict -> "4 verdict"; TUsage -> "5 usage"; THeap -> "6 heap"; TChat -> "7 chat"; TShell -> "8 shell"; TKnow -> "9 known" }

isPane :: Tab -> Bool
isPane t = t == TChat || t == TShell

data Msg = Msg { mId :: Int, mTime :: String, mKind :: String, mText :: String }

data St = St
  { sTab :: Tab
  , sScroll :: M.Map Tab Int          -- ^ the first line shown, per tab
  , sFollow :: M.Map Tab Bool         -- ^ stays at the end as lines are added
  , sStatus :: Json                   -- ^ status.json
  , sInfo :: Maybe Json               -- ^ the daemon's @info@, when it answers
  , sHist :: [Msg]                    -- ^ the last messages, oldest first
  , sLog :: [String], sLogSize :: Integer
  , sView :: [String], sMem :: Json, sViewAt :: Double
  , sUsage :: [String], sUsageAt :: Double
  , sNote :: String                   -- ^ the bottom line's message
  , sSize :: (Int, Int)               -- ^ columns, rows
  , sPanes :: M.Map Tab Pane
  , sVtErr :: Maybe String            -- ^ why there are no panes
  , sPrefix :: Bool                   -- ^ Ctrl-a was pressed: the next key is the monitor's
  , sOpen :: S.Set Int                -- ^ the history's messages shown whole (the others are cut at 'cutAt' lines)
  , sAllOpen :: Bool                  -- ^ every message shown whole
  , sCur :: Maybe Int                 -- ^ the history's current message (none: the last)
  , sMemHist :: [(Double, Int, Int)]  -- ^ the resident memory of the repl and the servers, MB, at each look; newest first
  , sHeap :: Maybe Heap               -- ^ the last report of the heap
  , sHeapBusy :: Maybe String         -- ^ a report being taken: which
  , sWake :: IO ()                    -- ^ what a pane calls when it has something to show
  , sInput :: Maybe String            -- ^ a line being written for the session's chat
  , sKnow :: [String], sKnowAt :: Double   -- ^ what is known by subject ("GhciSession.Know"), and when it was read
  , sLaid :: M.Map Int (Int, Bool, [[Span]])  -- ^ the history's messages laid out: for which width, open or cut, the lines
  , sChat :: [String]                  -- ^ the chat's turn (turn.json) and the rollover controller's state (history/roll.json)
  }

-- | A report of the heap: which, when it was taken and how long it took, its lines.
data Heap = Heap { hMode :: String, hAt :: String, hSecs :: Double, hLines :: [String] }

data Env = Env { eConf :: Conf, eName :: String, eDir :: FilePath
               , eHeap :: IORef (Maybe Heap) }      -- ^ a report taken on a thread, until the screen takes it

topMain :: Conf -> Maybe String -> IO ()
topMain conf mname = do
  name <- Mcp.pick conf mname >>= either (\e -> hPutStrLn stderr ("top: " ++ e) >> exitWith (ExitFailure 2)) pure
  term <- hIsTerminalDevice stdin
  unless term (hPutStrLn stderr "top: standard input is not a terminal" >> exitWith (ExitFailure 2))
  vtErr <- loadVt
  let dir = cStateDir conf </> name
  heapBox <- newIORef Nothing
  let env = Env conf name dir heapBox
      st0 = St THistory M.empty (M.fromList [ (t, t /= THeap && t /= TKnow) | t <- [minBound ..] ]) (JObj []) Nothing [] [] 0 [] (JObj []) 0 [] 0 "" (80, 24) M.empty vtErr False S.empty False Nothing [] Nothing Nothing (pure ()) Nothing [] 0 M.empty []
  stEnd <- runApp App { appTick = 0.5, appDraw = draw name, appEvent = event env } (\ev -> refresh env st0 { sWake = wake ev })
  mapM_ paneHangup (M.elems (sPanes stEnd))

-- | Load libghostty-vt: @GHS_LIBGHOSTTY@, else beside the executable, else the system's. The reason when not.
loadVt :: IO (Maybe String)
loadVt = do
  env <- lookupEnv "GHS_LIBGHOSTTY"
  exe <- getExecutablePath
  beside <- filterIO [takeDirectory exe </> "libghostty-vt.so", takeDirectory exe </> "libghostty-vt.dylib"]
  r <- Vt.load (fromMaybe (case beside of { (p : _) -> p; [] -> "" }) env)
  pure (either Just (const Nothing) r)
  where filterIO = fmap concat . mapM (\p -> (\b -> [p | b]) <$> doesFileExist p)

-- | An event: a key, or a look at the session.
event :: Env -> Event -> St -> IO (Maybe St)
event env e st = case e of
  EvTick -> Just . laid <$> (refresh env st >>= panes env)
  EvResize -> Just . laid <$> (refresh env st >>= panes env)
  EvWake -> Just . laid <$> (takeHeap env st >>= panes env)
  EvKey kp -> do
    r <- key env kp st
    case r of
      Nothing -> pure Nothing
      Just (st', again) -> Just . laid <$> ((if again then refresh env st' else pure st') >>= panes env)

-- | A key's effect, and whether a fresh look follows; 'Nothing' to quit.
key :: Env -> KeyPress -> St -> IO (Maybe (St, Bool))
key env kp@(KeyPress k _ bytes) st
  -- a line for the session's chat: Enter leaves it in the chat's inbox ("GhciSession.Inbox"), Esc drops it
  | Just t <- sInput st, not (isPane (sTab st)) = case k of
      KEsc -> pure (Just (st { sInput = Nothing, sNote = "" }, False))
      KChar 'c' | Ctrl `elem` kMods kp -> pure (Just (st { sInput = Nothing, sNote = "" }, False))
      KChar 'u' | Ctrl `elem` kMods kp -> pure (Just (st { sInput = Just "" }, False))
      KBackspace -> pure (Just (st { sInput = Just (take (length t - 1) t) }, False))
      KEnter | null (words t) -> pure (Just (st { sInput = Nothing }, False))
             | otherwise -> do
                 r <- Inbox.send (eConf env) (eName env) t
                 pure (Just (case r of
                   Left why -> st { sNote = "not sent: " ++ why }
                   Right _ -> st { sInput = Nothing, sNote = "sent to the chat", sFollow = M.insert THistory True (sFollow st) }, True))
      -- (what is typed, and what is pasted: the printable characters of the bytes)
      _ | not (Ctrl `elem` kMods kp), s@(_ : _) <- [ c | KChar c <- [k], c >= ' ', c /= '\DEL' ] -> pure (Just (st { sInput = Just (t ++ s) }, False))
      _ -> pure (Just (st, False))
  | sPrefix st || not (isPane (sTab st)) = case k of
      KChar 'i' | not (isPane (sTab st)) -> pure (Just (st' { sInput = Just "", sNote = "" }, False))
      _ | sPrefix st && ctrlA -> withPane (\p -> paneInput p (BC.pack "\SOH")) >> pure (Just (st', False))
      KChar 'q' -> pure Nothing
      -- (Ctrl-C comes as the letter with the modifier: as the character it never matched, and did nothing)
      KChar 'c' | Ctrl `elem` kMods kp, not (isPane (sTab st)) -> pure Nothing
      KEsc | not (isPane (sTab st)) -> pure Nothing
      KChar c | Just t <- lookup c tabKeys -> pure (Just (st' { sTab = t }, True))
      KChar 'j' -> scrollBy 1
      KDown -> scrollBy 1
      KChar 'k' -> scrollBy (-1)
      KUp -> scrollBy (-1)
      KPgDn -> scrollBy (page - 1)
      KChar ' ' -> scrollBy (page - 1)
      KPgUp -> scrollBy (1 - page)
      KChar 'g' -> pure (Just (st' { sScroll = M.insert (sTab st) 0 (sScroll st), sFollow = M.insert (sTab st) False (sFollow st) }, False))
      KHome -> key env kp { kKey = KChar 'g' } st
      KChar 'G' -> pure (Just (follow True, False))
      KEnd -> pure (Just (follow True, False))
      KChar 'f' -> pure (Just (follow True, False))
      KChar 'r' -> pure (Just (st', True))
      KChar 'n' | history -> pure (Just (moveCur 1, False))
      KTab | history -> pure (Just (moveCur 1, False))
      KChar 'p' | history -> pure (Just (moveCur (-1), False))
      KChar 'N' | history -> pure (Just (moveCur (-1), False))
      KEnter | history -> pure (Just (toggle, False))
      KChar 'o' | history -> pure (Just (toggle, False))
      KChar 'x' | history -> pure (Just (toggle, False))
      KChar 'a' | history -> pure (Just (showCur st' { sAllOpen = not (sAllOpen st), sOpen = S.empty }, False))
      KChar c | sTab st == THeap, Just mode <- lookup c heapKeys -> heapReport mode
      KChar 'R' -> act "reload" [] >> pure (Just (st' { sNote = "reload sent" }, True))
      KChar 'T' -> act "check" [] >> pure (Just (st' { sNote = "test sent" }, True))
      KEnter | isPane (sTab st) -> restartDead
      _ -> pure (Just (st', False))
  | ctrlA = pure (Just (st { sPrefix = True }, False))
  | otherwise = do
      dead <- maybe (pure Nothing) paneStatus (M.lookup (sTab st) (sPanes st))
      case (dead, k) of
        (Just _, KEnter) -> restartDead
        _ -> withPane (\p -> paneKey p kp) >> pure (Just (st, False))
  where
    ctrlA = bytes == BC.pack "\SOH"
    st' = st { sPrefix = False }
    tabKeys = [('1', THistory), ('h', THistory), ('2', TView), ('v', TView), ('3', TLog), ('l', TLog), ('4', TVerdict), ('d', TVerdict), ('5', TUsage), ('u', TUsage), ('6', THeap), ('m', THeap), ('7', TChat), ('c', TChat), ('8', TShell), ('s', TShell), ('9', TKnow)]
    heapKeys = [('M', "mem"), ('C', "cafs"), ('S', "strings"), ('K', "kept"), ('D', "dups")]
    -- a report of the heap, on a thread (a major collection and a walk of the heap: a second or more, the
    -- session paused); the screen is woken when it is in
    heapReport mode = case sHeapBusy st of
      Just _ -> pure (Just (st' { sNote = "a report is being taken" }, False))
      Nothing -> do
        _ <- forkIO $ do
          t0 <- now
          r <- try (Mcp.request (eConf env) (eName env) (JObj [("op", JStr "census"), ("mode", JStr mode), ("top", JNum 12)])) :: IO (Either SomeException Json)
          t1 <- now
          at <- formatTime defaultTimeLocale "%H:%M:%S" <$> utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac t1))
          let ls = case r of
                Right j -> lines (maybe "" T.unpack (lookupText "out" j))
                Left e -> ["the request failed: " ++ show e]
          writeIORef (eHeap env) (Just (Heap mode at (t1 - t0) ls))
          sWake st
        pure (Just (st' { sHeapBusy = Just mode, sScroll = M.insert THeap 0 (sScroll st) }, False))
    page = max 1 (snd (sSize st) - 4)
    follow on = st' { sFollow = M.insert (sTab st) on (sFollow st) }
    history = sTab st == THistory && not (null (sHist st))
    -- the current message, as an index into the history
    curIx = case sCur st of
      Just i | Just ix <- lookup i (zip (map mId (sHist st)) [0 ..]) -> ix
      _ -> length (sHist st) - 1
    moveCur d = let ix = max 0 (min (length (sHist st) - 1) (curIx + d))
                    s1 = st' { sCur = Just (mId (sHist st !! ix)) }
                in if ix == length (sHist st) - 1 then s1 { sFollow = M.insert THistory True (sFollow st) } else showCur s1
    toggle = let i = mId (sHist st !! curIx)
                 s1 = st' { sCur = Just i, sOpen = (if S.member i (sOpen st) then S.delete i else S.insert i) (sOpen st) }
             in if M.findWithDefault True THistory (sFollow st) && curIx == length (sHist st) - 1 then s1 else showCur s1
    -- scrolled so the current message's first line is on the screen, the end no longer followed
    showCur s = let starts = scanl (+) 0 [ length (historyLines (fst (sSize st)) s m) | m <- sHist s ]
                    ix = case sCur s of { Just i | Just x <- lookup i (zip (map mId (sHist s)) [0 ..]) -> x; _ -> length (sHist s) - 1 }
                    start = starts !! ix
                    off = M.findWithDefault 0 THistory (sScroll s)
                    following = M.findWithDefault True THistory (sFollow s)
                    total = last starts
                    off0 = if following then max 0 (total - page) else off
                    off' | start < off0 = start
                         | start >= off0 + page = start - page + 1
                         | otherwise = off0
                in s { sScroll = M.insert THistory off' (sScroll s), sFollow = M.insert THistory False (sFollow s) }
    scrollBy d = pure (Just (st' { sScroll = M.insertWith (+) (sTab st) d (sScroll st), sFollow = M.insert (sTab st) False (sFollow st) }, False))
    act op args = void (forkIO (void (try (Mcp.request (eConf env) (eName env) (JObj (("op", JStr op) : args))) :: IO (Either SomeException Json))))
    withPane f = forM_ (M.lookup (sTab st) (sPanes st)) f
    restartDead = case M.lookup (sTab st) (sPanes st) of
      Just p -> do
        dead <- paneStatus p
        case dead of
          Just _ -> freePane p >> pure (Just (st' { sPanes = M.delete (sTab st) (sPanes st) }, False))
          Nothing -> pure (Just (st', False))
      Nothing -> pure (Just (st', False))

-- | The pane of the current tab exists and has the body's size.
panes :: Env -> St -> IO St
panes env st
  | not (isPane (sTab st)) || isJust (sVtErr st) = pure st
  | otherwise = do
      let (w, h) = sSize st
          size = (w, max 1 (h - 4))
      case M.lookup (sTab st) (sPanes st) of
        Just p -> paneResize p size >> pure st
        Nothing -> do
          exe <- getExecutablePath
          shell <- fromMaybe "/bin/sh" <$> lookupEnv "SHELL"
          let cmd = if sTab st == TChat then shq exe ++ " chat --tui -s " ++ shq (eName env) else shell
          r <- newPane cmd (cRoot (eConf env)) size (sWake st)
          pure $ case r of
            Right p -> st { sPanes = M.insert (sTab st) p (sPanes st) }
            Left e -> st { sNote = e }
  where shq x = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) x ++ "'"

-- | The chat's turn and the rollover controller, as the usage tab shows them: from @turn.json@ and @history/roll.json@.
chatLines :: Env -> IO [String]
chatLines env = do
  up <- isJust <$> Inbox.running (eConf env) (eName env)
  tj <- fromMaybe (JObj []) <$> Inbox.readTurn (eConf env) (eName env)
  rj <- readFileMaybe (eDir env </> "history" </> "roll.json")
  pure (Inbox.turnLines up tj ++ maybe ["rollover: no state yet (the chat keeps it in history/roll.json)"] (either (const []) (Roll.rollLines (rolloverRatioOf (eConf env) (eName env))) . parseJson) rj)

-- | A look at everything the screen shows: the status file, the daemon's info and its log, the history's
-- new messages; the view and the ledger only on their tabs, and not every time.
refresh :: Env -> St -> IO St
refresh env st = do
  size <- fromMaybe (80, 24) <$> termSize
  status <- maybe (JObj []) (either (const (JObj [])) id . parseJson) <$> readFileMaybe (eDir env </> "status.json")
  info <- answer "info" []
  chatInfo <- chatLines env
  let lastId = case sHist st of { [] -> -1; ms -> mId (last ms) }
  hj <- answer "history" ([("json", JBool True), ("n", JNum 400)] ++ [ ("since", JNum (fromIntegral (lastId + 1))) | lastId >= 0 ])
  newMsgs <- forM (maybe [] (\j -> case j of { JArr xs -> xs; _ -> [] }) hj) $ \j -> do
    tm <- stamp (fromMaybe 0 (lookupNum "date" j))
    pure (Msg (maybe 0 round (lookupNum "i" j)) tm (fromMaybe "?" (lookupStr "kind" j)) (maybe "" T.unpack (lookupText "text" j)))
  let hist = lastN 2000 (sHist st ++ [ m | m <- newMsgs, mId m > lastId ])
  (logLines, logSize) <- logTail (eDir env </> "daemon.log") (sLog st) (sLogSize st)
  t <- now
  (view, mem, viewAt) <- if sTab st == TView && t - sViewAt st >= 2
    then do
      v <- answer "view" [("json", JBool True), ("wait", JNum 0)]
      m <- answer "memory" []
      pure (maybe (sView st) (\j -> lines (maybe "" T.unpack (lookupText "view" j))) v, fromMaybe (sMem st) m, t)
    else pure (sView st, sMem st, sViewAt st)
  (usage, usageAt) <- if sTab st == TUsage && t - sUsageAt st >= 5
    then do
      rows <- usageRows (eConf env) [eName env] 0
      wins <- windowLines (map snd rows)
      pure (if null rows then ["  no model calls recorded (the chat and the compactor write usage.jsonl)"] else usageTable (eConf env) rows ++ [""] ++ wins, t)
    else pure (sUsage st, sUsageAt st)
  (know, knowAt) <- if sTab st == TKnow && t - sKnowAt st >= 3
    then do
      facts <- either (const []) id <$> (try (K.knowDir >>= K.loadFacts) :: IO (Either SomeException [K.Fact]))
      let block = K.render maxBound (takeFileName (dropWhileEnd (== '/') (cRoot (eConf env)))) facts
      pure (if null facts then ["  nothing is known yet (\"knowledge\": true in ghci-session.json keeps what the sessions establish, by subject)"]
            else (" " ++ show (length (K.current facts)) ++ " facts hold, " ++ show (length facts - length (K.current facts)) ++ " replaced") : "" : [ l | l <- lines (T.unpack block), not ("<" `isPrefixOf` l), not ("What is known" `isPrefixOf` l) ], t)
    else pure (sKnow st, sKnowAt st)
  let memHist = case info of
        Just i | Just r <- lookupNum "repl_mb" i -> take 2400 ((t, round r, maybe 0 round (lookupNum "servers_mb" i)) : sMemHist st)
        _ -> sMemHist st
  takeHeap env st { sSize = size, sStatus = status, sInfo = info, sHist = hist, sLog = logLines, sLogSize = logSize
                  , sView = view, sMem = mem, sViewAt = viewAt, sUsage = usage, sUsageAt = usageAt, sMemHist = memHist, sKnow = know, sKnowAt = knowAt, sChat = chatInfo }
  where
    answer op args = do
      r <- try (Mcp.request (eConf env) (eName env) (JObj (("op", JStr op) : args))) :: IO (Either SomeException Json)
      pure $ case r of
        Right j | lookupBool "ok" j == Just True, Just out <- lookupText "out" j, Right v <- parseJsonBS (TE.encodeUtf8 out) -> Just v
        _ -> Nothing
    stamp 0 = pure "--:--:--"
    stamp d = formatTime defaultTimeLocale "%H:%M:%S" <$> utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac d))

-- | A report of the heap that came in on its thread.
takeHeap :: Env -> St -> IO St
takeHeap env st = do
  r <- readIORef (eHeap env)
  case r of
    Nothing -> pure st
    Just h -> writeIORef (eHeap env) Nothing >> pure st { sHeap = Just h, sHeapBusy = Nothing }

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- | The log's last lines, read from where the last look ended (the whole tail when the file is new or
-- was truncated); at most a thousand kept.
logTail :: FilePath -> [String] -> Integer -> IO ([String], Integer)
logTail f old oldSize = do
  there <- doesFileExist f
  if not there then pure ([], 0) else do
    size <- either (\(_ :: IOException) -> 0) id <$> try (getFileSize f)
    if size == oldSize then pure (old, size) else do
      let from = if size < oldSize then max 0 (size - 65536) else oldSize
      r <- try (withBinaryFile f ReadMode $ \h -> do
                  hSeek h AbsoluteSeek from
                  B.hGet h (fromIntegral (size - from))) :: IO (Either IOException B.ByteString)
      let new = either (const []) (lines . T.unpack . TE.decodeUtf8With (\_ _ -> Just '?')) r
          base = if size < oldSize then [] else old
      pure (lastN 1000 (base ++ new), size)

-- drawing ---------------------------------------------------------------------------------

draw :: String -> (Int, Int) -> St -> IO ([Put], Maybe (Int, Int))
draw name (w, h) st = do
  let top = concat [ spansLine w y l | (y, l) <- zip [0 ..] (header name st w) ]
      foot = spansLine w (h - 1) (bottom st)
      body = Rect 0 3 w (max 0 (h - 4))
  (puts, cur) <- case M.lookup (sTab st) (sPanes st) of
    Just p | isPane (sTab st) -> do
      (ps, c) <- paneFrame p body
      dead <- paneStatus p
      let over = case dead of
            Just code -> spansLine w (h - 2) [ (stYellow, " [" ++ paneCommand p ++ ": ended with status " ++ show code ++ "; Enter starts it again, Ctrl-a 1 leaves] ") ]
            Nothing -> []
      pure (fillRect body plain ' ' ++ ps ++ over, if isNothing dead then c else Nothing)
    _ -> pure (concat [ spansLine w (3 + i) l | (i, l) <- zip [0 .. rH body - 1] (panelLines w st) ], Nothing)
  pure (top ++ puts ++ foot, cur)

header :: String -> St -> Int -> [[Span]]
header name st w =
  [ [ (stBold, " ghci-session top "), (plain, name ++ "  "), (verdictStyle verdict, verdict) ]
  , [ (plain, " "), (stDim, intercalate "  " facts) ]
  , [ (plain, " ") ] ++ concat [ [ (if t == sTab st then stHi else plain, " " ++ tabName t ++ " "), (plain, " ") ] | t <- [minBound ..] ] ++ [ (stDim, note) ]
  ]
  where
    j = sStatus st
    verdict = case sInfo st of
      Nothing -> "no session running (" ++ fromMaybe "stopped" (lookupStr "text" j) ++ ")"
      Just _ -> fromMaybe "?" (lookupStr "text" j)
    facts = case sInfo st of
      Nothing -> [ "ghci-session start " ++ name ]
      Just i -> [ "repl " ++ num "repl_mb" i ++ " MB", "servers " ++ num "servers_mb" i ++ " MB"
                , "serving " ++ (case strs (i .: "serving") of { [] -> "-"; xs -> unwords xs })
                , (if lookupBool "busy" i == Just True then "BUSY" else "idle " ++ num "idle_s" i ++ "s")
                , "gen " ++ num "generation" j, "stale " ++ num "stale" j, "warnings " ++ num "warnings" j ]
    num k x = maybe "?" (\d -> show (round d :: Int)) (lookupNum k x)
    note | sPrefix st = "  Ctrl-a: next key is the monitor's"
         | isPane (sTab st) = take (w - 70) "  (keys go to the program; Ctrl-a first for the monitor)"
         | sTab st == THeap || M.findWithDefault True (sTab st) (sFollow st) = ""
         | otherwise = take (w - 60) "  (scrolled: f to follow)"

verdictStyle :: String -> Style
verdictStyle v
  | any (`isPrefixOf` v) ["OK"] = stGreen
  | any (`isInfixOf` v) ["COMPILE-ERROR", "CHECK-FAIL", "DEAD", "TYPE-ERROR", "HANG", "ERROR"] = stRed
  | any (`isInfixOf` v) ["reloading", "running", "starting", "STALE"] = stYellow
  | otherwise = stBold

bottom :: St -> [Span]
bottom st
  | Just t <- sInput st, not (isPane (sTab st)) = [ (stYellow, " to the chat> "), (plain, t), (stHi, " "), (stDim, "   Enter sends  Esc drops it  "), (stYellow, sNote st) ]
  | isPane (sTab st) = [ (stDim, " Ctrl-a then: 1-9 tabs  q quit  a sends Ctrl-a   "), (stYellow, sNote st) ]
  | sTab st == THistory = [ (stDim, " q quit  1-9 tabs  j/k g/G scroll  f follow  n/p message  Enter open/close  a all  i write to the chat  R reload  T test  "), (stYellow, sNote st ++ maybe "" ("  panes: " ++) (sVtErr st)) ]
  | sTab st == THeap = [ (stDim, " q quit  1-9 tabs  j/k scroll  M the heap's figures  C CAFs  S strings  K kept  D dups  R reload  T test  "), (stYellow, sNote st ++ maybe "" ("  panes: " ++) (sVtErr st)) ]
  | otherwise = [ (stDim, " q quit  1-9 tabs  j/k PgUp/PgDn g/G scroll  f follow  R reload  T test  r look now  "), (stYellow, sNote st ++ maybe "" ("  panes: " ++) (sVtErr st)) ]

-- | The current tab's lines, from where it is scrolled.
panelLines :: Int -> St -> [[Span]]
panelLines w st = drop off ls
  where
    ls = case sTab st of
      THistory -> concatMap (historyLines w st) (sHist st)
      TView -> viewLines w st
      TLog -> [ wrapped (logLine l) | l <- sLog st ] >>= id
      TVerdict -> verdictLines w (sStatus st)
      TUsage -> [ [(stCyan, l)] | l <- sChat st ] ++ [[]] ++ [ [(plain, l)] | l <- sUsage st ]
      THeap -> heapLines w st
      TKnow -> concat [ wrapSpans w 4 [(if "## " `isPrefixOf` l then stBold else plain, l)] | l <- sKnow st ]
      _ -> [ [(stRed, "no panes: " ++ fromMaybe "?" (sVtErr st))], [], [(plain, "libghostty-vt is Ghostty's terminal emulation as a C library: tools/libghostty-vt.sh builds it into .bin/,")], [(plain, "or put a libghostty-vt.so of your own beside ghci-session, or name one in GHS_LIBGHOSTTY.")] ]
    (_, h) = sSize st
    page = max 1 (h - 4)
    following = M.findWithDefault True (sTab st) (sFollow st)
    off = if following then max 0 (length ls - page) else max 0 (min (length ls - 1) (M.findWithDefault 0 (sTab st) (sScroll st)))
    wrapped = wrapSpans w 4

-- | Lines of a message shown before it is cut.
cutAt :: Int
cutAt = 6

-- | A message's lines: its number, time and kind, then its text -- whole when it is open (or all are),
-- else its first 'cutAt' lines and how many more there are. The current message's number is marked.
historyLines :: Int -> St -> Msg -> [[Span]]
historyLines w st m = case M.lookup (mId m) (sLaid st) of
  Just (w', o, ls) | w' == w, o == open -> mark ls
  _ -> mark (layMsg w open m)
  where
    open = isOpen st m
    current = case sCur st of { Just i -> i == mId m; Nothing -> sLast st == Just (mId m) }
    -- (the current message's number is lit: the one thing of a message's lines that is not the message's own)
    mark ls = case ls of
      (((_, n) : l) : more) | current -> ((stHi, n) : l) : more
      _ -> ls

isOpen :: St -> Msg -> Bool
isOpen st m = sAllOpen st /= S.member (mId m) (sOpen st)

-- | The last message's number.
sLast :: St -> Maybe Int
sLast st = fst <$> M.lookupMax (sLaid st)

-- | The history's messages laid out once each, and again only when the width or what is open changed: a
-- screen drawn is then the lines it shows, not two thousand messages parsed and wrapped for every key.
laid :: St -> St
laid st = st { sLaid = M.fromList [ (mId m, entry m) | m <- sHist st ] }
  where
    w = fst (sSize st)
    entry m = let open = isOpen st m in case M.lookup (mId m) (sLaid st) of
      Just e@(w', o, _) | w' == w, o == open -> e
      _ -> (w, open, layMsg w open m)

layMsg :: Int -> Bool -> Msg -> [[Span]]
layMsg w open m = concatMap (wrapSpans w 4) (first : rest ++ more)
  where
    -- (the agent's words as the markdown they are, a tool's answer with its diff in colors: "GhciSession.Md")
    ls = case mKind m of
      k | k `elem` ["talk", "ai"] -> mdLines (w - 4) (mText m)
      "echo" -> outputLines (mText m)
      -- (a save is logged with what it changed)
      "tool" | "save: " `isPrefixOf` mText m -> outputLines (mText m)
      _ -> [ [(plain, l)] | l <- lines (mText m) ]
    shown = if open then ls else take cutAt ls
    first = [ (stDim, "#" ++ show (mId m)), (stDim, " " ++ mTime m ++ " "), (kindStyle (mKind m), mKind m ++ ":"), (plain, " ") ] ++ head' shown
    rest = [ (plain, "    ") : l | l <- drop 1 shown ]
    more = [ [(stDim, "    (" ++ show (length ls - cutAt) ++ " more lines; Enter opens)")] | not open, length ls > cutAt ]
    head' xs = case xs of { (x : _) -> x; [] -> [] }

kindStyle :: String -> Style
kindStyle k = case k of { "user" -> stCyan; "talk" -> stGreen; "ai" -> stGreen; "tool" -> stBlue; "echo" -> plain; "work" -> stYellow; "note" -> stMagenta; "known" -> stMagenta; _ -> stBold }

viewLines :: Int -> St -> [[Span]]
viewLines w st = [ [ (stBold, " memory  "), (plain, stats) ], [] ] ++ concatMap (wrapSpans w 6 . viewLine) (sView st)
  where
    m = sMem st
    n k = maybe "?" (\d -> show (round d :: Int)) (lookupNum k m)
    stats = intercalate "  " [ "messages " ++ n "messages", "lines " ++ n "parts", "unbuilt " ++ n "unbuilt", "built " ++ n "built"
                             , if lookupBool "settled" m == Just True then "settled" else "not settled"
                             , if lookupBool "compactor" m == Just True then "compactor on (" ++ n "busy" ++ " running, " ++ n "failed" ++ " retrying)" else "no compactor (summarize_cmd)" ]
    viewLine l = case break (== '|') l of
      (p, '|' : rest) | all (\c -> isDigit c || c == '+') p && not (null p) -> [ (stDim, p ++ "|"), (if "(not summarized yet" `isPrefixOf` rest then withFg (Ansi 3) plain else plain, rest) ]
      _ -> [ (stBold, l) ]

-- | The heap tab: the resident memory as the daemon reads it at every look, graphed (a column a look, the
-- last @w@ of them: half a second each); then the last report of the heap, and what the keys take.
heapLines :: Int -> St -> [[Span]]
heapLines w st = concat
  [ wrapped [ (stBold, "resident memory"), (stDim, printf "  repl %d MB, servers %d MB  --  the last %s: %d to %d MB" replNow serversNow span' lo hi) ]
  , graph 8 stGreen replSeries
  , wrapped [ (stDim, "  (the repl: its heap and what the RTS holds; a column each half second, the newest at the right; the axis starts near the lowest)") ]
  , if any (\(_, _, sv) -> sv > 0) (sMemHist st) then [ [] ] ++ graph 4 stBlue serverSeries ++ [ [ (stDim, "  (the servers)") ] ] else []
  , [ [] ]
  , wrapped [ (stBold, "the heap"), (stDim, "  M the heap's figures (live, what the RTS holds, major collections)  C the CAFs by what they retain  S the strings  K the kept values  D the sharing that is missed") ]
  , wrapped [ (stDim, "  (each is a major collection and a walk of the heap: a second or more, the session paused meanwhile; taken when asked, not on its own)") ]
  , case (sHeapBusy st, sHeap st) of
      (Just mode, _) -> [ [ (stYellow, "  taking " ++ mode ++ "...") ] ]
      (_, Nothing) -> [ [ (stDim, "  none taken yet") ] ]
      (_, Just h) -> [ (stYellow, printf "  %s at %s, %.1fs" (hMode h) (hAt h) (hSecs h)) ] : concat [ wrapped [(plain, "  " ++ l)] | l <- hLines h ]
  ]
  where
    wrapped = wrapSpans w 4
    n = max 1 (w - 10)
    samples = reverse (take n (sMemHist st))              -- oldest first
    replSeries = [ r | (_, r, _) <- samples ]
    serverSeries = [ sv | (_, _, sv) <- samples ]
    (replNow, serversNow) = case sMemHist st of { ((_, r, sv) : _) -> (r, sv); [] -> (0, 0) }
    (lo, hi) = if null replSeries then (0, 0) else (minimum replSeries, maximum replSeries)
    span' = case samples of
      ((t0, _, _) : _) | ((t1, _, _) : _) <- sMemHist st -> let secs = round (t1 - t0) :: Int in if secs >= 90 then printf "%d min" (secs `div` 60) else printf "%d s" secs
      _ -> "moment" :: String
    -- @rows@ rows of a bar graph of the series, from a little under its lowest value (so a change of a few
    -- per cent shows) to its largest, a y axis of two labels at the left
    graph rows style series = [ [ (stDim, printf "%6s |" (label r)), (style, concatMap (bar r) series) ] | r <- [rows - 1, rows - 2 .. 0] ]
      where
        top = maximum (0 : series)
        bottom = max 0 (min (minimum (top : series) - (top - minimum (top : series)) `div` 4) (top - max 1 (top `div` 10)))
        label r | r == rows - 1 = show top ++ " MB" | r == 0 = show bottom | otherwise = ""
        -- the eighths of the row a value fills, as a block character
        bar r v = let eighths = ((v - bottom) * rows * 8) `div` max 1 (top - bottom) - r * 8
                  in [ if eighths >= 8 then '\x2588' else if eighths <= 0 then ' ' else toEnum (0x2580 + 8 - eighths) ]

logLine :: String -> [Span]
logLine l = case break (== ']') l of
  ('[' : t, ']' : rest) -> [ (stDim, '[' : t ++ "]"), (style rest, rest) ]
  _ -> [ (style l, l) ]
  where style x = if any (`isInfixOf` x) ["error", "ERROR", "failed", "FAIL", "DEAD", "WARNING", "died"] then withFg (Ansi 1) plain else plain

verdictLines :: Int -> Json -> [[Span]]
verdictLines w j = concatMap (wrapSpans w 4) $
  [ [ (verdictStyle verdict, verdict) ]
  , [ (stDim, "kind " ++ s "kind" ++ "  ok " ++ show (lookupBool "ok" j == Just True) ++ "  generation " ++ n "generation" ++ "  failing " ++ n "failing" ++ "  warnings " ++ n "warnings") ]
  , [] ]
  ++ [ [ (withFg (Ansi 3) plain, "stale: "), (plain, unwords (strs (j .: "stale_files"))) ] | not (null (strs (j .: "stale_files"))) ]
  ++ [ [ (withFg (Ansi 1) plain, "  " ++ d) ] | d <- strs (j .: "detail") ]
  ++ [ [] | not (null (lookupArr "diagnostics" j)) ]
  ++ concat [ [ (sev (fromMaybe "" (lookupStr "severity" d)), fromMaybe "?" (lookupStr "file" d) ++ ":" ++ n' "line" d ++ ":" ++ n' "col" d ++ " " ++ fromMaybe "" (lookupStr "severity" d) ++ " " ++ fromMaybe "" (lookupStr "code" d)) ]
              : [ [ (plain, "    " ++ ml) ] | ml <- lines (fromMaybe "" (lookupStr "message" d)) ]
            | d <- lookupArr "diagnostics" j ]
  ++ [ [], [ (stBold, "members") ] ]
  ++ [ [ (plain, "  " ++ fromMaybe "?" (lookupStr "member" m <|> lookupStr "name" m) ++ "  " ++ fromMaybe "" (lookupStr "verdict" m <|> lookupStr "text" m)) ] | m <- lookupArr "members" j ]
  ++ [ [], [ (stBold, "servers") ] ]
  ++ [ [ (plain, "  " ++ encode sv) ] | sv <- lookupArr "servers" j ]
  where
    verdict = fromMaybe "?" (lookupStr "text" j)
    s k = fromMaybe "?" (lookupStr k j)
    n k = maybe "?" (\d -> show (round d :: Int)) (lookupNum k j)
    n' k x = maybe "?" (\d -> show (round d :: Int)) (lookupNum k x)
    sev x = if x == "error" then stRed else stYellow
    a <|> b = maybe b Just a
