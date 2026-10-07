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
import Data.List (intercalate, isInfixOf, isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (exitWith, ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO

import qualified Ghostty.Vt as Vt
import Tui
import GhciSession.Config
import GhciSession.Json
import qualified GhciSession.Mcp as Mcp
import GhciSession.Sys (now, readFileMaybe)
import GhciSession.Usage (usageRows, usageTable)

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

data Tab = THistory | TView | TLog | TVerdict | TUsage | TChat | TShell
  deriving (Eq, Ord, Show, Enum, Bounded)

tabName :: Tab -> String
tabName t = case t of { THistory -> "1 history"; TView -> "2 view"; TLog -> "3 log"; TVerdict -> "4 verdict"; TUsage -> "5 usage"; TChat -> "6 chat"; TShell -> "7 shell" }

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
  , sWake :: IO ()                    -- ^ what a pane calls when it has something to show
  }

data Env = Env { eConf :: Conf, eName :: String, eDir :: FilePath }

topMain :: Conf -> Maybe String -> IO ()
topMain conf mname = do
  name <- Mcp.pick conf mname >>= either (\e -> hPutStrLn stderr ("top: " ++ e) >> exitWith (ExitFailure 2)) pure
  term <- hIsTerminalDevice stdin
  unless term (hPutStrLn stderr "top: standard input is not a terminal" >> exitWith (ExitFailure 2))
  vtErr <- loadVt
  let dir = cStateDir conf </> name
      env = Env conf name dir
      st0 = St THistory M.empty (M.fromList [ (t, True) | t <- [minBound ..] ]) (JObj []) Nothing [] [] 0 [] (JObj []) 0 [] 0 "" (80, 24) M.empty vtErr False (pure ())
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
  EvTick -> Just <$> (refresh env st >>= panes env)
  EvResize -> Just <$> (refresh env st >>= panes env)
  EvWake -> Just <$> panes env st
  EvKey kp -> do
    r <- key env kp st
    case r of
      Nothing -> pure Nothing
      Just (st', again) -> Just <$> ((if again then refresh env st' else pure st') >>= panes env)

-- | A key's effect, and whether a fresh look follows; 'Nothing' to quit.
key :: Env -> KeyPress -> St -> IO (Maybe (St, Bool))
key env kp@(KeyPress k _ bytes) st
  | sPrefix st || not (isPane (sTab st)) = case k of
      _ | sPrefix st && ctrlA -> withPane (\p -> paneInput p (BC.pack "\SOH")) >> pure (Just (st', False))
      KChar 'q' -> pure Nothing
      KChar '\ETX' | not (isPane (sTab st)) -> pure Nothing
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
    tabKeys = [('1', THistory), ('h', THistory), ('2', TView), ('v', TView), ('3', TLog), ('l', TLog), ('4', TVerdict), ('d', TVerdict), ('5', TUsage), ('u', TUsage), ('6', TChat), ('c', TChat), ('7', TShell), ('s', TShell)]
    page = max 1 (snd (sSize st) - 4)
    follow on = st' { sFollow = M.insert (sTab st) on (sFollow st) }
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

-- | A look at everything the screen shows: the status file, the daemon's info and its log, the history's
-- new messages; the view and the ledger only on their tabs, and not every time.
refresh :: Env -> St -> IO St
refresh env st = do
  size <- fromMaybe (80, 24) <$> termSize
  status <- maybe (JObj []) (either (const (JObj [])) id . parseJson) <$> readFileMaybe (eDir env </> "status.json")
  info <- answer "info" []
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
      pure (if null rows then ["  no model calls recorded (the chat and the compactor write usage.jsonl)"] else usageTable (eConf env) rows, t)
    else pure (sUsage st, sUsageAt st)
  pure st { sSize = size, sStatus = status, sInfo = info, sHist = hist, sLog = logLines, sLogSize = logSize
          , sView = view, sMem = mem, sViewAt = viewAt, sUsage = usage, sUsageAt = usageAt }
  where
    answer op args = do
      r <- try (Mcp.request (eConf env) (eName env) (JObj (("op", JStr op) : args))) :: IO (Either SomeException Json)
      pure $ case r of
        Right j | lookupBool "ok" j == Just True, Just out <- lookupText "out" j, Right v <- parseJsonBS (TE.encodeUtf8 out) -> Just v
        _ -> Nothing
    stamp 0 = pure "--:--:--"
    stamp d = formatTime defaultTimeLocale "%H:%M:%S" <$> utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac d))

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
         | M.findWithDefault True (sTab st) (sFollow st) = ""
         | otherwise = take (w - 60) "  (scrolled: f to follow)"

verdictStyle :: String -> Style
verdictStyle v
  | any (`isPrefixOf` v) ["OK"] = stGreen
  | any (`isInfixOf` v) ["COMPILE-ERROR", "CHECK-FAIL", "DEAD", "TYPE-ERROR", "HANG", "ERROR"] = stRed
  | any (`isInfixOf` v) ["reloading", "running", "starting", "STALE"] = stYellow
  | otherwise = stBold

bottom :: St -> [Span]
bottom st
  | isPane (sTab st) = [ (stDim, " Ctrl-a then: 1-7 tabs  q quit  a sends Ctrl-a   "), (stYellow, sNote st) ]
  | otherwise = [ (stDim, " q quit  1-7 tabs  j/k PgUp/PgDn g/G scroll  f follow  R reload  T test  r look now  "), (stYellow, sNote st ++ maybe "" ("  panes: " ++) (sVtErr st)) ]

-- | The current tab's lines, from where it is scrolled.
panelLines :: Int -> St -> [[Span]]
panelLines w st = drop off ls
  where
    ls = case sTab st of
      THistory -> concatMap (historyLines w) (sHist st)
      TView -> viewLines w st
      TLog -> [ wrapped (logLine l) | l <- sLog st ] >>= id
      TVerdict -> verdictLines w (sStatus st)
      TUsage -> [ [(plain, l)] | l <- sUsage st ]
      _ -> [ [(stRed, "no panes: " ++ fromMaybe "?" (sVtErr st))], [], [(plain, "libghostty-vt is Ghostty's terminal emulation as a C library: tools/libghostty-vt.sh builds it into .bin/,")], [(plain, "or put a libghostty-vt.so of your own beside ghci-session, or name one in GHS_LIBGHOSTTY.")] ]
    (_, h) = sSize st
    page = max 1 (h - 4)
    following = M.findWithDefault True (sTab st) (sFollow st)
    off = if following then max 0 (length ls - page) else max 0 (min (length ls - 1) (M.findWithDefault 0 (sTab st) (sScroll st)))
    wrapped = wrapSpans w 4

historyLines :: Int -> Msg -> [[Span]]
historyLines w m = concatMap (wrapSpans w 4) (first : rest ++ more)
  where
    ls = lines (mText m)
    shown = take 6 ls
    first = [ (stDim, "#" ++ show (mId m) ++ " " ++ mTime m ++ " "), (kindStyle (mKind m), mKind m ++ ":"), (plain, " " ++ head' shown) ]
    rest = [ [(plain, "    " ++ l)] | l <- drop 1 shown ]
    more = [ [(stDim, "    (" ++ show (length ls - 6) ++ " more lines)")] | length ls > 6 ]
    head' xs = case xs of { (x : _) -> x; [] -> "" }

kindStyle :: String -> Style
kindStyle k = case k of { "user" -> stCyan; "talk" -> stGreen; "tool" -> stBlue; "echo" -> plain; "note" -> stMagenta; _ -> stBold }

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
