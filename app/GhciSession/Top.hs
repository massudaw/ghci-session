{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | __Watching a session, and working in it__: @ghci-session top [SESSION]@, a screen that follows a session as
-- it works -- its verdict, memory and servers in the header; below, a tab at a time: the history as the daemon
-- writes it (every request and verdict, a save and its diff, the chat's words), the view the model reads with
-- the memory's numbers, the daemon's log, the verdict with what is behind it (the compiler's diagnostics, the
-- failing tests, the members and servers), what the model calls cost -- and two terminal PANES, the chat and a
-- shell, running inside the monitor on their own pseudo-terminals. Half a second between looks; a session that
-- is not running shows as such and is picked up when it starts.
--
-- The panes are libghostty-vt's doing -- Ghostty's terminal emulation as a C library, loaded at run time
-- ('cbits/ghs_vt.c'): the program's output goes to a terminal of the pane's size, and what the monitor draws
-- is that terminal's screen, cell by cell with its colors. The real terminal never sees the program's escape
-- sequences, so a full-screen program in a pane (the chat, an editor) and the monitor around it do not fight.
-- Without the library the other tabs work and the pane tabs say what is missing.
--
-- The rest needs no library: raw mode through the @unix@ package, the size from an ioctl, ANSI sequences for
-- the frame, which is drawn whole (every line written over, the cursor hidden) so nothing flickers.
--
-- Keys: @1@-@7@ or @h v l d u c s@ for a tab, @j@ @k@ and the arrows, @PgUp@ @PgDn@ @g@ @G@ to scroll, @f@ to
-- follow new lines again, @R@ reload, @T@ test, @r@ look now, @q@ quit. In a pane every key goes to the
-- program; @Ctrl-a@ first makes the next key the monitor's (@Ctrl-a 1@: the history, @Ctrl-a q@: quit,
-- @Ctrl-a a@: a Ctrl-a for the program).
module GhciSession.Top (topMain, Key (..), decodeKey, Span, fit, visible, wrapSpans) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, finally, try)
import Control.Monad (forM, forM_, unless, void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import Data.Char (isDigit)
import Data.IORef
import Data.List (intercalate, isInfixOf, isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek)
import Data.Word (Word8)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (exitWith, ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO
import System.Posix.IO (fdToHandle, fdWriteBuf, stdInput)
import System.Posix.Signals (Handler (..), installHandler, sigHUP, signalProcess)
import System.Posix.Terminal
import System.Posix.Types (ByteCount, Fd (..))

import GhciSession.Config
import GhciSession.Json
import qualified GhciSession.Mcp as Mcp
import GhciSession.Sys (now, readFileMaybe, termSize)
import GhciSession.Usage (usageRows, usageTable)

-- libghostty-vt, through cbits/ghs_vt.c ---------------------------------------------------------

foreign import ccall unsafe "ghs_vt_open" c_vt_open :: CString -> IO CInt
foreign import ccall unsafe "ghs_vt_error" c_vt_error :: IO CString
foreign import ccall unsafe "ghs_vt_new" c_vt_new :: CInt -> CInt -> CInt -> IO (Ptr ())
foreign import ccall unsafe "ghs_vt_free" c_vt_free :: Ptr () -> IO ()
foreign import ccall unsafe "ghs_vt_write" c_vt_write :: Ptr () -> Ptr Word8 -> CSize -> IO ()
foreign import ccall unsafe "ghs_vt_resize" c_vt_resize :: Ptr () -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "ghs_vt_render" c_vt_render :: Ptr () -> Ptr CChar -> CSize -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "ghs_pty_spawn" c_pty_spawn :: CString -> CString -> CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "ghs_pty_resize" c_pty_resize :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "ghs_pty_wait" c_pty_wait :: CInt -> IO CInt
foreign import ccall unsafe "ghs_sigwinch" c_sigwinch :: IO CInt

-- | Load the library: @GHS_LIBGHOSTTY@, else @libghostty-vt.so@ beside the executable, else the system's.
-- The reason when it cannot.
loadVt :: IO (Maybe String)
loadVt = do
  env <- lookupEnv "GHS_LIBGHOSTTY"
  exe <- getExecutablePath
  beside <- filterIO [takeDirectory exe </> "libghostty-vt.so", takeDirectory exe </> "libghostty-vt.dylib"]
  let path = fromMaybe (case beside of { (p : _) -> p; [] -> "" }) env
  rc <- withCString path c_vt_open
  if rc == 0 then pure Nothing else Just <$> (c_vt_error >>= peekCString)
  where filterIO = fmap concat . mapM (\p -> (\b -> [p | b]) <$> doesFileExist p)

-- keys -------------------------------------------------------------------------------

data Key = KChar Char | KUp | KDown | KPgUp | KPgDn | KHome | KEnd | KEsc | KEnter
  deriving (Eq, Show)

-- | A key from the bytes a terminal sends for it.
decodeKey :: String -> Key
decodeKey s = case s of
  "\ESC[A" -> KUp
  "\ESC[B" -> KDown
  "\ESCOA" -> KUp
  "\ESCOB" -> KDown
  "\ESC[5~" -> KPgUp
  "\ESC[6~" -> KPgDn
  "\ESC[H" -> KHome
  "\ESC[1~" -> KHome
  "\ESCOH" -> KHome
  "\ESC[F" -> KEnd
  "\ESC[4~" -> KEnd
  "\ESCOF" -> KEnd
  "\ESC" -> KEsc
  "\r" -> KEnter
  "\n" -> KEnter
  [c] -> KChar c
  _ -> KEsc

-- | What was typed: the bytes as the terminal sent them (for a pane), and the key they mean.
data Input = Input { iBytes :: B.ByteString, iKey :: Key }

-- | Keys from standard input, as they come; an escape followed within a few milliseconds by more bytes is
-- one sequence.
reader :: TQueue Input -> IO ()
reader q = do
  hSetBinaryMode stdin True
  hSetBuffering stdin NoBuffering
  let loop = do
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
      push s = atomically (writeTQueue q (Input (BC.pack s) (decodeKey s)))
  loop

-- rendering -------------------------------------------------------------------------

-- | A run of text with its style (SGR parameters: @"1;32"@; empty for plain).
type Span = (String, String)

-- | How many columns a line of spans takes.
visible :: [Span] -> Int
visible = sum . map (length . snd)

-- | A line of exactly @w@ columns: cut, or padded with spaces; every span's style closed.
fit :: Int -> [Span] -> String
fit w spans = concat (go w spans) ++ replicate (w - min w (visible spans)) ' '
  where
    go _ [] = []
    go n _ | n <= 0 = []
    go n ((st, t) : r) = let t' = take n t in styled st t' : go (n - length t') r
    styled st t = if null st || null t then t else "\ESC[" ++ st ++ "m" ++ t ++ "\ESC[0m"

-- | A long line as several of at most @w@ columns, the continuation lines indented.
wrapSpans :: Int -> Int -> [Span] -> [[Span]]
wrapSpans w indent spans
  | visible spans <= w || w <= indent + 1 = [spans]
  | otherwise = let (a, b) = splitAt' w spans in a : map (\l -> (("", replicate indent ' ') : l)) (wrapSpans (w - indent) 0 b)
  where
    splitAt' _ [] = ([], [])
    splitAt' n ((st, t) : r)
      | length t <= n = let (a, b) = splitAt' (n - length t) r in ((st, t) : a, b)
      | otherwise = ([(st, take n t)], (st, drop n t) : r)

-- panes -------------------------------------------------------------------------------

-- | A program on a pseudo-terminal, its screen kept by libghostty-vt.
data Pane = Pane
  { pVt :: Ptr (), pFd :: CInt, pPid :: IORef Int
  , pLock :: MVar ()                   -- ^ the terminal is written by the reader and read by the drawing
  , pDead :: IORef (Maybe Int)         -- ^ the exit status, once the program ended
  , pSize :: IORef (Int, Int)          -- ^ cols, rows
  , pCmd :: String
  }

-- | Start a program in a pane of this size; its output wakes the screen through @dirty@.
spawnPane :: String -> FilePath -> (Int, Int) -> TVar Int -> IO (Either String Pane)
spawnPane cmd cwd (cols, rows) dirty = do
  (fd, pid) <- withCString cmd $ \c -> withCString cwd $ \d -> alloca $ \pp -> do
    fd <- c_pty_spawn c d (fromIntegral cols) (fromIntegral rows) pp
    pid <- if fd < 0 then pure 0 else peek pp
    pure (fd, fromIntegral pid)
  if fd < 0 then pure (Left "cannot open a pseudo-terminal") else do
    vt <- c_vt_new (fromIntegral cols) (fromIntegral rows) fd
    if vt == nullPtr then pure (Left "libghostty-vt could not make a terminal") else do
      p <- Pane vt fd <$> newIORef pid <*> newMVar () <*> newIORef Nothing <*> newIORef (cols, rows) <*> pure cmd
      h <- fdToHandle (Fd fd)
      hSetBinaryMode h True
      _ <- forkIO $ do
        let loop = do
              r <- try (B.hGetSome h 65536) :: IO (Either IOException B.ByteString)
              case r of
                Right b | not (B.null b) -> do
                  withMVar (pLock p) $ \_ -> BU.unsafeUseAsCStringLen b $ \(ptr, n) -> c_vt_write vt (castPtr' ptr) (fromIntegral n)
                  atomically (modifyTVar' dirty (+ 1))
                  loop
                _ -> do
                  st <- c_pty_wait (fromIntegral pid)
                  writeIORef (pDead p) (Just (fromIntegral st))
                  atomically (modifyTVar' dirty (+ 1))
        loop
      pure (Right p)
  where castPtr' = castCCharToWord8

castCCharToWord8 :: Ptr CChar -> Ptr Word8
castCCharToWord8 = castPtr

-- | The pane's screen: its rows as text with colors, and the cursor (x, y, visible).
renderPane :: Pane -> IO ([String], (Int, Int, Bool))
renderPane p = withMVar (pLock p) $ \_ -> do
  (cols, rows) <- readIORef (pSize p)
  let go cap = allocaBytes cap $ \buf -> alloca $ \px -> alloca $ \py -> alloca $ \pv -> do
        n <- c_vt_render (pVt p) buf (fromIntegral cap) px py pv
        if n < 0 then pure Nothing else do
          b <- B.packCStringLen (buf, fromIntegral n)
          x <- peek px
          y <- peek py
          v <- peek pv
          pure (Just (lines (T.unpack (TE.decodeUtf8With (\_ _ -> Just '?') b)), (fromIntegral x, fromIntegral y, v /= 0)))
  r <- go (cols * rows * 48 + 4096)
  case r of
    Just x -> pure x
    Nothing -> fromMaybe ([], (0, 0, False)) <$> go (cols * rows * 400 + 65536)

resizePane :: Pane -> (Int, Int) -> IO ()
resizePane p sz@(cols, rows) = do
  old <- readIORef (pSize p)
  when (old /= sz && cols > 0 && rows > 0) $ do
    writeIORef (pSize p) sz
    withMVar (pLock p) $ \_ -> void (c_vt_resize (pVt p) (fromIntegral cols) (fromIntegral rows))
    void (c_pty_resize (pFd p) (fromIntegral cols) (fromIntegral rows))

-- | Keys for the program: written to the descriptor itself (a Handle on it would close it when collected,
-- and the reader's Handle is busy in its read).
feedPane :: Pane -> B.ByteString -> IO ()
feedPane p b = do
  dead <- readIORef (pDead p)
  when (isNothing dead) $ BU.unsafeUseAsCStringLen b $ \(ptr, n) -> do
    let go off | off >= n = pure ()
               | otherwise = do
                   r <- try (fdWriteBuf (Fd (pFd p)) (castPtr ptr `plusPtr` off) (fromIntegral (n - off))) :: IO (Either IOException ByteCount)
                   case r of
                     Right k | k > 0 -> go (off + fromIntegral k)
                     _ -> pure ()
    go 0

-- | The pane's program told the terminal went away.
hangupPane :: Pane -> IO ()
hangupPane p = do
  dead <- readIORef (pDead p)
  pid <- readIORef (pPid p)
  when (isNothing dead && pid > 0) (void (try (signalProcess sigHUP (fromIntegral pid)) :: IO (Either SomeException ())))

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
  , sSize :: (Int, Int)
  , sPanes :: M.Map Tab Pane
  , sVtErr :: Maybe String            -- ^ why there are no panes
  , sPrefix :: Bool                   -- ^ Ctrl-a was pressed: the next key is the monitor's
  }

data Env = Env { eConf :: Conf, eName :: String, eDir :: FilePath, eKeys :: TQueue Input, eWake :: TVar Int }

topMain :: Conf -> Maybe String -> IO ()
topMain conf mname = do
  name <- Mcp.pick conf mname >>= either (\e -> hPutStrLn stderr ("top: " ++ e) >> exitWith (ExitFailure 2)) pure
  term <- hIsTerminalDevice stdin
  unless term (hPutStrLn stderr "top: standard input is not a terminal" >> exitWith (ExitFailure 2))
  vtErr <- loadVt
  keys <- newTQueueIO
  wake <- newTVarIO 0
  winch <- c_sigwinch
  _ <- installHandler winch (Catch (atomically (modifyTVar' wake (+ 1)))) Nothing
  old <- getTerminalAttributes stdInput
  let raw = (foldl withoutMode old [EnableEcho, ProcessInput, KeyboardInterrupts, StartStopOutput, ExtendedFunctions, MapCRtoLF]) `withMinInput` 1 `withTime` 0
  setTerminalAttributes stdInput raw Immediately
  hSetBuffering stdout (BlockBuffering (Just 65536))
  hPutStr stdout "\ESC[?1049h\ESC[?25l\ESC[2J"
  hFlush stdout
  _ <- forkIO (reader keys)
  let dir = cStateDir conf </> name
      st0 = St THistory M.empty (M.fromList [ (t, True) | t <- [minBound ..] ]) (JObj []) Nothing [] [] 0 [] (JObj []) 0 [] 0 "" (24, 80) M.empty vtErr False
  stEnd <- newIORef st0
  (run (Env conf name dir keys wake) st0 >>= writeIORef stEnd) `finally` do
    readIORef stEnd >>= mapM_ hangupPane . M.elems . sPanes
    hPutStr stdout "\ESC[?25h\ESC[?1049l"
    hFlush stdout
    setTerminalAttributes stdInput old Immediately

run :: Env -> St -> IO St
run env = loop True
  where
    loop fresh st0 = do
      st1 <- if fresh then refresh env st0 else pure st0
      st2 <- panes env st1
      draw (eName env) st2
      tv <- registerDelay 500000
      n0 <- readTVarIO (eWake env)
      ev <- atomically $ (Left <$> readTQueue (eKeys env)) `orElse` (readTVar (eWake env) >>= \n -> check (n /= n0) >> pure (Right True)) `orElse` (readTVar tv >>= check >> pure (Right False))
      case ev of
        Right woke -> loop (not woke || not (isPane (sTab st2))) st2     -- (a pane's output: draw, do not ask the daemon again)
        Left i -> do
          r <- key env i st2
          case r of
            Nothing -> pure st2
            Just (st3, again) -> loop again st3

-- | A key's effect, and whether a fresh look follows; 'Nothing' to quit.
key :: Env -> Input -> St -> IO (Maybe (St, Bool))
key env (Input bytes k) st
  -- the monitor's key after Ctrl-a, or any key outside a pane
  | sPrefix st || not (isPane (sTab st)) = case k of
      _ | sPrefix st && bytes == BC.pack "\SOH" -> withPane (\p -> feedPane p bytes) >> pure (Just (st', False))
      KChar 'q' -> pure Nothing
      KChar '\ETX' | not (isPane (sTab st)) -> pure Nothing        -- ^C
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
      KHome -> key env (Input bytes (KChar 'g')) st
      KChar 'G' -> pure (Just (follow True, False))
      KEnd -> pure (Just (follow True, False))
      KChar 'f' -> pure (Just (follow True, False))
      KChar 'r' -> pure (Just (st', True))
      KChar 'R' -> act "reload" [] >> pure (Just (st' { sNote = "reload sent" }, True))
      KChar 'T' -> act "check" [] >> pure (Just (st' { sNote = "test sent" }, True))
      KEnter | isPane (sTab st) -> restartDead
      _ -> pure (Just (st', False))
  | bytes == BC.pack "\SOH" = pure (Just (st { sPrefix = True }, False))
  | otherwise = do
      dead <- maybe (pure Nothing) (readIORef . pDead) (M.lookup (sTab st) (sPanes st))
      case (dead, k) of
        (Just _, KEnter) -> restartDead
        _ -> withPane (\p -> feedPane p bytes) >> pure (Just (st, False))
  where
    st' = st { sPrefix = False }
    tabKeys = [('1', THistory), ('h', THistory), ('2', TView), ('v', TView), ('3', TLog), ('l', TLog), ('4', TVerdict), ('d', TVerdict), ('5', TUsage), ('u', TUsage), ('6', TChat), ('c', TChat), ('7', TShell), ('s', TShell)]
    page = max 1 (fst (sSize st) - 4)
    follow on = st' { sFollow = M.insert (sTab st) on (sFollow st) }
    scrollBy d = pure (Just (st' { sScroll = M.insertWith (+) (sTab st) d (sScroll st), sFollow = M.insert (sTab st) False (sFollow st) }, False))
    act op args = void (forkIO (void (try (Mcp.request (eConf env) (eName env) (JObj (("op", JStr op) : args))) :: IO (Either SomeException Json))))
    withPane f = forM_ (M.lookup (sTab st) (sPanes st)) f
    -- a pane whose program ended starts it again on Enter
    restartDead = case M.lookup (sTab st) (sPanes st) of
      Just p -> do
        dead <- readIORef (pDead p)
        case dead of
          Just _ -> do
            c_vt_free (pVt p)
            pure (Just (st' { sPanes = M.delete (sTab st) (sPanes st) }, False))
          Nothing -> pure (Just (st', False))
      Nothing -> pure (Just (st', False))

-- | The pane of the current tab exists and has the body's size.
panes :: Env -> St -> IO St
panes env st
  | not (isPane (sTab st)) || isJust (sVtErr st) = pure st
  | otherwise = do
      let (h, w) = sSize st
          size = (w, max 1 (h - 4))
      case M.lookup (sTab st) (sPanes st) of
        Just p -> resizePane p size >> pure st
        Nothing -> do
          exe <- getExecutablePath
          shell <- fromMaybe "/bin/sh" <$> lookupEnv "SHELL"
          let cmd = if sTab st == TChat then shq exe ++ " chat -s " ++ shq (eName env) else shell
          r <- spawnPane cmd (cRoot (eConf env)) size (eWake env)
          pure $ case r of
            Right p -> st { sPanes = M.insert (sTab st) p (sPanes st) }
            Left e -> st { sNote = e }
  where shq x = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) x ++ "'"

-- | A look at everything the screen shows: the status file, the daemon's info and its log, the history's
-- new messages; the view and the ledger only on their tabs, and not every time.
refresh :: Env -> St -> IO St
refresh env st = do
  size <- fromMaybe (24, 80) <$> termSize
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

draw :: String -> St -> IO ()
draw name st = do
  let (h, w) = sSize st
      top = header name st w
  (body, cursor) <- case M.lookup (sTab st) (sPanes st) of
    Just p | isPane (sTab st) -> do
      (rows, (cx, cy, vis)) <- renderPane p
      dead <- readIORef (pDead p)
      let rows' = take (h - 4) (rows ++ repeat "")
          over = maybe rows' (\code -> init' rows' ++ [fit w [("1;33", " [" ++ pCmd p ++ ": ended with status " ++ show code ++ "; Enter starts it again, Ctrl-a 1 leaves] ")]]) dead
      pure (map Right over, if vis && isNothing dead then Just (cy + 4, cx + 1) else Nothing)
    _ -> pure (map Left (take (max 0 (h - 4)) (panelLines w st)), Nothing)
  let line (r, l) = "\ESC[" ++ show r ++ ";1H" ++ either (fit w) id l
      frame = concat (map line (zip [1 :: Int ..] (map Left top ++ body ++ [Left (bottom st w)])))
      cur = case cursor of { Just (r, c) -> "\ESC[" ++ show r ++ ";" ++ show c ++ "H\ESC[?25h"; Nothing -> "\ESC[?25l" }
  hPutStr stdout ("\ESC[?25l\ESC[H" ++ frame ++ "\ESC[J" ++ cur)
  hFlush stdout
  where init' xs = if null xs then [] else init xs

header :: String -> St -> Int -> [[Span]]
header name st w =
  [ [ ("1", " ghci-session top "), ("", name ++ "  "), (verdictStyle verdict, verdict) ]
  , [ ("", " "), ("2", intercalate "  " facts) ]
  , [ ("", " ") ] ++ concat [ [ (if t == sTab st then "7" else "", " " ++ tabName t ++ " "), ("", " ") ] | t <- [minBound ..] ] ++ [ ("2", note) ]
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

verdictStyle :: String -> String
verdictStyle v
  | any (`isPrefixOf` v) ["OK"] = "1;32"
  | any (`isInfixOf` v) ["COMPILE-ERROR", "CHECK-FAIL", "DEAD", "TYPE-ERROR", "HANG", "ERROR"] = "1;31"
  | any (`isInfixOf` v) ["reloading", "running", "starting", "STALE"] = "1;33"
  | otherwise = "1"

bottom :: St -> Int -> [Span]
bottom st _
  | isPane (sTab st) = [ ("2", " Ctrl-a then: 1-7 tabs  q quit  a sends Ctrl-a   "), ("1;33", sNote st) ]
  | otherwise = [ ("2", " q quit  1-7 tabs  j/k PgUp/PgDn g/G scroll  f follow  R reload  T test  r look now  "), ("1;33", sNote st ++ maybe "" ("  panes: " ++) (sVtErr st)) ]

-- | The current tab's lines, from where it is scrolled.
panelLines :: Int -> St -> [[Span]]
panelLines w st = drop off ls
  where
    ls = case sTab st of
      THistory -> concatMap (historyLines w) (sHist st)
      TView -> viewLines w st
      TLog -> [ wrapped (logLine l) | l <- sLog st ] >>= id
      TVerdict -> verdictLines w (sStatus st)
      TUsage -> [ [("", l)] | l <- sUsage st ]
      _ -> [ [("1;31", "no panes: " ++ fromMaybe "?" (sVtErr st))], [], [("", "libghostty-vt is Ghostty's terminal emulation as a C library: build it with `zig build -Demit-lib-vt` in a ghostty checkout")], [("", "and put zig-out/lib/libghostty-vt.so beside ghci-session (.bin/), or name it in GHS_LIBGHOSTTY.")] ]
    (h, _) = sSize st
    page = max 1 (h - 4)
    following = M.findWithDefault True (sTab st) (sFollow st)
    off = if following then max 0 (length ls - page) else max 0 (min (length ls - 1) (M.findWithDefault 0 (sTab st) (sScroll st)))
    wrapped = wrapSpans w 4

historyLines :: Int -> Msg -> [[Span]]
historyLines w m = concatMap (wrapSpans w 4) (first : rest ++ more)
  where
    ls = lines (mText m)
    shown = take 6 ls
    first = [ ("2", "#" ++ show (mId m) ++ " " ++ mTime m ++ " "), (kindStyle (mKind m), mKind m ++ ":"), ("", " " ++ head' shown) ]
    rest = [ [("", "    " ++ l)] | l <- drop 1 shown ]
    more = [ [("2", "    (" ++ show (length ls - 6) ++ " more lines)")] | length ls > 6 ]
    head' xs = case xs of { (x : _) -> x; [] -> "" }

kindStyle :: String -> String
kindStyle k = case k of { "user" -> "1;36"; "talk" -> "1;32"; "tool" -> "1;34"; "echo" -> "0"; "note" -> "1;35"; _ -> "1" }

viewLines :: Int -> St -> [[Span]]
viewLines w st = [ [ ("1", " memory  "), ("", stats) ], [] ] ++ concatMap (wrapSpans w 6 . viewLine) (sView st)
  where
    m = sMem st
    n k = maybe "?" (\d -> show (round d :: Int)) (lookupNum k m)
    stats = intercalate "  " [ "messages " ++ n "messages", "lines " ++ n "parts", "unbuilt " ++ n "unbuilt", "built " ++ n "built"
                             , if lookupBool "settled" m == Just True then "settled" else "not settled"
                             , if lookupBool "compactor" m == Just True then "compactor on (" ++ n "busy" ++ " running, " ++ n "failed" ++ " retrying)" else "no compactor (summarize_cmd)" ]
    viewLine l = case break (== '|') l of
      (p, '|' : rest) | all (\c -> isDigit c || c == '+') p && not (null p) -> [ ("2", p ++ "|"), (if "(not summarized yet" `isPrefixOf` rest then "33" else "", rest) ]
      _ -> [ ("1", l) ]

logLine :: String -> [Span]
logLine l = case break (== ']') l of
  ('[' : t, ']' : rest) -> [ ("2", '[' : t ++ "]"), (style rest, rest) ]
  _ -> [ (style l, l) ]
  where style x = if any (`isInfixOf` x) ["error", "ERROR", "failed", "FAIL", "DEAD", "WARNING", "died"] then "31" else ""

verdictLines :: Int -> Json -> [[Span]]
verdictLines w j = concatMap (wrapSpans w 4) $
  [ [ (verdictStyle verdict, verdict) ]
  , [ ("2", "kind " ++ s "kind" ++ "  ok " ++ show (lookupBool "ok" j == Just True) ++ "  generation " ++ n "generation" ++ "  failing " ++ n "failing" ++ "  warnings " ++ n "warnings") ]
  , [] ]
  ++ [ [ ("33", "stale: "), ("", unwords (strs (j .: "stale_files"))) ] | not (null (strs (j .: "stale_files"))) ]
  ++ [ [ ("31", "  " ++ d) ] | d <- strs (j .: "detail") ]
  ++ [ [] | not (null (lookupArr "diagnostics" j)) ]
  ++ concat [ [ (sev (fromMaybe "" (lookupStr "severity" d)), fromMaybe "?" (lookupStr "file" d) ++ ":" ++ n' "line" d ++ ":" ++ n' "col" d ++ " " ++ fromMaybe "" (lookupStr "severity" d) ++ " " ++ fromMaybe "" (lookupStr "code" d)) ]
              : [ [ ("", "    " ++ ml) ] | ml <- lines (fromMaybe "" (lookupStr "message" d)) ]
            | d <- lookupArr "diagnostics" j ]
  ++ [ [], [ ("1", "members") ] ]
  ++ [ [ ("", "  " ++ fromMaybe "?" (lookupStr "member" m <|> lookupStr "name" m) ++ "  " ++ fromMaybe "" (lookupStr "verdict" m <|> lookupStr "text" m)) ] | m <- lookupArr "members" j ]
  ++ [ [], [ ("1", "servers") ] ]
  ++ [ [ ("", "  " ++ encode sv) ] | sv <- lookupArr "servers" j ]
  where
    verdict = fromMaybe "?" (lookupStr "text" j)
    s k = fromMaybe "?" (lookupStr k j)
    n k = maybe "?" (\d -> show (round d :: Int)) (lookupNum k j)
    n' k x = maybe "?" (\d -> show (round d :: Int)) (lookupNum k x)
    sev x = if x == "error" then "1;31" else "1;33"
    a <|> b = maybe b Just a
