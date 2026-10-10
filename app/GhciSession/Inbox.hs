-- | __Lines for a chat that was started somewhere else__: the session's running chat takes what is left in
-- @\<state\>\/\<session\>\/chat-inbox@, a file a message, as if it had been typed at it -- so the monitor
-- ("GhciSession.Top"), @chat --send@ and a script can all talk to the one agent at work, whatever its
-- standard input is. The chat that takes them is the one @chat.pid@ names (the last started on the session).
--
-- And the other way round, for whoever sent a task and wants to know it is done (@chat --send MSG --wait@,
-- @chat --wait@): the chat keeps what it knows of the turn under way in @turn.json@ (where it began, the tool
-- calls so far, its last words, its summary line, and @done@ when it ended); 'waitMain' reads that.
module GhciSession.Inbox
  ( send, watch, chatPidFile
  , Mark (..), markOf, advance, settled, turnReport, stillRunning, readTurn, waitMain, running, turnLines
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (forM_, forever, unless, void, when)
import qualified Data.ByteString as B
import Data.List (intercalate, isPrefixOf, sort)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile, renameFile)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (CPid)
import Text.Printf (printf)

import GhciSession.Config
import GhciSession.Json
import GhciSession.Sys (now)

chatPidFile :: Conf -> String -> FilePath
chatPidFile conf name = cStateDir conf </> name </> "chat.pid"

inboxDir :: Conf -> String -> FilePath
inboxDir conf name = cStateDir conf </> name </> "chat-inbox"

-- | The session's running chat, if there is one.
running :: Conf -> String -> IO (Maybe CPid)
running conf name = do
  t <- try (readFile (chatPidFile conf name)) :: IO (Either IOException String)
  case reads (either (const "") id t) of
    [(pid, _)] -> either (const Nothing) (const (Just pid)) <$> (try (signalProcess nullSignal pid) :: IO (Either IOException ()))
    _ -> pure Nothing

-- | A message for the session's running chat: its pid, or why it was not left.
send :: Conf -> String -> String -> IO (Either String Int)
send conf name text = fmap fst <$> sendNamed conf name text

-- | The same, and the file it was left in (it is gone once the chat has taken the message).
sendNamed :: Conf -> String -> String -> IO (Either String (Int, FilePath))
sendNamed conf name text
  | null (words text) = pure (Left "nothing to send")
  | otherwise = do
      r <- running conf name
      case r of
        Nothing -> pure (Left ("no chat running on " ++ name))
        Just pid -> do
          let dir = inboxDir conf name
          createDirectoryIfMissing True dir
          t <- now
          me <- getProcessID
          -- (written whole under another name first: the chat never reads half a message)
          let file = printf "%017.0f-%d" (t * 1e6) (fromIntegral me :: Int) :: String
          w <- try (TIO.writeFile (dir </> ('.' : file)) (T.pack text) >> renameFile (dir </> ('.' : file)) (dir </> file)) :: IO (Either IOException ())
          pure (either (Left . show) (const (Right (fromIntegral pid, dir </> file))) w)

-- | The chat's side: what is left for it is taken, oldest first, three times a second -- while it is the
-- chat the session's pid file names.
watch :: Conf -> String -> (T.Text -> IO ()) -> IO ()
watch conf name take' = void $ forkIO $ forever $ do
  threadDelay 300000
  me <- getProcessID
  r <- try (readFile (chatPidFile conf name)) :: IO (Either IOException String)
  when (either (const False) (\s -> [ p | (p, _) <- reads s ] == [me]) r) $ do
    fs <- either (const []) id <$> (try (listDirectory (inboxDir conf name)) :: IO (Either IOException [FilePath]))
    forM_ (sort [ f | f <- fs, not ("." `isPrefixOf` f) ]) $ \f -> do
      t <- try (TIO.readFile (inboxDir conf name </> f)) :: IO (Either IOException T.Text)
      void (try (removeFile (inboxDir conf name </> f)) :: IO (Either IOException ()))
      forM_ t $ \x -> unless (T.null (T.strip x)) (take' (T.strip x))

-- waiting for a turn ------------------------------------------------------------------------------

-- | What @turn.json@ says of a turn: the id of the message it began at, and whether it is over (ended, or
-- stopped). No file -- no turn yet -- is a turn that is over.
-- @mFed@ counts the lines the turn was given while it ran.
data Mark = Mark { mStart :: Maybe Int, mDone :: Bool, mFed :: Int } deriving (Eq, Show)

markOf :: Json -> Mark
markOf j = Mark s (isNothing s || lookupBool "done" j == Just True || lookupBool "stopped" j == Just True) (maybe 0 round (lookupNum "fed" j))
  where s = round <$> lookupNum "start" j

-- | When the turn that stood in the file at the send ends with the line still not taken, the line will be a
-- turn of its own: what is waited for is then a turn that begins after that one (the file as it stands now).
advance :: Mark -> Mark -> Mark
advance before now'
  | not (mDone before), mDone now', mStart now' == mStart before = now'
  | otherwise = before

-- | Has the turn that was waited for ended? @before@: the file as it stood when the line was left (as 'advance'
-- moves it); @taken@: the chat has the line. A line taken while a turn runs is that turn's (the chat reads it
-- between two tool calls); one taken by a chat at rest begins a turn, which is not the one in the file already.
settled :: Mark -> Mark -> Bool -> Bool
settled before now' taken
  | not taken = False
  | mDone before = mStart now' /= mStart before && mDone now'
  -- (a turn that was running at the send and has ended: it took the line only if it fed it to itself; otherwise the line
  -- is the next turn's, which may not have begun yet -- the file still says the old turn is over)
  | mStart now' == mStart before = mDone now' && mFed now' > mFed before
  | otherwise = mDone now'

-- | What a waiter prints of a turn that ended: its last words, whole, and its summary line.
turnReport :: Json -> String
turnReport j = intercalate "\n" (filter (not . null)
  [ maybe "(the turn said nothing)" T.unpack (lookupText "last" j)
  , if lookupBool "stopped" j == Just True then "[the turn was stopped]" else ""
  , fromMaybe "" (lookupStr "summary" j) ])

-- | A turn still running when the wait ran out, and how far it had got.
stillRunning :: Json -> Double -> String
stillRunning j secs = printf "[still running after %.0fs: %d tool call%s so far]" secs n (if n == 1 then "" else "s" :: String)
  where n = maybe 0 round (lookupNum "tools" j) :: Int

-- | What a screen shows of the chat's turn: whether the chat runs, the turn under way (tool calls so far, its last words)
-- or the last one's. @turnLines chatRuns turnJson@.
turnLines :: Bool -> Json -> [String]
turnLines up j
  | not up = ["chat: not running" ++ (if started then " (the last turn: " ++ calls ++ ")" else "")]
  | not started = ["chat: running, no turn yet"]
  | markDone = ["chat: at rest; the last turn: " ++ calls, "  said: " ++ lastWords]
  | otherwise = ["chat: a turn is running, " ++ calls ++ " so far", "  last words: " ++ lastWords]
  where
    started = isJust (lookupNum "start" j)
    markDone = mDone (markOf j)
    n = maybe 0 round (lookupNum "tools" j) :: Int
    calls = printf "%d tool call%s" n (if n == 1 then "" else "s" :: String)
    lastWords = case lines (maybe "" T.unpack (lookupText "last" j)) of { [] -> "-"; (l : _) -> take 160 l }

-- | The file as it is: Nothing when it cannot be read whole (it is written under another name and moved, so
-- that is a file that is not there yet or not any more); no file at all is the empty object.
readTurn :: Conf -> String -> IO (Maybe Json)
readTurn conf name = do
  let f = cStateDir conf </> name </> "turn.json"
  there <- doesFileExist f
  if not there then pure (Just (JObj [])) else do
    r <- try (B.readFile f) :: IO (Either IOException B.ByteString)
    pure (either (const Nothing) (either (const Nothing) Just . parseJsonBS) r)

-- | @chat --send MSG --wait [SECS]@ and @chat --wait [SECS]@: the exit code (0 the turn ended, 3 the time
-- passed first, 1 no chat is running or it died meanwhile), and what is said of it on standard output.
waitMain :: Conf -> String -> Maybe String -> Maybe Double -> IO Int
waitMain conf name msg limit = do
  alive <- running conf name
  case alive of
    Nothing -> hPutStrLn stderr ("chat: no chat running on " ++ name) >> pure 1
    Just _ -> do
      j0 <- fromMaybe (JObj []) <$> readTurn conf name
      -- (no line sent: the turn under way is the one waited for, whether or not it fed anything to itself -- a fed
      -- count of -1 before it is one that any turn's count passes)
      let before0 = (markOf j0) { mFed = if isNothing msg then -1 else mFed (markOf j0) }
      sent <- maybe (pure (Right Nothing)) (fmap (fmap (Just . snd)) . sendNamed conf name) msg
      case sent of
        Left why -> hPutStrLn stderr ("chat: " ++ why) >> pure 1
        Right file
          | file == Nothing, mDone before0 -> putStrLn (if mStart before0 == Nothing then "[no turn yet]" else turnReport j0) >> pure 0
          | otherwise -> do
              t0 <- now
              loop before0 file t0 j0
  where
    loop before file t0 jLast = do
      alive <- running conf name
      case alive of
        Nothing -> hPutStrLn stderr ("chat: the chat on " ++ name ++ " is not running (it died, or was stopped)") >> pure 1
        Just _ -> do
          mj <- readTurn conf name
          taken <- maybe (pure True) (fmap not . doesFileExist) file
          let j = fromMaybe jLast mj
              now' = markOf j
              -- (a turn that ends while the line is still there is not the one waited for; once it is taken, it is)
              before' = if isNothing mj || taken then before else advance before now'
          t <- now
          if isNothing mj then pause before' file t0 j t
            else if settled before' now' taken then putStrLn (turnReport j) >> pure 0
            else pause before' file t0 j t
    pause before file t0 j t
      | Just l <- limit, t - t0 >= l = putStrLn (stillRunning j (t - t0)) >> pure 3
      | otherwise = threadDelay 100000 >> loop before file t0 j
