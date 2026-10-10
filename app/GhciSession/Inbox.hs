-- | __Lines for a chat that was started somewhere else__: the session's running chat takes what is left in
-- @\<state\>\/\<session\>\/chat-inbox@, a file a message, as if it had been typed at it -- so the monitor
-- ("GhciSession.Top"), @chat --send@ and a script can all talk to the one agent at work, whatever its
-- standard input is. The chat that takes them is the one @chat.pid@ names (the last started on the session).
module GhciSession.Inbox
  ( send, watch, chatPidFile
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (forM_, forever, unless, void, when)
import Data.List (isPrefixOf, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile, renameFile)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (CPid)
import Text.Printf (printf)

import GhciSession.Config
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
send conf name text
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
          pure (either (Left . show) (const (Right (fromIntegral pid))) w)

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
