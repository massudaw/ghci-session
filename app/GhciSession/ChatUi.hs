-- | Where the chat shows what happens. The turn loop ('GhciSession.Chat') does not print: it tells a 'Ui'
-- what the agent said, thought, called and got back, and what the harness noted, and the 'Ui' shows it --
-- on the standard streams as the chat always has ('stdoutUi': the words on standard output, the notes on
-- standard error), or on the screen the chat draws for itself ('GhciSession.ChatTui').
module GhciSession.ChatUi
  ( Ui (..), stdoutUi
  , Spent (..), spend, spentLine
  ) where

import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import Text.Printf (printf)

import GhciSession.Llm (Usage (..), human)

-- | What a turn cost so far: model calls, tokens in, of them cached, tokens out, tool calls -- and whether it
-- has written to the files, and been told it was about to end red.
data Spent = Spent { sCalls :: !Int, sIn :: !Int, sCached :: !Int, sOut :: !Int, sTools :: !Int, sTouched :: !Bool, sNudged :: !Bool }
  deriving (Eq, Show)

spend :: IORef Spent -> Usage -> IO ()
spend ref u = modifyIORef' ref (\s -> s { sCalls = sCalls s + 1, sIn = sIn s + uIn u, sCached = sCached s + fromMaybe 0 (uCached u), sOut = sOut s + uOut u })

-- | The turn's summary line: always said, whatever --usage.
spentLine :: Spent -> Double -> String
spentLine s secs = printf "[turn: %d model call%s, %s tokens in (%d%% cached), %s out, %d tool call%s, %.0fs]"
  (sCalls s) (plural (sCalls s)) (human (sIn s)) (if sIn s == 0 then 0 else (100 * sCached s) `div` sIn s :: Int) (human (sOut s)) (sTools s) (plural (sTools s)) secs
  where plural n = if n == 1 then "" else "s" :: String

-- | The chat's output, as actions: each is called from the turn's thread, in the order things happen.
data Ui = Ui
  { uiView :: T.Text -> IO ()             -- ^ the view at the start: the memory the first turn will read
  , uiTalk :: T.Text -> IO ()             -- ^ the agent's words (logged as @talk@)
  , uiThought :: T.Text -> IO ()          -- ^ its thinking (shown, never logged)
  , uiCall :: String -> String -> IO ()   -- ^ a tool call: the tool's name, its arguments as shown
  , uiAnswer :: T.Text -> IO ()           -- ^ a tool's answer, cut for showing
  , uiNote :: String -> IO ()             -- ^ a note of the harness (the bracketed lines)
  , uiBusy :: Maybe String -> IO ()       -- ^ what the turn is doing now; 'Nothing': between turns, waiting for a line
  , uiSpent :: Spent -> Double -> IO ()   -- ^ a turn's end: what it cost, in how many seconds
  , uiDone :: IO ()                       -- ^ the chat's end: no more lines will be taken
  , uiLeave :: IO ()                      -- ^ just before the process runs itself again: the terminal is put back
  }

-- | The streams: the agent's words, its tool calls and their answers on standard output, the thoughts and
-- the harness's notes on standard error, a @> @ prompt when a line is waited for.
stdoutUi :: Ui
stdoutUi = Ui
  { uiView = \v -> TIO.putStrLn v >> hFlush stdout
  , uiTalk = \t -> TIO.putStrLn t >> putStrLn "" >> hFlush stdout
  , uiThought = \t -> hPutStrLn stderr ("\n[thinking] " ++ T.unpack t ++ "\n")
  , uiCall = \name args -> putStrLn ("> " ++ name ++ " " ++ args) >> hFlush stdout
  , uiAnswer = \out -> TIO.putStrLn (T.pack "  " <> T.replace (T.pack "\n") (T.pack "\n  ") out) >> hFlush stdout
  , uiNote = hPutStrLn stderr
  , uiBusy = \b -> case b of { Nothing -> putStr "> " >> hFlush stdout; Just _ -> pure () }
  , uiSpent = \s secs -> hPutStrLn stderr (spentLine s secs)
  , uiDone = pure ()
  , uiLeave = hFlush stdout >> hFlush stderr
  }
