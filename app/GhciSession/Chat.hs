{-# LANGUAGE ScopedTypeVariables #-}
-- | __The endless chat__: @ghci-session chat@, an agent that works on a session as its sandbox and
-- remembers through the session's history (UniiChat's turn loop, over the daemon's log, tree and view).
--
-- > ghci-session chat                      # from the project: the chat, on the default session
-- > ghci-session chat -s dev               # a session
-- > ghci-session chat --once 'what was tried on Raster.depth last week?'
-- > ghci-session chat --instructions AGENTS.md
--
-- Each message starts a FRESH model call: no conversation is carried over. The call sees the system prompt
-- ('H.systemPrompt', 'master', the instructions file), then the view -- the whole history as one-line summaries,
-- rendered by the daemon before the new message is logged -- then the message. Its tools are the session's
-- operations (eval, status, typecheck, reload, test, doc, census, bench, mem: the daemon logs each with
-- its answer), the memory's (zoom, date), and the agent's hands on the files (read, write, edit, ls, sh),
-- which this harness logs. Replies are logged as @talk@; thoughts are shown and never logged. A line typed
-- while the agent works reaches it between tool calls. The summaries are the daemon's business
-- (@summarize_cmd@: 'summarizeMain' is the compactor, @ghci-session summarize@); a turn waits for the view
-- to settle first.
--
-- What a turn learned from running: a save answers with the verdict of the reload it caused, and behind a
-- bad verdict the compiler's diagnostics or the failing tests, so no status call follows a write; an eval
-- of several lines runs its leading imports as their own commands (in one GHCi block they do not parse);
-- an argument the model calls by another name is taken by its name, and a missing one is said; a reply cut
-- off at the output limit (the thinking ran on) is asked to go on in smaller steps, not taken as the end
-- of the turn. The model is 'GhciSession.Llm' -- DeepSeek's flash model by default, which caches the
-- prompt's prefix on its own, so the stable layout above (system, then the view whose start does not
-- change from turn to turn, then the message) is what makes a turn cheap.
module GhciSession.Chat
  ( chatMain, summarizeMain
  -- (the pure parts, for the self-tests)
  , arguments, chatTools, toolJson, applyEdit, agentShow, isWork, splitImports, writeRuns, groupByPaths, nearest, fuzzyReplace, replaceOnce, editPaths, saveWait, isRed, ownGhci, capWith, shCap
  , shownCut, parseOpts, Opts (..), hookEnv, unchangedNote, tcClean, numbered, numberBar, readHeader, budgetSaid, ownTurnStart, TurnState (..), ViewCtx (..), Spent (..), turnJson, turnFrom, renderTail, newBound, viewLines, tailMax, planMax, renderPlan
  , ReadRec (..), readRange, readAgainst, trimAt
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent (ThreadId, myThreadId, throwTo)
import Control.Exception (Exception, IOException, SomeException, catch, finally, fromException, onException, throwIO, try)
import Data.IORef
import Control.Monad (forM, forM_, unless, void, when)
import qualified Data.ByteString as B
import Data.Char (isAlphaNum, isSpace)
import Data.List (group, groupBy, intercalate, isInfixOf, isPrefixOf, isSuffixOf, partition, sort, tails)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe, isJust, isNothing)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getFileSize, getModificationTime, getTemporaryDirectory, listDirectory, removeFile, renameFile)
import System.Environment (getArgs, getEnvironment, getExecutablePath, lookupEnv, setEnv)
import System.Posix.Process (ProcessStatus, executeFile, getProcessID, getProcessStatus)
import System.Posix.Signals (installHandler, Handler (..), nullSignal, sigHUP, sigKILL, sigTERM, signalProcess, signalProcessGroup)
import System.Posix.Types (CPid, Fd)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, normalise, splitDirectories, takeDirectory, takeFileName, (</>))
import System.IO
import System.Process
import System.Timeout (timeout)
import Text.Printf (printf)
import qualified GhciSession.Search as Search
import qualified GhciSession.Image as Img
import qualified GhciSession.Vfs as Vfs

import GhciSession.ChatTui (chatTui)
import GhciSession.ChatUi
import GhciSession.Config
import GhciSession.Json
import GhciSession.Llm
import qualified GhciSession.History as H
import GhciSession.Mcp (Tool (..), pick, tools)
import qualified GhciSession.Mcp as Mcp
import GhciSession.Sys (now)
import qualified GhciSession.Sys as Sys
import qualified GhciSession.Wire as Wire
import GhciSession.Inbox (chatPidFile)
import GhciSession.Carry
import GhciSession.Roll
import GhciSession.Quota
import GhciSession.Replay (replayMain)
import qualified GhciSession.Inbox as Inbox
import System.IO.Unsafe (unsafePerformIO)
import qualified GhciSession.Anthropic as A
import qualified GhciSession.ClaudeCli as C
import qualified Data.ByteString.Char8 as B8
import qualified System.Posix.IO as PIO

-- | Characters of a tool result kept (head and tail), as the spec logs them.
capChars :: Int
capChars = 30000

-- | Seconds a write or edit waits for the verdict of the reload it causes, at least: a project whose check
-- takes longer gets three times the longest verdict seen (and a margin), up to ten minutes -- a wait that
-- ends before the verdict only sends the agent to ask status, reload and test by hand. (The longest, not the
-- last: the last is often a typecheck-only verdict of no duration, while the reload behind it takes the check's.)
saveWait :: Double -> Double
saveWait longest = min 600 (max 45 (3 * longest + 15))

-- the options ------------------------------------------------------------------------------

data Opts = Opts
  { oSession :: Maybe String, oOnce :: Maybe String, oInstructions :: Maybe FilePath, oModel :: Maybe String, oBase :: Maybe String
  , oMaxTokens :: Int, oMaxSteps :: Int, oSettle :: Double, oUsage :: Bool, oPrintView :: Bool
  , oRestart :: Bool, oSend :: Maybe String, oResume :: Maybe FilePath
  , oView :: Bool, oTail :: Int, oEffort :: Maybe String, oPlan :: Int, oTui :: Bool, oWeb :: Maybe Int, oContinue :: Bool, oRollover :: Int, oShow :: Int
  , oWait :: Maybe (Maybe Double)       -- ^ --wait [SECS]: Just Nothing waits as long as it takes
  , oCarry :: Int }                     -- ^ --carry N: what the last N rollovers' fresh calls carried, part by part (0: not asked)

chatUsage :: String
chatUsage = unlines
  [ "ghci-session chat [-s SESSION] [--once MESSAGE] [--instructions FILE] [--model M] [--base-url URL]"
  , "                  [--max-tokens N] [--max-steps N] [--settle SECS] [--usage] [--print-view] [--context turn|view] [--tail BYTES] [--effort none|low|high|max] [--plan BYTES] [--show BYTES] [--continue] [--rollover TOKENS]"
  , "ghci-session chat --tui [-s SESSION] ...   the same, on a screen of its own"
  , "ghci-session chat --restart [-s SESSION]"
  , "ghci-session chat --send MESSAGE [-s SESSION]   a line for the session's running chat, as if typed at it"
  , "ghci-session chat [--send MESSAGE] --wait [SECS] [-s SESSION]   and wait until the turn that took it has ended: its last words and summary"
  , "  are printed; exit 0 ended, 3 SECS passed first (still running, with its tool calls so far), 1 no chat running or it died; alone, the turn"
  , "  under way (at once if the chat is at rest: the last turn's last words); \"on_turn_end\": COMMAND in ghci-session.json runs when a turn ends"
  , "  the endless chat with an agent on the session (DEEPSEEK_API_KEY or OPENAI_API_KEY); --once: one message, then exit;"
  , "  --instructions: a file of the user's own instructions (an AGENTS.md), appended to the system prompt;"
  , "  --max-steps: tool calls per turn (60); --settle: seconds to wait for the view's last lines to be summarized (120);"
  , "  --usage: print each call's tokens and seconds;"
  , "  --context view: every model call is built from the log -- the view up to a boundary, the message, and the"
  , "  log after the boundary whole (<recent>), which moves on in batches once over --tail bytes (96000; 0: the view"
  , "  alone, every step waiting for the compactor); turn (the default): the view at the turn's start, then the turn's"
  , "  own conversation;"
  , "  --effort: reasoning effort (none, low, high, max); none disables thinking;"
  , "  --show: characters of a tool's answer shown (and written to the screen log) before it is cut with ... (600; 0: whole);"
  , "  --plan: bytes of the turn's plan and directives kept in <plan> when the boundary moves (32000);"
  , "  --rollover: a turn through the claude command whose context has grown past this many tokens goes on in a"
  , "  fresh call, from its log (auto: the controller's threshold, from what a fresh call and a call's growth cost; 0: never);"
  , "  --carry N: what the fresh calls after the last N rollovers were given, part by part, in bytes (no model call is made);"
  , "  --replay-rollover [--ratio R] FILE...: the controller over recorded chat logs (their [usage:] lines): the resets it would have made and"
  , "  what they cost against a fixed 150000 and 110000 tokens (no session, no model call; --usage logs are what it reads);"
  , "  --continue: go on with the last turn of the history, from its log (a turn that did not end: the chat was"
  , "  stopped, or died, in the middle of it);"
  , "  --tui: the chat on a screen of its own (the transcript, the turn's state, a line to type on; Ctrl-C leaves);"
  , "  --restart: the session's running chat restarts as the executable now on disk (build it first), in the middle"
  , "  of its turn, which goes on where it was: it writes the turn out before its next model call and runs itself"
  , "  again with --resume FILE (the same as kill -HUP)" ]

parseOpts :: [String] -> Either String Opts
parseOpts = go (Opts Nothing Nothing Nothing Nothing Nothing 8000 60 120 False False False Nothing Nothing False tailMax Nothing planMax False Nothing False (-1) shownMax Nothing 0)
  -- (--rollover's default, -1, is the controller's)
  where
    go o [] = Right o
    go o (k : v : r) | k `elem` ["-s", "--session"] = go o { oSession = Just v } r
                     | k == "--once" = go o { oOnce = Just v } r
                     | k == "--instructions" = go o { oInstructions = Just v } r
                     | k == "--model" = go o { oModel = Just v } r
                     | k == "--base-url" = go o { oBase = Just v } r
                     | k == "--max-tokens", [(n, "")] <- reads v = go o { oMaxTokens = n } r
                     | k == "--max-steps", [(n, "")] <- reads v = go o { oMaxSteps = n } r
                     | k == "--settle", [(n, "")] <- reads v = go o { oSettle = n } r
                     | k == "--resume" = go o { oResume = Just v } r
                     | k == "--send" = go o { oSend = Just v } r
                     | k == "--context", v `elem` ["turn", "view"] = go o { oView = v == "view" } r
                     | k == "--tail", [(n, "")] <- reads v = go o { oTail = n } r
                     | k == "--effort", v `elem` ["none", "low", "high", "max"] = go o { oEffort = Just v } r
                     | k == "--rollover", [(n, "")] <- reads v, (n :: Int) >= 0 = go o { oRollover = n } r
                     | k == "--rollover", v == "auto" = go o { oRollover = -1 } r
                     | k == "--web", [(n, "")] <- reads v, (n :: Int) >= 0 = go o { oWeb = Just n } r
                     | k == "--plan", [(n, "")] <- reads v = go o { oPlan = n } r
                     | k == "--show", [(n, "")] <- reads v, (n :: Int) >= 0 = go o { oShow = n } r
                     | k == "--carry", [(n, "")] <- reads v, (n :: Int) > 0 = go o { oCarry = n } r
                     | k == "--wait", [(n, "")] <- reads v, n >= 0 = go o { oWait = Just (Just n) } r
    go o (k : r) | k == "--usage" = go o { oUsage = True } r
                 | k == "--print-view" = go o { oPrintView = True } r
                 | k == "--restart" = go o { oRestart = True } r
                 | k == "--tui" = go o { oTui = True } r
                 | k == "--continue" = go o { oContinue = True } r
                 | k == "--wait" = go o { oWait = Just Nothing } r
    go _ (k : _) = Left ("chat: unexpected argument " ++ show k ++ "\n" ++ chatUsage)

-- the session ------------------------------------------------------------------------------

data Chat = Chat { cConf :: Conf, cName :: String, cDir :: FilePath, cWatched :: [FilePath], cAgent :: String
                 , cLongest :: IORef Double     -- ^ the longest verdict (a reload with its check) seen: what a save may take
                 , cPending :: IORef (Maybe Double)     -- ^ a save whose CHECK was still running when its answer went out: when it was written
                 , cDown :: IORef Bool                  -- ^ the last wait for the session to come back ended with it still down
                 , cRestart :: IORef Bool               -- ^ a restart was asked (SIGHUP): at the next model call
                 , cInTurn :: IORef Bool                -- ^ a turn is running (else a restart is at once)
                 , cSub :: Maybe (String, FilePath)     -- ^ a subagent's: its name and the file its chat is kept in (the agent's own: Nothing)
                 , cAgents :: IORef (M.Map String Sub)   -- ^ the subagents there are, at work or done
                 , cStart :: IORef (Maybe (Sub -> String -> IO ()))   -- ^ how one is run (a turn of its own, to its report), once the chat knows its endpoint
                 , cTurn :: IORef (Maybe ThreadId)      -- ^ the thread of the turn under way, for stopping it
                 , cStopped :: IORef Bool               -- ^ the turn under way was asked to stop
                 , cExe :: FilePath, cArgv :: [String]   -- ^ the executable and the arguments to run again as
                 , cBatch :: IORef (Maybe [Double])      -- ^ file writes of one reply are being made together: when each was written (they are not waited on one by one)
                 , cUi :: Ui                             -- ^ where what happens is shown
                 , cShow :: Int                          -- ^ characters of a tool's answer shown (--show; 0: whole)
                 , cRoll :: IORef Roll                   -- ^ what the rollover's controller has learned (kept in the history's directory)
                 }

-- | What a tool's answer is cut to for showing, unless the chat is told (--show).
shownMax :: Int
shownMax = 600

-- | A tool's answer cut for showing: at the characters, with ... when it was longer; 0 or less: whole.
shownCut :: Int -> T.Text -> T.Text
shownCut n t | n <= 0 || T.length t <= n = t
             | otherwise = T.take n t <> T.pack "..."

-- | Where the rollover's controller keeps what it has learned: beside the session's history.
rollFile :: Conf -> String -> FilePath
rollFile conf name = cStateDir conf </> name </> "history" </> "roll.json"

-- | The session's usage ledger: one line per model call, the chat's and the compactor's.
usageFile :: Chat -> FilePath
usageFile ch = cStateDir (cConf ch) </> cName ch </> "usage.jsonl"

-- | Where the session's images are kept ("GhciSession.Image").
imagesDir :: Chat -> FilePath
imagesDir ch = cStateDir (cConf ch) </> cName ch </> "images"

-- | Lines typed, with the images they name.
typedLines :: Chat -> [T.Text] -> IO [T.Text]
typedLines ch ls = do
  out <- mapM (Img.typed (imagesDir ch) (cDir ch)) ls
  -- (said, so that it is seen what went with the line: a screen that shows pictures shows them here)
  let new = [ n | (a, b) <- zip ls out, n <- Img.namesIn b, n `notElem` Img.namesIn a ]
  unless (null new) (uiNote (cUi ch) (T.unpack (T.intercalate (T.pack "\n") (T.pack "with the line:" : map Img.marker new))))
  pure out

-- | Is this path one the daemon watches (its targets' directories), or a build file? A save of one has a verdict.
watches :: Chat -> FilePath -> Bool
watches ch p0 = ".cabal" `isSuffixOf` p || takeFileName p == "cabal.project" || any (\d -> p == d || (d ++ "/") `isPrefixOf` p) (cWatched ch)
  where p = normalise p0

-- | A request to the daemon, as the command line sends it.
ask :: Chat -> String -> [(String, Json)] -> IO Json
ask ch op args = Mcp.request (cConf ch) (cName ch) (JObj (("op", JStr op) : args))

-- | A line of the history, on the user's or the agent's behalf.
logH :: Chat -> String -> T.Text -> IO ()
logH ch kind text = void (logId ch kind text)

-- | A line of the history, and its id.
logId :: Chat -> String -> T.Text -> IO (Maybe Int)
logId ch kind text | Just (_, file) <- cSub ch = agentAppend file kind text >> pure Nothing
logId ch kind text = do
  noteLogged ch kind text
  r <- try (ask ch "log" [("kind", JStr kind), ("text", JText text)]) :: IO (Either SomeException Json)
  pure $ case r of
    Right j | Just ('#' : n) <- lookupStr "out" j, [(i, "")] <- reads n -> Just i
    _ -> Nothing

-- | The view: its text, whether every line is a summary, how many lines, how many messages.
view :: Chat -> Double -> IO (T.Text, Bool, Int, Int)
view ch wait = do
  r <- ask ch "view" [("json", JBool True), ("wait", JNum wait)]
  let out = fromMaybe T.empty (lookupText "out" r)
  pure $ case parseJsonBS (TE.encodeUtf8 out) of
    Right j | isJust (lookupText "view" j) -> (fromMaybe T.empty (lookupText "view" j), lookupBool "settled" j /= Just False, maybe 0 round (lookupNum "parts" j), maybe 0 round (lookupNum "messages" j))
    _ -> (out, False, 0, 0)

-- | The verdict now: its time stamp, its line, what is behind it -- the compiler's diagnostics
-- (file:line:col and the message whole) behind a COMPILE-ERROR, the failing lines behind a CHECK-FAIL,
-- nothing behind an OK. Nothing when no session answers; the longest duration seen is remembered on the way.
-- @vStart@: when the reload that gave it began (a save's verdict is one whose reload began after the file was
-- written -- not the end of a check that was already running); @vRunning@: the code compiled and its check
-- is running now, so the line is the compile verdict.
data Verdict = Verdict { vStart :: Double, vLine :: String, vBehind :: [String], vRunning :: Bool }

verdictAt :: Chat -> IO (Maybe Verdict)
verdictAt ch = do
  r <- ask ch "status" []
  let j = r .: "status"
  forM_ (lookupNum "duration_s" j) (\d -> modifyIORef' (cLongest ch) (max d))
  let ck = r .: "checking"
  pure $ case (lookupBool "ok" r, obj j, lookupNum "at" j) of
    (Just _, _, _) | Just began <- lookupNum "began" ck ->
      Just (Verdict began (fromMaybe "OK" (lookupStr "compiled" ck) ++ " -- compiles; the check is running") [] True)
    (Just _, Just _, Just at) ->
      let headL = takeWhile (/= '\n') (fromMaybe "" (lookupStr "out" r))
          detail = strs (j .: "detail")
          ds = [ d | d <- lookupArr "diagnostics" j, isJust (obj d) ]
          errs = case [ d | d <- ds, lookupStr "severity" d == Just "error" ] of { [] -> ds; es -> es }
          diag d = fromMaybe "?" (lookupStr "file" d) ++ ":" ++ maybe "?" (show . (round :: Double -> Int)) (lookupNum "line" d) ++ ":" ++ maybe "?" (show . (round :: Double -> Int)) (lookupNum "col" d)
                   ++ ": " ++ fromMaybe "" (lookupStr "severity" d) ++ ": " ++ trim (fromMaybe "" (lookupStr "message" d))
          behind = case lookupStr "kind" j of
            -- (a session that could not start: the build tool's last words are why)
            Just k | k `elem` ["DEAD", "PREBUILD-ERROR", "CONFIG-ERROR"] -> detail
            Just "COMPILE-ERROR" -> (if null errs then detail else map diag (take 12 errs) ++ [ "[... " ++ show (length errs - 12) ++ " more]" | length errs > 12 ])
            Just "CHECK-FAIL" -> detail
            Just "CHECK-HANG" -> detail
            _ -> []
      in Just (Verdict (at - fromMaybe 0 (lookupNum "duration_s" j)) headL behind False)
    _ -> Nothing

-- | The verdict of a reload that began after @written@ (a save's), within the seconds; else Nothing. Once the
-- code compiles and its check runs, the check is given a few seconds more ('checkGrace') and then the compile
-- verdict is the answer: the check's own comes later ('pendingNote').
verdictAfter :: Chat -> Double -> Double -> IO (Maybe Verdict)
verdictAfter ch written secs = do
  t0 <- now
  let go running = do
        threadDelay 50000
        t <- now
        v <- verdictAt ch
        case v of
          -- (a session that is starting -- a build file was saved -- has no verdict yet: the start's is waited
          -- for, as long as a start takes; "STALE(5) starting" was given as a save's verdict, and said nothing)
          Just r | starting r -> if t - t0 >= max secs startWait then pure Nothing else threadDelay 250000 >> go running
          Just r | vStart r >= written - 0.05, not (vRunning r) -> pure (Just r)
          Just r | vStart r >= written - 0.05 -> do
            let since = fromMaybe t running
            if t - since >= checkGrace then pure (Just r) else go (Just since)
          _ | t - t0 >= secs -> pure Nothing
            | otherwise -> go running
  go Nothing

-- | Is this the session saying it is starting, and no verdict?
starting :: Verdict -> Bool
starting r = case words (vLine r) of
  ("starting" : _) -> True
  (w : "starting" : _) -> "STALE(" `isPrefixOf` w
  _ -> False

-- | Seconds a save waits for a session that is starting to have started.
startWait :: Double
startWait = 180

-- | Seconds a save waits, once its code compiles, for a check that runs: a quick check's verdict comes with
-- the save; a longer one is not waited for -- the agent goes on, and gets it with a later tool result.
-- (Saves were 79 of an agent's 249 tool minutes, most of it checks of 20-100 s, once per edit of a fix.)
checkGrace :: Double
checkGrace = 0

-- | The verdict of the check a save left running, once it is in: a note for the tool result it rides on.
pendingNote :: Chat -> IO T.Text
pendingNote ch = do
  p <- readIORef (cPending ch)
  case p of
    Nothing -> pure T.empty
    Just written -> do
      v <- verdictAt ch
      case v of
        Just r | not (vRunning r), vStart r >= written - 0.05 -> do
          writeIORef (cPending ch) Nothing
          pure (T.pack ("\n[the reload (and check) of your earlier save has finished: " ++ vLine r ++ concatMap ("\n" ++) (vBehind r) ++ "]"))
        _ -> pure T.empty

-- | Wait for the check a save left running (at the end of a turn, before its verdict is judged).
awaitPending :: Chat -> IO ()
awaitPending ch = do
  p <- readIORef (cPending ch)
  forM_ p $ \written -> do
    longest <- readIORef (cLongest ch)
    v <- verdictAfter' written (saveWait longest)
    when (isJust v) (writeIORef (cPending ch) Nothing)
  where
    verdictAfter' written secs = do
      t0 <- now
      let go = do
            threadDelay 50000
            t <- now
            v <- verdictAt ch
            case v of
              Just r | not (vRunning r), vStart r >= written - 0.05 -> pure (Just r)
              _ | t - t0 >= secs -> pure Nothing
                | otherwise -> go
      go

-- | Is the answer of a typecheck a clean one? (@OK -- TYPECHECK@, with warnings or not.)
tcClean :: T.Text -> Bool
tcClean = T.isPrefixOf (T.pack "OK")

-- | Has the session typechecked the sources of a save, and found them clean, while its reload has no verdict yet?
-- The typecheck is in within a fraction of a second; the reload may take half a minute. An error is left to the
-- verdict (it comes at once); so is a session that does not typecheck first, or an old daemon that does not know
-- the question: this waits ten seconds at most, and says no.
typecheckEarly :: Chat -> Double -> IO Bool
typecheckEarly ch written = do
  t0 <- now
  let go = do
        threadDelay 50000
        t <- now
        r <- try (ask ch "typecheck_cached" []) :: IO (Either SomeException Json)
        v <- verdictAt ch
        case (v, r) of
          (Just rv, _) | vStart rv >= written - 0.05 -> pure False
          (_, Right j) | lookupBool "ok" j == Just True, tcClean (fromMaybe T.empty (lookupText "out" j)) -> pure True
          (_, Right j) | lookupBool "ok" j /= Just True -> pure False
          _ | t - t0 >= 10 -> pure False
            | otherwise -> go
  go

-- | A save's answer: what was written, and the verdict of the reload it caused, when the file is one the
-- session watches. A verdict that is not there within the wait is said to be pending (a long compile):
-- status has it later. The save itself is good either way.
saved :: Chat -> String -> Maybe Double -> IO (Bool, T.Text)
saved _ what Nothing = pure (True, T.pack what)
saved ch what (Just written) = do
  b <- readIORef (cBatch ch)
  case b of
    -- one of several writes made together ('runBatch'): the reload and its verdict are the batch's, not each one's
    Just _ -> do
      atomicModifyIORef' (cBatch ch) (\m -> (fmap (written :) m, ()))
      pure (True, T.pack (what ++ "\n[saved with the others of this reply: one reload, its verdict with the last result]"))
    Nothing -> savedNow ch what (Just written)

-- | A save's answer, waiting for the verdict of the reload it caused.
savedNow :: Chat -> String -> Maybe Double -> IO (Bool, T.Text)
savedNow _ what Nothing = pure (True, T.pack what)
savedNow ch what (Just written) = do
  longest <- readIORef (cLongest ch)
  let wait = saveWait longest
  early <- typecheckEarly ch written
  v <- if early then pure Nothing else verdictAfter ch written wait
  case v of
    _ | early -> do
      -- (the reload goes on under the session's work lock, which the watcher took before it typechecked: an eval or
      -- a test asked now queues behind it, so none runs on the old code; its verdict comes with a later tool result)
      writeIORef (cPending ch) (Just written)
      pure (True, T.pack (what ++ "\ntypecheck: OK -- the reload is under way, its verdict comes with a later tool result (status with wait gives it); an eval or test now waits for it"))
    Nothing -> pure (True, T.pack (what ++ printf "\n[the session has not finished reloading this save after %.0fs (the longest verdict so far took %.0fs): status will have its verdict; do not reload by hand]" wait longest))
    Just r | vRunning r -> do
      writeIORef (cPending ch) (Just written)
      pure (True, T.pack (what ++ "\nverdict: " ++ vLine r ++ " -- its verdict comes with a later tool result; go on meanwhile"))
    Just r -> do
      -- (a newer save's verdict settles any check an earlier one left running)
      modifyIORef' (cPending ch) (\p -> case p of { Just w | w <= written -> Nothing; _ -> p })
      pure (not (isRed (vLine r)), T.pack (what ++ "\nverdict: " ++ vLine r ++ concatMap ("\n" ++) (vBehind r)))

-- | The runs of two or more file-writing calls one after the other in a reply (their indices): written
-- together and reloaded once.
writeRuns :: [String] -> [[Int]]
writeRuns names = [ map fst r | r@((_, True) : _ : _) <- groupBy (\a b -> snd a == snd b) (zip [0 ..] (map (`elem` ["write", "edit", "edits"]) names)) ]

-- | Calls that touch a common file stay together, in order; the groups share no file, so they may run at once.
groupByPaths :: [(Int, [FilePath])] -> [[Int]]
groupByPaths = map (sort . fst) . foldl add []
  where
    add gs (i, ps) =
      let (hit, miss) = partition (\(_, qs) -> any (`elem` qs) ps) gs
      in miss ++ [(i : concatMap fst hit, ps ++ concatMap snd hit)]

-- | The files of a write call, relative to the project.
callPaths :: Chat -> Tool -> ToolCall -> [FilePath]
callPaths ch t tc = map (normalise . makeRelative (cDir ch)) (maybe [] pure (lookupStr "path" a) ++ [ p | tName t == "edits", e <- lookupArr "edits" a, Just p <- [lookupStr "path" e] ])
  where a = fst (arguments t (tcArgs tc))

-- | The writes of a reply that come one after the other are made together: the session is told to hold its
-- reloads, the writes run at once (those on the same file in order), and one release reloads once -- one
-- compile and one verdict for the lot, which rides on the last result. (Written one by one, each save waited
-- for its own reload: ten files were ten reloads.) The calls are logged first, as the agent made them.
runBatches :: Chat -> [(String, Tool)] -> [ToolCall] -> IO (M.Map Int (Bool, T.Text))
runBatches ch byName calls = M.unions <$> mapM (runBatch ch byName calls) (writeRuns (map tcName calls))

runBatch :: Chat -> [(String, Tool)] -> [ToolCall] -> [Int] -> IO (M.Map Int (Bool, T.Text))
runBatch ch byName calls run = do
  forM_ run $ \i -> do
    let tc = calls !! i
    uiCall (cUi ch) (tcName tc) (take 300 (encode (tcArgs tc)))
    logH ch "tool" (T.pack (tcName tc ++ " " ++ encode (tcArgs tc)))
  let paths i = maybe [] (\t -> callPaths ch t (calls !! i)) (lookup (tcName (calls !! i)) byName)
      groups = groupByPaths [ (i, paths i) | i <- run ]
      watched = any (watches ch) (concatMap paths run)
  uiNote (cUi ch) (printf "[%d file writes in this reply: made together, %d at once, reloaded once]" (length run) (length groups))
  held <- if watched then (\r -> lookupBool "ok" r == Just True) <$> ask ch "hold" [("secs", JNum 30), ("quiet", JBool True)] else pure False
  writeIORef (cBatch ch) (Just [])
  let one i = do
        let tc = calls !! i
        r <- try (case lookup (tcName tc) byName of
                    Just t -> runTool ch t (tcArgs tc)
                    Nothing -> pure (False, T.pack ("unknown tool " ++ show (tcName tc))))
        pure (i, either (\(e :: SomeException) -> (False, T.pack (tcName tc ++ ": " ++ show e))) id r)
  dones <- forM groups $ \g -> do
    mv <- newEmptyMVar
    _ <- forkIO (mapM one g >>= putMVar mv)
    pure mv
  rs <- concat <$> mapM takeMVar dones
  (do ws <- readIORef (cBatch ch)
      writeIORef (cBatch ch) Nothing
      when held (void (ask ch "release" [("quiet", JBool True)]))
      let m = M.fromList rs
      case ws of
        Just ts@(_ : _) -> do
          (okV, txt) <- savedNow ch (printf "[batch: %d writes, reloaded once]" (length run)) (Just (minimum ts))
          pure (M.adjust (\(ok, o) -> (ok && okV, o <> T.pack "\n" <> txt)) (last run) m)
        _ -> pure m)
    `finally` (writeIORef (cBatch ch) Nothing)

-- | When a save is written, if the session is up and watches the file: the time, for 'saved'.
writtenAt :: Chat -> FilePath -> IO (Maybe Double)
writtenAt ch rel = if not (watches ch rel) then pure Nothing else do
  up <- isJust <$> verdictAt ch
  if up then Just <$> now else pure Nothing

-- | Is a verdict line a bad one: a compile error, failing or hung tests, a dead repl.
isRed :: String -> Bool
isRed l = any (`isInfixOf` l) ["ERROR", "FAIL", "HANG", "DEAD"]

-- the tools ----------------------------------------------------------------------------------

sessionToolNames :: [String]
sessionToolNames = ["eval", "status", "typecheck", "reload", "test", "doc", "census", "bench", "mem", "zoom", "date", "recall", "remember", "restart"]

-- | The session's tools (as the MCP server defines them, the chat being one session) and the agent's hands on the files.
chatTools :: [Tool]
chatTools =
  [ t { tProps = [ if k == "timeout" then (k, ("number", timeoutDesc)) else p | p@(k, _) <- tProps t, k /= "session" ] ++ [ ("wait", ("number", "seconds to wait for a verdict, while the session is starting or its check is running (default: none, the state as it is now)")) | tName t == "status" ]
      , tDesc = if tName t == "eval" then evalDesc else tDesc t } | t <- tools, tName t `elem` sessionToolNames ]
  ++ [ Tool "read" "A file of the project, each line as its number, the bar │, then the line exactly as the file has it (the indentation is every space after the bar; the number and the bar are not part of the line): from a line (start), or from the first line that holds a text (around) -- which is how to read a definition, a section of a reference or of a data file, without knowing its line. An image (png, jpeg, gif, webp) is shown to you as an image." [("path", ("string", "relative to the project")), ("start", ("number", "first line (default 1)")), ("around", ("string", "a text: reading starts a few lines before the first line that holds it, and the other lines that hold it are named")), ("before", ("number", "with around: lines shown before the match (default 3)")), ("lines", ("number", "how many (default 200; 60 with around)"))] ["path"]
     , Tool "grep" "Search file contents: across the project for an identifier, function, or pattern (the high-speed FFF engine; line numbers, content, git status) -- or, with path, in ONE file of any kind or size, each match with the lines after it (context), as read shows lines: >number│text for a match, number│text for a context line, the text exactly as the file has it. Always use this instead of running grep via sh." [("query", ("string", "the identifier or pattern to search for (with path: a text, as it is)")), ("path", ("string", "one file to search, relative to the project")), ("context", ("number", "with path: lines shown after each match (default 0)")), ("lines", ("number", "max matches (default 30)"))] ["query"]
     , Tool "find" "Fuzzy search file names across the project using FFF frecency and git status ranking. Always use this to locate files instead of find via sh." [("query", ("string", "filename or partial path")), ("n", ("number", "max results (default 20)"))] ["query"]
     , Tool "write" "Write a file of the project whole. A watched source (or a .cabal) is reloaded by the session itself and the answer carries the verdict of that reload, and a diff of what the write changed: NO status call is needed after it." [("path", ("string", "relative to the project")), ("content", ("string", "the whole content"))] ["path", "content"]
     , Tool "edit" "Change a file of the project, in one of four ways: old and new (one exact, unique occurrence of a text replaced); append: true and new (put at the file's end; the file is made if it is not there); first, until and new (the lines from the one that holds the text `first` up to, not including, the next that holds the text `until` are replaced -- a whole definition, without writing the old one out); start, end and new (those lines, by number). A watched source (or a .cabal) is reloaded by the session itself and the answer carries the verdict of that reload, and a diff of what the edit changed: NO status call is needed after it." [("path", ("string", "relative to the project")), ("old", ("string", "the text as it is, unique in the file")), ("new", ("string", "the replacement, or what is appended")), ("append", ("boolean", "put new at the end of the file")), ("first", ("string", "a text only the first line to replace holds")), ("until", ("string", "a text of the first line after them: it is kept")), ("start", ("number", "the first line to replace")), ("end", ("number", "the last (default: start)"))] ["path", "new"]
     , Tool "edits" "Several replacements at once, in one file or several: each is checked against the file (as the replacements before it leave it) before anything is written, then all are written together -- ONE reload and ONE verdict instead of one per edit, with a diff of each file. Use it for any change that touches more than one spot." [("edits", ("array", "the replacements, in order: each {\"path\", \"old\", \"new\"}, the old text exactly as it is (unique in its file) -- or, instead of old, any of edit's other forms (append, first and until, start and end); one without a path is in the file of the one before it")), ("path", ("string", "the file of the replacements that name none (optional)"))] ["edits"]
     , Tool "ls" "List a directory of the project." [("path", ("string", "relative to the project (default: the root)"))] []
     , Tool "vfs" "Virtual File System & Line Budget inspector. Inspect line counts, byte sizes, budget compliance (<250 lines), and git status for files loaded by the session or matching a path. Extremely fast (<1ms in-memory). Always use this instead of running wc -l or du via sh." [("path", ("string", "optional path or pattern filter (e.g. 'src', 'Gba/Cpu', or empty for all loaded files)")), ("budget", ("number", "line budget threshold to check against (default 250)"))] []
     , Tool "sh" "Run a shell command in the project's directory: its output, then how it ended and how long it took, as [exit 0 in 0.4 s]. Do NOT run cabal build, ghci, or grep here: the session is already warm and grep/find tools are built-in." [("cmd", ("string", "the command")), ("timeout", ("number", "seconds (default 120)"))] ["cmd"]
     , Tool "spawn" "Start one subagent per task, in parallel, and return their names at once. Each one's report reaches you as a message \"[Name] report\" when it finishes. A subagent sees the view and has your tools on the same session and files, but knows what you want only from its task: say in it what to do, where, and what to report." [("tasks", ("strings", "a task each, in full")), ("effort", ("string", "how hard they think (low | high | max); by default, as hard as you"))] ["tasks"]
     , Tool "tell" "Send a message to a subagent, by name. One at work reads it between its tool calls, and its report answers it; one that has finished goes on from its chat, and its reply reaches you as a message \"[Name] reply\"." [("name", ("string", "the subagent's name")), ("message", ("string", "what to tell it"))] ["name", "message"] ]
  where timeoutDesc = "seconds before it is interrupted (default 30): give more for a whole test run or a long benchmark, less to probe for a hang. An evaluation that does not stop when interrupted (a loop that does not allocate, a blocking foreign call) is said so; the session finishes it before its next answer"
        evalDesc = "Evaluate a Haskell expression, or run a GHCi command (:t, :i, :browse, import M), against the LOADED code. The answer is what GHCi printed. Multi-line expressions are automatically wrapped in a GHCi block by the session (do NOT write :{ or :}). ONE expression, command or declaration group per call: make a separate call for an import, and write `let a = 1; b = 2 in ...` on one line."

-- | Each replacement of an edits call with its file: its own path, else that of the replacement before
-- it, else the call's path. (A model writing several replacements to one file often names it only once.)
editPaths :: Maybe String -> [Json] -> [(Maybe String, Json)]
editPaths _ [] = []
editPaths before (e : es) = let p = case lookupStr "path" e of { Just x | not (null (trim x)) -> Just x; _ -> before } in (p, e) : editPaths p es

-- | The edit tool, whose arguments each of the edits' replacements are read as.
editTool :: Tool
editTool = Tool "edit" "" [("path", ("string", "")), ("old", ("string", "")), ("new", ("string", ""))] ["path", "old", "new"]

-- | A tool as the endpoint takes it. (An array is of replacements: the only one any tool takes.)
toolJson :: Tool -> Json
toolJson t = JObj [ ("type", JStr "function"), ("function", JObj
  [ ("name", JStr (tName t)), ("description", JStr (tDesc t))
  , ("parameters", JObj [ ("type", JStr "object")
                        , ("properties", JObj [ (k, JObj (if ty == "strings" then [("type", JStr "array"), ("items", JObj [("type", JStr "string")]), ("description", JStr d)]
                                                                 else [("type", JStr ty), ("description", JStr d)] ++ [ ("items", replacement) | ty == "array" ])) | (k, (ty, d)) <- tProps t ])
                        , ("required", JArr (map JStr (tReq t))) ]) ]) ]
  where replacement = JObj [ ("type", JStr "object")
                           , ("properties", JObj [ (k, JObj [("type", JStr "string")]) | k <- ["path", "old", "new"] ])
                           , ("required", JArr (map JStr ["new"])) ]

-- | What a model calls an argument when it does not call it by its name.
aliases :: [(String, [String])]
aliases = [ ("cmd", ["command", "shell", "script"]), ("path", ["file", "filename", "file_path", "filepath"]), ("content", ["text", "contents", "data"])
          , ("old", ["old_string", "old_text", "from", "search"]), ("new", ["new_string", "new_text", "to", "replace"]), ("expr", ["expression", "code", "command"])
          , ("query", ["q", "name", "words", "pattern", "search", "term"]), ("lines", ["count", "limit"]), ("start", ["from_line", "line", "offset"])
          , ("edits", ["changes", "replacements", "edit_list"]), ("budget", ["limit", "threshold", "max_lines", "max"]) ]

-- | The call's arguments by their names (an alias of an argument THIS tool takes is renamed), and what is missing.
arguments :: Tool -> Json -> (Json, [String])
arguments t a = case a of
  JObj kvs -> let kvs' = foldl rename kvs aliases in (JObj kvs', [ k | k <- tReq t, isNothing (lookup k kvs') ])
  _ -> (JObj [], tReq t)
  where
    props = map fst (tProps t)
    rename kvs (canon, als)
      | canon `elem` props, isNothing (lookup canon kvs), (al : _) <- [ x | x <- als, isJust (lookup x kvs) ] = [ (if k == al then canon else k, v) | (k, v) <- kvs ]
      | otherwise = kvs

-- | (ok, text) of one tool call.
runTool :: Chat -> Tool -> Json -> IO (Bool, T.Text)
runTool ch t a0
  | not (null missing) =
      let emptyHint = if null (obj a0) then " [hint: tool arguments were empty {}; if this was near the 8k token limit, output likely ran out of tokens before writing arguments -- please call the tool directly with less reasoning]" else ""
      in pure (False, T.pack (tName t ++ ": missing argument(s) " ++ intercalate ", " missing ++ "; it takes " ++ intercalate ", " (tReq t) ++ emptyHint))
  | tName t == "eval" = evalTool ch a
  -- (status with a wait: until the session has a verdict -- not starting, no check running -- or the seconds are
  -- up; a `sleep 14; tail daemon.log` through the shell was how that was waited for)
  | tName t == "status", Just secs <- lookupNum "wait" a = do
      t0 <- now
      let settle = do
            v <- verdictAt ch
            tn <- now
            let busy = maybe True (\r -> starting r || vRunning r) v
            when (busy && tn - t0 < min 900 secs) (threadDelay 300000 >> settle)
      settle
      waited <- subtract t0 <$> now
      (ok, out) <- sessionCall ch "status" (set "session" (JStr (cName ch)) a)
      pure (ok, out <> (if waited >= 1 then T.pack (printf "\n[waited %.0fs for it]" waited) else T.empty))
  | tName t == "spawn" = spawnTool ch a
  | tName t == "tell" = tellTool ch a
  -- (a test has the time the session gives one -- five minutes for an expression, the check's own for the whole --
  -- and not an evaluation's thirty seconds: the full suite, asked for without a time, was "timed out after 30s")
  | tName t == "test" = sessionCall ch "test" (set "session" (JStr (cName ch)) a)
  | tName t `elem` sessionToolNames = sessionCall ch (tName t) (withTimeout (set "session" (JStr (cName ch)) a))
  | otherwise = do
      r <- try (fileTool ch (tName t) a) :: IO (Either IOException (Bool, T.Text))
      pure (either (\e -> (False, T.pack ("IOError: " ++ show e))) id r)
  where (a, missing) = arguments t a0

-- | Seconds an eval or a bench may run before it is interrupted, when the call does not say: the session's
-- own default is ten minutes, and an agent's expression that hangs (a loop that never ends) is a dead ten
-- minutes of the turn; two are enough for anything an agent tries, and the answer says how to ask for more.
evalTimeout :: Double
evalTimeout = 30

withTimeout :: Json -> Json
withTimeout a = if isJust (lookupNum "timeout" a) then a else set "timeout" (JNum evalTimeout) a

-- | Seconds a session tool waits for a session that is down -- stopped, or restarting (a restart loads the
-- project again) -- to come back, before it answers that it is down; once a wait ended with it still down,
-- the next calls wait a few seconds only. (Answering at once sent an agent to load the project in a GHCi
-- of its own through sh, cold, on every call: 14 of them, 374 s.)
downWait :: IO Double
downWait = (\v -> fromMaybe 120 (v >>= \x -> case reads x of { [(n, "")] -> Just n; _ -> Nothing })) <$> lookupEnv "GHS_CHAT_DOWN_WAIT"

-- | A session tool, waiting out a session that is down: a call that never reached it is sent again once it
-- is back; one it went away during is not (what it did is not known), and the answer says so.
sessionCall :: Chat -> String -> Json -> IO (Bool, T.Text)
sessionCall ch name args0 = do
  pid <- getProcessID
  -- (who asks goes with it: an evaluation of this chat's is what a stopped turn interrupts)
  let args = set "from" (JStr ("chat-" ++ show pid)) (set "quiet" (JBool True) args0)
  (ok, out, reach) <- Mcp.callReach (cConf ch) name args
  if reach == Mcp.Reached then writeIORef (cDown ch) False >> pure (ok, out) else do
    wasDown <- readIORef (cDown ch)
    limit <- if wasDown then pure 5 else downWait
    t0 <- now
    uiNote (cUi ch) (printf "[the session is down: waiting up to %.0fs for it to come back]" limit)
    back <- waitUp (t0 + limit)
    secs <- subtract t0 <$> now
    writeIORef (cDown ch) (not back)
    case (back, reach) of
      (True, Mcp.Unreached) -> do
        (ok', out', _) <- Mcp.callReach (cConf ch) name args
        pure (ok', T.pack (printf "[the session was down (stopped or restarting); it came back after %.0fs and this is the answer]\n" secs) <> out')
      (True, _) -> pure (False, out <> T.pack (printf "\n[the session went down during this call and came back after %.0fs: the call was not sent again -- send it again if it is still wanted]" secs))
      (False, _) -> pure (False, out <> T.pack (printf "\n[still down after %.0fs of waiting. Starting it (as the answer says) loads the project once, warm for every call after; a GHCi of your own through sh loads it cold on every call]" secs))
  where
    waitUp deadline = do
      r <- timeout 10000000 (ask ch "status" [])
      let up = maybe True (isNothing . lookupStr "down") r     -- (a session that took the call but is busy loading is up)
      t <- now
      if up then pure True else if t >= deadline then pure False else threadDelay 2000000 >> waitUp deadline

-- | An eval: leading import lines (and : commands) are each their own command -- in one block with the
-- expression they do not parse -- and a block that still does not parse is told why.
evalTool :: Chat -> Json -> IO (Bool, T.Text)
evalTool ch a = do
  let rawLs = lines (trim (fromMaybe "" (lookupStr "expr" a)))
      ls = [ l | l <- rawLs, let s = dropWhile isSpace (reverse (dropWhile isSpace l)), s /= ":{" && s /= ":}" ]
      (heads, rest) = splitImports ls
      one e = sessionCall ch "eval" (withTimeout (JObj ([("expr", JStr e), ("session", JStr (cName ch))] ++ [ ("timeout", JNum n) | Just n <- [lookupNum "timeout" a] ])))
  headOuts <- forM heads $ \h -> do
    (ok, out) <- one h
    let isErr = not ok || T.isInfixOf (T.pack "error") out
    pure (ok && not isErr, [ T.pack h <> T.pack "\n" <> out | isErr ])
  let anyHeadFail = any (not . fst) headOuts
      headMsgs = concatMap snd headOuts
  (ok, out) <- if null rest
    then pure (not anyHeadFail, if not anyHeadFail && not (null heads) then T.pack "[ok: imported into session]" else T.empty)
    else do
      let expr = intercalate "\n" rest
      (eOk, eOut) <- one expr
      let isImp s = let t = dropWhile isSpace s in ("import " `isPrefixOf` t || (":" `isPrefixOf` t && not ("::" `isPrefixOf` t))) && t /= ":{" && t /= ":}"
          onlyImports = not (null ls) && all isImp ls
          eOut' = if eOk && T.null (T.strip eOut) && onlyImports
                    then T.pack "[ok: imported into session]"
                    else eOut
      pure (eOk && not anyHeadFail, eOut')
  let hint | '\n' `elem` intercalate "\n" rest && T.isInfixOf (T.pack "parse error") out =
               T.pack "\n[hint: multi-line eval is automatically wrapped in a GHCi block; a let on its own line does not parse there -- write `let a = 1; b = 2 in ...` on one line, or one declaration group per call]"
           | T.isInfixOf (T.pack "timed out after") out && isNothing (lookupNum "timeout" a) =
               T.pack (printf "\n[interrupted after %.0fs, the default: an expression that needs longer says so with timeout]" evalTimeout)
           | otherwise = T.empty
  let allOut = if null headMsgs
                 then (if T.null out then out else out <> hint)
                 else T.intercalate (T.pack "\n") (headMsgs ++ [out <> hint | not (T.null out)])
  pure (ok, allOut)

-- | An eval's leading import lines and : commands, each to be its own command, and the rest. A lone line
-- stays what it is. Semicolon-delimited imports on a single line are expanded into separate commands.
splitImports :: [String] -> ([String], [String])
splitImports rawLs
  | null other && length imports <= 1 = ([], expanded)
  | otherwise = (imports, other)
  where
    expanded = concatMap expandOne rawLs
    expandOne l
      | isImp l && ';' `elem` l =
          let parts = [ p' | p <- splitSemi l, let p' = trim p, not (null p') ]
          in if null parts then [l] else parts
      | otherwise = [l]
    splitSemi "" = []
    splitSemi s = let (w, r) = break (== ';') s in w : case r of { (';' : rest) -> splitSemi rest; _ -> [] }
    isImp s = let t = dropWhile isSpace s
              in ("import " `isPrefixOf` t || (":" `isPrefixOf` t && not ("::" `isPrefixOf` t)))
                 && t /= ":{" && t /= ":}"
    imports = [ dropWhile isSpace l | l <- expanded, isImp l ]
    other = [ l | l <- expanded, not (isImp l) ]

-- | The watched files that differ from the loaded code right now.
staleNow :: Chat -> IO [String]
staleNow ch = (\r -> strs (r .: "stale")) <$> ask ch "status" []

-- | After a shell command: the verdict of the reload it caused, if the session shows one coming -- a file
-- newly among those that differ from the loaded code, or a verdict newer than before the command -- else
-- Nothing. (Not "files differ": with a compile error on disk the session stays that way until it is fixed,
-- and a read-only command then waited the whole wait for a verdict that was never due.)
-- | The watched sources and the build files as they are now: each with when it was written and its size. What a
-- command changed of them is told by the difference -- a walk of the watched directories, a few milliseconds.
sourceStamp :: Chat -> IO [(FilePath, Integer, Integer)]
sourceStamp ch = do
  rootFiles <- either (\(_ :: IOException) -> []) id <$> try (listDirectory (cDir ch))
  let builds = [ f | f <- rootFiles, ".cabal" `isSuffixOf` f || "cabal.project" `isPrefixOf` f || f == "ghci-session.json" ]
  sort . concat <$> mapM walk (nubOrd (cWatched ch ++ builds))
  where
    exts = [".hs", ".hs-boot", ".hsc", ".lhs", ".c", ".h", ".cabal", ".project", ".json", ".x", ".y"]
    walk rel = do
      let p = cDir ch </> rel
      isDir <- doesDirectoryExist p
      if isDir
        then do
          names <- either (\(_ :: IOException) -> []) id <$> try (listDirectory p)
          concat <$> mapM (\n -> walk (rel </> n)) [ n | n <- names, take 1 n /= ".", n /= "dist-newstyle" ]
        else if not (any (`isSuffixOf` rel) exts) && not ("cabal.project" `isPrefixOf` takeFileName rel) then pure [] else do
          r <- try ((,) <$> getModificationTime p <*> getFileSize p) :: IO (Either IOException (UTCTime, Integer))
          pure [ (rel, round (utcTimeToPOSIXSeconds t * 1000), n) | Right (t, n) <- [r] ]
    nubOrd = map head . group . sort

shSaved :: Chat -> Maybe Double -> [String] -> IO (Maybe (Bool, T.Text))
shSaved _ Nothing _ = pure Nothing
shSaved ch (Just written) staleBefore = do
  let look n = do
        stale <- staleNow ch
        v <- verdictAt ch
        let fresh = any (`notElem` staleBefore) stale
            newer = maybe False ((>= written - 0.05) . vStart) v
        if fresh || newer then pure True else if n <= (0 :: Int) then pure False else threadDelay 300000 >> look (n - 1)
  coming <- look 6     -- (the watcher sees a save within a second or two)
  if not coming then pure Nothing else do
    (good, text) <- saved ch "" (Just written)
    pure (Just (good, text))

-- stopping a turn -------------------------------------------------------------------------------
--
-- A turn is stopped from outside it (Esc on the chat's screen, Ctrl-C on the streams): its thread is thrown
-- 'StopTurn', wherever it is -- waiting for the model, for a tool, for a verdict -- and the exchanges with the
-- model under way are given up, so no reply goes on being written for nobody. What the turn did so far is in
-- the history, as it is for a turn that ends; the conversation it carried is dropped, and the next turn starts
-- from the view. (What swallows the exception -- a tool's call that turns every failure into an answer -- only
-- delays it: the turn looks at 'cStopped' at each step.)

data StopTurn = StopTurn deriving Show
instance Exception StopTurn

-- | Ask the turn under way to stop. False when there is none.
stopTurn :: Chat -> IO Bool
stopTurn ch = do
  t <- readIORef (cTurn ch)
  case t of
    -- (no turn of the agent's own: the subagents at work, if any, are what is stopped)
    Nothing -> do
      n <- stopAgents ch
      when (n > 0) (cancelRequests >> interruptEval ch >> uiNote (cUi ch) (printf "[%d subagent(s) stopped]" n))
      pure (n > 0)
    Just tid -> writeIORef (cStopped ch) True >> stopAgents ch >> cancelRequests >> throwTo tid StopTurn >> interruptEval ch >> pure True

-- | The evaluation this chat has running in the session, if it has one, is interrupted: it would run on to its
-- end or its time, holding the session, for a turn that is no longer there. (Asked apart, and not waited for.)
interruptEval :: Chat -> IO ()
interruptEval ch = do
  pid <- getProcessID
  void (forkIO (void (try (timeout 5000000 (ask ch "interrupt" [("from", JStr ("chat-" ++ show pid))])) :: IO (Either SomeException (Maybe Json)))))

-- | Has the turn been asked to stop? Then it stops here.
checkStop :: Chat -> IO ()
checkStop ch = readIORef (cStopped ch) >>= \s -> when s (throwIO StopTurn)

-- | Where the turn under way began: the id of its first message, kept in the session's state while the turn
-- runs (and marked when it ends) -- what a turn gone on with from its log ('continueTurn') starts from. A line
-- typed in the middle of a turn is the user's too, and is not where the turn began.
turnFile :: Chat -> FilePath
turnFile ch = cStateDir (cConf ch) </> cName ch </> "turn.json"

turnBegan :: Chat -> Int -> IO ()
turnBegan ch i = void (try (B.writeFile (turnFile ch) (encodeBS (JObj [("start", JNum (fromIntegral i))]))) :: IO (Either IOException ()))

turnEnded :: Chat -> IO ()
turnEnded ch = turnNote ch (set "stopped" (JBool False) . set "done" (JBool True)) >> turnHook ch

{-# NOINLINE turnLock #-}
turnLock :: MVar ()
turnLock = unsafePerformIO (newMVar ())

-- | Change what turn.json says of the turn under way -- a waiter reads it ("GhciSession.Inbox"): written whole
-- under another name and moved, so that a reader never sees half of it. A subagent's turn is not the chat's.
turnNote :: Chat -> (Json -> Json) -> IO ()
turnNote ch f = when (isNothing (cSub ch)) $ withMVar turnLock $ \_ -> do
  r <- try (B.readFile (turnFile ch)) :: IO (Either IOException B.ByteString)
  forM_ (either (const Nothing) (either (const Nothing) Just . parseJsonBS) r) $ \j ->
    void (try (B.writeFile (turnFile ch ++ ".tmp") (encodeBS (f j)) >> renameFile (turnFile ch ++ ".tmp") (turnFile ch)) :: IO (Either IOException ()))

-- | Lines of the user's given to the turn under way between two of its calls: counted in turn.json, so that a
-- waiter whose line was taken while that turn ran can tell the turn took it (and did not leave it for the next).
fedNote :: Chat -> [a] -> IO ()
fedNote ch xs = unless (null xs) (turnNote ch (\j -> set "fed" (JNum (fromIntegral (length xs) + fromMaybe 0 (lookupNum "fed" j))) j))

-- | What the chat logs that a waiter wants: a talk message is the last words so far, a tool call is one more.
noteLogged :: Chat -> String -> T.Text -> IO ()
noteLogged ch kind text
  | kind == "talk" = turnNote ch (set "last" (JText text))
  | kind == "tool" = turnNote ch (\j -> set "tools" (JNum (1 + fromMaybe 0 (lookupNum "tools" j))) j)
  | otherwise = pure ()

-- | A turn's end, as turn.json has it: its summary line and the tool calls and seconds in it.
noteSpent :: Chat -> Spent -> Double -> IO ()
noteSpent ch s secs = turnNote ch (set "tools" (JNum (fromIntegral (sTools s))) . set "secs" (JNum secs) . set "summary" (JStr (spentLine s secs)))

-- | What the command of "on_turn_end" is told of a turn: the session, its seconds and tool calls (the last words go
-- on its standard input).
hookEnv :: String -> Json -> [(String, String)]
hookEnv name j = [ ("GHS_SESSION", name), ("GHS_TURN_SECONDS", printf "%.0f" (fromMaybe 0 (lookupNum "secs" j)))
                 , ("GHS_TURN_TOOL_CALLS", show (round (fromMaybe 0 (lookupNum "tools" j)) :: Int)) ]

-- | Run the project's "on_turn_end" command, apart from the turn: its failure is a note, never the turn's.
turnHook :: Chat -> IO ()
turnHook ch = when (isNothing (cSub ch)) $ forM_ (onTurnEndOf (cConf ch) (cName ch)) $ \cmd -> void $ forkIO $ do
  r <- try (B.readFile (turnFile ch)) :: IO (Either IOException B.ByteString)
  let j = either (const (JObj [])) (either (const (JObj [])) id . parseJsonBS) r
  base <- getEnvironment
  let p = (shell cmd) { env = Just (base ++ hookEnv (cName ch) j), cwd = Just (cRoot (cConf ch)) }
  res <- try (timeout 60000000 (readCreateProcessWithExitCode p (maybe "" T.unpack (lookupText "last" j)))) :: IO (Either SomeException (Maybe (ExitCode, String, String)))
  let note why = uiNote (cUi ch) ("[on_turn_end: " ++ why ++ "]")
  case res of
    Right (Just (ExitSuccess, _, _)) -> pure ()
    Right (Just (ExitFailure n, _, err)) -> note ("the command ended with " ++ show n ++ (if null (words err) then "" else ": " ++ unwords (take 30 (words err))))
    Right Nothing -> note "the command took over 60s and was stopped"
    Left x -> note ("the command could not be run: " ++ show x)

-- | The system prompt of the chat's own turns, for a turn that is begun again from inside one (a rollover).
{-# NOINLINE mainSystem #-}
mainSystem :: IORef String
mainSystem = unsafePerformIO (newIORef "")

-- | A turn, as something that can be stopped: said and logged when it was.
stoppable :: Chat -> IO () -> IO ()
stoppable ch act = do
  tid <- myThreadId
  writeIORef (cStopped ch) False
  writeIORef (cTurn ch) (Just tid)
  r <- try (act `finally` writeIORef (cTurn ch) Nothing)
  case r of
    Right () -> when (isNothing (cSub ch)) (turnEnded ch)
    Left StopTurn -> do
      writeIORef (cInTurn ch) False
      turnNote ch (set "stopped" (JBool True))
      turnHook ch
      writeIORef (cBatch ch) Nothing
      logH ch "echo" (T.pack "harness: the turn was stopped by the user before it ended")
      uiNote (cUi ch) "[the turn was stopped; what it did so far is in the history]"

-- subagents ------------------------------------------------------------------------------------
--
-- The agent can give work away: @spawn@ starts a subagent a task, each a turn of its own on a thread of its
-- own, with the agent's tools (less these two) on the same session and files, and answers at once with their
-- names. A subagent's chat is its own -- a file, @agents\/NAME.jsonl@ in the session's state: what it is, its
-- task, every call it makes and what it is told -- and is what its turn reads after the view, so one that has
-- finished and is told more ('tellTool') goes on from there. What reaches the agent is its last reply: a line
-- on the chat's queue, @[NAME] report@ and the text, which is a turn's message if the agent is waiting and
-- reaches it between two tool calls if it is at work -- logged as @work@, the kind the view has for it.
-- A turn that is stopped takes its subagents with it.

-- | A subagent: its name, where its chat is kept, the lines told to it and not yet read, the thread of its
-- turn while it is at work, how hard it thinks, and whether it was asked to stop.
data Sub = Sub { suName :: String, suFile :: FilePath, suQueue :: TQueue (Maybe T.Text), suThread :: Maybe ThreadId, suEffort :: Maybe String, suStop :: IORef Bool }

-- | The tools of whoever asks: a subagent has all but the two that make and tell subagents.
toolsFor :: Chat -> [Tool]
toolsFor ch = [ t | t <- chatTools, isNothing (cSub ch) || tName t `notElem` ["spawn", "tell"] ]

agentsDir :: Chat -> FilePath
agentsDir ch = cStateDir (cConf ch) </> cName ch </> "agents"

-- | A line of a subagent's chat.
agentAppend :: FilePath -> String -> T.Text -> IO ()
agentAppend file kind text = do
  t <- now
  createDirectoryIfMissing True (takeDirectory file)
  B.appendFile file (encodeBS (JObj [("date", JNum t), ("kind", JStr kind), ("text", JText text)]) <> B8.pack "\n")

-- | A subagent's chat as its turn reads it: a line a message, @i|kind: text@, a long one cut (it was the
-- subagent's own doing, and it can do it again), and of a long chat its first lines -- what it is, its task --
-- and its last.
agentShow :: [(String, T.Text)] -> T.Text
agentShow ls = T.unlines (if length shown <= 60 then shown else take 4 shown ++ [T.pack (printf "(%d lines left out)" (length shown - 44))] ++ drop (length shown - 40) shown)
  where shown = [ T.pack (show i ++ "|" ++ kind ++ ": ") <> flat (if T.length t > 3000 then T.take 3000 t <> T.pack " [...]" else t) | (i, (kind, t)) <- zip [0 :: Int ..] ls ]
        flat = T.replace (T.pack "\n") (T.pack "\n  ")

agentLines :: FilePath -> IO [(String, T.Text)]
agentLines file = do
  r <- try (B.readFile file) :: IO (Either IOException B.ByteString)
  pure [ (fromMaybe "?" (lookupStr "kind" j), fromMaybe T.empty (lookupText "text" j)) | Right b <- [r], l <- B8.lines b, Right j <- [parseJsonBS l] ]

-- | Is this line a subagent's word to the agent (@[NAME] report@, @[NAME] reply@, and its text)? It is logged
-- as @work@, not as the user's.
isWork :: T.Text -> Bool
isWork t = case T.words (T.takeWhile (/= '\n') t) of
  [n, w] -> T.pack "[Sub-" `T.isPrefixOf` n && T.pack "]" `T.isSuffixOf` n && w `elem` map T.pack ["report", "reply"]
  _ -> False

-- | A line that came on the queue, logged as what it is: the user's, or a subagent's report.
logTyped :: Chat -> T.Text -> IO (Maybe Int)
logTyped ch t = logId ch (if isWork t then "work" else "user") t

-- | What a subagent is told it is, before the view and the tools are described to it.
subHead :: String -> String
subHead who = unlines
  [ "You are a subagent of " ++ who ++ ", an AI agent that works for one user in a single chat"
  , "that never ends. " ++ who ++ " gave you a task. Do it yourself, with your tools, following"
  , "the user's instructions at the end of this prompt."
  , ""
  , "Your first message holds the view below, then your chat with " ++ who ++ " so far"
  , "inside <agent> tags, its lines as \"i|kind: text\": what you are (note), your task"
  , "(user), and later all you did and were told. The view shows you what " ++ who
  , "knows: what the user wants, decided and taught. Use it as context only, and do"
  , "what your task says, not what the user's last message says: " ++ who ++ " may have"
  , "given you just part of the work. Other subagents may be working on the same"
  , "files and the same session at the same time: keep to what your task names."
  , ""
  , "Your final reply is your report to " ++ who ++ ": what you did, what you found that"
  , "the task asks for, what failed. No one else sees your work. " ++ who ++ " may send"
  , "you more messages, even while you work: answer the last." ]

-- | Start a subagent a task.
spawnTool :: Chat -> Json -> IO (Bool, T.Text)
spawnTool ch a = do
  start <- readIORef (cStart ch)
  agents <- readIORef (cAgents ch)
  let tasks = [ T.strip t | j <- lookupArr "tasks" a, Just t <- [lookupText "t" (JObj [("t", j)])], not (T.null (T.strip t)) ]
      atWork = length [ () | s <- M.elems agents, isJust (suThread s) ]
      effort = case lookupStr "effort" a of { Just ef | ef `elem` ["low", "high", "max"] -> Just ef; _ -> Nothing }
  case (cSub ch, start) of
    (Just _, _) -> pure (False, T.pack "spawn: a subagent starts none of its own")
    (_, Nothing) -> pure (False, T.pack "spawn: not in this chat")
    _ | null tasks -> pure (False, T.pack "spawn: no tasks")
      | length tasks + atWork > maxAgents -> pure (False, T.pack (printf "spawn: %d task(s), and %d subagent(s) at work already: %d at a time at most. Give fewer, or wait for a report." (length tasks) atWork maxAgents))
    (_, Just run) -> do
      createDirectoryIfMissing True (agentsDir ch)
      names <- forM tasks $ \task -> do
        had <- length . filter (".jsonl" `isSuffixOf`) <$> listDirectory (agentsDir ch)
        let name = "Sub-" ++ show (had + 1)
            file = agentsDir ch </> (name ++ ".jsonl")
        agentAppend file "note" (T.pack ("subagent" ++ maybe "" (" at effort " ++) effort))
        agentAppend file "user" task
        launch ch run name file effort "report"
        pure name
      pure (True, T.pack ("Started " ++ intercalate ", " names ++ ". Each one's report will reach you as a message when it finishes."))

maxAgents :: Int
maxAgents = 8

-- | A subagent's turn, on a thread of its own, known to the chat while it runs.
launch :: Chat -> (Sub -> String -> IO ()) -> String -> FilePath -> Maybe String -> String -> IO ()
launch ch run name file effort label = do
  q <- newTQueueIO
  stop <- newIORef False
  let s0 = Sub name file q Nothing effort stop
  gate <- newEmptyMVar
  tid <- forkIO $ do
    takeMVar gate
    void (try (run s0 label) :: IO (Either SomeException ()))
    atomicModifyIORef' (cAgents ch) (\m -> (M.adjust (\s -> s { suThread = Nothing }) name m, ()))
  atomicModifyIORef' (cAgents ch) (\m -> (M.insert name s0 { suThread = Just tid } m, ()))
  putMVar gate ()

-- | Tell a subagent something: one at work reads it between two tool calls; one that has finished goes on.
tellTool :: Chat -> Json -> IO (Bool, T.Text)
tellTool ch a = do
  start <- readIORef (cStart ch)
  agents <- readIORef (cAgents ch)
  let name = fromMaybe "" (lookupStr "name" a)
      message = fromMaybe T.empty (lookupText "message" a)
      file = agentsDir ch </> (name ++ ".jsonl")
      known = not (null name) && all (\c -> isAlphaNum c || c == '-') name
  there <- if known then doesFileExist file else pure False
  case (M.lookup name agents, start) of
    _ | isJust (cSub ch) -> pure (False, T.pack "tell: a subagent tells none")
      | T.null (T.strip message) -> pure (False, T.pack "tell: no message")
    (Just s, _) | isJust (suThread s) -> do
      atomically (writeTQueue (suQueue s) (Just message))
      pure (True, T.pack ("Sent. " ++ name ++ " reads it between its tool calls, and its report answers it."))
    (_, Just run) | there -> do
      agentAppend file "user" message
      launch ch run name file (M.lookup name agents >>= suEffort) "reply"
      pure (True, T.pack ("Sent. " ++ name ++ "'s reply will reach you as a message."))
    _ -> pure (False, T.pack ("No agent " ++ name ++ " to tell." ++ (if M.null agents then "" else " There are: " ++ intercalate ", " (M.keys agents) ++ ".")))

-- | The subagents at work, stopped: how many there were.
stopAgents :: Chat -> IO Int
stopAgents ch = do
  agents <- readIORef (cAgents ch)
  let working = [ (s, tid) | s <- M.elems agents, Just tid <- [suThread s] ]
  forM_ working $ \(s, tid) -> writeIORef (suStop s) True >> throwTo tid StopTurn
  pure (length working)

-- | How a subagent is run, for a chat that knows its endpoint: its turn -- the view, then its chat -- to its
-- end, again while it was told more meanwhile, and its last reply put on the agent's queue as its report.
subRunner :: Chat -> Endpoint -> Opts -> String -> TQueue (Maybe T.Text) -> Sub -> String -> IO ()
subRunner ch e o instr toAgent s label = go
  where
    name = suName s
    go = do
      (v, _, _, _) <- view ch 0
      mine <- agentLines (suFile s)
      replies <- newIORef []
      failure <- newIORef ""
      batch <- newIORef Nothing
      pend <- newIORef Nothing
      down <- newIORef False
      never <- newIORef False
      inTurn <- newIORef False
      turnR <- newIORef Nothing
      tStart <- now
      let said x = uiNote (cUi ch) ("[" ++ name ++ ": " ++ x ++ "]")
          ui = Ui { uiView = \_ -> pure (), uiTalk = \t -> modifyIORef' replies (t :), uiThought = \_ -> pure ()
                  , uiCall = \n args -> said (n ++ " " ++ take 100 args), uiAnswer = \_ -> pure ()
                  , uiNote = \x -> when ("chat: " `isPrefixOf` x) (writeIORef failure (drop 6 x) >> said (drop 6 x))
                  , uiBusy = \_ -> pure (), uiSpent = \sp secs -> said (drop 1 (init (spentLine sp secs))), uiDone = pure (), uiLeave = pure (), uiOnStop = \_ -> pure () }
          ch' = ch { cUi = ui, cSub = Just (name, suFile s), cBatch = batch, cPending = pend, cDown = down, cRestart = never, cInTurn = inTurn, cTurn = turnR, cStopped = suStop s }
          o' = o { oView = False, oEffort = case suEffort s of { Just ef -> Just ef; Nothing -> oEffort o } }
          system = subHead (cAgent ch) ++ "\n" ++ T.unpack (H.viewPrompt (cAgent ch)) ++ "\n" ++ master ++ (if null instr then "" else "\n" ++ instr)
          first = v <> T.pack ("\n\n<agent name=\"" ++ name ++ "\">\n") <> agentShow mine <> T.pack "</agent>"
      said (if label == "report" then "started" else "goes on")
      r <- try (if eProvider e == ClaudeCli then turnCli ch' e o' system first (suQueue s)
                else goOn ch' e o' (suQueue s) (TurnState [ msg "system" (T.pack system), msg "user" first ] 0 0 [] (Spent 0 0 0 0 0 False False) tStart Nothing))
      rs <- readIORef replies
      why <- readIORef failure
      case r of
        Left x | Just StopTurn <- fromException x -> agentAppend (suFile s) "note" (T.pack "stopped by the user before the end")
        _ -> do
          -- (told more as it ended: it goes on, and what it then says is the report)
          left <- drain (suQueue s)
          if not (null left) then mapM_ (agentAppend (suFile s) "user") left >> go else do
            let text = case (r, rs) of
                  (Left x, _) -> T.pack ("failed: " ++ show (x :: SomeException))
                  (_, t : _) -> t
                  _ -> T.pack ("failed: it ended without a reply" ++ (if null why then "" else " (" ++ why ++ ")"))
                line = T.pack ("[" ++ name ++ "] " ++ label ++ "\n") <> text
            uiNote (cUi ch) (T.unpack line)
            atomically (writeTQueue toAgent (Just line))

-- | One change to a text, in one of four forms -- the text with the change, and how it was made (for the
-- answer), or why it cannot be:
--
-- * @old@, @new@: one exact, unique occurrence replaced ('replaceOnce');
-- * @append: true@, @new@: put at the end;
-- * @first@, @until@, @new@: the lines from the one that holds @first@ (the only one) up to, not including, the
--   next that holds @until@ -- a whole definition replaced without writing the old one out;
-- * @start@, @end@, @new@: those lines, by their numbers as @read@ shows them.
--
-- The last three are what a script run through the shell was written for, twenty times in a first long turn.
applyEdit :: T.Text -> Json -> Either T.Text (T.Text, String)
applyEdit t e
  | lookupBool "append" e == Just True =
      Right (t <> (if T.null t || T.pack "\n" `T.isSuffixOf` t then T.empty else T.pack "\n") <> new <> (if T.pack "\n" `T.isSuffixOf` new then T.empty else T.pack "\n"), " (appended)")
  | Just a <- num' "start" = let b = fromMaybe a (num' "end") in
      if a < 1 || b < a || b > n then Left (T.pack (printf "lines %d-%d: the file has %d lines" a b n)) else Right (between a (b + 1), printf " (lines %d-%d replaced)" a b)
  | Just f <- text' "first" = case [ i | (i, l) <- zip [1 :: Int ..] ls, f `T.isInfixOf` l ] of
      [a] -> case text' "until" of
        Nothing -> Left (T.pack "first, and no until: give until -- a text of the first line that is NOT to be replaced (the next definition's first line)")
        Just w -> case [ i | (i, l) <- zip [1 :: Int ..] ls, i > a, w `T.isInfixOf` l ] of
          (b : _) -> Right (between a b, printf " (lines %d-%d replaced)" a (b - 1))
          [] -> Left (T.pack (printf "until: no line after line %d holds %s" a (show (T.unpack w))))
      [] -> Left (T.pack ("first: no line holds " ++ show (T.unpack f)))
      is -> Left (T.pack (printf "first: %d lines hold %s (lines %s): give a text only the first line to replace holds" (length is) (show (T.unpack f)) (intercalate ", " (map show (take 8 is)))))
  | Just old <- text' "old" = replaceOnce t old new
  | otherwise = Left (T.pack "what to change is not said: give old (the text to replace), or append: true, or first and until (texts of the first line to replace and of the first line after it), or start and end (line numbers)")
  where
    new = fromMaybe T.empty (lookupText "new" e)
    ls = T.splitOn (T.pack "\n") t
    n = length ls
    num' k = round <$> lookupNum k e :: Maybe Int
    text' k = case lookupText k e of { Just w | not (T.null w) -> Just w; _ -> Nothing }
    -- (lines a .. b-1 are the new text's)
    between a b = T.intercalate (T.pack "\n") (take (a - 1) ls ++ [ fromMaybe new (T.stripSuffix (T.pack "\n") new) | not (T.null new) ] ++ drop (b - 1) ls)

-- | A line of a file as @read@ and @grep@ show it: its number, the bar 'numberBar', then the line EXACTLY as the
-- file has it -- its indentation is every space after the bar, and none before it. A line the search matched
-- has @>@ before its number, a line shown for context a space. (Two spaces after the number, as before, read
-- as part of the line's indentation: in 15 of 60 edits of an agent's rounds the new text was indented one or
-- two spaces too far. The bar is no whitespace, so it cannot be counted among the spaces; it is not an ASCII
-- @|@, which a guard or a table cell starts a line with, and a tab would be one more run of whitespace.)
numbered :: Char -> Int -> T.Text -> T.Text
numbered mark i l = T.pack (printf "%c%5d%c" mark i numberBar) <> l

-- | The line budget of the project's session ("line_budget": N in its target), if it has one.
lineBudget :: Chat -> Maybe Int
lineBudget ch = lineBudgetOf (cConf ch) (cName ch)

-- | A file's line count said against the budget, when there is one (a rule of the project, not of the tool:
-- with none, nothing is said -- it was said on every read and edit, 'OVER BUDGET' on some 120 calls of a
-- project that has no such rule).
budgetSaid :: Maybe Int -> Int -> Maybe String
budgetSaid mb n = Vfs.formatLineBudget n <$> mb

-- | The head of a read's answer: the file, the lines shown and how many it has (and the budget, if there is one).
readHeader :: Maybe Int -> FilePath -> Int -> Int -> Int -> String
readHeader mb rel from to total = printf "[%s: lines %d-%d of %d%s]\n" rel from to total (maybe "" said mb)
  where said b = " | " ++ Vfs.formatLineBudget total b ++ " " ++ (if b >= total then printf "(%d lines remaining)" (b - total) else printf "(%d lines OVER BUDGET!)" (total - b) :: String)

-- | What stands between a line's number and its text in 'numbered'.
numberBar :: Char
numberBar = '\x2502'

-- | The agent's hands on the files, inside the project only.
fileTool :: Chat -> String -> Json -> IO (Bool, T.Text)
fileTool ch name a = case name of
  "read" -> withPath $ \p -> do
    bytes <- B.readFile p
    case Img.kindOf bytes of
      -- (an image: kept, and named in the answer by the line that stands for it)
      Just _ -> either (\why -> (False, T.pack (rel ++ ": " ++ why))) (\(n, said) -> (True, T.pack (printf "[%s: %s]\n" rel said) <> Img.marker n)) <$> Img.keep (imagesDir ch) bytes
      Nothing -> do
        let t = decode bytes
        let allLines = if T.null t then [] else T.splitOn (T.pack "\n") t
            -- (`around`: from a few lines before the first line that holds the text -- what `grep -n -A40 TEXT
            -- file` was run through the shell for; where else it is, is said)
            around = lookupText "around" a
            hits = [ i | Just w <- [around], not (T.null w), (i, l) <- zip [1 :: Int ..] allLines, w `T.isInfixOf` l ]
            start = case hits of
              (i : _) -> max 1 (i - maybe 3 round (lookupNum "before" a))
              [] -> max 1 (maybe 1 round (lookupNum "start" a))
            n = maybe (if null hits then 200 else 60) round (lookupNum "lines" a) :: Int
            more = case hits of
              (_ : rest@(_ : _)) -> T.pack (printf "\n[%d more line(s) hold it: %s%s -- start: N reads from one]" (length rest) (intercalate ", " (map show (take 12 rest))) (if length rest > 12 then ", ..." else "" :: String))
              _ -> T.empty
            totalLines = length allLines
            ls = zip [1 :: Int ..] allLines
            shown = [ numbered ' ' i l | (i, l) <- ls, i >= start, i < start + n ]
            endLine = if null shown then 0 else start + length shown - 1
            header = T.pack (readHeader (lineBudget ch) rel (if null shown then 0 else start) endLine totalLines)
        if isJust around && null hits then pure (False, T.pack (printf "%s: no line holds %s (%d lines; the text is looked for as it is, in one line)" rel (show (maybe "" T.unpack around)) totalLines))
          else pure (True, header <> (if null shown then T.pack "(empty)" else T.intercalate (T.pack "\n") shown) <> more)
  "write" -> withPath $ \p -> do
    let content = fromMaybe T.empty (lookupText "content" a)
    written <- writtenAt ch rel
    was <- either (\(_ :: IOException) -> Nothing) (Just . decode) <$> try (B.readFile p)
    createDirectoryIfMissing True (takeDirectory p)
    B.writeFile p (TE.encodeUtf8 content)
    let nLines = if T.null content then 0 else T.count (T.pack "\n") content + (if T.last content == '\n' then 0 else 1)
        budget = maybe "" (", " ++) (budgetSaid (lineBudget ch) nLines)
    -- (a file written over is answered with what changed in it: a whole file sent again loses lines unseen)
    d <- maybe (pure (T.pack "\n[a new file]")) (\old -> changeOf rel old content) was
    withDiff d <$> saved ch (printf "wrote %s (%d characters%s)" rel (T.length content) budget) written
  "edit" -> withPath $ \p -> do
    -- (appending makes the file if it is not there)
    t <- if lookupBool "append" a == Just True then either (\(_ :: IOException) -> T.empty) decode <$> try (B.readFile p) else decode <$> B.readFile p
    case applyEdit t a of
      Right (t', how) -> do
        written <- writtenAt ch rel
        B.writeFile p (TE.encodeUtf8 t')
        let nLines = if T.null t' then 0 else T.count (T.pack "\n") t' + (if T.last t' == '\n' then 0 else 1)
            budget = maybe "" (\b -> " (" ++ b ++ ")") (budgetSaid (lineBudget ch) nLines)
        d <- changeOf rel t t'
        withDiff d <$> saved ch (printf "edited %s%s%s" rel how budget) written
      Left why -> pure (False, T.pack (rel ++ ": ") <> why)
  -- several replacements, in one or more files: all checked against the files (as the ones before them
  -- leave them) before any is written, then written together -- one reload, one verdict
  "edits" -> do
    -- (the list, or the list written out as a text: a model sends that now and then, and was refused)
    let given = case lookupArr "edits" a of
          [] | Just w <- lookupText "edits" a, Right (JArr es) <- parseJsonBS (TE.encodeUtf8 w) -> es
          es -> es
        items = editPaths (lookupStr "path" a) [ fst (arguments editTool e) | e <- given ]
        apply files [] = pure (Right files)
        apply _ ((i, (Nothing, _)) : _) =
          pure (Left (T.pack (printf "replacement %d: no path (each replacement is {path, old, new}; one without a path is in the file of the one before it)" (i :: Int))))
        apply files ((i, (Just rp, e)) : rest) = case inside ch rp of
          Left why -> pure (Left (T.pack (printf "replacement %d: %s" i why)))
          Right p -> do
            t <- maybe (decode <$> B.readFile p) pure (lookup p files)
            case applyEdit t e of
              Left why -> pure (Left (T.pack (printf "replacement %d, %s: " i rp) <> why))
              Right (t', _) -> apply ((p, t') : filter ((/= p) . fst) files) rest
    if null items then pure (False, T.pack "edits: no replacements given (edits: [{path, old, new}, ...])") else do
      r <- try (apply [] (zip [1 ..] items)) :: IO (Either IOException (Either T.Text [(FilePath, T.Text)]))
      case r of
        Left e -> pure (False, T.pack ("edits: " ++ show e ++ " -- nothing was written"))
        Right (Left why) -> pure (False, why <> T.pack "\n[nothing was written: fix that replacement and send them all again]")
        Right (Right files) -> do
          let rels = [ makeRelative (cDir ch) f | (f, _) <- files ]
          written <- fmap (listToMaybe . catMaybes) (mapM (writtenAt ch) rels)
          olds <- forM files (\(f, _) -> decode <$> B.readFile f)
          forM_ files (\(f, t) -> B.writeFile f (TE.encodeUtf8 t))
          ds <- forM (reverse (zip files olds)) (\((f, t), old) -> changeOf (makeRelative (cDir ch) f) old t)
          let summaries = [ printf "%s (%s)" (makeRelative (cDir ch) f) s
                            | (f, t) <- files, Just s <- [budgetSaid (lineBudget ch) (if T.null t then 0 else T.count (T.pack "\n") t + (if T.last t == '\n' then 0 else 1))] ] :: [String]
          withDiff (T.concat ds) <$> saved ch (printf "edited %s (%d replacement(s))%s" (intercalate ", " (reverse rels)) (length items) (if null summaries then "" else "\n[line budgets: " ++ intercalate "; " summaries ++ "]")) written
  "vfs" -> do
    let mPath = lookupStr "path" a
        budget = maybe 250 round (lookupNum "budget" a) :: Int
    files <- Vfs.inspectLoaded (cConf ch) (cName ch) budget mPath
    pure (True, Vfs.formatVfsTable files budget)
  "wc" -> do
    let mPath = lookupStr "path" a
        budget = maybe 250 round (lookupNum "budget" a) :: Int
    files <- Vfs.inspectLoaded (cConf ch) (cName ch) budget mPath
    pure (True, Vfs.formatVfsTable files budget)
  "files" -> do
    let mPath = lookupStr "path" a
        budget = maybe 250 round (lookupNum "budget" a) :: Int
    files <- Vfs.inspectLoaded (cConf ch) (cName ch) budget mPath
    pure (True, Vfs.formatVfsTable files budget)
  -- (in one file, with lines after each match: read here, a file of any size or kind -- a sample, a reference's source)
  "grep" | Just rp <- lookupStr "path" a -> case inside ch rp of
    Left why -> pure (False, T.pack why)
    Right p -> do
      r <- try (B.readFile p) :: IO (Either IOException B.ByteString)
      case r of
        Left err -> pure (False, T.pack ("grep: " ++ show err))
        Right bytes -> do
          let q = fromMaybe T.empty (lookupText "query" a)
              maxN = maybe 30 round (lookupNum "lines" a) :: Int
              ctx = max 0 (maybe 0 round (lookupNum "context" a)) :: Int
              ls = zip [1 :: Int ..] (T.splitOn (T.pack "\n") (decode bytes))
              hits = [ i | (i, l) <- ls, q `T.isInfixOf` l ]
              shownHits = take maxN hits
              wanted = concat [ [i .. i + ctx] | i <- shownHits ]
              out = [ numbered (if i `elem` shownHits then '>' else ' ') i (T.take 400 (T.filter (/= '\r') l)) | (i, l) <- ls, i `elem` wanted ]
          pure (if T.null q then (False, T.pack "grep: no query") else
                (True, T.pack (printf "[%s: %d line(s) hold %s%s]\n" rp (length hits) (show (T.unpack q)) (if length hits > maxN then printf "; the first %d shown" maxN else "" :: String)) <> T.intercalate (T.pack "\n") out))
  "grep" -> do
    let q = fromMaybe "" (lookupStr "query" a)
        maxN = maybe 30 round (lookupNum "lines" a) :: Int
    res <- Search.grep (cDir ch) q maxN
    case res of
      Left err -> pure (False, T.pack ("grep error: " ++ err))
      Right j -> do
        let hits = fromMaybe 0 (lookupNum "count" j >>= Just . round)
        Search.recordSearchMetadata (cConf ch) (cName ch) "grep" q hits
        pure (True, Search.formatGrep j)
  "find" -> do
    let q = fromMaybe "" (lookupStr "query" a)
        maxN = maybe 20 round (lookupNum "n" a) :: Int
    res <- Search.searchFiles (cDir ch) q maxN
    case res of
      Left err -> pure (False, T.pack ("find error: " ++ err))
      Right j -> do
        let hits = fromMaybe 0 (lookupNum "count" j >>= Just . round)
        Search.recordSearchMetadata (cConf ch) (cName ch) "files" q hits
        pure (True, Search.formatFiles j)
  "ls" -> withPath $ \p -> do
    es <- filter (not . ("." `isPrefixOf`)) <$> listDirectory p
    tagged <- forM (sort es) $ \e -> (\d -> e ++ (if d then "/" else "")) <$> doesDirectoryExist (p </> e)
    pure (True, T.pack (intercalate "\n" tagged))
  "sh" -> do
    up <- isJust <$> verdictAt ch
    written <- if up then Just <$> now else pure Nothing
    staleBefore <- staleNow ch
    before <- if up then sourceStamp ch else pure []
    let cmd = fromMaybe "" (lookupStr "cmd" a)
    t0 <- now
    (ok, out) <- shTool (cDir ch) cmd (fromMaybe 120 (lookupNum "timeout" a))
    secs <- subtract t0 <$> now
    after <- if up then sourceStamp ch else pure []
    -- a command that edited a watched source (sed -i, a generator, git) is a save too: the session reloads
    -- it, and the answer waits for that verdict as write and edit do, else the agent reloads by hand
    -- (only when it did: every command waited two seconds for a reload that most never cause)
    pending <- if after == before then pure Nothing else shSaved ch written staleBefore
    -- a GHCi of the agent's own loads the project cold, every time, what the session has loaded warm
    let own | up && ownGhci cmd = T.pack (printf "\n[note: this started a GHCi of its own, loading the project cold (%.1fs); the session has it loaded -- eval, test with expr (one group of tests alone), typecheck answer from it in about a second]" secs)
            | otherwise = T.empty
    -- (how it ended and how long it took, always: a command that printed nothing and one that hung for a minute
    -- answered alike)
    pure (ok && maybe True fst pending, capWith shCap shHint out <> T.pack (printf "\n[%s in %.1f s]" (if ok then "exit 0" else "failed") secs) <> own <> maybe T.empty snd pending)
  _ -> pure (False, T.pack ("unknown tool " ++ name))

  where
    rel = fromMaybe "." (lookupStr "path" a)
    withPath k = case inside ch rel of
      Left why -> pure (False, T.pack why)
      Right p -> k p

-- | One replacement in a text: exactly once as written, or (nowhere as written) once with its spacing
-- squeezed; the new text and what was done, or why not.
replaceOnce :: T.Text -> T.Text -> T.Text -> Either T.Text (T.Text, String)
replaceOnce t old new
  | k == 1 = let (pre, post) = T.breakOn old t in Right (pre <> new <> T.drop (T.length old) post, "")
  | k == 0, Just (t', l0, l1) <- lineTrimReplace t old new = Right (t', printf " (the text matched lines %d-%d with trailing whitespace trimmed: applied there)" l0 l1)
  | k == 0, Just r <- fuzzyReplace t old new = case r of
      Left why -> Left why
      Right (t', l0, l1, d) -> Right (t', printf " (the text matched lines %d-%d only with its spacing squeezed: applied there%s)" l0 l1
                                                 (if d == 0 then "" else printf "; its indentation differed from the file's by %d, so the new text's lines after the first are shifted by that" d :: String))
  | otherwise = Left (T.pack (printf "the text occurs %d times; it must occur exactly once" k) <> (if k == 0 then nearest t old else T.empty))
  where k = if T.null old then 0 else T.count old t

-- | The words of a text, each with the character offsets where it starts and ends.
wordSpans :: T.Text -> [(T.Text, Int, Int)]
wordSpans = go 0
  where
    go i s = let (sp, r) = T.span isSpace s
                 i1 = i + T.length sp
             in if T.null r then [] else
                  let (w, r') = T.break isSpace r
                      i2 = i1 + T.length w
                  in (w, i1, i2) : go i2 r'

-- | An edit whose text occurs nowhere as written but EXACTLY ONCE as the same words with any spacing
-- between them: the file with that run replaced, its first and last line, and by how many spaces the new text
-- was shifted. The run starts at its first word and ends at its last, so the whitespace the old text had
-- around its words (an indent, a newline) is taken off the new text too when the new text has the same.
-- Eight of 94 edits in one round of an agent's work failed on spacing alone, and all six of the next round
-- that were applied so were compile errors: the old text's indentation was off from the file's by one or two
-- spaces, and the new text, written the same way, put that into the file. So when the lines of the old text
-- and of the matched block differ in indentation by one constant, the new text's lines (after the first,
-- which stands where the match starts) are shifted by it; when they differ by more than that -- or in the
-- number of lines, or a new line has no room to be shifted left -- it is NOT applied: 'Left' says so, with the
-- lines as they are. 'Nothing': no single match.
fuzzyReplace :: T.Text -> T.Text -> T.Text -> Maybe (Either T.Text (T.Text, Int, Int, Int))
fuzzyReplace file old new
  | null ows = Nothing
  | otherwise = case matches of
      [(s, e)] -> Just (result s e)
      _ -> Nothing
  where
    ows = T.words old
    n = length ows
    matches = [ (s, e) | run@((w, s, _) : _) <- tails (wordSpans file), w == head ows
                       , let ws = take n run, length ws == n, [ x | (x, _, _) <- ws ] == ows, let (_, _, e) = last ws ]
    lead = T.takeWhile isSpace old
    trail = T.takeWhileEnd isSpace old
    new' = let a = fromMaybe new (T.stripPrefix lead new) in fromMaybe a (T.stripSuffix trail a)
    lineAt i = 1 + T.count (T.pack "\n") (T.take i file)
    nl = T.pack "\n"
    blank = T.null . T.strip
    ind = T.length . T.takeWhile isSpace
    result s e =
      let (l0, l1) = (lineAt s, lineAt e)
          block = take (l1 - l0 + 1) (drop (l0 - 1) (T.splitOn nl file))
          oldLs = reverse (dropWhile blank (reverse (dropWhile blank (T.splitOn nl old))))
          -- (the first line stands where the match starts: it is compared only when the old text begins with an
          -- indent of its own and the file has nothing but spaces before the match)
          firstIn = case oldLs of { (o : _) -> ind o > 0 && blank (T.takeWhileEnd (/= '\n') (T.take s file)); _ -> False }
          pairs = zip [0 :: Int ..] (zip block oldLs)
          deltas = [ ind f - ind o | (i, (f, o)) <- pairs, not (blank f), not (blank o), i > 0 || firstIn ]
          sameShape = length block == length oldLs && and [ blank f == blank o | (f, o) <- zip block oldLs ]
          d = case deltas of { (x : _) -> x; [] -> 0 }
          shiftLine x l | blank l || x == 0 = Just l
                        | x > 0 = Just (T.replicate x (T.pack " ") <> l)
                        | T.length (T.takeWhile (== ' ') l) >= negate x = Just (T.drop (negate x) l)
                        | otherwise = Nothing
          shifted = case T.splitOn nl new' of
            (h : rest) -> (\r -> T.intercalate nl (h : r)) <$> mapM (shiftLine d) rest
            [] -> Just new'
          refusal why = Left (T.pack (printf "the text matches lines %d-%d only with its spacing squeezed, and %s: not applied. Those lines as they are -- copy old from them:\n" l0 l1 (why :: String))
                               <> T.intercalate nl [ numbered ' ' i l | (i, l) <- zip [l0 ..] block ])
      in if not sameShape then refusal "its lines are not those lines (their number, or which are blank)"
         else if any (/= d) deltas then refusal "its indentation differs from theirs by more than one constant"
         else case shifted of
           Nothing -> refusal (printf "its indentation differs from theirs by %d, and a line of the new text has fewer spaces to give" (negate d))
           Just nt -> Right (T.take s file <> nt <> T.drop e file, l0, l1, d)

-- | An edit where exact and fuzzy matching failed, but every line of 'old' matches consecutive lines in 'file'
-- when line-trailing whitespace is stripped from both.
lineTrimReplace :: T.Text -> T.Text -> T.Text -> Maybe (T.Text, Int, Int)
lineTrimReplace file old new
  | null target = Nothing
  | length matches == 1 =
      let (startLine, endLine) = head matches
          preLines = take (startLine - 1) fileLs
          postLines = drop endLine fileLs
          newFile = T.intercalate (T.pack "\n") (preLines ++ [new] ++ postLines)
      in Just (newFile, startLine, endLine)
  | otherwise = Nothing
  where
    fileLs = T.splitOn (T.pack "\n") file
    oldLs = T.splitOn (T.pack "\n") old
    stripLs = map T.stripEnd
    target = stripLs oldLs
    n = length target
    matches = [ (i + 1, i + n)
              | i <- [0 .. length fileLs - n]
              , stripLs (take n (drop i fileLs)) == target
              ]

-- | Where a text that occurs nowhere was probably meant to be: the file's first line that matches its first
-- non-blank line with the spaces squeezed, quoted -- the mismatch is most often whitespace or one word.
nearest :: T.Text -> T.Text -> T.Text
nearest file old = case filter (not . T.null . T.strip) (T.lines old) of
  [] -> T.empty
  (l0 : _) ->
    let squeeze = T.unwords . T.words
        hits = [ (i, l) | (i, l) <- zip [1 :: Int ..] (T.lines file), squeeze l == squeeze l0 ]
    in case hits of
      ((i, l) : _) | T.null (T.strip (T.pack (T.unpack l))) -> T.empty
                   | otherwise -> T.pack (printf "\n[its first line matches line %d with spaces squeezed: %s -- the text differs after it, or in its spacing]" i (show (T.unpack l)))
      [] -> T.pack "\n[its first line matches no line of the file: read the file around the spot and copy the text as it is]"
inside :: Chat -> FilePath -> Either String FilePath
inside ch p =
  let full = normalise (cDir ch </> p)
      r = makeRelative (cDir ch) full
  in if ".." `elem` splitDirectories r || "/" `isPrefixOf` r then Left (p ++ ": outside the project") else Right full

-- | A shell command in the project, within the seconds: its output (standard error after it) and status.
-- | A tool's answer with what the write changed after it.
withDiff :: T.Text -> (Bool, T.Text) -> (Bool, T.Text)
withDiff d (ok, out) = (ok, out <> d)

-- | What a save says when the file after it is the file before it. It follows "lines 3-5 replaced", and
-- "[nothing changed]" read as the edit not applied: it was -- with a new text equal to the one it replaced.
unchangedNote :: T.Text
unchangedNote = T.pack "\n[the edit was applied, and the file is byte for byte what it was: the new text is the text it replaced]"

-- | What a write changed in a file, as a unified diff (the system's @diff@), at most 'diffMax' lines of it: for
-- the answer of the tool that wrote. Nothing when nothing changed, or there is no @diff@ to ask.
changeOf :: FilePath -> T.Text -> T.Text -> IO T.Text
changeOf rel old new
  | old == new = pure unchangedNote
  | otherwise = do
      tmp <- getTemporaryDirectory
      pid <- getProcessID
      t <- now
      let base = tmp </> ("ghs-diff-" ++ show pid ++ "-" ++ show (round (t * 1000) :: Integer))
          (a, b) = (base ++ ".a", base ++ ".b")
      r <- try (do
        B.writeFile a (TE.encodeUtf8 old)
        B.writeFile b (TE.encodeUtf8 new)
        (_, out, _) <- readProcessWithExitCode "diff" ["-u", "--label", "a/" ++ rel, "--label", "b/" ++ rel, a, b] ""
        pure out) :: IO (Either SomeException String)
      mapM_ (\f -> try (removeFile f) :: IO (Either IOException ())) [a, b]
      pure $ case r of
        Right out | not (null out) ->
          let ls = lines out
              (shown, rest) = splitAt diffMax ls
          in T.pack ("\n" ++ unlines shown ++ (if null rest then "" else "[the diff goes on: " ++ show (length rest) ++ " more lines]\n"))
        _ -> T.empty

diffMax :: Int
diffMax = 60

shTool :: FilePath -> String -> Double -> IO (Bool, T.Text)
shTool dir cmd secs = do
  -- (in a group of its own, so that what the shell started ends with it: the shell alone was ended, and a
  -- command that ran out of time, or whose turn was stopped, went on behind it)
  (_, Just o, Just e, ph) <- createProcess (shell cmd) { cwd = Just dir, std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe, create_group = True }
  mo <- newEmptyMVar
  me <- newEmptyMVar
  void (forkIO (B.hGetContents o >>= putMVar mo))
  void (forkIO (B.hGetContents e >>= putMVar me))
  let end = do
        pid <- getPid ph
        forM_ pid (\p -> void (try (signalProcessGroup sigTERM p) :: IO (Either IOException ())))
        terminateProcess ph
  r <- timeout (round (secs * 1e6)) (waitForProcess ph) `onException` end
  case r of
    Nothing -> end >> void (waitForProcess ph) >> pure (False, T.pack "timed out")
    Just code -> do
      out <- decode <$> takeMVar mo
      err <- decode <$> takeMVar me
      let text = T.strip (out <> (if T.null (T.strip err) then T.empty else T.pack "\n[stderr]\n" <> err))
          shown = if T.null text then T.pack "(no output)" else text
      pure (code == ExitSuccess, shown <> (case code of { ExitFailure n -> T.pack ("\n[exit " ++ show n ++ "]"); _ -> T.empty }))

decode :: B.ByteString -> T.Text
decode = TE.decodeUtf8With TE.lenientDecode

trim :: String -> String
trim = dropWhileEnd' isSpace . dropWhile isSpace
  where dropWhileEnd' p = reverse . dropWhile p . reverse

-- | A tool result, head and tail, when it is over the cap.
cap :: T.Text -> T.Text
cap = capWith capChars ""

-- | A text's head and tail when it is over n characters, the cut said (with a hint, if any).
capWith :: Int -> String -> T.Text -> T.Text
capWith n hint t
  | T.length t <= n = t
  | otherwise = T.take h t <> T.pack ("\n[... " ++ show (T.length t - 2 * h) ++ " characters cut" ++ hint ++ " ...]\n") <> T.takeEnd h t
  where h = n `div` 2

-- | Characters of a shell command's output kept (head and tail): a build log or a test run's whole output
-- stays in the context of every later call of the turn, read again each time.
shCap :: Int
shCap = 8000

shHint :: String
shHint = ": filter it with grep, head or tail, or send it to a file and read the part you need"

-- | Does a shell command start a GHCi (ghci, cabal repl, stack ghci, runghc, ghc -e)? The command of each
-- part of a pipeline or list, past a timeout, env, time or nice -- not any word (a grep for ghci).
ownGhci :: String -> Bool
ownGhci cmd = any (ghciCmd . command . words) (segments cmd)
  where
    segments = lines . map (\c -> if c `elem` ";|&()`" then '\n' else c)
    command (w : r) | w `elem` ["timeout", "env", "time", "nice", "exec", "stdbuf", "nohup"] = command (dropWhile arg r)
                    | '=' `elem` w = command r
                    | otherwise = (takeFileName w, r)
    command [] = ("", [])
    arg x = "-" `isPrefixOf` x || '=' `elem` x || (not (null x) && all (`elem` "0123456789.smh") x)
    ghciCmd (w, r) = w `elem` ["ghci", "runghc", "runhaskell"]
                  || ("ghci-" `isPrefixOf` w && all (`elem` "0123456789.") (drop 5 w))
                  || (w == "cabal" && take 1 r `elem` [["repl"], ["v2-repl"]])
                  || (w == "stack" && take 1 r `elem` [["ghci"], ["repl"]])
                  || (w == "ghc" && any (`elem` ["-e", "--interactive"]) r)

-- the prompts --------------------------------------------------------------------------------

-- | What follows the memory's own prompt ('H.systemPrompt': the view, turns, compactions -- the part a
-- compaction shares, and reads from the turns' cache): what the session is and what its tools are for.
master :: String
master = unlines
  [ "# The session"
  , ""
  , "The chat is about a Haskell project whose code is loaded in a warm GHCi"
  , "session. The session is your sandbox: eval runs against the loaded code in"
  , "milliseconds; a file you write or edit is reloaded by the session itself"
  , "and the write's answer carries that reload's verdict (COMPILE-ERROR,"
  , "CHECK-FAIL, or OK -- CHECK-PASS); status repeats the latest verdict"
  , "(STALE when the loaded code is behind the disk). Prefer an evaluation to"
  , "a guess, and the verdict to a belief that an edit is right. Fix a"
  , "COMPILE-ERROR before anything else: the session answers from the last"
  , "code that compiled until you do."
  , ""
  , "Your tools, by what you are after:"
  , "- Finding code: doc looks a declaration up in the session by name (a typo,"
  , "  a prefix or initials will do), qualified, or by words of its type or its"
  , "  comment, and answers its signature, its comment and file:line -- ask it"
  , "  before you grep or read to find out what something is. grep searches the"
  , "  contents of the files, find the file names, vfs gives line counts and"
  , "  the line budget, ls lists a directory, read shows a file (start, lines)."
  , "- Changing code: edit replaces one exact, unique text; edits makes several"
  , "  replacements, in one file or many, with ONE reload and ONE verdict (use"
  , "  it for any change of more than one spot); write puts a whole file."
  , "  Writes that come together in one reply are made together and reloaded"
  , "  once, so send them in the same reply."
  , "- Checking it: eval for an expression; typecheck for whether the sources on"
  , "  disk compile, without loading them; test for the project's whole check,"
  , "  or with expr one group of its tests; reload only when status says STALE"
  , "  (a save reloads by itself); bench times an IO action, census says what"
  , "  the heap holds, mem the memory of the repl; restart when the session is"
  , "  dead or stuck."
  , "- sh is for what none of these does: not grep or find (use the tools), not"
  , "  ghci or cabal (the session is already warm)."
  , ""
  , "A line \"[image NAME]\" in a message stands for an image: one you read, or"
  , "one the user gave by naming its file. You are shown it where the message is"
  , "given to you whole -- when it is new, or when you zoom(id, 1) on it; a"
  , "summary only names it."
  , ""
  , "- The session's own settings are in ghci-session.json, and a change to it is"
  , "  taken a second after it is saved (the repl restarts on it). Under"
  , "  \"targets\", the session's entry: \"units\" (the build tool's components"
  , "  loaded: lib:NAME, test:NAME, exe:NAME), \"watch\" (directories whose saves"
  , "  reload), \"modules\" (in scope at the prompt), \"test\": {\"expr\": an IO"
  , "  action, \"pass\": a pattern its output has when it passed, \"fail\": one it"
  , "  has when it did not} -- what the test tool runs. At the top: \"optimize\":"
  , "  true (the code loaded compiled and optimised: evaluations and bench at the"
  , "  built code's speed), \"watch_check\": true (the test on every save),"
  , "  \"eval_timeout\". status with wait: N waits for the verdict of a restart"
  , "  or of a check that is running, instead of sleeping."
  , "- Many hands: spawn starts a subagent a task, all at once, each with your"
  , "  tools on this same session and these same files; you go on, and each one's"
  , "  last reply reaches you as a message \"[Name] report\" -- while you work, between"
  , "  two of your calls, or as a turn of its own. Give away what is apart from the"
  , "  rest (a search, a file of its own, a question to answer) and say in the task"
  , "  all that it needs: a subagent knows the view, not what you have in mind. Two"
  , "  that write the same file undo each other. tell sends one a message."
  , ""
  , "The view holds what was done to the code by hand too: a save of a file is"
  , "a tool line, its verdict an echo. What you learned that will matter later"
  , "can also be kept with remember: a finding, a decision, what is left undone." ]

-- | In --context view, what follows the view: every call of a turn is built again from the log.
recentDoc :: String -> String
recentDoc who = unlines
  [ ""
  , "Each step of your turn starts fresh too: you see the view, the user's"
  , "message, and then <recent> -- the messages logged after the view's last"
  , "line, in full, oldest first: your tool calls (name and arguments) and"
  , "their results, your replies, the session's own saves and verdicts, and"
  , "what the user sent while you work. It is your record of this turn;"
  , "your latest call and its result are at its end. Your earlier steps are"
  , "only there, not in a conversation: when <recent> grows long its older"
  , "messages move into the view as summaries -- zoom them when you need one"
  , "whole. Your plan, replies and directives stay pinned in <plan> once the"
  , "boundary moves. " ++ who ++ "'s thinking is not kept between steps: say in a reply"
  , "what you found out and what you will do next, so it stays in the record." ]

-- the turn loop ------------------------------------------------------------------------------

-- | One fresh call: the view, then the message; its tools until it ends.
turn :: Chat -> Endpoint -> Opts -> String -> [T.Text] -> TQueue (Maybe T.Text) -> IO ()
turn ch e o system texts pending = do
  (v, settled, parts, count) <- view ch (oSettle o)
  unless settled (uiNote (cUi ch) (printf "[view: %d lines, not all summarized yet; going on]" parts))
  ids <- forM texts (logTyped ch)
  when (isNothing (cSub ch)) (forM_ (listToMaybe (catMaybes ids)) (turnBegan ch))
  tStart <- now
  let task = T.intercalate (T.pack "\n\n") texts
  if eProvider e == ClaudeCli then turnCli ch e o system (v <> T.pack "\n\n" <> task) pending else
   goOn ch e o pending (TurnState [ msg "system" (T.pack system), msg "user" (v <> T.pack "\n\n" <> task) ] 0 0 [] (Spent 0 0 0 0 0 False False) tStart
                                 (if oView o then Just (ViewCtx count (catMaybes ids) task) else Nothing))

msg :: String -> T.Text -> Json
msg role text = JObj [("role", JStr role), ("content", JText text)]

-- a turn through the claude command -------------------------------------------------------------
--
-- A subscription is used through the @claude@ command ("GhciSession.ClaudeCli"), and that program runs the tools
-- itself: there is no reply to take a tool call from and answer. So the turn is its, and the chat is what it
-- calls. The chat listens on a socket for the length of the turn and serves its tools there, as a tool server
-- (the protocol of @ghci-session mcp@, over these tools); the program is told to start @ghci-session mcp-relay@
-- on that socket as its tool server, and has no tool of its own.
--
-- So a call is still run HERE, by the same code as in the other turn: shown, logged as the agent made it, its
-- answer what a write's or an evaluation's is (the reload's verdict, the diff), and a line typed meanwhile is
-- given to the program between two calls. The agent's words come from the program's output as each is
-- finished, and are logged. At its end a turn that changed the files and stops with the verdict red is told so
-- once, as the other is -- as a message more, the program still running.
--
-- What is not here is what belonged to a conversation the chat kept: a read is not checked against the reads
-- before it, there is no step limit, and @--context view@ and a restart in the middle of a turn do not apply.
turnCli :: Chat -> Endpoint -> Opts -> String -> T.Text -> TQueue (Maybe T.Text) -> IO ()
turnCli ch e o system first pending = now >>= \t -> turnCliFrom ch e o system first pending (Spent 0 0 0 0 0 False False) t

-- | The same, for a turn that has spent something already and began before now (one gone on with in a fresh call).
turnCliFrom :: Chat -> Endpoint -> Opts -> String -> T.Text -> TQueue (Maybe T.Text) -> Spent -> Double -> IO ()
turnCliFrom ch e o system first pending spent0 tStart = do
  writeIORef (cInTurn ch) True
  pid <- getProcessID
  sockDir <- (\d -> takeDirectory d) <$> Sys.sockPath (cStateDir (cConf ch) </> cName ch)
  let sock = sockDir </> ("chat-" ++ show pid ++ maybe "" (("-" ++) . fst) (cSub ch) ++ ".sock")
      effort = case oEffort o of { Just "none" -> Just "low"; other -> other }
      web = maybe False (> 0) (oWeb o)
      gaveUp why = uiNote (cUi ch) ("chat: " ++ why ++ "; the turn ends") >> writeIORef (cInTurn ch) False
  void (try (removeFile sock) :: IO (Either IOException ()))
  ml <- Sys.unixListen sock
  env0 <- getEnvironment
  exe <- getExecutablePath
  started <- case ml of
    Nothing -> pure (Left ("cannot listen on " ++ sock))
    Just _ -> either (\(x :: IOException) -> Left ("the claude command could not be run (is Claude Code installed, and on the PATH?): " ++ show x)) Right
                <$> try (createProcess (proc "claude" (C.cliArgs (C.CliOpts (eModel e) effort (T.pack system) (Just (exe, sock)) web False)))
                           { cwd = Just (cDir ch), env = Just (C.cliEnv env0), std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
  case (ml, started) of
    (Just lfd, Right (Just i, Just out, Just err, ph)) -> do
      mp <- getPid ph
      case mp of
        Nothing -> gaveUp "the claude command ended as it started"
        Just cpid -> do
          -- (as descriptors, read by "GhciSession.Wire": nothing of them is in a buffer of the runtime's, so the
          -- program this chat restarts as can be handed them, and the turn with them)
          fi <- PIO.handleToFd i
          fo <- PIO.handleToFd out
          fe <- PIO.handleToFd err
          blocks <- cliBlocks ch (A.viewBlocks first) first
          -- (written aside: a message past the pipe's 64KB blocks until the program reads, and a program that reads its
          -- input only once its tools are connected would wait for this run's accept loop, which starts in 'runCli':
          -- a whole chat's view in a long chat is that big)
          void (forkIO (void (try (Wire.fdPut fi (C.userLine blocks)) :: IO (Either IOException ()))))
          runCli ch e o pending (CliRun (fromIntegral cpid) fi fo fe lfd sock B.empty [] spent0 tStart)
    (_, Left why) -> gaveUp why
    (Nothing, _) -> gaveUp ("cannot listen on " ++ sock)
    _ -> gaveUp "the claude command could not be run"

-- | A message's blocks, and the images its text names after them.
cliBlocks :: Chat -> [Json] -> T.Text -> IO [Json]
cliBlocks ch blocks t = (blocks ++) . map Img.apiBlock . catMaybes <$> mapM (Img.load (imagesDir ch)) (Img.namesIn t)

-- | A turn through the claude command, under way, as all that is needed to go on with it: the program (its
-- process, and the descriptors of its input, output and errors), the socket its tools are served on and the
-- connections made to it -- each with what was read from it and not yet used -- and the turn's own count.
-- It is what a chat hands to the program it restarts as: that program is this process still, so the claude
-- command is its child still and every descriptor is open still, and the turn goes on without the model
-- having seen anything happen.
data CliRun = CliRun { crPid :: Int, crIn, crOut, crErr, crListen :: Fd, crSock :: FilePath
                     , crOutRest :: B.ByteString, crConns :: [(Fd, B.ByteString)], crSpent :: Spent, crStart :: Double }

cliJson :: CliRun -> Json
cliJson r = JObj
  [ ("pid", n (crPid r)), ("in", fd (crIn r)), ("out", fd (crOut r)), ("err", fd (crErr r)), ("listen", fd (crListen r)), ("sock", JStr (crSock r))
  , ("out_rest", bytes (crOutRest r)), ("conns", JArr [ JObj [("fd", fd f), ("rest", bytes b)] | (f, b) <- crConns r ]), ("start", JNum (crStart r))
  , ("spent", let s = crSpent r in JArr (map n [sCalls s, sIn s, sCached s, sOut s, sTools s] ++ map JBool [sTouched s, sNudged s])) ]
  where n = JNum . fromIntegral
        fd = JNum . fromIntegral
        bytes = JText . TE.decodeLatin1      -- (a byte a character: any bytes go through)

cliFrom :: Json -> Maybe CliRun
cliFrom j = do
  pid <- num' "pid"
  [fi, fo, fe, fl] <- mapM (fmap fromIntegral . num') ["in", "out", "err", "listen"]
  sock <- lookupStr "sock" j
  start <- lookupNum "start" j
  spent <- case lookupArr "spent" j of
    [a, b, c, d, f, JBool t, JBool nd] | Just [a', b', c', d', f'] <- mapM (fmap round . num) [a, b, c, d, f] -> Just (Spent a' b' c' d' f' t nd)
    _ -> Nothing
  pure (CliRun pid fi fo fe fl sock (bytes (j .: "out_rest")) [ (fromIntegral (round f :: Int), bytes (c .: "rest")) | c <- lookupArr "conns" j, Just f <- [lookupNum "fd" c] ] spent start)
  where num' k = round <$> lookupNum k j :: Maybe Int
        bytes = maybe B.empty (B8.pack . T.unpack) . txt

-- | A turn through the claude command, from where it is to its end -- or to the chat's restart, which it is
-- handed over to.
--
-- The program's output, and each connection its tool relay makes, is read a line at a time with the wait for
-- a line able to end ("GhciSession.Wire"). So when a restart is asked for (@chat --restart@, a SIGHUP) every
-- reader comes to rest once it is between two lines -- one in the middle of a tool call first finishes it and
-- answers -- and then nothing is half done: the descriptors are kept open, what was read ahead is written
-- down with them, and the chat becomes the executable on disk, which takes the turn up ('cliFrom'). A call the
-- program makes meanwhile waits in its socket. Not while subagents are at work: they are threads of this
-- process, and would end with it.
runCli :: Chat -> Endpoint -> Opts -> TQueue (Maybe T.Text) -> CliRun -> IO ()
runCli ch e o pending run = do
  writeIORef (cInTurn ch) True
  spent <- newIORef (crSpent run)
  outW <- Wire.newWire (crOut run) (crOutRest run)
  errW <- Wire.newWire (crErr run) B.empty
  wlock <- newMVar ()
  done <- newIORef False
  errR <- newIORef B.empty
  _ <- forkIO $ void $ (try :: IO () -> IO (Either SomeException ())) $
         let go = Wire.nextLine errW (readIORef done) >>= \g -> case g of
               Wire.Line l -> modifyIORef' errR (\b -> B.take 20000 (b <> l <> B8.pack "\n")) >> go
               _ -> pure ()
         in go
  resting <- newTVarIO (0 :: Int)      -- the readers at rest, for a hand-over
  readers <- newTVarIO (1 :: Int)      -- the readers there are: who waits for connections, and one a connection
  again <- newTVarIO (0 :: Int)        -- moved when a hand-over did not happen: the readers go on
  connsV <- newIORef (M.empty :: M.Map Fd Wire.Wire)
  -- (a run taken up from a chat before this one began long ago: what it began with is not known, and is not what holds a rollover back)
  firstIn <- newIORef (if null (crConns run) && sCalls (crSpent run) == 0 then 0 else 1 :: Int)       -- the context of this run's first model call
  rollDue <- newIORef (0 :: Int)       -- the context that is over the rollover's tokens (0: it is not)
  rotR <- newIORef emptyRot             -- the reads of this run's context: the share of repeats is the rot signal
  rotSaid <- newIORef False             -- the rot has been said (once a run)
  rollSoft <- newIORef (0 :: Int)       -- the context that is near the controller's threshold: the run ends at the next natural boundary
  prevCtx <- newIORef (0 :: Int)        -- the context of this run's last call
  lastEnd <- newIORef (0 :: Double)     -- when this run's last model call ended (0: none yet)
  rollWhy <- newIORef ""                -- why the run rolls over, where it is not for the context's size
  rollNow <- newIORef False            -- a tool call has been answered into the log and not to the program: the run ends here
  let byName = [ (tName t, t) | t <- toolsFor ch ]
      textBlock t = JObj [("type", JStr "text"), ("text", JText t)]
      asked = readIORef (cRestart ch)
      put blocks = withMVar wlock $ \_ -> void (try (Wire.fdPut (crIn run) (C.userLine blocks)) :: IO (Either IOException ()))
      -- a reader at rest, until a hand-over that did not happen lets it go on
      rest = do
        g <- readTVarIO again
        atomically (modifyTVar' resting (+ 1))
        atomically (readTVar again >>= \g' -> check (g' /= g))
        atomically (modifyTVar' resting (subtract 1))
      -- a tool, called by the program: run here, as a turn's call is
      tool name args = do
        modifyIORef' spent (\x -> x { sTools = sTools x + 1, sTouched = sTouched x || name `elem` ["write", "edit", "edits", "sh"] })
        uiCall (cUi ch) name (take 300 (encode args))
        modifyIORef' rotR (seeRot name args)
        unless (name == "remember") (logH ch "tool" (T.pack (name ++ " " ++ encode args)))
        uiBusy (cUi ch) (Just ("running " ++ name))
        t2 <- now
        (ok, out0) <- case lookup name byName of
          Just t -> runTool ch t args
          Nothing -> pure (False, T.pack ("unknown tool " ++ show name))
        late <- pendingNote ch
        t3 <- now
        let said = cap out0 <> late
        when (oUsage o) (uiNote (cUi ch) (printf "[tool: %s %.1fs]" name (t3 - t2)))
        unless (name == "remember") (logH ch "echo" ((if ok then T.empty else T.pack "ERROR: ") <> said))
        uiAnswer (cUi ch) (shownCut (cShow ch) said)
        -- (the cache's lifetime has run out under this context: the next call would read it all uncached -- a fresh call is less)
        when (isNothing (cSub ch) && oRollover o < 0) $ do
          ended <- readIORef lastEnd
          ctxNow <- readIORef prevCtx
          ro <- readIORef (cRoll ch)
          f <- readIORef firstIn
          already <- readIORef rollDue
          when (already == 0 && f > 0 && ended > 0 && coldDue ro (t3 - ended) ctxNow) $ do
            writeIORef rollWhy (printf "%d minutes passed since its last call, past what the provider keeps its cache for (%d), with %s tokens of context" (round ((t3 - ended) / 60) :: Int) (round (lifetime ro / 60) :: Int) (human ctxNow))
            writeIORef rollDue ctxNow
        -- (near the threshold, a commit that went through or a green test is where to end: not in the middle of an edit)
        soft <- readIORef rollSoft
        when (soft > 0 && boundaryTool name args ok (T.unpack out0)) (writeIORef rollDue soft)
        due <- readIORef rollDue
        if due > 0
          then do
            -- The rollover. The call and its answer are in the log; the program is not given the answer: this
            -- run ends here, and a fresh one reads the log and goes on from it ('continueTurn').
            writeIORef rollNow True
            let wait = readIORef done >>= \d -> unless d (threadDelay 100000 >> wait)
            wait
            pure (ok, said)
          else do
            -- (a line typed while it works reaches it here, between two calls)
            mid <- drain pending >>= typedLines ch
            forM_ mid (logTyped ch)
            fedNote ch mid
            unless (null mid) (cliBlocks ch [textBlock (T.intercalate (T.pack "\n\n") mid)] (T.unlines mid) >>= put)
            uiBusy (cUi ch) (Just "the model is working")
            pure (ok, said)
      answer = Mcp.handleWith (toolsFor ch) tool (Img.toolBlocks (imagesDir ch))
      conn w = do
        g <- Wire.nextLine w asked
        case g of
          Wire.Line l | B.null (B8.strip l) -> conn w
                      | otherwise -> do
                          r <- either (const (pure Nothing)) answer (parseJsonBS l)
                          forM_ r (\j -> Wire.fdPut (Wire.wFd w) (encodeBS j <> B8.pack "\n"))
                          conn w
          Wire.Asked -> rest >> conn w
          Wire.End -> pure ()
      serveConn (fd, rest0) = do
        w <- Wire.newWire fd rest0
        atomicModifyIORef' connsV (\m -> (M.insert fd w m, ()))
        atomically (modifyTVar' readers (+ 1))
        void (forkIO ((void (try (conn w) :: IO (Either SomeException ())))
                        `finally` (do atomicModifyIORef' connsV (\m -> (M.delete fd m, ()))
                                      atomically (modifyTVar' readers (subtract 1))
                                      void (try (PIO.closeFd fd) :: IO (Either IOException ())))))
      accept = do
        stop <- readIORef done
        unless stop $ do
          a <- asked
          if a then rest >> accept else do
            mc <- Sys.unixAccept (crListen run) 300
            forM_ mc (\fd -> serveConn (fd, B.empty))
            accept
  mapM_ serveConn (crConns run)
  -- (a turn taken up by a newer chat: its tools may be other than the program was told at its start -- it is told
  -- to ask again)
  forM_ (crConns run) $ \(fd, _) -> void (try (Wire.fdPut fd (encodeBS (JObj [("jsonrpc", JStr "2.0"), ("method", JStr "notifications/tools/list_changed")]) <> B8.pack "\n")) :: IO (Either IOException ()))
  _ <- forkIO ((void (try accept :: IO (Either SomeException ()))) `finally` atomically (modifyTVar' readers (subtract 1)))
  seenR <- newIORef (0 :: Int, 0 :: Double)
  limitR <- newIORef ""
  refusedR <- newIORef (Nothing :: Maybe Double)      -- the subscription's limit was reached: when it resets
  callR <- newIORef (Nothing :: Maybe (Int, Int, Double))      -- the model call under way: its prompt's tokens, of them read from the cache, when it began
  callsR <- newIORef (0 :: Int)                                -- the calls written to the ledger since the last result
  rateR <- newIORef ([] :: [Window], Nothing :: Maybe Bool)    -- the plan's windows as last seen, and whether the last event said the calls are on usage credits
  writeR <- newIORef (0 :: Int, 0 :: Int, 0 :: Int, 0 :: Int)  -- the call under way: tokens neither read nor written, written, of them for five minutes, for an hour
  let -- each model call is in the ledger as it ends (its prompt's tokens and how many of them were read from the
      -- cache are in the stream's first event, what it wrote in its last): a turn of hours was one line, at its end
      usage ev = case lookupStr "type" ev of
        Just "message_start" -> do
          let u = ev .: "message" .: "usage"
              k name = maybe 0 round (lookupNum name u) :: Int
          t <- now
          let ctx = k "input_tokens" + k "cache_read_input_tokens" + k "cache_creation_input_tokens"
          writeIORef callR (Just (ctx, k "cache_read_input_tokens", t))
          let (wr, w5, w1) = writesOf u
          writeIORef writeR (k "input_tokens", wr, w5, w1)
          -- (the rollover: the chat's own turn, its context past the tokens -- and grown by a third since this run
          -- began, so that a run that starts over them does not end at its first call)
          f <- readIORef firstIn
          when (f == 0) (writeIORef firstIn ctx)
          prev <- readIORef prevCtx
          writeIORef prevCtx ctx
          when (isNothing (cSub ch)) $ do
            ended <- readIORef lastEnd
            ro <- seeGap (if ended > 0 then t - ended else 0) prev ctx (k "cache_read_input_tokens") . seeCall (f == 0) prev ctx . seeWrites w5 w1 <$> readIORef (cRoll ch)
            -- (the controller: what a restart and a call's growth are, as seen; its threshold said when it has moved)
            let thr = threshold ro
                mv = oRollover o < 0 && moved (rSaid ro) thr
            when (mv && oUsage o) (uiNote (cUi ch) (printf "[rollover: at %dk tokens (a fresh call %dk, %d tokens a call, a write %.1f reads)]" (thr `div` 1000) (round (rS ro) `div` 1000 :: Int) (round (rG ro) :: Int) (rRatio ro)))
            share <- rotShare <$> readIORef rotR
            let ro' = (if mv then ro { rSaid = thr } else ro) { rRot = fromMaybe (-1) share }
            writeIORef (cRoll ch) ro'
            saveRoll (rollFile (cConf ch) (cName ch)) ro'
            let grown = f > 0 && 3 * ctx > 4 * f
                auto = oRollover o < 0
            said <- readIORef rotSaid
            let rot = auto && rotOver share
                lim = (if rot then fmap (rotLimit True) else id) (limitOf (oRollover o) ro')
            when (rot && not said) $ do
              writeIORef rotSaid True
              uiNote (cUi ch) (printf "[rollover: %.0f%% of the last 20 reads read again what this context had read: it is rotting, so the threshold is a tenth lower, %s tokens]" (100 * fromMaybe 0 share :: Double) (maybe "-" human lim))
            when (grown && maybe False (\l -> rollAt auto l ctx False) lim) (writeIORef rollDue ctx)
            writeIORef rollSoft (if grown && auto && maybe False (\l -> rollAt auto l ctx True) lim then ctx else 0)
        Just "message_delta" | Just outN <- lookupNum "output_tokens" (ev .: "usage") -> do
          c <- readIORef callR
          forM_ c $ \(inN, cached, t0) -> do
            t <- now
            writeIORef callR Nothing
            modifyIORef' callsR (+ 1)
            writeIORef lastEnd t
            -- (and in the turn's count: a run that is ended before its result -- a rollover -- gave none)
            modifyIORef' spent (\sp -> sp { sCalls = sCalls sp + 1, sIn = sIn sp + inN, sCached = sCached sp + cached, sOut = sOut sp + round outN })
            (nw, wr, w5, w1) <- readIORef writeR
            (wins, credits) <- readIORef rateR
            let extra = [ ("new", JNum (fromIntegral nw)), ("wr", JNum (fromIntegral wr)), ("wr5m", JNum (fromIntegral w5)), ("wr1h", JNum (fromIntegral w1)) ]
                        ++ [ ("windows", windowsJson wins) | not (null wins) ] ++ [ ("credits", JBool b) | Just b <- [credits] ]
            recordUsageWith extra (usageFile ch) (maybe "chat" (const "agent") (cSub ch)) e (Usage inN (round outN) (Just cached)) (t - t0)
            when (oUsage o) (uiNote (cUi ch) (printf "[usage: in %d, out %d, cached %d (%d%%), %.1fs]" inN (round outN :: Int) cached (if inN == 0 then 0 else 100 * cached `div` inN) (t - t0)))
        _ -> pure ()
      live ev = usage ev >> case snd (A.streamEvent ev A.emptyStream) of
        Nothing -> pure ()
        Just (kind, piece) -> do
          (n, at) <- readIORef seenR
          t <- now
          let n' = n + T.length piece
              doing = case kind of { "mind" -> "thinking"; "tool" -> "calling a tool"; _ -> "writing" } :: String
          if t - at < 0.25 then writeIORef seenR (n', at) else writeIORef seenR (n', t) >> uiBusy (cUi ch) (Just (printf "the model is %s (%s characters so far)" doing (human n')))
      said b = case lookupStr "type" b of
        Just "text" | Just t <- lookupText "text" b, not (T.null (T.strip t)) -> uiTalk (cUi ch) (T.strip t) >> logH ch "talk" (T.strip t)
        Just "thinking" | Just t <- lookupText "thinking" b, not (T.null (T.strip t)) -> uiThought (cUi ch) (T.strip t)
        -- (a tool of the program's own -- the web, when it was asked for: ours are shown where they are run)
        Just "tool_use" | Just n <- lookupStr "name" b, not (("mcp__" ++ C.serverName ++ "__") `isPrefixOf` n) -> do
          uiCall (cUi ch) (n ++ " (by claude)") (take 300 (encode (b .: "input")))
          logH ch "tool" (T.pack (n ++ " " ++ encode (b .: "input")))
        _ -> pure ()
      -- the turn handed to the program this chat restarts as; back here only if that did not happen
      handOver = do
        working <- (\m -> length [ () | s <- M.elems m, isJust (suThread s) ]) <$> readIORef (cAgents ch)
        if working > 0
          then do
            writeIORef (cRestart ch) False
            uiNote (cUi ch) (printf "[harness: no restart while %d subagent(s) are at work (they would end with it): ask again when they have reported]" working)
          else do
            uiBusy (cUi ch) (Just "restarting the harness: waiting for the tool call under way, if there is one")
            atomically (do { r <- readTVar readers; n <- readTVar resting; check (n >= r) })
            conns <- readIORef connsV >>= mapM (\w -> (,) (Wire.wFd w) <$> Wire.wRest w) . M.elems
            outRest <- Wire.wRest outW
            sp <- readIORef spent
            mapM_ Wire.keepOnExec ([crIn run, crOut run, crErr run, crListen run] ++ map fst conns)
            restartWith ch pending Nothing [("cli", cliJson run { crOutRest = outRest, crConns = conns, crSpent = sp })]
        atomically (modifyTVar' again (+ 1))
      loop t0 = do
        g <- Wire.nextLine outW ((||) <$> asked <*> readIORef rollNow)
        case g of
          Wire.End -> pure Nothing
          Wire.Asked -> readIORef rollNow >>= \roll -> if roll then pure Nothing else handOver >> loop t0
          Wire.Line l -> case C.readEvent l of
            C.EvStream ev -> live ev >> loop t0
            C.EvAssistant blocks -> mapM_ said blocks >> loop t0
            C.EvRate info -> do
              modifyIORef' rateR (\(ws, cr) -> (mergeWindows ws (windowsOf info), maybe cr Just (creditsOf info)))
              writeIORef refusedR (if lookupStr "status" info == Just "rejected" then lookupNum "resetsAt" info else Nothing)
              note <- C.limitNote info
              was <- readIORef limitR
              forM_ note $ \n -> when (n /= was) (writeIORef limitR n >> uiNote (cUi ch) ("[" ++ n ++ "]"))
              loop t0
            C.EvResult j -> do
              let x = C.resultOf j
                  u = Usage (C.xIn x) (C.xOut x) (Just (C.xCached x))
              t1 <- now
              -- (the ledger and the turn's count have each call already, where the stream said them)
              each <- readIORef callsR
              writeIORef callsR 0
              when (each == 0) (modifyIORef' spent (\sp -> sp { sCalls = sCalls sp + C.xTurns x, sIn = sIn sp + C.xIn x, sCached = sCached sp + C.xCached x, sOut = sOut sp + C.xOut x }))
              when (each == 0) (recordUsage (usageFile ch) (maybe "chat" (const "agent") (cSub ch)) e u (t1 - t0))
              when (oUsage o) (uiNote (cUi ch) (printf "[usage: in %d, out %d, cached %d, %d model call(s), %.1fs]" (C.xIn x) (C.xOut x) (C.xCached x) (C.xTurns x) (t1 - t0)))
              if not (C.xOk x) then pure (Just (T.unpack (C.xText x))) else do
                -- a turn that changed the files and ends with the verdict red is told so, once
                sp <- readIORef spent
                when (sTouched sp && not (sNudged sp)) (awaitPending ch)
                vd <- if sTouched sp && not (sNudged sp) then verdictAt ch else pure Nothing
                case vd of
                  Just r | isRed (vLine r) -> do
                    modifyIORef' spent (\y -> y { sNudged = True })
                    uiNote (cUi ch) "[the turn would end with the verdict red; saying so once]"
                    put [textBlock (T.pack ("[harness: you are ending the turn with the session's verdict red: " ++ vLine r ++ concatMap ("\n" ++) (take 8 (vBehind r))
                                             ++ "\nFix it, or end by saying plainly that it is red and why you are stopping.]"))]
                    loop t1
                  _ -> pure Nothing
            _ -> loop t0
      cpid = fromIntegral (crPid run) :: CPid
      kill sig = void (try (signalProcess sig cpid) :: IO (Either IOException ()))
      -- (looked at, not waited on: a wait in the system cannot be given up)
      gone secs = do
        t0 <- now
        let go = do
              r <- try (getProcessStatus False False cpid) :: IO (Either IOException (Maybe ProcessStatus))
              t <- now
              case r of
                Right Nothing | t - t0 < secs -> threadDelay 50000 >> go
                Right Nothing -> pure False
                _ -> pure True
        go
  uiBusy (cUi ch) (Just "the model is working")
  r <- try (loop (crStart run)) :: IO (Either SomeException (Maybe String))
  writeIORef done True
  void (try (PIO.closeFd (crIn run)) :: IO (Either IOException ()))
  let stopped = case r of { Left x | Just StopTurn <- fromException x -> True; _ -> False }
  rolled <- (&& not stopped) <$> readIORef rollNow
  when (stopped || rolled) (kill sigTERM)
  ended <- gone 3
  unless ended (kill sigTERM >> gone 2 >>= \g -> unless g (kill sigKILL >> void (gone 2)))
  mapM_ (\f -> void (try (PIO.closeFd f) :: IO (Either IOException ()))) [crListen run, crOut run]
  void (try (removeFile (crSock run)) :: IO (Either IOException ()))
  errText <- T.unpack . T.strip . decode <$> readIORef errR
  when stopped (throwIO StopTurn)
  if rolled
    then do
      -- the turn goes on in a fresh call: the view as it is now, and the turn's log
      due <- readIORef rollDue
      sp <- readIORef spent
      uiNote (cUi ch) (printf "[the turn's context is at %s tokens: it goes on in a fresh call, from its log]" (human due))
      system <- readIORef mainSystem
      why <- readIORef rollWhy
      continueTurn ch e o system pending (Just (if null why then printf "its context had grown to %s tokens" (human due) else why, sp, crStart run))
    else do
     refused <- readIORef refusedR
     tNow <- now
     case (r, refused) of
      -- The subscription's limit, reached in the middle of the turn: the turn is not over, it is waiting. When the
      -- limit has reset it goes on from its log, by itself -- a run of hours left alone ended at its first limit.
      (Right (Just why), Just at) | isNothing (cSub ch), at > tNow - 60, at - tNow < 8 * 3600 -> do
        sp <- readIORef spent
        whenS <- formatTime defaultTimeLocale "%H:%M" <$> utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac at))
        uiNote (cUi ch) ("[" ++ why ++ " -- the turn waits for the limit to reset, at " ++ whenS ++ ", and then goes on from its log (Esc, or Ctrl-C, ends it instead)]")
        logH ch "echo" (T.pack ("harness: the subscription's limit was reached; the turn waits until " ++ whenS))
        let wait = do
              t <- now
              when (t < at + 20) $ do
                uiBusy (cUi ch) (Just (printf "waiting for the subscription's limit to reset at %s (%d min)" whenS (max 0 (round ((at - t) / 60)) :: Int)))
                threadDelay (round (1e6 * min 30 (at + 20 - t)))
                checkStop ch
                wait
        wait
        system <- readIORef mainSystem
        continueTurn ch e o system pending (Just ("the subscription's limit was reached, and has now reset", sp, crStart run))
      _ -> do
       case r of
        Left x -> uiNote (cUi ch) ("chat: the claude command: " ++ show x ++ "; the turn ends")
        Right (Just why) -> uiNote (cUi ch) ("chat: " ++ why ++ "; the turn ends")
        Right Nothing -> do
          sp <- readIORef spent
          -- (it ended and said nothing at all: what it wrote of itself is why)
          when (sCalls sp == 0 && not (null errText)) (uiNote (cUi ch) ("chat: the claude command ended: " ++ unwords (take 40 (words errText))))
       writeIORef (cInTurn ch) False
       tEnd <- now
       s <- readIORef spent
       uiSpent (cUi ch) s (tEnd - crStart run)
       noteSpent ch s (tEnd - crStart run)

-- a turn taken up from its log ---------------------------------------------------------------------
--
-- A turn's conversation is in the process that had it -- the chat's, or the claude command's -- and ends with
-- it: a crash, a kill, a machine that went down, a chat restarted as a program that cannot be handed the turn.
-- What does not end is the history: the turn's message and every call and answer since, word for word. So
-- @chat --continue@ starts by going on with the last turn there: a fresh call that reads the view up to where
-- the log it is given begins, then that log (@\<recent\>@: the turn's message, and its last messages as far as
-- the tail's bytes go -- what is between is in the view, as summaries), and is told to go on from its end. The
-- agent has what it did and what came of it; what it had in mind between the steps it has not.

-- | Go on with the last turn of the history, from its log.
continueTurn :: Chat -> Endpoint -> Opts -> String -> TQueue (Maybe T.Text) -> Maybe (String, Spent, Double) -> IO ()
continueTurn ch e o system pending rolled = do
  r <- ask ch "history" [("n", JNum 20000), ("json", JBool True)]
  tj <- (either (const JNull) (either (const JNull) id . parseJsonBS)) <$> (try (B.readFile (turnFile ch)) :: IO (Either IOException B.ByteString))
  let ms = case parseJsonBS (TE.encodeUtf8 (fromMaybe T.empty (lookupText "out" r))) of
        Right (JArr xs) -> [ (round i, k, t) | x <- xs, Just i <- [lookupNum "i" x], Just k <- [lookupText "kind" x], Just t <- [lookupText "text" x] ] :: [(Int, T.Text, T.Text)]
        _ -> []
      user = T.pack "user"
      -- where the turn began: as it was noted when it did; else (a turn of a chat that noted nothing) the last
      -- message of the user's
      began = case round <$> lookupNum "start" tj :: Maybe Int of
        Just b | any (\(i, _, _) -> i == b) ms -> Just b
        _ -> ownTurnStart ms
      turnMs = maybe [] (\b -> dropWhile (\(i, _, _) -> i < b) ms) began
  case turnMs of
    _ | isNothing rolled, lookupBool "done" tj == Just True -> uiNote (cUi ch) "[nothing to go on with: the last turn ended]"
    (task@(taskId, _, _) : after) -> do
      -- (a rollover carries a third of what a restart does: a fresh run that began at two thirds of the
      -- rollover's tokens was over them again in thirty calls, each time for a cold first call)
      let Just sp = carrySplit (if isJust rolled then max 12000 (oTail o `div` 3) else max 20000 (oTail o)) (task : after)
          why = maybe "the chat that was in it stopped, and this is a new one" (\(w, _, _) -> w ++ ", so it goes on here in a fresh call") rolled
      -- (what the reset before this one made the agent read again: counted once, logged, and learned -- the controller's relearn share)
      when (isJust rolled) $ case reverse (rollEchoes ms) of
        (r : _) | Just t0 <- taskBefore r ms
                , Just (h, n) <- relearned 10 [ m | m@(i, _, _) <- ms, i >= t0, i < r ] [ m | m@(i, _, _) <- ms, i > r ] -> do
          ro <- readIORef (cRoll ch)
          when (rRelId ro < r) $ do
            let ro' = seeRelearn r (fromIntegral h / fromIntegral n) ro
            writeIORef (cRoll ch) ro'
            saveRoll (rollFile (cConf ch) (cName ch)) ro'
            logH ch "echo" (T.pack (printf "harness: after the reset at message %d, %d of the next %d calls read again a file read before it (the controller counts %.0f%% of a fresh call as learned again)" r h n (100 * rRelearn ro' :: Double)))
        _ -> pure ()
      v <- viewBefore ch (spFrom sp) (oSettle o)
      logH ch "echo" (T.pack ("harness: this turn goes on in a fresh call, from its log (" ++ why ++ ")"))
      uiNote (cUi ch) (printf "[going on with the turn of message %d: %d message(s) of it given whole, %d as summaries]" taskId (1 + length (spTold sp) + length (spKept sp)) (spLeft sp))
      tNow <- now
      let first = carryFirst v sp (carryFiles (task : after)) (carryNote why)
          sys = system ++ (if "<recent> -- the turn's own messages" `isInfixOf` system then "" else continueDoc)
          (spent0, tStart) = maybe (Spent 0 0 0 0 0 False False, tNow) (\(_, sp, t) -> (sp, t)) rolled
      if eProvider e == ClaudeCli then turnCliFrom ch e o sys first pending spent0 tStart
        else goOn ch e o pending (TurnState [ msg "system" (T.pack sys), msg "user" first ] 0 0 [] spent0 tStart Nothing)
    [] -> uiNote (cUi ch) "[nothing to go on with: no turn of this chat's own -- no turn.json, and the last message of the user's is not followed by work of this chat (an imported conversation's is answered by ai messages)]"

-- | Where a turn began, when the chat noted nothing (no turn.json, or one that names a message not in the log): the
-- last message of the user's -- if it is THIS chat's. The log holds other agents' conversations as well (what
-- @import@ brings: the user's message of another session, answered by an @ai@ message), and going on with one of
-- those is answering a question nobody asked this chat. This chat's own turn is a user message whose next message
-- is its own work: a tool call, an echo, a reply. One followed by an @ai@ message, or by nothing at all (it is
-- not known whose it was), is not gone on with: turn.json is what says that a turn was begun and not ended.
ownTurnStart :: [(Int, T.Text, T.Text)] -> Maybe Int
ownTurnStart ms = case break (\(_, k, _) -> k == T.pack "user") (reverse ms) of
  (_, []) -> Nothing
  (afterRev, (i, _, _) : _) -> case [ k | (_, k, _) <- reverse afterRev, k `notElem` map T.pack ["note", "known"] ] of
    (k : _) | k `elem` map T.pack ["tool", "echo", "talk"] -> Just i
    _ -> Nothing

continueDoc :: String
continueDoc = unlines
  [ ""
  , "A turn that did not end is gone on with from its log: its first message then"
  , "holds the view, and <recent> -- the turn's own messages in full, oldest first,"
  , "as \"id|kind: text\": the user's message that began it, your tool calls (name"
  , "and arguments) and their results, your replies, the session's saves and"
  , "verdicts. It is your record of the turn so far; its end is where to go on." ]

-- | Where a turn is, between two model calls: what a restarted harness needs to go on with it -- the
-- conversation word for word (the provider's cache of it holds across the restart), the step and the
-- cut-off replies so far, the reads in it, what it has spent, and when it began.
data TurnState = TurnState { tsMsgs :: [Json], tsStep :: Int, tsCut :: Int, tsReads :: [ReadRec], tsSpent :: Spent, tsStart :: Double
                           , tsView :: Maybe ViewCtx }
  deriving (Eq, Show)

-- | A turn in --context view: the log's first message not in the view (the boundary), the ids of the
-- turn's own message (it is pinned after the view, so not shown again in <recent>), and that message.
-- tsMsgs is then the call's fixed head: the system prompt, and the view up to the boundary with the message.
data ViewCtx = ViewCtx { vcBound :: Int, vcSkip :: [Int], vcTask :: T.Text }
  deriving (Eq, Show)

turnJson :: TurnState -> Json
turnJson ts = JObj
  [ ("msgs", JArr (tsMsgs ts)), ("step", int (tsStep ts)), ("cut", int (tsCut ts)), ("start", JNum (tsStart ts))
  , ("reads", JArr [ JObj [ ("num", int (rrNum r)), ("idx", int (rrIdx r)), ("path", JStr (rrPath r)), ("lo", int (rrLo r)), ("hi", int (rrHi r))
                          , ("text", JText (rrText r)), ("by", maybe JNull int (rrBy r)), ("trimmed", JBool (rrTrimmed r)) ] | r <- tsReads ts ])
  , ("spent", let x = tsSpent ts in JObj [ ("calls", int (sCalls x)), ("in", int (sIn x)), ("cached", int (sCached x)), ("out", int (sOut x))
                                         , ("tools", int (sTools x)), ("touched", JBool (sTouched x)), ("nudged", JBool (sNudged x)) ])
  , ("view", maybe JNull (\vc -> JObj [ ("bound", int (vcBound vc)), ("skip", JArr (map int (vcSkip vc))), ("task", JText (vcTask vc)) ]) (tsView ts)) ]
  where int = JNum . fromIntegral

turnFrom :: Json -> Maybe TurnState
turnFrom j = do
  ms <- arr (j .: "msgs")
  sp <- obj (j .: "spent")
  let sj = JObj sp
  rs <- mapM readFrom (lookupArr "reads" j)
  TurnState ms <$> int "step" j <*> int "cut" j <*> pure rs
    <*> (Spent <$> int "calls" sj <*> int "in" sj <*> int "cached" sj <*> int "out" sj <*> int "tools" sj <*> lookupBool "touched" sj <*> lookupBool "nudged" sj)
    <*> lookupNum "start" j
    <*> pure (case j .: "view" of
                v | Just b <- int "bound" v, Just t <- lookupText "task" v -> Just (ViewCtx b [ round x | Just x <- map num (lookupArr "skip" v) ] t)
                _ -> Nothing)
  where
    int k x = round <$> lookupNum k x
    readFrom r = ReadRec <$> int "num" r <*> int "idx" r <*> lookupStr "path" r <*> int "lo" r <*> int "hi" r <*> lookupText "text" r
                         <*> pure (int "by" r) <*> lookupBool "trimmed" r

-- | Where a restarted chat finds what it goes on with.
resumeFile :: Chat -> FilePath
resumeFile ch = cStateDir (cConf ch) </> cName ch </> "chat-resume.json"

-- | Run again as the executable on disk now -- a harness upgraded, the flow kept: the turn where it is (if
-- one is running), the lines typed and not yet taken, the waits learned, written out for --resume. The
-- process stays the same (its pid, its output, its standard input). If it cannot, it goes on as it was.
restart :: Chat -> TQueue (Maybe T.Text) -> Maybe TurnState -> IO ()
restart ch pending mts = restartWith ch pending mts []

-- | The same, with more for the program it becomes to read (a turn through the claude command: 'CliRun').
restartWith :: Chat -> TQueue (Maybe T.Text) -> Maybe TurnState -> [(String, Json)] -> IO ()
restartWith ch pending mts extra = do
  writeIORef (cRestart ch) False
  typed <- drain pending
  longest <- readIORef (cLongest ch)
  pend <- readIORef (cPending ch)
  B.writeFile (resumeFile ch) (encodeBS (JObj ([ ("turn", maybe JNull turnJson mts), ("typed", JArr (map JText typed))
                                               , ("longest", JNum longest), ("pending", maybe JNull JNum pend) ] ++ extra)))
  uiNote (cUi ch) ("[harness: restarting as " ++ cExe ch ++ maybe "" (\ts -> printf "; the turn goes on at step %d" (tsStep ts)) mts ++ (if null extra then "" else "; the turn goes on, with the claude program it is in") ++ "]")
  uiLeave (cUi ch)
  r <- try (executeFile (cExe ch) False (dropResume (cArgv ch) ++ ["--resume", resumeFile ch]) Nothing)
  case r of
    Left (err :: IOException) -> do
      uiNote (cUi ch) ("[harness: the restart failed (" ++ show err ++ "); going on as before]")
      atomically (mapM_ (unGetTQueue pending . Just) (reverse typed))
    Right () -> pure ()
  where dropResume ("--resume" : _ : r) = dropResume r
        dropResume (a : r) = a : dropResume r
        dropResume [] = []

-- | A turn from where it is, until it ends.
goOn :: Chat -> Endpoint -> Opts -> TQueue (Maybe T.Text) -> TurnState -> IO ()
goOn ch e o pending ts = do
  writeIORef (cInTurn ch) True
  spent <- newIORef (tsSpent ts)
  readsR <- newIORef (tsReads ts)
  vcR <- newIORef (tsView ts)
  let loop readsR spent msgs0 step cut failures
        | step >= oMaxSteps o = uiNote (cUi ch) "[the turn reached its step limit; stopping]"
        | otherwise = do
            -- in --context view the call is built from the log; msgs is then its head only
            vc0 <- readIORef vcR
            (msgs, callMsgs) <- case vc0 of
              Nothing -> pure (msgs0, msgs0)
              Just vc -> do
                (hd, vc', c) <- viewCall ch o msgs0 vc
                writeIORef vcR (Just vc')
                pure (hd, c)
            step1 readsR spent msgs callMsgs step cut failures
      step1 = stepWith loop vcR
  loop readsR spent (tsMsgs ts) (tsStep ts) (tsCut ts) (0 :: Int)
  writeIORef (cInTurn ch) False
  tEnd <- now
  s <- readIORef spent
  uiSpent (cUi ch) s (tEnd - tsStart ts)
  noteSpent ch s (tEnd - tsStart ts)
  where
    byName = [ (tName t, t) | t <- toolsFor ch ]
    toolsJson = map toolJson (toolsFor ch)
    viewMode = isJust (tsView ts)
    -- a harness note: a message of the conversation, or in --context view a line of the log
    nudge msgs' text
      | viewMode = logH ch "echo" (T.pack "harness: " <> text) >> pure msgs'
      | otherwise = pure (msgs' ++ [msg "user" (T.pack "[harness: " <> text <> T.pack "]")])
    stepWith loop vcR readsR spent msgs callMsgs step cut _failures = do
          checkStop ch
          -- a restart asked for: here, between two model calls, nothing is half done
          asked <- readIORef (cRestart ch)
          when asked $ do
            s <- readIORef spent
            rs <- readIORef readsR
            vc <- readIORef vcR
            restart ch pending (Just (TurnState msgs step cut rs s (tsStart ts) vc))
          t0 <- now
          sp0 <- readIORef spent
          uiBusy (cUi ch) (Just (printf "model call %d of the turn (step %d; %s tokens in so far, %s out)" (sCalls sp0 + 1) (step + 1) (human (sIn sp0)) (human (sOut sp0))))
          let (think, effort) = case oEffort o of
                Just "none" -> (Just False, Nothing)
                Just ef     -> (Just True, Just ef)
                Nothing     -> (Nothing, Nothing)
          -- (asked again while the service is busy, the wait said; what asking again cannot mend ends the turn)
          -- (where the reply comes in pieces, what it is doing is said as it comes -- four times a second at most)
          seenR <- newIORef (0 :: Int, 0 :: Double)
          let live kind piece = do
                (n, at) <- readIORef seenR
                t <- now
                let n' = n + T.length piece
                    doing = case kind of { "mind" -> "thinking"; "tool" -> "calling a tool"; _ -> "writing" } :: String
                if t - at < 0.25 then writeIORef seenR (n', at) else do
                  writeIORef seenR (n', t)
                  uiBusy (cUi ch) (Just (printf "model call %d of the turn (step %d): %s, %s characters so far" (sCalls sp0 + 1) (step + 1) doing (human n')))
          -- (the images the messages name go with them, to a model that takes them)
          sent <- if eProvider e == OpenAI then pure callMsgs else Img.attach (imagesDir ch) callMsgs
          r <- requestWith (\w -> uiNote (cUi ch) ("[" ++ w ++ "]")) 8 e (Request sent toolsJson (oMaxTokens o) Nothing think effort 900 (Just live))
          t1 <- now
          case r of
            Left why -> uiNote (cUi ch) ("chat: " ++ why ++ "; the turn ends")
            Right p -> do
              forM_ (pUsage p) $ \u -> do
                spend spent u
                recordUsage (usageFile ch) (maybe "chat" (const "agent") (cSub ch)) e u (t1 - t0)
                when (oUsage o) (uiNote (cUi ch) (printf "[usage: in %d, out %d%s, %.1fs]" (uIn u) (uOut u) (maybe "" (\c -> ", cached " ++ show c) (uCached u)) (t1 - t0)))
              unless (T.null (T.strip (pReasoning p))) (uiThought (cUi ch) (T.strip (pReasoning p)))
              -- (what the endpoint's own tools did -- a web search -- is shown and logged as a tool's doing is)
              forM_ (pServed p) $ \(kind, text) -> do
                if kind == "tool" then uiCall (cUi ch) "(by the API)" (T.unpack text) else uiAnswer (cUi ch) (shownCut (cShow ch) text)
                logH ch kind text
              let content = T.strip (pContent p)
              unless (T.null content) (uiTalk (cUi ch) content >> logH ch "talk" content)
              let assistant = JObj ([("role", JStr "assistant"), ("content", JText (pContent p))] ++ [ ("tool_calls", JArr (map tcRaw (pToolCalls p))) | not (null (pToolCalls p)) ] ++ pKeep p)
                  msgs' = if viewMode then msgs else msgs ++ [assistant]
              if null (pToolCalls p)
                -- (the API stopped in the middle of its own tools' work: the reply goes back as it is, and it goes on)
                then if pFinish p == "pause_turn" && not viewMode && cut < 8 then loop readsR spent msgs' (step + 1) (cut + 1) 0
                else if pFinish p == "length" && cut < 3
                  then do
                    -- cut off at the output limit (most often: the thinking ran on) is not the end of the turn
                    uiNote (cUi ch) (printf "[the reply was cut off at %d tokens; asking it to go on in smaller steps]" (oMaxTokens o))
                    m' <- nudge msgs' (T.pack (printf "your reply was cut off at the output limit of %d tokens before any tool call -- the reasoning ran too long. Go on in smaller steps: act with a tool (eval to count or check, write a smaller piece) instead of working it all out first." (oMaxTokens o)))
                    loop readsR spent m' (step + 1) (cut + 1) 0
                  else do
                    -- a turn that changed the files and ends with the verdict red is told so, once: it
                    -- either fixes it or says plainly that it stops red, never ends there in silence
                    s <- readIORef spent
                    when (sTouched s && not (sNudged s)) (awaitPending ch)
                    v <- if sTouched s && not (sNudged s) then verdictAt ch else pure Nothing
                    case v of
                      Just r | isRed (vLine r) -> do
                        modifyIORef' spent (\x -> x { sNudged = True })
                        uiNote (cUi ch) "[the turn would end with the verdict red; saying so once]"
                        m' <- nudge msgs' (T.pack ("you are ending the turn with the session's verdict red: " ++ vLine r ++ concatMap ("\n" ++) (take 8 (vBehind r))
                                                    ++ "\nFix it, or end by saying plainly that it is red and why you are stopping."))
                        loop readsR spent m' (step + 1) 0 0
                      _ -> pure ()
                else do
                  modifyIORef' spent (\s -> s { sTools = sTools s + length (pToolCalls p), sTouched = sTouched s || any ((`elem` ["write", "edit", "edits", "sh"]) . tcName) (pToolCalls p) })
                  pre <- runBatches ch byName (pToolCalls p)      -- (the writes that come together in a reply: made at once, reloaded once)
                  replies <- forM (zip [0 :: Int ..] (pToolCalls p)) $ \(i, tc) -> do
                    let name = tcName tc
                        shownArgs = take 300 (encode (tcArgs tc))
                    -- every call is logged by the chat as the agent made it, and its answer as the agent saw it (the
                    -- session's own tools are asked quietly); remember writes the log itself
                    unless (M.member i pre) $ do      -- (a batched call was shown and logged when the batch began)
                      uiCall (cUi ch) name shownArgs
                      unless (name == "remember") (logH ch "tool" (T.pack (name ++ " " ++ encode (tcArgs tc))))
                    checkStop ch
                    uiBusy (cUi ch) (Just ("running " ++ name ++ (if length (pToolCalls p) > 1 then printf " (%d of %d)" (i + 1) (length (pToolCalls p)) else "")))
                    t2 <- now
                    (ok, out0) <- case (M.lookup i pre, lookup name byName) of
                      (Just r, _) -> pure r
                      (_, Just t) -> runTool ch t (tcArgs tc)
                      _ -> pure (False, T.pack ("unknown tool " ++ show name))
                    late <- pendingNote ch
                    t3 <- now
                    -- a read: numbered, an alias of an earlier one when its text is that one's, else
                    -- superseding the earlier reads of its lines
                    out1 <- case (if viewMode then "" else name, ok, readRange out0, lookup name byName) of
                      ("read", True, Just (lo, hi), Just t) -> do
                        let path = normalise (fromMaybe "" (lookupStr "path" (fst (arguments t (tcArgs tc)))))
                            idx = length msgs' + i
                        recs <- readIORef readsR
                        let k = length recs + 1
                        case readAgainst recs path (lo, hi) out0 of
                          Left j -> do
                            writeIORef readsR (recs ++ [ReadRec k idx path lo hi T.empty Nothing True])
                            pure (T.pack (printf "[read #%d: %s lines %d-%d are exactly as read #%d shows them above (unchanged since)]" k path lo hi j))
                          Right sup -> do
                            writeIORef readsR ([ if rrNum r `elem` sup then r { rrBy = Just k } else r | r <- recs ] ++ [ReadRec k idx path lo hi out0 Nothing False])
                            pure (T.pack (printf "[read #%d: %s, lines %d-%d]\n" k path lo hi) <> cap out0)
                      _ -> pure (cap out0)
                    let out = out1 <> late
                        tagged = (if ok then T.empty else T.pack "ERROR: ") <> out
                    when (oUsage o) (uiNote (cUi ch) (printf "[tool: %s %.1fs]" name (t3 - t2)))
                    unless (name == "remember") (logH ch "echo" tagged)
                    uiAnswer (cUi ch) (shownCut (cShow ch) out)
                    pure (JObj [("role", JStr "tool"), ("tool_call_id", JStr (tcId tc)), ("content", JText tagged)])
                  mid <- drain pending >>= typedLines ch
                  forM_ mid (logTyped ch)
                  fedNote ch mid
                  let next = if viewMode then msgs' else msgs' ++ replies ++ [ msg "user" (T.intercalate (T.pack "\n\n") mid) | not (null mid) ]
                  -- the superseded reads, rewritten as stubs once there is enough of them
                  recs <- readIORef readsR
                  let due = [ r | r <- recs, isJust (rrBy r), not (rrTrimmed r) ]
                      dueChars = sum (map (T.length . rrText) due)
                  limit <- trimAtNow
                  -- (never where the conversation is only to be appended to: 'appendOnly')
                  next' <- if viewMode || null due || dueChars < limit || appendOnly e then pure next else do
                    let stubs = M.fromList [ (rrIdx r, T.pack (printf "[read #%d: %s lines %d-%d -- superseded by read #%d, which shows these lines as they are now]" (rrNum r) (rrPath r) (rrLo r) (rrHi r) (fromMaybe 0 (rrBy r)))) | r <- due ]
                    writeIORef readsR [ if rrNum r `elem` map rrNum due then r { rrTrimmed = True, rrText = T.empty } else r | r <- recs ]
                    uiNote (cUi ch) (printf "[context: %d superseded read(s) trimmed, %d characters]" (length due) dueChars)
                    pure [ maybe m (\st -> set "content" (JText st) m) (M.lookup ix stubs) | (ix, m) <- zip [0 ..] next ]
                  loop readsR spent next' (step + 1) 0 0

-- the call built from the log (--context view) -----------------------------------------------
--
-- UniiChat starts each TURN fresh, from the view, and carries the turn's own steps as a conversation: a turn
-- of a hundred tool calls grew to 195k tokens and never met its own memory. Here every CALL is built from
-- the log: the view up to a boundary, the turn's message, and the log after the boundary word for word
-- (<recent>). Once <recent> is over 'tailMax' bytes, the boundary moves on to leave a third of it, and the
-- view is taken again up to there -- in a batch, as the reads are trimmed, because the head of the call
-- changes then and the provider's cache of it ends: between two moves each call is the last one and a bit
-- more. A tail of 0 is the view alone, each call waiting for the compactor to summarize the step before.

-- | Bytes of the log after the boundary that make the boundary move (a third of it stays).
tailMax :: Int
tailMax = 96000

-- | Bytes of the turn's plan and directives kept in <plan> when the boundary moves.
planMax :: Int
planMax = 32000

-- | The first @n@ bytes, without splitting a character.
cutBytes :: Int -> T.Text -> T.Text
cutBytes n = T.dropWhileEnd (== '\xFFFD') . TE.decodeUtf8With TE.lenientDecode . B.take n . TE.encodeUtf8

-- | Format a log message as `id|kind: text\n`.
-- | The turn's plan and directives before the boundary: talk and user messages up to the byte budget.
-- If they exceed the budget, the initial message (the plan) is kept and subsequent messages are kept
-- from the tail (the most recent updates).
renderPlan :: Int -> [Int] -> [(Int, T.Text, T.Text)] -> T.Text
renderPlan maxBytes skip ms
  | maxBytes <= 0 = T.empty
  | null relevant = T.empty
  | totalBytes <= maxBytes = T.concat (map formatMsg relevant)
  | otherwise = case relevant of
      [] -> T.empty
      (m0 : more) ->
        let b0 = byteLen (formatMsg m0)
        in if b0 >= maxBytes
             then cutBytes maxBytes (formatMsg m0)
             else let restTail = takeTail (maxBytes - b0) more
                  in T.concat (map formatMsg (m0 : restTail))
  where
    relevant = [ m | m@(i, k, _) <- ms, i `notElem` skip, k `elem` [T.pack "talk", T.pack "user"] ]
    totalBytes = sum (map (byteLen . formatMsg) relevant)
    byteLen = B.length . TE.encodeUtf8
    takeTail budget xs = go (reverse xs) budget []
      where
        go [] _ acc = acc
        go (y : ys) b acc
          | byteLen (formatMsg y) <= b = go ys (b - byteLen (formatMsg y)) (y : acc)
          | otherwise                  = acc

-- | The log after the boundary as <recent> shows it: one message after another, whole, `id|kind: text`.
renderTail :: [Int] -> [(Int, T.Text, T.Text)] -> T.Text
renderTail skip ms = T.concat [ formatMsg m | m@(i, _, _) <- ms, i `notElem` skip ]

-- | Where the boundary moves to: past the oldest messages, until what is left is within the bytes
-- (the id after the last message when nothing is).
newBound :: Int -> [Int] -> [(Int, T.Text, T.Text)] -> Int
newBound keep skip ms = go ms
  where go [] = maybe 0 (\(i, _, _) -> i + 1) (listToMaybe (reverse ms))
        go rest@((i, _, _) : more) | byteLen (renderTail skip rest) <= keep = i
                                   | otherwise = go more
        byteLen = B.length . TE.encodeUtf8

-- | The view's lines, each with its first message.
viewLines :: T.Text -> [(Int, T.Text)]
viewLines v = [ (n, l) | l <- T.lines v, (d, rest) <- [T.span (`elem` ['0' .. '9']) l], not (T.null d), T.pack "+" `T.isPrefixOf` rest, [(n, "")] <- [reads (T.unpack d)] ]

-- | The log from message b on.
logFrom :: Chat -> Int -> IO [(Int, T.Text, T.Text)]
logFrom ch b = do
  r <- ask ch "history" [("since", JNum (fromIntegral b)), ("n", JNum 1000000), ("json", JBool True)]
  pure $ case parseJsonBS (TE.encodeUtf8 (fromMaybe T.empty (lookupText "out" r))) of
    Right (JArr xs) -> [ (round i, k, t) | x <- xs, Just i <- [lookupNum "i" x], Just k <- [lookupText "kind" x], Just t <- [lookupText "text" x] ]
    _ -> []

-- | The log between messages a and b (exclusive of b).
logRange :: Chat -> Int -> Int -> IO [(Int, T.Text, T.Text)]
logRange ch a b
  | b <= a = pure []
  | otherwise = do
      r <- ask ch "history" [("since", JNum (fromIntegral a)), ("n", JNum (fromIntegral (b - a))), ("json", JBool True)]
      pure $ case parseJsonBS (TE.encodeUtf8 (fromMaybe T.empty (lookupText "out" r))) of
        Right (JArr xs) -> [ (round i, k, t) | x <- xs, Just i <- [lookupNum "i" x], Just k <- [lookupText "kind" x], Just t <- [lookupText "text" x] ]
        _ -> []

-- | The view of the messages before b, once its lines are summaries (at most the settle's seconds).
viewBefore :: Chat -> Int -> Double -> IO T.Text
viewBefore ch b secs = do
  t0 <- now
  let go = do
        (v, _, _, _) <- view ch 0
        let keep = [ l | (start, l) <- viewLines v, start < b ]
            unbuilt = any (T.isSuffixOf placeholderText) keep
        t <- now
        if unbuilt && t - t0 < secs then threadDelay 300000 >> go else do
          when unbuilt (uiNote (cUi ch) "[view: not all lines summarized yet; going on]")
          pure (T.unlines (T.pack "<chat>" : keep ++ [T.pack "</chat>"]))
  go
  where placeholderText = T.pack "(not summarized yet: zoom it)"

-- | This call's messages in --context view: the head, moved on first if <recent> has grown past the tail,
-- and <recent> after the message. (The head, and where the boundary is now.)
viewCall :: Chat -> Opts -> [Json] -> ViewCtx -> IO ([Json], ViewCtx, [Json])
viewCall ch o hd vc = do
  ms <- logFrom ch (vcBound vc)
  let size = B.length (TE.encodeUtf8 (renderTail (vcSkip vc) ms))
  (hd', vc', ms') <-
    if size <= oTail o || null ms then pure (hd, vc, ms) else do
      let b = newBound (oTail o `div` 3) (vcSkip vc) ms
      v <- viewBefore ch b (oSettle o)
      uiNote (cUi ch) (printf "[context: the view now runs to message %d; <recent> is %d messages]" b (length (filter (\(i, _, _) -> i >= b) ms)))
      pure (take 1 hd ++ [msg "user" (v <> T.pack "\n" <> vcTask vc)], vc { vcBound = b }, filter (\(i, _, _) -> i >= b) ms)
  let start = maybe (vcBound vc') (+ 1) (listToMaybe (reverse (vcSkip vc')))
      begun = vcBound vc' > start
  priorMs <- if begun then logRange ch start (vcBound vc') else pure []
  let plan = renderPlan (oPlan o) (vcSkip vc') priorMs
      recent = renderTail (vcSkip vc') ms'
      planBlock = if T.null plan then T.empty else T.pack "\n<plan>\n" <> plan <> T.pack "</plan>\n"
      turnNote = T.pack (if begun
        then printf "[Your work on this message so far is the log from message %d on: %slines %d.. of the view above, summarized (zoom them for the whole text), then <recent>. Go on from where it ends; do not start again.]"
                    start (if T.null plan then "" else "your plan is in <plan> above, ") start
        else printf "[Your work on this message so far is the log from message %d on, in <recent>. Go on from where it ends; do not start again.]" start)
      call | T.null recent && not begun = hd'
           | otherwise = init hd' ++ [msg "user" (fromMaybe T.empty (lookupText "content" (last hd')) <> T.pack "\n"
                                                  <> (if T.null plan then T.pack "\n" else planBlock <> T.pack "\n") <> turnNote
                                                  <> (if T.null recent then T.empty else T.pack "\n<recent>\n" <> recent <> T.pack "</recent>"))]
  pure (hd', vc', call)

-- the reads a turn's context holds ---------------------------------------------------------------
--
-- A turn's conversation grows with every tool result, and most of it is files read: 17 reads in the first 42
-- calls of one round, which then carried 128k tokens into every call. A file read again holds, in the new
-- read, everything the older read of those lines held -- the older copy is superseded. And a file read again
-- unchanged is the very text the context already has. So:
--
-- * every read's answer is numbered ("[read #3: src/A.hs, lines 1-200]");
-- * a read whose lines and text are those of a read the context still holds answers with a pointer to it
--   ("[read #7: src/A.hs lines 1-200 are exactly as read #3 shows them above]") -- an alias, no text;
-- * a read supersedes the earlier reads of the same file whose lines lie within its own; once the superseded
--   text adds up to 'trimAt' characters, those messages are rewritten as one-line stubs naming the read that
--   holds their lines now. In a batch: rewriting a message ends the provider's cache of the prompt from that
--   message on, so it is done seldom, for a large gain each time, not on every read.

-- | A read in the context: its number, the message it is, the file, its first and last line, its text (none
-- for an alias), the read that superseded it, and whether its message has been rewritten as a stub.
data ReadRec = ReadRec { rrNum :: Int, rrIdx :: Int, rrPath :: FilePath, rrLo :: Int, rrHi :: Int, rrText :: T.Text, rrBy :: Maybe Int, rrTrimmed :: Bool }
  deriving (Eq, Show)

-- | Characters of superseded reads that make the stubs worth a broken cache (GHS_CHAT_TRIM_AT overrides it,
-- to tune it against a round's tokens; 0 trims at every superseding read, a huge number never).
trimAt :: Int
trimAt = 40000

trimAtNow :: IO Int
trimAtNow = (\v -> case v >>= \x -> case reads x of { [(n, "")] -> Just n; _ -> Nothing } of { Just n -> n; Nothing -> trimAt }) <$> lookupEnv "GHS_CHAT_TRIM_AT"

-- | The first and last line a read's answer shows ("   12│text" lines, 'numbered').
readRange :: T.Text -> Maybe (Int, Int)
readRange out = case [ n | l <- T.lines out, (pre, rest) <- [T.break (== numberBar) l], not (T.null rest), [(n, "")] <- [reads (T.unpack (T.strip (T.dropWhile (== '>') (T.stripStart pre)))) :: [(Int, String)]] ] of
  [] -> Nothing
  ns -> Just (head ns, last ns)

-- | A new read against the ones the context holds: 'Left' the read it is an alias of (the same lines, the
-- same text, still whole in the context), or 'Right' the reads it supersedes (the same file, lines within its own).
readAgainst :: [ReadRec] -> FilePath -> (Int, Int) -> T.Text -> Either Int [Int]
readAgainst recs p (lo, hi) t =
  case [ rrNum r | r <- recs, whole r, rrPath r == p, rrLo r == lo, rrHi r == hi, rrText r == t ] of
    (j : _) -> Left j
    [] -> Right [ rrNum r | r <- recs, whole r, rrPath r == p, rrLo r >= lo, rrHi r <= hi ]
  where whole r = isNothing (rrBy r) && not (rrTrimmed r) && not (T.null (rrText r))

-- | The lines typed since last asked. (Taking the end of the input with them left a chat fed from a pipe
-- waiting for ever after its last turn.)
drain :: TQueue (Maybe T.Text) -> IO [T.Text]
drain q = atomically go
  where go = do
          m <- tryReadTQueue q
          case m of
            Just (Just l) -> (l :) <$> go
            -- (the end of the input is not a line: it is left for the one who waits for it)
            Just Nothing -> unGetTQueue q Nothing >> pure []
            Nothing -> pure []

-- | @chat --carry N@: what the fresh calls after the last N rollovers were given, part by part, in bytes -- the view as
-- it is now, cut at where each log began (the summaries it was then differ a little), the log as it was.
carryReport :: Chat -> String -> Opts -> IO Int
carryReport ch system o = do
  ms <- logFrom ch 0
  (full, _, _, _) <- view ch 0
  let echoes = reverse (take (oCarry o) (reverse (rollEchoes ms)))
      toolsB = B.length (encodeBS (JArr (map toolJson (toolsFor ch))))
      sysB = B.length (TE.encodeUtf8 (T.pack (system ++ continueDoc)))
  cols <- forM echoes $ \r -> do
    let turn = maybe [] (\t -> [ m | m@(i, _, _) <- ms, i >= t, i < r ]) (taskBefore r ms)
    case carrySplit (max 12000 (oTail o `div` 3)) turn of
      Nothing -> pure Nothing
      Just sp -> do
        v <- viewBefore ch (spFrom sp) 0
        let note = carryNote "its context had grown to 150k tokens, so it goes on here in a fresh call"
            (tid, _, _) = spTask sp
        -- (GHS_CARRY_DUMP=DIR: the message too, as the call is given it, to DIR/rNNN.txt -- to be counted by the model's own tokenizer)
        dump <- lookupEnv "GHS_CARRY_DUMP"
        let files = carryFiles turn
            parts = carryParts sysB toolsB full v sp note
        forM_ dump $ \d -> TIO.writeFile (d </> ("r" ++ show r ++ ".txt")) (carryFirst v sp files note)
        pure (Just ("#" ++ show r ++ " (turn " ++ show tid ++ ")", init parts ++ [Part "files" (B.length (TE.encodeUtf8 files)) (length (touches turn)), last parts], Nothing))
  putStr (partsTable (catMaybes cols))
  pure 0

-- | @ghci-session chat ARGS@.
chatMain :: Conf -> [String] -> IO Int
chatMain _ ("--replay-rollover" : files) = replayMain files
chatMain conf args = case parseOpts args of
  Left why -> hPutStrLn stderr why >> pure 2
  Right o -> do
    picked <- pick conf (oSession o)
    case picked of
      Left why -> hPutStrLn stderr ("chat: " ++ why) >> pure 2
      Right name | oRestart o -> askRestart conf name
      Right name | Just w <- oWait o -> Inbox.waitMain conf name (oSend o) w
      Right name | Just m <- oSend o -> Inbox.send conf name m >>= \r -> case r of
        Left why -> hPutStrLn stderr ("chat: " ++ why) >> pure 1
        Right pid -> putStrLn ("left for the chat on " ++ name ++ " (pid " ++ show pid ++ "): it reads it between two tool calls, or as its next turn") >> pure 0
      Right name -> do
        cfg <- resolve conf name
        members <- readMembers conf name
        longest <- newIORef 0
        pendingCheck <- newIORef Nothing
        down <- newIORef False
        restartR <- newIORef False
        turnR <- newIORef Nothing
        agentsR <- newIORef M.empty
        startR <- newIORef Nothing
        stoppedR <- newIORef False
        inTurn <- newIORef False
        batchR <- newIORef Nothing
        rollR <- loadRoll (rollFile conf name) (rolloverRatioSet conf name) >>= newIORef
        exe <- getExecutablePath
        argv <- getArgs
        let ms = if null members then [name] else members
            chatWith ui = Chat conf name (cRoot conf) (map normalise (concat [ strs (targetJson conf m .: "watch") | m <- ms ])) (either (const "Agent") gAgent cfg)
                               longest pendingCheck down restartR inTurn Nothing agentsR startR turnR stoppedR (stripDeleted exe) argv batchR ui (oShow o) rollR
        if oPrintView o then view (chatWith stdoutUi) 0 >>= \(v, _, _, _) -> TIO.putStrLn v >> pure 0 else do
          -- (--web N: the model may search the web, where the endpoint has it -- "GhciSession.Llm" reads this)
          forM_ (oWeb o) (setEnv "GHS_WEB_SEARCH" . show)
          ep <- endpointFromEnv
          case ep of
            Left why -> hPutStrLn stderr ("chat: " ++ why) >> pure 2
            Right e0 -> do
              let e = e0 { eModel = fromMaybe (eModel e0) (oModel o), eBase = maybe (eBase e0) (reverse . dropWhile (== '/') . reverse) (oBase o) }
                  agent = cAgent (chatWith stdoutUi)
              instr <- maybe (pure "") (\f -> trim <$> readFile f) (oInstructions o)
              let system = T.unpack ((if either (const False) gSharedPrompt cfg then H.systemPrompt else H.turnPrompt) agent) ++ "\n" ++ master ++ (if oView o then recentDoc agent else "") ++ (if null instr then "" else "\n" ++ instr)
                  dropPid = void (try (removeFile (chatPidFile conf name)) :: IO (Either IOException ()))
                  -- the chat on a Ui: the lines to take come on the queue (standard input's, or the screen's)
                  run ui pending = do
                    let ch = chatWith ui
                    writeIORef mainSystem system
                    uiOnStop ui (stopTurn ch)
                    writeIORef startR (Just (subRunner ch e o instr pending))
                    -- kill -HUP (chat --restart): in a turn, at its next model call; between turns, at once
                    getProcessID >>= writeFile (chatPidFile conf name) . show
                    -- (a line left for it from elsewhere -- the monitor, chat --send -- is a line typed)
                    Inbox.watch conf name (\l -> uiNote ui ("[sent to this chat: " ++ T.unpack l ++ "]") >> atomically (writeTQueue pending (Just l)))
                    void $ installHandler sigHUP (Catch $ do
                      writeIORef restartR True
                      busy <- readIORef inTurn
                      if busy || isJust (oOnce o) then uiNote ui "[harness: a restart is asked; it happens before the next model call]"
                        else restart ch pending Nothing) Nothing
                    resumed <- maybe (pure Nothing) (fmap Just . resumeFrom ch) (oResume o)
                    case resumed of
                      Nothing -> do
                        (v, _, parts, messages) <- view ch 0
                        uiView ui v
                        uiNote ui (printf "[%s: %d messages, %d lines; model %s]" name messages parts (eModel e))
                      Just _ -> uiNote ui (printf "[%s: the harness restarted as %s; model %s]" name (cExe ch) (eModel e))
                    flip finally dropPid $ do
                      -- a turn the restart was in goes on; lines typed before it and not yet taken are a turn of their own
                      case resumed of
                        Just (mts, typed, mcli) -> do
                          -- (a turn through the claude command, handed over with the program it is in)
                          forM_ mcli $ \cr -> do
                            uiNote ui "[harness: the turn goes on, with the claude program it was in]"
                            atomically (mapM_ (writeTQueue pending . Just) typed)
                            stoppable ch (runCli ch e o pending cr)
                          forM_ mts $ \ts -> do
                            uiNote ui (printf "[harness: the turn goes on at step %d]" (tsStep ts))
                            forM_ typed (logTyped ch)
                            stoppable ch (goOn ch e o pending ts { tsMsgs = tsMsgs ts ++ [ msg "user" (T.intercalate (T.pack "\n\n") typed) | not (null typed) ] })
                          when (isNothing mts && isNothing mcli && not (null typed)) (stoppable ch (turn ch e o system typed pending))
                        -- (--continue: the last turn of the history is gone on with, from its log)
                        Nothing -> when (oContinue o) (stoppable ch (continueTurn ch e o system pending Nothing))
                      case (oOnce o, resumed) of
                        (Just _, Just _) -> pure 0
                        (Just m, Nothing) -> typedLines ch [T.pack m] >>= \ls -> stoppable ch (turn ch e o system ls pending) >> pure 0
                        (Nothing, _) -> do
                          let loop = do
                                uiBusy ui Nothing
                                -- (a stop that comes as its turn ends finds no turn: it is nothing, here)
                                let waitLine = atomically (readTQueue pending) `catch` \StopTurn -> waitLine
                                first <- waitLine
                                case first of
                                  Nothing -> pure 0
                                  Just l -> do
                                    more <- drain pending
                                    texts <- typedLines ch (filter (not . T.null . T.strip) (l : more))
                                    unless (null texts) (writeIORef inTurn True >> stoppable ch (turn ch e o system texts pending) >> writeIORef inTurn False)
                                    loop
                          loop
              if oCarry o > 0 then carryReport (chatWith stdoutUi) system o else if oTui o
                then chatTui name (eModel e) (cStateDir conf </> name) run `finally` dropPid
                else do
                  pending <- newTQueueIO
                  hSetBuffering stdout LineBuffering
                  -- (unbuffered: a line read ahead into a buffer would be lost to a restart)
                  hSetBuffering stdin NoBuffering
                  void $ forkIO $ do
                    let reader = do
                          eof <- hIsEOF stdin
                          if eof then atomically (writeTQueue pending Nothing) else do
                            l <- TIO.getLine
                            atomically (writeTQueue pending (Just l))
                            reader
                    reader
                  run stdoutUi pending
  where stripDeleted p = maybe p reverse (stripPrefix' (reverse " (deleted)") (reverse p))
        stripPrefix' pre x = if pre `isPrefixOf` x then Just (drop (length pre) x) else Nothing

-- | What a restart left: the turn it was in, if any, and the lines typed and not yet taken; the waits it had
-- learned are the chat's again. (The file is removed: a second start does not go on with it again.)
resumeFrom :: Chat -> FilePath -> IO (Maybe TurnState, [T.Text], Maybe CliRun)
resumeFrom ch f = do
  b <- B.readFile f
  void (try (removeFile f) :: IO (Either IOException ()))
  case parseJsonBS b of
    Left why -> hPutStrLn stderr ("[harness: cannot read " ++ f ++ ": " ++ why ++ "]") >> pure (Nothing, [], Nothing)
    Right j -> do
      forM_ (lookupNum "longest" j) (writeIORef (cLongest ch))
      writeIORef (cPending ch) (lookupNum "pending" j)
      pure (turnFrom (j .: "turn"), [ t | Just t <- map txt (lookupArr "typed" j) ], cliFrom (j .: "cli"))

-- | @chat --restart@: the session's running chat is asked to restart as the executable on disk now.
askRestart :: Conf -> String -> IO Int
askRestart conf name = do
  t <- try (readFile (chatPidFile conf name)) :: IO (Either IOException String)
  case reads (either (const "") id t) of
    [(pid, _)] -> do
      alive <- try (signalProcess nullSignal pid) :: IO (Either IOException ())
      case alive of
        Left _ -> hPutStrLn stderr ("chat: no chat running on " ++ name ++ " (pid " ++ show pid ++ " is gone)") >> pure 1
        Right () -> do
          signalProcess sigHUP pid
          putStrLn ("asked the chat on " ++ name ++ " (pid " ++ show pid ++ ") to restart: in a turn, before its next model call (through the claude command: once the tool call under way is answered); between turns, at once")
          pure 0
    _ -> hPutStrLn stderr ("chat: no chat running on " ++ name) >> pure 1

-- the compactor ------------------------------------------------------------------------------

-- | @ghci-session summarize@, for @summarize_cmd@: one summary line from the prompt the daemon writes on
-- standard input -- the instructions, then the context (@<chat>@ ... @</chat>@), then the step. Everything
-- before the first @<chat>@ line is sent as the system prompt and the rest as the user message, so the
-- context -- the same prefix for every node of a stretch -- is cached by the provider. The answer is the
-- model's first non-empty line.
--
-- It is asked WITHOUT thinking. A reasoning model spends the answer's budget on its thoughts first, and
-- at the flash model's default effort a summary's budget (400 tokens) was most often all thoughts and no
-- line: the call ended on @length@ with an empty answer, the daemon logged a failed node and asked again
-- ten seconds later, and each try was paid for. A summary is a compression, not a problem.
-- @SUMMARIZE_EFFORT=low|high|max@ turns thinking on at that effort, with a budget to match; whatever the
-- setting, an answer cut off before any text is asked again, without thinking and with four times the
-- budget, before it is given up. @--dry@ says what would be sent.
summarizeMain :: [String] -> IO Int
summarizeMain args = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  prompt <- TIO.getContents
  let (system, user) = split prompt
  effort <- fromMaybe "none" <$> lookupEnv "SUMMARIZE_EFFORT"
  let think0 = effort /= "none"
      budget0 = if think0 then 4000 else 400 :: Int
  if "--dry" `elem` args
    then putStrLn (printf "system: %d bytes; user: %d bytes; thinking: %s; budget: %d" (T.length system) (T.length user) (if think0 then effort else "off") budget0) >> pure 0
    else do
      ep <- endpointFromEnv
      ledger <- lookupEnv usageFileEnv
      noTools <- lookupEnv "SUMMARIZE_TOOLS"      -- 0: send no tools (a local model that takes none)
      case ep of
        Left why -> hPutStrLn stderr ("summarize: " ++ why) >> pure 2
        Right e0 -> do
          -- (through the claude command a compaction may have a model of its own -- a small one: a subscription's
          -- limits are spent by every call, and there are many of these)
          small <- lookupEnv "GHS_CLAUDE_SUMMARIZE_MODEL"
          let e = case small of { Just m | eProvider e0 == ClaudeCli, not (null m) -> e0 { eModel = m }; _ -> e0 }
          -- the turns' tools go with the call, never called: a compaction is a call like a turn, with the
          -- same tools and system prompt, so it reads them from the turns' cache entry. A model that calls
          -- one all the same, and writes no line, is asked again without them.
          let go tries think budget withTools
                | tries >= (3 :: Int) = pure (Left "no line after three tries (cut off at the output limit, or a tool called)")
                | otherwise = do
                    t0 <- now
                    r <- request e (Request ([ JObj [("role", JStr "system"), ("content", JText system)] | not (T.null system) ] ++ [ JObj [("role", JStr "user"), ("content", JText user)] ])
                                            (if withTools then map toolJson chatTools else []) budget (Just 0.3) (if isDeepSeek e || eProvider e /= OpenAI then Just think else Nothing) (if think then Just effort else Nothing) 300 Nothing)
                    t1 <- now
                    forM_ ledger $ \f -> forM_ (either (const Nothing) pUsage r) $ \u -> recordUsage f "summarize" e u (t1 - t0)
                    case r of
                      -- (an endpoint whose model takes no tools refuses the request: once more without them)
                      Left why | withTools -> go (tries + 1) think budget False
                               | otherwise -> pure (Left why)
                      Right p | not (T.null (T.strip (pContent p))) -> pure (Right p)
                              | withTools && not (null (pToolCalls p)) -> go (tries + 1) think budget False
                              | pFinish p /= "length" -> pure (Right p)
                              -- cut off before any text: the budget went to thoughts -- ask again without them, with
                              -- room for a line and no more (four times a thinking budget was 16,000 tokens: seven
                              -- minutes of a local model's time for a line of 512 bytes)
                              | otherwise -> go (tries + 1) False 1600 withTools
          r <- go 0 think0 budget0 (noTools `notElem` map Just ["0", "no", "off"])
          case r of
            Left why -> hPutStrLn stderr ("summarize: " ++ why) >> pure 1
            -- (a code fence is not the line: an answer that is JSON -- "GhciSession.Know" -- may come inside one)
            Right p -> case filter (\l -> not (T.null l) && not (T.pack "```" `T.isPrefixOf` l)) (map T.strip (T.lines (pContent p))) of
              (l : _) -> TIO.putStrLn l >> pure 0
              [] -> hPutStrLn stderr ("summarize: the model answered nothing (finish_reason " ++ pFinish p ++ ")") >> pure 1
  where
    split prompt =
      let ls = T.lines prompt
      in case break ((== T.pack "<chat>") . T.strip) ls of
           (before, rest@(_ : _)) -> (T.strip (T.unlines before), T.strip (T.unlines rest))
           _ -> (T.empty, T.strip prompt)
