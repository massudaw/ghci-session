{-# LANGUAGE ScopedTypeVariables #-}
-- | __The endless chat__: @ghci-session chat@, an agent that works on a session as its sandbox and
-- remembers through the session's history (OptChat's turn loop, over the daemon's log, tree and view).
--
-- > ghci-session chat                      # from the project: the chat, on the default session
-- > ghci-session chat -s dev               # a session
-- > ghci-session chat --once 'what was tried on Raster.depth last week?'
-- > ghci-session chat --instructions AGENTS.md
--
-- Each message starts a FRESH model call: no conversation is carried over. The call sees the system prompt
-- ('master', 'viewDoc', the instructions file), then the view -- the whole history as one-line summaries,
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
  , arguments, chatTools, splitImports, nearest, fuzzyReplace, replaceOnce, editPaths, saveWait, isRed, ownGhci, capWith, shCap
  , ReadRec (..), readRange, readAgainst, trimAt
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, try)
import Data.IORef
import Control.Monad (forM, forM_, unless, void, when)
import qualified Data.ByteString as B
import Data.Char (isSpace)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, sort, tails)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe, isJust, isNothing)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, normalise, splitDirectories, takeDirectory, takeFileName, (</>))
import System.IO
import System.Process
import System.Timeout (timeout)
import Text.Printf (printf)

import GhciSession.Config
import GhciSession.Json
import GhciSession.Llm
import GhciSession.Mcp (Tool (..), pick, tools)
import qualified GhciSession.Mcp as Mcp
import GhciSession.Sys (now)

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
  , oMaxTokens :: Int, oMaxSteps :: Int, oSettle :: Double, oUsage :: Bool, oPrintView :: Bool }

chatUsage :: String
chatUsage = unlines
  [ "ghci-session chat [-s SESSION] [--once MESSAGE] [--instructions FILE] [--model M] [--base-url URL]"
  , "                  [--max-tokens N] [--max-steps N] [--settle SECS] [--usage] [--print-view]"
  , "  the endless chat with an agent on the session (DEEPSEEK_API_KEY or OPENAI_API_KEY); --once: one message, then exit;"
  , "  --instructions: a file of the user's own instructions (an AGENTS.md), appended to the system prompt;"
  , "  --max-steps: tool calls per turn (60); --settle: seconds to wait for the view's last lines to be summarized (120);"
  , "  --usage: print each call's tokens and seconds" ]

parseOpts :: [String] -> Either String Opts
parseOpts = go (Opts Nothing Nothing Nothing Nothing Nothing 8000 60 120 False False)
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
    go o (k : r) | k == "--usage" = go o { oUsage = True } r
                 | k == "--print-view" = go o { oPrintView = True } r
    go _ (k : _) = Left ("chat: unexpected argument " ++ show k ++ "\n" ++ chatUsage)

-- the session ------------------------------------------------------------------------------

data Chat = Chat { cConf :: Conf, cName :: String, cDir :: FilePath, cWatched :: [FilePath], cAgent :: String
                 , cLongest :: IORef Double     -- ^ the longest verdict (a reload with its check) seen: what a save may take
                 , cPending :: IORef (Maybe Double)     -- ^ a save whose CHECK was still running when its answer went out: when it was written
                 , cDown :: IORef Bool }                -- ^ the last wait for the session to come back ended with it still down

-- | The session's usage ledger: one line per model call, the chat's and the compactor's.
usageFile :: Chat -> FilePath
usageFile ch = cStateDir (cConf ch) </> cName ch </> "usage.jsonl"

-- | What a turn cost so far: model calls, tokens in, of them cached, tokens out, tool calls -- and whether it
-- has written to the files, and been told it was about to end red.
data Spent = Spent { sCalls :: !Int, sIn :: !Int, sCached :: !Int, sOut :: !Int, sTools :: !Int, sTouched :: !Bool, sNudged :: !Bool }

spend :: IORef Spent -> Usage -> IO ()
spend ref u = modifyIORef' ref (\s -> s { sCalls = sCalls s + 1, sIn = sIn s + uIn u, sCached = sCached s + fromMaybe 0 (uCached u), sOut = sOut s + uOut u })

-- | The turn's summary line: always said (stderr), whatever --usage.
spentLine :: Spent -> Double -> String
spentLine s secs = printf "[turn: %d model call%s, %s tokens in (%d%% cached), %s out, %d tool call%s, %.0fs]"
  (sCalls s) (plural (sCalls s)) (human (sIn s)) (if sIn s == 0 then 0 else (100 * sCached s) `div` sIn s :: Int) (human (sOut s)) (sTools s) (plural (sTools s)) secs
  where plural n = if n == 1 then "" else "s" :: String

-- | Is this path one the daemon watches (its targets' directories), or a build file? A save of one has a verdict.
watches :: Chat -> FilePath -> Bool
watches ch p0 = ".cabal" `isSuffixOf` p || takeFileName p == "cabal.project" || any (\d -> p == d || (d ++ "/") `isPrefixOf` p) (cWatched ch)
  where p = normalise p0

-- | A request to the daemon, as the command line sends it.
ask :: Chat -> String -> [(String, Json)] -> IO Json
ask ch op args = Mcp.request (cConf ch) (cName ch) (JObj (("op", JStr op) : args))

-- | A line of the history, on the user's or the agent's behalf.
logH :: Chat -> String -> T.Text -> IO ()
logH ch kind text = void (try (ask ch "log" [("kind", JStr kind), ("text", JText text)]) :: IO (Either SomeException Json))

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
        threadDelay 250000
        t <- now
        v <- verdictAt ch
        case v of
          Just r | vStart r >= written - 0.05, not (vRunning r) -> pure (Just r)
          Just r | vStart r >= written - 0.05 -> do
            let since = fromMaybe t running
            if t - since >= checkGrace then pure (Just r) else go (Just since)
          _ | t - t0 >= secs -> pure Nothing
            | otherwise -> go running
  go Nothing

-- | Seconds a save waits, once its code compiles, for a check that runs: a quick check's verdict comes with
-- the save; a longer one is not waited for -- the agent goes on, and gets it with a later tool result.
-- (Saves were 79 of an agent's 249 tool minutes, most of it checks of 20-100 s, once per edit of a fix.)
checkGrace :: Double
checkGrace = 6

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
          pure (T.pack ("\n[the check of your earlier save has finished: " ++ vLine r ++ concatMap ("\n" ++) (vBehind r) ++ "]"))
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
            threadDelay 250000
            t <- now
            v <- verdictAt ch
            case v of
              Just r | not (vRunning r), vStart r >= written - 0.05 -> pure (Just r)
              _ | t - t0 >= secs -> pure Nothing
                | otherwise -> go
      go

-- | A save's answer: what was written, and the verdict of the reload it caused, when the file is one the
-- session watches. A verdict that is not there within the wait is said to be pending (a long compile):
-- status has it later. The save itself is good either way.
saved :: Chat -> String -> Maybe Double -> IO (Bool, T.Text)
saved _ what Nothing = pure (True, T.pack what)
saved ch what (Just written) = do
  longest <- readIORef (cLongest ch)
  let wait = saveWait longest
  v <- verdictAfter ch written wait
  case v of
    Nothing -> pure (True, T.pack (what ++ printf "\n[the session has not finished reloading this save after %.0fs (the longest verdict so far took %.0fs): status will have its verdict; do not reload by hand]" wait longest))
    Just r | vRunning r -> do
      writeIORef (cPending ch) (Just written)
      pure (True, T.pack (what ++ "\nverdict: " ++ vLine r ++ " -- its verdict comes with a later tool result; go on meanwhile"))
    Just r -> do
      -- (a newer save's verdict settles any check an earlier one left running)
      modifyIORef' (cPending ch) (\p -> case p of { Just w | w <= written -> Nothing; _ -> p })
      pure (not (isRed (vLine r)), T.pack (what ++ "\nverdict: " ++ vLine r ++ concatMap ("\n" ++) (vBehind r)))

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
sessionToolNames = ["eval", "status", "typecheck", "reload", "test", "doc", "census", "bench", "mem", "zoom", "date", "remember"]

-- | The session's tools (as the MCP server defines them, the chat being one session) and the agent's hands on the files.
chatTools :: [Tool]
chatTools =
  [ t { tProps = [ if k == "timeout" then (k, ("number", timeoutDesc)) else p | p@(k, _) <- tProps t, k /= "session" ], tDesc = if tName t == "eval" then evalDesc else tDesc t } | t <- tools, tName t `elem` sessionToolNames ]
  ++ [ Tool "read" "A file of the project, with line numbers." [("path", ("string", "relative to the project")), ("start", ("number", "first line (default 1)")), ("lines", ("number", "how many (default 200)"))] ["path"]
     , Tool "write" "Write a file of the project whole. A watched source (or a .cabal) is reloaded by the session itself and the answer carries the verdict of that reload: no status call is needed after it." [("path", ("string", "relative to the project")), ("content", ("string", "the whole content"))] ["path", "content"]
     , Tool "edit" "Replace one exact, unique occurrence of a text in a file of the project. A watched source (or a .cabal) is reloaded by the session itself and the answer carries the verdict of that reload: no status call is needed after it." [("path", ("string", "relative to the project")), ("old", ("string", "the text as it is, unique in the file")), ("new", ("string", "its replacement"))] ["path", "old", "new"]
     , Tool "edits" "Several replacements at once, in one file or several: each is checked against the file (as the replacements before it leave it) before anything is written, then all are written together -- ONE reload and ONE verdict instead of one per edit. Use it for any change that touches more than one spot." [("edits", ("array", "the replacements, in order: each {\"path\", \"old\", \"new\"}, the old text exactly as it is (unique in its file); one without a path is in the file of the one before it")), ("path", ("string", "the file of the replacements that name none (optional)"))] ["edits"]
     , Tool "ls" "List a directory of the project." [("path", ("string", "relative to the project (default: the root)"))] []
     , Tool "sh" "Run a shell command in the project's directory: its output and status." [("cmd", ("string", "the command")), ("timeout", ("number", "seconds (default 120)"))] ["cmd"] ]
  where timeoutDesc = "seconds before it is interrupted (default 120): give more for a whole test run or a long benchmark, less to probe for a hang. An evaluation that does not stop when interrupted (a loop that does not allocate, a blocking foreign call) is said so; the session finishes it before its next answer"
        evalDesc = "Evaluate a Haskell expression, or run a GHCi command (:t, :i, :browse, import M), against the LOADED code. The answer is what GHCi printed. ONE expression, command or declaration group per call: several lines are one GHCi block (:{ :}), so an import or a let on its own line fails to parse -- make a separate call for an import, and write `let a = 1; b = 2 in ...` on one line."

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
                        , ("properties", JObj [ (k, JObj ([("type", JStr ty), ("description", JStr d)] ++ [ ("items", replacement) | ty == "array" ])) | (k, (ty, d)) <- tProps t ])
                        , ("required", JArr (map JStr (tReq t))) ]) ]) ]
  where replacement = JObj [ ("type", JStr "object")
                           , ("properties", JObj [ (k, JObj [("type", JStr "string")]) | k <- ["path", "old", "new"] ])
                           , ("required", JArr (map JStr ["path", "old", "new"])) ]

-- | What a model calls an argument when it does not call it by its name.
aliases :: [(String, [String])]
aliases = [ ("cmd", ["command", "shell", "script"]), ("path", ["file", "filename", "file_path", "filepath"]), ("content", ["text", "contents", "data"])
          , ("old", ["old_string", "old_text", "from", "search"]), ("new", ["new_string", "new_text", "to", "replace"]), ("expr", ["expression", "code", "command"])
          , ("query", ["q", "name", "words"]), ("lines", ["count", "limit"]), ("start", ["from_line", "line", "offset"])
          , ("edits", ["changes", "replacements", "edit_list"]) ]

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
  | not (null missing) = pure (False, T.pack (tName t ++ ": missing argument(s) " ++ intercalate ", " missing ++ "; it takes " ++ intercalate ", " (tReq t)))
  | tName t == "eval" = evalTool ch a
  | tName t `elem` sessionToolNames = sessionCall ch (tName t) (withTimeout (set "session" (JStr (cName ch)) a))
  | otherwise = do
      r <- try (fileTool ch (tName t) a) :: IO (Either IOException (Bool, T.Text))
      pure (either (\e -> (False, T.pack ("IOError: " ++ show e))) id r)
  where (a, missing) = arguments t a0

-- | Seconds an eval or a bench may run before it is interrupted, when the call does not say: the session's
-- own default is ten minutes, and an agent's expression that hangs (a loop that never ends) is a dead ten
-- minutes of the turn; two are enough for anything an agent tries, and the answer says how to ask for more.
evalTimeout :: Double
evalTimeout = 120

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
sessionCall ch name args = do
  (ok, out, reach) <- Mcp.callReach (cConf ch) name args
  if reach == Mcp.Reached then writeIORef (cDown ch) False >> pure (ok, out) else do
    wasDown <- readIORef (cDown ch)
    limit <- if wasDown then pure 5 else downWait
    t0 <- now
    hPutStrLn stderr (printf "[the session is down: waiting up to %.0fs for it to come back]" limit)
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
  let ls = lines (trim (fromMaybe "" (lookupStr "expr" a)))
      (heads, rest) = splitImports ls
      one e = sessionCall ch "eval" (withTimeout (JObj ([("expr", JStr e), ("session", JStr (cName ch))] ++ [ ("timeout", JNum n) | Just n <- [lookupNum "timeout" a] ])))
  outs <- forM heads $ \h -> do
    (ok, out) <- one h
    pure [ T.pack h <> T.pack "\n" <> out | not ok || T.isInfixOf (T.pack "error") out ]
  let expr = intercalate "\n" rest
  (ok, out) <- one expr
  let hint | '\n' `elem` expr && T.isInfixOf (T.pack "parse error") out =
               T.pack "\n[hint: a multi-line eval is ONE GHCi block (:{ :}); a let on its own line does not parse there -- write `let a = 1; b = 2 in ...` on one line, or one declaration group per call]"
           | T.isInfixOf (T.pack "timed out after") out && isNothing (lookupNum "timeout" a) =
               T.pack (printf "\n[interrupted after %.0fs, the default: an expression that needs longer says so with timeout]" evalTimeout)
           | otherwise = T.empty
  pure (ok, T.intercalate (T.pack "\n") (concat outs ++ [out <> hint]))

-- | An eval's leading import lines and : commands, each to be its own command, and the rest. A lone line
-- stays what it is.
splitImports :: [String] -> ([String], [String])
splitImports ls@(l : more)
  | not (null more), "import " `isPrefixOf` l || ":" `isPrefixOf` l = let (hs, r) = splitImports more in (l : hs, r)
  | otherwise = ([], ls)
splitImports [] = ([], [])

-- | The watched files that differ from the loaded code right now.
staleNow :: Chat -> IO [String]
staleNow ch = (\r -> strs (r .: "stale")) <$> ask ch "status" []

-- | After a shell command: the verdict of the reload it caused, if the session shows one coming -- a file
-- newly among those that differ from the loaded code, or a verdict newer than before the command -- else
-- Nothing. (Not "files differ": with a compile error on disk the session stays that way until it is fixed,
-- and a read-only command then waited the whole wait for a verdict that was never due.)
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

-- | The agent's hands on the files, inside the project only.
fileTool :: Chat -> String -> Json -> IO (Bool, T.Text)
fileTool ch name a = case name of
  "read" -> withPath $ \p -> do
    t <- decode <$> B.readFile p
    let start = max 1 (maybe 1 round (lookupNum "start" a))
        n = maybe 200 round (lookupNum "lines" a) :: Int
        ls = zip [1 :: Int ..] (T.splitOn (T.pack "\n") t)
        shown = [ T.pack (printf "%5d  " i) <> l | (i, l) <- ls, i >= start, i < start + n ]
    pure (True, if null shown then T.pack "(empty)" else T.intercalate (T.pack "\n") shown)
  "write" -> withPath $ \p -> do
    let content = fromMaybe T.empty (lookupText "content" a)
    written <- writtenAt ch rel
    createDirectoryIfMissing True (takeDirectory p)
    B.writeFile p (TE.encodeUtf8 content)
    saved ch (printf "wrote %s (%d characters)" rel (T.length content)) written
  "edit" -> withPath $ \p -> do
    t <- decode <$> B.readFile p
    let old = fromMaybe T.empty (lookupText "old" a)
        new = fromMaybe T.empty (lookupText "new" a)
    case replaceOnce t old new of
      Right (t', how) -> do
        written <- writtenAt ch rel
        B.writeFile p (TE.encodeUtf8 t')
        saved ch ("edited " ++ rel ++ how) written
      Left why -> pure (False, T.pack (rel ++ ": ") <> why)
  -- several replacements, in one or more files: all checked against the files (as the ones before them
  -- leave them) before any is written, then written together -- one reload, one verdict
  "edits" -> do
    let items = editPaths (lookupStr "path" a) [ fst (arguments editTool e) | e <- lookupArr "edits" a ]
        apply files [] = pure (Right files)
        apply _ ((i, (Nothing, _)) : _) =
          pure (Left (T.pack (printf "replacement %d: no path (each replacement is {path, old, new}; one without a path is in the file of the one before it)" (i :: Int))))
        apply files ((i, (Just rp, e)) : rest) = case inside ch rp of
          Left why -> pure (Left (T.pack (printf "replacement %d: %s" i why)))
          Right p -> do
            t <- maybe (decode <$> B.readFile p) pure (lookup p files)
            case replaceOnce t (fromMaybe T.empty (lookupText "old" e)) (fromMaybe T.empty (lookupText "new" e)) of
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
          forM_ files (\(f, t) -> B.writeFile f (TE.encodeUtf8 t))
          saved ch (printf "edited %s (%d replacement(s))" (intercalate ", " (reverse rels)) (length items)) written
  "ls" -> withPath $ \p -> do
    es <- filter (not . ("." `isPrefixOf`)) <$> listDirectory p
    tagged <- forM (sort es) $ \e -> (\d -> e ++ (if d then "/" else "")) <$> doesDirectoryExist (p </> e)
    pure (True, T.pack (intercalate "\n" tagged))
  "sh" -> do
    up <- isJust <$> verdictAt ch
    written <- if up then Just <$> now else pure Nothing
    staleBefore <- staleNow ch
    let cmd = fromMaybe "" (lookupStr "cmd" a)
    t0 <- now
    (ok, out) <- shTool (cDir ch) cmd (fromMaybe 120 (lookupNum "timeout" a))
    secs <- subtract t0 <$> now
    -- a command that edited a watched source (sed -i, a generator, git) is a save too: the session reloads
    -- it, and the answer waits for that verdict as write and edit do, else the agent reloads by hand
    pending <- shSaved ch written staleBefore
    -- a GHCi of the agent's own loads the project cold, every time, what the session has loaded warm
    let own | up && ownGhci cmd = T.pack (printf "\n[note: this started a GHCi of its own, loading the project cold (%.1fs); the session has it loaded -- eval, test with expr (one group of tests alone), typecheck answer from it in about a second]" secs)
            | otherwise = T.empty
    pure (ok && maybe True fst pending, capWith shCap shHint out <> own <> maybe T.empty snd pending)
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
  | k == 0, Just (t', l0, l1) <- fuzzyReplace t old new = Right (t', printf " (the text matched lines %d-%d only with its spacing squeezed: applied there)" l0 l1)
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
-- between them: the file with that run replaced, and its first and last line. The run starts at its first
-- word and ends at its last, so the whitespace the old text had around its words (an indent, a newline) is
-- taken off the new text too when the new text has the same. Eight of 94 edits in one round of an agent's
-- work failed on spacing alone.
fuzzyReplace :: T.Text -> T.Text -> T.Text -> Maybe (T.Text, Int, Int)
fuzzyReplace file old new
  | null ows = Nothing
  | otherwise = case matches of
      [(s, e)] -> Just (T.take s file <> new' <> T.drop e file, lineAt s, lineAt e)
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
shTool :: FilePath -> String -> Double -> IO (Bool, T.Text)
shTool dir cmd secs = do
  (_, Just o, Just e, ph) <- createProcess (shell cmd) { cwd = Just dir, std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
  mo <- newEmptyMVar
  me <- newEmptyMVar
  void (forkIO (B.hGetContents o >>= putMVar mo))
  void (forkIO (B.hGetContents e >>= putMVar me))
  r <- timeout (round (secs * 1e6)) (waitForProcess ph)
  case r of
    Nothing -> terminateProcess ph >> void (waitForProcess ph) >> pure (False, T.pack "timed out")
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

master :: String -> String
master who = unlines
  [ "You are " ++ who ++ ", an AI agent that works for one user in a single chat that"
  , "never ends, on a Haskell project whose code is loaded in a warm GHCi session."
  , "Do the user's tasks yourself, with your tools, following the user's"
  , "instructions at the end of this prompt: they say who the user is, how"
  , "their files are organized and how they want work done."
  , ""
  , "The session is your sandbox: eval runs against the loaded code in"
  , "milliseconds; a file you write or edit is reloaded by the session itself"
  , "and the write's answer carries that reload's verdict (COMPILE-ERROR,"
  , "CHECK-FAIL, or OK -- CHECK-PASS); status repeats the latest verdict"
  , "(STALE when the loaded code is behind the disk). Prefer an evaluation to"
  , "a guess, and the verdict to a belief that an edit is right. Fix a"
  , "COMPILE-ERROR before anything else: the session answers from the last"
  , "code that compiled until you do."
  , ""
  , "You keep no memory between turns. Each turn starts with the view below,"
  , "followed by the user's new message. Summaries keep little of tool"
  , "output, so say in your reply what you learned that will matter later,"
  , "or keep it with remember: a finding, a decision, what is left undone."
  , "Messages the user sends while you work reach you between tool calls." ]

viewDoc :: String -> String
viewDoc who = unlines
  [ "The view: the whole history of this session -- the chat between " ++ who ++ " and the"
  , "user, and everything done to the code by hand -- oldest first, inside"
  , "<chat> tags, as one-line summaries. Each line is"
  , ""
  , "  id+n|text   the n messages from id on, summarized (newlines shown as spaces)"
  , ""
  , "A summary tags each item with its kind: user (the user's words), talk"
  , "(" ++ who ++ "'s replies), tool (a request: an evaluation, a reload, a save of a"
  , "file), echo (its result: the output, the verdict), note (memories from"
  , "before this chat). A short message is its own line, word for word. Recent"
  , "lines cover one message each; the older the messages, the more a line"
  , "covers. A message not summarized yet shows as \"(not summarized yet: zoom"
  , "it)\". No message appears in full, not even the last ones."
  , ""
  , "Navigating: zoom(id, n) opens line id+n into the two lines of n/2"
  , "messages it was made from; zoom(id, 1) gives message id in full. Zoom"
  , "whenever a summary only mentions something you need, such as what your"
  , "last reply said, a decision, a past attempt or where a file is, before"
  , "you act, guess or ask. date(id) gives the date and time of message id." ]

-- the turn loop ------------------------------------------------------------------------------

-- | One fresh call: the view, then the message; its tools until it ends.
turn :: Chat -> Endpoint -> Opts -> String -> [T.Text] -> TQueue (Maybe T.Text) -> IO ()
turn ch e o system texts pending = do
  (v, settled, parts, _) <- view ch (oSettle o)
  unless settled (hPutStrLn stderr (printf "[view: %d lines, not all summarized yet; going on]" parts))
  forM_ texts (logH ch "user")
  let msgs0 = [ msg "system" (T.pack system), msg "user" (v <> T.pack "\n\n" <> T.intercalate (T.pack "\n\n") texts) ]
  spent <- newIORef (Spent 0 0 0 0 0 False False)
  readsR <- newIORef ([] :: [ReadRec])
  tStart <- now
  loop readsR spent msgs0 0 (0 :: Int) (0 :: Int)
  tEnd <- now
  s <- readIORef spent
  hPutStrLn stderr (spentLine s (tEnd - tStart))
  where
    msg role text = JObj [("role", JStr role), ("content", JText text)]
    byName = [ (tName t, t) | t <- chatTools ]
    toolsJson = map toolJson chatTools
    loop readsR spent msgs step cut failures
      | step >= oMaxSteps o = hPutStrLn stderr "[the turn reached its step limit; stopping]"
      | otherwise = do
          t0 <- now
          r <- request e (Request msgs toolsJson (oMaxTokens o) Nothing Nothing Nothing 900)
          t1 <- now
          case r of
            Left why | failures < 2 -> do
              hPutStrLn stderr ("[" ++ why ++ "; asking again in 5s]")
              threadDelay 5000000
              loop readsR spent msgs step cut (failures + 1)
            Left why -> hPutStrLn stderr ("chat: " ++ why ++ "; the turn ends")
            Right p -> do
              forM_ (pUsage p) $ \u -> do
                spend spent u
                recordUsage (usageFile ch) "chat" e u (t1 - t0)
                when (oUsage o) (hPutStrLn stderr (printf "[usage: in %d, out %d%s, %.1fs]" (uIn u) (uOut u) (maybe "" (\c -> ", cached " ++ show c) (uCached u)) (t1 - t0)))
              unless (T.null (T.strip (pReasoning p))) (hPutStrLn stderr ("\n[thinking] " ++ T.unpack (T.strip (pReasoning p)) ++ "\n"))
              let content = T.strip (pContent p)
              unless (T.null content) (TIO.putStrLn content >> putStrLn "" >> hFlush stdout >> logH ch "talk" content)
              let assistant = JObj ([("role", JStr "assistant"), ("content", JText (pContent p))] ++ [ ("tool_calls", JArr (map tcRaw (pToolCalls p))) | not (null (pToolCalls p)) ])
                  msgs' = msgs ++ [assistant]
              if null (pToolCalls p)
                then if pFinish p == "length" && cut < 3
                  then do
                    -- cut off at the output limit (most often: the thinking ran on) is not the end of the turn
                    hPutStrLn stderr (printf "[the reply was cut off at %d tokens; asking it to go on in smaller steps]" (oMaxTokens o))
                    loop readsR spent (msgs' ++ [msg "user" (T.pack (printf "[harness: your reply was cut off at the output limit of %d tokens before any tool call -- the reasoning ran too long. Go on in smaller steps: act with a tool (eval to count or check, write a smaller piece) instead of working it all out first.]" (oMaxTokens o)))]) (step + 1) (cut + 1) 0
                  else do
                    -- a turn that changed the files and ends with the verdict red is told so, once: it
                    -- either fixes it or says plainly that it stops red, never ends there in silence
                    s <- readIORef spent
                    when (sTouched s && not (sNudged s)) (awaitPending ch)
                    v <- if sTouched s && not (sNudged s) then verdictAt ch else pure Nothing
                    case v of
                      Just r | isRed (vLine r) -> do
                        modifyIORef' spent (\x -> x { sNudged = True })
                        hPutStrLn stderr "[the turn would end with the verdict red; saying so once]"
                        loop readsR spent (msgs' ++ [msg "user" (T.pack ("[harness: you are ending the turn with the session's verdict red: " ++ vLine r ++ concatMap ("\n" ++) (take 8 (vBehind r))
                                                                  ++ "\nFix it, or end by saying plainly that it is red and why you are stopping.]"))]) (step + 1) 0 0
                      _ -> pure ()
                else do
                  modifyIORef' spent (\s -> s { sTools = sTools s + length (pToolCalls p), sTouched = sTouched s || any ((`elem` ["write", "edit", "edits", "sh"]) . tcName) (pToolCalls p) })
                  replies <- forM (zip [0 :: Int ..] (pToolCalls p)) $ \(i, tc) -> do
                    let name = tcName tc
                        shownArgs = take 300 (encode (tcArgs tc))
                        isSession = name `elem` sessionToolNames
                    putStrLn ("> " ++ name ++ " " ++ shownArgs) >> hFlush stdout
                    unless isSession (logH ch "tool" (T.pack (name ++ " " ++ encode (tcArgs tc))))
                    t2 <- now
                    (ok, out0) <- case lookup name byName of
                      Just t -> runTool ch t (tcArgs tc)
                      Nothing -> pure (False, T.pack ("unknown tool " ++ show name))
                    late <- pendingNote ch
                    t3 <- now
                    -- a read: numbered, an alias of an earlier one when its text is that one's, else
                    -- superseding the earlier reads of its lines
                    out1 <- case (name, ok, readRange out0, lookup name byName) of
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
                    when (oUsage o) (hPutStrLn stderr (printf "[tool: %s %.1fs]" name (t3 - t2)))
                    unless isSession (logH ch "echo" tagged)
                    TIO.putStrLn (T.pack "  " <> T.replace (T.pack "\n") (T.pack "\n  ") (T.take 600 out) <> (if T.length out > 600 then T.pack "..." else T.empty)) >> hFlush stdout
                    pure (JObj [("role", JStr "tool"), ("tool_call_id", JStr (tcId tc)), ("content", JText tagged)])
                  mid <- drain pending
                  forM_ mid (logH ch "user")
                  let next = msgs' ++ replies ++ [ msg "user" (T.intercalate (T.pack "\n\n") mid) | not (null mid) ]
                  -- the superseded reads, rewritten as stubs once there is enough of them
                  recs <- readIORef readsR
                  let due = [ r | r <- recs, isJust (rrBy r), not (rrTrimmed r) ]
                      dueChars = sum (map (T.length . rrText) due)
                  limit <- trimAtNow
                  next' <- if null due || dueChars < limit then pure next else do
                    let stubs = M.fromList [ (rrIdx r, T.pack (printf "[read #%d: %s lines %d-%d -- superseded by read #%d, which shows these lines as they are now]" (rrNum r) (rrPath r) (rrLo r) (rrHi r) (fromMaybe 0 (rrBy r)))) | r <- due ]
                    writeIORef readsR [ if rrNum r `elem` map rrNum due then r { rrTrimmed = True, rrText = T.empty } else r | r <- recs ]
                    hPutStrLn stderr (printf "[context: %d superseded read(s) trimmed, %d characters]" (length due) dueChars)
                    pure [ maybe m (\st -> set "content" (JText st) m) (M.lookup ix stubs) | (ix, m) <- zip [0 ..] next ]
                  loop readsR spent next' (step + 1) 0 0

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

-- | The first and last line a read's answer shows ("   12  text" lines).
readRange :: T.Text -> Maybe (Int, Int)
readRange out = case [ n | l <- T.lines out, [(n, "")] <- [reads (T.unpack (T.takeWhile (/= ' ') (T.stripStart l))) :: [(Int, String)]] ] of
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

-- | The lines typed since last asked.
drain :: TQueue (Maybe T.Text) -> IO [T.Text]
drain q = atomically go
  where go = do
          m <- tryReadTQueue q
          case m of
            Just (Just l) -> (l :) <$> go
            _ -> pure []

-- | @ghci-session chat ARGS@.
chatMain :: Conf -> [String] -> IO Int
chatMain conf args = case parseOpts args of
  Left why -> hPutStrLn stderr why >> pure 2
  Right o -> do
    picked <- pick conf (oSession o)
    case picked of
      Left why -> hPutStrLn stderr ("chat: " ++ why) >> pure 2
      Right name -> do
        cfg <- resolve conf name
        members <- readMembers conf name
        longest <- newIORef 0
        pendingCheck <- newIORef Nothing
        down <- newIORef False
        let ms = if null members then [name] else members
            ch = Chat conf name (cRoot conf) (map normalise (concat [ strs (targetJson conf m .: "watch") | m <- ms ])) (either (const "Agent") gAgent cfg) longest pendingCheck down
        if oPrintView o then view ch 0 >>= \(v, _, _, _) -> TIO.putStrLn v >> pure 0 else do
          ep <- endpointFromEnv
          case ep of
            Left why -> hPutStrLn stderr ("chat: " ++ why) >> pure 2
            Right e0 -> do
              let e = e0 { eModel = fromMaybe (eModel e0) (oModel o), eBase = maybe (eBase e0) (reverse . dropWhile (== '/') . reverse) (oBase o) }
              instr <- maybe (pure "") (\f -> trim <$> readFile f) (oInstructions o)
              let system = master (cAgent ch) ++ "\n" ++ viewDoc (cAgent ch) ++ (if null instr then "" else "\n" ++ instr)
              (v, _, parts, messages) <- view ch 0
              TIO.putStrLn v
              putStrLn (printf "[%s: %d messages, %d lines; model %s]\n" name messages parts (eModel e))
              hFlush stdout
              pending <- newTQueueIO
              case oOnce o of
                Just m -> turn ch e o system [T.pack m] pending >> pure 0
                Nothing -> do
                  hSetBuffering stdout LineBuffering
                  void $ forkIO $ do
                    let reader = do
                          eof <- hIsEOF stdin
                          if eof then atomically (writeTQueue pending Nothing) else do
                            l <- TIO.getLine
                            atomically (writeTQueue pending (Just l))
                            reader
                    reader
                  let loop = do
                        putStr "> " >> hFlush stdout
                        first <- atomically (readTQueue pending)
                        case first of
                          Nothing -> pure 0
                          Just l -> do
                            more <- drain pending
                            let texts = filter (not . T.null . T.strip) (l : more)
                            unless (null texts) (turn ch e o system texts pending)
                            loop
                  loop

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
      case ep of
        Left why -> hPutStrLn stderr ("summarize: " ++ why) >> pure 2
        Right e -> do
          let go tries think budget
                | tries >= (3 :: Int) = pure (Left "cut off at the output limit three times")
                | otherwise = do
                    t0 <- now
                    r <- request e (Request ([ JObj [("role", JStr "system"), ("content", JText system)] | not (T.null system) ] ++ [ JObj [("role", JStr "user"), ("content", JText user)] ])
                                            [] budget (Just 0.3) (if isDeepSeek e then Just think else Nothing) (if think then Just effort else Nothing) 300)
                    t1 <- now
                    forM_ ledger $ \f -> forM_ (either (const Nothing) pUsage r) $ \u -> recordUsage f "summarize" e u (t1 - t0)
                    case r of
                      Left why -> pure (Left why)
                      Right p | not (T.null (T.strip (pContent p))) || pFinish p /= "length" -> pure (Right p)
                              -- cut off before any text: the budget went to thoughts -- ask again without them, and with more
                              | otherwise -> go (tries + 1) False (budget * 4)
          r <- go 0 think0 budget0
          case r of
            Left why -> hPutStrLn stderr ("summarize: " ++ why) >> pure 1
            Right p -> case filter (not . T.null) (map T.strip (T.lines (pContent p))) of
              (l : _) -> TIO.putStrLn l >> pure 0
              [] -> hPutStrLn stderr ("summarize: the model answered nothing (finish_reason " ++ pFinish p ++ ")") >> pure 1
  where
    split prompt =
      let ls = T.lines prompt
      in case break ((== T.pack "<chat>") . T.strip) ls of
           (before, rest@(_ : _)) -> (T.strip (T.unlines before), T.strip (T.unlines rest))
           _ -> (T.empty, T.strip prompt)
