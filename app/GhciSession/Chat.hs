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
module GhciSession.Chat (chatMain, summarizeMain) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, try)
import Data.IORef
import Control.Monad (forM, forM_, unless, void, when)
import qualified Data.ByteString as B
import Data.Char (isSpace)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe, isJust, isNothing)
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
import GhciSession.Mcp (Tool (..), call, pick, tools)
import qualified GhciSession.Mcp as Mcp
import GhciSession.Sys (now)

-- | Characters of a tool result kept (head and tail), as the spec logs them.
capChars :: Int
capChars = 30000

-- | Seconds a write or edit waits for the verdict of the reload it causes.
saveWait :: Double
saveWait = 45

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

data Chat = Chat { cConf :: Conf, cName :: String, cDir :: FilePath, cWatched :: [FilePath], cAgent :: String }

-- | The session's usage ledger: one line per model call, the chat's and the compactor's.
usageFile :: Chat -> FilePath
usageFile ch = cStateDir (cConf ch) </> cName ch </> "usage.jsonl"

-- | What a turn cost so far: model calls, tokens in, of them cached, tokens out, tool calls.
data Spent = Spent { sCalls :: !Int, sIn :: !Int, sCached :: !Int, sOut :: !Int, sTools :: !Int }

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

-- | The verdict now: its time stamp, its line, and what is behind it -- the compiler's diagnostics
-- (file:line:col and the message whole) behind a COMPILE-ERROR, the failing lines behind a CHECK-FAIL,
-- nothing behind an OK. Nothing when no session answers.
verdictAt :: Chat -> IO (Maybe (Double, String, [String]))
verdictAt ch = do
  r <- ask ch "status" []
  let j = r .: "status"
  pure $ case (lookupBool "ok" r, obj j, lookupNum "at" j) of
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
            _ -> []
      in Just (at, headL, behind)
    _ -> Nothing

-- | The verdict the session gives AFTER the one at @before@ (a save's), within the seconds; else Nothing.
verdictAfter :: Chat -> Double -> Double -> IO (Maybe (Double, String, [String]))
verdictAfter ch before secs = do
  t0 <- now
  let go = do
        threadDelay 250000
        t <- now
        v <- verdictAt ch
        case v of
          Just r@(at, _, _) | at > before -> pure (Just r)
          _ | t - t0 >= secs -> pure Nothing
            | otherwise -> go
  go

-- | A save's answer: what was written, and the verdict of the reload it caused, when the file is one the
-- session watches. A verdict that is not there within the wait is said to be pending (a long compile):
-- status has it later. The save itself is good either way.
saved :: Chat -> String -> Maybe (Double, String, [String]) -> IO (Bool, T.Text)
saved _ what Nothing = pure (True, T.pack what)
saved ch what (Just (before, _, _)) = do
  v <- verdictAfter ch before saveWait
  pure $ case v of
    Nothing -> (True, T.pack (what ++ printf "\n[the session has not finished reloading this save after %.0fs: status will have its verdict]" saveWait))
    Just (_, headL, behind) -> (not ("ERROR" `isInfixOf` headL || "FAIL" `isInfixOf` headL), T.pack (what ++ "\nverdict: " ++ headL ++ concatMap ("\n" ++) behind))

-- the tools ----------------------------------------------------------------------------------

sessionToolNames :: [String]
sessionToolNames = ["eval", "status", "typecheck", "reload", "test", "doc", "census", "bench", "mem", "zoom", "date"]

-- | The session's tools (as the MCP server defines them, the chat being one session) and the agent's hands on the files.
chatTools :: [Tool]
chatTools =
  [ t { tProps = [ p | p@(k, _) <- tProps t, k /= "session" ], tDesc = if tName t == "eval" then evalDesc else tDesc t } | t <- tools, tName t `elem` sessionToolNames ]
  ++ [ Tool "read" "A file of the project, with line numbers." [("path", ("string", "relative to the project")), ("start", ("number", "first line (default 1)")), ("lines", ("number", "how many (default 200)"))] ["path"]
     , Tool "write" "Write a file of the project whole. A watched source (or a .cabal) is reloaded by the session itself and the answer carries the verdict of that reload: no status call is needed after it." [("path", ("string", "relative to the project")), ("content", ("string", "the whole content"))] ["path", "content"]
     , Tool "edit" "Replace one exact, unique occurrence of a text in a file of the project. A watched source (or a .cabal) is reloaded by the session itself and the answer carries the verdict of that reload: no status call is needed after it." [("path", ("string", "relative to the project")), ("old", ("string", "the text as it is, unique in the file")), ("new", ("string", "its replacement"))] ["path", "old", "new"]
     , Tool "ls" "List a directory of the project." [("path", ("string", "relative to the project (default: the root)"))] []
     , Tool "sh" "Run a shell command in the project's directory: its output and status." [("cmd", ("string", "the command")), ("timeout", ("number", "seconds (default 120)"))] ["cmd"] ]
  where evalDesc = "Evaluate a Haskell expression, or run a GHCi command (:t, :i, :browse, import M), against the LOADED code. The answer is what GHCi printed. ONE expression, command or declaration group per call: several lines are one GHCi block (:{ :}), so an import or a let on its own line fails to parse -- make a separate call for an import, and write `let a = 1; b = 2 in ...` on one line."

-- | A tool as the endpoint takes it.
toolJson :: Tool -> Json
toolJson t = JObj [ ("type", JStr "function"), ("function", JObj
  [ ("name", JStr (tName t)), ("description", JStr (tDesc t))
  , ("parameters", JObj [ ("type", JStr "object")
                        , ("properties", JObj [ (k, JObj [("type", JStr ty), ("description", JStr d)]) | (k, (ty, d)) <- tProps t ])
                        , ("required", JArr (map JStr (tReq t))) ]) ]) ]

-- | What a model calls an argument when it does not call it by its name.
aliases :: [(String, [String])]
aliases = [ ("cmd", ["command", "shell", "script"]), ("path", ["file", "filename", "file_path", "filepath"]), ("content", ["text", "contents", "data"])
          , ("old", ["old_string", "old_text", "from", "search"]), ("new", ["new_string", "new_text", "to", "replace"]), ("expr", ["expression", "code", "command"])
          , ("query", ["q", "name", "words"]), ("lines", ["count", "limit"]), ("start", ["from_line", "line", "offset"]) ]

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
  | tName t `elem` sessionToolNames = call (cConf ch) (tName t) (set "session" (JStr (cName ch)) a)
  | otherwise = do
      r <- try (fileTool ch (tName t) a) :: IO (Either IOException (Bool, T.Text))
      pure (either (\e -> (False, T.pack ("IOError: " ++ show e))) id r)
  where (a, missing) = arguments t a0

-- | An eval: leading import lines (and : commands) are each their own command -- in one block with the
-- expression they do not parse -- and a block that still does not parse is told why.
evalTool :: Chat -> Json -> IO (Bool, T.Text)
evalTool ch a = do
  let ls = lines (trim (fromMaybe "" (lookupStr "expr" a)))
      (heads, rest) = split ls
      one e = call (cConf ch) "eval" (JObj ([("expr", JStr e), ("session", JStr (cName ch))] ++ [ ("timeout", JNum n) | Just n <- [lookupNum "timeout" a] ]))
  outs <- forM heads $ \h -> do
    (ok, out) <- one h
    pure [ T.pack h <> T.pack "\n" <> out | not ok || T.isInfixOf (T.pack "error") out ]
  let expr = intercalate "\n" rest
  (ok, out) <- one expr
  let hint = if '\n' `elem` expr && T.isInfixOf (T.pack "parse error") out
               then T.pack "\n[hint: a multi-line eval is ONE GHCi block (:{ :}); a let on its own line does not parse there -- write `let a = 1; b = 2 in ...` on one line, or one declaration group per call]"
               else T.empty
  pure (ok, T.intercalate (T.pack "\n") (concat outs ++ [out <> hint]))
  where
    split ls@(l : more) | not (null more), "import " `isPrefixOf` l || ":" `isPrefixOf` l = let (hs, r) = split more in (l : hs, r)
                        | otherwise = ([], ls)
    split [] = ([], [])

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
    before <- if watches ch rel then verdictAt ch else pure Nothing
    createDirectoryIfMissing True (takeDirectory p)
    B.writeFile p (TE.encodeUtf8 content)
    saved ch (printf "wrote %s (%d characters)" rel (T.length content)) before
  "edit" -> withPath $ \p -> do
    t <- decode <$> B.readFile p
    let old = fromMaybe T.empty (lookupText "old" a)
        new = fromMaybe T.empty (lookupText "new" a)
        k = if T.null old then 0 else T.count old t
    if k /= 1 then pure (False, T.pack (printf "%s: the text occurs %d times; it must occur exactly once" rel k)) else do
      before <- if watches ch rel then verdictAt ch else pure Nothing
      let (pre, post) = T.breakOn old t
      B.writeFile p (TE.encodeUtf8 (pre <> new <> T.drop (T.length old) post))
      saved ch ("edited " ++ rel) before
  "ls" -> withPath $ \p -> do
    es <- filter (not . ("." `isPrefixOf`)) <$> listDirectory p
    tagged <- forM (sort es) $ \e -> (\d -> e ++ (if d then "/" else "")) <$> doesDirectoryExist (p </> e)
    pure (True, T.pack (intercalate "\n" tagged))
  "sh" -> shTool (cDir ch) (fromMaybe "" (lookupStr "cmd" a)) (fromMaybe 120 (lookupNum "timeout" a))
  _ -> pure (False, T.pack ("unknown tool " ++ name))
  where
    rel = fromMaybe "." (lookupStr "path" a)
    withPath k = case inside ch rel of
      Left why -> pure (False, T.pack why)
      Right p -> k p

-- | A path of the project, or why not.
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
cap t | T.length t <= capChars = t
      | otherwise = T.take h t <> T.pack ("\n[... " ++ show (T.length t - 2 * h) ++ " characters cut ...]\n") <> T.takeEnd h t
  where h = capChars `div` 2

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
  , "output, so say in your reply what you learned that will matter later."
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
  spent <- newIORef (Spent 0 0 0 0 0)
  tStart <- now
  loop spent msgs0 0 (0 :: Int) (0 :: Int)
  tEnd <- now
  s <- readIORef spent
  hPutStrLn stderr (spentLine s (tEnd - tStart))
  where
    msg role text = JObj [("role", JStr role), ("content", JText text)]
    byName = [ (tName t, t) | t <- chatTools ]
    toolsJson = map toolJson chatTools
    loop spent msgs step cut failures
      | step >= oMaxSteps o = hPutStrLn stderr "[the turn reached its step limit; stopping]"
      | otherwise = do
          t0 <- now
          r <- request e (Request msgs toolsJson (oMaxTokens o) Nothing Nothing Nothing 900)
          t1 <- now
          case r of
            Left why | failures < 2 -> do
              hPutStrLn stderr ("[" ++ why ++ "; asking again in 5s]")
              threadDelay 5000000
              loop spent msgs step cut (failures + 1)
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
                    loop spent (msgs' ++ [msg "user" (T.pack (printf "[harness: your reply was cut off at the output limit of %d tokens before any tool call -- the reasoning ran too long. Go on in smaller steps: act with a tool (eval to count or check, write a smaller piece) instead of working it all out first.]" (oMaxTokens o)))]) (step + 1) (cut + 1) 0
                  else pure ()
                else do
                  modifyIORef' spent (\s -> s { sTools = sTools s + length (pToolCalls p) })
                  replies <- forM (pToolCalls p) $ \tc -> do
                    let name = tcName tc
                        shownArgs = take 300 (encode (tcArgs tc))
                        isSession = name `elem` sessionToolNames
                    putStrLn ("> " ++ name ++ " " ++ shownArgs) >> hFlush stdout
                    unless isSession (logH ch "tool" (T.pack (name ++ " " ++ encode (tcArgs tc))))
                    t2 <- now
                    (ok, out0) <- case lookup name byName of
                      Just t -> runTool ch t (tcArgs tc)
                      Nothing -> pure (False, T.pack ("unknown tool " ++ show name))
                    t3 <- now
                    let out = cap out0
                        tagged = (if ok then T.empty else T.pack "ERROR: ") <> out
                    when (oUsage o) (hPutStrLn stderr (printf "[tool: %s %.1fs]" name (t3 - t2)))
                    unless isSession (logH ch "echo" tagged)
                    TIO.putStrLn (T.pack "  " <> T.replace (T.pack "\n") (T.pack "\n  ") (T.take 600 out) <> (if T.length out > 600 then T.pack "..." else T.empty)) >> hFlush stdout
                    pure (JObj [("role", JStr "tool"), ("tool_call_id", JStr (tcId tc)), ("content", JText tagged)])
                  mid <- drain pending
                  forM_ mid (logH ch "user")
                  loop spent (msgs' ++ replies ++ [ msg "user" (T.intercalate (T.pack "\n\n") mid) | not (null mid) ]) (step + 1) 0 0

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
        let ms = if null members then [name] else members
            ch = Chat conf name (cRoot conf) (map normalise (concat [ strs (targetJson conf m .: "watch") | m <- ms ])) (either (const "Agent") gAgent cfg)
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
