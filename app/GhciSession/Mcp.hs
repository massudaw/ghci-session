{-# LANGUAGE ScopedTypeVariables #-}
-- | __The session as an agent's tools__: @ghci-session mcp@, a Model Context Protocol server on standard
-- input and output (JSON-RPC 2.0, one message a line), for any agent client -- Claude Code, an IDE, a
-- harness. Each tool is one of the session's operations, sent to the daemon over its socket as the command
-- line sends it, so the daemon logs it to the history like any other request; @view@, @zoom@ and @date@
-- are the memory. Nothing here touches the repl.
--
-- > claude mcp add ghci -- ghci-session mcp            # from the project's directory
module GhciSession.Mcp (mcpMain, Tool (..), tools, declaredTool, limited, toolArgs, toolBrief, call, callReach, Reach (..), pick, request, handleWith, serveOn, relayMain) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import System.FilePath ((</>))
import System.IO
import System.Posix.IO (fdToHandle)

import GhciSession.Config
import GhciSession.Declared (Declared (..), Param (..), builtinNames, substitute)
import qualified GhciSession.Know as K
import GhciSession.Json
import GhciSession.Sys
import qualified GhciSession.Vfs as Vfs

-- | Serve until standard input ends.
mcpMain :: Conf -> IO ()
mcpMain conf = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdout (BlockBuffering Nothing)
  let loop = do
        eof <- hIsEOF stdin
        unless eof $ do
          line <- BC.hGetLine stdin
          unless (B.null (BC.strip line)) $
            case parseJsonBS line of
              Left e -> send (errorReply JNull (-32700) ("parse error: " ++ e))
              Right req -> handle conf req >>= mapM_ send
          loop
  loop
  where send j = B.hPut stdout (encodeBS j) >> B.hPut stdout (BC.pack "\n") >> hFlush stdout

-- | Serve one connection: a message a line in, a reply a line out, until it closes.
serveOn :: Handle -> (Json -> IO (Maybe Json)) -> IO ()
serveOn h answer = do
  hSetBinaryMode h True
  hSetBuffering h (BlockBuffering Nothing)
  let send j = B.hPut h (encodeBS j) >> B.hPut h (BC.pack "\n") >> hFlush h
      loop = do
        eof <- hIsEOF h
        unless eof $ do
          line <- BC.hGetLine h
          unless (B.null (BC.strip line)) $
            case parseJsonBS line of
              Left e -> send (errorReply JNull (-32700) ("parse error: " ++ e))
              Right req -> answer req >>= mapM_ send
          loop
  r <- try loop :: IO (Either IOException ())
  either (const (pure ())) pure r

-- | @ghci-session mcp-relay SOCKET@: standard input and output joined to a unix socket, a line at a time each
-- way, until either side ends. What an agent program is given to start as its tool server when the server is a
-- process already running (the chat): it starts this, and this is a wire.
relayMain :: FilePath -> IO Int
relayMain path = do
  m <- unixConnect path
  case m of
    Nothing -> hPutStrLn stderr ("mcp-relay: cannot connect to " ++ path) >> pure 1
    Just fd -> do
      h <- fdToHandle fd
      mapM_ (`hSetBinaryMode` True) [h, stdin, stdout]
      hSetBuffering h (BlockBuffering Nothing)
      hSetBuffering stdout (BlockBuffering Nothing)
      done <- newEmptyMVar
      let pump from to = do
            r <- try (let go = do { eof <- hIsEOF from; unless eof (BC.hGetLine from >>= \l -> B.hPut to l >> B.hPut to (BC.pack "\n") >> hFlush to >> go) } in go) :: IO (Either IOException ())
            either (const (pure ())) pure r
            putMVar done ()
      _ <- forkIO (pump stdin h)
      _ <- forkIO (pump h stdout)
      takeMVar done
      pure 0

result :: Json -> Json -> Json
result rid r = JObj [("jsonrpc", JStr "2.0"), ("id", rid), ("result", r)]

errorReply :: Json -> Int -> String -> Json
errorReply rid code msg = JObj [("jsonrpc", JStr "2.0"), ("id", rid), ("error", JObj [("code", JNum (fromIntegral code)), ("message", JStr msg)])]

-- | One message: a reply for a request, none for a notification.
handle :: Conf -> Json -> IO (Maybe Json)
handle conf = handleWith offered run (\_ -> pure [])
  where
    -- (tools/list is for the whole project and not a session: the default session's tools are what is served)
    session = cDefault conf
    offered = limited (builtinToolsOf conf session) tools
              ++ [ (declaredTool d) { tDesc = dDesc d ++ " [declared by the project's session " ++ session ++ ", where it runs]" } | d <- declaredOf conf session ]
    run name args
      | name `notElem` map tName offered = pure (False, T.pack ("tool " ++ show name ++ " is not offered by this project (\"builtin_tools\" of its target)"))
      | otherwise = call conf name args

-- | A declared tool as the tool list has it.
declaredTool :: Declared -> Tool
declaredTool d = Tool (dName d) (dDesc d) [ (pName p, (pType p, pDesc p)) | p <- dParams d ] (dRequired d)

-- | The built-in tools a project offers: all, or those its "builtin_tools" names.
limited :: Maybe [String] -> [Tool] -> [Tool]
limited Nothing ts = ts
limited (Just ns) ts = [ t | t <- ts, tName t `elem` ns ]

-- | The same server over other tools: the ones given, each run by the function given. The last argument: blocks to
-- send after an answer's text. (The chat serves its own
-- this way, to an agent program that runs its tools itself -- "GhciSession.Chat".)
handleWith :: [Tool] -> (String -> Json -> IO (Bool, T.Text)) -> (T.Text -> IO [Json]) -> Json -> IO (Maybe Json)
handleWith served run more req = case (lookupStr "method" req, req .: "id") of
  (Just "initialize", rid) -> pure (Just (result rid (JObj
    [ ("protocolVersion", JStr (fromMaybe "2025-06-18" (lookupStr "protocolVersion" (req .: "params"))))
    , ("capabilities", JObj [("tools", JObj [("listChanged", JBool True)])])
    , ("serverInfo", JObj [("name", JStr "ghci-session"), ("version", JStr "0.2.0")])
    , ("instructions", JStr instructions) ])))
  (Just "ping", rid) -> pure (Just (result rid (JObj [])))
  (Just "tools/list", rid) -> pure (Just (result rid (JObj [("tools", JArr (map toolJson served))])))
  (Just "tools/call", rid) -> do
    let ps = req .: "params"
        name = fromMaybe "" (lookupStr "name" ps)
        args = ps .: "arguments"
    r <- try (run name args) :: IO (Either SomeException (Bool, T.Text))
    let (ok, out) = either (\e -> (False, T.pack (show e))) id r
    -- (what goes with the answer's text: the images it names, for the chat's tools)
    extra <- either (\(_ :: SomeException) -> []) id <$> try (more out)
    pure (Just (result rid (JObj [("content", JArr (JObj [("type", JStr "text"), ("text", JText out)] : extra)), ("isError", JBool (not ok))])))
  (Just m, JNull) | "notifications/" `isPrefixOfS` m -> pure Nothing      -- initialized, cancelled: nothing to say
  (Just m, rid) -> pure (Just (errorReply rid (-32601) ("unknown method " ++ show m)))
  (Nothing, rid) -> pure (Just (errorReply rid (-32600) "no method"))
  where isPrefixOfS p s = take (length p) s == p

instructions :: String
instructions = unlines
  [ "A warm GHCi session on this project, already loaded, with its memory. eval runs an expression (or a GHCi"
  , "command: `:t x`, `:i T`, `:browse M`) against the LOADED code in milliseconds; a save of a source file is"
  , "reloaded by the session itself and its verdict is in status (a reply marked STALE came from code that is"
  , "no longer on disk). typecheck, reload and test are the three questions: do the sources typecheck, load"
  , "them, run the project's tests. doc finds a definition by name, initials or words of its type or comment,"
  , "whether or not it compiles. view is the whole history of what was done to this code, oldest first, as"
  , "one-line summaries `id+n|text`; zoom opens a line into the two under it (n = 1: the message whole); zoom"
  , "before you act, guess or ask. remember keeps a finding for later turns: say what you learned that will"
  , "matter." ]

data Tool = Tool { tName :: String, tDesc :: String, tProps :: [(String, (String, String))], tReq :: [String] }

sessionArg :: (String, (String, String))
sessionArg = ("session", ("string", "the session, when the project has several (else the one running, or the default)"))

tools :: [Tool]
tools =
  [ Tool "eval" "Evaluate a Haskell expression, or run a GHCi command (:t, :i, :browse, :set), in the loaded session. The answer is what GHCi printed." [("expr", ("string", "the expression or command")), ("timeout", ("number", "seconds; a hung evaluation is interrupted (default 30)")), sessionArg] ["expr"]
  , Tool "status" "The session's verdict: OK -- CHECK-PASS, COMPILE-ERROR: n error(s), CHECK-FAIL: n failing; STALE(n) when watched sources differ from the loaded code." [sessionArg] []
  , Tool "typecheck" "Do the sources on disk typecheck? Nothing is loaded; the errors are listed." [sessionArg] []
  , Tool "reload" "Compile and load the sources on disk (a save does this by itself), then the tests unless test is false." [("test", ("boolean", "run the tests after a good load (default true)")), sessionArg] []
  , Tool "hold" "Before writing SEVERAL files: saves are not reloaded until release (or the timeout), so the batch is one reload instead of one per file. Write the files, then call release." [("timeout", ("number", "seconds before the hold ends by itself (default 30, at most 600)")), sessionArg] []
  , Tool "release" "End a hold: reload what was saved since it, once, and answer with that verdict." [sessionArg] []
  , Tool "test" "Run the project's tests on the loaded code -- the whole check, which sets the session's verdict; or, with expr, ONE expression run as a test (a group of the tests, say), scored by the check's own fail and pass patterns, the verdict left as it is: seconds instead of the whole suite." [("expr", ("string", "an expression to run as a test instead of the whole check, e.g. a test group")), ("member", ("string", "one member of a composed session")), ("timeout", ("number", "seconds, with expr (default 30)")), sessionArg] []
  , Tool "doc" "Find a definition: by name (a typo, a prefix or initials are fine), qualified, or by words of its type or comment. Answers the signature, the comment above it and file:line." [("query", ("string", "the name or words")), ("n", ("number", "how many answers (default 5)")), sessionArg] ["query"]
  , Tool "census" "What the heap holds: every CAF by what it retains (default), the Strings among it (mode strings), what a reload cannot drop (kept), sharing that is missed (dups), or one value alone (expr)." [("mode", ("string", "cafs | strings | kept | dups | mem")), ("expr", ("string", "one value: its bytes, closures, and what it is made of by constructor -- how to see what a data structure holds, in a fraction of a second")), ("top", ("number", "how many entries")), ("sites", ("boolean", "with expr: by WHERE each part was allocated -- constructor @ file:line (binding) -- in a session of its own that has the code compiled so (the first call compiles it); a thunk or a function is named by where it is defined")), ("opt", ("number", "with sites: the optimisation level of that session (default 1)")), sessionArg] []
  , Tool "bench" "Time an IO action in the session, or (opt, unit) in one that has the code at an optimisation level: wall, GC, allocation (live: and the live heap before and after, two major collections). A top-level value is computed once per load, so time a function applied to its input." [("expr", ("string", "the action")), ("live", ("boolean", "also the live heap before and after")), ("runs", ("number", "run it this many times and say the best, the median and the worst wall time, the median GC, and what a run allocates: for an action of milliseconds, where one run says little")), ("timeout", ("number", "seconds; a hung action is interrupted (default 30)")), ("opt", ("number", "the optimisation level to measure at (0, 1 or 2): the code is loaded at that level in a session of its own beside this one, kept warm and reloaded with what changed -- what a built executable at -O2 would measure, without building one")), ("unit", ("string", "a component of the build to load beside the code, whose own code the action uses: exe:NAME, bench:NAME, test:NAME (its modules are in scope; `:main ARGS` runs its main)")), sessionArg] ["expr"]
  , Tool "mem" "The repl's memory and its servers'." [sessionArg] []
  , Tool "view" "The whole history of this session as one-line summaries, oldest first: `id+n|text`, the n messages from id on. Recent lines cover one message; the older, the more. Read it before starting a task." [("wait", ("number", "seconds to wait for every line to be a summary (default 10)")), sessionArg] []
  , Tool "zoom" "Open the line id+n of the view into the two lines of n/2 under it; n = 1 gives the message whole." [("id", ("number", "the line's first message")), ("n", ("number", "how many messages it covers")), sessionArg] ["id", "n"]
  , Tool "date" "The date and time of message id." [("id", ("number", "the message")), sessionArg] ["id"]
  , Tool "history" "The last messages of the log, word for word: every request and verdict." [("n", ("number", "how many (default 40)")), ("since", ("number", "from this message id")), sessionArg] []
  , Tool "recall" "Find by words: the facts known by subject (from every project's sessions) and the messages of this session's log that hold them, best first. For a detail -- a name, a figure, an error, what was decided -- ask this before walking the view down; zoom id 1 then gives a message found whole." [("query", ("string", "the words to look for")), ("n", ("number", "how many of each (default 8)")), sessionArg] ["query"]
  , Tool "remember" "Keep a finding in the session's memory for later turns, as your own words: what you learned, decided or left undone." [("text", ("string", "the finding")), sessionArg] ["text"]
  , Tool "vfs" "Virtual File System & Line Budget inspector. Inspect line counts, byte sizes, budget compliance (<250 lines), and git status for files loaded by the session or matching a path." [("path", ("string", "optional path or pattern filter (e.g. 'src', or empty for all loaded files)")), ("budget", ("number", "line budget threshold to check against (default 250)")), sessionArg] []
  , Tool "restart" "Restart the GHCi session daemon cold. Re-runs cabal repl and reloads the project from scratch. Use this if the session is wedged, crashed, or after fundamental build-configuration changes." [("fast", ("boolean", "skip build tool check and restart immediately (default false)")), sessionArg] []
  ]

-- | The (tool, argument) pairs the schemas of these tools describe.
toolArgs :: [Tool] -> [(String, String)]
toolArgs ts = [ (tName t, k) | t <- ts, (k, _) <- tProps t ]

-- | A tool in one line, for a prompt: @name(arguments): the first sentence of what it does@ (at most 140 characters).
toolBrief :: Tool -> String
toolBrief t = tName t ++ "(" ++ intercalate ", " [ k | (k, _) <- tProps t, k /= "session" ] ++ "): " ++ cut (sentence (tDesc t))
  where sentence (c : '.' : ' ' : _) = [c, '.']
        sentence (c : r) = c : sentence r
        sentence [] = []
        cut s = if length s > 140 then take 137 s ++ "..." else s

toolJson :: Tool -> Json
toolJson t = JObj
  [ ("name", JStr (tName t)), ("description", JStr (tDesc t))
  , ("inputSchema", JObj [ ("type", JStr "object")
                         , ("properties", JObj [ (k, JObj (if ty == "strings" then [("type", JStr "array"), ("items", JObj [("type", JStr "string")]), ("description", JStr d)] else [("type", JStr ty), ("description", JStr d)])) | (k, (ty, d)) <- tProps t ])
                         , ("required", JArr (map JStr (tReq t))) ]) ]

-- | An action timed in the session that has the base session's code at an optimisation level, and a component
-- of the build beside it ('benchSession') -- started here if it is not up, reloaded (the sources may have
-- changed since it last was asked: it does not reload on a save), then asked. So a measurement at the level a
-- built executable has is a call, not a build of that executable and a process: the code is compiled once,
-- then only what changed, and stays loaded.
benchAt :: Conf -> String -> Json -> IO (Bool, T.Text, Reach)
benchAt conf base args = do
  let level = maybe (if lookupStr "__op" args == Just "census" then 1 else 2) round (lookupNum "opt" args) :: Int
      units = maybe [] (\u -> [u]) (lookupStr "unit" args)
      sites = lookupBool "sites" args == Just True
      census = lookupStr "__op" args == Just "census"
      name0 = benchSession base level units
      -- (BASE-O2s: the same, its info tables mapped to the source)
      name = if sites then (\(h, r) -> h ++ "s" ++ r) (break (== '+') name0) else name0
      ask op extra = request conf name (JObj (("op", JStr op) : ("quiet", JBool True) : extra))
      text r = fromMaybe T.empty (lookupText "out" r)
  t0 <- now
  up <- isJust <$> running conf name
  let tmoUs = round ((fromMaybe 30 (lookupNum "timeout" args) :: Double) * 1000000) :: Int
      sdir = stateOf conf name
      -- (what the session's log last said, and its verdict line: where a wait that ran out, or a boot that failed, is looked at)
      logTail = do
        r <- try (withBinaryFile (sdir </> "daemon.log") ReadMode (\h -> do
                    sz <- hFileSize h
                    hSeek h AbsoluteSeek (max 0 (sz - 4000))
                    BC.hGetContents h)) :: IO (Either IOException B.ByteString)
        pure (either (const "(no log)") (\b -> case filter (not . B.null) (BC.lines b) of { [] -> "(empty log)"; ls -> BC.unpack (last ls) }) r)
      verdictLine = maybe "" (takeWhile (/= '\n')) <$> readFileMaybe (sdir </> "status")
      -- the call's timeout bounds the WAIT for the session to be there and loaded as it is now, not the compile: the
      -- session goes on in its own process, and the next call finds it further (or done)
      load = do
        started <- if up then pure (Right ()) else do
          exe <- getExecutablePath
          r <- try (readProcessWithExitCode exe ["--root", cRoot conf, "start", name, "--no-check"] "") :: IO (Either IOException (ExitCode, String, String))
          case r of
            Right (ExitSuccess, _, _) -> pure (Right ())
            Right (_, out, err) -> do
              v <- verdictLine
              pure (Left (T.pack (unlines ((if null v then [] else ["its status: " ++ v]) ++ lastN 14 (lines (out ++ err))))))
            Left e -> pure (Left (T.pack (show e)))
        case started of
          Left why -> pure (Left why)
          -- (a session that was started by an earlier call and is still compiling has its process but not its socket:
          -- the wait is for it, up to the call's timeout, not an answer at once that it is not there)
          Right () -> let again = do
                            r <- ask "reload" [("check", JBool False), ("refork", JBool False)]
                            alive <- isJust <$> running conf name
                            if lookupStr "down" r == Just "before" && alive then threadDelay 500000 >> again else pure r
                      in Right <$> again
  let stillCompiling = do
        lg <- logTail
        v <- verdictLine
        tn <- now
        pure (False, T.pack ("[in " ++ name ++ ": " ++ (if up then "reloading the code at -O" ++ show level else "the FIRST compile of the code at -O" ++ show level ++ " is under way")
                             ++ ", " ++ show (round (tn - t0) :: Int) ++ " s so far -- not finished at the call's timeout]\n"
                             ++ "Nothing was measured. The session goes on in the background: ask the same again later and it picks up where it is (a first -O2 compile of a large project takes many minutes).\n"
                             ++ (if null v then "" else "its status: " ++ v ++ "\n") ++ "its log last said: " ++ lg ++ "\n"), Reached)
  loaded <- timeout tmoUs load
  t1 <- now
  case loaded of
    Nothing -> stillCompiling
    -- (booted, but not listening yet: its daemon is there, its socket not -- the same wait)
    Just (Right rl) | lookupStr "down" rl == Just "before", up -> stillCompiling
    Just (Left why) -> pure (False, T.pack ("bench: the session to measure in (" ++ name ++ ") did not start:\n") <> why, Reached)
    Just (Right rl) -> do
      -- (what it loaded to: the verdict the reload left, which is in its answer's status)
      let kind = fromMaybe "" (lookupStr "kind" (rl .: "status"))
          bad = kind `elem` ["COMPILE-ERROR", "DEAD", "CONFIG-ERROR", "PREBUILD-ERROR"] || lookupBool "ok" rl == Just False
          hd = T.pack ("[in " ++ name ++ ": the code at -O" ++ show level ++ (if sites then ", its info tables mapped to the source" else "") ++ concatMap (", with " ++) units ++ (if up then "" else "; started")
                       ++ (if t1 - t0 >= 1 then "; " ++ show (round (t1 - t0) :: Int) ++ " s to have it loaded as it is now" else "") ++ "]\n")
      if bad then pure (False, hd <> T.pack "the code as it is now does not load there, so nothing was measured (what is loaded is from before):\n" <> T.unlines (lastN 14 (T.lines (text rl))), Reached) else do
        r <- if census
          then ask "census" ([ ("mode", JStr (if isJust (lookupStr "expr" args) then "value" else fromMaybe "cafs" (lookupStr "mode" args))) ]
                             ++ [ ("expr", JStr e) | Just e <- [lookupStr "expr" args] ] ++ [ ("top", JNum v) | Just v <- [lookupNum "top" args] ])
          else ask "bench" ([ ("expr", JStr e) | Just e <- [lookupStr "expr" args] ] ++ [ ("timeout", JNum v) | Just v <- [lookupNum "timeout" args] ]
                           ++ [ ("live", JBool True) | lookupBool "live" args == Just True ] ++ [ ("runs", JNum v) | Just v <- [lookupNum "runs" args] ])
        pure (lookupBool "ok" r == Just True, hd <> text r, Reached)
  where
    lastN :: Int -> [a] -> [a]
    lastN k xs = drop (length xs - k) xs

-- | A tool, as a request to the daemon: ok, and the text.
call :: Conf -> String -> Json -> IO (Bool, T.Text)
call conf name args = (\(ok, t, _) -> (ok, t)) <$> callReach conf name args

-- | Whether a call reached its session: it did; there was none to reach (nothing was done, so the call can
-- be sent again); or the session went away during it (what it did is not known).
data Reach = Reached | Unreached | Lost deriving (Eq, Show)

-- | A tool, and whether it reached its session: a caller may wait out a session that is restarting.
callReach :: Conf -> String -> Json -> IO (Bool, T.Text, Reach)
callReach conf name args
  -- (a declared tool of the default session: its expression with the arguments in it, an eval)
  | name `notElem` builtinNames, Just d <- lookup name [ (dName x, x) | x <- declaredOf conf (cDefault conf) ] =
      case substitute d args of
        Left why -> pure (False, T.pack (name ++ ": " ++ why), Reached)
        Right expr -> callReach conf "eval" (JObj ([("expr", JStr expr), ("tool", JStr name), ("session", JStr (cDefault conf))] ++ [ ("timeout", JNum t) | Just t <- [dTimeout d] ]
                                              ++ [ (k, v) | k <- ["quiet", "from"], Just v <- [lookup k (fromMaybe [] (obj args))] ]))
  | otherwise = do
  let s k = lookupStr k args
      n k = lookupNum k args
      num k = maybe [] (\v -> [(k, JNum v)]) (n k)
      str k k' = maybe [] (\v -> [(k', JStr v)]) (s k)
  picked <- pick conf (s "session")
  case picked of
    Left e -> pure (False, T.pack e, Reached)
    Right session -> do
      let go op extra = request conf session (JObj (("op", JStr op) : extra ++ [ ("quiet", JBool True) | lookupBool "quiet" args == Just True ] ++ [ ("from", JStr f) | Just f <- [s "from"] ]))
          say r = pure (lookupBool "ok" r == Just True, staleNote r <> fromMaybe T.empty (lookupText "out" r)
                       , case lookupStr "down" r of { Just "before" -> Unreached; Just _ -> Lost; Nothing -> Reached })
      case name of
        "eval" -> go "eval" (str "expr" "expr" ++ str "tool" "tool" ++ num "timeout") >>= say
        "status" -> go "status" [] >>= say
        "typecheck" -> go "typecheck" [] >>= say
        "reload" -> go "reload" [("check", JBool (lookupBool "test" args /= Just False))] >>= say
        "hold" -> go "hold" [ ("secs", JNum v) | Just v <- [n "timeout"] ] >>= say
        "release" -> go "release" [] >>= say
        "test" | Just _ <- s "expr" -> go "check_expr" (str "expr" "expr" ++ str "member" "member" ++ num "timeout") >>= say
               | otherwise -> go "check" (str "member" "member") >>= say
        "doc" -> go "doc" ([("words", JArr (map JStr (words (fromMaybe "" (s "query")))))] ++ num "n") >>= say
        -- by where it was allocated: in the session that has the code so compiled ('benchAt')
        "census" | lookupBool "sites" args == Just True -> benchAt conf session (set "__op" (JStr "census") args)
        "census" -> go "census" ([("mode", JStr (if isJust (s "expr") then "value" else fromMaybe "cafs" (s "mode")))] ++ str "expr" "expr" ++ num "top") >>= say
        -- at a level, or with a component of the build beside the code: in the session that has it so ('benchAt')
        "bench" | isJust (n "opt") || isJust (s "unit") -> benchAt conf session args
        "bench" -> go "bench" (str "expr" "expr" ++ num "timeout" ++ num "runs" ++ [ ("live", JBool True) | lookupBool "live" args == Just True ]) >>= say
        "mem" -> go "mem" [] >>= say
        "view" -> go "view" [("wait", JNum (fromMaybe 10 (n "wait")))] >>= say
        "zoom" -> go "zoom" (num "id" ++ num "n") >>= say
        "date" -> go "date" (num "id") >>= say
        "history" -> go "history" (num "n" ++ num "since" ++ [("full", JBool True)]) >>= say
        "recall" -> do
          let q = T.pack (fromMaybe "" (s "query"))
              k = maybe 8 round (n "n") :: Int
          facts <- either (const []) id <$> (try (K.knowDir >>= K.loadFacts) :: IO (Either SomeException [K.Fact]))
          let found = take k (K.search q (K.current facts))
              known = [ T.pack "known:" | not (null found) ] ++ [ T.pack ("- [" ++ K.day (K.fFirst f) ++ "] (") <> K.fSubject f <> T.pack ") " <> K.fText f | f <- found ]
          r <- go "recall" ([("query", JText q)] ++ num "n")
          (ok, out, reach) <- say r
          pure (ok || not (null found), T.intercalate (T.pack "\n") (known ++ [ T.pack "messages:" | ok ] ++ [out]), reach)
        "remember" -> go "log" [("kind", JStr "talk"), ("text", JStr (fromMaybe "" (s "text")))] >>= say
        "vfs" -> do
          let mPath = s "path"
              budget = maybe 250 round (n "budget") :: Int
          files <- Vfs.inspectLoaded conf session budget mPath
          pure (True, Vfs.formatVfsTable files budget, Reached)
        "restart" -> go "restart" [("fast", JBool (lookupBool "fast" args == Just True))] >>= say
        _ -> pure (False, T.pack ("unknown tool " ++ show name), Reached)
  where
    staleNote r = case strs (r .: "stale") of
      [] -> T.empty
      st -> T.pack ("[STALE: " ++ show (length st) ++ " watched file(s) differ from the loaded code (e.g. " ++ head st ++ "): this answer is from the code before them; the session reloads a save by itself, see status]\n")

-- | The session a tool is for: the one named, else the only one running, else the default. (The command
-- line's rule; here an error is an answer, not an exit.)
pick :: Conf -> Maybe String -> IO (Either String String)
pick conf mname = do
  up <- filterMIO (\n -> isJust <$> running conf n) (sessionNames conf)
  let name = fromMaybe (case up of { [one] -> one; _ -> cDefault conf }) mname
  pure (if knownSession conf name then Right name else Left ("unknown session " ++ show name ++ "; have " ++ unwords (sessionNames conf)))
  where filterMIO p = fmap concat . mapM (\x -> (\b -> [x | b]) <$> p x)

running :: Conf -> String -> IO (Maybe Int)
running conf name = do
  let f = cStateDir conf </> name </> "pid"
  there <- doesFileExist f
  if not there then pure Nothing else do
    t <- either (\(_ :: IOException) -> "") id <$> try (readFile f)
    case reads t of
      [(pid, _)] -> (\a -> if a then Just pid else Nothing) <$> pidAlive pid
      _ -> pure Nothing

request :: Conf -> String -> Json -> IO Json
request conf name req = do
  sp <- sockPath (cStateDir conf </> name)
  mfd <- unixConnect sp
  case mfd of
    Nothing -> pure (JObj [("ok", JBool False), ("down", JStr "before"), ("out", JStr (name ++ ": no session running (ghci-session start " ++ name ++ ")"))])
    Just fd -> do
      -- (a session that stops or restarts during the request closes the connection: an answer, not an exception)
      r <- try $ do
        h <- fdToHandle fd
        hSetBinaryMode h True
        B.hPut h (encodeBS req)
        B.hPut h (BC.pack "\n")
        hFlush h
        line <- BC.hGetLine h
        hClose h
        pure line
      pure $ case r of
        Left (e :: IOException) -> JObj [("ok", JBool False), ("down", JStr "during"), ("out", JStr (name ++ ": the session closed the connection before answering (it stopped or restarted): " ++ show e))]
        Right line -> either (\e -> JObj [("ok", JBool False), ("out", JStr ("bad reply from the session: " ++ e))]) id (parseJsonBS line)
