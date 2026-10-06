{-# LANGUAGE ScopedTypeVariables #-}
-- | __The session as an agent's tools__: @ghci-session mcp@, a Model Context Protocol server on standard
-- input and output (JSON-RPC 2.0, one message a line), for any agent client -- Claude Code, an IDE, a
-- harness. Each tool is one of the session's operations, sent to the daemon over its socket as the command
-- line sends it, so the daemon logs it to the history like any other request; @view@, @zoom@ and @date@
-- are the memory. Nothing here touches the repl.
--
-- > claude mcp add ghci -- ghci-session mcp            # from the project's directory
module GhciSession.Mcp (mcpMain, Tool (..), tools, call, pick, request) where

import Control.Exception (IOException, SomeException, try)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO
import System.Posix.IO (fdToHandle)

import GhciSession.Config
import GhciSession.Json
import GhciSession.Sys

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

result :: Json -> Json -> Json
result rid r = JObj [("jsonrpc", JStr "2.0"), ("id", rid), ("result", r)]

errorReply :: Json -> Int -> String -> Json
errorReply rid code msg = JObj [("jsonrpc", JStr "2.0"), ("id", rid), ("error", JObj [("code", JNum (fromIntegral code)), ("message", JStr msg)])]

-- | One message: a reply for a request, none for a notification.
handle :: Conf -> Json -> IO (Maybe Json)
handle conf req = case (lookupStr "method" req, req .: "id") of
  (Just "initialize", rid) -> pure (Just (result rid (JObj
    [ ("protocolVersion", JStr (fromMaybe "2025-06-18" (lookupStr "protocolVersion" (req .: "params"))))
    , ("capabilities", JObj [("tools", JObj [("listChanged", JBool False)])])
    , ("serverInfo", JObj [("name", JStr "ghci-session"), ("version", JStr "0.2.0")])
    , ("instructions", JStr instructions) ])))
  (Just "ping", rid) -> pure (Just (result rid (JObj [])))
  (Just "tools/list", rid) -> pure (Just (result rid (JObj [("tools", JArr (map toolJson tools))])))
  (Just "tools/call", rid) -> do
    let ps = req .: "params"
        name = fromMaybe "" (lookupStr "name" ps)
        args = ps .: "arguments"
    r <- try (call conf name args) :: IO (Either SomeException (Bool, T.Text))
    let (ok, out) = either (\e -> (False, T.pack (show e))) id r
    pure (Just (result rid (JObj [("content", JArr [JObj [("type", JStr "text"), ("text", JText out)]]), ("isError", JBool (not ok))])))
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
  [ Tool "eval" "Evaluate a Haskell expression, or run a GHCi command (:t, :i, :browse, :set), in the loaded session. The answer is what GHCi printed." [("expr", ("string", "the expression or command")), ("timeout", ("number", "seconds; a hung evaluation is interrupted (default 600)")), sessionArg] ["expr"]
  , Tool "status" "The session's verdict: OK -- CHECK-PASS, COMPILE-ERROR: n error(s), CHECK-FAIL: n failing; STALE(n) when watched sources differ from the loaded code." [sessionArg] []
  , Tool "typecheck" "Do the sources on disk typecheck? Nothing is loaded; the errors are listed." [sessionArg] []
  , Tool "reload" "Compile and load the sources on disk (a save does this by itself), then the tests unless test is false." [("test", ("boolean", "run the tests after a good load (default true)")), sessionArg] []
  , Tool "test" "Run the project's tests on the loaded code." [("member", ("string", "one member of a composed session")), sessionArg] []
  , Tool "doc" "Find a definition: by name (a typo, a prefix or initials are fine), qualified, or by words of its type or comment. Answers the signature, the comment above it and file:line." [("query", ("string", "the name or words")), ("n", ("number", "how many answers (default 5)")), sessionArg] ["query"]
  , Tool "census" "What the heap holds: every CAF by what it retains (default), the Strings among it (mode strings), what a reload cannot drop (kept), sharing that is missed (dups), or one value alone (expr)." [("mode", ("string", "cafs | strings | kept | dups | mem")), ("expr", ("string", "one value: its bytes, closures and constructors")), ("top", ("number", "how many entries")), sessionArg] []
  , Tool "bench" "Time an IO action in the session: wall, GC, allocation." [("expr", ("string", "the action")), ("timeout", ("number", "seconds; a hung action is interrupted (default 600)")), sessionArg] ["expr"]
  , Tool "mem" "The repl's memory and its servers'." [sessionArg] []
  , Tool "view" "The whole history of this session as one-line summaries, oldest first: `id+n|text`, the n messages from id on. Recent lines cover one message; the older, the more. Read it before starting a task." [("wait", ("number", "seconds to wait for every line to be a summary (default 10)")), sessionArg] []
  , Tool "zoom" "Open line id+n of the view into the two lines of n/2 it was made from; n = 1 gives message id whole." [("id", ("number", "the line's first message")), ("n", ("number", "how many messages it covers")), sessionArg] ["id", "n"]
  , Tool "date" "The date and time of message id." [("id", ("number", "the message")), sessionArg] ["id"]
  , Tool "history" "The last messages of the log, word for word: every request and verdict." [("n", ("number", "how many (default 40)")), ("since", ("number", "from this message id")), sessionArg] []
  , Tool "remember" "Keep a finding in the session's memory for later turns, as your own words: what you learned, decided or left undone." [("text", ("string", "the finding")), sessionArg] ["text"]
  ]

toolJson :: Tool -> Json
toolJson t = JObj
  [ ("name", JStr (tName t)), ("description", JStr (tDesc t))
  , ("inputSchema", JObj [ ("type", JStr "object")
                         , ("properties", JObj [ (k, JObj [("type", JStr ty), ("description", JStr d)]) | (k, (ty, d)) <- tProps t ])
                         , ("required", JArr (map JStr (tReq t))) ]) ]

-- | A tool, as a request to the daemon: ok, and the text.
call :: Conf -> String -> Json -> IO (Bool, T.Text)
call conf name args = do
  let s k = lookupStr k args
      n k = lookupNum k args
      num k = maybe [] (\v -> [(k, JNum v)]) (n k)
      str k k' = maybe [] (\v -> [(k', JStr v)]) (s k)
  picked <- pick conf (s "session")
  case picked of
    Left e -> pure (False, T.pack e)
    Right session -> do
      let go op extra = request conf session (JObj (("op", JStr op) : extra))
          say r = pure (lookupBool "ok" r == Just True, staleNote r <> fromMaybe T.empty (lookupText "out" r))
      case name of
        "eval" -> go "eval" (str "expr" "expr" ++ num "timeout") >>= say
        "status" -> go "status" [] >>= say
        "typecheck" -> go "typecheck" [] >>= say
        "reload" -> go "reload" [("check", JBool (lookupBool "test" args /= Just False))] >>= say
        "test" -> go "check" (str "member" "member") >>= say
        "doc" -> go "doc" ([("words", JArr (map JStr (words (fromMaybe "" (s "query")))))] ++ num "n") >>= say
        "census" -> go "census" ([("mode", JStr (if isJust (s "expr") then "value" else fromMaybe "cafs" (s "mode")))] ++ str "expr" "expr" ++ num "top") >>= say
        "bench" -> go "bench" (str "expr" "expr" ++ num "timeout") >>= say
        "mem" -> go "mem" [] >>= say
        "view" -> go "view" [("wait", JNum (fromMaybe 10 (n "wait")))] >>= say
        "zoom" -> go "zoom" (num "id" ++ num "n") >>= say
        "date" -> go "date" (num "id") >>= say
        "history" -> go "history" (num "n" ++ num "since" ++ [("full", JBool True)]) >>= say
        "remember" -> go "log" [("kind", JStr "talk"), ("text", JStr (fromMaybe "" (s "text")))] >>= say
        _ -> pure (False, T.pack ("unknown tool " ++ show name))
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
  pure (if name `elem` sessionNames conf then Right name else Left ("unknown session " ++ show name ++ "; have " ++ unwords (sessionNames conf)))
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
    Nothing -> pure (JObj [("ok", JBool False), ("out", JStr (name ++ ": no session running (ghci-session start " ++ name ++ ")"))])
    Just fd -> do
      h <- fdToHandle fd
      hSetBinaryMode h True
      B.hPut h (encodeBS req)
      B.hPut h (BC.pack "\n")
      hFlush h
      line <- BC.hGetLine h
      hClose h
      pure (either (\e -> JObj [("ok", JBool False), ("out", JStr ("bad reply from the session: " ++ e))]) id (parseJsonBS line))
