{-# LANGUAGE ScopedTypeVariables #-}
-- | __The model, through the @claude@ command__ (Claude Code, run without its screen): how a subscription is
-- used. A subscription's sign-in is that program's, not a key for the API, so there is no request to make here
-- -- there is a process to run, given a prompt and answering with a stream of events.
--
-- Two things follow from its being a program and not an endpoint.
--
-- * __The loop is its own.__ An endpoint answers one call and the chat runs the tool it asked for; this program
--   runs the tools itself, the ones it is told of. So it is told of none of its own (@--tools ""@) and of ours
--   as a tool server it starts ('mcpConfig': our own executable, relaying to the chat that is running, where
--   the tools are -- "GhciSession.Chat"). The chat still sees every call, since it is the one that answers it.
-- * __Its surroundings are ours to clear.__ It reads the user's settings, memory files and environment. The
--   environment is the dangerous one: @ANTHROPIC_BASE_URL@ and a token, set for some other tool, send it to
--   another provider with another account. So the variables that are its or the API's are taken out ('cliEnv'),
--   its settings are not read (@--setting-sources ""@), and what it would add to the prompt is turned off.
--
-- It speaks lines of JSON both ways (@--input-format@, @--output-format stream-json@): a @user@ line in for
-- each message; out, the API's own events as they come (@stream_event@, the ones "GhciSession.Anthropic"
-- reads), each finished block of a reply (@assistant@), what the subscription's limits say
-- (@rate_limit_event@), and a @result@ when it has no more to do.
module GhciSession.ClaudeCli
  ( CliOpts (..), cliArgs, cliEnv, mcpConfig, serverName, userLine
  , Event (..), readEvent, Result (..), resultOf, limitNote
  , runOnce
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Environment (getEnvironment)
import System.IO (hClose, hFlush, hIsEOF, hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)

import GhciSession.Json

-- | A run of the program. @cMcp@: our executable and the socket the chat's tools are at, when there are tools.
-- @cWeb@: its own web tools are left to it.
data CliOpts = CliOpts { cModel :: String, cEffort :: Maybe String, cSystem :: T.Text, cMcp :: Maybe (FilePath, FilePath), cWeb :: Bool }

-- | The name our tools are served under: the program calls them @mcp__\<name\>__\<tool\>@.
serverName :: String
serverName = "ghs"

-- | The tool server as the program is told of it: a command to start, which is this executable relaying its
-- standard input and output to the chat's socket.
mcpConfig :: FilePath -> FilePath -> Json
mcpConfig exe sock = JObj [("mcpServers", JObj [(serverName, JObj [("type", JStr "stdio"), ("command", JStr exe), ("args", JArr [JStr "mcp-relay", JStr sock])])])]

-- | The program's arguments. No tools of its own, none of the user's settings or tool servers, no session kept;
-- ours allowed by name, which is all that is allowed (no permission is waived: a tool not named is refused).
cliArgs :: CliOpts -> [String]
cliArgs o =
  [ "-p", "--verbose", "--input-format", "stream-json", "--output-format", "stream-json", "--include-partial-messages"
  , "--setting-sources", "", "--strict-mcp-config", "--disable-slash-commands", "--no-session-persistence"
  , "--model", cModel o ]
  ++ maybe [] (\e -> ["--effort", e]) (cEffort o)
  ++ [ "--tools", if cWeb o then "WebSearch,WebFetch" else "" ]
  ++ [ "--system-prompt", T.unpack (cSystem o) ]
  ++ maybe [] (\(exe, sock) -> ["--mcp-config", encode (mcpConfig exe sock), "--allowedTools", "mcp__" ++ serverName]) (cMcp o)

-- | The environment the program is run in: the caller's, without the variables that are the program's or the
-- API's (a base URL and a token set for another tool would send it elsewhere, on another account; a key would
-- be charged where a subscription was meant), and with what it adds to a prompt, sends home, or changes on the
-- machine turned off. Where it keeps its sign-in (@CLAUDE_CONFIG_DIR@) is left as it is.
cliEnv :: [(String, String)] -> [(String, String)]
cliEnv env0 = [ kv | kv@(k, _) <- env0, not (ours k), k `notElem` map fst quiet ] ++ quiet
  where
    ours k = "ANTHROPIC_" `isPrefixOf` k || "CLAUDE_CODE_" `isPrefixOf` k || k `elem` ["CLAUDECODE", "CLAUDE_AGENT_SDK_VERSION"]
    quiet =
      [ ("CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC", "1"), ("CLAUDE_CODE_DISABLE_AUTO_MEMORY", "1"), ("CLAUDE_CODE_DISABLE_CLAUDE_MDS", "1")
      , ("CLAUDE_CODE_DISABLE_GIT_INSTRUCTIONS", "1"), ("CLAUDE_CODE_DISABLE_ATTACHMENTS", "1"), ("CLAUDE_CODE_DISABLE_TERMINAL_TITLE", "1")
      , ("ENABLE_CLAUDEAI_MCP_SERVERS", "false"), ("DISABLE_TELEMETRY", "1"), ("DISABLE_ERROR_REPORTING", "1"), ("DISABLE_AUTOUPDATER", "1")
      , ("MCP_TOOL_TIMEOUT", "86400000") ]      -- (a tool of ours may be a test run: the program's own limit is a minute)

-- | A message for the program: a line of its input, the content as the API's blocks. A block marked for the
-- cache is marked for an hour: the program marks what it adds for an hour, and the API takes no shorter mark
-- before a longer one (a five-minute mark on the view was a 400: "a ttl='1h' cache_control block must not come
-- after a ttl='5m' cache_control block").
userLine :: [Json] -> B.ByteString
userLine blocks0 = let blocks = map hour blocks0 in encodeBS (JObj [ ("type", JStr "user"), ("message", JObj [("role", JStr "user"), ("content", JArr blocks)]), ("parent_tool_use_id", JNull) ]) <> BC.pack "\n"

hour :: Json -> Json
hour b = case b .: "cache_control" of
  JNull -> b
  _ -> set "cache_control" (JObj [("type", JStr "ephemeral"), ("ttl", JStr "1h")]) b

-- | A line of the program's output. 'EvStream': one of the API's events, as it came. 'EvAssistant': the blocks
-- of a reply that are finished (one block a line, as it sends them). 'EvRate': what the subscription's limits
-- say. 'EvResult': it has no more to do.
data Event = EvInit Json | EvStream Json | EvAssistant [Json] | EvRate Json | EvResult Json | EvOther
  deriving (Eq, Show)

readEvent :: B.ByteString -> Event
readEvent line = case parseJsonBS line of
  Left _ -> EvOther
  Right j -> case fromMaybe "" (lookupStr "type" j) of
    "system" | lookupStr "subtype" j == Just "init" -> EvInit j
    "stream_event" -> EvStream (j .: "event")
    "assistant" | lookupBool "is_api_error_message" j /= Just True -> EvAssistant (lookupArr "content" (j .: "message"))
    "rate_limit_event" -> EvRate (j .: "rate_limit_info")
    "result" -> EvResult j
    _ -> EvOther

-- | How a run ended. @xText@: its last words (or, of a failure, why). @xBusy@: a failure that asking again can
-- mend (the service's, by its status). @xIn@: the prompts' tokens in all, @xCached@ those read from the cache.
-- @xTurns@: the model calls it made.
data Result = Result { xOk :: Bool, xText :: T.Text, xBusy :: Bool, xIn :: Int, xOut :: Int, xCached :: Int, xTurns :: Int, xUsd :: Double }
  deriving (Eq, Show)

resultOf :: Json -> Result
resultOf j =
  let u = j .: "usage"
      n k = maybe 0 round (lookupNum k u) :: Int
      status = maybe 0 round (lookupNum "api_error_status" j) :: Int
  in Result { xOk = lookupBool "is_error" j /= Just True
            , xText = fromMaybe (T.pack (fromMaybe "" (lookupStr "subtype" j))) (lookupText "result" j)
            , xBusy = status `elem` [408, 409, 429] || status >= 500
            , xIn = n "input_tokens" + n "cache_creation_input_tokens" + n "cache_read_input_tokens", xOut = n "output_tokens"
            , xCached = n "cache_read_input_tokens", xTurns = maybe 1 round (lookupNum "num_turns" j), xUsd = fromMaybe 0 (lookupNum "total_cost_usd" j) }

-- | What the subscription's limits say, when it is worth a line: refused (and until when), or a window nearly
-- used. 'Nothing' while there is room.
limitNote :: Json -> IO (Maybe String)
limitNote info = do
  let status = fromMaybe "" (lookupStr "status" info)
      kind = map (\c -> if c == '_' then ' ' else c) (fromMaybe "usage" (lookupStr "rateLimitType" info))
      used = maximum (0 : [ u | (_, w) <- lookupObj "unifiedWindows" info, Just u <- [lookupNum "utilization" w] ])
  at <- case lookupNum "resetsAt" info of
    Just t -> formatTime defaultTimeLocale "%H:%M on %b %e" <$> utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac t))
    Nothing -> pure "?"
  pure $ if status == "rejected" then Just ("the subscription's " ++ kind ++ " limit is reached: it resets at " ++ at)
         else if used >= 0.9 then Just ("the subscription's limits are " ++ show (round (used * 100) :: Int) ++ "% used (the " ++ kind ++ " window resets at " ++ at ++ ")")
         else Nothing

-- | One prompt, one answer: the program run with no tools, the prompt as its one message, its result. For a
-- call that is not a turn (a compaction). 'Left' why it could not be run, did not answer in the time or
-- failed, and whether asking again can mend it.
runOnce :: CliOpts -> FilePath -> [Json] -> Double -> IO (Either (String, Bool) Result)
runOnce o dir blocks secs = do
  env0 <- getEnvironment
  r <- try (createProcess (proc "claude" (cliArgs o)) { cwd = Just dir, env = Just (cliEnv env0), std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
  case r of
    Left (e :: IOException) -> pure (Left ("the claude command could not be run (is Claude Code installed, and on the PATH?): " ++ show e, False))
    Right (Just i, Just out, Just err, ph) -> do
      mapM_ (`hSetBinaryMode` True) [i, out, err]
      errV <- newEmptyMVar
      _ <- forkIO (B.hGetContents err >>= putMVar errV)
      void (try (B.hPut i (userLine blocks) >> hFlush i >> hClose i) :: IO (Either IOException ()))
      let loop limit = do
            eof <- hIsEOF out
            if eof then pure (Nothing, limit) else do
              l <- BC.hGetLine out
              case readEvent l of
                EvResult j -> pure (Just (resultOf j), limit)
                EvRate info | lookupStr "status" info == Just "rejected" -> limitNote info >>= \n -> loop (maybe limit Just n)
                _ -> loop limit
      got <- timeout (round (secs * 1e6)) (try (loop Nothing) :: IO (Either SomeException (Maybe Result, Maybe String)))
      terminateProcess ph
      _ <- waitForProcess ph
      said <- T.unpack . T.strip . TE.decodeUtf8With (\_ _ -> Just '?') <$> takeMVar errV
      pure $ case got of
        Nothing -> Left ("the claude command did not answer in " ++ show (round secs :: Int) ++ " s", True)
        Just (Left e) -> Left ("the claude command: " ++ show e, True)
        Just (Right (Just res, limit)) | xOk res -> Right res
                                       | otherwise -> Left (fromMaybe ("the claude command: " ++ T.unpack (xText res)) limit, xBusy res && limit == Nothing)
        Just (Right (Nothing, limit)) -> Left (fromMaybe ("the claude command ended without a result" ++ (if null said then "" else ": " ++ lastLines said)) limit, False)
    Right _ -> pure (Left ("the claude command could not be run", False))
  where lastLines = unwords . reverse . take 3 . reverse . lines
