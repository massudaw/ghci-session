{-# LANGUAGE ScopedTypeVariables #-}
-- | __Chats had elsewhere, into the history__: @ghci-session import@. What was done to this project in Claude
-- Code and in Codex is in their own session files, one a session, a line an event; read here into the
-- messages the history is made of, so that the view starts from them too.
--
-- A session becomes a @note@ saying what it is (the program, the directory, when it began) and then its
-- messages: the user's words as @user@, the other agent's as @ai@ -- not @talk@, which is this agent's own --
-- and, when asked for (@--tools@), its tool calls and their results as @tool@ and @echo@. What a program put
-- into a chat and nobody said is left out: a command's echo, an interrupted request, a reminder.
--
-- Two things decide what is taken. __Whose session it is__: a program that drives the model through its SDK
-- leaves a session file for every call -- of the files of the project this was written on, 1,132 of 1,137 were
-- a compactor's, a prompt and a line each -- so a session started by an SDK is passed over unless asked for
-- (@--all@), and so is one of a single exchange. __What was taken already__: each session's last imported
-- message is kept (@history\/imported.json@), and a second import takes only what is newer.
--
-- Nothing is written until it is asked for (@--go@): the plan is shown first, with what its summaries will
-- cost in model calls, since every imported message over a line's size is one.
module GhciSession.Import
  ( Session (..), Entry (..), Want (..), readSession, claudeDir, sessionFiles
  , claudeLine, codexLine, isoSeconds
  , wanted, plan, sessionNote, sessionKey
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAlphaNum, isAsciiLower)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, parseTimeM, utcToLocalZonedTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

import GhciSession.Json

-- | A message of a session: its kind as the history has kinds, its text, when it was said (seconds).
data Entry = Entry { eKind :: String, eText :: T.Text, eDate :: Double }
  deriving (Eq, Show)

-- | A session read: the program it is of, how it was started (@cli@, @claude-desktop@, @sdk-ts@, ...), where,
-- its file, its messages in order.
data Session = Session { sApp :: String, sEntry :: String, sWhere :: FilePath, sFile :: FilePath, sEntries :: [Entry] }
  deriving (Eq, Show)

-- | Where Claude Code keeps a project's sessions: @~\/.claude\/projects\/@ and the project's path with every
-- character that is not a letter or a digit made a dash.
claudeDir :: FilePath -> FilePath -> FilePath
claudeDir home root = home </> ".claude" </> "projects" </> map (\c -> if isAlphaNum c then c else '-') root

-- | The session files under a directory (@.jsonl@, any depth), but not a subagent's: that is its parent's work.
sessionFiles :: FilePath -> IO [FilePath]
sessionFiles dir = do
  there <- doesDirectoryExist dir
  if not there then pure [] else do
    names <- either (\(_ :: IOException) -> []) id <$> try (listDirectory dir)
    fmap concat $ forM names $ \n -> do
      let p = dir </> n
      isDir <- doesDirectoryExist p
      if isDir then (if n == "subagents" then pure [] else sessionFiles p)
        else pure [ p | ".jsonl" `isSuffixOf` n ]

-- | A session file read. 'Nothing': no message in it.
readSession :: FilePath -> IO (Maybe Session)
readSession file = do
  ok <- doesFileExist file
  b <- if ok then either (\(_ :: IOException) -> B.empty) id <$> try (B.readFile file) else pure B.empty
  let js = [ j | l <- BC.lines b, not (B.null l), Right j <- [parseJsonBS l] ]
      codex = any (\j -> lookupStr "type" j == Just "session_meta") (take 5 js)
      step (s, lastT, acc) j =
        let t = fromMaybe lastT (stamp j)
            (s', es) = if codex then codexLine s j t else claudeLine s j t
        in (s', t, reverse es ++ acc)
      (s1, _, es) = foldl step (Session (if codex then "Codex" else "Claude Code") "" "" file [], 0, []) js
  pure (if null es then Nothing else Just s1 { sEntries = reverse es })
  where stamp j = case j .: "timestamp" of
          JNum n -> Just n
          JText t -> isoSeconds (T.unpack t)
          _ -> Nothing

-- | @2026-10-08T09:08:23.523Z@ as seconds.
isoSeconds :: String -> Maybe Double
isoSeconds t = realToFrac . utcTimeToPOSIXSeconds <$> (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" t :: Maybe UTCTime)

-- | A line of a Claude Code session: the session with what the line says of it, and the line's messages. Only
-- what the user and the agent said in the session itself: not a line the program put there (@isMeta@), a
-- subagent's (@isSidechain@), or the summary it made of a chat it cut (@isCompactSummary@).
claudeLine :: Session -> Json -> Double -> (Session, [Entry])
claudeLine s j t =
  let ty = fromMaybe "" (lookupStr "type" j)
      flag k = lookupBool k j == Just True
      own = ty `elem` ["user", "assistant"] && not (flag "isMeta") && not (flag "isSidechain") && not (flag "isCompactSummary")
      s' = s { sWhere = if null (sWhere s) then fromMaybe "" (lookupStr "cwd" j) else sWhere s
             , sEntry = if null (sEntry s) then fromMaybe "" (lookupStr "entrypoint" j) else sEntry s }
      blocks = case j .: "message" .: "content" of
        JText x -> [JObj [("type", JStr "text"), ("text", JText x)]]
        JArr bs -> bs
        _ -> []
      said = T.intercalate (T.pack "\n") [ x | b <- blocks, lookupStr "type" b == Just "text", Just x <- [lookupText "text" b], not (injected x) ]
      tools = concat [ case lookupStr "type" b of
                         Just "tool_use" -> [Entry "tool" (T.pack (fromMaybe "?" (lookupStr "name" b) ++ " " ++ encode (b .: "input"))) t]
                         Just "tool_result" -> [Entry "echo" (partsText (b .: "content")) t]
                         _ -> []
                     | b <- blocks ]
  in if not own then (s', [])
     else (s', [ Entry (if ty == "user" then "user" else "ai") said t | not (T.null (T.strip said)) ] ++ [ e | e <- tools, not (T.null (T.strip (eText e))) ])
  where
    -- what the program wrote into the chat: a command and its output, an interruption, a reminder
    injected x = any (`T.isPrefixOf` T.stripStart x) (map T.pack ["<command-", "<local-command", "[Request interrupted", "<system-reminder>"])

-- | A line of a Codex session. Its first says where the session is (and whether it is a subagent's, or the
-- program's own: then nothing of it is taken); a @response_item@ is a message, a tool call or its result.
codexLine :: Session -> Json -> Double -> (Session, [Entry])
codexLine s j t =
  let p = j .: "payload"
      inner = case (p .: "source" .: "subagent", p .: "source" .: "internal") of { (JNull, JNull) -> False; _ -> True }
  in case fromMaybe "" (lookupStr "type" j) of
       "session_meta" -> (s { sWhere = if inner then "" else fromMaybe "" (lookupStr "cwd" p), sEntry = fromMaybe "codex" (lookupStr "originator" p) }, [])
       "response_item" | not (null (sWhere s)) -> (s, item p)
       _ -> (s, [])
  where
    item p = case fromMaybe "" (lookupStr "type" p) of
      "message" -> case lookupStr "role" p of
        Just r | r `elem` ["user", "assistant"] ->
          let x = T.intercalate (T.pack "\n") [ c | b <- lookupArr "content" p, Just c <- [lookupText "text" b], not (injected c) ]
          in [ Entry (if r == "user" then "user" else "ai") x t | not (T.null (T.strip x)) ]
        _ -> []
      "function_call" -> [Entry "tool" (T.pack (fromMaybe "?" (lookupStr "name" p) ++ " " ++ fromMaybe "" (lookupStr "arguments" p))) t]
      "custom_tool_call" -> [Entry "tool" (T.pack (fromMaybe "?" (lookupStr "name" p) ++ " " ++ encode (JObj [("input", p .: "input")]))) t]
      "local_shell_call" -> [Entry "tool" (T.pack ("shell " ++ encode (JObj [("command", p .: "action" .: "command")]))) t]
      ty | ty `elem` ["function_call_output", "custom_tool_call_output"] -> [ Entry "echo" x t | let x = partsText (p .: "output"), not (T.null (T.strip x)) ]
      _ -> []
    -- what the program put in a user's turn: a tagged block (its environment, its instructions), the project's notes
    injected c = let x = T.unpack (T.take 24 (T.stripStart c)) in "# AGENTS.md" `isPrefixOf` x || tagged x
    tagged x = case x of
      ('<' : r) -> let (name, rest) = span (\c -> isAsciiLower c || c == '_') r in not (null name) && take 1 rest `elem` [" ", ">"]
      _ -> False

-- | A tool result's text: a string, or the text parts of a list (an image is said to be one).
partsText :: Json -> T.Text
partsText c = case c of
  JText x -> x
  JArr ps -> T.intercalate (T.pack "\n") (mapMaybe part ps)
  JObj _ -> fromMaybe T.empty (lookupText "text" c)
  _ -> T.empty
  where part b = case b of
          JText x -> Just x
          _ | Just x <- lookupText "text" b -> Just x
            | maybe False ("image" `isInfixOf`) (lookupStr "type" b) -> Just (T.pack "[image]")
            | otherwise -> Nothing

-- the plan --------------------------------------------------------------------------------------

-- | What is asked for: tool calls too; sessions an SDK started and single exchanges too; nothing before a
-- time; only sessions in this directory (empty: wherever).
data Want = Want { wTools :: Bool, wAll :: Bool, wSince :: Double, wRoot :: FilePath }

-- | A session as what of it is to be imported: its messages of the kinds asked for, after the time asked
-- for, after what was imported of it before (@done@: a session's key to the date of its last imported
-- message). 'Nothing' when it is not this project's, not a person's session, or has nothing new.
wanted :: Want -> M.Map String Double -> Session -> Maybe Session
wanted w done s
  | not (null (wRoot w)) && sWhere s /= wRoot w = Nothing
  | not (wAll w) && (machine || length talk <= 2) = Nothing      -- (a prompt and its answer: one exchange)
  | null es = Nothing
  | otherwise = Just s { sEntries = es }
  where
    machine = sEntry s `elem` ["sdk-ts", "sdk-py"]       -- (a program's calls through the SDK: a compactor's, a harness's)
    talk = [ e | e <- sEntries s, eKind e `elem` ["user", "ai"] ]
    after = max (wSince w) (M.findWithDefault 0 (sessionKey s) done)
    es = [ e | e <- sEntries s, eDate e > after, wTools w || eKind e `elem` ["user", "ai"] ]

-- | What names a session across imports: its file.
sessionKey :: Session -> String
sessionKey = sFile

-- | The note a session starts with in the history.
sessionNote :: Session -> IO T.Text
sessionNote s = do
  at <- case sEntries s of
    (e : _) -> formatTime defaultTimeLocale "%Y-%m-%d %H:%M" <$> utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac (eDate e)))
    [] -> pure "?"
  pure (T.pack (sApp s ++ " session in " ++ (if null (sWhere s) then "?" else sWhere s) ++ ", from " ++ at ++ " (imported: " ++ show (length (sEntries s)) ++ " messages)"))

-- | The plan as lines: each session (when, the program, how it was started, its messages and their size), and
-- in all -- the messages, and how many of them are over a line's size (@node@ bytes), which is what their
-- summaries cost: a model call each, and about as many again for the merges above them.
plan :: Int -> [Session] -> IO [String]
plan node ss = do
  rows <- forM (sortOn (map eDate . take 1 . sEntries) ss) $ \s -> do
    at <- case sEntries s of
      (e : _) -> formatTime defaultTimeLocale "%Y-%m-%d %H:%M" <$> utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac (eDate e)))
      [] -> pure "?"
    let bytes = sum (map (T.length . eText) (sEntries s))
    pure ("  " ++ at ++ "  " ++ pad 12 (sApp s) ++ pad 15 (sEntry s) ++ padL 6 (show (length (sEntries s))) ++ " messages " ++ padL 8 (human bytes))
  let es = concatMap sEntries ss
      long = length [ () | e <- es, T.length (eText e) + length (eKind e) + 2 > node ]
  pure (rows ++ [ ""
                , "  " ++ show (length ss) ++ " session(s), " ++ show (length es) ++ " message(s), " ++ human (sum (map (T.length . eText) es)) ++ " of text"
                , "  " ++ show long ++ " of them are over a line's " ++ show node ++ " bytes: about " ++ show long ++ " model calls to summarize them, and as many again for the merges above"
                    ++ " (none without a summarize_cmd)" ])
  where
    pad n x = take n (x ++ replicate n ' ') ++ " "
    padL n x = replicate (n - length x) ' ' ++ x
    human n | n >= 1048576 = show (n `div` 1048576) ++ " MB"
            | n >= 1024 = show (n `div` 1024) ++ " KB"
            | otherwise = show n ++ " B"
