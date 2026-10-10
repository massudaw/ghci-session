-- | __What is known, by subject, across sessions__: the facts a person's sessions have established -- how a
-- tool is used, a rule the user gave, a project's design and figures -- kept apart from any one session's
-- history, so that what a session learned is there in the next one, and so that what holds NOW is told from
-- what was once said.
--
-- A session's history ("GhciSession.History") is a chain in time: the newest is fine, the old is coarse, and a
-- rule given once is summarized away with the lines around it; two histories joined are worse (the older is
-- squeezed by the newer's messages, whatever it was about). Measured on two real histories: asked what holds
-- now, a model reading the chain answered with the outdated way as often as with the current one; reading
-- these facts beside the session's view, it did not.
--
-- A fact is a RECORD and is never rewritten: a subject, a topic, one sentence, when it was first learned and
-- last confirmed, the message it came from. (A "current state" text rewritten by a model at each update lost
-- the dates and moved a fact from one project to another.) What a model decides is small: 'extractPrompt'
-- asks a piece of the log for at most three facts; 'reconPrompt' asks, of each new one against the nearest
-- that hold, whether it is new, restates one, or replaces one. The daemon applies the answer
-- ("GhciSession.Daemon"): a replaced fact is marked, not deleted.
--
-- Subjects: @tool/usage@ and @tool/config@ (true of this tool for any project), @user/rules@ (how the user
-- wants work done anywhere), and a project's own -- @P/architecture@, @P/performance@, @P/testing@,
-- @P/status@, @P/rules@ -- where P is the project directory's name. What a tool was called with is read from
-- the calls ('callArgs'), not asked of a model: a way of working that changed without anyone saying so.
--
-- A session reads them as a block before its view ('render'): the subjects every session uses first, then
-- the project's, then the others' by last use; in a subject the facts most recently confirmed first, cut
-- where its share ends. The block is written when the view is rewritten and not between: what is learned
-- meanwhile is a @known@ line appended to the session's history ('knownLine'), so the prompt a provider has
-- cached stays the prefix it was.
--
-- Storage, for the person and not the project: @$GHS_KNOWLEDGE@, or @$XDG_STATE_HOME/ghci-session/knowledge@
-- (@~/.local/state@ by default), one file @facts.jsonl@ that is only appended to -- a fact a line, and a line
-- for each mark (replaced by, confirmed at, forgotten). Several daemons write it; each line is one write, and
-- a read-then-write is under 'withLock'.
module GhciSession.Know
  ( Fact (..), New (..), Decision (..)
  , knowDir, loadFacts, current, addFact, markBy, markSeen, forget, withLock, newId
  , extractPrompt, parseNew, callArgs, callFact, isCall
  , candidates, reconPrompt, parseDecisions
  , foldLimit, foldDue, foldPrompt, parseFold
  , rank, search, snippet
  , render, renderFor, operatorSubject, knownLine, day, budget
  , markSeenFrom, Seen (..), confirmations, confirmationsOf, promotable, srcProject
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket_, try)
import Control.Monad (void)
import qualified Data.ByteString as B
import Data.Char (isAlphaNum, isDigit, toLower)
import Data.List (foldl', isPrefixOf, nub, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import System.Directory (createDirectory, createDirectoryIfMissing, doesFileExist, getHomeDirectory, getModificationTime, removeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (IOMode (..), SeekMode (..), hFileSize, hSeek, withBinaryFile)
import System.Posix.Process (getProcessID)
import Text.Printf (printf)

import GhciSession.Json
import GhciSession.Sys (now)

-- | A fact that is stored. @fBy@: the fact that replaced it (then it no longer holds).
data Fact = Fact
  { fId :: String, fSubject :: T.Text, fTopic :: T.Text, fText :: T.Text
  , fFirst :: Double, fLast :: Double, fSrc :: String, fReplaces :: [String], fBy :: Maybe String }
  deriving (Eq, Show)

-- | A fact just extracted, not yet stored.
data New = New { nSubject :: T.Text, nTopic :: T.Text, nText :: T.Text, nDate :: Double, nSrc :: String }
  deriving (Eq, Show)

-- | What is done with a new fact; the numbers are places in the list of candidates it was shown with.
data Decision = Add | Same [Int] | Replace [Int] | Drop
  deriving (Eq, Show)

-- | The bytes of the block a session reads ('render'); the session's view is given that much less.
budget :: Int
budget = 16000

knowDir :: IO FilePath
knowDir = do
  e <- lookupEnv "GHS_KNOWLEDGE"
  x <- lookupEnv "XDG_STATE_HOME"
  h <- getHomeDirectory
  pure (case (e, x) of
    (Just d, _) | not (null d) -> d
    (_, Just d) | not (null d) -> d </> "ghci-session" </> "knowledge"
    _ -> h </> ".local" </> "state" </> "ghci-session" </> "knowledge")

factsFile :: FilePath -> FilePath
factsFile dir = dir </> "facts.jsonl"

-- | Every fact, in the order they were stored, with its marks applied. A line that is not JSON is passed over.
loadFacts :: FilePath -> IO [Fact]
loadFacts dir = do
  there <- doesFileExist (factsFile dir)
  if not there then pure [] else do
    b <- B.readFile (factsFile dir)
    let js = [ j | l <- B.split 10 b, not (B.null l), Right j <- [parseJsonBS l] ]
        step (order, mp) j = case lookupStr "mark" j of
          Nothing -> case lookupStr "id" j of
            Just i | not (M.member i mp) ->
              (i : order, M.insert i (Fact i (tx "subject" j) (tx "topic" j) (tx "fact" j) (nm "first" j) (nm "last" j) (fromMaybe "" (lookupStr "src" j)) [ r | JStr r <- lookupArr "replaces" j ] Nothing) mp)
            _ -> (order, mp)
          Just "by" -> (order, upd j (\f -> f { fBy = lookupStr "by" j }) mp)
          Just "seen" -> (order, upd j (\f -> f { fLast = max (fLast f) (nm "at" j) }) mp)
          Just "forget" -> (order, maybe mp (`M.delete` mp) (lookupStr "id" j))
          _ -> (order, mp)
        (order, mp) = foldl' step ([], M.empty) js
    pure (mapMaybe (`M.lookup` mp) (reverse order))
  where
    tx k j = fromMaybe T.empty (lookupText k j)
    nm k j = fromMaybe 0 (lookupNum k j)
    upd j f mp = maybe mp (\i -> M.adjust f i mp) (lookupStr "id" j)

-- | The facts that hold.
current :: [Fact] -> [Fact]
current = filter (\f -> fBy f == Nothing)

appendJson :: FilePath -> Json -> IO ()
appendJson dir j = do
  createDirectoryIfMissing True dir
  -- (a line torn by a writer that died has no end: it is given one, or it would take this line with it)
  torn <- (try (withBinaryFile (factsFile dir) ReadMode (\h -> do
            n <- hFileSize h
            if n == 0 then pure False else hSeek h SeekFromEnd (-1) >> ((/= B.singleton 10) <$> B.hGet h 1))) :: IO (Either IOException Bool))
  B.appendFile (factsFile dir) ((if torn == Right True then B.singleton 10 else B.empty) <> encodeBS j <> B.singleton 10)

addFact :: FilePath -> Fact -> IO ()
addFact dir f = appendJson dir (JObj [ ("id", JStr (fId f)), ("subject", JText (fSubject f)), ("topic", JText (fTopic f)), ("fact", JText (fText f))
                                     , ("first", JNum (fFirst f)), ("last", JNum (fLast f)), ("src", JStr (fSrc f)), ("replaces", JArr (map JStr (fReplaces f))) ])

-- | This fact was replaced by that one.
markBy :: FilePath -> String -> String -> IO ()
markBy dir i by = appendJson dir (JObj [("mark", JStr "by"), ("id", JStr i), ("by", JStr by)])

-- | This fact was said again, then.
markSeen :: FilePath -> String -> Double -> IO ()
markSeen dir i at = appendJson dir (JObj [("mark", JStr "seen"), ("id", JStr i), ("at", JNum at)])

-- | This fact was said again, then, in the log that has this source (@project/session:from+n@).
markSeenFrom :: FilePath -> String -> Double -> String -> IO ()
markSeenFrom dir i at src = appendJson dir (JObj [("mark", JStr "seen"), ("id", JStr i), ("at", JNum at), ("src", JStr src)])

forget :: FilePath -> String -> IO ()
forget dir i = appendJson dir (JObj [("mark", JStr "forget"), ("id", JStr i)])

-- | A read of the facts and the writes that follow from it, one writer at a time (a directory made is the
-- lock; one left by a writer that died is taken after two minutes).
withLock :: FilePath -> IO a -> IO a
withLock dir act = createDirectoryIfMissing True dir >> bracket_ (take' 0) release act
  where
    lock = dir </> "lock"
    release = void (try (removeDirectory lock) :: IO (Either IOException ()))
    take' :: Int -> IO ()
    take' n = do
      r <- try (createDirectory lock) :: IO (Either IOException ())
      case r of
        Right () -> pure ()
        Left _ -> do
          t <- now
          m <- try (getModificationTime lock) :: IO (Either IOException UTCTime)
          let old = either (const False) (\x -> t - realToFrac (utcTimeToPOSIXSeconds x) > 120) m
          if old || n > 1200 then release >> take' 0 else threadDelay 100000 >> take' (n + 1)

newId :: IO String
newId = do
  t <- now
  p <- getProcessID
  pure (printf "%x-%d" (round (t * 1e6) :: Integer) (fromIntegral p :: Int))

day :: Double -> String
day = formatTime defaultTimeLocale "%Y-%m-%d" . posixSecondsToUTCTime . realToFrac

-- extraction -----------------------------------------------------------------------------------

-- | The prompt that asks a piece of a session's log for its durable facts. Everything before the @\<chat\>@ line
-- is the same for every piece (the system prompt of @ghci-session summarize@, cached by a provider).
extractPrompt :: [String] -> String -> Double -> T.Text -> T.Text
extractPrompt toolLines project date body = T.pack (unlines (
  [ "You maintain a knowledge base built from the log of an engineer's sessions with a coding agent that works through a tool called ghci-session. You are given a piece of the log of one project (messages, or one-line summaries of runs of messages), with its date."
  , "Extract the DURABLE knowledge in it: what would change how someone acts weeks later. A decision, a standing rule from the user, how a tool or setting is to be used, a result figure, a cause found. NOT the steps taken, transient errors, what was merely tried, or a state that the next hour changes. Most pieces hold nothing: then the list is empty. At most 3 facts a piece, the most consequential."
  , "Each fact has a SCOPE, decided strictly:"
  , "  \"tool\"    true of ghci-session itself for ANY project: what a tool call or a ghci-session.json setting does (tool scope even when this project is where it was learned), a limit, a behaviour. It must not depend on this project's files or modules; if it names them, it is not tool scope."
  , "  \"user\"    how this user wants work done on ANY project (process, what never to do). A rule that names this project's branch, files, formats or targets is project scope; when in doubt, project."
  , "  \"project\" everything else: this project's design, figures, tests, status, and the user's rules for this project only."
  , "  \"operator\" what only a PERSON at the command line does with ghci-session, never the agent's tool calls: import --go, chat --send, the i key in top, knowledge search."
  , "a SUBJECT: tool/usage or tool/config for tool scope; tool/operator for operator scope; user/rules for user scope; for project scope one of architecture, performance, testing, status, rules."
  , "and a TOPIC: three to six words naming WHAT the fact is about, the same words whenever the same thing is spoken of (\"optimised loading setting\", \"routine check command\")."
  , "Write the fact as ONE self-contained present-tense sentence with exact names, settings and figures."
  , "Reply with JSON only, on ONE line, no code fence: {\"facts\": [{\"scope\": \"...\", \"subject\": \"...\", \"topic\": \"...\", \"fact\": \"...\"}]}" ]
  ++ (if null toolLines then [] else
      [ "The agent already has these tools, each described to it as below (name(arguments): what it does). Do NOT extract what such a description already says -- that a tool exists, takes an argument, or what an argument is for; extract only what it does not say." ] ++ toolLines)
  ++ [ "<chat>"
     , "project: " ++ project
     , "date: " ++ day date
     , "" ])) <> body <> T.pack "\n</chat>\nThe JSON, on one line:\n"

-- | The JSON object in an answer: from its first brace to its last (a model may put a fence around it).
jsonIn :: T.Text -> Maybe Json
jsonIn t = let a = T.dropWhile (/= '{') t
               b = T.dropWhileEnd (/= '}') a
           in if T.null b then Nothing else either (const Nothing) Just (parseJson (T.unpack b))

-- | The facts of an answer to 'extractPrompt', at most three, each under the subject its scope allows.
parseNew :: String -> Double -> String -> T.Text -> [New]
parseNew project date src out = take 3
  [ New (T.pack (subject (fromMaybe "" (lookupStr "scope" f)) (map toLower (fromMaybe "" (lookupStr "subject" f))))) (fromMaybe T.empty (lookupText "topic" f)) (T.strip t) date src
  | Just j <- [jsonIn out], f <- lookupArr "facts" j, Just t <- [lookupText "fact" f], not (T.null (T.strip t)) ]
  where
    lastPart s = reverse (takeWhile (/= '/') (reverse s))
    subject scope s
      | scope == "tool" = if s == "tool/config" then s else "tool/usage"
      | scope == "operator" = T.unpack operatorSubject
      | scope == "user" = "user/rules"
      | otherwise = project ++ "/" ++ (let p = lastPart s in if p `elem` ["architecture", "performance", "testing", "status", "rules"] then p else "status")

-- | A tool call as the chat logs it -- @name {arguments}@: the tool and the arguments it was given.
callArgs :: T.Text -> Maybe (String, [String])
callArgs t = case T.break (== ' ') t of
  (name, rest) | not (T.null name), T.all (\c -> isAlphaNum c || c == '_') name, Just ('{', _) <- T.uncons (T.stripStart rest) ->
    case parseJson (T.unpack (T.stripStart rest)) of
      Right (JObj kvs) -> Just (T.unpack name, map fst kvs)
      _ -> Nothing
  _ -> Nothing

-- | The fact that a tool is called with an argument, with the call it was first seen in.
callFact :: String -> String -> Double -> String -> T.Text -> New
callFact tool key date src call =
  New (T.pack "tool/usage") (T.pack (tool ++ " call argument " ++ key))
      (T.pack ("The " ++ tool ++ " tool is called with the argument \"" ++ key ++ "\", e.g. ") <> T.take 160 (T.map (\c -> if c == '\n' then ' ' else c) call)) date src

-- | Is this fact one of 'callFact''s? Its tool and argument.
isCall :: T.Text -> Maybe (String, String)
isCall topic = case words (T.unpack topic) of
  [tool, "call", "argument", key] -> Just (tool, key)
  _ -> Nothing

-- reconciling ----------------------------------------------------------------------------------

toks :: T.Text -> S.Set T.Text
toks = S.fromList . filter (\w -> T.length w >= 3 && w `S.notMember` stop) . T.split (not . ok) . T.toLower
  where ok c = isAlphaNum c || c == '_'
        stop = S.fromList (map T.pack (words "the and for with that this from not was were now then than which when what how has have had can must should will does use used uses using via into are its"))

-- | The facts that hold nearest a new one, by the words they share (a topic's count twice): the ones a model
-- is asked about. At most ten; none when nothing is near.
candidates :: [Fact] -> New -> [Fact]
candidates facts n = nubBy' (near ++ recent)
  where
    near = map snd (take 10 (sortBy (comparing (Down . fst)) [ (s, f) | f <- facts, let s = score f, s >= 3 ]))
    -- (and its subject's latest: "the last commit is X" and "committed as Y" share no word, and one replaces the other)
    recent = take 5 (sortBy (comparing (Down . fLast)) [ f | f <- facts, fSubject f == nSubject n, isCall (fTopic f) == Nothing ])
    nubBy' = foldr (\f r -> f : filter ((/= fId f) . fId) r) []
    tn = toks (nTopic n <> T.pack " " <> nText n)
    tt = toks (nTopic n)
    score f = S.size (S.intersection tn (toks (fTopic f <> T.pack " " <> fText f))) + 2 * S.size (S.intersection tt (toks (fTopic f)))

-- | The prompt that asks what to do with new facts, beside the facts that hold and are near them (numbered
-- from 1: the numbers are what the answer names).
reconPrompt :: [Fact] -> [New] -> T.Text
reconPrompt existing news = T.pack (unlines
  [ "You keep a knowledge base of facts. EXISTING are facts that hold now, numbered. NEW are facts just extracted from a session's log, later than the existing ones."
  , "For each NEW fact decide one action:"
  , "  \"add\"      it is new knowledge"
  , "  \"same\"     it restates the EXISTING fact(s) named in \"ids\" and adds nothing (the existing one is confirmed)"
  , "  \"replace\"  it updates or contradicts the EXISTING fact(s) named in \"ids\": the new one is what holds now (also the same fact with a newer figure)"
  , "  \"drop\"     it is not durable knowledge: a step, a moment's state, a detail nobody will need"
  , "A newer statement of the same changing thing -- the last commit, what passes now, the current figure, what is left to do -- REPLACES the older one."
  , "Reply with JSON only, on ONE line, no code fence: {\"decisions\": [{\"n\": 1, \"action\": \"...\", \"ids\": [...]}]} with one decision for every NEW fact."
  , "<chat>"
  , "EXISTING:" ])
  <> T.unlines [ T.pack ("#" ++ show k ++ " [" ++ day (fFirst f) ++ "] (") <> fSubject f <> T.pack ") " <> fText f | (k, f) <- zip [1 :: Int ..] existing ]
  <> T.pack "\nNEW:\n"
  <> T.unlines [ T.pack ("N" ++ show k ++ " [" ++ day (nDate n) ++ "] (") <> nSubject n <> T.pack ") " <> nText n | (k, n) <- zip [1 :: Int ..] news ]
  <> T.pack "</chat>\nThe JSON, on one line:\n"

-- | The decisions of an answer to 'reconPrompt', one for each of @n@ new facts: a fact the answer does not
-- speak of is added (nothing learned is lost to a cut answer), and so is one whose \"same\" or \"replace\" names nothing.
parseDecisions :: Int -> Int -> T.Text -> [Decision]
parseDecisions n existing out = [ M.findWithDefault Add k said | k <- [1 .. n] ]
  where
    said = M.fromList [ (k, dec (fromMaybe "add" (lookupStr "action" d)) (ids d))
                      | Just j <- [jsonIn out], d <- lookupArr "decisions" j, Just k <- [number (d .: "n")] ]
    number j = case j of
      JNum x -> Just (round x)
      JStr s | ds@(_ : _) <- filter isDigit s -> Just (read ds)
      _ -> Nothing
    ids d = nub [ i | x <- lookupArr "ids" d, Just i <- [idOf x], i >= 1, i <= existing ]
    idOf x = case x of
      JNum v -> Just (round v)
      JStr s | not ("N" `isPrefixOf` s), ds@(_ : _) <- filter isDigit s -> Just (read ds)
      _ -> Nothing
    dec a is = case a of
      "same" | not (null is) -> Same is
      "replace" | not (null is) -> Replace is
      "drop" -> Drop
      _ -> Add

-- folding --------------------------------------------------------------------------------------

-- | The bytes of a subject's facts past which its older half is folded.
foldLimit :: Int
foldLimit = 6000

-- | A subject that holds more than the limit, with the half of its facts least recently confirmed: these are
-- to be folded into a few. (What a tool is called with is not counted or folded: it is a line a tool.)
foldDue :: Int -> [Fact] -> Maybe (T.Text, [Fact])
foldDue limit facts = case [ (s, fs) | (s, fs) <- M.toList by, sum (map (T.length . fText) fs) > limit, length fs >= 4 ] of
  ((s, fs) : _) -> Just (s, take (length fs `div` 2) (sortBy (comparing fLast) fs))
  [] -> Nothing
  where by = M.fromListWith (flip (++)) [ (fSubject f, [f]) | f <- current facts, isCall (fTopic f) == Nothing ]

-- | The prompt that asks for a subject's older facts as a few.
foldPrompt :: T.Text -> [Fact] -> T.Text
foldPrompt subject fs = T.pack (unlines
  [ "You keep a knowledge base of facts. Below are the older facts of one subject, oldest first. They are to be FOLDED: written again as at most 5 facts that keep what someone would still act on -- rules, settings, decisions, final figures, causes -- and drop what later facts made moot (an earlier figure, a step, a state long past)."
  , "Each is ONE self-contained present-tense sentence with exact names, settings and figures. Do not invent; do not merge unrelated things into one sentence."
  , "Reply with JSON only, on ONE line, no code fence: {\"facts\": [\"...\", \"...\"]}"
  , "<chat>"
  , "subject: " ++ T.unpack subject ])
  <> T.unlines [ T.pack ("[" ++ day (fFirst f) ++ "] ") <> fText f | f <- sortBy (comparing fFirst) fs ]
  <> T.pack "</chat>\nThe JSON, on one line:\n"

parseFold :: T.Text -> [T.Text]
parseFold out = take 5 [ T.strip t | Just j <- [jsonIn out], JText t <- lookupArr "facts" j, not (T.null (T.strip t)) ]

-- finding --------------------------------------------------------------------------------------

-- | Texts by how well they hold a query's words -- a rare word counts for more, a word said again for a little
-- more -- the best first; a text with none of them is left out.
rank :: T.Text -> [(a, T.Text)] -> [(Double, a)]
rank q docs = sortBy (comparing (Down . fst)) [ (s, a) | ((a, _), c) <- zip docs counts, let s = score c, s > 0 ]
  where
    qs = S.toList (toks q)
    -- (the words are counted where they stand, a query word at a time, not by splitting each text into words: that
    -- was nearly all the time and the allocation of a search over a history. A word is where the text has it
    -- with no letter, digit or underscore next to it -- and an overlapping find cannot hide one: the word's own
    -- letters are such characters)
    counts = [ M.fromList [ (w, k) | w <- qs, let k = occurrences w low, k > 0 ] | (_, t) <- docs, let low = T.toLower t ]
    occurrences w low = length [ () | (a, b) <- T.breakOnAll w low, edge (T.takeEnd 1 a), edge (T.take 1 (T.drop (T.length w) b)) ]
    edge e = T.all (\c -> not (isAlphaNum c || c == '_')) e
    n = fromIntegral (length docs) :: Double
    df = M.fromListWith (+) [ (w, 1 :: Double) | c <- counts, w <- M.keys c ]
    score c = sum [ log (1 + (n - d + 0.5) / (d + 0.5)) * (k * 2.2 / (k + 1.2)) | (w, k0) <- M.toList c, let k = fromIntegral k0, Just d <- [M.lookup w df] ]

-- | The facts that hold a query's words, the best first.
search :: T.Text -> [Fact] -> [Fact]
search q fs = map snd (rank q [ (f, fSubject f <> T.pack " " <> fTopic f <> T.pack " " <> fText f) | f <- fs ])

-- | A text around the first of a query's words in it, on one line.
snippet :: Int -> T.Text -> T.Text -> T.Text
snippet width q t =
  let low = T.toLower t
      at = minimum (T.length t : [ T.length a | w <- S.toList (toks q), let (a, b) = T.breakOn w low, not (T.null b) ])
      from = max 0 (at - width `div` 4)
  in (if from > 0 then T.pack "..." else T.empty) <> T.map (\c -> if c == '\n' || c == '\r' then ' ' else c) (T.take width (T.drop from t)) <> (if T.length t > from + width then T.pack "..." else T.empty)

-- reading --------------------------------------------------------------------------------------

-- | What a new fact is in the session's history: its subject and sentence, and what it replaces.
knownLine :: New -> [Fact] -> T.Text
knownLine n replaced = nSubject n <> T.pack ": " <> nText n
  <> (if null replaced then T.empty else T.pack " (THIS REPLACES: " <> T.intercalate (T.pack "; ") [ T.take 160 (fText f) | f <- replaced ] <> T.pack ")")

-- | The subject of what only a person at the command line does (@import --go@, @chat --send@, the @i@ key in
-- top, @knowledge search@): no agent's block holds it; @knowledge --subject@ and top's tab do.
operatorSubject :: T.Text
operatorSubject = T.pack "tool/operator"

-- | The block an AGENT reads ('render' without what it has no use for): the operator's subject left out, and
-- a fact that a tool is called with an argument left out when the tool's schema already describes that
-- argument (the known @(tool, argument)@ pairs are passed in). All of them stay in the store.
renderFor :: [(String, String)] -> Int -> String -> [Fact] -> T.Text
renderFor known bytes project facts = render bytes project (filter keep facts)
  where keep f = fSubject f /= operatorSubject && maybe True (`notElem` known) (isCall (fTopic f))

-- | The block a session of this project reads before its view, in so many bytes: the subjects every session
-- uses (45% of them), the project's (40%), the others' (15%), each tier's share split among its subjects, most
-- recently confirmed first; in a subject the facts most recently confirmed first, cut at its share. Empty
-- when nothing is known.
render :: Int -> String -> [Fact] -> T.Text
render bytes project facts0
  | null facts = T.empty
  | otherwise = T.pack "<subjects>\nWhat is known by subject, as it stood when this view was last rewritten; in each, the facts most recently confirmed first. Only a `known:` line in <chat> that comes AFTER this block was learned later, and holds over it; the block holds what an older one said.\n"
      <> T.concat [ subject share s | (tier, frac) <- [(t1, 0.45), (t2, 0.40), (t3, 0.15 :: Double)], let share = max 200 (floor (fromIntegral bytes * frac / fromIntegral (max 1 (length tier)))), s <- tier ]
      <> T.pack "</subjects>\n"
  where
    facts = current facts0
    by = M.fromListWith (flip (++)) [ (fSubject f, [f]) | f <- facts ]
    subjects = sortBy (comparing (\s -> Down (maximum (map fLast (by M.! s))))) (M.keys by)
    top s = T.takeWhile (/= '/') s
    t1 = [ s | s <- subjects, top s `elem` map T.pack ["tool", "user"] ]
    t2 = [ s | s <- subjects, top s == T.pack project, s `notElem` t1 ]
    t3 = [ s | s <- subjects, s `notElem` t1, s `notElem` t2 ]
    subject share s =
      let fs = by M.! s
          ls = map snd (sortBy (comparing (Down . fst)) (plain fs ++ calls fs))
          (kept, more) = fit share ls
      in T.pack "## " <> s <> T.pack (" (" ++ show (length fs) ++ (if length fs == 1 then " fact)" else " facts)") ++ "\n") <> T.unlines kept <> (if more then T.pack "  (more: recall, or ghci-session knowledge --subject " <> s <> T.pack ")\n" else T.empty)
    plain fs = [ (fLast f, T.pack ("- [" ++ day (fFirst f) ++ (if day (fLast f) /= day (fFirst f) then ", confirmed " ++ day (fLast f) else "") ++ "] ") <> fText f)
               | f <- fs, isCall (fTopic f) == Nothing ]
    -- (what a tool was called with, a tool a line: the arguments, the newest first, and the newest's call)
    calls fs =
      let m = M.fromListWith (flip (++)) [ (tool, [(fFirst f, key, f)]) | f <- fs, Just (tool, key) <- [isCall (fTopic f)] ]
      in [ (d0, T.pack ("- [" ++ day d0 ++ "] " ++ tool ++ " is called with: ") <> T.intercalate (T.pack ", ") [ T.pack ("\"" ++ k ++ "\" (since " ++ day d ++ ")") | (d, k, _) <- as ]
                 <> T.pack ". Newest, " <> snd (T.breakOn (T.pack "e.g. ") (fText f0)))
         | (tool, as0) <- M.toList m, let as = sortBy (comparing (\(d, _, _) -> Down d)) as0, ((d0, _, f0) : _) <- [as] ]
    fit share = go 0
      where go _ [] = ([], False)
            go used (l : r) = let n = B.length (TE.encodeUtf8 l) + 1
                              in if used + n > share then ([], True) else let (a, b) = go (used + n) r in (l : a, b)

-- candidates for the system prompt --------------------------------------------------------------

-- | Where a fact was learned and said again: the days, and the projects whose logs it came from.
data Seen = Seen { seenDays :: S.Set String, seenProjects :: S.Set String }
  deriving (Eq, Show)

-- | The project of a source (@project/session:from+n@); none for one without (a fold's).
srcProject :: String -> Maybe String
srcProject s = case break (== '/') s of
  (p, '/' : _) | not (null p) -> Just p
  _ -> Nothing

-- | Each fact's days and projects, from the store in this directory.
confirmations :: FilePath -> [Fact] -> IO (M.Map String Seen)
confirmations dir facts = do
  there <- doesFileExist (factsFile dir)
  file <- if there then B.readFile (factsFile dir) else pure B.empty
  pure (confirmationsOf file facts)

-- | Each fact's days and projects: when it was first learned and from which log, and each time it was said
-- again (the file's @seen@ marks, which carry the day and, since they were told, the log).
confirmationsOf :: B.ByteString -> [Fact] -> M.Map String Seen
confirmationsOf file facts = foldl' mark base [ j | l <- B.split 10 file, not (B.null l), Right j <- [parseJsonBS l], lookupStr "mark" j == Just "seen" ]
  where
    base = M.fromList [ (fId f, Seen (S.singleton (day (fFirst f))) (S.fromList (maybe [] (: []) (srcProject (fSrc f))))) | f <- facts ]
    mark m j = case lookupStr "id" j of
      Just i -> M.adjust (\(Seen d p) -> Seen (maybe d (\a -> S.insert (day a) d) (lookupNum "at" j)) (maybe p (\pr -> S.insert pr p) (lookupStr "src" j >>= srcProject))) i m
      Nothing -> m

-- | The facts that hold under @tool/@ and @user/@ (not the operator's), confirmed on three or more days or from
-- two or more projects, the most confirmed first: what might belong in the system prompt.
promotable :: M.Map String Seen -> [Fact] -> [(Fact, Seen)]
promotable m facts = sortBy (comparing (\(f, s) -> (Down (S.size (seenDays s)), Down (S.size (seenProjects s)), Down (fLast f))))
  [ (f, s) | f <- current facts, T.takeWhile (/= '/') (fSubject f) `elem` map T.pack ["tool", "user"], fSubject f /= operatorSubject
           , let s = M.findWithDefault (Seen S.empty S.empty) (fId f) m, S.size (seenDays s) >= 3 || S.size (seenProjects s) >= 2 ]
