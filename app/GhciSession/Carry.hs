-- | __What a fresh call carries__: a turn that goes on in a fresh call -- after a rollover, a restart that could
-- not hand the turn over, a limit that has reset -- is given the view up to where its log begins, then the log
-- ('Split': the turn's message, what the user said in it, its last messages as far as the tail's bytes go), then
-- a note. The parts are pure here, so that what a rollover carries can be measured ('carryParts', @chat --carry@)
-- with the same code that builds it.
module GhciSession.Carry
  ( Msg, formatMsg, resumeLog, capMsg, capOld, Split (..), carrySplit, carryRecent, carryFirst, carryNote
  , Part (..), carryParts, subjectsBytes, partsTable, taskBefore, rollEchoes
  , Touch (..), touches, carryFiles, relearned, Rot (..), emptyRot, seeRot, rotShare
  ) where

import Data.List (intercalate, partition)
import Data.Maybe (listToMaybe, maybeToList)
import Text.Read (readMaybe)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Printf (printf)

import GhciSession.Json

-- | A message of the history: its number, kind and text.
type Msg = (Int, T.Text, T.Text)

formatMsg :: Msg -> T.Text
formatMsg (i, k, t) = T.pack (show i ++ "|") <> k <> T.pack ": " <> t <> T.pack "\n"

-- | The last messages of a turn's log that fit the bytes, oldest first, and how many before them do not.
resumeLog :: Int -> [Msg] -> ([Msg], Int)
resumeLog budget ms = let kept = go 0 (reverse ms) in (reverse kept, length ms - length kept)
  where go _ [] = []
        go used (m@(_, _, t) : r) = let n = used + bytesOf t + 16 in if n > budget && used > 0 then [] else m : go n r

-- | A message cut to @n@ characters, with the note of what is left and where to read it (the message's number is
-- the one to zoom): a read of two hundred lines, answered and acted on, is not what the fresh call needs word for word.
capMsg :: Int -> Msg -> Msg
capMsg n m@(i, k, t)
  | T.length t <= n + 200 = m
  | otherwise = (i, k, T.take n t <> T.pack ("\n[... " ++ show (T.length t - n) ++ " more characters of message " ++ show i ++ ": zoom it to read the whole]"))

-- | The log with every message but its last @keep@ cut to @n@ characters ('capMsg'): the last steps are what the
-- fresh call goes on from, whole; the older ones' answers are for the summaries and the zoom. What the user said
-- is never cut.
capOld :: Int -> Int -> [Msg] -> [Msg]
capOld keep n ms = [ if j >= len - keep || k == T.pack "user" then m else capMsg n m | (j, m@(_, k, _)) <- zip [0 ..] ms ]
  where len = length ms

-- | What of a turn's log a fresh call is given: the turn's message, what the user said in it before the kept
-- messages (what the user said stays whole, however long ago: it is what to do), how many messages between are
-- left to the view as summaries, the kept messages, and the number of the first of them.
data Split = Split { spTask :: Msg, spTold :: [Msg], spLeft :: Int, spKept :: [Msg], spFrom :: Int }
  deriving (Eq, Show)

-- | The split of a turn's log (its message first) at a tail of so many bytes.
carrySplit :: Int -> [Msg] -> Maybe Split
carrySplit _ [] = Nothing
carrySplit budget (task@(ti, _, _) : after0) =
  let after = capOld 6 1500 after0
      (kept, left) = resumeLog budget after
      from = case kept of { ((i, _, _) : _) -> i; [] -> ti + 1 }
      told = [ m | m@(i, k, _) <- after, k == T.pack "user", i < from ]
  in Just (Split task told (left - length told) kept from)

-- | The log as the call is given it: @\<recent\>@.
carryRecent :: Split -> T.Text
carryRecent sp = T.pack "<recent>\n" <> formatMsg (spTask sp) <> T.concat (map formatMsg (spTold sp)) <> gap
                 <> T.concat (map formatMsg (spKept sp)) <> T.pack "</recent>\n\n"
  where gap = if spLeft sp > 0 then T.pack (printf "(%d messages of the turn are not here in full: they are the view's last lines, as summaries -- zoom them)\n" (spLeft sp)) else T.empty

-- | The call's first message: the view, the log, the note.
carryFirst :: T.Text -> Split -> T.Text -> T.Text -> T.Text
carryFirst v sp files note = v <> T.pack "\n\n" <> carryRecent sp <> files <> note

-- | The note that ends the message: why the turn goes on here.
carryNote :: String -> T.Text
carryNote why = T.pack $ "[harness: this turn did not end -- " ++ why ++ ". <recent> holds the turn's message, what the user said during it, and what you did in it last, word for word"
  ++ " (your tool calls and their answers, your replies; not what you had in mind between them). Go on from where it stops: look at what the last steps were doing, check the state of the"
  ++ " files and the session if you need to, and do what is left. Do not start over, and do not do again what <recent> shows done."
  ++ " <files>, if there, lists the files the turn has read and written, with the lines read last: what you read is no longer in front of you, so read again only the part you need.]"

-- the files ---------------------------------------------------------------------------------------

-- | A file the turn has touched: the lines of it last read (the latest first, three at most), whether it was read
-- at all (a read around a text has no lines) and whether it was written.
data Touch = Touch { tPath :: String, tReads :: [(Int, Int)], tSawRead :: Bool, tEdited :: Bool }
  deriving (Eq, Show)

data Ev = EvRead String (Maybe (Int, Int)) | EvEdit String

evPath :: Ev -> String
evPath (EvRead p _) = p
evPath (EvEdit p) = p

-- | What a tool message of the log did to a file.
eventsOf :: Msg -> [Ev]
eventsOf (_, k, t)
  | k /= T.pack "tool" = []
  | otherwise = case T.breakOn (T.pack " ") t of
      (n, rest) | n `elem` map T.pack ["read", "write", "edit", "edits"], Right j <- parseJsonBS (TE.encodeUtf8 (T.drop 1 rest)) -> callEvents (T.unpack n) j
      _ -> []

-- | What a call of a tool, by its name and arguments, did to files.
callEvents :: String -> Json -> [Ev]
callEvents n j0 = if n `elem` ["read", "write", "edit", "edits"] then go n j0 else []
  where
    go "read" j = [ EvRead p (range j) | p <- maybeToList (lookupStr "path" j) ]
    go "edits" j = map EvEdit (maybeToList (lookupStr "path" j) ++ [ q | e <- lookupArr "edits" j, q <- maybeToList (lookupStr "path" e) ])
    go _ j = map EvEdit (maybeToList (lookupStr "path" j))
    num j key = case lookupNum key j of
      Just x -> Just (round x :: Int)
      Nothing -> lookupStr key j >>= readMaybe
    range j
      | Just _ <- lookupStr "around" j = Nothing
      | otherwise = let s = maybe 1 id (num j "start"); n = maybe 200 id (num j "lines") in Just (s, s + max 1 n - 1)

-- | The reads of one context, to see if the agent reads what it has read: @rotSeen@ the lines of each file read since
-- it was last written, @rotFlags@ whether each read was a repeat of one of them (the latest first, twenty).
data Rot = Rot { rotSeen :: [(String, (Int, Int))], rotFlags :: [Bool] }
  deriving (Eq, Show)

emptyRot :: Rot
emptyRot = Rot [] []

-- | A tool call, as the context sees it: a read of lines that overlap lines read before (of the same file, not
-- written since) is a repeat; a write forgets the file's reads. A read around a text has no lines, and is not one.
seeRot :: String -> Json -> Rot -> Rot
seeRot name args rot0 = foldl step rot0 (callEvents name args)
  where
    step rot (EvEdit p) = rot { rotSeen = [ s | s@(q, _) <- rotSeen rot, q /= p ] }
    step rot (EvRead _ Nothing) = rot { rotFlags = take 20 (False : rotFlags rot) }
    step rot (EvRead p (Just (a, b))) =
      let again = or [ a <= d && c <= b | (q, (c, d)) <- rotSeen rot, q == p ]
      in rot { rotSeen = take 300 ((p, (a, b)) : rotSeen rot), rotFlags = take 20 (again : rotFlags rot) }

-- | The share of the last twenty reads that were repeats (none while fewer than ten have been seen).
rotShare :: Rot -> Maybe Double
rotShare rot
  | length fs < 10 = Nothing
  | otherwise = Just (fromIntegral (length (filter id fs)) / fromIntegral (length fs))
  where fs = rotFlags rot

-- | The files the messages touched, the latest first.
touches :: [Msg] -> [Touch]
touches = foldl step [] . concatMap eventsOf
  where
    step acc ev = let (old, rest) = partition ((== evPath ev) . tPath) acc
                      t0 = maybe (Touch (evPath ev) [] False False) id (listToMaybe old)
                  in apply ev t0 : rest
    apply (EvRead _ r) t = t { tSawRead = True, tReads = maybe id (\x -> take 3 . (x :) . filter (/= x)) r (tReads t) }
    apply (EvEdit _) t = t { tEdited = True }

-- | The turn's files as a fresh call is given them, in under a kilobyte: where it was, what it read, what it wrote.
carryFiles :: [Msg] -> T.Text
carryFiles ms = case fit 0 (map line (touches ms)) of
  [] -> T.empty
  ls -> T.pack ("<files>\n(what this turn has read and written, the latest first, and the lines it read last)\n" ++ unlines ls ++ "</files>\n\n")
  where
    line t = tPath t ++ ": " ++ intercalate "; " ([ "read " ++ (if null (tReads t) then "around a text" else intercalate ", " [ show a ++ "-" ++ show b | (a, b) <- tReads t ]) | tSawRead t ] ++ [ "written" | tEdited t ])
    fit _ [] = []
    fit used (l : r) = let n = used + length l + 1 in if n > 900 then [] else l : fit n r

-- | What a reset made the agent learn again: @relearned n before after@ -- the messages of the turn before the reset,
-- those after it -- is how many of the next @n@ tool calls read again a file read before, and of how many calls
-- (none if fewer than five followed: too little to say).
relearned :: Int -> [Msg] -> [Msg] -> Maybe (Int, Int)
relearned n before after
  | total < 5 = Nothing
  | otherwise = Just (length [ () | m <- calls, EvRead p _ <- eventsOf m, p `elem` seen ], total)
  where seen = [ tPath t | t <- touches before, tSawRead t ]
        calls = take n [ m | m@(_, k, _) <- after, k == T.pack "tool" ]
        total = length calls

-- measuring ---------------------------------------------------------------------------------------

-- | A part of what a fresh call is given: its name, bytes, and how many things (lines, messages) it is.
data Part = Part { pName :: String, pBytes :: Int, pCount :: Int }
  deriving (Eq, Show)

bytesOf :: T.Text -> Int
bytesOf = B.length . TE.encodeUtf8

-- | The bytes of the view's @\<subjects\>@ block (the lines of it, the tags too).
subjectsBytes :: T.Text -> Int
subjectsBytes v = sum [ bytesOf l + 1 | l <- inside (T.lines v) ]
  where inside ls = case break (== T.pack "<subjects>") ls of
          (_, _ : r) -> let (b, e) = break (== T.pack "</subjects>") r in T.pack "<subjects>" : b ++ take 1 e
          _ -> []

-- | The parts of a fresh call: the system prompt (with what is added to it), the tools' definitions, the subjects
-- block of the whole view (said apart: the call is not given it), the view's lines before the log, the log's
-- parts, the note.
carryParts :: Int -> Int -> T.Text -> T.Text -> Split -> T.Text -> [Part]
carryParts sysB toolsB fullView keptView sp note =
  [ Part "system prompt" sysB 0
  , Part "tools" toolsB 0
  , Part "view: summary lines" (bytesOf keptView) (length (T.lines keptView) - 2)
  , Part "log: the turn's message" (bytesOf (formatMsg (spTask sp))) 1
  , Part "log: what the user said" (sum (map (bytesOf . formatMsg) (spTold sp))) (length (spTold sp))
  , Part "log: the tail, whole" (sum (map (bytesOf . formatMsg) (spKept sp))) (length (spKept sp))
  , Part "log: gap line" (bytesOf (carryRecent sp { spTold = [], spKept = [] }) - bytesOf (formatMsg (spTask sp)) - bytesOf (T.pack "<recent>\n</recent>\n\n")) (if spLeft sp > 0 then 1 else 0)
  , Part "note" (bytesOf note) 0
  , Part "(not given) subjects block" (subjectsBytes fullView) 0 ]

-- | Parts of several fresh calls as a table: a row a part, a column a call, bytes (and, where the first call's
-- tokens are known, the tokens a byte is: the table's last rows).
partsTable :: [(String, [Part], Maybe Int)] -> String
partsTable cols = unlines [ "| " ++ concatMap (\(w, c) -> c ++ replicate (w - length c) ' ' ++ " | ") (zip widths r) | r <- rows ]
  where
    widths = [ maximum (map length col) | col <- columns rows ]
    columns rs = [ [ r !! k | r <- rs ] | k <- [0 .. length (head rs) - 1] ]
    names = [ pName p | p <- case cols of { ((_, ps, _) : _) -> ps; [] -> [] } ]
    head' = "part" : [ h | (h, _, _) <- cols ]
    cell ps n = case [ p | p <- ps, pName p == n ] of
      (p : _) -> show (pBytes p) ++ (if pCount p > 0 then " (" ++ show (pCount p) ++ ")" else "")
      [] -> "-"
    given ps = sum [ pBytes p | p <- ps, take 5 (pName p) /= "(not " ]
    tok (_, ps, Just t) | g > 0 = printf "%d tok, %.2f B/tok" t (fromIntegral g / fromIntegral t :: Double)
      where g = given ps
    tok _ = "-"
    rows = [head'] ++ [ n : [ cell ps n | (_, ps, _) <- cols ] | n <- names ]
           ++ [ "given (bytes)" : [ show (given ps) | (_, ps, _) <- cols ], "first call" : [ tok c | c <- cols ] ]

-- | The user message a turn began with, before message @r@: the nearest user message that is not a line typed in
-- the middle of a turn (one that follows a tool call or its answer).
taskBefore :: Int -> [Msg] -> Maybe Int
taskBefore r ms = go (reverse [ m | m@(i, _, _) <- ms, i < r ])
  where
    go [] = Nothing
    go ((i, k, _) : rest)
      | k == T.pack "user", not (afterWork rest) = Just i
      | otherwise = go rest
    afterWork rest = case [ k | (_, k, _) <- rest, k `notElem` map T.pack ["note", "known"] ] of
      (k : _) -> k `elem` map T.pack ["tool", "echo"]
      [] -> False

-- | The messages that are a rollover's: the harness's echo that a turn went on in a fresh call because its context
-- had grown.
rollEchoes :: [Msg] -> [Int]
rollEchoes ms = [ i | (i, k, t) <- ms, k == T.pack "echo", T.pack "harness: this turn goes on in a fresh call, from its log (" `T.isPrefixOf` t
                                    , any (`T.isInfixOf` t) (map T.pack ["its context had grown", "minutes passed since its last call"]) ]
