-- | The tool's own fast checks: the pure logic, in well under a second, with no GHC session involved. This is
-- the check of the session this package runs ON ITSELF (@ghci-session.json@ beside the cabal file): save a
-- module here, and the verdict is these. The behaviour end to end is @examples/tour.py@.
--
-- @ghci-session selftest@ runs them from the executable.
module GhciSession.SelfTest (run) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless, void, when)
import Data.IORef
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort, sortOn)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)

import GhciSession.Cli (Args (..), autostopPlan, parseArgs)
import GhciSession.Config
import GhciSession.Inbox (Mark (..), markOf, advance, settled, turnReport, stillRunning)
import GhciSession.Chat (shownCut, parseOpts, Opts (..), hookEnv, unchangedNote, tcClean, readHeader, budgetSaid, numbered, numberBar, ownTurnStart, toolJson, applyEdit, ReadRec (..), agentShow, arguments, chatTools, isWork, Spent (..), TurnState (..), ViewCtx (..), capWith, newBound, renderTail, renderPlan, planMax, viewLines, editPaths, fuzzyReplace, isRed, ownGhci, shCap, turnFrom, turnJson, nearest, readAgainst, readRange, replaceOnce, saveWait, splitImports, writeRuns, groupByPaths)
import GhciSession.Daemon (scopedSummary, cabalField, ccWords, countSub, hangLimit, moduleDelta, replace, unitsBelow, verdictOf, warningsIn)
import GhciSession.Mcp (Tool (..), toolArgs, toolBrief)
import GhciSession.Doc
import qualified GhciSession.History as H
import qualified GhciSession.Know as K
import qualified GhciSession.Anthropic as A
import qualified GhciSession.ClaudeCli as C
import qualified GhciSession.Import as I
import qualified GhciSession.ImportStamp as IS
import qualified GhciSession.Md as Md
import qualified GhciSession.Image as Img
import qualified GhciSession.ChatTui as ChatTui
import qualified GhciSession.Top as Top
import Tui (Cell (..), Put (..), Key (..), KeyPress (..), Mod (..), cellAt, cellsFor, decodeKey, decodeKeyPress, diff, forget, frame, keyEventFor, maxCells, placeholderRow, placeholderStyle, sgr, textLine, transmit)
import qualified Ghostty.Vt as Vt
import Tui.Types
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import qualified GhciSession.Search as Search
import qualified GhciSession.Vfs as Vfs
import GhciSession.Json
import GhciSession.Carry
import GhciSession.Roll
import GhciSession.Replay
import qualified GhciSession.SelfRound as Round
import GhciSession.Llm (Chunks (..), chunkEvent, chunksMessage, emptyChunks)
import GhciSession.Sys
import GhciSession.Watch
import GHC.Hygiene.Census (readSymbol, zdecode)

-- | Run every check; prints @[PASS]@ / @[FAIL]@ lines and a summary, and says whether all passed.
run :: IO Bool
run = do
  failed <- newIORef (0 :: Int)
  total <- newIORef (0 :: Int)
  let check name ok = do
        modifyIORef' total (+ 1)
        unless ok (modifyIORef' failed (+ 1))
        when (not ok) (putStrLn ("[FAIL] " ++ name))
      eq name a b = do
        check name (a == b)
        when (a /= b) (putStrLn ("       got " ++ show a ++ "\n       want " ++ show b))

  -- JSON
  let doc = "{\"a\": [1, 2.5, -3e2, true, null], \"s\": \"x\\ny \\u00e9 \\\"q\\\"\", \"o\": {\"k\": \"v\"}}"
  case parseJson doc of
    Left e -> check ("json parses: " ++ e) False
    Right j -> do
      eq "json: array" (j .: "a") (JArr [JNum 1, JNum 2.5, JNum (-300), JBool True, JNull])
      eq "json: string escapes" (lookupStr "s" j) (Just "x\ny \233 \"q\"")
      eq "json: nested" (lookupStr "k" (j .: "o")) (Just "v")
      eq "json: round trip" (parseJson (encode j)) (Right j)
      eq "json: pretty round trip" (parseJson (encodePretty j)) (Right j)
  check "json: garbage is an error" (either (const True) (const False) (parseJson "{\"a\": }"))
  eq "json: set replaces in place" (set "a" (JNum 2) (JObj [("a", JNum 1), ("b", JNum 3)])) (JObj [("a", JNum 2), ("b", JNum 3)])
  eq "json: integers print as integers" (encode (JNum 42)) "42"
  let numsOf s = [ (c, parseJson c) | c <- s ]
      isNeg j = case j of { JNum d -> isNegativeZero d; _ -> False }
      readNum c = case reads c :: [(Double, String)] of { [(d, "")] -> Right (JNum d); _ -> Left "" }
      shapes = [ sg ++ m ++ f ++ e | sg <- ["", "-"], m <- ["0", "7", "123456789012345", "1790123456", "9007199254740993", "12345678901234567890"], f <- ["", ".5", ".000001", ".790123456789"], e <- ["", "e9", "E+3", "e-5", "e22", "e-23", "e300"] ]
  eq "json: a number is what `read` makes of it (the exact fast path and the long one), -0 too"
     [ c | (c, r) <- numsOf shapes, fmap isNeg r /= fmap isNeg (readNum c) || r /= readNum c ] []
  eq "json: the odd numbers are as they were" (map (either (const Nothing) Just . parseJson) ["1.", "1.e5", "-", "+1", "1e", "0.1e1", "--1"]) [Just (JNum 1), Just (JNum 100000), Nothing, Nothing, Nothing, Just (JNum 1), Nothing]
  eq "json: a string's escapes -- a pair of surrogates is one character, a lone one is the replacement, any others are kept"
     (map parseJson ["\"a\\n\\t\\\\\\/\\\"b\"", "\"\\ud83d\\ude00!\"", "\"\\ud83dx\"", "\"\\u00e9\\u0041\"", "\"é\\nü\""])
     (map Right [JStr "a\n\t\\/\"b", JStr "\128512!", JStr "\65533x", JStr "\233A", JStr "\233\n\252"])
  eq "json: a string that does not end, or ends on a backslash, is an error"
     (map (either (const True) (const False) . parseJson) ["\"abc", "\"ab\\", "\"a\\\"", "\"\\u12\"", "{\"a\\nb\": 1"]) [True, True, True, True, True]

  -- chat --continue without a turn.json: only this chat's own turn is gone on with, not an imported conversation's
  let m i k = (i, T.pack k, T.pack "x")
  eq "continue: a user message followed by this chat's work is a turn" (ownTurnStart [m 1 "user", m 2 "tool", m 3 "echo"]) (Just 1)
  eq "continue: the last of several is the turn" (ownTurnStart [m 1 "user", m 2 "talk", m 3 "user", m 4 "echo"]) (Just 3)
  eq "continue: an imported user message, answered by ai, is not" (ownTurnStart [m 1 "user", m 2 "tool", m 3 "note", m 4 "user", m 5 "ai", m 6 "ai"]) Nothing
  eq "continue: a user message nothing follows is not (not known whose it is)" (ownTurnStart [m 1 "user", m 2 "talk", m 3 "user"]) Nothing
  eq "continue: notes between do not matter, an ai message that follows later does not either" (ownTurnStart [m 1 "user", m 2 "known", m 3 "tool", m 4 "ai"]) (Just 1)
  eq "continue: no user message, no turn" (ownTurnStart [m 1 "talk", m 2 "ai"]) Nothing

  -- what a fresh call carries
  let big i k n = (i, T.pack k, T.pack (replicate n 'y'))
      txtLen (_, _, t) = T.length t
  eq "carry: a short message is not cut" (capMsg 100 (big 5 "echo" 250)) (big 5 "echo" 250)
  eq "carry: a long one is cut, its number named" (let (i, k, t) = capMsg 100 (big 5 "echo" 5000) in (i, k, T.take 100 t == T.replicate 100 (T.pack "y"), "4900 more characters of message 5" `isInfixOf` T.unpack t)) (5, T.pack "echo", True, True)
  eq "carry: capOld cuts all but the last ones and the user's" (map ((> 1000) . txtLen) (capOld 2 100 [big 1 "echo" 5000, big 2 "user" 5000, big 3 "echo" 5000, big 4 "echo" 5000, big 5 "echo" 5000])) [False, True, False, True, True]
  let turnLog = (1, T.pack "user", T.pack "do it") : [ big i (if even i then "tool" else "echo") 4000 | i <- [2 .. 41] ]
      Just sp0 = carrySplit 60000 turnLog
  eq "carry: older messages are cut, so more of them fit the tail's bytes" (length (spKept sp0) > 60000 `div` 4000 + 6) True
  eq "carry: the last six are whole" (all (\x -> txtLen x == 4000) (drop (length (spKept sp0) - 6) (spKept sp0))) True
  eq "carry: the split adds up" (length (spKept sp0) + spLeft sp0 + length (spTold sp0) + 1) (length turnLog)
  eq "carry: nothing to split" (fmap spFrom (carrySplit 100 [])) Nothing
  eq "carry: a lone message" (fmap spFrom (carrySplit 100 [m 7 "user"])) (Just 8)
  eq "carry: <recent> holds the message, the gap and the tail" (T.unpack (carryRecent (Split (m 1 "user") [] 3 [m 5 "tool"] 5))) "<recent>\n1|user: x\n(3 messages of the turn are not here in full: they are the view's last lines, as summaries -- zoom them)\n5|tool: x\n</recent>\n\n"
  eq "carry: a rollover's echoes" (rollEchoes [m 1 "user", (4, T.pack "echo", T.pack "harness: this turn goes on in a fresh call, from its log (its context had grown to 150k)"), m 5 "echo"]) [4]
  eq "carry: a cold-cache rollover's echo is one too, a restart's is not" (rollEchoes [(4, T.pack "echo", T.pack "harness: this turn goes on in a fresh call, from its log (70 minutes passed since its last call, past what the provider keeps its cache for (55), with 90k tokens of context)"), (6, T.pack "echo", T.pack "harness: this turn goes on in a fresh call, from its log (the chat that was in it stopped, and this is a new one)")]) [4]
  let tl i s = (i, T.pack "tool", T.pack s)
      calls1 = [tl 1 "read {\"path\":\"a.hs\",\"start\":10,\"lines\":5}", tl 2 "read {\"path\":\"b.hs\",\"around\":\"foo\"}", tl 3 "edit {\"path\":\"a.hs\",\"old\":\"x\",\"new\":\"y\"}", tl 4 "read {\"path\":\"a.hs\",\"start\":\"40\"}", tl 5 "edits {\"path\":\"c.hs\",\"edits\":[{\"old\":\"x\",\"new\":\"y\"},{\"path\":\"d.hs\",\"old\":\"x\",\"new\":\"y\"}]}", tl 6 "sh {\"cmd\":\"ls\"}", (7, T.pack "echo", T.pack "read {\"path\":\"z.hs\"}")]
  eq "carry: the files touched, latest first, with the lines last read" (map (\t -> (tPath t, tReads t, tSawRead t, tEdited t)) (touches calls1)) [("d.hs", [], False, True), ("c.hs", [], False, True), ("a.hs", [(40, 239), (10, 14)], True, True), ("b.hs", [], True, False)]
  eq "carry: the file list is under a kilobyte, the latest first, and none when none" (let big = [ tl i ("read {\"path\":\"" ++ replicate 80 'p' ++ show i ++ ".hs\"}") | i <- [1 .. 40] ]; t = carryFiles big in (T.length t < 1100, T.pack (replicate 80 'p' ++ "40.hs") `T.isInfixOf` t, T.pack (replicate 80 'p' ++ "1.hs: ") `T.isInfixOf` t, carryFiles [m 1 "tool", m 2 "echo"] == T.empty)) (True, True, False, True)
  eq "carry: the list says read around a text, lines, written" (T.unpack (carryFiles (take 3 calls1))) "<files>\n(what this turn has read and written, the latest first, and the lines it read last)\na.hs: read 10-14; written\nb.hs: read around a text\n</files>\n\n"
  eq "carry: after a reset, the next calls that read a file read before it again" (relearned 10 (take 2 calls1) [tl 8 "read {\"path\":\"a.hs\"}", tl 9 "grep {\"query\":\"x\"}", tl 10 "read {\"path\":\"q.hs\"}", tl 11 "read {\"path\":\"b.hs\",\"start\":1}", tl 12 "edit {\"path\":\"a.hs\"}", tl 13 "read {\"path\":\"a.hs\"}"]) (Just (3, 6))
  eq "carry: fewer than five calls after a reset say nothing; only the first ten count" (map (\n -> relearned 10 (take 2 calls1) [ tl i "read {\"path\":\"a.hs\"}" | i <- [10 .. 9 + n] ]) [4, 5, 30]) [Nothing, Just (5, 5), Just (10, 10)]
  eq "roll: the share of what a reset made it learn again is a running estimate, counted once a reset" (let r1 = seeRelearn 40 1 (emptyRoll 12.5) { rRelearn = 0 } in (rRelearn r1, rRelId r1, seeRelearn 40 0 r1 == r1, rRelearn (seeRelearn 41 0 r1) < rRelearn r1)) (0.3, 40, True, True)
  let rd p s n = ("read", JObj [("path", JStr p), ("start", JNum s), ("lines", JNum n)])
      rotOf cs = foldl (\r (n, a) -> seeRot n a r) emptyRot cs
  eq "rot: a read of lines read before is a repeat, one of other lines, another file, or after a write is not" (rotFlags (rotOf [rd "a" 1 10, rd "a" 5 10, rd "a" 20 5, rd "b" 1 10, ("edit", JObj [("path", JStr "a")]), rd "a" 1 10])) [False, False, False, True, False]
  eq "rot: a read around a text is not a repeat, nor is another tool" (rotFlags (rotOf [("read", JObj [("path", JStr "a"), ("around", JStr "x")]), ("read", JObj [("path", JStr "a"), ("around", JStr "x")]), ("sh", JObj [("path", JStr "a")])])) [False, False]
  eq "rot: no share before ten reads; then the share of the last twenty" (map (rotShare . rotOf) [ [ rd "a" 1 10 | _ <- [1 .. 9 :: Int] ], replicate 10 (rd "a" 1 10) ++ [rd "b" 1 5, rd "c" 1 5] ]) [Nothing, Just (9 / 12)]
  eq "rot: only twenty reads count" (fmap (> 0.99) (rotShare (rotOf (rd "a" 1 10 : [ rd "b" 1 10 | _ <- [1 .. 40 :: Int] ])))) (Just True)
  eq "rot: over fifteen percent lowers the threshold by a tenth, not under 80k, nor a limit already under it" (map (\(r, l) -> rotLimit r l) [(True, 150000), (True, 85000), (True, 70000), (False, 150000)], map rotOver [Nothing, Just 0.15, Just 0.16]) ([135000, 80000, 70000, 150000], [False, False, True])
  eq "replay: usage lines are the calls, the rest of a log is not" (parseCalls "hello\n[usage: in 1200, out 30, cached 1000 (83%), 2.1s]\n[usage: in 5, out x]\n[usage: in 1300, out 20, cached 1200 (92%), 1s]\n") [Call 1200 1000 30, Call 1300 1200 20]
  eq "replay: growth is the log's where it grew by under 20k, the median where a reset came" (growths [Call 100000 0 0, Call 101000 0 0, Call 40000 0 0, Call 42000 0 0]) [0, 1000, 2000, 2000]
  eq "replay: a fresh call is the median of the contexts that began under 70% of the one before" (freshSize [Call 100000 0 0, Call 40000 0 0, Call 90000 0 0, Call 50000 0 0], freshSize [Call 10 0 0]) (50000, 50000)
  eq "replay: a policy that never resets has none, and a low fixed limit resets" (let cs = [ Call (50000 + 1000 * i) 0 10 | i <- [0 .. 99] ] in (length (smResets (simulate 12.5 50000 (Fixed 0) cs)), length (smResets (simulate 12.5 50000 (Fixed 80000) cs)) > 0, smCalls (simulate 12.5 50000 Auto cs))) (0, True, 100)
  eq "replay: too short a log is said so" (take 1 (lines (replayReport 12.5 [("x", [Call 1 1 1])]))) ["x: 1 calls: too few to replay"]
  eq "carry: the task before a rollover is the user's message, not a line typed mid-turn" (taskBefore 9 [m 1 "user", m 2 "tool", m 3 "echo", m 4 "user", m 5 "tool", m 6 "echo", m 8 "talk"]) (Just 1)
  eq "carry: partsTable has a row a part, and the given sum leaves out what is not given" [ l | l <- lines (partsTable [("c1", [Part "a" 10 2, Part "(not given) b" 99 0], Nothing)]), "given (bytes)" `isInfixOf` l ] ["| given (bytes) | 10     | "]

  -- the plan's metering (GhciSession.SelfRound)
  forM_ Round.checks (uncurry check)

  -- the rollover's controller
  let ro0 = emptyRoll 12.5
  eq "roll: the threshold is S + sqrt(2 r S (1+relearn) g)" (threshold ro0 { rRelearn = 0, rS = 50000, rG = 2000, rRatio = 12.5 }) (round (50000 + sqrt (2 * 12.5 * 50000 * 2000 :: Double)))
  eq "roll: it is kept within 80k and 200k" (map threshold [ro0 { rS = 5000, rG = 50 }, ro0 { rS = 190000, rG = 20000 }]) [80000, 200000]
  eq "roll: a dearer write is later, a faster growth is sooner" (let t r g = threshold ro0 { rRatio = r, rG = g } in (t 20 1590 > t 12.5 1590, t 12.5 3000 < t 12.5 1590)) (True, False)
  eq "roll: a run's first call moves the restart's size halfway" (rS (seeCall True 0 65000 ro0 { rS = 45000 })) 55000
  eq "roll: a later call moves the growth a twentieth of the way" (rG (seeCall False 50000 52000 ro0 { rG = 1000 })) 1050
  eq "roll: a call that shrank the context, or the first of a run with none before it, is not a rate" (map (rG . (\f -> f ro0)) [seeCall False 50000 40000, seeCall False 0 40000]) [rG ro0, rG ro0]
  eq "roll: one jump is cut at 20k" (rG (seeCall False 10000 90000 ro0 { rG = 0 })) 1000
  eq "roll: --rollover 0 never, N fixed, auto the threshold" (map (\n -> limitOf n ro0) [0, 123456, -1]) [Nothing, Just 123456, Just (threshold ro0)]
  eq "roll: said again when moved by more than 5k" (map (moved 100000) [104000, 106000, 94000]) [False, True, True]
  eq "roll: its file round-trips" (rollFrom Nothing (rollJson ro0 { rS = 51234, rG = 1700, rSaid = 99000 })) ro0 { rS = 51234, rG = 1700, rSaid = 99000 }
  eq "roll: the cache is taken to last 3300 s at the start" (lifetime ro0) 3300
  eq "roll: a call after a pause that came back cached: it lasted at least that" (let r = seeGap 5000 60000 62000 58000 ro0 in (rLo r, rHi r, lifetime r)) (5000, 5000, 5000)
  eq "roll: the same context after a pause, uncached: it lasted at most that" (let r = seeGap 1800 60000 62000 8000 ro0 in (rLo r, rHi r, lifetime r)) (0, 1800, 1800)
  eq "roll: a short pause says nothing, nor does a context that grew a lot, nor one half cached" (map (\r -> (rLo r, rHi r)) [seeGap 60 60000 62000 8000 ro0, seeGap 1800 60000 90000 8000 ro0, seeGap 1800 60000 62000 30000 ro0]) (replicate 3 (0, 3300))
  eq "roll: the bounds do not cross" (let r = seeGap 900 60000 62000 8000 (seeGap 1800 60000 62000 58000 ro0) in (rLo r <= rHi r, rHi r)) (True, 900)
  eq "roll: a cold cache with a big context rolls over; a small one, or a warm one, does not" (map (\(s, c) -> coldDue ro0 s c) [(4000, 80000), (4000, 50000), (600, 80000)]) [True, False, False]
  eq "roll: a fixed limit is passed or not; the controller's ends a run at 1.15, or from 0.85 at a boundary" (map (\(a, l, c, b) -> rollAt a l c b) [(False, 100000, 100001, False), (False, 100000, 100000, True), (True, 100000, 114999, False), (True, 100000, 115000, False), (True, 100000, 85000, True), (True, 100000, 84999, True)]) [True, False, False, True, True, False]
  eq "roll: a commit that went through is a boundary, a failed one, an empty one, or a plain command is not" (map (\(n, c, ok, o) -> boundaryTool n (JObj [("cmd", JStr c)]) ok o) [("sh", "git add -A && git commit -q -m x", True, "[exit 0 in 0.1 s]"), ("sh", "git commit -q -m x", True, "nothing to commit\n[exit 1 in 0.1 s]"), ("sh", "git commit -q -m x", True, "[exit 1 in 0.1 s]"), ("sh", "ls", True, "[exit 0 in 0.1 s]"), ("edit", "", True, "edited")]) [True, False, False, False, False]
  eq "roll: a green test is a boundary, a red one is not" (map (\o -> boundaryTool "test" JNull True o) ["selftest: all 438 passed", "OK -- CHECK-PASS (0.6s)", "selftest: 1 of 438 FAILED", "STALE(1) COMPILE-ERROR", "OK -- CHECK SKIPPED"]) [True, True, False, False, False]
  eq "roll: a file with nonsense in it is the start" (rollFrom Nothing (JObj [("S", JNum (-3)), ("g", JStr "x")])) ro0

  -- objects are taken only from a session compiled with the same flags (-O1 then -O2 on a module: "[Flags changed]")
  let bconf = Conf { cRoot = "/p", cStateDir = "/p/.s", cStateRel = ".s", cDefault = "tool", cTargets = [("tool", JNull), ("engine", JNull)], cSessions = [], cPrices = [] }
  check "flags: a measuring session takes from the same level, not from another or from the configuration's"
    (sameFlags bconf "tool-O2" "engine-O2" && sameFlags bconf "tool-O2" "tool-O2+exe.x" && not (sameFlags bconf "tool-O2" "tool-O1")
     && not (sameFlags bconf "tool-O2" "tool") && not (sameFlags bconf "tool-O2s" "tool-O2"))
  check "flags: a session of the configuration takes from those of the configuration" (sameFlags bconf "tool" "engine" && not (sameFlags bconf "tool" "tool-O1"))

  -- verdicts
  let st m l = JObj [("modules", JNum m), ("loaded", JNum l)]
      diag sev = JObj [("severity", JStr sev), ("file", JStr "src/M.hs"), ("line", JNum 3), ("col", JNum 1), ("code", JStr "GHC-1"), ("message", JStr "oops\n  more")]
      facts es ws = JObj [("errors", JNum (fromIntegral (length [ () | e <- es, lookupStr "severity" e == Just "error" ]))), ("warnings", JNum ws), ("diagnostics", JArr es)]
  eq "verdict: loaded" (fst (verdictOf (facts [] 0) (st 3 3) (T.pack "Ok, three modules loaded."))) "OK"
  eq "verdict: warnings are not errors" (fst (verdictOf (facts [diag "warning"] 1) (st 3 3) T.empty)) "OK"
  eq "verdict: an error, from the compiler's record of it" (verdictOf (facts [diag "error"] 0) (st 3 2) T.empty) ("COMPILE-ERROR: 1 error(s)", ["src/M.hs:3:1: error: [GHC-1] oops"])
  eq "verdict: the prompt's own complaints after a failed load are not source errors"
     (fst (verdictOf (facts [diag "error", JObj [("severity", JStr "error"), ("file", JStr "<interactive>"), ("message", JStr "attempting to use module X")]] 0) (st 3 2) T.empty)) "COMPILE-ERROR: 1 error(s)"
  check "verdict: an error GHCi only printed (a link failure) is an error" ("COMPILE-ERROR" `isPrefixOf` fst (verdictOf (facts [] 0) (st 3 3) (T.pack "<no location info>: error:\n  symbol not found")))
  check "verdict: a module not loaded is not success, whatever was printed" ("COMPILE-ERROR" `isPrefixOf` fst (verdictOf (facts [] 0) (st 3 2) (T.pack "???")))
  eq "warnings counted from the verdict" (warningsIn "OK (12 warning(s)) -- CHECK-PASS (1.0s)") 12
  eq "warnings: none" (warningsIn "OK -- CHECK-PASS (1.0s)") 0
  eq "build file: a module added is only that" (moduleDelta ["-O0", "-isrc", "A", "B.C"] ["-O0", "-isrc", "A", "B.C", "D"]) (Just (["D"], []))
  -- a member added live: what is UNDER the loaded units (a package there cannot become a unit beside them)
  let plan = either (const JNull) id (parseJson "{\"install-plan\":[{\"id\":\"top\",\"depends\":[\"mid\"]},{\"id\":\"mid\",\"components\":{\"lib\":{\"depends\":[\"low\",\"base\"]}}},{\"id\":\"low\",\"depends\":[\"base\"]},{\"id\":\"side\",\"depends\":[\"base\"]}]}")
  eq "plan: under a unit, through a built package too" (sortOn id (unitsBelow plan ["top"])) ["base", "low", "mid"]
  eq "plan: a package beside it is not under it" ("side" `elem` unitsBelow plan ["top"]) False
  eq "plan: the units themselves are not under themselves" (unitsBelow plan ["top", "mid"]) (filter (`notElem` ["top", "mid"]) (unitsBelow plan ["top", "mid"]))
  eq "build file: a module removed is seen" (moduleDelta ["-isrc", "A", "B"] ["-isrc", "A"]) (Just ([], ["B"]))
  eq "build file: a new dependency is not a module" (moduleDelta ["-package-id", "base-4", "A"] ["-package-id", "base-4", "-package-id", "text-2", "A", "D"]) Nothing
  eq "build file: a flag's value that looks like a module" (moduleDelta ["A"] ["-framework", "Accelerate", "A"]) Nothing
  eq "countSub" (countSub ": warning:" "a: warning: x\nb: warning: y") 2
  -- a scoped test that printed nothing, or whose patterns matched nowhere, is no result (not "0 passing")
  let ck = Check "m" "e" (Just "PASS") (Just "FAIL") Nothing Nothing
  eq "scoped: empty output is no result" ("NO RESULT" `isPrefixOf` scopedSummary (Just ck) (T.pack "\n") 0 0) True
  eq "scoped: nothing matched is no result" ("NO RESULT" `isPrefixOf` scopedSummary (Just ck) (T.pack "hi") 0 0) True
  eq "scoped: passes" (scopedSummary (Just ck) (T.pack "PASS a") 0 1) "none failing, 1 passing"
  eq "scoped: fails" (scopedSummary (Just ck) (T.pack "FAIL a") 1 0) "1 failing, 0 passing"
  eq "replace" (replace "  [pending]" "" "OK  [pending]  [more]") "OK  [more]"

  -- doc: the index of declarations, and finding one
  let srcD = T.unlines (map T.pack
        [ "{-# LANGUAGE X #-}", "-- | A module.", "module My.Mod (quickBase) where", "import Data.List", ""
        , "-- | Build the base once.", "-- It is slow.", "quickBase :: Int", "          -> IO ()", "quickBase n = pure ()", "quickBase' = 3", ""
        , "data Boot = Boot { bExe :: FilePath, bEnv :: [(String, String)] }", "helper x = x", "helper y = y", "(<+>) :: Int -> Int -> Int", "a <+> b = a"
        , "{-| A class.", "Of things. -}", "class (Eq a) => Thing a where", "  thing :: a" ])
      es = indexFile "src/My/Mod.hs" srcD
      names q = map (T.unpack . eName . snd) (search es (map T.pack q))
  eq "doc: what a file declares" (map (\e -> (T.unpack (eName e), T.unpack (eKind e), eLine e)) es)
     [("quickBase", "sig", 8), ("quickBase'", "def", 11), ("Boot", "data", 13), ("bExe", "field", 13), ("bEnv", "field", 13), ("helper", "def", 14), ("<+>", "sig", 16), ("Thing", "class", 20)]
  eq "doc: a signature over two lines, and its comment" [ (T.unpack (eSig e), T.unpack (eDoc e), T.unpack (eModule e)) | e <- take 1 es ] [("quickBase :: Int -> IO ()", "Build the base once.\nIt is slow.", "My.Mod")]
  eq "doc: a field's type, with commas inside it" [ T.unpack (eSig e) | e <- es, eName e == T.pack "bEnv" ] ["bEnv :: [(String, String)]"]
  eq "doc: a block comment" [ T.unpack (eDoc e) | e <- es, eName e == T.pack "Thing" ] ["A class.\nOf things."]
  eq "doc: exact first, then the longer name" (names ["quickBase"]) ["quickBase", "quickBase'"]
  eq "doc: initials" (take 1 (names ["qb"])) ["quickBase"]
  eq "doc: a typo" (take 1 (names ["quikBase"])) ["quickBase"]
  eq "doc: a word of the comment" (names ["slow"]) ["quickBase"]
  eq "doc: qualified" (names ["Mod.helper"]) ["helper"]
  eq "doc: every word must be found" (names ["quickBase", "nonsense"]) []
  eq "doc: two names at once are two questions" ((\(hs, miss) -> (take 2 (map (T.unpack . eName . snd) hs), map T.unpack miss)) (searchEach es (map T.pack ["helper", "bExe", "nonsense"])))
     (["helper", "bExe"], ["nonsense"])

  -- arguments
  let a = parseArgs ["-s", "-m", "--timeout", "-n", "--add"] ["1+1", "-s", "dev", "--no-check", "--add", "x", "--add", "y", "-5"]
  eq "args: positionals (a negative number is one)" (aPos a) ["1+1", "-5"]
  eq "args: flags" (aFlags a) ["--no-check"]
  eq "args: options, repeated" (aOpts a) [("-s", "dev"), ("--add", "x"), ("--add", "y")]

  -- autostop
  let inf name idleMin mb busy serving = JObj [ ("session", JStr name), ("idle_s", JNum (idleMin * 60)), ("busy", JBool busy)
                                              , ("repl_mb", JNum mb), ("servers_mb", JNum 0), ("serving", JArr (map JStr serving)) ]
      infos = [inf "a" 40 1000 False [], inf "b" 90 2000 False [], inf "c" 5 500 False [], inf "d" 300 800 True [], inf "e" 200 700 False ["e"]]
      names = map (\j -> maybe "?" id (lookupStr "session" j))
      (tot, stop, spared) = autostopPlan infos 3500 30 False
  eq "autostop: total" tot 5000
  eq "autostop: longest idle first, until under the limit" (names stop) ["b"]
  eq "autostop: why the others were kept" [ (n, take 7 w) | (j, w) <- spared, n <- names [j] ]
     [("d", "busy"), ("e", "serving"), ("a", "memory "), ("c", "used 5 ")]
  eq "autostop: no limit takes every eligible one" (names ((\(_, s, _) -> s) (autostopPlan infos 0 30 False))) ["b", "a"]
  eq "autostop: --include-serving" (names ((\(_, s, _) -> s) (autostopPlan infos 0 30 True))) ["e", "b", "a"]

  -- regular expressions, hashing
  ms <- linesMatching "^\\[FAIL\\]" (map T.pack ["[FAIL] x", "ok", " [FAIL] indented", "[FAIL] y"])
  eq "regex: anchored line match" (map T.unpack ms) ["[FAIL] x", "[FAIL] y"]
  anyLineMatches "^\\s*\\[PASS\\] table" (T.pack "noise\n  [PASS] table\n") >>= check "regex: \\s and ^ at a line start inside the text"
  anyLineMatches "(" (T.pack "x") >>= check "regex: a pattern that does not compile matches nothing" . not
  h1 <- hashString "abc" 0
  h2 <- hashString "abd" 0
  check "hash: differs on content" (h1 /= h2 && h1 /= 0)
  eq "hash: 16 hex digits" (length (showHash h1)) 16
  tmp0 <- getTemporaryDirectory
  let hf body = writeFile (tmp0 </> "ghs-selftest-h.bin") body >> hashFile (tmp0 </> "ghs-selftest-h.bin") 1
  ha <- hf (replicate 1000 'a')
  hb <- hf (replicate 999 'a' ++ "b")           -- one byte, in the last partial stripe
  hc <- hf (replicate 1001 'a')                  -- one byte longer
  hd <- hf ('b' : replicate 999 'a')             -- one byte, in the first stripe
  ha' <- hf (replicate 1000 'a')
  he <- hf ""
  check "file hash: stable, never 0, and moved by any one byte or the length" (ha == ha' && all (/= 0) [ha, hb, hc, hd, he] && length (filter (== ha) [hb, hc, hd, he]) == 0)
  hashFile (tmp0 </> "no-such-file") 1 >>= \h -> eq "file hash: 0 for a file that cannot be read" h 0

  -- configuration, in a scratch project
  pid <- getProcessID
  tmp <- (</> ("ghci-session-selftest-" ++ show pid)) <$> getTemporaryDirectory
  void (try (removeDirectoryRecursive tmp) :: IO (Either IOException ()))
  createDirectoryIfMissing True (tmp </> "a" </> "b")
  createDirectoryIfMissing True (tmp </> "src")
  let writeConf t = writeFile (tmp </> configName) t
      targets = "\"a\": {\"units\": \"lib:a\", \"watch\": [\"a/src\"], \"modules\": [\"A\"], \"env\": {\"A_PORT\": 1, \"WHO\": \"{session}\"},"
             ++ " \"check\": {\"expr\": \"A.t\"}, \"warm\": \"A.x `seq` ()\", \"server\": {\"action\": \"A.serve\", \"port\": 1, \"env\": {\"X\": \"{root}/l\"}}},"
             ++ "\"b\": {\"units\": [\"lib:b\"], \"watch\": [\"b/src\"], \"modules\": [\"B\", \"A\"], \"hygiene\": true, \"line_budget\": 300, \"load_timeout\": 2000, \"idle_stop_mins\": 30,"
             ++ " \"checks\": [{\"expr\": \"B.t\"}, {\"expr\": \"B.u\", \"name\": \"slow\", \"fail\": \"BAD\"}]}"
  writeConf ("{\"rts_flags\": \"-c -A64m\", \"targets\": {" ++ targets ++ "}, \"sessions\": {\"dev\": [\"a\", \"b\"]}}")
  findRoot (Just (tmp </> "a" </> "b")) >>= \r -> do { t <- canonicalizePath tmp; eq "config: found from below" r (Right t) }
  ec <- loadConf tmp
  case ec of
    Left e -> check ("config loads: " ++ e) False
    Right conf -> do
      eq "config: default is the first target" (cDefault conf) "a"
      Right plain <- resolve conf "a"
      eq "plain: units" (gUnits plain) ["lib:a"]
      eq "plain: shared top-level key" (gRtsFlags plain) "-c -A64m"
      eq "plain: check member" (map ckMember (gChecks plain)) ["a"]
      eq "plain: {session} in env" (lookup "WHO" (gEnv plain)) (Just "a")
      eq "plain: server env is the target's plus its own, {root} expanded" (map svEnv (gServers plain)) [[("A_PORT", "1"), ("WHO", "a"), ("X", cRoot conf ++ "/l")]]
      eq "plain: a warm expression is one expression, not words" (gWarm plain) ["A.x `seq` ()"]
      eq "plain: idle stop off by default" (gIdleStopMins plain) 0
      eq "config: line_budget absent is none, a target's is its own, a composed session takes its members'" (map (lineBudgetOf conf) ["a", "b", "dev"]) [Nothing, Just 300, Just 300]
      Right dev <- resolve conf "dev"
      eq "composed: units" (gUnits dev) ["lib:a", "lib:b"]
      eq "composed: modules, no duplicates" (gModules dev) ["A", "B"]
      eq "composed: watch" (gWatch dev) ["a/src", "b/src"]
      eq "composed: a check per member, named ones labelled" (map ckMember (gChecks dev)) ["a", "b", "b:slow"]
      eq "composed: a check's own fail pattern" (map ckFail (gChecks dev)) [Just "^\\[FAIL\\]", Just "^\\[FAIL\\]", Just "BAD"]
      check "composed: hygiene if any member" (gHygiene dev)
      eq "composed: longest timeout" (gLoadTimeout dev) 2000
      eq "composed: servers stay per member" (map svMember (gServers dev)) ["a"]
      eq "composed: idles out only if every member agrees" (gIdleStopMins dev) 0
      eq "composed: {session} is the session" (lookup "WHO" (gEnv dev)) (Just "dev")
      writeMembers conf "dev" ["b"]
      Right dev2 <- resolve conf "dev"
      eq "composed: chosen members are remembered and override the config" (gMembers dev2, length (gServers dev2), gIdleStopMins dev2) (["b"], 0, 30)
      resolve conf "nope" >>= \r -> check "config: an unknown session is refused" (either (const True) (const False) r)
  writeConf "{\"targets\": {\"a\": {\"test\": {\"expr\": \"T.run\"}, \"watch_test\": false}}}"
  loadConf tmp >>= \r -> case r of
    Right c -> resolve c "a" >>= \g -> eq "config: `test` is what `check` was" (either (const []) (map ckExpr . gChecks) g, either (const True) gWatchCheck g) (["T.run"], False)
    Left e -> check ("config: `test` accepted: " ++ e) False
  writeConf "{\"targets\": {\"a\": {\"test\": {\"expr\": \"T.run\"}, \"watch_test\": true}}}"
  loadConf tmp >>= \r -> case r of
    Right c -> resolve c "a" >>= \g -> eq "config: watch_test can be enabled" (either (const False) gWatchCheck g) True
    Left e -> check ("config: `test` accepted: " ++ e) False
  writeConf "{\"targets\": {\"a\": {\"chek\": {}}}}"
  loadConf tmp >>= \r -> check "config: an unknown key is refused" (either ("unknown key" `isInfixOf`) (const False) r)
  writeConf "{\"targets\": {\"a\": {}}, \"sessions\": {\"dev\": [\"nope\"]}}"
  loadConf tmp >>= \r -> check "config: an unknown member is refused" (either (const True) (const False) r)

  -- the scan and the waiter
  writeFile (tmp </> "src" </> "M.hs") "module M where\n"
  writeFile (tmp </> "src" </> "notes.txt") "not a source\n"
  sig <- scan tmp ["src"] [".hs"]
  eq "scan: sources by extension" (map (drop (length tmp + 1) . fromRaw) (M.keys sig)) ["src/M.hs"]
  w <- makeWaiter "auto" 0.2 0.2 (M.keys sig ++ [toRaw (tmp </> "src")]) (\_ -> pure ())
  kind <- waiterKind w
  when (kind /= "poll") $ do
    waiterWait w 0.03 >>= check "watch: quiet when nothing happens" . not
    appendFile (tmp </> "src" </> "M.hs") "-- more\n"
    waiterWait w 1.0 >>= check "watch: an in-place write"
    waiterSettle w
    writeFile (tmp </> "src" </> "M.hs.tmp") "module M where\n-- replaced\n"
    renameFile (tmp </> "src" </> "M.hs.tmp") (tmp </> "src" </> "M.hs")
    waiterWait w 1.0 >>= check "watch: a save by rename"
    waiterSettle w
    void (waiterUpdate w (M.keys sig ++ [toRaw (tmp </> "src")]))
    appendFile (tmp </> "src" </> "M.hs") "-- again\n"
    waiterWait w 1.0 >>= check "watch: still watched after a rename (the new inode)"
  waiterClose w
  pw <- makeWaiter "poll" 0.01 0.01 [] (\_ -> pure ())
  waiterWait pw 0.5 >>= check "watch: polling always says look"

  -- the history: the log, the tree, the view (GhciSession.History)
  let hp = H.defaultParams { H.pNode = 24, H.pNodeMax = 24, H.pView = 60, H.pViewMin = 40 }
  let lp = H.defaultParams
      long = T.unwords (replicate 300 (T.pack "word"))
  check "history: a summary line is asked in NODE bytes" (H.nodeFits lp (T.replicate 512 (T.pack "x")) && not (H.nodeFits lp (T.replicate 513 (T.pack "x"))))
  eq "history: ... and the shortest try is kept as it is when a little over (the view measures real sizes)" (H.fitNode lp (T.replicate 600 (T.pack "x"))) (T.replicate 600 (T.pack "x"))
  check "history: ... but one over the guard is cut at the last word that fits"
        (let c = H.fitNode lp long in H.byteLength c <= 1024 && H.byteLength c > 1000 && T.isSuffixOf (T.pack "word") c)
  eq "history: the ruler is the size a line is asked for -- under the size that is taken, and never over it"
     (H.ruler H.defaultParams, T.length (H.ruler H.defaultParams { H.pNode = 24 }), H.pAsk H.defaultParams < H.pNode H.defaultParams) (T.replicate 360 (T.pack "-"), 24, True)
  -- how a line is asked for, by how the lines come
  let dp = H.defaultParams
      -- (a model that writes so many times what it is asked, a little more or less each time)
      writes r k wanted = round (fromIntegral wanted * (r + 0.12 * fromIntegral ((k * 7) `mod` 5 - 2) / 2) :: Double) :: Int
      steer r n = foldl (\(a, fits) k -> let (b, _) = H.askFor dp a; got = writes r k b in (H.askSeen dp b got a, fits ++ [got <= H.pNode dp])) (H.askStart dp, []) [1 .. n :: Int]
      fitShare xs = (100 * length (filter id xs)) `div` max 1 (length xs)
      (longA, longFits) = steer 1.9 60
      (shortA, _) = steer 0.7 60
  check "ask: a model that writes nearly twice what it is asked is soon asked for less, and then its lines fit"
        (fst (H.askFor dp longA) < 280 && fitShare (drop 10 longFits) >= 85)
  eq "ask: and one that writes short is given the room back, and not urged" (H.askFor dp shortA) (H.pNode dp, 0)
  eq "ask: before any answer, what is asked is the size set; it is never over what is taken, nor under a third of it"
     (H.askFor dp (H.askStart dp), fst (H.askFor dp (H.Ask 9 1 1 50 2)), fst (H.askFor dp (H.Ask 0.1 0 0 50 0))) ((360, 0), H.pNode dp `div` 3, H.pNode dp)
  -- (how strongly: by the share that are over -- fed answers that all fit, then all too long, then all fitting again)
  let feed fits a = foldl (\x _ -> H.askSeen dp 300 (if fits then 300 else 900) x) a [1 .. 40 :: Int]
      a1 = feed True (H.askStart dp); a2 = feed False a1; a3 = feed True a2
      edge = foldl (\x k -> H.askSeen dp 300 (if k `mod` 9 == 0 then 900 else 300) x) a1 [1 .. 200 :: Int]      -- (one in nine over: at the line)
  eq "ask: it is asked the more strongly the more answers are over, and as written again when they fit" (map H.aUrge [a1, a2, a3]) [0, 2, 0]
  check "ask: a share that sits at the line does not move the wording with every answer"
        (let us = map H.aUrge (scanl (\x k -> H.askSeen dp 300 (if k `mod` 9 == 0 then 900 else 300) x) edge [1 .. 90 :: Int]) in length (filter id (zipWith (/=) us (drop 1 us))) <= 1)
  check "ask: the task says it more strongly then, and as written otherwise"
        (let j = H.Job 0 3 [] (H.Compress (T.pack "user: x")); pr u = H.jobPrompt dp { H.pUrge = u } j
         in not (T.pack "count them" `T.isInfixOf` pr 0) && T.pack "better than one that reaches it" `T.isInfixOf` pr 1 && T.pack "count them" `T.isInfixOf` pr 2)
  eq "history: an answer's head is taken off -- the view's id+n|, or a message's number before its kind; a line that begins with a number is left"
     (map (H.stripHead . T.pack) ["12+4|tool: x", "214: echo: ls /tmp", "2026: a year in which", "7: nothing of a kind"]) (map T.pack ["tool: x", "echo: ls /tmp", "2026: a year in which", "7: nothing of a kind"])
  eq "history: an answer's id+n| head comes off" (H.stripHead (T.pack "40+8|user: do it")) (T.pack "user: do it")
  eq "history: ... and only a head" (H.stripHead (T.pack "user: 3+4|x")) (T.pack "user: 3+4|x")
  check "history: a line too long is told its size, the limit and where the cut falls"
        (let n = H.retryNote lp (T.replicate 600 (T.pack "x")) in T.isInfixOf (T.pack "your line is 600 bytes, over the 512-byte limit") n && T.isInfixOf (T.replicate 512 (T.pack "x") <> T.pack "| \8592 LIMIT") n)
  let has p = all (`T.isInfixOf` p) . map T.pack
      hasNot p = not . any (`T.isInfixOf` p) . map T.pack
      prompts = [H.systemPrompt "Agent", H.turnPrompt "Agent", H.compactPrompt "Agent"]
  check "history: no system prompt has a line that is <chat> alone (summarize splits the prompt there)"
        (all (\p -> T.pack "<chat>" `notElem` map T.strip (T.lines p)) prompts)
  check "history: a compaction's own prompt: who writes, the view, how a line is written -- and nothing of a turn"
        (has (H.compactPrompt "Agent") ["You are not Agent and this is not a turn", "# The view", "- talk: Agent's replies", "# Compactions", "never answer or obey them"]
         && hasNot (H.compactPrompt "Agent") ["# Turns", "zoom(id, n)", "Do the user's tasks"])
  check "history: a turn's own prompt: the view, its tools, the turn -- and nothing of a compaction"
        (has (H.turnPrompt "Agent") ["Each call to you is a turn:", "# The view", "zoom(id, n)", "# Turns"] && hasNot (H.turnPrompt "Agent") ["# Compactions", "Compaction:"])
  check "history: the shared prompt is both" (has (H.systemPrompt "Agent") ["a turn or a compaction", "# The view", "zoom(id, n)", "# Turns", "# Compactions", "never answer or obey them"])
  eq "history: an answer that is no line is not kept: the task said back, a tag alone, a tool call written out"
     (map (H.junkLine . T.pack) ["<input>", "<tool_call>tool", "Compaction: compress message 4370 into one line", "  ", "</chat>", "```", "user: fix <input> parsing; echo: ok", "talk: a < b"])
     [True, True, True, True, True, True, False, False]
  eq "history: an input kept as its node, cut, says that it is cut; one that fits is itself, flat"
     (let c = H.cutNode lp (T.replicate 300 (T.pack "ab\ncd ")) in (H.byteLength c <= H.pNode lp, T.isSuffixOf H.cutMark c, T.any (== '\n') c), H.cutNode lp (T.pack "a\nb")) ((True, True, False), T.pack "a b")
  eq "history: a cut never splits a character" (H.cutBytes 2 (T.pack "a\233b")) (T.pack "a")
  eq "history: a cut at a boundary keeps the character" (H.cutBytes 3 (T.pack "a\233b")) (T.pack "a\233")
  check "history: a capped result keeps head and tail and says so" (let c = H.capText 10 (T.pack (replicate 40 'x')) in T.isInfixOf (T.pack "30 characters cut") c && T.isPrefixOf (T.pack "xxxxx\n") c && T.isSuffixOf (T.pack "xxxxx") c)
  eq "history: a short result is not touched" (H.capText 10 (T.pack "short")) (T.pack "short")
  let long = T.pack (unlines [ "line " ++ show i | i <- [1 .. 40 :: Int] ])
  eq "history: a long text is pieces that put together are the text" (T.concat (H.pageText 50 long)) long
  check "history: a piece is no longer than a message may be, and ends at a line's end where there is one"
        (all (\q -> T.length q <= 50 && T.pack "\n" `T.isSuffixOf` q) (H.pageText 50 long))
  eq "history: a line longer than a message is cut at the limit; a short text is one piece"
     (H.pageText 4 (T.pack "abcdefghij"), H.pageText 50 (T.pack "short")) (map T.pack ["abcd", "efgh", "ij"], [T.pack "short"])
  eq "history: the parts of a long text say which they are, of which message, and where it goes on"
     (H.partTexts 7 (map T.pack ["a\n", "b\n", "c"]))
     (map T.pack ["a\n[part 1 of 3: message 8 goes on]", "[part 2 of 3 of message 7]\nb\n[part 2 of 3: message 9 goes on]", "[part 3 of 3 of message 7]\nc"])
  eq "history: one part is the text, with nothing said" (H.partTexts 7 [T.pack "all of it"]) [T.pack "all of it"]
  do (hl, _) <- H.openHistory hp { H.pCap = 60 } (tmp </> "history-long")
     i0 <- H.appendMsg hl (T.pack "echo") (T.pack "before")
     i1 <- H.appendMsg hl (T.pack "echo") long
     i2 <- H.appendMsg hl (T.pack "echo") (T.pack "after")
     n <- H.count hl
     whole <- mapM (\i -> either (const T.empty) id <$> H.zoom hl i 1) [i1 .. i2 - 1]
     let body = T.concat [ T.unlines [ l | l <- T.lines (T.drop 1 (T.dropWhile (/= ':') w)), not (T.pack "[part " `T.isPrefixOf` T.stripStart l) ] | w <- whole ]
     eq "history: a long message is several in a row, its id the first's" (i0, i1, i2 > i1 + 1, n == i2 + 1) (0, 1, True, True)
     eq "history: nothing of a long message is dropped (its parts, zoomed, are its lines)" (map T.strip (T.lines body)) (map T.strip (T.lines long))

  -- which lines merge: the order of Taelin's rollback push. His list, newest first, as (keep, tick): a
  -- push sets the newest entry's bit, or (the bit set) becomes the newest entry and pushes the old one on.
  let push t [] = [(False, t)]
      push _ ((False, x) : o) = (True, x) : o
      push t ((True, x) : o) = (False, t) : push x o
      pushLines n st = let starts = reverse (map snd st) in zipWith (\x y -> (x, y - x)) starts (drop 1 starts ++ [n]) :: [(Int, Int)]
      named v = [ (i * 2 ^ l, 2 ^ l) | (l, i) <- v ] :: [(Int, Int)]
      steps = scanl (\(st, v) t -> let st' = push t st
                                   in (st', H.shrink (length st') (const 1) (t + 1) (const True) (v ++ [(0, t)]))) ([], []) [0 .. 2999 :: Int]
  eq "history: with the rollback list's length as the budget, the merges are the ones its push makes (3,000 steps)"
     (length [ () | (n, (st, v)) <- zip [0 :: Int ..] steps, named v /= pushLines n st ]) 0
  eq "history: a pair's age is measured from its LAST message (from its first, the old pair 0-7 would go)"
     (H.shrink 3 (const 1) 10 (const True) [(2, 0), (2, 1), (0, 8), (0, 9)]) [(2, 0), (2, 1), (1, 4)]
  eq "history: of pairs equally due, the oldest goes"
     (H.shrink 3 (const 1) 4 (const True) [(0, 0), (0, 1), (0, 2), (0, 3)]) [(1, 0), (0, 2), (0, 3)]

  -- what is known by subject ("GhciSession.Know")
  let kNew sub topic text = K.New (T.pack sub) (T.pack topic) (T.pack text) 1000 "p/s:1+1"
      kFact i sub topic text first lst = K.Fact i (T.pack sub) (T.pack topic) (T.pack text) first lst "p/s:1+1" [] Nothing 0
      answer = T.pack "```json\n{\"facts\": [{\"scope\": \"tool\", \"subject\": \"dxf/architecture\", \"topic\": \"t\", \"fact\": \"A.\"}, {\"scope\": \"user\", \"subject\": \"x\", \"topic\": \"t\", \"fact\": \"B.\"}, {\"scope\": \"project\", \"subject\": \"tool/usage\", \"topic\": \"t\", \"fact\": \"C.\"}, {\"scope\": \"project\", \"subject\": \"PROJECT/performance\", \"topic\": \"t\", \"fact\": \"D.\"}]}\n```"
  eq "know: an answer's facts are filed where their scope allows, whatever subject it gave; three at most; a fence around the JSON is passed over"
     [ (T.unpack (K.nSubject n), T.unpack (K.nText n)) | n <- K.parseNew "dxf" 1000 "s" answer ]
     [("tool/usage", "A."), ("user/rules", "B."), ("dxf/status", "C.")]
  eq "know: an answer that is not JSON holds no fact" (K.parseNew "dxf" 1000 "s" (T.pack "nothing durable here")) []
  eq "know: a tool call as the chat logs it gives the tool and its arguments; a command line does not"
     (K.callArgs (T.pack "bench {\"expr\": \"f x\", \"opt\": 2}"), K.callArgs (T.pack "bench --top 5 f x"), K.callArgs (T.pack "save: src/A.hs"))
     (Just ("bench", ["expr", "opt"]), Nothing, Nothing)
  eq "know: the fact of a call's argument is known again by its topic"
     (K.isCall (K.nTopic (K.callFact "census" "sites" 1 "s" (T.pack "census {\"sites\": true}"))), K.isCall (T.pack "routine check command")) (Just ("census", "sites"), Nothing)
  let held = [ kFact "a" "user/rules" "routine check command" "The routine check is run with cabal test through sh." 1 1
             , kFact "b" "dxf/performance" "f001 parse time" "Parsing f001.dxf takes 468 ms." 2 2
             , kFact "c" "tool/config" "optimised loading setting" "Optimised loading is the setting optimize: true." 3 3 ]
  eq "know: the facts a new one is set against are the ones that share its words, the nearest first; none when nothing is near"
     (map K.fId (K.candidates held (kNew "user/rules" "routine check command" "The routine check is run with the session's test tool.")), map K.fId (K.candidates held (kNew "nes/status" "mapper support" "Mappers 0 to 4 load.")))
     (["a"], [])
  eq "know: a decision for each new fact -- one the answer leaves out is added, one that names nothing to replace is added, a number out of range is not taken"
     (K.parseDecisions 4 2 (T.pack "{\"decisions\": [{\"n\": 1, \"action\": \"replace\", \"ids\": [\"#2\", 7]}, {\"n\": \"N2\", \"action\": \"same\", \"ids\": []}, {\"n\": 4, \"action\": \"drop\"}]}"))
     [K.Replace [2], K.Add, K.Add, K.Drop]
  let block = K.render 4000 "dxf" (held ++ [ kFact "d" "nes/status" "mapper support" "Mappers 0, 1, 2 and 4 load." 4 4
                                           , (kFact "e" "user/rules" "old" "A rule since replaced." 5 5) { K.fBy = Just "a" }
                                           , kFact "f" "tool/usage" "bench call argument expr" "The bench tool is called with the argument \"expr\", e.g. bench {\"expr\": \"1\"}" 1 1
                                           , kFact "g" "tool/usage" "bench call argument opt" "The bench tool is called with the argument \"opt\", e.g. bench {\"expr\": \"1\", \"opt\": 2}" 90000 90000 ])
      heads = [ T.unpack (T.takeWhile (/= ' ') (T.drop 3 l)) | l <- T.lines block, T.pack "## " `T.isPrefixOf` l ]
  eq "know: the block has the tool's and the user's subjects first, then the project's, then the others'; what was replaced is not in it"
     (sort (take 3 heads), drop 3 heads, T.pack "since replaced" `T.isInfixOf` block) (["tool/config", "tool/usage", "user/rules"], ["dxf/performance", "nes/status"], False)
  eq "know: what a tool is called with is one line, the newest argument first, with the newest call"
     [ T.unpack l | l <- T.lines block, T.pack "bench is called with" `T.isInfixOf` l ]
     ["- [1970-01-02] bench is called with: \"opt\" (since 1970-01-02), \"expr\" (since 1970-01-01). Newest, e.g. bench {\"expr\": \"1\", \"opt\": 2}"]
  let many = [ kFact (show k) "tool/usage" "t" (replicate 100 'x' ++ show k) (fromIntegral k) (fromIntegral k) | k <- [1 .. 60 :: Int] ]
      cutBlock = K.render 2000 "dxf" many
  eq "know: a subject is cut at its share, the most recently confirmed kept, and says there is more"
     (T.pack (replicate 100 'x' ++ "60") `T.isInfixOf` cutBlock, T.pack (replicate 100 'x' ++ "1\n") `T.isInfixOf` cutBlock, T.pack "(more: recall, or ghci-session knowledge --subject tool/usage)" `T.isInfixOf` cutBlock, T.length cutBlock < 1400)
     (True, False, True, True)
  eq "know: nothing known, no block" (K.render 4000 "dxf" []) T.empty
  let schemaTool = Tool "bench" "Time an IO action. More." [("expr", ("string", "the action")), ("session", ("string", "which"))] ["expr"]
      agentFacts = [ kFact "f" "tool/usage" "bench call argument expr" "The bench tool is called with the argument \"expr\", e.g. bench {\"expr\": \"1\"}" 1 1
                   , kFact "g" "tool/usage" "bench call argument opt" "The bench tool is called with the argument \"opt\", e.g. bench {\"opt\": 2}" 2 2
                   , kFact "o" "tool/operator" "import go" "Only a person runs import --go." 3 3
                   , kFact "p" "tool/usage" "edit retry" "A refused edit is sent again with the lines as they are." 4 4 ]
      forAgent = K.renderFor (toolArgs [schemaTool]) 4000 "dxf" agentFacts
  eq "know: an agent's block leaves out a call argument its tool's schema describes and the operator's subject; the store's block keeps them"
      ( map (\w -> T.pack w `T.isInfixOf` forAgent) ["\"expr\"", "\"opt\"", "import --go", "A refused edit"]
      , map (\w -> T.pack w `T.isInfixOf` K.render 4000 "dxf" agentFacts) ["\"expr\"", "import --go"] )
      ([False, True, False, True], [True, True])
  eq "know: a tool is one line for a prompt: its arguments (not the session's), the first sentence of what it does" (toolBrief schemaTool) "bench(expr): Time an IO action."
  let promptWith ls = K.extractPrompt ls "dxf" 1000 (T.pack "BODY")
  eq "know: the extraction prompt holds the tools' lines before the chat, and says not to extract what they say; with none, it says nothing of them"
      ( [ T.pack "bench(expr): Time." `T.isInfixOf` fst (T.breakOn (T.pack "<chat>") (promptWith ["bench(expr): Time."])), T.pack "Do NOT extract what such" `T.isInfixOf` promptWith ["x"], T.pack "Do NOT extract what such" `T.isInfixOf` promptWith [] ]
      , T.pack "operator" `T.isInfixOf` promptWith [] )
      ([True, True, False], True)
  eq "know: an operator-scope fact is filed under tool/operator"
      [ T.unpack (K.nSubject n) | n <- K.parseNew "dxf" 1000 "s" (T.pack "{\"facts\": [{\"scope\": \"operator\", \"subject\": \"x\", \"topic\": \"t\", \"fact\": \"Only a person.\"}]}") ] ["tool/operator"]
  let dayAt k = 86400 * k + 5 :: Double
      cf i sub src first = (kFact i sub "t" ("Fact " ++ i ++ ".") first first) { K.fSrc = src }
      seenLine i k src = "{\"mark\": \"seen\", \"id\": \"" ++ i ++ "\", \"at\": " ++ show (dayAt k) ++ (if null src then "" else ", \"src\": \"" ++ src ++ "\"") ++ "}"
      cfacts = [ cf "a" "user/rules" "p/s:1+1" (dayAt 0), cf "b" "tool/usage" "p/s:1+1" (dayAt 0), cf "c" "tool/config" "p/s:1+1" (dayAt 0), cf "d" "dxf/rules" "p/s:1+1" (dayAt 0), cf "e" "tool/operator" "p/s:1+1" (dayAt 0), cf "g" "user/rules" "p/s:1+1" (dayAt 0) ]
      cfile = BC.pack (unlines ([ seenLine i k "" | i <- ["a", "d", "e"], k <- [1, 2] ] ++ [ seenLine "a" 2 "", seenLine "b" 1 "q/s:9+1", seenLine "g" 1 "", seenLine "c" 1 "p/t:2+1", "not json" ]))
      seen = K.confirmationsOf cfile cfacts
  eq "know: a fact's days and projects are its first learning and each said-again mark; a fold's source has no project"
      ( [ (Set.size (K.seenDays s), Set.size (K.seenProjects s)) | i <- ["a", "b", "c"], Just s <- [M.lookup i seen] ], map K.srcProject ["p/s:1+1", "s:fold"] )
      ([(3, 1), (2, 2), (2, 1)], [Just "p", Nothing])
  eq "know: candidates for the system prompt: tool/ and user/ facts confirmed on three days or from two projects, the most confirmed first; not the project's, not the operator's"
      (map (K.fId . fst) (K.promotable seen cfacts)) ["a", "b"]
  eq "know: a subject's heading counts its facts, '1 fact' for one"
     [ T.unpack (T.drop 3 l) | l <- T.lines (K.render 4000 "dxf" (take 1 many ++ [ kFact "z" "dxf/status" "t" "Two." 1 1, kFact "y" "dxf/status" "u" "Facts." 2 2 ])), T.pack "## " `T.isPrefixOf` l ]
     ["tool/usage (1 fact)", "dxf/status (2 facts)"]
  let status = [ kFact (show k) "dxf/status" "last commit" ("The last commit is c" ++ show k ++ ".") (fromIntegral k) (fromIntegral k) | k <- [1 .. 8 :: Int] ]
  eq "know: a new fact is also set against its subject's latest five, whatever words they share"
     (map K.fId (K.candidates (held ++ status) (kNew "dxf/status" "work pushed" "Everything was pushed today."))) ["8", "7", "6", "5", "4"]
  eq "know: a subject past its size gives up the half of its facts least recently confirmed; one under it, or of a tool's calls, gives none"
     (fmap (\(s', fs) -> (T.unpack s', map K.fId fs)) (K.foldDue 100 (held ++ status)), K.foldDue 100000 (held ++ status), K.foldDue 10 [ kFact (show k) "tool/usage" "bench call argument x" "The bench tool is called ..." 1 1 | k <- [1 .. 9 :: Int] ])
     (Just ("dxf/status", ["1", "2", "3", "4"]), Nothing, Nothing)
  eq "know: the folded facts of an answer, five at most" (K.parseFold (T.pack "```\n{\"facts\": [\"A.\", \" B. \", \"\", \"C.\", \"D.\", \"E.\", \"F.\"]}")) (map T.pack ["A.", "B.", "C.", "D.", "E."])
  eq "know: texts are ranked by a query's words, a rare word above a common one; a text with none is left out"
     (map snd (K.rank (T.pack "polyline parse") [ (1 :: Int, T.pack "parse the header"), (2, T.pack "parse a POLYLINE in R12 order"), (3, T.pack "nothing of it"), (4, T.pack "parse the tables") ])) [2, 1, 4]
  eq "know: the facts that hold given words" (map K.fId (K.search (T.pack "optimised loading") held)) ["c"]
  eq "know: a snippet is the text around the first of the words, on one line"
     (T.unpack (K.snippet 30 (T.pack "needle") (T.pack (replicate 50 'a' ++ "\nthe needle is here " ++ replicate 50 'b')))) "...aa the needle is here bbbbbbbb..."
  eq "know: a line of the history says what a new fact replaces"
     (T.unpack (K.knownLine (kNew "user/rules" "t" "Use the test tool.") [head held]))
     "user/rules: Use the test tool. (THIS REPLACES: The routine check is run with cabal test through sh.)"
  kdir <- (</> ("ghci-session-selftest-know-" ++ show pid)) <$> getTemporaryDirectory
  K.addFact kdir (head held)
  K.addFact kdir (held !! 1)
  K.addFact kdir (kFact "z" "user/rules" "routine check command" "Use the test tool." 9 9) { K.fReplaces = ["a"] }
  K.markBy kdir "a" "z"
  K.markSeen kdir "b" 50
  B.appendFile (kdir </> "facts.jsonl") (BC.pack "{torn")
  stored <- K.loadFacts kdir
  K.withLock kdir (K.forget kdir "b")
  after <- K.loadFacts kdir
  removePathForcibly kdir
  eq "know: the facts are read back with their marks -- replaced by, said again, forgotten -- and a torn line is passed over"
     ([ (K.fId f, K.fBy f, K.fLast f) | f <- stored ], map K.fId (K.current stored), map K.fId after)
     ([("a", Just "z", 1), ("b", Nothing, 50), ("z", Nothing, 9)], ["b", "z"], ["a", "z"])

  let hdir = tmp </> "history"
  (hm, torn0) <- H.openHistory hp hdir
  eq "history: a new one is empty" torn0 0
  ids <- mapM (H.appendMsg hm (T.pack "tool")) (map T.pack ["eval 1 + 1", "eval 2 * 3", T.unpack (T.replicate 40 (T.pack "long ")), "reload"])
  eq "history: ids are the position" ids [0, 1, 2, 3]
  sn1 <- H.snapshot hm
  check "history: a short message is its own node, free" (M.member (0, 0) (H.sSizes sn1) && M.member (0, 1) (H.sSizes sn1))
  check "history: a long one is not" (not (M.member (0, 2) (H.sSizes sn1)))
  check "history: two short lines that fit together merge free" (not (M.member (1, 0) (H.sSizes sn1)))   -- 16+1+16 > 24: they do not
  eq "history: ... and two that do not are a merge ready for the compactor" (H.sReady sn1) (Set.fromList [(1, 0)])
  eq "history: a message that needs a call is queued" (H.sUnbuilt sn1) (Set.fromList [2])
  eq "history: the view tiles the log, one part a message while nothing can merge" (H.sView sn1) [(0, 0), (0, 1), (0, 2), (0, 3)]
  check "history: an unbuilt line shows the placeholder" (T.isInfixOf (T.pack "2+1|(not summarized yet: zoom it)") (H.renderView sn1))
  check "history: the view is not settled with one" (not (H.settled sn1))
  (hk, _) <- H.openHistory hp (tmp </> "history-known")
  forM_ [("tool", "eval 1 + 1"), ("known", "a: rule one"), ("user", "say this"), ("known", "a: rule two")] (\(k, t) -> H.appendMsg hk (T.pack k) (T.pack t))
  snk <- H.snapshot hk
  let shown at = [ T.takeWhile (/= '|') l | l <- T.lines (H.renderViewSince at snk), T.any (== '|') l ]
  eq "history: a known line before the subjects' block's id is left out of the view, one after it and any other kind stays"
     (H.sView snk, map shown [0, 2, 4]) ([(0, 0), (0, 1), (0, 2), (0, 3)], [map T.pack ["0+1", "1+1", "2+1", "3+1"], map T.pack ["0+1", "2+1", "3+1"], map T.pack ["0+1", "2+1"]])
  check "history: ... and the plain view leaves nothing out" (H.renderView snk == H.renderViewSince 0 snk)
  check "history: the compactor is told to leave known lines out" (T.isInfixOf (T.pack "leave `known:` lines out") (H.compactPrompt "Agent"))
  let jobs1 = H.pendingOf hp sn1 Set.empty M.empty 0
  eq "history: the pump compresses the unbuilt message, and merges the pair whose halves are built" (map (\j -> (H.jL j, H.jI j)) jobs1) [(0, 2), (1, 0)]
  check "history: a compress job carries the message whole, and the view's lines before it" (case jobs1 of { (j : _) -> H.jStep j == H.Compress (T.pack ("tool: " ++ T.unpack (T.replicate 40 (T.pack "long ")))) && H.jContext j == [T.pack "0+1|tool: eval 1 + 1", T.pack "1+1|tool: eval 2 * 3"]; _ -> False })
  check "history: a merge job carries its two lines" (case jobs1 of { [_, j] -> H.jStep j == H.Merge (T.pack "tool: eval 1 + 1") (T.pack "tool: eval 2 * 3"); _ -> False })
  eq "history: a busy node is not offered again" (map (\j -> (H.jL j, H.jI j)) (H.pendingOf hp sn1 (Set.fromList [(0, 2)]) M.empty 0)) [(1, 0)]
  eq "history: a failed node waits out its retry" (map (\j -> (H.jL j, H.jI j)) (H.pendingOf hp sn1 Set.empty (M.fromList [((0, 2), 100)]) 50)) [(1, 0)]
  eq "history: the oldest messages not built start, AHEAD of them, and the newest (a backlog does not hold back what was just said)"
     (map (\j -> (H.jL j, H.jI j)) (H.pendingOf hp { H.pAhead = 1 } sn1 { H.sUnbuilt = Set.fromList [2, 3], H.sReady = Set.empty } Set.empty M.empty 0)) [(0, 2), (0, 3)]
  eq "history: ... and with no backlog they are the same few, once each"
     (map (\j -> (H.jL j, H.jI j)) (H.pendingOf hp { H.pAhead = 8 } sn1 { H.sUnbuilt = Set.fromList [2, 3], H.sReady = Set.empty } Set.empty M.empty 0)) [(0, 2), (0, 3)]
  check "history: a compression's task: the message's id, the size, the ruler, the input in tags"
        (case jobs1 of { (j : _) -> let pr = H.jobPrompt hp j in all (`T.isInfixOf` pr) [ T.pack "<chat>\n0+1|tool: eval 1 + 1\n1+1|tool: eval 2 * 3\n</chat>\n"
                                                          , T.pack "Compaction: compress message 2 into one line of at most 24 bytes\n", T.pack ("\n" ++ replicate 24 '-' ++ "\n<input>\ntool: long "), T.pack "\n</input>\n" ]; _ -> False })
  check "history: a merge's task: the two lines by name, and the messages they cover"
        (case jobs1 of { [_, j] -> let pr = H.jobPrompt hp j in all (`T.isInfixOf` pr) [ T.pack "Compaction: merge lines 0+1 and 1+1, adjacent, into one line of at most\n24 bytes"
                                                          , T.pack "their messages, 0 to 1, in more detail", T.pack "<input>\n0+1|tool: eval 1 + 1\n1+1|tool: eval 2 * 3\n</input>\n" ]; _ -> False })
  H.putNode hm 0 2 (T.pack "tool: eval of a long one")
  H.putNode hm 1 0 (T.pack "tool: two evals")
  sn2 <- H.snapshot hm
  check "history: the view settles once every line is built" (H.settled sn2)
  eq "history: a built node leaves the view as it is (a merge happens at a message, in a batch)" (H.sView sn2) [(0, 0), (0, 1), (0, 2), (0, 3)]
  eq "history: ... and takes its work off the queues, and queues what it makes ready" (H.sReady sn2, H.sUnbuilt sn2) (Set.fromList [(1, 1)], Set.empty)
  -- the budget is the view as rendered (prefixes, newlines, tags): 4 lines of 68 bytes of text render as 103
  let tree2 = H.sSizes sn2
      four = [(0, 0), (0, 1), (0, 2), (0, 3)]
  eq "history: a view's size counts each line's id+n| and newline, and the tags" (H.viewSize tree2 four) 103
  eq "history: under its budget the view is left alone" (H.stepView hp { H.pView = 103 } 4 tree2 (False, four)) (False, four)
  eq "history: over it, one batch merges down to the lower mark: the most due pair with a built parent"
     (H.stepView hp { H.pView = 102, H.pViewMin = 90 } 4 tree2 (False, four)) (False, [(1, 0), (0, 2), (0, 3)])
  eq "history: a batch that cannot get there yet (a parent not built) says so"
     (H.stepView hp { H.pView = 102, H.pViewMin = 50 } 4 tree2 (False, four)) (True, [(1, 0), (0, 2), (0, 3)])
  eq "history: ... and goes on at the next message, under the budget or not"
     (fst (H.stepView hp { H.pView = 1000, H.pViewMin = 50 } 4 tree2 (True, four))) True
  zr <- H.zoom hm 0 2
  eq "history: zoom opens a line into its two" zr (Right (T.pack "0+1|tool: eval 1 + 1\n1+1|tool: eval 2 * 3\n"))
  z1 <- H.zoom hm 3 1
  eq "history: zoom 1 is the message whole" z1 (Right (T.pack "3+0|tool: reload"))
  H.zoom hm 1 2 >>= check "history: a line that is not in the tree's shape is refused" . either (const True) (const False)
  ids2 <- mapM (H.appendMsg hm (T.pack "echo")) (map T.pack ["ok", "ok", "ok", "ok"])
  eq "history: ids go on" ids2 [4 .. 7]
  sn3 <- H.snapshot hm
  eq "history: the batch goes on at each message, merging what has a parent" (H.sView sn3) [(1, 0), (0, 2), (0, 3), (1, 2), (1, 3)]
  check "history: a merged part is never split" (take 1 (H.sView sn3) == [(1, 0)])
  viewFiles <- mapM (doesFileExist . (hdir </>)) ["view.json", "view-compact.json"]
  eq "history: the views are saved beside the log" viewFiles [True, True]
  (hm2, torn1) <- H.openHistory hp hdir
  sn4 <- H.snapshot hm2
  eq "history: reopened: no torn lines" torn1 0
  texts3 <- map H.mText <$> H.messages hm 0 1000
  texts4 <- map H.mText <$> H.messages hm2 0 1000
  eq "history: reopened: the same messages (read from the log, each)" (H.sCount sn4, texts4) (H.sCount sn3, texts3)
  tree3 <- H.treeTexts hm
  tree4 <- H.treeTexts hm2
  eq "history: reopened: the same tree (its lines read from the files, or made of what they are)" tree4 tree3
  check "history: a node's line is there for every node that is built" (M.keysSet tree3 == M.keysSet (H.sSizes sn3) && not (M.null tree3))
  eq "history: a line's bytes are what was counted for it" (M.map H.byteLength tree3) (H.sSizes sn3)
  eq "history: reopened: the same queues" (H.sReady sn4, H.sUnbuilt sn4) (H.sReady sn3, H.sUnbuilt sn3)
  eq "history: reopened: the view as it was saved, not rebuilt" (H.sView sn4, H.sCView sn4) (H.sView sn3, H.sCView sn3)
  -- a saved view is taken as it is, even one the fold would not make: that is what "never rebuilt" means
  writeFile (hdir </> "view.json") "[[0,0],[0,1],[1,1],[2,1]]\n"
  (hm2b, _) <- H.openHistory hp hdir
  eq "history: a saved view is loaded as it is" . H.sView <$> H.snapshot hm2b >>= ($ [(0, 0), (0, 1), (1, 1), (2, 1)])
  writeFile (hdir </> "view.json") "[[0,0],[0,1],[1,1]]\n"
  (hm2c, _) <- H.openHistory hp hdir
  eq "history: ... messages logged after it was saved get their lines" . H.sView <$> H.snapshot hm2c >>= ($ [(0, 0), (0, 1), (1, 1), (0, 4), (0, 5), (0, 6), (0, 7)])
  writeFile (hdir </> "view.json") "[[0,0],[1,1]]\n"
  (hm2d, _) <- H.openHistory hp hdir
  sn4d <- H.snapshot hm2d
  eq "history: ... and one that does not tile the log is not taken: the view is folded again" (sum [ 2 ^ l | (l, _) <- H.sView sn4d ]) (8 :: Int)
  writeFile (hdir </> "view.json") "[[1,0],[0,2],[0,3],[1,2],[1,3]]\n"
  mainFiles <- listDirectory (hdir </> "main")
  forM_ (take 1 mainFiles) $ \f -> appendFile (hdir </> "main" </> f) "{\"i\": 8, \"kind\": \"tool\", \"te"
  (hm3, torn2) <- H.openHistory hp hdir
  n3 <- H.count hm3
  eq "history: a torn line is skipped and counted" (torn2, n3) (1, 8)
  i9 <- H.appendMsg hm3 (T.pack "note") (T.pack "after the tear")
  eq "history: the next line starts on its own line" i9 8
  d9 <- H.dateOf hm3 8
  check "history: a message has its date" (maybe False (> 0) d9)

  -- the compactions' view: the chat's view merged further, with its own two marks
  (hc, _) <- H.openHistory hp { H.pView = 100000, H.pViewMin = 50000, H.pCtxMax = 70, H.pCtxMin = 40 } (tmp </> "history-c")
  mapM_ (H.appendMsg hc (T.pack "echo")) (replicate 5 (T.pack "ok"))
  snc <- H.snapshot hc
  eq "history: the chat's view is not merged under its budget" (H.sView snc) [ (0, i) | i <- [0 .. 4] ]
  eq "history: the compactions' view is, past its own: the same order, further" (H.sCView snc) [(1, 0), (1, 1), (0, 4)]
  _ <- H.appendMsg hc (T.pack "user") (T.replicate 10 (T.pack "long "))
  snc2 <- H.snapshot hc
  let jobsC = H.pendingOf hp snc2 Set.empty M.empty 0
  eq "history: a compaction reads the compactions' view up to its node, as the view renders it"
     [ (H.jL j, H.jI j, H.jContext j) | j <- jobsC ]
     [ (0, 5, map T.pack ["0+2|echo: ok echo: ok", "2+2|echo: ok echo: ok", "4+1|echo: ok"]), (2, 0, map T.pack ["0+2|echo: ok echo: ok", "2+2|echo: ok echo: ok"]) ]

  -- the chat's harness (GhciSession.Chat): what it does to the model's calls and to the files
  let toolNamed n = head [ t | t <- chatTools, tName t == n ]
      args n kvs = arguments (toolNamed n) (JObj kvs)
  eq "chat: an argument called by another name is taken by its name" (args "sh" [("command", JStr "ls")]) (JObj [("cmd", JStr "ls")], [])
  eq "chat: an alias of an argument the tool does not take is left alone (zoom keeps its n)" (args "zoom" [("id", JNum 1), ("n", JNum 2)]) (JObj [("id", JNum 1), ("n", JNum 2)], [])
  eq "chat: a missing argument is named (an edit's old is not one: it has other forms)" (snd (args "edit" [("path", JStr "a")]), snd (args "edit" [("path", JStr "a"), ("new", JStr "b")])) (["new"], [])
  eq "chat: file is path" (args "read" [("file", JStr "src/A.hs")]) (JObj [("path", JStr "src/A.hs")], [])
  check "chat: remember is a tool, and no tool takes a session" ("remember" `elem` map tName chatTools && all (\t -> "session" `notElem` map fst (tProps t)) chatTools)
  check "chat: grep and find are chat tools" ("grep" `elem` map tName chatTools && "find" `elem` map tName chatTools)
  let doc5 = T.pack "one\nf :: Int\nf = 1\ng :: Int\ng = 2\n"
      ed kvs = either (Left . T.unpack) (Right . T.unpack . fst) (applyEdit doc5 (JObj kvs))
  eq "edit: put at the end (a line end added where there is none), and a file that was not there is the text"
     (ed [("append", JBool True), ("new", JStr "h = 3")], fmap fst (applyEdit T.empty (JObj [("append", JBool True), ("new", JStr "x")])), fmap fst (applyEdit (T.pack "a") (JObj [("append", JBool True), ("new", JStr "b\n")])))
     (Right "one\nf :: Int\nf = 1\ng :: Int\ng = 2\nh = 3\n", Right (T.pack "x\n"), Right (T.pack "a\nb\n"))
  eq "edit: from one line up to another -- a definition replaced whole, the line after it kept"
     (ed [("first", JStr "f ::"), ("until", JStr "g ::"), ("new", JStr "f :: Integer\nf = 10\n")]) (Right "one\nf :: Integer\nf = 10\ng :: Int\ng = 2\n")
  eq "edit: lines by their numbers; with nothing, they are gone"
     (ed [("start", JNum 2), ("end", JNum 3), ("new", JStr "x")], ed [("start", JNum 1), ("new", JStr "")]) (Right "one\nx\ng :: Int\ng = 2\n", Right "f :: Int\nf = 1\ng :: Int\ng = 2\n")
  eq "edit: what cannot be is said -- a first that two lines hold, an until that none after it holds, lines that are not there, nothing asked"
     ( map (either (take 22) (const "done")) [ed [("first", JStr "Int"), ("until", JStr "g"), ("new", JStr "")], ed [("first", JStr "g ::"), ("until", JStr "f ::"), ("new", JStr "")], ed [("start", JNum 9), ("new", JStr "")], ed [("new", JStr "z")], ed [("first", JStr "f ::"), ("new", JStr "")]] )
     ["first: 2 lines hold \"I", "until: no line after l", "lines 9-9: the file ha", "what to change is not ", "first, and no until: g"]
  eq "edit: the first form is as it was -- one exact, unique occurrence" (ed [("old", JStr "f = 1"), ("new", JStr "f = 7")]) (Right "one\nf :: Int\nf = 7\ng :: Int\ng = 2\n")
  check "chat: spawn and tell are chat tools, and a task list is a list of strings"
        (all (`elem` map tName chatTools) ["spawn", "tell"] && lookupStr "type" (toolJson (toolNamed "spawn") .: "function" .: "parameters" .: "properties" .: "tasks" .: "items") == Just "string")
  eq "chat: a subagent's word to the agent is told from the user's by how it begins"
     (map (isWork . T.pack) ["[Sub-3] report\nall done", "[Sub-12] reply\nyes", "[Sub-3] report and more\nx", "report\n[Sub-3]", "[Bob] report\nx", "please read [Sub-3] report"]) [True, True, False, False, False, False]
  eq "chat: a subagent's chat as its turn reads it -- numbered, by kind, a later line of a message set in; of a long one its head and its end"
     ( agentShow [("note", T.pack "subagent"), ("user", T.pack "do this\nand that"), ("tool", T.pack "ls {}")]
     , let ls = T.lines (agentShow [ ("echo", T.pack (show i)) | i <- [0 .. 99 :: Int] ]) in (length ls, take 1 ls, ls !! 4, last ls) )
     ( T.pack "0|note: subagent\n1|user: do this\n  and that\n2|tool: ls {}\n", (45, [T.pack "0|echo: 0"], T.pack "(56 lines left out)", T.pack "99|echo: 99") )
  check "chat: restart is a chat tool" ("restart" `elem` map tName chatTools)
  avail <- Search.isAvailable
  -- (libfff is optional -- build.sh fetches it when it can, and search falls back to a scan without it -- so it
  --  must load where it is there to load: the places ghs_fff.c looks first, from the directory this runs in)
  present <- or <$> mapM doesFileExist [ d ++ "/libfff." ++ e | d <- [".bin", "lib"], e <- ["so", "dylib"] ]
  if Search.builtWithFff
    then check "search: FFF C library is loaded where it is present" (avail || not present)
    else check "search: built without the fff flag, so never loaded" (not avail)
  grepRes <- Search.grep "." "cmdSearch" 10
  check "search: grep finds occurrences with line numbers" (case grepRes of { Right j -> fromMaybe 0 (lookupNum "count" j >>= Just . round) >= (1 :: Int); Left _ -> False })
  eq "C: the build tool's command, from its response-file line" (ccWords "GHC response file arguments: '-package-env=-' -c -fPIC -odir /b/build -I/b/build/autogen -optc-O2 '-optc-DA B' cbits/tui.c -Wall")
     ["-package-env=-", "-c", "-fPIC", "-odir", "/b/build", "-I/b/build/autogen", "-optc-O2", "-optc-DA B", "cbits/tui.c", "-Wall"]
  eq "C: the build tool's command, from a line that runs the compiler" (ccWords "Running: /usr/bin/ghc -c -odir /b cbits/x.c") ["-c", "-odir", "/b", "cbits/x.c"]
  eq "C: a .cabal's options for C, under a condition or not, a comment left out"
     (cabalField "cc-options" "library\n  c-sources: a.c\n  cc-options: -DA\n  if flag(x)\n    cc-options:     -DB -DC\n    -- cc-options: -DNO\n  ghc-options: -Wall\n") ["-DA", "-DB", "-DC"]
  eq "C: another line of the build tool's is no command" (ccWords "Preprocessing library for ghostty-tui-0.1.0.0...") []
  -- the conversation as Anthropic's Messages API takes it
  let um t = JObj [("role", JStr "user"), ("content", JStr t)]
      tm i t = JObj [("role", JStr "tool"), ("tool_call_id", JStr i), ("content", JStr t)]
      callJ i n a = JObj [("id", JStr i), ("type", JStr "function"), ("function", JObj [("name", JStr n), ("arguments", JStr a)])]
      am t cs = JObj ([("role", JStr "assistant"), ("content", JStr t)] ++ [ ("tool_calls", JArr cs) | not (null cs) ])
      kinds m = [ fromMaybe "?" (lookupStr "type" b) | b <- lookupArr "content" m ]
      shape = map (\m -> (fromMaybe "?" (lookupStr "role" m), kinds m)) . snd . A.toMessages
      has0 j = case j of { JNull -> False; _ -> True }
      marksOf bs = [ k | (k, b) <- zip [0 :: Int ..] bs, has0 (b .: "cache_control") ]
      viewOf n = unlines (["before", "<chat>"] ++ [ show i ++ "+1|line" | i <- [1 .. n :: Int] ] ++ ["</chat>", "", "the message"])
      texts = map (fromMaybe T.empty . lookupText "text")
      opus = A.Config "k" False "https://api.anthropic.com" "claude-opus-5-5"
      has k b = case b .: k of { JNull -> False; _ -> True }
      bodyOf c o = A.requestBody c o [JObj [("role", JStr "system"), ("content", JStr "sys")], um "hi"] []
  eq "anthropic: the system text is apart, and a step's results are one user message, results first"
     (shape [ JObj [("role", JStr "system"), ("content", JStr "sys")], um "do it", am "ok" [callJ "a" "eval" "{\"expr\":\"1\"}", callJ "b" "status" "{}"]
            , tm "a" "1", tm "b" "OK", um "and then?" ])
     [("user", ["text"]), ("assistant", ["text", "tool_use", "tool_use"]), ("user", ["tool_result", "tool_result", "text"])]
  eq "anthropic: the system text" (fst (A.toMessages [JObj [("role", JStr "system"), ("content", JStr "sys")], um "x"])) [T.pack "sys"]
  eq "anthropic: a call's arguments are an object, an error result is said to be one, an empty one says so"
     [ (b .: "input", lookupBool "is_error" b, lookupStr "content" b) | m <- snd (A.toMessages [um "x", am "" [callJ "a" "eval" "{\"expr\":\"1\"}"], tm "a" "ERROR: no", um "y", am "" [callJ "b" "s" "not json"], tm "b" ""]), b <- lookupArr "content" m, lookupStr "type" b /= Just "text" ]
     [ (JObj [("expr", JStr "1")], Nothing, Nothing), (JNull, Just True, Just "ERROR: no"), (JObj [], Nothing, Nothing), (JNull, Nothing, Just "(no output)") ]
  eq "anthropic: a reply's own blocks go back as they came, and a reply with nothing in it is no turn"
     (map (lookupArr "content") (snd (A.toMessages [um "x", JObj [("role", JStr "assistant"), ("content", JStr "ignored"), ("anthropic_content", JArr [JObj [("type", JStr "thinking"), ("signature", JStr "s")], JObj [("type", JStr "text"), ("text", JStr "t")]])], um "y", am "" [], um "z"])))
     [ [JObj [("type", JStr "text"), ("text", JStr "x")]], [JObj [("type", JStr "thinking"), ("signature", JStr "s")], JObj [("type", JStr "text"), ("text", JStr "t")]]
     , [JObj [("type", JStr "text"), ("text", JStr "y")], JObj [("type", JStr "text"), ("text", JStr "z")]] ]
  eq "anthropic: the view goes as blocks of four lines, the last whole one marked" (marksOf (A.viewBlocks (T.pack (viewOf 10)))) [2]
  eq "anthropic: the view's blocks put together are the text" (T.concat (texts (A.viewBlocks (T.pack (viewOf 10))))) (T.pack (viewOf 10))
  check "anthropic: a view with lines added keeps the blocks it had, up to the one that was marked"
        (let a = texts (A.viewBlocks (T.pack (viewOf 10))); b = texts (A.viewBlocks (T.pack (viewOf 13))) in take 3 a == take 3 b && length b > length a)
  eq "anthropic: a view shorter than a block has no mark, and a text with no view is one block" (marksOf (A.viewBlocks (T.pack (viewOf 3))), length (A.viewBlocks (T.pack "just words"))) ([], 1)
  eq "anthropic: the request -- thinking, an effort said, the cache asked for, another model if it declines; no temperature"
     (let b = bodyOf opus (A.Opts 100 Nothing Nothing 0 False) in (lookupStr "type" (b .: "thinking"), lookupStr "effort" (b .: "output_config"), has "cache_control" b, lookupStr "fallbacks" b, has "temperature" b, lookupNum "max_tokens" b, has "cache_control" (head (lookupArr "system" b))))
     (Just "adaptive", Just "high", True, Just "default", False, Just 4000, True)
  eq "anthropic: a compaction is the lightest effort (thinking cannot be turned off, and is not asked to be)"
     (let b = bodyOf opus (A.Opts 400 (Just False) Nothing 0 False) in (has "thinking" b, lookupStr "effort" (b .: "output_config")))
     (False, Just "low")
  eq "anthropic: a model that takes neither is sent neither, and another provider's endpoint only the conversation"
     ( let b = bodyOf opus { A.aModel = "claude-haiku-4-5" } (A.Opts 100 Nothing Nothing 0 False) in (has "thinking" b, has "output_config" b, has "fallbacks" b, lookupNum "max_tokens" b)
     , let b = bodyOf (A.Config "k" True "https://api.deepseek.com/anthropic" "deepseek-v4-pro") (A.Opts 100 Nothing Nothing 0 False) in (has "thinking" b, has "output_config" b, has "fallbacks" b, has "cache_control" b) )
     ((False, False, False, Just 100), (False, False, False, False))
  eq "anthropic: the headers -- a key, or a token as a bearer; the beta a fallback needs, where there is one"
     (A.headers opus, A.headers (A.Config "t" True "http://localhost:1" "m"), A.url (A.Config "t" True "http://localhost:1" "m"))
     ( ["Content-Type: application/json", "anthropic-version: 2023-06-01", "x-api-key: k", "anthropic-beta: server-side-fallback-2026-07-01"]
     , ["Content-Type: application/json", "anthropic-version: 2023-06-01", "Authorization: Bearer t"], "http://localhost:1/v1/messages" )
  let replyJ stop content usage = JObj ([("type", JStr "message"), ("content", JArr content), ("stop_reason", JStr stop), ("usage", JObj usage)])
      parsed = fmap (\p -> (A.rText p, A.rThinking p, A.rCalls p, A.rStop p, (A.rIn p, A.rOut p, A.rCached p), length (A.rBlocks p))) . A.parseReply
  eq "anthropic: a reply -- its text, its thinking, its calls; the prompt's tokens are the three counts together"
     (parsed (replyJ "tool_use" [ JObj [("type", JStr "thinking"), ("thinking", JStr "hm"), ("signature", JStr "s")], JObj [("type", JStr "text"), ("text", JStr "ok")]
                                , JObj [("type", JStr "tool_use"), ("id", JStr "t1"), ("name", JStr "eval"), ("input", JObj [("expr", JStr "1")])] ]
                     [("input_tokens", JNum 10), ("cache_read_input_tokens", JNum 900), ("cache_creation_input_tokens", JNum 90), ("output_tokens", JNum 7)]))
     (Right (T.pack "ok", T.pack "hm", [("t1", "eval", JObj [("expr", JStr "1")])], "tool_use", (1000, 7, 900), 3))
  eq "anthropic: a reply's empty text block is not kept to send back (its thinking and its call are)"
     (fmap (map (fromMaybe "?" . lookupStr "type") . A.rBlocks) (A.parseReply (replyJ "tool_use" [ JObj [("type", JStr "thinking"), ("thinking", JStr ""), ("signature", JStr "s")], JObj [("type", JStr "text"), ("text", JStr "")]
                                                                                        , JObj [("type", JStr "tool_use"), ("id", JStr "t"), ("name", JStr "n"), ("input", JObj [])] ] [])))
     (Right ["thinking", "tool_use"])
  eq "anthropic: a web search is asked for only where it was, and then as the API's own tool beside ours"
     ( [ lookupStr "type" t | t <- lookupArr "tools" (A.requestBody opus (A.Opts 100 Nothing Nothing 3 False) [um "hi"] [JObj [("function", JObj [("name", JStr "eval")])]]) ]
     , length (lookupArr "tools" (A.requestBody opus (A.Opts 100 Nothing Nothing 0 False) [um "hi"] [JObj [("function", JObj [("name", JStr "eval")])]])) )
     ([Nothing, Just "web_search_20260209"], 1)
  eq "anthropic: what the API's own tool did is said as a tool's doing, and its blocks are kept to send back"
     (fmap (\p -> (A.rServer p, length (A.rBlocks p), A.rCalls p, A.rStop p))
        (A.parseReply (replyJ "pause_turn" [ JObj [("type", JStr "server_tool_use"), ("id", JStr "s1"), ("name", JStr "web_search"), ("input", JObj [("query", JStr "ghc 9.14")])]
                                         , JObj [("type", JStr "web_search_tool_result"), ("tool_use_id", JStr "s1"), ("content", JArr [JObj [("type", JStr "web_search_result"), ("title", JStr "GHC"), ("url", JStr "https://x")]])] ] [])))
     (Right ([("tool", T.pack "web_search {\"query\": \"ghc 9.14\"}"), ("echo", T.pack "GHC https://x")], 2, [], "pause_turn"))
  let evJ ty kvs = JObj (("type", JStr ty) : kvs)
      deltaJ i ty k v = evJ "content_block_delta" [("index", JNum i), ("delta", JObj [("type", JStr ty), (k, JStr v)])]
      startJ i b = evJ "content_block_start" [("index", JNum i), ("content_block", JObj b)]
      events =
        [ evJ "message_start" [("message", JObj [("usage", JObj [("input_tokens", JNum 10), ("cache_read_input_tokens", JNum 900), ("output_tokens", JNum 1)])])]
        , evJ "ping" []
        , startJ 0 [("type", JStr "thinking"), ("thinking", JStr ""), ("signature", JStr "")], deltaJ 0 "thinking_delta" "thinking" "h", deltaJ 0 "thinking_delta" "thinking" "m", deltaJ 0 "signature_delta" "signature" "s"
        , startJ 1 [("type", JStr "text"), ("text", JStr "")], deltaJ 1 "text_delta" "text" "o", deltaJ 1 "text_delta" "text" "k"
        , startJ 2 [("type", JStr "tool_use"), ("id", JStr "t1"), ("name", JStr "eval"), ("input", JObj [])], deltaJ 2 "input_json_delta" "partial_json" "{\"expr\":", deltaJ 2 "input_json_delta" "partial_json" "\"1\"}"
        , evJ "content_block_stop" [("index", JNum 2)]
        , evJ "message_delta" [("delta", JObj [("stop_reason", JStr "tool_use")]), ("usage", JObj [("output_tokens", JNum 7)])]
        , evJ "message_stop" [] ]
      run evs = foldl (\(st, seen) ev -> let (st', l) = A.streamEvent ev st in (st', seen ++ maybe [] pure l)) (A.emptyStream, []) evs
      (whole, lives) = run events
  eq "anthropic: a reply that came as events is the reply that would have come whole"
     (parsed (A.streamMessage whole)) (Right (T.pack "ok", T.pack "hm", [("t1", "eval", JObj [("expr", JStr "1")])], "tool_use", (910, 7, 900), 3))
  eq "anthropic: what the events add is told as it comes: thinking, words, a call's name and its arguments"
     (lives, A.streamEnded whole) ([("mind", T.pack "h"), ("mind", T.pack "m"), ("talk", T.pack "o"), ("talk", T.pack "k"), ("tool", T.pack "eval "), ("tool", T.pack "{\"expr\":"), ("tool", T.pack "\"1\"}")], True)
  eq "anthropic: its thinking block is the one that was sent, to send back (its text and its signature)"
     (take 1 (lookupArr "content" (A.streamMessage whole))) [JObj [("type", JStr "thinking"), ("thinking", JStr "hm"), ("signature", JStr "s")]]
  eq "anthropic: a stream that stops before its end has not ended, and a call cut off inside its arguments is no call"
     (let (cut, _) = run (take 10 events ++ [evJ "message_delta" [("delta", JObj [("stop_reason", JStr "max_tokens")])], evJ "message_stop" []]) in (A.streamEnded (fst (run (take 10 events))), fmap (\p -> (A.rCalls p, A.rStop p)) (A.parseReply (A.streamMessage cut))))
     (False, Right ([], "max_tokens"))
  eq "anthropic: an error in the stream is the stream's end, and is kept"
     (let (st, _) = run (take 3 events ++ [evJ "error" [("error", JObj [("type", JStr "overloaded_error"), ("message", JStr "Overloaded")])]]) in (A.streamEnded st, fmap (\e -> lookupStr "type" (e .: "error")) (A.streamError st)))
     (True, Just (Just "overloaded_error"))
  -- markdown and diffs, as a screen shows them
  let txtOf = concatMap snd
      marks l = [ (t, (sBold st, sItalic st, sUnderline st, sStrike st, sFg st)) | (st, t) <- l, st /= plain ]
  eq "md: a line's marks are styles, and the marks are gone"
     (let l = Md.inline plain "a **b** and *c* and `d` ~~e~~" in (txtOf l, marks l))
     ("a b and c and d e", [("b", (True, False, False, False, Default)), ("c", (False, True, False, False, Default)), ("d", (False, False, False, False, Ansi 3)), ("e", (False, False, False, True, Default))])
  eq "md: a name with underscores, a product of two numbers and an escaped mark are left as they are"
     (map (txtOf . Md.inline plain) ["snake_case_name and __init__", "2 * 3 * 4", "\\*not italic\\*"], [ marks (Md.inline plain x) | x <- ["snake_case_name and __init__", "2 * 3 * 4"] ])
     (["snake_case_name and __init__", "2 * 3 * 4", "*not italic*"], [[], []])
  eq "md: a link is its text underlined and its address; a bare address is underlined, without the sentence's full stop"
     (let a = Md.inline plain "see [the docs](https://x.org/d)."; b = Md.inline plain "at https://example.org/page." in (txtOf a, [ t | (st, t) <- a, sUnderline st ], [ t | (st, t) <- b, sUnderline st ]))
     ("see the docs (https://x.org/d).", ["the docs"], ["https://example.org/page"])
  eq "md: a heading is bold without its hashes, a list has bullets, a quote a bar, a rule is a line"
     (map txtOf (Md.mdLines 20 "# Title\n- one\n  * two\n3. three\n> said\n---"), [ sBold st | (st, _) <- take 1 (head (Md.mdLines 20 "## T")) ])
     (["Title", "\8226 one", "  \8226 two", "3. three", "\9612 said", replicate 20 '\9472'], [True])
  eq "md: fenced code is shown as written, nothing in it marked; a fenced diff is a diff"
     ( map txtOf (Md.mdLines 40 "```haskell\nf **x** = 1\n```\nafter"), [ sFg st | l <- Md.mdLines 40 "```diff\n--- a/f\n+++ b/f\n+new\n-old\n```", (st, _) <- take 1 l ] )
     (["\9474 f **x** = 1", "after"], [Default, Default, Ansi 2, Ansi 1])
  eq "md: a table is laid out when it fits, its head bold; one too wide is shown as it was written"
     ( map txtOf (Md.mdLines 40 "| a | bb |\n|---|---|\n| ccc | d |"), take 1 (map txtOf (Md.mdLines 8 "| a | bb |\n|---|---|\n| ccc | d |")) )
     (["\9474 a   \9474 bb \9474", "\9500\9472\9472\9472\9472\9472\9532\9472\9472\9472\9472\9508", "\9474 ccc \9474 d  \9474"], ["| a | bb |"])
  eq "md: a tool's answer -- its words, then the change: added green, removed red, the hunk's head apart; and what follows is not the diff's"
     [ (txtOf l, [ sFg st | (st, _) <- take 1 l ]) | l <- Md.outputLines "edited f (2 lines)\n--- a/f\n+++ b/f\n@@ -1,2 +1,2 @@\n one\n-two\n+2\n\n[exit 0 in 0.1 s]\n- not a diff line" ]
     [ ("edited f (2 lines)", [Default]), ("--- a/f", [Default]), ("+++ b/f", [Default]), ("@@ -1,2 +1,2 @@", [Ansi 6]), (" one", [Default]), ("-two", [Ansi 1]), ("+2", [Ansi 2])
     , ("", [Default]), ("[exit 0 in 0.1 s]", [Default]), ("- not a diff line", [Default]) ]
  -- images: kept by name, a line standing for each, sent where the line is
  let png w h = B.pack ([0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0, 0, 0, 13] ++ map (fromIntegral . fromEnum) "IHDR" ++ map fromIntegral [0, 0, w `div` 256, w `mod` 256, 0, 0, h `div` 256, h `mod` 256 :: Int] ++ [8, 6, 0, 0, 0])
      gif = BC.pack "GIF89a" <> B.pack [0x20, 0x03, 0x58, 0x02, 0]
  eq "image: base64, with its padding" (map (BC.unpack . Img.base64 . BC.pack) ["", "f", "fo", "foo", "foob", "fooba", "foobar"]) ["", "Zg==", "Zm8=", "Zm9v", "Zm9vYg==", "Zm9vYmE=", "Zm9vYmFy"]
  eq "image: what bytes are, by how they begin, and their sides" (map Img.kindOf [png 800 600, gif, BC.pack "module Main where"], map Img.sizeOf [png 800 600, gif]) ([Just "image/png", Just "image/gif", Nothing], [Just (800, 600), Just (800, 600)])
  imgDir <- (</> ("ghci-session-selftest-img-" ++ show pid)) <$> getTemporaryDirectory
  kept <- Img.keep imgDir (png 4 2)
  again <- Img.keep imgDir (png 4 2)
  notOne <- Img.keep imgDir (BC.pack "words")
  let nm = either (const "") fst kept
      line = Img.marker nm
  eq "image: kept under a name of its bytes (the same bytes, the same name), said in words; what is no image is not kept"
     (fmap snd kept, kept == again, length nm, either (const True) (const False) notOne) (Right "image/png, 4x2, 1 KB", True, 20, True)
  eq "image: a line of its own stands for it -- not words that hold one, not a name that is none; each once"
     (Img.namesIn (T.unlines [T.pack "see", line, T.pack "and " <> line, T.pack "12+1|" <> line, T.pack "  " <> line, T.pack "[image ../../etc/passwd]", T.pack "[image photo.png]"])) [nm]
  sentTo <- Img.attach imgDir [um "x", JObj [("role", JStr "tool"), ("tool_call_id", JStr "a"), ("content", JText (T.pack "[f.png: image/png, 4x2, 1 KB]\n" <> line))], JObj [("role", JStr "user"), ("content", JText (T.pack "look\n" <> line))], um "[image 0000000000000000.png]"]
  eq "image: a message that names one is sent with it after its text -- a tool's answer in the answer, a typed line beside it; a name nothing is kept under is words"
     [ [ (lookupStr "type" b, [ lookupStr "type" c | c <- lookupArr "content" b ]) | b <- lookupArr "content" m ] | m <- snd (A.toMessages ([um "q", am "" [callJ "a" "read" "{}"]] ++ drop 1 sentTo)) ]
     [ [(Just "text", [])], [(Just "tool_use", [])], [(Just "tool_result", [Just "text", Just "image"]), (Just "text", []), (Just "image", []), (Just "text", [])] ]
  eq "image: as it is sent -- its type, and its bytes in base64"
     [ (lookupStr "media_type" (b .: "source"), lookupStr "data" (b .: "source") == Just (BC.unpack (Img.base64 (png 4 2)))) | m <- sentTo, b <- lookupArr "images" m ] [(Just "image/png", True), (Just "image/png", True)]
  eq "image: a turn's first message keeps its view in blocks with an image after it"
     (map (lookupStr "type") (concatMap (lookupArr "content") (snd (A.toMessages [set "images" (JArr [JObj [("type", JStr "image")]]) (um (viewOf 10 ++ "do it"))]))))
     (map (lookupStr "type") (A.viewBlocks (T.pack (viewOf 10 ++ "do it"))) ++ [Just "image"])
  B.writeFile (imgDir </> "my shot.png") (png 4 2)
  typedL <- mapM (Img.typed imgDir imgDir . T.pack) ["what is in my\\ shot.png here?", "and '" ++ (imgDir </> "my shot.png") ++ "'", "no such.png", "a .png in words"]
  eq "image: a line typed names one by its file -- a space escaped, or in quotes, in the project or by its whole path -- and gets its line; a file that is not there is words"
     (map (drop 1 . T.lines) typedL) [[line], [line], [], []]
  eq "md: the line that stands for an image is shown as one, in a reply and in a tool's answer"
     (map txtOf (Md.mdLines 40 (T.unpack (T.pack "seen:\n" <> line))), map txtOf (Md.outputLines (T.unpack line))) (["seen:", "\9635 image " ++ nm], ["\9635 image " ++ nm])
  -- a picture among the cells
  eq "picture: the cells for an image -- its own size where it fits (never larger), else the largest of its shape that does"
     [ cellsFor (8, 16) (64, 48) (80, 24), cellsFor (8, 16) (2400, 1200) (80, 24), cellsFor (8, 16) (400, 4000) (80, 24), cellsFor (0, 0) (10, 10) (5, 5), cellsFor (8, 16) (9000, 16) (500, 24) ]
     [ (8, 3), (80, 20), (5, 24), (5, 5), (maxCells, 1) ]
  eq "picture: a row is a placeholder a column, each with the marks of its row and its column -- and a cell of the frame each, the marks kept"
     ( map fromEnum (placeholderRow 1 2), [ (T.length (cText (cellAt f x 0)), cStyle (cellAt f x 0) == placeholderStyle 9) | let f = frame (6, 1) (textLine 0 0 6 [(plain, "ab"), (placeholderStyle 9, placeholderRow 0 3)]), x <- [0 .. 5] ] )
     ( [0x10EEEE, 0x030D, 0x0305, 0x10EEEE, 0x030D, 0x030D], [(1, False), (1, False), (3, True), (3, True), (3, True), (1, False)] )
  eq "picture: what turns one frame into the next writes a picture's cell as one column (the cell after it is where it thinks)"
     (diff (Just (frame (4, 1) [])) (frame (4, 1) [PutText 0 0 (placeholderStyle 9) (placeholderRow 0 2), PutText 2 0 (placeholderStyle 9) "x"]))
     ("\ESC[1;1H" ++ sgr (placeholderStyle 9) ++ placeholderRow 0 2 ++ "x")
  eq "picture: a mark that combines stays with its letter in a cell (it was dropped)" (T.unpack (cText (cellAt (frame (3, 1) [PutText 0 0 plain "e\769x"]) 0 0)), T.unpack (cText (cellAt (frame (3, 1) [PutText 0 0 plain "e\769x"]) 1 0))) ("e\769", "x")
  eq "picture: sent once in pieces, numbered, as a placement of cells, and no answer asked; forgotten by its number"
     ( let t = transmit 7 (10, 4) (BC.replicate 5000 'A') in (take 44 t, length (filter (== '\ESC') t) `div` 2, "\ESC_Gm=0;" `isInfixOf` t), forget 7 )
     ( ("\ESC_Ga=T,U=1,q=2,f=100,i=7,c=10,r=4,m=1;AAAAAA", 2, True), "\ESC_Ga=d,d=I,q=2,i=7\ESC\\" )
  eq "picture: in the chat's screen an image's line has its rows under it where there is one and it fits; else the line alone"
     ( [ map (take 1 . snd) l | l <- ChatTui.entryLinesWith (\n -> if n == nm then Just (9, 4, 2) else Nothing) 40 (ChatTui.Entry "echo" (T.pack "[f: image/png]\n" <> line)) ]
     , length (ChatTui.entryLinesWith (const (Just (9, 40, 2))) 40 (ChatTui.Entry "echo" line)), length (ChatTui.entryLines 40 (ChatTui.Entry "note" line)) )
     ( [["e", "["], [" ", "\9635", take 1 nm], [" ", "\1109742"], [" ", "\1109742"]], 1, 1 )
  shown <- Img.viewPng imgDir nm
  eq "picture: what a screen is given of a small PNG is itself" (fmap snd shown, fmap fst shown == Just (Img.base64 (png 4 2))) (Just (4, 2), True)
  removeDirectoryRecursive imgDir
  -- a completion that comes in pieces
  let dl d = JObj [("choices", JArr [JObj [("index", JNum 0), ("delta", JObj d)]])]
      tcd i extra f = dl [("tool_calls", JArr [JObj ([("index", JNum i)] ++ extra ++ [("function", JObj f)])])]
      evs = [ dl [("role", JStr "assistant"), ("content", JStr "")], dl [("reasoning_content", JStr "hm")], dl [("content", JStr "Hel")], dl [("content", JStr "lo")]
            , tcd 0 [("id", JStr "c1")] [("name", JStr "eval"), ("arguments", JStr "")], tcd 0 [] [("arguments", JStr "{\"expr\":")], tcd 0 [] [("arguments", JStr "\"1\"}")]
            , tcd 1 [("id", JStr "c2")] [("name", JStr "status"), ("arguments", JStr "{}")]
            , JObj [("choices", JArr [JObj [("index", JNum 0), ("delta", JObj []), ("finish_reason", JStr "tool_calls")]])], JObj [("choices", JArr []), ("usage", JObj [("prompt_tokens", JNum 7)])] ]
      (ckEnd, ckLive) = foldl (\(st, ls) ev -> let (st', l) = chunkEvent ev st in (st', ls ++ l)) (emptyChunks, []) evs
      whole = chunksMessage ckEnd
      wm = case lookupArr "choices" whole of { (c : _) -> c .: "message"; [] -> JNull }
  eq "llm: a completion in pieces is the completion whole -- its text, its reasoning, each call with its arguments put together, how it ended, what it used"
     ( lookupStr "content" wm, lookupStr "reasoning_content" wm, [ (lookupStr "id" c, lookupStr "name" (c .: "function"), lookupStr "arguments" (c .: "function")) | c <- lookupArr "tool_calls" wm ]
     , [ lookupStr "finish_reason" c | c <- lookupArr "choices" whole ], lookupNum "prompt_tokens" (whole .: "usage") )
     ( Just "Hello", Just "hm", [(Just "c1", Just "eval", Just "{\"expr\":\"1\"}"), (Just "c2", Just "status", Just "{}")], [Just "tool_calls"], Just 7 )
  eq "llm: and what each piece adds is told as it comes" (map fst ckLive, T.concat [ t | ("talk", t) <- ckLive ]) (["mind", "talk", "talk", "tool", "tool", "tool"], T.pack "Hello")
  eq "llm: an error in the middle of the pieces is kept" (fmap (lookupStr "message") (ckError (fst (chunkEvent (JObj [("error", JObj [("message", JStr "overloaded")])]) emptyChunks)))) (Just (Just "overloaded"))
  eq "md: a save's change -- hunks with no file's head -- is a diff too"
     [ [ sFg st | (st, _) <- take 1 l ] | l <- Md.outputLines "save: src/A.hs\n@@ -1,2 +1,2 @@\n one\n-two\n+2" ] [[Default], [Ansi 6], [Default], [Ansi 1], [Ansi 2]]
  -- chats had elsewhere, read into messages
  let s0 = I.Session "Claude Code" "" "" "/f.jsonl" []
      cl ty content extra = JObj ([("type", JStr ty), ("cwd", JStr "/proj"), ("entrypoint", JStr "cli"), ("message", JObj [("role", JStr ty), ("content", content)])] ++ extra)
      txt t = JObj [("type", JStr "text"), ("text", JStr t)]
      kinds (_, es) = [ (I.eKind e, T.unpack (I.eText e)) | e <- es ]
  eq "import: a user's words and the other agent's, its calls and their results; where the session is, how it was started"
     ( kinds (I.claudeLine s0 (cl "user" (JStr "fix it") []) 5)
     , kinds (I.claudeLine s0 (cl "assistant" (JArr [txt "looking", JObj [("type", JStr "tool_use"), ("name", JStr "Read"), ("input", JObj [("path", JStr "a.hs")])]]) []) 5)
     , kinds (I.claudeLine s0 (cl "user" (JArr [JObj [("type", JStr "tool_result"), ("content", JArr [txt "the file", JObj [("type", JStr "image")]])]]) []) 5)
     , (\(s, _) -> (I.sWhere s, I.sEntry s)) (I.claudeLine s0 (cl "user" (JStr "x") []) 5) )
     ([("user", "fix it")], [("ai", "looking"), ("tool", "Read {\"path\": \"a.hs\"}")], [("echo", "the file\n[image]")], ("/proj", "cli"))
  eq "import: what a program put in the chat is not taken: its own lines, a subagent's, a command's echo, a reminder"
     [ kinds (I.claudeLine s0 l 5) | l <- [ cl "user" (JStr "meta") [("isMeta", JBool True)], cl "assistant" (JStr "side") [("isSidechain", JBool True)]
                                          , cl "user" (JStr "<command-name>/model</command-name>") [], cl "user" (JArr [txt "<system-reminder>x</system-reminder>", txt "the real words"]) []
                                          , cl "attachment" (JStr "x") [] ] ]
     [[], [], [], [("user", "the real words")], []]
  let cx ty payload = JObj [("type", JStr ty), ("payload", JObj payload)]
      (sx, _) = I.codexLine (I.Session "Codex" "" "" "/c.jsonl" []) (cx "session_meta" [("cwd", JStr "/proj"), ("originator", JStr "codex_cli")]) 1
  eq "import: a Codex session -- where it is, a message, a call and its result; what it was given as its setting is not a message"
     ( I.sWhere sx
     , kinds (I.codexLine sx (cx "response_item" [("type", JStr "message"), ("role", JStr "user"), ("content", JArr [txt "<environment_context>cwd</environment_context>", txt "do it"])]) 2)
     , kinds (I.codexLine sx (cx "response_item" [("type", JStr "function_call"), ("name", JStr "shell"), ("arguments", JStr "{\"cmd\":\"ls\"}")]) 2)
     , kinds (I.codexLine sx (cx "response_item" [("type", JStr "function_call_output"), ("output", JStr "a b")]) 2)
     , kinds (I.codexLine (fst (I.codexLine sx (cx "session_meta" [("cwd", JStr "/p"), ("source", JObj [("subagent", JStr "x")])]) 1)) (cx "response_item" [("type", JStr "message"), ("role", JStr "user"), ("content", JArr [txt "sub"])]) 2) )
     ("/proj", [("user", "do it")], [("tool", "shell {\"cmd\":\"ls\"}")], [("echo", "a b")], [])
  eq "import: a date as the files have it" (I.isoSeconds "2026-10-08T09:08:23.523Z", I.isoSeconds "yesterday") (Just 1791450503.523, Nothing)
  let sess entry es = I.Session "Claude Code" entry "/proj" "/f.jsonl" [ I.Entry k (T.pack "t") d | (k, d) <- es ]
      talkS = sess "cli" [("user", 10), ("ai", 11), ("tool", 12), ("echo", 13), ("user", 20), ("ai", 21)]
      taken w done s = fmap (map (\e -> (I.eKind e, I.eDate e)) . I.sEntries) (I.wanted w done s)
      wantPlain = I.Want False False 0 ""
  eq "import: a person's session is taken, its words; its tool calls when asked for"
     (taken wantPlain M.empty talkS, fmap length (taken wantPlain { I.wTools = True } M.empty talkS)) (Just [("user", 10), ("ai", 11), ("user", 20), ("ai", 21)], Just 6)
  eq "import: a program's calls through the SDK, and a single exchange, are passed over unless asked for"
     ( taken wantPlain M.empty (sess "sdk-ts" [("user", 1), ("ai", 2), ("user", 3), ("ai", 4)]), taken wantPlain M.empty (sess "cli" [("user", 1), ("ai", 2)])
     , fmap length (taken wantPlain { I.wAll = True } M.empty (sess "sdk-ts" [("user", 1), ("ai", 2)])) )
     (Nothing, Nothing, Just 2)
  eq "import: only what is newer than what was taken of it before, and than the date asked for; another project's is not this one's"
     ( taken wantPlain (M.fromList [("/f.jsonl", 11)]) talkS, taken wantPlain (M.fromList [("/f.jsonl", 21)]) talkS, taken wantPlain { I.wSince = 15 } M.empty talkS, taken wantPlain { I.wRoot = "/other" } M.empty talkS )
     (Just [("user", 20), ("ai", 21)], Nothing, Just [("user", 20), ("ai", 21)], Nothing)
  eq "import: where Claude Code keeps a project's sessions" (I.claudeDir "/Users/me" "/Users/me/code/ghci-session") "/Users/me/.claude/projects/-Users-me-code-ghci-session"
  eq "import: a file as it was at the last import is not read again; a changed, a new, and an unreadable one are"
     (IS.stampsIn (M.fromList [("/a", (10, 5)), ("/b", (10, 5)), ("/c", (10, 5))]) [("/a", Just (10, 5)), ("/b", Just (11, 5)), ("/d", Just (1, 1)), ("/c", Nothing)])
     (["/a"], [("/b", (11, 5)), ("/d", (1, 1))])
  do tmp <- getTemporaryDirectory
     let kf = tmp </> "ghs-selftest-imported-files.json"
         sf = tmp </> "ghs-selftest-session.jsonl"
     writeFile sf "{}\n"
     s1 <- IS.fileStamp sf
     IS.saveStamps kf (M.fromList [(sf, maybe (0, 0) id s1)])
     kept <- IS.loadStamps kf
     s2 <- IS.fileStamp sf
     appendFile sf "{}\n"
     s3 <- IS.fileStamp sf
     gone <- IS.fileStamp (tmp </> "ghs-selftest-nothing-here")
     none <- IS.loadStamps (tmp </> "ghs-selftest-nothing-here")
     eq "import: the stamps are kept as they were (size, mtime to the microsecond), a file that grew has another, a missing one none"
        (M.lookup sf kept == s1, s1 == s2, s1 == s3, gone, M.null none) (True, True, False, Nothing, True)
     mapM_ removeFile [kf, sf]
  -- the claude command (a subscription)
  let cli = C.cliArgs (C.CliOpts "claude-haiku-4-5" (Just "low") (T.pack "sys") (Just ("/bin/ghs", "/tmp/s.sock")) False False)
      after k xs = take 1 [ v | (a, v) <- zip xs (drop 1 xs), a == k ]
  eq "claude: it is run without tools of its own, without the user's settings, with ours allowed by name"
     (after "--tools" cli, after "--setting-sources" cli, after "--allowedTools" cli, after "--model" cli, after "--effort" cli, "--strict-mcp-config" `elem` cli, "bypassPermissions" `elem` cli)
     ([""], [""], ["mcp__ghs"], ["claude-haiku-4-5"], ["low"], True, False)
  eq "claude: its tool server is this executable, relaying to the chat's socket"
     (fmap (\j -> (lookupStr "command" (j .: "mcpServers" .: "ghs"), lookupArr "args" (j .: "mcpServers" .: "ghs"))) (parseJson (concat (after "--mcp-config" cli))))
     (Right (Just "/bin/ghs", [JStr "mcp-relay", JStr "/tmp/s.sock"]))
  eq "claude: with no tools of ours there is no tool server, and the web is its own two tools when asked for"
     (let a = C.cliArgs (C.CliOpts "m" Nothing (T.pack "s") Nothing True False) in ("--mcp-config" `elem` a, after "--tools" a, "--effort" `elem` a)) (False, ["WebSearch,WebFetch"], False)
  eq "claude: the environment is cleared of what would send it elsewhere, and where it keeps its sign-in is left"
     (let e = C.cliEnv [("ANTHROPIC_BASE_URL", "https://other"), ("ANTHROPIC_AUTH_TOKEN", "t"), ("CLAUDECODE", "1"), ("CLAUDE_CODE_ENTRYPOINT", "cli"), ("CLAUDE_CONFIG_DIR", "/c"), ("PATH", "/bin"), ("DISABLE_TELEMETRY", "0")]
      in ([ k | (k, _) <- e, k `elem` ["ANTHROPIC_BASE_URL", "ANTHROPIC_AUTH_TOKEN", "CLAUDECODE", "CLAUDE_CODE_ENTRYPOINT"] ], lookup "CLAUDE_CONFIG_DIR" e, lookup "PATH" e, lookup "DISABLE_TELEMETRY" e, lookup "CLAUDE_CODE_DISABLE_CLAUDE_MDS" e))
     ([], Just "/c", Just "/bin", Just "1", Just "1")
  eq "claude: a call that wants no thinking has it off where the model lets it be, and nowhere else"
     ( C.noThinking (C.CliOpts "claude-haiku-4-5" Nothing T.empty Nothing False True), C.noThinking (C.CliOpts "claude-opus-5-5" Nothing T.empty Nothing False True)
     , C.noThinking (C.CliOpts "claude-haiku-4-5" Nothing T.empty Nothing False False) )
     ([("MAX_THINKING_TOKENS", "0")], [], [])
  eq "claude: a block marked for the cache is marked for an hour (it takes no shorter mark before its own)"
     (fmap (\j -> [ b .: "cache_control" | b <- lookupArr "content" (j .: "message") ]) (parseJsonBS (C.userLine [JObj [("type", JStr "text"), ("text", JStr "a"), ("cache_control", JObj [("type", JStr "ephemeral")])], JObj [("type", JStr "text"), ("text", JStr "b")]])))
     (Right [JObj [("type", JStr "ephemeral"), ("ttl", JStr "1h")], JNull])
  eq "claude: its lines -- one of the API's events, a reply's finished blocks, its end; anything else is passed over"
     ( C.readEvent (BC.pack "{\"type\":\"stream_event\",\"event\":{\"type\":\"message_stop\"}}")
     , C.readEvent (BC.pack "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"hi\"}]}}")
     , C.readEvent (BC.pack "{\"type\":\"system\",\"subtype\":\"status\"}"), C.readEvent (BC.pack "not json") )
     (C.EvStream (JObj [("type", JStr "message_stop")]), C.EvAssistant [JObj [("type", JStr "text"), ("text", JStr "hi")]], C.EvOther, C.EvOther)
  eq "claude: how a run ended -- its words, the prompts' tokens in all, the calls it made; a busy service is asked again"
     ( C.resultOf (JObj [("type", JStr "result"), ("is_error", JBool False), ("result", JStr "pong"), ("num_turns", JNum 2), ("total_cost_usd", JNum 0.5), ("usage", JObj [("input_tokens", JNum 10), ("cache_read_input_tokens", JNum 900), ("cache_creation_input_tokens", JNum 90), ("output_tokens", JNum 7)])])
     , (\x -> (C.xOk x, C.xBusy x)) (C.resultOf (JObj [("is_error", JBool True), ("api_error_status", JNum 529), ("subtype", JStr "error_during_execution")])) )
     (C.Result True (T.pack "pong") False 1000 7 900 2 0.5, (False, True))
  do room <- C.limitNote (JObj [("status", JStr "allowed"), ("rateLimitType", JStr "five_hour"), ("unifiedWindows", JObj [("five_hour", JObj [("utilization", JNum 0.49)])])])
     full <- C.limitNote (JObj [("status", JStr "rejected"), ("rateLimitType", JStr "five_hour"), ("resetsAt", JNum 1791532800)])
     near <- C.limitNote (JObj [("status", JStr "allowed"), ("rateLimitType", JStr "seven_day"), ("unifiedWindows", JObj [("seven_day", JObj [("utilization", JNum 0.93)])])])
     eq "claude: the subscription's limits are said when they are reached or nearly, and not while there is room"
        (room, fmap (take 49) full, fmap (take 39) near) (Nothing, Just "the subscription's five hour limit is reached: it", Just "the subscription's limits are 93% used ")
  eq "anthropic: a request declined is said, and an error is the API's words"
     ( fmap A.rText (A.parseReply (JObj [("type", JStr "message"), ("content", JArr []), ("stop_reason", JStr "refusal"), ("stop_details", JObj [("category", JStr "cyber")])]))
     , either id (const "") (A.parseReply (JObj [("type", JStr "error"), ("error", JObj [("type", JStr "overloaded_error"), ("message", JStr "Overloaded")])])) )
     (Right (T.pack "[the model declined this request (cyber)]"), "overloaded_error: Overloaded")
  eq "chat: writes that follow one another in a reply are a batch" (writeRuns ["read", "write", "edit", "edits", "eval", "write", "write", "write"]) [[1, 2, 3], [5, 6, 7]]
  eq "chat: a lone write is not a batch" (writeRuns ["write", "read", "edit", "sh", "write"]) []
  eq "chat: writes to distinct files are separate groups, those sharing one stay together in order"
     (sort (groupByPaths [(0, ["a.hs"]), (1, ["b.hs"]), (2, ["a.hs", "c.hs"]), (3, ["c.hs"]), (4, ["d.hs"])])) [[0, 2, 3], [1], [4]]
  eq "chat: leading imports are their own commands" (splitImports ["import A", ":set -XB", "f 1"]) (["import A", ":set -XB"], ["f 1"])
  eq "chat: a lone import stays what it is" (splitImports ["import A"]) ([], ["import A"])
  eq "chat: an expression alone is untouched" (splitImports ["let a = 1 in a", "+ 2"]) ([], ["let a = 1 in a", "+ 2"])
  eq "chat: semicolon imports are split" (splitImports ["import A; import B"]) (["import A", "import B"], [])
  eq "chat: semicolon import followed by expr is split" (splitImports ["import A; f 1"]) (["import A"], ["f 1"])
  let file = T.pack "x = 1\n  foo   bar\ny = 2\n"
  eq "chat: an edit that differs only in spacing is applied where its words are" (fuzzyReplace file (T.pack "foo bar") (T.pack "baz")) (Just (Right (T.pack "x = 1\n  baz\ny = 2\n", 2, 2, 0)))
  eq "chat: ... the indent and newline the old text had come off the new text" (fuzzyReplace file (T.pack "  foo bar\n") (T.pack "  qux\n")) (Just (Right (T.pack "x = 1\n  qux\ny = 2\n", 2, 2, 0)))
  check "chat: ... but not across lines it has not (the lines are not those lines): the lines are answered" (case fuzzyReplace file (T.pack "foo bar y = 2") (T.pack "z") of { Just (Left w) -> T.isInfixOf (T.pack "not applied") w && T.isInfixOf (T.pack "foo   bar") w && T.isInfixOf (T.pack "y = 2") w; _ -> False })
  let ind2 = T.pack "a\n  f x\n    g y\nb\n"
      ind4 = T.pack "a\n    f x\n      g y\nb\n"
  eq "chat: an old text indented two too far, its new text the same: the new text's lines are shifted back" (fuzzyReplace ind2 (T.pack "    f x\n      g y") (T.pack "    h x\n      k y\n      z")) (Just (Right (T.pack "a\n  h x\n    k y\n    z\nb\n", 2, 3, -2)))
  eq "chat: ... one too little: shifted forward" (fuzzyReplace ind4 (T.pack "  f x\n    g y") (T.pack "  h x\n    k")) (Just (Right (T.pack "a\n    h x\n      k\nb\n", 2, 3, 2)))
  check "chat: ... not applied when the lines differ by no one constant, the lines as they are answered" (case fuzzyReplace ind2 (T.pack "  f x\n  g y") (T.pack "h") of { Just (Left w) -> T.isInfixOf (T.pack "more than one constant") w && T.isInfixOf (T.pack "3\x2502\&    g y") w; _ -> False })
  check "chat: ... nor when a line of the new text has no spaces to give" (case fuzzyReplace ind2 (T.pack "      f x\n        g y") (T.pack "h\n  k") of { Just (Left w) -> T.isInfixOf (T.pack "fewer spaces") w; _ -> False })
  check "chat: an edit of it says by how much it shifted, or refuses" (case (applyEdit ind2 (JObj [("old", JStr "    f x\n      g y"), ("new", JStr "    h\n      k")]), applyEdit ind2 (JObj [("old", JStr "  f x\n  g y"), ("new", JStr "h")])) of { (Right (t, how), Left w) -> t == T.pack "a\n  h\n    k\nb\n" && "shifted" `isInfixOf` how && T.isInfixOf (T.pack "not applied") w; _ -> False })
  eq "chat: ... but not when a word differs" (fuzzyReplace file (T.pack "foo baz") (T.pack "q")) Nothing
  eq "chat: ... nor when the words occur twice" (fuzzyReplace (T.pack "a b\na  b\n") (T.pack "a b") (T.pack "c")) Nothing
  check "chat: a text that occurs nowhere points at the line its first line matches" (T.isInfixOf (T.pack "line 2") (nearest file (T.pack "foo bar\nnope")))
  check "chat: ... or says it matches none" (T.isInfixOf (T.pack "matches no line") (nearest file (T.pack "nothing like it")))
  eq "chat: a replacement as written" (fst <$> replaceOnce (T.pack "a = 1\nb = 2\n") (T.pack "b = 2") (T.pack "b = 3")) (Right (T.pack "a = 1\nb = 3\n"))
  check "chat: ... or with its spacing squeezed, and says so" (either (const False) (T.isInfixOf (T.pack "x = 9") . fst) (replaceOnce file (T.pack "foo bar") (T.pack "x = 9")) && either (const False) (("spacing" `isInfixOf`) . snd) (replaceOnce file (T.pack "foo bar") (T.pack "x = 9")))
  let ts = TurnState [ JObj [("role", JStr "system"), ("content", JText (T.pack "be brief \"quoted\" \955 \n tab\t"))]
                     , JObj [("role", JStr "assistant"), ("content", JText T.empty), ("tool_calls", JArr [JObj [("id", JStr "call_1"), ("type", JStr "function"), ("function", JObj [("name", JStr "read"), ("arguments", JStr "{\"path\": \"src/A.hs\", \"lines\": 40}")])]])]
                     , JObj [("role", JStr "tool"), ("tool_call_id", JStr "call_1"), ("content", JText (T.pack "    1  module A where"))] ]
                     7 1 [ReadRec 1 2 "src/A.hs" 1 40 (T.pack "    1  module A where") Nothing False, ReadRec 2 5 "src/A.hs" 1 40 T.empty (Just 3) True]
                     (Spent 7 123456 120000 2345 9 True False) 1759800000.25 (Just (ViewCtx 4400 [4400, 4401] (T.pack "the task, \"quoted\"\nline two")))
      back = either (const Nothing) turnFrom (parseJson (encode (turnJson ts)))
  eq "chat: a turn written out for a restart reads back as it was" back (Just ts)
  check "chat: ... its conversation byte for byte (the provider's cache of it holds)" (fmap (map encode . tsMsgs) back == Just (map encode (tsMsgs ts)))
  eq "chat: a turn in --context turn reads back as it was too" (either (const Nothing) turnFrom (parseJson (encode (turnJson ts { tsView = Nothing })))) (Just ts { tsView = Nothing })
  let lg = [ (10, T.pack "user", T.pack "the task"), (11, T.pack "tool", T.pack "read {}"), (12, T.pack "echo", T.pack (replicate 100 'x')), (13, T.pack "talk", T.pack "done") ]
  eq "chat: <recent> is the log after the boundary, whole, without the pinned message"
     (renderTail [10] lg) (T.pack ("11|tool: read {}\n12|echo: " ++ replicate 100 'x' ++ "\n13|talk: done\n"))
  eq "chat: the boundary moves past the oldest messages until the rest fits" (map (\k -> newBound k [10] lg) [1000, 50, 0]) [10, 13, 14]
  let lgPlan = [ (10, T.pack "user", T.pack "the task")
               , (11, T.pack "talk", T.pack "plan: 1. check, 2. fix")
               , (12, T.pack "tool", T.pack "read {}")
               , (13, T.pack "echo", T.pack "file content")
               , (14, T.pack "user", T.pack "also test it")
               , (15, T.pack "talk", T.pack "sure, adding test step") ]
  eq "chat: <plan> keeps talk and mid-turn user messages up to planMax"
     (renderPlan planMax [10] lgPlan)
     (T.pack "11|talk: plan: 1. check, 2. fix\n14|user: also test it\n15|talk: sure, adding test step\n")
  eq "chat: <plan> with small budget keeps the initial plan plus tail updates"
     (renderPlan 70 [10] lgPlan)
     (T.pack "11|talk: plan: 1. check, 2. fix\n15|talk: sure, adding test step\n")
  eq "chat: the view's lines and the message each starts at" (map fst (viewLines (T.pack "<chat>\n0+8|a b\n8+4|c\n12+1|(not summarized yet: zoom it)\n</chat>\n"))) [0, 8, 12]
  eq "chat: a GHCi of the agent's own, through sh"
     (map ownGhci [ "timeout 300 ghci -isrc 2>&1 <<'EOF' | tail -5", "cd x && cabal repl lib:nes", "echo main | /opt/ghc/bin/ghci-9.14.1 -v0", "runghc Setup.hs", "ghc -e 'print 1'"
                  , "ghci-session status nes", "grep -rn ghci src", "cabal build", "ghc -fno-code -isrc src/Nes.hs" ])
     [True, True, True, True, True, False, False, False, False]
  eq "chat: sh output within the cap is whole" (capWith shCap "" (T.pack "ok")) (T.pack "ok")
  check "chat: ... over it, head and tail with the cut said" (let c = capWith shCap ": hint" (T.replicate 20000 (T.pack "x")) in T.length c < shCap + 200 && T.isInfixOf (T.pack "12000 characters cut: hint") c)
  eq "chat: edits without a path are in the file before them, else the call's"
     (map fst (editPaths (Just "B.hs") [JObj [("old", JStr "x")], JObj [("path", JStr "A.hs"), ("old", JStr "y")], JObj [("path", JStr ""), ("old", JStr "z")]]))
     [Just "B.hs", Just "A.hs", Just "A.hs"]
  eq "chat: ... and none at all is said so" (map fst (editPaths Nothing [JObj [("old", JStr "x")]])) [Nothing]
  check "chat: ... refused when it occurs twice" (either (T.isInfixOf (T.pack "2 times")) (const False) (replaceOnce (T.pack "x x") (T.pack "x") (T.pack "y")))
  check "chat: the edits tool takes an array of replacements" ("edits" `elem` map tName chatTools && maybe False ((== "array") . fst) (lookup "edits" (tProps (toolNamed "edits"))))
  check "chat: test takes an expression" ("expr" `elem` map fst (tProps (toolNamed "test")))
  -- the reads a turn's context holds: numbered, aliased when unchanged, superseded by a read of their lines
  eq "chat: a read's lines, from its answer" (readRange (T.pack "[f: lines 7-9 of 20]\n    7\x2502\&a\n    8\x2502\&  b\n    9\x2502\&c")) (Just (7, 9))
  eq "chat: a line is its number, the bar, the line as it is -- its indent is all after the bar" (map (uncurry (numbered ' ')) [(7, T.pack "    x = 1"), (12, T.pack "  | guard"), (8, T.pack "\tTab"), (9, T.empty)]) (map T.pack ["     7\x2502    x = 1", "    12\x2502  | guard", "     8\x2502\tTab", "     9\x2502"])
  eq "chat: a typecheck is clean when it says OK (warnings or not); an error, or nothing, is not" (map (tcClean . T.pack) ["OK -- TYPECHECK (0.2s, no source changed since)", "OK (3 warning(s)) -- TYPECHECK", "TYPE-ERROR: 1 error(s)", ""]) [True, True, False, False]
  check "chat: the note of an edit that changed nothing says it was applied, and not 'nothing changed'" (T.pack "applied" `T.isInfixOf` unchangedNote && not (T.pack "nothing changed" `T.isInfixOf` unchangedNote))
  eq "chat: a tool's answer is cut for showing at --show characters, with ...; 0 shows it whole" (map (\n -> shownCut n (T.pack "abcdef")) [3, 6, 9, 0]) (map T.pack ["abc...", "abcdef", "abcdef", "abcdef"])
  eq "chat: --wait takes seconds when a number follows it, and waits as long as it takes when not" (map (fmap oWait . parseOpts) [[], ["--send", "hi", "--wait", "30", "-s", "x"], ["--wait", "-s", "x"], ["--wait"]]) [Right Nothing, Right (Just (Just 30)), Right (Just Nothing), Right (Just Nothing)]
  eq "wait: what turn.json says of a turn -- no file or no start is over, a start without done is running, stopped is over" (map markOf [JObj [], JObj [("start", JNum 5)], JObj [("start", JNum 5), ("done", JBool True)], JObj [("start", JNum 5), ("stopped", JBool True)]]) [Mark Nothing True 0, Mark (Just 5) False 0, Mark (Just 5) True 0, Mark (Just 5) True 0]
  eq "wait: the lines a turn was given while it ran are counted" (markOf (JObj [("start", JNum 5), ("fed", JNum 2)])) (Mark (Just 5) False 2)
  let m5 = Mark (Just 5) False 0; m5d = Mark (Just 5) True 0; m6 = Mark (Just 6) False 0; m6d = Mark (Just 6) True 0; m5f = Mark (Just 5) True 1
  eq "wait: a line left at a chat at rest waits for the turn that begins after the one in the file, once the line is taken" [ settled m5d n t | (n, t) <- [(m5d, True), (m6, True), (m6d, True), (m6d, False)] ] [False, False, True, False]
  eq "wait: a line taken in the middle of a turn waits for that turn, when the turn fed it to itself" [ settled m5 n t | (n, t) <- [(m5, True), (m5f, True), (m5f, False)] ] [False, True, False]
  eq "wait: with no line sent (fed count -1), the turn under way is the one waited for even if it fed nothing" [ settled (Mark (Just 5) False (-1)) n True | n <- [m5, m5d, m6d] ] [False, True, True]
  eq "wait: a turn that ended without feeding the line taken meanwhile is not the one waited for: the next one is (even before it has begun)" [ settled m5 n True | n <- [m5d, m6, m6d] ] [False, False, True]
  eq "wait: a turn that ended while the line was still not taken leaves the wait to the next one" (let b = advance m5 m5d in (b, settled b m5d True, settled b m6d True)) (m5d, False, True)
  eq "wait: a turn running does not move what is waited for" (advance m5 m5, advance m5 m6, advance m5d m6d) (m5, m5, m5d)
  eq "wait: what is printed of an ended turn: its last words whole, then its summary" (turnReport (JObj [("last", JText (T.pack "All six done.\nSee the log.")), ("summary", JStr "[turn: 3 model calls]")])) "All six done.\nSee the log.\n[turn: 3 model calls]"
  eq "wait: a turn that was stopped says so; one that said nothing says that" (turnReport (JObj [("stopped", JBool True)])) "(the turn said nothing)\n[the turn was stopped]"
  eq "wait: a turn still running says how far it got" (stillRunning (JObj [("tools", JNum 7)]) 30.4) "[still running after 30s: 7 tool calls so far]"
  eq "on_turn_end: the command is told the session, the seconds and the tool calls of the turn" (hookEnv "main" (JObj [("secs", JNum 41.6), ("tools", JNum 12)])) [("GHS_SESSION", "main"), ("GHS_TURN_SECONDS", "42"), ("GHS_TURN_TOOL_CALLS", "12")]
  eq "chat: --show is 600 unless given" (map (fmap oShow . parseOpts) [[], ["--show", "0"], ["--show", "5000"]]) [Right 600, Right 0, Right 5000]
  eq "chat: with no line budget a read's head says nothing of one" (readHeader Nothing "f.hs" 1 3 9) "[f.hs: lines 1-3 of 9]\n"
  eq "chat: with a budget the head says the count against it" (readHeader (Just 250) "f.hs" 1 3 260) "[f.hs: lines 1-3 of 260 | 260 lines [OVER BUDGET: 260/250 lines!] (10 lines OVER BUDGET!)]\n"
  eq "chat: an edit's budget is said only when there is one" (map (budgetSaid Nothing) [10, 999], budgetSaid (Just 5) 10) ([Nothing, Nothing], Just "10 lines [OVER BUDGET: 10/5 lines!]")
  eq "chat: ... a match of grep has > before the number" (numbered '>' 41 (T.pack "a")) (T.pack ">   41\x2502\&a")
  check "chat: ... taking off up to the first bar gives every line back as it was" (and [ T.drop 1 (T.dropWhile (/= numberBar) (numbered m i l)) == l | m <- " >", i <- [1, 99999], l <- map T.pack ["", "  x", "a\x2502\&b", "\x2502", " 5  "] ])
  eq "chat: ... a read's lines are found in its numbered lines, matches and context" (readRange (T.pack "[f: 3 line(s) hold]\n>   41\x2502\&a\n   42\x2502\&b\n  100\x2502\&c")) (Just (41, 100))
  eq "chat: ... none in an answer that shows none" (readRange (T.pack "(empty)")) Nothing
  let r1 = ReadRec 1 4 "src/A.hs" 1 200 (T.pack "text of A") Nothing False
      r2 = ReadRec 2 6 "src/A.hs" 50 80 (T.pack "a part of A") Nothing False
      r3 = ReadRec 3 8 "src/B.hs" 1 200 (T.pack "text of B") Nothing False
  eq "chat: the same lines with the same text are an alias" (readAgainst [r1, r2, r3] "src/A.hs" (1, 200) (T.pack "text of A")) (Left 1)
  eq "chat: changed text supersedes the reads its lines cover, in that file only" (readAgainst [r1, r2, r3] "src/A.hs" (1, 200) (T.pack "new A")) (Right [1, 2])
  eq "chat: a narrower read supersedes nothing wider" (readAgainst [r1, r3] "src/A.hs" (50, 80) (T.pack "x")) (Right [])
  eq "chat: a superseded read is no alias" (readAgainst [r1 { rrBy = Just 9 }] "src/A.hs" (1, 200) (T.pack "text of A")) (Right [])
  eq "chat: a save waits at least 45 s" (saveWait 0) 45
  eq "chat: ... three times the longest verdict and a margin" (saveWait 100) 315
  eq "chat: ... at most ten minutes" (saveWait 1000) 600
  check "chat: red verdicts are red" (all isRed ["COMPILE-ERROR: 1 error(s)", "CHECK-FAIL: 2 failing in x", "CHECK-HANG: the check did not end in x", "DEAD: the repl died"])
  check "chat: a pass is not, nor a stale pass" (not (any isRed ["OK -- CHECK-PASS (3.6s)", "STALE(1) OK (2 warning(s)) -- CHECK-PASS (0.1s)"]))
  check "chat: vfs tool is present" ("vfs" `elem` map tName chatTools)
  eq "vfs: formatLineBudget under" (Vfs.formatLineBudget 120 250) "120 lines [budget: 120/250 lines]"
  eq "vfs: formatLineBudget over" (Vfs.formatLineBudget 260 250) "260 lines [OVER BUDGET: 260/250 lines!]"

  -- the census's names: a symbol read back as unit:Module.name
  eq "census: a closure's symbol is read back" (readSymbol "hellozm0zi1zi0zi0zminplace_Hello_bigTable_closure") "hello-0.1.0.0-inplace:Hello.bigTable"
  eq "census: ... with dots in the module and the name's own codes" (readSymbol "ghczm9zi14zi1zmc6c3_GHCziDataziFastString_stringTable_closure") "ghc-9.14.1-c6c3:GHC.Data.FastString.stringTable"
  eq "census: a name that is not one is left alone" (readSymbol "a local CAF of Hello") "a local CAF of Hello"
  eq "census: z-codes" (zdecode "zdwgo_zuzu_ZCzpZLZR") "$wgo____:+()"

  -- the hang detector: five times the median of the last passing checks, at least 15 s
  eq "hang: no history, no limit (the check's own timeout)" (hangLimit []) Nothing
  eq "hang: a fast check is given a minute (a suite that was a stub and is now a real one is not hung at fifteen seconds)" (hangLimit [3, 3.2, 2.9]) (Just 60)
  eq "hang: five times the median" (hangLimit [10, 30, 20]) (Just 100)

  -- the TUI library's frame (ghostty-tui): cells from puts, the difference between two frames, keys
  let fr = frame (8, 2) (textLine 0 0 8 [(plain, "ab"), (bold plain, "c")] ++ [PutText 0 1 plain "x\26085y"])   -- \26085 is a wide character
  eq "tui: a line is padded to the width" (map (\x -> T.unpack (cText (cellAt fr x 0))) [0 .. 7]) ["a", "b", "c", " ", " ", " ", " ", " "]
  eq "tui: a span keeps its style" (sBold (cStyle (cellAt fr 2 0)), sBold (cStyle (cellAt fr 1 0))) (True, False)
  eq "tui: a wide character takes two columns" (map (\x -> (T.unpack (cText (cellAt fr x 1)), cCont (cellAt fr x 1))) [0 .. 3]) [("x", False), ("\26085", False), ("", True), ("y", False)]
  check "tui: the first frame is written whole" (length (diff Nothing fr) > 20)
  eq "tui: the same frame again costs nothing" (diff (Just fr) fr) ""
  let fr2 = frame (8, 2) (textLine 0 0 8 [(plain, "ab"), (bold plain, "C")] ++ [PutText 0 1 plain "x\26085y"])
  check "tui: one changed cell is one move and the cell" (let d = diff (Just fr) fr2 in T.count (T.pack "\ESC[") (T.pack d) == 2 && "C" `isSuffixOf` d)
  eq "tui: the SGR of a style" (sgr (bold (withFg (Ansi 2) plain))) "\ESC[0;1;32;49m"
  eq "tui: a true color" (sgr (withBg (Rgb 1 2 3) plain)) "\ESC[0;39;48;2;1;2;3m"
  eq "tui: arrow and page keys" (map decodeKey ["\ESC[A", "\ESC[B", "\ESCOA", "\ESC[5~", "\ESC[6~", "\ESC[3~"]) [KUp, KDown, KUp, KPgUp, KPgDn, KDelete]
  eq "tui: plain keys" (map decodeKey ["q", "\r", "\ESC", "\DEL"]) [KChar 'q', KEnter, KEsc, KBackspace]
  eq "tui: modified keys" (map (\x -> let KeyPress k m _ = decodeKeyPress x in (k, m)) ["\ESC[1;5A", "\ESC[1;2C", "\ESC[3;3~", "\ESC[Z", "\ESCx", "\ETX"])
     [(KUp, [Ctrl]), (KRight, [Shift]), (KDelete, [Alt]), (KTab, [Shift]), (KChar 'x', [Alt]), (KChar 'c', [Ctrl])]
  let kev x = keyEventFor (decodeKeyPress x)
  eq "tui: a letter is its key and its text" (kev "a") (Just (Vt.KeyEvent Vt.Press "a" [] (T.pack "a") (Just 'a')))
  eq "tui: a capital is the key with shift" (kev "A") (Just (Vt.KeyEvent Vt.Press "a" [Vt.Shift] (T.pack "A") (Just 'a')))
  eq "tui: a shifted symbol names its key" (kev "!") (Just (Vt.KeyEvent Vt.Press "digit_1" [Vt.Shift] (T.pack "!") (Just '1')))
  eq "tui: a control character is ctrl and the letter" (kev "\ETX") (Just (Vt.KeyEvent Vt.Press "c" [Vt.Ctrl] T.empty (Just 'c')))
  eq "tui: an arrow with ctrl" (kev "\ESC[1;5A") (Just (Vt.KeyEvent Vt.Press "arrow_up" [Vt.Ctrl] T.empty Nothing))
  eq "top: a long line wraps with its continuation indented" (Top.wrapSpans 6 2 [(plain, "abcdefghij")]) [[(plain, "abcdef")], [(plain, "  "), (plain, "ghij")]]
  eq "top: a span is not split when it fits" (Top.wrapSpans 10 2 [(bold plain, "abc"), (plain, "def")]) [[(bold plain, "abc"), (plain, "def")]]
  -- the chat's screen: the line typed, the transcript
  let press x = decodeKeyPress (x :: String)
      typed = foldl (\ed x -> maybe ed id (ChatTui.editKey (press x) ed)) ChatTui.editor
  eq "chat tui: typing" (ChatTui.editText (typed ["h", "e", "y"])) "hey"
  eq "chat tui: left, insert, backspace" (ChatTui.editText (typed ["a", "c", "\ESC[D", "b", "\DEL", "x"])) "axc"
  eq "chat tui: ctrl-a, delete, ctrl-e" (ChatTui.editText (typed ["a", "b", "\SOH", "\ESC[3~", "\ENQ", "z"])) "bz"
  eq "chat tui: ctrl-w takes the word before, ctrl-u the line before, ctrl-k the line after"
     (map (ChatTui.editText . typed) [["o", "n", "e", " ", "t", "w", "o", "\ETB"], ["a", "b", "\NAK"], ["a", "b", "\ESC[D", "\VT"]]) ["one ", "", "a"]
  eq "chat tui: a key that does not edit leaves the line alone" (ChatTui.editKey (press "\ESC[5~") (ChatTui.Editor "ba" "c")) Nothing
  let entries = [ChatTui.Entry "talk" (T.pack "two\nlines"), ChatTui.Entry "user" (T.pack "hi")]     -- (newest first)
  eq "chat tui: an entry is its label and its lines, what was said followed by a blank"
     (map (concatMap snd) (ChatTui.entryLines 40 (ChatTui.Entry "tool" (T.pack "eval 1+1")))) ["tool:  eval 1+1"]
  eq "chat tui: the lines of a talk" (map (concatMap snd) (ChatTui.entryLines 40 (head entries))) ["talk:  two", "       lines", ""]
  eq "chat tui: the end of the transcript, following" (map (concatMap snd) (snd (ChatTui.scrollLines 40 2 0 entries))) ["       lines", ""]
  eq "chat tui: scrolled back as far as there is" (let (b, ls) = ChatTui.scrollLines 40 2 100 entries in (b, map (concatMap snd) ls)) (3, ["user:  hi", ""])

  -- small OS things
  sp <- sockPath ("/a/very/" ++ concat (replicate 40 "long/") ++ "state")
  check "socket path is short" (length sp < 100)
  pidAlive (fromIntegral pid) >>= check "pidAlive: this process"
  pidAlive 4190000 >>= check "pidAlive: nobody" . not
  rawSystemOut 0.05 "sleep" ["5"] >>= \r -> eq "a helper that hangs is cut off" r Nothing

  void (try (removeDirectoryRecursive tmp) :: IO (Either IOException ()))
  f <- readIORef failed
  n <- readIORef total
  putStrLn (if f == 0 then "selftest: all " ++ show n ++ " passed" else "selftest: " ++ show f ++ " of " ++ show n ++ " FAILED")
  pure (f == 0)
