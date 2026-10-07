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
import GhciSession.Chat (ReadRec (..), arguments, chatTools, Spent (..), TurnState (..), ViewCtx (..), capWith, newBound, renderTail, renderPlan, planMax, viewLines, editPaths, fuzzyReplace, isRed, ownGhci, shCap, turnFrom, turnJson, nearest, readAgainst, readRange, replaceOnce, saveWait, splitImports, writeRuns, groupByPaths)
import GhciSession.Daemon (countSub, hangLimit, moduleDelta, replace, unitsBelow, verdictOf, warningsIn)
import GhciSession.Mcp (Tool (..))
import GhciSession.Doc
import qualified GhciSession.History as H
import qualified GhciSession.ChatTui as ChatTui
import qualified GhciSession.Top as Top
import Tui (Cell (..), Put (..), Key (..), KeyPress (..), Mod (..), cellAt, decodeKey, decodeKeyPress, diff, frame, keyEventFor, sgr, textLine)
import qualified Ghostty.Vt as Vt
import Tui.Types
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import qualified GhciSession.Search as Search
import qualified GhciSession.Vfs as Vfs
import GhciSession.Json
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
             ++ "\"b\": {\"units\": [\"lib:b\"], \"watch\": [\"b/src\"], \"modules\": [\"B\", \"A\"], \"hygiene\": true, \"load_timeout\": 2000, \"idle_stop_mins\": 30,"
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
  let hp = H.Params { H.pNode = 24, H.pNodeMax = 24, H.pView = 60, H.pCap = 30000, H.pCtxMax = 65536, H.pCtxMin = 32768 }
  let lp = H.defaultParams
      long = T.unwords (replicate 300 (T.pack "word"))
  check "history: a summary line up to NODE_MAX bytes is taken as it is" (H.nodeFits lp (T.replicate 1000 (T.pack "x")) && not (H.nodeFits lp (T.replicate 1025 (T.pack "x"))))
  check "history: ... one still over it after the tries is cut at the last word that fits"
        (let c = H.fitNode lp long in H.byteLength c <= 1024 && H.byteLength c > 1000 && T.isSuffixOf (T.pack "word") c)
  eq "history: the scale line is exactly NODE bytes" (H.byteLength (H.scaleLine H.defaultParams)) 512
  eq "history: a cut never splits a character" (H.cutBytes 2 (T.pack "a\233b")) (T.pack "a")
  eq "history: a cut at a boundary keeps the character" (H.cutBytes 3 (T.pack "a\233b")) (T.pack "a\233")
  check "history: a capped result keeps head and tail and says so" (let c = H.capText 10 (T.pack (replicate 40 'x')) in T.isInfixOf (T.pack "30 characters cut") c && T.isPrefixOf (T.pack "xxxxx\n") c && T.isSuffixOf (T.pack "xxxxx") c)
  eq "history: a short result is not touched" (H.capText 10 (T.pack "short")) (T.pack "short")
  let hdir = tmp </> "history"
  (hm, torn0) <- H.openHistory hp hdir
  eq "history: a new one is empty" torn0 0
  ids <- mapM (H.appendMsg hm (T.pack "tool")) (map T.pack ["eval 1 + 1", "eval 2 * 3", T.unpack (T.replicate 40 (T.pack "long ")), "reload"])
  eq "history: ids are the position" ids [0, 1, 2, 3]
  sn1 <- H.snapshot hm
  check "history: a short message is its own node, free" (M.member (0, 0) (H.sTree sn1) && M.member (0, 1) (H.sTree sn1))
  check "history: a long one is not" (not (M.member (0, 2) (H.sTree sn1)))
  check "history: two short lines that fit together merge free" (not (M.member (1, 0) (H.sTree sn1)))   -- 16+1+16 > 24: they do not
  eq "history: the view tiles the log, one part a message while nothing can merge" (H.sView sn1) [(0, 0), (0, 1), (0, 2), (0, 3)]
  check "history: an unbuilt line shows the placeholder" (T.isInfixOf (T.pack "2+1|(not summarized yet: zoom it)") (H.renderView sn1))
  check "history: the view is not settled with one" (not (H.settled sn1))
  let (jobs1, _) = H.pendingOf hp sn1 Set.empty M.empty 0 M.empty
  eq "history: the pump compresses the first unbuilt message, and merges what lies before it" (map (\j -> (H.jL j, H.jI j)) jobs1) [(0, 2), (1, 0)]
  check "history: a compress job carries the message whole, and the lines before it, bare" (case jobs1 of { (j : _) -> H.jStep j == H.Compress (T.pack ("tool: " ++ T.unpack (T.replicate 40 (T.pack "long ")))) && H.jContext j == [T.pack "tool: eval 1 + 1", T.pack "tool: eval 2 * 3"]; _ -> False })
  check "history: a merge job carries its two lines" (case jobs1 of { [_, j] -> H.jStep j == H.Merge (T.pack "tool: eval 1 + 1") (T.pack "tool: eval 2 * 3"); _ -> False })
  let (jobs2, _) = H.pendingOf hp sn1 (Set.fromList [(0, 2)]) M.empty 0 M.empty
  eq "history: a busy node is not offered again" (map (\j -> (H.jL j, H.jI j)) jobs2) [(1, 0)]
  let (jobs3, _) = H.pendingOf hp sn1 Set.empty (M.fromList [((0, 2), 100)]) 50 M.empty
  eq "history: a failed node waits out its retry" (map (\j -> (H.jL j, H.jI j)) jobs3) [(1, 0)]
  check "history: the prompt has no ids" (let pr = H.jobPrompt hp (head jobs1) in not (T.isInfixOf (T.pack "0+1") pr) && T.isInfixOf (T.pack "<chat>\ntool: eval 1 + 1\n") pr && T.isInfixOf (T.pack "exactly 24 bytes") pr)
  H.putNode hm 0 2 (T.pack "tool: eval of a long one")
  H.putNode hm 1 0 (T.pack "tool: two evals")
  sn2 <- H.snapshot hm
  check "history: the view settles once every line is built" (H.settled sn2)
  eq "history: over budget, the most due pair with a built parent merges" (H.sView sn2) [(1, 0), (0, 2), (0, 3)]
  -- the budget is the view as rendered (prefixes, newlines, tags): 4 lines of 68 bytes of text render as 103
  eq "history: the budget counts each line's id+n| and newline, not the texts alone"
     (H.fitView hp { H.pView = 80 } 4 (H.sTree sn2) [(0, 0), (0, 1), (0, 2), (0, 3)]) [(1, 0), (0, 2), (0, 3)]
  check "history: ... and a view that fits as rendered is left alone"
     (H.fitView hp { H.pView = 103 } 4 (H.sTree sn2) [(0, 0), (0, 1), (0, 2), (0, 3)] == [(0, 0), (0, 1), (0, 2), (0, 3)])
  zr <- H.zoom hm 0 2
  eq "history: zoom opens a line into its two" zr (Right (T.pack "0+1|tool: eval 1 + 1\n1+1|tool: eval 2 * 3\n"))
  z1 <- H.zoom hm 3 1
  eq "history: zoom 1 is the message whole" z1 (Right (T.pack "3+0|tool: reload"))
  H.zoom hm 1 2 >>= check "history: a line that is not in the tree's shape is refused" . either (const True) (const False)
  ids2 <- mapM (H.appendMsg hm (T.pack "echo")) (map T.pack ["ok", "ok", "ok", "ok"])
  eq "history: ids go on" ids2 [4 .. 7]
  sn3 <- H.snapshot hm
  check "history: a merged part is never split" (take 1 (H.sView sn3) == [(1, 0)])
  (hm2, torn1) <- H.openHistory hp hdir
  sn4 <- H.snapshot hm2
  eq "history: reopened: no torn lines" torn1 0
  eq "history: reopened: the same messages" (fmap H.mText (H.sRoot sn4)) (fmap H.mText (H.sRoot sn3))
  eq "history: reopened: the same tree" (H.sTree sn4) (H.sTree sn3)
  eq "history: reopened: the same view, folded again from message 0" (H.sView sn4) (H.sView sn3)
  mainFiles <- listDirectory (hdir </> "main")
  forM_ (take 1 mainFiles) $ \f -> appendFile (hdir </> "main" </> f) "{\"i\": 8, \"kind\": \"tool\", \"te"
  (hm3, torn2) <- H.openHistory hp hdir
  n3 <- H.count hm3
  eq "history: a torn line is skipped and counted" (torn2, n3) (1, 8)
  i9 <- H.appendMsg hm3 (T.pack "note") (T.pack "after the tear")
  eq "history: the next line starts on its own line" i9 8
  d9 <- H.dateOf hm3 8
  check "history: a message has its date" (maybe False (> 0) d9)

  -- the compactor's context: the last pCtxMin..pCtxMax bytes of the lines before the node, cut at the front
  let hpw = hp { H.pCtxMax = 30, H.pCtxMin = 20 }
      (jobsW, _) = H.pendingOf hpw sn1 Set.empty M.empty 0 M.empty
  check "history: over the context's maximum, the front is cut down to its minimum" (case jobsW of { (j : _) -> H.jContext j == [T.pack "tool: eval 2 * 3"]; _ -> False })
  check "history: under the maximum, the context is every line before the node" (case jobs1 of { (j : _) -> length (H.jContext j) == 2; _ -> False })

  -- the chat's harness (GhciSession.Chat): what it does to the model's calls and to the files
  let toolNamed n = head [ t | t <- chatTools, tName t == n ]
      args n kvs = arguments (toolNamed n) (JObj kvs)
  eq "chat: an argument called by another name is taken by its name" (args "sh" [("command", JStr "ls")]) (JObj [("cmd", JStr "ls")], [])
  eq "chat: an alias of an argument the tool does not take is left alone (zoom keeps its n)" (args "zoom" [("id", JNum 1), ("n", JNum 2)]) (JObj [("id", JNum 1), ("n", JNum 2)], [])
  eq "chat: a missing argument is named" (snd (args "edit" [("path", JStr "a"), ("new", JStr "b")])) ["old"]
  eq "chat: file is path" (args "read" [("file", JStr "src/A.hs")]) (JObj [("path", JStr "src/A.hs")], [])
  check "chat: remember is a tool, and no tool takes a session" ("remember" `elem` map tName chatTools && all (\t -> "session" `notElem` map fst (tProps t)) chatTools)
  check "chat: grep and find are chat tools" ("grep" `elem` map tName chatTools && "find" `elem` map tName chatTools)
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
  eq "chat: an edit that differs only in spacing is applied where its words are" (fuzzyReplace file (T.pack "foo bar") (T.pack "baz")) (Just (T.pack "x = 1\n  baz\ny = 2\n", 2, 2))
  eq "chat: ... the indent and newline the old text had come off the new text" (fuzzyReplace file (T.pack "  foo bar\n") (T.pack "  qux\n")) (Just (T.pack "x = 1\n  qux\ny = 2\n", 2, 2))
  eq "chat: ... across lines" (fuzzyReplace file (T.pack "foo bar y = 2") (T.pack "z")) (Just (T.pack "x = 1\n  z\n", 2, 3))
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
  eq "chat: a read's lines, from its answer" (readRange (T.pack "    7  a\n    8  b\n    9  c")) (Just (7, 9))
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
  eq "hang: a fast check is given 15 s" (hangLimit [3, 3.2, 2.9]) (Just 15)
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
