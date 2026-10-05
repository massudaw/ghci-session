-- | The tool's own fast checks: the pure logic, in well under a second, with no GHC session involved. This is
-- the check of the session this package runs ON ITSELF (@ghci-session.json@ beside the cabal file): save a
-- module here, and the verdict is these. The behaviour end to end is @examples/tour.py@.
--
-- @ghci-session selftest@ runs them from the executable.
module GhciSession.SelfTest (run) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless, void, when)
import Data.IORef
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)

import GhciSession.Cli (Args (..), autostopPlan, parseArgs)
import GhciSession.Config
import GhciSession.Daemon (countSub, moduleDelta, replace, verdictOf, warningsIn)
import GhciSession.Doc
import GhciSession.Json
import GhciSession.Sys
import GhciSession.Watch

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
