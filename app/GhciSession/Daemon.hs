{-# LANGUAGE ScopedTypeVariables #-}

-- | The per-session daemon: owns one repl, serves reload/eval/check/status on a unix socket, watches the
-- sources, forks the session's servers.
module GhciSession.Daemon (runDaemon, dataDir, verdictOf, warningsIn, countSub, replace) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (IOException, SomeException, bracket_, displayException, finally, throwIO, try)
import Control.Monad (filterM, foldM, forM, forM_, unless, void, when)
import Data.Char (isDigit, isSpace, isUpper, toLower)
import Data.IORef
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, sort, sortOn)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, takeDirectory, takeFileName, (</>))
import System.IO
import System.Info (os)
import Data.Word (Word64)
import System.Posix.Files (FileStatus, fileSize, getFileStatus, modificationTimeHiRes)
import System.Posix.IO (fdToHandle)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Posix.Types (CPid (..))
import System.Process (CreateProcess (..), readCreateProcessWithExitCode, shell)
import Text.Printf (printf)

import GhciSession.Config
import GhciSession.Json
import GhciSession.Repl
import GhciSession.Sys
import GhciSession.Watch
import qualified Paths_ghci_session as Paths

-- | Where the package's data is (the hygiene C sources and build script, the RTS wrapper): the installed
-- data directory, or -- running from a build tree -- the nearest directory above the executable that has it.
dataDir :: IO FilePath
dataDir = do
  env <- lookupEnv "GHCI_SESSION_DATA"
  inst <- Paths.getDataDir
  exe <- getExecutablePath
  let ups d = d : (let p = takeDirectory d in if p == d then [] else ups p)
      has d = doesFileExist (d </> "hygiene" </> "build.sh")
  found <- filterM has (maybe [] pure env ++ [inst] ++ ups (takeDirectory exe))
  pure (fromMaybe inst (listToMaybe found))

data S = S
  { sConf :: Conf, sName :: String, sCfg :: Cfg, sRoot :: FilePath, sDir :: FilePath, sObjRel :: FilePath
  , sData :: FilePath, sBootCheck :: Bool
  , vRepl :: IORef (Maybe Repl)
  , vStatus :: IORef String, vJson :: IORef Json, vStatusText :: IORef String
  , vLoadedSig :: IORef Sig, vPendingSig :: IORef Sig
  , vLoadedAt :: IORef Double, vCheckedAt :: IORef Double
  , vStopping :: IORef Bool, vKeepServers :: IORef Bool, vStopReason :: IORef String
  , vOwed :: IORef [String]
  , vWork :: MVar ()                 -- ^ the watcher and a client both drive the repl; never at once
  , vHygieneOn :: IORef Bool, vZygoteOn :: IORef Bool, vHasUnlink :: IORef Bool
  , vUnlinkDue :: IORef Bool         -- ^ a reload has superseded code that has not been unlinked yet
  , vEvaluated :: IORef Bool         -- ^ ... and something has been evaluated since (its replacement is linked)
  , vContextOk :: IORef Bool
  , vGeneration :: IORef Int, vCwd :: IORef FilePath
  , vLastUsed :: IORef Double        -- ^ the last client command or source change: what "idle" is measured from
  , vBusy :: IORef Int               -- ^ >0 while a command, a reload or a check holds the repl
  , vRefork :: IORef (Maybe (MVar ()))        -- ^ a background re-fork in progress (filled when it ends)
  , vReforkPending :: IORef (Maybe (IO ()))   -- ^ ... and one to start once the verdict is published
  , vPhases :: IORef (M.Map String Double)
  , vHold :: IORef Int               -- ^ >0: an operation is in progress; its status is published once, at its end
  , vOkPrefix :: IORef String, vLiveMb :: IORef String
  , vMem :: IORef (Maybe (Double, Double)), vMemDone :: IORef (Maybe (MVar ()))
  , vSpawned :: IORef Double
  , vHashes :: IORef (M.Map FilePath (Integer, Double, Word64))   -- ^ a file's hash, while its size and mtime stand
  }

rd :: IORef a -> IO a
rd = readIORef

(=:) :: IORef a -> a -> IO ()
(=:) = writeIORef

logS :: S -> String -> IO ()
logS s msg = do
  t <- formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime
  void (try (appendFileUtf8 (sDir s </> "daemon.log") ("[" ++ t ++ "] " ++ msg ++ "\n")) :: IO (Either IOException ()))

rel :: S -> FilePath -> FilePath
rel s = makeRelative (sRoot s)

-- where the time goes ----------------------------------------------------------

-- | Time a part of an operation: accumulated, logged and put in status.json by the operation when it ends
-- (@phases_s@). The answer to "why did that take 2 s".
phase :: S -> String -> IO a -> IO a
phase s name act = do
  t0 <- now
  act `finally` (now >>= \t1 -> modifyIORef' (vPhases s) (M.insertWith (+) name (t1 - t0)))

phasesDone :: S -> String -> Double -> IO ()
phasesDone s what t0 = do
  t1 <- now
  ph <- rd (vPhases s)
  let total = t1 - t0
      all' = M.insert "other" (total - sum (M.elems ph)) ph
      shown = [ k ++ " " ++ printf "%.2f" v | (k, v) <- sortOn (negate . snd) (M.toList all'), v >= 0.005 ]
  logS s (printf "[time] %s %.2fs: %s" what total (intercalate ", " shown))
  modifyIORef' (vJson s) (set "op" (JStr what) . set "phases_s" (JObj [ (k, JNum (r3 v)) | (k, v) <- M.toList all' ]))
  vPhases s =: M.empty

r3, r2 :: Double -> Double
r3 x = fromIntegral (round (x * 1000) :: Integer) / 1000
r2 x = fromIntegral (round (x * 100) :: Integer) / 100

-- status -------------------------------------------------------------------------

staleFiles :: S -> IO [FilePath]
staleFiles s = do
  cur <- scan (sRoot s) (gWatch (sCfg s)) (gWatchExt (sCfg s))
  loaded <- rd (vLoadedSig s)
  pure (sort [ fromRaw p | p <- M.keys (M.union cur loaded), M.lookup p cur /= M.lookup p loaded ])

stamp :: Double -> IO String
stamp 0 = pure "-"
stamp t = formatTime defaultTimeLocale "%H:%M:%S" <$> utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac t))

-- | Set the verdict. @keep@: an annotation of the current one, whose facts (members, servers) stay.
setStatus :: S -> Bool -> String -> [String] -> [(String, Json)] -> IO ()
setStatus s keep text0 detail facts = do
  -- A failure in a session that has NEVER passed is probably the target's, not the edit just made: say so.
  let lastpass = sDir s </> "last-pass"
  passed <- doesFileExist lastpass
  text <- if "OK" `isPrefixOf` text0 && not ("CHECK SKIPPED" `isInfixOf` text0)
            then do unless passed (dateTime >>= \d -> writeAtomic lastpass (d ++ "\n")); pure text0
            else pure (if "CHECK-FAIL" `isPrefixOf` text0 && not ("[NEVER-PASSED" `isInfixOf` text0) && not passed
                         then text0 ++ "  [NEVER-PASSED: no check has been green in this state dir -- suspect the target as much as your edit]"
                         else text0)
  vStatus s =: text
  stale <- if text == "starting" then pure [] else staleFiles s
  gen <- rd (vGeneration s)
  la <- rd (vLoadedAt s) >>= stamp
  ca <- rd (vCheckedAt s) >>= stamp
  dt <- dateTime
  t <- now
  let headL = if null stale then text else "STALE(" ++ show (length stale) ++ ") " ++ text
      line2 = "session=" ++ sName s ++ " gen=" ++ show gen ++ " at=" ++ dt ++ " loaded=" ++ la ++ " checked=" ++ ca
      ls = [headL, line2] ++ [ "stale: " ++ intercalate ", " (map (rel s) (take 6 stale)) | not (null stale) ] ++ detail
      kind = fromMaybe (fromMaybe "OK" (listToMaybe [ k | k <- ["CHECK-FAIL", "CHECK-PASS"], k `isInfixOf` text ]))
               (listToMaybe [ k | k <- ["DEAD", "stopped", "starting", "PREBUILD-ERROR", "CONFIG-ERROR", "COMPILE-ERROR"], k `isPrefixOf` text ])
  vStatusText s =: (unlines ls)
  old <- rd (vJson s)
  let base = if keep then old else JObj []
      j1 = foldl (\j (k, v) -> set k v j) base
             [ ("session", JStr (sName s)), ("target", JStr (sName s)), ("kind", JStr kind), ("ok", JBool (kind `elem` ["OK", "CHECK-PASS"]))
             , ("stale", JNum (fromIntegral (length stale))), ("stale_files", JArr (map (JStr . rel s) stale))
             , ("warnings", JNum (fromIntegral (warningsIn text))), ("verdict", JStr text), ("text", JStr headL)
             , ("failing", JNum (if kind == "CHECK-FAIL" then fromIntegral (length detail) else 0))
             , ("detail", JArr (map JStr (take 30 detail))), ("generation", JNum (fromIntegral gen)), ("at", JNum t) ]
      j2 = setDefault "servers" (JArr []) (setDefault "members" (JArr []) j1)
  vJson s =: foldl (\j (k, v) -> set k v j) j2 facts
  hold <- rd (vHold s)
  when (hold == 0) (publish s)
  push s headL line2
  where dateTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getZonedTime

warningsIn :: String -> Int
warningsIn t = fromMaybe 0 (listToMaybe [ read ds | r <- tailsOf t, take 1 r == "(", let (ds, rest) = span isDigit (drop 1 r)
                                                   , not (null ds), " warning(s))" `isPrefixOf` rest ])
  where tailsOf [] = []
        tailsOf x@(_ : r) = x : tailsOf r

-- | POST a verdict to @status_url@, if one is configured: what lets a dashboard be event-driven instead of
-- polling. Sent for every status, the intermediate ones too. Best effort, 0.25 s.
push :: S -> String -> String -> IO ()
push s verdict detail = case gStatusUrl (sCfg s) of
  Nothing -> pure ()
  Just url -> do
    j <- rd (vJson s)
    httpPost url (encode (JObj [ ("session", JStr (sName s)), ("target", JStr (sName s)), ("verdict", JStr verdict)
                               , ("detail", JStr detail), ("alive", JStr "1"), ("kind", j .: "kind"), ("ok", j .: "ok") ])) 250

-- | status first, status.json second: a reader that finds the JSON can trust the text beside it.
publish :: S -> IO ()
publish s = do
  rd (vStatusText s) >>= writeAtomic (sDir s </> "status")
  rd (vJson s) >>= writeAtomic (sDir s </> "status.json") . encodePretty

-- | A reload is a verdict AND what then happened to the servers: published once, when both are known, so a
-- reader never sees the first half as if it were the whole.
oneVerdict :: S -> IO a -> IO a
oneVerdict s = bracket_ (modifyIORef' (vHold s) (+ 1)) (do modifyIORef' (vHold s) (subtract 1); h <- rd (vHold s); when (h == 0) (publish s))

-- | Append to the verdict (what happened to the servers, a restart) without losing its facts.
note :: S -> String -> [(String, Json)] -> IO ()
note s suffix facts = do
  st <- rd (vStatus s)
  j <- rd (vJson s)
  setStatus s True (st ++ "  " ++ suffix) (strs (j .: "detail")) facts

compiled :: S -> IO Bool
compiled s = (\st -> not (any (`isPrefixOf` st) ["COMPILE-ERROR", "DEAD", "PREBUILD-ERROR", "CONFIG-ERROR"])) <$> rd (vStatus s)

-- the repl -------------------------------------------------------------------------

theRepl :: S -> IO Repl
theRepl s = rd (vRepl s) >>= maybe (throwIO (ReplDied "")) pure

-- | A command's output as text: a load log or an evaluation can be megabytes.
cmd :: S -> Maybe Double -> String -> IO T.Text
cmd s t e = theRepl s >>= \r -> replCommand r t e

-- | ... and as a String, for the small answers the daemon itself reads.
cmdS :: S -> Maybe Double -> String -> IO String
cmdS s t e = T.unpack <$> cmd s t e

ghcVersion :: IO (Int, Int)
ghcVersion = do
  v <- rawSystemOut 20 "ghc" ["--numeric-version"]
  pure $ case map read (take 2 (splitOn '.' (filter (\c -> isDigit c || c == '.') (fromMaybe "" v)))) of
    [a, b] -> (a, b)
    _ -> (0, 0)

splitOn :: Char -> String -> [String]
splitOn c x = case break (== c) x of
  (a, []) -> [a | not (null a)]
  (a, _ : r) -> a : splitOn c r

replCommandLine :: S -> IO String
replCommandLine s = case gRepl cfg of
  Just c -> pure c
  Nothing -> do
    v <- if objects then ghcVersion else pure (0, 0)
    pure $ unwords $ filter (not . null) $
      [ "cabal repl" ]
      ++ [ "--enable-multi-repl" | length (gUnits cfg) > 1 || not (null (gServers cfg)) ]
      ++ [ "--with-repl=" ++ (sData s </> "bin" </> "ghci-rts.sh") | gRtsFlags cfg `notElem` ["", "none"] ]
      ++ [ "--repl-options=-fdiagnostics-color=never" ]
      ++ [ "--repl-options=-j" ++ show (gGhcJobs cfg) | gGhcJobs cfg > 0 ]
         -- object code: CAFs of interpreted code are not prunable by address, and a server's code is its
         -- objects. Its own -odir (relative: under each unit's package dir) so `cabal build` is not disturbed.
      ++ (if objects then [ "--repl-options=-fobject-code", "--repl-options=-odir=" ++ sObjRel s, "--repl-options=-hidir=" ++ sObjRel s ] else [])
         -- without it GHCi's recompile of IDENTICAL source gives a different .o, and every reload would
         -- look like a change to a running server
      ++ [ "--repl-options=-fobject-determinism" | objects, v >= (9, 12) ]
      ++ [ gCabalArgs cfg, unwords (gUnits cfg) ]
  where cfg = sCfg s
        objects = gHygiene cfg || not (null (gServers cfg))

-- | Build the hygiene C libraries against this GHC's RTS -- unless they are newer than their sources, the
-- script and the compiler (asked in a few stats: running the script to find that out is 0.15 s).
buildHygiene :: S -> IO ()
buildHygiene s = do
  let script = sData s </> "hygiene" </> "build.sh"
      clib = cStateDir (sConf s) </> "clib"
  ghc <- findExecutable "ghc" >>= maybe (pure "") canonicalizePath
  cs <- either (\(_ :: IOException) -> []) (map ((sData s </> "hygiene" </> "c") </>) . filter (".c" `isSuffixOf`))
          <$> try (getDirectoryContents (sData s </> "hygiene" </> "c"))
  srcT <- mapM modTime ([script, ghc] ++ cs)
  libT <- mapM (modTime . (clib </>)) ["libghscafs.dylib", "libghscensus.dylib", "libghsloader.dylib"]
  let fresh = not (null ghc) && all isJust (srcT ++ libT) && minimum (catMaybes libT) > maximum (catMaybes srcT)
  unless fresh $ do
    (_, o, e) <- readCreateProcessWithExitCode (shell (shq script ++ " " ++ shq clib)) { cwd = Just (sRoot s) } ""
    let msg = if null (trim e) then (if null (trim o) then "ok" else trim o) else trim e
    logS s ("hygiene build: " ++ map (\c -> if c == '\n' then ';' else c) msg)

shq :: String -> String
shq x = "'" ++ concatMap (\c -> if c == '\'' then "'\\''" else [c]) x ++ "'"

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

noModule :: String -> Bool
noModule out = any (`isInfixOf` map toLower out)
  ["could not find module", "could not load module", "not in scope", "is not loaded", "hidden package", "no such module"]

postLoad :: S -> Repl -> IO ()
postLoad s r = do
  let cfg = sCfg s
      c t e = T.unpack <$> replCommand r (Just t) e
  postLoadBasics r
  when (gCapabilities cfg > 0) (void (c 60 ("GHC.Conc.setNumCapabilities " ++ show (gCapabilities cfg))))
  forM_ (gPreload cfg) (c 120)
  -- one command for all the imports (forty round trips were 0.3 s of a boot); one at a time only if that
  -- fails, so a single missing module does not cost the rest
  unless (null (gModules cfg)) $ do
    out <- c 120 (":module + " ++ unwords (gModules cfg))
    when (noModule out) (forM_ (gModules cfg) (\m -> c 60 (":module + " ++ m)))
  when (gHygiene cfg) $ do
    out <- c 60 (":module + " ++ gHygieneModule cfg ++ " GHC.Stats")
    vHygieneOn s =: not (noModule out)
    when (noModule out) (logS s ("hygiene OFF: " ++ gHygieneModule cfg ++ " is not in scope in this repl (add the ghci-session library to build-depends)"))
  unless (null (gServers cfg)) $ do
    out <- c 60 (":module + " ++ gZygoteModule cfg)
    vZygoteOn s =: not (noModule out)
    when (noModule out) (logS s ("servers OFF: " ++ gZygoteModule cfg ++ " is not in scope in this repl (add the ghci-session library to build-depends)"))

-- | The signature the loaded code was built from, as a file the loaded code can read:
-- @<mtime ns>\\t<path relative to the root>@ per watched source.
writeLoadedSources :: S -> IO ()
writeLoadedSources s = do
  sig <- rd (vLoadedSig s)
  void (try (writeAtomic (sDir s </> "loaded_sources.tsv")
               (concat [ show (round (m * 1e9) :: Integer) ++ "\t" ++ rel s (fromRaw p) ++ "\n" | (p, m) <- M.toList sig ])) :: IO (Either IOException ()))

-- | GHC's own verdict lines decide whether a load succeeded.
verdictOf :: T.Text -> (String, [String])
verdictOf out
  | any (T.isPrefixOf (T.pack "Failed, ")) ls || not (null errs) = ("COMPILE-ERROR: " ++ show (length errs) ++ " error(s)", map T.unpack (take 20 errs))
  | any okLine ls = ("OK", [])
  | otherwise = ("COMPILE-ERROR: no GHC verdict in the load output", map T.unpack (lastN 5 (T.lines (T.strip out))))
  where
    ls = T.lines out
    -- not anchored on a source location: GHC also emits `<no location info>: error:` for link/IO failures
    errs = filter (T.isInfixOf (T.pack ": error:")) ls
    okLine l = T.pack "Ok, " `T.isPrefixOf` l && T.pack "loaded." `T.isSuffixOf` l && T.pack "module" `T.isInfixOf` l

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

afterLoad :: S -> T.Text -> Bool -> Double -> IO ()
afterLoad s out doCheck t0 = do
  pend <- rd (vPendingSig s)
  sig <- if M.null pend then scan (sRoot s) (gWatch (sCfg s)) (gWatchExt (sCfg s)) else pure pend
  vLoadedSig s =: sig
  now >>= (vLoadedAt s =:)
  modifyIORef' (vGeneration s) (+ 1)
  writeLoadedSources s
  writeAtomicT (sDir s </> "load.log") out
  let (v, detail) = verdictOf out
      warns = T.count (T.pack ": warning:") out
      prefix = "OK" ++ (if warns > 0 then " (" ++ show warns ++ " warning(s))" else "")
  vOkPrefix s =: prefix
  let took = (\t -> ("duration_s", JNum (r2 (t - t0)))) <$> now
  if v /= "OK" then took >>= \d -> setStatus s False v detail [d]
    else if not doCheck || null (gChecks (sCfg s))
      then took >>= \d -> setStatus s False (prefix ++ " -- CHECK SKIPPED (" ++ (if doCheck then "no check configured" else "--no-check") ++ "): this is a COMPILE verdict only") [] [d]
      else void (runCheck s (Just t0) Nothing)

countSub :: String -> String -> Int
countSub needle = go 0
  where go n [] = n :: Int
        go n x@(_ : r) = if needle `isPrefixOf` x then go (n + 1) (drop (length needle) x) else go n r

-- checks: per member, never merged ---------------------------------------------------

data CheckResult = CheckResult { crMember :: String, crKind :: String, crFailing :: [String], crBody :: T.Text, crSecs :: Double }

checkOne :: S -> Check -> IO CheckResult
checkOne s e = do
  cwd' <- rd (vCwd s)
  -- Remember how old the check's log is, so a check that never ran cannot be scored against the PREVIOUS
  -- run's file: a link failure leaves the log untouched and would read as a pass.
  let logp = (cwd' </>) <$> ckLog e
  before <- maybe (pure Nothing) modTime logp
  t0 <- now
  r <- try (cmd s (ckTimeout e) (ckExpr e))
  t1 <- now
  case r of
    Left ex@(ReplTimeout _) -> pure (CheckResult (ckMember e) "TIMEOUT" [show ex] (T.pack (show ex)) (t1 - t0))
    Left ex -> pure (CheckResult (ckMember e) "DEAD" [show ex] (T.pack (show ex)) (t1 - t0))
    Right out -> do
      body <- case logp of
        Nothing -> pure out
        Just p -> do
          after <- modTime p
          if isNothing after || after <= before
            then pure (out <> T.pack ("\n[session] " ++ fromMaybe "" (ckLog e) ++ " was NOT rewritten by this run: the check did not get as far as producing output"))
            else fromMaybe (out <> T.pack ("\n[session] " ++ p ++ " unreadable")) <$> readFileText p
      fails <- maybe (pure []) (\pat -> map (T.unpack . T.strip) <$> linesMatching pat (T.lines body)) (ckFail e)
      passOk <- maybe (pure True) (`anyLineMatches` body) (ckPass e)
      pure $ if not (null fails) then CheckResult (ckMember e) "FAIL" fails body (t1 - t0)
        else if not passOk then CheckResult (ckMember e) "INCOMPLETE" ("the pass marker never appeared (did the check run?)" : map T.unpack (lastN 3 (T.lines (T.strip body)))) body (t1 - t0)
        else CheckResult (ckMember e) "PASS" [] body (t1 - t0)

runCheck :: S -> Maybe Double -> Maybe String -> IO T.Text
runCheck s mt0 member = do
  let entries = [ e | e <- gChecks (sCfg s), maybe True (\m -> m == ckMember e || m == takeWhile (/= ':') (ckMember e)) member ]
  if null entries then pure (T.pack ("no check configured" ++ maybe "" (\m -> " for " ++ show m) member)) else do
    t <- maybe now pure mt0
    prefix <- rd (vOkPrefix s)
    push s (prefix ++ " -- running check") ""
    results <- phase s "check" (mapM (checkOne s) entries)
    vEvaluated s =: True
    now >>= (vCheckedAt s =:)
    writeAtomicT (sDir s </> "run.log") (T.concat [ T.pack ("===== " ++ crMember r ++ " =====\n") <> crBody r <> T.pack "\n" | r <- results ])
    t1 <- now
    let took = r2 (t1 - t)
        bad = filter ((/= "PASS") . crKind) results
        times = intercalate ", " [ crMember r ++ " " ++ printf "%.1fs" (crSecs r) | r <- results ]
        tag = if length results > 1 then " [" ++ show (length results) ++ " members: " ++ times ++ "]" else ""
        facts = [ ("members", JArr [ JObj [ ("member", JStr (crMember r)), ("name", JStr (crMember r)), ("kind", JStr (crKind r))
                                          , ("duration_s", JNum (r2 (crSecs r))), ("failing", JNum (fromIntegral (length (crFailing r))))
                                          , ("detail", JArr (map JStr (take 12 (crFailing r)))) ] | r <- results ])
                , ("duration_s", JNum took) ]
    if any ((== "DEAD") . crKind) results
      then setStatus s False "DEAD: the repl died during a check" (take 20 [ crMember r ++ ": " ++ T.unpack (T.take 2000 (crBody r)) | r <- bad ]) facts
      else if not (null bad)
        then setStatus s False ("CHECK-FAIL: " ++ show (sum [ max 1 (length (crFailing r)) | r <- bad ]) ++ " failing in " ++ intercalate ", " (map crMember bad) ++ tag)
               (take 30 [ crMember r ++ ": " ++ l | r <- bad, l <- (if null (crFailing r) then [crKind r] else crFailing r) ]) facts
        else setStatus s False (prefix ++ " -- CHECK-PASS (" ++ printf "%.1f" took ++ "s)" ++ tag) [] facts
    pure (T.intercalate (T.pack "\n") (map crBody results))

-- memory ---------------------------------------------------------------------------

descendantsOf :: [(Int, Int, Int)] -> Int -> [Int]
descendantsOf rows pid = go [pid] [pid]
  where go seen [] = seen
        go seen (p : todo) = let kids = [ c | (c, pp, _) <- rows, pp == p, c `notElem` seen ] in go (seen ++ kids) (todo ++ kids)

-- | A process and everything under it, in MB (the kernel's own numbers: 'processTable').
treeMb :: [(Int, Int, Int)] -> Int -> Double
treeMb rows pid = let pids = descendantsOf rows pid in fromIntegral (sum [ m | (p, _, m) <- rows, p `elem` pids ]) / 1024

-- | The repl's own memory: its process tree less the servers forked from it.
replMb :: S -> IO Double
replMb s = do
  mp <- rd (vRepl s) >>= maybe (pure Nothing) replPid
  case mp of
    Nothing -> pure 0
    Just pid -> do
      rows <- processTable
      let under = descendantsOf rows pid
      kids <- filter (`elem` under) . catMaybes <$> mapM (serverRunning s) (serverLabels s)
      pure (max 0 (treeMb rows pid - sum (map (treeMb rows) kids)))

serversMb :: S -> IO Double
serversMb s = do
  rows <- processTable
  pids <- catMaybes <$> mapM (serverRunning s) (serverLabels s)
  pure (sum (map (treeMb rows) pids))

-- | Measure the session's memory OFF the reload path, for the log and for the next reload's budget check:
-- asking the OS for a process tree's footprint is 0.15-0.3 s a time.
memSampleAsync :: S -> IO ()
memSampleAsync s = do
  done <- newEmptyMVar
  vMemDone s =: Just done
  void $ forkIO $ (do
    r <- replMb s
    sv <- serversMb s
    vMem s =: Just (r, sv)
    live <- rd (vLiveMb s)
    let line = printf "[mem] repl %.0f MB, live heap %s MB, servers %.0f MB" r live sv
    logS s line
    void (try (appendFileUtf8 (sDir s </> "reload.log") ("\n" ++ line ++ "\n")) :: IO (Either IOException ())))
    `finally` putMVar done ()

-- | The repl's memory as last sampled; 0 if never (no reading is not a reason to stall a reload).
replMbRecent :: S -> IO Double
replMbRecent s = do
  m <- rd (vMem s)
  when (isNothing m) $ rd (vMemDone s) >>= mapM_ (\d -> void (timeoutMVar 1.0 d))
  maybe 0 fst <$> rd (vMem s)

timeoutMVar :: Double -> MVar () -> IO Bool
timeoutMVar secs m = go (round (secs / 0.02) :: Int)
  where go n = do
          r <- tryReadMVar m
          case r of
            Just () -> pure True
            Nothing | n <= 0 -> pure False
                    | otherwise -> threadDelay 20000 >> go (n - 1)

-- the reload leak -----------------------------------------------------------------------
--
-- UNLINKING the superseded CAFs from the RTS's root list makes their values reclaimable, and the major GC
-- that follows returns the memory. The unlink waits for an evaluation: GHCi links a reloaded module into its
-- new library on the first evaluation that needs it, so right after `:reload` the generation just replaced
-- does not yet look superseded. The GC follows AT ONCE, in the same call (`pruneCafs`): deferring it to an
-- idle moment crashed a large session (see the README), so this version does not offer it.

unlinkCafs :: S -> IO ()
unlinkCafs s = do
  ok <- and <$> mapM rd [vHygieneOn s, vUnlinkDue s, vEvaluated s]
  when ok $ do
    vUnlinkDue s =: False
    t0 <- now
    r <- try (cmdS s (Just 300) (gHygieneModule (sCfg s) ++ ".pruneCafs >>= \\k -> GHC.Stats.getRTSStatsEnabled >>= \\e -> "
           ++ "(if e then GHC.Stats.getRTSStats >>= \\s -> return (show (GHC.Stats.gcdetails_live_bytes (GHC.Stats.gc s) `div` 1000000)) else return \"?\") >>= \\l -> "
           ++ "putStrLn (\"unlinked=\" ++ show k ++ \" live_mb=\" ++ l)"))
    t1 <- now
    case r of
      Left (e :: SomeException) -> logS s ("unlink_cafs failed: " ++ displayException e)
      Right out -> do
        let field k = listToMaybe [ takeWhile (not . isSpace) (drop (length k) w) | l <- lines out, w <- tailsW l, k `isPrefixOf` w ]
            tailsW l = [ drop i l | i <- [0 .. length l - 1] ]
        mapM_ (vLiveMb s =:) (field "live_mb=")
        live <- rd (vLiveMb s)
        logS s (printf "unlink_cafs: %s unlinked in %.2fs with its GC, live heap %s MB" (fromMaybe "?" (field "unlinked=")) (t1 - t0) live)

-- | After the verdict is out: link the reloaded code if nothing has yet (the @warm@ expressions), then the
-- unlink and its GC. In the background, under the work lock: a command that arrives first simply runs first.
warmAsync :: S -> IO ()
warmAsync s = void $ forkIO $ withMVar (vWork s) $ \_ -> do
  stopping <- rd (vStopping s)
  due <- rd (vUnlinkDue s)
  alive <- rd (vRepl s) >>= maybe (pure False) replAlive
  when (not stopping && due && alive) $ do
    t0 <- now
    linked <- rd (vEvaluated s)
    r <- try (do unless linked (forM_ (gWarm (sCfg s)) (cmd s (Just 120)) >> (vEvaluated s =: True))
                 unlinkCafs s)
    t1 <- now
    case r of
      Left (e :: SomeException) -> logS s ("warm failed: " ++ displayException e)
      Right () -> logS s (printf "[warm] %.2fs after the verdict%s" (t1 - t0) (if linked then "" else " (linked the reloaded code first)" :: String))

-- servers: forked children of the repl (GHC.Hygiene.Zygote) ---------------------------------

serverLabels :: S -> [String]
serverLabels s = map svMember (gServers (sCfg s))

serverSpec :: S -> String -> Maybe Server
serverSpec s l = listToMaybe [ z | z <- gServers (sCfg s), svMember z == l ]

sfile :: S -> String -> String -> FilePath
sfile s label what = sDir s </> ("server-" ++ label ++ "." ++ what)

readInt :: FilePath -> IO (Maybe Int)
readInt p = (>>= \t -> case reads (trim t) of { [(n, "")] -> Just n; _ -> Nothing }) <$> readFileMaybe p

serverRunning :: S -> String -> IO (Maybe Int)
serverRunning s label = do
  mp <- readInt (sfile s label "pid")
  case mp of
    Nothing -> pure Nothing
    Just pid -> (\a -> if a then Just pid else Nothing) <$> pidAlive pid

zm :: S -> String
zm = gZygoteModule . sCfg

-- | Work the child must INHERIT: it is a fork without an exec, so anything not fork-safe (and anything slow)
-- is done here in the parent. On a re-fork this runs BEFORE the old server is stopped.
serverPrefork :: S -> Server -> IO ()
serverPrefork s z = forM_ (svPrefork z) $ \pre -> do
  t0 <- now
  r <- try (cmd s Nothing pre)
  t1 <- now
  logS s $ case r of
    Left (e :: SomeException) -> "server[" ++ svMember z ++ "]: prefork failed (" ++ displayException e ++ "); forking cold"
    Right _ -> printf "server[%s]: prefork done in %.1fs" (svMember z) (t1 - t0)

-- | 'Nothing' when the server's action typechecks in the loaded code, else GHC's complaint. Asked BEFORE an
-- old server is stopped: a fork that cannot happen must not cost the server that is running.
serverActionOk :: S -> Server -> IO (Maybe String)
serverActionOk s z = do
  on <- rd (vZygoteOn s)
  if not on then pure (Just (zm s ++ " is not in scope in this repl")) else do
    r <- try (cmdS s (Just 120) (":type (" ++ svAction z ++ ") :: IO ()"))
    pure $ case r of
      Left (e :: SomeException) -> Just (displayException e)
      Right out -> if "error" `isInfixOf` out then Just (take 300 (unwords (words out))) else Nothing

-- | Fork one server out of the code the repl holds RIGHT NOW. @handover@: this continues a server that was
-- just stopped (carry its state in); the child never decides that for itself.
serverFork :: S -> Server -> Bool -> Bool -> IO (Maybe Int)
serverFork s z handover preforked = do
  let label = svMember z
  on <- rd (vZygoteOn s)
  if not on then logS s ("server[" ++ label ++ "]: cannot fork, " ++ zm s ++ " is not in scope") >> pure Nothing else do
    let hpath = sfile s label "handover"
        (hOut, hIn) = gHandoverEnv (sCfg s)
    carried <- (handover &&) <$> doesFileExist hpath
    let env' = svEnv z ++ [(hOut, hpath)] ++ [ (hIn, hpath) | carried ]
        -- positional, not record update: GHC rejects a qualified record update on a field it also sees as a selector
        spec = zm s ++ ".zygoteSpec " ++ show label ++ " " ++ show (sfile s label "log") ++ " " ++ show env' ++ " " ++ show (gComposed (sCfg s))
    unless preforked (phase s "prefork" (serverPrefork s z))
    r <- try (phase s "fork" (cmdS s Nothing ("fmap " ++ zm s ++ ".zcPid (" ++ zm s ++ ".zygoteFork (" ++ spec ++ ") (" ++ svAction z ++ "))")))
    case r of
      Left (e :: SomeException) -> logS s ("server[" ++ label ++ "]: fork failed: " ++ displayException e) >> pure Nothing
      Right out ->
        -- a pid is a whole line of digits, never a number lifted out of prose
        case [ read l | l <- map trim (lines out), not (null l), all isDigit l ] of
          [] -> logS s ("server[" ++ label ++ "]: fork produced no pid: " ++ take 300 (unwords (words out))) >> pure Nothing
          (pid : _) -> do
            logS s ("server[" ++ label ++ "]: forked pid " ++ show pid ++ " -> " ++ sfile s label "log")
            -- verified BEFORE the pid file is written: a fork that dies on a busy port must not overwrite
            -- the record of the healthy child it collided with
            ok <- phase s "fork_verify" (serverVerify s z pid)
            if not ok then pure Nothing else do
              writeAtomic (sfile s label "pid") (show pid ++ "\n")
              fp <- phase s "fork_fingerprint" (codeFingerprint s z)
              case fp of
                Just h -> writeAtomic (sfile s label "code") (h ++ "\n")
                Nothing -> rm (sfile s label "code")
              pure (Just pid)

rm :: FilePath -> IO ()
rm p = void (try (removeFile p) :: IO (Either IOException ()))

portListener :: Int -> IO (Maybe Int)
portListener port = do
  out <- rawSystemOut 10 "lsof" ["-nP", "-iTCP:" ++ show port, "-sTCP:LISTEN", "-Fp"]
  pure (listToMaybe [ read ds | ('p' : ds) <- lines (fromMaybe "" out), not (null ds), all isDigit ds ])

logTail :: S -> String -> IO String
logTail s label = maybe "" (take 300 . intercalate " | " . lastN 3 . lines) <$> readFileMaybe (sfile s label "log")

-- | Did the fork produce a SERVER, or just a process? A dead child is a failure; so is the port held by a
-- different process. Alive with the port still free is not: a server may build its state before it listens.
serverVerify :: S -> Server -> Int -> IO Bool
serverVerify s z pid = case svPort z of
  Nothing -> do
    -- no port to watch: give a child that dies at once the moment to do it
    -- (60 ms in three looks: asking is free now, and a child that dies of a bad action does so in a few)
    let wait n = do
          threadDelay 20000
          a <- pidAlive pid
          if a && n > (1 :: Int) then wait (n - 1) else pure a
    alive <- wait 3
    unless alive $ do
      t <- logTail s label
      logS s ("server[" ++ label ++ "]: pid " ++ show pid ++ " died at once -- " ++ t)
      serverStopPid s label pid
    pure alive
  Just port -> do
    t0 <- now
    let loop = do
          t <- now
          alive <- pidAlive pid
          if not alive then do
              tl <- logTail s label
              logS s ("server[" ++ label ++ "]: pid " ++ show pid ++ " DIED before taking port " ++ show port ++ " -- " ++ tl)
              serverStopPid s label pid   -- reap it: GHCi never waits on a child
              pure False
            else do
              holder <- portListener port
              if holder == Just pid then logS s ("server[" ++ label ++ "]: pid " ++ show pid ++ " serving port " ++ show port) >> pure True
                else if isJust holder && t - t0 > 3 then do
                  logS s ("server[" ++ label ++ "]: port " ++ show port ++ " is held by pid " ++ show (fromMaybe 0 holder) ++ ", not our child " ++ show pid ++ " -- stopping it")
                  serverStopPid s label pid
                  pure False
                else if t - t0 > svVerifyTimeout z then do
                  logS s ("server[" ++ label ++ "]: pid " ++ show pid ++ " alive but port " ++ show port ++ " not taken -- leaving it to finish booting")
                  pure True
                else threadDelay 250000 >> loop
    loop
  where label = svMember z

-- | Stop one pid THROUGH the repl, which is what reaps it (GHCi installs no SIGCHLD handling, so a child
-- killed from outside stays a zombie for the life of the session). Signals if the repl cannot take a command.
serverStopPid :: S -> String -> Int -> IO ()
serverStopPid s label pid = do
  on <- rd (vZygoteOn s)
  alive <- rd (vRepl s) >>= maybe (pure False) replAlive
  viaRepl <- if on && alive
    then either (\(_ :: SomeException) -> False) (const True)
           <$> try (cmd s (Just 30) (zm s ++ ".zygoteStop (" ++ zm s ++ ".ZygoteChild " ++ show pid ++ " " ++ show label ++ " " ++ show (sfile s label "log") ++ ") 30"))
    else pure False
  still <- pidAlive pid
  if viaRepl && not still then logS s ("server[" ++ label ++ "]: stopped pid " ++ show pid) else do
    logS s ("server[" ++ label ++ "]: stopping pid " ++ show pid ++ " by signal")
    forM_ [(sigTERM, 30 :: Int), (sigKILL, 20)] $ \(sig, n) -> do
      a <- pidAlive pid
      when a $ do
        void (try (signalProcess sig (CPid (fromIntegral pid))) :: IO (Either IOException ()))
        let wait k = when (k > 0) (pidAlive pid >>= \x -> when x (threadDelay 100000 >> wait (k - 1)))
        wait n

serverStop :: S -> String -> IO ()
serverStop s label = do
  serverRunning s label >>= mapM_ (serverStopPid s label)
  rm (sfile s label "pid")

-- | Live children this session no longer declares: dropping a member does not stop its server, and the child
-- keeps its PORT, so the running set is read from the pid files, not from the member list.
serverOrphans :: S -> IO [(String, Int)]
serverOrphans s = do
  names <- either (\(_ :: IOException) -> []) id <$> try (getDirectoryContents (sDir s))
  let labels = [ drop 7 (take (length n - 4) n) | n <- sort names, "server-" `isPrefixOf` n, ".pid" `isSuffixOf` n ]
  fmap catMaybes $ forM [ l | l <- labels, l `notElem` serverLabels s ] $ \l -> do
    mp <- serverRunning s l
    when (isNothing mp) (rm (sfile s l "pid"))
    pure ((,) l <$> mp)

-- | After a boot: stop what is no longer a member, ADOPT what is still running, start what must always
-- serve. Loading a target is not serving it.
serversBoot :: S -> IO [String]
serversBoot s = do
  orphans <- serverOrphans s
  dropped <- forM orphans $ \(l, pid) -> do
    logS s ("server[" ++ l ++ "]: no longer a member -- stopping pid " ++ show pid)
    serverStop s l
    pure (l ++ ": dropped")
  ok <- compiled s
  started <- fmap catMaybes $ forM (gServers (sCfg s)) $ \z -> do
    mp <- serverRunning s (svMember z)
    case mp of
      Just pid -> logS s ("server[" ++ svMember z ++ "]: adopted running pid " ++ show pid) >> pure (Just (svMember z ++ ": adopted pid " ++ show pid))
      Nothing | svServeOnLoad z && ok -> (\p -> Just (svMember z ++ ": " ++ maybe "FAILED" (("pid " ++) . show) p)) <$> serverFork s z False False
              | otherwise -> pure Nothing
  pure (dropped ++ started)

serversStopAll :: S -> IO ()
serversStopAll s = do
  orphans <- serverOrphans s
  mapM_ (serverStop s) (serverLabels s ++ map fst orphans)

-- Is the running server's CODE still the code? A child needs replacing exactly when the code it would run
-- differs from the code it was forked from: the object files of its units and of every in-session unit they
-- depend on (cabal's per-unit argument files list both), the declared extra files, and its declaration.

data Unit = Unit { uPkg :: String, uMods :: [String], uDeps :: [String], uWd :: FilePath }

unitFiles :: S -> IO (M.Map String Unit)
unitFiles s = do
  let dn = sRoot s </> "dist-newstyle"
  names <- either (\(_ :: IOException) -> []) id <$> try (getDirectoryContents dn)
  dirs <- forM [ dn </> n | n <- names, "multi-out-" `isPrefixOf` n ] (\d -> (,) d <$> modTime d)
  case sortOn snd [ (d, t) | (d, Just t) <- dirs ] of
    [] -> pure M.empty
    ds -> do
      let dir = fst (last ds)
      fs <- either (\(_ :: IOException) -> []) id <$> try (getDirectoryContents dir)
      us <- forM [ dir </> f | f <- fs, f `notElem` [".", ".."] ] $ \f -> do
        isF <- doesFileExist f
        if not isF then pure Nothing else maybe Nothing (parseUnit . map trim . lines) <$> readFileMaybe f
      fmap M.fromList $ forM (catMaybes us) $ \(uid, u, maybeMods) -> do
        -- a capitalised word right after a flag may be that flag's VALUE (`-framework Accelerate`), not a
        -- module: the missing object decides
        extra <- filterM (\m -> doesFileExist (objPath (uWd u </> sObjRel s) m)) maybeMods
        pure (uid, u { uMods = uMods u ++ extra })
  where
    parseUnit args = go args Nothing "" (sRoot s) [] [] [] ""
    go (a : n : r) uid pkg wd deps mods maybeM prev
      | a == "-this-unit-id" = go r (Just n) pkg wd deps mods maybeM n
      | a == "-this-package-name" = go r uid n wd deps mods maybeM n
      | a == "-working-dir" = go r uid pkg n deps mods maybeM n
      | a == "-package-id" = go r uid pkg wd (n : deps) mods maybeM n
    go (a : r) uid pkg wd deps mods maybeM prev
      | isModule a = if "-" `isPrefixOf` prev then go r uid pkg wd deps mods (a : maybeM) a else go r uid pkg wd deps (a : mods) maybeM a
      | otherwise = go r uid pkg wd deps mods maybeM a
    go [] uid pkg wd deps mods maybeM _ = (\u -> (u, Unit pkg (reverse mods) deps wd, reverse maybeM)) <$> uid
    isModule a = not (null a) && all part (splitOn '.' a) && not ("." `isSuffixOf` a) && not ("." `isPrefixOf` a) && not (".." `isInfixOf` a)
    part p = case p of { (c : cs) -> isUpper c && all (\x -> x `elem` ("_'" :: String) || x `elem` ['a' .. 'z'] || x `elem` ['A' .. 'Z'] || isDigit x) cs; [] -> False }

objPath :: FilePath -> String -> FilePath
objPath odir m = foldl (</>) odir (splitOn '.' m) ++ ".o"

-- | A hash of everything a child forked now would run, or 'Nothing' when that cannot be said (then the
-- child is always re-forked).
codeFingerprint :: S -> Server -> IO (Maybe String)
codeFingerprint s z = do
  units <- unitFiles s
  objs <- if M.null units then allObjects else pure (fromUnits units)
  case objs of
    Nothing -> pure Nothing
    Just files -> do
      let step named h p = case h of
            Nothing -> pure Nothing
            Just x -> do
              fh <- fileHash s p
              if fh == 0 then pure Nothing else Just <$> hashString (named p ++ ":" ++ showHash fh) x
      h1 <- foldM (step (rel s)) (Just 1) (sort files)
      h2 <- foldM (step id) h1 (map (sRoot s </>) (gFingerprintFiles (sCfg s)))
      case h2 of
        Nothing -> pure Nothing   -- something we cannot see: make no claim
        Just x -> Just . showHash <$> hashString (svSpec z ++ show (svEnv z)) x
  where
    fromUnits units =
      let want = [ (takeWhile (/= ':') u, drop 1 (dropWhile (/= ':') u)) | u <- svUnits z, ':' `elem` u ]
          rootsOf (kind, comp) = [ uid | (uid, u) <- M.toList units
                                       , (kind == "lib" && uPkg u == comp && "-inplace" `isSuffixOf` uid) || (kind /= "lib" && ("-inplace-" ++ comp) `isSuffixOf` uid) ]
          roots = map rootsOf want
          close seen [] = seen
          close seen (u : todo) | u `elem` seen = close seen todo
                                | otherwise = close (u : seen) (todo ++ maybe [] (filter (`M.member` units) . uDeps) (M.lookup u units))
      in if null want || any null roots then Nothing
         else Just (nub [ objPath (uWd u </> sObjRel s) m | uid <- close [] (concat roots), Just u <- [M.lookup uid units], m <- uMods u ])
    -- without cabal's per-unit files (a single-unit repl has none): every object this session compiled.
    -- Over-inclusive, which is the safe side -- it can only re-fork more often, never less.
    allObjects = do
      fs <- findObjs (sRoot s)
      pure (if null fs then Nothing else Just fs)
    findObjs dir = do
      names <- either (\(_ :: IOException) -> []) id <$> try (getDirectoryContents dir)
      fmap concat $ forM [ n | n <- names, n `notElem` [".", "..", "dist-newstyle", ".git"] ] $ \n -> do
        let p = dir </> n
        isD <- doesDirectoryExist p
        if isD then findObjs p
          else pure [ p | ".o" `isSuffixOf` n, ("/" ++ sObjRel s ++ "/") `isInfixOf` p ]

-- | A file's content hash, 0 if it cannot be read. Remembered while the file's size and modification time
-- stand: a reload recompiles a module or two, and the other hundred objects need a stat, not a read.
fileHash :: S -> FilePath -> IO Word64
fileHash s p = do
  st <- try (getFileStatus p) :: IO (Either IOException FileStatus)
  case st of
    Left _ -> pure 0
    Right f -> do
      let key = (fromIntegral (fileSize f), realToFrac (modificationTimeHiRes f))
      cache <- rd (vHashes s)
      case M.lookup p cache of
        Just (sz, mt, h) | (sz, mt) == key -> pure h
        _ -> do
          h <- hashFile p 1
          when (h /= 0) (modifyIORef' (vHashes s) (M.insert p (fst key, snd key, h)))
          pure h

codeUnchanged :: S -> String -> IO Bool
codeUnchanged s label = case serverSpec s label of
  Nothing -> pure False
  Just z -> do
    was <- fmap trim <$> readFileMaybe (sfile s label "code")
    cur <- codeFingerprint s z
    pure (isJust was && was /= Just "" && was == cur)

pendingNote :: String
pendingNote = "  [servers: re-fork running in the background -- the old server serves until the new one is up]"

-- | Bring the servers that were running onto the code now loaded: keep those whose code did not change, stop
-- and fork the rest. Prefork first, with the old servers still serving.
refork :: S -> [String] -> IO ()
refork s wasRunning0 = do
  owed <- rd (vOwed s)
  vOwed s =: []
  let wasRunning = nub (wasRunning0 ++ owed)
  unless (null wasRunning) $ do
    kept <- phase s "fingerprint" (filterM (\l -> (&&) <$> (isJust <$> serverRunning s l) <*> codeUnchanged s l) wasRunning)
    let todo = [ z | z <- gServers (sCfg s), svMember z `elem` wasRunning, svMember z `notElem` kept ]
    broken <- fmap catMaybes $ forM todo $ \z -> do
      why <- phase s "typecheck_action" (serverActionOk s z)
      case why of
        Just w -> logS s ("server[" ++ svMember z ++ "]: action does not typecheck, not re-forked: " ++ w) >> pure (Just (svMember z))
        Nothing -> phase s "prefork" (serverPrefork s z) >> pure Nothing
    res <- fmap catMaybes $ forM todo $ \z -> do
      let label = svMember z
      if label `elem` broken
        then do r <- serverRunning s label; when (isNothing r) (modifyIORef' (vOwed s) (label :)); pure Nothing
        else do
          carried <- isJust <$> serverRunning s label
          phase s "server_stop" (serverStop s label)   -- it writes its state on the way out
          pid <- serverFork s z carried True
          st <- (carried &&) <$> doesFileExist (sfile s label "handover")
          pure (Just (label, pid, st))
    keptPids <- forM (sort kept) (\l -> do p <- serverRunning s l; logS s ("server[" ++ l ++ "]: code unchanged -- kept pid " ++ maybe "?" show p); pure (l, p))
    stillOld <- filterM (fmap isJust . serverRunning s) broken
    let okS = [ l ++ ":" ++ show p ++ (if st then "+state" else "") | (l, Just p, st) <- res ]
        bad = [ l | (l, Nothing, _) <- res ]
        parts = [ "re-forked " ++ show (length okS) ++ " server(s) (" ++ intercalate ", " okS ++ "), now running the NEW code" | not (null okS) ]
             ++ [ "kept " ++ show (length kept) ++ " (" ++ intercalate ", " [ l ++ ":" ++ maybe "?" show p | (l, p) <- keptPids ] ++ "): their code did not change" | not (null kept) ]
             ++ [ "RE-FORK FAILED for " ++ intercalate ", " bad ++ " -- not running" | not (null bad) ]
             ++ [ "NOT re-forked: " ++ intercalate ", " broken ++ " (the action no longer typechecks: see daemon.log)"
                  ++ (if null stillOld then "" else "; " ++ intercalate ", " stillOld ++ " still on the OLD code") | not (null broken) ]
        fact l a p = JObj [ ("member", JStr l), ("action", JStr a), ("pid", maybe JNull (JNum . fromIntegral) p) ]
    brokenPids <- mapM (serverRunning s) broken
    modifyIORef' (vStatus s) (replace pendingNote "")
    note s ("[servers: " ++ intercalate "; " parts ++ "]")
      [ ("servers", JArr ([ fact l (if isJust p then "re-forked" else "failed") p | (l, p, _) <- res ]
                          ++ [ fact l "kept" p | (l, p) <- keptPids ] ++ [ fact l "broken" p | (l, p) <- zip broken brokenPids ]))
      , ("servers_pending", JBool False) ]

replace :: String -> String -> String -> String
replace old new = go
  where go [] = []
        go x@(c : r) = if old `isPrefixOf` x then new ++ go (drop (length old) x) else c : go r

-- | Wait for a background re-fork: anything that touches the servers or reloads must not overlap one.
reforkJoin :: S -> IO ()
reforkJoin s = rd (vRefork s) >>= mapM_ (\d -> do
  done <- isJust <$> tryReadMVar d
  unless done (logS s "waiting for the background re-fork" >> readMVar d))

serverOp :: S -> String -> Maybe String -> Bool -> IO String
serverOp s action member resume = do
  let specs = [ z | z <- gServers (sCfg s), maybe True (== svMember z) member ]
  if null specs then pure (maybe "this session declares no server" (\m -> "no server " ++ show m ++ " in this session") member) else
    fmap (intercalate "\n") $ forM specs $ \z -> do
      let label = svMember z
      running <- serverRunning s label
      case action of
        "stop" -> phase s "server_stop" (serverStop s label) >> pure (label ++ ": stopped")
        a | a `elem` ["start", "restart"] ->
          if a == "start" && isJust running then pure (label ++ ": already running pid " ++ maybe "" show running) else do
            why <- phase s "typecheck_action" (serverActionOk s z)
            case why of
              Just w -> pure (label ++ ": FAILED, the action does not typecheck (" ++ w ++ ")" ++ (if isJust running then "; the running server was left alone" else ""))
              Nothing -> do
                -- a restart is a cut-over and carries the state the old child just wrote; a start brings up
                -- something that was NOT running, for an unknown time, so it is cold unless asked (--resume)
                let carry = resume || (a == "restart" && isJust running)
                phase s "server_stop" (serverStop s label)
                pid <- serverFork s z carry False
                took <- (carry &&) <$> doesFileExist (sfile s label "handover")
                pure (label ++ ": " ++ maybe "FAILED (see daemon.log)" (("pid " ++) . show) pid ++ (if isJust pid && took then "+state" else ""))
        _ -> do
          cur <- if isJust running then codeUnchanged s label else pure False
          pure (label ++ ": " ++ maybe "not running" (("running pid " ++) . show) running ++ maybe "" ((" port " ++) . show) (svPort z)
                ++ (if isNothing running then "" else if cur then " (current code)" else " (code differs from what is loaded, or unknown)"))

-- lifecycle --------------------------------------------------------------------------------

boot :: S -> IO ()
boot s = do
  let cfg = sCfg s
  when (gHygiene cfg && any (`isInfixOf` gRtsFlags cfg) ["-xn", "--nonmoving-gc"]) $ do
    -- the pruner edits RTS lists the non-moving collector reads concurrently: the repl died at the first unlink
    setStatus s False "CONFIG-ERROR: hygiene cannot be used with the non-moving collector (rts_flags)" [] []
    throwIO (userError "hygiene with the non-moving GC")
  when (gHygiene cfg && gHygieneBuild cfg) (phase s "hygiene_build" (buildHygiene s))
  forM_ (gPrebuild cfg) $ \pb -> do
    t0 <- now
    (code, o, e) <- readCreateProcessWithExitCode (shell pb) { cwd = Just (sRoot s) } ""
    t1 <- now
    writeAtomic (sDir s </> "prebuild.log") (o ++ e)
    modifyIORef' (vPhases s) (M.insert "prebuild" (t1 - t0))
    logS s (printf "prebuild: exit %s in %.1fs" (show code) (t1 - t0))
    when (code /= ExitSuccess) $ do
      setStatus s False "PREBUILD-ERROR: see prebuild.log" (lastN 8 (lines (trim (o ++ e)))) []
      throwIO (userError "prebuild failed")
  let env' = gEnv cfg ++ [ ("GHS_RTS_FLAGS", gRtsFlags cfg) | gRtsFlags cfg `notElem` ["", "none"], isNothing (lookup "GHS_RTS_FLAGS" (gEnv cfg)) ]
               ++ [ ("GHS_DIR", cStateDir (sConf s) </> "clib") | isNothing (lookup "GHS_DIR" (gEnv cfg)) ] ++ [("GHCI_SESSION", sName s)]
  hold <- rd (vHold s)
  vHold s =: 0
  setStatus s False "starting" [] []       -- always visible at once: `start` waits on it
  vHold s =: hold
  t0 <- now
  vSpawned s =: t0
  mapM_ (vEvaluated s =:) [False]
  vUnlinkDue s =: False
  scan (sRoot s) (gWatch cfg) (gWatchExt cfg) >>= (vPendingSig s =:)
  line <- replCommandLine s
  r <- try (startRepl line (sRoot s) env' (gLoadTimeout cfg) (gEvalTimeout cfg) (logS s)
              (\t -> void (try (B.appendFile (sDir s </> "async.log") (TE.encodeUtf8 t)) :: IO (Either IOException ())))
              (\r -> do
                 -- called once GHCi has answered: everything before this instant was cabal and the load
                 t <- now
                 modifyIORef' (vPhases s) (M.insert "load" (t - t0))
                 phase s "post_load" (postLoad s r)))
  case r of
    Left (e :: ReplError) -> do
      setStatus s False ("DEAD: " ++ takeWhile (/= '\n') (show e)) (lastN 8 (lines (show e))) []
      throwIO e
    Right (repl, out) -> do
      vRepl s =: Just repl
      -- `cabal repl` may chdir into the package: a check that writes a relative path lands there. GHCi's own
      -- answer, no shell: "current working directory:\n  /path"
      shown <- either (\(_ :: SomeException) -> []) (lines . T.unpack) <$> try (replCommand repl (Just 60) ":show paths")
      vCwd s =: fromMaybe (sRoot s) (listToMaybe [ trim b | (a, b) <- zip shown (drop 1 shown), "current working directory" `isInfixOf` a, not (null (trim b)) ])
      vContextOk s =: True
      afterLoad s out (sBootCheck s) t0

-- | A fresh repl. The servers that were running come back on the new code (a plain session's children die
-- with its repl; a composed session's are kept if their code did not change).
restart :: S -> IO String
restart s = do
  reforkJoin s
  t0 <- now
  oneVerdict s $ do
    logS s "restart"
    was <- filterM (fmap isJust . serverRunning s) (serverLabels s)
    phase s "repl_stop" (rd (vRepl s) >>= mapM_ (\r -> stopRepl r (logS s)))
    vRepl s =: Nothing
    boot s
    unless (null (gServers (sCfg s))) $ do
      ok <- compiled s
      if ok then refork s was else modifyIORef' (vOwed s) (nub . (was ++))
    phasesDone s "restart" t0
  vMem s =: Nothing
  memSampleAsync s
  rd (vStatus s)

reload :: S -> Bool -> Bool -> Maybe Bool -> IO T.Text
reload s doCheck doRefork asyncReq = do
  reforkJoin s
  envAsync <- (== Just "1") <$> lookupEnv "GHS_ASYNC_REFORK"
  let async = fromMaybe (gAsyncRefork (sCfg s) || envAsync) asyncReq
  vReforkPending s =: Nothing
  t0 <- now
  vPhases s =: M.empty
  out <- oneVerdict s $ do
    o <- reload' s doCheck doRefork async
    phasesDone s "reload" t0
    pure o
  due <- rd (vUnlinkDue s)
  ok <- compiled s
  evaluated <- rd (vEvaluated s)
  -- The verdict does not depend on the unlink or its GC, so they run once it is published. After a check the
  -- reloaded code is already linked; after a compile-only reload the `warm` expressions link it first.
  when (due && ok && (evaluated || not (null (gWarm (sCfg s))))) (warmAsync s)
  rd (vReforkPending s) >>= mapM_ (\go -> do      -- only now: the verdict it amends is on disk
    done <- newEmptyMVar
    vRefork s =: Just done
    void (forkIO (go `finally` putMVar done ())))
  vReforkPending s =: Nothing
  memSampleAsync s
  pure out

reload' :: S -> Bool -> Bool -> Bool -> IO T.Text
reload' s doCheck doRefork async = do
  let cfg = sCfg s
  envBudget <- (>>= \v -> case reads v of { [(b, "")] -> Just b; _ -> Nothing }) <$> lookupEnv "GHS_REPL_BUDGET_MB"
  let budget = fromMaybe (gBudgetMb cfg) envBudget
  rss <- phase s "budget_mem" (if budget > 0 then replMbRecent s else pure 0)
  if budget > 0 && rss > budget
    then do
      logS s (printf "reload: repl at %.0f MB > budget %.0f MB -- restarting instead" rss budget)
      out <- T.pack <$> restart s
      note s (printf "[repl RESTARTED instead of reloaded: it had grown to %.0f MB, over the %.0f MB budget; now restarted]" rss budget) []
      pure out
    else do
      t0 <- now
      phase s "scan" (scan (sRoot s) (gWatch cfg) (gWatchExt cfg) >>= (vPendingSig s =:))
      push s "reloading" ""
      r <- try (phase s "ghci_reload" (cmd s (Just (gLoadTimeout cfg)) ":reload"))
      case r of
        Left (e :: ReplError) -> setStatus s False ("DEAD: " ++ takeWhile (/= '\n') (show e)) [] [] >> pure (T.pack (show e))
        Right out -> do
          writeAtomicT (sDir s </> "reload.log") out
          -- A reload that succeeds keeps GHCi's context (imports, prompt, buffering); one that fails drops
          -- the imports. So they are re-issued only after a failure.
          let failedNow = fst (verdictOf out) /= "OK"
          ctx <- rd (vContextOk s)
          when (not failedNow && not ctx) $
            void (try (phase s "post_load" (theRepl s >>= postLoad s)) :: IO (Either SomeException ()))
          vContextOk s =: not failedNow
          vUnlinkDue s =: True
          vEvaluated s =: (gUnlinkAfter cfg == "reload")
          phase s "unlink" (unlinkCafs s)   -- "reload": now, which reaches the generation BEFORE the one just replaced
          afterLoad s out doCheck t0
          running <- phase s "servers_running" (filterM (fmap isJust . serverRunning s) (serverLabels s))
          owed <- rd (vOwed s)
          when (not (null running) || not (null owed)) $ do
            -- A reload updates the code the repl HOLDS, not the code a running child IS.
            ok <- compiled s
            if not ok then note s "[servers: NOT re-forked (compile error) -- they still run the OLD code]" []
              else if not doRefork then note s "[servers: NOT re-forked -- they still run the OLD code; `reload` cuts them over]" []
              else if async then do
                st <- rd (vStatus s)
                j <- rd (vJson s)
                setStatus s True (st ++ pendingNote) (strs (j .: "detail")) [("servers_pending", JBool True)]
                vReforkPending s =: Just (void (try (refork s running) :: IO (Either SomeException ())))
              else refork s running
          pure out

-- | The checkout's HEAD commit, "" when it cannot be said. Read from the files (@.git@ may itself be a file
-- naming the real directory, in a worktree): it is asked every two seconds, and spawning @git@ for it was 10 ms.
headCommit :: S -> IO String
headCommit s = do
  dotGit <- readFileMaybe (sRoot s </> ".git")
  let gitDir = case dotGit of
        Just t | "gitdir: " `isPrefixOf` t -> let d = trim (drop 8 t) in if "/" `isPrefixOf` d then d else sRoot s </> d
        _ -> sRoot s </> ".git"
  h <- fmap trim <$> readFileMaybe (gitDir </> "HEAD")
  case h of
    Just r | "ref: " `isPrefixOf` r -> do
      let ref = drop 5 r
      common <- maybe gitDir (\c -> let d = trim c in if "/" `isPrefixOf` d then d else gitDir </> d) <$> readFileMaybe (gitDir </> "commondir")
      direct <- firstJust [readFileMaybe (gitDir </> ref), readFileMaybe (common </> ref)]
      case direct of
        Just c -> pure (trim c)
        Nothing -> maybe "" (\t -> fromMaybe "" (listToMaybe [ w | l <- lines t, [w, n] <- [words l], n == ref ])) <$> readFileMaybe (common </> "packed-refs")
    Just c -> pure c
    Nothing -> pure ""
  where firstJust [] = pure Nothing
        firstJust (a : as) = a >>= maybe (firstJust as) (pure . Just)

-- | What an eviction decision needs, answered without the repl: so it works while a reload holds it.
info :: S -> IO Json
info s = do
  running <- filterM (fmap isJust . serverRunning s) (serverLabels s)
  t <- now
  lu <- rd (vLastUsed s)
  busy <- rd (vBusy s)
  rf <- rd (vRefork s) >>= maybe (pure False) (fmap isNothing . tryReadMVar)
  r <- replMb s
  sv <- serversMb s
  st <- rd (vStatus s)
  pure (JObj [ ("session", JStr (sName s)), ("idle_s", JNum (r2 (t - lu))), ("busy", JBool (busy > 0 || rf))
             , ("repl_mb", JNum (fromIntegral (round r :: Int))), ("servers_mb", JNum (fromIntegral (round sv :: Int)))
             , ("serving", JArr (map JStr running)), ("verdict", JStr st) ])

-- | This session's own rule (@idle_stop_mins@): unused that long, not busy, and serving nothing -- a server
-- is in use by whoever is connected to it, which this daemon cannot see.
idleStopDue :: S -> IO Bool
idleStopDue s = do
  let mins = gIdleStopMins (sCfg s)
  busy <- rd (vBusy s)
  t <- now
  lu <- rd (vLastUsed s)
  if mins <= 0 || busy > 0 || t - lu < mins * 60 then pure False
    else not . any isJust <$> mapM (serverRunning s) (serverLabels s)

-- | Run a reload or restart from the watcher: under the same lock a client's command takes.
drive :: S -> IO () -> IO ()
drive s act = withMVar (vWork s) $ \_ -> do
  stopping <- rd (vStopping s)
  unless stopping $
    bracket_ (modifyIORef' (vBusy s) (+ 1)) (modifyIORef' (vBusy s) (subtract 1) >> now >>= (vLastUsed s =:)) $ do
      r <- try act
      case r of
        Left (e :: SomeException) -> logS s ("watch: " ++ displayException e)
        Right () -> pure ()

-- | Reload when a watched source changes. A waiter says WHEN to look; the scan says WHAT changed, and runs
-- every couple of seconds regardless, because a waiter may miss an event.
watchLoop :: S -> IO ()
watchLoop s = do
  let cfg = sCfg s
      doScan = scan (sRoot s) (gWatch cfg) (gWatchExt cfg)
  first <- doScan
  roots <- filterM doesDirectoryExist [ sRoot s </> d | d <- gWatch cfg ]
  w <- makeWaiter (gWatcher cfg) (gPollInterval cfg) (gDebounce cfg) (M.keys first ++ map toRaw roots) (logS s)
  kind <- waiterKind w
  logS s ("watch: " ++ show (M.size first) ++ " sources by " ++ kind)
  head0 <- if gReloadOnCommit cfg then headCommit s else pure ""
  t0 <- now
  let loop lastSig headC slow = do
        stopping <- rd (vStopping s)
        unless stopping $ do
          fired <- waiterWait w 0.5
          t <- now
          let due = t - slow >= 2.0
          if not fired && not due then loop lastSig headC slow else do
            (lastSig1, headC1) <- if due && gReloadOnCommit cfg
              then do
                h <- headCommit s
                if not (null h) && not (null headC) && h /= headC
                  then do
                    -- a commit is when everything catches up, whatever a save does: checks and servers too
                    logS s ("commit " ++ take 10 h ++ ": full reload (check, re-fork)")
                    drive s (void (reload s True True Nothing))
                    sg <- doScan
                    pure (sg, h)
                  else pure (lastSig, if null h then headC else h)
              else pure (lastSig, headC)
            let slow1 = if due then t else slow
            cur <- doScan
            if cur == lastSig1
              then do
                idle <- if due then idleStopDue s else pure False
                when idle $ do
                  logS s ("idle for " ++ show (gIdleStopMins cfg) ++ " min -- stopping (idle_stop_mins)")
                  vStopReason s =: (printf "stopped: idle for %s min (idle_stop_mins); `ghci-session start %s`" (showG (gIdleStopMins cfg)) (sName s))
                  vStopping s =: True
                loop lastSig1 headC1 slow1
              else do
                now >>= (vLastUsed s =:)      -- someone is editing
                waiterSettle w                -- an editor's save is several writes
                cur2 <- doScan
                ok <- waiterUpdate w (M.keys cur2 ++ map toRaw roots)   -- new files, and files saved by rename (a new inode)
                unless ok (logS s "watch: cannot watch these paths with kernel events -- polling from here" >> toPolling w)
                loaded <- rd (vLoadedSig s)
                when (gAutoReload cfg && cur2 /= loaded) $ do
                  let changed = [ fromRaw p | p <- M.keys (M.union cur2 loaded), M.lookup p cur2 /= M.lookup p loaded ]
                  if any (\p -> any (`isSuffixOf` p) [".c", ".h", ".cabal"] || "cabal.project" `isPrefixOf` takeFileName p) changed
                    then do
                      logS s "a .c/.h/.cabal changed: restarting the repl (a loaded C object, or a package set, cannot be replaced)"
                      drive s (void (restart s))
                    else do
                      logS s ("watch: " ++ show (length changed) ++ " file(s) changed -- reload")
                      drive s (void (reload s (gWatchCheck cfg) (gWatchRefork cfg) Nothing))
                loop cur2 headC1 slow1
  loop first head0 t0 `finally` waiterClose w

showG :: Double -> String
showG x = if x == fromIntegral (round x :: Integer) then show (round x :: Integer) else show x

-- the socket ---------------------------------------------------------------------------------

serve :: S -> IO ()
serve s = do
  sp <- sockPath (sDir s)
  let link = sDir s </> "sock"
  rm link
  void (try (createFileLink sp link) :: IO (Either IOException ()))
  mfd <- unixListen sp
  case mfd of
    Nothing -> logS s ("cannot listen on " ++ sp)
    Just fd -> do
      let loop = do
            stopping <- rd (vStopping s)
            unless stopping $ do
              mc <- unixAccept fd 500
              forM_ mc $ \c -> forkIO $ do
                h <- fdToHandle c
                hSetBinaryMode h True
                void (try (handle s h) :: IO (Either SomeException ()))
                void (try (hClose h) :: IO (Either IOException ()))
              loop
      loop
      rm sp
      rm link

handle :: S -> Handle -> IO ()
handle s h = do
  line <- B.hGetLine h
  let reply ok out = do
        stale <- staleFiles s
        j <- rd (vJson s)
        -- bytes straight to the socket: an evaluation's output can be megabytes
        B.hPut h (encodeBS (JObj [ ("ok", JBool ok), ("out", JText out), ("stale", JArr (map JStr (take 6 stale))), ("status", j) ]))
        B.hPut h (BC.pack "\n")
        hFlush h
      replyS ok = reply ok . T.pack
  case parseJsonBS line of
    Left e -> replyS False e
    Right req -> do
      let op = fromMaybe "" (lookupStr "op" req)
      r <- try $ case op of
        "status" -> do       -- reads only the last verdict: must answer while a reload holds the repl
          stale <- staleFiles s
          st <- rd (vStatus s)
          replyS True ((if null stale then "" else "STALE(" ++ show (length stale) ++ ") ") ++ st)
        "info" -> info s >>= replyS True . encode
        "stop" -> do
          forM_ (lookupStr "reason" req) (vStopReason s =:)
          vKeepServers s =: fromMaybe False (lookupBool "keep_servers" req)
          vStopping s =: True
          replyS True "stopping"
        _ -> do
          now >>= (vLastUsed s =:)
          out <- bracket_ (modifyIORef' (vBusy s) (+ 1)) (modifyIORef' (vBusy s) (subtract 1) >> now >>= (vLastUsed s =:)) (dispatch s op req)
          maybe (replyS False ("unknown op " ++ show op)) (reply True) out
      case r of
        Left (e :: SomeException) -> void (try (replyS False (displayException e)) :: IO (Either SomeException ()))   -- a broken eval must not kill the daemon
        Right () -> pure ()

dispatch :: S -> String -> Json -> IO (Maybe T.Text)
dispatch s op req = withMVar (vWork s) $ \_ -> case op of    -- eval is inside the lock too: its answer must not straddle a reload
  "eval" -> do
    out <- cmd s (lookupNum "timeout" req >>= \t -> if t > 0 then Just t else Nothing) (fromMaybe "" (lookupStr "expr" req))
    vEvaluated s =: True
    unlinkCafs s
    pure (Just out)
  "reload" -> Just <$> reload s (fromMaybe True (lookupBool "check" req)) (fromMaybe True (lookupBool "refork" req)) (lookupBool "async_refork" req)
  "check" -> do
    out <- runCheck s Nothing (lookupStr "member" req)
    unlinkCafs s
    pure (Just out)
  "restart" -> Just . T.pack <$> restart s
  _ | op `elem` ["server", "zygote"] -> do     -- "zygote" with fork/refork: the names an older client of this protocol used
    let action = case fromMaybe "status" (lookupStr "action" req) of { "fork" -> "start"; "refork" -> "restart"; a -> a }
    reforkJoin s
    t0 <- now
    vPhases s =: M.empty
    out <- serverOp s action (lookupStr "member" req) (fromMaybe False (lookupBool "resume" req))
    phasesDone s ("server " ++ action) t0
    publish s
    pure (Just (T.pack out))
  "mem" -> do
    r <- replMb s
    sv <- serversMb s
    vMem s =: Just (r, sv)      -- a fresh reading: the next reload's budget check uses it
    pure (Just (T.pack (printf "repl %.0f MB (budget %.0f), servers %.0f MB" r (gBudgetMb (sCfg s)) sv)))
  _ -> pure Nothing

-- | Run the daemon for one session until it is told to stop (or idles out).
runDaemon :: Conf -> String -> Bool -> IO ()
runDaemon conf name bootCheck = do
  cfg0 <- resolve conf name >>= either (throwIO . userError) pure
  let root = cRoot conf
      dir = cStateDir conf </> name
  createDirectoryIfMissing True dir
  -- build files at the root are watched too: a changed .cabal means a new package set, which a reload cannot adopt
  rootFiles <- sort . filter (\f -> ".cabal" `isSuffixOf` f || "cabal.project" `isPrefixOf` f) <$> getDirectoryContents root
  let cfg = cfg0 { gWatch = gWatch cfg0 ++ [ f | f <- rootFiles, f `notElem` gWatch cfg0 ] }
  dd <- dataDir
  t <- now
  s <- S conf name cfg root dir (cStateRel conf </> name </> "obj") dd bootCheck
         <$> newIORef Nothing <*> newIORef "starting" <*> newIORef (JObj []) <*> newIORef ""
         <*> newIORef M.empty <*> newIORef M.empty <*> newIORef 0 <*> newIORef 0
         <*> newIORef False <*> newIORef False <*> newIORef "stopped" <*> newIORef []
         <*> newMVar ()
         <*> newIORef (gHygiene cfg) <*> newIORef (not (null (gServers cfg))) <*> newIORef False
         <*> newIORef False <*> newIORef False <*> newIORef True
         <*> newIORef 0 <*> newIORef root <*> newIORef t <*> newIORef 0
         <*> newIORef Nothing <*> newIORef Nothing <*> newIORef M.empty <*> newIORef 0
         <*> newIORef "OK" <*> newIORef "?" <*> newIORef Nothing <*> newIORef Nothing
         <*> newIORef t <*> newIORef M.empty
  pid <- getProcessID
  writeAtomic (dir </> "pid") (show pid)
  t0 <- now
  r <- try $ oneVerdict s $ do
    boot s
    started <- phase s "servers_boot" (serversBoot s)
    unless (null started) (note s ("[servers: " ++ intercalate "; " started ++ "]") [])
    phasesDone s "boot" t0
  case r of
    Left (e :: SomeException) -> do
      logS s ("boot failed: " ++ displayException e)
      st <- rd (vStatus s)
      when ("starting" `isPrefixOf` st) $ do      -- a failure that named itself keeps its name
        vHold s =: 0
        setStatus s False ("DEAD: boot failed: " ++ takeWhile (/= '\n') (displayException e)) [] []
      rd (vRepl s) >>= mapM_ (\rp -> stopRepl rp (logS s))
      rm (dir </> "pid")
    Right () -> do
      vMem s =: Nothing
      memSampleAsync s
      now >>= (vLastUsed s =:)     -- idle is counted from the end of the boot, not from the daemon's start
      void (forkIO (void (try (watchLoop s) :: IO (Either SomeException ()))))
      serve s `finally` do
        reforkJoin s
        keep <- rd (vKeepServers s)
        unless keep (serversStopAll s)
        rd (vRepl s) >>= mapM_ (\rp -> stopRepl rp (logS s))
        rd (vStopReason s) >>= \why -> setStatus s False why [] []
        rm (dir </> "pid")
