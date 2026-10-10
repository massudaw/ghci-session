{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
-- 'sCfg' reads a variable through unsafePerformIO: a use must read it when it runs, not once for a loop it
-- was floated out of. (NOT with -fno-cse as well: the two together give a daemon that dies with a bus error
-- as soon as it has booted, GHC 9.14.1 -- each alone is fine.)
{-# OPTIONS_GHC -fno-full-laziness #-}

-- | The per-session daemon: owns one repl, serves reload/eval/check/status on a unix socket, watches the
-- sources, forks the session's servers.
module GhciSession.Daemon (runDaemon, agentTools, scopedSummary, verdictOf, warningsIn, countSub, replace, packageSources, moduleDelta, unitsBelow, hangLimit, ccWords, cabalField) where

import Control.Concurrent (forkIO, threadDelay)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically, check, orElse, readTVar, registerDelay)
import qualified Data.Sequence as Seq
import Control.Exception (IOException, SomeException, bracket_, displayException, finally, throwIO, try)
import Control.Applicative ((<|>))
import Control.Monad (filterM, foldM, forM, forM_, unless, void, when)
import Data.Char (isAlphaNum, isDigit, isSpace, isUpper, toLower)
import Text.Read (readMaybe)
import Data.IORef
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, nubBy, partition, sort, sortOn, (\\))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Time (defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime, getZonedTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (addTrailingPathSeparator, isAbsolute, makeRelative, normalise, replaceExtension, takeDirectory, takeExtension, takeFileName, (</>))
import System.IO
import System.Info (os)
import Data.Word (Word64)
import System.Posix.Files (FileStatus, fileSize, getFileStatus, modificationTimeHiRes)
import System.Posix.IO (closeFd, fdToHandle)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Posix.Types (CPid (..))
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readCreateProcessWithExitCode, shell, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Text.Printf (printf)

import GhciSession.Config
import GhciSession.Doc
import GhciSession.Json
import GhciSession.Repl
import GhciSession.Sys
import GhciSession.Watch
import qualified GhciSession.History as H
import qualified GhciSession.Know as K
import qualified GhciSession.Mcp as Mcp
import qualified GhciSession.Chat as Chat
import qualified GhciSession.Llm as Llm

data S = S
  { sConf :: Conf, sName :: String, sCfgV :: IORef Cfg, sRoot :: FilePath, sDir :: FilePath, sObjRel :: FilePath
  , sBootCheck :: Bool, sFastStart :: Bool
  , sHist :: Maybe H.Mem           -- ^ the history: every request and verdict, the summary tree, the view ("GhciSession.History")
  , vRepl :: IORef (Maybe Repl)
  , vStatus :: IORef String, vJson :: IORef Json, vStatusText :: IORef String
  , vLoadedSig :: IORef Sig, vPendingSig :: IORef Sig
  , vLoadedAt :: IORef Double, vCheckedAt :: IORef Double
  , vStopping :: IORef Bool, vKeepServers :: IORef Bool, vStopReason :: IORef String
  , vOwed :: IORef [String]
  , vWork :: MVar ()                 -- ^ the watcher and a client both drive the repl; never at once
  , vHygieneOn :: IORef Bool
  , vLoaded :: IORef Json            -- ^ the engine's account of what is loaded (its @state@), as of the last load
  , vDiags :: IORef [Json]           -- ^ the compiler's diagnostics from the last load
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
  , vDocs :: IORef (M.Map B.ByteString (Double, [Entry]))   -- ^ each source's declarations, while its time stands
  , vLoadedOk :: IORef Bool          -- ^ did the last LOAD succeed (a save's type error is reported without one)
  , vBoot :: IORef (Maybe Boot)       -- ^ how the engine now running was started
  , vWatchGen :: IORef Int            -- ^ which watcher is the one: a watcher of an earlier number ends ('addMembers' starts another, on the new sources)
  , vTcLast :: IORef (Maybe (Sig, (Bool, String, Double, [String], [Json])))   -- ^ the sources the last typecheck saw, and its answer
  , vLastLoad :: IORef (Maybe Reply)   -- ^ what the engine said of the last reload (the answer again, while no source has changed)
  , vLinksSeen :: IORef Int, vLinksPruned :: IORef Int   -- ^ libraries the engine has linked: now, and at the last unlink
  , vCheckSecs :: IORef (M.Map String [Double])   -- ^ per member, the seconds its last passing checks took: what a hang is measured against
  , vChecking :: IORef (Maybe (Double, Double, String))   -- ^ a check running now: when its reload began, when the check did, the compile verdict
  , vBatch :: IORef (Maybe Double)   -- ^ a batch of edits is being written ('hold'): until when the watcher leaves the sources alone; 'release' reloads once
  }

-- | How the running engine was started: what the build tool was asked, and what it answered.
data Boot = Boot { bExe :: FilePath, bLine :: String, bEnv :: [(String, String)], bLaunchDir :: FilePath, bLaunch :: Launch }

-- | The session's configuration NOW. It was fixed for a daemon's life until a member could be added to a
-- running session ('addMembers'); it is read where it is used, so nothing is passed a copy to go stale.
{-# NOINLINE sCfg #-}
sCfg :: S -> Cfg
sCfg s = unsafePerformIO (readIORef (sCfgV s))

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
  diags <- rd (vDiags s)
  let headL = if null stale then text else "STALE(" ++ show (length stale) ++ ") " ++ text
      line2 = "session=" ++ sName s ++ " gen=" ++ show gen ++ " at=" ++ dt ++ " loaded=" ++ la ++ " checked=" ++ ca
      ls = [headL, line2] ++ [ "stale: " ++ intercalate ", " (map (rel s) (take 6 stale)) | not (null stale) ] ++ detail
      kind = fromMaybe (fromMaybe "OK" (listToMaybe [ k | k <- ["CHECK-FAIL", "CHECK-HANG", "CHECK-PASS"], k `isInfixOf` text ]))
               (listToMaybe [ k | k <- ["DEAD", "stopped", "starting", "PREBUILD-ERROR", "CONFIG-ERROR", "COMPILE-ERROR"], k `isPrefixOf` text ])
  vStatusText s =: (unlines ls)
  old <- rd (vJson s)
  let base = if keep then old else JObj []
      j1 = foldl (\j (k, v) -> set k v j) base
             [ ("session", JStr (sName s)), ("target", JStr (sName s)), ("kind", JStr kind), ("ok", JBool (kind `elem` ["OK", "CHECK-PASS"]))
             , ("stale", JNum (fromIntegral (length stale))), ("stale_files", JArr (map (JStr . rel s) stale))
             , ("warnings", JNum (fromIntegral (warningsIn text))), ("verdict", JStr text), ("text", JStr headL)
             , ("failing", JNum (if kind `elem` ["CHECK-FAIL", "CHECK-HANG"] then fromIntegral (length detail) else 0))
             , ("detail", JArr (map JStr (take 30 detail))), ("generation", JNum (fromIntegral gen)), ("at", JNum t)
             , ("diagnostics", JArr diags) ]
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

-- | What a scoped test says of itself, from its output and the lines that matched its patterns. An output of nothing,
-- or one in which neither pattern matched anywhere, is no result: "none failing, 0 passing" read as one (a call that
-- answered at once with it, after a hung one, was taken for a pass).
scopedSummary :: Maybe Check -> T.Text -> Int -> Int -> String
scopedSummary mc out nFails nPasses = case mc of
  _ | T.null (T.strip out) -> "NO RESULT: the expression printed nothing, so nothing was checked (0 passing is not a pass)"
  Nothing -> "no check configured: read the output"
  Just c | isNothing (ckFail c) -> printf "%d line(s) match the pass pattern; no fail pattern is configured, so read the output for failures" nPasses
  Just c | nFails == 0, nPasses == 0, isJust (ckPass c) -> "NO RESULT: no line matched the pass or the fail pattern (0 passing is not a pass): the expression may not have run to its end -- read the output"
  _ -> printf "%s, %d passing" (if nFails == 0 then "none failing" else show nFails ++ " failing") nPasses

-- | A command's output as text: a load log or an evaluation can be megabytes.
cmd :: S -> Maybe Double -> String -> IO T.Text
cmd s t e = do
  Reply facts out <- theRepl s >>= \r -> replRun r t e
  noteLinks s facts
  pure out

-- | GHCi links a reloaded module when a command first needs it, so a command may supersede code AFTER the
-- unlink that followed the reload's first evaluation: one stale generation of every late-linked module
-- stayed alive (60 MB of a 101-module session, for good). The engine says how many libraries it has linked;
-- when that has grown since the last unlink, another is due.
noteLinks :: S -> Json -> IO ()
noteLinks s facts = forM_ (lookupNum "links" facts) $ \n -> do
  was <- rd (vLinksPruned s)
  when (round n > was) (vUnlinkDue s =: True)
  vLinksSeen s =: round n

-- | Ask the engine (a query: see engine/GhsEngine.hs). An answer with @error@ is an exception here.
ask :: S -> Maybe Double -> String -> [(String, Json)] -> IO Json
ask s t q args = do
  r <- theRepl s >>= \rp -> replQuery rp t q args
  maybe (pure r) (throwIO . userError) (lookupStr "error" r)

splitOn :: Char -> String -> [String]
splitOn c x = case break (== c) x of
  (a, []) -> [a | not (null a)]
  (a, _ : r) -> a : splitOn c r

-- | The build tool's repl command, with the engine as the GHCi it starts. A target's own @repl@ says where
-- with @{engine}@ (@"stack ghci --with-ghc {engine}"@).
replCommandLine :: S -> FilePath -> (Int, Int) -> String
replCommandLine s exe v = case gRepl cfg of
  Just c -> replace "{engine}" (shq exe) c
  Nothing -> unwords $ filter (not . null) $
      [ "cabal repl" ]
      -- (a composed session too, with one member: started as units, it can be given another while it runs)
      ++ [ "--enable-multi-repl" | length (gUnits cfg) > 1 || not (null (gServers cfg)) || gComposed cfg ]
      ++ [ "--with-repl=" ++ shq exe, "--repl-options=-fdiagnostics-color=never" ]
         -- -1 (the default): bare -j, GHC takes the number of processors; 0: off
      ++ [ "--repl-options=-j" ++ (if gGhcJobs cfg > 0 then show (gGhcJobs cfg) else "") | gGhcJobs cfg /= 0 ]
         -- object code: CAFs of interpreted code are not prunable by address, and a server's code is its
         -- objects. Its own -odir (relative: under each unit's package dir) so `cabal build` is not disturbed.
      ++ (if objects then [ "--repl-options=-fobject-code", "--repl-options=-odir=" ++ sObjRel s, "--repl-options=-hidir=" ++ sObjRel s ] else [])
         -- without it GHCi's recompile of IDENTICAL source gives a different .o, and every reload would
         -- look like a change to a running server
      ++ [ "--repl-options=-fobject-determinism" | objects, v >= (9, 12) ]
         -- Linking what a reload recompiled into its temporary library is then ONE tool run, not four: with
         -- rpaths GHC follows the link with two `otool`s and an `install_name_tool` (0.08 s of every first
         -- evaluation after an edit). The library needs no rpath: everything it names is already loaded.
      ++ [ "--repl-options=-fno-use-rpaths" | objects, os == "darwin" ]
         -- A module that asks to be optimised (`{-# OPTIONS_GHC -O1 #-}`: an engine everything else calls)
         -- is NOT, in GHCi, unless interface pragmas are read from the start: without this the pragma runs
         -- the optimiser over code that still does its arithmetic through class dictionaries, because the
         -- libraries' unfoldings were never loaded. It has to be on the command line; in the pragma it is
         -- too late. (A session of 87 modules whose engine has the pragma: its check 33 s -> 25 s, for 9 s
         -- more of cold compile.)
      ++ [ "--repl-options=-fno-ignore-interface-pragmas" | objects ]
         -- ("optimize": the loaded code is what the built library is -- object code at -O1 -- so an evaluation
         -- of it, and what `bench` measures, run at its real speed. Interpreted, a scan of a 19 MB file that
         -- takes 0.3 s ran for minutes. A reload compiles more slowly for it.)
      ++ [ "--repl-options=-O" ++ show (max 1 (gOptLevel cfg)) | gOptimize cfg ]
         -- ("sites": the info tables mapped to the source, and a constructor's own table at each place it is built)
      ++ concat [ [ "--repl-options=-finfo-table-map", "--repl-options=-fdistinct-constructor-tables" ] | gSites cfg ]
      ++ [ gCabalArgs cfg, unwords (gUnits cfg) ]
  where cfg = sCfg s
        objects = gHygiene cfg || gOptimize cfg || gSites cfg || not (null (gServers cfg))

-- | The engine for this session -- our GHCi, beside this executable -- its compiler's version, and that
-- compiler's library directory. It must have been built for exactly the compiler on PATH: it is that
-- compiler's own front end, linked against its libraries.
engineExe :: S -> IO (FilePath, (Int, Int), FilePath)
engineExe s = do
  exe <- (</> "ghci-session-engine") . takeDirectory <$> getExecutablePath
  there <- doesFileExist exe
  if not there then no ("there is no ghci-session-engine beside " ++ exe ++ " (it is built for GHC 9.14.1: see the README for another)") else do
    (mine, theirs, libdir) <- engineFacts s exe
    case (mine, theirs, libdir) of
      (Just a, Just b, Just l) | a == b && not (null l) -> do
        logS s ("engine: GHCi " ++ a)
        let v = case map read (take 2 (splitOn '.' (filter (\c -> isDigit c || c == '.') a))) of { [x, y] -> (x, y); _ -> (0, 0) }
        pure (exe, v, l)
      _ -> no ("the engine is GHC " ++ fromMaybe "?" mine ++ ", the compiler on PATH is " ++ fromMaybe "?" theirs)
  where no why = do
          setStatus s False ("CONFIG-ERROR: " ++ why) [] []
          throwIO (userError why)

-- | The engine's GHC version, the compiler's, and the compiler's library directory. Three process starts
-- (0.2 s of a boot), so remembered in the state directory for as long as neither executable changes.
engineFacts :: S -> FilePath -> IO (Maybe String, Maybe String, Maybe String)
engineFacts s exe = do
  ghc <- findExecutable "ghc" >>= maybe (pure "") canonicalizePath
  te <- modTime exe
  tg <- modTime ghc
  let cache = cStateDir (sConf s) </> "engine-facts"
      key = exe ++ ":" ++ show te ++ ":" ++ ghc ++ ":" ++ show tg
  old <- fmap lines <$> readFileMaybe cache
  case old of
    Just [k, a, b, l] | k == key -> pure (Just a, Just b, Just l)
    _ -> do
      mine <- fmap trim <$> rawSystemOut 20 exe ["--numeric-version"]
      theirs <- fmap trim <$> rawSystemOut 20 "ghc" ["--numeric-version"]
      libdir <- fmap trim <$> rawSystemOut 20 "ghc" ["--print-libdir"]
      case (mine, theirs, libdir) of
        (Just a, Just b, Just l) | not (null ghc) -> void (try (writeAtomic cache (unlines [key, a, b, l])) :: IO (Either IOException ()))
        _ -> pure ()
      pure (mine, theirs, libdir)

-- | On macOS, `gcc` -- what GHC is configured to link with -- is a shim that asks `xcrun` which compiler to
-- run, 20 ms a time, and GHCi runs it TEN times while it starts (to ask where each system library is) and
-- once for every library it links: 0.2 s of every boot. The compiler itself, with the SDK it would have been
-- told about, is the same tool without the detour. Only when the `gcc` on PATH is that shim; remembered in
-- the state directory per selected Xcode.
toolchain :: S -> IO ([String], [(String, String)])
toolchain s
  | os /= "darwin" = pure ([], [])
  | otherwise = do
      gcc <- findExecutable "gcc"
      sel <- either (\(_ :: IOException) -> "") id <$> try (getSymbolicLinkTarget "/var/db/xcode_select_link")
      let cache = cStateDir (sConf s) </> "toolchain"
      old <- fmap lines <$> readFileMaybe cache
      found <- case old of
        Just [k, c, sdk] | k == sel -> pure (Just (c, sdk))
        _ | gcc /= Just "/usr/bin/gcc" -> pure Nothing
          | otherwise -> do
              c <- fmap trim <$> rawSystemOut 20 "xcrun" ["-f", "clang"]
              sdk <- fmap trim <$> rawSystemOut 20 "xcrun" ["--show-sdk-path"]
              case (c, sdk) of
                (Just c', Just sdk') | not (null c') && not (null sdk') -> do
                  void (try (writeAtomic cache (unlines [sel, c', sdk'])) :: IO (Either IOException ()))
                  pure (Just (c', sdk'))
                _ -> pure Nothing
      case found of
        Just (c, sdk) | gcc == Just "/usr/bin/gcc" -> do
          ok <- (&&) <$> doesFileExist c <*> doesDirectoryExist sdk
          set <- lookupEnv "SDKROOT"
          pure (if ok then (["-pgml", c, "-pgmc", c], [ ("SDKROOT", sdk) | isNothing set ]) else ([], []))
        _ -> pure ([], [])

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
  when (gCapabilities cfg > 0) (void (replQuery r (Just 60) "capabilities" [("n", JNum (fromIntegral (gCapabilities cfg)))]))
  -- the engine is built with -H as GHC is; a session does without unless it asks (heap_auto)
  unless (gHeapAuto cfg) (void (try (replQuery r (Just 60) "heap_auto" [("on", JBool False)]) :: IO (Either SomeException Json)))
  -- an expression typed at a session is not the program: `1 + 1` should not answer with a paragraph about
  -- defaulting because the package is built -Wall
  void (c 60 ":seti -Wno-type-defaults")
  forM_ (gPreload cfg) (c 120)
  -- one command for all the imports (forty round trips were 0.3 s of a boot); one at a time only if that
  -- fails, so a single missing module does not cost the rest
  unless (null (gModules cfg)) $ do
    out <- c 120 (":module + " ++ unwords (gModules cfg))
    when (noModule out) (forM_ (gModules cfg) (\m -> c 60 (":module + " ++ m)))

-- | The signature the loaded code was built from, as a file the loaded code can read:
-- @<mtime ns>\\t<path relative to the root>@ per watched source.
writeLoadedSources :: S -> IO ()
writeLoadedSources s = do
  sig <- rd (vLoadedSig s)
  void (try (writeAtomic (sDir s </> "loaded_sources.tsv")
               (concat [ show (round (m * 1e9) :: Integer) ++ "\t" ++ rel s (fromRaw p) ++ "\n" | (p, m) <- M.toList sig ])) :: IO (Either IOException ()))

-- | Did a load succeed? The engine says: the errors the compiler logged while it ran (@facts@), and whether
-- every module of the graph is now loaded (@st@). The output is read for one thing only -- an error GHCi
-- printed without logging it (a link failure thrown as an exception has no diagnostic).
verdictOf :: Json -> Json -> T.Text -> (String, [String])
verdictOf facts st out
  | nErr > 0 = ("COMPILE-ERROR: " ++ show nErr ++ " error(s)", take 20 (map line errs))
  -- the compiler gave up: nothing was loaded, whatever the module counts still say (a load that panicked
  -- left the session's verdict "OK" and its code the old code)
  | not (null panicked) = ("COMPILE-ERROR: the compiler panicked", map T.unpack (take 8 panicked))
  | not (null printed) = ("COMPILE-ERROR: " ++ show (length printed) ++ " error(s)", map T.unpack (take 20 printed))
  | loaded < total = ("COMPILE-ERROR: " ++ show (total - loaded) ++ " of " ++ show total ++ " module(s) not loaded", map T.unpack (lastN 5 (T.lines (T.strip out))))
  | otherwise = ("OK", [])
  where
    -- (not GHCi's own complaints about the prompt's context -- "attempting to use module X which is not
    -- loaded", once per import it could not restore after a load that failed: they are not errors in a source)
    errs = [ d | d <- lookupArr "diagnostics" facts, lookupStr "severity" d == Just "error", lookupStr "file" d /= Just "<interactive>" ]
    nErr = length errs
    printed = filter (T.isInfixOf (T.pack ": error:")) (T.lines out)
    panicked = case break (T.isInfixOf (T.pack "panic! (the 'impossible' happened)")) (T.lines out) of
      (_, []) -> []
      (_, ls) -> ls
    total = maybe 0 round (lookupNum "modules" st) :: Int
    loaded = maybe total round (lookupNum "loaded" st) :: Int
    line d = maybe "<no location>" (\f -> f ++ ":" ++ n "line" ++ ":" ++ n "col") (lookupStr "file" d) ++ ": error: "
               ++ maybe "" (\c -> "[" ++ c ++ "] ") (lookupStr "code" d) ++ unwords (words (takeWhile (/= '\n') (fromMaybe "" (lookupStr "message" d))))
      where n k = maybe "?" (show . (round :: Double -> Int)) (lookupNum k d)

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

afterLoad :: S -> Reply -> Bool -> Double -> IO ()
afterLoad s (Reply facts out) doCheck t0 = do
  pend <- rd (vPendingSig s)
  sig <- if M.null pend then scan (sRoot s) (gWatch (sCfg s)) (gWatchExt (sCfg s)) else pure pend
  vLoadedSig s =: sig
  now >>= (vLoadedAt s =:)
  modifyIORef' (vGeneration s) (+ 1)
  writeLoadedSources s
  writeAtomicT (sDir s </> "load.log") out
  st <- phase s "state" (either (\(_ :: SomeException) -> JObj []) id <$> try (ask s (Just 60) "state" []))
  vLoaded s =: st
  forM_ (lookupStr "cwd" st) (vCwd s =:)
  vDiags s =: take 100 (lookupArr "diagnostics" facts)
  let (v, detail) = verdictOf facts st out
      warns = maybe 0 round (lookupNum "warnings" facts) :: Int
  vLoadedOk s =: (v == "OK")
  let prefix = "OK" ++ (if warns > 0 then " (" ++ show warns ++ " warning(s))" else "")
  vOkPrefix s =: prefix
  let took = (\t -> ("duration_s", JNum (r2 (t - t0)))) <$> now
  if v /= "OK" then took >>= \d -> setStatus s False v detail [d]
    else if not doCheck || null (gChecks (sCfg s))
      then took >>= \d -> setStatus s False (prefix ++ " -- CHECK SKIPPED (" ++ (if null (gChecks (sCfg s)) then "no check configured" else "a load compiles; `test` runs the check, \"watch_check\": true runs it on every save") ++ "): this is a COMPILE verdict only") [] [d]
      else void (runCheck s (Just t0) Nothing)

countSub :: String -> String -> Int
countSub needle = go 0
  where go n [] = n :: Int
        go n x@(_ : r) = if needle `isPrefixOf` x then go (n + 1) (drop (length needle) x) else go n r

-- checks: per member, never merged ---------------------------------------------------

data CheckResult = CheckResult { crMember :: String, crKind :: String, crFailing :: [String], crBody :: T.Text, crSecs :: Double }

-- | A check that runs far longer than it has been taking is HUNG (an evaluation that never ends, a loop
-- in the code under test), and is interrupted at five times the median of its last passing runs -- at
-- least a minute, never past its own timeout -- with a verdict that says so and says where it hung (the last
-- line it printed). The fixed timeout alone (ten minutes by default) cost an agent's loop ten minutes a
-- save, four times in one hour, while the check it was guarding takes three seconds. (The least was fifteen
-- seconds: a suite that had been a stub, passing in no time, was "hung" at fifteen the day it became a real
-- one of forty -- and its agent made the quick half of it the check, to live with that.)
hangLimit :: [Double] -> Maybe Double
hangLimit [] = Nothing
hangLimit ds = Just (max 60 (5 * medianOf ds))

medianOf :: [Double] -> Double
medianOf [] = 0
medianOf ds = sort ds !! (length ds `div` 2)

checkOne :: S -> Check -> IO CheckResult
checkOne s e = do
  cwd' <- rd (vCwd s)
  -- Remember how old the check's log is, so a check that never ran cannot be scored against the PREVIOUS
  -- run's file: a link failure leaves the log untouched and would read as a pass.
  let logp = (cwd' </>) <$> ckLog e
  before <- maybe (pure Nothing) modTime logp
  hist <- M.findWithDefault [] (ckMember e) <$> rd (vCheckSecs s)
  let hard = fromMaybe (gEvalTimeout (sCfg s)) (ckTimeout e)
      soft = [ so | Just so <- [hangLimit hist], so < hard ]
      limit = case soft of { (so : _) -> Just so; [] -> ckTimeout e }
  t0 <- now
  r <- try (cmd s limit (ckExpr e))
  t1 <- now
  case r of
    Left (ReplTimeout t _ said) | not (null soft) ->
      let lastLine = case [ l | l <- map T.strip (T.lines said), not (T.null l), l /= T.pack "Interrupted." ] of { [] -> "nothing yet"; ls -> T.unpack (T.takeEnd 160 (last ls)) }
          why = printf "hung: ran %.0fs where it takes about %.1fs (interrupted); last output: %s" t (medianOf hist) lastLine
      in pure (CheckResult (ckMember e) "HANG" [why] (said <> T.pack ("\n[session] " ++ why)) (t1 - t0))
    Left ex@(ReplTimeout _ _ _) -> pure (CheckResult (ckMember e) "TIMEOUT" [show ex] (T.pack (show ex)) (t1 - t0))
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
      let result = if not (null fails) then CheckResult (ckMember e) "FAIL" fails body (t1 - t0)
            else if not passOk then CheckResult (ckMember e) "INCOMPLETE" ("the pass marker never appeared (did the check run?)" : map T.unpack (lastN 3 (T.lines (T.strip body)))) body (t1 - t0)
            else CheckResult (ckMember e) "PASS" [] body (t1 - t0)
      -- (a passing run's seconds are what the next run's hang is measured against: the last five)
      when (crKind result == "PASS") (modifyIORef' (vCheckSecs s) (M.insertWith (\new old -> take 5 (new ++ old)) (ckMember e) [t1 - t0]))
      pure result

runCheck :: S -> Maybe Double -> Maybe String -> IO T.Text
runCheck s mt0 member = do
  let entries = [ e | e <- gChecks (sCfg s), maybe True (\m -> m == ckMember e || m == takeWhile (/= ':') (ckMember e)) member ]
  if null entries then pure (T.pack ("no check configured" ++ maybe "" (\m -> " for " ++ show m) member)) else do
    t <- maybe now pure mt0
    prefix <- rd (vOkPrefix s)
    push s (prefix ++ " -- running check") ""
    -- (every reply says so while it runs: a client that saved knows the code compiled, and need not wait
    --  for the check to know that)
    tc <- now
    vChecking s =: Just (t, tc, prefix)
    results <- phase s "check" (mapM (checkOne s) entries) `finally` (vChecking s =: Nothing)
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
      else if any ((== "HANG") . crKind) results
        then setStatus s False ("CHECK-HANG: the check did not end in " ++ intercalate ", " [ crMember r | r <- bad, crKind r == "HANG" ] ++ " (interrupted)" ++ tag)
               (take 30 [ crMember r ++ ": " ++ l | r <- bad, l <- (if null (crFailing r) then [crKind r] else crFailing r) ]) facts
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
    rd (vLinksSeen s) >>= (vLinksPruned s =:)
    t0 <- now
    gcMode <- maybe (gPruneGc (sCfg s)) id <$> lookupEnv "GHS_PRUNE_GC"
    r <- try (theRepl s >>= \rp -> replQueryOut rp (Just 300) "prune" [("gc", JStr gcMode)])
    t1 <- now
    case r of
      Left (e :: SomeException) -> logS s ("unlink_cafs failed: " ++ displayException e)
      Right (Reply j said) -> do
        unless (T.null (T.strip said)) (logS s ("unlink_cafs said:\n" ++ T.unpack said))      -- (GHS_CAF_DEBUG in the target's env)
        let k = maybe 0 round (lookupNum "unlinked" j) :: Int
        mapM_ ((vLiveMb s =:) . show . (round :: Double -> Int)) (lookupNum "live_mb" j)
        live <- rd (vLiveMb s)
        if k < 0
          then do vHygieneOn s =: False
                  logS s "hygiene OFF: the pruner cannot read this RTS (its CAF list, or a temporary library's handle): CAFs a reload supersedes are not unlinked; GHS_CAF_DEBUG=1 in the target's env says why"
          else logS s (printf "unlink_cafs: %d unlinked in %.2fs with its GC, live heap %s MB" k (t1 - t0) live)

-- | After the verdict is out: link the reloaded code if nothing has yet (the @warm@ expressions), then the
-- unlink and its GC. In the background, under the work lock: a command that arrives first simply runs first.
warmAsync :: S -> IO ()
warmAsync s = rd (vUnlinkDue s) >>= \dueNow -> when dueNow $ void $ forkIO $ withMVar (vWork s) $ \_ -> do
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

-- | Keep the typecheck's own interfaces up with the sources, in the background. A save typechecks before it
-- reloads; an explicit reload does not, so after a day of those the first save paid for all of it (4.2 s
-- measured, against 0.13). After a reload that compiled -- and after a start -- the sources are typechecked
-- once the verdict is out, under the work lock: a command that arrives first runs first. Nothing to do when
-- no source changed since the last one.
typecheckAsync :: S -> IO ()
typecheckAsync s = when (gWatchTypecheck (sCfg s) && gAutoReload (sCfg s)) $ void $ forkIO $ do
  -- (when the session has been left alone for a second: started at once, it stood in front of the first
  --  command after a start for the 0.3 s it takes)
  let idle k = do
        threadDelay 1000000
        busy <- rd (vBusy s)
        used <- rd (vLastUsed s)
        t <- now
        if (busy == 0 && t - used >= 1.0) || k <= (0 :: Int) then pure () else idle (k - 1)
  idle 30
  withMVar (vWork s) $ \_ -> do
   stopping <- rd (vStopping s)
   ok <- (&&) <$> rd (vLoadedOk s) <*> compiled s
   alive <- rd (vRepl s) >>= maybe (pure False) replAlive
   when (not stopping && ok && alive) (void (try (typecheckNow s) :: IO (Either SomeException (Bool, String, Double, [String], [Json]))))

-- servers: forked children of the engine ---------------------------------------------------

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
  r <- try (ask s (Just 120) "typecheck_expr" [("expr", JStr (svAction z))])
  pure $ case r of
    Left (e :: SomeException) -> Just (displayException e)
    Right j | lookupBool "ok" j == Just True -> Nothing
            | otherwise -> Just (take 300 (unwords (words (fromMaybe "?" (lookupStr "error" j)))))

-- | Fork one server out of the code the engine holds RIGHT NOW. @handover@: this continues a server that was
-- just stopped (carry its state in); the child never decides that for itself.
serverFork :: S -> Server -> Bool -> Bool -> IO (Maybe Int)
serverFork s z handover preforked = do
  let label = svMember z
      hpath = sfile s label "handover"
      (hOut, hIn) = gHandoverEnv (sCfg s)
  carried <- (handover &&) <$> doesFileExist hpath
  let env' = svEnv z ++ [(hOut, hpath)] ++ [ (hIn, hpath) | carried ]
  unless preforked (phase s "prefork" (serverPrefork s z))
  r <- try (phase s "fork" (ask s Nothing "fork"
         [ ("label", JStr label), ("log", JStr (sfile s label "log")), ("action", JStr (svAction z))
         , ("env", JArr [ JArr [JStr k, JStr v] | (k, v) <- env' ]), ("detach", JBool (gComposed (sCfg s))) ]))
  case fmap (lookupNum "pid") r of
    Left (e :: SomeException) -> logS s ("server[" ++ label ++ "]: fork failed: " ++ takeWhile (/= '\n') (displayException e)) >> pure Nothing
    Right Nothing -> logS s ("server[" ++ label ++ "]: fork produced no pid") >> pure Nothing
    Right (Just p) -> do
      let pid = round p :: Int
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
    pure alive
  Just port -> do
    t0 <- now
    let loop = do
          t <- now
          alive <- pidAlive pid
          if not alive then do
              tl <- logTail s label
              logS s ("server[" ++ label ++ "]: pid " ++ show pid ++ " DIED before taking port " ++ show port ++ " -- " ++ tl)
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

-- | Stop one pid: SIGTERM (a server writes its state out on it), then SIGKILL. The engine waits on every
-- child it forks, so a stopped one is gone at once and not a zombie; one adopted from an earlier engine is
-- init's to reap.
serverStopPid :: S -> String -> Int -> IO ()
serverStopPid s label pid = do
  forM_ [(sigTERM, 300 :: Int), (sigKILL, 200)] $ \(sig, n) -> do
    a <- pidAlive pid
    when a $ do
      when (sig == sigKILL) (logS s ("server[" ++ label ++ "]: pid " ++ show pid ++ " did not exit on SIGTERM -- killing it"))
      void (try (signalProcess sig (CPid (fromIntegral pid))) :: IO (Either IOException ()))
      let wait k = when (k > 0) (pidAlive pid >>= \x -> when x (threadDelay 10000 >> wait (k - 1)))
      wait n
  logS s ("server[" ++ label ++ "]: stopped pid " ++ show pid)

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
-- depend on (the engine's own account of what is loaded: `state`), the declared extra files, and its declaration.

-- | A hash of everything a child forked now would run, or 'Nothing' when that cannot be said (then the
-- child is always re-forked).
codeFingerprint :: S -> Server -> IO (Maybe String)
codeFingerprint s z = do
  st <- rd (vLoaded s)
  let units = [ (u, (fromMaybe "" (lookupStr "package" j), strs (j .: "deps"), strs (j .: "objects")))
              | j <- lookupArr "units" st, Just u <- [lookupStr "id" j] ]
      want = [ (takeWhile (/= ':') u, drop 1 (dropWhile (/= ':') u)) | u <- svUnits z, ':' `elem` u ]
      -- a library's unit is <package>-<version>-inplace, another component's ends -inplace-<component>
      isRoot (kind, comp) (uid, (pkg, _, _))
        | kind /= "lib" = ("-inplace-" ++ comp) `isSuffixOf` uid
        | not (null pkg) = pkg == comp && "-inplace" `isSuffixOf` uid
        | otherwise = "-inplace" `isSuffixOf` uid && (comp ++ "-") `isPrefixOf` uid && all isDigit (take 1 (drop (length comp + 1) uid))
      roots = [ [ uid | u@(uid, _) <- units, isRoot w u ] | w <- want ]
      close seen [] = seen
      close seen (u : todo) | u `elem` seen = close seen todo
                            | otherwise = close (u : seen) (todo ++ maybe [] (\(_, ds, _) -> filter (`elem` map fst units) ds) (lookup u units))
      -- a server that names no unit runs, for all we know, everything loaded: over-inclusive is the safe side
      chosen = if null want then map fst units else close [] (concat roots)
      files = nub [ o | u <- chosen, Just (_, _, os) <- [lookup u units], o <- os ]
  if null units || any null roots || null files then pure Nothing else do
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

-- | What a recorded start depends on, in two parts.
--
-- The ANSWER (how the engine is to be started: each unit's flags and modules) depends on the command and on
-- every build file and non-Haskell source the session watches -- the build tool is what compiles a loaded
-- package's C, and what reads its @.cabal@. Not on the engine's binary: rebuilding the tool used to send
-- every session back to the build tool for an answer that could not have changed; the capture's FORMAT is
-- what it would have to agree with, and that is the version here.
--
-- The DEPENDENCIES are the sources of the local packages the repl uses without loading. A change there
-- changes nothing in the answer: those packages have to be BUILT, which is a different and much cheaper
-- question to ask (0.5 s when there is nothing to do, against 4-8 s for the repl's own configuring).
buildInputs :: S -> String -> ([FilePath], [FilePath]) -> IO (String, String)
buildInputs s line (deps, own) = do
  let built = [".cabal", ".project", ".freeze", ".local", ".c", ".h", ".cmm", ".hsc", ".chs", ".x", ".y"]
      rows m = unlines [ fromRaw p ++ " " ++ show t | (p, t) <- M.toList m ]
  sig <- M.union <$> scan (sRoot s) (gWatch (sCfg s)) built <*> scan (sRoot s) own [".cabal"]
  dsig <- scan (sRoot s) deps (built ++ [".hs", ".lhs", ".hs-boot", ".cpp", ".m"])
  pure (unlines [line, "capture format 2"] ++ rows sig, rows (dsig `M.difference` sig))

writeInputs :: FilePath -> (String, String) -> IO ()
writeInputs dir (ask, bld) = writeAtomic (dir </> "inputs") ask >> writeAtomic (dir </> "inputs.deps") bld

-- | Build the local packages the repl uses without loading (and only those): the build tool's own
-- "dependencies only" of the session's units. 'Nothing': this session's command is its own (@repl@), so
-- how to ask is not known.
depsBuildLine :: S -> Maybe String
depsBuildLine s = case gRepl cfg of
  Just _ -> Nothing
  Nothing -> Just (unwords (filter (not . null) ["cabal build -v1 --only-dependencies", gCabalArgs cfg, unwords (gUnits cfg)]))
  where cfg = sCfg s

-- | The local packages the repl USES but does not load, as what they are built from -- or 'Nothing' when
-- that cannot be said for one of them. (The repl's arguments, and the per-unit argument files of a
-- multi-unit repl: @-this-unit-id@ is a unit loaded, @-package-id X-inplace@ a local package used. Where a
-- package's source is, is in the build tool's own plan.)
-- With them, the @.cabal@ files of the packages the repl LOADS: part of what the build
-- tool's answer depends on, and not under a session's watched source directories (a start did not notice
-- one edited while the session was down).
localPackages :: S -> Launch -> IO (Maybe ([FilePath], [FilePath]))
localPackages s l = do
  r <- planDirs s l
  case r of
    Nothing -> pure Nothing
    Just (mineDirs, outDirs) -> do
      own <- concat <$> forM mineDirs (\d -> do
               names <- either (\(_ :: IOException) -> []) id <$> try (getDirectoryContents d)
               pure [ d </> n | n <- names, ".cabal" `isSuffixOf` n ])
      (\ds -> Just (nub (concat ds), own)) <$> mapM packageSources outDirs

-- | Where the packages are, as the build tool's plan says: the directories of the packages the repl LOADS, and of
-- the local packages it uses without loading. 'Nothing' when it cannot be said of one of them.
planDirs :: S -> Launch -> IO (Maybe ([FilePath], [FilePath]))
planDirs s l = do
  files <- forM [ f | ('@' : f) <- lArgs l ] (fmap (maybe [] lines) . readFileMaybe)
  let args = lArgs l ++ concat files
      after k = [ v | (a, v) <- zip args (drop 1 args), a == k ]
      mine = after "-this-unit-id"
      outside = nub [ p | p <- after "-package-id", "-inplace" `isInfixOf` p, p `notElem` mine ]
  if null mine then pure Nothing else do
    plan <- (>>= either (const Nothing) Just . parseJson) <$> readFileMaybe (sRoot s </> "dist-newstyle" </> "cache" </> "plan.json")
    let dirOf u = listToMaybe [ d | e <- maybe [] (lookupArr "install-plan") plan, lookupStr "id" e == Just u
                                  , Just d <- [lookupStr "path" (e .: "pkg-src")] ]
    pure (fmap (\o -> (nub (mapMaybe dirOf mine), nub o)) (mapM dirOf outside))

-- | The sources of the local packages the repl uses WITHOUT loading them (a library the executable depends on): what
-- they are built from, minus anything a loaded package also lists. They are object code from the build tool, so
-- an edit there cannot be reloaded, only built and the repl started again ('applyChanges').
depSourceDirs :: S -> Launch -> IO [FilePath]
depSourceDirs s l = do
  r <- planDirs s l
  case r of
    Nothing -> pure []
    Just (mineDirs, outDirs) -> do
      loaded <- concat <$> mapM packageSources mineDirs
      deps <- concat <$> mapM packageSources outDirs
      pure [ d | d <- deps, not (any (`covers` d) loaded) ]
  where covers a b = a == b || addTrailingPathSeparator a `isPrefixOf` b

-- | Watch those sources too: a save in one of them is a dependency to build and the repl to start again.
addDepWatch :: S -> IO ()
addDepWatch s = do
  mb <- rd (vBoot s)
  ds <- maybe (pure []) (depSourceDirs s . bLaunch) mb
  -- (the list now, read once: 'sCfg' reads the reference when it is evaluated, and a lazy 'fresh' evaluated after the
  -- write below would ask the configuration that has it in it -- a loop)
  cur <- gWatch <$> rd (sCfgV s)
  let new = [ makeRelative (sRoot s) d | d <- ds ]
      fresh = [ d | d <- nub new, d `notElem` cur ]
  unless (null fresh) $ do
    modifyIORef' (sCfgV s) (\c -> c { gWatch = gWatch c ++ fresh })
    -- (they were built at this boot: their sources as they are are what is loaded, or the first look would find them all new)
    sig <- scan (sRoot s) fresh (gWatchExt (sCfg s))
    modifyIORef' (vLoadedSig s) (M.union sig)
    modifyIORef' (vPendingSig s) (\p -> if M.null p then p else M.union sig p)
    logS s ("watch: also the sources of the packages the repl uses without loading: " ++ unwords fresh)

-- | What a package is built from, as far as its @.cabal@ file says: the file, its @hs-source-dirs@, and its C
-- sources and include directories, for every component (more than a dependency needs, never less). A package
-- that names no source directory is its whole directory.
packageSources :: FilePath -> IO [FilePath]
packageSources dir = do
  names <- either (\(_ :: IOException) -> []) id <$> try (getDirectoryContents dir)
  let cabals = [ dir </> n | n <- names, ".cabal" `isSuffixOf` n ]
  texts <- catMaybes <$> mapM readFileMaybe cabals
  let ls = concatMap lines texts
      indent = length . takeWhile (== ' ')
      field name = concat [ vals (drop (length name + 1) (dropWhile (== ' ') l)) ++ concatMap vals (takeWhile (\c -> indent c > indent l || null (trim c)) rest)
                          | (l : rest) <- tailsOf ls, (name ++ ":") `isPrefixOf` map toLower (dropWhile (== ' ') l) ]
      vals x = if "--" `isPrefixOf` dropWhile (== ' ') x then [] else filter (not . null) (words (map (\c -> if c == ',' then ' ' else c) x))
      tailsOf [] = []
      tailsOf x@(_ : r) = x : tailsOf r
      hs = field "hs-source-dirs"
      others = concatMap field ["c-sources", "cxx-sources", "asm-sources", "cmm-sources", "include-dirs"]
  pure (cabals ++ (if null hs then [dir] else map (dir </>) (nub (hs ++ others))))

-- | Start the engine. The build tool's last answer for this command is used again, and the tool not run, when
-- nothing it reads has changed since AND either that was asked for (@Just True@: `--fast`, a restart for
-- memory) or the session can see everything the tool would build ('selfContained'). @Just False@: ask it
-- (a `restart` by hand, a changed C or build file).
boot :: S -> Maybe Bool -> IO ()
boot s how = do
  let cfg = sCfg s
  when (gHygiene cfg && any (`isInfixOf` gRtsFlags cfg) ["-xn", "--nonmoving-gc"]) $ do
    -- the pruner edits RTS lists the non-moving collector reads concurrently: the repl died at the first unlink
    setStatus s False "CONFIG-ERROR: hygiene cannot be used with the non-moving collector (rts_flags)" [] []
    throwIO (userError "hygiene with the non-moving GC")
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
  let env' = gEnv cfg ++ [("GHCI_SESSION", sName s)]
      outF = sDir s </> "repl.out"
      rts = if gRtsFlags cfg `elem` ["", "none"] then [] else ["+RTS"] ++ words (gRtsFlags cfg) ++ ["-RTS"]
  hold <- rd (vHold s)
  vHold s =: 0
  setStatus s False "starting" [] []       -- always visible at once: `start` waits on it
  vHold s =: hold
  t0 <- now
  vSpawned s =: t0
  vEvaluated s =: False
  vUnlinkDue s =: False
  vLinksSeen s =: 0
  vLinksPruned s =: 0
  vHygieneOn s =: gHygiene cfg
  scan (sRoot s) (gWatch cfg) (gWatchExt cfg) >>= (vPendingSig s =:)
  (exe, ver, libdir) <- phase s "engine_facts" (engineExe s)
  let dead why detail e = do
        setStatus s False ("DEAD: " ++ why) detail []
        throwIO (e :: ReplError)
  let line = replCommandLine s exe ver
  -- one recorded start per COMMAND: a composed session that goes back to a member set it has had finds that
  -- set's answer still there
  launchDir <- (\h -> sDir s </> "launch" </> showHash h) <$> hashString line 7
  -- Without being asked, the build tool is skipped only when this session can SEE everything it would build:
  -- its own units, and the sources of every other local package the repl uses.
  before <- readLaunch launchDir
  deps <- maybe (pure Nothing) (localPackages s) before
  inputs <- phase s "build_inputs" (buildInputs s line (fromMaybe ([], []) deps))
  was <- readFileMaybe (launchDir </> "inputs")
  wasDeps <- readFileMaybe (launchDir </> "inputs.deps")
  let asked = if was == Just (fst inputs) then before else Nothing     -- the answer still stands
      whole = isJust deps
      depsMoved = wasDeps /= Just (snd inputs)
  -- the answer stands but a package the repl uses without loading was edited: have it built, and no more
  recorded <- case (asked, depsMoved, depsBuildLine s) of
    (Just l, False, _) -> pure (Just l)
    (Just l, True, Just bl) | how /= Just False && whole -> do
      logS s ("a local dependency's source changed: " ++ bl)
      base <- getEnvironment
      (ec, o, e) <- phase s "build_deps" (readCreateProcessWithExitCode
                      (shell bl) { cwd = Just (sRoot s), env = Just ([ kv | kv@(k, _) <- base, k `notElem` map fst env' ] ++ env') } "")
      let said = o ++ e
      if ec == ExitSuccess then writeInputs launchDir inputs >> pure (Just l) else do
        writeAtomic (sDir s </> "load.log") said
        dead "the build failed (load.log)" (lastN 12 (lines said)) (ReplDied said)
    _ -> pure Nothing
  let fast = how == Just True || gFastStart cfg
      wanted = how /= Just False && (fast || whole)
      known = if wanted then recorded else Nothing
  when (how /= Just False && fast && isNothing known) (logS s "fast start: a build file or the command changed since the build tool was last asked (or it never was) -- asking it")
  launch <- case known of
    Just l -> do
      logS s "the build's answer is reused: the build tool is not run"
      writeAtomic outF ""
      pure l
    Nothing -> do
      -- (the build tool is asked again: how it compiles each unit's C is asked again too -- 'learnC')
      void (try (removeDirectoryRecursive (launchDir </> "cc")) :: IO (Either IOException ()))
      r <- phase s "build" (captureLaunch line (sRoot s) env' launchDir outF (gLoadTimeout cfg) (logS s))
      case r of
        Right l -> do
          deps' <- localPackages s l      -- (now that the build tool has said what the repl uses)
          buildInputs s line (fromMaybe ([], []) deps') >>= writeInputs launchDir
          pure l
        Left said -> do
          writeAtomic (sDir s </> "load.log") said
          dead "the build failed (load.log)" (lastN 12 (lines said)) (ReplDied said)
  vBoot s =: Just (Boot exe line env' launchDir launch)
  phase s "seed_objects" (seedObjects s launch)
  built <- fromMaybe "" <$> readFileMaybe outF
  (tools, toolEnv) <- phase s "engine_facts" (toolchain s)
  -- macOS: the RTS's "returned" memory stays in the footprint unless the engine starts with this inserted
  let memLib = takeDirectory exe </> "libghsmem.dylib"
  -- (and a temporary library answers for names it does not define: that half is not optional, the engine
  -- keeps unchanged modules linked only with it -- so `mem_return: false` turns off the memory half alone)
  haveMem <- if os == "darwin" then doesFileExist memLib else pure False
  let memEnv = [ ("DYLD_INSERT_LIBRARIES", memLib) | haveMem ] ++ [ ("GHS_MEM_RETURN", "0") | haveMem, not (gMemReturn cfg) ]
  -- The optimisation's flags for the SESSION, not only for its units. The build tool starts a repl of several
  -- units with each unit's flags in the unit's own file, and nothing of them on the command line: the session
  -- itself is then an interpreter's, at no optimisation, and what it decides -- how the interfaces of the
  -- packages are read, their unfoldings with them -- is decided so for every unit. The units' code was compiled
  -- at -O2 and ran as if it were not: printing a 19 MB drawing took 2.0 s and 6.4 GB of allocation in a session
  -- of two units, 0.10 s and 140 MB in a session of one, or as a built executable (measured with the compiler
  -- alone, on the same arguments, with and without these three).
  let objects = gHygiene cfg || gOptimize cfg || not (null (gServers cfg))
      top = if not objects || isJust (gRepl cfg) then [] else ["-fobject-code", "-fno-ignore-interface-pragmas"] ++ [ "-O" ++ show (max 1 (gOptLevel cfg)) | gOptimize cfg ]
  r <- try (phase s "load" (startRepl exe (("-B" ++ libdir) : rts ++ tools ++ top) launch (memEnv ++ toolEnv ++ env') outF (gLoadTimeout cfg) (gEvalTimeout cfg) (logS s)))
  case r of
    Left (e :: ReplError) -> dead (takeWhile (/= '\n') (show e)) (lastN 12 (lines (show e))) e
    Right (repl, Reply facts out) -> do
      vRepl s =: Just repl
      vLastLoad s =: Nothing       -- (another engine: nothing it said is known yet)
      vTcLast s =: Nothing
      phase s "post_load" (postLoad s repl)
      guessC s
      afterLoad s (Reply facts (T.pack built <> out)) (sBootCheck s) t0
      compiled s >>= (vContextOk s =:)      -- a load that failed dropped the imports: the next good reload re-issues them

-- | A @.cabal@ or @cabal.project@ changed. Most often that is a MODULE ADDED to a component's list, which is
-- no reason to lose the session: the file is already found by the modules that import it, or can be added
-- as a target. So the build tool is asked again, and its answer compared with the one the engine is running
-- on. Only module names added: the session stays up. Anything else -- a dependency, a flag, a module removed
-- -- is a new package set, and the engine is restarted on the answer just had.
buildFileChanged :: S -> IO ()
buildFileChanged s = do
  mb <- rd (vBoot s)
  alive <- rd (vRepl s) >>= maybe (pure False) replAlive
  ok <- compiled s
  case mb of
    Just b | alive && ok -> do
      let cfg = sCfg s
      logS s "a build file changed: asking the build tool what it changes"
      t0 <- now
      r <- captureLaunch (bLine b) (sRoot s) (bEnv b) (bLaunchDir b) (sDir s </> "build.out") (gLoadTimeout cfg) (logS s)
      t1 <- now
      case r of
        Left _ -> void (restart s (Just False))            -- (it will say what the build tool said)
        Right new -> do
          deps <- localPackages s new
          buildInputs s (bLine b) (fromMaybe ([], []) deps) >>= writeInputs (bLaunchDir b)
          old' <- launchWords (bLaunch b)
          new' <- launchWords new
          case moduleDelta old' new' of
            Just (added, []) -> do
              vBoot s =: Just b { bLaunch = new }
              logS s (printf "the build tool's answer (%.1fs) differs only by %d module(s) added (%s): the session stays up" (t1 - t0) (length added) (unwords added))
              -- a module nothing imports yet becomes a target of the unit that lists it. Not GHCi's `:add`: in
              -- a multi-unit session (every GHCi 9.14 started from a unit file, `-unit` or not) that puts the
              -- file in the INTERACTIVE unit, which compiles it too, into the same object file as the unit
              -- that lists it -- the two builds overwrite each other and the next link of the real unit
              -- fails with an undefined symbol. The engine gives it to the unit whose import paths hold it.
              -- (by FILE: GHCi remembers that it once looked for the module and did not find it)
              targets <- forM added $ \m -> do
                let rels = [ d </> foldr1 (</>) (splitOn '.' m) ++ e | ('-' : 'i' : d) <- new', not (null d), e <- [".hs", ".lhs"] ]
                found <- filterM (doesFileExist . (lCwd new </>)) rels
                pure (fromMaybe m (listToMaybe found))
              unless (null targets) $ do
                r <- try (theRepl s >>= \rp -> replQueryOut rp (Just (gLoadTimeout cfg)) "add_targets" [("files", JArr (map JStr targets))]) :: IO (Either SomeException Reply)
                logS s $ case r of
                  Right (Reply j _) | lookupBool "ok" j == Just True ->
                    "added as targets: " ++ unwords [ f ++ " [" ++ u ++ "]" | JObj o <- lookupArr "targets" j, Just (JStr f) <- [lookup "file" o], Just (JStr u) <- [lookup "unit" o] ]
                  Right (Reply j _) -> "not added as targets (loaded when something imports them): " ++ fromMaybe "?" (lookupStr "error" j)
                  Left e -> "not added as targets (loaded when something imports them): " ++ takeWhile (/= '\n') (show e)
              void (try (cmd s (Just 60) ":set -Wno-missing-home-modules") :: IO (Either SomeException T.Text))   -- (its list of them is the old one)
              void (reload s (gWatchCheck cfg) (gWatchRefork cfg) Nothing)
              unless (null added) (note s ("[" ++ show (length added) ++ " module(s) added to the build file: no restart]") [])
            _ -> do
              logS s "the build tool's answer changed (more than modules added): restarting the repl on it"
              void (restart s (Just True))
    _ -> void (restart s (Just False))

-- | A start as one list of words: the arguments, with each argument file in place of its name, and the
-- files those name in place of theirs (cabal 3.18 on Linux: one file holding a `-unit @file` per unit).
launchWords :: Launch -> IO [String]
launchWords l = expand (3 :: Int) (lArgs l)
  where expand 0 as = pure as
        expand n as = concat <$> mapM (\a -> case a of
          '@' : f -> maybe (pure [a]) (expand (n - 1) . lines) =<< readFileMaybe f
          _ -> pure [a]) as

-- | A start's arguments as the engine reads them: an argument file that holds `-unit` entries (the whole
-- command line, as cabal 3.18 on Linux writes it) opened up; a unit's own file left as its name.
launchArgs :: Launch -> IO [String]
launchArgs l = concat <$> mapM (\a -> case a of
  '@' : f -> (\t -> case t of { Just c | "-unit" `elem` lines c -> lines c; _ -> [a] }) <$> readFileMaybe f
  _ -> pure [a]) (lArgs l)

-- | If two starts differ only in the module names they list: those added and those removed.
moduleDelta :: [String] -> [String] -> Maybe ([String], [String])
moduleDelta old new
  | filter (not . isModuleName) old == filter (not . isModuleName) new = Just (mods new \\ mods old, mods old \\ mods new)
  | otherwise = Nothing
  where mods = filter isModuleName
        isModuleName a = not (null a) && all part (splitOn '.' a) && take 1 a /= "." && not ("." `isSuffixOf` a) && not (".." `isInfixOf` a)
        part p = case p of { (c : cs) -> isUpper c && all (\x -> isDigit x || x `elem` ("_'" :: String) || x `elem` ['a' .. 'z'] || x `elem` ['A' .. 'Z']) cs; [] -> False }

-- | The configuration read again from @ghci-session.json@, and taken if it is not the one the session has:
-- what was taken, in words. (The session read it when it started and never again: a check set in it, a unit or
-- a watched directory added, did nothing until the session was stopped and started.) What a restart of the repl
-- reads is then the new one -- its units, its options, its checks. What only a new daemon takes -- the history
-- and its compactor, the state's place -- stays as it was. A file that does not parse is said, and not taken.
reconfigure :: S -> IO (Maybe String)
reconfigure s = do
  r <- try (loadConf (sRoot s)) :: IO (Either SomeException (Either String Conf))
  case r of
    Right (Right conf) -> do
      rootFiles <- sort . filter (\f -> ".cabal" `isSuffixOf` f || "cabal.project" `isPrefixOf` f) <$> getDirectoryContents (sRoot s)
      new <- fmap (withRootFiles rootFiles) <$> resolve conf (sName s)
      was <- readFileMaybe (sDir s </> "config.taken")
      now' <- readFileMaybe (sRoot s </> "ghci-session.json")
      case new of
        Right cfg | now' /= was -> do
          writeIORef (sCfgV s) cfg
          forM_ now' (writeAtomic (sDir s </> "config.taken"))
          logS s "the configuration was read again (ghci-session.json changed)"
          pure (Just "[ghci-session.json changed: the session is on the configuration as it is now]")
        Right _ -> pure Nothing
        Left e -> logS s ("ghci-session.json: " ++ e ++ " -- the configuration is kept as it was") >> pure Nothing
    Right (Left e) -> logS s ("ghci-session.json does not read: " ++ e ++ " -- the configuration is kept as it was") >> pure Nothing
    Left e -> logS s ("ghci-session.json does not read: " ++ displayException e ++ " -- the configuration is kept as it was") >> pure Nothing

-- | The sources watched are the configuration's as it is now: another watcher, and the one before ends.
rewatch :: S -> IO ()
rewatch s = do
  modifyIORef' (vWatchGen s) (+ 1)
  void (forkIO (void (try (watchLoop s) :: IO (Either SomeException ()))))

-- | The configuration's file is looked at every second: changed (and the same a second later, so not in the
-- middle of being written), the session takes it -- the repl is restarted on it, under the work lock as any
-- request is.
configLoop :: S -> IO ()
configLoop s = do
  let file = sRoot s </> "ghci-session.json"
      stamp = either (\(_ :: IOException) -> Nothing) Just <$> try (getModificationTime file)
      go was = do
        threadDelay 1000000
        t <- stamp
        if t == was then go was else do
          threadDelay 500000
          t' <- stamp
          if t' /= t then go was else do
            r <- try (withMVar (vWork s) $ \_ -> do
                        said <- reconfigure s
                        when (isJust said) $ do
                          histEvent s "save: ghci-session.json (the configuration: a restart on it)" (void (restart s (Just False)))
                          rewatch s) :: IO (Either SomeException ())
            either (\e -> logS s ("the configuration: " ++ displayException e)) pure r
            go t
  stamp >>= go

-- | A fresh repl. The servers that were running come back on the new code (a plain session's children die
-- with its repl; a composed session's are kept if their code did not change).
restart :: S -> Maybe Bool -> IO String
restart s fast = do
  reforkJoin s
  t0 <- now
  oneVerdict s $ do
    logS s "restart"
    was <- filterM (fmap isJust . serverRunning s) (serverLabels s)
    phase s "repl_stop" (rd (vRepl s) >>= mapM_ (\r -> stopRepl r (logS s)))
    vRepl s =: Nothing
    vLastLoad s =: Nothing
    boot s fast
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
  typecheckAsync s
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
      out <- T.pack <$> restart s (Just True)
      note s (printf "[repl RESTARTED instead of reloaded: it had grown to %.0f MB, over the %.0f MB budget; now restarted]" rss budget) []
      pure out
    else do
      t0 <- now
      phase s "scan" (scan (sRoot s) (gWatch cfg) (gWatchExt cfg) >>= (vPendingSig s =:))
      -- which files differ from what is loaded, when that is all that happened (none added, none gone):
      -- the engine then reloads without scanning every module (GhsFastLoad). Said afresh before every
      -- reload -- an empty list too, which is the scan.
      loaded <- rd (vLoadedSig s)
      pending <- rd (vPendingSig s)
      let moved = if M.keysSet loaded == M.keysSet pending then [ fromRaw p | (p, t) <- M.toList pending, M.lookup p loaded /= Just t ] else []
      void (try (theRepl s >>= \rp -> replQuery rp (Just 10) "changed" [("files", JArr (map JStr moved))]) :: IO (Either SomeException Json))
      push s "reloading" ""
      -- Nothing to reload: every watched source is what the loaded code was built from, and that load
      -- compiled. GHCi would still scan every module and walk the graph to find that out (0.09 s and 150 MB
      -- on a hundred modules); its last answer is the answer. (The checks and the servers follow as ever.)
      lastOk <- rd (vLoadedOk s)
      lastRep <- rd (vLastLoad s)
      r <- case lastRep of
        Just rep0 | lastOk && loaded == pending && not (M.null loaded) -> do
          logS s "reload: no source changed since the load -- not asked again"
          pure (Right rep0)
        _ -> try (phase s "ghci_reload" (theRepl s >>= \rp -> replRun rp (Just (gLoadTimeout cfg)) ":reload"))
      case r of
        Left (e :: ReplError) -> setStatus s False ("DEAD: " ++ takeWhile (/= '\n') (show e)) [] [] >> pure (T.pack (show e))
        Right rep@(Reply facts out) -> do
          vLastLoad s =: Just rep
          writeAtomicT (sDir s </> "reload.log") out
          let nf k = maybe 0 round (lookupNum k facts) :: Int
          when (nf "kept_linked" + nf "relink" > 0) $
            logS s ("reload: " ++ show (nf "kept_linked") ++ " module(s) stay linked, " ++ show (nf "relink") ++ " to link again" ++ (let ms = strs (facts .: "relink_modules") in if null ms then "" else " (" ++ unwords ms ++ (if length ms < nf "relink" then " ..." else "") ++ ")"))
          vUnlinkDue s =: True
          vEvaluated s =: (gUnlinkAfter cfg == "reload")
          phase s "unlink" (unlinkCafs s)   -- "reload": now, which reaches the generation BEFORE the one just replaced
          afterLoad s rep doCheck t0
          -- A reload that succeeds keeps GHCi's context (its imports); one that fails drops them. So they
          -- are re-issued after the first reload that succeeds again.
          okNow <- compiled s
          ctx <- rd (vContextOk s)
          when (okNow && not ctx) $
            void (try (phase s "post_load" (theRepl s >>= postLoad s)) :: IO (Either SomeException ()))
          vContextOk s =: okNow
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

-- | Do the sources on disk typecheck? Asked of the engine, which answers without generating code and without
-- touching what is loaded (so it says nothing about the loaded code, and the session's verdict stands). The
-- answer is one line -- @OK -- TYPECHECK@ or @TYPE-ERROR: n error(s)@ -- and the errors; the compiler's
-- output is in @typecheck.log@.
typecheckSources :: S -> IO T.Text
typecheckSources s = (\(_, line, secs, detail, _) -> T.pack (unlines ((line ++ printf " (%.1fs)" secs) : detail))) <$> typecheckNow s

-- | ... as its parts: did they, the line, the errors, the compiler's diagnostics.
-- | The last typecheck's answer, formatted as 'typecheckSources' does, when no source has changed since it
-- was asked: it needs neither the work lock nor the repl. Nothing otherwise.
typecheckCached :: S -> IO (Maybe T.Text)
typecheckCached s = do
  sig <- scan (sRoot s) (gWatch (sCfg s)) (gWatchExt (sCfg s))
  lastTc <- rd (vTcLast s)
  pure $ case lastTc of
    Just (sig0, (_, line, secs, detail, _)) | sig0 == sig ->
      Just (T.pack (unlines ((line ++ printf " (%.1fs, no source changed since)" secs) : detail)))
    _ -> Nothing

typecheckNow :: S -> IO (Bool, String, Double, [String], [Json])
typecheckNow s = do
  t0 <- now
  -- The answer is a function of the sources: asked again with none of them changed, it is the answer there
  -- was. And with the same files and some of them changed, the engine is told which (it then typechecks
  -- without scanning every module).
  sig <- scan (sRoot s) (gWatch (sCfg s)) (gWatchExt (sCfg s))
  lastTc <- rd (vTcLast s)
  case lastTc of
    Just (sig0, res@(_, line, _, _, _)) | sig0 == sig -> do
      logS s ("[time] typecheck 0.00s: " ++ line ++ " (no source changed since it was last asked)")
      pure res
    _ -> do
      let since = case lastTc of
            Just (sig0, _) | M.keysSet sig0 == M.keysSet sig -> [ ("since", JBool True), ("files", JArr [ JStr (fromRaw p) | (p, t) <- M.toList sig, M.lookup p sig0 /= Just t ]) ]
            _ -> []
      res <- typecheckAsk s since t0
      vTcLast s =: Just (sig, res)
      pure res

typecheckAsk :: S -> [(String, Json)] -> Double -> IO (Bool, String, Double, [String], [Json])
typecheckAsk s since t0 = do
  Reply j out <- theRepl s >>= \rp -> replQueryOut rp (Just (gLoadTimeout (sCfg s))) "typecheck" (("dir", JStr (sDir s </> "typecheck")) : since)
  t1 <- now
  writeAtomicT (sDir s </> "typecheck.log") out
  let (v, detail) = verdictOf j (JObj []) T.empty
      warns = maybe 0 round (lookupNum "warnings" j) :: Int
      failed = lookupBool "ok" j /= Just True
      line | v /= "OK" = replace "COMPILE-ERROR" "TYPE-ERROR" v
           | failed = "TYPE-ERROR: " ++ fromMaybe "the load failed (typecheck.log)" (lookupStr "thrown" j <|> lookupStr "error" j)
           | otherwise = "OK" ++ (if warns > 0 then " (" ++ show warns ++ " warning(s))" else "") ++ " -- TYPECHECK"
  logS s (printf "[time] typecheck %.2fs: %s" (t1 - t0) line)
  pure (not failed && v == "OK", line, t1 - t0, detail, lookupArr "diagnostics" j)

-- | A save, from the watcher. The sources are typechecked FIRST: that answer comes in a fraction of the time
-- a reload takes to compile what the edit reaches, and when it is "no" the reload is not done at all -- it
-- would fail the same way, later, and a failed load takes the modules it could not compile (and the prompt's
-- imports) out of the session. So a type error costs nothing: the session goes on running the last code that
-- compiled, and says so. (`watch_typecheck`: false for a reload on every save, as before.)
watchReload :: S -> IO ()
watchReload s = do
  let cfg = sCfg s
  ok <- rd (vLoadedOk s)        -- (what is LOADED compiled: the verdict may be a type error of ours, below)
  tc <- if gWatchTypecheck cfg && ok then either (\(_ :: SomeException) -> Nothing) Just <$> try (typecheckNow s) else pure Nothing
  case tc of
    Just (False, line, secs, detail, diags) -> do
      vDiags s =: take 100 diags
      setStatus s False (replace "TYPE-ERROR" "COMPILE-ERROR" line ++ "  [typecheck: NOT reloaded -- the session still runs the last code that compiled]")
        detail [("duration_s", JNum (r2 secs)), ("typecheck_only", JBool True)]
    _ -> void (reload s (gWatchCheck cfg) (gWatchRefork cfg) Nothing)

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

-- | `doc`: the session's declarations that a query finds (see "GhciSession.Doc"). The index is the watched
-- Haskell sources, read on the first query and again only where a file has changed.
docSearch :: S -> Json -> IO T.Text
docSearch s req = do
  t0 <- now
  sig <- scan (sRoot s) (gWatch (sCfg s)) [".hs"]
  old <- rd (vDocs s)
  new <- fmap M.fromList $ forM (M.toList sig) $ \(p, mt) -> case M.lookup p old of
    Just (mt', es) | mt' == mt -> pure (p, (mt, es))
    _ -> do
      txt <- fromMaybe T.empty <$> readFileText (fromRaw p)
      let es = indexFile (rel s (fromRaw p)) txt
      length es `seq` pure (p, (mt, es))
  vDocs s =: new
  let entries = concatMap (snd . snd) (M.toList new)
      ws = map T.pack (strs (req .: "words"))
      n = maybe 8 round (lookupNum "n" req) :: Int
      together = search entries ws
      -- every word in one declaration if there is such a one; else each word answered on its own
      (found, missing) = if null together && length ws > 1 then searchEach entries ws else (together, [])
      apart = null together && not (null found)
      hits = take n found
      docLines = if n == 1 || length hits == 1 then 40 else 4
      notes = [ T.pack ("(no declaration has all of: " ++ unwords (map T.unpack ws) ++ " -- each word on its own)") | apart ]
              ++ [ T.pack ("(nothing matches: " ++ unwords (map T.unpack missing) ++ ")") | not (null missing), apart ]
  t1 <- now
  logS s (printf "[time] doc %.3fs: %d declarations in %d files, %d read again" (t1 - t0) (length entries) (M.size new)
            (length [ () | (p, (mt, _)) <- M.toList new, fmap fst (M.lookup p old) /= Just mt ]))
  pure $ if lookupBool "json" req == Just True then T.pack (encode (JArr [ entryJson sc e | (sc, e) <- hits ]))
         else if null hits then T.pack ("nothing in this session's " ++ show (length entries) ++ " declarations matches " ++ unwords (map T.unpack ws))
         else T.intercalate (T.pack "\n") (notes ++ map (render docLines . snd) hits)

-- | What changes when HEAD does: HEAD itself, and the directory the branch's ref is rewritten in (a commit
-- replaces the ref file, so it is the directory that sees it). Watched, a commit is noticed when it happens
-- rather than at the next two-second look.
gitWatchPaths :: S -> IO [FilePath]
gitWatchPaths s = do
  dotGit <- readFileMaybe (sRoot s </> ".git")
  let gitDir = case dotGit of
        Just t | "gitdir: " `isPrefixOf` t -> let d = trim (drop 8 t) in if "/" `isPrefixOf` d then d else sRoot s </> d
        _ -> sRoot s </> ".git"
  h <- fmap trim <$> readFileMaybe (gitDir </> "HEAD")
  common <- maybe gitDir (\c -> let d = trim c in if "/" `isPrefixOf` d then d else gitDir </> d) <$> readFileMaybe (gitDir </> "commondir")
  let refDirs = case h of
        Just r | "ref: " `isPrefixOf` r -> [takeDirectory (common </> drop 5 r), takeDirectory (gitDir </> drop 5 r)]
        _ -> []
  filterM (\p -> (||) <$> doesFileExist p <*> doesDirectoryExist p) (nub ([gitDir </> "HEAD", gitDir] ++ refDirs))

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

-- | Stop: say so, and wake the accept loop (it looks at the flag between connections, and would otherwise
-- sit out its half-second wait: every `stop` was 0.55 s).
stopNow :: S -> IO ()
stopNow s = do
  vStopping s =: True
  void (forkIO (sockPath (sDir s) >>= unixConnect >>= mapM_ (\fd -> void (try (closeFd fd) :: IO (Either IOException ())))))

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
  gen <- rd (vWatchGen s)
  let cfg = sCfg s
      doScan = scan (sRoot s) (gWatch cfg) (gWatchExt cfg)
  -- the baseline is what was LOADED, not a scan of now: the boot publishes its verdict before this thread
  -- starts, and a save in between -- a client that saves as soon as `start` answers -- would otherwise be
  -- taken for the loaded source and never reloaded (seen on CI: the save waited out its 120 s)
  loaded <- rd (vLoadedSig s)
  first <- if M.null loaded then doScan else pure loaded
  roots0 <- filterM doesDirectoryExist [ sRoot s </> d | d <- gWatch cfg ]
  git <- if gReloadOnCommit cfg then gitWatchPaths s else pure []
  let roots = roots0 ++ git
  w <- makeWaiter (gWatcher cfg) (gPollInterval cfg) (gDebounce cfg) (M.keys first ++ map toRaw roots) (logS s)
  kind <- waiterKind w
  logS s ("watch: " ++ show (M.size first) ++ " sources by " ++ kind)
  head0 <- if gReloadOnCommit cfg then headCommit s else pure ""
  t0 <- now
  let loop lastSig headC slow = do
        stopping <- rd (vStopping s)
        mine <- (== gen) <$> rd (vWatchGen s)       -- (a member was added: another watcher has its sources)
        unless (stopping || not mine) $ do
          fired <- waiterWait w 0.5
          t <- now
          let due = t - slow >= 2.0
          if not fired && not due then loop lastSig headC slow else do
            (lastSig1, headC1) <- if gReloadOnCommit cfg       -- (a file is read: asked whenever anything stirred)
              then do
                h <- headCommit s
                if not (null h) && not (null headC) && h /= headC
                  then do
                    -- a commit is when everything catches up, whatever a save does: checks and servers too
                    logS s ("commit " ++ take 10 h ++ ": full reload (check, re-fork)")
                    histEvent s ("commit " ++ take 10 h ++ ": full reload") (drive s (void (reload s True True Nothing)))
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
                  stopNow s
                loop lastSig1 headC1 slow1
              else do
                now >>= (vLastUsed s =:)      -- someone is editing
                waiterSettle w                -- an editor's save is several writes
                cur2 <- doScan
                ok <- waiterUpdate w (M.keys cur2 ++ map toRaw roots)   -- new files, and files saved by rename (a new inode)
                unless ok (logS s "watch: cannot watch these paths with kernel events -- polling from here" >> toPolling w)
                held <- batchHeld s
                unless held (when (gAutoReload cfg) (applyChanges s (drive s) cur2))
                -- (held: the old signature stays, so the next look finds the edits again, and a hold that ran out reloads)
                loop (if held then lastSig1 else cur2) headC1 slow1
  loop first head0 t0 `finally` waiterClose w

-- | A batch of edits is being written ('hold'): the watcher leaves the sources alone until 'release', or until the
-- hold runs out (a client that died must not leave the session deaf to saves).
batchHeld :: S -> IO Bool
batchHeld s = do
  b <- rd (vBatch s)
  case b of
    Nothing -> pure False
    Just until' -> do
      t <- now
      if t < until' then pure True else do
        vBatch s =: Nothing
        logS s "watch: the hold ran out -- reloading what was saved"
        pure False

-- | Reload for the sources as they are now, when they differ from the loaded ones: what the watcher does after a
-- save, and what 'release' does for the saves it held back. @run@ says how the reload is driven: the watcher takes
-- the work lock ('drive'), a client request already has it.
applyChanges :: S -> (IO () -> IO ()) -> Sig -> IO ()
applyChanges s run cur2 = do
  loaded <- rd (vLoadedSig s)
  when (cur2 /= loaded) $ do
    let changed = [ fromRaw p | p <- M.keys (M.union cur2 loaded), M.lookup p cur2 /= M.lookup p loaded ]
    let buildFile p = ".cabal" `isSuffixOf` p || "cabal.project" `isPrefixOf` takeFileName p
        cSrc p = any (`isSuffixOf` p) [".c", ".h"]
    deps <- rd (vBoot s) >>= maybe (pure []) (depSourceDirs s . bLaunch)
    let inDep p = let a = sRoot s </> p in any (\d -> a == d || addTrailingPathSeparator d `isPrefixOf` a) deps
        (depCh, ownCh) = partition inDep changed
    saved <- saveLine s changed
    if any cSrc ownCh && (any buildFile ownCh || not (null depCh))
      then do
        logS s "a .c/.h changed, and a build file or a dependency with it: restarting the repl"
        histEvent s (saved ++ " (a restart)") (run (void (restart s (Just False))))      -- (through the build tool: it is what compiles a package's C)
    else if any cSrc ownCh
      then histEvent s (saved ++ " (C)") (run (cChanged s (filter cSrc ownCh)))
    else if not (null depCh)
      then do
        logS s ("a local dependency's source changed (" ++ unwords (take 3 depCh) ++ "): building it and restarting the repl (it is object code, not reloadable)")
        histEvent s (saved ++ " (a dependency: rebuilt, restart)") (run (void (restart s Nothing)))      -- (the build tool is asked for the dependency alone, and the answer it gave is reused)
    else if any buildFile ownCh
      then histEvent s (saved ++ " (a build file)") (run (buildFileChanged s))
    else do
      logS s ("watch: " ++ show (length changed) ++ " file(s) changed -- reload")
      histEvent s saved (run (watchReload s))

-- the C of the loaded units, compiled again and taken by the running repl ------------------------
--
-- A loaded C object cannot be replaced, so a saved @.c@ was a restart: the build tool, and every module loaded
-- again. It can be SUPERSEDED, as a reloaded module is (the engine's @load_objects@): the source compiled here,
-- the object linked as the newest, and the modules that call into it linked again when next needed.
--
-- Compiled here with the build tool's own command, which nothing writes down: the unit's flags say where its
-- headers are but not its C options. So the first time a unit's C changes the build tool is asked to build that
-- unit, verbosely, and the command it ran is kept beside the start it belongs to (@launch/<hash>/cc/@, so a
-- changed build file, which is another start, asks again); from then on the compiler is run directly, in the
-- unit's directory, into the session's own directory (@cobj/@ -- the build tool's objects are left as it built
-- them). A header is every C source of its unit. What cannot be done this way -- a source of no loaded unit, a
-- session with a repl command of its own, an engine that does not take objects -- is a restart, as before; and C
-- that does not compile is a COMPILE-ERROR with the compiler's words, the session running what it had.

-- | A loaded unit's C: where it is compiled, where its objects go, and each source with its object.
data CUnit = CUnit { cuId :: String, cuWd :: FilePath, cuOdir :: FilePath, cuC :: [(FilePath, FilePath)], cuArgs :: [String] }

-- | The units of a start and their C (the unit files as the build tool wrote them; one unit: the arguments).
cUnits :: Launch -> IO [CUnit]
cUnits l = do
  let files = [ f | ('@' : f) <- lArgs l ]
  argss <- if null files then pure [lArgs l] else mapM (fmap (maybe [] lines) . readFileMaybe) files
  fmap catMaybes $ forM argss $ \as -> do
    let after k = listToMaybe [ v | (a, v) <- zip as (drop 1 as), a == k ]
        wd = fromMaybe (lCwd l) (after "-working-dir")
        absIn q = normalise (if isAbsolute q then q else wd </> q)
    case (after "-this-unit-id", after "-odir") of
      (Just uid, Just od) -> do
        let odir = absIn od
        cs <- fmap catMaybes $ forM [ absIn a | a <- as, ".o" `isSuffixOf` a, take 1 a /= "-" ] $ \o -> do
          let rel = makeRelative odir o
              src = normalise (wd </> replaceExtension rel "c")
          ok <- if isAbsolute rel then pure False else doesFileExist src
          pure (if ok then Just (src, o) else Nothing)
        pure (Just (CUnit uid wd odir cs as))
      _ -> pure Nothing

data COutcome = CDone Int [String] Double | CError FilePath [String] | CCannot String

-- | A saved @.c@ or @.h@ of a loaded unit: its C compiled and taken, then what a save does (a reload, the check).
cChanged :: S -> [FilePath] -> IO ()
cChanged s changed = do
  r <- try (reloadC s changed) :: IO (Either SomeException COutcome)
  case r of
    Right (CDone n names secs) -> do
      logS s ("C: " ++ unwords (map takeFileName changed) ++ " compiled and taken in " ++ showG (r2 secs) ++ "s; " ++ show n ++ " module(s) to link again"
              ++ (if null names then "" else " (" ++ unwords names ++ ")"))
      watchReload s
    Right (CError f out) -> do
      logS s ("C: " ++ f ++ " does not compile")
      setStatus s False ("COMPILE-ERROR: " ++ takeFileName f ++ " (C)  [NOT taken -- the session still runs the last code that compiled]") (lastN 30 out) []
    Right (CCannot why) -> restartFor why
    Left e -> restartFor (takeWhile (/= '\n') (displayException e))
  where restartFor why = do
          logS s ("a .c/.h changed: restarting the repl (" ++ why ++ ")")
          void (restart s (Just False))

reloadC :: S -> [FilePath] -> IO COutcome
reloadC s changed = do
  t0 <- now
  mb <- rd (vBoot s)
  case mb of
    Nothing -> pure (CCannot "no start is recorded")
    Just b -> do
      units <- cUnits (bLaunch b)
      let absP q = normalise (sRoot s </> q)
          ofSource c = [ (u, sc) | u <- units, sc@(src, _) <- cuC u, src == c ]
          -- (a header is its unit's: the unit whose directory holds it, the innermost when one is inside another)
          ofHeader h = let holds = [ u | u <- units, addTrailingPathSeparator (cuWd u) `isPrefixOf` h, not (null (cuC u)) ]
                           deep = maximum (0 : map (length . cuWd) holds)
                       in [ (u, sc) | u <- holds, length (cuWd u) == deep, sc <- cuC u ]
          found = [ (q, if ".h" `isSuffixOf` q then ofHeader (absP q) else ofSource (absP q)) | q <- changed ]
          work = nubBy (\(_, a) (_, c) -> snd a == snd c) (concatMap snd found)
      case [ q | (q, []) <- found ] of
        (q : _) -> pure (CCannot (q ++ " is not the C of a loaded unit"))
        [] -> do
          let us = nubBy (\a c -> cuId a == cuId c) (map fst work)
          have <- mapM (cTemplate b) us
          tmpls <- if all isJust have then pure have else do
            learned <- learnC s b units us
            if learned then mapM (cTemplate b) us else pure have
          case sequence tmpls of
            Nothing -> pure (CCannot "how the build tool compiles this unit's C is not known")
            Just ts -> do
              built <- forM work $ \(u, (src, _)) -> do
                let tmpl = fromMaybe [] (lookup (cuId u) (zip (map cuId us) ts))
                    out = sDir s </> "cobj" </> cuId u
                    rel = makeRelative (cuWd u) src
                    args = setOdirTo out tmpl ++ [rel]
                createDirectoryIfMissing True out
                (ec, o, e) <- readCreateProcessWithExitCode (proc "ghc" args) { cwd = Just (cuWd u) } ""
                pure (if ec == ExitSuccess then Right (out </> replaceExtension rel "o") else Left (src, lines (o ++ e)))
              case [ x | Left x <- built ] of
                ((src, said) : _) -> pure (CError (makeRelative (sRoot s) src) said)
                [] -> do
                  Reply j _ <- theRepl s >>= \rp -> replQueryOut rp (Just 120) "load_objects"
                                 [("files", JArr [ JStr o | Right o <- built ]), ("units", JArr (map (JStr . cuId) us))]
                  t1 <- now
                  pure $ case lookupStr "error" j of
                    Just why -> CCannot why
                    Nothing -> CDone (maybe 0 round (lookupNum "relink" j)) [ m | JStr m <- lookupArr "relink_modules" j ] (t1 - t0)

-- | The build tool's command for a unit's C, as it was last seen (the arguments, without the source).
cTemplate :: Boot -> CUnit -> IO (Maybe [String])
cTemplate b u = fmap (filter (not . null) . lines) <$> readFileMaybe (bLaunchDir b </> "cc" </> cuId u)

-- | Have the build tool build these units, verbosely, and keep the command it compiles each unit's C with.
-- Did any command turn up?
learnC :: S -> Boot -> [CUnit] -> [CUnit] -> IO Bool
learnC s b units us = case gRepl cfg of
  Just _ -> pure False        -- (a repl command of its own: how to ask for a build is not known)
  Nothing -> do
    -- the targets that are these units, by name (a unit id starts with its package's); all of them when none is
    let named = [ t | t <- gUnits cfg, any (\u -> drop 1 (dropWhile (/= ':') t) `isPrefixOf` cuId u) us ]
        line = unwords (filter (not . null) ["cabal build -v2", gCabalArgs cfg, unwords (if null named then gUnits cfg else named)])
    logS s ("C: asking the build tool how it compiles it, once: " ++ line)
    (_, o, e) <- readCreateProcessWithExitCode (shell line) { cwd = Just (sRoot s) } ""
    let cmds = [ ws | l <- lines (o ++ e), let ws = ccWords l, "-c" `elem` ws, "-odir" `elem` ws, "-dynamic" `notElem` ws
                    , any (".c" `isSuffixOf`) ws ]
        odirOf ws = listToMaybe [ v | (a, v) <- zip ws (drop 1 ws), a == "-odir" ]
    kept <- forM units $ \u -> case [ ws | ws <- cmds, fmap normalise (odirOf ws) == Just (cuOdir u) ] of
      (ws : _) -> do
        createDirectoryIfMissing True (bLaunchDir b </> "cc")
        writeAtomic (bLaunchDir b </> "cc" </> cuId u) (unlines [ w | w <- ws, not (".c" `isSuffixOf` w) ])
        pure True
      [] -> pure False
    pure (or kept)
  where cfg = sCfg s

-- | The same, found at a start without asking the build tool -- which says how it compiles C only when it
-- compiles some, and at a start it has none to compile. The command is put together from what is on record: the
-- unit's own flags (where its headers are, its packages), the options the package's @.cabal@ names for C, and
-- the optimisation the build tool gives C by default. Whether that IS the build tool's command is not assumed: a
-- source that has not changed is compiled with it, and the object compared, byte for byte, with the one the build
-- tool made, for every source of the unit. The first candidate whose objects are all the same is kept; when none is (an option given under a
-- condition that does not hold, a compiler flag from elsewhere, objects older than their sources) nothing is,
-- and the build tool is asked at the unit's first change as before. On a thread: it is a few compiles.
guessC :: S -> IO ()
guessC s = void $ forkIO $ void $ (try :: IO () -> IO (Either SomeException ())) $ do
  mb <- rd (vBoot s)
  forM_ mb $ \b -> do
    units <- cUnits (bLaunch b)
    forM_ [ u | u <- units, not (null (cuC u)) ] $ \u -> do
      known <- cTemplate b u
      when (isNothing known) $ do
        names <- either (\(_ :: IOException) -> []) id <$> try (getDirectoryContents (cuWd u))
        texts <- catMaybes <$> mapM (\n -> readFileMaybe (cuWd u </> n)) [ n | n <- names, ".cabal" `isSuffixOf` n ]
        let ccAll = nub (concatMap (cabalField "cc-options") texts)
            base = ["-package-env=-", "-c", "-fPIC", "-odir", cuOdir u] ++ keep (cuArgs u)
            candidates = nub [ base ++ o ++ map ("-optc" ++) cc | o <- [["-optc-O2"], []], cc <- [[], ccAll] ]
            probe = sDir s </> "cobj" </> ".probe" </> cuId u
            same tmpl (src, obj) = do
              let rel = makeRelative (cuWd u) src
              void (try (removeDirectoryRecursive probe) :: IO (Either IOException ()))
              createDirectoryIfMissing True probe
              (ec, _, _) <- readCreateProcessWithExitCode (proc "ghc" (setOdirTo probe tmpl ++ [rel])) { cwd = Just (cuWd u) } ""
              if ec /= ExitSuccess then pure False else do
                a <- try (B.readFile (probe </> replaceExtension rel "o")) :: IO (Either IOException B.ByteString)
                c <- try (B.readFile obj) :: IO (Either IOException B.ByteString)
                pure (case (a, c) of { (Right x, Right y) -> x == y; _ -> False })
            allM _ [] = pure True
            allM f (x : xs) = f x >>= \b -> if b then allM f xs else pure False
            firstThat [] = pure Nothing
            firstThat (t : ts) = do
              -- (EVERY source: one that does not read an option compiles the same with it and without, and a
              -- command right for that one alone would be kept -- gvt_pty.c against -DGVT_STATIC, tried)
              ok <- allM (same t) (cuC u)
              if ok then pure (Just t) else firstThat ts
        found <- firstThat candidates
        void (try (removeDirectoryRecursive probe) :: IO (Either IOException ()))
        case found of
          Just t -> do
            createDirectoryIfMissing True (bLaunchDir b </> "cc")
            writeAtomic (bLaunchDir b </> "cc" </> cuId u) (unlines t)
            logS s ("C: " ++ cuId u ++ ": how its C is compiled is known (checked: it makes the build tool's own object)")
          Nothing -> logS s ("C: " ++ cuId u ++ ": how its C is compiled was not found by trying; the build tool is asked at its first change")
  where
    -- of a unit's flags, what a C compile reads: where the headers are, and the packages (theirs too)
    keep as = case as of
      (a : v : rest) | a `elem` ["-package-db", "-package-id"] -> a : v : keep rest
      (a : rest) | "-I" `isPrefixOf` a || a `elem` ["-hide-all-packages", "-no-user-package-db"] -> a : keep rest
                 | otherwise -> keep rest
      [] -> []

-- | The values of a field in a @.cabal@ file's text, wherever it is given (under a condition or not).
cabalField :: String -> String -> [String]
cabalField name text =
  concat [ vals (drop (length name + 1) (dropWhile (== ' ') l)) ++ concatMap vals (takeWhile (\c -> indent c > indent l || null (trim c)) rest)
         | (l : rest) <- tailsOf (lines text), (name ++ ":") `isPrefixOf` map toLower (dropWhile (== ' ') l) ]
  where
    indent = length . takeWhile (== ' ')
    vals x = if "--" `isPrefixOf` dropWhile (== ' ') x then [] else words x
    tailsOf [] = []
    tailsOf x@(_ : r) = x : tailsOf r

-- | A compile's arguments with its object directory another.
setOdirTo :: FilePath -> [String] -> [String]
setOdirTo out as = case as of
  ("-odir" : _ : rest) -> "-odir" : out : rest
  (a : rest) -> a : setOdirTo out rest
  [] -> []

-- | A line of the build tool's verbose output as a command's words, if it is one: after @Running: PROGRAM@ or
-- @GHC response file arguments:@, split at spaces, a word in single quotes whole.
ccWords :: String -> [String]
ccWords l
  | Just r <- strip "GHC response file arguments: " = go r
  | Just r <- strip "Running: " = drop 1 (go r)
  | otherwise = []
  where
    strip pre = if pre `isPrefixOf` l then Just (drop (length pre) l) else Nothing
    go str = case dropWhile (== ' ') str of
      [] -> []
      ('\'' : r) -> let (w, r') = break (== '\'') r in w : go (drop 1 r')
      r -> let (w, r') = break (== ' ') r in w : go r'

showG :: Double -> String
showG x = if x == fromIntegral (round x :: Integer) then show (round x :: Integer) else show x

-- the socket ---------------------------------------------------------------------------------

-- the history -----------------------------------------------------------------------
--
-- Every request a client makes and what it was answered, and every save with its verdict, go to the
-- session's history ("GhciSession.History") as a @tool@ line and an @echo@ line: the daemon is the one
-- process that sees them all. A verdict that is routine ("OK -- CHECK-PASS (0.4s)") is a short line, which
-- the tree keeps verbatim at no cost; an error or an evaluation's output goes whole, capped, and is
-- summarized. @status@ and @info@ are not logged: a tool polls them.

histAdd :: S -> String -> T.Text -> IO ()
histAdd s kind t = forM_ (sHist s) $ \m -> void (try (H.appendMsg m (T.pack kind) t) :: IO (Either SomeException Int))

-- | A request as one line: the operation and what matters of its arguments.
describeReq :: String -> Json -> T.Text
describeReq op req = T.pack (unwords (op' : args))
  where
    op' = case op of { "check" -> "test"; "zygote" -> "server"; o -> o }
    args = [ a | Just a <- [lookupStr "action" req] ] ++ [ "-m " ++ m | Just m <- [lookupStr "member" req] ]
        ++ [ "--" ++ m | Just m <- [lookupStr "mode" req], m /= "cafs" ] ++ [ "--top " ++ show (round n :: Int) | Just n <- [lookupNum "top" req] ]
        ++ [ "--no-test" | lookupBool "check" req == Just False ] ++ [ "--no-refork" | lookupBool "refork" req == Just False ]
        ++ [ "--fast" | lookupBool "fast" req == Just True ] ++ [ "--resume" | lookupBool "resume" req == Just True ] ++ [ "--live" | lookupBool "live" req == Just True ]
        ++ [ unwords [ w | JStr w <- lookupArr "words" req ] | op == "doc" ]
        ++ [ "[tool " ++ n ++ "]" | Just n <- [lookupStr "tool" req], not (null n) ]     -- (an eval a project's declared tool made: the history says whose)
        ++ [ e | Just e <- [lookupStr "expr" req], not (null e) ]
        ++ [ f | JStr f <- lookupArr "files" req ]

-- | A reply, whole (a long one is several messages: 'H.appendMsg'), with the stale warning the client was given.
histEcho :: S -> [FilePath] -> T.Text -> IO ()
histEcho s stale out = forM_ (sHist s) $ \_ -> do
  let warn = if null stale then T.empty else T.pack ("[STALE: " ++ show (length stale) ++ " watched file(s) differ from the loaded code: " ++ intercalate ", " (map (rel s) (take 4 stale)) ++ "]\n")
      body = T.strip out
  histAdd s "echo" (warn <> (if T.null body then T.pack "(no output)" else body))

-- | The verdict as the log has it: the status line (with its STALE prefix) and the failing lines.
verdictLine :: S -> IO T.Text
verdictLine s = do
  st <- rd (vStatus s)
  j <- rd (vJson s)
  stale <- if "starting" `isPrefixOf` st then pure [] else staleFiles s
  pure (T.pack (intercalate "\n" ((if null stale then st else "STALE(" ++ show (length stale) ++ ") " ++ st) : take 30 (strs (j .: "detail")))))

-- | What a save changed: the files, and for each a diff against the copy kept since it was last seen
-- (@history/loaded/@, seeded at boot), capped -- the log then says WHAT was edited, not only that a file
-- was. The copies are brought up to date here.
saveLine :: S -> [FilePath] -> IO String
saveLine s changed = do
  let head' = "save: " ++ intercalate ", " (map (rel s) (take 8 changed)) ++ (if length changed > 8 then ", ..." else "")
  case sHist s of
    Nothing -> pure head'
    Just _ -> do
      ds <- forM (take 8 changed) $ \f -> do
        let copy = sDir s </> "history" </> "loaded" </> rel s f
        there <- doesFileExist f
        had <- doesFileExist copy
        d <- if not there then pure "(deleted)"
             else if not had then pure "(new, or not seen before)"
             else do
               out <- rawSystemOut 10 "diff" ["-u", copy, f]
               pure (maybe "(diff failed)" (hunks 60) out)
        void (try (if there then createDirectoryIfMissing True (takeDirectory copy) >> copyFile f copy else removeFile copy) :: IO (Either IOException ()))
        pure (if length changed == 1 then d else "== " ++ rel s f ++ "\n" ++ d)
      pure (intercalate "\n" (head' : filter (not . null) ds))
  where
    -- the hunks of a unified diff, without the two header lines and their times; at most n lines
    hunks n out = let ls = drop 2 (lines out) in intercalate "\n" (take n ls ++ [ "... (" ++ show (length ls - n) ++ " more lines)" | length ls > n ])

-- | The copies the save diffs are taken against: every watched source, once the session is up.
seedLoaded :: S -> IO ()
seedLoaded s = forM_ (sHist s) $ \_ -> void (try go :: IO (Either SomeException ()))
  where
    go = do
      sig <- rd (vLoadedSig s)
      forM_ (M.keys sig) $ \p -> do
        let f = fromRaw p
            copy = sDir s </> "history" </> "loaded" </> rel s f
        there <- doesFileExist copy
        same <- if not there then pure False else (==) <$> modTime f <*> modTime copy
        unless same $ void (try (createDirectoryIfMissing True (takeDirectory copy) >> copyFile f copy) :: IO (Either IOException ()))

-- | Something the watcher did: what, then the verdict it ended on.
histEvent :: S -> String -> IO () -> IO ()
histEvent s what act = do
  histAdd s "tool" (T.pack what)
  act
  verdictLine s >>= histAdd s "echo"

histOps :: [String]
histOps = ["history", "zoom", "date", "view", "log", "pending", "tree_put", "memory", "recall"]

-- | The history's own operations: answered from the daemon's memory, with the repl untouched.
histOp :: S -> String -> Json -> IO (Either String T.Text)
histOp s op req = case sHist s of
  Nothing -> pure (Left "this session keeps no history (\"history\": false)")
  Just m -> case op of
    "log" -> do
      let kind = fromMaybe "" (lookupStr "kind" req)
      if kind `notElem` ["user", "talk", "tool", "echo", "work", "note", "ai", "known"] then pure (Left ("log: the kind must be one of user, talk, tool, echo, work, note, ai, known; not " ++ show kind)) else do
        -- (a date: a message said elsewhere before now, brought in -- `ghci-session import`)
        i <- H.appendMsgAt m (T.pack kind) (fromMaybe T.empty (lookupText "text" req)) (lookupNum "date" req)
        pure (Right (T.pack ("#" ++ show i)))
    "history" -> do
      n <- H.count m
      let want = maybe 40 round (lookupNum "n" req) :: Int
          from = maybe (max 0 (n - want)) round (lookupNum "since" req)
      ms <- H.messages m from want
      if lookupBool "json" req == Just True
        then pure (Right (T.pack (encode (JArr [ JObj [("i", JNum (fromIntegral (H.mId x))), ("kind", JStr (T.unpack (H.mKind x))), ("text", JText (H.mText x)), ("date", JNum (H.mDate x))] | x <- ms ]))))
        else do
          ls <- forM ms $ \x -> do
            d <- stamp (H.mDate x)
            let full = lookupBool "full" req == Just True
                body = if full then H.mText x else let l1 = T.takeWhile (/= '\n') (H.mText x) in (if T.length l1 > 160 then T.take 160 l1 <> T.pack "..." else l1) <> (if T.any (== '\n') (H.mText x) then T.pack " ..." else T.empty)
            pure (T.pack ("#" ++ show (H.mId x) ++ " " ++ d ++ " " ++ T.unpack (H.mKind x) ++ ": ") <> body)
          pure (Right (T.intercalate (T.pack "\n") ls <> (if null ms then T.pack ("no messages" ++ (if n > 0 then " from #" ++ show from else "")) else T.empty)))
    -- the messages that hold a query's words, the best first ("GhciSession.Know": 'K.rank')
    "recall" -> do
      n <- H.count m
      ms <- H.messages m 0 n
      let q = fromMaybe T.empty (lookupText "query" req)
          hits = take (maybe 8 round (lookupNum "n" req)) (K.rank q [ (x, H.mKind x <> T.pack ": " <> H.mText x) | x <- ms, H.mKind x /= T.pack "known" ])
      ls <- forM hits $ \(_, x) -> do
        d <- stamp (H.mDate x)
        pure (T.pack ("#" ++ show (H.mId x) ++ " " ++ d ++ " " ++ T.unpack (H.mKind x) ++ ": ") <> K.snippet 240 q (H.mText x))
      pure (Right (if null ls then T.pack "no message holds those words" else T.intercalate (T.pack "\n") ls))
    "zoom" -> fmap T.stripEnd <$> H.zoom m (maybe (-1) round (lookupNum "id" req)) (maybe 0 round (lookupNum "n" req))
    "date" -> do
      d <- H.dateOf m (maybe (-1) round (lookupNum "id" req))
      case d of
        Nothing -> pure (Left ("no message " ++ maybe "?" (show . (round :: Double -> Int)) (lookupNum "id" req)))
        Just t -> Right . T.pack <$> (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" <$> utcToLocalZonedTime (posixSecondsToUTCTime (realToFrac t)))
    "view" -> do
      -- `wait`: until every line is a summary (a turn starts on a settled view), at most that many seconds --
      -- and not at all where nothing summarizes (no compactor: a turn waited two minutes for what could not come)
      let secs = if isJust (gSummarizeCmd (sCfg s)) then fromMaybe 0 (lookupNum "wait" req) else 0
      t0 <- now
      let go = do
            sn <- H.snapshot m
            t <- now
            if H.settled sn || t - t0 >= secs then pure sn else do
              n <- H.changes m
              tv <- registerDelay (round (min 1.0 (max 0.01 (secs - (t - t0))) * 1e6))
              atomically (H.waitChange m n `orElse` (readTVar tv >>= check))
              go
      sn <- go
      -- (what is known by subject goes before the view: "GhciSession.Know")
      (subjects, since) <- if gKnowledge (sCfg s) then subjectsFor s sn else pure (T.empty, 0)
      let r = subjects <> H.renderViewSince since sn
      pure (Right (if lookupBool "json" req == Just True
        then T.pack (encode (JObj [ ("view", JText r), ("settled", JBool (H.settled sn)), ("parts", JNum (fromIntegral (length (H.sView sn)))), ("messages", JNum (fromIntegral (H.sCount sn))) ]))
        else r))
    -- the memory's numbers, for a monitor: messages, lines of the view, is it settled, nodes built, the
    -- compactor's jobs running and waiting on a retry, and whether one is configured
    "memory" -> do
      sn <- H.snapshot m
      busy <- H.busyCount m
      fails <- H.failedCount m
      pure (Right (T.pack (encode (JObj [ ("messages", JNum (fromIntegral (H.sCount sn))), ("parts", JNum (fromIntegral (length (H.sView sn))))
                                        , ("settled", JBool (H.settled sn)), ("built", JNum (fromIntegral (M.size (H.sSizes sn))))
                                        , ("unbuilt", JNum (fromIntegral (length [ () | p <- H.sView sn, not (M.member p (H.sSizes sn)) ])))
                                        , ("busy", JNum (fromIntegral busy)), ("failed", JNum (fromIntegral fails))
                                        , ("compactor", JBool (isJust (gSummarizeCmd (sCfg s)))) ]))))
    -- for a compactor outside the daemon: the nodes ready to build, with their prompts; and one built
    "pending" -> do
      js <- H.pending m
      let ps = H.params m
      pure (Right (T.pack (encode (JArr [ JObj [("l", JNum (fromIntegral (H.jL j))), ("i", JNum (fromIntegral (H.jI j))), ("prompt", JText (H.jobPrompt ps j))] | j <- js ]))))
    "tree_put" -> case (lookupNum "l" req, lookupNum "i" req, lookupText "text" req) of
      (Just l, Just i, Just t) | not (T.null (T.strip t)) -> H.putNode m (round l) (round i) (T.strip t) >> pure (Right (T.pack "ok"))
      _ -> pure (Left "tree_put: l, i and a text are needed")
    _ -> pure (Left ("unknown op " ++ show op))

-- | The project a session's facts are filed under: its directory's name (the sessions of one checkout are one project).
projectOf :: S -> String
projectOf s = takeFileName (dropTrailing (sRoot s))
  where dropTrailing p = if length p > 1 && last p == '/' then init p else p

-- | The subjects' block of a view ("GhciSession.Know"). It is written when the view is REWRITTEN -- a batch
-- merged its lines, so the prompt a provider cached is lost from there anyway -- and read as it was written
-- while the view only grows: what is learned meanwhile is in the view's own lines (@known@).
subjectsFor :: S -> H.Snap -> IO (T.Text, Int)
subjectsFor s sn = do
  let file = sDir s </> "history" </> "subjects.txt"
      partsFile = sDir s </> "history" </> "subjects-view.json"
      atFile = sDir s </> "history" </> "subjects-at"
      parts = H.sView sn
  was <- readFileMaybe partsFile
  let before = [ (round l, round i) | Just t <- [was], Right (JArr ps) <- [parseJson t], JArr [JNum l, JNum i] <- ps ] :: [(Int, Int)]
  old <- if isJust was && before `isPrefixOf` parts then fmap T.pack <$> readFileMaybe file else pure Nothing
  case old of
    -- (one written when nothing was known is not kept: the first facts are worth a prompt read again)
    -- (the id the block was written at: a block of an older version has none, and no line is left out for it)
    Just t | not (T.null t) -> do
      at <- readFileMaybe atFile
      pure (t, fromMaybe 0 (at >>= readMaybe . takeWhile isDigit))
    _ -> do
      dir <- K.knowDir
      facts <- either (const []) id <$> (try (K.loadFacts dir) :: IO (Either SomeException [K.Fact]))
      let t = K.renderFor (Mcp.toolArgs agentTools) K.budget (projectOf s) facts
      void (try (writeFileUtf8 file (T.unpack t) >> writeFileUtf8 partsFile (encode (JArr [ JArr [JNum (fromIntegral l), JNum (fromIntegral i)] | (l, i) <- parts ])) >> writeFileUtf8 atFile (show (H.sCount sn))) :: IO (Either IOException ()))
      pure (t, if T.null t then 0 else H.sCount sn)

-- | The tools an agent has: the chat's own, then the others the MCP server has (a name once).
agentTools :: [Mcp.Tool]
agentTools = Chat.chatTools ++ [ t | t <- Mcp.tools, Mcp.tName t `notElem` map Mcp.tName Chat.chatTools ]

-- | What the session establishes, kept by subject ("GhciSession.Know"), through the compactor's command. The
-- log is read once, in order, from where it was left (@history\/know.json@): a message of the user's, an
-- agent's or a note is asked for its facts as it is; the tools' traffic is asked a hundred and twenty-eight
-- messages at a time, as the lines the compactor made of them (thirty-two messages late, so that they are
-- made); and an argument a tool is called with for the first time is a fact with no model asked. New facts
-- are set against the nearest that hold -- new, said again, or replacing one -- and one that is new or
-- replaces is appended to the history as a @known@ line: the view's block of subjects is not rewritten for it.
knowLoop :: S -> H.Mem -> String -> IO ()
knowLoop s m cmd = do
  dir <- K.knowDir
  let stateFile = sDir s </> "history" </> "know.json"
      project = projectOf s
      src i n = project ++ "/" ++ sName s ++ ":" ++ show i ++ "+" ++ show n
  st <- readFileMaybe stateFile
  let upto0 = case st of { Just t | Right j <- parseJson t, Just n <- lookupNum "upto" j -> round n; _ -> 0 } :: Int
  seenArgs <- do
    fs <- either (const []) id <$> (try (K.loadFacts dir) :: IO (Either SomeException [K.Fact]))
    newIORef [ a | f <- fs, Just a <- [K.isCall (K.fTopic f)] ]
  failing <- newIORef False
  behind <- newIORef False            -- facts were stored that the session was not told of: its block is to be written again
  let ask prompt = do
        r <- runShell [(Llm.usageFileEnv, sDir s </> "usage.jsonl"), ("SUMMARIZE_TOOLS", "0")] cmd prompt 300
        case r of
          Just (ExitSuccess, out, _) -> failing =: False >> pure (Just out)
          _ -> do
            was <- rd failing
            unless was (logS s "knowledge: the command failed; asked again in a minute")
            failing =: True
            pure Nothing
      -- new facts into the store; True when it was done (False: the model could not be asked -- again later)
      -- (@recent@: the facts are of the log's last messages, so the session is told of them as they are learned.
      -- Facts of long ago -- a history read from its start -- are not news at its end: they go to the block.
      -- A message is recent by its place (among the last 256) and by its date (within the last hour): an import
      -- brings days-old messages in at the log's end, and by place alone their facts were appended as known lines)
      settle :: Bool -> [K.New] -> IO Bool
      settle _ [] = pure True
      settle recent news = do
        held <- K.current <$> K.loadFacts dir
        let cands = nubBy (\a b -> K.fId a == K.fId b) (concatMap (K.candidates held) news)
        answer <- if null cands then pure (Just T.empty) else ask (K.reconPrompt cands news)
        case answer of
          Nothing -> pure False
          Just out -> do
            let decs = if null cands then map (const K.Add) news else K.parseDecisions (length news) (length cands) out
                at ks = [ cands !! (k - 1) | k <- ks ]
            forM_ (zip news decs) $ \(n, d) -> case d of
              K.Drop -> pure ()
              K.Same ks -> K.withLock dir (forM_ (at ks) (\f -> K.markSeenFrom dir (K.fId f) (K.nDate n) (K.nSrc n)))
              _ -> do
                let replaced = case d of { K.Replace ks -> at ks; _ -> [] }
                i <- K.newId
                K.withLock dir $ do
                  K.addFact dir (K.Fact i (K.nSubject n) (K.nTopic n) (K.nText n) (K.nDate n) (K.nDate n) (K.nSrc n) (map K.fId replaced) Nothing 0)
                  forM_ replaced (\f -> K.markBy dir (K.fId f) i)
                if recent then histAdd s "known" (K.knownLine n replaced) else behind =: True
            fold
            logS s (printf "knowledge: %d fact(s): %s" (length news) (unwords [ case d of { K.Add -> "new"; K.Same _ -> "said-again"; K.Replace _ -> "replaces"; K.Drop -> "dropped" } | d <- decs ]))
            pure True
      -- a subject grown past its size: the half of its facts least recently confirmed, written again as a few
      -- (the folded ones are kept, marked as replaced by the first of them)
      fold = do
        limit <- maybe K.foldLimit (\v -> case reads v of { [(k, "")] -> k; _ -> K.foldLimit }) <$> lookupEnv "GHS_KNOWLEDGE_FOLD"
        facts <- K.loadFacts dir
        forM_ (K.foldDue limit facts) $ \(subject, old) -> do
          out <- ask (K.foldPrompt subject old)
          forM_ out $ \o -> case K.parseFold o of
            [] -> logS s ("knowledge: " ++ T.unpack subject ++ " was not folded (no facts in the answer)")
            texts -> do
              ids <- mapM (const K.newId) texts
              K.withLock dir $ do
                forM_ (zip3 [0 :: Int ..] ids texts) $ \(k, i, t) ->
                  K.addFact dir (K.Fact (i ++ "-f" ++ show k) subject (T.pack "folded from older facts") t (minimum (map K.fFirst old)) (maximum (map K.fLast old)) (sName s ++ ":fold") (if k == 0 then map K.fId old else []) Nothing (sum (map K.fSeen old)))
                forM_ old (\f -> K.markBy dir (K.fId f) (head ids ++ "-f0"))
              logS s (printf "knowledge: %s: %d older fact(s) folded into %d" (T.unpack subject) (length old) (length texts))
      piece recent date sr body = do
        out <- ask (K.extractPrompt (map Mcp.toolBrief agentTools) project date body)
        case out of
          Nothing -> pure False
          Just o -> settle recent (K.parseNew project date sr o)
      -- the lines the compactor made of a stretch of messages, from level four down to what is built
      stretch from to = do
        texts <- H.treeTexts m
        let line l i | Just t <- M.lookup (l, i) texts = [T.pack (show (i * 2 ^ l) ++ "+" ++ show (2 ^ l :: Int) ++ "|") <> T.map (\c -> if c == '\n' then ' ' else c) t]
                     | l == 0 = []
                     | otherwise = line (l - 1) (2 * i) ++ line (l - 1) (2 * i + 1)
        pure (T.unlines (concat [ line 4 i | i <- [from `div` 16 .. (to - 1) `div` 16] ]))
      step i = do
        ms <- H.messages m i 1
        count <- H.count m
        t <- now
        let recent = i >= count - 256 && any (\x -> t - H.mDate x <= 3600) (take 1 ms)
        ok1 <- case ms of
          -- (ai: what another agent said, brought in by `import`)
          (x : _) | H.mKind x `elem` map T.pack ["user", "talk", "note", "ai"], T.length (H.mText x) > 80 ->
                      piece recent (H.mDate x) (src i 1) (H.mKind x <> T.pack ": " <> T.take 6000 (H.mText x))
                  | H.mKind x == T.pack "tool", Just (tool, keys) <- K.callArgs (H.mText x) -> do
                      known <- rd seenArgs
                      let fresh = [ k | k <- keys, (tool, k) `notElem` known ]
                      seenArgs =: (map ((,) tool) fresh ++ known)
                      forM_ fresh $ \k -> do
                        let n = K.callFact tool k (H.mDate x) (src i 1) (H.mText x)
                        fid <- K.newId
                        K.withLock dir (K.addFact dir (K.Fact (fid ++ "-" ++ k) (K.nSubject n) (K.nTopic n) (K.nText n) (K.nDate n) (K.nDate n) (K.nSrc n) [] Nothing 0))
                      pure True
          _ -> pure True
        -- a stretch of 128 messages that ended 32 ago
        let end = i + 1 - 32
        ok2 <- if not ok1 || end <= 0 || end `mod` 128 /= 0 then pure ok1 else do
          body <- stretch (end - 128) end
          d <- H.dateOf m (end - 1)
          if T.null (T.strip body) then pure True else piece recent (fromMaybe 0 d) (src (end - 128) 128) body
        pure ok2
      loop upto = do
        stopping <- rd (vStopping s)
        unless stopping $ do
          n <- H.changes m
          count <- H.count m
          if upto < count
            then do
              ok <- either (\e -> logS s ("knowledge: " ++ displayException (e :: SomeException)) >> pure True) pure =<< try (step upto)
              if ok then void (try (writeFileUtf8 stateFile (encode (JObj [("upto", JNum (fromIntegral (upto + 1)))]))) :: IO (Either IOException ())) >> loop (upto + 1)
                    else threadDelay 60000000 >> loop upto
            else do
              was <- rd behind
              when was $ do
                behind =: False
                void (try (removeFile (sDir s </> "history" </> "subjects-view.json")) :: IO (Either IOException ()))
              tv <- registerDelay 10000000
              atomically (H.waitChange m n `orElse` (readTVar tv >>= check))
              loop upto
  loop upto0

-- | The compactor, through the configured command (@summarize_cmd@): it reads the system prompt (its own,
-- 'H.compactPrompt'; or with @"compact_prompt": "shared"@ the one a turn has, 'H.systemPrompt', and then
-- the command sends the turns' tools too), its view and the task on its standard input and answers the
-- line on its standard output. An answer that is no line ('H.junkLine') is asked for again. Up to @summarize_jobs@ at once; a line over the size is asked again with the line cut
-- where the limit falls, up to five times, and the shortest try is kept (a few bytes over is fine: the
-- view measures real sizes); a failed node is tried again after ten seconds, for ever, and only its first
-- failure is logged. (UniiChat's retry goes on in the same conversation; a command has none, so the
-- earlier answer and the note are appended to the prompt instead -- at its end, so the prefix a provider
-- caches is the same.)
--
-- Many at once ask one endpoint, so two things are decided here for all of them and not by each.
--
-- * __One goes first when the prompt's cache is cold.__ Every compaction starts with the same prompt and nearly
--   the same view, which a provider caches once a call with it has been answered. Sixty-four started together
--   find no entry and each pays for the whole prompt (and where a cache write is charged, for writing it). So
--   when no compaction has been answered in four minutes -- an entry lives five -- or the view is not the one
--   the last was answered with (it was merged), ONE call is made and the rest wait for its answer.
-- * __None is started while the endpoint is failing.__ A node whose command failed is tried again ten seconds
--   later, by itself: all of them did, together, every ten seconds, at a service that had just said it was
--   busy. After a failure no call is started for 5 s, then 10, 20, ... up to five minutes, until one succeeds.
compactorLoop :: S -> H.Mem -> String -> IO ()
compactorLoop s m cmd = do
  pauseV <- newIORef (0 :: Double, 0 :: Int)                       -- no call before this time; the failures in a row
  warmV <- newIORef (Nothing :: Maybe (Double, [T.Text]))          -- when a compaction was last answered, and its view
  leadV <- newIORef False                                          -- the one call that goes first is out
  let loop = do
        stopping <- rd (vStopping s)
        unless stopping $ do
          n <- H.changes m
          busy <- H.busyCount m
          jobs <- H.pending m
          t <- now
          (until, _) <- rd pauseV
          leading <- rd leadV
          warm <- rd warmV
          let room = take (max 0 (gSummarizeJobs (sCfg s) - busy)) jobs
              cold j = case warm of
                Nothing -> True
                Just (at, ctx) -> t - at > 240 || not (sameView ctx (H.jContext j))
              (chosen, lead) | t < until || leading = ([], False)
                             | otherwise = case room of
                                 (j : _) | cold j -> ([j], True)
                                 js -> (js, False)
          when lead (leadV =: True)
          when (lead && length jobs > 1) (logS s ("summarize: the prompt's cache is cold: one call first, " ++ show (length jobs - 1) ++ " after its answer"))
          forM_ chosen $ \j -> do
            H.claim m (H.jL j, H.jI j)
            void $ forkIO $ do
              r <- try (runJob s m cmd j) :: IO (Either SomeException Bool)
              t1 <- now
              when lead (leadV =: False)
              case r of
                Right True -> pauseV =: (0, 0) >> warmV =: Just (t1, H.jContext j)
                Right False -> do
                  (_, k) <- rd pauseV
                  let wait = min 300 (5 * 2 ^ min 8 k) :: Double
                  pauseV =: (t1 + wait, k + 1)
                  when (k == 0 || wait >= 300) (logS s ("summarize: the command failed: no call for " ++ showG wait ++ " s"))
                Left e -> H.release m (H.jL j, H.jI j) >> logS s ("summarize: " ++ displayException e)
          tv <- registerDelay (if lead || t < until then 1000000 else 10000000)
          atomically (H.waitChange m n `orElse` (readTVar tv >>= check))
          loop
  loop
  where
    -- the same view, but for where each ends (a node's view stops at the node): all but their last lines agree
    sameView a b = let k = min (length a) (length b) - 80 in k <= 0 || take k a == take k b

-- | One node built by the command: did the command RUN (whatever it answered)? A failed or timed-out one has
-- been marked to be tried again.
-- | What has been seen of the compactor's answers, by kind of line (False: a message compressed; True: two
-- lines merged) -- what the next is asked by ('H.Ask').
{-# NOINLINE askSeenV #-}
askSeenV :: IORef (M.Map Bool H.Ask)
askSeenV = unsafePerformIO (newIORef M.empty)

runJob :: S -> H.Mem -> String -> H.Job -> IO Bool
runJob s m cmd j = go0
  where
    ps0 = H.params m
    merge = H.jL j > 0
    askFile = sDir s </> "history" </> "ask.json"
    go0 = do
      -- (what was learnt of the answers is kept with the history: a session started again asked as if it had
      -- seen none, and learnt it over again from a hundred lines that came back too long)
      known <- rd askSeenV
      when (M.null known) $ do
        t <- readFileMaybe askFile
        forM_ (t >>= either (const Nothing) Just . parseJson) $ \j ->
          askSeenV =: M.fromList [ (k == "merge", H.Ask m sp ms (round n) (round u))
                                 | (k, v) <- lookupObj "seen" (JObj [("seen", j)]), Just [m, sp, ms, n, u] <- [mapM num (lookupArr "a" (JObj [("a", v)]))] ]
      seen <- M.findWithDefault (H.askStart ps0) merge <$> rd askSeenV
      let p = (H.jL j, H.jI j)
          shared = gSharedPrompt (sCfg s)
          -- (asked for as the answers have been coming: so many bytes, so strongly)
          (bytes, urge) = H.askFor ps0 seen
          ps = ps0 { H.pAsk = bytes, H.pUrge = urge }
          base = (if shared then H.systemPrompt else H.compactPrompt) (gAgent (sCfg s)) <> T.pack "\n" <> H.jobPrompt ps j
          -- a first answer is what the next line is asked by
          note line = do
            (was, new) <- atomicModifyIORef' askSeenV (\mp ->
              let { a0 = M.findWithDefault (H.askStart ps0) merge mp; a1 = H.askSeen ps0 bytes (H.byteLength line) a0 } in (M.insert merge a1 mp, (a0, a1)))
            let (b0, u0) = H.askFor ps0 was
                (b1, u1) = H.askFor ps0 new
            when (H.aSeen new `mod` 10 == 0) $ do
              mp <- rd askSeenV
              void (try (writeAtomic askFile (encode (JObj [ (if k then "merge" else "message", JArr (map JNum [H.aMean a, H.aSpread a, H.aMiss a, fromIntegral (H.aSeen a), fromIntegral (H.aUrge a)])) | (k, a) <- M.toList mp ]))) :: IO (Either SomeException ()))
            -- (said when the wording changes, or the size by more than a step or two: not for every twenty bytes)
            when (u0 /= u1 || abs (b0 - b1) >= 60 || (b0 /= b1 && H.aSeen new `mod` 50 == 0)) $
              logS s (printf "compactor: a %s is now asked for in %d bytes%s (answers come at %.2f of what is asked, give or take %.2f; %d%% over the %d taken; %d seen)"
                        (if merge then "merge" else "message's line" :: String) b1 (case u1 of { 0 -> ""; 1 -> ", more strongly"; _ -> ", most strongly" } :: String)
                        (H.aMean new) (H.aSpread new) (round (100 * H.aMiss new) :: Int) (H.pNode ps0) (H.aSeen new))
          go :: IORef Bool -> Int -> [T.Text] -> T.Text -> IO [T.Text]
          go bad n tries extra
            | n >= (5 :: Int) = pure tries
            | otherwise = do
                r <- runShell ((Llm.usageFileEnv, sDir s </> "usage.jsonl") : [ ("SUMMARIZE_TOOLS", "0") | not shared ]) cmd (base <> extra) 300
                case r of
                  Just (ExitSuccess, out, _) | not (T.null (T.strip out)) -> do
                    let line = T.strip (H.stripHead (T.takeWhile (/= '\n') (T.strip out)))
                    when (n == 0 && not (H.junkLine line)) (note line)
                    if H.junkLine line then pure tries        -- (no line: not asked again -- the tries so far, or the input cut)
                      else if H.nodeFits ps line then pure (tries ++ [line])
                      else go bad (n + 1) (tries ++ [line]) (T.pack "\n\nYour earlier answer:\n" <> line <> T.pack "\n\n" <> H.retryNote ps line)
                  Just (ExitSuccess, _, _) -> pure tries     -- (it ran and said nothing: the same)
                  Just (code, _, err) -> do
                    bad =: True
                    first <- H.failed m p 10
                    when first (logS s ("summarize " ++ show p ++ ": the command failed (" ++ show code ++ "): " ++ take 300 (T.unpack (T.strip err))))
                    pure tries
                  Nothing -> do
                    bad =: True
                    first <- H.failed m p 10
                    when first (logS s ("summarize " ++ show p ++ ": the command timed out"))
                    pure tries
      bad <- newIORef False
      tries <- go bad 0 [] T.empty
      ran <- not <$> rd bad
      case tries of
        -- the command ran and its answer was no line (the task said back, a tag alone, nothing): the node is its
        -- input cut at the size (what it compresses, flat), at once -- a model that answers so once answers so
        -- again, and asking five times was five calls for the same cut. A node that is never built holds up every
        -- merge above it, and the view's batch with them; a cut line is a poor summary and the tree goes on.
        [] | ran -> do
          let src = case H.jStep j of { H.Compress msg -> msg; H.Merge a b -> a <> T.pack " " <> b }
          logS s ("summarize " ++ show p ++ ": no line in the answer; its input is kept, cut")
          H.putNode m (H.jL j) (H.jI j) (H.cutNode ps src)
        -- (a command that failed or timed out has said so, and is tried again: the endpoint may come back)
        [] -> pure ()
        _ -> H.putNode m (H.jL j) (H.jI j) (H.fitNode ps (snd (minimum [ (H.byteLength t, t) | t <- tries, not (T.null t) ])))
      pure ran

-- | A shell command with text on its standard input: its exit status, output and errors (UTF-8), or
-- 'Nothing' when it ran past the timeout (it is then stopped).
runShell :: [(String, String)] -> String -> T.Text -> Double -> IO (Maybe (ExitCode, T.Text, T.Text))
runShell extraEnv cmd input secs = do
  env0 <- getEnvironment
  (Just i, Just o, Just e, ph) <- createProcess (shell cmd) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe, close_fds = True
                                                             , env = Just ([ kv | kv@(k, _) <- env0, k `notElem` map fst extraEnv ] ++ extraEnv) }
  mapM_ (`hSetBinaryMode` True) [i, o, e]
  ov <- newEmptyMVar
  ev <- newEmptyMVar
  _ <- forkIO (B.hGetContents o >>= putMVar ov)
  _ <- forkIO (B.hGetContents e >>= putMVar ev)
  void (try (B.hPut i (TE.encodeUtf8 input) >> hClose i) :: IO (Either IOException ()))
  code <- timeout (round (secs * 1e6)) (waitForProcess ph)
  case code of
    Nothing -> do
      terminateProcess ph
      void (timeout 2000000 (waitForProcess ph))
      pure Nothing
    Just c -> do
      out <- takeMVar ov
      err <- takeMVar ev
      pure (Just (c, decode out, decode err))

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
  line <- BC.hGetLine h
  let reply ok out = do
        stale <- staleFiles s
        j <- rd (vJson s)
        ck <- rd (vChecking s)
        -- (while a check runs: when its reload began, when the check did, and what the load compiled to)
        let checking = [ ("checking", JObj [("began", JNum b), ("since", JNum c), ("compiled", JStr p)]) | Just (b, c, p) <- [ck] ]
        -- bytes straight to the socket: an evaluation's output can be megabytes
        B.hPut h (encodeBS (JObj ([ ("ok", JBool ok), ("out", JText out), ("stale", JArr (map JStr (take 6 stale))), ("status", j) ] ++ checking)))
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
        -- an evaluation under way is interrupted, as one that runs out of time is: it ends with what it printed
        -- and "Interrupted.", and the session answers the next request. `from`: only if it is that asker's (a
        -- chat whose turn was stopped does not end an evaluation someone else is waiting for). Not through the
        -- work lock: the evaluation holds it.
        "interrupt" -> do
          running <- rd evalNow
          case (running, lookupStr "from" req) of
            (Nothing, _) -> replyS True "no evaluation is running"
            (Just who, Just me) | who /= me -> replyS True "the evaluation running is another's"
            _ -> do
              theRepl s >>= replInterrupt
              logS s "interrupt: the evaluation under way was interrupted (asked)"
              replyS True "interrupted"
        -- a batch of edits: the watcher waits (it must not take the work lock: a reload may be running) ...
        "hold" -> do
          let secs = max 1 (min 600 (fromMaybe 30 (lookupNum "secs" req)))
          t <- now
          vBatch s =: Just (t + secs)
          vLastUsed s =: t
          logS s ("hold: saves are not reloaded for up to " ++ showG secs ++ " s, until `release`")
          replyS True ("holding: saves are not reloaded for up to " ++ showG secs ++ " s; `release` reloads them once")
        -- ... and `release` reloads what was written, once, and answers with that verdict
        "release" -> viaWork op req      -- (the hold stays until the reload is done: the watcher must not queue the same one)
        "doc" -> do          -- (reads the sources, not the repl: answers during a reload)
          logged req (histAdd s "tool" (describeReq op req))
          out <- docSearch s req
          logged req (histEcho s [] out)
          reply True out
        _ | op `elem` histOps -> histOp s op req >>= either (replyS False) (reply True)
        "stop" -> do
          forM_ (lookupStr "reason" req) (vStopReason s =:)
          vKeepServers s =: fromMaybe False (lookupBool "keep_servers" req)
          stopNow s
          replyS True "stopping"
        -- (the answer there was, when no source changed since: without waiting for the repl, which a
        --  running check holds -- four typechecks of an agent's waited 226 s each behind one)
        -- (the typecheck of the sources as they are, if it has been made -- the watcher makes it first, under the
        --  work lock, which it keeps for the reload: a save's answer can come with it, and a later eval queues behind
        --  the reload. Empty when it has not been made. No lock, no repl.)
        "typecheck_cached" -> typecheckCached s >>= reply True . fromMaybe T.empty
        "typecheck" -> typecheckCached s >>= \c -> case c of
          Just out -> logged req (histAdd s "tool" (describeReq op req) >> histEcho s [] out) >> reply True out
          Nothing -> viaWork op req
        _ -> viaWork op req
      case r of
        Left (e :: SomeException) -> void (try (replyS False (displayException e)) :: IO (Either SomeException ()))   -- a broken eval must not kill the daemon
        Right () -> pure ()
  where
    -- a request with "quiet" is not logged: its client logs it itself, as its agent saw it (the chat)
    logged req act = unless (lookupBool "quiet" req == Just True) act
    -- a request that needs the repl: under the work lock, logged with its answer
    viaWork op req = do
      now >>= (vLastUsed s =:)
      logged req (histAdd s "tool" (describeReq op req))
      r' <- try (bracket_ (modifyIORef' (vBusy s) (+ 1)) (modifyIORef' (vBusy s) (subtract 1) >> now >>= (vLastUsed s =:)) (dispatch s op req))
      case r' of
        Left (e :: SomeException) -> logged req (histAdd s "echo" (T.pack ("ERROR: " ++ displayException e))) >> throwIO e
        Right Nothing -> logged req (histAdd s "echo" (T.pack ("unknown op " ++ show op))) >> replyS' False ("unknown op " ++ show op)
        Right (Just out) -> do
          stale <- staleFiles s
          logged req (histEcho s stale out)
          reply' True out
    reply' ok out = do
      stale <- staleFiles s
      j <- rd (vJson s)
      ck <- rd (vChecking s)
      let checking = [ ("checking", JObj [("began", JNum b), ("since", JNum c), ("compiled", JStr p)]) | Just (b, c, p) <- [ck] ]
      B.hPut h (encodeBS (JObj ([ ("ok", JBool ok), ("out", JText out), ("stale", JArr (map JStr (take 6 stale))), ("status", j) ] ++ checking)))
      B.hPut h (BC.pack "\n")
      hFlush h
    replyS' ok = reply' ok . T.pack

-- | The evaluation under way, if one is: who asked for it (their tag; "" for none given).
{-# NOINLINE evalNow #-}
evalNow :: IORef (Maybe String)
evalNow = unsafePerformIO (newIORef Nothing)

dispatch :: S -> String -> Json -> IO (Maybe T.Text)
dispatch s op req = withMVar (vWork s) $ \_ -> case op of    -- eval is inside the lock too: its answer must not straddle a reload
  "eval" -> do
    -- (who asked is kept while it runs, for `interrupt`: only an evaluation is ever interrupted, and only its asker's)
    r <- try (bracket_ (evalNow =: Just (fromMaybe "" (lookupStr "from" req))) (evalNow =: Nothing)
               (cmd s (lookupNum "timeout" req >>= \t -> if t > 0 then Just t else Nothing) (fromMaybe "" (lookupStr "expr" req))))
    out <- case r of
      Right o -> pure o
      -- It ran out of time and did not stop when interrupted (a loop that does not allocate cannot be): the
      -- session would run it to its end before answering anything -- minutes, for an agent that then asks
      -- again and waits behind it. So it is ended the one way there is, a restart, and the answer says so.
      Left (ReplTimeout t False _) | gRestartStuck (sCfg s) -> do
        logS s "eval: it ran out of time and did not stop when interrupted: the repl is restarted to end it"
        v <- restart s (Just True)
        throwIO (ReplDied (printf "timed out after %ds, and it did not stop when interrupted (a loop that does not allocate cannot be). The session was restarted to end it and is ready again: %s\n[the code loaded is as it was; what was bound at the prompt is gone. Run it on less, or with a larger timeout -- and if it is the library's own loop, it is interpreted unless the session has \"optimize\": true]" (round t :: Int) (takeWhile (/= '\n') v)))
      Left e -> throwIO e
    vEvaluated s =: True
    warmAsync s          -- the unlink and its GC, once this answer is out: they are not the caller's to wait for
    pure (Just out)
  "reload" -> Just <$> reload s (fromMaybe True (lookupBool "check" req)) (fromMaybe True (lookupBool "refork" req)) (lookupBool "async_refork" req)
  "typecheck" -> Just <$> typecheckSources s
  "release" -> (`finally` (vBatch s =: Nothing)) $ do
    cur <- scan (sRoot s) (gWatch (sCfg s)) (gWatchExt (sCfg s))
    loaded <- rd (vLoadedSig s)
    if cur == loaded then pure (Just (T.pack "released: nothing changed since the last load")) else do
      applyChanges s id cur
      v <- verdictLine s
      pure (Just (T.pack "released: reloaded once\n" <> v))
  -- the members chosen since it started, taken by the running repl if they can be ('addMembers')
  "add_members" -> do
    r <- addMembers s
    pure (Just (either (\why -> T.pack ("RESTART-NEEDED: " ++ why)) id r))
  -- units added to the running repl (the engine's GhsAddUnits), then loaded by a reload: no restart
  "add_units" -> do
    Reply j out <- theRepl s >>= \rp -> replQueryOut rp (Just 120) "add_units" [("files", JArr [ JStr f | JStr f <- lookupArr "files" req ])]
    case lookupStr "error" j of
      Just e -> pure (Just (T.pack ("add-unit: " ++ e) <> out))
      Nothing -> do
        logS s ("added to the running repl: " ++ unwords [ u | JStr u <- lookupArr "units" j ])
        vLastLoad s =: Nothing        -- (the engine has targets it has not loaded: this reload is a real one)
        v <- reload s False False Nothing
        pure (Just (T.pack ("added " ++ unwords [ u | JStr u <- lookupArr "units" j ] ++ "\n") <> v))
  -- what the heap holds, and what an action costs (the engine's own: see its `census` and `bench`)
  _ | op `elem` ["census", "bench"] -> do
    Reply j out <- theRepl s >>= \rp -> replQueryOut rp (lookupNum "timeout" req >>= \t -> if t > 0 then Just t else Nothing) op
                     ([ ("mode", JStr (fromMaybe "cafs" (lookupStr "mode" req))), ("expr", JStr (fromMaybe "" (lookupStr "expr" req))) ]
                      ++ maybe [] (\n -> [("top", JNum n)]) (lookupNum "top" req) ++ [ ("live", JBool True) | lookupBool "live" req == Just True ]
                      ++ maybe [] (\n -> [("runs", JNum n)]) (lookupNum "runs" req))
    vEvaluated s =: True
    warmAsync s
    -- a bench that did no work timed a value already evaluated (a top-level value is computed once per
    -- load): say so, or the answer reads as "free" (22 of an agent's 89 benches were this, and it then timed
    -- its tests in fresh GHCi processes through the shell, minutes each)
    let idle = op == "bench" && T.isInfixOf (T.pack ": 0.00 s wall") out && T.isInfixOf (T.pack " 0 MB allocated") out
        note = if idle then T.pack "\n[bench: no work was done -- the value was already evaluated (a top-level value, a CAF, is computed once per load and kept). Time a function applied to its input, or a value built inside the action; `reload` recomputes CAFs only for modules it recompiles.]" else T.empty
    pure (Just (T.dropWhileEnd (== '\n') out <> note <> maybe T.empty (\e -> T.pack ("\n" ++ e)) (lookupStr "error" j)))
  -- one expression run as a test (a group of the target's): its failing lines by the target's own fail
  -- pattern, its passing ones by the pass pattern -- and the session's verdict left as it is
  "check_expr" -> do
    let es = [ e | e <- gChecks (sCfg s), maybe True (\m -> m == ckMember e || m == takeWhile (/= ':') (ckMember e)) (lookupStr "member" req) ]
        expr = fromMaybe "" (lookupStr "expr" req)
        -- (a test is not a probe: where no time is given it has five minutes, not an evaluation's thirty seconds)
        tmo = Just (case lookupNum "timeout" req of { Just t | t > 0 -> t; _ -> max 300 (gEvalTimeout (sCfg s)) })
    t0 <- now
    -- (a command that did not stop when interrupted is still running: this one waits behind it, and says so)
    owed <- either (\(_ :: ReplError) -> 0) id <$> try (theRepl s >>= replOwed)
    r0 <- try (cmd s tmo expr)
    -- (out of time and not stopped by the interrupt: ended by a restart, as an evaluation is -- the session ran it
    -- to its end, with whatever was asked next waiting behind it)
    r <- case r0 of
      Left (ReplTimeout t False _) | gRestartStuck (sCfg s) -> do
        logS s "test: it ran out of time and did not stop when interrupted: the repl is restarted to end it"
        v <- restart s (Just True)
        pure (Left (ReplDied (printf "timed out after %ds, and it did not stop when interrupted. The session was restarted to end it and is ready again: %s\n[give it more time with timeout: N if it is only long]" (round t :: Int) (takeWhile (/= '\n') v))))
      other -> pure other
    t1 <- now
    vEvaluated s =: True
    warmAsync s
    case r of
      Left (e :: ReplError) -> pure (Just (T.pack (printf "SCOPED-TEST: %s (the session's verdict is not changed)%s" (show e)
        (case e of
           ReplTimeout _ False _ -> "\n[it is still running in the session and did not stop when interrupted: the next command waits behind it until it ends -- `restart` ends it]" :: String
           _ -> ""))))
      Right out -> do
        let ls = T.lines out
        fails <- maybe (pure []) (`linesMatching` ls) (listToMaybe es >>= ckFail)
        passes <- maybe (pure []) (`linesMatching` ls) (listToMaybe es >>= ckPass)
        let summary = scopedSummary (listToMaybe es) out (length fails) (length passes)
            waited = if owed > 0 then printf "[the session was still running %d earlier command(s) that did not stop when interrupted: this one waited behind them]\n" owed else "" :: String
        pure (Just (T.pack waited <> out <> T.pack (printf "\n[scoped test, %.1fs: %s -- the session's verdict is not changed]" (t1 - t0) (summary :: String))
                         -- (the failing lines again only when the output is too long to read them in)
                         <> (if length ls > 40 then T.concat [ T.pack "\n  " <> f | f <- take 30 fails ] else T.empty)))
  "check" -> do
    out <- runCheck s Nothing (lookupStr "member" req)
    warmAsync s
    pure (Just out)
  "restart" -> do      -- asked for by hand: through the build tool unless --fast, and on the configuration as it is now
    said <- reconfigure s
    v <- restart s (Just (fromMaybe False (lookupBool "fast" req) && isNothing said))
    when (isJust said) (rewatch s)
    pure (Just (T.pack (maybe "" (++ "\n") said ++ v)))
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

-- | Every package under these units in the build tool's plan (@plan.json@: what each depends on, package
-- by package or component by component), the units themselves left out.
unitsBelow :: Json -> [String] -> [String]
unitsBelow plan roots = filter (`notElem` roots) (go [] roots)
  where
    deps u = nub (concat [ strs (e .: "depends") ++ concat [ strs (c .: "depends") | (_, c) <- fields (e .: "components") ]
                         | e <- lookupArr "install-plan" plan, lookupStr "id" e == Just u ])
    fields j = case j of { JObj kvs -> kvs; _ -> [] }
    go seen [] = seen
    go seen (u : rest)
      | u `elem` seen = go seen rest
      | otherwise = go (u : seen) (deps u ++ rest)

-- | __Objects another session has already compiled.__ Each session keeps its objects in a directory of its
-- own (two of them running must not write one file), so a session started after a day's work in another
-- compiled all of that work again: `mm` after `dev`, 37 s of a 52 s start for modules `dev` held compiled.
-- Before the engine starts, each unit's object directory takes, module by module, the interface and object
-- of a SIBLING session (the same unit's directory under another session's name) whose interface is newer
-- than its own, with their times. Nothing is assumed of them: the compiler checks an interface against the
-- source, the flags and what it imports before it uses the object, and compiles the module when it does
-- not fit -- a copy that is no use costs only the copy. A pair written in the last two seconds is left (a
-- session may be writing it).
seedObjects :: S -> Launch -> IO ()
seedObjects s l = void (try go :: IO (Either SomeException ()))
  where
    go = do
      files <- forM [ f | ('@' : f) <- lArgs l ] (fmap (maybe [] lines) . readFileMaybe)
      let wds = nub ([ (w, srcDirs ls) | ls <- files, (k, w) <- zip ls (drop 1 ls), k == "-working-dir" ] ++ [ (lCwd l, srcDirs (lArgs l)) | null files ])
          -- (a unit's source directories: its -i flags)
          srcDirs ls = [ d | ('-' : 'i' : d) <- ls, not (null d) ]
          stateRel = takeDirectory (takeDirectory (sObjRel s))        -- <state>/<session>/obj
      t <- getCurrentTime
      took <- forM wds $ \(wd, dirs) -> do
        let mine = wd </> sObjRel s
        names <- either (\(_ :: IOException) -> []) id <$> try (listDirectory (wd </> stateRel))
        sibs <- filterM doesDirectoryExist [ wd </> stateRel </> n </> "objs" | n <- names, n /= sName s, sameFlags (sConf s) (sName s) n ]
        his <- concat <$> forM sibs (\d -> map ((,) d) <$> hiFiles d "")
        -- the newest interface of each module among the siblings, if newer than ours
        best <- foldM (\m (d, rel) -> do
                  tm <- getModificationTime (d </> rel)
                  pure (M.insertWith (\a b -> if fst a >= fst b then a else b) rel (tm, d) m)) M.empty his
        fmap catMaybes $ forM (M.toList best) $ \(rel, (tm, d)) -> do
          have <- doesFileExist (mine </> rel)
          own <- if have then Just <$> getModificationTime (mine </> rel) else pure Nothing
          -- ours is only replaced when it is STALE -- older than the module's source (or missing). A sibling's
          -- newer copy of an interface that is still good is the same module compiled later: taking it
          -- would be eighty files copied back and forth between two sessions for nothing.
          srcs <- filterM doesFileExist [ (if isAbsolute d then d else wd </> d) </> replaceExtension rel e | d <- dirs, e <- ["hs", "lhs"] ]
          srcT <- mapM getModificationTime (take 1 srcs)
          let good = case (own, srcT) of
                (Just o, st : _) -> o >= st
                _ -> False                     -- (ours missing, or no source found: by the times alone)
          let obj = replaceExtension rel "o"
          hasObj <- doesFileExist (d </> obj)
          otm <- if hasObj then getModificationTime (d </> obj) else pure tm
          if good || maybe False (>= tm) own || not hasObj || diffUTCTime t (max tm otm) < 2 then pure Nothing else do
            createDirectoryIfMissing True (takeDirectory (mine </> rel))
            copyFileWithMetadata (d </> obj) (mine </> obj)
            copyFileWithMetadata (d </> rel) (mine </> rel)
            pure (Just (takeFileName (takeDirectory d)))
      let n = length (concat took)
      when (n > 0) (logS s ("objects: " ++ show n ++ " module(s) taken from " ++ intercalate ", " (nub (concat took)) ++ " (compiled there since this session last compiled them)"))
    hiFiles root rel = do
      names <- either (\(_ :: IOException) -> []) id <$> try (listDirectory (root </> rel))
      fmap concat $ forM names $ \n -> do
        let r = if null rel then n else rel </> n
        isDir <- doesDirectoryExist (root </> r)
        if isDir then hiFiles root r else pure [ r | takeExtension n == ".hi" ]

withRootFiles :: [FilePath] -> Cfg -> Cfg
withRootFiles rootFiles cfg0 = cfg0 { gWatch = gWatch cfg0 ++ [ f | f <- rootFiles, f `notElem` gWatch cfg0 ] }

-- | __Members added to the session that is running__, without a restart.
--
-- A repl's packages were fixed when it started, so a new member meant a new repl: every module loaded and
-- linked again, every cached value computed again. The engine can take a unit while it runs ('GhsAddUnits'),
-- so the daemon asks the build tool what the NEW member set is started with -- the same question a start
-- asks, 4-8 s -- and, if the units already loaded would be started exactly as they were, hands the engine
-- the new ones. Then it becomes the daemon of the new set: its configuration, its launch record (a later
-- restart starts the whole set), the new members' environment, imports, sources to watch, checks and
-- servers.
--
-- @Left why@: nothing was changed, and the caller restarts as before -- a member that was removed, a
-- session with its own repl command, other settings for the process (its RTS flags, its prebuild step, a
-- variable already set to something else), or units the build tool would now start differently.
addMembers :: S -> IO (Either String T.Text)
addMembers s = do
  let old = sCfg s
  rootFiles <- sort . filter (\f -> ".cabal" `isSuffixOf` f || "cabal.project" `isPrefixOf` f) <$> getDirectoryContents (sRoot s)
  rnew <- fmap (withRootFiles rootFiles) <$> resolve (sConf s) (sName s)
  mboot <- rd (vBoot s)
  case (rnew, mboot) of
    (Left e, _) -> pure (Left e)
    (_, Nothing) -> pure (Left "the session has not started")
    (Right new, Just b)
      | not (all (`elem` gMembers new) (gMembers old)) -> pure (Left "a member was removed")
      | isJust (gRepl old) || isJust (gRepl new) -> pure (Left "a session with its own repl command")
      | not (all (`elem` gUnits new) (gUnits old)) -> pure (Left "a unit was removed")
      | same gRtsFlags || same gPrebuild || same gHygiene || same gCapabilities || same gCabalArgs || same gGhcJobs -> pure (Left "the new member set has other settings for the repl's process")
      | not (all (`elem` gPreload new) (gPreload old)) -> pure (Left "a preload step was removed")
      | not (null [ () | (k, v) <- gEnv new, Just v' <- [lookup k (gEnv old)], v /= v' ]) -> pure (Left "a variable of the repl's environment would change")
      | otherwise -> do
          tmp <- newIORef new
          let s' = s { sCfgV = tmp }
              env' = gEnv new ++ [("GHCI_SESSION", sName s)]
          (exe, ver, _) <- engineExe s
          let line = replCommandLine s' exe ver
          launchDir <- (\h -> sDir s </> "launch" </> showHash h) <$> hashString line 7
          -- the build tool's answer for THIS set, if it was asked before and nothing it reads has changed
          -- since (a member added, removed and added again; the same rule a start follows): 2-6 s not spent
          before <- readLaunch launchDir
          depsB <- maybe (pure Nothing) (localPackages s') before
          inputsB <- buildInputs s' line (fromMaybe ([], []) depsB)
          wasIn <- readFileMaybe (launchDir </> "inputs")
          wasDeps <- readFileMaybe (launchDir </> "inputs.deps")
          r <- case before of
            Just l0 | wasIn == Just (fst inputsB) && wasDeps == Just (snd inputsB) && isJust depsB -> do
              logS s "the build's answer for the new member set is reused: the build tool is not run"
              pure (Right l0)
            _ -> captureLaunch line (sRoot s) env' launchDir (sDir s </> "add.out") (gLoadTimeout new) (logS s)
          case r of
            Left said -> writeAtomic (sDir s </> "load.log") said >> pure (Left "the build tool could not say how to start the new set (load.log)")
            Right l -> do
              -- the start as the engine reads it: cabal on macOS puts `-unit @file` per unit on the command
              -- line; on Linux (3.18) it puts EVERYTHING in one response file -- the flags, then a `-unit
              -- @file` per unit -- which is opened up here, so a unit's file is the one after `-unit` either way
              argsB <- launchArgs (bLaunch b)
              argsL <- launchArgs l
              let unitFiles args = [ f | ("-unit", '@' : f) <- zip ("" : args) args ]
                  plain args = [ a | a <- args, take 1 a /= "@", a /= "-unit" ]
                  -- a unit by the id it declares (-this-unit-id): its file's NAME carries the build tool's
                  -- numbering of that run, which moves when a unit is added before it
                  unitOf f t = fromMaybe (takeFileName f) (listToMaybe [ v | ("-this-unit-id", v) <- zip ls (drop 1 ls) ])
                    where ls = lines (fromMaybe "" t)
              was <- forM (unitFiles argsB) (\f -> readFileMaybe f >>= \t -> pure (unitOf f t, t))
              plan <- (>>= either (const Nothing) Just . parseJson) <$> readFileMaybe (sRoot s </> "dist-newstyle" </> "cache" </> "plan.json")
              let below = maybe [] (\pl -> unitsBelow pl (map fst was)) plan
              now' <- forM (unitFiles argsL) (\f -> readFileMaybe f >>= \t -> pure (unitOf f t, f, t))
              let moved = [ u | (u, t) <- was, [ t' | (u', _, t') <- now', u' == u ] /= [t] ]
                  fresh = [ f | (u, f, _) <- now', u `notElem` map fst was ]
                  freshIds = [ u | (u, _, _) <- now', u `notElem` map fst was ]
                  -- (named in a loaded unit's own flags, or anywhere UNDER one in the build tool's plan: a package
                  --  a loaded unit reaches only through another built package is used built all the same)
                  usedBuilt = [ u | u <- freshIds, any (\(_, t) -> u `elem` lines (fromMaybe "" t)) was || u `elem` below ]
              -- (the build tool starts ONE unit with its flags on the command line and no unit file: there is
              --  then nothing to compare the new set's with, and the flags the engine kept are that unit's)
              if null (unitFiles argsB) then pure (Left "the session was started with a single unit, not as units (it takes members live once it has two)")
                else if plain argsL /= plain argsB then pure (Left "the engine would be started with other arguments")
                else if not (null moved) then pure (Left ("units already loaded would be built differently: " ++ unwords moved))
                else if null fresh then pure (Left "the build tool names no new unit")
                -- A package the loaded units already USE, built, cannot become a unit beside them: they were
                -- set up with it as a package of the database, and the compiler then finds it in both places
                -- (a panic in its module graph: tried, with the core library under two packages that use it).
                -- Everything that uses it has to be set up again, which is a restart.
                else if not (null usedBuilt) then pure (Left ("the units already loaded use " ++ unwords usedBuilt ++ " as a built package: making it a unit means loading them again"))
                else do
                  rp <- theRepl s
                  Reply j _ <- replQueryOut rp (Just 300) "add_units" [("files", JArr (map JStr fresh))]
                  case lookupStr "error" j of
                    Just e -> pure (Left ("the engine did not take the units: " ++ e))
                    Nothing -> do
                      let added = [ u | JStr u <- lookupArr "units" j ]
                          c t e = void (replCommand rp (Just t) e)
                      logS s ("added to the running repl: " ++ unwords added)
                      -- from here the daemon is the new set's
                      writeIORef (sCfgV s) new
                      deps' <- localPackages s l
                      buildInputs s line (fromMaybe ([], []) deps') >>= writeInputs launchDir
                      vBoot s =: Just (Boot (bExe b) line env' launchDir l)
                      forM_ [ kv | kv@(k, _) <- gEnv new, isNothing (lookup k (gEnv old)) ] $ \(k, v) ->
                        c 60 ("System.Environment.setEnv " ++ show k ++ " " ++ show (replaceSession v))
                      forM_ [ pl | pl <- gPreload new, pl `notElem` gPreload old ] (c 120)
                      -- (with the servers looked at: one whose code did not change is KEPT, and the verdict says so
                      --  rather than "not re-forked")
                      vLastLoad s =: Nothing        -- (the engine has targets it has not loaded)
                      v <- reload s True True Nothing
                      let ms = [ m | m <- gModules new, m `notElem` gModules old ]
                      unless (null ms) (forM_ ms (\m -> c 60 (":module + " ++ m)))
                      modifyIORef' (vWatchGen s) (+ 1)
                      void (forkIO (void (try (watchLoop s) :: IO (Either SomeException ()))))
                      started <- serversBoot s
                      pure (Right (T.pack ("added " ++ unwords [ m | m <- gMembers new, m `notElem` gMembers old ] ++ " to the running repl (" ++ unwords added ++ "): no restart\n")
                                   <> v <> (if null started then T.empty else T.pack ("\n[servers: " ++ intercalate "; " started ++ "]"))))
      where same f = f old /= f new
            replaceSession = id

-- | Run the daemon for one session until it is told to stop (or idles out).
runDaemon :: Conf -> String -> Bool -> Bool -> IO ()
runDaemon conf name bootCheck fastStart = do
  cfg0 <- resolve conf name >>= either (throwIO . userError) pure
  let root = cRoot conf
      dir = cStateDir conf </> name
  createDirectoryIfMissing True dir
  -- build files at the root are watched too: a changed .cabal means a new package set, which a reload cannot adopt
  rootFiles <- sort . filter (\f -> ".cabal" `isSuffixOf` f || "cabal.project" `isPrefixOf` f) <$> getDirectoryContents root
  let cfg = withRootFiles rootFiles cfg0
  t <- now
  cfgV <- newIORef cfg
  hist <- if not (gHistory cfg) then pure Nothing else do
    -- (a message's node starts once fewer than this many before it are unbuilt: as many as run at once)
    -- (the view is given less by what the subjects' block takes: a turn reads both)
    let room = if gKnowledge cfg then K.budget else 0
    r <- try (H.openHistory H.defaultParams { H.pAhead = max 1 (gSummarizeJobs cfg), H.pView = H.pView H.defaultParams - room, H.pViewMin = H.pViewMin H.defaultParams - room } (dir </> "history")) :: IO (Either SomeException (H.Mem, Int))
    case r of
      Right (m, torn) -> do
        when (torn > 0) (void (try (appendFileUtf8 (dir </> "daemon.log") ("history: " ++ show torn ++ " torn line(s) skipped\n")) :: IO (Either IOException ())))
        pure (Just m)
      Left e -> do
        void (try (appendFileUtf8 (dir </> "daemon.log") ("history: cannot open: " ++ displayException e ++ "\n")) :: IO (Either IOException ()))
        pure Nothing
  -- (the objects' directory was `obj`: what is there was compiled by sessions whose own flags were an
  -- interpreter's -- see the load -- and the compiler takes such an object as good for as long as its unit's
  -- flags are the same. So the objects are kept under another name, and the old ones are removed.)
  void (try (removeDirectoryRecursive (dir </> "obj")) :: IO (Either IOException ()))
  s <- S conf name cfgV root dir (cStateRel conf </> name </> "objs") bootCheck fastStart hist
         <$> newIORef Nothing <*> newIORef "starting" <*> newIORef (JObj []) <*> newIORef ""
         <*> newIORef M.empty <*> newIORef M.empty <*> newIORef 0 <*> newIORef 0
         <*> newIORef False <*> newIORef False <*> newIORef "stopped" <*> newIORef []
         <*> newMVar ()
         <*> newIORef (gHygiene cfg) <*> newIORef (JObj []) <*> newIORef []
         <*> newIORef False <*> newIORef False <*> newIORef True
         <*> newIORef 0 <*> newIORef root <*> newIORef t <*> newIORef 0
         <*> newIORef Nothing <*> newIORef Nothing <*> newIORef M.empty <*> newIORef 0
         <*> newIORef "OK" <*> newIORef "?" <*> newIORef Nothing <*> newIORef Nothing
         <*> newIORef t <*> newIORef M.empty <*> newIORef M.empty <*> newIORef False <*> newIORef Nothing
         <*> newIORef 0 <*> newIORef Nothing <*> newIORef Nothing <*> newIORef 0 <*> newIORef 0
         <*> newIORef M.empty <*> newIORef Nothing <*> newIORef Nothing
  pid <- getProcessID
  writeAtomic (dir </> "pid") (show pid)
  t0 <- now
  histAdd s "tool" (T.pack ("start " ++ name))
  r <- try $ oneVerdict s $ do
    boot s (if sFastStart s then Just True else Nothing)
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
      verdictLine s >>= histAdd s "echo"
      rm (dir </> "pid")
    Right () -> do
      verdictLine s >>= histAdd s "echo"
      exe <- getExecutablePath
      -- ("ghci-session summarize" is this executable's own compactor, wherever the binary is -- also after
      -- settings for it, as a shell takes them: "GHS_PROVIDER=claude ghci-session summarize")
      let self c = let (sets, rest) = span isSetting (words c)
                       isSetting w = case break (== '=') w of { (k@(_ : _), '=' : _) -> all (\x -> isAlphaNum x || x == '_') k; _ -> False }
                   in case rest of { ("ghci-session" : more) -> unwords (sets ++ show exe : more); _ -> c }
      -- (GHS_KNOWLEDGE_ONLY=1: no summaries are made -- a history brought in only to be read for its facts, as
      -- tools/know-trial.sh does, is not paid for twice)
      factsOnly <- (== Just "1") <$> lookupEnv "GHS_KNOWLEDGE_ONLY"
      unless factsOnly $ forM_ (sHist s) $ \m -> forM_ (gSummarizeCmd cfg) $ \c -> forkIO (void (try (compactorLoop s m (self c)) :: IO (Either SomeException ())))
      when (gKnowledge cfg) $ forM_ (sHist s) $ \m -> forM_ (gSummarizeCmd cfg) $ \c -> forkIO (void (try (knowLoop s m (self c)) :: IO (Either SomeException ())))
      void (forkIO (seedLoaded s))
      vMem s =: Nothing
      memSampleAsync s
      now >>= (vLastUsed s =:)     -- idle is counted from the end of the boot, not from the daemon's start
      addDepWatch s
      void (forkIO (void (try (watchLoop s) :: IO (Either SomeException ()))))
      -- (what the session was started on, to tell a later change of the file by; and the file watched for one)
      readFileMaybe (root </> "ghci-session.json") >>= mapM_ (writeAtomic (dir </> "config.taken"))
      void (forkIO (void (try (configLoop s) :: IO (Either SomeException ()))))
      typecheckAsync s
      serve s `finally` do
        reforkJoin s
        keep <- rd (vKeepServers s)
        unless keep (serversStopAll s)
        rd (vRepl s) >>= mapM_ (\rp -> stopRepl rp (logS s))
        rd (vStopReason s) >>= \why -> setStatus s False why [] []
        histAdd s "tool" (T.pack "stop")
        rd (vStopReason s) >>= histAdd s "echo" . T.pack
        rm (dir </> "pid")
