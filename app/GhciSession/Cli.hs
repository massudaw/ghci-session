{-# LANGUAGE ScopedTypeVariables #-}

-- | The client: @ghci-session start|stop|restart|status|reload|typecheck|test|eval|server|compose|mem|log|list|gc|autostop|init@.
module GhciSession.Cli (cliMain, Args (..), parseArgs, autostopPlan) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (filterM, forM, forM_, unless, void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit)
import Data.List (intercalate, isPrefixOf, nub, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import System.Directory
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (makeRelative, takeFileName, (</>))
import System.IO
import System.Posix.IO (fdToHandle)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Types (CPid (..))
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readCreateProcessWithExitCode, shell)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Text.Printf (printf)

import GhciSession.Config
import GhciSession.Daemon (agentTools, runDaemon)
import GhciSession.Gc
import GhciSession.Json
import GhciSession.Chat (chatMain, summarizeMain)
import GhciSession.Top (topMain)
import GhciSession.Usage (usageRows, usageTable)
import GhciSession.Quota (windowLines)
import GhciSession.QuotaFit (quotaLines)
import GhciSession.Mcp (mcpMain, relayMain, toolArgs)
import qualified GhciSession.Mcp as Mcp
import GhciSession.Sys
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import qualified GhciSession.Search as Search
import qualified GhciSession.Vfs as Vfs
import qualified GhciSession.Import as I
import qualified GhciSession.ImportStamp as IS
import qualified GhciSession.History as H
import qualified GhciSession.Know as K

-- arguments --------------------------------------------------------------------

data Args = Args { aPos :: [String], aFlags :: [String], aOpts :: [(String, String)] }

-- | Split a command's arguments into positionals, flags and options (the ones that take a value).
parseArgs :: [String] -> [String] -> Args
parseArgs valued = go (Args [] [] [])
  where
    go a [] = a { aPos = reverse (aPos a), aOpts = reverse (aOpts a) }
    go a (x : v : r) | x `elem` valued = go a { aOpts = (x, v) : aOpts a } r
    go a (x : r) | "-" `isPrefixOf` x && length x > 1 && not (isNumber x) = go a { aFlags = x : aFlags a } r
                 | otherwise = go a { aPos = x : aPos a } r
    isNumber x = case reads x :: [(Double, String)] of { [(_, "")] -> True; _ -> False }

flag :: Args -> [String] -> Bool
flag a names = any (`elem` aFlags a) names

opt :: Args -> [String] -> Maybe String
opt a names = case [ v | (k, v) <- aOpts a, k `elem` names ] of { (v : _) -> Just v; [] -> Nothing }

opts :: Args -> [String] -> [String]
opts a names = [ v | (k, v) <- aOpts a, k `elem` names ]

pos :: Args -> Int -> Maybe String
pos a i = case drop i (aPos a) of { (x : _) -> Just x; [] -> Nothing }

die' :: String -> IO a
die' msg = hPutStrLn stderr msg >> exitWith (ExitFailure 2)

-- | @import [SESSION] [--tools] [--all] [--since YYYY-MM-DD] [--claude DIR] [--codex DIR] [--go]@: the chats had
-- on this project in Claude Code and Codex, into the session's history ("GhciSession.Import"). The plan is
-- shown; @--go@ writes it, through the daemon (it is the one that writes the history), a session at a time, and
-- what was taken of each is written down as it is done -- an import stopped half way goes on where it was.
cmdImport :: Conf -> Args -> IO Int
cmdImport conf a = do
  name <- pick conf (opt a ["-s", "-t", "--session"] <|> pos a 0)
  up <- if flag a ["--go"] then sessionUp conf name else pure True
  if up then cmdImportUp conf a name else
    -- (the hook at a Claude Code turn's end runs this: with no daemon it neither starts a build nor reads the files)
    putStrLn ("import: no session running on " ++ name ++ " -- nothing imported (ghci-session start " ++ name ++ ")") >> pure 0

-- | Does the session's daemon answer on its socket?
sessionUp :: Conf -> String -> IO Bool
sessionUp conf name = do
  mfd <- sockPath (stateOf conf name) >>= unixConnect
  case mfd of
    Nothing -> pure False
    Just fd -> fdToHandle fd >>= hClose >> pure True

cmdImportUp :: Conf -> Args -> String -> IO Int
cmdImportUp conf a name = do
  home <- getHomeDirectory
  let root = cRoot conf
      claude = fromMaybe (I.claudeDir home root) (opt a ["--claude"])
      codex = maybe [home </> ".codex" </> "sessions", home </> ".codex" </> "archived_sessions"] pure (opt a ["--codex"])
      doneFile = stateOf conf name </> "history" </> "imported.json"
  since <- case opt a ["--since"] of
    Nothing -> pure 0
    Just d -> maybe (die' ("--since " ++ d ++ ": a date is YYYY-MM-DD")) pure (I.isoSeconds (d ++ "T00:00:00Z"))
  files <- (++) <$> I.sessionFiles claude <*> (concat <$> mapM I.sessionFiles codex)
  done0 <- (\b -> either (const M.empty) (\j -> M.fromList [ (k, d) | (k, JNum d) <- fromMaybe [] (obj j) ]) (parseJsonBS b))
             . either (\(_ :: IOException) -> B.empty) id <$> try (B.readFile doneFile)
  -- (a file as it was at the last import is not read again: its decision stands. Only for the plain import: what
  -- --tools, --all and --since take is another decision. Without imported.json, nothing is skipped)
  let plain = not (flag a ["--tools", "--all"] || isJust (opt a ["--since"]))
      stampFile = stateOf conf name </> "history" </> "imported-files.json"
  kept <- if plain && not (M.null done0) then IS.loadStamps stampFile else pure M.empty
  now <- mapM (\f -> (,) f <$> IS.fileStamp f) files
  let (still, changed) = IS.stampsIn kept now
      stamps = M.fromList changed
  sessions <- catMaybes <$> mapM (I.readSession (flag a ["--tools"])) [ f | (f, _) <- now, f `notElem` still ]
  let want s = I.Want (flag a ["--tools"]) (flag a ["--all"]) since (if I.sApp s == "Codex" then root else "")      -- (Codex keeps every project's sessions together)
      picked = sortOn (map I.eDate . take 1 . I.sEntries) [ x | s <- sessions, Just x <- [I.wanted (want s) done0 s] ]
  putStrLn (name ++ ": " ++ show (length sessions) ++ " session file(s) with messages under " ++ claude ++ (if any ((== "Codex") . I.sApp) sessions then " and ~/.codex" else "")
            ++ (if null still then "" else "; " ++ show (length still) ++ " more as they were at the last import, not read")
            ++ "; " ++ show (length sessions - length picked) ++ " passed over (another project's, a program's own calls through the SDK, a single exchange, or imported already"
            ++ (if flag a ["--all"] then "" else "; --all takes the programs' and the single ones too") ++ ")")
  -- (what has nothing to take is written down now; what is taken, as it is taken)
  let taken = map I.sessionKey picked
      settled = M.union (M.filterWithKey (\f _ -> f `notElem` taken) stamps) kept
  when (plain && settled /= kept) (IS.saveStamps stampFile settled)
  if null picked then putStrLn "  nothing to import" >> pure 0 else do
    I.plan (H.pNode H.defaultParams) picked >>= mapM_ putStrLn
    if not (flag a ["--go"]) then putStrLn ("\n  nothing was written: `ghci-session import" ++ concat [ " " ++ f | f <- ["--tools", "--all"], flag a [f] ] ++ " --go` imports them") >> pure 0 else do
      doneV <- pure done0
      let logOne kind text date = do
            r <- request conf name (JObj [("op", JStr "log"), ("kind", JStr kind), ("text", JText text), ("date", JNum date)])
            unless (lookupBool "ok" r == Just True) (die' ("import: " ++ T.unpack (fromMaybe T.empty (lookupText "out" r))))
          go _ _ [] = pure ()
          go done st (s : rest) = do
            note <- I.sessionNote s
            let start = maybe 0 I.eDate (listToMaybe (I.sEntries s))
            logOne "note" note start
            forM_ (I.sEntries s) (\e -> logOne (I.eKind e) (I.eText e) (I.eDate e))
            let done' = M.insert (I.sessionKey s) (maximum (map I.eDate (I.sEntries s))) done
            createDirectoryIfMissing True (stateOf conf name </> "history")
            B.writeFile (doneFile ++ ".new") (encodeBS (JObj [ (k, JNum d) | (k, d) <- M.toList done' ]))
            renameFile (doneFile ++ ".new") doneFile
            let st' = maybe st (\x -> M.insert (I.sessionKey s) x st) (M.lookup (I.sessionKey s) stamps)
            when plain (IS.saveStamps stampFile st')
            putStrLn ("  imported: " ++ T.unpack note)
            go done' st' rest
      go doneV settled picked
      pure 0

-- plumbing -----------------------------------------------------------------------

-- | The session a command is for: the one named, else the only one running, else the config's default.
pick :: Conf -> Maybe String -> IO String
pick conf mname = do
  name <- case mname of
    Just n -> pure n
    Nothing -> do
      up <- filterM (fmap isJust . daemonPid conf) (sessionNames conf)
      pure (case up of { [one] -> one; _ -> cDefault conf })
  unless (knownSession conf name) (die' ("unknown session " ++ show name ++ "; have " ++ intercalate ", " (sessionNames conf)))
  pure name

request :: Conf -> String -> Json -> IO Json
request conf name req = do
  sp <- sockPath (stateOf conf name)
  mfd <- unixConnect sp
  case mfd of
    Nothing -> die' (name ++ ": no session running (ghci-session start " ++ name ++ ")")
    Just fd -> do
      h <- fdToHandle fd
      hSetBinaryMode h True
      B.hPut h (encodeBS req)
      B.hPut h (BC.pack "\n")
      hFlush h
      line <- BC.hGetLine h
      hClose h
      either (\e -> die' ("bad reply from the session: " ++ e)) pure (parseJsonBS line)

-- | Print a reply; warn when it came from code that is no longer on disk.
say :: Json -> IO Int
say r = do
  case strs (r .: "stale") of
    st@(f : _) -> hPutStrLn stderr ("warning: STALE -- " ++ show (length st) ++ " watched file(s) differ from the loaded code (e.g. "
                                    ++ takeFileName f ++ "); `ghci-session reload`")
    [] -> pure ()
  B.putStr (TE.encodeUtf8 (fromMaybe T.empty (lookupText "out" r)))   -- as bytes: it can be megabytes
  B.putStr (BC.pack "\n")
  pure (if lookupBool "ok" r == Just True then 0 else 1)

firstLine :: FilePath -> IO String
firstLine p = maybe "" (takeWhile (/= '\n')) <$> readFileMaybe p

statusOk :: Conf -> String -> IO Bool
statusOk conf name = do
  t <- readFileMaybe (stateOf conf name </> "status.json")
  pure (case t >>= either (const Nothing) Just . parseJson of { Just j -> lookupBool "ok" j == Just True; Nothing -> False })

-- commands ------------------------------------------------------------------------

cmdStart :: Conf -> Maybe String -> Bool -> Bool -> IO Int
cmdStart conf target noCheck fast = do
  name <- pick conf target
  up <- daemonPid conf name
  case up of
    Just pid -> putStrLn (name ++ ": already running (pid " ++ show pid ++ ")") >> pure 0
    Nothing -> do
      let d = stateOf conf name
      createDirectoryIfMissing True d
      forM_ ["status", "status.json"] (\f -> void (try (removeFile (d </> f)) :: IO (Either IOException ())))
      cfg <- resolve conf name >>= either die' pure
      exe <- getExecutablePath
      out <- openFile (d </> "daemon.out") AppendMode
      void (createProcess (proc exe (["--root", cRoot conf, "_daemon", name] ++ [ "--no-check" | noCheck ] ++ [ "--fast" | fast ]))
              { std_in = NoStream, std_out = UseHandle out, std_err = UseHandle out, close_fds = True, new_session = True })
      cwd' <- getCurrentDirectory
      hPutStrLn stderr (name ++ ": booting (log: " ++ makeRelative cwd' (d </> "daemon.log") ++ ")")
      t0 <- now
      let limit = gLoadTimeout cfg + 120
          wait = do
            threadDelay 10000
            t <- now
            first <- firstLine (d </> "status")
            alive <- daemonPid conf name
            let verdict = if "STALE(" `isPrefixOf` first then drop 2 (dropWhile (/= ')') first) else first
            if not (null first) && verdict /= "starting"
              then do
                answering
                putStrLn first >> pure (if any (`isPrefixOf` first) ["OK", "STALE"] then 0 else 1)
              else if isNothing alive && t - t0 > 3
                then hPutStrLn stderr (name ++ ": the daemon died while booting; see " ++ (d </> "daemon.out")) >> pure 1
                else if t - t0 > limit then hPutStrLn stderr (name ++ ": still booting after " ++ show (round limit :: Int) ++ "s") >> pure 1
                else wait
          -- the verdict is written before the daemon listens on its socket: a command sent the moment `start`
          -- returns could find no session ("no session running" -- seen in the tour's autostop step). So
          -- `start` returns once the session answers a request too (an `info`), or once its daemon is gone, or
          -- after 10 s.
          answering = go (0 :: Int)
            where go n = do
                    sp <- sockPath d
                    mfd <- unixConnect sp
                    case mfd of
                      Just fd -> do
                        h <- fdToHandle fd
                        hSetBinaryMode h True
                        void (try (B.hPut h (encodeBS (JObj [("op", JStr "info")])) >> B.hPut h (BC.pack "\n") >> hFlush h
                                   >> BC.hGetLine h) :: IO (Either IOException BC.ByteString))
                        hClose h
                      Nothing -> do
                        alive <- daemonPid conf name
                        when (isJust alive && n < 500) (threadDelay 20000 >> go (n + 1))
      wait

cmdStop :: Conf -> Maybe String -> Bool -> Maybe String -> IO Int
cmdStop conf target keepServers reason = do
  name <- pick conf target
  up <- daemonPid conf name
  case up of
    Nothing -> putStrLn (name ++ ": not running") >> pure 0
    Just pid -> do
      void (try (request conf name (JObj ([("op", JStr "stop"), ("keep_servers", JBool keepServers)] ++ maybe [] (\r -> [("reason", JStr r)]) reason)))
              :: IO (Either SomeException Json))
      let wait n = do
            a <- daemonPid conf name
            if isNothing a then putStrLn (name ++ ": stopped")
              else if n <= (0 :: Int) then do
                void (try (signalProcess sigKILL (CPid (fromIntegral pid))) :: IO (Either IOException ()))
                putStrLn (name ++ ": killed")
              else threadDelay 5000 >> wait (n - 1)
      wait 2000
      pure 0

cmdStatus :: Conf -> Maybe String -> Bool -> IO Int
cmdStatus conf target detail = do
  names <- case target of
    Just _ -> pure <$> pick conf target
    Nothing -> filterM (fmap isJust . daemonPid conf) (sessionNames conf)
  when (null names) (putStrLn "no session running")
  when (isNothing target) $ forM_ (sessionNames conf) $ \name -> do     -- say why a session that was running is not
    up <- daemonPid conf name
    first <- firstLine (stateOf conf name </> "status")
    when (isNothing up && "stopped" `isPrefixOf` first && first /= "stopped") (putStrLn (name ++ ": " ++ first))
  left <- findLeftovers conf
  let k = length (lDaemons left) + length (lServers left) + length (lBuilds left)
  when (k > 0) $ hPutStrLn stderr (printf "warning: %d leftover process(es) of this project that no session tracks (%d daemon, %d server, %d build); `ghci-session gc`"
                                     k (length (lDaemons left)) (length (lServers left)) (length (lBuilds left)))
  forM_ names $ \name -> do
    up <- daemonPid conf name
    if isNothing up then putStrLn (name ++ ": not running") else do
      r <- request conf name (JObj [("op", JStr "status")])
      putStrLn (name ++ ": " ++ fromMaybe "" (lookupStr "out" r))
      when detail (readFileMaybe (stateOf conf name </> "status") >>= mapM_ (putStr . ensureNl))
  pure 0
  where ensureNl t = if null t || last t == '\n' then t else t ++ "\n"

cmdReload :: Conf -> Args -> IO Int
cmdReload conf a = do
  name <- pick conf (pos a 0)
  t0 <- now
  rc <- request conf name (JObj ([ ("op", JStr "reload"), ("check", JBool (not (flag a ["--no-test", "--no-check"]))), ("refork", JBool (not (flag a ["--no-refork"]))) ]
                                 ++ [ ("async_refork", JBool True) | flag a ["--async-refork"] ])) >>= say
  -- the verdict is in the status file: print it, not only GHC's load log
  readFileMaybe (stateOf conf name </> "status") >>= mapM_ putStr
  t1 <- now
  hPutStrLn stderr (printf "(%.1fs)" (t1 - t0))
  ok <- statusOk conf name
  pure (if rc /= 0 then rc else if ok then 0 else 1)

cmdCheck :: Conf -> Args -> IO Int
cmdCheck conf a = do
  name <- pick conf (pos a 0)
  rc <- request conf name (JObj ([("op", JStr "check")] ++ maybe [] (\m -> [("member", JStr m)]) (opt a ["-m", "--member"]))) >>= say
  ok <- statusOk conf name
  pure (if rc /= 0 then rc else if ok then 0 else 1)

cmdEval :: Conf -> Args -> IO Int
cmdEval conf a = case pos a 0 of
  Nothing -> die' "eval: an expression is needed"
  Just e -> do
    name <- pick conf (opt a ["-s", "-t", "--session"])
    request conf name (JObj ([("op", JStr "eval"), ("expr", JStr e)] ++ maybe [] (\t -> [("timeout", JNum (read t))]) (opt a ["--timeout"]))) >>= say

cmdSimple :: String -> Conf -> Args -> IO Int
cmdSimple op conf a = pick conf (pos a 0) >>= \name -> request conf name (JObj [("op", JStr op)]) >>= say

cmdServer :: Conf -> Args -> IO Int
cmdServer conf a = do
  let action = fromMaybe "status" (pos a 0)
  unless (action `elem` ["status", "start", "stop", "restart"]) (die' "server: status | start | stop | restart")
  name <- pick conf (opt a ["-s", "--session"])
  r <- request conf name (JObj ([("op", JStr "server"), ("action", JStr action), ("resume", JBool (flag a ["--resume"]))]
                                ++ maybe [] (\m -> [("member", JStr m)]) (opt a ["-m", "--member"])))
  let out = fromMaybe "" (lookupStr "out" r)
  putStrLn out
  pure (if lookupBool "ok" r == Just True && not (isInfixOf' "FAILED" out) then 0 else 1)
  where isInfixOf' n h = any (n `isPrefixOf`) (tails' h)
        tails' [] = [[]]
        tails' x@(_ : r) = x : tails' r

-- | Set a composed session's members. A repl's package set is fixed when it boots, so a change restarts that
-- repl -- but the servers already running are kept and adopted by the new one.
cmdCompose :: Conf -> Args -> IO Int
cmdCompose conf a = case aPos a of
  [] -> die' "compose: a session name is needed"
  (name : members) -> do
    when (isNothing (lookup name (cSessions conf))) $
      die' (show name ++ " is not a composed session; declare it under \"sessions\" (have: " ++ (if null (cSessions conf) then "none" else intercalate ", " (map fst (cSessions conf))) ++ ")")
    cur <- readMembers conf name
    let adds = opts a ["--add"]
        removes = opts a ["--remove"]
    if null adds && null removes && null members
      then do
        putStrLn (name ++ " members: " ++ (if null cur then "(none)" else intercalate ", " cur))
        cmdStatus conf (Just name) False
      else do
        let new = if null adds && null removes then nub members else [ m | m <- cur, m `notElem` removes ] ++ [ m | m <- adds, m `notElem` cur ]
            unknown = [ m | m <- new, isNothing (lookup m (cTargets conf)) ]
        unless (null unknown) (die' ("unknown target(s): " ++ intercalate ", " unknown ++ "; have " ++ intercalate ", " (map fst (cTargets conf))))
        up <- daemonPid conf name
        if new == cur && isJust up then putStrLn (name ++ ": unchanged (" ++ (if null new then "none" else intercalate ", " new) ++ ")") >> pure 0 else do
          writeMembers conf name new
          putStrLn (name ++ " members: " ++ (if null new then "(none)" else intercalate ", " new))
          -- members only ADDED to a session that is up: the running repl takes them if it can (no restart);
          -- the daemon says why not otherwise, and the repl is restarted as it always was
          live <- if isJust up && all (`elem` new) cur && not (flag a ["--restart"])
            then do
              r <- try (request conf name (JObj [("op", JStr "add_members")])) :: IO (Either SomeException Json)
              let out = either (const "RESTART-NEEDED: the daemon did not answer") (\j -> fromMaybe "" (lookupStr "out" j)) r
              if "RESTART-NEEDED" `isPrefixOf` out
                then putStrLn (name ++ ": " ++ drop 16 out ++ " -- restarting the repl") >> pure False
                else putStrLn out >> pure True
            else pure False
          if live then pure 0 else do
            when (isJust up) (void (cmdStop conf (Just name) True Nothing))
            cmdStart conf (Just name) (flag a ["--no-test", "--no-check"]) (flag a ["--fast"])

-- | __What the model calls cost__: the session's ledger (@<state>/<session>/usage.jsonl@, one line a call --
-- the chat's and the compactor's: when, who asked, the model, tokens in and of them cached, tokens out,
-- seconds) summed by who asked and by day, and in money when @"prices"@ in the config prices the model
-- (@{"MODEL": {"input": .., "input_cached": .., "output": ..}}@, per million tokens). Every session's, when
-- none is named.
-- | @knowledge@: what is known, by subject ("GhciSession.Know") -- the subjects and how much each holds; one
-- subject's facts, with where each came from (@--all@: the replaced ones too); the block a session of a
-- project reads; a fact forgotten.
cmdKnowledge :: Args -> IO Int
cmdKnowledge a = do
  dir <- K.knowDir
  facts <- K.loadFacts dir
  case (pos a 0, pos a 1) of
    (Just "search", Just _) -> do
      let found = take (maybe 10 read (opt a ["-n"])) (K.search (T.pack (unwords (drop 1 (aPos a)))) ((if flag a ["--all"] then id else K.current) facts))
      forM_ found $ \f -> TIO.putStrLn (T.pack (K.fId f ++ "  [" ++ K.day (K.fFirst f) ++ "] (") <> K.fSubject f <> T.pack ") " <> K.fText f <> T.pack (maybe "" ("  -- replaced by " ++) (K.fBy f)))
      when (null found) (putStrLn "no fact holds those words")
      pure 0
    (Just "forget", Just i)
      | any ((== i) . K.fId) facts -> K.withLock dir (K.forget dir i) >> putStrLn ("forgotten: " ++ i) >> pure 0
      | otherwise -> hPutStrLn stderr ("knowledge: no fact " ++ i) >> pure 1
    _ | Just p <- opt a ["--block"] -> TIO.putStr (K.renderFor (toolArgs agentTools) K.budget p facts) >> pure 0
      | flag a ["--candidates"] -> do
          seen <- K.confirmations dir facts
          let cs = K.promotable seen facts
          forM_ cs $ \(f, s) ->
            TIO.putStrLn (T.pack (printf "%2d days %d project(s)  %-12s %s  " (S.size (K.seenDays s)) (S.size (K.seenProjects s)) (T.unpack (K.fSubject f)) (K.fId f)) <> K.fText f)
          when (null cs) (putStrLn "no tool/ or user/ fact is confirmed on three days or from two projects yet")
          pure 0
      | Just sub <- opt a ["--subject"] -> do
          let shown = [ f | f <- (if flag a ["--all"] then id else K.current) facts, K.fSubject f == T.pack sub ]
          forM_ (sortOn (negate . K.fLast) shown) $ \f ->
            TIO.putStrLn (T.pack (K.fId f ++ "  [" ++ K.day (K.fFirst f) ++ (if K.day (K.fLast f) /= K.day (K.fFirst f) then ", confirmed " ++ K.day (K.fLast f) else "") ++ "] ")
                          <> K.fText f <> T.pack ("  (" ++ K.fSrc f ++ maybe "" (" -- replaced by " ++) (K.fBy f) ++ ")"))
          when (null shown) (putStrLn ("nothing under " ++ sub))
          pure 0
      | otherwise -> do
          let cur = K.current facts
              subjects = M.toList (M.fromListWith (+) [ (K.fSubject f, 1 :: Int) | f <- cur ])
          putStrLn (dir ++ ": " ++ show (length cur) ++ " fact(s) that hold, " ++ show (length facts - length cur) ++ " replaced")
          forM_ subjects $ \(sub, n) -> putStrLn (printf "  %-28s %4d" (T.unpack sub) n)
          pure 0

cmdUsage :: Conf -> Args -> IO Int
cmdUsage conf a = do
  names <- case pos a 0 of
    Just n -> (: []) <$> pick conf (Just n)
    Nothing -> filterM (\n -> doesFileExist (stateOf conf n </> "usage.jsonl")) (sessionNames conf)
  since <- case opt a ["--since"] of
    Just d | [(k, "")] <- reads d -> (\t -> t - k * 86400) <$> now
    _ -> pure 0
  rows <- usageRows conf names since
  if null rows then putStrLn "no model calls recorded (the chat and the compactor write <state>/<session>/usage.jsonl)" >> pure 0
  else if flag a ["--json"] then putStrLn (encode (JArr (map snd rows))) >> pure 0
  else if flag a ["--quota"] then mapM_ putStrLn (quotaLines (rolloverRatioOf conf (fromMaybe "" (listToMaybe names))) rows) >> pure 0
  else mapM_ putStrLn (usageTable conf rows) >> windowLines (map snd rows) >>= mapM_ putStrLn >> pure 0

-- | __The session's own scenario, timed__: the steps a target lists under @"profile"@, run in order against
-- the running session, each against its budget and against the last run.
--
-- A step is @{"name": ..., and one of "eval": EXPR | "cmd": "reload --no-test" | "sh": "a shell command"}@
-- with, optionally, @"budget"@ (seconds) and @"expect"@ (a regular expression its output must contain).
-- @eval@ is an expression in the session, @cmd@ one of this tool's own commands, @sh@ anything else (an
-- edit, its revert). A run is saved in @<state>/<session>/profile/@; a step more than 1.5 times and 1 s
-- slower than in the run before is a REGRESSION, and the command fails if a step failed, missed what it
-- expected or went over its budget.
cmdProfile :: Conf -> Args -> IO Int
cmdProfile conf a = do
  name <- pick conf (case opt a ["-s", "-t", "--session"] of { Just n -> Just n; Nothing -> pos a 0 })
  cfg <- resolve conf name >>= either die' pure
  let only = opts a ["--only"]
      steps = [ st | st <- gProfile cfg, null only || fromMaybe "" (lookupStr "name" st) `elem` only ]
      dir = stateOf conf name </> "profile"
  when (null steps) (die' (name ++ ": no \"profile\" steps (a target lists them: {\"name\": .., \"eval\" | \"cmd\" | \"sh\": .., \"budget\": seconds, \"expect\": regex})"))
  exe <- getExecutablePath
  createDirectoryIfMissing True dir
  olds <- filter (\f -> ".json" `isSuffixOf'` f) <$> getDirectoryContents dir
  prev <- case reverse (sortOn id olds) of
    (f : _) -> (>>= either (const Nothing) Just . parseJson) <$> readFileMaybe (dir </> f)
    [] -> pure Nothing
  let was nm = listToMaybe' [ sec | st <- maybe [] (lookupArr "steps") prev, lookupStr "name" st == Just nm, Just sec <- [lookupNum "seconds" st] ]
  rows <- forM steps $ \st -> do
    let nm = fromMaybe "?" (lookupStr "name" st)
        budget = lookupNum "budget" st
    t0 <- now
    (ok, out) <- case (lookupStr "eval" st, lookupStr "cmd" st, lookupStr "sh" st) of
      (Just e, _, _) -> do
        r <- try (request conf name (JObj [("op", JStr "eval"), ("expr", JStr e)])) :: IO (Either SomeException Json)
        pure (either (\x -> (False, show x)) (\j -> (lookupBool "ok" j == Just True, fromMaybe "" (lookupStr "out" j))) r)
      (_, Just c, _) -> ran <$> readCreateProcessWithExitCode (proc exe (["--root", cRoot conf] ++ words c)) ""
      (_, _, Just c) -> ran <$> readCreateProcessWithExitCode ((shell c) { cwd = Just (cRoot conf) }) ""
      _ -> pure (False, "a step needs \"eval\", \"cmd\" or \"sh\"")
    t1 <- now
    met <- maybe (pure True) (\re -> anyLineMatches re (T.pack out)) (lookupStr "expect" st)
    let secs = t1 - t0
        over = maybe False (secs >) budget
        slower = maybe False (\w -> secs > 1.5 * w && secs - w > 1) (was nm)
        flags = [ "FAILED" | not ok ] ++ [ "NOT WHAT WAS EXPECTED" | ok && not met ] ++ [ "OVER BUDGET" | over ] ++ [ "REGRESSION" | slower ]
    printf "  %-44s %7.2f s%s%s%s\n" (take 44 nm) secs (maybe "" (printf "  (budget %g)") budget :: String)
      (maybe "" (printf "  [was %.2f]") (was nm) :: String) (if null flags then "" else "  " ++ intercalate ", " flags)
    when (not ok || not met) (mapM_ (putStrLn . ("      " ++)) (lastLines 4 out))
    hFlush stdout
    pure (JObj [ ("name", JStr nm), ("seconds", JNum (fromIntegral (round (secs * 1000) :: Int) / 1000)), ("ok", JBool (ok && met)), ("over", JBool over), ("regression", JBool slower) ])
  stamp <- formatTime defaultTimeLocale "%Y-%m-%dT%H%M%SZ" <$> getCurrentTime
  let bad = length [ () | r <- rows, lookupBool "ok" r /= Just True || lookupBool "over" r == Just True ]
      slow = length [ () | r <- rows, lookupBool "regression" r == Just True ]
      total = sum [ x | r <- rows, Just x <- [lookupNum "seconds" r] ]
  unless (flag a ["--no-save"]) (writeAtomic (dir </> (stamp ++ ".json")) (encodePretty (JObj [("at", JStr stamp), ("session", JStr name), ("steps", JArr rows)])))
  printf "%s: %d steps in %.1f s: %d failed or over budget, %d regression(s)%s\n" name (length rows) total bad slow
    (if isJust prev then " against the run before" else " (the first run: nothing to compare with)" :: String)
  pure (if bad > 0 then 1 else 0)
  where
    ran (code, o, e) = (code == ExitSuccess, o ++ e)
    isSuffixOf' x y = reverse x `isPrefixOf` reverse y
    listToMaybe' xs = case xs of { (x : _) -> Just x; [] -> Nothing }
    lastLines n t = let ls = lines t in drop (length ls - n) ls

cmdLog :: Conf -> Args -> IO Int
cmdLog conf a = do
  name <- pick conf (opt a ["-s", "--session"])
  let which = fromMaybe "daemon.log" (pos a 0)
      n = maybe 40 read (opt a ["-n"])
  t <- readFileMaybe (stateOf conf name </> which)
  case t of
    Nothing -> hPutStrLn stderr ("no " ++ which ++ " for " ++ name) >> pure 1
    Just txt -> putStr (unlines (let ls = lines txt in drop (length ls - n) ls)) >> pure 0

cmdList :: Conf -> IO Int
cmdList conf = do
  forM_ (cTargets conf) $ \(name, t) -> do
    up <- daemonPid conf name
    let nChecks = case lookupArr "checks" t of { [] -> (case t .: "check" of { JObj _ -> 1; _ -> 0 }); cs -> length cs } :: Int
        units = case t .: "units" of { JStr u -> u; j -> unwords (strs j) }
    putStrLn (name ++ (if name == cDefault conf then " (default)" else "") ++ ": " ++ maybe "stopped" (("running pid " ++) . show) up
              ++ "  units=" ++ (if null units then "-" else units) ++ "  watch=" ++ intercalate "," (strs (t .: "watch"))
              ++ "  hygiene=" ++ (if lookupBool "hygiene" t == Just True then "on" else "off") ++ "  checks=" ++ show nChecks
              ++ (case t .: "server" of { JObj _ -> "  server"; _ -> "" }))
  forM_ (cSessions conf) $ \(name, _) -> do
    up <- daemonPid conf name
    ms <- readMembers conf name
    putStrLn (name ++ " [composed]: " ++ maybe "stopped" (("running pid " ++) . show) up ++ "  members=" ++ (if null ms then "(none)" else intercalate ", " ms))
  pure 0

cmdInit :: IO Int
cmdInit = do
  there <- doesFileExist configName
  if there then hPutStrLn stderr (configName ++ " exists") >> pure 1 else do
    writeFile configName (encodePretty (JObj
      [ ("default", JStr "lib")
      , ("targets", JObj [ ("lib", JObj [ ("units", JArr [JStr "lib:yourpackage"]), ("watch", JArr [JStr "src"]), ("modules", JArr [])
                                       , ("test", JNull), ("hygiene", JBool False) ]) ]) ]) ++ "\n")
    putStrLn ("wrote " ++ configName ++ "; edit \"units\", then `ghci-session start`")
    pure 0

-- | Which sessions to stop: the longest-idle go first, until the total is back under the limit; with no
-- limit (0), every eligible one goes.
autostopPlan :: [Json] -> Double -> Double -> Bool -> (Double, [Json], [(Json, String)])
autostopPlan infos maxMem idleMins includeServing = go total [] [] (sortOn (negate . num' "idle_s") infos)
  where
    num' k j = fromMaybe 0 (lookupNum k j)
    mb j = num' "repl_mb" j + num' "servers_mb" j
    total = sum (map mb infos)
    go _ stop spared [] = (total, reverse stop, reverse spared)
    go left stop spared (i : r)
      | lookupBool "busy" i == Just True = go left stop ((i, "busy") : spared) r
      | num' "idle_s" i < idleMins * 60 = go left stop ((i, printf "used %.0f min ago" (num' "idle_s" i / 60)) : spared) r
      | not (null (strs (i .: "serving"))) && not includeServing = go left stop ((i, "serving " ++ intercalate ", " (strs (i .: "serving"))) : spared) r
      | maxMem > 0 && left <= maxMem = go left stop ((i, "memory is back within the limit") : spared) r
      | otherwise = go (left - mb i) (i : stop) spared r

-- | Stop sessions nobody is using. A session is idle from its last client command or source change; one
-- that is busy, or serving, is left alone.
cmdAutostop :: Conf -> Args -> IO Int
cmdAutostop conf a = do
  let maxMem = maybe 0 read (opt a ["--max-mem-mb"]) :: Double
      idleMins = maybe 30 read (opt a ["--idle-mins"]) :: Double
      dry = flag a ["-n", "--dry-run"]
  up <- filterM (fmap isJust . daemonPid conf) (sessionNames conf)
  infos <- fmap catMaybes $ forM up $ \name -> do
    r <- try (request conf name (JObj [("op", JStr "info")])) :: IO (Either SomeException Json)
    pure (case r of { Right j -> lookupStr "out" j >>= either (const Nothing) Just . parseJson; Left _ -> Nothing })
  let (total, stop, spared) = autostopPlan infos maxMem idleMins (flag a ["--include-serving"])
      mb j = fromMaybe 0 (lookupNum "repl_mb" j) + fromMaybe 0 (lookupNum "servers_mb" j)
      nm j = fromMaybe "?" (lookupStr "session" j)
      idle j = fromMaybe 0 (lookupNum "idle_s" j) / 60
  putStrLn (printf "autostop: %d session(s) using %.0f MB (%s; idle after %s min)" (length infos) total
              (if maxMem > 0 then printf "limit %.0f MB" maxMem else "no memory limit: every idle session goes" :: String) (showG idleMins))
  if maxMem > 0 && total <= maxMem then putStrLn "autostop: within the limit -- nothing to stop" >> pure 0 else do
    forM_ spared (\(i, why) -> putStrLn (printf "autostop: keeping %s (%.0f MB): %s" (nm i) (mb i) why))
    forM_ stop $ \i -> do
      putStrLn (printf "autostop: %s %s (idle %.0f min, %.0f MB)" (if dry then "would stop" else "stopping" :: String) (nm i) (idle i) (mb i))
      unless dry $ void (try (request conf (nm i) (JObj [ ("op", JStr "stop")
                           , ("reason", JStr (printf "stopped by autostop after %.0f min idle; `ghci-session start %s`" (idle i) (nm i))) ])) :: IO (Either SomeException Json))
    let after = total - sum (map mb stop)
    when (maxMem > 0 && after > maxMem) (putStrLn (printf "autostop: still %.0f MB, over the limit -- nothing else is eligible" after))
    pure 0
  where showG x = if x == fromIntegral (round x :: Integer) then show (round x :: Integer) else show x

cmdSearch :: Conf -> Args -> IO Int
cmdSearch conf a = do
  let q = fromMaybe "" (pos a 0)
  when (null q) (die' "search: what are you looking for? (e.g. `ghci-session search mySymbol` or `ghci-session search --files myFile`)")
  sname <- pick conf (opt a ["-s", "-t", "--session"])
  let maxN = maybe 30 read (opt a ["-n"]) :: Int
      isFiles = flag a ["--files", "-f"]
      isJson = flag a ["--json"]
      mode = if isFiles then "files" else "grep"
  res <- if isFiles
           then Search.searchFiles (cRoot conf) q maxN
           else Search.grep (cRoot conf) q maxN
  case res of
    Left err -> die' ("search error: " ++ err)
    Right j -> do
      let hits = fromMaybe 0 (lookupNum "count" j >>= Just . round)
      Search.recordSearchMetadata conf sname mode q hits
      if isJson
        then putStrLn (encodePretty j)
        else TIO.putStrLn (if isFiles then Search.formatFiles j else Search.formatGrep j)
      pure (if hits > 0 then 0 else 1)

cmdVfs :: Conf -> Args -> IO Int
cmdVfs conf a = do
  sname <- pick conf (opt a ["-s", "-t", "--session"])
  let mPath = pos a 0
      budget = maybe 250 read (opt a ["--budget", "-b"]) :: Int
      isJson = flag a ["--json"]
  files <- Vfs.inspectLoaded conf sname budget mPath
  if isJson
    then putStrLn (encodePretty (Vfs.vfsJson files))
    else TIO.putStrLn (Vfs.formatVfsTable files budget)
  let overCount = length (filter Vfs.vfOver files)
  pure (if overCount == 0 then 0 else 1)

usage :: String
usage = unlines
  [ "ghci-session: a warm GHCi per project"
  , ""
  , "  start [--no-test] [--fast] | stop | restart [--fast] | status [-d]   [SESSION]"
  , "  reload [--no-test] [--no-refork] [--async-refork]  [SESSION]"
  , "  hold [--timeout SECS] [SESSION]    saves are not reloaded until `release` (or SECS, default 30): write several files, then reload once"
  , "  release [SESSION]                  reload what was saved since `hold`, once"
  , "  typecheck [SESSION]                do the sources on disk typecheck? (no code generated, nothing reloaded)"
  , "  vfs [PATH] [-s SESSION] [--budget N] [--json]   virtual file system and line budget inspector (<250 lines)"
  , "  test [-m MEMBER] [SESSION]         run the target's test(s) on the loaded code"
  , "  eval EXPR [-s SESSION] [--timeout SECS]"
  , "  search QUERY [-s SESSION] [--files] [-n N] [--json]   high-speed SIMD search across code or files (FFF engine), with session metadata"
  , "  history [-n N] [--since ID] [--full] [--json]   the session's log: every request and verdict, a save and what it compiled to"
  , "  history --search WORDS [-n N]      the messages that hold the words, the best first"
  , "  history --kind user|talk|note TEXT  add to it (a harness logs the user's words and the agent's replies)"
  , "  view [--wait SECS] [--json]        the whole history as the one-line summaries a model reads; zoom ID N opens a line, date ID says when"
  , "  mcp                                serve the session's operations and its memory to an agent (MCP on stdin/stdout): claude mcp add ghci -- ghci-session mcp"
  , "  top [SESSION]                      watch the session: its verdict, memory and servers, the history as it is written, the view, the daemon's log, the model calls' cost"
  , "  chat [-s SESSION] [--once MSG] [--instructions FILE] [--usage]   the endless chat: an agent on the session, remembering through its history (DEEPSEEK_API_KEY)"
  , "  summarize                          the compactor for \"summarize_cmd\": one summary line from the prompt on stdin (\"summarize_cmd\": \"ghci-session summarize\")"
  , "  knowledge [--subject S] [--all] [--block PROJECT] [--candidates] | knowledge search WORDS | knowledge forget ID   what the sessions established, by subject (\"knowledge\": true in ghci-session.json keeps it)"
  , "  usage [SESSION] [--since DAYS] [--json]   what the model calls cost -- the chat's and the compactor's -- by who asked and by day; in money with \"prices\" in ghci-session.json"
  , "  census [EXPR | --strings | --kept] [--top N] [-s SESSION]   what the heap holds: every CAF by size, the Strings, the kept values, or one value alone"
  , "  census --dups [EXPR | --kept] [--top N]                      sharing that is missed: values built more than once, the bytes sharing would give back, who holds the copies"
  , "  store [--drop NAME] [-s SESSION]                             the named slots that outlive a reload (GHC.Hygiene.Store): list them, or forget one"
  , "  why [--all] [-s SESSION]                                     what the memo recomputed since last asked, the input that moved, the seconds (GHC.Hygiene.Kept)"
  , "  bench [--live] ACTION [-s SESSION] an IO action timed: wall, GC, allocation (--live: and the live heap before and after, two collections)"
  , "  profile [SESSION] [--only NAME] [--no-save]   the target's \"profile\" steps timed, against their budgets and the last run"
  , "  mem --heap [SESSION]               the live heap, and what the CAFs and the kept values retain"
  , "  doc WORDS... [-n N] [--json] [-s SESSION]   find a declaration of the session: its signature, its comment, where it is"
  , "  compose SESSION [MEMBERS...] [--add M] [--remove M] [--no-test]"
  , "  server [status|start|stop|restart] [-m MEMBER] [-s SESSION] [--resume]"
  , "  gc [-n] [--days N]                 autostop [--max-mem-mb N] [--idle-mins M] [--include-serving] [-n]"
  , "  mem [SESSION] | log [FILE] [-s SESSION] [-n LINES] | list | init"
  , ""
  , "  --root DIR   the project (default: the nearest directory with ghci-session.json)"
  , "With no session named, a command goes to the one that is running (else the config's default)." ]

cliMain :: IO ()
cliMain = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args0 <- getArgs
  let (rootOpt, args) = case args0 of { ("--root" : r : rest) -> (Just r, rest); _ -> (Nothing, args0) }
  rc <- case args of
    [] -> putStr usage >> pure 2
    (c : _) | c `elem` ["-h", "--help", "help"] -> putStr usage >> pure 0
    ("init" : _) -> cmdInit
    ("summarize" : rest) -> summarizeMain rest          -- (no project needed: the daemon runs it from anywhere)
    ("mcp-relay" : sock : _) -> relayMain sock        -- (a wire to a chat that is running: an agent program starts it)
    (c : rest) -> do
      root <- maybe (findRoot Nothing) (pure . Right) rootOpt >>= either (\e -> die' ("ghci-session: " ++ e)) pure
      conf <- loadConf root >>= either (\e -> die' ("ghci-session: " ++ e)) pure
      let a = parseArgs ["-s", "-t", "--session", "-m", "--member", "--timeout", "-n", "--days", "--max-mem-mb", "--idle-mins", "--add", "--remove", "--top", "--only", "--drop", "--since", "--wait", "--kind", "--subject", "--block", "--search", "--claude", "--codex", "--opt", "--unit", "--runs"] rest
          -- `gc -n` and `autostop -n` are flags, `log -n 40` takes a value
          aNoN = parseArgs ["--days", "--max-mem-mb", "--idle-mins"] rest
      case c of
        "_daemon" -> case aPos a of
          (name : _) -> runDaemon conf name (not (flag a ["--no-test", "--no-check"])) (flag a ["--fast"]) >> pure 0
          [] -> die' "_daemon: a session name is needed"
        "start" -> cmdStart conf (pos a 0) (flag a ["--no-test", "--no-check"]) (flag a ["--fast"])
        "stop" -> cmdStop conf (pos a 0) (flag a ["--keep-servers"]) Nothing
        "restart" -> pick conf (pos a 0) >>= \name -> request conf name (JObj [("op", JStr "restart"), ("fast", JBool (flag a ["--fast"]))]) >>= say
        "status" -> cmdStatus conf (pos a 0) (flag a ["-d", "--detail"])
        "reload" -> cmdReload conf a
        "hold" -> do
          name <- pick conf (pos a 0)
          request conf name (JObj ([("op", JStr "hold")] ++ [ ("secs", JNum v) | Just t <- [opt a ["-t", "--timeout"]], [(v, "")] <- [reads t] ])) >>= say
        "release" -> do
          name <- pick conf (pos a 0)
          t0 <- now
          rc <- request conf name (JObj [("op", JStr "release")]) >>= say
          t1 <- now
          hPutStrLn stderr (printf "(%.1fs)" (t1 - t0))
          ok <- statusOk conf name
          pure (if rc /= 0 then rc else if ok then 0 else 1)
        c' | c' `elem` ["test", "check"] -> cmdCheck conf a      -- (`check` is the name it had)
        "typecheck" -> do
          name <- pick conf (pos a 0)
          r <- request conf name (JObj [("op", JStr "typecheck")])
          rc <- say r
          pure (if rc /= 0 then rc else if maybe False (T.isPrefixOf (T.pack "OK")) (lookupText "out" r) then 0 else 1)
        "eval" -> cmdEval conf a
        "mem" | flag a ["--heap"] -> do
          name <- pick conf (pos a 0)
          request conf name (JObj [("op", JStr "census"), ("mode", JStr "mem")]) >>= say
        "mem" -> cmdSimple "mem" conf a
        -- (--sites: a value by where its parts were allocated, in the session that has the code compiled for it)
        "census" | flag a ["--sites"], Just e <- pos a 0 -> do
          name <- pick conf (opt a ["-s", "-t", "--session"])
          (ok, out, _) <- Mcp.callReach conf "census" (JObj ([ ("session", JStr name), ("expr", JStr e), ("sites", JBool True) ]
                            ++ maybe [] (\k -> [("top", JNum (read k))]) (opt a ["--top"]) ++ maybe [] (\n -> [("opt", JNum (read n))]) (opt a ["--opt"])))
          TIO.putStrLn out >> pure (if ok then 0 else 1)
        "census" -> do
          name <- pick conf (opt a ["-s", "-t", "--session"])
          let mode = case (pos a 0, flag a ["--strings"], flag a ["--kept"]) of
                (Just _, _, _) | flag a ["--dups"] -> "dups-value"
                (_, _, True) | flag a ["--dups"] -> "dups-kept"
                _ | flag a ["--dups"] -> "dups"
                (Just _, _, _) -> "value"
                (_, True, True) -> "kept-strings"
                (_, True, _) -> "strings"
                (_, _, True) -> "kept"
                _ -> "cafs"
          request conf name (JObj ([ ("op", JStr "census"), ("mode", JStr mode) ] ++ maybe [] (\e -> [("expr", JStr e)]) (pos a 0)
                                   ++ maybe [] (\k -> [("top", JNum (read k))]) (opt a ["--top"]))) >>= say
        -- what the memo recomputed, and why (GHC.Hygiene.Kept, in the project's own code: asked there)
        "why" -> do
          name <- pick conf (opt a ["-s", "-t", "--session"])
          r <- request conf name (JObj [("op", JStr "eval"), ("expr", JStr (if flag a ["--all"] then "GHC.Hygiene.Kept.whyAll" else "GHC.Hygiene.Kept.why"))])
          if maybe False (T.isInfixOf (T.pack "GHC.Hygiene.Kept")) (lookupText "out" r) && maybe False (T.isInfixOf (T.pack "error")) (lookupText "out" r)
            then putStrLn "why: this session's code does not use GHC.Hygiene.Kept (the ghci-hygiene package), so there is nothing to ask" >> pure 1
            else say r
        -- (a trial: units, as cabal's unit files, added to a session that is running)
        "add-unit" -> do
          name <- pick conf (opt a ["-s", "-t", "--session"])
          fs <- mapM makeAbsolute (catMaybes [ pos a i | i <- [0 .. 7] ])
          if null fs then die' "add-unit: a unit file is needed (.ghci-session/<session>/launch/unit-*)" else
            request conf name (JObj [("op", JStr "add_units"), ("files", JArr (map JStr fs))]) >>= say
        "store" -> do
          name <- pick conf (opt a ["-s", "-t", "--session"])
          request conf name (JObj (("op", JStr "census") : maybe [("mode", JStr "store")] (\n -> [("mode", JStr "store-drop"), ("expr", JStr n)]) (opt a ["--drop"]))) >>= say
        "bench" -> case pos a 0 of
          Nothing -> die' "bench: an IO action is needed"
          -- (--opt N, --unit COMPONENT: in the session that has the code at that level, and the component beside it)
          Just e -> do
            name <- pick conf (opt a ["-s", "-t", "--session"])
            (ok, out, _) <- Mcp.callReach conf "bench" (JObj ([ ("session", JStr name), ("expr", JStr e) ] ++ [ ("live", JBool True) | flag a ["--live"] ]
                              ++ maybe [] (\t -> [("timeout", JNum (read t))]) (opt a ["--timeout"]) ++ maybe [] (\n -> [("opt", JNum (read n))]) (opt a ["--opt"])
                              ++ maybe [] (\u -> [("unit", JStr u)]) (opt a ["--unit"]) ++ maybe [] (\n -> [("runs", JNum (read n))]) (opt a ["--runs"])))
            TIO.putStrLn out >> pure (if ok then 0 else 1)
        "server" -> cmdServer conf a
        "compose" -> cmdCompose conf a
        "log" -> cmdLog conf a
        "profile" -> cmdProfile conf a
        "doc" -> do
          name <- pick conf (opt a ["-s", "-t", "--session"])
          when (null (aPos a)) (die' "doc: what are you looking for? (a name, part of one, its initials, or words of its type or documentation)")
          request conf name (JObj ([ ("op", JStr "doc"), ("words", JArr (map JStr (aPos a))), ("json", JBool (flag a ["--json"])) ]
                                   ++ maybe [] (\k -> [("n", JNum (read k))]) (opt a ["-n"]))) >>= say
        -- the session's history (see GhciSession.History): the log, the view a model reads, a line opened
        "history" -> do
          name <- pick conf (opt a ["-s", "-t", "--session"])
          case (opt a ["--kind"], aPos a) of
            (Just k, ws) | not (null ws) -> request conf name (JObj [("op", JStr "log"), ("kind", JStr k), ("text", JStr (unwords ws))]) >>= say
            (Just _, []) -> die' "history --kind KIND TEXT: the text is needed"
            _ | Just q <- opt a ["--search"] -> request conf name (JObj ([ ("op", JStr "recall"), ("query", JStr (unwords (q : aPos a))) ] ++ maybe [] (\k -> [("n", JNum (read k))]) (opt a ["-n"]))) >>= say
            _ -> request conf name (JObj ([ ("op", JStr "history"), ("json", JBool (flag a ["--json"])), ("full", JBool (flag a ["--full"])) ]
                                           ++ maybe [] (\k -> [("n", JNum (read k))]) (opt a ["-n"]) ++ maybe [] (\k -> [("since", JNum (read k))]) (opt a ["--since"]))) >>= say
        "view" -> do
          name <- pick conf (opt a ["-s", "-t", "--session"])
          request conf name (JObj ([ ("op", JStr "view"), ("json", JBool (flag a ["--json"])) ] ++ maybe [] (\k -> [("wait", JNum (read k))]) (opt a ["--wait"]))) >>= say
        -- (ID, then N and the session in either order: a word that is not a number is the session)
        "zoom" -> case (filter (all isDigit) (aPos a), filter (not . all isDigit) (aPos a)) of
          (i : n, others) | not (null i), length n <= 1, length others <= 1 -> do
            name <- pick conf (case opt a ["-s", "-t", "--session"] of { Just s -> Just s; Nothing -> listToMaybe others })
            request conf name (JObj [ ("op", JStr "zoom"), ("id", JNum (read i)), ("n", JNum (maybe 1 read (listToMaybe n))) ]) >>= say
          _ -> die' "zoom ID [N] [SESSION]: open the view's line ID+N into the two lines under it (N = 1, or none: the message whole)"
        "date" -> case pos a 0 of
          Just i | all isDigit i -> do
            name <- pick conf (case opt a ["-s", "-t", "--session"] of { Just s -> Just s; Nothing -> pos a 1 })
            request conf name (JObj [ ("op", JStr "date"), ("id", JNum (read i)) ]) >>= say
          _ -> die' "date ID: when message ID was written"
        "import" -> cmdImport conf a
        "mcp" -> mcpMain conf >> pure 0
        c' | c' `elem` ["top", "tui", "monitor"] -> topMain conf (opt a ["-s", "-t", "--session"] <|> pos a 0) >> pure 0
        "chat" -> chatMain conf rest
        "usage" -> cmdUsage conf a
        "knowledge" -> cmdKnowledge a
        "list" -> cmdList conf
        "search" -> cmdSearch conf a
        "find" -> cmdSearch conf a
        "grep" -> cmdSearch conf a
        "vfs" -> cmdVfs conf a
        "budget" -> cmdVfs conf a
        "gc" -> runGc conf (flag aNoN ["-n", "--dry-run"]) (maybe 0 read (opt aNoN ["--days"])) >> pure 0
        "autostop" -> cmdAutostop conf aNoN
        _ -> hPutStr stderr usage >> pure 2
  hFlush stdout
  exitWith (if rc == 0 then ExitSuccess else ExitFailure rc)
