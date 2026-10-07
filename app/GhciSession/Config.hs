-- | Finding and reading @ghci-session.json@, and turning targets into a session's configuration.
--
-- A TARGET is a definition: what to load, what to check, what to serve. A SESSION is one repl. A plain
-- session holds one target and has its name; a COMPOSED session (declared under @"sessions"@) holds whichever
-- targets you choose (@ghci-session compose NAME a b@), and its members are remembered in the state directory.
module GhciSession.Config
  ( Conf (..), Cfg (..), Check (..), Server (..)
  , configName, findRoot, loadConf, resolve, sessionNames, readMembers, writeMembers
  , targetJson
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM, unless, when)
import Data.List (isPrefixOf, nub)
import Data.Maybe (fromMaybe, isJust)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import System.Info (os)

import GhciSession.Json

configName :: FilePath
configName = "ghci-session.json"

-- | The project: where it is, where its state goes, and its targets as written (defaults and the shared
-- top-level keys already merged in).
data Conf = Conf
  { cRoot :: FilePath
  , cStateDir :: FilePath
  , cStateRel :: FilePath
  , cDefault :: String
  , cTargets :: [(String, Json)]
  , cSessions :: [(String, [String])]
  , cPrices :: [(String, Json)]   -- ^ @"prices"@: per model, @{"input": .., "input_cached": .., "output": ..}@ per million tokens (for `usage`)
  }

data Check = Check
  { ckMember :: String, ckExpr :: String, ckPass :: Maybe String, ckFail :: Maybe String
  , ckLog :: Maybe FilePath, ckTimeout :: Maybe Double }

data Server = Server
  { svMember :: String, svAction :: String, svPort :: Maybe Int, svEnv :: [(String, String)]
  , svPrefork :: Maybe String, svServeOnLoad :: Bool, svVerifyTimeout :: Double, svUnits :: [String]
  , svSpec :: String   -- ^ the declaration as text: part of what the server's code is
  }

-- | One session's configuration: a target's own, or the union of a composed session's members.
data Cfg = Cfg
  { gMembers :: [String], gComposed :: Bool
  , gRepl :: Maybe String, gUnits :: [String], gCabalArgs :: String
  , gWatch :: [String], gWatchExt :: [String], gModules :: [String]
  , gPrebuild :: Maybe String, gPreload :: [String], gWarm :: [String]
  , gChecks :: [Check], gServers :: [Server], gEnv :: [(String, String)]
  , gLoadTimeout :: Double, gEvalTimeout :: Double, gBudgetMb :: Double
  , gRtsFlags :: String, gGhcJobs :: Int, gCapabilities :: Int
  , gMemReturn :: Bool     -- ^ macOS: insert the library that makes the RTS's memory returns real
  , gHeapAuto :: Bool      -- ^ keep the RTS's -H (allocation area up to the largest heap it has needed)
  , gPruneGc :: String   -- ^ "compact" (the RTS's own choice) or "copying": the collection after a reload's unlink
  , gHygiene :: Bool
  , gHandoverEnv :: (String, String), gUnlinkAfter :: String, gPruneGcIdle :: Double
  , gAutoReload :: Bool, gWatchCheck :: Bool, gWatchRefork :: Bool, gReloadOnCommit :: Bool
  , gWatcher :: String, gPollInterval :: Double, gDebounce :: Double
  , gStatusUrl :: Maybe String, gIdleStopMins :: Double, gAsyncRefork :: Bool, gFingerprintFiles :: [String], gFastStart :: Bool, gWatchTypecheck :: Bool, gProfile :: [Json]
  , gHistory :: Bool             -- ^ keep the session's history (every request and verdict) and its summary tree
  , gSummarizeCmd :: Maybe String   -- ^ the command that compresses a message, or merges two lines, into one line (the compactor)
  , gSummarizeJobs :: Int, gAgent :: String
  }

-- | Every key a target may have, with its default. An unknown key is refused: a misspelt @chek@ would
-- otherwise be a session that silently checks nothing.
defaults :: [(String, Json)]
defaults =
  [ ("repl", JNull), ("units", JArr []), ("cabal_args", JStr ""), ("watch", JArr [JStr "src"])
  , ("modules", JArr []), ("prebuild", JNull), ("preload", JArr []), ("warm", JArr [])
  , ("check", JNull), ("checks", JArr []), ("server", JNull), ("env", JObj [])
  , ("load_timeout", JNum 900), ("eval_timeout", JNum 30), ("repl_budget_mb", JNum 6144)
  , ("rts_flags", JStr "-c -Fd0.5"), ("ghc_jobs", JNum 0), ("capabilities", JNum 0), ("prune_gc", JStr "copying"), ("heap_auto", JBool False), ("mem_return", JBool True)
  , ("handover_env", JArr [JStr "GHS_HANDOVER_OUT", JStr "GHS_HANDOVER_IN"])
  , ("unlink_after", JStr "eval"), ("prune_gc_idle_s", JNum 0), ("hygiene", JBool False)
  , ("auto_reload", JBool True), ("watch_check", JBool True), ("watch_refork", JBool True)
  , ("reload_on_commit", JBool False), ("watch_ext", JArr (map JStr [".hs", ".hs-boot", ".c", ".h", ".cabal"]))
  , ("watcher", JStr "auto"), ("poll_interval", JNum 0.2), ("debounce", JNum 0.2)
  , ("status_url", JNull), ("idle_stop_mins", JNum 0), ("async_refork", JBool False), ("fingerprint_files", JArr []), ("fast_start", JBool False), ("watch_typecheck", JBool True), ("profile", JArr [])
  , ("history", JBool True), ("summarize_cmd", JNull), ("summarize_jobs", JNum 8), ("agent", JStr "Agent")
  ]

reserved :: [String]
reserved = ["targets", "state_dir", "default", "sessions", "prices"]

-- | The nearest directory at or above the start holding @ghci-session.json@.
findRoot :: Maybe FilePath -> IO (Either String FilePath)
findRoot start = do
  d0 <- maybe getCurrentDirectory canonicalizePath start
  let go d = do
        there <- doesFileExist (d </> configName)
        if there then pure (Right d)
          else let p = takeDirectory d in
               if p == d then pure (Left ("no " ++ configName ++ " in " ++ d0 ++ " or above (see `ghci-session init`)")) else go p
  go d0

loadConf :: FilePath -> IO (Either String Conf)
loadConf root0 = do
  root <- canonicalizePath root0
  r <- try (readFile (root </> configName)) :: IO (Either IOException String)
  pure $ case r of
    Left e -> Left (show e)
    Right txt -> case parseJson txt of
      Left e -> Left (configName ++ ": " ++ e)
      Right raw -> build root raw
  where
    build root raw = do
      let targets = lookupObj "targets" raw
          common = [ kv | kv@(k, _) <- fromMaybe [] (obj raw), k `notElem` reserved ]
          rel = fromMaybe ".ghci-session" (lookupStr "state_dir" raw)
      when (null targets) (Left (configName ++ ": no \"targets\""))
      ts <- forM targets $ \(name, t) -> do
        -- `test` is what a target's check is called now; the old names mean the same
        let named k = fromMaybe k (lookup k [("test", "check"), ("tests", "checks"), ("watch_test", "watch_check")])
            merged = foldl (\acc (k, v) -> set (named k) v acc) (JObj defaults) (common ++ fromMaybe [] (obj t))
            unknown = [ k | (k, _) <- fromMaybe [] (obj merged), k `notElem` map fst defaults ]
        let gone = [ k | k <- unknown, k `elem` ["engine", "hygiene_build", "hygiene_module", "zygote_module"] ]
        unless (null gone) (Left ("target " ++ show name ++ ": " ++ show gone ++ " no longer exist(s): the session's GHCi is always the engine, and it prunes and forks itself -- remove the key(s)"))
        unless (null unknown) (Left ("target " ++ show name ++ ": unknown key(s) " ++ show unknown))
        Right (name, merged)
      let sessions = [ (s, strs m) | (s, m) <- lookupObj "sessions" raw ]
      mapM_ (\(s, ms) -> do
               when (isJust (lookup s ts)) (Left ("session " ++ show s ++ " has the name of a target"))
               mapM_ (\m -> unless (isJust (lookup m ts)) (Left ("session " ++ show s ++ ": unknown member " ++ show m))) ms) sessions
      Right Conf { cRoot = root, cStateDir = root </> rel, cStateRel = rel
                 , cDefault = fromMaybe (fst (head ts)) (lookupStr "default" raw)
                 , cTargets = ts, cSessions = sessions, cPrices = lookupObj "prices" raw }

targetJson :: Conf -> String -> Json
targetJson conf name = fromMaybe JNull (lookup name (cTargets conf))

sessionNames :: Conf -> [String]
sessionNames conf = map fst (cTargets conf) ++ map fst (cSessions conf)

-- composed sessions ----------------------------------------------------------

membersFile :: Conf -> String -> FilePath
membersFile conf s = cStateDir conf </> (s ++ ".members")

-- | A composed session's members: what was last chosen, else what the config declares.
readMembers :: Conf -> String -> IO [String]
readMembers conf s = do
  r <- try (readFile (membersFile conf s)) :: IO (Either IOException String)
  pure $ case r >>= either (const (Left (userError "json"))) Right . parseJson of
    Right j | Just _ <- arr j -> [ m | m <- strs j, isJust (lookup m (cTargets conf)) ]
    _ -> fromMaybe [] (lookup s (cSessions conf))

writeMembers :: Conf -> String -> [String] -> IO ()
writeMembers conf s ms = do
  createDirectoryIfMissing True (cStateDir conf)
  writeFile (membersFile conf s) (encode (JArr (map JStr ms)))

-- resolution -----------------------------------------------------------------

jStr :: String -> Json -> String
jStr k t = fromMaybe "" (lookupStr k t)

jMaybeStr :: String -> Json -> Maybe String
jMaybeStr k t = case lookupStr k t of { Just "" -> Nothing; r -> r }

jNum :: String -> Json -> Double
jNum k t = fromMaybe 0 (lookupNum k t)

jBool :: String -> Json -> Bool
jBool k t = fromMaybe False (lookupBool k t)

jStrs :: String -> Json -> [String]
jStrs k t = case t .: k of { JStr s -> words s; j -> strs j }

-- | "warm" and "preload" hold EXPRESSIONS: a string is one expression, not words.
jExprs :: String -> Json -> [String]
jExprs k t = strs (t .: k)

envOf :: Json -> [(String, String)]
envOf j = [ (k, showVal v) | (k, v) <- fromMaybe [] (obj j) ]
  where showVal (JStr s) = s
        showVal (JBool b) = if b then "1" else "0"
        showVal v = encode v

checksOf :: String -> Json -> Either String [Check]
checksOf member t =
  let raw = case lookupArr "checks" t of { [] -> [ c | c@(JObj _) <- [t .: "check"] ]; cs -> cs }
  in forM raw $ \c -> case lookupStr "expr" c of
       Nothing -> Left ("target " ++ show member ++ ": a check needs \"expr\"")
       Just e -> Right Check
         { ckMember = maybe member (\n -> member ++ ":" ++ n) (lookupStr "name" c), ckExpr = e
         , ckPass = lookupStr "pass" c
         , ckFail = case c .: "fail" of { JNull -> Just "^\\[FAIL\\]"; JStr "" -> Nothing; JStr f -> Just f; _ -> Nothing }
         , ckLog = lookupStr "log" c, ckTimeout = lookupNum "timeout" c }

serverOf :: String -> Json -> Either String (Maybe Server)
serverOf member t = case t .: "server" of
  s@(JObj _) -> case lookupStr "action" s of
    Nothing -> Left ("target " ++ show member ++ ": a server needs \"action\"")
    Just a -> Right (Just Server
      { svMember = member, svAction = a, svPort = round <$> lookupNum "port" s
      , svEnv = mergeEnv (envOf (t .: "env")) (envOf (s .: "env"))
      , svPrefork = lookupStr "prefork" s, svServeOnLoad = jBool "serve_on_load" s
      , svVerifyTimeout = fromMaybe 60 (lookupNum "verify_timeout" s), svUnits = jStrs "units" t
      , svSpec = encode s })
  _ -> Right Nothing

mergeEnv :: [(String, String)] -> [(String, String)] -> [(String, String)]
mergeEnv a b = [ kv | kv@(k, _) <- a, k `notElem` map fst b ] ++ b

-- | A session's configuration. Checks and servers stay PER MEMBER: each carries its own log, patterns and
-- port, and merging those would lose exactly what a verdict is made of.
resolve :: Conf -> String -> IO (Either String Cfg)
resolve conf session = do
  let composed = isJust (lookup session (cSessions conf))
  members <- if composed then readMembers conf session else pure [session]
  pure $ if not composed && not (isJust (lookup session (cTargets conf)))
    then Left ("unknown session " ++ show session ++ "; have " ++ unwords (sessionNames conf))
    else build composed members
  where
    build composed members = do
      let ts = [ targetJson conf m | m <- members ]
          t0 = case ts of { (t : _) -> t; [] -> JObj defaults }
          union f = nub (concatMap f ts)
          orDef k f = if null ts then f (JObj defaults) else k
          vals = [ ("session", session), ("root", cRoot conf), ("state", cStateDir conf)
                 , ("dylib", if os == "darwin" then "dylib" else "so") ]
          ex = expand vals
      checks <- concat <$> mapM (\(m, t) -> checksOf m t) (zip members ts)
      servers <- concat <$> mapM (\(m, t) -> maybe [] pure <$> serverOf m t) (zip members ts)
      when (composed && length ts > 1 && any (isJust . jMaybeStr "repl") ts)
        (Left ("session " ++ show session ++ ": a member with its own \"repl\" command cannot be composed (give it \"units\" instead)"))
      let mins = map (jNum "idle_stop_mins") ts
          hand = case jStrs "handover_env" t0 of { [a, b] -> (a, b); _ -> ("GHS_HANDOVER_OUT", "GHS_HANDOVER_IN") }
      Right Cfg
        { gMembers = members, gComposed = composed
        , gRepl = ex <$> jMaybeStr "repl" t0, gUnits = union (jStrs "units"), gCabalArgs = jStr "cabal_args" t0
        , gWatch = orDef (union (jStrs "watch")) (jStrs "watch"), gWatchExt = orDef (union (jStrs "watch_ext")) (jStrs "watch_ext")
        , gModules = union (jStrs "modules")
        , gPrebuild = ex <$> jMaybeStr "prebuild" t0, gPreload = map ex (union (jExprs "preload")), gWarm = map ex (union (jExprs "warm"))
        , gChecks = [ c { ckExpr = ex (ckExpr c), ckLog = ex <$> ckLog c } | c <- checks ]
        , gServers = [ s { svAction = ex (svAction s), svPrefork = ex <$> svPrefork s, svEnv = [ (k, ex v) | (k, v) <- svEnv s ], svSpec = ex (svSpec s) } | s <- servers ]
        , gEnv = [ (k, ex v) | (k, v) <- foldl mergeEnv [] (map (envOf . (.: "env")) ts) ]
        , gLoadTimeout = maxOf "load_timeout" ts, gEvalTimeout = maxOf "eval_timeout" ts, gBudgetMb = maxOf "repl_budget_mb" ts
        , gRtsFlags = jStr "rts_flags" t0, gPruneGc = jStr "prune_gc" t0, gHeapAuto = jBool "heap_auto" t0, gMemReturn = jBool "mem_return" t0, gGhcJobs = round (maxOf "ghc_jobs" ts), gCapabilities = round (maxOf "capabilities" ts)
        , gHygiene = any (jBool "hygiene") ts
        , gHandoverEnv = hand, gUnlinkAfter = jStr "unlink_after" t0, gPruneGcIdle = jNum "prune_gc_idle_s" t0
        , gAutoReload = null ts || any (jBool "auto_reload") ts
        , gWatchCheck = all (jBool "watch_check") ts, gWatchRefork = all (jBool "watch_refork") ts
        , gReloadOnCommit = any (jBool "reload_on_commit") ts
        , gWatcher = jStr "watcher" t0, gPollInterval = jNum "poll_interval" t0, gDebounce = jNum "debounce" t0
        , gStatusUrl = ex <$> jMaybeStr "status_url" t0
          -- a composed session idles out only if every member agrees to, at the longest of their waits
        , gIdleStopMins = if not (null mins) && all (> 0) mins then maximum mins else 0
        , gAsyncRefork = any (jBool "async_refork") ts
        , gFingerprintFiles = map ex (union (jStrs "fingerprint_files"))
        , gFastStart = not (null ts) && all (jBool "fast_start") ts
        , gWatchTypecheck = all (jBool "watch_typecheck") ts
        , gProfile = [ exJ st | t <- ts, st <- lookupArr "profile" t ]
        , gHistory = all (jBool "history") ts, gSummarizeCmd = ex <$> jMaybeStr "summarize_cmd" t0
        , gSummarizeJobs = max 1 (round (maxOf "summarize_jobs" ts)), gAgent = jStr "agent" t0
        }
    exJ j = case j of { JObj kvs -> JObj [ (k, case v of { JStr x -> JStr (expand vals0 x); _ -> v }) | (k, v) <- kvs ]; _ -> j }
    vals0 = [ ("session", session), ("root", cRoot conf), ("state", cStateDir conf), ("dylib", if os == "darwin" then "dylib" else "so") ]
    maxOf k ts = maximum (fromMaybe 0 (lookupNum k (JObj defaults)) : map (jNum k) ts)
      `seq` (if null ts then fromMaybe 0 (lookupNum k (JObj defaults)) else maximum (map (jNum k) ts))

-- | @{session}@, @{root}@, @{state}@, @{dylib}@ in any string of a session's configuration.
expand :: [(String, String)] -> String -> String
expand vals = go
  where
    go [] = []
    go s@(c : r) = case [ (v, drop (length k + 2) s) | (k, v) <- vals, ("{" ++ k ++ "}") `isPrefixOf` s ] of
      ((v, rest) : _) -> v ++ go rest
      [] -> c : go r
