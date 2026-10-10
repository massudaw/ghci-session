{-# LANGUAGE ScopedTypeVariables #-}

-- | Finding and reaping what a session left behind.
--
-- Three kinds of leftover, each invisible to @status@ and unkillable by @stop@ because nothing records it: a
-- DAEMON whose pid file is gone or names another process; a SERVER still running after its session's daemon
-- died; a BUILD process -- the @ghc --interactive@ a @cabal repl@ exec'd -- reparented to init after its
-- daemon went, still holding the lock on dist-newstyle.
--
-- Attribution is by ABSOLUTE PATH, never by name: several checkouts of one project are routinely live at
-- once. A daemon carries @--root <abs>@ on its command line; a build process this project's dist-newstyle
-- or state directory.
module GhciSession.Gc (Leftovers (..), orphanBuilds, isPidFile, findLeftovers, runGc, daemonPid, listProcesses, neverLoaded, holdsObjects, deadSessions) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (filterM, forM, forM_, unless, void, when)
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import System.Directory
import System.FilePath ((</>))
import System.Posix.Signals (Signal, sigKILL, sigTERM, signalProcess)
import System.Posix.Types (CPid (..))
import Text.Printf (printf)

import GhciSession.Config
import GhciSession.Sys

data Leftovers = Leftovers { lDaemons :: [(String, Int)], lServers :: [(String, String, Int)], lBuilds :: [(Int, String)] }

-- | Every process: (pid, parent, command line). The command line is read only for the processes that can be
-- leftovers -- those whose parent is init: a daemon detaches, and an orphan is by definition reparented --
-- so this is a kernel table and a few dozen lookups, not a @ps -o command@ of everything (40-60 ms).
listProcesses :: IO [(Int, Int, String)]
listProcesses = do
  rows <- processTable
  forM rows $ \(pid, pp, _) -> (,,) pid pp <$> (if pp /= 1 then pure "" else do
    -- on macOS nearly every process is launchd's child: ask for the arguments only of what could be ours
    nm <- processName pid
    if null nm || any (`isInfixOf` nm) ["ghc", "cabal", "ghci"] then processArgs pid else pure "")

descendants :: [(Int, Int, String)] -> Int -> [Int]
descendants procs pid = go [] [ c | (c, pp, _) <- procs, pp == pid ]
  where go seen [] = seen
        go seen (p : todo) | p `elem` seen || p == pid = go seen todo
                           | otherwise = go (seen ++ [p]) (todo ++ [ c | (c, pp, _) <- procs, pp == p ])

killTree :: Int -> [Int] -> IO ()
killTree pid kids = forM_ [(sigTERM, 30 :: Int), (sigKILL, 10)] $ \(sig, n) -> do
  alive <- anyAlive
  when alive $ do
    forM_ (pid : kids) (sendSig sig)
    let wait k = when (k > 0) (anyAlive >>= \a -> when a (threadDelay 100000 >> wait (k - 1)))
    wait n
  where anyAlive = or <$> mapM pidAlive (pid : kids)

sendSig :: Signal -> Int -> IO ()
sendSig sig p = void (try (signalProcess sig (CPid (fromIntegral p))) :: IO (Either IOException ()))

readInt :: FilePath -> IO (Maybe Int)
readInt p = (>>= \t -> case reads t of { [(n, r)] | all (`elem` " \n") r -> Just n; _ -> Nothing }) <$> readFileMaybe p

-- | State dirs that are sessions (not clib, not a members file).
sessionDirs :: Conf -> IO [String]
sessionDirs conf = do
  let sd = cStateDir conf
  names <- either (\(_ :: IOException) -> []) id <$> try (getDirectoryContents sd)
  filterM (\e -> do
             isD <- doesDirectoryExist (sd </> e)
             marks <- or <$> mapM (\f -> doesFileExist (sd </> e </> f)) ["status", "pid", "daemon.log"]
             pure (isD && marks && not ("." `isPrefixOf` e))) (sort names)

daemonPid :: Conf -> String -> IO (Maybe Int)
daemonPid conf name = do
  mp <- readInt (cStateDir conf </> name </> "pid")
  case mp of
    Nothing -> pure Nothing
    Just pid -> (\a -> if a then Just pid else Nothing) <$> pidAlive pid

findLeftovers :: Conf -> IO Leftovers
findLeftovers conf = do
  procs <- listProcesses
  root <- canonicalizePath (cRoot conf)
  dirs <- sessionDirs conf
  let names = nub (sessionNames conf ++ dirs)
  live <- forM names (\n -> (,) n <$> daemonPid conf n)
  -- a daemon of THIS root that its state dir does not name
  daemons <- fmap catMaybes $ forM procs $ \(pid, _, cmd) -> case daemonOf (words cmd) of
    Just (r, name) -> do
      r' <- either (\(_ :: IOException) -> r) id <$> try (canonicalizePath r)
      pure (if r' == root && lookup name live /= Just (Just pid) then Just (name, pid) else Nothing)
    Nothing -> pure Nothing
  servers <- fmap concat $ forM dirs $ \n -> do
    fs <- either (\(_ :: IOException) -> []) id <$> try (getDirectoryContents (cStateDir conf </> n))
    fmap catMaybes $ forM [ f | f <- sort fs, "server-" `isPrefixOf` f, ".pid" `isSuffixOf` f ] $ \f -> do
      mp <- readInt (cStateDir conf </> n </> f)
      alive <- maybe (pure False) pidAlive mp
      pure (case mp of { Just pid | alive -> Just (n, drop 7 (take (length f - 4) f), pid, isJust (fromMaybe Nothing (lookup n live))); _ -> Nothing })
  -- a live chat (its pid is in the session's chat.pid) is no leftover, whatever its command line says (a path with
  -- "ghci-session" holds "ghc"): nor what it started; nor any process a session's state names
  named <- fmap concat $ forM dirs $ \n -> do
    fs <- either (\(_ :: IOException) -> []) id <$> try (getDirectoryContents (cStateDir conf </> n))
    fmap catMaybes $ forM [ f | f <- fs, isPidFile f ] $ \f -> do
      mp <- readInt (cStateDir conf </> n </> f)
      alive <- maybe (pure False) pidAlive mp
      pure (if alive then mp else Nothing)
  let tracked = [ pid | (_, _, pid, _) <- servers ]
      owned = tracked ++ concat [ p : descendants procs p | p <- catMaybes (map snd live) ++ map snd daemons ++ named ]
  sd <- either (\(_ :: IOException) -> cStateDir conf) id <$> try (canonicalizePath (cStateDir conf))
  let needles = [root </> "dist-newstyle", sd]
      builds = orphanBuilds procs owned needles
  pure (Leftovers daemons [ (n, m, pid) | (n, m, pid, sessionUp) <- servers, not sessionUp ] builds)
  where
    daemonOf ws = case dropWhile (/= "--root") ws of
      (_ : r : "_daemon" : name : _) | any ("ghci-session" `isInfixOf`) ws || any ("ghci_session" `isInfixOf`) ws -> Just (r, name)
      _ -> Nothing

-- | The orphaned build processes (parent init, a command line with ghc or cabal and one of the needles) that no
-- session owns. A chat's own command line holds "ghc" (in "ghci-session") and the build directory: it is in @owned@.
orphanBuilds :: [(Int, Int, String)] -> [Int] -> [String] -> [(Int, String)]
orphanBuilds procs owned needles =
  [ (pid, cmd) | (pid, pp, cmd) <- procs, pp == 1, pid `notElem` owned, any (`isInfixOf` cmd) needles, any (`isInfixOf` cmd) ["ghc", "cabal"] ]

-- | A file of a session's state that names a process: @pid@, @chat.pid@, @server-X.pid@.
isPidFile :: FilePath -> Bool
isPidFile f = f == "pid" || ".pid" `isSuffixOf` f

-- | Does a session's state say it never loaded? Its status says @loaded=-@, and it holds no history, no record of
-- sources loaded, no turn: what a boot that failed or timed out leaves.
neverLoaded :: String -> Bool
neverLoaded status = "loaded=-" `isInfixOf` status

-- | Does a directory hold, at any depth, a compiled module (@.o@ or @.hi@)? A session that timed out in the middle of
-- a first -O2 build never loaded, but keeps what it compiled -- over fifteen minutes of work the next boot resumes from.
holdsObjects :: FilePath -> IO Bool
holdsObjects dir = do
  es <- either (\(_ :: IOException) -> []) id <$> try (listDirectory dir)
  let go [] = pure False
      go (n : r) | any (`isSuffixOf` n) [".o", ".hi"] = pure True
                 | otherwise = do
                     isD <- doesDirectoryExist (dir </> n)
                     hit <- if isD then holdsObjects (dir </> n) else pure False
                     if hit then pure True else go r
  go es

-- | Sessions whose daemon is not running (no live pid, no daemon process of that name) and that never loaded, idle
-- longer than ten minutes (a boot may still be under way), and hold no compiled module: (name, idle days, MB).
deadSessions :: Conf -> [(Int, Int, String)] -> IO [(String, Double, Double)]
deadSessions conf procs = do
  dirs <- sessionDirs conf
  t <- now
  fmap catMaybes $ forM dirs $ \name -> do
    let d = cStateDir conf </> name
    up <- daemonPid conf name
    st <- fromMaybe "" <$> readFileMaybe (d </> "status")
    used <- or <$> mapM (\f -> doesPathExist (d </> f)) ["history", "loaded_sources.tsv", "turn.json", "usage.jsonl", "chat.pid"]
    compiled <- if used then pure True else holdsObjects (d </> "objs")
    mt <- modTime (d </> "status") >>= maybe (modTime d) (pure . Just)
    let booting = any (\(_, _, c) -> let ws = words c in "_daemon" `elem` ws && name `elem` ws) procs
    case mt of
      Just m | up == Nothing, not booting, not used, not compiled, neverLoaded st, m < t - 600 -> do
        size <- duMb d
        pure (Just (name, (t - m) / 86400, size))
      _ -> pure Nothing

describeBuild :: String -> String
describeBuild cmd = case words cmd of { (w : _) -> reverse (takeWhile (/= '/') (reverse w)); [] -> "?" }

duMb :: FilePath -> IO Double
duMb dir = do
  names <- either (\(_ :: IOException) -> []) id <$> try (getDirectoryContents dir)
  fmap sum $ forM [ n | n <- names, n `notElem` [".", ".."] ] $ \n -> do
    let p = dir </> n
    isD <- doesDirectoryExist p
    if isD then duMb p else either (\(_ :: IOException) -> 0) ((/ 1e6) . fromIntegral) <$> try (getFileSize p)

-- | Reap the leftovers; with @days@ > 0 also prune state dirs of sessions idle longer than that.
runGc :: Conf -> Bool -> Double -> IO Int
runGc conf dry days = do
  procs <- listProcesses
  found <- findLeftovers conf
  let verb = if dry then "would reap" else "reaping" :: String
  forM_ (lDaemons found) $ \(name, pid) -> do
    putStrLn (printf "gc: %s orphaned daemon pid %d (session %s: not the one its state dir records)" verb pid name)
    unless dry (killTree pid (descendants procs pid))
  forM_ (lServers found) $ \(name, member, pid) -> do
    putStrLn (printf "gc: %s server %s pid %d: its session %s is not running" verb member pid name)
    unless dry $ do
      killTree pid (descendants procs pid)
      void (try (removeFile (cStateDir conf </> name </> ("server-" ++ member ++ ".pid"))) :: IO (Either IOException ()))
  found2 <- if dry then pure found else findLeftovers conf
  procs2 <- if dry then pure procs else listProcesses
  forM_ (lBuilds found2) $ \(pid, cmd) -> do
    putStrLn (printf "gc: %s orphaned build process %d (%s) -- it holds a lock on dist-newstyle" verb pid (describeBuild cmd))
    unless dry (killTree pid (descendants procs2 pid))
  when (days > 0) $ do
    t <- now
    dirs <- sessionDirs conf
    forM_ dirs $ \name -> do
      let d = cStateDir conf </> name
      up <- daemonPid conf name
      mt <- modTime (d </> "status") >>= maybe (modTime d) (pure . Just)
      case (up, mt) of
        (Nothing, Just m) | m < t - days * 86400 -> do
          size <- duMb d
          if dry then putStrLn (printf "gc: would prune %s (idle %.1fd, %.0f MB)" name ((t - m) / 86400) size)
            else do
              void (try (removeDirectoryRecursive d) :: IO (Either IOException ()))
              putStrLn (printf "gc: pruned %s (idle %.1fd, freed %.0f MB)" name ((t - m) / 86400) size)
        _ -> pure ()
  dead <- deadSessions conf procs
  forM_ dead $ \(name, idle, size) -> do
    putStrLn (printf "gc: %s session %s: its daemon is not running and it never loaded (idle %.1fd, %.1f MB)" verb name idle size)
    unless dry (void (try (removeDirectoryRecursive (cStateDir conf </> name)) :: IO (Either IOException ())))
  let n = length (lDaemons found) + length (lServers found) + length (lBuilds found2) + length dead
  when (n == 0) (putStrLn "gc: no orphaned daemons, servers or build processes")
  pure n
