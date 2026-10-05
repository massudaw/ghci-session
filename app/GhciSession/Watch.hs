-- | Knowing WHAT changed and WHEN to look.
--
-- The truth about what changed is always the scan ('scan': every watched source's modification time); a
-- waiter only says "look now". With kernel events a save is noticed in milliseconds and its burst of writes
-- is over when the events stop, so the watcher need not sleep a fixed poll and a fixed debounce before every
-- reload. kqueue on macOS/BSD (a descriptor per file and directory), inotify on Linux (a watch per
-- directory), a poll anywhere and whenever the others cannot be set up. A waiter may miss an event: the
-- daemon still scans every couple of seconds.
module GhciSession.Watch
  ( Sig, scan, toRaw, fromRaw
  , Waiter, makeWaiter, waiterKind, waiterUpdate, waiterWait, waiterSettle, waiterClose, toPolling
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (foldM, forM_, unless, when)
import Data.IORef
import qualified Data.Map.Strict as M
import Foreign.C.Types (CInt)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.Posix.ByteString.FilePath (RawFilePath)
import qualified System.Posix.Directory.ByteString as RD
import System.Posix.Files (FileStatus, fileID, isDirectory, modificationTimeHiRes)
import qualified System.Posix.Files.ByteString as R

import GhciSession.Sys

-- | A watched source's path and modification time (seconds). Paths are BYTES, as the OS has them: as
-- @String@s the 526 sources of one project were 1 MB held for the life of the daemon (24 bytes a character)
-- and 5.6 MB allocated by every scan.
type Sig = M.Map RawFilePath Double

toRaw :: FilePath -> RawFilePath
toRaw = TE.encodeUtf8 . T.pack

fromRaw :: RawFilePath -> FilePath
fromRaw = T.unpack . TE.decodeUtf8With TE.lenientDecode

rawDir :: RawFilePath -> RawFilePath
rawDir p = let d = B.dropWhileEnd (== 47) (B.dropWhileEnd (/= 47) p) in if B.null d then BC.pack "/" else d

-- | Modification times of every watched source: the given directories (recursively, skipping dot
-- directories and @dist-newstyle@) and files, relative to the root, with one of the extensions.
scan :: FilePath -> [FilePath] -> [String] -> IO Sig
scan root dirs exts = foldM top M.empty dirs
  where
    bexts = map BC.pack exts
    slash a b = B.concat [a, BC.pack "/", b]
    top acc d = do
      let p = if take 1 d == "/" then toRaw d else slash (toRaw root) (toRaw d)      -- (an absolute path is itself)
      st <- try (R.getFileStatus p) :: IO (Either IOException FileStatus)
      case st of
        Left _ -> pure acc
        Right s | isDirectory s -> walk acc p
                | otherwise -> pure (M.insert p (realToFrac (modificationTimeHiRes s)) acc)
    walk acc dir = do
      names <- either (\e -> const [] (e :: IOException)) id <$> try (listDir dir)
      foldM (entry dir) acc [ n | n <- names, B.take 1 n /= BC.pack ".", n /= BC.pack "dist-newstyle" ]
    entry dir acc n
      | any (`B.isSuffixOf` n) bexts = stat1 (\s -> if isDirectory s then walk acc p else pure (M.insert p (realToFrac (modificationTimeHiRes s)) acc))
      | otherwise = stat1 (\s -> if isDirectory s then walk acc p else pure acc)
      where p = slash dir n
            stat1 k = (try (R.getFileStatus p) :: IO (Either IOException FileStatus)) >>= either (const (pure acc)) k
    listDir dir = do
      ds <- RD.openDirStream dir
      let go acc = do
            n <- RD.readDirStream ds
            if B.null n then pure acc else go (n : acc)
      r <- go []
      RD.closeDirStream ds
      pure r

data Waiter = Waiter
  { wKind :: IORef Int                           -- ^ 0 poll, 1 kqueue, 2 inotify
  , wHandle :: Maybe CInt
  , wWatched :: IORef (M.Map RawFilePath (CInt, Integer))   -- ^ path -> (descriptor or watch, inode)
  , wInterval :: Double, wDebounce :: Double
  }

waiterKind :: Waiter -> IO String
waiterKind w = (\k -> case k of { 1 -> "kqueue"; 2 -> "inotify"; _ -> "poll" }) <$> readIORef (wKind w)

-- | The best waiter this platform offers (@"auto"@), or the polling one (@"poll"@, or on any failure).
makeWaiter :: String -> Double -> Double -> [RawFilePath] -> (String -> IO ()) -> IO Waiter
makeWaiter kind interval debounce paths logF = do
  k <- if kind == "poll" then pure 0 else watchKind
  h <- if k == 0 then pure Nothing else watchNew
  ref <- newIORef M.empty
  kr <- newIORef (maybe 0 (const k) h)
  let w = Waiter kr h ref interval debounce
  ok <- waiterUpdate w paths
  unless ok $ do
    logF ("watch: no kernel events for these paths -- polling every " ++ show interval ++ "s")
    toPolling w
  pure w

toPolling :: Waiter -> IO ()
toPolling w = do
  waiterClose w
  writeIORef (wKind w) 0

-- | Watch these files and directories. A file an editor saved by rename is a NEW inode: the old descriptor
-- watches a file that is gone. 'False' if they cannot all be watched (too many descriptors).
waiterUpdate :: Waiter -> [RawFilePath] -> IO Bool
waiterUpdate w paths = do
  k <- readIORef (wKind w)
  case (k, wHandle w) of
    (1, Just kq) -> do
      budget <- watchBudget
      let want = M.fromList [ (p, ()) | p <- paths ++ map rawDir paths ]
      if M.size want > budget then pure False else do
        have <- readIORef (wWatched w)
        forM_ (M.toList (have `M.difference` want)) (\(_, (fd, _)) -> watchRm kq fd)
        kept <- foldM (one kq have) M.empty (M.keys want)
        writeIORef (wWatched w) kept
        pure True
    (2, Just fd) -> do
      dirs <- mapM (\p -> either (\e -> const (rawDir p) (e :: IOException)) (\s -> if isDirectory s then p else rawDir p)
                            <$> try (R.getFileStatus p)) paths
      let want = M.fromList [ (d, ()) | d <- dirs ]
      have <- readIORef (wWatched w)
      forM_ (M.toList (have `M.difference` want)) (\(_, (wd, _)) -> watchRm fd wd)
      new <- foldM (\acc d -> case M.lookup d have of
                       Just x -> pure (M.insert d x acc)
                       Nothing -> maybe acc (\wd -> M.insert d (wd, 0) acc) <$> watchAdd fd d) M.empty (M.keys want)
      writeIORef (wWatched w) new
      pure True
    _ -> pure True
  where
    one kq have acc p = do
      st <- try (R.getFileStatus p) :: IO (Either IOException FileStatus)
      case st of
        Left _ -> pure acc
        Right s -> do
          let ino = fromIntegral (fileID s)
          case M.lookup p have of
            Just (fd, i) | i == ino -> pure (M.insert p (fd, i) acc)
            old -> do
              mapM_ (\(fd, _) -> watchRm kq fd) old
              maybe acc (\fd -> M.insert p (fd, ino) acc) <$> watchAdd kq p

-- | Wait up to the seconds given; 'True' if something may have changed (always, when polling).
waiterWait :: Waiter -> Double -> IO Bool
waiterWait w secs = do
  k <- readIORef (wKind w)
  case (k, wHandle w) of
    (0, _) -> threadDelay (round (min secs (wInterval w) * 1e6)) >> pure True
    (_, Just h) -> watchWait h (round (secs * 1000))
    _ -> threadDelay (round (secs * 1e6)) >> pure True

-- | Let a burst of writes finish: with events, until they stop for 50 ms (at most the debounce, or 0.3 s);
-- when polling, the debounce.
waiterSettle :: Waiter -> IO ()
waiterSettle w = do
  k <- readIORef (wKind w)
  if k == 0 then threadDelay (round (wDebounce w * 1e6)) else go (0 :: Int)
  where
    limit = round (max (wDebounce w) 0.3 / 0.05) :: Int
    go n = when (n < limit) $ do
      more <- waiterWait w 0.05
      when more (go (n + 1))

waiterClose :: Waiter -> IO ()
waiterClose w = case wHandle w of
  Nothing -> pure ()
  Just h -> do
    have <- readIORef (wWatched w)
    forM_ (M.elems have) (\(x, _) -> watchRm h x)
    writeIORef (wWatched w) M.empty
