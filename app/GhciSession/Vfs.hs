-- | Virtual File System (VFS) and line-budget modeler for ghci-session.
-- Tracks files loaded by the session, evaluates line counts in memory (<1ms),
-- monitors budget compliance (e.g. <250 lines), and decorates with git & runtime sync status.
module GhciSession.Vfs
  ( VfsFile (..)
  , inspectLoaded
  , inspectPath
  , formatVfsTable
  , vfsJson
  , formatLineBudget
  ) where

import Control.Exception (try, IOException)
import Control.Monad (forM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), makeRelative, normalise, takeExtension)
import System.Posix.Files (FileStatus, getFileStatus, modificationTimeHiRes)
import System.Process (readProcess)
import Text.Printf (printf)

import GhciSession.Config (Conf (..), stateOf)
import GhciSession.Json (Json (..))

data VfsFile = VfsFile
  { vfPath      :: !FilePath   -- ^ path relative to project root
  , vfLines     :: !Int        -- ^ total line count
  , vfBytes     :: !Int        -- ^ byte size
  , vfBudget    :: !Int        -- ^ line budget threshold (e.g. 250)
  , vfOver      :: !Bool       -- ^ whether file exceeds line budget
  , vfGitStatus :: !String     -- ^ \"[clean]\", \"[modified]\", \"[untracked]\"
  , vfSyncState :: !String     -- ^ \"[synced]\", \"[stale]\", \"[unloaded]\"
  , vfIsLoaded  :: !Bool       -- ^ whether tracked in session's loaded_sources.tsv
  } deriving (Show, Eq)

-- | Read the loaded sources TSV file maintained by the daemon for a session.
-- Format: <mtime_ns>\t<path>
readLoadedTsv :: Conf -> String -> IO (M.Map FilePath Integer)
readLoadedTsv conf name = do
  let tsvPath = stateOf conf name </> "loaded_sources.tsv"
  res <- try (readFile tsvPath) :: IO (Either IOException String)
  pure $ case res of
    Left _ -> M.empty
    Right content ->
      M.fromList
        [ (normalise p, readTs (takeWhile (/= '\t') line))
        | line <- lines content
        , '\t' `elem` line
        , let p = dropWhile isSpace (dropWhile (/= '\t') line)
        , not (null p)
        ]
  where
    readTs s = case reads s of
      [(n, "")] -> n
      _         -> 0

-- | Check Git status across the entire repository in one batch call (<5ms).
gitStatusLookup :: FilePath -> IO (FilePath -> String)
gitStatusLookup root = do
  res <- try (readProcess "git" ["-C", root, "status", "--porcelain"] "") :: IO (Either IOException String)
  pure $ case res of
    Left _ -> const "[?]"
    Right out ->
      let entries = [ (normalise (drop 3 l), statusOf (take 2 l))
                    | l <- lines out
                    , length l >= 4
                    ]
          m = M.fromList entries
      in \rel -> fromMaybe "[clean]" (M.lookup (normalise rel) m)
  where
    statusOf st
      | "??" `isPrefixOf` st = "[untracked]"
      | otherwise            = "[modified]"

-- | Inspect a single file, computing line count and budget compliance in memory.
inspectOne :: FilePath -> FilePath -> Int -> Maybe Integer -> (FilePath -> String) -> IO (Maybe VfsFile)
inspectOne root rel budget mLoadedNano gitLookup = do
  let full = root </> rel
  res <- try (B.readFile full) :: IO (Either IOException B.ByteString)
  case res of
    Left _ -> pure Nothing
    Right bs -> do
      let nLines = if B.null bs then 0 else BC.count '\n' bs + (if BC.last bs == '\n' then 0 else 1)
          nBytes = B.length bs
          st = gitLookup rel
      mStatus <- try (getFileStatus full) :: IO (Either IOException FileStatus)
      let (syncState, isLoaded) = case (mStatus, mLoadedNano) of
            (Right fs, Just loadedNano) ->
              let diskNano = round ((realToFrac (modificationTimeHiRes fs) :: Double) * 1e9) :: Integer
              in if diskNano <= loadedNano + 1000000 -- 1ms tolerance
                   then ("[synced]", True)
                   else ("[stale]", True)
            (_, Just _)  -> ("[synced]", True)
            (_, Nothing) -> ("[unloaded]", False)
      pure $ Just $ VfsFile
        { vfPath      = rel
        , vfLines     = nLines
        , vfBytes     = nBytes
        , vfBudget    = budget
        , vfOver      = nLines > budget
        , vfGitStatus = st
        , vfSyncState = syncState
        , vfIsLoaded  = isLoaded
        }

-- | Recursively scan directory for source files (.hs, .cabal, etc.)
scanSources :: FilePath -> FilePath -> IO [FilePath]
scanSources root dir = do
  let full = root </> dir
  isDir <- doesDirectoryExist full
  if not isDir
    then do
      isFile <- doesFileExist full
      pure [ dir | isFile ]
    else do
      entries <- listDirectory full
      paths <- forM entries $ \e -> do
        let sub = if null dir then e else dir </> e
            subFull = root </> sub
        d <- doesDirectoryExist subFull
        if d
          then if e `elem` [".git", ".ghci-session", "dist-newstyle", ".bin", "node_modules"]
                 then pure []
                 else scanSources root sub
          else if takeExtension e `elem` [".hs", ".cabal", ".project"]
                 then pure [sub]
                 else pure []
      pure (concat paths)

-- | Inspect loaded sources for a session, or all sources if TSV is missing.
inspectLoaded :: Conf -> String -> Int -> Maybe String -> IO [VfsFile]
inspectLoaded conf name budget mFilter = do
  loadedMap <- readLoadedTsv conf name
  let root = cRoot conf
      loadedList = M.keys loadedMap
  sources <- if null loadedList
               then scanSources root ""
               else pure loadedList
  gitLookup <- gitStatusLookup root
  let matched = case mFilter of
        Nothing -> sources
        Just pat -> filter (\p -> pat `isPrefixOf` p || pat `elem` words p || T.pack pat `T.isInfixOf` T.pack p) sources
  results <- mapM (\rel -> inspectOne root rel budget (M.lookup (normalise rel) loadedMap) gitLookup) matched
  pure (sortOn vfPath [ f | Just f <- results ])

-- | Inspect sources under a given path/pattern directly.
inspectPath :: FilePath -> Int -> Maybe String -> IO [VfsFile]
inspectPath root budget mFilter = do
  allFiles <- scanSources root (case mFilter of Just f | not (null f) -> f; _ -> "")
  gitLookup <- gitStatusLookup root
  results <- mapM (\rel -> inspectOne root rel budget Nothing gitLookup) allFiles
  pure (sortOn vfPath [ f | Just f <- results ])

-- | Format a compact budget summary string for tool responses (e.g. after write/edit).
formatLineBudget :: Int -> Int -> String
formatLineBudget lines' budget
  | lines' > budget = printf "%d lines [OVER BUDGET: %d/%d lines!]" lines' lines' budget
  | otherwise       = printf "%d lines [budget: %d/%d lines]" lines' lines' budget

-- | Format a rich VFS table for console output or agent tools.
formatVfsTable :: [VfsFile] -> Int -> T.Text
formatVfsTable files budget =
  if null files
    then T.pack "No matching source files found in session VFS."
    else T.unlines (header : fileLines ++ [footer])
  where
    header = T.pack $ printf "Virtual File System & Line Budget (Threshold: %d lines):\n  %-32s %7s %8s  %-15s %-10s %-9s"
               budget "FILE" "LINES" "SIZE" "BUDGET" "GIT" "RUNTIME"
    fileLines = map formatRow files
    formatRow f = T.pack $ printf "  %-32s %5d l  %6.1f KB  %-15s %-10s %-9s"
      (vfPath f)
      (vfLines f)
      (fromIntegral (vfBytes f) / 1024.0 :: Double)
      (if vfOver f
         then printf "[OVER: %d/%d]" (vfLines f) (vfBudget f) :: String
         else printf "[OK: %d/%d]" (vfLines f) (vfBudget f))
      (vfGitStatus f)
      (vfSyncState f)
    totalLines = sum (map vfLines files)
    overCount = length (filter vfOver files)
    footer = T.pack $ printf "\nTotal: %d file(s), %d lines | Over budget: %d file(s)"
               (length files) totalLines overCount

-- | Convert VFS inspection results to JSON.
vfsJson :: [VfsFile] -> Json
vfsJson files = JObj
  [ ("count", JNum (fromIntegral (length files)))
  , ("total_lines", JNum (fromIntegral (sum (map vfLines files))))
  , ("over_budget", JNum (fromIntegral (length (filter vfOver files))))
  , ("files", JArr (map rowJson files))
  ]
  where
    rowJson f = JObj
      [ ("path", JStr (vfPath f))
      , ("lines", JNum (fromIntegral (vfLines f)))
      , ("bytes", JNum (fromIntegral (vfBytes f)))
      , ("budget", JNum (fromIntegral (vfBudget f)))
      , ("over", JBool (vfOver f))
      , ("git_status", JStr (vfGitStatus f))
      , ("sync_state", JStr (vfSyncState f))
      , ("loaded", JBool (vfIsLoaded f))
      ]
