{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Code and file search for the agent and the command line, answered as JSON, with session-aware metadata logging
-- and formatted output. Two engines. The default is the system @grep@ for contents and a walk of the project for
-- file names: on every machine this runs on, nothing to download. FFF (a native file-finder library,
-- 'cbits/ghs_fff.c', loaded at run time) is opt-in: @GHS_SEARCH=fff@ in the environment, or @search --fff@, and
-- @FETCH_FFF=1 ./build.sh@ fetches the library. If it was asked for and cannot be loaded, the answer is from the
-- default engine and says so.
module GhciSession.Search
  ( grep
  , searchFiles
  , fffRequested
  , isAvailable
  , formatGrep
  , formatFiles
  , recordSearchMetadata
  -- (the pure parts, for the self-tests)
  , parseGrepLine, grepFlags, fuzzyScore
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import System.Environment (lookupEnv)
import Data.Char (isUpper, toLower)
import Data.List (isInfixOf, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeFileName)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)
import Text.Printf (printf)

import GhciSession.Json
import GhciSession.Config
import qualified GhciSession.Mcp as Mcp

-- the engine ---------------------------------------------------------------------------------

foreign import ccall unsafe "ghs_fff_available" c_fff_available :: IO CInt
foreign import ccall safe "ghs_fff_grep" c_fff_grep :: CString -> CString -> CInt -> CString -> CSize -> IO CInt
foreign import ccall safe "ghs_fff_search_files" c_fff_search_files :: CString -> CString -> CInt -> CString -> CSize -> IO CInt

-- | Was FFF asked for (@GHS_SEARCH=fff@)?
fffRequested :: IO Bool
fffRequested = (== Just "fff") <$> lookupEnv "GHS_SEARCH"

-- | Is the FFF native library loaded and operational?
isAvailable :: IO Bool
isAvailable = (== 1) <$> c_fff_available

-- | Run a search with FFF when it was asked for and loads; else with the default engine, the answer saying why.
withEngine :: (FilePath -> String -> Int -> IO (Either String Json))      -- ^ the default engine
           -> (CString -> CString -> CInt -> CString -> CSize -> IO CInt)  -- ^ FFF's
           -> Int -> FilePath -> String -> Int -> IO (Either String Json)
withEngine plain fff bufSize dir query maxResults = do
  wanted <- fffRequested
  avail <- if wanted then isAvailable else pure False
  if avail
    then withCString dir $ \cDir -> withCString query $ \cQ -> allocaBytes bufSize $ \cBuf -> do
           _ <- fff cDir cQ (fromIntegral maxResults) cBuf (fromIntegral bufSize)
           str <- peekCString cBuf
           pure (fmap (set "engine" (JStr "fff")) (parseJson str))
    else do
      r <- plain dir query maxResults
      pure (if wanted then fmap (set "note" (JStr "FFF was asked for (GHS_SEARCH=fff) but its library could not be loaded: this is grep's answer")) r else r)

-- | Search file contents (live grep) in the given directory.
grep :: FilePath -> String -> Int -> IO (Either String Json)
grep = withEngine grepSystem c_fff_grep (512 * 1024)

-- | Search file names in the given directory.
searchFiles :: FilePath -> String -> Int -> IO (Either String Json)
searchFiles = withEngine filesWalk c_fff_search_files (256 * 1024)

-- | Directories no search looks in: version control, this tool's own state, build output (@dist*@).
skippedDir :: String -> Bool
skippedDir n = n `elem` [".git", ".hg", ".svn", ".ghci-session", ".bin", "node_modules"] || "dist" `isPrefixOf` n

-- | The flags of a @grep@ over a tree: recursive with line numbers, text files only, the skipped directories
-- left out, at most @n@ lines from any one file; case-insensitive unless the query has a capital (smart case).
-- A regular expression ('-E'), or with @literal@ the text as it is ('-F'), for a query that is not one.
grepFlags :: Bool -> Int -> String -> [String]
grepFlags literal n query =
  ["-r", "-n", "-I", "-H", if literal then "-F" else "-E", "-m", show n]
  ++ [ "-i" | not (any isUpper query) ]
  ++ [ "--exclude-dir=" ++ d | d <- [".git", ".hg", ".svn", ".ghci-session", ".bin", "node_modules", "dist*"] ]
  ++ ["-e", query, "."]

-- | A line of @grep -rn@ output, @path:line:text@ (the path as grep walked it, from the project's root).
parseGrepLine :: String -> Maybe (FilePath, Int, String)
parseGrepLine l = case break (== ':') l of
  (path, ':' : rest) | not (null path), (num@(_ : _), ':' : text) <- span (`elem` ['0' .. '9']) rest ->
    Just (dropDot path, read num, text)
  _ -> Nothing
  where dropDot p = if "./" `isPrefixOf` p then drop 2 p else p

-- | Search file contents in the given directory: the lines matching a regular expression (the literal text when
-- it is not one), by path and line.
grepSystem :: FilePath -> String -> Int -> IO (Either String Json)
grepSystem dir query maxResults
  | null query = pure (Left "empty query")
  | otherwise = do
      let n = if maxResults <= 0 then 30 else maxResults
          run literal = timeout 30000000 (readCreateProcessWithExitCode (proc "grep" (grepFlags literal n query)) { cwd = Just dir } "")
      r <- try (run False) :: IO (Either SomeException (Maybe (ExitCode, String, String)))
      r' <- case r of
        Right (Just (ExitFailure 2, _, _)) -> try (run True)      -- (not a regular expression: the text itself)
        _ -> pure r
      case r' of
        Left e -> pure (Left ("grep: " ++ show e))
        Right Nothing -> pure (Left "grep: no answer in 30 s")
        Right (Just (code, out, err))
          | code == ExitFailure 2 && null out -> pure (Left ("grep: " ++ takeWhile (/= '\n') err))
          | otherwise -> do
              let found = sortOn (\(p, l, _) -> (p, l)) (mapMaybe parseGrepLine (lines out))
                  shown = take n found
                  cut c = if length c > 300 then take 300 c ++ "..." else c
              pure $ Right $ JObj
                [ ("ok", JBool True)
                , ("query", JStr query)
                , ("mode", JStr "grep")
                , ("count", JNum (fromIntegral (length shown)))
                , ("total_matched", JNum (fromIntegral (length found)))
                , ("items", JArr [ JObj [("path", JStr p), ("line", JNum (fromIntegral l)), ("content", JStr (cut c))] | (p, l, c) <- shown ]) ]

-- | Search file names in the given directory: those that contain the query (case does not matter), the ones whose
-- file name does first, then those that have its letters in order.
filesWalk :: FilePath -> String -> Int -> IO (Either String Json)
filesWalk dir query maxResults
  | null query = pure (Left "empty query")
  | otherwise = do
      let n = if maxResults <= 0 then 20 else maxResults
      files <- walk dir ""
      let scored = sortOn (\(sc, f) -> (sc, length f, f)) [ (sc, f) | f <- files, Just sc <- [fuzzyScore query f] ]
      pure $ Right $ JObj
        [ ("ok", JBool True)
        , ("query", JStr query)
        , ("mode", JStr "files")
        , ("count", JNum (fromIntegral (min n (length scored))))
        , ("total_matched", JNum (fromIntegral (length scored)))
        , ("items", JArr [ JObj [("path", JStr f)] | (_, f) <- take n scored ]) ]

-- | How well a query names a path: 0 when the file's own name contains it, 1 when the path does, 2 when the
-- path has its letters in order; nothing otherwise.
fuzzyScore :: String -> FilePath -> Maybe Int
fuzzyScore query path
  | q `isInfixOf` map toLower (takeFileName path) = Just 0
  | q `isInfixOf` p = Just 1
  | inOrder q p = Just 2
  | otherwise = Nothing
  where
    q = map toLower query
    p = map toLower path
    inOrder [] _ = True
    inOrder _ [] = False
    inOrder (x : xs) (y : ys) = if x == y then inOrder xs ys else inOrder (x : xs) ys

-- | Every file under a directory, as paths from it, leaving out hidden and skipped directories.
walk :: FilePath -> FilePath -> IO [FilePath]
walk root rel = do
  es <- try (listDirectory (if null rel then root else root </> rel)) :: IO (Either SomeException [FilePath])
  case es of
    Left _ -> pure []
    Right names -> fmap concat $ mapM (entry) [ n | n <- names, not (null n), head n /= '.' ]
  where
    entry n = do
      let r = if null rel then n else rel </> n
      isD <- doesDirectoryExist (root </> r)
      if isD then (if skippedDir n then pure [] else walk root r) else pure [r]

-- | Format grep results for human terminal consumption or agent replies.
formatGrep :: Json -> T.Text
formatGrep j = case lookupArr "items" j of
  [] -> T.pack ("No matches found." ++ concatMap ("\n" ++) (noteOf j))
  items ->
    let total = fromMaybe (length items) (lookupNum "total_matched" j >>= Just . round) :: Int
        q = fromMaybe "" (lookupStr "query" j)
        hdr = (if total > length items then printf "Found %d match(es) for \"%s\" (showing %d):\n" total q (length items)
                                       else printf "Found %d match(es) for \"%s\":\n" total q) :: String
        renderItem item =
          let p = fromMaybe "" (lookupStr "path" item)
              ln = fromMaybe 0 (lookupNum "line" item >>= Just . round) :: Int
              c = fromMaybe "" (lookupStr "content" item)
              st = fromMaybe "" (lookupStr "git_status" item)
              badge = if null st || st == "clean" then "" else " [" ++ st ++ "]"
          in printf "  %s:%d:%s %s" p ln badge (trimLeading c) :: String
        linesOut = map renderItem items
    in T.pack (unlines (hdr : linesOut ++ noteOf j))
  where trimLeading = dropWhile (== ' ')

-- | Format file search results.
formatFiles :: Json -> T.Text
formatFiles j = case lookupArr "items" j of
  [] -> T.pack ("No files found." ++ concatMap ("\n" ++) (noteOf j))
  items ->
    let total = fromMaybe (length items) (lookupNum "total_matched" j >>= Just . round) :: Int
        q = fromMaybe "" (lookupStr "query" j)
        hdr = printf "Found %d file(s) matching \"%s\":\n" total q
        renderItem item =
          let p = fromMaybe "" (lookupStr "path" item)
              st = fromMaybe "" (lookupStr "git_status" item)
              frec = fromMaybe 0 (lookupNum "frecency" item >>= Just . round) :: Int
              badge = if null st || st == "clean" then "" else " [" ++ st ++ "]"
              frecBadge = if frec > 0 then printf " (frecency: %d)" frec else "" :: String
          in printf "  %s%s%s" p badge frecBadge :: String
        linesOut = map renderItem items
    in T.pack (unlines (hdr : linesOut ++ noteOf j))

-- | What the answer says about how it was made, when it matters: FFF asked for and not there.
noteOf :: Json -> [String]
noteOf j = [ "[" ++ n ++ "]" | Just n <- [lookupStr "note" j] ]

-- | Note a search in the session's history (through the daemon, which owns the log: opening it here cost 0.9 s
-- on a 5 MB history). A session that is not running has no history to add to, and nothing is lost by that.
recordSearchMetadata :: Conf -> String -> String -> String -> Int -> IO ()
recordSearchMetadata conf sname mode query hitCount = do
  let msg = T.pack (printf "search [%s]: \"%s\" -> %d match(es)" mode query hitCount)
  void (try (Mcp.request conf sname (JObj [("op", JStr "log"), ("kind", JStr "note"), ("text", JText msg)])) :: IO (Either SomeException Json))
