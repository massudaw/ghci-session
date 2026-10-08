{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High-performance code and file search backed by the FFF SIMD library ('cbits/ghs_fff.c'),
-- with session-aware metadata logging and formatted terminal output.
module GhciSession.Search
  ( isAvailable
  , grep
  , searchFiles
  , formatGrep
  , formatFiles
  , recordSearchMetadata
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import System.FilePath ((</>))
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

import GhciSession.Json
import GhciSession.Config
import qualified GhciSession.History as H

foreign import ccall unsafe "ghs_fff_available" c_fff_available :: IO CInt
foreign import ccall safe "ghs_fff_grep" c_fff_grep :: CString -> CString -> CInt -> CString -> CSize -> IO CInt
foreign import ccall safe "ghs_fff_search_files" c_fff_search_files :: CString -> CString -> CInt -> CString -> CSize -> IO CInt

-- | Is the FFF native C library loaded and operational?
isAvailable :: IO Bool
isAvailable = (== 1) <$> c_fff_available

-- | Search file contents (live grep) in the given directory.
grep :: FilePath -> String -> Int -> IO (Either String Json)
grep dir query maxResults = do
  avail <- isAvailable
  if not avail
    then fallbackGrep dir query maxResults
    else withCString dir $ \cDir ->
           withCString query $ \cQ ->
             let bufSize = 512 * 1024  -- 512 KB buffer
             in allocaBytes bufSize $ \cBuf -> do
                  rc <- c_fff_grep cDir cQ (fromIntegral maxResults) cBuf (fromIntegral bufSize)
                  str <- peekCString cBuf
                  -- (0 is a failure, and the buffer then says why -- unless it is empty)
                  pure (if rc == 0 && null str then Left "fff: no answer" else parseJson str)

-- | Fuzzy search file names in the given directory.
searchFiles :: FilePath -> String -> Int -> IO (Either String Json)
searchFiles dir query maxResults = do
  avail <- isAvailable
  if not avail
    then fallbackSearchFiles dir query maxResults
    else withCString dir $ \cDir ->
           withCString query $ \cQ ->
             let bufSize = 256 * 1024  -- 256 KB buffer
             in allocaBytes bufSize $ \cBuf -> do
                  rc <- c_fff_search_files cDir cQ (fromIntegral maxResults) cBuf (fromIntegral bufSize)
                  str <- peekCString cBuf
                  -- (0 is a failure, and the buffer then says why -- unless it is empty)
                  pure (if rc == 0 && null str then Left "fff: no answer" else parseJson str)

-- | Format grep results for human terminal consumption or agent replies.
formatGrep :: Json -> T.Text
formatGrep j = case lookupArr "items" j of
  [] -> T.pack "No matches found."
  items ->
    let total = fromMaybe (length items) (lookupNum "total_matched" j >>= Just . round) :: Int
        searched = fromMaybe 0 (lookupNum "files_searched" j >>= Just . round) :: Int
        q = fromMaybe "" (lookupStr "query" j)
        hdr = printf "Found %d match(es) for \"%s\" (searched %d files):\n" total q searched :: String
        renderItem item =
          let p = fromMaybe "" (lookupStr "path" item)
              ln = fromMaybe 0 (lookupNum "line" item >>= Just . round) :: Int
              c = fromMaybe "" (lookupStr "content" item)
              st = fromMaybe "" (lookupStr "git_status" item)
              badge = if null st || st == "clean" then "" else " [" ++ st ++ "]"
          in printf "  %s:%d:%s %s" p ln badge (trimLeading c) :: String
        linesOut = map renderItem items
    in T.pack (unlines (hdr : linesOut))
  where trimLeading s = dropWhile (== ' ') s

-- | Format file search results.
formatFiles :: Json -> T.Text
formatFiles j = case lookupArr "items" j of
  [] -> T.pack "No files found."
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
    in T.pack (unlines (hdr : linesOut))

-- | Record search metadata into the session's history log.
recordSearchMetadata :: Conf -> String -> String -> String -> Int -> IO ()
recordSearchMetadata conf sname mode query hitCount = do
  let sdir = cStateDir conf </> sname
      hdir = sdir </> "history"
  exists <- doesDirectoryExist hdir
  if not exists then pure () else do
    r <- try (do
      (hm, _) <- H.openHistory H.defaultParams hdir
      let msg = T.pack (printf "search [%s]: \"%s\" -> %d match(es)" mode query hitCount)
      _ <- H.appendMsg hm (T.pack "note") msg
      pure ()) :: IO (Either SomeException ())
    case r of
      Left _ -> pure ()
      Right () -> pure ()

-- | Fallback grep if FFF dylib is not loaded on this system.
fallbackGrep :: FilePath -> String -> Int -> IO (Either String Json)
fallbackGrep dir query maxResults = do
  let qT = T.pack query
  files <- findFiles dir
  matches <- fmap concat $ forM' (take 100 files) $ \f -> do
    t <- try (B.readFile (dir </> f)) :: IO (Either SomeException B.ByteString)
    case t of
      Left _ -> pure []
      -- a binary file is not searched as text (a NUL in its first 8000 bytes, as git and grep tell): decoding
      -- one leniently is a byte-at-a-time repair, and an 11 MB library in the tree took over a minute
      Right bs | B.elem 0 (B.take 8000 bs) -> pure []
      Right bs ->
        let txt = TE.decodeUtf8With (\_ _ -> Just ' ') bs
            ls = zip [1 :: Int ..] (T.lines txt)
            hits = [ JObj [ ("path", JStr f)
                          , ("line", JNum (fromIntegral lnum))
                          , ("content", JText l)
                          , ("git_status", JStr "clean")
                          , ("frecency", JNum 0) ]
                   | (lnum, l) <- ls, qT `T.isInfixOf` l ]
        in pure (take maxResults hits)
  let res = take maxResults matches
  pure $ Right $ JObj
    [ ("ok", JBool True)
    , ("query", JStr query)
    , ("mode", JStr "grep-fallback")
    , ("count", JNum (fromIntegral (length res)))
    , ("total_matched", JNum (fromIntegral (length res)))
    , ("files_searched", JNum (fromIntegral (length files)))
    , ("items", JArr res) ]
  where forM' = flip mapM

-- | Fallback file search.
fallbackSearchFiles :: FilePath -> String -> Int -> IO (Either String Json)
fallbackSearchFiles dir query maxResults = do
  files <- findFiles dir
  let qLower = map toLower query
      matched = [ JObj [ ("path", JStr f), ("git_status", JStr "clean"), ("frecency", JNum 0) ]
                | f <- files, qLower `isInfixOf'` map toLower f ]
      res = take maxResults matched
  pure $ Right $ JObj
    [ ("ok", JBool True)
    , ("query", JStr query)
    , ("mode", JStr "files-fallback")
    , ("count", JNum (fromIntegral (length res)))
    , ("total_matched", JNum (fromIntegral (length matched)))
    , ("items", JArr res) ]
  where
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    isInfixOf' needle haystack = needle `elem` [ take (length needle) (drop i haystack) | i <- [0 .. length haystack - length needle] ]

findFiles :: FilePath -> IO [FilePath]
findFiles root = go ""
  where
    go rel = do
      let full = if null rel then root else root </> rel
      es <- try (listDirectory full) :: IO (Either SomeException [FilePath])
      case es of
        Left _ -> pure []
        Right names -> do
          let valid = filter (\n -> not (null n) && head n /= '.' && n /= "dist-newstyle") names
          fmap concat $ forM valid $ \name -> do
            let subRel = if null rel then name else rel </> name
                subFull = root </> subRel
            isD <- doesDirectoryExist subFull
            if isD then go subRel else pure [subRel]
    forM = flip mapM
