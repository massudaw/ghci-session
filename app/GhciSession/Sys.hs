{-# LANGUAGE ForeignFunctionInterface #-}

-- | The C half ('cbits/ghs_sys.c') and the small OS helpers around it.
module GhciSession.Sys
  ( unixListen, unixAccept, unixConnect, socketShutdown, socketPair
  , Regex, compileRegex, regexMatch, anyLineMatches, linesMatching
  , writeAtomicT, readFileText
  , hashFile, hashString, showHash
  , watchKind, watchNew, watchBudget, watchAdd, watchRm, watchWait
  , httpPost
  , processTable, processArgs, processName
  , now, writeAtomic, readFileMaybe, readFileUtf8, writeFileUtf8, appendFileUtf8, modTime, pidAlive, rawSystemOut, sockPath
  , termSize
  ) where

import Control.Exception (IOException, SomeException, evaluate, try)
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, getModificationTime, makeAbsolute, renameFile)
import System.IO (hGetContents, hSetEncoding, utf8)
import System.Posix.Types (Fd (..))
import System.Posix.User (getRealUserID)
import System.Process (CreateProcess (..), StdStream (..), proc, terminateProcess, waitForProcess, withCreateProcess)
import System.Timeout (timeout)

foreign import ccall safe "ghs_unix_listen" c_listen :: CString -> CInt -> IO CInt
foreign import ccall safe "ghs_unix_accept" c_accept :: CInt -> CInt -> IO CInt
foreign import ccall safe "ghs_unix_connect" c_connect :: CString -> IO CInt
foreign import ccall unsafe "ghs_shutdown" c_shutdown :: CInt -> IO CInt
foreign import ccall unsafe "ghs_socketpair" c_socketpair :: Ptr CInt -> IO CInt
foreign import ccall unsafe "ghs_regex_compile" c_recomp :: CString -> IO (Ptr ())
foreign import ccall unsafe "ghs_regex_match" c_rematch :: Ptr () -> CString -> IO CInt
foreign import ccall safe "ghs_hash_file" c_hash_file :: CString -> Word64 -> IO Word64
foreign import ccall unsafe "ghs_hash_bytes" c_hash_bytes :: CString -> CSize -> Word64 -> IO Word64
foreign import ccall unsafe "ghs_watch_kind" c_watch_kind :: IO CInt
foreign import ccall unsafe "ghs_watch_new" c_watch_new :: IO CInt
foreign import ccall unsafe "ghs_watch_budget" c_watch_budget :: IO CInt
foreign import ccall unsafe "ghs_watch_add" c_watch_add :: CInt -> CString -> IO CInt
foreign import ccall unsafe "ghs_watch_rm" c_watch_rm :: CInt -> CInt -> IO CInt
foreign import ccall safe "ghs_watch_wait" c_watch_wait :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "ghs_pid_state" c_pid_state :: CInt -> IO CInt
foreign import ccall unsafe "ghs_proc_table" c_proc_table :: Ptr CInt -> Ptr CInt -> Ptr Int64 -> Ptr Int64 -> CInt -> IO CInt
foreign import ccall unsafe "ghs_proc_args" c_proc_args :: CInt -> CString -> CInt -> IO CInt
foreign import ccall unsafe "ghs_proc_name" c_proc_name :: CInt -> CString -> CInt -> IO CInt
foreign import ccall safe "ghs_http_post" c_http_post :: CString -> CString -> CString -> CString -> CInt -> IO CInt

fdOrNothing :: CInt -> Maybe Fd
fdOrNothing n = if n < 0 then Nothing else Just (Fd n)

unixListen :: FilePath -> IO (Maybe Fd)
unixListen p = fdOrNothing <$> withCString p (\c -> c_listen c 8)

-- | A connection, or 'Nothing' when none came within the timeout (milliseconds).
unixAccept :: Fd -> Int -> IO (Maybe Fd)
unixAccept (Fd fd) ms = fdOrNothing <$> c_accept fd (fromIntegral ms)

unixConnect :: FilePath -> IO (Maybe Fd)
unixConnect p = fdOrNothing <$> withCString p c_connect

-- | Hang up: a thread blocked reading the socket sees end of file, and so does the other end.
socketShutdown :: Fd -> IO ()
socketShutdown (Fd fd) = void (c_shutdown fd)

-- | A connected pair of sockets: ours (closed on exec), and the one a child is given.
socketPair :: IO (Maybe (Fd, Fd))
socketPair = allocaArray 2 $ \p -> do
  r <- c_socketpair p
  if r < 0 then pure Nothing else (\fds -> case fds of { [a, b] -> Just (Fd a, Fd b); _ -> Nothing }) <$> peekArray 2 p

-- regular expressions ----------------------------------------------------------

newtype Regex = Regex (Ptr ())

-- | POSIX extended (with macOS's enhanced syntax). 'Nothing' if the pattern does not compile. Never freed:
-- a session compiles a handful, once.
compileRegex :: String -> IO (Maybe Regex)
compileRegex pat = do
  p <- withCString pat c_recomp
  pure (if p == nullPtr then Nothing else Just (Regex p))

-- | Text goes to C as its UTF-8 bytes, once: no per-character marshalling.
regexMatch :: Regex -> T.Text -> IO Bool
regexMatch (Regex p) s = (/= 0) <$> B.useAsCString (TE.encodeUtf8 s) (c_rematch p)

-- | Does the pattern match anywhere in the text (@^@ and @$@ at line boundaries)? A pattern that does not
-- compile matches nothing.
anyLineMatches :: String -> T.Text -> IO Bool
anyLineMatches pat text = compileRegex pat >>= maybe (pure False) (`regexMatch` text)

linesMatching :: String -> [T.Text] -> IO [T.Text]
linesMatching pat ls = compileRegex pat >>= maybe (pure []) (\re -> filterIO (regexMatch re) ls)
  where filterIO f = foldr (\x r -> do { b <- f x; xs <- r; pure (if b then x : xs else xs) }) (pure [])

-- hashing ------------------------------------------------------------------------

-- | A content hash chained from a previous one (0 to start); 0 when the file cannot be read.
hashFile :: FilePath -> Word64 -> IO Word64
hashFile p h = withCString p (\c -> c_hash_file c h)

hashString :: String -> Word64 -> IO Word64
hashString s h = B.useAsCStringLen (TE.encodeUtf8 (T.pack s)) (\(p, n) -> c_hash_bytes p (fromIntegral n) h)

showHash :: Word64 -> String
showHash h = let x = showHex h "" in replicate (16 - length x) '0' ++ x

-- file events --------------------------------------------------------------------

-- | 0: none; 1: a descriptor per file and directory (kqueue); 2: a watch per directory (inotify).
watchKind :: IO Int
watchKind = fromIntegral <$> c_watch_kind

watchNew :: IO (Maybe CInt)
watchNew = (\n -> if n < 0 then Nothing else Just n) <$> c_watch_new

watchBudget :: IO Int
watchBudget = fromIntegral <$> c_watch_budget

watchAdd :: CInt -> B.ByteString -> IO (Maybe CInt)
watchAdd k p = (\n -> if n < 0 then Nothing else Just n) <$> B.useAsCString p (c_watch_add k)

watchRm :: CInt -> CInt -> IO ()
watchRm k w = void (c_watch_rm k w)

watchWait :: CInt -> Int -> IO Bool
watchWait k ms = (/= 0) <$> c_watch_wait k (fromIntegral ms)

-- | POST a JSON body to an @http://host[:port]/path@ URL. Best effort, never throws.
httpPost :: String -> String -> Int -> IO ()
httpPost url body ms = case parse url of
  Nothing -> pure ()
  Just (host, port, path) ->
    void (try (withCString host $ \h -> withCString port $ \p -> withCString path $ \pa -> withCString body $ \b ->
                 c_http_post h p pa b (fromIntegral ms)) :: IO (Either SomeException CInt))
  where
    parse u = case splitAt 7 u of
      ("http://", rest) ->
        let (hp, path) = break (== '/') rest
            (host, port) = break (== ':') hp
        in Just (host, if null port then "80" else drop 1 port, if null path then "/" else path)
      _ -> Nothing

-- small things --------------------------------------------------------------------

now :: IO Double
now = realToFrac <$> getPOSIXTime

-- | Text files are UTF-8 whatever the locale says (a daemon started from a bare environment has none).
readFileUtf8 :: FilePath -> IO String
readFileUtf8 p = T.unpack . TE.decodeUtf8With TE.lenientDecode <$> B.readFile p

writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 p = B.writeFile p . TE.encodeUtf8 . T.pack

appendFileUtf8 :: FilePath -> String -> IO ()
appendFileUtf8 p = B.appendFile p . TE.encodeUtf8 . T.pack

writeAtomic :: FilePath -> String -> IO ()
writeAtomic path text = do
  writeFileUtf8 (path ++ ".tmp") text
  renameFile (path ++ ".tmp") path

writeAtomicT :: FilePath -> T.Text -> IO ()
writeAtomicT path text = do
  B.writeFile (path ++ ".tmp") (TE.encodeUtf8 text)
  renameFile (path ++ ".tmp") path

readFileText :: FilePath -> IO (Maybe T.Text)
readFileText p = either (\e -> const Nothing (e :: IOException)) (Just . TE.decodeUtf8With TE.lenientDecode) <$> try (B.readFile p)

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe p = either (\e -> const Nothing (e :: IOException)) Just <$> try (readFileUtf8 p >>= \s -> length s `seq` pure s)

modTime :: FilePath -> IO (Maybe Double)
modTime p = either (\e -> const Nothing (e :: IOException)) (Just . realToFrac . utcTimeToPOSIXSeconds) <$> try (getModificationTime p)

-- | Running, and not a zombie (a forked child that died is a zombie until its parent waits on it). Asked of
-- the kernel directly: spawning @ps@ for it was 20 ms, several times a reload and once per session in every
-- client command.
pidAlive :: Int -> IO Bool
pidAlive pid = (== 1) <$> c_pid_state (fromIntegral pid)

-- | A process's command line (its arguments joined by spaces), "" if it cannot be read.
processArgs :: Int -> IO String
processArgs pid = allocaBytes 16384 $ \buf -> do
  n <- c_proc_args (fromIntegral pid) buf 16384
  if n <= 0 then pure "" else T.unpack . TE.decodeUtf8With TE.lenientDecode <$> B.packCStringLen (buf, fromIntegral n)

-- | A process's executable name, "" if it cannot be had. A few microseconds, where its arguments are tens.
processName :: Int -> IO String
processName pid = allocaBytes 256 $ \buf -> do
  n <- c_proc_name (fromIntegral pid) buf 256
  if n <= 0 then pure "" else T.unpack . TE.decodeUtf8With TE.lenientDecode <$> B.packCStringLen (buf, fromIntegral n)

-- | Every process: (pid, parent, memory in KB). The memory is the physical footprint where the OS gives one
-- (macOS: what memory pressure is about -- RSS collapses when pages are compressed or swapped), else RSS.
processTable :: IO [(Int, Int, Int)]
processTable = do
  let cap = 8192
  allocaArray cap $ \pids -> allocaArray cap $ \ppids -> allocaArray cap $ \rss -> allocaArray cap $ \foot -> do
    n <- fromIntegral <$> c_proc_table pids ppids rss foot (fromIntegral cap)
    ps <- peekArray n pids
    pps <- peekArray n ppids
    rs <- peekArray n rss
    fs <- peekArray n foot
    pure [ (fromIntegral p, fromIntegral pp, fromIntegral (if f > 0 then f else r)) | (p, pp, r, f) <- zip4' ps pps rs fs ]
  where zip4' (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4' as bs cs ds
        zip4' _ _ _ _ = []

-- | A command's stdout, or 'Nothing' if it could not be run or did not finish within the seconds given
-- (it is then killed: a helper that hangs must not hang the session).
rawSystemOut :: Double -> FilePath -> [String] -> IO (Maybe String)
rawSystemOut secs cmd args = do
  r <- try (withCreateProcess (proc cmd args) { std_in = NoStream, std_out = CreatePipe, std_err = NoStream } $ \_ mout _ ph ->
              case mout of
                Nothing -> pure Nothing
                Just h -> do
                  hSetEncoding h utf8
                  got <- timeout (round (secs * 1e6)) (hGetContents h >>= \s -> evaluate (length s) >> pure s)
                  case got of
                    Nothing -> terminateProcess ph >> pure Nothing
                    Just s -> waitForProcess ph >> pure (Just s)) :: IO (Either SomeException (Maybe String))
  pure (either (const Nothing) id r)

-- | A short, collision-free unix socket path (macOS caps sun_path near 104 bytes, which a nested worktree
-- easily exceeds), unique per absolute state dir. The same rule as the daemon's clients in other languages
-- must use: @/tmp/ghci-session-<uid>/<16 hex of FNV-1a 64 of the absolute path>.sock@.
sockPath :: FilePath -> IO FilePath
sockPath stateDir = do
  uid <- getRealUserID
  let base = "/tmp/ghci-session-" ++ show uid
  createDirectoryIfMissing True base
  d <- makeAbsolute stateDir
  h <- hashString d 0
  pure (base ++ "/" ++ showHash h ++ ".sock")

foreign import ccall unsafe "ghs_term_size" c_term_size :: Ptr CInt -> Ptr CInt -> IO CInt

-- | The terminal's rows and columns (standard output), if it is one.
termSize :: IO (Maybe (Int, Int))
termSize = alloca $ \pr -> alloca $ \pc -> do
  rc <- c_term_size pr pc
  if rc /= 0 then pure Nothing else do
    r <- peek pr
    c <- peek pc
    pure (Just (fromIntegral r, fromIntegral c))
