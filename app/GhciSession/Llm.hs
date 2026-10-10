{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | __The model, as a chat endpoint__: one request, one reply, over HTTPS through libcurl
-- ('cbits/ghs_http.c', loaded at run time). What the chat and the compactor ('GhciSession.Chat') share: the
-- endpoint from the environment, the request's shape, the reply's parts. Two protocols, one conversation: an
-- OpenAI-compatible endpoint (DeepSeek's by default) is spoken to here, Anthropic's Messages API through
-- "GhciSession.Anthropic", which turns the same request into its own.
--
-- The environment. An OpenAI-compatible endpoint: @DEEPSEEK_API_KEY@ (or @OPENAI_API_KEY@); @DEEPSEEK_MODEL@ /
-- @DEEPSEEK_BASE_URL@, or @OPENAI_MODEL@ / @OPENAI_BASE_URL@, override the flash model at
-- @https://api.deepseek.com@. Anthropic's: @ANTHROPIC_API_KEY@ (or @ANTHROPIC_AUTH_TOKEN@), @ANTHROPIC_MODEL@,
-- @ANTHROPIC_BASE_URL@. With keys for both, the first is used -- as it was before there were two -- unless
-- @GHS_PROVIDER@ says @anthropic@ (or @openai@).
module GhciSession.Llm
  ( Endpoint (..), Provider (..), endpointFromEnv, isDeepSeek, appendOnly
  , Request (..), request, requestWith, Reply (..), Usage (..), ToolCall (..)
  , httpsPost, cancelRequests
  , Chunks (..), emptyChunks, chunkEvent, chunksMessage
  , usageFileEnv, recordUsage, recordUsageWith, human
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified System.Posix.IO as PIO
import System.Posix.Types (Fd (..))
import Control.Exception (SomeException, try)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CChar (..), CInt (..), CLong (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.IO (IOMode (..), hClose, hPutStrLn, hSetBinaryMode, openFile)
import Text.Printf (printf)

import qualified GhciSession.Anthropic as A
import qualified GhciSession.ClaudeCli as C
import GhciSession.Json

foreign import ccall safe "ghs_https_post" c_post
  :: CString -> CString -> Ptr CChar -> CSize -> CLong -> CString -> Ptr CString -> Ptr CSize -> Ptr CLong -> CString -> CSize -> IO CInt
foreign import ccall safe "ghs_https_request" c_request
  :: CString -> CString -> Ptr CChar -> CSize -> CLong -> CLong -> CString -> CInt -> Ptr CString -> Ptr CSize -> Ptr CLong -> Ptr CLong -> CString -> CSize -> IO CInt
foreign import ccall unsafe "ghs_https_free" c_free :: CString -> IO ()
foreign import ccall unsafe "ghs_https_cancel" c_cancel :: IO ()

-- | Give up the exchanges under way in this process: each ends itself within a second, as a failure.
cancelRequests :: IO ()
cancelRequests = c_cancel

-- | 'httpsPost', and how long the server asked to be left alone (its @Retry-After@, in seconds), if it did.
httpsRequest :: String -> [String] -> B.ByteString -> Double -> IO (Either String (Int, Maybe Int, B.ByteString))
httpsRequest url headers body timeout = do
  ca <- fromMaybe "" . firstJust <$> mapM lookupEnv ["CURL_CA_BUNDLE", "SSL_CERT_FILE"]
  r <- try $ withCString url $ \u -> withCString (unlines headers) $ \h -> withCString ca $ \c ->
    BU.unsafeUseAsCStringLen body $ \(b, n) ->
      with nullPtr $ \pout -> with 0 $ \plen -> with 0 $ \pstatus -> with 0 $ \pafter -> allocaBytes 512 $ \err -> do
        rc <- c_request u h b (fromIntegral n) (round timeout) 0 c (-1) pout plen pstatus pafter err 512
        if rc /= 0 then Left <$> peekCString err else do
          out <- peek pout
          len <- peek plen
          status <- peek pstatus
          after <- peek pafter
          bs <- B.packCStringLen (out, fromIntegral len)
          c_free out
          pure (Right (fromIntegral status, if after > 0 then Just (fromIntegral after) else Nothing, bs))
  pure (either (\(e :: SomeException) -> Left (show e)) id r)
  where firstJust xs = case [ x | Just x <- xs, not (null x) ] of { (x : _) -> Just x; [] -> Nothing }

-- | POST a body with headers (@"Name: value"@ each), within the timeout: the status and the body, or why not.
-- The CA bundle is the one the environment names for curl, when it names one.
httpsPost :: String -> [String] -> B.ByteString -> Double -> IO (Either String (Int, B.ByteString))
httpsPost url headers body timeout = do
  ca <- fromMaybe "" . firstJust <$> mapM lookupEnv ["CURL_CA_BUNDLE", "SSL_CERT_FILE"]
  r <- try $ withCString url $ \u -> withCString (unlines headers) $ \h -> withCString ca $ \c ->
    BU.unsafeUseAsCStringLen body $ \(b, n) ->
      with nullPtr $ \pout -> with 0 $ \plen -> with 0 $ \pstatus -> allocaBytes 512 $ \err -> do
        rc <- c_post u h b (fromIntegral n) (round timeout) c pout plen pstatus err 512
        if rc /= 0 then Left <$> peekCString err else do
          out <- peek pout
          len <- peek plen
          status <- peek pstatus
          bs <- B.packCStringLen (out, fromIntegral len)
          c_free out
          pure (Right (fromIntegral status, bs))
  pure (either (\(e :: SomeException) -> Left (show e)) id r)
  where firstJust xs = case [ x | Just x <- xs, not (null x) ] of { (x : _) -> Just x; [] -> Nothing }

-- | POST, the reply read as it comes: each line of it is handed over while the next is still being sent. The
-- call is held to how long the server may be SILENT (@idle@ seconds), not to how long it may take. The status,
-- the seconds the server asked to be left alone, and all that was received (an error is not a stream: it is
-- read whole, after).
httpsStream :: String -> [String] -> B.ByteString -> Double -> (B.ByteString -> IO ()) -> IO (Either String (Int, Maybe Int, B.ByteString))
httpsStream url headers body idle line = do
  ca <- fromMaybe "" . firstJust <$> mapM lookupEnv ["CURL_CA_BUNDLE", "SSL_CERT_FILE"]
  r <- try $ do
    (rd, wr) <- PIO.createPipe
    rh <- PIO.fdToHandle rd
    hSetBinaryMode rh True
    allV <- newIORef []
    done <- newEmptyMVar
    -- the reader: a line at a time, until the writer's end is closed
    _ <- forkIO $ do
      let loop = do
            l <- try (BC.hGetLine rh) :: IO (Either SomeException B.ByteString)
            case l of
              Right x -> modifyIORef' allV (x :) >> (try (line x) :: IO (Either SomeException ())) >> loop
              Left _ -> pure ()
      loop
      hClose rh
      putMVar done ()
    res <- withCString url $ \u -> withCString (unlines headers) $ \h -> withCString ca $ \c ->
      BU.unsafeUseAsCStringLen body $ \(b, n) ->
        with nullPtr $ \pout -> with 0 $ \plen -> with 0 $ \pstatus -> with 0 $ \pafter -> allocaBytes 512 $ \err -> do
          let Fd w = wr
          rc <- c_request u h b (fromIntegral n) 0 (round idle) c w pout plen pstatus pafter err 512
          if rc /= 0 then Left <$> peekCString err else do
            out <- peek pout
            c_free out
            status <- peek pstatus
            after <- peek pafter
            pure (Right (fromIntegral status :: Int, if after > 0 then Just (fromIntegral after :: Int) else Nothing))
    PIO.closeFd wr
    takeMVar done
    got <- B.intercalate (B.singleton 10) . reverse <$> readIORef allV
    pure (fmap (\(st, after) -> (st, after, got)) res)
  pure (either (\(e :: SomeException) -> Left (show e)) id r)
  where firstJust xs = case [ x | Just x <- xs, not (null x) ] of { (x : _) -> Just x; [] -> Nothing }

-- the ledger ----------------------------------------------------------------------------------

-- | The environment variable that names the usage ledger for a process the daemon runs (the compactor).
usageFileEnv :: String
usageFileEnv = "GHS_USAGE_FILE"

-- | One model call appended to the ledger (@<state>/<session>/usage.jsonl@): when, who asked (chat,
-- summarize), the model, the tokens in (and how many of them the provider had cached), the tokens out,
-- the seconds. Best effort: accounting never fails a call.
recordUsage :: FilePath -> String -> Endpoint -> Usage -> Double -> IO ()
recordUsage file who e u secs = recordUsageWith [] file who e u secs

-- | The same, with more to say of the call (the cache it wrote, the plan's windows as last seen): fields added to the line.
recordUsageWith :: [(String, Json)] -> FilePath -> String -> Endpoint -> Usage -> Double -> IO ()
recordUsageWith extra file who e u secs = do
  t <- realToFrac <$> getPOSIXTime :: IO Double
  stamp <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime
  let line = encode (JObj $ [ ("t", JNum t), ("date", JStr stamp), ("who", JStr who), ("model", JStr (eModel e))
                          , ("in", JNum (fromIntegral (uIn u))), ("cached", JNum (fromIntegral (fromMaybe 0 (uCached u))))
                          , ("out", JNum (fromIntegral (uOut u))), ("secs", JNum (fromIntegral (round (secs * 10) :: Int) / 10)) ] ++ extra)
  void (try (do { h <- openFile file AppendMode; hPutStrLn h line; hClose h }) :: IO (Either SomeException ()))
  where void = fmap (const ())

-- | A count of tokens as a person reads it: 950, 12.3k, 1.2M.
human :: Int -> String
human n | n >= 10000000 = printf "%.0fM" (fromIntegral n / 1e6 :: Double)
        | n >= 1000000 = printf "%.1fM" (fromIntegral n / 1e6 :: Double)
        | n >= 100000 = printf "%.0fk" (fromIntegral n / 1e3 :: Double)
        | n >= 1000 = printf "%.1fk" (fromIntegral n / 1e3 :: Double)
        | otherwise = show n

-- the endpoint --------------------------------------------------------------------------------

-- | Which protocol an endpoint speaks. 'Anthropic': whether its key is a bearer token (not an API key).
-- 'ClaudeCli': no endpoint at all but the @claude@ command, which is how a subscription is used
-- ("GhciSession.ClaudeCli"); there is then no key and no base, only a model.
data Provider = OpenAI | Anthropic Bool | ClaudeCli
  deriving (Eq, Show)

data Endpoint = Endpoint { eKey :: String, eBase :: String, eModel :: String, eProvider :: Provider }

-- | The endpoint the environment names, or why there is none (no key).
endpointFromEnv :: IO (Either String Endpoint)
endpointFromEnv = do
  key <- firstEnv ["DEEPSEEK_API_KEY", "OPENAI_API_KEY"]
  model <- fromMaybe "deepseek-v4-flash" <$> firstEnv ["DEEPSEEK_MODEL", "OPENAI_MODEL"]
  base <- fromMaybe "https://api.deepseek.com" <$> firstEnv ["DEEPSEEK_BASE_URL", "OPENAI_BASE_URL"]
  ant <- A.configFromEnv
  want <- firstEnv ["GHS_PROVIDER"]
  cliModel <- firstEnv ["GHS_CLAUDE_MODEL"]
  let openai = (\k -> Endpoint k (reverse (dropWhile (== '/') (reverse base))) model OpenAI) <$> key
      anthropic = (\c -> Endpoint (A.aKey c) (A.aBase c) (A.aModel c) (Anthropic (A.aBearer c))) <$> ant
      none = "no DEEPSEEK_API_KEY (or OPENAI_API_KEY), and no ANTHROPIC_API_KEY (or ANTHROPIC_AUTH_TOKEN), in the environment"
  pure (case want of
    Just "anthropic" -> maybe (Left "GHS_PROVIDER=anthropic, and no ANTHROPIC_API_KEY (or ANTHROPIC_AUTH_TOKEN) in the environment") Right anthropic
    Just "openai" -> maybe (Left "GHS_PROVIDER=openai, and no DEEPSEEK_API_KEY (or OPENAI_API_KEY) in the environment") Right openai
    -- (asked for, never taken by default: it spends a subscription's limits, and runs a program)
    Just w | w `elem` ["claude", "claude-cli", "subscription"] -> Right (Endpoint "" "claude" (fromMaybe A.defaultModel cliModel) ClaudeCli)
    Just other -> Left ("GHS_PROVIDER=" ++ other ++ ": anthropic, openai or claude")
    Nothing -> maybe (maybe (Left none) Right anthropic) Right openai)
  where firstEnv names = (\vs -> case [ v | Just v <- vs, not (null v) ] of { (v : _) -> Just v; [] -> Nothing }) <$> mapM lookupEnv names

-- | Is a conversation with this endpoint only ever to be appended to? Anthropic's is: a reply's thinking is
-- sent back as it came, and stands only in the conversation that made it -- an earlier message rewritten (the
-- chat's stubs for superseded reads) makes the thinking after it invalid, and on some accounts the request.
appendOnly :: Endpoint -> Bool
appendOnly e = eProvider e /= OpenAI

-- | DeepSeek's endpoint takes its own @thinking@ field; another provider's may reject it.
isDeepSeek :: Endpoint -> Bool
isDeepSeek e = T.isInfixOf (T.pack "deepseek") (T.toLower (T.pack (eBase e)))

-- the request and the reply --------------------------------------------------------------------

data Request = Request
  { rMessages :: [Json]             -- ^ the conversation, each @{"role": .., "content": ..}@ (a tool reply has @tool_call_id@)
  , rTools :: [Json]                -- ^ the tools, as the endpoint describes them (none: no tools)
  , rMaxTokens :: Int
  , rTemperature :: Maybe Double
  , rThinking :: Maybe Bool         -- ^ DeepSeek: thinking on or off (Nothing: the model's default)
  , rEffort :: Maybe String         -- ^ @reasoning_effort@ (low | high | max), when thinking
  , rTimeout :: Double
  , rLive :: Maybe (String -> T.Text -> IO ()) }   -- ^ told what a reply adds as it comes, where it comes in pieces: (@mind@ | @talk@ | @tool@, the piece)

data ToolCall = ToolCall { tcId :: String, tcName :: String, tcArgs :: Json, tcRaw :: Json }
data Usage = Usage { uIn :: Int, uOut :: Int, uCached :: Maybe Int }
data Reply = Reply
  { pContent :: T.Text, pReasoning :: T.Text, pToolCalls :: [ToolCall], pFinish :: String, pUsage :: Maybe Usage
  , pMessage :: Json                -- ^ the assistant message as received, to append to the conversation
  , pKeep :: [(String, Json)]       -- ^ what the assistant message has to carry besides its text and its calls, for
                                    --   the conversation to go on (Anthropic: the reply's blocks, its thinking with them)
  , pServed :: [(String, T.Text)] } -- ^ what the endpoint's own tools did in the call (a web search): (@tool@, the
                                    --   call) and (@echo@, its result), for the log -- no answer is owed them

-- | Why a call gave no reply. @fBusy@: the kind that asking again can mend -- the service busy or limiting
-- (408, 409, 429, 5xx), the connection lost -- as against a request it will never take (400, a key refused).
-- @fAfter@: the seconds it asked to be left alone.
data Failure = Failure { fWhy :: String, fBusy :: Bool, fAfter :: Maybe Int }

busyStatus :: Int -> Bool
busyStatus st = st `elem` [408, 409, 429] || st >= 500

-- | One call: the reply, or why none (the transport, a status other than 200, an error object, a shape
-- that is not a chat completion).
requestOnce :: Endpoint -> Request -> IO (Either Failure Reply)
requestOnce e q = apart $ case eProvider e of
  OpenAI -> requestOpenAI e q
  Anthropic bearer -> requestAnthropic (A.Config (eKey e) bearer (eBase e) (eModel e)) q
  ClaudeCli -> requestCli e q
  where
    -- (on a thread of its own, waited for: the exchange is a foreign call, which nothing can interrupt -- so
    -- whoever asked can be, and a turn that is stopped leaves at once; 'cancelRequests' then ends the exchange)
    apart act = do
      v <- newEmptyMVar
      _ <- forkIO ((try act :: IO (Either SomeException (Either Failure Reply))) >>= putMVar v)
      takeMVar v >>= either (\x -> pure (Left (Failure ("the model endpoint: " ++ show x) False Nothing))) pure

-- | A call that is one prompt and one answer, through the @claude@ command with no tools: what a compaction is.
-- (A turn is not made of such calls there: the command runs the tools itself, and the chat runs it for a whole
-- turn -- "GhciSession.Chat".) A call that wants no thinking is the lightest effort.
requestCli :: Endpoint -> Request -> IO (Either Failure Reply)
requestCli e q = do
  let (sys, msgs) = A.toMessages (rMessages q)
      blocks = concat [ lookupArr "content" m | m <- msgs, lookupStr "role" m == Just "user" ]
      effort = case rEffort q of { Just ef | ef /= "none" -> Just ef; _ | rThinking q == Just False -> Just "low"; _ -> Nothing }
  dir <- getTemporaryDirectory
  r <- C.runOnce (C.CliOpts (eModel e) effort (T.intercalate (T.pack "\n\n") sys) Nothing False (rThinking q == Just False)) dir blocks (rTimeout q)
  pure $ case r of
    Left (why, busy) -> Left (Failure why busy Nothing)
    Right x -> Right (Reply (C.xText x) T.empty [] "stop" (Just (Usage (C.xIn x) (C.xOut x) (Just (C.xCached x)))) (JObj [("role", JStr "assistant"), ("content", JText (C.xText x))]) [] [])

-- | A call, asked again while the service is busy: up to @tries@ times in all, after the seconds it asked for
-- or else 2, 4, 8, ... (two minutes at most), @say@ told each time. What asking again cannot mend is answered
-- at once: a request refused was asked three times, five seconds apart, and refused three times.
requestWith :: (String -> IO ()) -> Int -> Endpoint -> Request -> IO (Either String Reply)
requestWith say tries e q = go 1
  where
    go k = do
      r <- requestOnce e q
      case r of
        Right p -> pure (Right p)
        Left f | fBusy f && k < tries -> do
          let wait = min 120 (fromMaybe (2 ^ k) (fAfter f)) :: Int
          say (fWhy f ++ "; asking again in " ++ show wait ++ " s (" ++ show k ++ " of " ++ show (tries - 1) ++ ")")
          threadDelay (wait * 1000000)
          go (k + 1)
        Left f -> pure (Left (fWhy f))

request :: Endpoint -> Request -> IO (Either String Reply)
request = requestWith (const (pure ())) 4

-- | The same request to Anthropic's Messages API ("GhciSession.Anthropic" says what is sent and what comes
-- back). The reply is given in the terms the chat reads: a finish reason as the other protocol names it, a tool
-- call in that protocol's shape (it is what the conversation keeps), the prompt's tokens whole.
requestAnthropic :: A.Config -> Request -> IO (Either Failure Reply)
requestAnthropic c q = do
  -- (GHS_WEB_SEARCH=N: the model may search the web, N times a call at most. A turn's calls only -- the ones with
  -- tools; each search is charged by the API, so it is asked for, never assumed)
  web <- (\v -> case v >>= \x -> case reads x of { [(n, "")] -> Just n; _ -> Nothing } of { Just n | not (null (rTools q)) -> max 0 n; _ -> 0 }) <$> lookupEnv "GHS_WEB_SEARCH"
  -- (GHS_STREAM=0: the reply whole, in one piece, as it was before replies were read as they come)
  streamed <- (/= Just "0") <$> lookupEnv "GHS_STREAM"
  let body = A.requestBody c (A.Opts (rMaxTokens q) (rThinking q) (rEffort q) web streamed) (rMessages q) (rTools q)
  if streamed then requestStream c q (encodeBS body) else do
   r <- httpsRequest (A.url c) (A.headers c) (encodeBS body) (rTimeout q)
   pure $ case r of
    Left why -> Left (Failure ("the model endpoint: " ++ why) True Nothing)
    Right (status, after, bs) -> anthropicReply status after bs

-- | A reply to Anthropic's API read from what it sent whole: the reply, or the failure and its kind.
anthropicReply :: Int -> Maybe Int -> B.ByteString -> Either Failure Reply
anthropicReply status after bs = let failed why = Left (Failure why (busyStatus status) after) in case parseJsonBS bs of
      Left _ | status /= 200 -> failed ("the model endpoint answered " ++ show status ++ ": " ++ take 400 (show bs))
      Left err -> failed ("the model endpoint's reply is not JSON (" ++ err ++ "): " ++ take 400 (show bs))
      Right j -> case A.parseReply j of
        Left why -> failed ("the model endpoint: " ++ why ++ (if status /= 200 then " (" ++ show status ++ ")" else ""))
        Right _ | status /= 200 -> failed ("the model endpoint answered " ++ show status ++ ": " ++ take 400 (encode j))
        Right p ->
          let call (i, name, input) = ToolCall i name input (JObj [ ("id", JStr i), ("type", JStr "function")
                                                                  , ("function", JObj [("name", JStr name), ("arguments", JStr (encode input))]) ])
              finish = case A.rStop p of { "end_turn" -> "stop"; "stop_sequence" -> "stop"; "tool_use" -> "tool_calls"; "max_tokens" -> "length"; other -> other }
          in Right (Reply (A.rText p) (A.rThinking p) (map call (A.rCalls p)) finish (Just (Usage (A.rIn p) (A.rOut p) (Just (A.rCached p))))
                          (JObj [("role", JStr "assistant"), ("content", JArr (A.rBlocks p))]) [("anthropic_content", JArr (A.rBlocks p))] (A.rServer p))

-- | The same call with its reply read as it comes ('httpsStream', "GhciSession.Anthropic"'s events): what it adds
-- is told to whoever watches, and the call is held to five minutes of silence, not to a time in all. The events
-- make the message the API would have sent whole, which is read as that one is. A stream that ends before its
-- end, or says the service is overloaded, is the kind of failure that is asked again.
requestStream :: A.Config -> Request -> B.ByteString -> IO (Either Failure Reply)
requestStream c q body = do
  stV <- newIORef A.emptyStream
  let onLine l = case B.stripPrefix (BC.pack "data:") l of
        Just d | Right ev <- parseJsonBS d -> do
          st <- readIORef stV
          let (st', live) = A.streamEvent ev st
          writeIORef stV st'
          case (live, rLive q) of { (Just (k, t), Just f) -> f k t; _ -> pure () }
        _ -> pure ()
  r <- httpsStream (A.url c) (A.headers c) body 300 onLine
  st <- readIORef stV
  pure $ case r of
    Left why -> Left (Failure ("the model endpoint: " ++ why) True Nothing)
    Right (status, after, got)
      | status /= 200 -> anthropicReply status after got
      | Just e <- A.streamError st ->
          let kind = fromMaybe "" (lookupStr "type" (e .: "error"))
          in Left (Failure ("the model endpoint: " ++ kind ++ ": " ++ fromMaybe "(no message)" (lookupStr "message" (e .: "error"))) (kind `elem` ["overloaded_error", "rate_limit_error", "api_error", "timeout_error"]) after)
      | not (A.streamEnded st) -> Left (Failure "the model endpoint: the reply's stream ended before its end" True after)
      | otherwise -> anthropicReply 200 after (encodeBS (A.streamMessage st))

requestOpenAI :: Endpoint -> Request -> IO (Either Failure Reply)
requestOpenAI e q = do
  -- (GHS_STREAM=0: the reply whole, in one piece -- for an endpoint that does not send it as it comes)
  streamed <- (/= Just "0") <$> lookupEnv "GHS_STREAM"
  let body = JObj (  [ ("model", JStr (eModel e)), ("messages", JArr (rMessages q)), ("max_tokens", JNum (fromIntegral (rMaxTokens q))) ]
                  ++ [ ("tools", JArr (rTools q)) | not (null (rTools q)) ] ++ [ ("tool_choice", JStr "auto") | not (null (rTools q)) ]
                  ++ [ ("temperature", JNum t) | Just t <- [rTemperature q] ]
                  ++ [ ("thinking", JObj [("type", JStr (if on then "enabled" else "disabled"))]) | isDeepSeek e, Just on <- [rThinking q] ]
                  ++ [ ("reasoning_effort", JStr ef) | rThinking q /= Just False, Just ef <- [rEffort q] ]
                  ++ concat [ [("stream", JBool True), ("stream_options", JObj [("include_usage", JBool True)])] | streamed ])
      url = eBase e ++ "/chat/completions"
      headers = ["Content-Type: application/json", "Authorization: Bearer " ++ eKey e]
  if not streamed
    then either (\why -> Left (Failure ("the model endpoint: " ++ why) True Nothing)) (\(status, after, bs) -> openaiReply status after bs) <$> httpsRequest url headers (encodeBS body) (rTimeout q)
    else do
      -- the reply read as it comes: what it adds is told to whoever watches, and the call is held to five minutes of
      -- silence, not to a time in all. The pieces make the completion the endpoint would have sent whole
      stV <- newIORef emptyChunks
      let onLine l = case B.stripPrefix (BC.pack "data:") l of
            Just d | Right ev <- parseJsonBS d -> do
              st <- readIORef stV
              let (st', live) = chunkEvent ev st
              writeIORef stV st'
              case rLive q of { Just f -> mapM_ (uncurry f) live; Nothing -> pure () }
            Just d | BC.strip d == BC.pack "[DONE]" -> modifyIORef' stV (\st -> st { ckDone = True })
            _ -> pure ()
      r <- httpsStream url headers (encodeBS body) 300 onLine
      st <- readIORef stV
      pure $ case r of
        Left why -> Left (Failure ("the model endpoint: " ++ why) True Nothing)
        Right (status, after, got)
          | status /= 200 -> openaiReply status after got
          | Just err <- ckError st -> Left (Failure ("the model endpoint: " ++ fromMaybe (encode err) (lookupStr "message" err)) True after)
          | not (ckDone st) && null (ckFinish st) -> Left (Failure "the model endpoint: the reply's stream ended before its end" True after)
          | otherwise -> openaiReply 200 after (encodeBS (chunksMessage st))

-- | A reply that comes in pieces, so far: its text and its reasoning (newest first), its tool calls by their
-- place (each: id, name, the pieces of its arguments newest first), how it finished, what it used.
data Chunks = Chunks { ckText, ckMind :: [T.Text], ckCalls :: M.Map Int (String, String, [T.Text]), ckFinish :: String, ckUsage :: Json, ckError :: Maybe Json, ckDone :: Bool }

emptyChunks :: Chunks
emptyChunks = Chunks [] [] M.empty "" JNull Nothing False

-- | A piece taken in, and what it adds for whoever watches: (@talk@ | @mind@ | @tool@, the text).
chunkEvent :: Json -> Chunks -> (Chunks, [(String, T.Text)])
chunkEvent ev st0
  | Just _ <- obj (ev .: "error") = (st0 { ckError = Just (ev .: "error") }, [])
  | otherwise = foldl choice (st0 { ckUsage = case obj (ev .: "usage") of { Just _ -> ev .: "usage"; Nothing -> ckUsage st0 } }, []) (take 1 (lookupArr "choices" ev))
  where
    choice (st, live) ch =
      let d = ch .: "delta"
          text = fromMaybe T.empty (lookupText "content" d)
          mind = fromMaybe T.empty (lookupText "reasoning_content" d)
          (calls, told) = foldl call (ckCalls st, []) (lookupArr "tool_calls" d)
      in ( st { ckText = [ text | not (T.null text) ] ++ ckText st, ckMind = [ mind | not (T.null mind) ] ++ ckMind st, ckCalls = calls
              , ckFinish = fromMaybe (ckFinish st) (lookupStr "finish_reason" ch) }
         , live ++ [ ("mind", mind) | not (T.null mind) ] ++ [ ("talk", text) | not (T.null text) ] ++ told )
    call (m, told) tc =
      let i = maybe (M.size m) round (lookupNum "index" tc)
          fn = tc .: "function"
          args = fromMaybe T.empty (lookupText "arguments" fn)
          (i0, n0, as) = M.findWithDefault ("", "", []) i m
          pick old new = case new of { Just x | not (null x) -> x; _ -> old }
      in (M.insert i (pick i0 (lookupStr "id" tc), pick n0 (lookupStr "name" fn), [ args | not (T.null args) ] ++ as) m, told ++ [ ("tool", args) | not (T.null args) ])

-- | The completion the pieces make, in the shape it has when sent whole.
chunksMessage :: Chunks -> Json
chunksMessage st = JObj
  [ ("choices", JArr [ JObj [ ("index", JNum 0), ("finish_reason", JStr (ckFinish st)), ("message", JObj (
      [ ("role", JStr "assistant"), ("content", JText (T.concat (reverse (ckText st)))) ]
      ++ [ ("reasoning_content", JText (T.concat (reverse (ckMind st)))) | not (null (ckMind st)) ]
      ++ [ ("tool_calls", JArr [ JObj [ ("id", JStr i), ("type", JStr "function"), ("function", JObj [("name", JStr n), ("arguments", JText (T.concat (reverse as)))]) ] | (i, n, as) <- M.elems (ckCalls st) ]) | not (M.null (ckCalls st)) ])) ] ])
  , ("usage", ckUsage st) ]

-- | A completion read from what the endpoint sent whole: the reply, or the failure and its kind.
openaiReply :: Int -> Maybe Int -> B.ByteString -> Either Failure Reply
openaiReply status after bs = let failed why = Left (Failure why (busyStatus status) after) in case parseJsonBS bs of
      Left _ | status /= 200 -> failed ("the model endpoint answered " ++ show status ++ ": " ++ take 400 (show bs))
      Left err -> failed ("the model endpoint's reply is not JSON (" ++ err ++ "): " ++ take 400 (show bs))
      Right j | isJust (lookupStr "message" (j .: "error")) -> failed ("the model endpoint: " ++ fromMaybe "" (lookupStr "message" (j .: "error")) ++ (if status /= 200 then " (" ++ show status ++ ")" else ""))
      Right j | status /= 200 -> failed ("the model endpoint answered " ++ show status ++ ": " ++ take 400 (encode j))
      Right j -> case lookupArr "choices" j of
        (ch : _) ->
          let m = ch .: "message"
              calls = [ ToolCall (fromMaybe "" (lookupStr "id" tc)) (fromMaybe "" (lookupStr "name" fn)) (either (const (JObj [])) id (parseJson (fromMaybe "{}" (lookupStr "arguments" fn)))) tc
                      | tc <- lookupArr "tool_calls" m, let fn = tc .: "function" ]
              usage = (\u -> Usage (maybe 0 round (lookupNum "prompt_tokens" u)) (maybe 0 round (lookupNum "completion_tokens" u)) (round <$> lookupNum "prompt_cache_hit_tokens" u)) <$> (const (j .: "usage") <$> obj (j .: "usage"))
          in Right (Reply (fromMaybe T.empty (lookupText "content" m)) (fromMaybe T.empty (lookupText "reasoning_content" m)) calls (fromMaybe "" (lookupStr "finish_reason" ch)) usage m [] [])
        [] -> failed ("the model endpoint's reply has no choices: " ++ take 400 (encode j))
