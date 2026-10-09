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
  , Request (..), request, Reply (..), Usage (..), ToolCall (..)
  , httpsPost
  , usageFileEnv, recordUsage, human
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
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
import System.Environment (lookupEnv)
import System.IO (IOMode (..), hClose, hPutStrLn, openFile)
import Text.Printf (printf)

import qualified GhciSession.Anthropic as A
import GhciSession.Json

foreign import ccall safe "ghs_https_post" c_post
  :: CString -> CString -> Ptr CChar -> CSize -> CLong -> CString -> Ptr CString -> Ptr CSize -> Ptr CLong -> CString -> CSize -> IO CInt
foreign import ccall unsafe "ghs_https_free" c_free :: CString -> IO ()

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

-- the ledger ----------------------------------------------------------------------------------

-- | The environment variable that names the usage ledger for a process the daemon runs (the compactor).
usageFileEnv :: String
usageFileEnv = "GHS_USAGE_FILE"

-- | One model call appended to the ledger (@<state>/<session>/usage.jsonl@): when, who asked (chat,
-- summarize), the model, the tokens in (and how many of them the provider had cached), the tokens out,
-- the seconds. Best effort: accounting never fails a call.
recordUsage :: FilePath -> String -> Endpoint -> Usage -> Double -> IO ()
recordUsage file who e u secs = do
  t <- realToFrac <$> getPOSIXTime :: IO Double
  stamp <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime
  let line = encode (JObj [ ("t", JNum t), ("date", JStr stamp), ("who", JStr who), ("model", JStr (eModel e))
                          , ("in", JNum (fromIntegral (uIn u))), ("cached", JNum (fromIntegral (fromMaybe 0 (uCached u))))
                          , ("out", JNum (fromIntegral (uOut u))), ("secs", JNum (fromIntegral (round (secs * 10) :: Int) / 10)) ])
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
data Provider = OpenAI | Anthropic Bool
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
  let openai = (\k -> Endpoint k (reverse (dropWhile (== '/') (reverse base))) model OpenAI) <$> key
      anthropic = (\c -> Endpoint (A.aKey c) (A.aBase c) (A.aModel c) (Anthropic (A.aBearer c))) <$> ant
      none = "no DEEPSEEK_API_KEY (or OPENAI_API_KEY), and no ANTHROPIC_API_KEY (or ANTHROPIC_AUTH_TOKEN), in the environment"
  pure (case want of
    Just "anthropic" -> maybe (Left "GHS_PROVIDER=anthropic, and no ANTHROPIC_API_KEY (or ANTHROPIC_AUTH_TOKEN) in the environment") Right anthropic
    Just "openai" -> maybe (Left "GHS_PROVIDER=openai, and no DEEPSEEK_API_KEY (or OPENAI_API_KEY) in the environment") Right openai
    Just other -> Left ("GHS_PROVIDER=" ++ other ++ ": anthropic or openai")
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
  , rTimeout :: Double }

data ToolCall = ToolCall { tcId :: String, tcName :: String, tcArgs :: Json, tcRaw :: Json }
data Usage = Usage { uIn :: Int, uOut :: Int, uCached :: Maybe Int }
data Reply = Reply
  { pContent :: T.Text, pReasoning :: T.Text, pToolCalls :: [ToolCall], pFinish :: String, pUsage :: Maybe Usage
  , pMessage :: Json                -- ^ the assistant message as received, to append to the conversation
  , pKeep :: [(String, Json)] }     -- ^ what the assistant message has to carry besides its text and its calls, for
                                    --   the conversation to go on (Anthropic: the reply's blocks, its thinking with them)

-- | One call: the reply, or why none (the transport, a status other than 200, an error object, a shape
-- that is not a chat completion).
request :: Endpoint -> Request -> IO (Either String Reply)
request e q = case eProvider e of
  OpenAI -> requestOpenAI e q
  Anthropic bearer -> requestAnthropic (A.Config (eKey e) bearer (eBase e) (eModel e)) q

-- | The same request to Anthropic's Messages API ("GhciSession.Anthropic" says what is sent and what comes
-- back). The reply is given in the terms the chat reads: a finish reason as the other protocol names it, a tool
-- call in that protocol's shape (it is what the conversation keeps), the prompt's tokens whole.
requestAnthropic :: A.Config -> Request -> IO (Either String Reply)
requestAnthropic c q = do
  let body = A.requestBody c (A.Opts (rMaxTokens q) (rThinking q) (rEffort q)) (rMessages q) (rTools q)
  r <- httpsPost (A.url c) (A.headers c) (encodeBS body) (rTimeout q)
  pure $ case r of
    Left why -> Left ("the model endpoint: " ++ why)
    Right (status, bs) -> case parseJsonBS bs of
      Left _ | status /= 200 -> Left ("the model endpoint answered " ++ show status ++ ": " ++ take 400 (show bs))
      Left err -> Left ("the model endpoint's reply is not JSON (" ++ err ++ "): " ++ take 400 (show bs))
      Right j -> case A.parseReply j of
        Left why -> Left ("the model endpoint: " ++ why ++ (if status /= 200 then " (" ++ show status ++ ")" else ""))
        Right _ | status /= 200 -> Left ("the model endpoint answered " ++ show status ++ ": " ++ take 400 (encode j))
        Right p ->
          let call (i, name, input) = ToolCall i name input (JObj [ ("id", JStr i), ("type", JStr "function")
                                                                  , ("function", JObj [("name", JStr name), ("arguments", JStr (encode input))]) ])
              finish = case A.rStop p of { "end_turn" -> "stop"; "stop_sequence" -> "stop"; "tool_use" -> "tool_calls"; "max_tokens" -> "length"; other -> other }
          in Right (Reply (A.rText p) (A.rThinking p) (map call (A.rCalls p)) finish (Just (Usage (A.rIn p) (A.rOut p) (Just (A.rCached p))))
                          (JObj [("role", JStr "assistant"), ("content", JArr (A.rBlocks p))]) [("anthropic_content", JArr (A.rBlocks p))])

requestOpenAI :: Endpoint -> Request -> IO (Either String Reply)
requestOpenAI e q = do
  let body = JObj (  [ ("model", JStr (eModel e)), ("messages", JArr (rMessages q)), ("max_tokens", JNum (fromIntegral (rMaxTokens q))) ]
                  ++ [ ("tools", JArr (rTools q)) | not (null (rTools q)) ] ++ [ ("tool_choice", JStr "auto") | not (null (rTools q)) ]
                  ++ [ ("temperature", JNum t) | Just t <- [rTemperature q] ]
                  ++ [ ("thinking", JObj [("type", JStr (if on then "enabled" else "disabled"))]) | isDeepSeek e, Just on <- [rThinking q] ]
                  ++ [ ("reasoning_effort", JStr ef) | rThinking q /= Just False, Just ef <- [rEffort q] ])
  r <- httpsPost (eBase e ++ "/chat/completions") ["Content-Type: application/json", "Authorization: Bearer " ++ eKey e] (encodeBS body) (rTimeout q)
  pure $ case r of
    Left why -> Left ("the model endpoint: " ++ why)
    Right (status, bs) -> case parseJsonBS bs of
      Left _ | status /= 200 -> Left ("the model endpoint answered " ++ show status ++ ": " ++ take 400 (show bs))
      Left err -> Left ("the model endpoint's reply is not JSON (" ++ err ++ "): " ++ take 400 (show bs))
      Right j | isJust (lookupStr "message" (j .: "error")) -> Left ("the model endpoint: " ++ fromMaybe "" (lookupStr "message" (j .: "error")) ++ (if status /= 200 then " (" ++ show status ++ ")" else ""))
      Right j | status /= 200 -> Left ("the model endpoint answered " ++ show status ++ ": " ++ take 400 (encode j))
      Right j -> case lookupArr "choices" j of
        (ch : _) ->
          let m = ch .: "message"
              calls = [ ToolCall (fromMaybe "" (lookupStr "id" tc)) (fromMaybe "" (lookupStr "name" fn)) (either (const (JObj [])) id (parseJson (fromMaybe "{}" (lookupStr "arguments" fn)))) tc
                      | tc <- lookupArr "tool_calls" m, let fn = tc .: "function" ]
              usage = (\u -> Usage (maybe 0 round (lookupNum "prompt_tokens" u)) (maybe 0 round (lookupNum "completion_tokens" u)) (round <$> lookupNum "prompt_cache_hit_tokens" u)) <$> (const (j .: "usage") <$> obj (j .: "usage"))
          in Right (Reply (fromMaybe T.empty (lookupText "content" m)) (fromMaybe T.empty (lookupText "reasoning_content" m)) calls (fromMaybe "" (lookupStr "finish_reason" ch)) usage m [])
        [] -> Left ("the model endpoint's reply has no choices: " ++ take 400 (encode j))
