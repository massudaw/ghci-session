{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | __The model, as an OpenAI-compatible chat endpoint__ (DeepSeek's by default): one request, one reply,
-- over HTTPS through libcurl ('cbits/ghs_http.c', loaded at run time). What the chat and the compactor
-- ('GhciSession.Chat') share: the endpoint from the environment, the request's shape, the reply's parts.
--
-- The environment: @DEEPSEEK_API_KEY@ (or @OPENAI_API_KEY@); @DEEPSEEK_MODEL@ / @DEEPSEEK_BASE_URL@, or
-- @OPENAI_MODEL@ / @OPENAI_BASE_URL@, override the flash model at @https://api.deepseek.com@.
module GhciSession.Llm
  ( Endpoint (..), endpointFromEnv, isDeepSeek
  , Request (..), request, Reply (..), Usage (..), ToolCall (..)
  , httpsPost
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
import System.Environment (lookupEnv)

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

-- the endpoint --------------------------------------------------------------------------------

data Endpoint = Endpoint { eKey :: String, eBase :: String, eModel :: String }

-- | The endpoint the environment names, or why there is none (no key).
endpointFromEnv :: IO (Either String Endpoint)
endpointFromEnv = do
  key <- firstEnv ["DEEPSEEK_API_KEY", "OPENAI_API_KEY"]
  model <- fromMaybe "deepseek-v4-flash" <$> firstEnv ["DEEPSEEK_MODEL", "OPENAI_MODEL"]
  base <- fromMaybe "https://api.deepseek.com" <$> firstEnv ["DEEPSEEK_BASE_URL", "OPENAI_BASE_URL"]
  pure (case key of
    Nothing -> Left "no DEEPSEEK_API_KEY (or OPENAI_API_KEY) in the environment"
    Just k -> Right (Endpoint k (reverse (dropWhile (== '/') (reverse base))) model))
  where firstEnv names = (\vs -> case [ v | Just v <- vs, not (null v) ] of { (v : _) -> Just v; [] -> Nothing }) <$> mapM lookupEnv names

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
  , pMessage :: Json }              -- ^ the assistant message as received, to append to the conversation

-- | One call: the reply, or why none (the transport, a status other than 200, an error object, a shape
-- that is not a chat completion).
request :: Endpoint -> Request -> IO (Either String Reply)
request e q = do
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
          in Right (Reply (fromMaybe T.empty (lookupText "content" m)) (fromMaybe T.empty (lookupText "reasoning_content" m)) calls (fromMaybe "" (lookupStr "finish_reason" ch)) usage m)
        [] -> Left ("the model endpoint's reply has no choices: " ++ take 400 (encode j))
