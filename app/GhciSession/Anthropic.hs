-- | __The model, as Anthropic's Messages API__ (@POST \/v1\/messages@): what a request is and what a reply says,
-- as values -- the transport is "GhciSession.Llm"'s, which also speaks the OpenAI protocol and hands this module
-- the same conversation it hands that one.
--
-- The conversation is kept by the chat in the OpenAI shape (a @system@ message, @user@, @assistant@ with
-- @tool_calls@, a @tool@ message a result). 'toMessages' turns it into this API's: the system text apart, a
-- @tool_use@ block a call, the results of one step in ONE @user@ message as @tool_result@ blocks (ahead of any
-- text: the API wants them first), turns of one role in a row made one.
--
-- Three things are this API's own.
--
-- * __The cache is asked for.__ A cached prefix is read only at a block's end that was marked, so: the system
--   prompt is one marked block (the tools are before it, and are cached with it); the view -- the @\<chat\>@ at the
--   head of a turn's first message -- is sent as blocks of 'viewBlock' lines with the last whole one marked, so a
--   turn finds the view of the turn before, which is this one's less a few lines at its end, as far as its last
--   whole block (one text, marked at its end, is never the prefix of a longer one); and the conversation after
--   it is cached by the request's own @cache_control@, which the API moves along as it grows.
-- * __Thinking is the model's, and is given back.__ On the current models it cannot be turned off, only made
--   lighter (@output_config.effort@); a reply's blocks -- its thinking with them -- are kept as they came
--   (@anthropic_content@ on the assistant message) and sent back unchanged, which the API requires of a
--   conversation that goes on. So such a conversation is only ever appended to ('appendOnly': the chat does not
--   rewrite an earlier tool result here).
-- * __A request can be declined__ (@stop_reason: refusal@, with a 200): it is said, not taken for an empty reply;
--   and where the API offers it, another model is asked in the same call (@fallbacks: default@).
--
-- The environment: @ANTHROPIC_API_KEY@ (sent as @x-api-key@), or @ANTHROPIC_AUTH_TOKEN@ (a bearer token);
-- @ANTHROPIC_BASE_URL@ (default @https:\/\/api.anthropic.com@) and @ANTHROPIC_MODEL@ (default @claude-opus-5-5@).
module GhciSession.Anthropic
  ( Config (..), configFromEnv, defaultModel, firstParty
  , Opts (..), url, headers, requestBody, toMessages, viewBlock, viewBlocks
  , Parsed (..), parseReply
  , Stream, emptyStream, streamEvent, streamMessage, streamEnded, streamError
  ) where

import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)

import GhciSession.Json

-- | Where the API is and who asks. @aBearer@: the key is a token for @Authorization: Bearer@, not an API key.
data Config = Config { aKey :: String, aBearer :: Bool, aBase :: String, aModel :: String }

defaultModel :: String
defaultModel = "claude-opus-5-5"

-- | The configuration the environment names, if it names a key.
configFromEnv :: IO (Maybe Config)
configFromEnv = do
  key <- nonEmpty <$> lookupEnv "ANTHROPIC_API_KEY"
  tok <- nonEmpty <$> lookupEnv "ANTHROPIC_AUTH_TOKEN"
  base <- fromMaybe "https://api.anthropic.com" . nonEmpty <$> lookupEnv "ANTHROPIC_BASE_URL"
  model <- fromMaybe defaultModel . nonEmpty <$> lookupEnv "ANTHROPIC_MODEL"
  let mk k b = Config k b (trimBase base) model
  pure (case (key, tok) of
    (Just k, _) -> Just (mk k False)
    (Nothing, Just t) -> Just (mk t True)
    _ -> Nothing)
  where nonEmpty v = case v of { Just x | not (null x) -> Just x; _ -> Nothing }

-- | A base as the API's root: no slash at its end, and no @\/v1@ (it is added).
trimBase :: String -> String
trimBase b0 = let b = reverse (dropWhile (== '/') (reverse b0)) in if "/v1" `isSuffixOf'` b then take (length b - 3) b else b
  where isSuffixOf' s x = reverse s `isPrefixOf` reverse x

-- | Is this Anthropic's own API (and not a gateway or another provider's endpoint that speaks its protocol)?
-- What only it is known to take -- the request's own @cache_control@, a fallback model -- is sent only there.
firstParty :: Config -> Bool
firstParty c = "api.anthropic.com" `isInfixOf` aBase c

url :: Config -> String
url c = aBase c ++ "/v1/messages"

headers :: Config -> [String]
headers c =
  [ "Content-Type: application/json", "anthropic-version: 2023-06-01"
  , if aBearer c then "Authorization: Bearer " ++ aKey c else "x-api-key: " ++ aKey c ]
  ++ [ "anthropic-beta: " ++ joinComma betas | not (null betas) ]
  where
    betas = [ "oauth-2025-04-20" | aBearer c && firstParty c ] ++ [ "server-side-fallback-2026-07-01" | fallsBack c ]
    joinComma = foldr1 (\a b -> a ++ "," ++ b)

-- what a model takes ----------------------------------------------------------------------------
--
-- Said of the models by name, since the API answers 400 to what one does not take. A model this does not
-- know (another provider's, behind this protocol) is sent neither thinking nor an effort unless one is asked.

-- | Adaptive thinking and an effort: the current models, of which Haiku 4.5 is not one (it takes neither).
adaptive :: Config -> Bool
adaptive c = "claude-" `isPrefixOf` aModel c && not ("haiku-4-5" `isInfixOf` aModel c)

-- | Another model asked in the same call when this one declines: Anthropic's API, and the models that have it.
fallsBack :: Config -> Bool
fallsBack c = firstParty c && aModel c `elem` ["claude-fable-5-1", "claude-opus-5-5", "claude-opus-5", "claude-sonnet-5-5"]

-- the request -----------------------------------------------------------------------------------

-- | What a call asks beyond its conversation. @oThink@: @Just False@ is a call that wants no thinking (a
-- compaction) -- which here is the lightest effort, thinking being the model's to do. @oEffort@: low, medium,
-- high, xhigh or max. @oSearches@: how many web searches the model may make in the call (0: none) -- the
-- API's own tool, run by it: the search and its results come back in the reply, as blocks to keep. @oStream@:
-- the reply is asked for as a stream of events ('streamEvent').
data Opts = Opts { oMaxTokens :: Int, oThink :: Maybe Bool, oEffort :: Maybe String, oSearches :: Int, oStream :: Bool }

-- | The request's body, from the conversation and the tools as the chat keeps them (the OpenAI shape).
requestBody :: Config -> Opts -> [Json] -> [Json] -> Json
requestBody c o msgs tools = JObj $
  [ ("model", JStr (aModel c))
  -- (thinking is spent out of this: a line of a few hundred tokens asked with a budget of as many would be
  -- cut off inside its thoughts. It is a ceiling, and costs nothing unused)
  , ("max_tokens", JNum (fromIntegral (if adaptive c then max 4000 (oMaxTokens o) else oMaxTokens o))) ]
  ++ [ ("system", JArr [ JObj [("type", JStr "text"), ("text", JText (T.intercalate (T.pack "\n\n") sys)), ("cache_control", ephemeral)] ]) | not (null sys) ]
  ++ [ ("tools", JArr (map tool tools ++ web)) | not (null tools) ]
  ++ [ ("messages", JArr body) ]
  ++ [ ("thinking", JObj [("type", JStr "adaptive"), ("display", JStr "summarized")]) | adaptive c, oThink o /= Just False ]
  ++ [ ("output_config", JObj [("effort", JStr e)]) | Just e <- [effort] ]
  ++ [ ("cache_control", ephemeral) | firstParty c ]
  ++ [ ("fallbacks", JStr "default") | fallsBack c ]
  ++ [ ("stream", JBool True) | oStream o ]
  where
    (sys, body) = toMessages msgs
    -- (asked for, so sent, whoever the endpoint is: one that has no such tool says so. The version of the tool
    -- is the one the model takes)
    web = [ JObj [("type", JStr (if adaptive c then "web_search_20260209" else "web_search_20250305")), ("name", JStr "web_search"), ("max_uses", JNum (fromIntegral (oSearches o)))]
          | oSearches o > 0 ]
    effort = case oEffort o of
      Just e | e /= "none" -> Just e
      -- (said, not left to the model's default, which is not the same from one model to the next)
      _ | adaptive c -> Just (if oThink o == Just False then "low" else "high")
        | otherwise -> Nothing
    tool t = let f = t .: "function" in JObj
      [ ("name", JStr (fromMaybe "" (lookupStr "name" f))), ("description", JText (fromMaybe T.empty (lookupText "description" f)))
      , ("input_schema", case f .: "parameters" of { JNull -> JObj [("type", JStr "object"), ("properties", JObj [])]; p -> p }) ]

ephemeral :: Json
ephemeral = JObj [("type", JStr "ephemeral")]

-- | The system texts, and the messages: each a role and its blocks, no two of one role in a row.
toMessages :: [Json] -> ([T.Text], [Json])
toMessages msgs = ( [ t | m <- msgs, role m == "system", Just t <- [lookupText "content" m], not (T.null (T.strip t)) ]
                  , map (\(r, bs) -> JObj [("role", JStr r), ("content", JArr bs)]) (merge (firstView (concatMap one msgs))) )
  where
    role m = fromMaybe "" (lookupStr "role" m)
    text m = fromMaybe T.empty (lookupText "content" m)
    one m = case role m of
      "user" -> [("user", [textBlock (text m)])]
      "assistant" -> case m .: "anthropic_content" of
        -- (the reply's own blocks, as they came: its thinking has to go back unchanged)
        JArr bs | not (null bs) -> [("assistant", bs)]
        _ -> case [ textBlock (text m) | not (T.null (T.strip (text m))) ] ++ map toolUse (lookupArr "tool_calls" m) of
          [] -> []          -- (a reply with nothing in it is no turn: the API takes no empty one)
          bs -> [("assistant", bs)]
      "tool" -> [("user", [JObj ([ ("type", JStr "tool_result"), ("tool_use_id", JStr (fromMaybe "" (lookupStr "tool_call_id" m))), ("content", JText (orSay "(no output)" (text m))) ]
                                 ++ [ ("is_error", JBool True) | T.pack "ERROR: " `T.isPrefixOf` text m ])])]
      _ -> []
    toolUse tc = let f = tc .: "function" in JObj
      [ ("type", JStr "tool_use"), ("id", JStr (fromMaybe "" (lookupStr "id" tc))), ("name", JStr (fromMaybe "" (lookupStr "name" f)))
      , ("input", case parseJson (fromMaybe "{}" (lookupStr "arguments" f)) of { Right j@(JObj _) -> j; _ -> JObj [] }) ]
    textBlock t = JObj [("type", JStr "text"), ("text", JText (orSay "(nothing)" t))]
    orSay d t = if T.null (T.strip t) then T.pack d else t
    -- the first user message is the turn's: its view goes as blocks
    firstView ((("user", [b])) : rest) | Just t <- lookupText "text" b = ("user", viewBlocks t) : rest
    firstView ms = ms
    merge ((r, a) : (r', b) : rest) | r == r' = merge ((r, a ++ b) : rest)
    merge (m : rest) = m : merge rest
    merge [] = []

-- | The lines of the view a block holds. A cached prefix is read only at a block's end, and of the blocks before
-- the request's last twenty: a turn's view is the one before and a few lines more, so it is found.
viewBlock :: Int
viewBlock = 4

-- | A turn's first message as blocks: what is before the view, the view's lines 'viewBlock' at a time with the
-- last whole block marked for the cache, its last lines with the closing tag, and what follows it (the message
-- itself). A text with no view is one block.
viewBlocks :: T.Text -> [Json]
viewBlocks t = case break ((== T.pack "<chat>") . T.strip) (T.lines t) of
  (before, _ : rest) | (inside, _ : after) <- break ((== T.pack "</chat>") . T.strip) rest ->
    let groups = chunks inside
        whole = [ g | g <- groups, length g == viewBlock ]
        tailLines = concat [ g | g <- groups, length g /= viewBlock ]
        nWhole = length whole
        block marked ls = JObj ([("type", JStr "text"), ("text", JText (T.unlines ls))] ++ [ ("cache_control", ephemeral) | marked ])
    in [ block False (before ++ [T.pack "<chat>"]) ]
       ++ [ block (k == nWhole) g | (k, g) <- zip [1 ..] whole ]
       ++ [ block False (tailLines ++ [T.pack "</chat>"]) ]
       ++ [ JObj [("type", JStr "text"), ("text", JText (T.unlines after))] | not (T.null (T.strip (T.unlines after))) ]
  _ -> [ JObj [("type", JStr "text"), ("text", JText (if T.null (T.strip t) then T.pack "(nothing)" else t))] ]
  where chunks [] = []
        chunks ls = let (a, b) = splitAt viewBlock ls in a : chunks b

-- the reply -------------------------------------------------------------------------------------

-- | A reply's parts. @rIn@ is the whole prompt (what was read from the cache and what was written to it are
-- counted apart by the API, and @input_tokens@ is only what was neither); @rCached@ what of it was read from
-- the cache. @rStop@: @end_turn@, @tool_use@, @max_tokens@, @refusal@, @pause_turn@ (the API stopped in the middle
-- of its own tools' work: the reply is sent back as it is and it goes on), ... as the API says it. @rBlocks@: the
-- content as it came, to send back.
data Parsed = Parsed
  { rText :: T.Text, rThinking :: T.Text, rCalls :: [(String, String, Json)], rStop :: String
  , rIn :: Int, rOut :: Int, rCached :: Int, rBlocks :: [Json]
  , rServer :: [(String, T.Text)] }   -- ^ what the API's own tools did, as the log has a tool's doing: (@tool@, the call), (@echo@, its result)

-- | A reply, or what the API said instead (@type: error@, with its kind and message).
parseReply :: Json -> Either String Parsed
parseReply j
  | lookupStr "type" j == Just "error" =
      Left (fromMaybe "error" (lookupStr "type" (j .: "error")) ++ ": " ++ fromMaybe "(no message)" (lookupStr "message" (j .: "error")))
  | Nothing <- arr (j .: "content") = Left ("no content in the reply: " ++ take 300 (encode j))
  | otherwise =
      let blocks = lookupArr "content" j
          of' ty = [ b | b <- blocks, lookupStr "type" b == Just ty ]
          said = T.concat [ t | b <- of' "text", Just t <- [lookupText "text" b] ]
          stop = fromMaybe "" (lookupStr "stop_reason" j)
          u = j .: "usage"
          n k = maybe 0 round (lookupNum k u) :: Int
          -- a request declined is a 200 with this stop reason and, often, nothing said: say it
          declined = T.pack ("[the model declined this request" ++ maybe "" (\c -> " (" ++ c ++ ")") (lookupStr "category" (j .: "stop_details")) ++ "]")
      in Right Parsed
           { rText = if stop == "refusal" && T.null (T.strip said) then declined else said
           , rThinking = T.intercalate (T.pack "\n") [ t | b <- of' "thinking", Just t <- [lookupText "thinking" b], not (T.null t) ]
           , rCalls = [ (fromMaybe "" (lookupStr "id" b), fromMaybe "" (lookupStr "name" b), case b .: "input" of { JNull -> JObj []; i -> i }) | b <- of' "tool_use" ]
           , rStop = stop
           , rIn = n "input_tokens" + n "cache_creation_input_tokens" + n "cache_read_input_tokens"
           , rOut = n "output_tokens", rCached = n "cache_read_input_tokens"
           -- (as they came, but for a text block with nothing in it: the API gives one at times and takes none back)
           , rBlocks = [ b | b <- blocks, lookupStr "type" b /= Just "text" || maybe False (not . T.null . T.strip) (lookupText "text" b) ]
           , rServer = concatMap served blocks }
  where
    served b = case fromMaybe "" (lookupStr "type" b) of
      "server_tool_use" -> [("tool", T.pack (fromMaybe "?" (lookupStr "name" b) ++ " " ++ encode (b .: "input")))]
      ty | "_tool_result" `isSuffixOf'` ty, ty /= "tool_result" -> [("echo", case b .: "content" of
              -- a search's results are a list (their pages are not readable here); anything else is an error, an object
              JArr rs -> T.intercalate (T.pack "\n") [ T.pack (fromMaybe "" (lookupStr "title" r) ++ " " ++ fromMaybe "" (lookupStr "url" r)) | r <- rs ]
              other -> T.pack ("ERROR: " ++ fromMaybe (encode other) (lookupStr "error_code" other)))]
      _ -> []
    isSuffixOf' x y = reverse x `isPrefixOf` reverse y

-- the reply, as it comes ------------------------------------------------------------------------
--
-- Asked to stream, the API sends the reply as events: the message's start (with what the prompt cost), each
-- block's start, its text or thinking or a tool call's arguments a piece at a time, its end, the message's end
-- (why it stopped, what the reply cost). So a reply that takes ten minutes says so all along, and the call is
-- held to how long the API may be SILENT, not to how long it may take.
--
-- The events are put together into the message the API would have sent whole, and that is read as any reply is
-- ('parseReply'): one reading of a reply, however it came.

-- | A reply being received: its blocks by position, a tool call's arguments as the text they arrive as, why it
-- stopped, its usage as last said, an error the stream carried, whether its end was seen.
data Stream = Stream
  { sBlocks :: M.Map Int Json, sArgs :: M.Map Int T.Text, sClosed :: [Int], sStop :: Json, sDetails :: Json, sUsage :: [(String, Json)]
  , sErr :: Maybe Json, sEnd :: Bool }

emptyStream :: Stream
emptyStream = Stream M.empty M.empty [] JNull JNull [] Nothing False

streamEnded :: Stream -> Bool
streamEnded = sEnd

-- | The error a stream carried, as the API's error object (an overloaded service says so here, after a 200).
streamError :: Stream -> Maybe Json
streamError = sErr

-- | One event taken: the reply so far, and what it added for whoever is watching -- (@mind@, thinking),
-- (@talk@, text), (@tool@, a call's name or a piece of its arguments).
streamEvent :: Json -> Stream -> (Stream, Maybe (String, T.Text))
streamEvent ev st = case fromMaybe "" (lookupStr "type" ev) of
  "message_start" -> (st { sUsage = merge (lookupObj "usage" (ev .: "message")) }, Nothing)
  "content_block_start" ->
    let b = ev .: "content_block"
        named = case lookupStr "type" b of
          Just ty | "tool_use" `isSuffixOf'` ty -> Just ("tool", T.pack (fromMaybe "" (lookupStr "name" b) ++ " "))
          _ -> Nothing
    in (st { sBlocks = M.insert ix b (sBlocks st) }, named)
  "content_block_delta" ->
    let d = ev .: "delta"
        add k t = st { sBlocks = M.adjust (\b -> set k (JText (fromMaybe T.empty (lookupText k b) <> t)) b) ix (sBlocks st) }
        piece k = fromMaybe T.empty (lookupText k d)
    in case fromMaybe "" (lookupStr "type" d) of
         "text_delta" -> (add "text" (piece "text"), Just ("talk", piece "text"))
         "thinking_delta" -> (add "thinking" (piece "thinking"), Just ("mind", piece "thinking"))
         "signature_delta" -> (st { sBlocks = M.adjust (set "signature" (JText (piece "signature"))) ix (sBlocks st) }, Nothing)
         "input_json_delta" -> (st { sArgs = M.insertWith (flip (<>)) ix (piece "partial_json") (sArgs st) }, Just ("tool", piece "partial_json"))
         "citations_delta" -> (st { sBlocks = M.adjust (\b -> set "citations" (JArr (lookupArr "citations" b ++ [d .: "citation"])) b) ix (sBlocks st) }, Nothing)
         _ -> (st, Nothing)
  "message_delta" ->
    let d = ev .: "delta"
        keep new old = case new of { JNull -> old; _ -> new }
    in (st { sStop = keep (d .: "stop_reason") (sStop st), sDetails = keep (d .: "stop_details") (sDetails st), sUsage = merge (lookupObj "usage" ev) }, Nothing)
  "content_block_stop" -> (st { sClosed = ix : sClosed st }, Nothing)
  "message_stop" -> (st { sEnd = True }, Nothing)
  "error" -> (st { sErr = Just ev, sEnd = True }, Nothing)
  _ -> (st, Nothing)
  where
    ix = maybe 0 round (lookupNum "index" ev) :: Int
    -- (a count said again is the count now; one not said stands)
    merge new = [ (k, v) | (k, v) <- sUsage st, k `notElem` map fst said ] ++ said where said = [ kv | kv@(_, v) <- new, v /= JNull ]
    isSuffixOf' x y = reverse x `isPrefixOf` reverse y

-- | The message the events made, as the API sends one whole. A call's arguments are the object their pieces
-- spell; a call whose pieces spell none -- the reply was cut off inside it -- is left out, since half a call
-- is no call (the reply is then one cut off at the limit, which is what it is).
streamMessage :: Stream -> Json
streamMessage st = JObj
  [ ("type", JStr "message"), ("role", JStr "assistant"), ("content", JArr (concatMap block (M.toAscList (sBlocks st))))
  , ("stop_reason", sStop st), ("stop_details", sDetails st), ("usage", JObj (sUsage st)) ]
  where
    isCall b = maybe False (\ty -> reverse "tool_use" `isPrefixOf` reverse ty) (lookupStr "type" b)
    block (ix, b)
      | isCall b && ix `notElem` sClosed st = []          -- (its end never came)
      | otherwise = case M.lookup ix (sArgs st) of
          Nothing -> [b]
          Just t | T.null (T.strip t) -> [set "input" (JObj []) b]
                 | otherwise -> case parseJson (T.unpack t) of
                     Right j@(JObj _) -> [set "input" j b]
                     _ -> []
