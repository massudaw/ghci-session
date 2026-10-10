{-# OPTIONS_GHC -O2 #-}  -- the byte-level parser and the encoder: -O2 is a third faster here (measured: parse 0.46 -> 0.31 ms on a 0.5 MB reply) and nowhere else in the package
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | A small JSON value: enough for the configuration file, the status file and the socket protocol,
-- without a dependency outside GHC's boot packages. Objects keep their key order.
--
-- Strings are 'Text' and the wire is bytes: a reply can carry megabytes of GHCi output, and as a @String@
-- every character of it was a 24-byte cons cell, decoded, escaped and re-encoded a character at a time
-- (a 0.5 MB reply allocated 16 MB to encode). 'JStr' is a pattern over the same constructor for the many
-- small strings that are more convenient as @String@.
module GhciSession.Json
  ( Json (JNull, JBool, JNum, JText, JArr, JObj, JStr)
  , parseJson, parseJsonBS, encode, encodeBS, encodePretty
  , (.:), str, txt, num, bool, arr, obj, strs, lookupStr, lookupText, lookupBool, lookupNum, lookupArr, lookupObj
  , set, setDefault
  ) where

import Prelude

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import Data.Bits (shiftL, (.|.))
import Data.Char (chr, isSpace, ord)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Word (Word8)
import Numeric (showHex)

data Json = JNull | JBool Bool | JNum Double | JText T.Text | JArr [Json] | JObj [(String, Json)]
  deriving (Eq, Show)

pattern JStr :: String -> Json
pattern JStr s <- JText (T.unpack -> s) where JStr s = JText (T.pack s)

{-# COMPLETE JNull, JBool, JNum, JStr, JArr, JObj #-}

-- | A member of an object, 'JNull' when absent or when the value is not an object.
(.:) :: Json -> String -> Json
JObj kvs .: k = maybe JNull id (lookup k kvs)
_ .: _ = JNull

str :: Json -> Maybe String
str (JText s) = Just (T.unpack s)
str _ = Nothing

txt :: Json -> Maybe T.Text
txt (JText s) = Just s
txt _ = Nothing

num :: Json -> Maybe Double
num (JNum n) = Just n
num _ = Nothing

bool :: Json -> Maybe Bool
bool (JBool b) = Just b
bool _ = Nothing

arr :: Json -> Maybe [Json]
arr (JArr xs) = Just xs
arr _ = Nothing

obj :: Json -> Maybe [(String, Json)]
obj (JObj kvs) = Just kvs
obj _ = Nothing

strs :: Json -> [String]
strs (JArr xs) = [ T.unpack s | JText s <- xs ]
strs (JText s) = [T.unpack s]
strs _ = []

lookupStr :: String -> Json -> Maybe String
lookupStr k j = str (j .: k)

lookupText :: String -> Json -> Maybe T.Text
lookupText k j = txt (j .: k)

lookupBool :: String -> Json -> Maybe Bool
lookupBool k j = bool (j .: k)

lookupNum :: String -> Json -> Maybe Double
lookupNum k j = num (j .: k)

lookupArr :: String -> Json -> [Json]
lookupArr k j = maybe [] id (arr (j .: k))

lookupObj :: String -> Json -> [(String, Json)]
lookupObj k j = maybe [] id (obj (j .: k))

-- | Set a member, replacing it in place if it is there, else appending it.
set :: String -> Json -> Json -> Json
set k v (JObj kvs)
  | any ((== k) . fst) kvs = JObj [ (k', if k' == k then v else v') | (k', v') <- kvs ]
  | otherwise = JObj (kvs ++ [(k, v)])
set k v _ = JObj [(k, v)]

setDefault :: String -> Json -> Json -> Json
setDefault k v j = case j .: k of { JNull -> set k v j; _ -> j }

-- encoding ---------------------------------------------------------------------

-- | The wire form: one line of UTF-8 (no newline inside: they are escaped).
encodeBS :: Json -> B.ByteString
encodeBS = BL.toStrict . BB.toLazyByteString . go
  where
    go JNull = "null"
    go (JBool b) = if b then "true" else "false"
    go (JNum n) = BB.string7 (showNum n)
    go (JText s) = text s
    go (JArr xs) = BB.char7 '[' <> commas (map go xs) <> BB.char7 ']'
    go (JObj kvs) = BB.char7 '{' <> commas [ text (T.pack k) <> ": " <> go v | (k, v) <- kvs ] <> BB.char7 '}'
    commas [] = mempty
    commas (x : xs) = x <> mconcat [ ", " <> y | y <- xs ]

encode :: Json -> String
encode = T.unpack . TE.decodeUtf8 . encodeBS

-- | A string: the runs that need no escaping are copied whole.
text :: T.Text -> BB.Builder
text s = BB.char7 '"' <> go s <> BB.char7 '"'
  where
    go t = case T.break needs t of
      (a, b) | T.null b -> TE.encodeUtf8Builder a
             | otherwise -> TE.encodeUtf8Builder a <> esc (T.head b) <> go (T.tail b)
    needs c = c == '"' || c == '\\' || c < ' '
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c = let h = showHex (ord c) "" in BB.string7 ("\\u" ++ replicate (4 - length h) '0' ++ h)

-- | One member per line at each level: what a person reads in @status.json@.
encodePretty :: Json -> String
encodePretty = go 0
  where
    go _ (JArr []) = "[]"
    go _ (JObj []) = "{}"
    go d (JArr xs) = "[\n" ++ intercalate ",\n" [ pad (d + 1) ++ go (d + 1) x | x <- xs ] ++ "\n" ++ pad d ++ "]"
    go d (JObj kvs) = "{\n" ++ intercalate ",\n" [ pad (d + 1) ++ encode (JStr k) ++ ": " ++ go (d + 1) v | (k, v) <- kvs ] ++ "\n" ++ pad d ++ "}"
    go _ v = encode v
    pad d = replicate d ' '

showNum :: Double -> String
showNum n
  | isNaN n || isInfinite n = "null"
  | n == fromIntegral (round n :: Integer) && abs n < 1e15 = show (round n :: Integer)
  | otherwise = show n

-- parsing ------------------------------------------------------------------------

parseJson :: String -> Either String Json
parseJson = parseJsonBS . TE.encodeUtf8 . T.pack

-- | Parse one JSON value from UTF-8 bytes (trailing space allowed); 'Left' says where it went wrong.
-- A string with no escapes -- nearly all of them -- is a slice of the input, decoded once.
parseJsonBS :: B.ByteString -> Either String Json
parseJsonBS b = case value b (skip b 0) of
  Right (v, i) | skip b i == B.length b -> Right v
               | otherwise -> Left ("trailing text at byte " ++ show i)
  Left e -> Left e

at :: B.ByteString -> Int -> Word8
at b i = if i < B.length b then BU.unsafeIndex b i else 0

skip :: B.ByteString -> Int -> Int
skip b i = if i < B.length b && (let w = BU.unsafeIndex b i in w == 32 || (w >= 9 && w <= 13)) then skip b (i + 1) else i

w2c :: Word8 -> Char
w2c = chr . fromIntegral

lit :: B.ByteString -> Int -> B.ByteString -> Bool
lit b i s = B.isPrefixOf s (B.drop i b)

value :: B.ByteString -> Int -> Either String (Json, Int)
value b i = case w2c (at b i) of
  'n' | lit b i "null" -> Right (JNull, i + 4)
  't' | lit b i "true" -> Right (JBool True, i + 4)
  'f' | lit b i "false" -> Right (JBool False, i + 5)
  '"' -> (\(s, j) -> (JText s, j)) <$> string b (i + 1)
  '[' -> let j = skip b (i + 1) in if w2c (at b j) == ']' then Right (JArr [], j + 1) else elems b j []
  '{' -> let j = skip b (i + 1) in if w2c (at b j) == '}' then Right (JObj [], j + 1) else members b j []
  c | c == '-' || (c >= '0' && c <= '9') -> number b i
  _ -> Left ("unexpected input at byte " ++ show i)

elems :: B.ByteString -> Int -> [Json] -> Either String (Json, Int)
elems b i acc = do
  (v, j) <- value b (skip b i)
  let k = skip b j
  case w2c (at b k) of
    ',' -> elems b (k + 1) (v : acc)
    ']' -> Right (JArr (reverse (v : acc)), k + 1)
    _ -> Left ("in an array, expected , or ] at byte " ++ show k)

members :: B.ByteString -> Int -> [(String, Json)] -> Either String (Json, Int)
members b i acc = do
  let i' = skip b i
  if w2c (at b i') /= '"' then Left ("expected a key at byte " ++ show i') else do
    (k, j) <- key b (i' + 1)
    let j' = skip b j
    if w2c (at b j') /= ':' then Left ("expected : after a key at byte " ++ show j') else do
      (v, n) <- value b (skip b (j' + 1))
      let n' = skip b n
      case w2c (at b n') of
        ',' -> members b (n' + 1) ((k, v) : acc)
        '}' -> Right (JObj (reverse ((k, v) : acc)), n' + 1)
        _ -> Left ("in an object, expected , or } at byte " ++ show n')

-- | From just after the opening quote: the string's bytes, escapes undone, and the index just after the closing
-- quote. With no escape (nearly always) the bytes are a slice of the input, found by two scans of the machine's
-- (@memchr@); with some, the pieces between them are joined once, not a 'T.Text' to each piece and each escape.
rawString :: B.ByteString -> Int -> Either String (B.ByteString, Int)
rawString b i0 = case B.elemIndex 34 (B.drop i0 b) of
  Nothing -> Left "unterminated string"
  Just q | not (B.elem 92 (B.take q (B.drop i0 b))) -> Right (B.take q (B.drop i0 b), i0 + q + 1)
         | otherwise -> go i0 i0 []
  where
    slice a z = B.take (z - a) (B.drop a b)
    go a i acc = case B.findIndex (\w -> w == 34 || w == 92) (B.drop i b) of
      Nothing -> Left "unterminated string"
      Just k -> let j = i + k in case BU.unsafeIndex b j of
        34 -> Right (B.concat (reverse (slice a j : acc)), j + 1)
        _ -> case w2c (at b (j + 1)) of
          'u' -> case hex4 (j + 2) of
            Nothing -> Left "bad \\u escape"
            Just hi
              | hi >= 0xD800 && hi < 0xDC00, w2c (at b (j + 6)) == '\\', w2c (at b (j + 7)) == 'u', Just lo <- hex4 (j + 8), lo >= 0xDC00, lo < 0xE000 ->
                  let c = chr (0x10000 + ((hi - 0xD800) `shiftL` 10 .|. (lo - 0xDC00))) in go (j + 12) (j + 12) (utf8 c : slice a j : acc)
              | otherwise -> go (j + 6) (j + 6) (utf8 (chr hi) : slice a j : acc)
          c -> let r = case c of { 'n' -> '\n'; 't' -> '\t'; 'r' -> '\r'; 'b' -> '\b'; 'f' -> '\f'; x -> x }
               in go (j + 2) (j + 2) (utf8 r : slice a j : acc)
    utf8 = TE.encodeUtf8 . T.singleton       -- (a lone surrogate comes out as the replacement character, as it did)
    hex4 j | j + 4 > B.length b = Nothing
           | otherwise = foldl (\m w -> (\x d -> (x `shiftL` 4) .|. d) <$> m <*> hexDigit w) (Just 0) (B.unpack (B.take 4 (B.drop j b)))
    hexDigit w | w >= 48 && w <= 57 = Just (fromIntegral w - 48)
               | w >= 97 && w <= 102 = Just (fromIntegral w - 87)
               | w >= 65 && w <= 70 = Just (fromIntegral w - 55)
               | otherwise = Nothing :: Maybe Int

string :: B.ByteString -> Int -> Either String (T.Text, Int)
string b i = (\(s, j) -> (TE.decodeUtf8With TE.lenientDecode s, j)) <$> rawString b i

-- | A key as a 'String': straight from the bytes when they are ASCII (they are), without a 'T.Text' between.
key :: B.ByteString -> Int -> Either String (String, Int)
key b i = (\(s, j) -> (if B.all (< 128) s then BC.unpack s else T.unpack (TE.decodeUtf8With TE.lenientDecode s), j)) <$> rawString b i

number :: B.ByteString -> Int -> Either String (Json, Int)
number b i = case plain of
  Just d -> Right (JNum d, i + B.length tok)
  Nothing -> numberSlow b i
  where
    tok = B.takeWhile (\w -> (w >= 48 && w <= 57) || w == 45 || w == 43 || w == 46 || w == 101 || w == 69) (B.drop i b)
    -- (digits, a point and digits, an exponent: when the mantissa is a whole number a double holds (up to 2^53) and
    -- the power of ten is one too (up to 10^22), the one multiplication or division rounds as 'read' does -- and that
    -- is the shape of every number the programs write; any other goes the long way)
    plain =
      let (neg, d0) = case B.uncons tok of { Just (45, r) -> (True, r); _ -> (False, tok) }
          (ip, r) = B.span isDig d0
          (fp, r1) = case B.uncons r of { Just (46, f) -> B.span isDig f; _ -> (B.empty, r) }
          expo = case B.uncons r1 of
            Just (c, q) | c == 101 || c == 69 ->
              let (sg, q1) = case B.uncons q of { Just (45, x) -> (-1, x); Just (43, x) -> (1, x); _ -> (1, q) }
                  (es, q2) = B.span isDig q1
              in if B.null es || B.length es > 3 then Nothing else Just (sg * B.foldl' (\a w -> a * 10 + fromIntegral (w - 48)) 0 es, q2)
            _ -> Just (0 :: Int, r1)
          m = B.foldl' (\a w -> a * 10 + fromIntegral (w - 48)) (0 :: Int) (B.append ip fp)
      in case expo of
           Just (ex, rest) | not (B.null ip), B.null rest, B.length r == B.length r1 || not (B.null fp), B.length ip + B.length fp <= 18, m <= 9007199254740992
                           , let e10 = ex - B.length fp, abs e10 <= 22 ->
             let d = if e10 >= 0 then fromIntegral m * 10 ^ e10 else fromIntegral m / 10 ^ negate e10 :: Double
             in Just (if neg then negate d else d)
           _ -> Nothing
    isDig w = w >= 48 && w <= 57

numberSlow :: B.ByteString -> Int -> Either String (Json, Int)
numberSlow b i =
  let s = BC.unpack (B.takeWhile (\w -> let c = w2c w in (c >= '0' && c <= '9') || c `elem` ("-+.eE" :: String)) (B.drop i b))
      fix x = case break (== '.') x of      -- `read` wants a digit on both sides of the point, and no leading +
        (ip, '.' : r) | null (takeWhile (`elem` ['0' .. '9']) r) -> ip ++ ".0" ++ r
        (ip, []) | any (`elem` ("eE" :: String)) ip -> let (m, e) = break (`elem` ("eE" :: String)) ip in m ++ ".0" ++ filter (/= '+') e
        _ -> filter (/= '+') x
  in case reads (fix s) :: [(Double, String)] of
       [(n, "")] -> Right (JNum n, i + length s)
       _ -> Left ("bad number at byte " ++ show i)
