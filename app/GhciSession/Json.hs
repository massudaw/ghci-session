-- | A small JSON value: enough for the configuration file, the status file and the socket protocol,
-- without a dependency outside GHC's boot packages. Objects keep their key order.
module GhciSession.Json
  ( Json (..)
  , parseJson, encode, encodePretty
  , (.:), str, num, bool, arr, obj, strs, lookupStr, lookupBool, lookupNum, lookupArr, lookupObj
  , set, setDefault
  ) where

import Data.Char (chr, isDigit, isHexDigit, isSpace, ord)
import Data.List (intercalate)
import Numeric (readHex, showHex)

data Json = JNull | JBool Bool | JNum Double | JStr String | JArr [Json] | JObj [(String, Json)]
  deriving (Eq, Show)

-- | A member of an object, 'JNull' when absent or when the value is not an object.
(.:) :: Json -> String -> Json
JObj kvs .: k = maybe JNull id (lookup k kvs)
_ .: _ = JNull

str :: Json -> Maybe String
str (JStr s) = Just s
str _ = Nothing

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
strs (JArr xs) = [ s | JStr s <- xs ]
strs (JStr s) = [s]
strs _ = []

lookupStr :: String -> Json -> Maybe String
lookupStr k j = str (j .: k)

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

-- ---------------------------------------------------------------------------

encode :: Json -> String
encode j = go j ""
  where
    go JNull = showString "null"
    go (JBool b) = showString (if b then "true" else "false")
    go (JNum n) = showString (showNum n)
    go (JStr s) = showStr s
    go (JArr xs) = showChar '[' . commas (map go xs) . showChar ']'
    go (JObj kvs) = showChar '{' . commas [ showStr k . showString ": " . go v | (k, v) <- kvs ] . showChar '}'
    commas [] = id
    commas (x : xs) = x . foldr (\y r -> showString ", " . y . r) id xs

-- | One member per line at each level: what a person reads in @status.json@.
encodePretty :: Json -> String
encodePretty = go 0
  where
    go _ JNull = "null"
    go _ (JBool b) = if b then "true" else "false"
    go _ (JNum n) = showNum n
    go _ (JStr s) = showStr s ""
    go _ (JArr []) = "[]"
    go _ (JObj []) = "{}"
    go d (JArr xs) = "[\n" ++ intercalate ",\n" [ pad (d + 1) ++ go (d + 1) x | x <- xs ] ++ "\n" ++ pad d ++ "]"
    go d (JObj kvs) = "{\n" ++ intercalate ",\n" [ pad (d + 1) ++ showStr k "" ++ ": " ++ go (d + 1) v | (k, v) <- kvs ] ++ "\n" ++ pad d ++ "}"
    pad d = replicate d ' '

showNum :: Double -> String
showNum n
  | isNaN n || isInfinite n = "null"
  | n == fromIntegral (round n :: Integer) && abs n < 1e15 = show (round n :: Integer)
  | otherwise = show n

showStr :: String -> ShowS
showStr s = showChar '"' . foldr (\c r -> esc c . r) id s . showChar '"'
  where
    esc '"' = showString "\\\""
    esc '\\' = showString "\\\\"
    esc '\n' = showString "\\n"
    esc '\r' = showString "\\r"
    esc '\t' = showString "\\t"
    esc c
      | ord c < 0x20 = showString "\\u" . showString (replicate (4 - length h) '0') . showString h
      | otherwise = showChar c
      where h = showHex (ord c) ""

-- ---------------------------------------------------------------------------

-- | Parse one JSON value (trailing space allowed); 'Left' says where it went wrong.
parseJson :: String -> Either String Json
parseJson s = case value (skip s) of
  Right (v, rest) | all isSpace rest -> Right v
                  | otherwise -> Left ("trailing text: " ++ take 20 rest)
  Left e -> Left e

skip :: String -> String
skip = dropWhile isSpace

value :: String -> Either String (Json, String)
value ('n' : 'u' : 'l' : 'l' : r) = Right (JNull, r)
value ('t' : 'r' : 'u' : 'e' : r) = Right (JBool True, r)
value ('f' : 'a' : 'l' : 's' : 'e' : r) = Right (JBool False, r)
value ('"' : r) = do { (s, r') <- string r; Right (JStr s, r') }
value ('[' : r) = case skip r of
  ']' : r' -> Right (JArr [], r')
  r' -> elems r' []
value ('{' : r) = case skip r of
  '}' : r' -> Right (JObj [], r')
  r' -> members r' []
value s@(c : _) | c == '-' || isDigit c = number s
value s = Left ("unexpected: " ++ take 20 s)

elems :: String -> [Json] -> Either String (Json, String)
elems s acc = do
  (v, r) <- value (skip s)
  case skip r of
    ',' : r' -> elems r' (v : acc)
    ']' : r' -> Right (JArr (reverse (v : acc)), r')
    r' -> Left ("in an array, expected , or ]: " ++ take 20 r')

members :: String -> [(String, Json)] -> Either String (Json, String)
members s acc = case skip s of
  '"' : r -> do
    (k, r1) <- string r
    case skip r1 of
      ':' : r2 -> do
        (v, r3) <- value (skip r2)
        case skip r3 of
          ',' : r4 -> members r4 ((k, v) : acc)
          '}' : r4 -> Right (JObj (reverse ((k, v) : acc)), r4)
          r4 -> Left ("in an object, expected , or }: " ++ take 20 r4)
      r2 -> Left ("expected : after a key: " ++ take 20 r2)
  r -> Left ("expected a key: " ++ take 20 r)

string :: String -> Either String (String, String)
string = go []
  where
    go acc ('"' : r) = Right (reverse acc, r)
    go acc ('\\' : c : r) = case c of
      'n' -> go ('\n' : acc) r
      't' -> go ('\t' : acc) r
      'r' -> go ('\r' : acc) r
      'b' -> go ('\b' : acc) r
      'f' -> go ('\f' : acc) r
      'u' -> case splitAt 4 r of
        (h, r') | length h == 4, all isHexDigit h -> go (chr (fst (head (readHex h))) : acc) r'
        _ -> Left "bad \\u escape"
      _ -> go (c : acc) r
    go acc (c : r) = go (c : acc) r
    go _ [] = Left "unterminated string"

number :: String -> Either String (Json, String)
number s =
  let (sign, r0) = case s of { '-' : r -> (-1, r); _ -> (1, s) }
      (ip, r1) = span isDigit r0
      (fp, r2) = case r1 of { '.' : r -> let (d, r') = span isDigit r in (d, r'); _ -> ("", r1) }
      (ex, r3) = case r2 of
        e : r | e == 'e' || e == 'E' ->
          let (sg, r') = case r of { '-' : x -> ("-", x); '+' : x -> ("", x); _ -> ("", r) }
              (d, r'') = span isDigit r'
          in (sg ++ d, r'')
        _ -> ("", r2)
  in if null ip then Left ("bad number: " ++ take 20 s)
     else Right (JNum (sign * read (ip ++ "." ++ (if null fp then "0" else fp) ++ (if null ex then "" else "e" ++ ex))), r3)
