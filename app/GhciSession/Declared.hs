-- | A project's own tools, declared in the target's @"tools"@: a name, typed parameters and an expression with a
-- @{param}@ for each. A call is a substitution of Haskell LITERALS into that expression, and then an eval --
-- so what a model writes into an argument can never be more than a value of its declared type.
-- Pure: the validation of the declaration, and the substitution.
module GhciSession.Declared
  ( Declared (..), Param (..), parseDeclared, substitute, placeholders, builtinNames, validWritePaths, validBuiltin
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (intercalate, nub)
import Data.Maybe (isNothing)

import GhciSession.Json

data Param = Param { pName :: String, pType :: String, pDesc :: String } deriving (Eq, Show)

data Declared = Declared
  { dName :: String, dDesc :: String, dParams :: [Param], dRequired :: [String]
  , dExpr :: String, dTimeout :: Maybe Double } deriving (Eq, Show)

-- | The names of the tools the chat and the MCP server have (a declared tool may not take one of them; a test
-- keeps this list as the tools' own).
builtinNames :: [String]
builtinNames =
  [ "eval", "status", "typecheck", "reload", "hold", "release", "test", "doc", "census", "bench", "mem", "view", "zoom", "date"
  , "history", "recall", "remember", "vfs", "restart", "read", "grep", "find", "write", "edit", "edits", "ls", "sh", "spawn", "tell" ]

-- | The tools a target declares (@"tools"@: null or absent, none), or why the declaration is refused.
parseDeclared :: Json -> Either String [Declared]
parseDeclared JNull = Right []
parseDeclared (JArr ts) = do
  ds <- mapM (\(i, t) -> one i t) (zip [0 :: Int ..] ts)
  let names = map dName ds
  case [ n | (n, k) <- zip names [0 :: Int ..], n `elem` take k names ] of
    n : _ -> Left ("tool " ++ show n ++ " is declared twice")
    [] -> Right ds
parseDeclared _ = Left "\"tools\" is a list of {name, description, params, required, expr}"

one :: Int -> Json -> Either String Declared
one i t = case t of
  JObj _ -> do
    name <- maybe (Left (at ++ "has no \"name\"")) Right (lookupStr "name" t)
    let here = "tool " ++ show name ++ ": "
    if not (identOk "-" name) then Left (here ++ "a name is letters, digits, _ and - (and starts with a letter)")
      else if name `elem` builtinNames then Left (here ++ "that is one of the built-in tools' names")
      else do
        expr <- case lookupStr "expr" t of
          Just e | any (/= ' ') e -> Right e
          _ -> Left (here ++ "no \"expr\" (the expression a call evaluates, with a {param} for each parameter)")
        params <- mapM (param here) (lookupObj "params" t)
        let declared = map pName params
            required = strs (t .: "required")
        case [ r | r <- required, r `notElem` declared ] of
          r : _ -> Left (here ++ "required parameter " ++ show r ++ " is not declared in \"params\"")
          [] -> pure ()
        case [ h | h <- placeholders expr, h `notElem` declared ] of
          h : _ -> Left (here ++ "{" ++ h ++ "} in \"expr\" is not a declared parameter (declared: " ++ (if null declared then "none" else intercalate ", " declared) ++ ")")
          [] -> pure ()
        tmo <- case t .: "timeout" of
          JNull -> Right Nothing
          JNum s | s > 0 -> Right (Just s)
          _ -> Left (here ++ "\"timeout\" is a number of seconds, above 0")
        Right Declared { dName = name, dDesc = maybe "" id (lookupStr "description" t), dParams = params, dRequired = required, dExpr = expr, dTimeout = tmo }
  _ -> Left (at ++ "is an object {name, description, params, required, expr}")
  where
    at = "tools[" ++ show i ++ "] "
    param here (k, p)
      | not (identOk "" k) = Left (here ++ "parameter " ++ show k ++ ": a name is letters, digits and _ (and starts with a letter), to be written {" ++ k ++ "}")
      | otherwise = case lookupStr "type" p of
          Just ty | ty `elem` ["string", "number", "integer", "boolean"] -> Right (Param k ty (maybe "" id (lookupStr "description" p)))
          Just ty -> Left (here ++ "parameter " ++ show k ++ " has type " ++ show ty ++ "; a type is string, number, integer or boolean")
          Nothing -> Left (here ++ "parameter " ++ show k ++ " has no \"type\" (string, number, integer or boolean)")

-- | A name: a letter first, then letters, digits, _ and the extra characters.
identOk :: String -> String -> Bool
identOk extra s = case s of
  c : r -> (isAsciiLower c || isAsciiUpper c || c == '_') && all (\x -> isAsciiLower x || isAsciiUpper x || isDigit x || x == '_' || x `elem` extra) r
  [] -> False

data Seg = Lit String | Hole String

-- | An expression as text and holes: a hole is a name in braces.
segments :: String -> [Seg]
segments = go ""
  where
    go acc [] = lit acc
    go acc ('{' : r) | (n, '}' : rest) <- span (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '_') r, identOk "" n = lit acc ++ [Hole n] ++ go "" rest
    go acc (c : r) = go (c : acc) r
    lit "" = []
    lit acc = [Lit (reverse acc)]

-- | The names written as {name} in an expression.
placeholders :: String -> [String]
placeholders e = nub [ n | Hole n <- segments e ]

-- | The expression of a call: each {param} a Haskell literal of its declared type -- a string as @show@ writes it
-- (nothing in it can end the quotes), a number or an integer checked (and in parentheses when negative), a boolean
-- True or False; an optional parameter not given is @Nothing@, one given is @(Just literal)@.
substitute :: Declared -> Json -> Either String String
substitute d args = do
  given <- case args of
    JObj kvs -> Right [ (k, v) | (k, v) <- kvs, v /= JNull ]
    JNull -> Right []
    _ -> Left "the arguments are an object"
  let names = map pName (dParams d)
      takes = if null names then "no parameters" else "it takes " ++ intercalate ", " names
  case [ k | (k, _) <- given, k `notElem` names ] of
    [] -> Right ()
    unknown -> Left ("unknown parameter(s) " ++ intercalate ", " unknown ++ "; " ++ takes)
  case [ r | r <- dRequired d, isNothing (lookup r given) ] of
    [] -> Right ()
    missing -> Left ("missing required parameter(s) " ++ intercalate ", " missing ++ "; " ++ takes)
  lits <- mapM (\p -> case lookup (pName p) given of
                        Nothing -> Right (pName p, "Nothing")
                        Just v -> (\l -> (pName p, if pName p `elem` dRequired d then l else "(Just " ++ l ++ ")")) <$> literal p v) (dParams d)
  Right (concatMap (\s -> case s of { Lit x -> x; Hole n -> maybe "" id (lookup n lits) }) (segments (dExpr d)))

literal :: Param -> Json -> Either String String
literal p v = case (pType p, v) of
  ("string", JStr s) -> Right (show s)
  ("boolean", JBool b) -> Right (if b then "True" else "False")
  ("integer", JNum x) | x == fromIntegral (round x :: Integer), abs x < 9.0e18 -> Right (neg x (show (round x :: Integer)))
                      | otherwise -> bad "an integer"
  ("number", JNum x) | isNaN x || isInfinite x -> bad "a finite number"
                     | abs x < 1.0e15, x == fromIntegral (round x :: Integer) -> Right (neg x (show (round x :: Integer)))
                     | otherwise -> Right (neg x (show x))
  (ty, _) -> bad (if ty == "integer" then "an integer" else "a " ++ ty)
  where
    bad what = Left ("parameter " ++ pName p ++ " is " ++ what ++ ": " ++ take 60 (encode v) ++ " is not")
    neg x s = if x < 0 then "(" ++ s ++ ")" else s

-- | Whether the @"write_paths"@ of a target are fit: relative to the project, none escaping it.
validWritePaths :: Json -> Either String [FilePath]
validWritePaths JNull = Right []
validWritePaths j = case j of
  JArr ps | all isStr ps -> case [ p | p <- strs j, bad p ] of
    p : _ -> Left ("\"write_paths\": " ++ show p ++ " is not a path relative to the project and inside it")
    [] -> Right (strs j)
  _ -> Left "\"write_paths\" is a list of directories or files, relative to the project"
  where
    isStr x = case x of { JStr _ -> True; _ -> False }
    bad p = null p || head p == '/' || ".." `elem` splitOn p
    splitOn s = case break (== '/') s of { (a, _ : r) -> a : splitOn r; (a, []) -> [a] }

-- | Whether the @"builtin_tools"@ of a target name tools there are.
validBuiltin :: Json -> Either String [String]
validBuiltin JNull = Right []
validBuiltin j = case j of
  JArr _ -> case [ n | n <- strs j, n `notElem` builtinNames ] of
    n : _ -> Left ("\"builtin_tools\": " ++ show n ++ " is not a built-in tool (they are " ++ unwords builtinNames ++ ")")
    [] -> Right (strs j)
  _ -> Left "\"builtin_tools\" is a list of the built-in tools' names"
