{-# OPTIONS_GHC -O2 #-}  -- the scorer runs over every definition of the session for every query
-- | __Find a definition, and what is written about it__: `ghci-session doc QUERY`.
--
-- The index is read from the session's own sources -- every watched Haskell file -- by a scanner of top-level
-- declarations, not asked of the compiler: so it has what is NOT exported, it has the documentation without
-- compiling anything with @-haddock@, and it has a module that does not compile at the moment (often the one
-- being looked for). An entry is one declaration: its name, its signature as written, the comment above it,
-- and where it is.
--
-- The search is fuzzy on the name -- exact, then prefix, then the initials of a camelCase name, then a
-- substring, then a subsequence, then a near miss (a typo) -- and literal on everything else, so a word that
-- is not a name still finds the definitions whose type or documentation says it.
module GhciSession.Doc (Entry (..), indexFile, search, render, entryJson) where

import Data.Char (isAlphaNum, isLower, isSpace, isUpper, toLower)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Text as T

import GhciSession.Json

data Entry = Entry
  { eModule :: !T.Text, eName :: !T.Text, eKind :: !T.Text
  , eSig :: !T.Text       -- ^ the declaration as written, on one line (for a function with no signature: empty)
  , eDoc :: !T.Text       -- ^ the comment above it, as written
  , eFile :: !FilePath, eLine :: !Int
  , eLower :: !T.Text     -- ^ the name in lower case (what most of the matching reads)
  }

-- | Every top-level declaration of one source file.
indexFile :: FilePath -> T.Text -> [Entry]
indexFile file src = go (zip [1 ..] ls) [] Nothing []
  where
    ls = T.lines src
    modName = case mapMaybe moduleOf ls of { (m : _) -> m; [] -> T.pack file }
    moduleOf l = case T.words l of
      (w : m : _) | w == T.pack "module" -> Just (T.takeWhile (\c -> isAlphaNum c || c == '.' || c == '_' || c == '\'') m)
      _ -> Nothing
    mk name kind sig doc n = Entry modName name kind (oneLine sig) (T.intercalate (T.pack "\n") (reverse doc)) file n (T.toLower name)
    -- @doc@: the comment lines directly above, newest first. @open@: the block comment we are inside, if any.
    -- @seen@: names already given (a function's later clauses are not new definitions).
    go [] _ _ _ = []
    go ((n, l) : rest) doc open seen
      | Just acc <- open =
          if T.pack "-}" `T.isInfixOf` l then go rest (dropEnd (T.strip (fst (T.breakOn (T.pack "-}") l))) acc) Nothing seen
          else go rest (l : acc) open seen
      | T.pack "{-|" `T.isPrefixOf` l || T.pack "{- |" `T.isPrefixOf` l =
          let body = T.strip (T.drop 1 (T.dropWhile (/= '|') l))
          in if T.pack "-}" `T.isInfixOf` l then go rest [T.strip (fst (T.breakOn (T.pack "-}") body))] Nothing seen
             else go rest [] (Just [ body | not (T.null body) ]) seen
      | T.pack "-- |" `T.isPrefixOf` l = go rest [T.strip (T.drop 4 l)] Nothing seen
      | T.pack "--" `T.isPrefixOf` l = go rest (if null doc then [] else T.strip (T.dropWhile (== '-') l) : doc) Nothing seen
      | T.null (T.strip l) = go rest doc Nothing seen                     -- (a blank line between a comment and its declaration is allowed)
      | isSpace (T.head l) || T.head l == '#' || T.head l == '{' = go rest [] Nothing seen     -- a continuation, CPP, a pragma
      | otherwise =
          let (cont, rest') = span (\(_, c) -> not (T.null c) && isSpace (T.head c)) rest
              whole = T.unwords (l : map snd cont)
              ws = T.words l
              keyword k = take 1 ws == [T.pack k]
              declName = case dropWhile (`elem` map T.pack ["data", "newtype", "type", "class", "family", "instance", "pattern", "foreign", "import", "ccall", "capi", "safe", "unsafe", "interruptible"]) ws of
                (w : _) | T.isPrefixOf (T.pack "\"") w -> T.empty
                        | otherwise -> T.takeWhile (\c -> isAlphaNum c || c `elem` "_'") (T.dropWhile (== '(') w)
                [] -> T.empty
              found
                | any keyword ["import", "module", "infixl", "infixr", "infix", "deriving", "default", "instance", "where"] = []
                | any keyword ["data", "newtype", "type", "class", "pattern"] =
                    let owner = classHead declName ws
                    in [ mk owner (head ws) whole doc n | not (T.null declName) ]
                       -- a record's fields are definitions too: `name :: T` inside the braces
                       ++ [ mk f (T.pack "field") (f <> T.pack " :: " <> ty) [T.pack "A field of " <> owner <> T.pack "."] n
                          | any keyword ["data", "newtype"], (f, ty) <- fields whole ]
                | keyword "foreign" = [ mk nm (T.pack "foreign") whole doc n | nm <- take 1 (sigNames (T.unwords (drop 1 (dropWhile (not . T.isPrefixOf (T.pack "\"")) ws)))) ]
                | not (null (sigNames l)) = [ mk nm (T.pack "sig") whole doc n | nm <- sigNames l ]
                | otherwise = [ mk nm (T.pack "def") T.empty doc n | nm <- take 1 (defName l), nm `notElem` seen ]
          in found ++ go rest' [] Nothing (map eName found ++ seen)
    dropEnd x acc = if T.null x then acc else x : acc
    -- the fields of a record declaration (on one line by now): each `name :: type` after the first brace
    fields d = case T.breakOn (T.pack "{") d of
      (_, r) | not (T.null r) ->
        [ (f, T.strip ty)
        | piece <- splitTop (T.takeWhile (/= '}') (T.drop 1 r))
        , let (lhs, rhs) = T.breakOn (T.pack "::") piece, not (T.null rhs)
        , let ty = T.strip (fst (T.breakOn (T.pack "--") (T.drop 2 rhs)))
        , f <- map T.strip (T.splitOn (T.pack ",") (lastField lhs)), not (T.null f), T.all (\c -> isAlphaNum c || c `elem` "_'") f ]
      _ -> []
    -- (a comment before a field is part of the piece: the name is what follows the last of it)
    lastField lhs = case reverse (T.words lhs) of { (w : _) -> w; [] -> T.empty }
    -- split on the commas that separate fields, not those inside a type's parentheses or brackets
    splitTop t = go0 (0 :: Int) T.empty (T.unpack t)
      where go0 _ acc [] = [acc | not (T.null (T.strip acc))]
            go0 d acc (c : cs)
              | c `elem` "([" = go0 (d + 1) (T.snoc acc c) cs
              | c `elem` ")]" = go0 (d - 1) (T.snoc acc c) cs
              | c == ',' && d == 0 && T.pack "::" `T.isInfixOf` acc = acc : go0 d T.empty cs
              | otherwise = go0 d (T.snoc acc c) cs
    -- a class or type head may have a context first: @class (Eq a) => Foo a@
    classHead d ws = case break (== T.pack "=>") ws of
      (_, _ : (w : _)) | take 1 ws `elem` [[T.pack "class"], [T.pack "data"], [T.pack "newtype"]] -> T.takeWhile (\c -> isAlphaNum c || c `elem` "_'") w
      _ -> d
    -- @a, b :: T@ and @(<+>) :: T@
    sigNames l = case T.breakOn (T.pack "::") l of
      (lhs, r) | not (T.null r), not (T.null (T.strip lhs)), T.all (\c -> isAlphaNum c || c `elem` "_', ()!#$%&*+./<=>?@\\^|-~:") lhs
               , T.pack "=" `notElem` T.words lhs ->
                   let names = filter (not . T.null) (map (T.filter (`notElem` "()") . T.strip) (T.splitOn (T.pack ",") lhs))
                   in if all (\x -> length (T.words x) == 1) names then names else []
      _ -> []
    defName l = case T.words l of
      -- (`a <+> b = ...` defines the operator, not `a`)
      (_ : op : _) | op `notElem` [T.pack "=", T.pack "|"], T.isPrefixOf (T.pack "`") op || T.all (`elem` "!#$%&*+./<=>?@\\^|-~:") op -> []
      (w : _) | Just (c, _) <- T.uncons w, isLower c || c == '_' -> [T.takeWhile (\x -> isAlphaNum x || x `elem` "_'") w]
      _ -> []

oneLine :: T.Text -> T.Text
oneLine = T.unwords . T.words

-- | The entries a query finds, best first. Every word of the query must be found in an entry -- as (or in)
-- its name, its module, or literally in its signature or documentation -- and the name counts for most.
search :: [Entry] -> [T.Text] -> [(Int, Entry)]
search es ws0 = sortOn (\(sc, e) -> (Down sc, T.length (eName e), eModule e)) (mapMaybe one es)
  where
    ws = [ (w, T.toLower w) | w <- ws0, not (T.null w) ]
    one e = do
      scores <- mapM (word e) ws
      let sc = sum scores
      if null ws || sc <= 0 then Nothing else Just (sc, e)
    word e (w, lw)
      | Just (q, n) <- qualified w =                       -- Mod.name: the module must end with (or be) Mod
          if T.toLower q `T.isSuffixOf` T.toLower (eModule e) then (+ 50) <$> nameScore e n (T.toLower n) else Nothing
      | otherwise = case nameScore e w lw of
          Just s -> Just s
          Nothing | lw `T.isInfixOf` T.toLower (eModule e) -> Just 60
                  | lw `T.isInfixOf` T.toLower (eSig e) -> Just 40
                  | lw `T.isInfixOf` T.toLower (eDoc e) -> Just 20
                  | otherwise -> Nothing
    qualified w = case T.breakOnEnd (T.pack ".") w of
      (q, n) | not (T.null q), not (T.null n), isUpper (T.head q), T.all (\c -> isAlphaNum c || c `elem` "_'") n -> Just (T.dropEnd 1 q, n)
      _ -> Nothing

-- | How well a word names an entry, or 'Nothing'.
nameScore :: Entry -> T.Text -> T.Text -> Maybe Int
nameScore e w lw
  | w == name = Just 1000
  | lw == lname = Just 900
  | lw `T.isPrefixOf` lname = Just (700 - min 100 (T.length name - T.length w))
  | T.length lw >= 2 && lw == humps = Just 650
  | T.length lw >= 2 && lw `T.isPrefixOf` humps = Just 560
  | lw `T.isInfixOf` lname = Just (500 - min 100 (T.length name - T.length w))
  | T.length lw >= 3, Just gaps <- subsequence lw lname = Just (max 150 (380 - 12 * gaps))
  | T.length lw >= 4, abs (T.length lw - T.length lname) <= 2, d <- editDistance lw lname, d <= (if T.length lw >= 8 then 2 else 1) = Just (330 - 40 * d)
  | otherwise = Nothing
  where
    name = eName e
    lname = eLower e
    -- quickDivergences -> qd ; unlink_cafs -> uc
    humps = T.toLower (T.pack (go True (T.unpack name)))
      where go _ [] = []
            go start (c : cs) | c == '_' = go True cs
                              | start || isUpper c = c : go False cs
                              | otherwise = go False cs

-- | Is @q@ a subsequence of @t@? The number of characters skipped between its first and last match.
subsequence :: T.Text -> T.Text -> Maybe Int
subsequence q t = go (T.unpack q) (T.unpack t) (0 :: Int) False
  where
    go [] _ gaps _ = Just gaps
    go _ [] _ _ = Nothing
    go qq@(a : as) (b : bs) gaps started
      | a == b = go as bs gaps True
      | otherwise = go qq bs (if started then gaps + 1 else gaps) started

-- | Levenshtein distance (the names are short).
editDistance :: T.Text -> T.Text -> Int
editDistance a b = last (foldl row [0 .. T.length a] (T.unpack b))
  where
    row prev@(p : ps) c = scanl step (p + 1) (zip3 (T.unpack a) prev ps)
      where step left (x, diag, up) = minimum [left + 1, up + 1, diag + (if x == c then 0 else 1)]
    row [] _ = []

-- | An entry for a person: the declaration, its documentation (at most @docLines@ of it), where it is.
render :: Int -> Entry -> T.Text
render docLines e = T.unlines $
  [ if eKind e `elem` map T.pack ["sig", "field", "foreign"] then eModule e <> T.pack "." <> cut (eSig e)
    else if T.null (eSig e) then eModule e <> T.pack "." <> eName e <> T.pack "   (no signature)"
    else cut (eSig e) <> T.pack "   -- " <> eModule e ]
  ++ [ T.pack "    " <> l | l <- shown ]
  ++ [ T.pack "    ..." | length docs > docLines ]
  ++ [ T.pack "    " <> T.pack (eFile e) <> T.pack ":" <> T.pack (show (eLine e)) ]
  where docs = if T.null (eDoc e) then [] else T.lines (eDoc e)
        cut t = if T.length t > 220 then T.take 217 t <> T.pack "..." else t
        shown = take docLines docs

entryJson :: Int -> Entry -> Json
entryJson sc e = JObj
  [ ("module", JText (eModule e)), ("name", JText (eName e)), ("kind", JText (eKind e)), ("signature", JText (eSig e))
  , ("doc", JText (eDoc e)), ("file", JStr (eFile e)), ("line", JNum (fromIntegral (eLine e))), ("score", JNum (fromIntegral sc)) ]
