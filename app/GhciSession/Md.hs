-- | __What an agent writes, and what a tool answers, as a screen shows them__: markdown as styled lines, a
-- unified diff in its colors. For the chat's screen and the monitor's history ("GhciSession.ChatTui",
-- "GhciSession.Top"), which showed both as they are typed -- asterisks, pipes, and a diff all in one color.
--
-- Markdown, as much of it as an agent's replies have: headings, lists, quotes, rules, fenced code, tables;
-- and in a line, @**bold**@, @*italic*@, @`code`@, @~~struck~~@, a link, a bare address (not the underscore
-- forms: a reply about code is full of names with underscores). A line is a line:
-- nothing is re-flowed (the caller wraps what is too wide), and what does not parse is shown as it was written.
--
-- A diff is recognised by its head -- a @---@ line and then a @+++@ line, or a hunk's @\@\@ -@ -- and lasts while lines look like
-- one; so the answer of a tool that wrote a file is its words, then the change in green and red.
module GhciSession.Md
  ( Span, mdLines, outputLines, inline, diffLine, imageLine, imageOf
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate, isPrefixOf)

import qualified Data.Text as T

import qualified GhciSession.Image as Img
import Tui

type Span = (Style, String)

green, red, cyan, yellow, under :: Style -> Style
green = withFg (Ansi 2)
red = withFg (Ansi 1)
cyan = withFg (Ansi 6)
yellow = withFg (Ansi 3)
under s = s { sUnderline = True }

-- | A text of markdown as lines of spans, for @w@ columns (a table is laid out when it fits them, a rule is as
-- wide as they allow).
mdLines :: Int -> String -> [[Span]]
mdLines w = go . lines . filter (/= '\r')
  where
    go [] = []
    go (l : rest)
      | Just n <- Img.isMarker (T.pack l) = imageLine n : go rest
      | Just lang <- fence l =
          let (body, after) = break (\x -> fence x /= Nothing) rest
              shown = if lang == "diff" || looksDiff body then map diffLine body else map (\x -> [(dim plain, "│ "), (yellow plain, x)]) body
          in shown ++ go (drop 1 after)
      | Just (n, t) <- heading l = [inline (if n <= 2 then under (bold plain) else bold plain) t] ++ go rest
      | rule l = [[(dim plain, replicate (max 3 (min w 60)) '─')]] ++ go rest
      | Just t <- quote l = ((dim plain, "▌ ") : inline (dim plain) t) : go rest
      | Just (ind, mark, t) <- item l = ((plain, ind ++ mark) : inline plain t) : go rest
      | isRow l, (bar : more) <- rest, isBar bar =
          let (rows, after) = span isRow more
          in table (map cells (l : rows)) (l : bar : rows) ++ go after
      | otherwise = inline plain l : go rest

    fence l = let t = dropWhile isSpace l in if "```" `isPrefixOf` t then Just (trim (dropWhile (== '`') t)) else Nothing
    looksDiff b = case b of { (a : c : _) -> "--- " `isPrefixOf` a && "+++ " `isPrefixOf` c; _ -> False }
    heading l = let (hs, r) = span (== '#') l in if not (null hs) && length hs <= 6 && take 1 r == " " then Just (length hs, trim r) else Nothing
    rule l = let t = trim l in length t >= 3 && (all (== '-') t || all (== '*') t || all (== '_') t)
    quote l = case dropWhile isSpace l of { ('>' : r) -> Just (dropWhile (== ' ') r); _ -> Nothing }
    item l =
      let (ind, r) = span isSpace l
      in case r of
           (c : ' ' : t) | c `elem` "-*+" -> Just (ind, "• ", t)
           _ -> let (ds, r') = span isDigit r
                in case r' of
                     (c : ' ' : t) | not (null ds), c `elem` ".)" -> Just (ind, ds ++ ". ", t)
                     _ -> Nothing
    isRow l = '|' `elem` l && not (null (trim l))
    isBar l = let t = filter (not . isSpace) l in '-' `elem` t && all (`elem` "|:-") t
    cells l = let t = trim l
                  inner = (if take 1 t == "|" then drop 1 else id) (if lastIs '|' t then init t else t)
              in map trim (splitOn '|' inner)
    lastIs c s = not (null s) && last s == c
    -- a table: its columns as wide as their widest cell, the head bold, a line under it. One that does not fit
    -- the width is shown as it was written (cut cells are worse than pipes).
    table rows raw =
      let n = maximum (map length rows)
          rendered = [ [ inline (if k == 0 then bold plain else plain) c | c <- r ++ replicate (n - length r) "" ] | (k, r) <- zip [0 :: Int ..] rows ]
          widths = [ maximum (1 : [ visible (r !! c) | r <- rendered ]) | c <- [0 .. n - 1] ]
          total = sum widths + 3 * n + 1
          row r = (dim plain, "│ ") : intercalate [(dim plain, " │ ")] [ c ++ [(plain, replicate (wd - visible c) ' ')] | (c, wd) <- zip r widths ] ++ [(dim plain, " │")]
          line = [(dim plain, "├─" ++ intercalate "─┼─" [ replicate wd '─' | wd <- widths ] ++ "─┤")]
      in if n == 0 || total > w then map (inline plain) raw
         else case rendered of
                (h : body) -> row h : line : map row body
                [] -> []
    visible = sum . map (putWidth . snd)

-- | The line that stands for an image ("GhciSession.Image"): the screen is cells, so its name is what is shown.
imageLine :: String -> [Span]
imageLine n = [(withFg (Ansi 5) plain, "▣ image "), (dim plain, n)]

-- | Is this line the one that stands for an image? Its name.
imageOf :: [Span] -> Maybe String
imageOf l = case l of
  [(_, "\9635 image "), (_, n)] -> Just n
  _ -> Nothing

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : r) -> a : splitOn c r
  (a, []) -> [a]

-- | A line's marks as spans, over a style the line has already (a heading's bold, a quote's dim).
inline :: Style -> String -> [Span]
inline base = merge . go
  where
    go [] = []
    go ('\\' : c : r) | c `elem` "*_`~[]\\#" = (base, [c]) : go r
    go ('`' : r) | (code, '`' : after) <- break (== '`') r, not (null code) = (yellow base, code) : go after
    go ('*' : '*' : r) | Just (x, after) <- upTo "**" r = inline (bold base) x ++ go after
    go ('~' : '~' : r) | Just (x, after) <- upTo "~~" r = inline (base { sStrike = True }) x ++ go after
    go ('*' : r) | startsWord r, Just (x, after) <- upTo "*" r, endsWord x = inline (base { sItalic = True }) x ++ go after
    go ('[' : r) | (text, ']' : '(' : r') <- break (== ']') r, (url, ')' : after) <- break (== ')') r', not (null text), not (any isSpace url) =
                     inline (under base) text ++ [ (dim base, " (" ++ url ++ ")") | url /= text, not (null url) ] ++ go after
    go s | Just (url, after) <- address s = (under base, url) : go after
    go (c : r) = (base, [c]) : go r

    -- the text up to a closing mark, which must have something before it
    upTo mark s = find' [] s
      where find' acc t | mark `isPrefixOf` t = if null acc then Nothing else Just (reverse acc, drop (length mark) t)
            find' _ [] = Nothing
            find' acc (c : t) = find' (c : acc) t
    startsWord r = case r of { (c : _) -> not (isSpace c); [] -> False }
    endsWord x = not (null x) && not (isSpace (last x))
    address s
      | any (`isPrefixOf` s) ["http://", "https://"] = let (u, after) = span (\c -> not (isSpace c) && c `notElem` "<>\"") s
                                                           -- (a sentence's last mark is not the address's)
                                                           (u', back) = spanEnd (`elem` ".,;:)") u
                                                       in if length u' > 8 then Just (u', back ++ after) else Nothing
      | otherwise = Nothing
    spanEnd p s = let (a, b) = span p (reverse s) in (reverse b, reverse a)
    merge ((s1, a) : (s2, b) : r) | s1 == s2 = merge ((s1, a ++ b) : r)
    merge (x : r) = x : merge r
    merge [] = []

-- | What a tool answered, as lines: a unified diff in its colors wherever one is, the rest as it is.
outputLines :: String -> [[Span]]
outputLines = go False . lines . filter (/= '\r')
  where
    go _ [] = []
    go False (a : b : rest) | "--- " `isPrefixOf` a, "+++ " `isPrefixOf` b = diffLine a : diffLine b : go True rest
    go False (l : rest) | "diff --git " `isPrefixOf` l = diffLine l : go True rest
    -- (a hunk with no file's head before it: what a save is logged with)
    go False (l : rest) | "@@ -" `isPrefixOf` l = diffLine l : go True rest
    go False (l : rest) | Just n <- Img.isMarker (T.pack l) = imageLine n : go False rest
    go False (l : rest) = [(plain, l)] : go False rest
    go True (l : rest) | inDiff l = diffLine l : go True rest
                       | otherwise = go False (l : rest)
    inDiff l = case l of
      [] -> False
      (c : _) -> c `elem` "+- \\" || any (`isPrefixOf` l) ["@@", "diff --git ", "index ", "new file", "deleted file", "similarity ", "rename "]

-- | A line of a diff, in its color: a line added, a line removed, a hunk's head, a file's.
diffLine :: String -> [Span]
diffLine l
  | "+++ " `isPrefixOf` l || "--- " `isPrefixOf` l = [(bold plain, l)]
  | "@@" `isPrefixOf` l = [(cyan plain, l)]
  | "+" `isPrefixOf` l = [(green plain, l)]
  | "-" `isPrefixOf` l = [(red plain, l)]
  | any (`isPrefixOf` l) ["diff --git ", "index ", "new file", "deleted file", "similarity ", "rename ", "\\"] = [(dim plain, l)]
  | otherwise = [(plain, l)]
