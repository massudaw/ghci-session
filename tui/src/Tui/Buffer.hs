-- | A frame: the screen as cells, built from what to put where, and the bytes that turn the last frame
-- into it. Only the cells that changed are written, each run of a style under one SGR sequence, so a frame
-- that is mostly the same costs almost nothing and nothing flickers.
module Tui.Buffer
  ( Cell (..), Put (..), Frame, frameSize, cellAt
  , frame, textLine, fillRect, diff, sgr, putWidth
  ) where

import Data.Array
import qualified Data.List as L
import qualified Data.Text as T

import Tui.Types

-- | One cell: its style and its text (a grapheme, or empty for a blank), and whether it is the second
-- column of a wide character (then its text is empty and it is never drawn).
data Cell = Cell { cStyle :: !Style, cText :: !T.Text, cCont :: !Bool }
  deriving (Eq, Show)

blank :: Cell
blank = Cell plain T.empty False

-- | Something to draw: text at a column and row (cut at the frame's edge; a wide character takes two
-- columns), or cells as they are (a pane's screen), with their own widths.
data Put
  = PutText !Int !Int !Style String
  | PutCells !Int !Int [(Cell, Int)]      -- ^ each cell with the columns it takes (0 is skipped, 2 is wide)
  deriving (Show)

type Frame = Array (Int, Int) Cell        -- ^ indexed by (row, column)

frameSize :: Frame -> (Int, Int)
frameSize f = let (_, (h, w)) = bounds f in (w + 1, h + 1)

cellAt :: Frame -> Int -> Int -> Cell
cellAt f x y = let (_, (h, w)) = bounds f in if x < 0 || y < 0 || x > w || y > h then blank else f ! (y, x)

-- | The columns a text takes.
putWidth :: String -> Int
putWidth = sum . map charWidth

-- | A frame of @w@ x @h@ from puts, later ones over earlier ones.
frame :: (Int, Int) -> [Put] -> Frame
frame (w, h) puts = accumArray (\_ c -> c) blank ((0, 0), (max 0 (h - 1), max 0 (w - 1))) (concatMap cells puts)
  where
    cells (PutText x y st s) = place x y [ (Cell st (T.pack g) False, gw) | (g, gw) <- clusters s ]
    cells (PutCells x y cs) = place x y cs
    place x y cs = go x cs
      where
        go _ [] = []
        go cx ((c, cw) : r)
          | y < 0 || y >= h = []
          | cw <= 0 = go cx r                                  -- (a combining mark: dropped; a spacer: skipped)
          | cx + cw > w = []                                   -- (cut at the edge: a wide character that does not fit is left out)
          | cx < 0 = go (cx + cw) r
          | cw == 2 = ((y, cx), c) : ((y, cx + 1), Cell (cStyle c) T.empty True) : go (cx + 2) r
          | otherwise = ((y, cx), c) : go (cx + 1) r

-- | A text as what each cell holds: a character with the marks that combine with it, and its columns. (A mark
-- with nothing before it is left out.)
clusters :: String -> [(String, Int)]
clusters [] = []
clusters (c : r) | charWidth c == 0 = clusters r
                 | otherwise = let (ms, rest) = span ((== 0) . charWidth) r in (c : ms, charWidth c) : clusters rest

-- | A line of styled runs at a row and column, cut to @width@ columns and padded with blanks to it.
textLine :: Int -> Int -> Int -> [(Style, String)] -> [Put]
textLine x y width runs = go x runs
  where
    end = x + width
    go cx [] = [ PutText cx y plain (replicate (end - cx) ' ') | cx < end ]
    go cx ((st, s) : r) = let (taken, used) = fitIn (end - cx) s in PutText cx y st taken : (if used >= end - cx then [] else go (cx + used) r)
    fitIn n s = let step (acc, used) c = if used + charWidth c > n then (acc, n) else (c : acc, used + charWidth c)
                    (acc', used') = L.foldl' step ([], 0) s
                in (reverse acc', min n used')

fillRect :: Rect -> Style -> Char -> [Put]
fillRect (Rect x y w h) st c = [ PutText x r st (replicate w c) | r <- [y .. y + h - 1] ]

-- | The SGR sequence that sets a style from nothing.
sgr :: Style -> String
sgr s = "\ESC[0" ++ concat [ ";1" | sBold s ] ++ concat [ ";2" | sFaint s ] ++ concat [ ";3" | sItalic s ] ++ concat [ ";4" | sUnderline s ]
        ++ concat [ ";7" | sInverse s ] ++ concat [ ";9" | sStrike s ] ++ color 38 (sFg s) ++ color 48 (sBg s) ++ "m"
  where
    color base c = case c of
      Default -> ";" ++ show (base + 1)
      Rgb r g b -> ";" ++ show base ++ ";2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b
      Ansi n | n < 8 -> ";" ++ show (base - 8 + n)
             | n < 16 -> ";" ++ show (base + 52 + n)
             | otherwise -> ";" ++ show base ++ ";5;" ++ show n
      
-- | The bytes that make the terminal show @new@, given it shows @old@ (or nothing known: everything is
-- written). The cursor is left wherever; the caller places it after.
diff :: Maybe Frame -> Frame -> String
diff old new = concat (go Nothing Nothing [ (y, x) | y <- [0 .. h - 1], x <- [0 .. w - 1] ])
  where
    (w, h) = frameSize new
    same y x = case old of
      Just o | frameSize o == (w, h) -> o ! (y, x) == new ! (y, x)
      _ -> False
    go _ _ [] = []
    go at st ((y, x) : rest)
      | cCont c = go at st rest
      | same y x && not (nextChanged y x) = go at st rest     -- (a wide cell changed shows in its continuation too)
      | otherwise =
          let move = if at == Just (y, x) then "" else "\ESC[" ++ show (y + 1) ++ ";" ++ show (x + 1) ++ "H"
              style = if st == Just (cStyle c) then "" else sgr (cStyle c)
              txt = if T.null (cText c) then " " else T.unpack (cText c)
              -- (the columns it takes are the frame's: a wide cell is followed by its second column)
              width = if x + 1 < w && cCont (new ! (y, x + 1)) then 2 else 1
          in (move ++ style ++ txt) : go (Just (y, x + width)) (Just (cStyle c)) rest
      where c = new ! (y, x)
    nextChanged y x = x + 1 < w && cCont (new ! (y, x + 1)) && not (same y (x + 1))
