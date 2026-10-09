-- | __A picture among the cells__, by the kitty graphics protocol's placeholders (Ghostty and kitty have
-- them). An image is sent to the terminal once, under a number, as a /virtual/ placement of so many columns
-- and rows ('transmit'); it is then shown wherever cells hold the placeholder character in the color of that
-- number, each saying by two combining marks which row and column of the picture it is ('placeholderRow').
-- So a picture is text: it is put in a frame as any line is, scrolls with the lines around it, is cut at an
-- edge, and a frame's difference from the last redraws it -- nothing has to follow where it went.
--
-- The terminal fits the image into the cells it was given, keeping its shape; 'cellsFor' chooses them.
module Tui.Graphics
  ( graphicsTerm, maxCells, cellsFor, transmit, forget, placeholderRow, placeholderStyle, isPlaceholder
  ) where

import qualified Data.ByteString.Char8 as BC
import Data.Char (chr, toLower)
import Data.List (isInfixOf)
import System.Environment (lookupEnv)

import Tui.Types

-- | Is the terminal one that shows such pictures, by its name? (A pane of ours is not: it is told another.)
graphicsTerm :: IO Bool
graphicsTerm = maybe False (\t -> any (`isInfixOf` map toLower t) ["ghostty", "kitty"]) <$> lookupEnv "TERM"

-- | The most columns or rows a picture takes: a cell names its row and column by a mark of the table.
maxCells :: Int
maxCells = length marks

-- | The cells for an image of these pixels, in cells of those, within so many columns and rows: its own size
-- where it fits (never made larger), else the largest of its shape that does.
cellsFor :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
cellsFor (cw0, ch0) (iw, ih) (maxC, maxR) =
  let cw = max 1 cw0; ch = max 1 ch0
      up a b = (a + b - 1) `div` b
      capC = max 1 (min maxCells maxC); capR = max 1 (min maxCells maxR)
      c0 = min capC (up iw cw)
      r0 = up (c0 * cw * ih) (max 1 iw * ch)
      (c, r) = if r0 <= capR then (c0, r0) else (min capC (up (capR * ch * iw) (max 1 ih * cw)), capR)
  in (max 1 c, max 1 r)

-- | The bytes that give the terminal an image (a PNG, in base64) under a number (1 to 255), to be shown in
-- so many columns and rows. It answers nothing (it is asked not to: an answer would be read as keys).
transmit :: Int -> (Int, Int) -> BC.ByteString -> String
transmit n (c, r) b64 = go True b64
  where
    go first b =
      let (piece, rest) = BC.splitAt 4096 b
          more = if BC.null rest then "m=0" else "m=1"
          keys = if first then "a=T,U=1,q=2,f=100,i=" ++ show n ++ ",c=" ++ show c ++ ",r=" ++ show r ++ "," ++ more else more
      in "\ESC_G" ++ keys ++ ";" ++ BC.unpack piece ++ "\ESC\\" ++ (if BC.null rest then "" else go False rest)

-- | The bytes that make the terminal forget an image.
forget :: Int -> String
forget n = "\ESC_Ga=d,d=I,q=2,i=" ++ show n ++ "\ESC\\"

-- | The style of a picture's cells: the image's number is their color.
placeholderStyle :: Int -> Style
placeholderStyle n = withFg (Ansi n) plain

-- | A row of a picture as text: a placeholder for each column, marked with the row and the column.
placeholderRow :: Int -> Int -> String
placeholderRow row cols = concat [ [placeholder, marks !! row, marks !! c] | row < maxCells, c <- [0 .. min cols maxCells - 1] ]

placeholder :: Char
placeholder = '\x10EEEE'

isPlaceholder :: String -> Bool
isPlaceholder s = take 1 s == [placeholder]

-- (the protocol's table of marks, its first 128: the n-th says n)
marks :: [Char]
marks = map chr
  [ 0x0305,0x030D,0x030E,0x0310,0x0312,0x033D,0x033E,0x033F,0x0346,0x034A,0x034B,0x034C,0x0350,0x0351,0x0352,0x0357
  , 0x035B,0x0363,0x0364,0x0365,0x0366,0x0367,0x0368,0x0369,0x036A,0x036B,0x036C,0x036D,0x036E,0x036F,0x0483,0x0484
  , 0x0485,0x0486,0x0487,0x0592,0x0593,0x0594,0x0595,0x0597,0x0598,0x0599,0x059C,0x059D,0x059E,0x059F,0x05A0,0x05A1
  , 0x05A8,0x05A9,0x05AB,0x05AC,0x05AF,0x05C4,0x0610,0x0611,0x0612,0x0613,0x0614,0x0615,0x0616,0x0617,0x0657,0x0658
  , 0x0659,0x065A,0x065B,0x065D,0x065E,0x06D6,0x06D7,0x06D8,0x06D9,0x06DA,0x06DB,0x06DC,0x06DF,0x06E0,0x06E1,0x06E2
  , 0x06E4,0x06E7,0x06E8,0x06EB,0x06EC,0x0730,0x0732,0x0733,0x0735,0x0736,0x073A,0x073D,0x073F,0x0740,0x0741,0x0743
  , 0x0745,0x0747,0x0749,0x074A,0x07EB,0x07EC,0x07ED,0x07EE,0x07EF,0x07F0,0x07F1,0x07F3,0x0816,0x0817,0x0818,0x0819
  , 0x081B,0x081C,0x081D,0x081E,0x081F,0x0820,0x0821,0x0822,0x0823,0x0825,0x0826,0x0827,0x0829,0x082A,0x082B,0x082C ]
