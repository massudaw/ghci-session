-- | What a cell looks like, and where things are.
module Tui.Types
  ( Color (..), Style (..), plain, withFg, withBg, bold, dim, inverse
  , Rect (..), inside, charWidth
  ) where

import Data.Word (Word8)

data Color = Default | Rgb !Word8 !Word8 !Word8 | Ansi !Int   -- ^ 'Ansi': 0-7 the normal colors, 8-15 the bright ones, up to 255 the palette
  deriving (Eq, Ord, Show)

data Style = Style
  { sFg, sBg :: !Color
  , sBold, sFaint, sItalic, sUnderline, sInverse, sStrike :: !Bool
  } deriving (Eq, Ord, Show)

plain :: Style
plain = Style Default Default False False False False False False

withFg :: Color -> Style -> Style
withFg c s = s { sFg = c }

withBg :: Color -> Style -> Style
withBg c s = s { sBg = c }

bold, dim, inverse :: Style -> Style
bold s = s { sBold = True }
dim s = s { sFaint = True }
inverse s = s { sInverse = True }

-- | A rectangle of cells: its left column, top row, width and height.
data Rect = Rect { rX, rY, rW, rH :: !Int }
  deriving (Eq, Show)

inside :: Rect -> (Int, Int) -> Bool
inside (Rect x y w h) (cx, cy) = cx >= x && cx < x + w && cy >= y && cy < y + h

-- | How many columns a character takes: 2 for the East Asian wide and the emoji ranges, 0 for a combining
-- mark, else 1. (Grapheme clusters a terminal makes of several characters are a pane's business: there
-- libghostty-vt says.)
charWidth :: Char -> Int
charWidth c
  | n < 0x300 = 1
  | n >= 0x300 && n <= 0x36F = 0
  | n >= 0x200B && n <= 0x200F = 0
  | n == 0xFE0F || (n >= 0xFE00 && n <= 0xFE0E) = 0
  | n >= 0x1100 && n <= 0x115F = 2
  | n >= 0x2E80 && n <= 0xA4CF && n /= 0x303F = 2
  | n >= 0xAC00 && n <= 0xD7A3 = 2
  | n >= 0xF900 && n <= 0xFAFF = 2
  | n >= 0xFE30 && n <= 0xFE4F = 2
  | n >= 0xFF00 && n <= 0xFF60 = 2
  | n >= 0xFFE0 && n <= 0xFFE6 = 2
  | n >= 0x1F300 && n <= 0x1F64F = 2
  | n >= 0x1F900 && n <= 0x1F9FF = 2
  | n >= 0x20000 && n <= 0x3FFFD = 2
  | otherwise = 1
  where n = fromEnum c
