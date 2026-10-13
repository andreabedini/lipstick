-- | Display width of text in a terminal.
--
-- This is an approximation of grapheme segmentation and East Asian Width:
-- combining marks, format characters, variation selectors and emoji
-- modifiers attach to the preceding character; ZWJ sequences and regional
-- indicator pairs form one cluster; wide and fullwidth characters take two
-- columns. The wide ranges are a hand-written table for now and will be
-- replaced by one generated from the Unicode data files.
module Lipstick.Width
  ( charWidth
  , textWidth
  , clusters
  ) where

import Data.Char (GeneralCategory (..), generalCategory, ord)
import Data.Text (Text)
import Data.Text qualified as T

-- | Display width of a whole text.
textWidth :: Text -> Int
textWidth = sum . map snd . clusters

-- | Split text into clusters, each with its display width.
clusters :: Text -> [(Text, Int)]
clusters = go . T.unpack
  where
    go [] = []
    go (c : c2 : rest)
      | isRegional c && isRegional c2 = (T.pack [c, c2], 2) : go rest
    go (c : rest) =
      let (ext, rest') = extend rest
          w
            | '\xFE0F' `elem` ext = 2
            | '\xFE0E' `elem` ext = min 1 (charWidth c)
            | otherwise = charWidth c
       in (T.pack (c : ext), w) : go rest'

    extend (z : n : rest)
      | z == '\x200D' = let (ext, rest') = extend rest in (z : n : ext, rest')
    extend (c : rest)
      | isExtender c = let (ext, rest') = extend rest in (c : ext, rest')
    extend rest = ([], rest)

isRegional :: Char -> Bool
isRegional c = c >= '\x1F1E6' && c <= '\x1F1FF'

isExtender :: Char -> Bool
isExtender c = isZeroWidth c || (c >= '\x1F3FB' && c <= '\x1F3FF')

isZeroWidth :: Char -> Bool
isZeroWidth c = case generalCategory c of
  NonSpacingMark -> True
  EnclosingMark -> True
  Format -> True
  _ -> c >= '\x1160' && c <= '\x11FF' -- Hangul medial vowels and final consonants

-- | Display width of a single character, ignoring context.
charWidth :: Char -> Int
charWidth c
  | n < 0x20 || (n >= 0x7F && n < 0xA0) = 0
  | n < 0x1100 = if isZeroWidth c then 0 else 1
  | isZeroWidth c = 0
  | isWide n = 2
  | otherwise = 1
  where
    n = ord c

isWide :: Int -> Bool
isWide n = any (\(lo, hi) -> n >= lo && n <= hi) wideRanges

wideRanges :: [(Int, Int)]
wideRanges =
  [ (0x1100, 0x115F), (0x231A, 0x231B), (0x2329, 0x232A), (0x23E9, 0x23EC)
  , (0x23F0, 0x23F0), (0x23F3, 0x23F3), (0x25FD, 0x25FE), (0x2614, 0x2615)
  , (0x2648, 0x2653), (0x267F, 0x267F), (0x2693, 0x2693), (0x26A1, 0x26A1)
  , (0x26AA, 0x26AB), (0x26BD, 0x26BE), (0x26C4, 0x26C5), (0x26CE, 0x26CE)
  , (0x26D4, 0x26D4), (0x26EA, 0x26EA), (0x26F2, 0x26F3), (0x26F5, 0x26F5)
  , (0x26FA, 0x26FA), (0x26FD, 0x26FD), (0x2705, 0x2705), (0x270A, 0x270B)
  , (0x2728, 0x2728), (0x274C, 0x274C), (0x274E, 0x274E), (0x2753, 0x2755)
  , (0x2757, 0x2757), (0x2795, 0x2797), (0x27B0, 0x27B0), (0x27BF, 0x27BF)
  , (0x2B1B, 0x2B1C), (0x2B50, 0x2B50), (0x2B55, 0x2B55), (0x2E80, 0x303E)
  , (0x3041, 0x33FF), (0x3400, 0x4DBF), (0x4E00, 0x9FFF), (0xA000, 0xA4CF)
  , (0xA960, 0xA97F), (0xAC00, 0xD7A3), (0xF900, 0xFAFF), (0xFE10, 0xFE19)
  , (0xFE30, 0xFE6F), (0xFF00, 0xFF60), (0xFFE0, 0xFFE6), (0x16FE0, 0x16FE4)
  , (0x17000, 0x18CFF), (0x1B000, 0x1B2FF), (0x1F004, 0x1F004), (0x1F0CF, 0x1F0CF)
  , (0x1F18E, 0x1F18E), (0x1F191, 0x1F19A), (0x1F200, 0x1F251), (0x1F300, 0x1F320)
  , (0x1F32D, 0x1F335), (0x1F337, 0x1F37C), (0x1F37E, 0x1F393), (0x1F3A0, 0x1F3CA)
  , (0x1F3CF, 0x1F3D3), (0x1F3E0, 0x1F3F0), (0x1F3F4, 0x1F3F4), (0x1F3F8, 0x1F43E)
  , (0x1F440, 0x1F440), (0x1F442, 0x1F4FC), (0x1F4FF, 0x1F53D), (0x1F54B, 0x1F54E)
  , (0x1F550, 0x1F567), (0x1F57A, 0x1F57A), (0x1F595, 0x1F596), (0x1F5A4, 0x1F5A4)
  , (0x1F5FB, 0x1F64F), (0x1F680, 0x1F6C5), (0x1F6CC, 0x1F6CC), (0x1F6D0, 0x1F6D2)
  , (0x1F6D5, 0x1F6D7), (0x1F6DC, 0x1F6DF), (0x1F6EB, 0x1F6EC), (0x1F6F4, 0x1F6FC)
  , (0x1F7E0, 0x1F7EB), (0x1F7F0, 0x1F7F0), (0x1F90C, 0x1F93A), (0x1F93C, 0x1F945)
  , (0x1F947, 0x1F9FF), (0x1FA70, 0x1FAFF), (0x20000, 0x2FFFD), (0x30000, 0x3FFFD)
  ]
