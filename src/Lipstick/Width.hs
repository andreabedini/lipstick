-- | Display width of text in a terminal.
--
-- Text is split into extended grapheme clusters following UAX #29 (rules
-- GB3–GB13, including GB9c for Indic conjuncts). A cluster is as wide as
-- its first character with a non-zero width: two columns for East Asian
-- Wide and Fullwidth characters, none for controls, combining marks and
-- Hangul vowels and final consonants, one otherwise. An emoji followed by
-- VS16 is two columns wide and by VS15 one; a pair of regional indicators
-- (a flag) is two.
--
-- The tables are generated from the Unicode Character Database by
-- @scripts/GenUnicodeTable.hs@; see 'unicodeVersion'.
module Lipstick.Width
  ( charWidth
  , textWidth
  , clusters
  , unicodeVersion
  ) where

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (bounds)
import Data.Bits (shiftR, testBit, (.&.))
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import Lipstick.Internal.UnicodeTable

-- | The Unicode version the tables were generated from.
unicodeVersion :: String
unicodeVersion = unicodeTableVersion

-- | Display width of a whole text.
textWidth :: Text -> Int
textWidth = sum . map snd . clusters

-- | Display width of a single character, ignoring context.
charWidth :: Char -> Int
charWidth = codeWidth . props

-- | Split text into grapheme clusters, each with its display width.
clusters :: Text -> [(Text, Int)]
clusters = go . T.unpack
  where
    go [] = []
    go (c : cs) =
      let p = props c
          (ext, rest) = extend (start p) p cs
          cl = c : map fst ext
       in (T.pack cl, clusterWidth ((c, p) : ext)) : go rest

    extend _ _ [] = ([], [])
    extend st prev (c : cs)
      | joins st prev p = let (ext, rest) = extend (step st p) p cs in ((c, p) : ext, rest)
      | otherwise = ([], c : cs)
      where
        p = props c

-- Properties ------------------------------------------------------------------

-- | Packed properties of a character (see the generator for the layout).
props :: Char -> Word16
props c
  | n < 0x7F = if n < 0x20 then asciiControl n else asciiProps n
  | otherwise = unsafeAt runValues (search 0 hi)
  where
    n = ord c
    (_, hi) = bounds runStarts
    -- The last run starting at or before n.
    search lo h
      | lo >= h = lo
      | otherwise =
          let mid = (lo + h + 1) `div` 2
           in if unsafeAt runStarts mid <= n then search mid h else search lo (mid - 1)
    asciiControl k
      | k == 0x0D = gcbCR
      | k == 0x0A = gcbLF
      | otherwise = gcbControl
    asciiProps k
      -- '#', '*' and the digits have the Emoji property (keycaps).
      | k == 0x23 || k == 0x2A || (k >= 0x30 && k <= 0x39) = 0x40
      | otherwise = 0

gcb :: Word16 -> Word16
gcb p = p .&. 0xF

isWide, isPictographic, isEmoji :: Word16 -> Bool
isWide p = testBit p 4
isPictographic p = testBit p 5
isEmoji p = testBit p 6

incb :: Word16 -> Word16
incb p = (p `shiftR` 7) .&. 3

gcbCR, gcbLF, gcbControl, gcbExtend, gcbZWJ, gcbRI :: Word16
gcbCR = 1
gcbLF = 2
gcbControl = 3
gcbExtend = 4
gcbZWJ = 5
gcbRI = 6

gcbPrepend, gcbSpacingMark, gcbL, gcbV, gcbT, gcbLV, gcbLVT :: Word16
gcbPrepend = 7
gcbSpacingMark = 8
gcbL = 9
gcbV = 10
gcbT = 11
gcbLV = 12
gcbLVT = 13

incbLinker, incbConsonant, incbExtend :: Word16
incbLinker = 1
incbConsonant = 2
incbExtend = 3

-- Segmentation ----------------------------------------------------------------

-- | What the current cluster has seen, for the rules that look back further
-- than one character.
data State = State
  { stRegional :: !Int -- ^ regional indicators in a row
  , stPict :: !Pict
  , stConjunct :: !Conjunct
  }

data Pict = NoPict | InPict | PictZWJ
  deriving (Eq)

-- | Indic conjunct: a consonant followed by extends and linkers.
data Conjunct = NoConjunct | Consonant !Bool -- ^ a linker has been seen
  deriving (Eq)

start :: Word16 -> State
start = step (State 0 NoPict NoConjunct)

step :: State -> Word16 -> State
step (State regional pict conjunct) p =
  State
    { stRegional = if gcb p == gcbRI then regional + 1 else 0
    , stPict = pict'
    , stConjunct = conjunct'
    }
  where
    pict'
      | isPictographic p = InPict
      | pict == InPict && gcb p == gcbExtend = InPict
      | pict == InPict && gcb p == gcbZWJ = PictZWJ
      | otherwise = NoPict
    conjunct' = case conjunct of
      _ | incb p == incbConsonant -> Consonant False
      Consonant _ | incb p == incbLinker -> Consonant True
      Consonant l | incb p == incbExtend -> Consonant l
      _ -> NoConjunct

-- | Whether the next character continues the current cluster.
joins :: State -> Word16 -> Word16 -> Bool
joins st prevP nextP
  | prev == gcbCR && next == gcbLF = True -- GB3
  | prev `elem` [gcbControl, gcbCR, gcbLF] = False -- GB4
  | next `elem` [gcbControl, gcbCR, gcbLF] = False -- GB5
  | prev == gcbL && next `elem` [gcbL, gcbV, gcbLV, gcbLVT] = True -- GB6
  | prev `elem` [gcbLV, gcbV] && next `elem` [gcbV, gcbT] = True -- GB7
  | prev `elem` [gcbLVT, gcbT] && next == gcbT = True -- GB8
  | next == gcbExtend || next == gcbZWJ = True -- GB9
  | next == gcbSpacingMark = True -- GB9a
  | prev == gcbPrepend = True -- GB9b
  | incb nextP == incbConsonant && stConjunct st == Consonant True = True -- GB9c
  | prev == gcbZWJ && isPictographic nextP && stPict st == PictZWJ = True -- GB11
  | prev == gcbRI && next == gcbRI = odd (stRegional st) -- GB12, GB13
  | otherwise = False -- GB999
  where
    prev = gcb prevP
    next = gcb nextP

-- Width -----------------------------------------------------------------------

codeWidth :: Word16 -> Int
codeWidth p
  | g `elem` [gcbControl, gcbCR, gcbLF, gcbExtend, gcbZWJ, gcbV, gcbT] = 0
  | isWide p = 2
  | otherwise = 1
  where
    g = gcb p

clusterWidth :: [(Char, Word16)] -> Int
clusterWidth cl = case cl of
  [(_, p1), (_, p2)] | gcb p1 == gcbRI && gcb p2 == gcbRI -> 2
  (_, p) : rest
    | isEmoji p && any ((== '\xFE0F') . fst) rest -> 2
    | isEmoji p && any ((== '\xFE0E') . fst) rest -> 1
  _ -> case filter (> 0) (map (codeWidth . snd) cl) of
    w : _ -> w
    [] -> 0
