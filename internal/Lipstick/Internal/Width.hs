-- | The implementation of "Lipstick.Width", hidden from other packages: see
-- that module.
module Lipstick.Internal.Width
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

-- | Split text into grapheme clusters, each with its display width. The
-- clusters are slices of the text, not copies.
clusters :: Text -> [(Text, Int)]
clusters = go
  where
    go t = case T.uncons t of
      Nothing -> []
      Just (c, rest) ->
        let p = props c
            (n, ext) = extend (start p) p 1 rest
            (cl, more) = T.splitAt n t
         in (cl, clusterWidth ((c, p) : ext)) : go more

    -- How many characters the cluster has, and the ones after the first.
    extend st prev n t = case T.uncons t of
      Just (c, rest)
        | let p = props c
        , joins st prev p ->
            let (n', ext) = extend (step st p) p (n + 1) rest in (n', (c, p) : ext)
      _ -> (n :: Int, [])

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
  | isEmojiModifier p = 2
  | g `elem` [gcbControl, gcbCR, gcbLF, gcbExtend, gcbZWJ, gcbV, gcbT] = 0
  | isWide p = 2
  | otherwise = 1
  where
    g = gcb p

-- | The skin tone modifiers: Extend, so they join the emoji before them,
-- but wide.
isEmojiModifier :: Word16 -> Bool
isEmojiModifier p = gcb p == gcbExtend && isEmoji p

-- | VS16 and a skin tone modifier both make an emoji wide (UTS #51), also
-- one that is narrow on its own, such as ☝. VS15 changes nothing: it asks
-- for text presentation, not for a width.
clusterWidth :: [(Char, Word16)] -> Int
clusterWidth cl = case cl of
  [(_, p1), (_, p2)] | gcb p1 == gcbRI && gcb p2 == gcbRI -> 2
  (_, p) : rest
    | isEmoji p && any (\(c, q) -> c == '\xFE0F' || isEmojiModifier q) rest -> 2
  _ -> case filter (> 0) (map (codeWidth . snd) cl) of
    w : _ -> w
    [] -> 0
