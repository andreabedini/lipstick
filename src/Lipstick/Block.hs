-- | Blocks of styled text and how to combine them.
module Lipstick.Block
  ( -- * Blocks
    Block
  , blockWidth
  , blockHeight
  , emptyBlock
  , text
  , fromInline

    -- * Inline text
  , Inline
  , plain
  , styled
  , wrap
  , styleRanges
  , styleIndices

    -- * Joining
  , joinHorizontal
  , joinVertical
  , hcat
  , vcat
  ) where

import Data.List (transpose)
import Data.Text (Text)
import Data.Text qualified as T
import Lipstick.Internal.Block
import Lipstick.Internal.Types
import Lipstick.Mod (Mod, Props (..), propsOf, runMod)

-- | Unstyled text; lines are split on newlines.
text :: Text -> Block
text = fromInline . plain

-- | Inline text as a block, without any box around it.
fromInline :: Inline -> Block
fromInline = fromCellLines . inlineLines 4

-- | Text with attributes, for building 'Inline' text with '<>'. Only the
-- text attributes of the modifier apply.
--
-- > styled bold "Error: " <> "file not found"
styled :: Mod Inline -> Text -> Inline
styled m t = Inline [(propAttrs (snd (runMod m mempty)), t)]

-- | Place blocks side by side. The position aligns shorter blocks
-- vertically ('top', 'center', 'bottom').
joinHorizontal :: Position -> [Block] -> Block
joinHorizontal p bs =
  Block (sum (map blockWidth bs)) h (map (cellsLine . concat) (transpose (map columnOf bs)))
  where
    h = maximum (0 : map blockHeight bs)
    columnOf b =
      let (before, after) = splitExtra p (h - blockHeight b)
          blank = blankCells mempty (blockWidth b)
       in replicate before blank ++ blockCells b ++ replicate after blank

-- | Stack blocks. The position aligns narrower blocks horizontally
-- ('left', 'center', 'right').
joinVertical :: Position -> [Block] -> Block
joinVertical p bs =
  Block w (sum (map blockHeight bs)) [cellsLine (alignCells mempty p w l) | b <- bs, l <- blockCells b]
  where
    w = maximum (0 : map blockWidth bs)

-- | Side by side, aligned to the top.
hcat :: [Block] -> Block
hcat = joinHorizontal top

-- | Stacked, aligned to the left.
vcat :: [Block] -> Block
vcat = joinVertical left

-- | Word-wrap text to a width, breaking at spaces and after any of the
-- given characters, without any box around it.
--
-- > wrap 20 "-" "a long-winded, hyphenated sentence"
wrap :: Int -> [Char] -> Inline -> Block
wrap w breakpoints = fromCellLines . concatMap (wrapCellsWith breakpoints w) . inlineLines 4

-- | Add attributes to ranges of display columns @[start, end)@, counted
-- across the text with newlines taking no columns. The range attributes
-- go on top of the text's own.
--
-- > styleRanges [(0, 3, bold <> fg red)] "foobar"
styleRanges :: [(Int, Int, Mod Inline)] -> Inline -> Inline
styleRanges ranges = restyle $ \col _ ->
  mconcat [propAttrs (propsOf m) | (s, e, m) <- ranges, col >= s, col < e]

-- | Add attributes to the characters (grapheme clusters, not counting
-- newlines) at the given indices, and other attributes to the rest. Handy
-- for highlighting fuzzy-search matches.
--
-- > styleIndices [0, 3] (fg pink <> underline) mempty "foobar"
styleIndices :: [Int] -> Mod Inline -> Mod Inline -> Inline -> Inline
styleIndices indices matched unmatched = restyle $ \_ i ->
  propAttrs (propsOf (if i `elem` indices then matched else unmatched))

-- | Layer extra attributes over each cluster, given its starting column and
-- its index.
restyle :: (Int -> Int -> Attrs) -> Inline -> Inline
restyle extra (Inline segs) = Inline (merge (go 0 0 (styledClusters segs)))
  where
    go _ _ [] = []
    go col i ((a, c, cw) : rest)
      | T.any (== '\n') c = (a, c) : go col i rest
      | otherwise = (a <> extra col i, c) : go (col + cw) (i + 1) rest
    merge ((a1, t1) : (a2, t2) : rest)
      | a1 == a2 = merge ((a1, t1 <> t2) : rest)
    merge (x : rest) = x : merge rest
    merge [] = []
