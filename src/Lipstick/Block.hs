-- | Blocks of styled text and how to combine them.
--
-- A 'Block' does not have a width until it is laid out: text in it wraps,
-- and things that 'Lipstick.Mod.grow' take spare columns, to fit the width
-- it gets. Output functions lay blocks out in the output's width (see
-- 'Lipstick.Ansi.envWidth'); 'layout' does it explicitly.
module Lipstick.Block
  ( -- * Blocks
    Block
  , blockWidth
  , blockHeight
  , blockMinWidth
  , emptyBlock
  , text
  , fromInline

    -- * Laying out
  , layout
  , Grid
  , gridWidth
  , gridHeight
  , fromGrid

    -- * Inline text
  , Inline
  , plain
  , styled
  , styleRanges
  , styleIndices

    -- * Joining
  , joinHorizontal
  , joinVertical
  , hcat
  , vcat
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Lipstick.Internal.Grid
import Lipstick.Internal.Layout
import Lipstick.Internal.Types
import Lipstick.Internal.Mod (Mod, Props (..), propsOf, runMod)

-- | The natural width of a block: as wide as it is with nothing wrapped.
blockWidth :: Block -> Int
blockWidth = reqMax . blockRequest . measure

-- | The height of a block at its natural width.
blockHeight :: Block -> Int
blockHeight b = snd (blockLayout (measure b) (Assign (blockWidth b) 0))

-- | The narrowest a block can be laid out without breaking words.
blockMinWidth :: Block -> Int
blockMinWidth = reqMin . blockRequest . measure

-- | Unstyled text; lines are split on newlines, and wrap to the width the
-- block is given.
text :: Text -> Block
text = fromInline . plain

-- | Inline text as a block, without any box around it. Lines wrap to the
-- width the block is given.
fromInline :: Inline -> Block
fromInline = textBlock

-- | Text with attributes, for building 'Inline' text with '<>'. Only the
-- text attributes of the modifier apply.
--
-- > styled bold "Error: " <> "file not found"
styled :: Mod Inline -> Text -> Inline
styled m t = Inline [(propAttrs (snd (runMod m mempty)), t)]

-- | Place blocks side by side. The width is shared out with spare columns
-- going to blocks that 'Lipstick.Mod.grow', and the widest shrinking first
-- when there is not enough. The position aligns shorter blocks vertically
-- ('top', 'center', 'bottom'); blocks that 'Lipstick.Mod.stretchHeight'
-- grow to the tallest instead.
joinHorizontal :: Position -> [Block] -> Block
joinHorizontal p bs = Block $ \ctx ->
  let ns = map (measureIn ctx) bs
      rs = map blockRequest ns
   in rowNode p ns rs

rowNode :: Position -> [Measured] -> [Request] -> Measured
rowNode p ns rs = Measured req run
  where
    req =
      Request
        { reqMin = sum (map reqMin rs)
        , reqMax = sum (map reqMax rs)
        , reqGrow = sum (map reqGrow rs)
        , reqLimit = addLimits rs
        , reqStretch = any reqStretch rs
        , reqFill = False
        }
    run (Assign w h) = (fitWidth w (joinGridsH p (map fst results)), natural)
      where
        ws = allocateWithin w [(reqMin r, reqMax r, reqGrow r, reqLimit r) | r <- rs]
        -- The row height goes into the calls that give the natural heights
        -- it is computed from: a block's natural height never depends on
        -- the height it is asked to stretch to.
        results = [blockLayout n (Assign bw rowH) | (n, bw) <- zip ns ws]
        natural = maximum (0 : map snd results)
        rowH = if reqStretch req then max h natural else natural

-- | Stack blocks. Each takes as much of the column's width as it can use
-- (all of it if it grows without a limit); the position aligns those horizontally ('left',
-- 'center', 'right').
joinVertical :: Position -> [Block] -> Block
joinVertical p bs = Block $ \ctx ->
  let ns = map (measureIn ctx) bs
      rs = map blockRequest ns
   in columnNode p ns rs

columnNode :: Position -> [Measured] -> [Request] -> Measured
columnNode p ns rs = Measured req run
  where
    req =
      Request
        { reqMin = maximum (0 : map reqMin rs)
        , reqMax = maximum (0 : map reqMax rs)
        , reqGrow = maximum (0 : map reqGrow rs)
        , reqLimit = maxLimit rs
        , reqStretch = False
        , reqFill = False
        }
    run (Assign w _) = (joinGridsV p w (map fst results), sum (map snd results))
      where
        results = [blockLayout n (Assign (takenWidth r w) 0) | (n, r) <- zip ns rs]

-- | Side by side, aligned to the top.
hcat :: [Block] -> Block
hcat = joinHorizontal top

-- | Stacked, aligned to the left.
vcat :: [Block] -> Block
vcat = joinVertical left

-- | Add attributes to ranges of display columns @[start, end)@, counted
-- across the text with newlines taking no columns. A tab counts as four
-- columns, the default 'Lipstick.Style.tabWidth': where the text ends up,
-- and so its tab width, is not known yet. The range attributes go on top
-- of the text's own.
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
      | c == T.singleton '\t' = (a <> extra col i, c) : go (col + textTabWidth defTextStyle) (i + 1) rest
      | otherwise = (a <> extra col i, c) : go (col + cw) (i + 1) rest
    merge ((a1, t1) : (a2, t2) : rest)
      | a1 == a2 = merge ((a1, t1 <> t2) : rest)
    merge (x : rest) = x : merge rest
    merge [] = []
