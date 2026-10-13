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

    -- * Joining
  , joinHorizontal
  , joinVertical
  , hcat
  , vcat
  ) where

import Data.List (transpose)
import Data.Text (Text)
import Lipstick.Internal.Block
import Lipstick.Internal.Types
import Lipstick.Mod (Mod, Props (..), runMod)

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
