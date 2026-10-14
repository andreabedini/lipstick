-- | Laid-out blocks, for showing them somewhere other than a terminal (a
-- vty image, an SVG, …).
--
-- A 'Grid' is what 'Lipstick.Block.layout' gives: a rectangle of lines,
-- each exactly 'gridWidth' columns wide, made of 'Cell's, one grapheme
-- cluster each. 'lineSpans' groups a line's cells into runs with the same
-- attributes, which is usually what an output needs.
--
-- == Examples
--
-- >>> let g = layout Nothing (render (border asciiBorder) (styled bold "hi" <> " there"))
-- >>> (gridWidth g, gridHeight g)
-- (10,3)
-- >>> map lineWidth (gridLines g)
-- [10,10,10]
-- >>> let middle = gridLines g !! 1
-- >>> map cellText (lineCells middle)
-- ["|","h","i"," ","t","h","e","r","e","|"]
-- >>> [(spanText s, spanWidth s, attrBold (spanAttrs s)) | s <- lineSpans middle]
-- [("|",1,Nothing),("hi",2,Just True),(" there|",7,Nothing)]
--
-- A wide character takes one cell, two columns wide:
--
-- >>> [cellWidth c | l <- gridLines (layout Nothing "a你b"), c <- lineCells l]
-- [1,2,1]
module Lipstick.Grid
  ( -- * Grids
    Grid
  , gridWidth
  , gridHeight
  , gridLines

    -- * Lines
  , Line
  , lineWidth
  , lineCells
  , lineSpans
  , Span (..)

    -- * Cells
  , Cell
  , cellText
  , cellAttrs
  , cellWidth
  ) where

import Data.Text (Text)
import Lipstick.Internal.Grid (Cell, Grid, Line, Span (..), lineSpans)
import Lipstick.Internal.Grid qualified as I
import Lipstick.Internal.Types (Attrs)

-- $setup
-- >>> import Lipstick
-- >>> import Lipstick.Grid

gridWidth, gridHeight :: Grid -> Int
gridWidth = I.gridWidth
gridHeight = I.gridHeight

-- | The lines, from the top.
gridLines :: Grid -> [Line]
gridLines = I.gridLines

lineWidth :: Line -> Int
lineWidth = I.lineWidth

-- | The cells of a line, from the left.
lineCells :: Line -> [Cell]
lineCells = I.lineCells

-- | The grapheme cluster in a cell.
cellText :: Cell -> Text
cellText = I.cellText

cellAttrs :: Cell -> Attrs
cellAttrs = I.cellAttrs

-- | How many columns the cell takes (0, 1 or 2).
cellWidth :: Cell -> Int
cellWidth = I.cellWidth
