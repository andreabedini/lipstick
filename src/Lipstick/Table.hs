-- | Tables.
--
-- > table
-- >   (  border roundedBorder <> borderFg purple
-- >   <> headerStyle (bold <> align center)
-- >   <> cellStyle (\c -> padding 0 1 <> if even (cellRow c) then fg grey else mempty)
-- >   <> width 60 )
-- >   ["Language", "Formal", "Informal"]
-- >   [["Chinese", "您好", "你好"], ["Japanese", "こんにちは", "やあ"]]
module Lipstick.Table
  ( Table
  , table
  , TableCell (..)
  , cellRow
  , cellColumn
  , cellStyle
  , headerStyle
  , headerRule
  , columnRules
  , rowRules
  ) where

import Data.List (intercalate, transpose)
import Lipstick.Border
import Lipstick.Internal.Block
import Lipstick.Internal.Types (Attrs (..), left)
import Lipstick.Mod
import Lipstick.Style (Style, render)
import Lipstick.Width (textWidth)

-- | Where a cell is: in the header, or in a body row (counting from 0).
data TableCell = HeaderCell !Int | BodyCell !Int !Int
  deriving (Eq, Show)

-- | The body row of a cell, or -1 for the header.
cellRow :: TableCell -> Int
cellRow = \case
  HeaderCell _ -> -1
  BodyCell r _ -> r

cellColumn :: TableCell -> Int
cellColumn = \case
  HeaderCell c -> c
  BodyCell _ c -> c

data Table = Table
  { tableBorder :: !BorderProps
  , tableSize :: !Size
  , tableWrap :: !Bool
  , tableHeaderRule :: !Bool
  , tableColumnRules :: !Bool
  , tableRowRules :: !Bool
  , tableCellStyle :: TableCell -> Mod Style
  }

instance HasBorder Table where
  overBorder f t = t {tableBorder = f (tableBorder t)}

-- | 'width' is the total width of the table: columns shrink (wrapping their
-- text) or grow to fit.
instance HasSize Table where
  overSize f t = t {tableSize = f (tableSize t)}

-- | With 'noWrap', cells that are too wide are cut with an ellipsis.
instance HasWrap Table where
  overWrap f t = t {tableWrap = f (tableWrap t)}

defTable :: Table
defTable =
  Table
    { tableBorder = defBorderProps {borderChars = Just roundedBorder}
    , tableSize = defSize
    , tableWrap = True
    , tableHeaderRule = True
    , tableColumnRules = True
    , tableRowRules = False
    , tableCellStyle = const mempty
    }

-- | Style cells by position. Successive 'cellStyle's add up: later ones
-- win where they set the same thing.
cellStyle :: (TableCell -> Mod Style) -> Mod Table
cellStyle f = fieldMod (\t -> t {tableCellStyle = \c -> tableCellStyle t c <> f c})

-- | Style the header cells.
headerStyle :: Mod Style -> Mod Table
headerStyle m = cellStyle $ \case
  HeaderCell _ -> m
  BodyCell _ _ -> mempty

-- | The rule under the header (default on).
headerRule :: Bool -> Mod Table
headerRule v = fieldMod (\t -> t {tableHeaderRule = v})

-- | Rules between columns (default on).
columnRules :: Bool -> Mod Table
columnRules v = fieldMod (\t -> t {tableColumnRules = v})

-- | Rules between body rows (default off).
rowRules :: Bool -> Mod Table
rowRules v = fieldMod (\t -> t {tableRowRules = v})

-- | A table with a header row (none if empty) and body rows. Short rows
-- are padded with empty cells.
table :: Mod Table -> [Inline] -> [[Inline]] -> Block
table m headers rows
  | ncols == 0 = emptyBlock
  | otherwise = withProps props allLines
  where
    (t, props) = runMod m defTable
    (chars, edges) = borderEdges (tableBorder t)
    ba = mempty {attrFg = borderFgColor (tableBorder t), attrBg = borderBgColor (tableBorder t)}
    Size w _ = tableSize t

    ncols = maximum (0 : length headers : map length rows)
    padRow r = take ncols (r ++ repeat mempty)
    allRows =
      [(HeaderCell, padRow headers) | not (null headers)]
        ++ [(BodyCell i, padRow r) | (i, r) <- zip [0 ..] rows]

    styleAt pos = tableCellStyle t pos <> (if tableWrap t then mempty else noWrap)
    cellPositions mk = map mk [0 .. ncols - 1]

    -- Column widths.
    natural =
      map
        (maximum . (0 :))
        (transpose [[blockWidth (render (styleAt pos) c) | (pos, c) <- zip (cellPositions mk) r] | (mk, r) <- allRows])
    sideW f e = if f edges then max 1 (textWidth e) else 0
    ruleW = if tableColumnRules t then max 1 (textWidth (leftEdge chars)) else 0
    frameW = sideW edgeLeft (leftEdge chars) + sideW edgeRight (rightEdge chars) + ruleW * (ncols - 1)
    widths = maybe natural (\total -> fitWidths (total - frameW) natural) w

    -- Cells rendered to their column width, as rows of cell lines.
    renderRow (mk, r) =
      let poss = cellPositions mk
          first = [render (styleAt pos <> width cw) c | (pos, c, cw) <- zip3 poss r widths]
          h = maximum (1 : map blockHeight first)
          blocks = [render (styleAt pos <> width cw <> height h) c | (pos, c, cw) <- zip3 poss r widths]
       in [ [alignCells mempty left cw (cropCells cw l) | (cw, l) <- zip widths ls]
          | ls <- transpose (map (padTo h . blockCells) blocks)
          ]
    padTo h ls = take h (ls ++ repeat [])

    piece txt n = fillCells ba txt n
    leftV = [piece (leftEdge chars) (sideW edgeLeft (leftEdge chars)) | edgeLeft edges]
    rightV = [piece (rightEdge chars) (sideW edgeRight (rightEdge chars)) | edgeRight edges]
    colRule = [piece (leftEdge chars) ruleW | tableColumnRules t]
    line cellsOf = concat (leftV ++ [intercalate (concat colRule) cellsOf] ++ rightV)
    rule l mid r edge =
      concat
        ( [piece l (sideW edgeLeft (leftEdge chars)) | edgeLeft edges]
            ++ [intercalate (concat [piece mid ruleW | tableColumnRules t]) [piece edge cw | cw <- widths]]
            ++ [piece r (sideW edgeRight (rightEdge chars)) | edgeRight edges]
        )

    rendered = map (\row -> (fst row, map line (renderRow row))) allRows
    isHeader (mk, _) = case mk 0 of
      HeaderCell _ -> True
      BodyCell _ _ -> False
    headerLines = concat [ls | (row, ls) <- zip allRows (map snd rendered), isHeader row]
    bodyLines = [ls | (row, ls) <- zip allRows (map snd rendered), not (isHeader row)]

    middleRule = rule (middleLeft chars) (middle chars) (middleRight chars) (topEdge chars)
    body = intercalate [middleRule | tableRowRules t] bodyLines
    allLines =
      [rule (topLeftCorner chars) (middleTop chars) (topRightCorner chars) (topEdge chars) | edgeTop edges]
        ++ headerLines
        ++ [middleRule | tableHeaderRule t, not (null headerLines), not (null bodyLines)]
        ++ body
        ++ [rule (bottomLeftCorner chars) (middleBottom chars) (bottomRightCorner chars) (bottomEdge chars) | edgeBottom edges]


-- | Make column widths add up to a target: shrink the widest column one
-- step at a time (no column goes below 1), or share out the extra space
-- from the left.
fitWidths :: Int -> [Int] -> [Int]
fitWidths target ws
  | total > target && any (> 1) ws = fitWidths target (decrementWidest ws)
  | total < target && not (null ws) =
      let (q, r) = (target - total) `divMod` length ws
       in [x + q + (if i < r then 1 else 0) | (i, x) <- zip [0 :: Int ..] ws]
  | otherwise = ws
  where
    total = sum ws
    decrementWidest xs = case break (== maximum xs) xs of
      (a, x : b) -> a ++ (x - 1) : b
      (a, []) -> a
