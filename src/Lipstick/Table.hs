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
  , yOffset
  , fitContent
  , visibleRows
  ) where

import Data.List (intercalate, transpose)
import Data.Text qualified as T
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
  , tableYOffset :: !Int
  , tableFitContent :: !Bool
  }

instance HasBorder Table where
  overBorder f t = t {tableBorder = f (tableBorder t)}

-- | 'width' is the total width of the table: columns shrink (wrapping their
-- text) or grow to fit. 'height' is the most lines the table takes: body
-- rows that do not fit are replaced by a row of ellipses, starting from
-- the row set by 'yOffset'.
instance HasSize Table where
  overSize f t = t {tableSize = f (tableSize t)}

-- | With 'noWrap', body cells that are too wide are cut with an ellipsis.
-- Header cells are always cut, never wrapped.
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
    , tableYOffset = 0
    , tableFitContent = False
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

-- | The first body row shown when the table has a 'height' (default 0).
-- When the rows from there on do not fill the height, earlier rows are
-- shown too.
yOffset :: Int -> Mod Table
yOffset n = fieldMod (\t -> t {tableYOffset = max 0 n})

-- | Treat 'width' as a maximum: the table is as wide as its content needs,
-- unless that is wider than 'width'.
fitContent :: Mod Table
fitContent = fieldMod (\t -> t {tableFitContent = True})

-- | A table with a header row (none if empty) and body rows. Short rows
-- are padded with empty cells.
table :: Mod Table -> [Inline] -> [[Inline]] -> Block
table m headers rows = fst (layoutTable m headers rows)

-- | The first and last body rows a table shows (the last is one less than
-- the first when none are shown). All rows unless the table has a
-- 'height'.
visibleRows :: Mod Table -> [Inline] -> [[Inline]] -> (Int, Int)
visibleRows m headers rows = snd (layoutTable m headers rows)

layoutTable :: Mod Table -> [Inline] -> [[Inline]] -> (Block, (Int, Int))
layoutTable m headers rows
  | ncols == 0 = (emptyBlock, (0, -1))
  | otherwise = (withProps props (maybe allLines (`take` allLines) hgt), (firstV, lastV))
  where
    (t, props) = runMod m defTable
    (chars, edges) = borderEdges (tableBorder t)
    bp = tableBorder t
    sideAttrs side = mempty {attrFg = side (borderFgColors bp), attrBg = side (borderBgColors bp)}
    -- Outer edges take their side's colours; inner rules take the top's.
    ba = sideAttrs sideTop
    Size w hgt = tableSize t

    ncols = maximum (0 : length headers : map length rows)
    padRow r = take ncols (r ++ repeat mempty)
    allRows =
      [(HeaderCell, padRow headers) | not (null headers)]
        ++ [(BodyCell i, padRow r) | (i, r) <- zip [0 ..] rows]

    styleAt pos = tableCellStyle t pos <> case pos of
      HeaderCell _ -> noWrap
      BodyCell _ _ -> if tableWrap t then mempty else noWrap
    cellPositions mk = map mk [0 .. ncols - 1]

    -- Column widths.
    natural =
      map
        (maximum . (0 :))
        (transpose [[blockWidth (render (styleAt pos) c) | (pos, c) <- zip (cellPositions mk) r] | (mk, r) <- allRows])
    sideW f e = if f edges then max 1 (textWidth e) else 0
    ruleW = if tableColumnRules t then max 1 (textWidth (leftEdge chars)) else 0
    frameW = sideW edgeLeft (leftEdge chars) + sideW edgeRight (rightEdge chars) + ruleW * (ncols - 1)
    widths = case w of
      Nothing -> natural
      Just total
        | tableFitContent t && sum natural + frameW <= total -> natural
        | otherwise -> fitWidths (total - frameW) natural

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

    piece a txt n = fillCells a txt n
    leftV = [piece (sideAttrs sideLeft) (leftEdge chars) (sideW edgeLeft (leftEdge chars)) | edgeLeft edges]
    rightV = [piece (sideAttrs sideRight) (rightEdge chars) (sideW edgeRight (rightEdge chars)) | edgeRight edges]
    colRule = [piece ba (leftEdge chars) ruleW | tableColumnRules t]
    line cellsOf = concat (leftV ++ [intercalate (concat colRule) cellsOf] ++ rightV)
    rule a l mid r edge =
      concat
        ( [piece a l (sideW edgeLeft (leftEdge chars)) | edgeLeft edges]
            ++ [intercalate (concat [piece a mid ruleW | tableColumnRules t]) [piece a edge cw | cw <- widths]]
            ++ [piece a r (sideW edgeRight (rightEdge chars)) | edgeRight edges]
        )

    headerLines = concat [map line (renderRow row) | row@(mk, _) <- allRows, isHeader mk]
    isHeader mk = case mk 0 of
      HeaderCell _ -> True
      BodyCell _ _ -> False
    nBody = length rows
    bodyRow i = map line (renderRow (BodyCell i, padRow (rows !! i)))
    bodyLines = map bodyRow [0 .. nBody - 1]
    overflowRow i = map line (renderRow (BodyCell i, replicate ncols (plain (T.singleton '…'))))

    middleRule = rule ba (middleLeft chars) (middle chars) (middleRight chars) (topEdge chars)
    topLines = [rule ba (topLeftCorner chars) (middleTop chars) (topRightCorner chars) (topEdge chars) | edgeTop edges]
    headerRuleLines = [middleRule | tableHeaderRule t, not (null headerLines), nBody > 0]
    bottomLines = [rule (sideAttrs sideBottom) (bottomLeftCorner chars) (middleBottom chars) (bottomRightCorner chars) (bottomEdge chars) | edgeBottom edges]

    -- Which body rows to show.
    rr = if tableRowRules t then 1 else 0
    heights = map length bodyLines
    -- Lines taken by rows f..l, plus an overflow row if rows remain.
    linesFor f l =
      sum (take (l - f + 1) (drop f heights))
        + rr * max 0 (l - f)
        + (if l < nBody - 1 then (if l >= f then rr else 0) + length (overflowRow (l + 1)) else 0)
    (firstV, lastV) = case hgt of
      Nothing -> (0, nBody - 1)
      Just h ->
        let avail = h - length topLines - length headerLines - length headerRuleLines - length bottomLines
            f0 = max 0 (min (nBody - 1) (tableYOffset t))
            l0 = last (f0 - 1 : takeWhile (\l -> linesFor f0 l <= avail) [f0 .. nBody - 1])
            f1 =
              if l0 == nBody - 1
                then last (f0 : takeWhile (\f -> linesFor f l0 <= avail) [f0 - 1, f0 - 2 .. 0])
                else f0
         in (f1, l0)
    shown = [bodyLines !! i | i <- [firstV .. lastV]]
    overflow = [overflowRow (lastV + 1) | lastV < nBody - 1]
    body = intercalate [middleRule | tableRowRules t] shown ++ concat [[middleRule | tableRowRules t, not (null shown)] ++ o | o <- overflow]
    allLines = topLines ++ headerLines ++ headerRuleLines ++ body ++ bottomLines


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
