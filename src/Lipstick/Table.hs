-- | Tables.
--
-- > table
-- >   (  border roundedBorder <> borderFg purple
-- >   <> headerStyle (bold <> align center)
-- >   <> cellStyle (\c -> padding 0 1 <> if maybe False even (cellRow c) then fg grey else mempty)
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
import Data.Maybe (isJust)
import Data.Text qualified as T
import Lipstick.Border
import Lipstick.Internal.Grid
import Lipstick.Internal.Layout (Assign (..), Block (..), Request (..), allocate, cropLayoutWidth, takenWidth)
import Lipstick.Internal.Types (Attrs (..))
import Lipstick.Mod
import Lipstick.Style (Style, render)
import Lipstick.Width (textWidth)

-- | Where a cell is: in the header, or in a body row (counting from 0).
data TableCell = HeaderCell !Int | BodyCell !Int !Int
  deriving (Eq, Show)

-- | The body row of a cell, or 'Nothing' for the header.
cellRow :: TableCell -> Maybe Int
cellRow = \case
  HeaderCell _ -> Nothing
  BodyCell r _ -> Just r

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

-- | Without a 'width', columns are as wide as their cells need, and shrink
-- (wrapping their text) when the table is laid out in less space: widest
-- first, but not below their longest word while another column can still
-- wrap; with 'fillWidth', they grow to fill it. 'width' is the total width
-- of the table: columns shrink or grow to fit it. 'height' is the most lines the table takes: body
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
-- are padded with empty cells. Its columns fit the width the table is
-- laid out in (see 'HasSize').
table :: Mod Table -> [Inline] -> [[Inline]] -> Block
table m headers rows = Block req (\(Assign w _) -> let (g, _) = run w in (g, gridHeight g))
  where
    (req, run) = tableLayout m headers rows

-- | The first and last body rows a table shows, laid out in an available
-- width ('Nothing' for its natural width), as for 'Lipstick.Block.layout'.
-- The last is one less than the first when none are shown. All rows unless
-- the table has a 'height'.
visibleRows :: Maybe Int -> Mod Table -> [Inline] -> [[Inline]] -> (Int, Int)
visibleRows avail m headers rows = snd (run (maybe (reqMax req) (takenWidth req) avail))
  where
    (req, run) = tableLayout m headers rows

-- | What a table asks for, and how it lays out at a width, with the body
-- rows it shows.
tableLayout :: Mod Table -> [Inline] -> [[Inline]] -> (Request, Int -> (Grid, (Int, Int)))
tableLayout m headers rows
  | ncols == 0 = (Request 0 0 0 Nothing False, \w -> (Grid w 0 [], (0, -1)))
  | otherwise = (req, run)
  where
    (t, props) = runMod m defTable
    (chars, edges) = borderEdges (tableBorder t)
    bp = tableBorder t
    sideAttrs side = mempty {attrFg = side (borderFgColors bp), attrBg = side (borderBgColors bp)}
    -- Outer edges take their side's colours; inner rules take the top's.
    ba = sideAttrs sideTop
    Size wSet hgt weight _ = tableSize t

    ncols = maximum (0 : length headers : map length rows)
    padRow r = take ncols (r ++ repeat mempty)
    headerRow = [(HeaderCell, padRow headers) | not (null headers)]
    bodyRows = [(BodyCell i, padRow r) | (i, r) <- zip [0 ..] rows]

    -- Cells stretch to the height of their row. Header cells, and body
    -- cells with noWrap, are cut with an ellipsis instead of wrapped.
    truncates pos = case pos of
      HeaderCell _ -> True
      BodyCell _ _ -> not (tableWrap t)
    cellBlock pos = render (tableCellStyle t pos <> stretchHeight <> if truncates pos then noWrap else mempty)
    cellBlocks (mk, r) = [cellBlock (mk c) inl | (c, inl) <- zip [0 ..] r]

    -- Column widths: the natural width, and the narrowest that breaks no
    -- word (cells that are cut short can shrink to one column).
    columns = transpose [[(pos, blockRequest b) | (pos, b) <- zip (map mk [0 ..]) (cellBlocks row)] | row@(mk, _) <- headerRow ++ bodyRows]
    natural = [maximum (0 : [reqMax r | (_, r) <- col]) | col <- columns]
    narrowest = [maximum (0 : [if truncates pos then min 1 (reqMax r) else reqMin r | (pos, r) <- col]) | col <- columns]
    sideW f e = if f edges then max 1 (textWidth e) else 0
    ruleW = if tableColumnRules t then max 1 (textWidth (leftEdge chars)) else 0
    frameW = sideW edgeLeft (leftEdge chars) + sideW edgeRight (rightEdge chars) + ruleW * (ncols - 1)
    smallest = sum narrowest + frameW
    full = sum natural + frameW
    capW x = maybe x (min x . max 0) (propMaxWidth props)
    -- The narrowest and natural widths, before a maxWidth crop.
    (lo, hi) = case wSet of
      Just n
        | tableFitContent t -> (min n smallest, min n full)
        | otherwise -> (n, n)
      Nothing -> (smallest, full)
    req = Request (capW lo) (capW hi) weight (max 0 <$> propMaxWidth props) False
    -- Columns grow to a set width (unless they fit their content) or when
    -- the table grows. They shrink widest first, but not below their
    -- longest word while another column can still wrap; then further,
    -- widest first, down to one column.
    columnWeight = if tableFitContent t || (not (isJust wSet) && weight == 0) then 0 else 1

    run w = (fitWidth w (withProps props (maybe allLines (`take` allLines) hgt)), (firstV, lastV))
      where
        tableW = cropLayoutWidth (propMaxWidth props) weight hi w
        widths = allocate (tableW - frameW) [(colLo, colHi, columnWeight) | (colLo, colHi) <- zip narrowest natural]

        -- A row's cells at their column widths, stretched to the tallest:
        -- one layout per cell. Its natural height is the number of lines.
        renderRow row =
          let results = [blockLayout b (Assign cw rowH) | (b, cw) <- zip (cellBlocks row) widths]
              rowH = maximum (1 : map snd results)
           in map line (transpose [gridCells g | (g, _) <- results])

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

        headerLines = concatMap renderRow headerRow
        nBody = length rows
        bodyLines = map renderRow bodyRows
        overflowRow i = renderRow (BodyCell i, replicate ncols (plain (T.singleton '…')))

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
        shown = take (lastV - firstV + 1) (drop firstV bodyLines)
        overflow = [overflowRow (lastV + 1) | lastV < nBody - 1]
        body = intercalate [middleRule | tableRowRules t] shown ++ concat [[middleRule | tableRowRules t, not (null shown)] ++ o | o <- overflow]
        allLines = topLines ++ headerLines ++ headerRuleLines ++ body ++ bottomLines
