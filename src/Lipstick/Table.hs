-- | Tables: a header row and body rows of blocks, with borders and rules
-- between them.
--
-- == Examples
--
-- The examples print with 'Lipstick.Terminal.putBlock'; the output is
-- shown without colours and other attributes.
--
-- >>> let header = ["Language", "Formal", "Informal"]
-- >>> let rows = [["Chinese", "您好", "你好"], ["Japanese", "こんにちは", "やあ"], ["Russian", "Здравствуйте", "Привет"]]
-- >>> putBlock (table mempty header rows)
-- ╭────────┬────────────┬────────╮
-- │Language│Formal      │Informal│
-- ├────────┼────────────┼────────┤
-- │Chinese │您好        │你好    │
-- │Japanese│こんにちは  │やあ    │
-- │Russian │Здравствуйте│Привет  │
-- ╰────────┴────────────┴────────╯
--
-- Styled cells, by position:
--
-- >>> let padded = cellStyle (const (padding 0 1))
-- >>> let striped = cellStyle (\c -> if maybe False odd (cellRow c) then faint else mempty)
-- >>> putBlock (table (padded <> striped <> headerStyle (bold <> align center)) header rows)
-- ╭──────────┬──────────────┬──────────╮
-- │ Language │    Formal    │ Informal │
-- ├──────────┼──────────────┼──────────┤
-- │ Chinese  │ 您好         │ 你好     │
-- │ Japanese │ こんにちは   │ やあ     │
-- │ Russian  │ Здравствуйте │ Привет   │
-- ╰──────────┴──────────────┴──────────╯
--
-- Choosing the lines:
--
-- >>> putBlock (table (columnRules False <> rowRules True) header rows)
-- ╭────────────────────────────╮
-- │LanguageFormal      Informal│
-- ├────────────────────────────┤
-- │Chinese 您好        你好    │
-- ├────────────────────────────┤
-- │Japaneseこんにちは  やあ    │
-- ├────────────────────────────┤
-- │Russian ЗдравствуйтеПривет  │
-- ╰────────────────────────────╯
-- >>> putBlock (table (border markdownBorder <> borderTop False <> borderBottom False) header rows)
-- |Language|Formal      |Informal|
-- |--------|------------|--------|
-- |Chinese |您好        |你好    |
-- |Japanese|こんにちは  |やあ    |
-- |Russian |Здравствуйте|Привет  |
--
-- Laid out in less width than it needs, a table shrinks its widest
-- columns: body cells wrap, and header cells are cut short:
--
-- >>> putBlock (fromGrid (layout (Just 28) (table mempty header rows)))
-- ╭────────┬──────────┬──────╮
-- │Language│Formal    │Infor…│
-- ├────────┼──────────┼──────┤
-- │Chinese │您好      │你好  │
-- │Japanese│こんにちは│やあ  │
-- │Russian │Здравствуй│Привет│
-- │        │те        │      │
-- ╰────────┴──────────┴──────╯
--
-- With a 'maxHeight', rows that do not fit are replaced by a row of
-- ellipses, and 'yOffset' scrolls:
--
-- >>> let numbers = [[text (T.pack (show n)), text (T.pack (show (n * n)))] | n <- [1 .. 20 :: Int]]
-- >>> putBlock (table (maxHeight 8 <> yOffset 5) ["n", "n²"] numbers)
-- ╭──┬───╮
-- │n │n² │
-- ├──┼───┤
-- │6 │36 │
-- │7 │49 │
-- │8 │64 │
-- │… │…  │
-- ╰──┴───╯
-- >>> visibleRows Nothing (maxHeight 8 <> yOffset 5) ["n", "n²"] numbers
-- Just (5,7)
--
-- Cells are blocks, so they can hold anything:
--
-- >>> putBlock (table mempty ["Tool", "Platforms"] [["GHC", list mempty ["Linux", "macOS", "Windows"]]])
-- ╭────┬─────────╮
-- │Tool│Platforms│
-- ├────┼─────────┤
-- │GHC │• Linux  │
-- │    │• macOS  │
-- │    │• Windows│
-- ╰────┴─────────╯
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

import Data.Array (Array, listArray, (!))
import Data.List (intercalate, transpose)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Lipstick.Border
import Lipstick.Internal.Grid
import Lipstick.Internal.Layout (Assign (..), Block (..), Measured (..), Request (..), TextStyle, allocate, cropLayoutWidth, defTextStyle, measureIn, takenWidth, textBlock)
import Lipstick.Internal.Types (Attrs (..))
import Lipstick.Internal.Mod
import Lipstick.Style (Style, frame)
import Lipstick.Width (textWidth)

-- $setup
-- >>> import Lipstick
-- >>> import Data.Text qualified as T

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
-- of the table: columns shrink or grow to fit it. 'height' is the least
-- number of lines the table takes: rows of empty cells are added after the
-- body to reach it.
--
-- 'maxHeight' is the most lines the table takes: rather than being cut
-- off, body rows that do not fit are replaced by a row of ellipses,
-- starting from the row set by 'yOffset'.
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

-- | The first body row shown when the table has a 'maxHeight' (default
-- 0). When the rows from there on do not fill the height, earlier rows are
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
table :: Mod Table -> [Block] -> [[Block]] -> Block
table m headers rows = Block $ \ctx ->
  let (req, run) = tableLayout ctx m headers rows
   in Measured req (\(Assign w _) -> let (g, _) = run w in (g, gridHeight g))

-- | The first and last body rows a table shows, laid out in an available
-- width ('Nothing' for its natural width), as for 'Lipstick.Block.layout';
-- 'Nothing' when it shows none. All rows unless the table has a
-- 'maxHeight'.
visibleRows :: Maybe Int -> Mod Table -> [Block] -> [[Block]] -> Maybe (Int, Int)
visibleRows avail m headers rows = snd (run (maybe (reqMax req) (takenWidth req) avail))
  where
    (req, run) = tableLayout defTextStyle m headers rows

-- | What a table asks for, and how it lays out at a width, with the body
-- rows it shows.
tableLayout :: TextStyle -> Mod Table -> [Block] -> [[Block]] -> (Request, Int -> (Grid, Maybe (Int, Int)))
tableLayout ctx m headers rows
  | ncols == 0 = (Request 0 0 0 Nothing False False, \w -> (Grid w 0 [], Nothing))
  | otherwise = (req, run)
  where
    (t, props) = runMod m defTable
    (chars, edges) = borderEdges (tableBorder t)
    bp = tableBorder t
    sideAttrs side = mempty {attrFg = side (borderFgColors bp), attrBg = side (borderBgColors bp)}
    -- Outer edges take their side's colours; inner rules take the top's.
    ba = sideAttrs sideTop
    Size wSet hMin weight _ = tableSize t
    -- At most this many lines: body rows that do not fit are left out.
    hgt = propMaxHeight props

    ncols = maximum (0 : length headers : map length rows)
    padRow r = take ncols (r ++ repeat (textBlock mempty))
    headerRow = [(HeaderCell, padRow headers) | not (null headers)]
    bodyRows = [(BodyCell i, padRow r) | (i, r) <- zip [0 ..] rows]

    -- Cells stretch to the height of their row. Header cells, and body
    -- cells with noWrap, are cut with an ellipsis instead of wrapped.
    truncates pos = case pos of
      HeaderCell _ -> True
      BodyCell _ _ -> not (tableWrap t)
    cellNodes (mk, r) =
      [ measureIn ctx (frame (tableCellStyle t pos <> stretchHeight <> if truncates pos then noWrap else mempty) b)
      | (c, b) <- zip [0 ..] r
      , let pos = mk c
      ]
    -- Each cell's node is made once, for measuring and for layout.
    headerNodes = map cellNodes headerRow
    bodyNodes = map cellNodes bodyRows

    -- Column widths: the natural width, and the narrowest that breaks no
    -- word (cells that are cut short can shrink to one column).
    columns = transpose [[(mk c, blockRequest n) | (c, n) <- zip [0 ..] ns] | ((mk, _), ns) <- zip (headerRow ++ bodyRows) (headerNodes ++ bodyNodes)]
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
    req = Request (capW lo) (capW hi) weight (max 0 <$> propMaxWidth props) False False
    -- Columns grow to a set width (unless they fit their content) or when
    -- the table grows. They shrink widest first, but not below their
    -- longest word while another column can still wrap; then further,
    -- widest first, down to one column.
    columnWeight = if tableFitContent t || (not (isJust wSet) && weight == 0) then 0 else 1

    run w = (fitWidth w (withProps props (maybe allLines (`take` allLines) hgt)), shownRange)
      where
        tableW = cropLayoutWidth (propMaxWidth props) weight hi w
        widths = allocate (tableW - frameW) [(colLo, colHi, columnWeight) | (colLo, colHi) <- zip narrowest natural]

        -- A row's cells at their column widths, stretched to the tallest:
        -- one layout per cell. Its natural height is the number of lines.
        renderRow ns =
          let results = [blockLayout n (Assign cw rowH) | (n, cw) <- zip ns widths]
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

        headerLines = concatMap renderRow headerNodes
        nBody = length rows
        bodyLines = map renderRow bodyNodes
        overflowRow i = renderRow (cellNodes (BodyCell i, replicate ncols (textBlock (plain (T.singleton '…')))))

        middleRule = rule ba (middleLeft chars) (middle chars) (middleRight chars) (topEdge chars)
        topLines = [rule ba (topLeftCorner chars) (middleTop chars) (topRightCorner chars) (topEdge chars) | edgeTop edges]
        headerRuleLines = [middleRule | tableHeaderRule t, not (null headerLines), nBody > 0]
        bottomLines = [rule (sideAttrs sideBottom) (bottomLeftCorner chars) (middleBottom chars) (bottomRightCorner chars) (bottomEdge chars) | edgeBottom edges]

        -- Which body rows to show.
        rr = if tableRowRules t then 1 else 0
        -- Lines taken by rows before each row, and by the overflow row in
        -- place of each row (made when first needed, then kept).
        before = listArray (0, nBody) (scanl (+) 0 (map length bodyLines)) :: Array Int Int
        overflowH = listArray (0, nBody) [length (overflowRow i) | i <- [0 .. nBody]] :: Array Int Int
        -- Lines taken by rows f..l, plus an overflow row if rows remain.
        linesFor f l =
          before ! (l + 1) - before ! f
            + rr * max 0 (l - f)
            + (if l < nBody - 1 then (if l >= f then rr else 0) + overflowH ! (l + 1) else 0)
        (firstV, lastV) = case hgt of
          Nothing -> (0, nBody - 1)
          Just h ->
            let avail = h - length topLines - length headerLines - length headerRuleLines - length bottomLines
                f0 = max 0 (min (nBody - 1) (tableYOffset t))
                l0
                  -- All the rows from f0 fit: no overflow row is needed.
                  | linesFor f0 (nBody - 1) <= avail = nBody - 1
                  | otherwise = last (f0 - 1 : takeWhile (\l -> linesFor f0 l <= avail) [f0 .. nBody - 1])
                f1 =
                  if l0 == nBody - 1
                    then last (f0 : takeWhile (\f -> linesFor f l0 <= avail) [f0 - 1, f0 - 2 .. 0])
                    else f0
             in (f1, l0)
        shownRange = if lastV >= firstV then Just (firstV, lastV) else Nothing
        shown = take (lastV - firstV + 1) (drop firstV bodyLines)
        overflow = [overflowRow (lastV + 1) | lastV < nBody - 1]
        body = intercalate [middleRule | tableRowRules t] shown ++ concat [[middleRule | tableRowRules t, not (null shown)] ++ o | o <- overflow]
        -- Up to the minimum height, lines of empty cells after the body.
        drawn = length topLines + length headerLines + length headerRuleLines + length body + length bottomLines
        emptyLine = take 1 (renderRow (cellNodes (BodyCell nBody, replicate ncols (textBlock mempty))))
        filler = concat (replicate (maybe 0 (subtract drawn) hMin) emptyLine)
        allLines = topLines ++ headerLines ++ headerRuleLines ++ body ++ filler ++ bottomLines
