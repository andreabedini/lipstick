-- | The representation of rendered output. Not part of the stable API.
--
-- A 'Grid' is a rectangle: every line is exactly 'gridWidth' columns
-- wide and there are exactly 'gridHeight' lines. Layout code works on
-- lines of 'Cell's (one cluster each) and packs them back into 'Span's.
module Lipstick.Internal.Grid
  ( -- * Grids
    Grid (..)
  , Line (..)
  , Span (..)
  , emptyGrid
  , fromCellLines
  , gridCells

    -- * Cells
  , Cell (..)
  , cellsWidth
  , lineCells
  , cellsLine
  , blankCells
  , fillCells
  , alignCells
  , cropCells
  , wrapCells
  , wrapCellsWith
  , truncateCells
  , overCellAttrs

    -- * Inline text
  , Inline (..)
  , plain
  , inlineLines
  , styledClusters
  ) where

import Data.Function (on)
import Data.List (groupBy)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Lipstick.Internal.Types
import Lipstick.Width (clusters)

data Grid = Grid
  { gridWidth :: !Int
  , gridHeight :: !Int
  , gridLines :: ![Line]
  }
  deriving (Eq, Show)

data Line = Line
  { lineWidth :: !Int
  , lineSpans :: ![Span]
  }
  deriving (Eq, Show)

-- | A run of clusters with the same attributes. The text contains no
-- newlines or control characters.
data Span = Span
  { spanAttrs :: !Attrs
  , spanText :: !Text
  , spanWidth :: !Int
  }
  deriving (Eq, Show)

-- | One cluster occupying 'cellWidth' columns.
data Cell = Cell
  { cellAttrs :: !Attrs
  , cellText :: !Text
  , cellWidth :: !Int
  }
  deriving (Eq, Show)

-- | Unstyled text; lines are split on newlines.
instance IsString Grid where
  fromString = fromCellLines . inlineLines 4 . plain . T.pack

-- | The block with no lines.
emptyGrid :: Grid
emptyGrid = Grid 0 0 []

-- | Build a block, padding lines on the right to the widest one.
fromCellLines :: [[Cell]] -> Grid
fromCellLines ls =
  let w = maximum (0 : map cellsWidth ls)
   in Grid w (length ls) [cellsLine (alignCells mempty left w l) | l <- ls]

gridCells :: Grid -> [[Cell]]
gridCells = map lineCells . gridLines

cellsWidth :: [Cell] -> Int
cellsWidth = sum . map cellWidth

lineCells :: Line -> [Cell]
lineCells = concatMap spanCells . lineSpans
  where
    spanCells (Span a t _) = [Cell a c w | (c, w) <- clusters t]

cellsLine :: [Cell] -> Line
cellsLine cs = Line (cellsWidth cs) (map toSpan (groupBy ((==) `on` cellAttrs) cs))
  where
    toSpan grp = case grp of
      c : _ -> Span (cellAttrs c) (T.concat (map cellText grp)) (cellsWidth grp)
      [] -> Span mempty T.empty 0

blankCells :: Attrs -> Int -> [Cell]
blankCells a n = replicate n (Cell a (T.singleton ' ') 1)

-- | Exactly @n@ columns made by repeating the clusters of a text. Clusters
-- that would overflow, and an empty text, give spaces instead.
fillCells :: Attrs -> Text -> Int -> [Cell]
fillCells a t n = case filter ((> 0) . snd) (clusters t) of
  [] -> blankCells a n
  cs -> go n (cycle cs)
  where
    go k ((c, w) : rest)
      | k <= 0 = []
      | w <= k = Cell a c w : go (k - w) rest
    go k _ = blankCells a k

-- | Pad a line with blanks to @n@ columns, placing the content at a
-- position. Lines already at least @n@ wide are unchanged.
alignCells :: Attrs -> Position -> Int -> [Cell] -> [Cell]
alignCells a p n cs =
  let (before, after) = splitExtra p (n - cellsWidth cs)
   in blankCells a before ++ cs ++ blankCells a after

-- | Keep at most @n@ columns. A wide cluster cut by the edge is replaced by
-- spaces, so the result is exactly @min n (cellsWidth cs)@ wide.
cropCells :: Int -> [Cell] -> [Cell]
cropCells n cs
  | n <= 0 = []
  | otherwise = case cs of
      [] -> []
      c : rest
        | cellWidth c <= n -> c : cropCells (n - cellWidth c) rest
        | otherwise -> blankCells (cellAttrs c) n

-- | Greedy word wrap to @w@ columns. Spaces at a break are dropped; words
-- longer than a line are broken. Leading spaces are kept as indentation.
wrapCells :: Int -> [Cell] -> [[Cell]]
wrapCells = wrapCellsWith []

-- | Like 'wrapCells', also allowing a break after any of the given
-- characters (such as @-@).
wrapCellsWith :: [Char] -> Int -> [Cell] -> [[Cell]]
wrapCellsWith breakpoints w0 cells
  | cellsWidth cells <= w = [cells]
  | otherwise = go [] 0 (tokens cells)
  where
    w = max 1 w0
    isSpaceCell c = cellText c == T.singleton ' '
    tokens = concatMap splitAfterBreaks . groupBy ((==) `on` isSpaceCell)
    isBreak c = T.length (cellText c) == 1 && T.head (cellText c) `elem` breakpoints
    splitAfterBreaks tok
      | all isSpaceCell tok = [tok]
      | otherwise = case break isBreak tok of
          (a, b : rest) | not (null rest) -> (a ++ [b]) : splitAfterBreaks rest
          _ -> [tok]
    isSpaceTok = all isSpaceCell

    -- acc holds the current line's tokens in reverse.
    go acc _ [] = [concat (reverse acc)]
    go acc accW (t : ts)
      | isSpaceTok t = case ts of
          [] -> [concat (reverse acc)]
          wd : ts' -> place acc accW t wd ts'
      | otherwise = place acc accW [] t ts

    place acc accW sp wd ts
      | accW + cellsWidth sp + cellsWidth wd <= w = go (wd : sp : acc) (accW + cellsWidth sp + cellsWidth wd) ts
      | null acc = hard (sp ++ wd) ts
      | otherwise = concat (reverse acc) : hard wd ts

    -- Start a new line with a word, breaking it if it is too long.
    hard wd ts =
      let (full, lastPart) = chunks wd
       in full ++ go [lastPart] (cellsWidth lastPart) ts

    chunks cs =
      let (line, rest) = takeWidth w cs
       in if null rest then ([], line) else let (more, l) = chunks rest in (line : more, l)

    takeWidth n cs = case cs of
      c : rest
        | cellWidth c <= n -> let (a, b) = takeWidth (n - cellWidth c) rest in (c : a, b)
        | n == w -> ([c], rest) -- a cluster wider than the line: take it anyway
      _ -> ([], cs)

-- | Cut a line to at most @w@ columns, ending with an ellipsis when
-- something was cut.
truncateCells :: Int -> [Cell] -> [Cell]
truncateCells w cs
  | cellsWidth cs <= w = cs
  | w <= 0 = []
  | otherwise =
      let kept = cropCells (w - 1) cs
          a = case reverse kept of
            c : _ -> cellAttrs c
            [] -> maybe mempty cellAttrs (safeHead cs)
       in kept ++ [Cell a (T.singleton '…') 1]
  where
    safeHead xs = case xs of
      x : _ -> Just x
      [] -> Nothing

overCellAttrs :: (Attrs -> Attrs) -> [[Cell]] -> [[Cell]]
overCellAttrs f = map (map (\c -> c {cellAttrs = f (cellAttrs c)}))

-- | Text with inline styling, possibly spanning several lines.
newtype Inline = Inline [(Attrs, Text)]
  deriving (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance IsString Inline where
  fromString = plain . T.pack

plain :: Text -> Inline
plain t = Inline [(mempty, t)]

-- | Split inline text into lines of cells. Tabs become @tabWidth@ spaces;
-- other control characters, including escape sequences' ESC, are dropped.
-- Use 'Lipstick.Ansi.fromAnsi' for text that contains escape sequences.
inlineLines :: Int -> Inline -> [[Cell]]
inlineLines tabWidth (Inline segs) = splitLines (styledClusters [(a, sanitise t) | (a, t) <- segs])
  where
    sanitise = T.filter isKept . T.replace (T.singleton '\t') (T.replicate tabWidth (T.singleton ' '))
    -- Drop C0 and C1 controls (and DEL): they could start escape sequences.
    isKept c = c == '\n' || (c >= ' ' && (c < '\DEL' || c > '\x9F'))
    isNewline (_, c, _) = c == T.singleton '\n'
    cells cs = [Cell a c w | (a, c, w) <- cs]
    splitLines xs = case break isNewline xs of
      (cs, []) -> [cells cs]
      (cs, _ : rest) -> cells cs : splitLines rest

-- | The grapheme clusters of styled pieces of text, each with its width
-- and the attributes of the piece its first character is in. Clusters are
-- found in the text as a whole, as the terminal sees it, so one can span
-- pieces: an emoji and its variation selector, say, styled separately.
styledClusters :: [(Attrs, Text)] -> [(Attrs, Text, Int)]
styledClusters segs =
  go [(a, T.length t) | (a, t) <- segs, not (T.null t)] (clusters (T.concat (map snd segs)))
  where
    -- The pieces still to cover, with how many characters are left of the
    -- first.
    go pieces@((a, _) : _) ((c, w) : cs) = (a, c, w) : go (consume (T.length c) pieces) cs
    go _ _ = []
    consume k pieces = case pieces of
      (a, n) : rest
        | k < n -> (a, n - k) : rest
        | otherwise -> consume (k - n) rest
      [] -> []
