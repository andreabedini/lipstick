-- | The representation of rendered output. Not part of the stable API.
--
-- A 'Block' is a rectangle: every line is exactly 'blockWidth' columns
-- wide and there are exactly 'blockHeight' lines. Layout code works on
-- lines of 'Cell's (one cluster each) and packs them back into 'Span's.
module Lipstick.Internal.Block
  ( -- * Blocks
    Block (..)
  , Line (..)
  , Span (..)
  , emptyBlock
  , fromCellLines
  , blockCells

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
  , overCellAttrs

    -- * Inline text
  , Inline (..)
  , plain
  , inlineLines
  ) where

import Data.Function (on)
import Data.List (groupBy)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Lipstick.Internal.Types
import Lipstick.Width (clusters)

data Block = Block
  { blockWidth :: !Int
  , blockHeight :: !Int
  , blockLines :: ![Line]
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
instance IsString Block where
  fromString = fromCellLines . inlineLines 4 . plain . T.pack

-- | The block with no lines.
emptyBlock :: Block
emptyBlock = Block 0 0 []

-- | Build a block, padding lines on the right to the widest one.
fromCellLines :: [[Cell]] -> Block
fromCellLines ls =
  let w = maximum (0 : map cellsWidth ls)
   in Block w (length ls) [cellsLine (alignCells mempty left w l) | l <- ls]

blockCells :: Block -> [[Cell]]
blockCells = map lineCells . blockLines

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
wrapCells w0 cells
  | cellsWidth cells <= w = [cells]
  | otherwise = go [] 0 (tokens cells)
  where
    w = max 1 w0
    isSpaceCell c = cellText c == T.singleton ' '
    tokens = groupBy ((==) `on` isSpaceCell)
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
inlineLines tabWidth (Inline segs) = splitLines (concatMap segment segs)
  where
    segment (a, t) =
      concat
        [ [Right (Cell a c w) | (c, w) <- clusters (sanitise part)] ++ [Left () | not lastPart]
        | (part, lastPart) <- markLast (T.splitOn (T.singleton '\n') t)
        ]
    sanitise = T.filter isPrintable . T.replace (T.singleton '\t') (T.replicate tabWidth (T.singleton ' '))
    -- Drop C0 and C1 controls (and DEL): they could start escape sequences.
    isPrintable c = c >= ' ' && (c < '\DEL' || c > '\x9F')
    markLast xs = zip xs (map (const False) (drop 1 xs) ++ [True])

    splitLines xs = case break (either (const True) (const False)) xs of
      (cs, []) -> [[c | Right c <- cs]]
      (cs, _ : rest) -> [c | Right c <- cs] : splitLines rest
