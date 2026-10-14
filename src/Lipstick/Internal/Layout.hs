-- | Laying out blocks at a width. Not part of the stable API.
--
-- A 'Block' is first given the text settings it inherits ('TextStyle'),
-- which can change what it measures, and becomes a 'Measured'. A tree of
-- nodes is laid out in one pass that goes down the tree and back up.
-- Going up, every node reports a 'Request': its smallest and natural
-- width, which depend only on its content, and, from 'blockLayout', its
-- natural height at the width it was given. Going down, a parent shares
-- its width among its children with 'allocate' and tells children that
-- stretch the height of their row, which it knows only once the same
-- calls have reported their natural heights. Laziness resolves the
-- apparent circle (see jaspervdj's "Lazy Layout"), as long as
--
-- * a 'Request' never depends on the 'Assign'ment, and
-- * the natural height never depends on 'assignHeight'.
--
-- So the fields of 'Request' and 'Assign', and the result pair, must stay
-- lazy.
module Lipstick.Internal.Layout
  ( -- * Blocks
    Block (..)
  , Measured (..)
  , measureIn
  , measure
  , TextStyle (..)
  , defTextStyle
  , Request (..)
  , Assign (..)
  , layout
  , layoutIn
  , fromGrid
  , emptyBlock
  , textBlock
  , cropLayoutWidth
  , takenWidth
  , addLimits
  , maxLimit
  , usableWidth

    -- * Sharing out width
  , allocate
  , allocateWithin
  ) where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Lipstick.Internal.Grid
import Lipstick.Internal.Types (Position, left)

-- | Styled content that can be laid out at any width, once it knows the
-- text settings it inherits.
newtype Block = Block (TextStyle -> Measured)

-- | A block with its inherited text settings applied.
data Measured = Measured
  { blockRequest :: Request
  , blockLayout :: Assign -> (Grid, Int)
  -- ^ the grid, exactly 'assignWidth' wide, and the natural height at
  -- that width (the grid is taller only when the block stretches)
  }

-- | Text settings that nested text inherits, as in CSS: set on a style,
-- they apply to all the text inside what it frames, unless something
-- nearer sets them again. They are known before anything is measured,
-- since some of them change widths.
data TextStyle = TextStyle
  { textAlign :: !Position
  , textWrap :: !Bool
  -- ^ wrap lines, or cut them short with an ellipsis
  , textBreaks :: ![Char]
  -- ^ characters to also break lines after
  , textTabWidth :: !Int
  , textTransform :: !(Maybe (Text -> Text))
  , textInline :: !Bool
  -- ^ newlines removed, never wrapped
  }

-- | Left-aligned, wrapping text with four-column tabs.
defTextStyle :: TextStyle
defTextStyle = TextStyle left True [] 4 Nothing False

-- | A block in inherited text settings.
measureIn :: TextStyle -> Block -> Measured
measureIn ctx (Block f) = f ctx

-- | A block in the default text settings.
measure :: Block -> Measured
measure = measureIn defTextStyle

-- | What a block asks of its parent. Lazy, and independent of the
-- assignment.
data Request = Request
  { reqMin :: Int
  -- ^ the narrowest width that breaks no words
  , reqMax :: Int
  -- ^ the natural width: nothing wrapped
  , reqGrow :: Int
  -- ^ the weight for spare width; 0 for none
  , reqLimit :: Maybe Int
  -- ^ the most it can use when it grows; 'Nothing' for no limit
  , reqStretch :: Bool
  -- ^ whether the block grows to the height of its row
  , reqFill :: Bool
  -- ^ whether it takes the whole width of a box it is framed in, so the
  -- box's alignment applies to its lines (text), rather than what it
  -- needs, the box aligning it as a whole (boxes, rows)
  }

-- | What a parent gives a block. Lazy: 'assignHeight' can depend on the
-- natural heights the same layout reports.
data Assign = Assign
  { assignWidth :: Int
  , assignHeight :: Int
  -- ^ the height of the row, for blocks that stretch
  }

-- | Unstyled text, word-wrapped to the width it is given.
instance IsString Block where
  fromString = textBlock . plain . T.pack

-- | Lay a block out in an available width: its natural width when there is
-- no limit, and otherwise as much as it needs (all of it if it grows).
layout :: Maybe Int -> Block -> Grid
layout = layoutIn defTextStyle

-- | Like 'layout', in inherited text settings.
layoutIn :: TextStyle -> Maybe Int -> Block -> Grid
layoutIn ctx avail b = fst (blockLayout n (Assign w 0))
  where
    n = measureIn ctx b
    r = blockRequest n
    w = maybe (reqMax r) (takenWidth r) avail

-- | How much of an available width a block takes, when it does not have to
-- shrink: its natural width, or as much as it can use if it grows.
takenWidth :: Request -> Int -> Int
takenWidth r avail
  | reqGrow r > 0 = maybe a (min a . max (reqMax r)) (reqLimit r)
  | otherwise = min a (reqMax r)
  where
    a = max 0 avail

-- | The most a row of blocks can use: the sum of what each can.
addLimits :: [Request] -> Maybe Int
addLimits rs = sum <$> traverse usableWidth rs

-- | The most a column of blocks can use: what the widest can.
maxLimit :: [Request] -> Maybe Int
maxLimit rs = maximum . (0 :) <$> traverse usableWidth rs

-- | The most a block can use, at most its natural width if it does not
-- grow.
usableWidth :: Request -> Maybe Int
usableWidth r
  | reqGrow r > 0 = max (reqMax r) <$> reqLimit r
  | otherwise = Just (reqMax r)

-- | A grid as a block: cropped or padded to the width it is given. It
-- inherits nothing.
fromGrid :: Grid -> Block
fromGrid g =
  Block . const $
    Measured
      (Request (gridWidth g) (gridWidth g) 0 Nothing False False)
      (\(Assign w _) -> (fitWidth w g, gridHeight g))

-- | The block with no lines.
emptyBlock :: Block
emptyBlock = fromGrid emptyGrid

-- | Inline text, laid out in the text settings it inherits: word-wrapped
-- to the width it is given (or cut short with an ellipsis), each line
-- aligned in it.
textBlock :: Inline -> Block
textBlock inl = Block $ \ctx ->
  let transformed = maybe inl (\f -> let Inline segs = inl in Inline [(a, f t) | (a, t) <- segs]) (textTransform ctx)
      ls0 = inlineLines (textTabWidth ctx) transformed
      ls = if textInline ctx then [concat ls0] else ls0
      hi = maximum (0 : map cellsWidth ls)
      lo
        | textWrap ctx && not (textInline ctx) = maximum (0 : concatMap (wordWidths (textBreaks ctx)) ls)
        | otherwise = hi
      run (Assign w _) =
        let fitted
              | textInline ctx = map (cropCells w) ls
              | textWrap ctx = concatMap (wrapCellsWith (textBreaks ctx) w) ls
              | otherwise = map (truncateCells w) ls
         in (fitWidth w (fromCellLines (map (alignText (textAlign ctx) w) fitted)), length fitted)
   in Measured (Request lo hi 0 Nothing False True) run

-- | The width to lay out something with a 'Lipstick.Mod.maxWidth' crop at,
-- before cropping, given the crop, its weight for spare width, its natural
-- width and the width it is given. When the crop is what limits it, that
-- is its natural width, so the crop cuts instead of making text wrap; or,
-- for something that grows, the crop width, so it fills up to it.
cropLayoutWidth :: Maybe Int -> Int -> Int -> Int -> Int
cropLayoutWidth crop weight natural w = case crop of
  Just c
    | w >= max 0 c -> if weight > 0 then max 0 c else max w natural
  _ -> w

-- | Share out the columns of a row among its children, given each child's
-- smallest and natural width and its weight for spare space.
--
-- * When every child fits at its natural width, each gets it, and the
--   rest goes to the children with a weight, in proportion (left ones
--   first for what does not divide evenly). Without weights the row is
--   narrower than the space.
--
-- * Otherwise the widest children shrink first, towards their smallest
--   width: every child ends up at its natural width or at a common level,
--   whichever is less.
--
-- * When even the smallest widths do not fit, the widest children shrink
--   further, down to one column (zero for children with nothing in them),
--   and the result is wider than the space only if that is still too much.
--
-- The widths add up to the space whenever the bounds allow it.
allocate :: Int -> [(Int, Int, Int)] -> [Int]
allocate space children = allocateWithin space [(lo, hi, k, Nothing) | (lo, hi, k) <- children]

-- | Like 'allocate', with the most each child can use when it grows
-- ('Nothing' for no limit): what a child cannot use goes to the others.
allocateWithin :: Int -> [(Int, Int, Int, Maybe Int)] -> [Int]
allocateWithin space children
  | sum his <= w = growWithin (w - sum his) [(hi, max 0 k, l) | (hi, (_, _, k, l)) <- zip his children]
  | sum los <= w = level los his
  | otherwise = level floors los
  where
    w = max 0 space
    los = [max 0 lo | (lo, _, _, _) <- children]
    his = [max lo hi | (lo, (_, hi, _, _)) <- zip los children]
    floors = map (min 1) los

    -- Share out spare columns by weight. Children that reach their limit
    -- keep it and drop out, and the rest is shared again.
    growWithin extra cs
      | extra <= 0 || total == 0 = [x | (x, _, _) <- cs]
      | or full = growWithin (extra - sum used) [if f then (x + u, 0, l) else c | (c@(x, _, l), f, u) <- zip3 cs full used]
      | otherwise = zipWith3 (\(x, _, _) sh b -> x + sh + b) cs shares (ones (extra - sum shares) (map open cs))
      where
        open (x, k, l) = k > 0 && maybe True (> x) l
        total = sum [k | c@(_, k, _) <- cs, open c]
        shares = [if open c then extra * k `div` total else 0 | c@(_, k, _) <- cs]
        room (x, _, l) = maybe maxBound (subtract x) l
        full = [open c && sh >= room c | (c, sh) <- zip cs shares]
        used = [if f then room c else 0 | (c, f) <- zip cs full]

    -- Every child at @clamp lo hi t@ for the highest level @t@ that fits;
    -- the columns left over go to the children cut at the level, right
    -- ones first (the widest-first order shrinks left ones first).
    level lows highs
      | sum lows >= w = lows
      | otherwise =
          let t = search 0 (maximum (0 : highs))
              ws = [clamp lo hi t | (lo, hi) <- zip lows highs]
              spare = w - sum ws
              cut = [hi > t && lo <= t | (lo, hi) <- zip lows highs]
           in zipWith (+) ws (reverse (ones spare (reverse cut)))
      where
        total t = sum [clamp lo hi t | (lo, hi) <- zip lows highs]
        -- The highest t in [lo, hi] with total t <= w; total lo <= w.
        search lo hi
          | lo >= hi = lo
          | otherwise =
              let mid = (lo + hi + 1) `div` 2
               in if total mid <= w then search mid hi else search lo (mid - 1)

    clamp lo hi = max lo . min hi

    -- 1 at the first n positions that are True, 0 elsewhere.
    ones :: Int -> [Bool] -> [Int]
    ones n bs = case bs of
      b : rest
        | b && n > 0 -> 1 : ones (n - 1) rest
        | otherwise -> 0 : ones n rest
      [] -> []
