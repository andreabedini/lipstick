-- | Laying out blocks at a width. Not part of the stable API.
module Lipstick.Internal.Layout
  ( allocate
  ) where

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
allocate space children
  | sum his <= w = grow
  | sum los <= w = level los his
  | otherwise = level floors los
  where
    w = max 0 space
    los = [max 0 lo | (lo, _, _) <- children]
    his = [max lo hi | (lo, (_, hi, _)) <- zip los children]
    weights = [max 0 k | (_, _, k) <- children]
    floors = map (min 1) los

    grow
      | totalWeight == 0 = his
      | otherwise =
          let shares = [extra * k `div` totalWeight | k <- weights]
              spare = extra - sum shares
           in zipWith3 (\hi s b -> hi + s + b) his shares (ones spare (map (> 0) weights))
      where
        extra = w - sum his
        totalWeight = sum weights

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
