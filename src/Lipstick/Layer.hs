-- | Compositing: blocks stacked on top of each other at any position.
--
-- > compose
-- >   [ layer mempty background
-- >   , layer (offset 10 2 <> zIndex 1 <> layerId "dialog") dialog
-- >   ]
--
-- Layers are drawn from the lowest 'zIndex' up; layers with the same
-- 'zIndex' are drawn in list order, children after their parent. A layer
-- hides what is below it, unless it is 'transparent'. Where a layer covers
-- half of a wide character, the other half becomes a space, so columns
-- never shift.
module Lipstick.Layer
  ( Layer
  , layer
  , offset
  , zIndex
  , layerId
  , transparent
  , sublayers
  , compose
  , overlay
  , layerAt
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lipstick.Internal.Block
import Lipstick.Mod (Mod (..), appMod)

data Layer = Layer
  { layerX :: !Int
  , layerY :: !Int
  , layerZ :: !Int
  , layerName :: !(Maybe Text)
  , layerTransparent :: !Bool
  , layerChildren :: [Layer]
  , layerBlock :: Block
  }

-- | A layer showing a block, at the origin unless moved with 'offset'.
layer :: Mod Layer -> Block -> Layer
layer m b = appMod m (Layer 0 0 0 Nothing False [] b)

-- | Move the layer right and down (relative to its parent, for sublayers).
-- Parts moved off the top or left of the canvas are cut off.
offset :: Int -> Int -> Mod Layer
offset x y = Mod (\l -> l {layerX = x, layerY = y})

-- | Layers with a higher z-index are drawn on top (relative to the parent,
-- for sublayers).
zIndex :: Int -> Mod Layer
zIndex z = Mod (\l -> l {layerZ = z})

-- | Name the layer, for 'layerAt'.
layerId :: Text -> Mod Layer
layerId t = Mod (\l -> l {layerName = Just t})

-- | Let unstyled spaces show what is below them.
transparent :: Mod Layer
transparent = Mod (\l -> l {layerTransparent = True})

-- | Layers positioned relative to this one.
sublayers :: [Layer] -> Mod Layer
sublayers ls = Mod (\l -> l {layerChildren = layerChildren l ++ ls})

-- | Absolute layers, in drawing order.
flatten :: [Layer] -> [Layer]
flatten = sortOn layerZ . concatMap (go 0 0 0)
  where
    go dx dy dz l =
      let l' = l {layerX = layerX l + dx, layerY = layerY l + dy, layerZ = layerZ l + dz}
       in l' : concatMap (go (layerX l') (layerY l') (layerZ l')) (layerChildren l)

-- | What occupies a column: a cluster starting there, or the second half
-- of a wide cluster starting to the left.
data Slot = Start !Cell | Continued

-- | Draw layers onto a canvas just big enough for all of them.
compose :: [Layer] -> Block
compose layers =
  fromCellLines [row r | r <- [0 .. h - 1]]
  where
    flat = flatten layers
    w = maximum (0 : [layerX l + blockWidth (layerBlock l) | l <- flat])
    h = maximum (0 : [layerY l + blockHeight (layerBlock l) | l <- flat])

    canvas = foldl paintLayer Map.empty flat

    row r = go 0
      where
        go c
          | c >= w = []
          | otherwise = case Map.lookup (r, c) canvas of
              Just (Start cell) -> cell : go (c + max 1 (cellWidth cell))
              _ -> blank : go (c + 1)
    blank = Cell mempty (T.singleton ' ') 1

-- | Draw one block on top of another, at a column and row.
overlay :: Int -> Int -> Block -> Block -> Block
overlay x y top base = compose [layer mempty base, layer (offset x y) top]

paintLayer :: Map (Int, Int) Slot -> Layer -> Map (Int, Int) Slot
paintLayer canvas l = foldl paintRow canvas (zip [layerY l ..] (blockCells (layerBlock l)))
  where
    paintRow m (r, cells)
      | r < 0 = m
      | otherwise = go m (layerX l) cells
      where
        go acc _ [] = acc
        go acc c (cell : rest)
          -- Zero-width clusters (a lone zero-width space, say) are
          -- invisible on their own and would otherwise take a column.
          | cw == 0 = go acc c rest
          | layerTransparent l && isBlank cell = go acc (c + cw) rest
          | c < 0 = go (clipLeft acc c cell) (c + cw) rest
          | otherwise = go (put acc c cell) (c + cw) rest
          where
            cw = cellWidth cell

        -- A wide cluster cut by the left edge: show a space for the part
        -- still on the canvas.
        clipLeft acc c cell =
          foldl (\a p -> put a p (spaceLike cell)) acc [p | p <- [c .. c + cellWidth cell - 1], p >= 0]

        put acc c cell =
          let cw = cellWidth cell
              -- Repair wide clusters that this cell cuts in half.
              acc1 = case Map.lookup (r, c) acc of
                Just Continued -> breakWide acc (c - 1)
                _ -> acc
              acc2 = case Map.lookup (r, c + cw - 1) acc1 of
                Just (Start old) | cellWidth old > 1 -> blankFrom acc1 (c + cw) (cellWidth old - 1) old
                _ -> acc1
              acc3 = Map.insert (r, c) (Start cell) acc2
           in foldl (\a p -> Map.insert (r, p) Continued a) acc3 [c + 1 .. c + cw - 1]

        -- The cluster that starts at or before column c and continues into
        -- column c + 1 is replaced by spaces up to c.
        breakWide acc c = case findStart acc c of
          Just (sc, old) -> foldl (\a p -> Map.insert (r, p) (Start (spaceLike old)) a) acc [sc .. c]
          Nothing -> acc
        findStart acc c = listToMaybe [(sc, old) | sc <- [c, c - 1 .. max 0 (c - 3)], Just (Start old) <- [Map.lookup (r, sc) acc]]

        -- Columns left over from a wide cluster whose start was overwritten.
        blankFrom acc from n old =
          foldl
            ( \a p -> case Map.lookup (r, p) a of
                Just Continued -> Map.insert (r, p) (Start (spaceLike old)) a
                _ -> a
            )
            acc
            [from .. from + n - 1]

    spaceLike cell = Cell (cellAttrs cell) (T.singleton ' ') 1
    isBlank cell = cellText cell == T.singleton ' ' && cellAttrs cell == mempty

-- | The topmost named layer covering a column and row, for mouse handling.
layerAt :: Int -> Int -> [Layer] -> Maybe Text
layerAt x y layers =
  listToMaybe
    [ name
    | l <- reverse (flatten layers)
    , Just name <- [layerName l]
    , x >= layerX l
    , x < layerX l + blockWidth (layerBlock l)
    , y >= layerY l
    , y < layerY l + blockHeight (layerBlock l)
    ]
