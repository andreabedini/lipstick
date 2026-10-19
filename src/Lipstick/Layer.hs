-- | Compositing: blocks stacked on top of each other at any position.
--
-- Layers are drawn from the lowest 'zIndex' up; layers with the same
-- 'zIndex' are drawn in list order, children after their parent. A layer
-- hides what is below it, unless it is 'transparent'. Where a layer covers
-- half of a wide character, the other half becomes a space, so columns
-- never shift.
--
-- == Examples
--
-- >>> let background = place (width 26 <> height 6 <> whitespaceChars "╱") emptyBlock
-- >>> let shadow = render (bg black <> width 16 <> height 3) ""
-- >>> let dialog = render (border roundedBorder <> width 16 <> align center) "Really quit?"
-- >>> let layers = [layer mempty background, layer (offset 6 2 <> zIndex 1) shadow, layer (offset 4 1 <> zIndex 2 <> layerId "dialog") dialog]
-- >>> putBlock (compose layers)
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- ╱╱╱╱╭──────────────╮╱╱╱╱╱╱
-- ╱╱╱╱│ Really quit? │  ╱╱╱╱
-- ╱╱╱╱╰──────────────╯  ╱╱╱╱
-- ╱╱╱╱╱╱                ╱╱╱╱
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
--
-- Finding the named layer under a column and row, for mouse handling, and
-- where a layer is:
--
-- >>> layerAt Nothing 8 2 layers
-- Just "dialog"
-- >>> layerAt Nothing 1 1 layers
-- Nothing
-- >>> findLayer Nothing "dialog" layers
-- Just (Rect {rectX = 4, rectY = 1, rectWidth = 16, rectHeight = 3})
--
-- A 'transparent' layer shows what is below its unstyled spaces:
--
-- >>> let label = render (padding 0 2) "label"
-- >>> putBlock (compose [layer mempty background, layer (offset 2 2) label])
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- ╱╱  label  ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- >>> putBlock (compose [layer mempty background, layer (offset 2 2 <> transparent) label])
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- ╱╱╱╱label╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
-- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
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
  , layerHit
  , findLayer
  , Rect (rectX, rectY, rectWidth, rectHeight)
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lipstick.Internal.Grid
import Lipstick.Internal.Layout (Assign (..), Block (..), Measured (..), Request (..), TextStyle, cropLayoutWidth, defTextStyle, measureIn, takenWidth, usableWidth)
import Lipstick.Internal.Mod (Mod, Props (..), defProps, fieldMod, runMod, withProps)

-- $setup
-- >>> import Lipstick

-- | A block to draw on a canvas, with its position, z-index, name and
-- sublayers. Make one with 'layer', set those with a @t'Mod' t'Layer'@
-- ('offset', 'zIndex', …), and draw layers with 'compose'.
data Layer = Layer
  { layerX :: !Int
  , layerY :: !Int
  , layerZ :: !Int
  , layerName :: !(Maybe Text)
  , layerTransparent :: !Bool
  , layerChildren :: [Layer]
  , layerBlock :: Block
  , layerProps :: Props
  }

-- | A layer showing a block, at the origin unless moved with 'offset'. The
-- layer's text attributes are the base for the block. The block is laid
-- out when the layers are composed, in the width of the canvas to the
-- right of the layer.
layer :: Mod Layer -> Block -> Layer
layer m b =
  let (l, props) = runMod m (Layer 0 0 0 Nothing False [] b defProps)
   in l {layerProps = props}

-- | Move the layer right and down (relative to its parent, for sublayers).
-- Parts moved off the top or left of the canvas are cut off.
offset :: Int -> Int -> Mod Layer
offset x y = fieldMod (\l -> l {layerX = x, layerY = y})

-- | Layers with a higher z-index are drawn on top (relative to the parent,
-- for sublayers).
zIndex :: Int -> Mod Layer
zIndex z = fieldMod (\l -> l {layerZ = z})

-- | Name the layer, for 'layerAt'.
layerId :: Text -> Mod Layer
layerId t = fieldMod (\l -> l {layerName = Just t})

-- | Let unstyled spaces show what is below them.
transparent :: Mod Layer
transparent = fieldMod (\l -> l {layerTransparent = True})

-- | Layers positioned relative to this one.
sublayers :: [Layer] -> Mod Layer
sublayers ls = fieldMod (\l -> l {layerChildren = layerChildren l ++ ls})

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

-- | Draw layers onto a canvas just big enough for all of them, or as wide
-- as the space it is laid out in if a layer grows.
compose :: [Layer] -> Block
compose layers = Block $ \ctx -> Measured (canvasRequest (measured ctx layers)) $ \(Assign w _) ->
  let g = fitWidth w (canvas w (drawLayers w (measured ctx layers)))
   in (g, gridHeight g)

-- | Absolute layers, in drawing order, with their blocks in inherited text
-- settings.
measured :: TextStyle -> [Layer] -> [(Layer, Measured)]
measured ctx layers = [(l, measureIn ctx (layerBlock l)) | l <- flatten layers]

-- | What a canvas of layers asks for: room for each layer to the right of
-- its column.
canvasRequest :: [(Layer, Measured)] -> Request
canvasRequest ls =
  Request
    { reqMin = maximum (0 : [max 0 (layerX l + reqMin r) | (l, r) <- rs])
    , reqMax = maximum (0 : [max 0 (layerX l + reqMax r) | (l, r) <- rs])
    , reqGrow = maximum (0 : map (reqGrow . snd) rs)
    , reqLimit = maximum . (0 :) <$> traverse (\(l, r) -> (layerX l +) <$> usableWidth r) rs
    , reqStretch = False
    , reqFill = False
    }
  where
    rs = [(l, layerRequest l m) | (l, m) <- ls]

-- | A layer's request, within its 'Lipstick.Mod.maxWidth' crop.
layerRequest :: Layer -> Measured -> Request
layerRequest l m =
  let r = blockRequest m
      cap x = maybe x (min x . max 0) (propMaxWidth (layerProps l))
   in r {reqMin = cap (reqMin r), reqMax = cap (reqMax r), reqLimit = cap <$> reqLimit r}

-- | Absolute layers laid out in a canvas width: each takes what it needs
-- of the columns to its right.
drawLayers :: Int -> [(Layer, Measured)] -> [(Layer, Grid)]
drawLayers w ls =
  [ (l, withProps (layerProps l) (gridCells (fst (blockLayout m (Assign lw 0)))))
  | (l, m) <- ls
  , let r = blockRequest m
        -- As for a style: when the crop is what limits the layer, it is
        -- laid out at its natural width and cut, not made to wrap.
        lw = cropLayoutWidth (propMaxWidth (layerProps l)) (reqGrow r) (reqMax r) (takenWidth (layerRequest l m) (w - layerX l))
  ]

-- | The width a canvas of layers takes in an available width, as for
-- 'Lipstick.Block.layout'.
canvasWidth :: Maybe Int -> [(Layer, Measured)] -> Int
canvasWidth avail ls = let r = canvasRequest ls in maybe (reqMax r) (takenWidth r) avail

canvas :: Int -> [(Layer, Grid)] -> Grid
canvas w drawn = fromCellLines [row r | r <- [0 .. h - 1]]
  where
    h = maximum (0 : [layerY l + gridHeight g | (l, g) <- drawn])
    painted = foldl paintLayer Map.empty drawn

    row r = go 0
      where
        go c
          | c >= w = []
          | otherwise = case Map.lookup (r, c) painted of
              Just (Start cell) -> cell : go (c + max 1 (cellWidth cell))
              _ -> blank : go (c + 1)
    blank = Cell mempty (T.singleton ' ') 1 False

-- | Draw one block on top of another, at a column and row.
overlay :: Int -> Int -> Block -> Block -> Block
overlay x y top base = compose [layer mempty base, layer (offset x y) top]

paintLayer :: Map (Int, Int) Slot -> (Layer, Grid) -> Map (Int, Int) Slot
paintLayer painted (l, g) = foldl paintRow painted (zip [layerY l ..] (gridCells g))
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

    spaceLike cell = Cell (cellAttrs cell) (T.singleton ' ') 1 (cellFill cell)
    isBlank cell = cellText cell == T.singleton ' ' && cellAttrs cell == mempty

-- | Where a layer is drawn, in canvas columns and rows.
data Rect = Rect
  { rectX :: !Int
  , rectY :: !Int
  , rectWidth :: !Int
  , rectHeight :: !Int
  }
  deriving (Eq, Show)

bounds :: (Layer, Grid) -> Rect
bounds (l, g) = Rect (layerX l) (layerY l) (gridWidth g) (gridHeight g)

contains :: Int -> Int -> Rect -> Bool
contains x y (Rect rx ry w h) = x >= rx && x < rx + w && y >= ry && y < ry + h

-- | Layers laid out as 'compose' lays them out in an available width
-- ('Nothing' for its natural width), topmost first.
topmostFirst :: Maybe Int -> [Layer] -> [(Layer, Grid)]
topmostFirst avail layers =
  let ls = measured defTextStyle layers
   in reverse (drawLayers (canvasWidth avail ls) ls)

-- | The topmost named layer covering a column and row, for mouse handling,
-- with the layers composed in an available width ('Nothing' for their
-- natural width).
layerAt :: Maybe Int -> Int -> Int -> [Layer] -> Maybe Text
layerAt avail x y = fmap fst . layerHit avail x y

-- | Like 'layerAt', with where the layer is drawn.
layerHit :: Maybe Int -> Int -> Int -> [Layer] -> Maybe (Text, Rect)
layerHit avail x y layers =
  listToMaybe
    [ (name, bounds d)
    | d@(l, _) <- topmostFirst avail layers
    , Just name <- [layerName l]
    , contains x y (bounds d)
    ]

-- | Where the layer with an id is drawn (the topmost, if several share it).
findLayer :: Maybe Int -> Text -> [Layer] -> Maybe Rect
findLayer avail name layers = listToMaybe [bounds d | d@(l, _) <- topmostFirst avail layers, layerName l == Just name]
