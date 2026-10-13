-- | Styles: text attributes plus a box model (alignment, padding, border,
-- margins), applied to text or to an existing block.
module Lipstick.Style
  ( Style
  , render
  , frame
  , tabWidth
  , resolveStyle
  , inherit
  ) where

import Data.Maybe (fromMaybe)
import Lipstick.Border
import Lipstick.Internal.Block
import Lipstick.Internal.Types
import Lipstick.Mod
import Lipstick.Width (textWidth)

data Style = Style
  { styleAttrs :: !Attrs
  , styleBox :: !Box
  , styleBorder :: !BorderProps
  , styleSize :: !Size
  , styleAlign :: !Align
  , styleTabWidth :: !Int
  }
  deriving (Eq, Show)

instance HasAttrs Style where
  overAttrs f s = s {styleAttrs = f (styleAttrs s)}

instance HasBox Style where
  overBox f s = s {styleBox = f (styleBox s)}

instance HasBorder Style where
  overBorder f s = s {styleBorder = f (styleBorder s)}

instance HasSize Style where
  overSize f s = s {styleSize = f (styleSize s)}

instance HasAlign Style where
  overAlign f s = s {styleAlign = f (styleAlign s)}

defStyle :: Style
defStyle = Style mempty defBox defBorderProps defSize defAlign 4

-- | The style a modifier produces from the defaults.
resolveStyle :: Mod Style -> Style
resolveStyle m = appMod m defStyle

-- | How many spaces a tab becomes (default 4).
tabWidth :: Int -> Mod Style
tabWidth n = Mod (\s -> s {styleTabWidth = max 0 n})

-- | Apply a parent style, keeping the padding and margins already set:
-- lipgloss's @Inherit@.
inherit :: Mod Style -> Mod Style
inherit m = Mod (\s -> (appMod m s) {styleBox = styleBox s})

-- | Render text in a box. When a 'width' is set, the text is word-wrapped
-- to fit inside the padding and border.
render :: Mod Style -> Inline -> Block
render m inl =
  let s = resolveStyle m
      ls = inlineLines (styleTabWidth s) inl
      wrapped = case contentWidth s of
        Just w -> concatMap (wrapCells w) ls
        Nothing -> ls
   in layout s wrapped

-- | Put an existing block in a box. The block is not re-wrapped: if it is
-- wider than the 'width' allows, the box grows.
frame :: Mod Style -> Block -> Block
frame m b = layout (resolveStyle m) (blockCells b)

data Edges = Edges {edgeTop, edgeRight, edgeBottom, edgeLeft :: !Bool}

borderEdges :: BorderProps -> (Border, Edges)
borderEdges p =
  let on side = fromMaybe (maybe False (const True) (borderChars p)) (side (borderSides p))
   in ( fromMaybe normalBorder (borderChars p)
      , Edges (on sideTop) (on sideRight) (on sideBottom) (on sideLeft)
      )

-- | Widths of the left and right border columns.
borderColumns :: BorderProps -> (Int, Int)
borderColumns p =
  let (b, e) = borderEdges p
      w on t = if on then max 1 (textWidth t) else 0
   in (w (edgeLeft e) (leftEdge b), w (edgeRight e) (rightEdge b))

contentWidth :: Style -> Maybe Int
contentWidth s = do
  w <- sizeWidth (styleSize s)
  let Sides _ pr _ pl = boxPadding (styleBox s)
      (bl, br) = borderColumns (styleBorder s)
  pure (max 0 (w - pl - pr - bl - br))

layout :: Style -> [[Cell]] -> Block
layout s content0 =
  let attrs = styleAttrs s
      fill = fillAttrs attrs
      Box (Sides pt pr pb pl) (Sides mt mr mb ml) mbg = styleBox s
      Size _ hgt maxW maxH = styleSize s
      Align ah av = styleAlign s
      bp = styleBorder s
      (chars, edges) = borderEdges bp
      (bl, br) = borderColumns bp
      bt = if edgeTop edges then 1 else 0
      bb = if edgeBottom edges then 1 else 0

      content = overCellAttrs (attrs <>) content0

      -- Horizontal alignment within the content width.
      innerW = maximum (fromMaybe 0 (contentWidth s) : map cellsWidth content)
      aligned = map (alignCells fill ah innerW) content

      -- Vertical alignment within the content height.
      innerH = maximum (length aligned : [h - pt - pb - bt - bb | Just h <- [hgt]])
      (above, below) = splitExtra av (innerH - length aligned)
      blankLine = blankCells fill innerW
      tall = replicate above blankLine ++ aligned ++ replicate below blankLine

      -- Padding.
      padW = pl + innerW + pr
      padded =
        replicate pt (blankCells fill padW)
          ++ [blankCells fill pl ++ l ++ blankCells fill pr | l <- tall]
          ++ replicate pb (blankCells fill padW)

      -- Border.
      ba = mempty {attrFg = borderFgColor bp, attrBg = borderBgColor bp}
      piece t n = fillCells ba t n
      corner on t n = if on then piece t n else []
      hRule l edge r = corner (edgeLeft edges) l bl ++ piece edge padW ++ corner (edgeRight edges) r br
      topRow = hRule (topLeftCorner chars) (topEdge chars) (topRightCorner chars)
      bottomRow = hRule (bottomLeftCorner chars) (bottomEdge chars) (bottomRightCorner chars)
      sideL = if edgeLeft edges then piece (leftEdge chars) bl else []
      sideR = if edgeRight edges then piece (rightEdge chars) br else []
      bordered =
        [topRow | edgeTop edges]
          ++ [sideL ++ l ++ sideR | l <- padded]
          ++ [bottomRow | edgeBottom edges]

      -- Margins.
      ma = mempty {attrBg = mbg}
      boxW = bl + padW + br
      outerW = ml + boxW + mr
      margined =
        replicate mt (blankCells ma outerW)
          ++ [blankCells ma ml ++ l ++ blankCells ma mr | l <- bordered]
          ++ replicate mb (blankCells ma outerW)

      -- Cropping.
      croppedW = maybe margined (\n -> map (cropCells n) margined) maxW
      cropped = maybe croppedW (`take` croppedW) maxH
   in fromCellLines cropped
