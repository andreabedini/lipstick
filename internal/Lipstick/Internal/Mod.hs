-- | The representation of modifiers and the records they change, and the
-- modifiers themselves. Hidden: the public API is "Lipstick.Mod".
--
-- Modifiers, in the style of optparse-applicative's builder.
--
-- A @'Mod' f@ is a change to a thing @f@ (a 'Lipstick.Style.Style', a
-- 'Lipstick.Table.Table', a 'Lipstick.Place.Place', …). Modifiers form a
-- monoid in which the later setting wins, so styles are built and extended
-- with '<>':
--
-- > card :: Mod Style
-- > card = bold <> fg "#FAFAFA" <> bg "#7D56F4" <> padding 1 2
-- >
-- > warning :: Mod Style
-- > warning = card <> bg "#F25D94"
--
-- Like optparse-applicative's @Mod@, a modifier has two parts:
--
-- * a change to properties every thing has ('Props'): text attributes and
--   the 'maxWidth' / 'maxHeight' crop. Modifiers of these, such as 'fg' or
--   'bold', work on any thing. Each thing's text attributes are the base
--   for everything it draws, overridden by anything more specific inside it
--   (a cell style, an item style, the attributes of a placed block).
--
-- * a change to the thing's own fields. Modifiers shared by some things go
--   through the classes 'HasBox', 'HasBorder', 'HasSize', 'HasAlign' and
--   'HasWrap'.
module Lipstick.Internal.Mod
  ( Mod (..)
  , fieldMod
  , propMod
  , runMod
  , appMod

    -- * Properties of every thing
  , Props (..)
  , defProps
  , propsOf
  , withProps
  , cropToProps

    -- * Components
  , HasBox (..)
  , HasBorder (..)
  , HasSize (..)
  , HasAlign (..)
  , HasWrap (..)
  , Sides (..)
  , uniformSides
  , Box (..)
  , defBox
  , BorderProps (..)
  , defBorderProps
  , Edges (..)
  , borderEdges
  , Size (..)
  , defSize
  , Align (..)
  , defAlign

    -- * Text attributes
  , bold
  , faint
  , italic
  , blink
  , reverseVideo
  , strikethrough
  , flag
  , unsetFlag
  , underline
  , underlineStyle
  , underlineColor
  , fg
  , bg
  , unsetFg
  , unsetBg
  , unsetUnderline
  , unsetUnderlineColor
  , link
  , unsetLink

    -- * Padding and margins
  , padding
  , paddingAll
  , paddingTop
  , paddingRight
  , paddingBottom
  , paddingLeft
  , margin
  , marginAll
  , marginTop
  , marginRight
  , marginBottom
  , marginLeft
  , marginBg
  , paddingChar
  , marginChar
  , unsetPadding
  , unsetMargins
  , unsetMarginBg

    -- * Borders
  , border
  , borderFg
  , borderBg
  , borderFgBlend
  , borderFgBlendOffset
  , borderTopFg
  , borderRightFg
  , borderBottomFg
  , borderLeftFg
  , borderTopBg
  , borderRightBg
  , borderBottomBg
  , borderLeftBg
  , unsetBorder
  , borderTop
  , borderRight
  , borderBottom
  , borderLeft

    -- * Size
  , width
  , height
  , maxWidth
  , maxHeight
  , grow
  , fillWidth
  , stretchHeight
  , unsetWidth
  , unsetHeight
  , unsetMaxWidth
  , unsetMaxHeight

    -- * Alignment
  , align
  , valign
  , unsetAlign

    -- * Wrapping
  , noWrap
  ) where

import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Lipstick.Internal.Border (Border, normalBorder)
import Lipstick.Internal.Color (Color)
import Lipstick.Internal.Grid
import Lipstick.Internal.Types

-- | A modification of a thing @f@: of its own fields, and of the
-- properties every thing has. '<>' applies the left modifier first, so
-- settings on the right win.
data Mod f = Mod (f -> f) (Props -> Props)

instance Semigroup (Mod f) where
  Mod f1 g1 <> Mod f2 g2 = Mod (f2 . f1) (g2 . g1)

instance Monoid (Mod f) where
  mempty = Mod id id

-- | A modifier of the thing's own fields.
fieldMod :: (f -> f) -> Mod f
fieldMod f = Mod f id

-- | A modifier of the properties every thing has.
propMod :: (Props -> Props) -> Mod f
propMod = Mod id

-- | Apply a modifier to a thing's defaults.
runMod :: Mod f -> f -> (f, Props)
runMod (Mod f g) x = (f x, g defProps)

-- | Apply only the field part of a modifier.
appMod :: Mod f -> f -> f
appMod (Mod f _) = f

-- | Properties every thing has.
data Props = Props
  { propAttrs :: !Attrs
  -- ^ base text attributes for everything the thing draws
  , propMaxWidth :: !(Maybe Int)
  , propMaxHeight :: !(Maybe Int)
  }
  deriving (Eq, Show)

defProps :: Props
defProps = Props mempty Nothing Nothing

-- | The common properties a modifier sets, starting from the defaults.
propsOf :: Mod f -> Props
propsOf (Mod _ g) = g defProps

-- | What most builders do with the properties: lay the attributes under
-- everything, then crop.
withProps :: Props -> [[Cell]] -> Grid
withProps p = cropToProps p . underlay (propAttrs p) (fillAttrs (propAttrs p))

-- | Crop lines to 'maxWidth' and 'maxHeight'.
cropToProps :: Props -> [[Cell]] -> Grid
cropToProps p ls =
  let croppedW = maybe ls (\n -> map (cropCells n) ls) (propMaxWidth p)
   in fromCellLines (maybe croppedW (`take` croppedW) (propMaxHeight p))

-- | Things with padding and margins.
class HasBox f where
  overBox :: (Box -> Box) -> f -> f

-- | Things with a border.
class HasBorder f where
  overBorder :: (BorderProps -> BorderProps) -> f -> f

-- | Things with a size.
class HasSize f where
  overSize :: (Size -> Size) -> f -> f

-- | Things whose content can be aligned.
class HasAlign f where
  overAlign :: (Align -> Align) -> f -> f

-- | Things whose text can be wrapped.
class HasWrap f where
  overWrap :: (Bool -> Bool) -> f -> f

data Sides a = Sides {sideTop, sideRight, sideBottom, sideLeft :: !a}
  deriving (Eq, Show, Functor)

uniformSides :: a -> Sides a
uniformSides a = Sides a a a a

data Box = Box
  { boxPadding :: !(Sides Int)
  , boxMargin :: !(Sides Int)
  , boxMarginBg :: !(Maybe Color)
  , boxPaddingChar :: !Char
  , boxMarginChar :: !Char
  }
  deriving (Eq, Show)

defBox :: Box
defBox = Box (uniformSides 0) (uniformSides 0) Nothing ' ' ' '

-- | A border is drawn when its characters are set or any side is switched
-- on explicitly. Sides left as 'Nothing' are drawn only when the
-- characters are set.
data BorderProps = BorderProps
  { borderChars :: !(Maybe Border)
  , borderSides :: !(Sides (Maybe Bool))
  , borderFgColors :: !(Sides (Maybe Color))
  , borderBgColors :: !(Sides (Maybe Color))
  , borderFgStops :: ![Color]
  -- ^ when not empty, a gradient that replaces 'borderFgColors'
  , borderFgOffset :: !Int
  -- ^ how far the gradient is rotated clockwise
  }
  deriving (Eq, Show)

defBorderProps :: BorderProps
defBorderProps = BorderProps Nothing (uniformSides Nothing) (uniformSides Nothing) (uniformSides Nothing) [] 0

-- | Which sides of a border are drawn.
data Edges = Edges {edgeTop, edgeRight, edgeBottom, edgeLeft :: !Bool}
  deriving (Eq, Show)

-- | The characters to draw a border with, and the sides to draw. Without
-- characters, sides switched on explicitly use 'normalBorder'.
borderEdges :: BorderProps -> (Border, Edges)
borderEdges p =
  let on side = fromMaybe (isJust (borderChars p)) (side (borderSides p))
   in ( fromMaybe normalBorder (borderChars p)
      , Edges (on sideTop) (on sideRight) (on sideBottom) (on sideLeft)
      )

-- | Width and height include padding and border, but not margins.
data Size = Size
  { sizeWidth :: !(Maybe Int)
  , sizeHeight :: !(Maybe Int)
  , sizeGrow :: !Int
  -- ^ weight for spare width; 0 for none
  , sizeStretch :: !Bool
  -- ^ grow to the height of the row
  }
  deriving (Eq, Show)

defSize :: Size
defSize = Size Nothing Nothing 0 False

data Align = Align
  { alignH :: !Position
  , alignV :: !Position
  }
  deriving (Eq, Show)

defAlign :: Align
defAlign = Align left top

-- Text attributes -------------------------------------------------------------

attrsMod :: (Attrs -> Attrs) -> Mod f
attrsMod g = propMod (\p -> p {propAttrs = g (propAttrs p)})

bold, faint, italic, blink, reverseVideo, strikethrough :: Mod f
bold = flag Bold True
faint = flag Faint True
italic = flag Italic True
blink = flag Blink True
reverseVideo = flag Reverse True
strikethrough = flag Strikethrough True

-- | Set a flag explicitly; @flag Bold False@ turns off bold inherited from
-- an enclosing style.
flag :: Flag -> Bool -> Mod f
flag f v = attrsMod (setFlag f (Just v))

-- | Forget a flag, so an enclosing style's setting shows through.
unsetFlag :: Flag -> Mod f
unsetFlag f = attrsMod (setFlag f Nothing)

underline :: Mod f
underline = underlineStyle SingleUnderline

-- | Curly, dotted and dashed underlines need a true-colour terminal; on
-- other profiles they are shown as a single underline.
underlineStyle :: Underline -> Mod f
underlineStyle u = attrsMod (\a -> a {attrUnderline = Just u})

underlineColor :: Color -> Mod f
underlineColor c = attrsMod (\a -> a {attrUnderlineColor = Just c})

fg, bg :: Color -> Mod f
fg c = attrsMod (\a -> a {attrFg = Just c})
bg c = attrsMod (\a -> a {attrBg = Just c})

unsetFg, unsetBg :: Mod f
unsetFg = attrsMod (\a -> a {attrFg = Nothing})
unsetBg = attrsMod (\a -> a {attrBg = Nothing})

unsetUnderline, unsetUnderlineColor :: Mod f
unsetUnderline = attrsMod (\a -> a {attrUnderline = Nothing})
unsetUnderlineColor = attrsMod (\a -> a {attrUnderlineColor = Nothing})

-- | Make the text a hyperlink (OSC 8).
link :: Text -> Mod f
link url = attrsMod (\a -> a {attrLink = Just url})

unsetLink :: Mod f
unsetLink = attrsMod (\a -> a {attrLink = Nothing})

-- Padding and margins ---------------------------------------------------------

-- Negative padding and margins count as 0.
paddingMod :: HasBox f => (Sides Int -> Sides Int) -> Mod f
paddingMod g = fieldMod (overBox (\b -> b {boxPadding = fmap (max 0) (g (boxPadding b))}))

marginMod :: HasBox f => (Sides Int -> Sides Int) -> Mod f
marginMod g = fieldMod (overBox (\b -> b {boxMargin = fmap (max 0) (g (boxMargin b))}))

-- | @padding vertical horizontal@. Negative amounts count as 0, as for
-- all padding and margins.
padding :: HasBox f => Int -> Int -> Mod f
padding v h = paddingMod (const (Sides v h v h))

paddingAll :: HasBox f => Int -> Mod f
paddingAll n = paddingMod (const (uniformSides n))

paddingTop, paddingRight, paddingBottom, paddingLeft :: HasBox f => Int -> Mod f
paddingTop n = paddingMod (\s -> s {sideTop = n})
paddingRight n = paddingMod (\s -> s {sideRight = n})
paddingBottom n = paddingMod (\s -> s {sideBottom = n})
paddingLeft n = paddingMod (\s -> s {sideLeft = n})

-- | @margin vertical horizontal@
margin :: HasBox f => Int -> Int -> Mod f
margin v h = marginMod (const (Sides v h v h))

marginAll :: HasBox f => Int -> Mod f
marginAll n = marginMod (const (uniformSides n))

marginTop, marginRight, marginBottom, marginLeft :: HasBox f => Int -> Mod f
marginTop n = marginMod (\s -> s {sideTop = n})
marginRight n = marginMod (\s -> s {sideRight = n})
marginBottom n = marginMod (\s -> s {sideBottom = n})
marginLeft n = marginMod (\s -> s {sideLeft = n})

-- | Background colour of the margins.
marginBg :: HasBox f => Color -> Mod f
marginBg c = fieldMod (overBox (\b -> b {boxMarginBg = Just c}))

unsetPadding, unsetMargins, unsetMarginBg :: HasBox f => Mod f
unsetPadding = paddingMod (const (uniformSides 0))
unsetMargins = marginMod (const (uniformSides 0))
unsetMarginBg = fieldMod (overBox (\b -> b {boxMarginBg = Nothing}))

-- | The character padding is filled with (default a space).
paddingChar :: HasBox f => Char -> Mod f
paddingChar c = fieldMod (overBox (\b -> b {boxPaddingChar = c}))

-- | The character margins are filled with (default a space).
marginChar :: HasBox f => Char -> Mod f
marginChar c = fieldMod (overBox (\b -> b {boxMarginChar = c}))

-- Borders ---------------------------------------------------------------------

borderMod :: HasBorder f => (BorderProps -> BorderProps) -> Mod f
borderMod = fieldMod . overBorder

-- | Draw a border with these characters, on all sides not switched off.
border :: HasBorder f => Border -> Mod f
border b = borderMod (\p -> p {borderChars = Just b})

borderFg, borderBg :: HasBorder f => Color -> Mod f
borderFg c = borderMod (\p -> p {borderFgColors = uniformSides (Just c), borderFgStops = []})
borderBg c = borderMod (\p -> p {borderBgColors = uniformSides (Just c)})

-- | The colour of one side of the border. The top and bottom sides include
-- the corners.
borderTopFg, borderRightFg, borderBottomFg, borderLeftFg :: HasBorder f => Color -> Mod f
borderTopFg c = borderFgSide (\s -> s {sideTop = Just c})
borderRightFg c = borderFgSide (\s -> s {sideRight = Just c})
borderBottomFg c = borderFgSide (\s -> s {sideBottom = Just c})
borderLeftFg c = borderFgSide (\s -> s {sideLeft = Just c})

borderTopBg, borderRightBg, borderBottomBg, borderLeftBg :: HasBorder f => Color -> Mod f
borderTopBg c = borderBgSide (\s -> s {sideTop = Just c})
borderRightBg c = borderBgSide (\s -> s {sideRight = Just c})
borderBottomBg c = borderBgSide (\s -> s {sideBottom = Just c})
borderLeftBg c = borderBgSide (\s -> s {sideLeft = Just c})

borderFgSide, borderBgSide :: HasBorder f => (Sides (Maybe Color) -> Sides (Maybe Color)) -> Mod f
borderFgSide g = borderMod (\p -> p {borderFgColors = g (borderFgColors p), borderFgStops = []})
borderBgSide g = borderMod (\p -> p {borderBgColors = g (borderBgColors p)})

-- | Rotate a 'borderFgBlend' gradient clockwise by this many cells
-- (anticlockwise if negative).
borderFgBlendOffset :: HasBorder f => Int -> Mod f
borderFgBlendOffset n = borderMod (\p -> p {borderFgOffset = n})

-- | No border, and no border colours.
unsetBorder :: HasBorder f => Mod f
unsetBorder = borderMod (const defBorderProps)

-- | Colour the border with a gradient through these colours, going
-- clockwise from the top-left corner.
borderFgBlend :: HasBorder f => [Color] -> Mod f
borderFgBlend cs = borderMod (\p -> p {borderFgStops = cs})

borderTop, borderRight, borderBottom, borderLeft :: HasBorder f => Bool -> Mod f
borderTop v = borderSide (\s -> s {sideTop = Just v})
borderRight v = borderSide (\s -> s {sideRight = Just v})
borderBottom v = borderSide (\s -> s {sideBottom = Just v})
borderLeft v = borderSide (\s -> s {sideLeft = Just v})

borderSide :: HasBorder f => (Sides (Maybe Bool) -> Sides (Maybe Bool)) -> Mod f
borderSide g = borderMod (\p -> p {borderSides = g (borderSides p)})

-- Size ------------------------------------------------------------------------

sizeMod :: HasSize f => (Size -> Size) -> Mod f
sizeMod = fieldMod . overSize

-- | Total width, including padding and border, when there is room for
-- it. Laid out in less space (a narrow terminal, a full row), the thing
-- shrinks like any other rather than overflowing. A negative width counts
-- as 0, as do negative heights and crops.
width :: HasSize f => Int -> Mod f
width n = sizeMod (\s -> s {sizeWidth = Just (max 0 n)})

-- | Minimum total height, including padding and border: the thing is at
-- least this many lines tall.
height :: HasSize f => Int -> Mod f
height n = sizeMod (\s -> s {sizeHeight = Just (max 0 n)})

-- | Take a share of the spare width, in proportion to the weight, when the
-- thing is laid out next to others or on its own in the output's width.
-- With a 'width', that width is the least it takes.
grow :: HasSize f => Int -> Mod f
grow k = sizeMod (\s -> s {sizeGrow = max 0 k})

-- | Fill the width available: @grow 1@.
fillWidth :: HasSize f => Mod f
fillWidth = grow 1

-- | Grow to the height of the row the thing is in (as the tallest thing
-- next to it).
stretchHeight :: HasSize f => Mod f
stretchHeight = sizeMod (\s -> s {sizeStretch = True})

-- | Crop the final result (for a style, margins included) to at most this
-- many columns.
maxWidth :: Int -> Mod f
maxWidth n = propMod (\p -> p {propMaxWidth = Just (max 0 n)})

-- | Crop the final result (for a style, margins included) to at most this
-- many rows. A table leaves out the body rows that do not fit instead (see
-- 'Lipstick.Table.yOffset').
maxHeight :: Int -> Mod f
maxHeight n = propMod (\p -> p {propMaxHeight = Just (max 0 n)})

unsetWidth, unsetHeight :: HasSize f => Mod f
unsetWidth = sizeMod (\s -> s {sizeWidth = Nothing})
unsetHeight = sizeMod (\s -> s {sizeHeight = Nothing})

unsetMaxWidth, unsetMaxHeight :: Mod f
unsetMaxWidth = propMod (\p -> p {propMaxWidth = Nothing})
unsetMaxHeight = propMod (\p -> p {propMaxHeight = Nothing})

-- Alignment -------------------------------------------------------------------

-- | Horizontal alignment.
align :: HasAlign f => Position -> Mod f
align p = fieldMod (overAlign (\a -> a {alignH = p}))

-- | Vertical alignment.
valign :: HasAlign f => Position -> Mod f
valign p = fieldMod (overAlign (\a -> a {alignV = p}))

-- | Back to the default alignment, 'left' and 'top'.
unsetAlign :: HasAlign f => Mod f
unsetAlign = fieldMod (overAlign (const defAlign))

-- Wrapping --------------------------------------------------------------------

-- | Cut lines that are too long with an ellipsis instead of wrapping them.
noWrap :: HasWrap f => Mod f
noWrap = fieldMod (overWrap (const False))
