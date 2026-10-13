-- | Modifiers, in the style of optparse-applicative's builder.
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
module Lipstick.Mod
  ( Mod (..)
  , fieldMod
  , propMod
  , runMod
  , appMod

    -- * Properties of every thing
  , Props (..)
  , defProps
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
  , link

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

    -- * Borders
  , border
  , borderFg
  , borderBg
  , borderFgBlend
  , borderTop
  , borderRight
  , borderBottom
  , borderLeft

    -- * Size
  , width
  , height
  , maxWidth
  , maxHeight

    -- * Alignment
  , align
  , valign

    -- * Wrapping
  , noWrap
  ) where

import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Lipstick.Border (Border, normalBorder)
import Lipstick.Color (Color)
import Lipstick.Internal.Block
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

-- | What most builders do with the properties: lay the attributes under
-- everything, then crop.
withProps :: Props -> [[Cell]] -> Block
withProps p = cropToProps p . overCellAttrs (propAttrs p <>)

-- | Crop lines to 'maxWidth' and 'maxHeight'.
cropToProps :: Props -> [[Cell]] -> Block
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
  deriving (Eq, Show)

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
  , borderFgColor :: !(Maybe Color)
  , borderBgColor :: !(Maybe Color)
  , borderFgStops :: ![Color]
  -- ^ when not empty, a gradient that replaces 'borderFgColor'
  }
  deriving (Eq, Show)

defBorderProps :: BorderProps
defBorderProps = BorderProps Nothing (uniformSides Nothing) Nothing Nothing []

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
  }
  deriving (Eq, Show)

defSize :: Size
defSize = Size Nothing Nothing

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

-- | Make the text a hyperlink (OSC 8).
link :: Text -> Mod f
link url = attrsMod (\a -> a {attrLink = Just url})

-- Padding and margins ---------------------------------------------------------

paddingMod :: HasBox f => (Sides Int -> Sides Int) -> Mod f
paddingMod g = fieldMod (overBox (\b -> b {boxPadding = g (boxPadding b)}))

marginMod :: HasBox f => (Sides Int -> Sides Int) -> Mod f
marginMod g = fieldMod (overBox (\b -> b {boxMargin = g (boxMargin b)}))

-- | @padding vertical horizontal@
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
borderFg c = borderMod (\p -> p {borderFgColor = Just c, borderFgStops = []})
borderBg c = borderMod (\p -> p {borderBgColor = Just c})

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

-- | Total width, including padding and border.
width :: HasSize f => Int -> Mod f
width n = sizeMod (\s -> s {sizeWidth = Just n})

-- | Minimum total height, including padding and border.
height :: HasSize f => Int -> Mod f
height n = sizeMod (\s -> s {sizeHeight = Just n})

-- | Crop the final result (for a style, margins included) to at most this
-- many columns.
maxWidth :: Int -> Mod f
maxWidth n = propMod (\p -> p {propMaxWidth = Just n})

-- | Crop the final result (for a style, margins included) to at most this
-- many rows.
maxHeight :: Int -> Mod f
maxHeight n = propMod (\p -> p {propMaxHeight = Just n})

-- Alignment -------------------------------------------------------------------

-- | Horizontal alignment.
align :: HasAlign f => Position -> Mod f
align p = fieldMod (overAlign (\a -> a {alignH = p}))

-- | Vertical alignment.
valign :: HasAlign f => Position -> Mod f
valign p = fieldMod (overAlign (\a -> a {alignV = p}))

-- Wrapping --------------------------------------------------------------------

-- | Cut lines that are too long with an ellipsis instead of wrapping them.
noWrap :: HasWrap f => Mod f
noWrap = fieldMod (overWrap (const False))
