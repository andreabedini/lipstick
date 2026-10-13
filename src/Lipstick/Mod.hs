-- | Modifiers, in the style of optparse-applicative's builder.
--
-- A @'Mod' f@ is a change to a thing @f@ (a 'Lipstick.Style.Style', a
-- 'Lipstick.Place.Place', plain 'Attrs', …). Modifiers form a monoid in
-- which the later setting wins, so styles are built and extended with '<>':
--
-- > card :: Mod Style
-- > card = bold <> fg "#FAFAFA" <> bg "#7D56F4" <> padding 1 2
-- >
-- > warning :: Mod Style
-- > warning = card <> bg "#F25D94"
--
-- Each modifier works on every thing that has the component it changes,
-- which is expressed by the classes 'HasAttrs', 'HasBox', 'HasBorder',
-- 'HasSize' and 'HasAlign'.
module Lipstick.Mod
  ( Mod (..)
  , appMod

    -- * Components
  , HasAttrs (..)
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
import Lipstick.Internal.Types

-- | A modification of a thing @f@. '<>' applies the left modifier first, so
-- settings on the right win.
newtype Mod f = Mod (f -> f)

instance Semigroup (Mod f) where
  Mod g <> Mod h = Mod (h . g)

instance Monoid (Mod f) where
  mempty = Mod id

appMod :: Mod f -> f -> f
appMod (Mod g) = g

-- | Things with text attributes.
class HasAttrs f where
  overAttrs :: (Attrs -> Attrs) -> f -> f

instance HasAttrs Attrs where
  overAttrs = id

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
  , sizeMaxWidth :: !(Maybe Int)
  , sizeMaxHeight :: !(Maybe Int)
  }
  deriving (Eq, Show)

defSize :: Size
defSize = Size Nothing Nothing Nothing Nothing

data Align = Align
  { alignH :: !Position
  , alignV :: !Position
  }
  deriving (Eq, Show)

defAlign :: Align
defAlign = Align left top

-- Text attributes -------------------------------------------------------------

attrsMod :: HasAttrs f => (Attrs -> Attrs) -> Mod f
attrsMod = Mod . overAttrs

bold, faint, italic, blink, reverseVideo, strikethrough :: HasAttrs f => Mod f
bold = flag Bold True
faint = flag Faint True
italic = flag Italic True
blink = flag Blink True
reverseVideo = flag Reverse True
strikethrough = flag Strikethrough True

-- | Set a flag explicitly; @flag Bold False@ turns off bold inherited from
-- an enclosing style.
flag :: HasAttrs f => Flag -> Bool -> Mod f
flag f v = attrsMod (setFlag f (Just v))

-- | Forget a flag, so an enclosing style's setting shows through.
unsetFlag :: HasAttrs f => Flag -> Mod f
unsetFlag f = attrsMod (setFlag f Nothing)

underline :: HasAttrs f => Mod f
underline = underlineStyle SingleUnderline

-- | Curly, dotted and dashed underlines need a true-colour terminal; on
-- other profiles they are shown as a single underline.
underlineStyle :: HasAttrs f => Underline -> Mod f
underlineStyle u = attrsMod (\a -> a {attrUnderline = Just u})

underlineColor :: HasAttrs f => Color -> Mod f
underlineColor c = attrsMod (\a -> a {attrUnderlineColor = Just c})

fg, bg :: HasAttrs f => Color -> Mod f
fg c = attrsMod (\a -> a {attrFg = Just c})
bg c = attrsMod (\a -> a {attrBg = Just c})

unsetFg, unsetBg :: HasAttrs f => Mod f
unsetFg = attrsMod (\a -> a {attrFg = Nothing})
unsetBg = attrsMod (\a -> a {attrBg = Nothing})

-- | Make the text a hyperlink (OSC 8).
link :: HasAttrs f => Text -> Mod f
link url = attrsMod (\a -> a {attrLink = Just url})

-- Padding and margins ---------------------------------------------------------

paddingMod :: HasBox f => (Sides Int -> Sides Int) -> Mod f
paddingMod g = Mod (overBox (\b -> b {boxPadding = g (boxPadding b)}))

marginMod :: HasBox f => (Sides Int -> Sides Int) -> Mod f
marginMod g = Mod (overBox (\b -> b {boxMargin = g (boxMargin b)}))

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
marginBg c = Mod (overBox (\b -> b {boxMarginBg = Just c}))

-- | The character padding is filled with (default a space).
paddingChar :: HasBox f => Char -> Mod f
paddingChar c = Mod (overBox (\b -> b {boxPaddingChar = c}))

-- | The character margins are filled with (default a space).
marginChar :: HasBox f => Char -> Mod f
marginChar c = Mod (overBox (\b -> b {boxMarginChar = c}))

-- Borders ---------------------------------------------------------------------

borderMod :: HasBorder f => (BorderProps -> BorderProps) -> Mod f
borderMod = Mod . overBorder

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
sizeMod = Mod . overSize

-- | Total width, including padding and border.
width :: HasSize f => Int -> Mod f
width n = sizeMod (\s -> s {sizeWidth = Just n})

-- | Minimum total height, including padding and border.
height :: HasSize f => Int -> Mod f
height n = sizeMod (\s -> s {sizeHeight = Just n})

-- | Crop the final result, margins included, to at most this many columns.
maxWidth :: HasSize f => Int -> Mod f
maxWidth n = sizeMod (\s -> s {sizeMaxWidth = Just n})

-- | Crop the final result, margins included, to at most this many rows.
maxHeight :: HasSize f => Int -> Mod f
maxHeight n = sizeMod (\s -> s {sizeMaxHeight = Just n})

-- Alignment -------------------------------------------------------------------

-- | Horizontal alignment.
align :: HasAlign f => Position -> Mod f
align p = Mod (overAlign (\a -> a {alignH = p}))

-- | Vertical alignment.
valign :: HasAlign f => Position -> Mod f
valign p = Mod (overAlign (\a -> a {alignV = p}))

-- Wrapping --------------------------------------------------------------------

-- | Cut lines that are too long with an ellipsis instead of wrapping them.
noWrap :: HasWrap f => Mod f
noWrap = Mod (overWrap (const False))
