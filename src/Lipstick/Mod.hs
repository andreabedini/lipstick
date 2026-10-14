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
-- * a change to properties every thing has: text attributes and
--   the 'maxWidth' / 'maxHeight' crop. Modifiers of these, such as 'fg' or
--   'bold', work on any thing. Each thing's text attributes are the base
--   for everything it draws, overridden by anything more specific inside it
--   (a cell style, an item style, the attributes of a placed block).
--
-- * a change to the thing's own fields. Modifiers shared by some things go
--   through the classes 'HasBox', 'HasBorder', 'HasSize', 'HasAlign' and
--   'HasWrap'.
module Lipstick.Mod
  ( Mod
  , HasBox
  , HasBorder
  , HasSize
  , HasAlign
  , HasWrap
  , Sides (..)

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

import Lipstick.Internal.Mod
