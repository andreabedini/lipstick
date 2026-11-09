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
-- As in optparse-applicative's @Mod@, a change to the properties every
-- thing has is kept apart from a change to the thing's own fields:
--
-- * the properties every thing has are text attributes and the
--   'maxWidth' / 'maxHeight' crop. Text attributes, such as 'fg' or
--   'bold', can be set on any thing, and the crop on any thing but
--   'Lipstick.Block.Inline' text ('HasCrop'). Each thing's text attributes
--   are the base for what it draws (for a style, only its content: see
--   'Lipstick.Style.Style'), overridden by anything more specific inside
--   it (a cell style, an item style, the attributes of a placed block).
--
-- * modifiers of fields that several things have go through the classes
--   'HasBox', 'HasBorder', 'HasSize', 'HasAlign' and 'HasWrap'; the rest,
--   such as 'Lipstick.Table.cellStyle', belong to one thing.
--
-- == Examples
--
-- The examples print with 'Lipstick.Terminal.putBlock'; the output is
-- shown without colours and other attributes.
--
-- Text attributes work on any thing:
--
-- >>> let base = bold <> fg red
-- >>> putBlock (render (base <> border normalBorder) "a style")
-- ┌───────┐
-- │a style│
-- └───────┘
-- >>> putBlock (table base ["a table"] [["cell"]])
-- ╭───────╮
-- │a table│
-- ├───────┤
-- │cell   │
-- ╰───────╯
-- >>> putBlock (list base ["a list"])
-- • a list
--
-- Padding and margins are given as vertical then horizontal amounts, or
-- per side:
--
-- >>> putBlock (render (border normalBorder <> padding 0 3) "wide")
-- ┌──────────┐
-- │   wide   │
-- └──────────┘
-- >>> putBlock (render (border normalBorder <> paddingLeft 4) "indented")
-- ┌────────────┐
-- │    indented│
-- └────────────┘
--
-- Borders, and which of their sides are drawn:
--
-- >>> putBlock (render (border roundedBorder <> borderBottom False) "open below")
-- ╭──────────╮
-- │open below│
--
-- 'width' is the width with padding and border. The thing shrinks when
-- there is less room:
--
-- >>> let note = render (width 24 <> border normalBorder) "Wraps to its width"
-- >>> putBlock note
-- ┌──────────────────────┐
-- │Wraps to its width    │
-- └──────────────────────┘
-- >>> putBlock (fromGrid (layout (Just 14) note))
-- ┌────────────┐
-- │Wraps to its│
-- │width       │
-- └────────────┘
--
-- 'maxWidth' and 'maxHeight' crop the result:
--
-- >>> putBlock (render (border normalBorder <> maxWidth 6) "cropped")
-- ┌─────
-- │cropp
-- └─────
--
-- Alignment of content in a box that is larger than it:
--
-- >>> putBlock (render (width 12 <> height 5 <> border normalBorder <> align right <> valign bottom) "corner")
-- ┌──────────┐
-- │          │
-- │          │
-- │    corner│
-- └──────────┘
--
-- An @unset@ modifier undoes a setting made earlier:
--
-- >>> let boxed = border normalBorder <> padding 0 1
-- >>> putBlock (render (boxed <> unsetBorder) "no border")
--  no border
module Lipstick.Mod
  ( Mod
  , HasBox
  , HasBorder
  , HasSize
  , HasAlign
  , HasWrap
  , HasCrop
  , Sides (..)

    -- * Text attributes
  , bold
  , faint
  , italic
  , blink
  , reverseVideo
  , strikethrough
  , Flag (..)
  , flag
  , unsetFlag
  , underline
  , Underline (..)
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
  , Position
  , position
  , left
  , center
  , right
  , top
  , bottom

    -- * Wrapping
  , noWrap
  , unsetNoWrap
  ) where

import Lipstick.Internal.Mod
import Lipstick.Internal.Types (Flag (..), Position, Underline (..), bottom, center, left, position, right, top)

-- $setup
-- >>> import Lipstick
