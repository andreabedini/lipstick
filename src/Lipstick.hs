-- | Style definitions for nice terminal layouts.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Lipstick
-- >
-- > card :: Mod Style
-- > card = bold <> fg "#FAFAFA" <> bg "#7D56F4" <> padding 1 4 <> width 22
-- >
-- > main :: IO ()
-- > main = putBlock (render card "Hello, kitty")
module Lipstick
  ( -- * Modifiers
    Mod
  , HasBox
  , HasBorder
  , HasSize
  , HasAlign
  , HasWrap

    -- * Styles
  , Style
  , render
  , frame
  , inherit
  , tabWidth
  , unsetTabWidth
  , inline
  , unsetInline
  , transform
  , unsetTransform
  , breakAfter
  , colorWhitespace
  , underlineSpaces
  , strikethroughSpaces

    -- * Blocks
  , Block
  , blockWidth
  , blockHeight
  , blockMinWidth
  , emptyBlock
  , text
  , fromInline
  , layout
  , Grid
  , gridWidth
  , gridHeight
  , fromGrid
  , Inline
  , plain
  , styled
  , fromAnsi
  , styleRanges
  , styleIndices
  , Attrs (..)

    -- * Layout
  , joinHorizontal
  , joinVertical
  , hcat
  , vcat
  , Place
  , place
  , whitespaceChars
  , Position
  , position
  , left
  , center
  , right
  , top
  , bottom

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
  , Sides (..)

    -- * Borders
  , Border (..)
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
  , normalBorder
  , roundedBorder
  , thickBorder
  , doubleBorder
  , blockBorder
  , outerHalfBlockBorder
  , innerHalfBlockBorder
  , hiddenBorder
  , asciiBorder
  , markdownBorder

    -- * Size, alignment and wrapping
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
  , align
  , valign
  , unsetAlign
  , noWrap

    -- * Getters
  , getAttrs
  , getMaxWidth
  , getMaxHeight
  , getPadding
  , getMargins
  , getMarginBg
  , getBorder
  , getBorderSides
  , getWidth
  , getHeight
  , getAlign
  , getVAlign
  , getTabWidth
  , getInline
  , horizontalPadding
  , verticalPadding
  , horizontalMargins
  , verticalMargins
  , horizontalBorderSize
  , verticalBorderSize
  , horizontalFrameSize
  , verticalFrameSize
  , frameSize

    -- * Tables
  , Table
  , table
  , TableCell (..)
  , cellRow
  , cellColumn
  , cellStyle
  , headerStyle
  , headerRule
  , columnRules
  , rowRules
  , yOffset
  , fitContent
  , visibleRows

    -- * Lists and trees
  , Outline
  , tree
  , forest
  , list
  , nestedList
  , Tree (..)
  , Forest
  , ItemPos (..)
  , rootStyle
  , itemStyle
  , itemStyleWith
  , enumeratorStyle
  , enumeratorStyleWith
  , indenterStyle
  , indenterStyleWith
  , itemOffset
  , Enumerator (..)
  , enumerator
  , enumeratorBy
  , indenter
  , treeEnumerator
  , roundedTreeEnumerator
  , bullet
  , dash
  , asterisk
  , arabic
  , alphabet
  , roman

    -- * Colours
  , module Lipstick.Color

    -- * Output
  , RenderEnv (..)
  , defaultRenderEnv
  , toAnsi
  , toAnsiLines
  , renderPlain
  , putBlock
  , hPutBlock
  , hPutBlockWith
  , detectEnv
  , detectEnvQuery
  , detectProfile
  , queryDarkBackground

    -- * Compositing
  , Layer
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
  , Rect (..)

    -- * Measuring text
  , textWidth
  , ansiWidth
  , ansiHeight
  , unicodeVersion
  ) where

import Data.Tree (Forest, Tree (..))
import Lipstick.Ansi
import Lipstick.Block
import Lipstick.Border
import Lipstick.Color
import Lipstick.Internal.Types
import Lipstick.Layer
import Lipstick.Mod
import Lipstick.Outline
import Lipstick.Place
import Lipstick.Style
import Lipstick.Table
import Lipstick.Terminal
import Lipstick.Width (textWidth, unicodeVersion)
