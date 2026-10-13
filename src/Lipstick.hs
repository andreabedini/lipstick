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
  , HasAttrs
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
  , inline

    -- * Blocks
  , Block
  , blockWidth
  , blockHeight
  , emptyBlock
  , text
  , fromInline
  , Inline
  , plain
  , styled
  , fromAnsi
  , Attrs

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
  , Border (..)
  , border
  , borderFg
  , borderBg
  , borderFgBlend
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
  , align
  , valign
  , noWrap

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
  , Enumerator (..)
  , enumerator
  , enumeratorBy
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

    -- * Measuring text
  , textWidth
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
