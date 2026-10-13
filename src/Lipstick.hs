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

    -- * Size and alignment
  , width
  , height
  , maxWidth
  , maxHeight
  , align
  , valign

    -- * Colours
  , module Lipstick.Color

    -- * Output
  , RenderEnv (..)
  , defaultRenderEnv
  , toAnsi
  , renderPlain
  , putBlock
  , hPutBlock
  , detectEnv
  , detectProfile
  , queryDarkBackground

    -- * Measuring text
  , textWidth
  , unicodeVersion
  ) where

import Lipstick.Ansi
import Lipstick.Block
import Lipstick.Border
import Lipstick.Color
import Lipstick.Internal.Types
import Lipstick.Mod
import Lipstick.Place
import Lipstick.Style
import Lipstick.Terminal
import Lipstick.Width (textWidth, unicodeVersion)
