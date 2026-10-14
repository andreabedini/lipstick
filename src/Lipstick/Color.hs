-- | Colours, colour profiles and downsampling.
--
-- A 'Color' is what you write in a style. It may be adaptive ('LightDark',
-- 'Complete'), so it is only turned into a concrete 'TermColor' when the
-- block is serialised and the terminal's 'ColorProfile' is known.
module Lipstick.Color
  ( -- * Colours
    Color (..)
  , rgb
  , ansi
  , ansi256
  , hex
  , lightDark
  , complete

    -- ** The 16 basic colours
  , black, red, green, yellow, blue, magenta, cyan, white
  , brightBlack, brightRed, brightGreen, brightYellow
  , brightBlue, brightMagenta, brightCyan, brightWhite

    -- * Manipulating colours
    -- | These work in the OkLab colour space, so steps look even. Palette
    -- colours are converted to RGB using xterm's palette, and adaptive
    -- colours are transformed in every variant.
  , mix
  , blend
  , blend2D
  , darken
  , lighten
  , complementary

    -- * Terminal colours
  , TermColor (..)
  , ColorProfile (..)
  , resolveColor
  , downsample
  , termColorRGB
  ) where

import Lipstick.Internal.Color
