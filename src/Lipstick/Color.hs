-- | Colours, colour profiles and downsampling.
--
-- A 'Color' is what you write in a style. It may be adaptive ('lightDark',
-- 'complete'), so it is only turned into a concrete 'TermColor' when the
-- block is serialised and the terminal's 'ColorProfile' is known.
--
-- == Examples
--
-- Colours are written as strings (hex or a palette index), or with the
-- functions and names below:
--
-- >>> ["#7D56F4", "#fa0", "201", "5"] :: [Color]
-- [rgb 125 86 244,rgb 255 170 0,ansi256 201,ansi 5]
-- >>> (rgb 125 86 244, ansi256 201, brightBlue)
-- (rgb 125 86 244,ansi256 201,ansi 12)
-- >>> hex "7D56F4"
-- Just (rgb 125 86 244)
-- >>> hex "nope"
-- Nothing
--
-- An adaptive colour is resolved for a profile and background (dark or
-- not) when it is printed:
--
-- >>> let accent = lightDark "#5A3FC0" "#B39DFF"
-- >>> resolveColor TrueColor True accent
-- Just (TCRGB 179 157 255)
-- >>> resolveColor TrueColor False accent
-- Just (TCRGB 90 63 192)
-- >>> resolveColor ANSI256 True "#7D56F4"
-- Just (TC256 99)
-- >>> resolveColor ANSI16 True "#7D56F4"
-- Just (TC16 12)
-- >>> resolveColor Ascii True "#7D56F4"
-- Nothing
-- >>> resolveColor ANSI16 True (complete "#7D56F4" "99" magenta)
-- Just (TC16 5)
--
-- Gradients and adjustments:
--
-- >>> blend 3 ["#ff0000", "#0000ff"]
-- [rgb 255 0 0,rgb 140 83 162,rgb 0 0 255]
-- >>> mix 0.25 black white
-- rgb 29 29 29
-- >>> darken 0.5 "#7D56F4"
-- rgb 44 28 94
-- >>> lighten 0.5 "#7D56F4"
-- rgb 184 175 255
-- >>> complementary "#7D56F4"
-- rgb 138 128 0
module Lipstick.Color
  ( -- * Colours
    Color
  , rgb
  , ansi
  , ansi256
  , hex
  , lightDark
  , complete

    -- ** The 16 basic colours
    -- | What they look like is up to the terminal's palette. Where they
    -- are turned into RGB, xterm's palette is used.
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

-- $setup
-- >>> import Lipstick
