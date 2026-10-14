-- | Border character sets.
--
-- == Examples
--
-- >>> let sample b = render (border b <> marginRight 1) "text"
-- >>> putBlock (hcat (map sample [normalBorder, roundedBorder, thickBorder, doubleBorder]))
-- ┌────┐ ╭────╮ ┏━━━━┓ ╔════╗
-- │text│ │text│ ┃text┃ ║text║
-- └────┘ ╰────╯ ┗━━━━┛ ╚════╝
-- >>> putBlock (hcat (map sample [blockBorder, outerHalfBlockBorder, innerHalfBlockBorder]))
-- ██████ ▛▀▀▀▀▜ ▗▄▄▄▄▖
-- █text█ ▌text▐ ▐text▌
-- ██████ ▙▄▄▄▄▟ ▝▀▀▀▀▘
-- >>> putBlock (hcat (map sample [asciiBorder, markdownBorder, hiddenBorder]))
-- +----+ |----|
-- |text| |text|  text
-- +----+ |----|
--
-- A border is a record of the pieces, so a new one is made by changing
-- some of them:
--
-- >>> let dotted = normalBorder {topEdge = "┈", bottomEdge = "┈", leftEdge = "┊", rightEdge = "┊"}
-- >>> putBlock (render (border dotted <> padding 0 1) "dotted")
-- ┌┈┈┈┈┈┈┈┈┐
-- ┊ dotted ┊
-- └┈┈┈┈┈┈┈┈┘
--
-- Tables use the @middle*@ pieces:
--
-- >>> putBlock (table (border doubleBorder) ["a", "b"] [["1", "2"]])
-- ╔═╦═╗
-- ║a║b║
-- ╠═╬═╣
-- ║1║2║
-- ╚═╩═╝
module Lipstick.Border
  ( Border (..)
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
  ) where

import Lipstick.Internal.Border

-- $setup
-- >>> import Lipstick
