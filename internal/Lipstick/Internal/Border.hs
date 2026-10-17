{-# LANGUAGE OverloadedStrings #-}

-- | The implementation of "Lipstick.Border", hidden from other packages: see
-- that module.
module Lipstick.Internal.Border
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

import Data.Text (Text)

-- | The characters used to draw a border. The @middle*@ pieces are used
-- where table rules meet.
data Border = Border
  { topEdge :: !Text
  , bottomEdge :: !Text
  , leftEdge :: !Text
  , rightEdge :: !Text
  , topLeftCorner :: !Text
  , topRightCorner :: !Text
  , bottomLeftCorner :: !Text
  , bottomRightCorner :: !Text
  , middleLeft :: !Text
  , middleRight :: !Text
  , middle :: !Text
  , middleTop :: !Text
  , middleBottom :: !Text
  }
  deriving (Eq, Show)

-- | Thin lines with square corners: ┌ ┐ └ ┘.
normalBorder :: Border
normalBorder = Border "─" "─" "│" "│" "┌" "┐" "└" "┘" "├" "┤" "┼" "┬" "┴"

-- | Thin lines with rounded corners: ╭ ╮ ╰ ╯.
roundedBorder :: Border
roundedBorder = normalBorder {topLeftCorner = "╭", topRightCorner = "╮", bottomLeftCorner = "╰", bottomRightCorner = "╯"}

-- | Thick lines: ┏ ┓ ┗ ┛.
thickBorder :: Border
thickBorder = Border "━" "━" "┃" "┃" "┏" "┓" "┗" "┛" "┣" "┫" "╋" "┳" "┻"

-- | Double lines: ╔ ╗ ╚ ╝.
doubleBorder :: Border
doubleBorder = Border "═" "═" "║" "║" "╔" "╗" "╚" "╝" "╠" "╣" "╬" "╦" "╩"

-- | Full blocks (█) all round.
blockBorder :: Border
blockBorder = Border "█" "█" "█" "█" "█" "█" "█" "█" "█" "█" "█" "█" "█"

-- | Half blocks on the outer half of each border cell: ▛ ▜ ▙ ▟. It has
-- no pieces for table rules.
outerHalfBlockBorder :: Border
outerHalfBlockBorder = Border "▀" "▄" "▌" "▐" "▛" "▜" "▙" "▟" "" "" "" "" ""

-- | Half blocks on the inner half of each border cell: ▗ ▖ ▝ ▘. It has
-- no pieces for table rules.
innerHalfBlockBorder :: Border
innerHalfBlockBorder = Border "▄" "▀" "▐" "▌" "▗" "▖" "▝" "▘" "" "" "" "" ""

-- | Spaces: takes the room of a border without drawing one.
hiddenBorder :: Border
hiddenBorder = Border " " " " " " " " " " " " " " " " " " " " " " " " " "

-- | ASCII only: @-@, @|@ and @+@ for the corners and joins.
asciiBorder :: Border
asciiBorder = Border "-" "-" "|" "|" "+" "+" "+" "+" "+" "+" "+" "+" "+"

-- | Markdown table syntax: @-@ and @|@, with @|@ at the corners and
-- joins. For a Markdown table, switch off its top and bottom sides.
markdownBorder :: Border
markdownBorder = Border "-" "-" "|" "|" "|" "|" "|" "|" "|" "|" "|" "|" "|"
