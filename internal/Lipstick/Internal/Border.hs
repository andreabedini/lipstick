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

normalBorder :: Border
normalBorder = Border "─" "─" "│" "│" "┌" "┐" "└" "┘" "├" "┤" "┼" "┬" "┴"

roundedBorder :: Border
roundedBorder = normalBorder {topLeftCorner = "╭", topRightCorner = "╮", bottomLeftCorner = "╰", bottomRightCorner = "╯"}

thickBorder :: Border
thickBorder = Border "━" "━" "┃" "┃" "┏" "┓" "┗" "┛" "┣" "┫" "╋" "┳" "┻"

doubleBorder :: Border
doubleBorder = Border "═" "═" "║" "║" "╔" "╗" "╚" "╝" "╠" "╣" "╬" "╦" "╩"

blockBorder :: Border
blockBorder = Border "█" "█" "█" "█" "█" "█" "█" "█" "█" "█" "█" "█" "█"

outerHalfBlockBorder :: Border
outerHalfBlockBorder = Border "▀" "▄" "▌" "▐" "▛" "▜" "▙" "▟" "" "" "" "" ""

innerHalfBlockBorder :: Border
innerHalfBlockBorder = Border "▄" "▀" "▐" "▌" "▗" "▖" "▝" "▘" "" "" "" "" ""

hiddenBorder :: Border
hiddenBorder = Border " " " " " " " " " " " " " " " " " " " " " " " " " "

asciiBorder :: Border
asciiBorder = Border "-" "-" "|" "|" "+" "+" "+" "+" "+" "+" "+" "+" "+"

markdownBorder :: Border
markdownBorder = Border "-" "-" "|" "|" "|" "|" "|" "|" "|" "|" "|" "|" "|"
