{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lipstick

purple, pink, grey :: Color
purple = lightDark "#874BFD" "#AD8CFF"
pink = "#F25D94"
grey = lightDark "#D9DCCF" "#383838"

card :: Mod Style
card =
  bold
    <> fg "#FAFAFA"
    <> bg "#7D56F4"
    <> padding 1 4
    <> width 24
    <> border roundedBorder
    <> borderFg purple

tab :: Mod Style
tab = padding 0 1 <> border normalBorder <> borderFg purple

borders :: Block
borders =
  hcat
    [ render (tab <> border b <> marginRight 1) name
    | (name, b) <-
        [ ("normal", normalBorder)
        , ("rounded", roundedBorder)
        , ("thick", thickBorder)
        , ("double", doubleBorder)
        , ("ascii", asciiBorder)
        , ("block", blockBorder)
        ]
    ]

paragraph :: Block
paragraph =
  render
    (width 40 <> padding 1 2 <> border doubleBorder <> borderFg pink <> align center)
    ( "Lipstick renders to blocks, not strings. "
        <> styled (italic <> fg pink) "Widths are exact"
        <> ", even for 日本語 and emoji 🎨, and "
        <> styled (underlineStyle CurlyUnderline <> underlineColor pink) "nested styles"
        <> " keep the outer background."
    )

-- | A gradient made of blocks with blended backgrounds.
swatches :: Block
swatches =
  hcat
    [ render (bg c <> width 3) " "
    | c <- blend 24 ["#7D56F4", "#F25D94", "#FAD45A"]
    ]

gradientCard :: Block
gradientCard =
  render
    (padding 1 3 <> border roundedBorder <> borderFgBlend ["#7D56F4", "#F25D94", "#FAD45A"] <> align center)
    ( "Gradient borders, "
        <> styled (fg (darken 0.3 pink)) "darker"
        <> " and "
        <> styled (fg (lighten 0.4 pink)) "lighter"
        <> " colours\n"
        <> fromAnsi "and \ESC[1;32mANSI\ESC[0m from \ESC]8;;https://example.com\ESC\\other tools\ESC]8;;\ESC\\"
    )

main :: IO ()
main = do
  putBlock $
    joinHorizontal
      center
      [ render card "Hello, kitty"
      , render (card <> bg pink <> marginLeft 2) "Override by appending"
      ]
  putBlock borders
  putBlock paragraph
  putBlock $
    place
      (width 60 <> height 5 <> align center <> valign center <> fg grey <> whitespaceChars "·")
      (render (bold <> fg pink) "placed in the middle")
  putBlock swatches
  putBlock gradientCard
  putBlock $ render (link "https://github.com/charmbracelet/lipgloss" <> underline) "A hyperlink to lipgloss"
