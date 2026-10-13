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
  putBlock $ render (link "https://github.com/charmbracelet/lipgloss" <> underline) "A hyperlink to lipgloss"
