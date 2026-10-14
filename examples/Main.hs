{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
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

languages :: Block
languages =
  table
    ( borderFg purple
        <> width 46
        <> headerStyle (bold <> fg pink <> align center)
        <> cellStyle (\case BodyCell r _ | odd r -> padding 0 1 <> fg grey; _ -> padding 0 1)
    )
    ["Language", "Formal", "Informal"]
    [ ["Chinese", "您好", "你好"]
    , ["Japanese", "こんにちは", "やあ"]
    , ["Arabic", "أهلين", "أهلا"]
    , ["Russian", "Здравствуйте", "Привет"]
    , ["Spanish", "Hola", "¿Qué tal?"]
    ]

outlines :: Block
outlines =
  joinHorizontal
    top
    [ tree
        (enumerator roundedTreeEnumerator <> enumeratorStyle (fg purple) <> rootStyle (bold <> fg pink))
        (Node "." [Node "macOS" [], Node "Linux" [Node "NixOS" [], Node "Arch" [], Node "Debian" []], Node "BSD" []])
    , render (paddingLeft 6) ""
    , nestedList
        ( enumerator (byDepth (\d -> if d == 0 then roman else bullet))
            <> enumeratorStyleWith (\p -> fg (if itemDepth p == 0 then pink else purple))
        )
        [ Node "Glossier" [Node "Cloud Paint" [], Node "Boy Brow" []]
        , Node "Nyx" []
        , Node "Mac" [Node "Ruby Woo" []]
        ]
    ]

-- | A dialog composited over a background, with a drop shadow.
dialogScene :: Block
dialogScene =
  compose
    [ layer mempty (render (fg grey) (plain (T.unlines (replicate 8 (T.replicate 60 "╱")))))
    , layer (offset 13 2 <> zIndex 1) (render (bg "#222222" <> width 34 <> height 6) "")
    , layer
        (offset 11 1 <> zIndex 2 <> layerId "dialog")
        ( render
            (border roundedBorder <> borderFg pink <> bg "#3C3C3C" <> padding 1 2 <> width 34 <> align center)
            "Are you sure you want to eat marmalade?"
        )
    ]

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
  putBlock languages
  putBlock outlines
  putBlock dialogScene
  putBlock swatches
  putBlock gradientCard
  putBlock $ render (link "https://github.com/charmbracelet/lipgloss" <> underline) "A hyperlink to lipgloss"
