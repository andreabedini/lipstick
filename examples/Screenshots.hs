{-# LANGUAGE OverloadedStrings #-}

-- | The examples shown in the README, and their screenshots.
--
-- > cabal run -v0 lipstick-screenshots -- docs/images   # write the SVGs
-- > cabal run -v0 lipstick-screenshots -- --ansi table  # print one example
module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lipstick
import Svg (blockSvg)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))

card :: Mod Style
card =
  bold
    <> fg "#FAFAFA"
    <> bg "#7D56F4"
    <> padding 1 4
    <> width 24
    <> border roundedBorder
    <> borderFg "#874BFD"

warning :: Mod Style
warning = card <> bg "#F25D94"

quickstart :: Block
quickstart = render card "Hello, kitty"

layout :: Block
layout =
  joinHorizontal
    center
    [ render card "Hello, kitty"
    , render (warning <> marginLeft 2) "Later settings win"
    ]

placed :: Block
placed =
  place
    (width 30 <> height 3 <> align center <> valign center <> whitespaceChars "·" <> fg "#383838")
    (render (bold <> fg "#F25D94") "centred")

inlineText :: Block
inlineText =
  render
    (width 30 <> border normalBorder <> borderFg "#874BFD" <> padding 0 1)
    (styled (bold <> fg "#F25D94") "Error: " <> "the file " <> styled italic "notes.txt" <> " was not found")

languages :: Block
languages =
  table
    ( borderFg "#874BFD"
        <> headerStyle (bold <> fg "#F25D94" <> align center)
        <> cellStyle (\case BodyCell r _ | odd r -> padding 0 1 <> faint; _ -> padding 0 1)
    )
    ["Language", "Formal", "Informal"]
    [ ["Chinese", "您好", "你好"]
    , ["Japanese", "こんにちは", "やあ"]
    , ["Russian", "Здравствуйте", "Привет"]
    ]

outlines :: Block
outlines =
  joinHorizontal
    top
    [ tree
        (enumerator roundedTreeEnumerator <> enumeratorStyle (fg "#874BFD") <> rootStyle (fg "#F25D94"))
        (Node "." [Node "macOS" [], Node "Linux" [Node "NixOS" [], Node "Arch" []], Node "BSD" []])
    , text "      "
    , nestedList
        ( enumeratorBy (\depth -> if depth == 0 then roman else bullet)
            <> enumeratorStyle (fg "#F25D94")
        )
        [Node "Glossier" [Node "Cloud Paint" [], Node "Boy Brow" []], Node "Nyx" []]
    ]

layers :: Block
layers =
  compose
    [ layer (fg "#383838") (text (T.intercalate "\n" (replicate 6 (T.replicate 34 "╱"))))
    , layer (offset 7 2 <> zIndex 1) (render (bg "#0A0A0A" <> width 22 <> height 3) "")
    , layer
        (offset 5 1 <> zIndex 2 <> layerId "dialog")
        (render (border roundedBorder <> borderFg "#F25D94" <> bg "#3C3C3C" <> padding 0 2 <> width 22 <> align center) "Really quit?")
    ]

colours :: Block
colours =
  vcat
    [ hcat [render (bg c) "  " | c <- blend 24 ["#7D56F4", "#F25D94", "#FAD45A"]]
    , text ""
    , render
        (border roundedBorder <> borderFgBlend ["#7D56F4", "#F25D94", "#FAD45A"] <> padding 1 4)
        ( styled (fg (darken 0.4 "#F25D94")) "darker "
            <> styled (fg "#F25D94") "pink "
            <> styled (fg (lighten 0.4 "#F25D94")) "lighter"
        )
    ]

examples :: [(String, Block)]
examples =
  [ ("quickstart", quickstart)
  , ("layout", layout)
  , ("place", placed)
  , ("text", inlineText)
  , ("table", languages)
  , ("outlines", outlines)
  , ("layers", layers)
  , ("colours", colours)
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--ansi", name] | Just b <- lookup name examples -> T.putStrLn (toAnsi defaultRenderEnv b)
    [dir] -> mapM_ (\(name, b) -> T.writeFile (dir </> name <> ".svg") (blockSvg b)) examples
    _ -> die ("usage: lipstick-screenshots DIR | --ansi (" ++ unwords (map fst examples) ++ ")")
