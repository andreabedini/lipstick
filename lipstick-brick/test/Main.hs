{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick.Main (renderWidget)
import Brick.Widgets.Core (raw)
import Brick.Types (Size (..), Widget, hSize)
import Graphics.Vty qualified as V
import Lipstick
import Lipstick.Brick
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "lipstick-brick"
      [ testCase "image has the block's size" $ do
          let b = render (border roundedBorder <> padding 1 2 <> width 20) "Hello, brick"
              img = blockImage defaultRenderEnv b
          (V.imageWidth img, V.imageHeight img) @?= (blockWidth b, blockHeight b)
      , testCase "tables keep their size" $ do
          let b = table (width 30) ["A", "B"] [["one", "two three four five"]]
              img = blockImage defaultRenderEnv b
          (V.imageWidth img, V.imageHeight img) @?= (blockWidth b, blockHeight b)
      , testCase "zero-width blocks are empty" $
          blockImage defaultRenderEnv (render (paddingTop 2) "") @?= V.emptyImage
      , testCase "attributes" $ do
          let img = blockImage defaultRenderEnv (render (bold <> fg red <> bg "#102030" <> link "https://x.y") "a")
              expected =
                V.defAttr
                  `V.withStyle` V.bold
                  `V.withForeColor` V.ISOColor 1
                  `V.withBackColor` V.RGBColor 16 32 48
                  `V.withURL` "https://x.y"
          img @?= V.text' expected "a"
      , testCase "link controls are percent-encoded" $
          blockImage defaultRenderEnv (render (link "http://x\ESC\\ y") "a")
            @?= V.text' (V.defAttr `V.withURL` "http://x%1B\\%20y") "a"
      , testCase "256 colours" $
          blockImage defaultRenderEnv (render (fg "200") "a") @?= V.text' (V.defAttr `V.withForeColor` V.Color240 184) "a"
      , testCase "light and dark backgrounds" $ do
          let b = render (fg (lightDark "1" "2")) "a"
          blockImage (RenderEnv TrueColor False Nothing) b @?= V.text' (V.defAttr `V.withForeColor` V.ISOColor 1) "a"
          blockImage (RenderEnv TrueColor True Nothing) b @?= V.text' (V.defAttr `V.withForeColor` V.ISOColor 2) "a"
      , testCase "a block that grows is a greedy widget" $ do
          hSize (blockWidget defaultRenderEnv (render fillWidth "x") :: Widget ()) @?= Greedy
          hSize (blockWidget defaultRenderEnv (render mempty "x") :: Widget ()) @?= Fixed
      , testCase "a widget takes the width brick gives it when it grows" $
          map V.imageWidth (V.picLayers (shown (render (fillWidth <> bg red) "x") (12, 3))) @?= [12]
      , testCase "a widget wraps its text to the width brick gives it" $ do
          let b = text "the quick brown fox"
              at env = V.picLayers (renderWidget Nothing [raw (blockImage env b) :: Widget ()] (10, 5))
          V.picLayers (shown b (10, 5)) @?= at defaultRenderEnv {envWidth = Just 10}
          assertBool "laid out at its natural width it would be cut, not wrapped" $
            V.picLayers (shown b (10, 5)) /= at defaultRenderEnv
      , testCase "colour modes" $ do
          profileForColorMode V.FullColor @?= TrueColor
          profileForColorMode (V.ColorMode240 240) @?= ANSI256
          profileForColorMode V.NoColor @?= Ascii
      ]
  where
    shown b region = renderWidget Nothing [blockWidget defaultRenderEnv b :: Widget ()] region
