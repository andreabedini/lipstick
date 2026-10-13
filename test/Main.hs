{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import Lipstick
import Lipstick.Terminal (profileFromEnv)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main =
  defaultMain $
    testGroup
      "lipstick"
      [ widthTests
      , colorTests
      , renderTests
      , layoutTests
      , ansiTests
      , profileTests
      , properties
      ]

plainLines :: Block -> [Text]
plainLines = T.lines . renderPlain

widthTests :: TestTree
widthTests =
  testGroup
    "width"
    [ testCase "ascii" $ textWidth "hello" @?= 5
    , testCase "CJK is wide" $ textWidth "日本語" @?= 6
    , testCase "combining marks are zero width" $ textWidth "e\x0301" @?= 1
    , testCase "emoji" $ textWidth "🎨" @?= 2
    , testCase "ZWJ sequence is one cluster" $ textWidth "👩\x200D💻" @?= 2
    , testCase "flag is one cluster" $ textWidth "🇮🇹" @?= 2
    , testCase "VS16 makes text emoji wide" $ textWidth "❤\xFE0F" @?= 2
    ]

colorTests :: TestTree
colorTests =
  testGroup
    "colour"
    [ testCase "hex literal" $ ("#ff8000" :: Color) @?= RGB 255 128 0
    , testCase "short hex" $ hex "#f80" @?= Just (RGB 255 136 0)
    , testCase "palette literal" $ ("201" :: Color) @?= Color256 201
    , testCase "basic literal" $ ("5" :: Color) @?= Color16 5
    , testCase "invalid hex" $ hex "#ggg" @?= Nothing
    , testCase "red to 256" $ resolveColor ANSI256 True "#ff0000" @?= Just (TC256 196)
    , testCase "grey to 256" $ resolveColor ANSI256 True "#808080" @?= Just (TC256 244)
    , testCase "black to 256" $ resolveColor ANSI256 True "#000000" @?= Just (TC256 16)
    , testCase "red to 16" $ resolveColor ANSI16 True "#ff0000" @?= Just (TC16 9)
    , testCase "no colour in ascii" $ resolveColor Ascii True "#ff0000" @?= Nothing
    , testCase "light/dark" $ do
        resolveColor TrueColor True (lightDark "1" "2") @?= Just (TC16 2)
        resolveColor TrueColor False (lightDark "1" "2") @?= Just (TC16 1)
    , testCase "complete" $
        resolveColor ANSI256 True (complete "#ff0000" "100" "3") @?= Just (TC256 100)
    ]

renderTests :: TestTree
renderTests =
  testGroup
    "render"
    [ testCase "plain" $ plainLines (render mempty "a\nbcd") @?= ["a  ", "bcd"]
    , testCase "padding" $
        plainLines (render (padding 1 2) "hi") @?= ["      ", "  hi  ", "      "]
    , testCase "rounded border" $
        plainLines (render (border roundedBorder) "hi") @?= ["╭──╮", "│hi│", "╰──╯"]
    , testCase "border with some sides off" $
        plainLines (render (border normalBorder <> borderLeft False <> borderRight False) "hi")
          @?= ["──", "hi", "──"]
    , testCase "width includes padding and border" $ do
        let b = render (width 10 <> padding 0 1 <> border normalBorder) "hi"
        blockWidth b @?= 10
        plainLines b @?= ["┌────────┐", "│ hi     │", "└────────┘"]
    , testCase "width wraps words" $
        plainLines (render (width 10) "the quick brown fox")
          @?= ["the quick ", "brown fox "]
    , testCase "long words are broken" $
        plainLines (render (width 4) "abcdefghij") @?= ["abcd", "efgh", "ij  "]
    , testCase "center alignment" $
        plainLines (render (width 7 <> align center) "abc") @?= ["  abc  "]
    , testCase "right alignment" $
        plainLines (render (width 5 <> align right) "ab\nabc") @?= ["   ab", "  abc"]
    , testCase "height and vertical alignment" $
        plainLines (render (height 3 <> valign bottom) "x") @?= [" ", " ", "x"]
    , testCase "margins" $
        plainLines (render (margin 1 1) "x") @?= ["   ", " x ", "   "]
    , testCase "maxWidth crops" $
        plainLines (render (maxWidth 3) "abcdef") @?= ["abc"]
    , testCase "maxWidth replaces a cut wide character" $
        plainLines (render (maxWidth 3) "日本") @?= ["日 "]
    , testCase "tabs" $ plainLines (render (tabWidth 2) "a\tb") @?= ["a  b"]
    , testCase "frame does not rewrap" $
        plainLines (frame (border asciiBorder) (text "a b c"))
          @?= ["+-----+", "|a b c|", "+-----+"]
    , testCase "inherit keeps padding" $
        plainLines (render (paddingLeft 1 <> inherit (paddingLeft 5 <> bold)) "x") @?= [" x"]
    ]

layoutTests :: TestTree
layoutTests =
  testGroup
    "layout"
    [ testCase "joinHorizontal top" $
        plainLines (hcat [text "a\nb", text "c"]) @?= ["ac", "b "]
    , testCase "joinHorizontal bottom" $
        plainLines (joinHorizontal bottom [text "a\nb", text "c"]) @?= ["a ", "bc"]
    , testCase "joinVertical center" $
        plainLines (joinVertical center [text "abcde", text "x"]) @?= ["abcde", "  x  "]
    , testCase "place" $
        plainLines (place (width 5 <> height 3 <> align center <> valign center) "x")
          @?= ["     ", "  x  ", "     "]
    , testCase "place with whitespace chars" $
        plainLines (place (width 5 <> align right <> whitespaceChars "ab") "x") @?= ["abab" <> "x"]
    , testCase "empty block" $ do
        blockWidth emptyBlock @?= 0
        blockHeight emptyBlock @?= 0
        plainLines (hcat [emptyBlock, text "x"]) @?= ["x"]
    ]

ansiTests :: TestTree
ansiTests =
  testGroup
    "ansi"
    [ testCase "bold and colour" $
        toAnsi defaultRenderEnv (render (bold <> fg "#ff0000") "hi")
          @?= "\ESC[0;1;38;2;255;0;0mhi\ESC[0m"
    , testCase "downsampled to 256" $
        toAnsi (RenderEnv ANSI256 True) (render (fg "#ff0000") "hi")
          @?= "\ESC[0;38;5;196mhi\ESC[0m"
    , testCase "basic colours" $
        toAnsi (RenderEnv ANSI16 True) (render (fg red <> bg brightBlue) "hi")
          @?= "\ESC[0;31;104mhi\ESC[0m"
    , testCase "ascii keeps attributes, drops colours" $
        toAnsi (RenderEnv Ascii True) (render (bold <> fg red) "hi")
          @?= "\ESC[0;1mhi\ESC[0m"
    , testCase "NoTTY is plain" $
        toAnsi (RenderEnv NoTTY True) (render (bold <> fg red <> link "x") "hi") @?= "hi"
    , testCase "nested style keeps outer background" $
        toAnsi defaultRenderEnv (render (bg "1") (styled bold "a" <> "b"))
          @?= "\ESC[0;1;41ma\ESC[0;41mb\ESC[0m"
    , testCase "padding gets only the background" $
        toAnsi defaultRenderEnv (render (underline <> bg "1" <> paddingLeft 1) "a")
          @?= "\ESC[0;41m \ESC[0;4;41ma\ESC[0m"
    , testCase "curly underline downgraded below true colour" $
        toAnsi (RenderEnv ANSI256 True) (render (underlineStyle CurlyUnderline) "a")
          @?= "\ESC[0;4ma\ESC[0m"
    , testCase "hyperlink" $
        toAnsi defaultRenderEnv (render (link "https://x.y") "a")
          @?= "\ESC]8;;https://x.y\ESC\\a\ESC]8;;\ESC\\"
    , testCase "light/dark resolved at output" $ do
        let b = render (fg (lightDark "1" "2")) "a"
        toAnsi (RenderEnv TrueColor True) b @?= "\ESC[0;32ma\ESC[0m"
        toAnsi (RenderEnv TrueColor False) b @?= "\ESC[0;31ma\ESC[0m"
    ]

profileTests :: TestTree
profileTests =
  testGroup
    "profile detection"
    [ testCase "not a terminal" $ profileFromEnv False [("TERM", "xterm-256color")] @?= NoTTY
    , testCase "forced" $ profileFromEnv False [("CLICOLOR_FORCE", "1"), ("TERM", "xterm-256color")] @?= ANSI256
    , testCase "force=0 is not forced" $ profileFromEnv False [("CLICOLOR_FORCE", "0")] @?= NoTTY
    , testCase "NO_COLOR" $ profileFromEnv True [("NO_COLOR", "1"), ("COLORTERM", "truecolor")] @?= Ascii
    , testCase "COLORTERM" $ profileFromEnv True [("COLORTERM", "truecolor"), ("TERM", "xterm")] @?= TrueColor
    , testCase "256 colours" $ profileFromEnv True [("TERM", "screen-256color")] @?= ANSI256
    , testCase "kitty" $ profileFromEnv True [("TERM", "xterm-kitty")] @?= TrueColor
    , testCase "ghostty" $ profileFromEnv True [("TERM", "xterm-ghostty")] @?= TrueColor
    , testCase "plain xterm" $ profileFromEnv True [("TERM", "xterm")] @?= ANSI16
    ]

-- Properties ------------------------------------------------------------------

newtype Words = Words Text
  deriving (Show)

instance Arbitrary Words where
  arbitrary = do
    ws <- listOf (elements ["a", "bb", "ccc", "日本", "🎨", "e\x0301", "longerword", " ", "\n", "x\ty"])
    pure (Words (T.concat ws))

data StyleSpec = StyleSpec
  { specWidth :: Maybe Int
  , specPadding :: (Int, Int)
  , specMargin :: (Int, Int)
  , specBorder :: Bool
  }
  deriving (Show)

instance Arbitrary StyleSpec where
  arbitrary =
    StyleSpec
      <$> oneof [pure Nothing, Just <$> chooseInt (6, 30)]
      <*> ((,) <$> chooseInt (0, 2) <*> chooseInt (0, 2))
      <*> ((,) <$> chooseInt (0, 2) <*> chooseInt (0, 2))
      <*> arbitrary

specMod :: StyleSpec -> Mod Style
specMod (StyleSpec w (pv, ph) (mv, mh) b) =
  maybe mempty width w
    <> padding pv ph
    <> margin mv mh
    <> (if b then border roundedBorder else mempty)

-- | Every line of the plain output is exactly as wide as the block says.
rectangular :: Block -> Property
rectangular b =
  let ls = plainLines b
   in counterexample (show ls) $
        conjoin
          [ length ls === blockHeight b
          , conjoin [textWidth l === blockWidth b | l <- ls]
          ]

stripAnsi :: Text -> Text
stripAnsi t = case T.breakOn "\ESC" t of
  (before, rest)
    | T.null rest -> before
    | "\ESC]" `T.isPrefixOf` rest ->
        before <> stripAnsi (T.drop 2 (snd (T.breakOn "\ESC\\" (T.drop 2 rest))))
    | otherwise -> before <> stripAnsi (T.drop 1 (T.dropWhile (/= 'm') rest))

properties :: TestTree
properties =
  testGroup
    "properties"
    [ testProperty "render is rectangular" $ \spec (Words t) ->
        rectangular (render (specMod spec) (plain t))
    , testProperty "width is respected" $ \spec (Words t) ->
        let b = render (specMod spec) (plain t)
            (_, ph) = specPadding spec
            (_, mh) = specMargin spec
            frameW = 2 * ph + (if specBorder spec then 2 else 0)
         in case specWidth spec of
              -- Room for at least one wide character inside the frame.
              Just w | w - frameW >= 2 -> blockWidth b === w + 2 * mh
              _ -> property True
    , testProperty "joinHorizontal is rectangular and adds widths" $ \specs ->
        let bs = [render (specMod s) "some text" | s <- take 4 specs]
            j = hcat bs
         in rectangular j .&&. blockWidth j === sum (map blockWidth bs)
    , testProperty "joinVertical is rectangular and adds heights" $ \specs ->
        let bs = [render (specMod s) "some text" | s <- take 4 specs]
            j = vcat bs
         in rectangular j .&&. blockHeight j === sum (map blockHeight bs)
    , testProperty "stripping ANSI gives the plain text" $ \spec (Words t) ->
        let b = render (specMod spec <> bold <> fg "#123456" <> bg "200" <> link "u") (plain t)
         in stripAnsi (toAnsi defaultRenderEnv b) === renderPlain b
    , testProperty "maxWidth bounds the width" $ \spec (Words t) (Positive n) ->
        let b = render (specMod spec <> maxWidth n) (plain t)
         in rectangular b .&&. blockWidth b <= n
    ]
