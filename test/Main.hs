{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (chr)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as Utf8
import Lipstick
import Lipstick.Block qualified as B
import Lipstick.Grid (Span (..), gridLines, lineSpans, lineWidth)
import Lipstick.Grid qualified as G
import Lipstick.Internal.Color (Color (..))
import Lipstick.Internal.Layout (Assign (..), Measured (..), Request (..), allocate, allocateWithin, measure)
import Lipstick.Mod qualified as M
import Lipstick.Terminal (backgroundFromEnv, profileFromEnv, widthFromEnv)
import Lipstick.Width (clusters)
import Numeric (readHex)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, hSetEncoding, mkTextEncoding, openTempFile)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = do
  graphemeCases <- readGraphemeTests "test/data/GraphemeBreakTest.txt"
  defaultMain $
    testGroup
      "lipstick"
      [ widthTests
      , graphemeTests graphemeCases
      , colorTests
      , blendTests
      , fromAnsiTests
      , extrasTests
      , tableTests
      , outlineTests
      , layerTests
      , commonPropsTests
      , styleParityTests
      , textParityTests
      , tableParityTests
      , outlineParityTests
      , renderTests
      , layoutTests
      , ansiTests
      , profileTests
      , allocateTests
      , inheritanceTests
      , deferredLayoutTests
      , properties
      ]

plainLines :: Block -> [Text]
plainLines = T.lines . renderPlain

-- | Blocks are equal when they lay out the same at their natural width.
-- | An environment, field by field.
renderEnv :: ColorProfile -> Bool -> Maybe Int -> RenderEnv
renderEnv p dark w = defaultRenderEnv {envProfile = p, envDarkBackground = dark, envWidth = w}

rectOf :: Rect -> (Int, Int, Int, Int)
rectOf r = (rectX r, rectY r, rectWidth r, rectHeight r)

(@?~) :: Block -> Block -> Assertion
a @?~ b = layout Nothing a @?= layout Nothing b

infix 1 @?~

-- | The plain lines of a block laid out in a width.
plainLinesAt :: Int -> Block -> [Text]
plainLinesAt w = T.lines . toAnsi (renderEnv NoTTY True (Just w))

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
    , testCase "VS15 does not change the width" $ do
        textWidth "⌚\xFE0E" @?= 2
        textWidth "😀\xFE0E" @?= 2
        textWidth "\x2764\xFE0E" @?= 1
    , testCase "skin tone modifiers are wide and make an emoji wide" $ do
        map textWidth ["\x261D\x1F3FB", "\x270C\x1F3FD", "👍\x1F3FB", "\x1F3FB", "\x1F3FB\x1F3FF"] @?= [2, 2, 2, 2, 2]
        blockWidth (render (border normalBorder) "\x1F3FBx") @?= 5
    , testCase "keycap" $ textWidth "1\xFE0F\x20E3" @?= 2
    , testCase "skin tone modifier" $ textWidth "👍🏽" @?= 2
    , testCase "family ZWJ sequence" $ textWidth "👨\x200D👩\x200D👧" @?= 2
    , testCase "precomposed Hangul" $ textWidth "한국" @?= 4
    , testCase "conjoining Hangul jamo" $ textWidth "\x1112\x1161\x11AB" @?= 2
    , testCase "Devanagari conjunct is one cluster" $ length (clusters "क्ष") @?= 1
    , testCase "zero width space" $ textWidth "a\x200B\&b" @?= 2
    , testCase "ambiguous width is narrow" $ textWidth "±" @?= 1
    , testCase "CJK extension B" $ textWidth "\x20000" @?= 2
    , testCase "unicode version" $ unicodeVersion @?= "16.0.0"
    , testCase "a cluster split across pieces is measured whole" $ do
        blockWidth (fromInline ("\x2764" <> "\xFE0F" <> "x")) @?= 3
        blockWidth (fromInline (styled bold "👨" <> "\x200D👩")) @?= 2
    , testCase "a cluster split across pieces takes the first piece's style" $
        toAnsi defaultRenderEnv (fromInline (styled bold "\x2764" <> "\xFE0F"))
          @?= "\ESC[0;1m\x2764\xFE0F\ESC[0m"
    , testCase "bidi embeddings, overrides and isolates are dropped" $
        renderPlain (render (border normalBorder) "a\x202E\&b\x2067\&c\x2069\&d\x202C")
          @?= "┌────┐\n│abcd│\n└────┘"
    , testCase "bidi marks are kept" $
        renderPlain (text "a\x200F\&b\x200E\&c\x061C\&d") @?= "a\x200F\&b\x200E\&c\x061C\&d"
    , testCase "styleRanges counts a tab as four columns" $
        toAnsi defaultRenderEnv (fromInline (styleRanges [(4, 5, bold)] "\tab"))
          @?= "    \ESC[0;1ma\ESC[0mb"
    , testCase "styleIndices counts a split cluster once" $
        toAnsi defaultRenderEnv (fromInline (styleIndices [1] bold mempty ("\x2764" <> "\xFE0F" <> "x")))
          @?= "\x2764\xFE0F\ESC[0;1mx\ESC[0m"
    ]

-- | Test cases from the Unicode conformance file: the text and its
-- expected clusters.
readGraphemeTests :: FilePath -> IO [(Text, [Text])]
readGraphemeTests path = do
  contents <- Utf8.readFile path
  pure
    [ (T.concat cs, cs)
    | l <- T.lines contents
    , not (T.null l)
    , not ("#" `T.isPrefixOf` l)
    , let cs = map (T.pack . map hexChar . filter (/= "×") . T.words) (filter (not . T.null . T.strip) (T.splitOn "÷" l))
    ]
  where
    hexChar w = case readHex (T.unpack w) of
      [(n, "")] -> chr n
      _ -> error ("bad code point " ++ show w)

graphemeTests :: [(Text, [Text])] -> TestTree
graphemeTests cases =
  testGroup
    "grapheme clusters (UAX #29 conformance)"
    [ testCase "file has test cases" $ assertBool "no cases" (length cases > 1000)
    , testCase "all cases" $ do
        let failures = [(t, expected, map fst (clusters t)) | (t, expected) <- cases, map fst (clusters t) /= expected]
        assertBool
          (show (length failures) ++ " failures, first: " ++ show (take 3 failures))
          (null failures)
    ]

colorTests :: TestTree
colorTests =
  testGroup
    "colour"
    [ testCase "hex literal" $ ("#ff8000" :: Color) @?= RGB 255 128 0
    , testCase "short hex" $ hex "#f80" @?= Just (RGB 255 136 0)
    , testCase "palette literal" $ ("201" :: Color) @?= Color256 201
    , testCase "basic literal" $ ("5" :: Color) @?= Color16 5
    , testCase "colours show as the functions that make them" $ do
        show ("#7D56F4" :: Color) @?= "rgb 125 86 244"
        show (lightDark red (ansi256 200)) @?= "lightDark (ansi 1) (ansi256 200)"
        show (complete "#000" "16" "0") @?= "complete (rgb 0 0 0) (ansi256 16) (ansi 0)"
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

blendTests :: TestTree
blendTests =
  testGroup
    "colour manipulation"
    [ testCase "mix keeps the variants of complete colours" $
        mix 0.5 (complete (rgb 255 0 0) (ansi256 196) red) blue
          @?= complete (mix 0.5 (rgb 255 0 0) blue) (mix 0.5 (ansi256 196) blue) (mix 0.5 red blue)
    , testCase "mix keeps both kinds of adaptive colour" $
        mix 0.25 (lightDark white black) (complete red green blue)
          @?= lightDark
            (complete (mix 0.25 white red) (mix 0.25 white green) (mix 0.25 white blue))
            (complete (mix 0.25 black red) (mix 0.25 black green) (mix 0.25 black blue))
    , testCase "blend endpoints" $
        blend 3 ["#ff0000", "#0000ff"] @?= ["#ff0000", RGB 140 83 162, "#0000ff"]
    , testCase "blend through three stops" $ do
        let cs = blend 5 ["#000000", "#ffffff", "#000000"]
        length cs @?= 5
        cs !! 2 @?= "#ffffff"
        last cs @?= "#000000"
    , testCase "blend degenerate" $ do
        blend 0 ["#ff0000", "#0000ff"] @?= []
        blend 1 ["#ff0000", "#0000ff"] @?= ["#ff0000"]
        blend 2 ["#123456"] @?= ["#123456", "#123456"]
    , testCase "darken fully is black" $ darken 1 "#7D56F4" @?= "#000000"
    , testCase "lighten fully is white" $ lighten 1 "#7D56F4" @?= "#ffffff"
    , testCase "complementary of grey is grey" $ complementary "#808080" @?= "#808080"
    , testCase "adaptive colours stay adaptive" $
        darken 1 (lightDark "#ffffff" "#eeeeee") @?= lightDark "#000000" "#000000"
    , testCase "mixing adaptive with plain" $
        mix 0 (lightDark "#ff0000" "#00ff00") "#0000ff" @?= lightDark "#ff0000" "#00ff00"
    , testCase "palette colours become RGB" $ mix 0 red red @?= RGB 205 0 0
    , testProperty "OkLab round trip" $ \r g b ->
        mix 0 (RGB r g b) (RGB r g b) === RGB r g b
    ]

fromAnsiTests :: TestTree
fromAnsiTests =
  testGroup
    "fromAnsi"
    [ testCase "plain text" $ parsed "hello" @?= "hello"
    , testCase "SGR round trip" $
        parsed "\ESC[1;31mred\ESC[0m plain" @?= "\ESC[0;1;31mred\ESC[0m plain"
    , testCase "256 and RGB colours" $
        parsed "\ESC[38;5;200;48;2;1;2;3mx" @?= "\ESC[0;38;5;200;48;2;1;2;3mx\ESC[0m"
    , testCase "a colour cut short does not turn into attributes" $ do
        parsed "\ESC[38;5mA" @?= "A"
        parsed "\ESC[48;5mA" @?= "A"
        parsed "\ESC[1;38;2;1;2mA" @?= "\ESC[0;1mA\ESC[0m"
        parsed "\ESC[38:5mA" @?= "A"
    , testCase "a colour out of range is ignored" $ do
        parsed "\ESC[31;38;5;300;1mA" @?= "\ESC[0;1;31mA\ESC[0m"
        parsed "\ESC[38;2;999;0;0mA" @?= "A"
        parsed "\ESC[31;38:5:300mA" @?= "\ESC[0;31mA\ESC[0m"
        parsed "\ESC[18446744073709551647mA" @?= "A"
    , testCase "colon forms" $
        parsed "\ESC[4:3;58:2::10:20:30;38:5:9mx" @?= "\ESC[0;4:3;91;58;2;10;20;30mx\ESC[0m"
    , testCase "off codes unset" $
        parsed "\ESC[1;3;31mA\ESC[22;39mB" @?= "\ESC[0;1;3;31mA\ESC[0;3mB\ESC[0m"
    , testCase "empty SGR resets" $ parsed "\ESC[1mA\ESC[mB" @?= "\ESC[0;1mA\ESC[0mB"
    , testCase "hyperlink with ST" $
        parsed "\ESC]8;;http://a\ESC\\x\ESC]8;;\ESC\\y" @?= "\ESC]8;;http://a\ESC\\x\ESC]8;;\ESC\\y"
    , testCase "hyperlink with BEL" $
        parsed "\ESC]8;id=1;http://a\ax\ESC]8;;\a" @?= "\ESC]8;;http://a\ESC\\x\ESC]8;;\ESC\\"
    , testCase "other sequences are dropped" $
        parsed "\ESC[2K\ESC(Bab\ESC]0;title\acd\ESC7" @?= "abcd"
    , testCase "private and intermediate CSI ending in m are not SGR" $ do
        parsed "\ESC[1mA\ESC[>4;2mB" @?= "\ESC[0;1mAB\ESC[0m"
        parsed "\ESC[1m\ESC[<5mx\ESC[?0my" @?= "\ESC[0;1mxy\ESC[0m"
        parsed "\ESC[1m\ESC[0 mx" @?= "\ESC[0;1mx\ESC[0m"
    , testCase "a newline cuts an escape sequence short and stays" $ do
        plainLines (fromInline (fromAnsi "\ESC[31\nfoo")) @?= ["   ", "foo"]
        ansiHeight "a\ESC\nb" @?= 2
        ansiHeight "a\ESC(\nb" @?= 2
    , testCase "8-bit CSI" $ parsed "\x9B\&1mA" @?= "\ESC[0;1mA\ESC[0m"
    , testCase "control string payloads are dropped" $
        parsed "a\ESC_Gf=100;AAAA\ESC\\b\ESCPtmux;payload\ESC\\c\ESCXsos\ESC\\d\ESC^pm\ae" @?= "abcde"
    , testCase "tmux passthrough is dropped" $ do
        parsed "a\ESCPtmux;\ESC\ESC_Gf=100,a=T;iVBORw0KGgo=\ESC\ESC\\\ESC\\b" @?= "ab"
        parsed "\ESCPtmux;\ESC\ESC]52;c;aGVsbG8=\a\ESC\\after" @?= "after"
    , testCase "ESC ESC starts the sequence again" $
        parsed "\ESC\ESC[31mx" @?= "\ESC[0;31mx\ESC[0m"
    , testCase "8-bit control strings are dropped" $
        parsed "a\x90q#0;2\x9C\&b\x9F\&apc\x9C\&c\x98sos\ESC\\d" @?= "abcd"
    , testCase "an escape cuts a control string short" $
        parsed "\ESC]0;title\ESC[1mA\ESCPdcs\ESC[3mB" @?= "\ESC[0;1mA\ESC[0;1;3mB\ESC[0m"
    , testCase "hyperlink with a C1 control is escaped on output" $
        parsed "\ESC]8;;http://a\x9B\&2J\ESC\\x" @?= "\ESC]8;;http://a%C2%9B2J\ESC\\x\ESC]8;;\ESC\\"
    , testCase "keeps the outer style" $
        toAnsi defaultRenderEnv (render (bg "4") (fromAnsi "\ESC[1ma\ESC[0mb"))
          @?= "\ESC[0;1;44ma\ESC[0;44mb\ESC[0m"
    , testCase "escapes inside a cluster do not split it" $
        ansiWidth "\ESC[31m👩\ESC[0m\x200D💻" @?= 2
    , testCase "width ignores escapes" $
        blockWidth (render (border normalBorder) (fromAnsi "\ESC[31mred\ESC[0m")) @?= 5
    , testProperty "toAnsi . fromAnsi . toAnsi = toAnsi" $ \spec (StyledText inl) ->
        let b = render (specMod spec) inl
            out = toAnsi defaultRenderEnv b
         in toAnsi defaultRenderEnv (fromInline (fromAnsi out)) === out
    ]
  where
    parsed = toAnsi defaultRenderEnv . fromInline . fromAnsi

extrasTests :: TestTree
extrasTests =
  testGroup
    "style extras"
    [ testCase "inline ignores the box" $
        plainLines (render (inline <> padding 1 1 <> border normalBorder <> width 20) "a\nb") @?= ["ab"]
    , -- Quadratic versions took seconds (styleRanges) and minutes
      -- (styleIndices) on this text.
      localOption (mkTimeout 5000000) $ testCase "styleRanges and styleIndices take linear time" $ do
        let long = T.replicate 40000 "word "
            n = T.length long
            size = T.length . renderPlain . fromInline
        size (styleRanges [(0, n, bold)] (plain long)) @?= n
        size (styleIndices [0, 2 .. n] bold mempty (plain long)) @?= n
    , testCase "inline respects maxWidth" $
        plainLines (render (inline <> maxWidth 3) "abcdef") @?= ["abc"]
    , testCase "inline keeps attributes" $
        toAnsi defaultRenderEnv (render (inline <> bold) "a\nb") @?= "\ESC[0;1mab\ESC[0m"
    , testCase "padding char" $
        plainLines (render (padding 1 1 <> paddingChar '.') "x") @?= ["...", ".x.", "..."]
    , testCase "margin char" $
        plainLines (render (margin 0 2 <> marginChar '~') "x") @?= ["~~x~~"]
    , testCase "border gradient" $ do
        let b = render (border normalBorder <> borderFgBlend [red, blue]) "hi"
            ls = T.lines (toAnsi defaultRenderEnv b)
        renderPlain b @?= "┌──┐\n│hi│\n└──┘"
        case ls of
          [l1, l2, _] -> do
            assertBool "top-left corner is the first stop" $
              "\ESC[0;38;2;205;0;0m┌\ESC[" `T.isPrefixOf` l1
            assertBool "left border of the middle row is the last stop" $
              "\ESC[0;38;2;0;0;238m│" `T.isPrefixOf` l2
          _ -> assertFailure (show ls)
    , testCase "borderFg clears a gradient" $
        render (border normalBorder <> borderFgBlend [red, blue] <> borderFg green) "x"
          @?~ render (border normalBorder <> borderFg green) "x"
    ]

tableTests :: TestTree
tableTests =
  testGroup
    "table"
    [ testCase "defaults" $
        plainLines (table mempty ["A", "Bb"] [["1", "2"], ["333"]])
          @?= [ "╭───┬──╮"
              , "│A  │Bb│"
              , "├───┼──┤"
              , "│1  │2 │"
              , "│333│  │"
              , "╰───┴──╯"
              ]
    , testCase "row rules, no header rule, padded cells" $
        plainLines (table (border normalBorder <> rowRules True <> headerRule False <> cellStyle (const (padding 0 1))) ["A"] [["1"], ["2"]])
          @?= ["┌───┐", "│ A │", "│ 1 │", "├───┤", "│ 2 │", "└───┘"]
    , testCase "no column rules" $
        plainLines (table (border asciiBorder <> columnRules False) [] [["a", "b"]]) @?= ["+--+", "|ab|", "+--+"]
    , testCase "markdown" $
        plainLines (table (border markdownBorder <> borderTop False <> borderBottom False <> cellStyle (const (padding 0 1))) ["A", "B"] [["1", "2"]])
          @?= ["| A | B |", "|---|---|", "| 1 | 2 |"]
    , testCase "no outer border" $
        plainLines (table (borderTop False <> borderBottom False <> borderLeft False <> borderRight False) ["A", "B"] [["1", "2"]])
          @?= ["A│B", "─┼─", "1│2"]
    , testCase "width grows columns" $
        plainLines (table (width 9) ["A", "B"] [["1", "2"]])
          @?= ["╭───┬───╮", "│A  │B  │", "├───┼───┤", "│1  │2  │", "╰───┴───╯"]
    , testCase "width shrinks and wraps the widest column" $
        plainLines (table (width 12) ["Id", "Text"] [["1", "one two three"]])
          @?= ["╭──┬───────╮", "│Id│Text   │", "├──┼───────┤", "│1 │one two│", "│  │three  │", "╰──┴───────╯"]
    , testCase "noWrap truncates" $
        plainLines (table (width 12 <> noWrap) ["Id", "Text"] [["1", "one two three"]])
          @?= ["╭──┬───────╮", "│Id│Text   │", "├──┼───────┤", "│1 │one tw…│", "╰──┴───────╯"]
    , testCase "header style" $
        toAnsi defaultRenderEnv (table (border hiddenBorder <> borderTop False <> borderBottom False <> headerRule False <> columnRules False <> headerStyle bold) ["H"] [["b"]])
          @?= " \ESC[0;1mH\ESC[0m \n b "
    , testCase "cell positions" $ do
        let b = table (cellStyle (\c -> if cellRow c == Just 1 && cellColumn c == 0 then fg red else mempty) <> border hiddenBorder) ["h"] [["a"], ["b"]]
        assertBool "only the second body row is red" $
          T.count "\ESC[0;31m" (toAnsi defaultRenderEnv b) == 1 && "\ESC[0;31mb" `T.isInfixOf` toAnsi defaultRenderEnv b
    , testCase "border colour" $
        T.isPrefixOf "\ESC[0;31m╭" (toAnsi defaultRenderEnv (table (borderFg red) [] [["x"]])) @? "border is red"
    , testCase "empty table" $ blockHeight (table mempty [] []) @?= 0
    , testCase "maxWidth crops" $ blockWidth (table (maxWidth 4) ["abcdef"] []) @?= 4
    , testProperty "rectangular and respects width" $ \(Positive extra) (TableData hs rs) ->
        let natural = table mempty (map fromInline hs) (map (map fromInline) rs)
            target = blockWidth natural + extra - 3
            b = table (width target) (map fromInline hs) (map (map fromInline) rs)
         in blockWidth natural > 0 && target >= 2 * length (concat (hs : rs)) + 2 ==>
              rectangular b .&&. blockWidth b === target
    ]

-- | Table contents: a header and ragged rows of short words.
data TableData = TableData [Inline] [[Inline]]
  deriving (Show)

instance Arbitrary TableData where
  arbitrary = do
    let cell = elements ["a", "bb", "ccc dd", "日本", "", "longer words here"]
    hs <- resize 4 (listOf cell)
    rs <- resize 4 (listOf (resize 4 (listOf cell)))
    pure (TableData hs rs)

outlineTests :: TestTree
outlineTests =
  testGroup
    "outline"
    [ testCase "tree" $
        plainLines (tree mempty (Node "." [Node "a" [], Node "b" [Node "c" [], Node "d" []], Node "e" []]))
          @?= [".      ", "├── a  ", "├── b  ", "│   ├── c", "│   └── d", "└── e  "] `withWidth` 9
    , testCase "multi-line items continue the branch" $
        plainLines (tree mempty (Node "r" [Node "a\nb" [], Node "c" []]))
          @?= ["r", "├── a", "│   b", "└── c"] `withWidth` 5
    , testCase "rounded tree" $
        plainLines (forest (enumerator roundedTreeEnumerator) [Node "a" [], Node "b" []]) @?= ["├── a", "╰── b"]
    , testCase "bullet list" $ plainLines (list mempty ["a", "b"]) @?= ["• a", "• b"]
    , testCase "numbers are right-aligned" $
        take 2 (drop 8 (plainLines (list (enumerator arabic) (map (text . T.pack . show) [1 .. 10 :: Int]))))
          @?= [" 9. 9 ", "10. 10"]
    , testCase "alphabet" $
        map (T.take 4) (plainLines (list (enumerator alphabet) (replicate 28 "x"))) !! 27 @?= "AB. "
    , testCase "roman" $
        map (T.takeWhile (/= '.')) (plainLines (list (enumerator roman) (replicate 14 "x"))) !! 13 @?= " XIV"
    , testCase "nested list with an enumerator per depth" $
        plainLines (nestedList (enumerator (byDepth (\d -> if d == 0 then arabic else dash))) [Node "a" [Node "b" []], Node "c" []])
          @?= ["1. a  ", "   - b", "2. c  "]
    , testCase "item and enumerator styles" $
        toAnsi defaultRenderEnv (list (itemStyle bold <> enumeratorStyle (fg red)) ["a"])
          @?= "\ESC[0;31m• \ESC[0;1ma\ESC[0m"
    , testCase "items by position" $
        plainLines (list (itemStyleWith (\p -> if itemIndex p == 1 then paddingLeft 2 else mempty)) ["a", "b"])
          @?= ["• a  ", "•   b"]
    , testCase "root style" $
        toAnsi defaultRenderEnv (tree (rootStyle bold) (Node "r" [])) @?= "\ESC[0;1mr\ESC[0m"
    , testCase "a table inside a tree" $
        plainLines (tree mempty (Node "r" [Node (table (border asciiBorder) [] [["x"]]) []]))
          @?= ["r      ", "└── +-+", "    |x|", "    +-+"]
    ]
  where
    -- Lines are padded to the block's width.
    withWidth ls w = [l <> T.replicate (w - textWidth l) " " | l <- ls]

layerTests :: TestTree
layerTests =
  testGroup
    "layers"
    [ testCase "canvas fits all layers" $
        plainLines (compose [layer mempty "ab", layer (offset 3 1) "cd"]) @?= ["ab   ", "   cd"]
    , testCase "maxWidth crops a layer instead of wrapping it" $ do
        plainLines (compose [layer (maxWidth 5) "hello world"]) @?= ["hello"]
        plainLines (compose [layer (maxWidth 4) (render (border normalBorder) "ab cd")])
          @?= ["┌───", "│ab ", "└───"]
    , testCase "later layers are on top" $
        plainLines (compose [layer mempty "aaaa", layer (offset 1 0) "bb"]) @?= ["abba"]
    , testCase "z-index beats list order" $
        plainLines (compose [layer (zIndex 1) "aaaa", layer (offset 1 0) "bb"]) @?= ["aaaa"]
    , testCase "layers are opaque" $
        plainLines (overlay 0 0 "  " "ab") @?= ["  "]
    , testCase "transparent layers show unstyled spaces through" $
        plainLines (compose [layer mempty "abc", layer transparent " X "]) @?= ["aXc"]
    , testCase "styled spaces are not transparent" $
        toAnsi defaultRenderEnv (compose [layer mempty "ab", layer transparent (render (bg red) " ")])
          @?= "\ESC[0;41m \ESC[0mb"
    , testCase "covering the second half of a wide character" $
        plainLines (overlay 1 0 "x" "日本") @?= [" x本"]
    , testCase "covering the first half of a wide character" $
        plainLines (overlay 2 0 "x" "日本") @?= ["日x "]
    , testCase "a wide character over narrow ones" $
        plainLines (overlay 1 0 "日" "abcd") @?= ["a日d"]
    , testCase "a wide character over two halves" $
        plainLines (overlay 1 0 "本" "日日") @?= [" 本 "]
    , testCase "left edge clipping" $
        plainLines (overlay (-1) 0 "日b" ".....") @?= [" b..."]
    , testCase "top edge clipping" $
        plainLines (overlay 0 (-1) "a\nb" "..") @?= ["b."]
    , testCase "sublayers are relative to their parent" $
        plainLines (compose [layer (offset 2 1 <> sublayers [layer (offset 1 1) "c"]) "p"])
          @?= ["   ", "  p", "   c"] `withWidth'` 4
    , testCase "sublayer z-index is relative" $
        plainLines (compose [layer (zIndex 2) "aa", layer (zIndex 1 <> sublayers [layer (zIndex 2) "b"]) "x"])
          @?= ["ba"]
    , testCase "attributes survive" $
        toAnsi defaultRenderEnv (overlay 1 0 (render bold "x") "ab") @?= "a\ESC[0;1mx\ESC[0m"
    , testCase "zero-width clusters do not take a column" $
        plainLines (overlay 0 0 "a\x200B\&b" "....") @?= ["ab.."]
    , testCase "layerHit and findLayer report bounds" $ do
        let ls = [layer (layerId "bg") (text "....\n...."), layer (offset 1 0 <> sublayers [layer (offset 1 1 <> layerId "btn") "ok"]) "x"]
        fmap rectOf <$> layerHit Nothing 2 1 ls @?= Just ("btn", (2, 1, 2, 1))
        fmap rectOf <$> layerHit Nothing 0 1 ls @?= Just ("bg", (0, 0, 4, 2))
        rectOf <$> findLayer Nothing "btn" ls @?= Just (2, 1, 2, 1)
        findLayer Nothing "nope" ls @?= Nothing
    , testCase "layerAt finds the topmost named layer" $ do
        let ls = [layer (layerId "bg") (text "....\n...."), layer (offset 1 1 <> layerId "btn") "ok", layer (offset 0 0) "x"]
        layerAt Nothing 1 1 ls @?= Just "btn"
        layerAt Nothing 3 1 ls @?= Just "bg"
        layerAt Nothing 0 0 ls @?= Just "bg"
        layerAt Nothing 9 9 ls @?= Nothing
    , testProperty "compose is rectangular and fits its layers" $ \placed ->
        let ls = [layer (offset x y <> zIndex z) (render (specMod s) "日本 text") | (s, (NonNegative x, NonNegative y, z)) <- take 4 placed]
            b = compose ls
         in rectangular b
              .&&. blockWidth b === maximum (0 : [x + blockWidth (render (specMod s) "日本 text") | (s, (NonNegative x, _, _ :: Int)) <- take 4 placed])
    ]
  where
    withWidth' ls w = [l <> T.replicate (w - textWidth l) " " | l <- ls]

-- | Text attributes and cropping are common to every thing.
commonPropsTests :: TestTree
commonPropsTests =
  testGroup
    "common properties"
    [ testCase "the submodules export the types their functions take" $ do
        getAlign (M.align M.center) @?= center
        blockHeight (B.joinHorizontal B.top ["a", "b\nc"]) @?= 2
        getAttrs (M.underlineStyle M.CurlyUnderline <> M.flag M.Bold True :: Mod Style)
          @?= getAttrs (underlineStyle CurlyUnderline <> bold :: Mod Style)
        G.attrBold (getAttrs (bold :: Mod Style)) @?= Just True
    , testCase "positions show as what makes them" $
        map show [center, position 0.25] @?= ["position 0.5", "position 0.25"]
    , testCase "style attributes do not colour the border" $
        toAnsi defaultRenderEnv (render (fg red <> border asciiBorder <> borderTop False <> borderBottom False) "x")
          @?= "|\ESC[0;31mx\ESC[0m|"
    , testCase "style background does not fill margins" $
        toAnsi defaultRenderEnv (render (bg red <> marginLeft 1) "x") @?= " \ESC[0;41mx\ESC[0m"
    , testCase "table attributes are the base for cells and borders" $
        toAnsi defaultRenderEnv (table (bold <> border asciiBorder <> borderTop False <> borderBottom False) [] [["x"]])
          @?= "\ESC[0;1m|x|\ESC[0m"
    , testCase "cell styles and borderFg override the table's" $
        toAnsi defaultRenderEnv (table (fg red <> borderFg blue <> cellStyle (const (fg green)) <> border asciiBorder <> borderTop False <> borderBottom False) [] [["x"]])
          @?= "\ESC[0;34m|\ESC[0;32mx\ESC[0;34m|\ESC[0m"
    , testCase "outline attributes are the base for items and enumerators" $
        toAnsi defaultRenderEnv (list (fg red <> itemStyle bold) ["a"])
          @?= "\ESC[0;31m• \ESC[0;1;31ma\ESC[0m"
    , testCase "layer attributes are the base for its block" $
        toAnsi defaultRenderEnv (compose [layer (bg red) (text "a" `besides` render (bg blue) "b")])
          @?= "\ESC[0;41ma\ESC[0;44mb\ESC[0m"
    , testCase "a layer with a background is not transparent" $
        plainLines (compose [layer mempty "ab", layer (transparent <> bg red) " "]) @?= [" b"]
    , testCase "place attributes colour the whitespace and the block" $
        toAnsi defaultRenderEnv (place (width 3 <> align center <> bg red) (text "x"))
          @?= "\ESC[0;41m x \ESC[0m"
    , testCase "the placed block's own attributes win" $
        toAnsi defaultRenderEnv (place (width 2 <> bg red) (render (bg blue) "x"))
          @?= "\ESC[0;44mx\ESC[0;41m \ESC[0m"
    , testCase "maxWidth crops tables, outlines, places and layers" $ do
        blockWidth (table (maxWidth 3) ["abcdef"] []) @?= 3
        blockWidth (tree (maxWidth 3) (Node "root" [Node "child" []])) @?= 3
        blockWidth (place (width 10 <> maxWidth 4) "x") @?= 4
        blockWidth (compose [layer (maxWidth 2) "abcd"]) @?= 2
    , testCase "maxHeight crops outlines" $
        blockHeight (list (maxHeight 2) ["a", "b", "c"]) @?= 2
    , testCase "the same modifier works on every thing" $ do
        let base :: Mod f
            base = fg red <> bold
        renderPlain (render base "x") @?= "x"
        renderPlain (table base [] [["x"]]) @?= "╭─╮\n│x│\n╰─╯"
        renderPlain (list base ["x"]) @?= "• x"
        renderPlain (place base "x") @?= "x"
    ]
  where
    besides a b = hcat [a, b]

styleParityTests :: TestTree
styleParityTests =
  testGroup
    "style parity"
    [ testCase "per-side border colours" $ do
        let ls = T.lines (toAnsi defaultRenderEnv (render (border asciiBorder <> borderTopFg red <> borderLeftFg blue <> borderRightBg green) "x"))
        ls
          @?= [ "\ESC[0;31m+-+\ESC[0m"
              , "\ESC[0;34m|\ESC[0mx\ESC[0;42m|\ESC[0m"
              , "+-+"
              ]
    , testCase "borderFg sets every side, a side after it overrides one" $
        T.lines (toAnsi defaultRenderEnv (render (border asciiBorder <> borderFg red <> borderBottomFg blue) "x"))
          @?= ["\ESC[0;31m+-+\ESC[0m", "\ESC[0;31m|\ESC[0mx\ESC[0;31m|\ESC[0m", "\ESC[0;34m+-+\ESC[0m"]
    , testCase "gradient offset rotates clockwise" $ do
        let colours m = [c | Just c <- map (attrFgOf . T.unpack) (take 1 (T.lines (toAnsi defaultRenderEnv (render (border asciiBorder <> m) "x"))))]
            attrFgOf l = if take 2 l == "\ESC[" then Just (takeWhile (/= 'm') l) else Nothing
        -- With offset 1 the top-left corner takes the colour the left side
        -- had at the end of the clockwise walk.
        colours (borderFgBlend [red, blue]) @?= ["\ESC[0;38;2;205;0;0"]
        colours (borderFgBlend [red, blue] <> borderFgBlendOffset 1) @?= ["\ESC[0;38;2;0;0;238"]
        colours (borderFgBlend [red, blue] <> borderFgBlendOffset 8) @?= colours (borderFgBlend [red, blue])
    , testCase "colorWhitespace False keeps the background off padding" $
        toAnsi defaultRenderEnv (render (bg red <> paddingLeft 1 <> colorWhitespace False) "x") @?= " \ESC[0;41mx\ESC[0m"
    , testCase "reverse video covers padding" $
        toAnsi defaultRenderEnv (render (reverseVideo <> paddingLeft 1) "x") @?= "\ESC[0;7m x\ESC[0m"
    , testCase "underlineSpaces False" $
        toAnsi defaultRenderEnv (render (underline <> underlineSpaces False) "a b")
          @?= "\ESC[0;4ma\ESC[0m \ESC[0;4mb\ESC[0m"
    , testCase "underlineSpaces True without underline" $
        toAnsi defaultRenderEnv (render (underlineSpaces True) "a b") @?= "a\ESC[0;4m \ESC[0mb"
    , testCase "strikethroughSpaces False" $
        toAnsi defaultRenderEnv (render (strikethrough <> strikethroughSpaces False) "a b")
          @?= "\ESC[0;9ma\ESC[0m \ESC[0;9mb\ESC[0m"
    , testCase "transform" $ do
        plainLines (render (transform T.toUpper) ("raow " <> styled bold "meow")) @?= ["RAOW MEOW"]
        -- Text inside a framed block inherits the transform.
        plainLines (frame (transform T.toUpper) (text "raow")) @?= ["RAOW"]
    , testCase "unset modifiers" $ do
        render (padding 1 1 <> unsetPadding) "x" @?~ render mempty "x"
        render (margin 1 1 <> unsetMargins) "x" @?~ render mempty "x"
        render (border roundedBorder <> borderFg red <> unsetBorder) "x" @?~ render mempty "x"
        render (width 10 <> unsetWidth) "x" @?~ render mempty "x"
        render (maxWidth 1 <> unsetMaxWidth) "xy" @?~ render mempty "xy"
        render (width 5 <> align right <> unsetAlign) "x" @?~ render (width 5) "x"
        render (link "u" <> underline <> unsetLink <> unsetUnderline) "x" @?~ render mempty "x"
        render (tabWidth 1 <> unsetTabWidth) "a\tb" @?~ render mempty "a\tb"
        render (transform T.toUpper <> unsetTransform) "x" @?~ render mempty "x"
    , testCase "frame size" $ do
        let m = padding 1 2 <> border normalBorder <> margin 1 1
        frameSize m @?= (8, 6)
        horizontalPadding m @?= 4
        verticalBorderSize m @?= 2
        horizontalMargins m @?= 2
        frameSize (border normalBorder <> borderLeft False) @?= (1, 2)
    , testCase "getters" $ do
        let m = bold <> fg red <> width 10 <> maxWidth 8 <> border roundedBorder <> borderTop False <> align center
        attrBold (getAttrs m) @?= Just True
        attrFg (getAttrs m) @?= Just red
        getWidth m @?= Just 10
        getMaxWidth m @?= Just 8
        getBorder m @?= Just roundedBorder
        getBorderSides m @?= Sides False True True True
        getAlign m @?= center
        getBorder (mempty :: Mod Style) @?= Nothing
        attrFg (getAttrs (fg blue :: Mod Table)) @?= Just blue
    , testCase "table per-side border colours" $
        T.lines (toAnsi defaultRenderEnv (table (border asciiBorder <> borderLeftFg red <> borderTop False <> borderBottom False) [] [["x"]]))
          @?= ["\ESC[0;31m|\ESC[0mx|"]
    ]

textParityTests :: TestTree
textParityTests =
  testGroup
    "text and colour parity"
    [ testCase "wrap breaks after break characters" $
        plainLinesAt 10 (render (breakAfter "-") "long-winded words") @?= ["long-     ", "winded    ", "words     "]
    , testCase "wrap without break characters" $
        plainLinesAt 10 (render mempty "long-winded words") @?= ["long-winde", "d words   "]
    , testCase "styleRanges" $
        toAnsi defaultRenderEnv (fromInline (styleRanges [(0, 3, bold)] "foobar"))
          @?= "\ESC[0;1mfoo\ESC[0mbar"
    , testCase "styleRanges counts display columns" $
        toAnsi defaultRenderEnv (fromInline (styleRanges [(2, 4, bold)] "日本語"))
          @?= "日\ESC[0;1m本\ESC[0m語"
    , testCase "styleRanges keeps the text's own attributes" $
        toAnsi defaultRenderEnv (fromInline (styleRanges [(1, 2, bold)] (styled (fg red) "abc")))
          @?= "\ESC[0;31ma\ESC[0;1;31mb\ESC[0;31mc\ESC[0m"
    , testCase "styleRanges across lines" $
        toAnsi defaultRenderEnv (fromInline (styleRanges [(1, 3, bold)] "ab\ncd"))
          @?= "a\ESC[0;1mb\ESC[0m\n\ESC[0;1mc\ESC[0md"
    , testCase "styleIndices" $
        toAnsi defaultRenderEnv (fromInline (styleIndices [0, 3] underline faint "foobar"))
          @?= "\ESC[0;4mf\ESC[0;2moo\ESC[0;4mb\ESC[0;2mar\ESC[0m"
    , testCase "ansiWidth and ansiHeight" $ do
        ansiWidth "\ESC[31m日本\ESC[0m\nab" @?= 4
        ansiHeight "\ESC[31m日本\ESC[0m\nab" @?= 2
        ansiWidth (toAnsi defaultRenderEnv (render (border roundedBorder <> padding 0 1) "hi")) @?= 6
    , testCase "blend2D shape" $ do
        let g = blend2D 4 3 45 [red, blue]
        length g @?= 3
        map length g @?= [4, 4, 4]
        blend2D 2 2 0 [green] @?= [[green, green], [green, green]]
        blend2D 2 2 0 [] @?= []
    , testCase "blend2D directions" $ do
        let rowsOf a = blend2D 5 3 a [red, blue]
        all (\r -> all (== take 1 r) [[c] | c <- r]) (rowsOf 90) @? "rows are uniform at 90 degrees"
        rowsOf 270 @?= reverse (rowsOf 90)
        map reverse (rowsOf 180) @?= rowsOf 0
        take 1 (rowsOf 90) /= drop 2 (rowsOf 90) @? "top and bottom differ"
    ]

tableParityTests :: TestTree
tableParityTests =
  testGroup
    "table parity"
    [ testCase "maxHeight shows an overflow row" $
        plainLines (table (maxHeight 7) ["N", "What"] numbered)
          @?= ["╭─┬────╮", "│N│What│", "├─┼────┤", "│0│row │", "│1│row │", "│…│…   │", "╰─┴────╯"]
    , testCase "yOffset scrolls" $ do
        take 2 (drop 3 (plainLines (table (maxHeight 7 <> yOffset 4) ["N", "What"] numbered))) @?= ["│4│row │", "│5│row │"]
        visibleRows Nothing (maxHeight 7 <> yOffset 4) ["N", "What"] numbered @?= Just (4, 5)
    , testCase "scrolled to the end, earlier rows fill the space" $ do
        visibleRows Nothing (maxHeight 7 <> yOffset 9) ["N", "What"] numbered @?= Just (7, 9)
        visibleRows Nothing (maxHeight 7 <> yOffset 99) ["N", "What"] numbered @?= Just (7, 9)
    , testCase "all rows without a maxHeight" $
        visibleRows Nothing mempty ["N"] numbered @?= Just (0, 9)
    , testCase "an empty table shows no rows" $
        visibleRows Nothing mempty ["N"] [] @?= Nothing
    , testCase "row rules count towards the maxHeight" $
        plainLines (table (maxHeight 8 <> rowRules True <> border asciiBorder) [] numbered)
          -- 6 lines for rows: two rows, a rule, a rule and the overflow row;
          -- a third row would need 7.
          @?= ["+-+---+", "|0|row|", "+-+---+", "|1|row|", "+-+---+", "|…|…  |", "+-+---+"]
    , testCase "too small for any row" $ do
        visibleRows Nothing (maxHeight 3) ["N"] numbered @?= Nothing
        blockHeight (table (maxHeight 3) ["N"] numbered) @?= 3
    , testCase "fitContent does not stretch" $
        plainLines (table (width 30 <> fitContent) ["N", "What"] [["0", "row"]])
          @?= ["╭─┬────╮", "│N│What│", "├─┼────┤", "│0│row │", "╰─┴────╯"]
    , testCase "fitContent still shrinks to the width" $
        blockWidth (table (width 8 <> fitContent) ["Number", "What"] [["0", "row"]]) @?= 8
    , testCase "headers are cut, not wrapped" $
        plainLines (table (width 8) ["Header text", "H"] [["a", "b"]])
          @?= ["╭────┬─╮", "│Hea…│H│", "├────┼─┤", "│a   │b│", "╰────┴─╯"]
    , testCase "height is a minimum, filled with empty rows" $
        plainLines (table (height 6) ["N"] [["0"]]) @?= ["╭─╮", "│N│", "├─┤", "│0│", "│ │", "╰─╯"]
    , testCase "height and maxHeight together fix the height" $ do
        blockHeight (table (height 7 <> maxHeight 7) ["N", "What"] numbered) @?= 7
        blockHeight (table (height 7 <> maxHeight 7) ["N", "What"] (take 1 numbered)) @?= 7
    , testProperty "a table never exceeds its maxHeight" $ \(Positive h) (TableData hs rs) ->
        blockHeight (table (maxHeight h) (map fromInline hs) (map (map fromInline) rs)) <= h
    ]
  where
    numbered = [[text (T.pack (show i)), "row"] | i <- [0 .. 9 :: Int]]

outlineParityTests :: TestTree
outlineParityTests =
  testGroup
    "outline parity"
    [ testCase "custom indenter" $
        plainLines (forest (indenter (const ":   ")) [Node "a\nb" [Node "c" []]])
          @?= ["└── a    ", ":   b    ", ":   └── c"]
    , testCase "an indenter wider than the enumerator widens it" $ do
        plainLines (list (indenter (const ">>>>>>")) ["one\ntwo", "three"])
          @?= ["•     one  ", ">>>>>>two  ", "•     three"]
        plainLines (forest (indenter (const "::::::")) [Node "a" [Node "b\nc" []]])
          @?= ["└──   a      ", "::::::└──   b", "::::::::::::c"]
    , testCase "indentation uses the enumerator style by default" $
        toAnsi defaultRenderEnv (forest (enumeratorStyle (fg red)) [Node "a\nb" []])
          @?= "\ESC[0;31m└── \ESC[0ma\n\ESC[0;31m    \ESC[0mb"
    , testCase "enumerators and indenters see the item's position" $ do
        let lastOnly = markers (\p -> if isLast p then "> " else "- ")
        plainLines (list (enumerator lastOnly) ["a", "b", "c"]) @?= ["- a", "- b", "> c"]
        plainLines (forest (indenter (\p -> T.pack (show (itemDepth p)) <> "   ")) [Node "a\nb" [Node "c" []]])
          @?= ["└── a    ", "0   b    ", "0   └── c"]
    , testCase "default indentation is as wide as a padded enumerator" $ do
        plainLines (nestedList (enumeratorStyle (paddingRight 1)) [Node "first\nsecond" [Node "child" []], Node "c" []])
          @?= ["•  first   ", "   second  ", "   •  child", "•  c       "]
        plainLines (list (enumeratorStyle (marginRight 2)) ["a\nb"]) @?= ["•   a", "    b"]
        plainLines (list (enumeratorStyle (paddingLeft 1) <> indenterStyle (paddingRight 2)) ["a\nb"])
          @?= [" • a", "   b"]
    , testCase "indenter style replaces it" $
        toAnsi defaultRenderEnv (forest (enumeratorStyle (fg red) <> indenterStyle (fg blue)) [Node "a\nb" []])
          @?= "\ESC[0;31m└── \ESC[0ma\n\ESC[0;34m    \ESC[0mb"
    , testCase "width pads with the item's background" $
        toAnsi defaultRenderEnv (list (width 5 <> itemStyle (bg red)) ["a"]) @?= "• \ESC[0;41ma  \ESC[0m"
    , testCase "height pads with blank lines" $
        plainLines (list (height 3) ["a"]) @?= ["• a", "   ", "   "]
    , testCase "itemOffset skips items at both ends" $
        plainLines (list (enumerator arabic <> itemOffset 1 1) ["a", "b", "c", "d"]) @?= ["1. b", "2. c"]
    , testCase "itemOffset only affects the top level" $
        plainLines (nestedList (itemOffset 1 0) [Node "a" [], Node "b" [Node "c" [], Node "d" []]])
          @?= ["• b  ", "  • c", "  • d"]
    , testCase "marker alignment keeps the enumerator's background" $
        take 1 (T.lines (toAnsi defaultRenderEnv (list (enumerator arabic <> enumeratorStyle (bg red)) (replicate 10 "x"))))
          @?= ["\ESC[0;41m 1. \ESC[0mx"]
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
    , testCase "noWrap truncates with an ellipsis" $
        plainLines (render (width 5 <> noWrap) "abcdefgh\nabc") @?= ["abcd…", "abc  "]
    , testCase "noWrap does not cut a wide character in half" $
        plainLines (render (width 4 <> noWrap) "日本語") @?= ["日 …"]
    , testCase "negative padding and margins count as 0" $ do
        render (border normalBorder <> padding 0 (-1)) "hello" @?~ render (border normalBorder) "hello"
        render (border normalBorder <> margin (-1) (-2)) "hello" @?~ render (border normalBorder) "hello"
        getPadding (paddingLeft (-3)) @?= Sides 0 0 0 0
    , testCase "negative widths and heights count as 0" $ do
        render (width (-5)) "hello" @?~ render (width 0) "hello"
        render (height (-5)) "hello" @?~ render (height 0) "hello"
        render (maxWidth (-1) <> maxHeight (-1)) "hello" @?~ render (maxWidth 0 <> maxHeight 0) "hello"
        blockWidth (frame (border normalBorder) (render (width (-5)) "a")) @?= 2
        blockWidth (table (width (-4)) ["a", "b"] [["abc", "def"]]) @?= blockWidth (table (width 0) ["a", "b"] [["abc", "def"]])
    , testCase "inherit leaves out padding and margins" $
        plainLines (render (paddingLeft 1 <> inherit (paddingLeft 5 <> marginLeft 3 <> bold)) "x") @?= [" x"]
    , testCase "settings after inherit win" $ do
        let parent = fg "4" <> width 20 <> border doubleBorder <> paddingLeft 5
            s = inherit parent <> fg "1" <> width 10
        (attrFg (getAttrs s), getWidth s, getBorder s == Just doubleBorder, getPadding s)
          @?= (Just "1", Just 10, True, Sides 0 0 0 0)
    , testCase "inherit keeps the margin colour" $
        getMarginBg (inherit (marginBg "4" <> marginLeft 2)) @?= Just "4"
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
        toAnsi (renderEnv ANSI256 True Nothing) (render (fg "#ff0000") "hi")
          @?= "\ESC[0;38;5;196mhi\ESC[0m"
    , testCase "basic colours" $
        toAnsi (renderEnv ANSI16 True Nothing) (render (fg red <> bg brightBlue) "hi")
          @?= "\ESC[0;31;104mhi\ESC[0m"
    , testCase "ascii keeps attributes, drops colours" $
        toAnsi (renderEnv Ascii True Nothing) (render (bold <> fg red) "hi")
          @?= "\ESC[0;1mhi\ESC[0m"
    , testCase "NoTTY is plain" $
        toAnsi (renderEnv NoTTY True Nothing) (render (bold <> fg red <> link "x") "hi") @?= "hi"
    , testCase "nested style keeps outer background" $
        toAnsi defaultRenderEnv (render (bg "1") (styled bold "a" <> "b"))
          @?= "\ESC[0;1;41ma\ESC[0;41mb\ESC[0m"
    , testCase "padding gets only the background" $
        toAnsi defaultRenderEnv (render (underline <> bg "1" <> paddingLeft 1) "a")
          @?= "\ESC[0;41m \ESC[0;4;41ma\ESC[0m"
    , testCase "curly underline downgraded below true colour" $
        toAnsi (renderEnv ANSI256 True Nothing) (render (underlineStyle CurlyUnderline) "a")
          @?= "\ESC[0;4ma\ESC[0m"
    , testCase "hyperlink" $
        toAnsi defaultRenderEnv (render (link "https://x.y") "a")
          @?= "\ESC]8;;https://x.y\ESC\\a\ESC]8;;\ESC\\"
    , testCase "hyperlink controls and non-ASCII are percent-encoded" $
        toAnsi defaultRenderEnv (render (link "http://x\ESC]0;t\a y/\233") "a")
          @?= "\ESC]8;;http://x%1B]0;t%07%20y/%C3%A9\ESC\\a\ESC]8;;\ESC\\"
    , testProperty "escaped URIs are printable ASCII, and escaping is idempotent" $ \s ->
        let u = escapeUri (T.pack s)
         in T.all (\c -> c > ' ' && c < '\DEL') u .&&. escapeUri u === u
    , testCase "every line resets attributes and closes links" $
        toAnsiLines defaultRenderEnv (render (link "u" <> bold) "a\nb")
          @?= [ "\ESC]8;;u\ESC\\\ESC[0;1ma\ESC[0m\ESC]8;;\ESC\\"
              , "\ESC]8;;u\ESC\\\ESC[0;1mb\ESC[0m\ESC]8;;\ESC\\"
              ]
    , testCase "output is UTF-8 whatever the handle's encoding" $ do
        dir <- getTemporaryDirectory
        (path, h) <- openTempFile dir "lipstick.txt"
        hSetEncoding h =<< mkTextEncoding "ASCII"
        let b = render (border roundedBorder) "héllo"
        hPutBlockWith (renderEnv NoTTY True Nothing) h b
        hClose h
        out <- Utf8.readFile path
        removeFile path
        out @?= renderPlain b <> "\n"
    , testCase "light/dark resolved at output" $ do
        let b = render (fg (lightDark "1" "2")) "a"
        toAnsi (renderEnv TrueColor True Nothing) b @?= "\ESC[0;32ma\ESC[0m"
        toAnsi (renderEnv TrueColor False Nothing) b @?= "\ESC[0;31ma\ESC[0m"
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
    , testCase "screen ignores COLORTERM, and screen and tmux show 256 colours" $ do
        profileFromEnv True [("TERM", "screen"), ("COLORTERM", "truecolor")] @?= ANSI256
        profileFromEnv True [("TERM", "screen.xterm-256color"), ("COLORTERM", "24bit")] @?= ANSI256
        profileFromEnv True [("TERM", "tmux"), ("COLORTERM", "truecolor")] @?= TrueColor
        profileFromEnv True [("TERM", "tmux")] @?= ANSI256
    , testCase "kitty" $ profileFromEnv True [("TERM", "xterm-kitty")] @?= TrueColor
    , testCase "ghostty" $ profileFromEnv True [("TERM", "xterm-ghostty")] @?= TrueColor
    , testCase "plain xterm" $ profileFromEnv True [("TERM", "xterm")] @?= ANSI16
    , testCase "COLORFGBG dark" $ backgroundFromEnv [("COLORFGBG", "15;0")] @?= Just True
    , testCase "COLORFGBG light" $ backgroundFromEnv [("COLORFGBG", "0;15")] @?= Just False
    , testCase "COLORFGBG with default" $ backgroundFromEnv [("COLORFGBG", "0;default;7")] @?= Just False
    , testCase "COLORFGBG bright black is dark" $ backgroundFromEnv [("COLORFGBG", "7;8")] @?= Just True
    , testCase "COLORFGBG unparsable" $ backgroundFromEnv [("COLORFGBG", "default;default")] @?= Nothing
    , testCase "COLORFGBG unset" $ backgroundFromEnv [] @?= Nothing
    , testCase "COLUMNS" $ do
        widthFromEnv [("COLUMNS", "120")] @?= Just 120
        widthFromEnv [("COLUMNS", "0")] @?= Nothing
        widthFromEnv [("COLUMNS", "wide")] @?= Nothing
        widthFromEnv [("COLUMNS", "")] @?= Nothing
        widthFromEnv [] @?= Nothing
    , testCase "a file has no width, unless COLUMNS sets one" $ do
        saved <- lookupEnv "COLUMNS"
        dir <- getTemporaryDirectory
        (path, h) <- openTempFile dir "lipstick-width.txt"
        unsetEnv "COLUMNS"
        none <- detectWidth h
        setEnv "COLUMNS" "33"
        set <- detectWidth h
        maybe (unsetEnv "COLUMNS") (setEnv "COLUMNS") saved
        hClose h
        removeFile path
        (none, set) @?= (Nothing, Just 33)
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

-- | Inline text made of segments with random attributes.
newtype StyledText = StyledText Inline
  deriving (Show)

instance Arbitrary StyledText where
  arbitrary = do
    segs <- listOf $ do
      Words t <- arbitrary
      m <- mconcat <$> sublistOf attrMods
      pure (styled m t)
    pure (StyledText (mconcat segs))
    where
      attrMods =
        [ bold, italic, faint, strikethrough, reverseVideo, underline
        , underlineStyle CurlyUnderline, underlineColor "#00ff00"
        , fg "#ff8000", fg "200", fg red, bg "#102030", bg brightCyan
        , link "https://example.com"
        ]

specMod :: StyleSpec -> Mod Style
specMod (StyleSpec w (pv, ph) (mv, mh) b) =
  maybe mempty width w
    <> padding pv ph
    <> margin mv mh
    <> (if b then border roundedBorder else mempty)

-- | Every line of the plain output is exactly as wide as the block says.
rectangular :: Block -> Property
rectangular b =
  -- Split on newlines exactly: T.lines would drop a trailing empty line,
  -- and an empty block still renders as one (empty) line of text.
  let ls = T.splitOn "\n" (renderPlain b)
   in counterexample (show ls) $
        conjoin
          [ length ls === max 1 (blockHeight b)
          , conjoin [textWidth l === blockWidth b | l <- ls]
          ]

-- | Text made of pieces that combine into clusters: emoji with variation
-- selectors and ZWJs, flags, combining marks, Hangul jamo.
trickyText :: Gen Text
trickyText =
  T.concat
    <$> listOf
      ( elements
          [ "a", " ", "e", "\x301", "\x2764", "\xFE0F", "\xFE0E", "\x200D", "👨", "👩"
          , "🇮", "🇹", "日", "\x1100", "\x1161", "#", "\x20E3"
          ]
      )

-- | A text cut into pieces at random characters, each with random styles.
styledPieces :: Text -> Gen Inline
styledPieces t = do
  cuts <- sublistOf [1 .. T.length t - 1]
  let bounds = zip (0 : cuts) (cuts ++ [T.length t])
      pieces = [T.take (e - s) (T.drop s t) | (s, e) <- bounds]
  mconcat <$> mapM (\p -> elements [plain p, styled bold p, styled (fg red) p]) pieces

stripAnsi :: Text -> Text
stripAnsi t = case T.breakOn "\ESC" t of
  (before, rest)
    | T.null rest -> before
    | "\ESC]" `T.isPrefixOf` rest ->
        before <> stripAnsi (T.drop 2 (snd (T.breakOn "\ESC\\" (T.drop 2 rest))))
    | otherwise -> before <> stripAnsi (T.drop 1 (T.dropWhile (/= 'm') rest))

-- | A description of a block, to generate block trees.
data Shape
  = ShapeText Text
  | ShapeRender StyleSpec Bool Bool Text
  -- ^ style, grows, stretches
  | ShapeFrame StyleSpec Bool Bool Shape
  | ShapeRow Int [Shape]
  | ShapeColumn Int [Shape]
  | ShapePlace (Maybe Int) Bool Shape
  -- ^ width, stretches
  | ShapeTable (Maybe Int) Bool [[Text]]
  -- ^ width, grows
  | ShapeList Bool [Shape]
  | ShapeTree Shape [Shape]
  | ShapeLayers [((Int, Int), Shape)]
  | ShapeEmpty
  deriving (Show)

instance Arbitrary Shape where
  arbitrary = sized go
    where
      go n
        | n <= 1 = leaf
        | otherwise =
            frequency
              [ (3, leaf)
              , (2, ShapeFrame <$> arbitrary <*> arbitrary <*> arbitrary <*> go (n `div` 2))
              , (2, ShapeRow <$> chooseInt (0, 2) <*> children n)
              , (2, ShapeColumn <$> chooseInt (0, 2) <*> children n)
              , (1, ShapePlace <$> oneof [pure Nothing, Just <$> chooseInt (0, 30)] <*> arbitrary <*> go (n `div` 2))
              , (1, ShapeList <$> arbitrary <*> children n)
              , (1, ShapeTree <$> go (n `div` 2) <*> children n)
              , (1, ShapeLayers <$> (zip <$> vectorOf 3 ((,) <$> chooseInt (-2, 8) <*> chooseInt (0, 3)) <*> children n))
              ]
      children n = do
        k <- chooseInt (0, 3)
        vectorOf k (go (n `div` (k + 1)))
      leaf =
        frequency
          [ (2, (\(Words t) -> ShapeText t) <$> arbitrary)
          , (3, (\spec g st (Words t) -> ShapeRender spec g st t) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
          , (1, ShapeTable <$> oneof [pure Nothing, Just <$> chooseInt (0, 30)] <*> arbitrary <*> vectorOf 2 (vectorOf 2 ((\(Words t) -> t) <$> arbitrary)))
          , (1, pure ShapeEmpty)
          ]

toBlock :: Shape -> Block
toBlock = \case
  ShapeText t -> text t
  ShapeRender spec g st t -> render (specMod spec <> flags g st) (plain t)
  ShapeFrame spec g st sh -> frame (specMod spec <> flags g st) (toBlock sh)
  ShapeRow p shs -> joinHorizontal ([top, center, bottom] !! p) (map toBlock shs)
  ShapeColumn p shs -> joinVertical ([left, center, right] !! p) (map toBlock shs)
  ShapePlace w st sh -> place (maybe mempty width w <> (if st then stretchHeight else mempty) <> align center) (toBlock sh)
  ShapeTable w g rows -> table (maybe mempty width w <> (if g then fillWidth else mempty)) ["a", "b"] (map (map text) rows)
  ShapeList g shs -> list (if g then fillWidth else mempty) (map toBlock shs)
  ShapeTree root shs -> tree mempty (Node (toBlock root) [Node (toBlock sh) [] | sh <- shs])
  ShapeLayers ls -> compose [layer (offset x y) (toBlock sh) | ((x, y), sh) <- ls]
  ShapeEmpty -> emptyBlock
  where
    flags g st = (if g then fillWidth else mempty) <> (if st then stretchHeight else mempty)

-- | Every line is as wide as the grid, and so is its text on a terminal.
gridIsRectangular :: Grid -> Property
gridIsRectangular g =
  counterexample (show g) $
    length (gridLines g) === gridHeight g
      .&&. conjoin [lineWidth l === gridWidth g | l <- gridLines g]
      .&&. conjoin [textWidth (T.concat (map spanText (lineSpans l))) === gridWidth g | l <- gridLines g]

deferredLayoutTests :: TestTree
deferredLayoutTests =
  testGroup
    "deferred layout"
    [ testProperty "a block is exactly as wide as the width it is given" $ \sh ->
        let b = toBlock sh
         in forAll (chooseInt (0, blockWidth b + 10)) $ \w ->
              let g = fst (blockLayout (measure b) (Assign w 0))
               in gridWidth g === w .&&. gridIsRectangular g
    , testProperty "the natural height is the height of the grid" $ \sh ->
        let b = toBlock sh
         in forAll (chooseInt (0, blockWidth b + 10)) $ \w ->
              let (g, n) = blockLayout (measure b) (Assign w 0)
               in gridHeight g === n
    , testProperty "the natural height does not depend on the height to stretch to" $ \sh ->
        let b = toBlock sh
         in forAll (chooseInt (0, blockWidth b + 10)) $ \w ->
              snd (blockLayout (measure b) (Assign w (error "the target height was forced")))
                === snd (blockLayout (measure b) (Assign w 0))
    , testProperty "only blocks that stretch grow to the height they are asked for" $ \sh (NonNegative extra) ->
        let b = toBlock sh
            w = blockWidth b
            n = blockHeight b
            g = fst (blockLayout (measure b) (Assign w (n + extra)))
         in gridHeight g === (if reqStretch (blockRequest (measure b)) then n + extra else n)
    , testProperty "the smallest width is at most the natural width" $ \sh ->
        let b = toBlock sh in blockMinWidth b <= blockWidth b
    , testProperty "with no limit, a block is laid out at its natural width" $ \sh ->
        let b = toBlock sh
            g = layout Nothing b
         in gridWidth g === blockWidth b .&&. gridHeight g === blockHeight b
    , testCase "text wraps to the width" $
        plainLinesAt 10 (text "the quick brown fox") @?= ["the quick ", "brown fox "]
    , testCase "indentation is dropped when the first word does not fit after it" $ do
        plainLinesAt 10 (text "    indented word") @?= ["indented  ", "word      "]
        plainLinesAt 3 (text "  abc") @?= ["abc"]
        plainLinesAt 3 (text "\tfoo") @?= ["foo"]
        plainLinesAt 10 (text "  indented word") @?= ["  indented", "word      "]
        plainLinesAt 4 (text "  abcdef") @?= ["  ab", "cdef"]
    , testProperty "text laid out at its smallest width breaks no word" $
        forAll (chooseInt (0, 6)) $ \n ->
          forAll (listOf1 (T.pack <$> listOf1 (elements "ab"))) $ \ws ->
            let t = T.replicate n " " <> T.unwords ws
                b = text t
             in T.words (T.unlines (plainLinesAt (blockMinWidth b) b)) === ws
    , testCase "text with no room is cut, not wrapped" $ do
        gridHeight (layout (Just 0) (text "hello world\nagain")) @?= 2
        plainLinesAt 4 (hcat [render (border normalBorder) "hello", render (border normalBorder) "world"])
          @?= ["┌┐┌┐", "││││", "└┘└┘"]
    , testCase "a box wraps its text inside the border" $
        plainLinesAt 12 (render (border normalBorder <> padding 0 1) "the quick brown fox")
          @?= ["┌──────────┐", "│ the      │", "│ quick    │", "│ brown    │", "│ fox      │", "└──────────┘"]
    , testCase "a frame lays its block out in the width left inside" $
        plainLinesAt 7 (frame (border normalBorder) (text "aaa bbb ccc"))
          @?= ["┌─────┐", "│aaa  │", "│bbb  │", "│ccc  │", "└─────┘"]
    , testCase "a block narrower than the space keeps its natural width" $
        plainLinesAt 40 (render (border normalBorder) "hi") @?= ["┌──┐", "│hi│", "└──┘"]
    , testCase "tables and outlines stretch to the height of a row" $ do
        let row b = map (T.drop 1) (plainLines (hcat [render (height 6) "x", b]))
        row (table stretchHeight ["a"] [["1"]]) @?= ["╭─╮", "│a│", "├─┤", "│1│", "│ │", "╰─╯"]
        row (list (stretchHeight <> itemStyle (bg red)) ["q"]) @?= ["• q", "   ", "   ", "   ", "   ", "   "]
        blockHeight (table stretchHeight ["a"] [["1"]]) @?= 5
    , testCase "a table under an inherited noWrap shrinks as with its own" $ do
        let t m = table m ["name"] [["a long cell", "x"]]
        blockMinWidth (frame noWrap (t mempty)) @?= blockMinWidth (t noWrap)
        plainLinesAt 30 (hcat [frame noWrap (t mempty), render (border normalBorder) "aaa bbb ccc"])
          @?= plainLinesAt 30 (hcat [t noWrap, render (border normalBorder) "aaa bbb ccc"])
    , testCase "borderFgBlend colours a table's border as a style's" $ do
        let colours b = [c | l <- gridLines (layout Nothing b), sp <- lineSpans l, Just c <- [attrFg (spanAttrs sp)]]
            tableColours = colours (table (borderFgBlend [red, blue] <> borderFgBlendOffset 1) ["a"] [["1"]])
        tableColours @?= colours (render (border roundedBorder <> borderFgBlend [red, blue] <> borderFgBlendOffset 1) "a\n─\n1")
        length tableColours @?= 12
    , testCase "a table does not stretch past maxHeight, and keeps its bottom border" $ do
        last (plainLines (table (maxHeight 9 <> height 12) ["n"] [["1"], ["2"], ["3"]])) @?= "╰─╯"
        plainLines (hcat [render (height 8) "x", table (stretchHeight <> maxHeight 6) ["a"] [["1"]]]) !! 5 @?= " ╰─╯"
    , testCase "fillWidth takes the spare columns of a row" $
        plainLinesAt 12 (hcat [render (width 4) "ab", render (fillWidth <> bg red) "cd", "!"])
          @?= ["ab  cd     !"]
    , testCase "spare columns are shared by weight" $
        map T.length (plainLinesAt 12 (hcat [render (grow 1) "a", render (grow 2) "b"])) @?= [12]
    , testCase "fillWidth fills the output's width" $
        plainLinesAt 6 (render (fillWidth <> align right) "ab") @?= ["    ab"]
    , testCase "what a block cannot use goes to the others" $
        plainLinesAt 12 (hcat [render (fillWidth <> maxWidth 4 <> bg red) "a", render fillWidth "b"])
          @?= ["a   b       "]
    , testCase "fillWidth with maxWidth fills up to the crop" $
        plainLinesAt 20 (render (fillWidth <> maxWidth 5 <> align right) "ab") @?= ["   ab"]
    , testCase "maxWidth crops instead of wrapping" $
        plainLinesAt 20 (render (maxWidth 3) "abc def") @?= ["abc"]
    , testCase "the widest block shrinks first" $
        plainLinesAt 10 (hcat [text "aaa bbb ccc", text "|", text "dd"])
          @?= ["aaa bbb|dd", "ccc       "]
    , testCase "stretchHeight grows a box to the tallest in its row" $
        plainLines (hcat [render (border normalBorder <> stretchHeight) "a", text "1\n2\n3\n4"])
          @?= ["┌─┐1", "│a│2", "│ │3", "└─┘4"]
    , testCase "stretched frames pass the height on to stretching content" $
        plainLines (hcat [frame (border normalBorder <> stretchHeight) (render (bg red <> stretchHeight) "a"), text "1\n2\n3\n4"])
          @?= ["┌─┐1", "│a│2", "│ │3", "└─┘4"]
    , testCase "place without a width fills the space" $
        plainLinesAt 5 (place (align center) "x") @?= ["  x  "]
    , testCase "place keeps its natural width when there is no limit" $
        plainLines (place (align center) "x") @?= ["x"]
    , testCase "a place's width is its width, as a style's" $ do
        plainLines (place (width 5) "hello world") @?= ["hello", "world"]
        blockWidth (place (width 5) "hi") @?= 5
        plainLinesAt 3 (place (width 5) "hi") @?= ["hi "]
    , testCase "an outline's width is its width, as a style's" $ do
        plainLines (list (width 8) ["one two three"]) @?= ["• one   ", "  two   ", "  three "]
        blockWidth (list (width 8) ["a"]) @?= 8
        gridWidth (layout (Just 4) (list (width 8) ["a b"])) @?= 4
    , testCase "a table fits the width it is given, wrapping its cells" $
        -- Even the longest words (8 and 9) do not fit side by side in 14
        -- columns, so the columns shrink widest first below them.
        plainLinesAt 17 (table mempty ["Name", "Notes"] [["lipstick", "styled text for terminals"]])
          @?= [ "╭───────┬───────╮"
              , "│Name   │Notes  │"
              , "├───────┼───────┤"
              , "│lipstic│styled │"
              , "│k      │text   │"
              , "│       │for    │"
              , "│       │termina│"
              , "│       │ls     │"
              , "╰───────┴───────╯"
              ]
    , testCase "columns do not break a word while another column can wrap" $
        plainLinesAt 15 (table mempty ["Name", "Notes"] [["lipstick", "ab cd ef gh ij kl"]])
          @?= [ "╭────────┬────╮"
              , "│Name    │Not…│"
              , "├────────┼────┤"
              , "│lipstick│ab  │"
              , "│        │cd  │"
              , "│        │ef  │"
              , "│        │gh  │"
              , "│        │ij  │"
              , "│        │kl  │"
              , "╰────────┴────╯"
              ]
    , testCase "a long header is cut rather than holding its column wide" $
        plainLinesAt 15 (table mempty ["A very long header", "B"] [["x", "y"]])
          @?= ["╭───────────┬─╮", "│A very lon…│B│", "├───────────┼─┤", "│x          │y│", "╰───────────┴─╯"]
    , testCase "a table with fillWidth grows its columns to fill the width" $
        plainLinesAt 13 (table fillWidth ["a", "b"] [["1", "2"]])
          @?= ["╭─────┬─────╮", "│a    │b    │", "├─────┼─────┤", "│1    │2    │", "╰─────┴─────╯"]
    , testCase "which rows a table shows depends on its width" $ do
        let rows = [[text (T.pack (show i)), "a few words"] | i <- [0 .. 9 :: Int]]
        visibleRows Nothing (maxHeight 8) ["N", "What"] rows @?= Just (0, 2)
        visibleRows (Just 9) (maxHeight 8) ["N", "What"] rows @?= Just (0, 0)
    , testCase "list items wrap after their enumerator" $
        plainLinesAt 10 (list mempty ["one two three", "four"])
          @?= ["• one two ", "  three   ", "• four    "]
    , testCase "tree items wrap inside their branch" $
        plainLinesAt 12 (tree mempty (Node "root" [Node "alpha beta gamma" [], Node "delta" []]))
          @?= ["root        ", "├── alpha   ", "│   beta    ", "│   gamma   ", "└── delta   "]
    , testCase "an outline with fillWidth pads to the width" $
        plainLinesAt 8 (list (fillWidth <> bg red) ["a"]) @?= ["• a     "]
    , testCase "a layer reflows in a narrow canvas" $ do
        let ls = [layer mempty (text "aaaa bbbb"), layer (offset 2 0 <> layerId "top") "x"]
        plainLinesAt 6 (compose ls) @?= ["aaxa  ", "bbbb  "]
        rectOf <$> findLayer (Just 6) "top" ls @?= Just (2, 0, 1, 1)
    , testCase "a column aligns blocks that do not grow" $
        plainLinesAt 8 (joinVertical right [render fillWidth "abc", "d"])
          @?= ["abc     ", "       d"]
    ]

inheritanceTests :: TestTree
inheritanceTests =
  testGroup
    "inherited text settings"
    [ testCase "a frame aligns the lines of text inside it" $
        plainLines (frame (align center <> width 9) (text "aa bbbbbbb c"))
          @?= ["   aa    ", "bbbbbbb c"]
    , testCase "render is frame of inline text" $
        render (align right <> width 8 <> border normalBorder) "ab cd ef"
          @?~ frame (align right <> width 8 <> border normalBorder) (fromInline "ab cd ef")
    , testCase "a frame aligns a row as a whole" $
        plainLines (frame (align center <> width 8) (hcat ["ab", "cd"])) @?= ["  abcd  "]
    , testCase "noWrap is inherited" $
        plainLines (frame (noWrap <> width 6) (frame mempty (text "abcdefgh"))) @?= ["abcde…"]
    , testCase "the nearer setting wins" $
        plainLines (frame (align right <> width 6) (frame (align left <> width 6) (text "ab"))) @?= ["ab    "]
    , testCase "settings not set are inherited through nested frames" $
        plainLines (frame (align right <> width 6) (frame (border normalBorder <> width 6) (text "ab")))
          @?= ["┌────┐", "│  ab│", "└────┘"]
    , testCase "alignment space gets only the background" $
        toAnsi defaultRenderEnv (frame (underline <> bg red <> align right <> width 4) (text "ab"))
          @?= "\ESC[0;41m  \ESC[0;4;41mab\ESC[0m"
    , testCase "underlineSpaces leaves alignment space alone" $
        toAnsi defaultRenderEnv (frame (underlineSpaces True <> align right <> width 4) (text "a b"))
          @?= " a\ESC[0;4m \ESC[0mb"
    , testCase "table cells can hold any block" $
        plainLines (table (border asciiBorder) ["Name", "Items"] [["fruit", list mempty ["apple", "pear"]]])
          @?= ["+-----+-------+", "|Name |Items  |", "+-----+-------+", "|fruit|• apple|", "|     |• pear |", "+-----+-------+"]
    , testCase "a centred cell centres each of its lines" $
        plainLinesAt 12 (table (border asciiBorder <> cellStyle (const (align center))) ["x"] [["aa bbbbbbbb"]])
          @?= ["+----------+", "|    x     |", "+----------+", "|    aa    |", "| bbbbbbbb |", "+----------+"]
    ]

-- | (min, max, weight) for a child of a row.
newtype Child = Child (Int, Int, Int)
  deriving (Show)

instance Arbitrary Child where
  arbitrary = do
    lo <- chooseInt (0, 12)
    hi <- (lo +) <$> chooseInt (0, 20)
    k <- frequency [(2, pure 0), (1, chooseInt (1, 3))]
    pure (Child (lo, hi, k))

allocateTests :: TestTree
allocateTests =
  testGroup
    "allocate"
    [ testProperty "matches the table's widest-first fitting" $ \(NonNegative target) ns ->
        let natural = map getNonNegative ns
         in allocate target [(min 1 n, n, 1) | n <- natural] === widestFirst target natural
    , testProperty "spare space goes by weight" $ \cs ->
        let children = [c | Child c <- cs]
            his = [hi | (_, hi, _) <- children]
         in forAll (chooseInt (sum his, sum his + 50)) $ \w ->
              let ws = allocate w children
               in conjoin [a >= hi | (a, hi) <- zip ws his]
                    .&&. conjoin [a === hi | (a, (_, hi, 0)) <- zip ws children]
                    .&&. (if any (\(_, _, k) -> k > 0) children then sum ws === w else ws === his)
    , testProperty "shrinking stays within the bounds and fills the space" $ \cs ->
        let children = [c | Child c <- cs]
            los = [lo | (lo, _, _) <- children]
            his = [hi | (_, hi, _) <- children]
         in forAll (chooseInt (sum los, sum his)) $ \w ->
              let ws = allocate w children
               in sum ws === w .&&. conjoin [lo <= a .&&. a <= hi | (a, lo, hi) <- zip3 ws los his]
    , testProperty "below the smallest widths, nothing goes under one column" $ \cs ->
        let children = [c | Child c <- cs]
            los = [lo | (lo, _, _) <- children]
         in forAll (chooseInt (0, max 0 (sum los - 1))) $ \w ->
              let ws = allocate w children
               in sum ws === max w (sum (map (min 1) los))
                    .&&. conjoin [min 1 lo <= a .&&. a <= lo | (a, lo) <- zip ws los]
    , testProperty "growing children stay within their limits" $ \cs ->
        forAll (vectorOf (length cs) (oneof [pure Nothing, Just <$> chooseInt (0, 40)])) $ \limits ->
          let children = [(lo, hi, k, l) | (Child (lo, hi, k), l) <- zip cs limits]
              his = [hi | (_, hi, _, _) <- children]
              room = sum [if k > 0 then maybe 1000 (\lim -> max 0 (lim - hi)) l else 0 | (_, hi, k, l) <- children]
           in forAll (chooseInt (sum his, sum his + 50)) $ \w ->
                let ws = allocateWithin w children
                 in conjoin [maybe True (\lim -> a <= max hi lim) l | (a, (_, hi, _, l)) <- zip ws children]
                      .&&. sum ws === min w (sum his + room)
    , testCase "shrinks the widest first" $ allocate 10 [(1, 8, 0), (1, 3, 0), (1, 6, 0)] @?= [3, 3, 4]
    , testCase "spare space by weight, left first" $ allocate 10 [(1, 2, 1), (1, 2, 0), (1, 2, 2)] @?= [4, 2, 4]
    ]
  where
    -- How tables fitted their columns before 'allocate': shrink the widest
    -- column (the leftmost of equals) one step at a time, down to one
    -- column, or share out spare space evenly from the left.
    widestFirst target ws
      | tot > target && any (> 1) ws = widestFirst target (decrementWidest ws)
      | tot < target && not (null ws) =
          let (q, r) = (target - tot) `divMod` length ws
           in [x + q + (if i < r then 1 else 0) | (i, x) <- zip [0 :: Int ..] ws]
      | otherwise = ws
      where
        tot = sum ws
        decrementWidest xs = case break (== maximum xs) xs of
          (a, x : b) -> a ++ (x - 1) : b
          (a, []) -> a

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
    , testProperty "styling pieces of a text does not change its width" $
        forAll trickyText $ \t -> forAll (styledPieces t) $ \inl ->
          blockWidth (fromInline inl) === textWidth t
            .&&. rectangular (render (border roundedBorder <> padding 0 1) inl)
    , testProperty "maxWidth bounds the width" $ \spec (Words t) (Positive n) ->
        let b = render (specMod spec <> maxWidth n) (plain t)
         in rectangular b .&&. blockWidth b <= n
    ]
