-- | Style definitions for nice terminal layouts: colours, borders,
-- padding, alignment, tables, lists, trees and layered compositions,
-- printed with ANSI escape sequences.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Lipstick
-- >
-- > card :: Mod Style
-- > card = bold <> fg "#FAFAFA" <> bg "#7D56F4" <> padding 1 4 <> width 22
-- >
-- > main :: IO ()
-- > main = putBlock (render card "Hello, kitty")
--
-- == Concepts
--
-- [Modifiers] A @'Mod' f@ is a change to a thing @f@: a 'Style', a
-- 'Table', an 'Outline', a 'Place' or a 'Layer'. Modifiers form a monoid:
-- '<>' applies the left one first, so later settings win, and 'mempty'
-- changes nothing. Text attributes ('fg', 'bold', 'link', …) and the
-- 'maxWidth' and 'maxHeight' crop work on every thing. The other
-- modifiers work on the things that have the part they set, which the
-- classes 'HasBox', 'HasBorder', 'HasSize', 'HasAlign' and 'HasWrap' say;
-- a modifier on a thing without that part is a type error. A modifier
-- meant for several kinds of thing needs a signature, as in
-- @base :: Mod f@ or @boxed :: (HasBorder f, HasSize f) => Mod f@:
-- without one, the monomorphism restriction fixes its type.
--
-- [Text] 'Inline' is text with styles: string literals, 'styled' pieces
-- joined with '<>', and text with escape sequences read by 'fromAnsi'.
-- Styles nest, and the enclosing style shows through wherever an inner
-- one does not set an attribute.
--
-- [Blocks] A 'Block' is a rectangle of styled text: 'render' and
-- 'frame' draw text and blocks in a box, and 'table', 'list', 'tree',
-- 'place' and 'compose' make the other kinds. Blocks are joined with
-- 'hcat', 'vcat', 'joinHorizontal' and 'joinVertical'. A block is not laid
-- out until it is printed: it knows the narrowest width it can take
-- without breaking words and the width it would like, and is laid out in
-- the width it is given, so text wraps, table columns shrink, and blocks
-- with 'fillWidth' take the spare columns of their row. The text settings
-- of a style ('align', 'noWrap', 'breakAfter', 'tabWidth', 'transform')
-- also apply to the text inside the blocks it frames, unless something
-- nearer sets them again.
--
-- [Sizes] They mean the same for every thing that has them. @'width' n@
-- is the width when there is room for it: the content is laid out in @n@
-- columns, and with less room the thing shrinks. @'height' n@ is the
-- least number of lines. 'maxWidth' and 'maxHeight' are the most, and
-- crop what does not fit; a table leaves out rows instead. For a style,
-- 'width' and 'height' include padding and border but not margins.
--
-- [Output] Nothing becomes escape sequences until a block is printed.
-- A 'RenderEnv' says what the output can show (the colour profile,
-- whether the background is dark) and its width; 'putBlock' detects it
-- for the terminal, 'toAnsi' gives the text for a given one, and
-- 'renderPlain' gives text without escape sequences. Adaptive colours
-- ('lightDark', 'complete') are resolved then, and colours are reduced
-- to what the output supports. 'layout' gives the laid-out 'Grid', to
-- show blocks in other ways ("Lipstick.Grid", the lipstick-brick
-- package).
--
-- == Names
--
-- The names are short and some are common: 'bold', 'fg', 'border',
-- 'center', 'text', 'render', 'width' and the colour names are also
-- exported by brick, vty, colour or prettyprinter. Programs that use
-- those as well can import lipstick qualified,
--
-- > import Lipstick qualified as L
--
-- or import only the names they need.
--
-- == Modules
--
-- This module exports what most programs need. The API is also split
-- into modules by feature, each with more examples, and "Lipstick.Grid",
-- "Lipstick.Width" and "Lipstick.Terminal" have a few functions more:
--
-- * "Lipstick.Mod": modifiers, which configure everything below
-- * "Lipstick.Style": text in a box with padding, border and margins
-- * "Lipstick.Block": blocks of text, and joining them
-- * "Lipstick.Place": a block in a larger area
-- * "Lipstick.Border": border character sets
-- * "Lipstick.Table": tables
-- * "Lipstick.Outline": lists and trees
-- * "Lipstick.Layer": blocks stacked at any position
-- * "Lipstick.Color": colours and gradients
-- * "Lipstick.Terminal": printing, and detecting what the terminal supports
-- * "Lipstick.Ansi": blocks as text with escape sequences, and back
-- * "Lipstick.Width": the display width of text
-- * "Lipstick.Grid": laid-out blocks, for other outputs
--
-- The examples print with 'putBlock' and need @OverloadedStrings@. The
-- output is shown without colours and other attributes.
module Lipstick
  ( -- * Modifiers
    -- | A @'Mod' f@ configures a thing @f@. Modifiers combine with '<>',
    -- and later settings win. See "Lipstick.Mod".
    --
    -- >>> let card = bold <> fg "#FAFAFA" <> bg "#7D56F4" <> border roundedBorder <> padding 0 1
    -- >>> putBlock (render card "a card")
    -- ╭────────╮
    -- │ a card │
    -- ╰────────╯
    -- >>> putBlock (render (card <> border doubleBorder) "a card with a double border")
    -- ╔═════════════════════════════╗
    -- ║ a card with a double border ║
    -- ╚═════════════════════════════╝
    Mod
  , HasBox
  , HasBorder
  , HasSize
  , HasAlign
  , HasWrap

    -- * Styles
    -- | A style draws text, or a block, in a box. See "Lipstick.Style".
    --
    -- >>> putBlock (render (width 20 <> border normalBorder <> align center) "Text wraps to fit the width")
    -- ┌──────────────────┐
    -- │Text wraps to fit │
    -- │    the width     │
    -- └──────────────────┘
    -- >>> putBlock (frame (border roundedBorder) (render (border normalBorder) "nested"))
    -- ╭────────╮
    -- │┌──────┐│
    -- ││nested││
    -- │└──────┘│
    -- ╰────────╯
  , Style
  , render
  , frame
  , inherit
  , tabWidth
  , unsetTabWidth
  , inline
  , unsetInline
  , transform
  , unsetTransform
  , breakAfter
  , colorWhitespace
  , underlineSpaces
  , strikethroughSpaces

    -- * Blocks
    -- | A block is a rectangle of styled text, laid out in the width it is
    -- given. See "Lipstick.Block".
    --
    -- >>> let para = text "A block takes the width it is given."
    -- >>> putBlock para
    -- A block takes the width it is given.
    -- >>> putBlock (fromGrid (layout (Just 16) para))
    -- A block takes
    -- the width it is
    -- given.
    -- >>> putBlock (fromInline (styled bold "Error: " <> "file not found"))
    -- Error: file not found
  , Block
  , blockWidth
  , blockHeight
  , blockMinWidth
  , blockGrows
  , emptyBlock
  , text
  , fromInline
  , layout
  , Grid
  , gridWidth
  , gridHeight
  , fromGrid
  , Inline
  , plain
  , styled
  , fromAnsi
  , escapeUri
  , styleRanges
  , styleIndices
  , Attrs (attrFg, attrBg, attrUnderlineColor, attrBold, attrFaint, attrItalic, attrBlink, attrReverse, attrStrikethrough, attrUnderline, attrLink)

    -- * Layout
    -- | Blocks side by side, stacked, or placed in a larger area. See
    -- "Lipstick.Block" and "Lipstick.Place".
    --
    -- >>> let tall = render (border normalBorder) "one\ntwo\nthree"
    -- >>> let short = render (border normalBorder) "four"
    -- >>> putBlock (joinHorizontal center [tall, short])
    -- ┌─────┐
    -- │one  │┌────┐
    -- │two  ││four│
    -- │three│└────┘
    -- └─────┘
    -- >>> putBlock (place (width 12 <> height 3 <> align center <> valign center <> whitespaceChars "·") "centred")
    -- ············
    -- ··centred···
    -- ············
  , joinHorizontal
  , joinVertical
  , hcat
  , vcat
  , Place
  , place
  , whitespaceChars
  , Position
  , position
  , left
  , center
  , right
  , top
  , bottom

    -- * Text attributes
    -- | Text attributes are modifiers of every thing.
    --
    -- >>> toAnsi defaultRenderEnv (render (bold <> underline <> fg red) "hi")
    -- "\ESC[0;1;4;31mhi\ESC[0m"
    -- >>> toAnsi defaultRenderEnv (render (link "https://example.com") "a link")
    -- "\ESC]8;;https://example.com\ESC\\a link\ESC]8;;\ESC\\"
  , bold
  , faint
  , italic
  , blink
  , reverseVideo
  , strikethrough
  , Flag (..)
  , flag
  , unsetFlag
  , underline
  , Underline (..)
  , underlineStyle
  , underlineColor
  , fg
  , bg
  , unsetFg
  , unsetBg
  , unsetUnderline
  , unsetUnderlineColor
  , link
  , unsetLink

    -- * Padding and margins
    -- | @padding@ and @margin@ take vertical then horizontal amounts.
    --
    -- >>> putBlock (render (border normalBorder <> padding 1 2 <> paddingChar '·' <> margin 0 2 <> marginChar '░') "box")
    -- ░░┌───────┐░░
    -- ░░│·······│░░
    -- ░░│··box··│░░
    -- ░░│·······│░░
    -- ░░└───────┘░░
  , padding
  , paddingAll
  , paddingTop
  , paddingRight
  , paddingBottom
  , paddingLeft
  , margin
  , marginAll
  , marginTop
  , marginRight
  , marginBottom
  , marginLeft
  , marginBg
  , paddingChar
  , marginChar
  , unsetPadding
  , unsetMargins
  , unsetMarginBg
  , Sides (..)

    -- * Borders
    -- | See "Lipstick.Border" for all the border character sets.
    --
    -- >>> putBlock (hcat [render (border b) "text" | b <- [normalBorder, roundedBorder, thickBorder, doubleBorder]])
    -- ┌────┐╭────╮┏━━━━┓╔════╗
    -- │text││text│┃text┃║text║
    -- └────┘╰────╯┗━━━━┛╚════╝
    -- >>> putBlock (render (border roundedBorder <> borderBottom False) "open below")
    -- ╭──────────╮
    -- │open below│
  , Border (topEdge, bottomEdge, leftEdge, rightEdge, topLeftCorner, topRightCorner, bottomLeftCorner, bottomRightCorner, middleLeft, middleRight, middle, middleTop, middleBottom)
  , border
  , borderFg
  , borderBg
  , borderFgBlend
  , borderFgBlendOffset
  , borderTopFg
  , borderRightFg
  , borderBottomFg
  , borderLeftFg
  , borderTopBg
  , borderRightBg
  , borderBottomBg
  , borderLeftBg
  , unsetBorder
  , borderTop
  , borderRight
  , borderBottom
  , borderLeft
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

    -- * Size, alignment and wrapping
    -- | Sizes include padding and border. With less room than its 'width',
    -- a thing shrinks.
    --
    -- >>> putBlock (render (width 12 <> height 4 <> border normalBorder <> align right <> valign bottom) "corner")
    -- ┌──────────┐
    -- │          │
    -- │    corner│
    -- └──────────┘
    -- >>> putBlock (render (width 14 <> border normalBorder <> noWrap) "This line is too long")
    -- ┌────────────┐
    -- │This line i…│
    -- └────────────┘
  , width
  , height
  , maxWidth
  , maxHeight
  , grow
  , fillWidth
  , stretchHeight
  , unsetWidth
  , unsetHeight
  , unsetMaxWidth
  , unsetMaxHeight
  , align
  , valign
  , unsetAlign
  , noWrap

    -- * Getters
    -- | The settings of a modifier, read back.
    --
    -- >>> let s = border normalBorder <> padding 1 2 <> width 20
    -- >>> (getWidth s, getPadding s, frameSize s)
    -- (Just 20,Sides {sideTop = 1, sideRight = 2, sideBottom = 1, sideLeft = 2},(6,4))
  , getAttrs
  , getMaxWidth
  , getMaxHeight
  , getPadding
  , getMargins
  , getMarginBg
  , getBorder
  , getBorderSides
  , getWidth
  , getHeight
  , getAlign
  , getVAlign
  , getTabWidth
  , getInline
  , horizontalPadding
  , verticalPadding
  , horizontalMargins
  , verticalMargins
  , horizontalBorderSize
  , verticalBorderSize
  , horizontalFrameSize
  , verticalFrameSize
  , frameSize

    -- * Tables
    -- | See "Lipstick.Table".
    --
    -- >>> putBlock (table (headerStyle (align center) <> cellStyle (const (padding 0 1))) ["Language", "Greeting"] [["Japanese", "こんにちは"], ["Russian", "Привет"]])
    -- ╭──────────┬────────────╮
    -- │ Language │  Greeting  │
    -- ├──────────┼────────────┤
    -- │ Japanese │ こんにちは │
    -- │ Russian  │ Привет     │
    -- ╰──────────┴────────────╯
  , Table
  , table
  , TableCell (..)
  , cellRow
  , cellColumn
  , cellStyle
  , headerStyle
  , headerRule
  , columnRules
  , rowRules
  , yOffset
  , fitContent
  , visibleRows

    -- * Lists and trees
    -- | See "Lipstick.Outline".
    --
    -- >>> putBlock (tree mempty (Node "." [Node "macOS" [], Node "Linux" [Node "NixOS" [], Node "Arch" []]]))
    -- .
    -- ├── macOS
    -- └── Linux
    --     ├── NixOS
    --     └── Arch
    -- >>> putBlock (list (enumerator arabic) ["Glossier", "Claire's Boutique", "Nyx"])
    -- 1. Glossier
    -- 2. Claire's Boutique
    -- 3. Nyx
  , Outline
  , tree
  , forest
  , list
  , nestedList
  , Tree (..)
  , Forest
  , ItemPos (itemDepth, itemIndex, itemSiblings)
  , rootStyle
  , itemStyle
  , itemStyleWith
  , enumeratorStyle
  , enumeratorStyleWith
  , indenterStyle
  , indenterStyleWith
  , itemOffset
  , Enumerator (enumMarker, enumIndent)
  , markers
  , enumerator
  , byDepth
  , isLast
  , indenter
  , treeEnumerator
  , roundedTreeEnumerator
  , bullet
  , dash
  , asterisk
  , arabic
  , alphabet
  , roman

    -- * Colours
    -- | See "Lipstick.Color".
    --
    -- >>> blend 3 ["#ff0000", "#0000ff"]
    -- [rgb 255 0 0,rgb 140 83 162,rgb 0 0 255]
    -- >>> resolveColor TrueColor True (lightDark "#5A3FC0" "#B39DFF")
    -- Just (TCRGB 179 157 255)
  , module Lipstick.Color

    -- * Output
    -- | Printing detects what the output supports; 'toAnsi' gives the text
    -- for an environment of your own. See "Lipstick.Terminal" and
    -- "Lipstick.Ansi".
    --
    -- >>> toAnsi defaultRenderEnv {envProfile = ANSI16} (render (fg "#F25D94") "pink")
    -- "\ESC[0;35mpink\ESC[0m"
    -- >>> renderPlain (render (fg "#F25D94") "pink")
    -- "pink"
  , RenderEnv (envProfile, envDarkBackground, envWidth)
  , defaultRenderEnv
  , toAnsi
  , toAnsiLines
  , renderPlain
  , putBlock
  , hPutBlock
  , hPutBlockWith
  , detectEnv
  , detectEnvQuery
  , detectProfile
  , detectWidth
  , queryDarkBackground

    -- * Compositing
    -- | Blocks on top of each other. See "Lipstick.Layer".
    --
    -- >>> let background = place (width 20 <> height 5 <> whitespaceChars "╱") emptyBlock
    -- >>> putBlock (overlay 3 1 (render (border roundedBorder <> padding 0 1) "dialog") background)
    -- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
    -- ╱╱╱╭────────╮╱╱╱╱╱╱╱
    -- ╱╱╱│ dialog │╱╱╱╱╱╱╱
    -- ╱╱╱╰────────╯╱╱╱╱╱╱╱
    -- ╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
  , Layer
  , layer
  , offset
  , zIndex
  , layerId
  , transparent
  , sublayers
  , compose
  , overlay
  , layerAt
  , layerHit
  , findLayer
  , Rect (rectX, rectY, rectWidth, rectHeight)

    -- * Measuring text
    -- | Display widths. See "Lipstick.Width".
    --
    -- >>> textWidth "こんにちは"
    -- 10
    -- >>> ansiWidth "\ESC[1mbold\ESC[0m"
    -- 4
  , textWidth
  , ansiWidth
  , ansiHeight
  , unicodeVersion
  ) where

import Data.Tree (Forest, Tree (..))
import Lipstick.Ansi
import Lipstick.Block
import Lipstick.Border
import Lipstick.Color
import Lipstick.Internal.Types
import Lipstick.Layer
import Lipstick.Mod
import Lipstick.Outline
import Lipstick.Place
import Lipstick.Style
import Lipstick.Table
import Lipstick.Terminal
import Lipstick.Width (textWidth, unicodeVersion)

-- $setup
-- >>> import Lipstick
