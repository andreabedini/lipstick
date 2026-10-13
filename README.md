# lipstick

Style definitions for nice terminal layouts, in Haskell. lipstick is a port
of [lipgloss](https://github.com/charmbracelet/lipgloss) (v2) for
command-line programs: colours, borders, padding, alignment, tables, lists,
trees and layered compositions, rendered to text with ANSI escape sequences.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Lipstick

card :: Mod Style
card =
  bold
    <> fg "#FAFAFA"
    <> bg "#7D56F4"
    <> padding 1 4
    <> width 24
    <> border roundedBorder
    <> borderFg "#874BFD"

main :: IO ()
main = putBlock (render card "Hello, kitty")
```

```
╭──────────────────────╮
│                      │
│    Hello, kitty      │
│                      │
╰──────────────────────╯
```

The outputs in this README are shown without colours. `cabal run
lipstick-demo` shows them all in a terminal.

lipstick builds with GHC 9.12 and is not on Hackage yet; add this
repository to your `cabal.project` to use it.

## Styles are modifiers

A `Mod Style` is a modifier, in the same sense as optparse-applicative's
`Mod`: modifiers form a monoid and later settings win, so styles are built
and extended with `<>`.

```haskell
warning :: Mod Style
warning = card <> bg "#F25D94"
```

Text attributes (`fg`, `bg`, `bold`, `underline`, `link`, …) and the
`maxWidth` / `maxHeight` crop apply to every kind of thing lipstick draws,
not just styles, so the same modifier works everywhere:

```haskell
base :: Mod f
base = fg red <> bold

render base "text"            -- a styled box
table base headers rows       -- the base style of a table
list base items               -- the base style of a list
```

Other modifiers apply to the things that have the corresponding part:
`padding` and `margin` to styles, `border` to styles and tables, `width` to
styles, tables, lists, trees and placed blocks, `align` to styles and placed
blocks. A modifier on something that does not support it is a type error.

`width` and `height` include padding and border but not margins, as in
lipgloss. With a width, text is word-wrapped to fit, or cut with an ellipsis
with `noWrap`.

## Blocks and layout

`render` returns a `Block`: a rectangle of styled text that knows its width
and height. Blocks are joined, placed and framed without ever looking at
escape sequences.

```haskell
joinHorizontal center
  [ render card "Hello, kitty"
  , render (warning <> marginLeft 2) "Later settings win"
  ]
```

```
╭──────────────────────╮  ╭──────────────────────╮
│                      │  │                      │
│    Hello, kitty      │  │    Later settings    │
│                      │  │    win               │
╰──────────────────────╯  │                      │
                          ╰──────────────────────╯
```

```haskell
place (width 30 <> height 3 <> align center <> valign center <> whitespaceChars "·") "centred"
```

```
······························
···········centred············
······························
```

`frame` puts an existing block in a box, `joinVertical`, `hcat` and `vcat`
stack blocks, and `blockWidth` / `blockHeight` measure them.

## Text

Text with several styles is `Inline`, built with `styled` and `<>`. Styles
nest: the attributes of the enclosing style show through wherever the inner
ones do not set them.

```haskell
render (width 30 <> border normalBorder <> padding 0 1)
  (styled (bold <> fg red) "Error: " <> "the file " <> styled italic "notes.txt" <> " was not found")
```

```
┌────────────────────────────┐
│ Error: the file notes.txt  │
│ was not found              │
└────────────────────────────┘
```

Output from other programs that already contains escape sequences goes
through `fromAnsi`, which keeps colours, attributes and hyperlinks and drops
everything else:

```haskell
render (border roundedBorder) (fromAnsi "\ESC[1;32mok\ESC[0m 3 tests passed")
```

```
╭─────────────────╮
│ok 3 tests passed│
╰─────────────────╯
```

Also: `wrap` (word wrap with extra break characters such as `-`),
`styleRanges` and `styleIndices` (highlight columns or characters of
existing text, e.g. fuzzy-search matches), `transform`, `ansiWidth`.

Widths follow the Unicode 16.0.0 grapheme cluster rules (the official
conformance tests pass) and East Asian Width, so CJK text, emoji, flags and
combining marks line up.

## Tables

```haskell
table
  ( borderFg "#874BFD"
      <> headerStyle (bold <> align center)
      <> cellStyle (\c -> padding 0 1 <> if odd (cellRow c) then faint else mempty)
  )
  ["Language", "Formal", "Informal"]
  [ ["Chinese", "您好", "你好"]
  , ["Japanese", "こんにちは", "やあ"]
  , ["Russian", "Здравствуйте", "Привет"]
  ]
```

```
╭──────────┬──────────────┬──────────╮
│ Language │    Formal    │ Informal │
├──────────┼──────────────┼──────────┤
│ Chinese  │ 您好         │ 你好     │
│ Japanese │ こんにちは   │ やあ     │
│ Russian  │ Здравствуйте │ Привет   │
╰──────────┴──────────────┴──────────╯
```

With a `width`, columns shrink (wrapping their cells) or grow to fit it;
`fitContent` makes the width a maximum instead. With a `height`, rows that
do not fit become a row of `…`, `yOffset` scrolls, and `visibleRows` tells
which rows are shown. `headerRule`, `columnRules`, `rowRules` and the
`border*` modifiers choose which lines are drawn.

## Lists and trees

Trees take a `Data.Tree`; items are blocks, so anything can go in them,
tables included.

```haskell
tree
  (enumerator roundedTreeEnumerator <> enumeratorStyle (fg "#874BFD"))
  (Node "." [Node "macOS" [], Node "Linux" [Node "NixOS" [], Node "Arch" []], Node "BSD" []])
```

```
.
├── macOS
├── Linux
│   ├── NixOS
│   ╰── Arch
╰── BSD
```

```haskell
nestedList
  (enumeratorBy (\depth -> if depth == 0 then roman else bullet))
  [Node "Glossier" [Node "Cloud Paint" [], Node "Boy Brow" []], Node "Nyx" []]
```

```
 I. Glossier
    • Cloud Paint
    • Boy Brow
II. Nyx
```

The enumerators are `treeEnumerator`, `roundedTreeEnumerator`, `bullet`,
`dash`, `asterisk`, `arabic`, `alphabet` and `roman`, or your own. Items,
enumerators and indentation are styled separately, by position if needed.

## Layers

Blocks can be stacked at any position, for dialogs, overlays and the like.
`layerAt` finds the named layer under a point, for mouse handling.

```haskell
compose
  [ layer mempty background
  , layer (offset 6 0 <> zIndex 1 <> layerId "dialog") (render (border roundedBorder <> padding 0 1) "Really quit?")
  ]
```

```
╱╱╱╱╱╱╭──────────────╮╱╱╱╱╱╱
╱╱╱╱╱╱│ Really quit? │╱╱╱╱╱╱
╱╱╱╱╱╱╰──────────────╯╱╱╱╱╱╱
╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱╱
```

## Colours

Colours are written as `"#7D56F4"`, `"#fa0"`, palette indices (`"201"`,
`"5"`) or names (`red`, `brightBlue`, …). Adaptive colours are resolved only
when printing:

- `lightDark light dark` picks by the terminal background;
- `complete trueColor color256 color16` picks by what the terminal supports.

`blend` and `blend2D` make gradients, `darken`, `lighten` and `complementary`
adjust colours; they work in the OkLab colour space. `borderFgBlend` draws a
gradient around a border.

## Output

`putBlock` and `hPutBlock` print a block, reducing colours to what the
output supports. Detection follows the usual conventions: no escape
sequences when the output is not a terminal (unless `CLICOLOR_FORCE` is
set), no colours with `NO_COLOR`, and the colour depth from `COLORTERM` and
`TERM`. On Windows the console's ANSI processing is switched on as needed.

The background is taken from `COLORFGBG` when the terminal sets it and is
assumed dark otherwise. To ask the terminal, which takes a round trip and
reads from stdin, detect once and keep the result:

```haskell
main = do
  env <- detectEnvQuery stdout
  hPutBlockWith env stdout (render card "Hello")
```

`toAnsi env block` gives the text without printing it, and `renderPlain`
gives it without any escape sequences.

## brick and vty

The `lipstick-brick` package in this repository shows blocks in brick and
vty applications: `blockWidget` and `blockImage`.

## How lipstick differs from lipgloss

lipgloss renders to strings with escape sequences in them, and measures and
joins them by parsing those sequences again. lipstick renders to blocks of
styled spans and produces escape sequences only when printing. Widths are
exact, layout never sees an escape sequence, and adaptive colours are
resolved for the actual terminal at the end.

That also changes some behaviour:

- Nested styles keep the outer style. In lipgloss the outer style wraps the
  line once, so a reset at the end of nested styled text ends it for the
  rest of the line.
- The text attributes of any thing are the base for everything it draws:
  for a table they are lipgloss's `BaseStyle`, and for `place` they apply to
  the placed block as well as to the whitespace.
- Lists and trees are both `Outline`s (`Mod Tree` would clash with
  `Data.Tree`).

Not ported: `Alpha` (terminals cannot show transparency), the Windows
build-number check (lipstick requires a Windows that supports ANSI
sequences anyway), and Go-specific API such as `SetString`, `Copy`,
`WrapWriter` and the table data interfaces, which plain values and lists
cover.

## Development

```
cabal build all
cabal test all
cabal run lipstick-demo
```

The Unicode tables in `src/Lipstick/Internal/UnicodeTable.hs` are generated
from the Unicode Character Database, at the version GHC's `base` uses (see
`GHC.Unicode.unicodeVersion`). To regenerate them, download
`EastAsianWidth.txt`, `auxiliary/GraphemeBreakProperty.txt`,
`emoji/emoji-data.txt` and `DerivedCoreProperties.txt` from
`https://www.unicode.org/Public/<version>/ucd/` into one directory and run

```
runghc scripts/GenUnicodeTable.hs <version> <directory> > src/Lipstick/Internal/UnicodeTable.hs
```

The conformance test data in `test/data/GraphemeBreakTest.txt` comes from
`auxiliary/GraphemeBreakTest.txt` at the same version.
