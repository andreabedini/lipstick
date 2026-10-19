-- | Styles: text attributes plus a box model (alignment, padding, border,
-- margins), applied to text or to an existing block.
--
-- A style is a @'Mod' 'Style'@, built from the modifiers in
-- "Lipstick.Mod". The examples print with 'Lipstick.Terminal.putBlock';
-- the output is shown without colours.
--
-- == Examples
--
-- Text in a box:
--
-- >>> putBlock (render (border roundedBorder <> padding 0 1) "Hello, kitty")
-- ╭──────────────╮
-- │ Hello, kitty │
-- ╰──────────────╯
--
-- With a 'width', text is word-wrapped and aligned inside it:
--
-- >>> putBlock (render (width 18 <> border normalBorder <> align center) "Text wraps to fit the width")
-- ┌────────────────┐
-- │ Text wraps to  │
-- │ fit the width  │
-- └────────────────┘
--
-- With 'noWrap', lines that are too long are cut short instead:
--
-- >>> putBlock (render (width 14 <> border normalBorder <> noWrap) "This line is too long")
-- ┌────────────┐
-- │This line i…│
-- └────────────┘
--
-- Padding is inside the border and margins outside it; 'paddingChar' and
-- 'marginChar' make them visible:
--
-- >>> putBlock (render (border normalBorder <> padding 1 2 <> paddingChar '·' <> margin 1 3 <> marginChar '░') "box")
-- ░░░░░░░░░░░░░░░
-- ░░░┌───────┐░░░
-- ░░░│·······│░░░
-- ░░░│··box··│░░░
-- ░░░│·······│░░░
-- ░░░└───────┘░░░
-- ░░░░░░░░░░░░░░░
--
-- Some sides of the border only:
--
-- >>> putBlock (render (border thickBorder <> borderLeft False <> borderRight False) "rules above and below")
-- ━━━━━━━━━━━━━━━━━━━━━
-- rules above and below
-- ━━━━━━━━━━━━━━━━━━━━━
--
-- Later settings win, so a style is extended with '<>':
--
-- >>> let card = border roundedBorder <> padding 0 1 <> width 20
-- >>> let wide = card <> width 30
-- >>> putBlock (vcat [render card "a card", render wide "a wider card"])
-- ╭──────────────────╮
-- │ a card           │
-- ╰──────────────────╯
-- ╭────────────────────────────╮
-- │ a wider card               │
-- ╰────────────────────────────╯
--
-- 'frame' puts a block that already exists in a box:
--
-- >>> putBlock (frame (border doubleBorder <> padding 0 1) (vcat ["one", "two", "three"]))
-- ╔═══════╗
-- ║ one   ║
-- ║ two   ║
-- ║ three ║
-- ╚═══════╝
--
-- The getters read a style back:
--
-- >>> frameSize (border normalBorder <> padding 1 2 <> margin 0 1)
-- (8,4)
module Lipstick.Style
  ( Style
  , render
  , frame
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
  , inherit

    -- * Getters
    -- | The getters read what a style sets, applied to the defaults. They
    -- do not see what it inherits where it is used: inside a 'frame', the
    -- 'align', 'noWrap', 'breakAfter', 'tabWidth' and 'transform' it leaves
    -- unset come from the enclosing style.
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
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lipstick.Border
import Lipstick.Color (Color)
import Lipstick.Internal.Grid
import Lipstick.Internal.Layout (Assign (..), Block (..), Measured (..), Request (..), TextStyle (..), cropLayoutWidth, measureIn, takenWidth, textBlock)
import Lipstick.Internal.Types
import Lipstick.Internal.Mod
import Lipstick.Width (textWidth)

-- $setup
-- >>> import Lipstick
-- >>> import Data.Text qualified as T

-- | The fields of a style. Its text attributes are in the common
-- 'Props': they apply to the content and padding, not to the border or
-- margins, which have their own colours ('borderFg', 'marginBg').
data Style = Style
  { styleBox :: !Box
  , styleBorder :: !BorderProps
  , styleSize :: !Size
  , styleAlign :: !Align
  , styleTabWidth :: !Int
  , styleInline :: !Bool
  , styleWrap :: !Bool
  , styleBreaks :: ![Char]
  , styleColorWhitespace :: !Bool
  , styleUnderlineSpaces :: !(Maybe Bool)
  , styleStrikeSpaces :: !(Maybe Bool)
  , styleTransform :: !(Maybe (Text -> Text))
  }

instance HasBox Style where
  overBox f s = s {styleBox = f (styleBox s)}

instance HasBorder Style where
  overBorder f s = s {styleBorder = f (styleBorder s)}

instance HasSize Style where
  overSize f s = s {styleSize = f (styleSize s)}

instance HasAlign Style where
  overAlign f s = s {styleAlign = f (styleAlign s)}

instance HasWrap Style where
  overWrap f s = s {styleWrap = f (styleWrap s)}

defStyle :: Style
defStyle = Style defBox defBorderProps defSize defAlign 4 False True [] True Nothing Nothing Nothing

-- | The style and properties a modifier produces from the defaults.
resolveStyle :: Mod Style -> (Style, Props)
resolveStyle m = runMod m defStyle

-- | How many spaces a tab becomes (default 4).
tabWidth :: Int -> Mod Style
tabWidth n = fieldMod (\s -> s {styleTabWidth = max 0 n})

-- | Back to the default of 4 spaces per tab, even inside a style that
-- sets another 'tabWidth'.
unsetTabWidth :: Mod Style
unsetTabWidth = fieldMod (\s -> s {styleTabWidth = styleTabWidth defStyle})

-- | Render on a single line: newlines are removed, and padding, margins,
-- border, size and alignment are ignored, except 'maxWidth' and
-- 'maxHeight'.
--
-- >>> putBlock (render (inline <> border normalBorder <> padding 1 1) "one\ntwo")
-- onetwo
inline :: Mod Style
inline = fieldMod (\s -> s {styleInline = True})

-- | Undo 'inline': render with the box.
unsetInline :: Mod Style
unsetInline = fieldMod (\s -> s {styleInline = False})

-- | Change the text before rendering it (each styled piece separately),
-- including text inside blocks the style frames.
--
-- >>> putBlock (render (transform T.toUpper) "raow!")
-- RAOW!
transform :: (Text -> Text) -> Mod Style
transform f = fieldMod (\s -> s {styleTransform = Just f})

-- | No 'transform', not even one from an enclosing style.
unsetTransform :: Mod Style
unsetTransform = fieldMod (\s -> s {styleTransform = Nothing})

-- | Also allow a line break after any of these characters (such as @-@),
-- besides at spaces.
--
-- >>> putBlock (render (width 9) "a long-winded sentence")
-- a
-- long-wind
-- ed
-- sentence
-- >>> putBlock (render (breakAfter "-" <> width 9) "a long-winded sentence")
-- a long-
-- winded
-- sentence
breakAfter :: [Char] -> Mod Style
breakAfter cs = fieldMod (\s -> s {styleBreaks = cs})

-- | Whether the background colours the padding and alignment space
-- (default 'True').
colorWhitespace :: Bool -> Mod Style
colorWhitespace v = fieldMod (\s -> s {styleColorWhitespace = v})

-- | Whether spaces in the text are underlined. By default they are when
-- the text is; @underlineSpaces True@ underlines them even when the text
-- is not.
underlineSpaces :: Bool -> Mod Style
underlineSpaces v = fieldMod (\s -> s {styleUnderlineSpaces = Just v})

-- | Like 'underlineSpaces', for strikethrough.
strikethroughSpaces :: Bool -> Mod Style
strikethroughSpaces v = fieldMod (\s -> s {styleStrikeSpaces = Just v})

-- | A parent style without its padding and margins, to put before a
-- child style: in @inherit parent <> child@, the child gets the parent's
-- settings except the padding and margins, and its own settings win, as
-- with lipgloss's @child.Inherit(parent)@.
--
-- >>> let parent = border roundedBorder <> padding 1 4 <> width 20
-- >>> putBlock (render (inherit parent <> padding 0 1 <> width 9) "child")
-- ╭───────╮
-- │ child │
-- ╰───────╯
--
-- Settings that come after a modifier always win over it, so
-- @child <> inherit parent@ would let the parent override the child.
inherit :: Mod Style -> Mod Style
inherit (Mod f g) = Mod (\s -> keepSpacing s (f s)) g
  where
    keepSpacing s s' =
      s' {styleBox = (styleBox s') {boxPadding = boxPadding (styleBox s), boxMargin = boxMargin (styleBox s)}}

-- | Render text in a box: @frame m ('Lipstick.Block.fromInline' inl)@.
-- The text is word-wrapped to fit inside the padding and border: of the
-- 'width' if one is set, and otherwise of the width the box is given when
-- it is laid out. With 'noWrap', lines that are too long are cut short
-- with an ellipsis instead.
render :: Mod Style -> Inline -> Block
render m = frame m . textBlock

-- | Put a block in a box. The block is laid out in the width left inside
-- the padding and border: text in it wraps to fit, and other blocks take
-- what they need, aligned by 'align'.
--
-- The style's text settings ('align', 'noWrap', 'breakAfter', 'tabWidth',
-- 'transform', 'inline') apply to all the text inside, unless something
-- nearer sets them again, as its colours do.
frame :: Mod Style -> Block -> Block
frame m b = Block $ \ctx ->
  let (s, p) = runMod m (inheriting ctx)
      n = measureIn (textStyleOf ctx s) b
   in if styleInline s then inlineNode p n else box s p n

-- | The defaults of a style inside inherited text settings. Whether the
-- style itself is 'inline' (has no box) is not inherited.
inheriting :: TextStyle -> Style
inheriting ctx =
  defStyle
    { styleAlign = (styleAlign defStyle) {alignH = textAlign ctx}
    , styleWrap = textWrap ctx
    , styleBreaks = textBreaks ctx
    , styleTabWidth = textTabWidth ctx
    , styleTransform = textTransform ctx
    }

-- | The text settings a style passes on to what it frames.
textStyleOf :: TextStyle -> Style -> TextStyle
textStyleOf ctx s =
  TextStyle
    { textAlign = alignH (styleAlign s)
    , textWrap = styleWrap s
    , textBreaks = styleBreaks s
    , textTabWidth = styleTabWidth s
    , textTransform = styleTransform s
    , textInline = textInline ctx || styleInline s
    }

-- | An 'inline' style: no box, only its attributes under the content and
-- its 'maxWidth' and 'maxHeight' crop.
inlineNode :: Props -> Measured -> Measured
inlineNode p n = Measured req run
  where
    r = blockRequest n
    capW x = maybe x (min x . max 0) (propMaxWidth p)
    capH x = maybe x (min x . max 0) (propMaxHeight p)
    req = r {reqMin = capW (reqMin r), reqMax = capW (reqMax r), reqLimit = max 0 <$> propMaxWidth p, reqFill = False}
    run (Assign w h) =
      let (g, natural) = blockLayout n (Assign (cropLayoutWidth (propMaxWidth p) (reqGrow r) (reqMax r) w) h)
       in (fitWidth w (withProps p (gridCells g)), capH natural)

-- | A box around a block, laid out at the width it is given: margins,
-- border and padding take their columns and the block gets the rest (all
-- of it if it is text, and what it needs otherwise).
box :: Style -> Props -> Measured -> Measured
box s p c = Measured req run
  where
    Box (Sides pt pr pb pl) (Sides mt mr mb ml) _ _ _ = styleBox s
    Size wSet hSet weight stretch = styleSize s
    (bl, br) = borderColumns (styleBorder s)
    Edges et _ eb _ = snd (borderEdges (styleBorder s))
    frameW = pl + pr + bl + br
    frameH = pt + pb + fromEnum et + fromEnum eb
    capW x = maybe x (min x . max 0) (propMaxWidth p)
    capH x = maybe x (min x . max 0) (propMaxHeight p)
    cr = blockRequest c
    natW = maybe (reqMax cr + frameW) id wSet + ml + mr
    req =
      Request
        { reqMin = capW (maybe (reqMin cr + frameW) id wSet + ml + mr)
        , reqMax = capW natW
        , reqGrow = weight
        , reqLimit = max 0 <$> propMaxWidth p
        , reqStretch = stretch
        , reqFill = False
        }
    run (Assign w h) = (fitWidth w grid, natural)
      where
        boxW = max 0 (cropLayoutWidth (propMaxWidth p) weight natW w - ml - mr)
        innerW = max 0 (boxW - frameW)
        -- The height the box (without margins) is drawn at; the natural
        -- height must not depend on it.
        target = max (fromMaybe 0 hSet) (if stretch then h - mt - mb else 0)
        childW = if reqFill cr then innerW else takenWidth cr innerW
        (g, n) = blockLayout c (Assign childW (max 0 (target - frameH)))
        natural = capH (max (n + frameH) (fromMaybe 0 hSet) + mt + mb)
        sized = (styleSize s) {sizeWidth = Just boxW, sizeHeight = Just target}
        grid = layoutBox s {styleSize = sized} p (gridCells g)

-- | Widths of the left and right border columns.
borderColumns :: BorderProps -> (Int, Int)
borderColumns p =
  let (b, e) = borderEdges p
      w on t = if on then max 1 (textWidth t) else 0
   in (w (edgeLeft e) (leftEdge b), w (edgeRight e) (rightEdge b))

contentWidth :: Style -> Maybe Int
contentWidth s = do
  w <- sizeWidth (styleSize s)
  let Sides _ pr _ pl = boxPadding (styleBox s)
      (bl, br) = borderColumns (styleBorder s)
  pure (max 0 (w - pl - pr - bl - br))

-- | Draw a box around lines of content, at the style's width and height.
layoutBox :: Style -> Props -> [[Cell]] -> Grid
layoutBox s props content0 =
  let attrs = propAttrs props
      fill
        | styleColorWhitespace s = fillAttrs attrs
        | otherwise = (fillAttrs attrs) {attrBg = Nothing}
      Box (Sides pt pr pb pl) (Sides mt mr mb ml) mbg pch mch = styleBox s
      hgt = sizeHeight (styleSize s)
      Align ah av = styleAlign s
      bp = styleBorder s
      (chars, edges) = borderEdges bp
      (bl, br) = borderColumns bp
      bt = if edgeTop edges then 1 else 0
      bb = if edgeBottom edges then 1 else 0

      content = map (map spaceRule) (underlay attrs fill content0)
      spaceRule c
        | cellText c == T.singleton ' ' && not (cellFill c) = c {cellAttrs = spaceAttrs (cellAttrs c)}
        | otherwise = c
      spaceAttrs a =
        a
          { attrUnderline = case styleUnderlineSpaces s of
              Just True -> Just (fromMaybe SingleUnderline (attrUnderline a >>= visible))
              Just False -> Just NoUnderline
              Nothing -> attrUnderline a
          , attrStrikethrough = maybe (attrStrikethrough a) Just (styleStrikeSpaces s)
          }
      visible u = if u == NoUnderline then Nothing else Just u

      -- Horizontal alignment within the content width.
      innerW = maximum (fromMaybe 0 (contentWidth s) : map cellsWidth content)
      aligned = map (alignCells fill ah innerW) content

      -- Vertical alignment within the content height.
      innerH = maximum (length aligned : [h - pt - pb - bt - bb | Just h <- [hgt]])
      (above, below) = splitExtra av (innerH - length aligned)
      blankLine = blankCells fill innerW
      tall = replicate above blankLine ++ aligned ++ replicate below blankLine

      -- Padding.
      padW = pl + innerW + pr
      padCells = fillCells fill (T.singleton pch)
      padded =
        replicate pt (padCells padW)
          ++ [padCells pl ++ l ++ padCells pr | l <- tall]
          ++ replicate pb (padCells padW)

      -- Border.
      sideAttrs side = mempty {attrFg = side (borderFgColors bp), attrBg = side (borderBgColors bp)}
      piece a t n = fillCells a t n
      corner a on t n = if on then piece a t n else []
      hRule a l edge r = corner a (edgeLeft edges) l bl ++ piece a edge padW ++ corner a (edgeRight edges) r br
      topRow = hRule (sideAttrs sideTop) (topLeftCorner chars) (topEdge chars) (topRightCorner chars)
      bottomRow = hRule (sideAttrs sideBottom) (bottomLeftCorner chars) (bottomEdge chars) (bottomRightCorner chars)
      sideL = if edgeLeft edges then piece (sideAttrs sideLeft) (leftEdge chars) bl else []
      sideR = if edgeRight edges then piece (sideAttrs sideRight) (rightEdge chars) br else []
      bordered =
        blendBorder (borderFgStops bp) (borderFgOffset bp) edges (length sideL) (length sideR) $
          [topRow | edgeTop edges]
            ++ [sideL ++ l ++ sideR | l <- padded]
            ++ [bottomRow | edgeBottom edges]

      -- Margins.
      ma = mempty {attrBg = mbg}
      boxW = bl + padW + br
      outerW = ml + boxW + mr
      marginCells = fillCells ma (T.singleton mch)
      margined =
        replicate mt (marginCells outerW)
          ++ [marginCells ml ++ l ++ marginCells mr | l <- bordered]
          ++ replicate mb (marginCells outerW)

   in cropToProps props margined

-- Getters ---------------------------------------------------------------------

-- | The text attributes a modifier sets (on any thing).
getAttrs :: Mod f -> Attrs
getAttrs = propAttrs . propsOf

-- | The 'maxWidth' a modifier sets (on any thing).
getMaxWidth :: Mod f -> Maybe Int
getMaxWidth = propMaxWidth . propsOf

-- | The 'maxHeight' a modifier sets (on any thing).
getMaxHeight :: Mod f -> Maybe Int
getMaxHeight = propMaxHeight . propsOf

field :: (Style -> a) -> Mod Style -> a
field f = f . fst . resolveStyle

-- | The padding on each side.
getPadding :: Mod Style -> Sides Int
getPadding = field (boxPadding . styleBox)

-- | The margins on each side.
getMargins :: Mod Style -> Sides Int
getMargins = field (boxMargin . styleBox)

-- | The 'marginBg', if one is set.
getMarginBg :: Mod Style -> Maybe Color
getMarginBg = field (boxMarginBg . styleBox)

-- | The border characters, if a border is drawn.
getBorder :: Mod Style -> Maybe Border
getBorder m =
  let bp = field styleBorder m
      (chars, Edges t r b l) = borderEdges bp
   in if or [t, r, b, l] then Just chars else Nothing

-- | Which sides of the border are drawn.
getBorderSides :: Mod Style -> Sides Bool
getBorderSides m = let Edges t r b l = snd (borderEdges (field styleBorder m)) in Sides t r b l

-- | The 'width', if one is set.
getWidth :: Mod Style -> Maybe Int
getWidth = field (sizeWidth . styleSize)

-- | The 'height', if one is set.
getHeight :: Mod Style -> Maybe Int
getHeight = field (sizeHeight . styleSize)

-- | The horizontal alignment ('left' by default).
getAlign :: Mod Style -> Position
getAlign = field (alignH . styleAlign)

-- | The vertical alignment ('top' by default).
getVAlign :: Mod Style -> Position
getVAlign = field (alignV . styleAlign)

-- | How many spaces a tab becomes.
getTabWidth :: Mod Style -> Int
getTabWidth = field styleTabWidth

-- | Whether the style is 'inline'.
getInline :: Mod Style -> Bool
getInline = field styleInline

-- | Columns of padding: left plus right.
horizontalPadding :: Mod Style -> Int
horizontalPadding m = let Sides _ r _ l = getPadding m in l + r

-- | Rows of padding: top plus bottom.
verticalPadding :: Mod Style -> Int
verticalPadding m = let Sides t _ b _ = getPadding m in t + b

-- | Columns of margins: left plus right.
horizontalMargins :: Mod Style -> Int
horizontalMargins m = let Sides _ r _ l = getMargins m in l + r

-- | Rows of margins: top plus bottom.
verticalMargins :: Mod Style -> Int
verticalMargins m = let Sides t _ b _ = getMargins m in t + b

-- | Columns taken by the left and right sides of the border, where they
-- are drawn.
horizontalBorderSize :: Mod Style -> Int
horizontalBorderSize m = let (l, r) = borderColumns (field styleBorder m) in l + r

-- | Rows taken by the top and bottom sides of the border, where they are
-- drawn.
verticalBorderSize :: Mod Style -> Int
verticalBorderSize m = let Sides t _ b _ = getBorderSides m in fromEnum t + fromEnum b

-- | Columns taken by padding, border and margins: what is left for content
-- is the available width minus this.
horizontalFrameSize :: Mod Style -> Int
horizontalFrameSize m = horizontalPadding m + horizontalBorderSize m + horizontalMargins m

-- | Rows taken by padding, border and margins.
verticalFrameSize :: Mod Style -> Int
verticalFrameSize m = verticalPadding m + verticalBorderSize m + verticalMargins m

-- | @(horizontalFrameSize, verticalFrameSize)@
frameSize :: Mod Style -> (Int, Int)
frameSize m = (horizontalFrameSize m, verticalFrameSize m)
