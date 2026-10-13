{-# LANGUAGE OverloadedStrings #-}

-- | Render a block as an SVG "screenshot" of a terminal window.
--
-- Everything is placed on the terminal's cell grid: each cluster is drawn
-- at its column and stretched to exactly its width with @textLength@, and
-- box-drawing and block characters are drawn as lines and rectangles, as
-- terminals do. The result looks right whatever fonts the viewer has, and
-- is a pure function of the block, so it can be compared in CI.
module Svg (blockSvg) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lipstick (ColorProfile (TrueColor), TermColor (..), resolveColor, termColorRGB)
import Lipstick.Internal.Block (Block (..), Cell (..), blockCells)
import Lipstick.Internal.Types (Attrs (..), Underline (..))
import Numeric (showFFloat, showHex)

-- Geometry ----------------------------------------------------------------------

fontSize, cellW, cellH :: Double
fontSize = 14
cellW = 8.4
cellH = 18

-- | Space around the content inside the window, and the window's margin.
padX, padTop, padBottom, margin, titleBar :: Double
padX = 20
padTop = 14
padBottom = 20
margin = 24
titleBar = 36

windowBg, defaultFg :: Text
windowBg = "#171717"
defaultFg = "#DDDDDD"

num :: Double -> Text
num x =
  let s = T.pack (showFFloat (Just 2) x "")
   in fromMaybe s (T.stripSuffix ".00" s)

-- | The block as an SVG document.
blockSvg :: Block -> Text
blockSvg b =
  T.unlines $
    [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" <> num totalW <> "\" height=\"" <> num totalH <> "\" viewBox=\"0 0 " <> num totalW <> " " <> num totalH <> "\">"
    , "<defs><filter id=\"shadow\" x=\"-20%\" y=\"-20%\" width=\"140%\" height=\"150%\"><feDropShadow dx=\"0\" dy=\"6\" stdDeviation=\"8\" flood-color=\"#000\" flood-opacity=\"0.45\"/></filter></defs>"
    , "<rect x=\"" <> num margin <> "\" y=\"" <> num margin <> "\" width=\"" <> num winW <> "\" height=\"" <> num winH <> "\" rx=\"10\" fill=\"" <> windowBg <> "\" filter=\"url(#shadow)\"/>"
    , circle 0 "#FF5F57"
    , circle 1 "#FEBC2E"
    , circle 2 "#28C840"
    , "<g font-family=\"'JetBrains Mono', 'SF Mono', Menlo, Consolas, 'DejaVu Sans Mono', 'Noto Sans Mono CJK SC', monospace\" font-size=\"" <> num fontSize <> "\">"
    ]
      ++ concat [backgrounds row cells | (row, cells) <- rows]
      ++ concat [foregrounds row cells | (row, cells) <- rows]
      ++ ["</g>", "</svg>"]
  where
    rows = zip [0 :: Int ..] (blockCells b)
    contentW = fromIntegral (blockWidth b) * cellW
    contentH = fromIntegral (blockHeight b) * cellH
    winW = padX * 2 + contentW
    winH = titleBar + padTop + contentH + padBottom
    totalW = winW + margin * 2
    totalH = winH + margin * 2
    originX = margin + padX
    originY = margin + titleBar + padTop

    circle :: Int -> Text -> Text
    circle i colour =
      "<circle cx=\"" <> num (margin + 20 + 20 * fromIntegral i) <> "\" cy=\"" <> num (margin + 18) <> "\" r=\"6\" fill=\"" <> colour <> "\"/>"

    cellsAt cells = zip (scanl (+) 0 (map cellWidth cells)) cells
    cellX col = originX + fromIntegral col * cellW
    cellY row = originY + fromIntegral row * cellH

    backgrounds row cells =
      [ "<rect x=\"" <> num (cellX col) <> "\" y=\"" <> num (cellY row) <> "\" width=\"" <> num (fromIntegral (cellWidth c) * cellW + 0.5) <> "\" height=\"" <> num (cellH + 0.5) <> "\" fill=\"" <> colour <> "\"/>"
      | (col, c) <- cellsAt cells
      , Just colour <- [snd (colours (cellAttrs c))]
      ]

    foregrounds row cells = concatMap (glyph row) (cellsAt cells)

    glyph row (col, c)
      | cellWidth c == 0 = []
      | otherwise =
          let a = cellAttrs c
              (fgc, _) = colours a
              ink = fromMaybe defaultFg fgc
              x = cellX col
              y = cellY row
              w = fromIntegral (cellWidth c) * cellW
              opacity = if on attrFaint a then " opacity=\"0.55\"" else ""
              decorations = underlineSvg a ink x y w ++ strikeSvg a ink x y w
              ch = T.unpack (cellText c)
           in case ch of
                [one] | Just shape <- boxShape one -> [wrapG ink opacity (shape x y)] ++ decorations
                _
                  | cellText c == " " -> decorations
                  | otherwise ->
                      [ "<text x=\"" <> num x <> "\" y=\"" <> num (y + cellH * 0.76) <> "\" textLength=\"" <> num w <> "\" lengthAdjust=\"spacingAndGlyphs\" fill=\"" <> ink <> "\"" <> opacity <> weight a <> style a <> ">" <> escape (cellText c) <> "</text>"
                      ]
                        ++ decorations

    wrapG ink opacity inner = "<g stroke=\"" <> ink <> "\" fill=\"" <> ink <> "\"" <> opacity <> ">" <> inner <> "</g>"
    weight a = if on attrBold a then " font-weight=\"bold\"" else ""
    style a = if on attrItalic a then " font-style=\"italic\"" else ""

on :: (Attrs -> Maybe Bool) -> Attrs -> Bool
on f = fromMaybe False . f

-- | Foreground and background, with reverse video applied. Adaptive
-- colours are resolved for a true-colour terminal with a dark background.
colours :: Attrs -> (Maybe Text, Maybe Text)
colours a =
  let fgc = hexOf <$> (attrFg a >>= resolveColor TrueColor True)
      bgc = hexOf <$> (attrBg a >>= resolveColor TrueColor True)
   in if on attrReverse a
        then (Just (fromMaybe windowBg bgc), Just (fromMaybe defaultFg fgc))
        else (fgc, bgc)

hexOf :: TermColor -> Text
hexOf c =
  let (r, g, b) = termColorRGB c
      two v = let s = showHex v "" in T.pack (if length s == 1 then '0' : s else s)
   in "#" <> T.toUpper (two r <> two g <> two b)

escape :: Text -> Text
escape = T.concatMap $ \case
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  ch -> T.singleton ch

underlineSvg :: Attrs -> Text -> Double -> Double -> Double -> [Text]
underlineSvg a ink x y w = case fromMaybe NoUnderline (attrUnderline a) of
  NoUnderline -> []
  CurlyUnderline ->
    let yy = y + cellH - 2.5
        waves = [x + fromIntegral i * 4 | i <- [0 .. floor (w / 4) - 1 :: Int]]
        path = T.concat ["q 1 -2 2 0 t 2 0" | _ <- waves]
     in ["<path d=\"M " <> num x <> " " <> num yy <> " " <> path <> "\" stroke=\"" <> underlineInk <> "\" fill=\"none\" stroke-width=\"1\"/>"]
  DoubleUnderline -> [hline (y + cellH - 3.5), hline (y + cellH - 1.5)]
  _ -> [hline (y + cellH - 2.5)]
  where
    underlineInk = maybe ink hexOf (attrUnderlineColor a >>= resolveColor TrueColor True)
    hline yy = "<line x1=\"" <> num x <> "\" y1=\"" <> num yy <> "\" x2=\"" <> num (x + w) <> "\" y2=\"" <> num yy <> "\" stroke=\"" <> underlineInk <> "\" stroke-width=\"1\"/>"

strikeSvg :: Attrs -> Text -> Double -> Double -> Double -> [Text]
strikeSvg a ink x y w
  | on attrStrikethrough a =
      ["<line x1=\"" <> num x <> "\" y1=\"" <> num (y + cellH / 2) <> "\" x2=\"" <> num (x + w) <> "\" y2=\"" <> num (y + cellH / 2) <> "\" stroke=\"" <> ink <> "\" stroke-width=\"1\"/>"]
  | otherwise = []

-- Box drawing ---------------------------------------------------------------------

-- | How a line leaves the centre of a cell towards one edge.
data Arm = None | Light | Heavy | Double
  deriving (Eq)

-- | Arms towards the top, right, bottom and left edges.
arms :: Char -> Maybe (Arm, Arm, Arm, Arm)
arms ch = lookup ch table
  where
    l = Light
    h = Heavy
    d = Double
    n = None
    table =
      [ ('─', (n, l, n, l)), ('│', (l, n, l, n)), ('┌', (n, l, l, n)), ('┐', (n, n, l, l))
      , ('└', (l, l, n, n)), ('┘', (l, n, n, l)), ('├', (l, l, l, n)), ('┤', (l, n, l, l))
      , ('┬', (n, l, l, l)), ('┴', (l, l, n, l)), ('┼', (l, l, l, l))
      , ('━', (n, h, n, h)), ('┃', (h, n, h, n)), ('┏', (n, h, h, n)), ('┓', (n, n, h, h))
      , ('┗', (h, h, n, n)), ('┛', (h, n, n, h)), ('┣', (h, h, h, n)), ('┫', (h, n, h, h))
      , ('┳', (n, h, h, h)), ('┻', (h, h, n, h)), ('╋', (h, h, h, h))
      , ('═', (n, d, n, d)), ('║', (d, n, d, n)), ('╔', (n, d, d, n)), ('╗', (n, n, d, d))
      , ('╚', (d, d, n, n)), ('╝', (d, n, n, d)), ('╠', (d, d, d, n)), ('╣', (d, n, d, d))
      , ('╦', (n, d, d, d)), ('╩', (d, d, n, d)), ('╬', (d, d, d, d))
      ]

-- | Characters drawn as shapes, given the cell's top-left corner.
boxShape :: Char -> Maybe (Double -> Double -> Text)
boxShape ch
  | Just a <- arms ch = Just (drawArms a)
  | Just corner <- lookup ch rounded = Just (drawRounded corner)
  | Just quads <- lookup ch blocks = Just (drawBlocks quads)
  | ch == '╱' = Just (\x y -> line x (y + cellH) (x + cellW) y 1.2)
  | ch == '╲' = Just (\x y -> line x y (x + cellW) (y + cellH) 1.2)
  | otherwise = Nothing
  where
    rounded = [('╭', (1, 1)), ('╮', (-1, 1)), ('╰', (1, -1)), ('╯', (-1, -1))]
    -- Quadrants: top-left, top-right, bottom-left, bottom-right.
    blocks =
      [ ('█', [True, True, True, True]), ('▀', [True, True, False, False]), ('▄', [False, False, True, True])
      , ('▌', [True, False, True, False]), ('▐', [False, True, False, True]), ('▛', [True, True, True, False])
      , ('▜', [True, True, False, True]), ('▙', [True, False, True, True]), ('▟', [False, True, True, True])
      , ('▖', [False, False, True, False]), ('▗', [False, False, False, True]), ('▘', [True, False, False, False])
      , ('▝', [False, True, False, False])
      ]

line :: Double -> Double -> Double -> Double -> Double -> Text
line x1 y1 x2 y2 sw =
  "<line x1=\"" <> num x1 <> "\" y1=\"" <> num y1 <> "\" x2=\"" <> num x2 <> "\" y2=\"" <> num y2 <> "\" stroke-width=\"" <> num sw <> "\" stroke-linecap=\"square\"/>"

drawArms :: (Arm, Arm, Arm, Arm) -> Double -> Double -> Text
drawArms (up, right, down, left) x y =
  T.concat (single ++ doubles)
  where
    cx = x + cellW / 2
    cy = y + cellH / 2
    width = \case
      Heavy -> 2.4
      _ -> 1.2
    single =
      [line cx cy cx y (width up) | up `elem` [Light, Heavy]]
        ++ [line cx cy (x + cellW) cy (width right) | right `elem` [Light, Heavy]]
        ++ [line cx cy cx (y + cellH) (width down) | down `elem` [Light, Heavy]]
        ++ [line cx cy x cy (width left) | left `elem` [Light, Heavy]]
    -- Double lines: two strokes either side of the centre line. A stroke
    -- facing an arm on its side is split around that arm; the other runs
    -- through, stopping at the strokes it meets.
    o = 2
    du = up == Double
    dr = right == Double
    dd = down == Double
    dl = left == Double
    doubles =
      (if du || dd then vertical (-1) ++ vertical 1 else [])
        ++ (if dl || dr then horizontal (-1) ++ horizontal 1 else [])
    vertical side =
      let sx = cx + side * o
          facing = if side < 0 then dl else dr
       in if facing
            then [line sx y sx (cy - o) 1 | du] ++ [line sx (cy + o) sx (y + cellH) 1 | dd]
            else [line sx (if du then y else cy - o) sx (if dd then y + cellH else cy + o) 1]
    horizontal side =
      let sy = cy + side * o
          facing = if side < 0 then du else dd
       in if facing
            then [line x sy (cx - o) sy 1 | dl] ++ [line (cx + o) sy (x + cellW) sy 1 | dr]
            else [line (if dl then x else cx - o) sy (if dr then x + cellW else cx + o) sy 1]

-- | A rounded corner: the directions (towards the right or left, towards
-- the bottom or top) its two arms go.
drawRounded :: (Double, Double) -> Double -> Double -> Text
drawRounded (hx, vy) x y =
  let cx = x + cellW / 2
      cy = y + cellH / 2
      r = cellW / 2
      edgeX = if hx > 0 then x + cellW else x
      edgeY = if vy > 0 then y + cellH else y
      -- ╭ and ╯ turn anticlockwise on screen, ╮ and ╰ clockwise.
      sweep = if hx * vy > 0 then "0" else "1" :: Text
   in "<path d=\"M "
        <> num edgeX
        <> " "
        <> num cy
        <> " H "
        <> num (cx + hx * r)
        <> " A "
        <> num r
        <> " "
        <> num r
        <> " 0 0 "
        <> sweep
        <> " "
        <> num cx
        <> " "
        <> num (cy + vy * r)
        <> " V "
        <> num edgeY
        <> "\" fill=\"none\" stroke-width=\"1.2\"/>"

drawBlocks :: [Bool] -> Double -> Double -> Text
drawBlocks quads x y =
  T.concat
    [ "<rect x=\"" <> num qx <> "\" y=\"" <> num qy <> "\" width=\"" <> num (cellW / 2 + 0.3) <> "\" height=\"" <> num (cellH / 2 + 0.3) <> "\" stroke=\"none\"/>"
    | (filled, (qx, qy)) <- zip quads [(x, y), (x + cellW / 2, y), (x, y + cellH / 2), (x + cellW / 2, y + cellH / 2)]
    , filled
    ]
