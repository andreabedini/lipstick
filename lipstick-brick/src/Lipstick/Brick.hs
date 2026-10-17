-- | Show lipstick blocks in brick and vty applications.
--
-- > ui :: Widget ()
-- > ui = blockWidget defaultRenderEnv (render (border roundedBorder <> padding 1 2) "Hello")
--
-- Widgets lay their block out in the width brick gives them, and take all
-- of it ('Greedy') when the block grows. Colours are resolved for the
-- 'RenderEnv' (light or dark background, profile) and handed to vty, which downsamples them further if its output
-- needs it. 'profileForColorMode' gives the profile matching a vty output.
--
-- vty measures text with its own width tables, which can disagree with
-- lipstick's for some emoji; layouts made of such characters may be off by
-- a column.
module Lipstick.Brick
  ( blockWidget
  , blockImage
  , profileForColorMode
  ) where

import Brick.Types (Size (..), Widget (..), availWidthL, getContext)
import Brick.Widgets.Core (raw)
import Lens.Micro ((^.))
import Data.Maybe (fromMaybe)
import Graphics.Vty qualified as V
import Lipstick (Attrs (..), Block, ColorProfile (..), RenderEnv (..), TermColor (..), Underline (..), blockGrows, escapeUri, layout, resolveColor)
import Lipstick.Grid (Span (..), gridLines, lineSpans)

-- | A widget showing a block, laid out in the width brick makes available
-- (whatever the environment's 'envWidth'). It is 'Greedy' horizontally
-- when the block grows (see 'Lipstick.fillWidth'), and 'Fixed' otherwise.
blockWidget :: RenderEnv -> Block -> Widget n
blockWidget env b = Widget policy Fixed $ do
  ctx <- getContext
  render (raw (blockImage env {envWidth = Just (ctx ^. availWidthL)} b))
  where
    policy = if blockGrows b then Greedy else Fixed

-- | A vty image of a block, laid out in the environment's 'envWidth', and
-- exactly as wide and tall as the laid-out block. vty has
-- no images zero columns wide, so a block of width 0 gives 'V.emptyImage'
-- whatever its height.
blockImage :: RenderEnv -> Block -> V.Image
blockImage env b = V.vertCat (map line (gridLines (layout (envWidth env) b)))
  where
    line l = V.horizCat [V.text' (attr env (spanAttrs s)) (spanText s) | s <- lineSpans l]

-- | The profile to resolve colours for, given a vty output's colour mode.
profileForColorMode :: V.ColorMode -> ColorProfile
profileForColorMode = \case
  V.NoColor -> Ascii
  V.ColorMode8 -> ANSI16
  V.ColorMode16 -> ANSI16
  V.ColorMode240 _ -> ANSI256
  V.FullColor -> TrueColor

attr :: RenderEnv -> Attrs -> V.Attr
attr env a
  | profile == NoTTY = V.defAttr
  | otherwise =
      withColor V.withForeColor (attrFg a)
        . withColor V.withBackColor (attrBg a)
        . withLink (attrLink a)
        $ foldl V.withStyle V.defAttr styles
  where
    profile = envProfile env
    withColor f c x = maybe x (f x . vtyColor) (c >>= resolveColor profile (envDarkBackground env))
    withLink url x = maybe x (V.withURL x . escapeUri) url
    on f = fromMaybe False (f a)
    styles =
      [V.bold | on attrBold]
        ++ [V.dim | on attrFaint]
        ++ [V.italic | on attrItalic]
        ++ [V.blink | on attrBlink]
        ++ [V.reverseVideo | on attrReverse]
        ++ [V.strikethrough | on attrStrikethrough]
        ++ [V.underline | maybe False (/= NoUnderline) (attrUnderline a)]

vtyColor :: TermColor -> V.Color
vtyColor = \case
  TC16 n -> V.ISOColor n
  TC256 n
    | n < 16 -> V.ISOColor n
    | otherwise -> V.Color240 (n - 16)
  TCRGB r g b -> V.RGBColor r g b
