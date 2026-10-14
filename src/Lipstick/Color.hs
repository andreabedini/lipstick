{-# LANGUAGE LambdaCase #-}

-- | Colours, colour profiles and downsampling.
--
-- A 'Color' is what you write in a style. It may be adaptive ('LightDark',
-- 'Complete'), so it is only turned into a concrete 'TermColor' when the
-- block is serialised and the terminal's 'ColorProfile' is known.
module Lipstick.Color
  ( -- * Colours
    Color (..)
  , rgb
  , ansi
  , ansi256
  , hex
  , lightDark
  , complete

    -- ** The 16 basic colours
  , black, red, green, yellow, blue, magenta, cyan, white
  , brightBlack, brightRed, brightGreen, brightYellow
  , brightBlue, brightMagenta, brightCyan, brightWhite

    -- * Manipulating colours
    -- | These work in the OkLab colour space, so steps look even. Palette
    -- colours are converted to RGB using xterm's palette, and adaptive
    -- colours are transformed in every variant.
  , mix
  , blend
  , blend2D
  , darken
  , lighten
  , complementary

    -- * Terminal colours
  , TermColor (..)
  , ColorProfile (..)
  , resolveColor
  , downsample
  , termColorRGB
  ) where

import Data.Bits (shiftL, (.|.))
import Data.Char (digitToInt, isDigit, isHexDigit)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)

-- | A colour as written in a style.
data Color
  = -- | A 24-bit colour.
    RGB !Word8 !Word8 !Word8
  | -- | A colour from the 256-colour palette.
    Color256 !Word8
  | -- | One of the 16 basic colours (0–15).
    Color16 !Word8
  | -- | Pick the first colour on light backgrounds, the second on dark ones.
    LightDark Color Color
  | -- | Explicit choices for true-colour, 256-colour and 16-colour terminals.
    Complete Color Color Color
  deriving (Eq, Ord, Show)

-- | Parses @"#rgb"@, @"#rrggbb"@ or a palette index @"0"@–@"255"@.
-- Calls 'error' on anything else; use 'hex' for a total alternative.
instance IsString Color where
  fromString s =
    fromMaybe (error ("Lipstick.Color: invalid colour literal " ++ show s)) (parseColor s)

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb = RGB

-- | One of the 16 basic colours; the index is taken modulo 16.
ansi :: Int -> Color
ansi n = Color16 (fromIntegral (n `mod` 16))

-- | A colour from the 256-colour palette; the index is taken modulo 256.
ansi256 :: Int -> Color
ansi256 n = Color256 (fromIntegral (n `mod` 256))

-- | Parses @"#rgb"@ or @"#rrggbb"@ (the @#@ is optional).
hex :: Text -> Maybe Color
hex t = parseHex (T.unpack (fromMaybe t (T.stripPrefix (T.singleton '#') t)))

lightDark :: Color -> Color -> Color
lightDark = LightDark

-- | @complete trueColor color256 color16@
complete :: Color -> Color -> Color -> Color
complete = Complete

parseColor :: String -> Maybe Color
parseColor = \case
  '#' : ds -> parseHex ds
  ds
    | not (null ds), length ds <= 3, all isDigit ds
    , n <- read ds :: Int
    , n <= 255 ->
        Just (if n < 16 then Color16 (fromIntegral n) else Color256 (fromIntegral n))
  _ -> Nothing

parseHex :: String -> Maybe Color
parseHex ds
  | not (all isHexDigit ds) = Nothing
  | [r, g, b] <- ds = Just (RGB (byte [r, r]) (byte [g, g]) (byte [b, b]))
  | [r1, r2, g1, g2, b1, b2] <- ds = Just (RGB (byte [r1, r2]) (byte [g1, g2]) (byte [b1, b2]))
  | otherwise = Nothing
  where
    byte = foldl (\acc c -> acc `shiftL` 4 .|. fromIntegral (digitToInt c)) 0

black, red, green, yellow, blue, magenta, cyan, white :: Color
black = Color16 0
red = Color16 1
green = Color16 2
yellow = Color16 3
blue = Color16 4
magenta = Color16 5
cyan = Color16 6
white = Color16 7

brightBlack, brightRed, brightGreen, brightYellow :: Color
brightBlack = Color16 8
brightRed = Color16 9
brightGreen = Color16 10
brightYellow = Color16 11

brightBlue, brightMagenta, brightCyan, brightWhite :: Color
brightBlue = Color16 12
brightMagenta = Color16 13
brightCyan = Color16 14
brightWhite = Color16 15

-- | A concrete colour a terminal understands.
data TermColor
  = TC16 !Word8
  | TC256 !Word8
  | TCRGB !Word8 !Word8 !Word8
  deriving (Eq, Ord, Show)

-- | What the output can display, from least to most capable.
data ColorProfile
  = -- | Not a terminal: no escape sequences at all.
    NoTTY
  | -- | Text attributes (bold, underline, …) but no colours.
    Ascii
  | ANSI16
  | ANSI256
  | TrueColor
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Resolve an adaptive colour for a profile and background, then downsample.
-- 'Nothing' means the profile cannot show colours.
resolveColor :: ColorProfile -> Bool -> Color -> Maybe TermColor
resolveColor p dark = go
  where
    go = \case
      LightDark l d -> go (if dark then d else l)
      Complete tc c256 c16 -> case p of
        TrueColor -> go tc
        ANSI256 -> go c256
        _ -> go c16
      RGB r g b -> downsample p (TCRGB r g b)
      Color256 n -> downsample p (if n < 16 then TC16 n else TC256 n)
      Color16 n -> downsample p (if n < 16 then TC16 n else TC256 n)

-- | Convert a colour to the closest one the profile supports.
downsample :: ColorProfile -> TermColor -> Maybe TermColor
downsample p c = case p of
  NoTTY -> Nothing
  Ascii -> Nothing
  TrueColor -> Just c
  ANSI256 -> Just $ case c of
    TCRGB r g b -> TC256 (rgbTo256 r g b)
    _ -> c
  ANSI16 -> Just $ case c of
    TC16 _ -> c
    _ -> TC16 (nearest16 (termColorRGB c))

-- | The RGB value of a terminal colour, using xterm's default palette for
-- the 16 basic colours.
termColorRGB :: TermColor -> (Word8, Word8, Word8)
termColorRGB = \case
  TCRGB r g b -> (r, g, b)
  TC16 n -> ansi16Palette !! fromIntegral (n `mod` 16)
  TC256 n
    | n < 16 -> ansi16Palette !! fromIntegral n
    | n < 232 ->
        let i = fromIntegral n - 16
         in (cubeLevel (i `div` 36), cubeLevel ((i `div` 6) `mod` 6), cubeLevel (i `mod` 6))
    | otherwise -> let v = fromIntegral (8 + 10 * (fromIntegral n - 232 :: Int)) in (v, v, v)

ansi16Palette :: [(Word8, Word8, Word8)]
ansi16Palette =
  [ (0, 0, 0), (205, 0, 0), (0, 205, 0), (205, 205, 0)
  , (0, 0, 238), (205, 0, 205), (0, 205, 205), (229, 229, 229)
  , (127, 127, 127), (255, 0, 0), (0, 255, 0), (255, 255, 0)
  , (92, 92, 255), (255, 0, 255), (0, 255, 255), (255, 255, 255)
  ]

cubeLevel :: Int -> Word8
cubeLevel i = [0, 95, 135, 175, 215, 255] !! i

-- | Nearest colour in the 6×6×6 cube or the grey ramp (tmux's algorithm).
rgbTo256 :: Word8 -> Word8 -> Word8 -> Word8
rgbTo256 r8 g8 b8
  | cube == target = cubeIx
  | dist cube <= dist grey = cubeIx
  | otherwise = fromIntegral (232 + greyIx)
  where
    target@(r, g, b) = (fromIntegral r8, fromIntegral g8, fromIntegral b8) :: (Int, Int, Int)
    toCube v
      | v < 48 = 0
      | v < 115 = 1
      | otherwise = (v - 35) `div` 40
    (ri, gi, bi) = (toCube r, toCube g, toCube b)
    lvl = fromIntegral . cubeLevel
    cube = (lvl ri, lvl gi, lvl bi)
    cubeIx = fromIntegral (16 + 36 * ri + 6 * gi + bi)
    avg = (r + g + b) `div` 3
    greyIx = if avg > 238 then 23 else max 0 ((avg - 3) `div` 10)
    greyV = 8 + 10 * greyIx
    grey = (greyV, greyV, greyV)
    dist (x, y, z) = sq (x - r) + sq (y - g) + sq (z - b)
    sq v = v * v

nearest16 :: (Word8, Word8, Word8) -> Word8
nearest16 (r, g, b) =
  fst (minimumBy (comparing (dist . snd)) (zip [0 ..] ansi16Palette))
  where
    dist (x, y, z) = sq x r + sq y g + sq z b
    sq :: Word8 -> Word8 -> Int
    sq x y = let d = fromIntegral x - fromIntegral y in d * d

-- Manipulating colours ----------------------------------------------------------

data OkLab = OkLab !Double !Double !Double

-- | Apply a function to every concrete colour inside an adaptive one.
mapConcrete :: (Color -> Color) -> Color -> Color
mapConcrete f = \case
  LightDark l d -> LightDark (mapConcrete f l) (mapConcrete f d)
  Complete tc c256 c16 -> Complete (mapConcrete f tc) (mapConcrete f c256) (mapConcrete f c16)
  c -> f c

mapOkLab :: (OkLab -> OkLab) -> Color -> Color
mapOkLab f = mapConcrete (fromOkLab . f . toOkLab)

-- | Move a colour a fraction (0–1) of the way to black.
darken :: Double -> Color -> Color
darken k = mapOkLab (lerpOkLab (clamp01 k) (OkLab 0 0 0))

-- | Move a colour a fraction (0–1) of the way to white.
lighten :: Double -> Color -> Color
lighten k = mapOkLab (lerpOkLab (clamp01 k) (OkLab 1 0 0))

lerpOkLab :: Double -> OkLab -> OkLab -> OkLab
lerpOkLab k (OkLab l2 a2 b2) (OkLab l1 a1 b1) =
  OkLab (l1 + (l2 - l1) * k) (a1 + (a2 - a1) * k) (b1 + (b2 - b1) * k)

-- | The colour on the opposite side of the colour wheel, with the same
-- lightness.
complementary :: Color -> Color
complementary = mapOkLab (\(OkLab l a b) -> OkLab l (negate a) (negate b))

-- | @mix t c1 c2@ is @c1@ at 0, @c2@ at 1, and in between otherwise.
-- Adaptive colours are mixed variant by variant: for each background and
-- each profile, with the matching variant of the other colour.
mix :: Double -> Color -> Color -> Color
mix t c1 c2
  | adaptive = LightDark (forBackground False) (forBackground True)
  | otherwise = forBackground True
  where
    adaptive = hasLightDark c1 || hasLightDark c2
    forBackground dark
      | hasComplete c1 || hasComplete c2 =
          Complete (at dark TrueColor) (at dark ANSI256) (at dark ANSI16)
      | otherwise = at dark TrueColor
    at dark p = fromOkLab (lerpOkLab (clamp01 t) (toOkLab (variant dark p c2)) (toOkLab (variant dark p c1)))

-- | The concrete colour an adaptive one stands for on a background and
-- profile.
variant :: Bool -> ColorProfile -> Color -> Color
variant dark p = \case
  LightDark l d -> variant dark p (if dark then d else l)
  Complete tc c256 c16 -> variant dark p $ case p of
    TrueColor -> tc
    ANSI256 -> c256
    _ -> c16
  c -> c

hasLightDark, hasComplete :: Color -> Bool
hasLightDark = \case
  LightDark _ _ -> True
  Complete a b c -> any hasLightDark [a, b, c]
  _ -> False
hasComplete = \case
  Complete _ _ _ -> True
  LightDark a b -> any hasComplete [a, b]
  _ -> False

-- | @blend n stops@ is a gradient of @n@ colours going evenly through the
-- stops, starting at the first and ending at the last.
blend :: Int -> [Color] -> [Color]
blend n stops = case stops of
  [] -> []
  [c] -> replicate n c
  _ | n <= 0 -> []
  _ | n == 1 -> take 1 stops
  _ -> [at (fromIntegral i / fromIntegral (n - 1)) | i <- [0 .. n - 1]]
  where
    segments = fromIntegral (length stops - 1) :: Double
    at t =
      let x = t * segments
          i = min (length stops - 2) (floor x)
       in mix (x - fromIntegral i) (stops !! i) (stops !! (i + 1))

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

-- | Adaptive colours are taken in their true-colour, dark-background form.
toOkLab :: Color -> OkLab
toOkLab c = case c of
  LightDark _ d -> toOkLab d
  Complete tc _ _ -> toOkLab tc
  RGB r g b -> fromRGB r g b
  Color256 n -> let (r, g, b) = termColorRGB (TC256 n) in fromRGB r g b
  Color16 n -> let (r, g, b) = termColorRGB (TC16 n) in fromRGB r g b
  where
    fromRGB r g b =
      let lr = linear r
          lg = linear g
          lb = linear b
          l = cbrt (0.4122214708 * lr + 0.5363325363 * lg + 0.0514459929 * lb)
          m = cbrt (0.2119034982 * lr + 0.6806995451 * lg + 0.1073969566 * lb)
          s = cbrt (0.0883024619 * lr + 0.2817188376 * lg + 0.6299787005 * lb)
       in OkLab
            (0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s)
            (1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s)
            (0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s)
    linear v =
      let x = fromIntegral v / 255 :: Double
       in if x <= 0.04045 then x / 12.92 else ((x + 0.055) / 1.055) ** 2.4
    cbrt x = signum x * abs x ** (1 / 3)

fromOkLab :: OkLab -> Color
fromOkLab (OkLab ll a b) =
  let l = cube (ll + 0.3963377774 * a + 0.2158037573 * b)
      m = cube (ll - 0.1055613458 * a - 0.0638541728 * b)
      s = cube (ll - 0.0894841775 * a - 1.2914855480 * b)
   in RGB
        (gamma (4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s))
        (gamma (-1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s))
        (gamma (-0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s))
  where
    cube x = x * x * x
    gamma x =
      let v = if x <= 0.0031308 then 12.92 * x else 1.055 * x ** (1 / 2.4) - 0.055
       in round (255 * clamp01 v)

-- | A two-dimensional gradient, as rows of colours: the gradient runs
-- through the stops along the direction given by an angle in degrees,
-- anticlockwise as in lipgloss (0 is left to right, 90 bottom to top).
blend2D :: Int -> Int -> Double -> [Color] -> [[Color]]
blend2D w0 h0 angle stops = case stops of
  [] -> []
  [c] -> replicate h (replicate w c)
  _ -> [[at x y | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
  where
    w = max 1 w0
    h = max 1 h0
    gradient = blend (max w h) stops
    -- Without this, right angles pick up rounding noise (cos 270° is about
    -- -1.8e-16) that can tip cells on a boundary into the wrong colour.
    snap v = if abs v < 1e-9 then 0 else v
    gradientLen = fromIntegral (length gradient - 1) :: Double
    rad = angle * pi / 180
    centreX = fromIntegral (w - 1) / 2
    centreY = fromIntegral (h - 1) / 2
    diagonal = sqrt (fromIntegral (w * w + h * h))
    at x y =
      let dx = fromIntegral x - centreX
          dy = fromIntegral y - centreY
          along = dx * snap (cos rad) - dy * snap (sin rad)
          pos = clamp01 ((along + diagonal / 2) / diagonal)
       in gradient !! min (length gradient - 1) (floor (pos * gradientLen))
