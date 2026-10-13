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
