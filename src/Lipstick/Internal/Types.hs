-- | Types shared by every module. Not part of the stable API.
module Lipstick.Internal.Types
  ( -- * Text attributes
    Attrs (..)
  , Underline (..)
  , Flag (..)
  , setFlag
  , getFlag
  , fillAttrs

    -- * Positions
  , Position (..)
  , position
  , left
  , center
  , right
  , top
  , bottom
  , splitExtra
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import Lipstick.Color (Color)

data Underline
  = NoUnderline
  | SingleUnderline
  | DoubleUnderline
  | CurlyUnderline
  | DottedUnderline
  | DashedUnderline
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Boolean text attributes.
data Flag = Bold | Faint | Italic | Blink | Reverse | Strikethrough
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Partial text attributes. 'Nothing' means "not set here": when attributes
-- are layered with '<>', set fields on the right win and unset ones let the
-- left show through.
data Attrs = Attrs
  { attrFg :: !(Maybe Color)
  , attrBg :: !(Maybe Color)
  , attrUnderlineColor :: !(Maybe Color)
  , attrBold :: !(Maybe Bool)
  , attrFaint :: !(Maybe Bool)
  , attrItalic :: !(Maybe Bool)
  , attrBlink :: !(Maybe Bool)
  , attrReverse :: !(Maybe Bool)
  , attrStrikethrough :: !(Maybe Bool)
  , attrUnderline :: !(Maybe Underline)
  , attrLink :: !(Maybe Text)
  }
  deriving (Eq, Show)

instance Semigroup Attrs where
  a <> b =
    Attrs
      { attrFg = attrFg b <|> attrFg a
      , attrBg = attrBg b <|> attrBg a
      , attrUnderlineColor = attrUnderlineColor b <|> attrUnderlineColor a
      , attrBold = attrBold b <|> attrBold a
      , attrFaint = attrFaint b <|> attrFaint a
      , attrItalic = attrItalic b <|> attrItalic a
      , attrBlink = attrBlink b <|> attrBlink a
      , attrReverse = attrReverse b <|> attrReverse a
      , attrStrikethrough = attrStrikethrough b <|> attrStrikethrough a
      , attrUnderline = attrUnderline b <|> attrUnderline a
      , attrLink = attrLink b <|> attrLink a
      }

instance Monoid Attrs where
  mempty = Attrs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

setFlag :: Flag -> Maybe Bool -> Attrs -> Attrs
setFlag f v a = case f of
  Bold -> a {attrBold = v}
  Faint -> a {attrFaint = v}
  Italic -> a {attrItalic = v}
  Blink -> a {attrBlink = v}
  Reverse -> a {attrReverse = v}
  Strikethrough -> a {attrStrikethrough = v}

getFlag :: Flag -> Attrs -> Maybe Bool
getFlag f = case f of
  Bold -> attrBold
  Faint -> attrFaint
  Italic -> attrItalic
  Blink -> attrBlink
  Reverse -> attrReverse
  Strikethrough -> attrStrikethrough

-- | The attributes used for whitespace a style adds around content
-- (alignment, padding): only the background and reverse video carry over.
fillAttrs :: Attrs -> Attrs
fillAttrs a = mempty {attrBg = attrBg a, attrReverse = attrReverse a}

-- | A position along an axis: 0 is left/top, 1 is right/bottom.
newtype Position = Position Double
  deriving (Eq, Ord, Show)

-- | A position, clamped to [0, 1].
position :: Double -> Position
position = Position . max 0 . min 1

left, center, right, top, bottom :: Position
left = Position 0
center = Position 0.5
right = Position 1
top = Position 0
bottom = Position 1

-- | Split @extra@ columns (or rows) of space into the amounts before and
-- after content placed at a position.
splitExtra :: Position -> Int -> (Int, Int)
splitExtra (Position p) extra =
  let e = max 0 extra
      before = floor (fromIntegral e * max 0 (min 1 p))
   in (before, e - before)
