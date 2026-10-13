-- | Placing a block in a larger area filled with whitespace.
module Lipstick.Place
  ( Place
  , place
  , whitespaceChars
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lipstick.Internal.Block
import Lipstick.Internal.Types
import Lipstick.Mod

-- | The surrounding area: its size, where the block sits in it, and the
-- attributes and characters of the whitespace.
data Place = Place
  { placeAttrs :: !Attrs
  , placeSize :: !Size
  , placeAlign :: !Align
  , placeChars :: !Text
  }
  deriving (Eq, Show)

instance HasAttrs Place where
  overAttrs f p = p {placeAttrs = f (placeAttrs p)}

instance HasSize Place where
  overSize f p = p {placeSize = f (placeSize p)}

instance HasAlign Place where
  overAlign f p = p {placeAlign = f (placeAlign p)}

defPlace :: Place
defPlace = Place mempty defSize defAlign (T.singleton ' ')

-- | Characters repeated to fill the whitespace (default a space).
whitespaceChars :: Text -> Mod Place
whitespaceChars t = Mod (\p -> p {placeChars = t})

-- | Place a block in an area of the given 'width' and 'height', at the
-- position set by 'align' and 'valign'. A block larger than the area is
-- left as it is in that direction.
--
-- > place (width 80 <> height 5 <> align center <> valign center) b
place :: Mod Place -> Block -> Block
place m b =
  let Place a (Size w h maxW maxH) (Align ah av) chars = appMod m defPlace
      outerW = max (blockWidth b) (fromMaybe 0 w)
      outerH = max (blockHeight b) (fromMaybe 0 h)
      ws n = fillCells a chars n
      (left', right') = splitExtra ah (outerW - blockWidth b)
      (above, below) = splitExtra av (outerH - blockHeight b)
      body = [ws left' ++ l ++ ws right' | l <- blockCells b]
      ls = replicate above (ws outerW) ++ body ++ replicate below (ws outerW)
      croppedW = maybe ls (\n -> map (cropCells n) ls) maxW
   in fromCellLines (maybe croppedW (`take` croppedW) maxH)
