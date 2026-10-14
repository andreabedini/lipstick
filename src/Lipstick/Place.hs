-- | Placing a block in a larger area filled with whitespace.
module Lipstick.Place
  ( Place
  , place
  , whitespaceChars
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lipstick.Internal.Grid
import Lipstick.Internal.Types
import Lipstick.Mod

-- | The surrounding area: its size, where the block sits in it, and the
-- characters of the whitespace. Its text attributes colour the whitespace
-- and are the base for the placed block.
data Place = Place
  { placeSize :: !Size
  , placeAlign :: !Align
  , placeChars :: !Text
  }
  deriving (Eq, Show)

instance HasSize Place where
  overSize f p = p {placeSize = f (placeSize p)}

instance HasAlign Place where
  overAlign f p = p {placeAlign = f (placeAlign p)}

defPlace :: Place
defPlace = Place defSize defAlign (T.singleton ' ')

-- | Characters repeated to fill the whitespace (default a space).
whitespaceChars :: Text -> Mod Place
whitespaceChars t = fieldMod (\p -> p {placeChars = t})

-- | Place a block in an area of the given 'width' and 'height', at the
-- position set by 'align' and 'valign'. A block larger than the area is
-- left as it is in that direction.
--
-- > place (width 80 <> height 5 <> align center <> valign center) b
place :: Mod Place -> Grid -> Grid
place m b =
  let (Place (Size w h) (Align ah av) chars, props) = runMod m defPlace
      outerW = max (gridWidth b) (fromMaybe 0 w)
      outerH = max (gridHeight b) (fromMaybe 0 h)
      ws n = fillCells mempty chars n
      (left', right') = splitExtra ah (outerW - gridWidth b)
      (above, below) = splitExtra av (outerH - gridHeight b)
      body = [ws left' ++ l ++ ws right' | l <- gridCells b]
      ls = replicate above (ws outerW) ++ body ++ replicate below (ws outerW)
   in withProps props ls
