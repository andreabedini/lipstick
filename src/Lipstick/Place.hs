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
import Lipstick.Internal.Layout (Assign (..), Block (..), Request (..), cropLayoutWidth)
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

-- | Place a block in an area, at the position set by 'align' and
-- 'valign'. The area is as wide as the space it is laid out in, or 'width'
-- columns if set, and 'height' rows if set. A block larger than the area
-- is left as it is in that direction.
--
-- > place (align center <> valign center <> height 5) b
place :: Mod Place -> Block -> Block
place m b = Block req run
  where
    (Place (Size wSet hSet weight stretch) (Align ah av) chars, props) = runMod m defPlace
    r = blockRequest b
    capW x = maybe x (min x . max 0) (propMaxWidth props)
    capH x = maybe x (min x . max 0) (propMaxHeight props)
    req =
      Request
        { reqMin = capW (maybe (reqMin r) (max (reqMin r)) wSet)
        , reqMax = capW (maybe (reqMax r) (max (reqMax r)) wSet)
        , -- Without a width, the area is all the space there is.
          reqGrow = maybe (max 1 weight) (const weight) wSet
        , reqLimit = max 0 <$> propMaxWidth props
        , reqStretch = stretch
        }
    run (Assign w h) = (fitWidth w (withProps props ls), natural)
      where
        outerW = cropLayoutWidth (propMaxWidth props) (reqGrow req) (maybe (reqMax r) (max (reqMax r)) wSet) w
        (g, n) = blockLayout b (Assign (if reqGrow r > 0 then outerW else min outerW (reqMax r)) outerH)
        natural = capH (max (fromMaybe 0 hSet) n)
        -- From the child's natural height, not its grid's: the grid is
        -- laid out with this height, for a child that stretches.
        outerH = maximum [n, fromMaybe 0 hSet, if stretch then h else 0]
        ws k = fillCells mempty chars k
        (left', right') = splitExtra ah (outerW - gridWidth g)
        (above, below) = splitExtra av (outerH - gridHeight g)
        body = [ws left' ++ l ++ ws right' | l <- gridCells g]
        ls = replicate above (ws outerW) ++ body ++ replicate below (ws outerW)
