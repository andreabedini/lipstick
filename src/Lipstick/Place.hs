-- | Placing a block in a larger area filled with whitespace.
--
-- == Examples
--
-- >>> let hi = render (border roundedBorder) "hi"
-- >>> putBlock (place (width 16 <> height 5 <> align center <> valign center <> whitespaceChars "·") hi)
-- ················
-- ······╭──╮······
-- ······│hi│······
-- ······╰──╯······
-- ················
-- >>> putBlock (place (width 16 <> height 3 <> align right <> valign bottom <> whitespaceChars "·") hi)
-- ············╭──╮
-- ············│hi│
-- ············╰──╯
--
-- Without a 'Lipstick.Mod.width', the area is as wide as the space it is
-- laid out in:
--
-- >>> putBlock (fromGrid (layout (Just 20) (place (align center <> whitespaceChars "-") " centred ")))
-- ----- centred ------
module Lipstick.Place
  ( Place
  , place
  , whitespaceChars
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lipstick.Internal.Grid
import Lipstick.Internal.Layout (Assign (..), Block (..), Measured (..), Request (..), cropLayoutWidth, measureIn)
import Lipstick.Internal.Types
import Lipstick.Internal.Mod

-- $setup
-- >>> import Lipstick

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
-- columns if set and there is room for them, and at least 'height' rows
-- if set. A block wider than the area is laid out in its width, so its
-- text wraps; a block taller than the area makes it taller.
--
-- >>> putBlock (place (width 7 <> height 3 <> align center <> valign center <> whitespaceChars ".") "b")
-- .......
-- ...b...
-- .......
place :: Mod Place -> Block -> Block
place m b = Block $ \ctx -> placeNode m (measureIn ctx b)

placeNode :: Mod Place -> Measured -> Measured
placeNode m nb = Measured req run
  where
    (Place (Size wSet hSet weight stretch) (Align ah av) chars, props) = runMod m defPlace
    r = blockRequest nb
    capW x = maybe x (min x . max 0) (propMaxWidth props)
    capH x = maybe x (min x . max 0) (propMaxHeight props)
    req =
      Request
        { reqMin = capW (fromMaybe (reqMin r) wSet)
        , reqMax = capW (fromMaybe (reqMax r) wSet)
        , -- Without a width, the area is all the space there is.
          reqGrow = maybe (max 1 weight) (const weight) wSet
        , reqLimit = max 0 <$> propMaxWidth props
        , reqStretch = stretch
        , reqFill = False
        }
    run (Assign w h) = (fitWidth w (withProps props ls), natural)
      where
        outerW = cropLayoutWidth (propMaxWidth props) (reqGrow req) (fromMaybe (reqMax r) wSet) w
        (g, n) = blockLayout nb (Assign (if reqGrow r > 0 then outerW else min outerW (reqMax r)) outerH)
        natural = capH (max (fromMaybe 0 hSet) n)
        -- From the child's natural height, not its grid's: the grid is
        -- laid out with this height, for a child that stretches.
        outerH = maximum [n, fromMaybe 0 hSet, if stretch then h else 0]
        ws k = fillCells mempty chars k
        (left', right') = splitExtra ah (outerW - gridWidth g)
        (above, below) = splitExtra av (outerH - gridHeight g)
        body = [ws left' ++ l ++ ws right' | l <- gridCells g]
        ls = replicate above (ws outerW) ++ body ++ replicate below (ws outerW)
