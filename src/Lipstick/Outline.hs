{-# LANGUAGE OverloadedStrings #-}

-- | Lists and trees.
--
-- Both are outlines: items with an enumerator (a bullet, a number, a tree
-- branch) and nested items indented below them. Trees have a root and use
-- tree branches by default; lists have no root and use bullets.
--
-- > tree (enumerator roundedTreeEnumerator <> enumeratorStyle (fg grey))
-- >   (Node "." [Node "macOS" [], Node "Linux" [Node "NixOS" [], Node "Arch" []], Node "BSD" []])
-- >
-- > list (enumerator arabic) ["Glossier", "Claire's Boutique", "Nyx"]
--
-- Items are blocks, so anything can go in an outline, tables included.
module Lipstick.Outline
  ( Outline
  , tree
  , forest
  , list
  , nestedList

    -- * Styling
  , ItemPos (..)
  , rootStyle
  , itemStyle
  , itemStyleWith
  , enumeratorStyle
  , enumeratorStyleWith
  , indenterStyle
  , indenterStyleWith
  , itemOffset

    -- * Enumerators
  , Enumerator (..)
  , enumerator
  , enumeratorBy
  , indenter
  , treeEnumerator
  , roundedTreeEnumerator
  , bullet
  , dash
  , asterisk
  , arabic
  , alphabet
  , roman
  ) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Forest, Tree (..))
import Lipstick.Internal.Grid
import Lipstick.Internal.Layout (Assign (..), Block (..), Measured (..), Request (..), TextStyle, cropLayoutWidth, layoutIn, measureIn, takenWidth)
import Lipstick.Internal.Types (Attrs, fillAttrs, left, right)
import Lipstick.Mod
import Lipstick.Style (Style, frame, render)

-- | How items are marked.
data Enumerator = Enumerator
  { enumMarker :: Int -> Int -> Text
  -- ^ the marker for the item at an index, given the number of siblings
  , enumIndent :: Int -> Int -> Maybe Text
  -- ^ what goes before the item's further lines and its children;
  -- 'Nothing' for spaces as wide as the marker
  }

-- | Where an item is: its depth (0 for the top level), its index among its
-- siblings, and how many siblings there are.
data ItemPos = ItemPos
  { itemDepth :: !Int
  , itemIndex :: !Int
  , itemSiblings :: !Int
  }
  deriving (Eq, Show)

data Outline = Outline
  { outEnumerator :: Int -> Enumerator
  , outIndenter :: Maybe (Int -> Int -> Text)
  , outItemStyle :: ItemPos -> Mod Style
  , outEnumStyle :: ItemPos -> Mod Style
  , outIndentStyle :: Maybe (ItemPos -> Mod Style)
  , outRootStyle :: Mod Style
  , outSize :: Size
  , outOffset :: (Int, Int)
  }

-- | Items are laid out in the width left after their indentation and
-- enumerator, so their text wraps to fit. 'width' pads every line to at
-- least that many columns, with the background of its item's style, and
-- 'fillWidth' to the width the outline is given; 'height' pads with blank
-- lines.
instance HasSize Outline where
  overSize f o = o {outSize = f (outSize o)}

treeDefaults, listDefaults :: Outline
treeDefaults =
  Outline
    { outEnumerator = const treeEnumerator
    , outIndenter = Nothing
    , outItemStyle = const mempty
    , outEnumStyle = const mempty
    , outIndentStyle = Nothing
    , outRootStyle = mempty
    , outSize = defSize
    , outOffset = (0, 0)
    }
listDefaults = treeDefaults {outEnumerator = const bullet}

-- | A tree: the root, then its children.
tree :: Mod Outline -> Tree Block -> Block
tree m (Node root kids) =
  let (o, props) = runMod m treeDefaults
   in outline o props $ \ctx -> Entry [] [] [] (fillOf (outRootStyle o)) (frame (outRootStyle o) root) : items ctx o 0 [] kids

-- | Trees without a common root, with tree branches.
forest :: Mod Outline -> Forest Block -> Block
forest m kids = let (o, props) = runMod m treeDefaults in outline o props (\ctx -> items ctx o 0 [] kids)

-- | A list of items, with bullets.
list :: Mod Outline -> [Block] -> Block
list m = nestedList m . map (`Node` [])

-- | A list whose items can have nested items, with bullets.
nestedList :: Mod Outline -> Forest Block -> Block
nestedList m kids = let (o, props) = runMod m listDefaults in outline o props (\ctx -> items ctx o 0 [] kids)

-- | An item of an outline, flattened: what goes before its lines, and the
-- item itself, laid out in the width that is left.
data Entry = Entry
  { entryOuter :: [Cell]
  -- ^ the indentation of the levels above, before every line
  , entryMarker :: [Cell]
  -- ^ before the first line: the enumerator
  , entryIndent :: [Cell]
  -- ^ before the other lines
  , entryFill :: Attrs
  -- ^ for padding the item's lines
  , entryBlock :: Block
  }

-- | The columns before an entry's lines.
prefixWidth :: Entry -> Int
prefixWidth e = cellsWidth (entryOuter e) + max (cellsWidth (entryMarker e)) (cellsWidth (entryIndent e))

-- | Entries laid out in the width they are given, padded to it (each line
-- with its item's fill attributes) and to the height, with the common
-- properties on top.
outline :: Outline -> Props -> (TextStyle -> [Entry]) -> Block
outline o props entriesIn = Block $ \ctx -> outlineNode o props [(e, measureIn ctx (entryBlock e)) | e <- entriesIn ctx]

outlineNode :: Outline -> Props -> [(Entry, Measured)] -> Measured
outlineNode o props entries = Measured req run
  where
    Size wSet hSet weight _ = outSize o
    natural = maximum (fromMaybe 0 wSet : [prefixWidth e + reqMax (blockRequest n) | (e, n) <- entries])
    smallest = maximum (fromMaybe 0 wSet : [prefixWidth e + reqMin (blockRequest n) | (e, n) <- entries])
    capW x = maybe x (min x . max 0) (propMaxWidth props)
    capH x = maybe x (min x . max 0) (propMaxHeight props)
    req = Request (capW smallest) (capW natural) weight (max 0 <$> propMaxWidth props) False False
    run (Assign w _) = (fitWidth w (withProps props tall), capH (max (fromMaybe 0 hSet) (sum (map snd laid))))
      where
        wide = cropLayoutWidth (propMaxWidth props) weight natural w
        laid = map entryLines entries
        padded = [alignCells a left wide l | (ls, _) <- laid, (a, l) <- ls]
        tall = padded ++ replicate (fromMaybe 0 hSet - length padded) (blankCells mempty wide)
        entryLines (e, node) =
          let (g, n) = blockLayout node (Assign (takenWidth (blockRequest node) (wide - prefixWidth e)) 0)
              body = case gridCells g of
                [] -> [[]]
                ls -> ls
           in ( [(entryFill e, entryOuter e ++ p ++ l) | (p, l) <- zip (entryMarker e : repeat (entryIndent e)) body]
              , max 1 n
              )

-- | The background (and reverse video) of a style, for padding.
fillOf :: Mod Style -> Attrs
fillOf = fillAttrs . propAttrs . propsOf

-- | The entries of a level of items, below the indentation of the levels
-- above.
items :: TextStyle -> Outline -> Int -> [Cell] -> Forest Block -> [Entry]
items ctx o depth outer kids0 = concat (zipWith3 item [0 ..] markers kids)
  where
    kids =
      if depth == 0
        then let (start, end) = outOffset o in take (length kids0 - start - end) (drop start kids0)
        else kids0
    n = length kids
    e = outEnumerator o depth
    pos i = ItemPos depth i n
    styledLine m t = concat (take 1 (gridCells (layoutIn ctx Nothing (render m (plain t)))))
    enumStyle i = outEnumStyle o (pos i)
    indentStyle i = maybe (enumStyle i) ($ pos i) (outIndentStyle o)
    markers = [styledLine (enumStyle i) (enumMarker e n i) | i <- [0 .. n - 1]]
    markerW = maximum (0 : map cellsWidth markers)

    item i mk (Node b children) =
      let -- Alignment space keeps the enumerator's background, as lipgloss.
          marker = alignCells (fillOf (enumStyle i)) right markerW mk
          indentText =
            fromMaybe (T.replicate markerW " ") (fmap (\f -> f n i) (outIndenter o) <|> enumIndent e n i)
          indent = alignCells (fillOf (indentStyle i)) left markerW (styledLine (indentStyle i) indentText)
          itemStyle' = outItemStyle o (pos i)
       in Entry outer marker indent (fillOf itemStyle') (frame itemStyle' b)
            : items ctx o (depth + 1) (outer ++ indent) children

-- Styling ---------------------------------------------------------------------

-- | Style the root of a tree.
rootStyle :: Mod Style -> Mod Outline
rootStyle m = fieldMod (\o -> o {outRootStyle = outRootStyle o <> m})

-- | Style every item.
itemStyle :: Mod Style -> Mod Outline
itemStyle m = itemStyleWith (const m)

-- | Style items by position. Successive item styles add up.
itemStyleWith :: (ItemPos -> Mod Style) -> Mod Outline
itemStyleWith f = fieldMod (\o -> o {outItemStyle = \p -> outItemStyle o p <> f p})

-- | Style every enumerator (and the indentation below it).
enumeratorStyle :: Mod Style -> Mod Outline
enumeratorStyle m = enumeratorStyleWith (const m)

-- | Style the indentation before an item's further lines and its
-- children. Until an indenter style is set, the enumerator style is used.
indenterStyle :: Mod Style -> Mod Outline
indenterStyle m = indenterStyleWith (const m)

-- | Style the indentation by position. Successive indenter styles add up.
indenterStyleWith :: (ItemPos -> Mod Style) -> Mod Outline
indenterStyleWith f = fieldMod (\o -> o {outIndentStyle = Just (\p -> maybe mempty ($ p) (outIndentStyle o) <> f p)})

-- | Show only part of the top-level items: skip @start@ items at the
-- beginning and @end@ at the end. Enumerators count from the first shown.
itemOffset :: Int -> Int -> Mod Outline
itemOffset start end = fieldMod (\o -> o {outOffset = (max 0 start, max 0 end)})

-- | Style enumerators by position. Successive enumerator styles add up.
enumeratorStyleWith :: (ItemPos -> Mod Style) -> Mod Outline
enumeratorStyleWith f = fieldMod (\o -> o {outEnumStyle = \p -> outEnumStyle o p <> f p})

-- Enumerators -----------------------------------------------------------------

-- | Use this enumerator at every depth.
enumerator :: Enumerator -> Mod Outline
enumerator e = enumeratorBy (const e)

-- | What goes before an item's further lines and its children, given the
-- number of siblings and the item's index, instead of the enumerator's
-- own indentation.
indenter :: (Int -> Int -> Text) -> Mod Outline
indenter f = fieldMod (\o -> o {outIndenter = Just f})

-- | Choose the enumerator by depth (0 for the top level).
enumeratorBy :: (Int -> Enumerator) -> Mod Outline
enumeratorBy f = fieldMod (\o -> o {outEnumerator = f})

-- | @├── @ and @└── @, with @│@ continuing the branch.
treeEnumerator :: Enumerator
treeEnumerator = branches "└── "

-- | Like 'treeEnumerator' with a rounded last branch: @╰── @.
roundedTreeEnumerator :: Enumerator
roundedTreeEnumerator = branches "╰── "

branches :: Text -> Enumerator
branches lastBranch =
  Enumerator
    { enumMarker = \n i -> if i == n - 1 then lastBranch else "├── "
    , enumIndent = \n i -> Just (if i == n - 1 then "    " else "│   ")
    }

constant :: Text -> Enumerator
constant t = Enumerator (\_ _ -> t) (\_ _ -> Nothing)

-- | @• @
bullet :: Enumerator
bullet = constant "• "

-- | @- @
dash :: Enumerator
dash = constant "- "

-- | @* @
asterisk :: Enumerator
asterisk = constant "* "

numbered :: (Int -> Text) -> Enumerator
numbered f = Enumerator (\_ i -> f (i + 1) <> ". ") (\_ _ -> Nothing)

-- | @1. @, @2. @, … (numbers are right-aligned)
arabic :: Enumerator
arabic = numbered (T.pack . show)

-- | @A. @, @B. @, … @Z. @, @AA. @, …
alphabet :: Enumerator
alphabet = numbered letters
  where
    letters k
      | k <= 0 = ""
      | otherwise = let (q, r) = (k - 1) `divMod` 26 in letters q <> T.singleton (toEnum (fromEnum 'A' + r))

-- | @I. @, @II. @, @III. @, @IV. @, …
roman :: Enumerator
roman = numbered (T.pack . go)
  where
    go 0 = ""
    go k = case dropWhile ((> k) . fst) numerals of
      (v, s) : _ -> s ++ go (k - v)
      [] -> ""
    numerals =
      [ (1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC")
      , (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")
      ]
