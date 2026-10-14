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
import Lipstick.Internal.Layout (Block, fromGrid, layout)
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

-- | 'width' pads every line to at least that many columns, with the
-- background of its item's style; 'height' pads with blank lines.
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
      rootLines = [(fillOf (outRootStyle o), l) | l <- gridCells (layout Nothing (frame (outRootStyle o) root))]
   in finish o props (rootLines ++ items o 0 kids)

-- | Trees without a common root, with tree branches.
forest :: Mod Outline -> Forest Block -> Block
forest m kids = let (o, props) = runMod m treeDefaults in finish o props (items o 0 kids)

-- | A list of items, with bullets.
list :: Mod Outline -> [Block] -> Block
list m = nestedList m . map (`Node` [])

-- | A list whose items can have nested items, with bullets.
nestedList :: Mod Outline -> Forest Block -> Block
nestedList m kids = let (o, props) = runMod m listDefaults in finish o props (items o 0 kids)

-- | Pad lines to the width (each with its own fill attributes) and the
-- height, then apply the common properties.
finish :: Outline -> Props -> [(Attrs, [Cell])] -> Block
finish o props ls =
  let Size w h _ _ = outSize o
      wide = maximum (fromMaybe 0 w : map (cellsWidth . snd) ls)
      padded = [alignCells a left wide l | (a, l) <- ls]
      tall = padded ++ replicate (fromMaybe 0 h - length padded) (blankCells mempty wide)
   in fromGrid (withProps props tall)

-- | The background (and reverse video) of a style, for padding.
fillOf :: Mod Style -> Attrs
fillOf = fillAttrs . propAttrs . propsOf

-- | The lines of a level of items, each with the fill attributes for
-- padding it.
items :: Outline -> Int -> Forest Block -> [(Attrs, [Cell])]
items o depth kids0 = concat (zipWith item [0 ..] kids)
  where
    kids =
      if depth == 0
        then let (start, end) = outOffset o in take (length kids0 - start - end) (drop start kids0)
        else kids0
    n = length kids
    e = outEnumerator o depth
    pos i = ItemPos depth i n
    styledLine m t = concat (take 1 (gridCells (layout Nothing (render m (plain t)))))
    enumStyle i = outEnumStyle o (pos i)
    indentStyle i = maybe (enumStyle i) ($ pos i) (outIndentStyle o)
    markers = [styledLine (enumStyle i) (enumMarker e n i) | i <- [0 .. n - 1]]
    markerW = maximum (0 : map cellsWidth markers)

    item i (Node b children) =
      let -- Alignment space keeps the enumerator's background, as lipgloss.
          marker = alignCells (fillOf (enumStyle i)) right markerW (markers !! i)
          indentText =
            fromMaybe (T.replicate markerW " ") (fmap (\f -> f n i) (outIndenter o) <|> enumIndent e n i)
          indent = alignCells (fillOf (indentStyle i)) left markerW (styledLine (indentStyle i) indentText)
          itemFill = fillOf (outItemStyle o (pos i))
          body = case gridCells (layout Nothing (frame (outItemStyle o (pos i)) b)) of
            [] -> [[]]
            ls -> ls
       in [(itemFill, l) | l <- zipWith (++) (marker : repeat indent) body]
            ++ [(a, indent ++ l) | (a, l) <- items o (depth + 1) children]

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
