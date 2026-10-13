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

    -- * Enumerators
  , Enumerator (..)
  , enumerator
  , enumeratorBy
  , treeEnumerator
  , roundedTreeEnumerator
  , bullet
  , dash
  , asterisk
  , arabic
  , alphabet
  , roman
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Forest, Tree (..))
import Lipstick.Internal.Block
import Lipstick.Internal.Types (left, right)
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
  , outItemStyle :: ItemPos -> Mod Style
  , outEnumStyle :: ItemPos -> Mod Style
  , outRootStyle :: Mod Style
  }

treeDefaults, listDefaults :: Outline
treeDefaults = Outline (const treeEnumerator) (const mempty) (const mempty) mempty
listDefaults = treeDefaults {outEnumerator = const bullet}

-- | A tree: the root, then its children.
tree :: Mod Outline -> Tree Block -> Block
tree m (Node root kids) =
  let o = appMod m treeDefaults
   in fromCellLines (blockCells (frame (outRootStyle o) root) ++ items o 0 kids)

-- | Trees without a common root, with tree branches.
forest :: Mod Outline -> Forest Block -> Block
forest m = fromCellLines . items (appMod m treeDefaults) 0

-- | A list of items, with bullets.
list :: Mod Outline -> [Block] -> Block
list m = nestedList m . map (`Node` [])

-- | A list whose items can have nested items, with bullets.
nestedList :: Mod Outline -> Forest Block -> Block
nestedList m = fromCellLines . items (appMod m listDefaults) 0

items :: Outline -> Int -> Forest Block -> [[Cell]]
items o depth kids = concat (zipWith item [0 ..] kids)
  where
    n = length kids
    e = outEnumerator o depth
    pos i = ItemPos depth i n
    styledLine i t = concat (take 1 (blockCells (render (outEnumStyle o (pos i)) (plain t))))
    markers = [styledLine i (enumMarker e n i) | i <- [0 .. n - 1]]
    markerW = maximum (0 : map cellsWidth markers)

    item i (Node b children) =
      let marker = alignCells mempty right markerW (markers !! i)
          indentText = fromMaybe (T.replicate markerW " ") (enumIndent e n i)
          indent = alignCells mempty left markerW (styledLine i indentText)
          body = case blockCells (frame (outItemStyle o (pos i)) b) of
            [] -> [[]]
            ls -> ls
       in zipWith (++) (marker : repeat indent) body
            ++ map (indent ++) (items o (depth + 1) children)

-- Styling ---------------------------------------------------------------------

-- | Style the root of a tree.
rootStyle :: Mod Style -> Mod Outline
rootStyle m = Mod (\o -> o {outRootStyle = outRootStyle o <> m})

-- | Style every item.
itemStyle :: Mod Style -> Mod Outline
itemStyle m = itemStyleWith (const m)

-- | Style items by position. Successive item styles add up.
itemStyleWith :: (ItemPos -> Mod Style) -> Mod Outline
itemStyleWith f = Mod (\o -> o {outItemStyle = \p -> outItemStyle o p <> f p})

-- | Style every enumerator (and the indentation below it).
enumeratorStyle :: Mod Style -> Mod Outline
enumeratorStyle m = enumeratorStyleWith (const m)

-- | Style enumerators by position. Successive enumerator styles add up.
enumeratorStyleWith :: (ItemPos -> Mod Style) -> Mod Outline
enumeratorStyleWith f = Mod (\o -> o {outEnumStyle = \p -> outEnumStyle o p <> f p})

-- Enumerators -----------------------------------------------------------------

-- | Use this enumerator at every depth.
enumerator :: Enumerator -> Mod Outline
enumerator e = enumeratorBy (const e)

-- | Choose the enumerator by depth (0 for the top level).
enumeratorBy :: (Int -> Enumerator) -> Mod Outline
enumeratorBy f = Mod (\o -> o {outEnumerator = f})

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
