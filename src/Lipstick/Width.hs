-- | Display width of text in a terminal.
--
-- Text is split into extended grapheme clusters following UAX #29 (rules
-- GB3–GB13, including GB9c for Indic conjuncts). A cluster is as wide as
-- its first character with a non-zero width: two columns for East Asian
-- Wide and Fullwidth characters, none for controls, combining marks and
-- Hangul vowels and final consonants, one otherwise. An emoji followed by
-- VS16 is two columns wide and by VS15 one; a pair of regional indicators
-- (a flag) is two.
--
-- The tables are generated from the Unicode Character Database by
-- @scripts/GenUnicodeTable.hs@; see 'unicodeVersion'.
module Lipstick.Width
  ( charWidth
  , textWidth
  , clusters
  , unicodeVersion
  ) where

import Lipstick.Internal.Width
