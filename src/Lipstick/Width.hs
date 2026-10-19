-- | Display width of text in a terminal.
--
-- Text is split into extended grapheme clusters following UAX #29 (rules
-- GB3–GB13, including GB9c for Indic conjuncts). A cluster is as wide as
-- its first character with a non-zero width: two columns for East Asian
-- Wide and Fullwidth characters, none for controls, combining marks and
-- Hangul vowels and final consonants, one otherwise. An emoji followed by
-- VS16 or a skin tone modifier is two columns wide, as is a skin tone
-- modifier on its own; VS15 does not change the width. A pair of regional
-- indicators (a flag) is two.
--
-- The tables are generated from the Unicode Character Database by
-- @scripts/GenUnicodeTable.hs@; see 'unicodeVersion'.
--
-- == Examples
--
-- >>> textWidth "hello"
-- 5
-- >>> textWidth "こんにちは"
-- 10
-- >>> textWidth "🇦🇺"
-- 2
-- >>> textWidth "e\x301"
-- 1
-- >>> map snd (clusters "a🇦🇺e\x301")
-- [1,2,1]
-- >>> (charWidth 'a', charWidth '你', charWidth '\x301')
-- (1,2,0)
-- >>> unicodeVersion
-- "16.0.0"
module Lipstick.Width
  ( charWidth
  , textWidth
  , clusters
  , unicodeVersion
  ) where

import Lipstick.Internal.Width

-- $setup
-- >>> import Lipstick.Width
