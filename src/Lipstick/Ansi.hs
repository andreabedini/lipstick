{-# LANGUAGE OverloadedStrings #-}

-- | Serialising blocks to text with ANSI escape sequences, and parsing
-- such text back into styled 'Inline' text.
module Lipstick.Ansi
  ( RenderEnv (..)
  , defaultRenderEnv
  , toAnsi
  , toAnsiLines
  , renderPlain
  , fromAnsi
  , ansiWidth
  , ansiHeight
  ) where

import Data.Array (Array, listArray, (!))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Lipstick.Color
import Lipstick.Internal.Grid
import Lipstick.Internal.Types

-- | What the output can display.
data RenderEnv = RenderEnv
  { envProfile :: !ColorProfile
  , envDarkBackground :: !Bool
  }
  deriving (Eq, Show)

-- | True colour on a dark background.
defaultRenderEnv :: RenderEnv
defaultRenderEnv = RenderEnv TrueColor True

-- | The block's lines, separated by newlines, without a trailing newline.
-- Attributes are reset at the end of every line.
toAnsi :: RenderEnv -> Grid -> Text
toAnsi env = T.intercalate (T.singleton '\n') . toAnsiLines env

-- | The block's lines, each with its own escape sequences: attributes are
-- reset and hyperlinks closed at the end of every line.
toAnsiLines :: RenderEnv -> Grid -> [Text]
toAnsiLines env = map (T.concat . renderLine env) . gridLines

-- | The block as plain text, without any escape sequences.
renderPlain :: Grid -> Text
renderPlain = toAnsi (RenderEnv NoTTY True)

-- | Fully resolved graphic rendition.
data SGR = SGR
  { sgrFg, sgrBg, sgrUl :: !(Maybe TermColor)
  , sgrBold, sgrFaint, sgrItalic, sgrBlink, sgrReverse, sgrStrike :: !Bool
  , sgrUnderline :: !Underline
  }
  deriving (Eq)

plainSGR :: SGR
plainSGR = SGR Nothing Nothing Nothing False False False False False False NoUnderline

resolveAttrs :: RenderEnv -> Attrs -> (SGR, Maybe Text)
resolveAttrs (RenderEnv NoTTY _) _ = (plainSGR, Nothing)
resolveAttrs (RenderEnv p dark) a =
  ( SGR
      { sgrFg = col (attrFg a)
      , sgrBg = col (attrBg a)
      , sgrUl = if ul /= NoUnderline && p >= ANSI256 then col (attrUnderlineColor a) else Nothing
      , sgrBold = on attrBold
      , sgrFaint = on attrFaint
      , sgrItalic = on attrItalic
      , sgrBlink = on attrBlink
      , sgrReverse = on attrReverse
      , sgrStrike = on attrStrikethrough
      , sgrUnderline = if ul /= NoUnderline && p < TrueColor then SingleUnderline else ul
      }
  , attrLink a
  )
  where
    col c = c >>= resolveColor p dark
    on f = fromMaybe False (f a)
    ul = fromMaybe NoUnderline (attrUnderline a)

-- | The pieces of a line: span texts and escape sequences, to be joined
-- with one 'T.concat'.
renderLine :: RenderEnv -> Line -> [Text]
renderLine env = go plainSGR Nothing . lineSpans
  where
    go cur lnk [] =
      [sgrCode plainSGR | cur /= plainSGR] ++ [closeLink | Just _ <- [lnk]]
    go cur lnk (Span a t _ : rest) =
      let (sgr, lnk') = resolveAttrs env a
          linkB
            | lnk' == lnk = []
            | otherwise = [closeLink | Just _ <- [lnk]] ++ [openLink u | Just u <- [lnk']]
          sgrB = [sgrCode sgr | sgr /= cur]
       in linkB ++ sgrB ++ t : go sgr lnk' rest

openLink :: Text -> Text
openLink url = T.concat ["\ESC]8;;", escapeUri url, "\ESC\\"]

closeLink :: Text
closeLink = "\ESC]8;;\ESC\\"

-- | A complete SGR sequence: reset, then every active attribute.
sgrCode :: SGR -> Text
sgrCode s = T.concat ["\ESC[", T.intercalate ";" ("0" : params), "m"]
  where
    params =
      concat
        [ ["1" | sgrBold s]
        , ["2" | sgrFaint s]
        , ["3" | sgrItalic s]
        , underlineParams (sgrUnderline s)
        , ["5" | sgrBlink s]
        , ["7" | sgrReverse s]
        , ["9" | sgrStrike s]
        , maybe [] (colorParams 30 90 "38") (sgrFg s)
        , maybe [] (colorParams 40 100 "48") (sgrBg s)
        , maybe [] ulColorParams (sgrUl s)
        ]
    underlineParams = \case
      NoUnderline -> []
      SingleUnderline -> ["4"]
      DoubleUnderline -> ["4:2"]
      CurlyUnderline -> ["4:3"]
      DottedUnderline -> ["4:4"]
      DashedUnderline -> ["4:5"]
    colorParams :: Word8 -> Word8 -> Text -> TermColor -> [Text]
    colorParams base brightBase ext = \case
      TC16 n
        | n < 8 -> [decimal (base + n)]
        | otherwise -> [decimal (brightBase + n - 8)]
      TC256 n -> [ext, "5", decimal n]
      TCRGB r g b -> [ext, "2", decimal r, decimal g, decimal b]
    ulColorParams = \case
      TC16 n -> ["58", "5", decimal n]
      TC256 n -> ["58", "5", decimal n]
      TCRGB r g b -> ["58", "2", decimal r, decimal g, decimal b]

-- | A byte in decimal, from a table: SGR parameters are all bytes.
decimal :: Word8 -> Text
decimal = (numbers !)
  where
    numbers :: Array Word8 Text
    numbers = listArray (0, 255) [T.pack (show i) | i <- [0 :: Int .. 255]]

-- Parsing ---------------------------------------------------------------------

-- | Parse text containing ANSI escape sequences, such as the output of
-- another program, into styled text.
--
-- SGR sequences (colours, attributes, underline styles and colours) and
-- OSC 8 hyperlinks are kept; every other escape sequence is dropped, along
-- with the payload of control strings (DCS, SOS, PM, APC: sixel and kitty
-- graphics, tmux passthrough).
-- Sequences that switch an attribute off (such as @22@ or @39@) unset it,
-- so the attributes of an enclosing style show through, and a reset
-- reveals the enclosing style completely.
fromAnsi :: Text -> Inline
fromAnsi = Inline . go mempty
  where
    -- ESC, and the 8-bit forms of CSI, OSC and the control strings.
    isEscape c = c == '\ESC' || c `elem` ['\x9B', '\x9D', '\x90', '\x98', '\x9E', '\x9F']

    go a t =
      let (txt, rest) = T.break isEscape t
       in [(a, txt) | not (T.null txt)] ++ case T.uncons rest of
            Nothing -> []
            Just ('\x9B', r) -> csi a r
            Just ('\x9D', r) -> osc a r
            Just ('\ESC', r) -> case T.uncons r of
              Just ('[', r') -> csi a r'
              Just (']', r') -> osc a r'
              Just (c, r')
                -- DCS, SOS, PM, APC
                | c `elem` ['P', 'X', '^', '_'] -> go a (afterString (T.dropWhile (not . endsString) r'))
                -- ESC, intermediates, final (e.g. ESC ( B)
                | c >= ' ' && c <= '/' -> go a (T.drop 1 (T.dropWhile (\x -> x >= ' ' && x <= '/') r'))
                | otherwise -> go a r'
              Nothing -> []
            Just (_, r) -> go a (afterString (T.dropWhile (not . endsString) r))

    -- CSI: parameters, intermediates, final byte.
    csi a r =
      let (params, r1) = T.span (\c -> c >= '0' && c <= '?') r
          r2 = T.dropWhile (\c -> c >= ' ' && c <= '/') r1
       in case T.uncons r2 of
            Just ('m', r3) -> go (applySGR (sgrGroups params) a) r3
            Just (_, r3) -> go a r3
            Nothing -> []

    -- OSC: up to BEL or ST.
    osc a r =
      let (body, r1) = T.break endsString r
       in go (applyOSC body a) (afterString r1)

    -- A control string ends with ST (ESC \ or 0x9C) or BEL, or is cut
    -- short by the ESC of another sequence.
    endsString c = c == '\a' || c == '\ESC' || c == '\x9C'
    afterString r = case T.uncons r of
      Just ('\ESC', x) -> fromMaybe r (T.stripPrefix (T.singleton '\\') x)
      Just (_, x) -> x
      Nothing -> T.empty

    applyOSC body a = case T.splitOn ";" body of
      "8" : _params : uri -> a {attrLink = if T.null (T.intercalate ";" uri) then Nothing else Just (T.intercalate ";" uri)}
      _ -> a

-- | SGR parameters: groups separated by @;@, each with @:@ sub-parameters.
-- Missing numbers count as 0.
sgrGroups :: Text -> [[Int]]
sgrGroups params
  | T.null params = [[0]]
  | otherwise = map (map number . T.splitOn ":") (T.splitOn ";" params)
  where
    number t
      | not (T.null t), T.all isDigit t = read (T.unpack t)
      | otherwise = 0

applySGR :: [[Int]] -> Attrs -> Attrs
applySGR groups a = case groups of
  [] -> a
  grp : gs -> case grp of
    -- A reset does not end a hyperlink: that is OSC 8, not SGR, state.
    [0] -> applySGR gs mempty {attrLink = attrLink a}
    [1] -> next a {attrBold = Just True}
    [2] -> next a {attrFaint = Just True}
    [3] -> next a {attrItalic = Just True}
    [4] -> next a {attrUnderline = Just SingleUnderline}
    [4, n] -> next a {attrUnderline = if n == 0 then Nothing else Just (underlineFrom n)}
    [n] | n == 5 || n == 6 -> next a {attrBlink = Just True}
    [7] -> next a {attrReverse = Just True}
    [9] -> next a {attrStrikethrough = Just True}
    [21] -> next a {attrUnderline = Just DoubleUnderline}
    [22] -> next a {attrBold = Nothing, attrFaint = Nothing}
    [23] -> next a {attrItalic = Nothing}
    [24] -> next a {attrUnderline = Nothing}
    [25] -> next a {attrBlink = Nothing}
    [27] -> next a {attrReverse = Nothing}
    [29] -> next a {attrStrikethrough = Nothing}
    [n]
      | n >= 30 && n <= 37 -> next a {attrFg = Just (Color16 (fromIntegral (n - 30)))}
      | n >= 40 && n <= 47 -> next a {attrBg = Just (Color16 (fromIntegral (n - 40)))}
      | n >= 90 && n <= 97 -> next a {attrFg = Just (Color16 (fromIntegral (n - 82)))}
      | n >= 100 && n <= 107 -> next a {attrBg = Just (Color16 (fromIntegral (n - 92)))}
    [39] -> next a {attrFg = Nothing}
    [49] -> next a {attrBg = Nothing}
    [59] -> next a {attrUnderlineColor = Nothing}
    -- "38;5;n" and "38;2;r;g;b"
    [n] | isColorParam n -> case gs of
      [5] : [i] : rest -> applySGR rest (setColor n (Just (Color256 (fromIntegral i))) a)
      [2] : [r] : [g] : [b] : rest -> applySGR rest (setColor n (Just (rgbOf r g b)) a)
      _ -> next a
    -- "38:5:n", "38:2:r:g:b" and "38:2:colourspace:r:g:b"
    n : sub | isColorParam n -> next (setColor n (colon sub) a)
    _ -> next a
    where
      next = applySGR gs
  where
    isColorParam n = n == 38 || n == 48 || n == 58

    colon = \case
      [5, i] -> Just (Color256 (fromIntegral i))
      [2, r, g, b] -> Just (rgbOf r g b)
      2 : _ : r : g : b : _ -> Just (rgbOf r g b)
      _ -> Nothing

    rgbOf r g b = RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)

    setColor n c x = case n of
      38 -> x {attrFg = c}
      48 -> x {attrBg = c}
      _ -> x {attrUnderlineColor = c}

    underlineFrom n = case n of
      2 -> DoubleUnderline
      3 -> CurlyUnderline
      4 -> DottedUnderline
      5 -> DashedUnderline
      _ -> SingleUnderline

-- | The display width of text that may contain escape sequences: the width
-- of its widest line.
ansiWidth :: Text -> Int
ansiWidth = gridWidth . fromCellLines . inlineLines 4 . fromAnsi

-- | The number of lines of text that may contain escape sequences.
ansiHeight :: Text -> Int
ansiHeight = gridHeight . fromCellLines . inlineLines 4 . fromAnsi
