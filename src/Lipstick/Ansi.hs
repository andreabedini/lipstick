{-# LANGUAGE OverloadedStrings #-}

-- | Serialising blocks to text with ANSI escape sequences.
module Lipstick.Ansi
  ( RenderEnv (..)
  , defaultRenderEnv
  , toAnsi
  , renderPlain
  ) where

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as B
import Lipstick.Color
import Lipstick.Internal.Block
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
toAnsi :: RenderEnv -> Block -> Text
toAnsi env b =
  TL.toStrict (B.toLazyText (mconcat (intersperse (B.singleton '\n') (map (renderLine env) (blockLines b)))))

-- | The block as plain text, without any escape sequences.
renderPlain :: Block -> Text
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

renderLine :: RenderEnv -> Line -> Builder
renderLine env = go plainSGR Nothing . lineSpans
  where
    go cur lnk [] =
      (if cur /= plainSGR then sgrCode plainSGR else mempty) <> maybe mempty (const closeLink) lnk
    go cur lnk (Span a t _ : rest) =
      let (sgr, lnk') = resolveAttrs env a
          linkB
            | lnk' == lnk = mempty
            | otherwise = maybe mempty (const closeLink) lnk <> maybe mempty openLink lnk'
          sgrB = if sgr == cur then mempty else sgrCode sgr
       in linkB <> sgrB <> B.fromText t <> go sgr lnk' rest

openLink :: Text -> Builder
openLink url = "\ESC]8;;" <> B.fromText url <> "\ESC\\"

closeLink :: Builder
closeLink = "\ESC]8;;\ESC\\"

-- | A complete SGR sequence: reset, then every active attribute.
sgrCode :: SGR -> Builder
sgrCode s = "\ESC[" <> mconcat (intersperse ";" (map B.fromString ("0" : params))) <> "m"
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
    colorParams :: Int -> Int -> String -> TermColor -> [String]
    colorParams base brightBase ext = \case
      TC16 n
        | n < 8 -> [show (base + fromIntegral n)]
        | otherwise -> [show (brightBase + fromIntegral n - 8)]
      TC256 n -> [ext, "5", show n]
      TCRGB r g b -> [ext, "2", show r, show g, show b]
    ulColorParams = \case
      TC16 n -> ["58", "5", show n]
      TC256 n -> ["58", "5", show n]
      TCRGB r g b -> ["58", "2", show r, show g, show b]
