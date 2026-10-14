-- | Detecting what the terminal supports, and printing blocks.
--
-- Printing detects the colour profile on every call, which is cheap. The
-- background is taken from @COLORFGBG@ when the terminal sets it, and is
-- otherwise assumed dark. To ask the terminal itself, which is slower and
-- reads from stdin, detect the environment once with 'detectEnvQuery' and
-- print with 'hPutBlockWith'.
module Lipstick.Terminal
  ( putBlock
  , hPutBlock
  , hPutBlockWith
  , detectEnv
  , detectEnvQuery
  , detectProfile
  , profileFromEnv
  , backgroundFromEnv
  , queryDarkBackground
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isDigit)
import Data.Colour.SRGB (RGB (..))
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text.IO.Utf8 qualified as Utf8
import Lipstick.Ansi
import Lipstick.Color (ColorProfile (..))
import Lipstick.Internal.Block (Block)
import System.Console.ANSI (ConsoleLayer (Background), hGetLayerColor, hNowSupportsANSI)
import System.Environment (getEnvironment)
import System.IO (Handle, hIsTerminalDevice, stdin, stdout)
import System.Info (os)

-- | Print a block to standard output, followed by a newline.
putBlock :: Block -> IO ()
putBlock = hPutBlock stdout

-- | Print a block to a handle, followed by a newline, downsampling colours
-- to what the handle supports.
hPutBlock :: Handle -> Block -> IO ()
hPutBlock h b = do
  env <- detectEnv h
  hPutBlockWith env h b

-- | Print a block to a handle, followed by a newline, for a known
-- environment.
--
-- The output is always UTF-8, whatever the handle's encoding: terminals
-- expect it, and a locale such as @C@ (common in containers and cron jobs)
-- would otherwise make the handle reject box-drawing characters.
hPutBlockWith :: RenderEnv -> Handle -> Block -> IO ()
hPutBlockWith env h b = Utf8.hPutStrLn h (toAnsi env b)

-- | The colour profile of a handle, and whether the background is dark
-- according to @COLORFGBG@ (dark if it is not set). Does not talk to the
-- terminal.
detectEnv :: Handle -> IO RenderEnv
detectEnv h = do
  profile <- detectProfile h
  vars <- getEnvironment
  pure (RenderEnv profile (fromMaybe True (backgroundFromEnv vars)))

-- | Like 'detectEnv', but first asks the terminal for its background
-- colour (see 'queryDarkBackground'). Call it once and keep the result.
detectEnvQuery :: Handle -> IO RenderEnv
detectEnvQuery h = do
  env <- detectEnv h
  answer <- queryDarkBackground h
  pure (maybe env (\dark -> env {envDarkBackground = dark}) answer)

-- | The colour profile of a handle. On Windows this also switches on ANSI
-- processing in the console, if needed.
--
-- @NO_COLOR@ removes colours; @CLICOLOR_FORCE@ keeps them even when the
-- handle is not a terminal.
detectProfile :: Handle -> IO ColorProfile
detectProfile h = do
  supported <- hNowSupportsANSI h
  vars <- getEnvironment
  pure (profileFromEnv supported vars)

-- | The rules behind 'detectProfile': whether the handle is an ANSI
-- terminal and the environment variables.
profileFromEnv :: Bool -> [(String, String)] -> ColorProfile
profileFromEnv supported vars
  | not supported && not forced = NoTTY
  | set "NO_COLOR" = Ascii
  | colorterm `elem` ["truecolor", "24bit"] = TrueColor
  | set "WT_SESSION" = TrueColor
  | any (`isInfixOf` term) ["truecolor", "24bit", "direct"] = TrueColor
  | any (`isPrefixOf` term) trueColorTerms = TrueColor
  | "256color" `isInfixOf` term = ANSI256
  | null term && os == "mingw32" = TrueColor
  | otherwise = ANSI16
  where
    get k = fromMaybe "" (lookup k vars)
    set k = not (null (get k))
    forced = set "CLICOLOR_FORCE" && get "CLICOLOR_FORCE" /= "0"
    term = get "TERM"
    colorterm = get "COLORTERM"
    trueColorTerms = ["alacritty", "contour", "foot", "kitty", "rio", "wezterm", "xterm-ghostty", "xterm-kitty"]

-- | Whether the background is dark according to @COLORFGBG@, which some
-- terminals (rxvt, Konsole, …) set to @"fg;bg"@ palette indices.
backgroundFromEnv :: [(String, String)] -> Maybe Bool
backgroundFromEnv vars = do
  v <- lookup "COLORFGBG" vars
  let bgField = reverse (takeWhile (/= ';') (reverse v))
  if not (null bgField) && all isDigit bgField
    then let n = read bgField :: Int in Just (n <= 6 || n == 8)
    else Nothing

-- | Ask the terminal behind a handle for its background colour, and
-- whether it is dark. The reply is read from stdin.
--
-- 'Nothing' when stdin or the handle is not a terminal, when the terminal
-- does not answer within half a second, and on Windows, where the query
-- does not work in Windows Terminal.
queryDarkBackground :: Handle -> IO (Maybe Bool)
queryDarkBackground h
  | os == "mingw32" = pure Nothing
  | otherwise = do
      ttys <- (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice h
      if not ttys
        then pure Nothing
        else do
          answer <- try (hGetLayerColor h Background)
          pure $ case answer of
            Right (Just c) -> Just (isDark c)
            Right Nothing -> Nothing
            Left (_ :: SomeException) -> Nothing
  where
    isDark (RGB r g b) =
      let hi = maximum [r, g, b]
          lo = minimum [r, g, b]
          lightness = (fromIntegral hi + fromIntegral lo) / (2 * 65535) :: Double
       in lightness < 0.5
