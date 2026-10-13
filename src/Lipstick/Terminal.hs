-- | Detecting what the terminal supports, and printing blocks.
module Lipstick.Terminal
  ( putBlock
  , hPutBlock
  , detectEnv
  , detectProfile
  , profileFromEnv
  , queryDarkBackground
  ) where

import Data.Colour.SRGB (RGB (..))
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as T
import Lipstick.Ansi
import Lipstick.Color (ColorProfile (..))
import Lipstick.Internal.Block (Block)
import System.Console.ANSI (ConsoleLayer (Background), getLayerColor, hNowSupportsANSI)
import System.Environment (getEnvironment)
import System.IO (Handle, stdout)
import System.Info (os)

-- | Print a block to standard output, followed by a newline.
putBlock :: Block -> IO ()
putBlock = hPutBlock stdout

-- | Print a block to a handle, followed by a newline, downsampling colours
-- to what the handle supports.
hPutBlock :: Handle -> Block -> IO ()
hPutBlock h b = do
  env <- detectEnv h
  T.hPutStrLn h (toAnsi env b)

-- | The colour profile of a handle, assuming a dark background. Use
-- 'queryDarkBackground' to find out the background.
detectEnv :: Handle -> IO RenderEnv
detectEnv h = (`RenderEnv` True) <$> detectProfile h

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

-- | Ask the terminal for its background colour. 'Nothing' if it does not
-- answer (or stdin/stdout are not a terminal).
--
-- This writes a query and waits for the reply, so call it once and keep
-- the result. It does not work on Windows Terminal.
queryDarkBackground :: IO (Maybe Bool)
queryDarkBackground = fmap isDark <$> getLayerColor Background
  where
    isDark (RGB r g b) =
      let hi = maximum [r, g, b]
          lo = minimum [r, g, b]
          lightness = (fromIntegral hi + fromIntegral lo) / (2 * 65535) :: Double
       in lightness < 0.5
