-- | Top-level entry point for the RogueTrooper game.
--
-- Bootstrap scaffolding only: opens a window, clears the screen, and draws a
-- title until the window is closed. This exists to prove the GHC + rerebase +
-- h-raylib toolchain end-to-end before any game mechanics are built.
module RogueTrooper
  ( runGame
  ) where

import           Raylib.Core        (clearBackground)
import           Raylib.Core.Text   (drawText)
import           Raylib.Util        (drawing, whileWindowOpen0, withWindow)
import qualified Raylib.Util.Colors as Colors

screenWidth, screenHeight, targetFps :: Int
screenWidth = 1280
screenHeight = 720
targetFps = 60

runGame :: IO ()
runGame =
  withWindow screenWidth screenHeight "RogueTrooper" targetFps $ \_ ->
    whileWindowOpen0 $
      drawing $ do
        clearBackground Colors.black
        drawText "RogueTrooper" 40 40 40 Colors.rayWhite
        drawText "bootstrap window - close to exit" 40 96 20 Colors.gray
