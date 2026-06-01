-- | IO shell for the game: window lifecycle, input reading, and rendering.
-- All simulation logic is pure and lives in "RogueTrooper.Engine" /
-- "RogueTrooper.Aim"; this module only reads input, calls the pure step, and
-- draws the result.
module RogueTrooper
  ( runGame
  ) where

import           Raylib.Core         (clearBackground, getFrameTime, getMousePosition)
import           Raylib.Core.Shapes  (drawCircleLinesV, drawEllipseLines, drawLineV,
                                       drawRectangleLinesEx, drawRectangleV)
import           Raylib.Core.Text    (drawText)
import           Raylib.Types        (Color, Vector2, pattern Rectangle, pattern Vector2)
import           Raylib.Util         (drawing, whileWindowOpen_, withWindow)
import qualified Raylib.Util.Colors  as Colors
import           RogueTrooper.Behaviours (turretBehaviour)
import           RogueTrooper.Engine     (ScriptInput (..), stepTurret)
import           RogueTrooper.Types      (Box (..), BoxShape (..), GameState (..))

screenWidth, screenHeight, targetFps :: Int
screenWidth = 1280
screenHeight = 720
targetFps = 60

initialState :: GameState
initialState =
  GameState
    { turret       = Box (Vector2 640 360) (Circle 60)
    , seekSpeed    = 320
    , tower        = Vector2 640 660
    , towerHp      = 10
    , turretScript = turretBehaviour
    }

runGame :: IO ()
runGame =
  withWindow screenWidth screenHeight "RogueTrooper" targetFps $ \_ ->
    whileWindowOpen_ step initialState

-- | One frame: read input, advance the pure simulation, render.
step :: GameState -> IO GameState
step gs = do
  dt    <- getFrameTime
  mouse <- getMousePosition
  let gs' = stepTurret (ScriptInput dt mouse) gs
  drawing $ do
    clearBackground Colors.black
    renderTower gs'
    renderBox Colors.lime gs'.turret
    renderAimBox mouse
    drawText "move mouse - turret box seeks the cursor" 20 (screenHeight - 36) 18 Colors.darkGray
  pure gs'

-- | Draw the targeting-region outline for a box, by shape.
renderBox :: Color -> Box -> IO ()
renderBox col box = case box.shape of
  Circle r -> drawCircleLinesV box.center r col
  Rect hw hh ->
    let Vector2 cx cy = box.center
     in drawRectangleLinesEx (Rectangle (cx - hw) (cy - hh) (2 * hw) (2 * hh)) 2 col
  Oval rx ry ->
    let Vector2 cx cy = box.center
     in drawEllipseLines (round cx) (round cy) rx ry col

-- | Draw the mouse aim box: a circle with a crosshair.
renderAimBox :: Vector2 -> IO ()
renderAimBox m = do
  let Vector2 mx my = m
      k = 10
  drawCircleLinesV m 14 Colors.rayWhite
  drawLineV (Vector2 (mx - k) my) (Vector2 (mx + k) my) Colors.rayWhite
  drawLineV (Vector2 mx (my - k)) (Vector2 mx (my + k)) Colors.rayWhite

-- | Draw the tower and its HP readout.
renderTower :: GameState -> IO ()
renderTower gs = do
  let Vector2 tx ty = gs.tower
  drawRectangleV (Vector2 (tx - 20) (ty - 20)) (Vector2 40 40) Colors.gray
  drawText ("Tower HP: " <> show gs.towerHp) 20 20 20 Colors.rayWhite
