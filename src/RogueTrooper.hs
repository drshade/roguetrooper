-- | IO shell for the game: window lifecycle, input reading, and rendering.
-- All simulation logic is pure and lives in "RogueTrooper.Engine" /
-- "RogueTrooper.Aim"; this module only reads input, calls the pure step, and
-- draws the result.
module RogueTrooper
  ( runGame
  ) where

import qualified Data.Map.Strict         as Map
import           Raylib.Core             (clearBackground, getFPS, getFrameTime,
                                          getMousePosition)
import           Raylib.Core.Shapes      (drawCircleLinesV, drawEllipseLines,
                                          drawLineEx, drawLineV,
                                          drawRectangleLinesEx, drawRectangleV)
import           Raylib.Core.Text        (drawText)
import           Raylib.Types            (Color, Vector2, pattern Rectangle,
                                          pattern Vector2)
import           Raylib.Util             (drawing, whileWindowOpen_, withWindow)
import qualified Raylib.Util.Colors      as Colors
import           Raylib.Util.Math        (vectorNormalize, (|*), (|+|), (|-|))
import           RogueTrooper.Behaviours (enemyBehaviour, straightBullet,
                                          turretBehaviour)
import           RogueTrooper.Engine     (World (..), step)
import           RogueTrooper.Types       (Box (..), BoxShape (..), Entity (..),
                                          EntityId (..), EntityKind (..),
                                          GameState (..), ProjectileType (..))

screenWidth, screenHeight, targetFps :: Int
screenWidth = 1280
screenHeight = 720
targetFps = 600

initialState :: GameState
initialState =
  GameState
    { entities      = Map.fromList [(EntityId 0, turret)]
    , tower         = Vector2 640 660
    , towerHp       = 10
    , score         = 0
    , nextId        = 1
    , spawnTimer    = 0.5
    , spawnInterval = 0.1
    , seed          = 12345
    }
  where
    turret = Entity (EntityId 0) Turret (Box (Vector2 640 360) (Circle 60)) (Vector2 0 0) 1 turretBehaviour

-- | Content-side projectile factory handed to the engine: map a 'ProjectileType'
-- to an assembled entity (its kind, behaviour script, shape). The engine assigns
-- the real id. This is the single registry of projectile types.
mkProjectile :: ProjectileType -> Vector2 -> Entity
mkProjectile pt@(StraightBullet target) origin =
  Entity (EntityId 0) (Projectile pt) (Box origin (Circle 4)) (Vector2 0 0) 1 (straightBullet target)

-- | Content-side enemy factory handed to the engine's spawner. Normal enemies
-- have 3 hit points.
spawnEnemy :: Vector2 -> Entity
spawnEnemy pos = Entity (EntityId 0) Enemy (Box pos (Circle 12)) (Vector2 0 0) 3 enemyBehaviour

runGame :: IO ()
runGame =
  withWindow screenWidth screenHeight "RogueTrooper" targetFps $ \_ ->
    whileWindowOpen_ frame initialState

-- | One frame: read input, advance the pure simulation, render.
frame :: GameState -> IO GameState
frame gs = do
  dt    <- getFrameTime
  mouse <- getMousePosition
  let enemyList = [(e.eid, e.box.center, e.vel) | e <- Map.elems gs.entities, e.kind == Enemy]
      world     = World dt mouse gs.tower enemyList mkProjectile spawnEnemy
      gs'       = step world gs
      ents      = Map.elems gs'.entities
      mTurret   = find (\e -> e.kind == Turret) ents
  drawing $ do
    clearBackground Colors.black
    maybe (pure ()) (\t -> renderBarrel gs'.tower t.box.center) mTurret
    renderTower gs'
    mapM_ renderEntity ents
    renderAimBox mouse
    drawText ("Score: " <> show gs'.score) 20 48 20 Colors.rayWhite
    fps <- getFPS
    drawText ("FPS: " <> show fps) 20 76 20 Colors.lime
    drawText "move mouse to aim - turret auto-fires at locked targets" 20 (screenHeight - 36) 18 Colors.darkGray
  pure gs'

-- | Draw a single entity, coloured/annotated by its kind.
renderEntity :: Entity -> IO ()
renderEntity e = case e.kind of
  Turret       -> renderBox Colors.lime e.box
  Enemy        -> renderBox Colors.red e.box >> renderEnemyHp e
  Projectile _ -> renderBox Colors.gold e.box

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

-- | Debug overlay: draw an enemy's remaining HP in the centre of its circle.
renderEnemyHp :: Entity -> IO ()
renderEnemyHp e =
  let Vector2 cx cy = e.box.center
   in drawText (show e.hp) (round cx - 4) (round cy - 8) 16 Colors.rayWhite

-- | Draw the mouse aim box: a circle with a crosshair.
renderAimBox :: Vector2 -> IO ()
renderAimBox m = do
  let Vector2 mx my = m
      k = 10
  drawCircleLinesV m 14 Colors.rayWhite
  drawLineV (Vector2 (mx - k) my) (Vector2 (mx + k) my) Colors.rayWhite
  drawLineV (Vector2 mx (my - k)) (Vector2 mx (my + k)) Colors.rayWhite

-- | Draw the turret barrel: a thick stub from the tower pointing at the scanbox.
renderBarrel :: Vector2 -> Vector2 -> IO ()
renderBarrel tower aim =
  let end = tower |+| (vectorNormalize (aim |-| tower) |* 48)
   in drawLineEx tower end 8 Colors.darkGray

-- | Draw the tower and its HP readout.
renderTower :: GameState -> IO ()
renderTower gs = do
  let Vector2 tx ty = gs.tower
  drawRectangleV (Vector2 (tx - 20) (ty - 20)) (Vector2 40 40) Colors.gray
  drawText ("Tower HP: " <> show gs.towerHp) 20 20 20 Colors.rayWhite
