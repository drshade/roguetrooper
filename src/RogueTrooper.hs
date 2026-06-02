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
import           RogueTrooper.Behaviours (enemyBehaviour, groundLevel,
                                          missionDirector, straightBullet,
                                          turretBehaviour)
import           RogueTrooper.Engine     (World (..), step)
import           RogueTrooper.Types      (Box (..), BoxShape (..), Entity (..),
                                          EntityId (..), EntityKind (..),
                                          GameState (..), ProjectileType (..))

screenWidth, screenHeight, targetFps :: Int
screenWidth = 1280
screenHeight = 720
targetFps = 600

-- | Downward acceleration applied to airborne physical entities (px/s²).
worldGravity :: Vector2
worldGravity = Vector2 0 900

initialState :: GameState
initialState =
  GameState
    { entities = Map.fromList [(EntityId 0, turret), (EntityId 1, director)]
    , tower    = Vector2 640 620   -- just above the ground line
    , towerHp  = 10
    , score    = 0
    , nextId   = 2
    }
  where
    turret   = Entity (EntityId 0) Turret (Box (Vector2 640 360) (Circle 60)) (Vector2 0 0) 1 turretBehaviour
    director = Entity (EntityId 1) Director (Box (Vector2 0 0) (Circle 0)) (Vector2 0 0) 1 missionDirector

-- | Content-side projectile factory handed to the engine: map a 'ProjectileType'
-- to an assembled entity (its kind, behaviour script, shape). The engine assigns
-- the real id. This is the single registry of projectile types.
mkProjectile :: ProjectileType -> Vector2 -> Entity
mkProjectile pt@(StraightBullet v) origin =
  Entity (EntityId 0) (Projectile pt) (Box origin (Circle 4)) v 1 straightBullet

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
      world     = World dt mouse gs.tower worldGravity groundLevel enemyList mkProjectile spawnEnemy
      gs'       = step world gs
      ents      = Map.elems gs'.entities
      mTurret   = find (\e -> e.kind == Turret) ents
  drawing $ do
    clearBackground Colors.black
    renderGround
    maybe (pure ()) (\t -> renderBarrel gs'.tower t.box.center) mTurret
    renderTower gs'
    mapM_ renderEntity ents
    renderAimBox mouse
    drawText ("Score: " <> show gs'.score) 20 48 20 Colors.rayWhite
    fps <- getFPS
    drawText ("FPS: " <> show fps) 20 76 20 Colors.lime
    drawText "move mouse to aim - turret fires toward the scanbox" 20 (screenHeight - 36) 18 Colors.darkGray
  pure gs'

-- | Draw the ground line across the screen.
renderGround :: IO ()
renderGround =
  drawLineV (Vector2 0 groundLevel) (Vector2 (fromIntegral screenWidth) groundLevel) Colors.darkGray

-- | Draw a single entity, coloured/annotated by its kind.
renderEntity :: Entity -> IO ()
renderEntity e = case e.kind of
  Turret       -> renderBox Colors.lime e.box
  Enemy        -> renderBox Colors.red e.box >> renderEnemyHp e
  Projectile _ -> renderBox Colors.gold e.box
  Director     -> pure ()                       -- invisible mission script

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
