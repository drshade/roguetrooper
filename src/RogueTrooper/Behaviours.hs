-- | Entity behaviour scripts, authored purely in the "RogueTrooper.Script" DSL.
--
-- This module is content, not engine: it may only depend on the DSL, never on
-- the engine or raylib IO. Adding or changing behaviour here must not require
-- engine changes.
module RogueTrooper.Behaviours
  ( turretBehaviour
  , enemyBehaviour
  , straightBullet
  , groundLevel
  ) where

import Raylib.Types     (Vector2, pattern Vector2)
import Raylib.Util.Math (vectorDistance, vectorNormalize, (|*), (|+|), (|-|))
import RogueTrooper.Script (ProjectileType (..), Script, damage, despawnSelf, fire, getAimPos,
                            getEnemies, getMyPos, getTargetInBox, getTowerPos, moveToward, yield)

-- | The y coordinate at which descending enemies are considered landed.
-- (raylib screen coords: y grows downward.)
groundLevel :: Float
groundLevel = 640

-- | Is this position on the ground?
onLand :: Vector2 -> Bool
onLand (Vector2 _ y) = y >= groundLevel

-- | The turret: every frame, move toward the current aim position, then yield.
-- Purely reactive, expressed as a forever-loop over the DSL.
turretBehaviour :: Script ()
turretBehaviour = forever $ do
  aim <- getAimPos
  moveToward aim
  target <- getTargetInBox
  case target of
    Just tp -> do
      tower <- getTowerPos
      fire tower (StraightBullet (farPoint tower tp))  -- shoot from the tower through the target
    Nothing -> pure ()                                 -- no target: hold fire
  yield

-- | A point far along the ray from @from@ through @to@, so a straight bullet
-- flies through its target rather than stopping on it.
farPoint :: Vector2 -> Vector2 -> Vector2
farPoint from to
  | vectorDistance from to < 1 = from |+| Vector2 0 (-5000)   -- degenerate: aim up
  | otherwise                  = from |+| (vectorNormalize (to |-| from) |* 5000)

-- | Radius within which a straight bullet counts as hitting an enemy.
bulletHitRadius :: Float
bulletHitRadius = 16

-- | Is a position outside the (margin-padded) screen?
offScreen :: Vector2 -> Bool
offScreen (Vector2 x y) = x < -50 || x > 1330 || y < -50 || y > 770

-- | An enemy (paratrooper): descend until on the ground, then advance on the
-- tower. Reactive — each frame it decides based on its current position.
enemyBehaviour :: Script ()
enemyBehaviour = forever $ do
  myPos <- getMyPos
  let Vector2 mx _ = myPos
  if onLand myPos
    then getTowerPos >>= moveToward       -- landed: advance on the tower
    else moveToward (Vector2 mx groundLevel)  -- airborne: descend straight down
  yield

-- | A dumb projectile that flies toward a fixed aim point, hits the first enemy
-- within its radius (emitting damage + removing itself), and despawns when it
-- leaves the screen.
straightBullet :: Vector2 -> Script ()
straightBullet aim = fly
  where
    fly = do
      moveToward aim
      me <- getMyPos
      es <- getEnemies
      case [tid | (tid, p) <- es, vectorDistance me p <= bulletHitRadius] of
        tid : _ -> damage tid >> despawnSelf            -- hit: kill it and remove myself
        []      -> if offScreen me then despawnSelf else yield >> fly
