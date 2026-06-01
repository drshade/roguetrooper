-- | Entity behaviour scripts, authored purely in the "RogueTrooper.Script" DSL.
--
-- This module is content, not engine: it may only depend on the DSL, never on
-- the engine or raylib IO. Adding or changing behaviour here must not require
-- engine changes.
module RogueTrooper.Behaviours
  ( turretBehaviour
  , enemyBehaviour
  , straightBullet
  , predictLead
  , bulletSpeed
  , groundLevel
  ) where

import Raylib.Types     (Vector2, pattern Vector2)
import Raylib.Util.Math (vectorDistance, vectorNormalize, (|*), (|+|), (|-|))
import RogueTrooper.Script (ProjectileType (..), Script, damage, despawnSelf, fire, getAimPos,
                            getDt, getEnemies, getMyPos, getTargetInBox, getTowerPos, moveToward,
                            yield)

-- | The y coordinate at which descending enemies are considered landed.
-- (raylib screen coords: y grows downward.)
groundLevel :: Float
groundLevel = 640

-- | Is this position on the ground?
onLand :: Vector2 -> Bool
onLand (Vector2 _ y) = y >= groundLevel

-- | The turret: every frame, move toward the current aim position, then yield.
-- Purely reactive, expressed as a forever-loop over the DSL.
-- | Seconds between turret shots (fire rate).
fireInterval :: Float
fireInterval = 0.18

turretBehaviour :: Script ()
turretBehaviour = turret 0   -- cooldown carried in the continuation, starts ready
  where
    turret cooldown = do
      aim <- getAimPos
      moveToward aim
      target <- getTargetInBox
      cooldown' <- case target of
        Just (tp, tv) | cooldown <= 0 -> do
          tower <- getTowerPos
          let lead = predictLead tower tp tv bulletSpeed  -- aim where the target will be
          fire tower (StraightBullet (farPoint tower lead))
          pure fireInterval                               -- reset cooldown after firing
        _ -> pure cooldown                                -- no target or still cooling down
      dt <- getDt
      yield
      turret (cooldown' - dt)                             -- tick the cooldown down

-- | A point far along the ray from @from@ through @to@, so a straight bullet
-- flies through its target rather than stopping on it.
farPoint :: Vector2 -> Vector2 -> Vector2
farPoint from to
  | vectorDistance from to < 1 = from |+| Vector2 0 (-5000)   -- degenerate: aim up
  | otherwise                  = from |+| (vectorNormalize (to |-| from) |* 5000)

-- | Radius within which a straight bullet counts as hitting an enemy.
bulletHitRadius :: Float
bulletHitRadius = 18

-- | Speed of fired bullets. Shared by the turret's lead calculation and the
-- projectile factory so they stay in sync.
bulletSpeed :: Float
bulletSpeed = 900

-- | Predict where to aim to hit a moving target: the intercept point given the
-- bullet's travel time. Two fixed-point iterations refine the estimate.
predictLead :: Vector2 -> Vector2 -> Vector2 -> Float -> Vector2
predictLead origin pos vel speed = pos |+| (vel |* travelTime)
  where
    t1         = vectorDistance origin pos / speed
    p1         = pos |+| (vel |* t1)
    travelTime = vectorDistance origin p1 / speed

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
