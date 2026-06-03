-- | The turret and its projectiles (weapon scripts). Content only.
module RogueTrooper.Behaviours.Weapon
  ( turretBehaviour
  , straightBullet
  , homingMissile
  , bulletSpeed
  ) where

import           Raylib.Types                   (Vector2, pattern Vector2)
import           Raylib.Util.Math               (magnitude, vectorDistance,
                                                 vectorNormalize, (|*), (|-|))
import           RogueTrooper.Behaviours.Common (offScreen, onLand, seekAt)
import           RogueTrooper.Script            (EntityId, ProjectileType (..),
                                                 Script, damage, despawnSelf,
                                                 fire, getAimPos, getDt,
                                                 getEnemies, getEntityPos,
                                                 getMyPos, getMyVel,
                                                 getTargetInBox, getTowerPos,
                                                 push, steer, yield)

-- Turret ---------------------------------------------------------------------

-- | How fast the scanbox reticle tracks the crosshair (constant speed).
scanSpeed :: Float
scanSpeed = 900

-- | Seconds between turret shots (fire rate).
fireInterval :: Float
fireInterval = 0.3

-- | The turret: the scanbox tracks the crosshair; every cooldown it locks the
-- nearest enemy inside the scanbox and launches a homing missile at it (holding
-- fire when there's no lock). The scanbox is a non-physical reticle.
turretBehaviour :: Script ()
turretBehaviour = turret 0
  where
    turret cooldown = do
      aim <- getAimPos
      seekAt scanSpeed aim                                 -- reticle tracks the crosshair
      cooldown' <-
        if cooldown <= 0
          then do
            target <- getTargetInBox
            case target of
              Just (tid, tpos) -> do
                tower <- getTowerPos
                fire tower (HomingMissile tid (launchToward tower tpos missileLaunchSpeed))
                pure fireInterval
              Nothing -> pure cooldown                     -- no lock: hold fire
          else pure cooldown
      dt <- getDt
      yield
      turret (cooldown' - dt)

-- | A velocity of magnitude @speed@ pointing from @from@ toward @to@.
launchToward :: Vector2 -> Vector2 -> Float -> Vector2
launchToward from to speed
  | vectorDistance from to < 1 = Vector2 0 (negate speed)
  | otherwise                  = vectorNormalize (to |-| from) |* speed

-- Straight bullet (a flatter, faster ballistic round) ------------------------

-- | Launch speed of straight bullets (used when the turret is equipped with them).
bulletSpeed :: Float
bulletSpeed = 1400

bulletDamage :: Int
bulletDamage = 2

bulletHitRadius :: Float
bulletHitRadius = 18

knockbackStrength :: Float
knockbackStrength = 260

-- | A ballistic bullet (launched with a velocity, then curved by gravity). Each
-- frame it checks for an overlapping enemy — emitting damage + knockback +
-- despawn — and despawns on hitting the ground or leaving the screen.
straightBullet :: Script ()
straightBullet = fly
  where
    fly = do
      me <- getMyPos
      es <- getEnemies
      case [tid | (tid, p) <- es, vectorDistance me p <= bulletHitRadius] of
        tid : _ -> do
          damage tid bulletDamage
          v <- getMyVel
          push tid (impulseAlong knockbackStrength v)
          despawnSelf
        []
          | onLand me || offScreen me -> despawnSelf
          | otherwise                 -> yield >> fly

-- Homing missile -------------------------------------------------------------

-- | missileSpeed = cruise (max) speed the propulsion builds to; missileLaunchSpeed
-- = the speed it pops out of the tube at; missileResponsiveness = how briskly it
-- accelerates / how hard it homes (lower = slower build, but more gravity droop:
-- droop ≈ gravity / missileResponsiveness).
missileSpeed, missileResponsiveness, missileLaunchSpeed, missileKnockback :: Float
missileSpeed = 750
missileResponsiveness = 3
missileLaunchSpeed = 10
missileKnockback = 380

missileDamage :: Int
missileDamage = 3

missileHitRadius :: Float
missileHitRadius = 20

-- | A homing missile: every frame its propulsion accelerates it toward a
-- constant cruise speed; the direction is the target while it's alive, otherwise
-- the missile's current heading (so it keeps powering on, just no longer
-- tracking). Detonates (damage + knockback) on any overlap; despawns on the
-- ground / off-screen.
homingMissile :: EntityId -> Script ()
homingMissile target = fly
  where
    fly = do
      me <- getMyPos
      es <- getEnemies
      case [tid | (tid, p) <- es, vectorDistance me p <= missileHitRadius] of
        tid : _ -> do
          damage tid missileDamage
          v <- getMyVel
          push tid (impulseAlong missileKnockback v)
          despawnSelf
        [] -> do
          mtp <- getEntityPos target
          v   <- getMyVel
          let dir = case mtp of
                      Just tp -> aimUnit me tp v   -- steer toward the live target
                      Nothing -> headingUnit v     -- no target: hold current heading
          steer missileResponsiveness (dir |* missileSpeed)   -- always accelerate to cruise
          if onLand me || offScreen me then despawnSelf else yield >> fly

    -- unit direction toward the target (falls back to heading if it's on top of us)
    aimUnit me tp v = let d = tp |-| me in if magnitude d < 1 then headingUnit v else vectorNormalize d
    -- unit heading from current velocity (up if essentially stationary)
    headingUnit v   = if magnitude v < 1 then Vector2 0 (-1) else vectorNormalize v

-- | An impulse of the given magnitude in the direction of travel @v@.
impulseAlong :: Float -> Vector2 -> Vector2
impulseAlong strength v
  | magnitude v < 1 = Vector2 0 0
  | otherwise       = vectorNormalize v |* strength
