-- | The turret and its projectiles (weapon scripts). Content only.
module RogueTrooper.Behaviours.Weapon
  ( turretBehaviour
  , straightBullet
  , pellet
  , homingMissile
  , repulsorWave
  , bulletSpeed
  , missileLaunchSpeed
  ) where

import           Raylib.Types                   (Vector2, pattern Vector2)
import           Raylib.Util.Math               (magnitude, vectorDistance,
                                                 vectorNormalize, (|*), (|-|))
import           RogueTrooper.Behaviours.Common (launchToHit, launchToward,
                                                 offScreen, onLand, seekAt)
import           RogueTrooper.Script            (EntityId, ProjectileType (..),
                                                 Script, damage, despawnSelf,
                                                 fire, getAimPos, getDt,
                                                 getEnemies, getEntityPos,
                                                 getGravity, getMyPos, getMyVel,
                                                 getTowerPos, push, setRadius,
                                                 steer, yield)

-- Turret ---------------------------------------------------------------------

-- | How fast the scanbox reticle tracks the crosshair (constant speed).
scanSpeed :: Float
scanSpeed = 900

-- | Seconds between turret shots (fire rate).
fireInterval :: Float
fireInterval = 0.3

-- | The main turret: the scanbox tracks the crosshair, and every cooldown it
-- fires its primary weapon (a ballistic straight bullet) at where the scanbox
-- points — player-aimed, no auto-lock. The scanbox is a non-physical reticle.
turretBehaviour :: Script ()
turretBehaviour = turret 0
  where
    turret cooldown = do
      aim <- getAimPos
      seekAt scanSpeed aim                                 -- reticle tracks the crosshair
      cooldown' <-
        if cooldown <= 0
          then do
            tower <- getTowerPos
            scan  <- getMyPos                              -- scanbox centre = where we point
            g     <- getGravity
            let Vector2 _ gy = g
                -- ballistic solution that lands on the scanbox; straight shot if out of range
                v = maybe (launchToward tower scan bulletSpeed) id (launchToHit bulletSpeed gy tower scan)
            fire tower (StraightBullet v)
            pure fireInterval
          else pure cooldown
      dt <- getDt
      yield
      turret (cooldown' - dt)

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

-- | Shotgun pellet stats: smaller hit radius, less damage and knockback than a
-- primary bullet (there are several per burst).
pelletDamage :: Int
pelletDamage = 1

pelletHitRadius, pelletKnockback :: Float
pelletHitRadius = 10
pelletKnockback = 120

-- | A ballistic round (launched with a velocity, then curved by gravity).
-- Parametrised by its damage / hit radius / knockback so primary bullets and
-- shotgun pellets share the flight logic. Each frame it checks for an
-- overlapping enemy — emitting damage + knockback + despawn — and despawns on
-- hitting the ground or leaving the screen.
ballistic :: Int -> Float -> Float -> Script ()
ballistic dmg hitRadius knockback = fly
  where
    fly = do
      me <- getMyPos
      es <- getEnemies
      case [tid | (tid, p) <- es, vectorDistance me p <= hitRadius] of
        tid : _ -> do
          damage tid dmg
          v <- getMyVel
          push tid (impulseAlong knockback v)
          despawnSelf
        []
          | onLand me || offScreen me -> despawnSelf
          | otherwise                 -> yield >> fly

-- | The turret's primary round.
straightBullet :: Script ()
straightBullet = ballistic bulletDamage bulletHitRadius knockbackStrength

-- | A shotgun pellet: a smaller, weaker ballistic round.
pellet :: Script ()
pellet = ballistic pelletDamage pelletHitRadius pelletKnockback

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

-- Repulsor wave --------------------------------------------------------------

-- | waveSpeed = how fast the wavefront expands (px/s); waveMaxRadius = how far it
-- reaches before fading; waveKnockback = how hard it shoves whatever it reaches.
waveSpeed, waveMaxRadius, waveKnockback :: Float
waveSpeed     = 900
waveMaxRadius = 340
waveKnockback = 560

-- | A repulsion wave: a non-damaging pulse anchored at its origin whose radius
-- grows each frame. Any enemy the expanding front first reaches is shoved
-- radially outward exactly once (tracked in @hit@). Despawns at max radius.
-- Knockback and the drawn ring both use the fixed @origin@, so the wave's own
-- (ignored, gravity-bound) position never matters.
repulsorWave :: Vector2 -> Script ()
repulsorWave origin = expand 0 []
  where
    expand radius hit = do
      dt <- getDt
      let radius' = radius + waveSpeed * dt
      setRadius radius'                                   -- grow the visual / hit ring
      es <- getEnemies
      let fresh = [ (i, p) | (i, p) <- es, i `notElem` hit, vectorDistance origin p <= radius' ]
      mapM_ (\(i, p) -> push i (shoveFrom origin p waveKnockback)) fresh
      if radius' >= waveMaxRadius
        then despawnSelf
        else yield >> expand radius' (hit <> map fst fresh)

-- | An outward impulse of magnitude @strength@, pushing @target@ away from @from@.
shoveFrom :: Vector2 -> Vector2 -> Float -> Vector2
shoveFrom from target strength =
  let d = target |-| from
   in if magnitude d < 1 then Vector2 0 (negate strength) else vectorNormalize d |* strength
