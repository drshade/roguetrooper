-- | Entity behaviour scripts, authored purely in the "RogueTrooper.Script" DSL.
--
-- This module is content, not engine: it may only depend on the DSL, never on
-- the engine or raylib IO. Adding or changing behaviour here must not require
-- engine changes.
module RogueTrooper.Behaviours
  ( turretBehaviour
  , enemyBehaviour
  , straightBullet
  , launchToHit
  , predictLead
  , bulletSpeed
  , groundLevel
  ) where

import           Raylib.Types        (Vector2, pattern Vector2)
import           Raylib.Util.Math    (magnitude, vectorDistance, vectorNormalize,
                                      (|*), (|+|), (|-|))
import           RogueTrooper.Script (ProjectileType (..), Script, damage,
                                      despawnSelf, fire, getAimPos, getDt,
                                      getEnemies, getGravity, getMyPos, getMyVel,
                                      getTowerPos, push, setVel, steer, yield)

-- Constants ------------------------------------------------------------------

-- | The y coordinate of the ground line (raylib screen coords: y grows down).
groundLevel :: Float
groundLevel = 640

-- | Seconds between turret shots (fire rate).
fireInterval :: Float
fireInterval = 0.3

-- | How fast the scanbox reticle tracks the crosshair (constant speed).
scanSpeed :: Float
scanSpeed = 900

-- | Enemy walk speed and how quickly its legs recover their desired velocity
-- (lower = knockback lingers longer).
enemySpeed, enemyResponsiveness :: Float
enemySpeed = 80
enemyResponsiveness = 6

-- | Parachute: the descent velocity an airborne enemy steers toward, and how
-- strongly. Terminal fall speed ≈ parachuteSpeed + gravity / parachuteResponsiveness
-- (≈ 70 + 900/40 ≈ 92 px/s). Higher responsiveness ⇒ terminal closer to parachuteSpeed.
parachuteSpeed, parachuteResponsiveness :: Float
parachuteSpeed = 70
parachuteResponsiveness = 40

-- | Launch speed of fired bullets.
bulletSpeed :: Float
bulletSpeed = 1400

-- | Damage a straight bullet deals on hit.
bulletDamage :: Int
bulletDamage = 2

-- | Radius within which a straight bullet counts as hitting an enemy.
bulletHitRadius :: Float
bulletHitRadius = 18

-- | Impulse magnitude a bullet imparts to an enemy on hit.
knockbackStrength :: Float
knockbackStrength = 260

-- Helpers --------------------------------------------------------------------

onLand :: Vector2 -> Bool
onLand (Vector2 _ y) = y >= groundLevel

offScreen :: Vector2 -> Bool
offScreen (Vector2 x y) = x < -50 || x > 1330 || y < -50 || y > 770

-- | Cap a vector's magnitude.
clampMag :: Float -> Vector2 -> Vector2
clampMag maxLen v
  | magnitude v > maxLen = vectorNormalize v |* maxLen
  | otherwise            = v

-- | Steer toward a target point: ease velocity toward a desired velocity that
-- points at the target, capped at @speed@ (so it arrives and slows). For
-- physical entities (enemy legs).
steerToward :: Float -> Float -> Vector2 -> Script ()
steerToward responsiveness speed target = do
  me <- getMyPos
  steer responsiveness (clampMag speed (target |-| me))

-- | Kinematic seek: move toward a point at a constant speed, hard-setting
-- velocity (no easing, no forces), clamped so it lands exactly without
-- overshooting. For the scanbox reticle.
seekAt :: Float -> Vector2 -> Script ()
seekAt speed target = do
  me <- getMyPos
  dt <- getDt
  let step = clampMag (speed * dt) (target |-| me)
  setVel (if dt <= 0 then Vector2 0 0 else step |* (1 / dt))

-- Behaviours -----------------------------------------------------------------

-- | The turret: the scanbox (its box) tracks the crosshair, and every cooldown
-- it fires a ballistic bullet from the tower toward the scanbox centre. The
-- scanbox is a non-physical reticle (no gravity). Cooldown lives in its own
-- continuation.
turretBehaviour :: Script ()
turretBehaviour = turret 0
  where
    turret cooldown = do
      aim <- getAimPos
      seekAt scanSpeed aim                                  -- reticle: constant-speed kinematic tracking
      cooldown' <-
        if cooldown <= 0
          then do
            tower <- getTowerPos
            scan  <- getMyPos                                -- scanbox centre = where we point
            g     <- getGravity
            let Vector2 _ gy = g
                -- ballistic solution that lands on the scanbox; straight shot if out of range
                v = maybe (launchVel tower scan) id (launchToHit bulletSpeed gy tower scan)
            fire tower (StraightBullet v)
            pure fireInterval
          else pure cooldown
      dt <- getDt
      yield
      turret (cooldown' - dt)

-- | Straight-line launch velocity from the tower toward the scanbox (fallback
-- when no ballistic solution exists).
launchVel :: Vector2 -> Vector2 -> Vector2
launchVel tower scan
  | vectorDistance tower scan < 1 = Vector2 0 (-bulletSpeed)   -- degenerate: straight up
  | otherwise                     = vectorNormalize (scan |-| tower) |* bulletSpeed

-- | Ballistic firing solution: the launch velocity (of magnitude @speed@) that,
-- under downward gravity @g@, lands a projectile fired from @origin@ exactly on
-- @target@. Returns the flatter (lower-arc) of the two solutions, or Nothing if
-- the target is out of range.
launchToHit :: Float -> Float -> Vector2 -> Vector2 -> Maybe Vector2
launchToHit speed g (Vector2 ox oy) (Vector2 tx ty)
  | g <= 0    = Nothing
  | otherwise =
      -- Solve 0.25 g² u² - (g·dy + v²) u + (dx²+dy²) = 0 for u = T² (flight time²).
      let dx   = tx - ox
          dy   = ty - oy
          v2   = speed * speed
          a    = 0.25 * g * g
          b    = negate (g * dy + v2)
          c    = dx * dx + dy * dy
          disc = b * b - 4 * a * c
       in if disc < 0
            then Nothing
            else case filter (> 0) [(-b - sqrt disc) / (2 * a), (-b + sqrt disc) / (2 * a)] of
                   [] -> Nothing
                   us -> let t  = sqrt (minimum us)          -- smallest T → flattest, fastest arc
                             vx = dx / t
                             vy = (dy - 0.5 * g * t * t) / t
                          in Just (Vector2 vx vy)

-- | A paratrooper: while airborne the parachute steers it toward a capped
-- descent velocity (gravity pulls, the chute limits the fall); once landed it
-- walks toward the tower along the ground (its legs).
enemyBehaviour :: Script ()
enemyBehaviour = forever $ do
  myPos <- getMyPos
  if onLand myPos
    then do
      tower <- getTowerPos
      let Vector2 tx _ = tower
      steerToward enemyResponsiveness enemySpeed (Vector2 tx groundLevel)  -- legs: walk to the tower
    else steer parachuteResponsiveness (Vector2 0 parachuteSpeed)          -- parachute: capped descent
  yield

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
          push tid (knockback v)
          despawnSelf
        []
          | onLand me || offScreen me -> despawnSelf
          | otherwise                 -> yield >> fly

-- | Knockback impulse in the bullet's direction of travel.
knockback :: Vector2 -> Vector2
knockback v
  | magnitude v < 1 = Vector2 0 0
  | otherwise       = vectorNormalize v |* knockbackStrength

-- | Predict where to aim to hit a moving target: the intercept point given the
-- bullet's travel time. Kept for the future auto-aimer. Two fixed-point
-- iterations refine the estimate.
predictLead :: Vector2 -> Vector2 -> Vector2 -> Float -> Vector2
predictLead origin pos vel speed = pos |+| (vel |* travelTime)
  where
    t1         = vectorDistance origin pos / speed
    p1         = pos |+| (vel |* t1)
    travelTime = vectorDistance origin p1 / speed
