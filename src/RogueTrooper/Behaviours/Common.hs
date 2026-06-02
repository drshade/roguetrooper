-- | Shared behaviour helpers and world constants used across the weapon, enemy
-- and mission scripts. Content only — depends on the DSL, never the engine.
module RogueTrooper.Behaviours.Common
  ( groundLevel
  , onLand
  , offScreen
  , clampMag
  , steerToward
  , seekAt
  , wait
  , launchToHit
  ) where

import           Raylib.Types        (Vector2, pattern Vector2)
import           Raylib.Util.Math    (magnitude, vectorNormalize, (|*), (|-|))
import           RogueTrooper.Script (Script, getDt, getMyPos, setVel, steer, yield)

-- | The y coordinate of the ground line (raylib screen coords: y grows down).
groundLevel :: Float
groundLevel = 640

-- | Is this position on the ground?
onLand :: Vector2 -> Bool
onLand (Vector2 _ y) = y >= groundLevel

-- | Is a position outside the (margin-padded) screen?
offScreen :: Vector2 -> Bool
offScreen (Vector2 x y) = x < -50 || x > 1330 || y < -50 || y > 770

-- | Cap a vector's magnitude.
clampMag :: Float -> Vector2 -> Vector2
clampMag maxLen v
  | magnitude v > maxLen = vectorNormalize v |* maxLen
  | otherwise            = v

-- | Steer toward a target point: ease velocity toward a desired velocity that
-- points at the target, capped at @speed@ (so it arrives and slows). For
-- physical entities (enemy legs, missile homing).
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

-- | Suspend the script for a duration, yielding each frame (sequential wait —
-- built from getDt + yield; the resumable continuation carries the countdown).
wait :: Float -> Script ()
wait remaining
  | remaining <= 0 = pure ()
  | otherwise      = do
      dt <- getDt
      yield
      wait (remaining - dt)

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
                   us -> let t  = sqrt (minimum us)
                             vx = dx / t
                             vy = (dy - 0.5 * g * t * t) / t
                          in Just (Vector2 vx vy)
