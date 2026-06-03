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
  , launchToward
  , nearestEnemy
  , lcg
  , randFloat
  , randIndex
  ) where

import           Raylib.Types        (Vector2, pattern Vector2)
import           Raylib.Util.Math    (magnitude, vectorDistance, vectorNormalize, (|*), (|-|))
import           RogueTrooper.Script (EntityId, Script, getDt, getMyPos, setVel, steer, yield)

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

-- | A velocity of magnitude @speed@ pointing from @from@ toward @to@ (up if they coincide).
launchToward :: Vector2 -> Vector2 -> Float -> Vector2
launchToward from to speed
  | vectorDistance from to < 1 = Vector2 0 (negate speed)
  | otherwise                  = vectorNormalize (to |-| from) |* speed

-- | The enemy nearest a point (id + position), if any.
nearestEnemy :: Vector2 -> [(EntityId, Vector2)] -> Maybe (EntityId, Vector2)
nearestEnemy _    [] = Nothing
nearestEnemy from es = Just (minimumBy (\(_, a) (_, b) -> compare (vectorDistance from a) (vectorDistance from b)) es)

-- | A deterministic LCG step (glibc constants), kept in [0, 2^31).
lcg :: Int -> Int
lcg s = (1103515245 * s + 12345) `mod` 2147483648

-- | Advance the seed and return a Float in [0, 1).
randFloat :: Int -> (Float, Int)
randFloat seed = let s = lcg seed in (fromIntegral (s `mod` 100000) / 100000, s)

-- | Advance the seed and return an index in [0, n).
randIndex :: Int -> Int -> (Int, Int)
randIndex seed n = let s = lcg seed in (s `mod` n, s)
