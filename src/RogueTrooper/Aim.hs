-- | Pure aiming logic: containment tests, finite-speed seeking, and target
-- selection. Geometry primitives are reused from @h-raylib@ (see
-- "Raylib.Util.Math" and "Raylib.Core.Shapes"); this module only composes them
-- into game-specific behaviour.
module RogueTrooper.Aim
  ( boxContains
  , seekToward
  , nearestInBox
  ) where

import           Raylib.Core.Shapes (checkCollisionPointCircle, checkCollisionPointRec)
import           Raylib.Types       (Vector2, pattern Rectangle, pattern Vector2)
import           Raylib.Util.Math   (magnitudeSqr, vectorMoveTowards, (|-|))
import           RogueTrooper.Types (Box (..), BoxShape (..))

-- | Is the given point inside the box?
--
-- Circle and rectangle use raylib's native collision checks; the oval has no
-- native point-in-ellipse test, so it uses the normalised ellipse inequality.
boxContains :: Box -> Vector2 -> Bool
boxContains box point = case box.shape of
  Circle r -> checkCollisionPointCircle point box.center r
  Rect hw hh ->
    let Vector2 cx cy = box.center
     in checkCollisionPointRec point (Rectangle (cx - hw) (cy - hh) (2 * hw) (2 * hh))
  Oval rx ry ->
    let Vector2 cx cy = box.center
        Vector2 px py = point
        dx = (px - cx) / rx
        dy = (py - cy) / ry
     in dx * dx + dy * dy <= 1

-- | Move a point toward a target at a finite speed, without overshooting.
-- Used for any entity's locomotion (turret seek, enemy advance).
-- Arguments: speed (units/sec), delta time (sec), current position, target.
seekToward :: Float -> Float -> Vector2 -> Vector2 -> Vector2
seekToward speed dt current target =
  vectorMoveTowards current target (speed * dt)

-- | The entity nearest the box centre among those inside the box, if any.
-- Ties are broken by list order (earliest wins).
nearestInBox :: Box -> [(a, Vector2)] -> Maybe a
nearestInBox box entities =
  case filter (boxContains box . snd) entities of
    []     -> Nothing
    inside -> Just (fst (foldl1 closer inside))
  where
    closer best cand
      | distSq (snd cand) < distSq (snd best) = cand
      | otherwise                             = best
    distSq p = magnitudeSqr (p |-| box.center)
