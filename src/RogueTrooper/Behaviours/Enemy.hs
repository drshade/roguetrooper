-- | Enemy behaviour scripts. Content only.
module RogueTrooper.Behaviours.Enemy
  ( enemyBehaviour
  ) where

import           Raylib.Types        (pattern Vector2)
import           RogueTrooper.Behaviours.Common (groundLevel, onLand, steerToward)
import           RogueTrooper.Script (Script, getMyPos, getTowerPos, steer, yield)

-- | Enemy walk speed and how quickly its legs recover their desired velocity
-- (lower = knockback lingers longer).
enemySpeed, enemyResponsiveness :: Float
enemySpeed = 80
enemyResponsiveness = 6

-- | Parachute: the descent velocity an airborne enemy steers toward, and how
-- strongly. Terminal fall speed ≈ parachuteSpeed + gravity / parachuteResponsiveness.
parachuteSpeed, parachuteResponsiveness :: Float
parachuteSpeed = 70
parachuteResponsiveness = 40

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
