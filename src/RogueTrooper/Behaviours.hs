-- | Entity behaviour scripts, authored purely in the "RogueTrooper.Script" DSL.
--
-- This module is content, not engine: it may only depend on the DSL, never on
-- the engine or raylib IO. Adding or changing behaviour here must not require
-- engine changes.
module RogueTrooper.Behaviours
  ( turretBehaviour
  , enemyBehaviour
  , groundLevel
  ) where

import Raylib.Types     (Vector2, pattern Vector2)
import RogueTrooper.Script (Script, getAimPos, getMyPos, getTowerPos, moveToward, yield)

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
  yield

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
