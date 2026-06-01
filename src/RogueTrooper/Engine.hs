-- | Pure per-frame simulation steps. Each takes the elapsed time and the
-- frame's inputs and returns a new 'GameState'; all IO (input reading,
-- rendering) lives in "RogueTrooper".
module RogueTrooper.Engine
  ( stepAim
  ) where

import Raylib.Types       (Vector2)
import RogueTrooper.Aim   (seekTurretBox)
import RogueTrooper.Types (Box (..), GameState (..))

-- | Advance the turret box one frame: seek it toward the aim target (the mouse)
-- at the turret's seek speed.
--
-- Arguments: delta time (sec), aim target (mouse position), current state.
stepAim :: Float -> Vector2 -> GameState -> GameState
stepAim dt aimTarget gs = gs { turret = turret' }
  where
    t       = gs.turret
    turret' = t { center = seekTurretBox gs.seekSpeed dt t.center aimTarget }
