-- | The script interpreter and per-frame simulation. The engine gives meaning
-- to the "RogueTrooper.Script" DSL: it answers queries from the current state,
-- carries out commands (owning the physics), and suspends scripts at 'Yield',
-- storing the continuation to resume next frame.
module RogueTrooper.Engine
  ( ScriptInput (..)
  , seekTurretToward
  , runFrame
  , stepTurret
  ) where

import Control.Monad.Free (Free (..))
import Raylib.Types       (Vector2)
import RogueTrooper.Aim   (seekTurretBox)
import RogueTrooper.Script (Script, ScriptF (..))
import RogueTrooper.Types (Box (..), GameState (..))

-- | The per-frame inputs available to scripts via queries.
data ScriptInput = ScriptInput
  { dt        :: Float    -- ^ seconds elapsed this frame
  , aimTarget :: Vector2  -- ^ current aim position (mouse)
  }

-- | Carry out a @SeekTo@ command: move the entity's box toward @target@ by
-- @seekSpeed * dt@. The engine owns this physics; the script only expresses the
-- intent to seek.
seekTurretToward :: Float -> Vector2 -> GameState -> GameState
seekTurretToward dt' target gs = gs { turret = t' }
  where
    t  = gs.turret
    t' = t { center = seekTurretBox gs.seekSpeed dt' t.center target }

-- | Run a script for one frame: execute queries and commands (threading game
-- state) until the script suspends at 'Yield' or finishes, returning the new
-- state and the continuation to resume next frame.
runFrame :: ScriptInput -> GameState -> Script () -> (GameState, Script ())
runFrame input = go
  where
    go gs script = case script of
      Pure ()                   -> (gs, pure ())                       -- finished; stays finished
      Free (Yield next)         -> (gs, next)                          -- frame boundary
      Free (GetAimPos k)        -> go gs (k input.aimTarget)           -- answer the query, continue
      Free (SeekTo target next) -> go (seekTurretToward input.dt target gs) next

-- | Advance the turret one frame by running its stored script continuation.
stepTurret :: ScriptInput -> GameState -> GameState
stepTurret input gs =
  let (gs', resume) = runFrame input gs gs.turretScript
   in gs' { turretScript = resume }
