-- | The entity-behaviour DSL: a free-monad scripting language for describing
-- what an entity (the turret, enemies, the wave director) wants to do, decoupled
-- from how the engine carries it out.
--
-- Instructions split into three kinds:
--
--   * /queries/ read world state and feed it back into the script
--     (e.g. 'getAimPos'),
--   * /commands/ ask the engine to do something (e.g. 'seekTo'),
--   * /suspension/ ('yield') pauses the script until the next frame, so a
--     script can describe behaviour that spans many frames.
--
-- A script is stored as its current continuation; the interpreter (in
-- "RogueTrooper.Engine") runs it each frame until it yields, then resumes it
-- next frame. A purely reactive entity loops @forever (… >> yield)@; a scripted
-- sequence just runs straight through with 'yield'/waits between steps.
module RogueTrooper.Script
  ( ScriptF (..)
  , Script
  , getAimPos
  , getMyPos
  , getTowerPos
  , moveToward
  , yield
  ) where

import Control.Monad.Free (Free, liftF)
import Raylib.Types       (Vector2)

-- | The instruction set. Grows on demand as behaviours need new verbs.
data ScriptF next
  = GetAimPos (Vector2 -> next)   -- ^ query: the current aim position (mouse)
  | GetMyPos (Vector2 -> next)    -- ^ query: this entity's own position
  | GetTowerPos (Vector2 -> next) -- ^ query: the defended tower's position
  | MoveToward Vector2 next       -- ^ command: move this entity's box toward a point at its speed
  | Yield next                    -- ^ suspend until the next frame
  deriving (Functor)

-- | A behaviour script: a free monad over 'ScriptF'.
type Script = Free ScriptF

-- | Query the current aim position (the mouse / aim box).
getAimPos :: Script Vector2
getAimPos = liftF (GetAimPos id)

-- | Query this entity's own current position.
getMyPos :: Script Vector2
getMyPos = liftF (GetMyPos id)

-- | Query the defended tower's position.
getTowerPos :: Script Vector2
getTowerPos = liftF (GetTowerPos id)

-- | Command the engine to move this entity's box toward a point this frame
-- (at the entity's own speed).
moveToward :: Vector2 -> Script ()
moveToward target = liftF (MoveToward target ())

-- | Suspend the script until the next frame.
yield :: Script ()
yield = liftF (Yield ())
