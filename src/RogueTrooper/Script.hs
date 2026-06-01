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
  , seekTo
  , yield
  ) where

import Control.Monad.Free (Free, liftF)
import Raylib.Types       (Vector2)

-- | The instruction set. Grows on demand as behaviours need new verbs.
data ScriptF next
  = GetAimPos (Vector2 -> next)  -- ^ query: the current aim position (mouse)
  | SeekTo Vector2 next          -- ^ command: seek this entity's box toward a point
  | Yield next                   -- ^ suspend until the next frame
  deriving (Functor)

-- | A behaviour script: a free monad over 'ScriptF'.
type Script = Free ScriptF

-- | Query the current aim position (the mouse / aim box).
getAimPos :: Script Vector2
getAimPos = liftF (GetAimPos id)

-- | Command the engine to seek this entity's box toward a point this frame.
seekTo :: Vector2 -> Script ()
seekTo target = liftF (SeekTo target ())

-- | Suspend the script until the next frame.
yield :: Script ()
yield = liftF (Yield ())
