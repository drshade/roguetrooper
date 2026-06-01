-- | The script interpreter and per-frame simulation. The engine gives meaning
-- to the "RogueTrooper.Script" DSL: it answers queries from the current state,
-- carries out commands (owning the physics), and suspends scripts at 'Yield',
-- storing the continuation to resume next frame.
--
-- Scripts run in the context of a single 'Entity' (the one whose script is
-- executing) plus a read-only 'World' of per-frame state. The same interpreter
-- drives the turret and every enemy.
module RogueTrooper.Engine
  ( World (..)
  , moveEntity
  , runEntityFrame
  , step
  ) where

import Control.Monad.Free (Free (..))
import Raylib.Types       (Vector2)
import RogueTrooper.Aim   (seekToward)
import RogueTrooper.Script (Script, ScriptF (..))
import RogueTrooper.Types (Box (..), Entity (..), GameState (..))

-- | Read-only per-frame context available to scripts via queries.
data World = World
  { dt        :: Float    -- ^ seconds elapsed this frame
  , aimTarget :: Vector2  -- ^ current aim position (mouse)
  }

-- | Carry out a @MoveToward@ command: move the entity's box toward @target@ by
-- @entity.speed * dt@. The engine owns this physics; the script only expresses
-- the intent to move.
moveEntity :: Float -> Vector2 -> Entity -> Entity
moveEntity dt' target ent = ent { box = b' }
  where
    b  = ent.box
    b' = b { center = seekToward ent.speed dt' b.center target }

-- | Run one entity's script for a single frame: execute queries and commands
-- (updating the entity) until the script suspends at 'Yield' or finishes,
-- returning the entity with its resumed continuation stored back.
runEntityFrame :: World -> Entity -> Entity
runEntityFrame world ent0 = go ent0 ent0.script
  where
    go ent script' = case script' of
      Pure ()                       -> ent { script = pure () }       -- finished; stays finished
      Free (Yield next)             -> ent { script = next }          -- frame boundary
      Free (GetAimPos k)            -> go ent (k world.aimTarget)      -- answer the query, continue
      Free (MoveToward target next) -> go (moveEntity world.dt target ent) next

-- | Advance the whole simulation one frame: run the turret and every enemy
-- through their scripts.
step :: World -> GameState -> GameState
step world gs =
  gs
    { turret  = runEntityFrame world gs.turret
    , enemies = map (runEntityFrame world) gs.enemies
    }
