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
  , resolveTowerHits
  , step
  ) where

import Control.Monad.Free (Free (..))
import Raylib.Types        (Vector2)
import Raylib.Util.Math    (vectorDistance)
import RogueTrooper.Aim    (seekToward)
import RogueTrooper.Script (ScriptF (..))
import RogueTrooper.Types  (Box (..), Entity (..), GameState (..))

-- | An enemy within this distance of the tower counts as having reached it.
towerHitRadius :: Float
towerHitRadius = 28

-- | Read-only per-frame context available to scripts via queries.
data World = World
  { dt        :: Float    -- ^ seconds elapsed this frame
  , aimTarget :: Vector2  -- ^ current aim position (mouse)
  , towerPos  :: Vector2  -- ^ the defended tower's position
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
      Free (GetMyPos k)             -> go ent (k ent.box.center)
      Free (GetTowerPos k)          -> go ent (k world.towerPos)
      Free (MoveToward target next) -> go (moveEntity world.dt target ent) next

-- | Remove enemies that have reached the tower, dealing 1 HP of tower damage
-- per reaching enemy.
resolveTowerHits :: GameState -> GameState
resolveTowerHits gs = gs { enemies = survivors, towerHp = gs.towerHp - length hits }
  where
    (hits, survivors) = partition reached gs.enemies
    reached e = vectorDistance e.box.center gs.tower <= towerHitRadius

-- | Advance the whole simulation one frame: run the turret and every enemy
-- through their scripts.
step :: World -> GameState -> GameState
step world gs =
  resolveTowerHits $
    gs
      { turret  = runEntityFrame world gs.turret
      , enemies = map (runEntityFrame world) gs.enemies
      }
