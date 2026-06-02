-- | The entity-behaviour DSL: a free-monad scripting language for describing
-- what an entity (the turret, enemies, projectiles, the wave director) wants to
-- do, decoupled from how the engine carries it out.
--
-- Instructions are queries (read world state into the script), commands (move),
-- effect-emitters (spawn / damage / despawn — applied by the engine), and
-- suspension ('yield', so behaviour can span frames). A script is stored as its
-- current continuation; the interpreter (in "RogueTrooper.Engine") runs it each
-- frame until it yields, then resumes it next frame.
module RogueTrooper.Script
  ( EntityId (..)
  , ProjectileType (..)
  , ScriptF (..)
  , Script
  , getDt
  , getAimPos
  , getMyPos
  , getMyVel
  , getMyId
  , getTowerPos
  , getEnemies
  , getTargetInBox
  , steer
  , setVel
  , push
  , fire
  , damage
  , despawn
  , despawnSelf
  , yield
  ) where

import Control.Monad.Free (Free, liftF)
import Raylib.Types       (Vector2)

-- | A stable identity for an entity, so scripts can refer to specific targets.
newtype EntityId = EntityId Int
  deriving (Eq, Ord, Show)

-- | The kinds of projectile the turret can fire. Each variant carries the data
-- its behaviour needs, and drives which script it runs and how it is rendered.
data ProjectileType
  = StraightBullet Vector2   -- ^ launched with this initial velocity (then ballistic under gravity)
  deriving (Eq, Show)

-- | The instruction set. Grows on demand as behaviours need new verbs.
data ScriptF next
  = GetDt (Float -> next)                     -- ^ query: seconds elapsed this frame
  | GetAimPos (Vector2 -> next)               -- ^ query: current aim position (mouse)
  | GetMyPos (Vector2 -> next)                -- ^ query: this entity's position
  | GetMyVel (Vector2 -> next)                -- ^ query: this entity's velocity
  | GetMyId (EntityId -> next)                -- ^ query: this entity's id
  | GetTowerPos (Vector2 -> next)             -- ^ query: the tower's position
  | GetEnemies ([(EntityId, Vector2)] -> next) -- ^ query: all enemies (id + position)
  | GetTargetInBox (Maybe (Vector2, Vector2) -> next) -- ^ query: nearest enemy in MY box (pos, velocity)
  | DoSteer Float Vector2 next                -- ^ command: ease my velocity toward a target (responsiveness, target velocity)
  | DoSetVel Vector2 next                     -- ^ command: hard-set my velocity (kinematic — no forces/easing)
  | DoPush EntityId Vector2 next              -- ^ effect: apply an impulse (Δv) to an entity
  | Fire Vector2 ProjectileType next          -- ^ effect: spawn a projectile from a given origin
  | Hit EntityId Int next                      -- ^ effect: deal N damage to the named enemy
  | Expire EntityId next                      -- ^ effect: remove the named entity
  | Yield next                                -- ^ suspend until the next frame
  deriving (Functor)

-- | A behaviour script: a free monad over 'ScriptF'.
type Script = Free ScriptF

-- | Query the seconds elapsed this frame.
getDt :: Script Float
getDt = liftF (GetDt id)

-- | Query the current aim position (the mouse / aim box).
getAimPos :: Script Vector2
getAimPos = liftF (GetAimPos id)

-- | Query this entity's own current position.
getMyPos :: Script Vector2
getMyPos = liftF (GetMyPos id)

-- | Query this entity's own current velocity.
getMyVel :: Script Vector2
getMyVel = liftF (GetMyVel id)

-- | Query this entity's own id.
getMyId :: Script EntityId
getMyId = liftF (GetMyId id)

-- | Query the defended tower's position.
getTowerPos :: Script Vector2
getTowerPos = liftF (GetTowerPos id)

-- | Query all enemies (id + position) currently in the world.
getEnemies :: Script [(EntityId, Vector2)]
getEnemies = liftF (GetEnemies id)

-- | Query the position and velocity of the nearest enemy inside this entity's
-- own box, if any.
getTargetInBox :: Script (Maybe (Vector2, Vector2))
getTargetInBox = liftF (GetTargetInBox id)

-- | Ease this entity's velocity toward a target velocity, at the given
-- responsiveness (1/sec). A soft pull — impulses (knockback) linger and recover.
steer :: Float -> Vector2 -> Script ()
steer responsiveness target = liftF (DoSteer responsiveness target ())

-- | Hard-set this entity's velocity for this frame (kinematic — bypasses forces
-- and easing). For non-physical entities like the scanbox reticle.
setVel :: Vector2 -> Script ()
setVel v = liftF (DoSetVel v ())

-- | Apply an impulse (instant change in velocity) to an entity. Used for knockback.
push :: EntityId -> Vector2 -> Script ()
push tid dv = liftF (DoPush tid dv ())

-- | Spawn a projectile of the given type from the given origin.
fire :: Vector2 -> ProjectileType -> Script ()
fire origin pt = liftF (Fire origin pt ())

-- | Deal @amount@ damage to the named enemy.
damage :: EntityId -> Int -> Script ()
damage tid amount = liftF (Hit tid amount ())

-- | Remove the named entity from the world.
despawn :: EntityId -> Script ()
despawn tid = liftF (Expire tid ())

-- | Remove this entity from the world.
despawnSelf :: Script ()
despawnSelf = getMyId >>= despawn

-- | Suspend the script until the next frame.
yield :: Script ()
yield = liftF (Yield ())
