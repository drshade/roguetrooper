-- | Core pure data types for the game simulation.
module RogueTrooper.Types
  ( BoxShape (..)
  , Box (..)
  , EntityId (..)
  , EntityKind (..)
  , Entity (..)
  , ProjectileType (..)
  , Event (..)
  , GameState (..)
  ) where

import qualified Data.Map.Strict   as Map
import           Raylib.Types        (Vector2)
import           RogueTrooper.Script (EntityId (..), ProjectileType (..), Script)

-- | The shape of a targeting region, centred on its position.
data BoxShape
  = Circle Float       -- ^ radius
  | Rect Float Float   -- ^ half-width, half-height
  | Oval Float Float   -- ^ x radius, y radius
  deriving (Eq, Show)

-- | A targeting region: a shape centred at a world position.
data Box = Box
  { center :: Vector2
  , shape  :: BoxShape
  }
  deriving (Eq, Show)

-- | What an entity is — drives rendering and which queries see it. Projectiles
-- carry their 'ProjectileType' (the old @Bullet@ wrapper is gone; a bullet is
-- just an entity).
data EntityKind
  = Turret               -- ^ the player's main turret (the scanbox reticle)
  | Companion            -- ^ an autonomous companion turret beside the main one
  | Enemy
  | Projectile ProjectileType
  | Director              -- ^ an invisible, non-physical script carrier (the mission)
  deriving (Eq, Show)

-- | A scripted actor in the world. Every entity — the turret, enemies, and
-- projectiles — is one of these, held in 'GameState.entities' keyed by id.
--
-- Scripts never mutate an entity directly: a frame's run produces 'Event's, and
-- the engine folds those back (including this entity's velocity and resumed
-- continuation). Positions change only in the integration pass.
data Entity = Entity
  { eid    :: EntityId   -- ^ stable identity
  , kind   :: EntityKind -- ^ what it is (drives rendering / queries)
  , box    :: Box        -- ^ @center@ = current position; @shape@ = its region
  , vel    :: Vector2    -- ^ velocity (units/sec) for this frame's integration
  , hp     :: Int        -- ^ hit points; removed (and scored) when reduced to 0
  , script :: Script ()  -- ^ resumable behaviour continuation
  }

-- | A change a script asks the engine to make this frame. @SetVel@ and
-- @SetScript@ are self-targeted (the entity's own velocity and resumed
-- continuation); the rest affect the wider world. The engine folds a frame's
-- whole event list into the 'GameState' sequentially.
data Event
  = Steer EntityId Float Vector2  -- ^ ease this entity's velocity toward a target (responsiveness, target)
  | SetVel EntityId Vector2       -- ^ hard-set this entity's velocity (kinematic)
  | Resize EntityId Float         -- ^ set this entity's hitbox to a circle of the given radius
  | Impulse EntityId Vector2      -- ^ add a Δv to this entity's velocity (knockback)
  | SetScript EntityId (Script ()) -- ^ store this entity's resumed continuation
  | Spawn ProjectileType Vector2  -- ^ spawn a projectile (engine assembles + assigns id)
  | SpawnEnemy Vector2            -- ^ spawn an enemy at a position (via the enemy factory)
  | Damage EntityId Int           -- ^ deal N damage to the identified entity
  | Despawn EntityId              -- ^ remove the identified entity

-- | The whole game simulation state. Every actor lives in 'entities'; the tower
-- (the defended point) is not a scripted entity, so it stays separate.
--
-- No 'Eq'/'Show': entities hold resumable continuations (functions).
data GameState = GameState
  { entities :: Map.Map EntityId Entity  -- ^ all actors, keyed by id
  , tower    :: Vector2                  -- ^ the defended position
  , towerHp  :: Int
  , score    :: Int                      -- ^ enemy kill count
  , nextId   :: Int                      -- ^ counter for assigning fresh ids
  }
