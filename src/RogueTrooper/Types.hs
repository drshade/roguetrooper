-- | Core pure data types for the game simulation.
module RogueTrooper.Types
  ( BoxShape (..)
  , Box (..)
  , EntityId (..)
  , Entity (..)
  , ProjectileType (..)
  , Bullet (..)
  , Effect (..)
  , GameState (..)
  ) where

import Raylib.Types        (Vector2)
import RogueTrooper.Script (EntityId (..), ProjectileType (..), Script)

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

-- | A scripted actor in the world: the turret, every enemy, every bullet.
-- @script@ is the entity's resumable behaviour continuation; the interpreter
-- runs it each frame and stores the resumed continuation back.
data Entity = Entity
  { eid    :: EntityId   -- ^ stable identity
  , box    :: Box        -- ^ @center@ = current position; @shape@ = its region
  , vel    :: Vector2    -- ^ last frame's velocity (units/sec), set by the engine on move
  , hp     :: Int        -- ^ hit points; removed (and scored) when reduced to 0
  , script :: Script ()  -- ^ resumable behaviour continuation
  }

-- | A projectile in flight: its 'ProjectileType' (for rendering) plus the
-- 'Entity' that carries its position and behaviour.
data Bullet = Bullet
  { ptype  :: ProjectileType
  , entity :: Entity
  }

-- | A world effect emitted by a script and applied by the engine. The engine is
-- a generic fold over these; it has no per-projectile collision logic.
data Effect
  = Spawn ProjectileType Vector2  -- ^ spawn a projectile (origin injected by the interpreter)
  | Damage EntityId Int           -- ^ deal N damage to the identified enemy
  | Despawn EntityId              -- ^ remove the identified entity
  deriving (Eq, Show)

-- | The whole game simulation state. Grows as mechanics are added.
--
-- No 'Eq'/'Show': entities hold resumable continuations (functions), so the
-- state is neither comparable nor showable as data. Tests assert on individual
-- fields instead.
data GameState = GameState
  { turret        :: Entity    -- ^ the player's turret
  , enemies       :: [Entity]  -- ^ active enemies
  , bullets       :: [Bullet]  -- ^ projectiles in flight
  , tower         :: Vector2   -- ^ the defended position
  , towerHp       :: Int
  , score         :: Int       -- ^ enemy kill count
  , nextId        :: Int       -- ^ counter for assigning fresh 'EntityId's
  , spawnTimer    :: Float     -- ^ seconds until the next enemy spawn
  , spawnInterval :: Float     -- ^ seconds between enemy spawns
  , seed          :: Int       -- ^ RNG state for deterministic spawn positions
  }
