-- | Core pure data types for the game simulation.
module RogueTrooper.Types
  ( BoxShape (..)
  , Box (..)
  , Entity (..)
  , GameState (..)
  ) where

import Raylib.Types       (Vector2)
import RogueTrooper.Script (Script)

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

-- | A scripted actor in the world: the turret and every enemy are entities.
-- @script@ is the entity's resumable behaviour continuation; the interpreter
-- runs it each frame and stores the resumed continuation back.
data Entity = Entity
  { box    :: Box        -- ^ @center@ = current position; @shape@ = its region
  , speed  :: Float      -- ^ units/sec for movement commands (seek/advance)
  , script :: Script ()  -- ^ resumable behaviour continuation
  }

-- | The whole game simulation state. Grows as mechanics are added.
--
-- No 'Eq'/'Show': entities hold resumable continuations (functions), so the
-- state is neither comparable nor showable as data. Tests assert on individual
-- fields instead.
data GameState = GameState
  { turret  :: Entity    -- ^ the player's turret
  , enemies :: [Entity]  -- ^ active enemies
  , tower   :: Vector2   -- ^ the defended position
  , towerHp :: Int
  }
