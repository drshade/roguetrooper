-- | Core pure data types for the game simulation.
module RogueTrooper.Types
  ( BoxShape (..)
  , Box (..)
  , GameState (..)
  ) where

import Raylib.Types (Vector2)

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

-- | The whole game simulation state. Grows as mechanics are added; currently
-- just the turret box, its seek speed, and the defended tower.
data GameState = GameState
  { turret    :: Box      -- ^ @center@ = current turret-box position; @shape@ = its region
  , seekSpeed :: Float    -- ^ units/sec the turret box seeks toward the aim box
  , tower     :: Vector2  -- ^ the defended position
  , towerHp   :: Int
  }
  deriving (Eq, Show)
