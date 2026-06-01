-- | Core pure data types for the game simulation.
module RogueTrooper.Types
  ( BoxShape (..)
  , Box (..)
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
