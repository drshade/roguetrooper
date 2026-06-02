module Prelude
  ( module RerebasePrelude
  ) where

-- 'yield' (thread yield) and 'magnitude' (Data.Complex) are hidden so the game
-- can use them for the scripting DSL and vector math (Raylib.Util.Math.magnitude).
import RerebasePrelude hiding (magnitude, yield)
