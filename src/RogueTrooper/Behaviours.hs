-- | Entity behaviour scripts, authored purely in the "RogueTrooper.Script" DSL.
--
-- This module is content, not engine: it may only depend on the DSL, never on
-- the engine or raylib IO. Adding or changing behaviour here must not require
-- engine changes.
module RogueTrooper.Behaviours
  ( turretBehaviour
  ) where

import RogueTrooper.Script (Script, getAimPos, moveToward, yield)

-- | The turret: every frame, move toward the current aim position, then yield.
-- Purely reactive, expressed as a forever-loop over the DSL.
turretBehaviour :: Script ()
turretBehaviour = forever $ do
  aim <- getAimPos
  moveToward aim
  yield
