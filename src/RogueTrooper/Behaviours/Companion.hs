-- | Companion turret behaviours: autonomous guns placed beside the main turret.
-- Each one encodes its own target acquisition in its script. Content only.
module RogueTrooper.Behaviours.Companion
  ( missileCompanion
  , shotgunCompanion
  ) where

import           Raylib.Types                   (Vector2, pattern Vector2)
import           Raylib.Util.Math               (magnitude, vector2Rotate, vectorNormalize,
                                                 (|*), (|-|))
import           RogueTrooper.Behaviours.Common (launchToward, nearestEnemy, randFloat, randIndex)
import           RogueTrooper.Behaviours.Weapon (missileLaunchSpeed)
import           RogueTrooper.Script            (ProjectileType (..), Script, fire, getDt,
                                                 getEnemies, getMyPos, yield)

-- | Cooldowns are longer than the main turret's (companions start weaker).
missileCooldown, shotgunCooldown :: Float
missileCooldown = 1.4
shotgunCooldown = 1.6

-- | Shotgun: a burst of low-velocity pellets (so they don't travel far) spread
-- around the aim direction.
shotgunPellets :: Int
shotgunPellets = 5

shotgunSpeed, shotgunSpread :: Float
shotgunSpeed  = 380          -- low velocity → short range
shotgunSpread = 0.35         -- radians (~20°)

-- | Homing-missile companion: every cooldown it picks a RANDOM live enemy and
-- launches a homing missile at it.
missileCompanion :: Script ()
missileCompanion = run 7777 0
  where
    run seed cooldown = do
      me <- getMyPos
      es <- getEnemies
      (cooldown', seed') <-
        if cooldown <= 0 && not (null es)
          then do
            let (i, seed'')  = randIndex seed (length es)
                (tid, tpos)  = es !! i
            fire me (HomingMissile tid (launchToward me tpos missileLaunchSpeed))
            pure (missileCooldown, seed'')
          else pure (cooldown, seed)
      dt <- getDt
      yield
      run seed' (cooldown' - dt)

-- | Shotgun companion: every cooldown it targets the CLOSEST enemy and fires a
-- scattered burst of low-velocity bullets at it.
shotgunCompanion :: Script ()
shotgunCompanion = run 3131 0
  where
    run seed cooldown = do
      me <- getMyPos
      es <- getEnemies
      (cooldown', seed') <-
        if cooldown <= 0
          then case nearestEnemy me es of
                 Just (_, tpos) -> do
                   let d   = tpos |-| me
                       dir = if magnitude d < 1 then Vector2 0 (-1) else vectorNormalize d
                       (vels, seed'') = scatter seed dir shotgunPellets shotgunSpeed shotgunSpread
                   mapM_ (\v -> fire me (StraightBullet v)) vels
                   pure (shotgunCooldown, seed'')
                 Nothing -> pure (cooldown, seed)
          else pure (cooldown, seed)
      dt <- getDt
      yield
      run seed' (cooldown' - dt)

-- | @n@ velocities of magnitude @speed@ around the unit direction @dir@, each
-- rotated by a random angle in [-spread, spread]. Threads the RNG seed.
scatter :: Int -> Vector2 -> Int -> Float -> Float -> ([Vector2], Int)
scatter seed0 dir n speed spread = go seed0 n []
  where
    go s 0 acc = (acc, s)
    go s k acc =
      let (f, s') = randFloat s
          ang     = (f * 2 - 1) * spread
       in go s' (k - 1) (vector2Rotate dir ang |* speed : acc)
