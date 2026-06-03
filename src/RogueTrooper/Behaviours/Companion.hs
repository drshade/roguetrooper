-- | Companion turret behaviours: autonomous guns placed beside the main turret.
-- Each one encodes its own target acquisition in its script. Content only.
module RogueTrooper.Behaviours.Companion
  ( missileCompanion
  , shotgunCompanion
  ) where

import           Raylib.Types                   (Vector2, pattern Vector2)
import           Raylib.Util.Math               (magnitude, vectorNormalize,
                                                 (|*), (|+|), (|-|))
import           RogueTrooper.Behaviours.Common (launchToward, nearestEnemy,
                                                 randFloat, randIndex)
import           RogueTrooper.Behaviours.Weapon (missileLaunchSpeed)
import           RogueTrooper.Script            (ProjectileType (..), Script,
                                                 fire, getDt, getEnemies,
                                                 getMyPos, yield)

-- | Cooldowns are longer than the main turret's (companions start weaker).
missileCooldown, shotgunCooldown :: Float
missileCooldown = 3.0
shotgunCooldown = 1.6

-- | Shotgun: a burst of pellets sprayed into a randomized oval cloud leaving
-- the muzzle.
shotgunPellets :: Int
shotgunPellets = 17

-- | Base muzzle velocity of the burst (pellets are then gravity-dropped).
shotgunSpeed :: Float
shotgunSpeed = 1500

-- | The muzzle "oval": half-extents of each pellet's random velocity offset,
-- perpendicular to (sideways fan) and along (forward/back, i.e. speed variation)
-- the firing direction, in px/s. Along > perp makes the cloud an oval elongated
-- in the direction of travel.
shotgunSpreadPerp, shotgunSpreadAlong :: Float
shotgunSpreadPerp  = 260
shotgunSpreadAlong = 520

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
                       (vels, seed'') = scatter seed dir shotgunPellets shotgunSpeed
                                                 shotgunSpreadPerp shotgunSpreadAlong
                   mapM_ (\v -> fire me (Pellet v)) vels
                   pure (shotgunCooldown, seed'')
                 Nothing -> pure (cooldown, seed)
          else pure (cooldown, seed)
      dt <- getDt
      yield
      run seed' (cooldown' - dt)

-- | @n@ randomized velocities forming an oval "muzzle cloud" around the base
-- shot @dir |* speed@. Each pellet draws a uniform-random point inside the unit
-- disk (polar sampling: angle, sqrt-radius) and offsets the base velocity by it,
-- scaled by @perp@ sideways and @along@ forward/back — so the pattern is an
-- irregular ellipse rather than a regular arc. Threads the RNG seed.
scatter :: Int -> Vector2 -> Int -> Float -> Float -> Float -> ([Vector2], Int)
scatter seed0 dir n speed perp along = go seed0 n []
  where
    Vector2 dx dy = dir
    perpDir       = Vector2 (negate dy) dx   -- unit vector 90° from the firing direction
    base          = dir |* speed
    go s 0 acc = (acc, s)
    go s k acc =
      let (u1, s1) = randFloat s
          (u2, s2) = randFloat s1
          ang = u1 * 2 * pi
          rad = sqrt u2                        -- sqrt → uniform over the disk's area
          ox  = rad * cos ang                  -- sideways component of the offset
          oy  = rad * sin ang                  -- along-axis component
          v   = base |+| (perpDir |* (ox * perp)) |+| (dir |* (oy * along))
       in go s2 (k - 1) (v : acc)
