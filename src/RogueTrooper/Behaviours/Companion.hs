-- | Companion turret behaviours: autonomous guns placed beside the main turret.
-- Each one encodes its own target acquisition in its script. Content only.
module RogueTrooper.Behaviours.Companion
  ( missileCompanion
  , shotgunCompanion
  , repulsorCompanion
  , boomerangCompanion
  , teslaCompanion
  ) where

import           Raylib.Types                   (Vector2, pattern Vector2)
import           Raylib.Util.Math               (magnitude, vectorDistance,
                                                 vectorNormalize, (|*), (|+|),
                                                 (|-|))
import           RogueTrooper.Behaviours.Common (launchToward, nearestEnemy,
                                                 randFloat, randIndex)
import           RogueTrooper.Behaviours.Weapon (missileLaunchSpeed)
import           RogueTrooper.Script            (ProjectileType (..), Script,
                                                 fire, getDt, getEnemies,
                                                 getMyPos, yield)

-- | Cooldowns are longer than the main turret's (companions start weaker).
missileCooldown, shotgunCooldown, repulsorCooldown, boomerangCooldown, teslaCooldown :: Float
missileCooldown   = 3.0
shotgunCooldown   = 1.6
repulsorCooldown  = 2.2
boomerangCooldown = 2.6
teslaCooldown     = 4.5

-- | How far the tesla coil can reach for its first strike (long — the chain's
-- hops then use the bolt's own, shorter, bounce range).
teslaRange :: Float
teslaRange = 750

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
-- launches a homing missile at it. The seed makes two of the same companion
-- behave independently.
missileCompanion :: Int -> Script ()
missileCompanion seed0 = run seed0 0
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
-- scattered burst of low-velocity bullets at it. The seed makes two of the same
-- companion scatter independently.
shotgunCompanion :: Int -> Script ()
shotgunCompanion seed0 = run seed0 0
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

-- | Repulsor-field companion: every cooldown (when enemies are present) it emits
-- an expanding repulsion wave from its position that knocks enemies back without
-- dealing damage.
repulsorCompanion :: Script ()
repulsorCompanion = run 0
  where
    run cooldown = do
      me <- getMyPos
      es <- getEnemies
      cooldown' <-
        if cooldown <= 0 && not (null es)
          then fire me (RepulsorWave me) >> pure repulsorCooldown
          else pure cooldown
      dt <- getDt
      yield
      run (cooldown' - dt)

-- | Boomerang companion: every cooldown (when enemies are present) it flings a
-- large boomerang in a random upward-ish direction with a random S-side. It does
-- not target a specific enemy — it's a wide sweep that carves whatever it crosses.
boomerangCompanion :: Script ()
boomerangCompanion = go 24680 0
  where
    go seed cooldown = do
      me <- getMyPos
      es <- getEnemies
      (cooldown', seed') <-
        if cooldown <= 0 && not (null es)
          then do
            let (u1, s1) = randFloat seed
                (u2, s2) = randFloat s1
                -- upper arc only (never near-horizontal or downward): ~[-0.85π, -0.15π]
                ang  = negate (0.15 * pi + u1 * 0.7 * pi)
                axis = Vector2 (cos ang) (sin ang)
                side = if u2 < 0.5 then 1 else -1
            fire me (Boomerang axis side)
            pure (boomerangCooldown, s2)
          else pure (cooldown, seed)
      dt <- getDt
      yield
      go seed' (cooldown' - dt)

-- | Tesla-coil companion: every (slow) cooldown it strikes a RANDOM enemy
-- within its long range with a lightning bolt, which then arcs between nearby
-- troopers on its own (see 'RogueTrooper.Behaviours.Weapon.teslaBolt'). The
-- seed makes two of the same companion pick targets independently.
teslaCompanion :: Int -> Script ()
teslaCompanion seed0 = run seed0 0
  where
    run seed cooldown = do
      me <- getMyPos
      es <- getEnemies
      let candidates = [e | e@(_, p) <- es, vectorDistance me p <= teslaRange]
      (cooldown', seed') <-
        if cooldown <= 0 && not (null candidates)
          then do
            let (i, seed'') = randIndex seed (length candidates)
                (tid, tpos) = candidates !! i
            fire tpos (TeslaBolt me tid [])
            pure (teslaCooldown, seed'')
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
