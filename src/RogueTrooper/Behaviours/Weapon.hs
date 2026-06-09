-- | The turret and its projectiles (weapon scripts). Content only.
module RogueTrooper.Behaviours.Weapon
  ( turretBehaviour
  , flamethrowerTurret
  , straightBullet
  , pellet
  , homingMissile
  , flameParticle
  , repulsorWave
  , boomerang
  , teslaBolt
  , chainTarget
  , teslaBounceRange
  , bulletSpeed
  , missileLaunchSpeed
  ) where

import           Raylib.Types                   (Vector2, pattern Vector2)
import           Raylib.Util.Math               (magnitude, vector2Rotate,
                                                 vectorDistance, vectorNormalize,
                                                 (|*), (|+|), (|-|))
import           RogueTrooper.Behaviours.Common (launchToHit, launchToward,
                                                 nearestEnemy, offScreen, onLand,
                                                 randFloat, seekAt)
import           RogueTrooper.Script            (EntityId, ProjectileType (..),
                                                 Script, damage, despawnSelf,
                                                 fire, getAimPos, getDt,
                                                 getEnemies, getEntityPos,
                                                 getGravity, getMyPos, getMyVel,
                                                 getTowerPos, push, setRadius,
                                                 setVel, steer, yield)

-- Turret ---------------------------------------------------------------------

-- | How fast the scanbox reticle tracks the crosshair (constant speed).
scanSpeed :: Float
scanSpeed = 900

-- | Seconds between turret shots (fire rate).
fireInterval :: Float
fireInterval = 0.3

-- | The main turret: the scanbox tracks the crosshair, and every cooldown it
-- fires its primary weapon (a ballistic straight bullet) at where the scanbox
-- points — player-aimed, no auto-lock. The scanbox is a non-physical reticle.
turretBehaviour :: Script ()
turretBehaviour = turret 0
  where
    turret cooldown = do
      aim <- getAimPos
      seekAt scanSpeed aim                                 -- reticle tracks the crosshair
      cooldown' <-
        if cooldown <= 0
          then do
            tower <- getTowerPos
            scan  <- getMyPos                              -- scanbox centre = where we point
            g     <- getGravity
            let Vector2 _ gy = g
                -- ballistic solution that lands on the scanbox; straight shot if out of range
                v = maybe (launchToward tower scan bulletSpeed) id (launchToHit bulletSpeed gy tower scan)
            fire tower (StraightBullet v)
            pure fireInterval
          else pure cooldown
      dt <- getDt
      yield
      turret (cooldown' - dt)

-- Flamethrower ---------------------------------------------------------------

-- | flameRange = how far particles reach on average (px); flameRangeJitter =
-- per-particle ± variation on that reach; flameSpeed = their muzzle speed;
-- flameArc = full cone width (radians); flameInterval = seconds between puffs;
-- flameParticlesPerPuff = particles emitted each puff.
flameRange, flameRangeJitter, flameSpeed, flameArc, flameInterval :: Float
flameRange       = 450
flameRangeJitter = 50
flameSpeed       = 700
flameArc         = 0.44              -- ~25°
flameInterval    = 0.035

flameParticlesPerPuff :: Int
flameParticlesPerPuff = 4

-- | Slowest particles leave at this fraction of 'flameSpeed' (varying speed
-- gives the cone depth and a ragged leading edge).
flameSpeedMin :: Float
flameSpeedMin = 0.7

-- | The flamethrower primary: the turret tracks the crosshair at the normal turn
-- speed and, on a fast cadence, sprays a puff of short-lived flame particles in a
-- wide cone toward where it points. Player-aimed; the seed varies the spread.
flamethrowerTurret :: Int -> Script ()
flamethrowerTurret seed0 = turret seed0 0
  where
    turret seed cooldown = do
      aim   <- getAimPos
      seekAt scanSpeed aim                                  -- same slow turn as the bullet turret
      (cooldown', seed') <-
        if cooldown <= 0
          then do
            tower <- getTowerPos
            scan  <- getMyPos
            let dir         = aimUnit (scan |-| tower)
                (parts, s') = flamePuff seed dir flameParticlesPerPuff
            mapM_ (\(v, r) -> fire tower (Flame v r)) parts
            pure (flameInterval, s')
          else pure (cooldown, seed)
      dt <- getDt
      yield
      turret seed' (cooldown' - dt)

    aimUnit d = if magnitude d < 1 then Vector2 0 (-1) else vectorNormalize d

-- | @n@ flame particles as (velocity, reach): each fanned within @flameArc@ of
-- @dir@, at a random speed in [flameSpeedMin, 1] × 'flameSpeed', and a random
-- reach of 'flameRange' ± 'flameRangeJitter'. Threads the RNG seed.
flamePuff :: Int -> Vector2 -> Int -> ([(Vector2, Float)], Int)
flamePuff seed0 dir n = go seed0 n []
  where
    go s 0 acc = (acc, s)
    go s k acc =
      let (u1, s1) = randFloat s
          (u2, s2) = randFloat s1
          (u3, s3) = randFloat s2
          ang = (u1 * 2 - 1) * (flameArc / 2)
          spd = flameSpeed * (flameSpeedMin + (1 - flameSpeedMin) * u2)
          rng = flameRange + (u3 * 2 - 1) * flameRangeJitter
       in go s3 (k - 1) ((vector2Rotate dir ang |* spd, rng) : acc)

-- | A flame particle: flies straight at its launch velocity (kinematic — no
-- gravity droop), burning the first enemy it touches for a little damage (no
-- knockback). Travels @range@ px (lifetime scaled to its own speed) then expires
-- — or expires off-screen.
flameParticle :: Vector2 -> Float -> Script ()
flameParticle vel reach = burn (reach / max 1 (magnitude vel))
  where
    burn life = do
      setVel vel                                           -- hold a straight line
      me <- getMyPos
      es <- getEnemies
      case [tid | (tid, p) <- es, vectorDistance me p <= flameHitRadius] of
        tid : _ -> damage tid flameDamage >> despawnSelf
        []
          | life <= 0 || offScreen me -> despawnSelf
          | otherwise                 -> do
              dt <- getDt
              yield
              burn (life - dt)

flameDamage :: Int
flameDamage = 1

flameHitRadius :: Float
flameHitRadius = 14

-- Straight bullet (a flatter, faster ballistic round) ------------------------

-- | Launch speed of straight bullets (used when the turret is equipped with them).
bulletSpeed :: Float
bulletSpeed = 1400

bulletDamage :: Int
bulletDamage = 2

bulletHitRadius :: Float
bulletHitRadius = 18

knockbackStrength :: Float
knockbackStrength = 260

-- | Shotgun pellet stats: smaller hit radius, less damage and knockback than a
-- primary bullet (there are several per burst).
pelletDamage :: Int
pelletDamage = 1

pelletHitRadius, pelletKnockback :: Float
pelletHitRadius = 10
pelletKnockback = 120

-- | A ballistic round (launched with a velocity, then curved by gravity).
-- Parametrised by its damage / hit radius / knockback so primary bullets and
-- shotgun pellets share the flight logic. Each frame it checks for an
-- overlapping enemy — emitting damage + knockback + despawn — and despawns on
-- hitting the ground or leaving the screen.
ballistic :: Int -> Float -> Float -> Script ()
ballistic dmg hitRadius knockback = fly
  where
    fly = do
      me <- getMyPos
      es <- getEnemies
      case [tid | (tid, p) <- es, vectorDistance me p <= hitRadius] of
        tid : _ -> do
          damage tid dmg
          v <- getMyVel
          push tid (impulseAlong knockback v)
          despawnSelf
        []
          | onLand me || offScreen me -> despawnSelf
          | otherwise                 -> yield >> fly

-- | The turret's primary round.
straightBullet :: Script ()
straightBullet = ballistic bulletDamage bulletHitRadius knockbackStrength

-- | A shotgun pellet: a smaller, weaker ballistic round.
pellet :: Script ()
pellet = ballistic pelletDamage pelletHitRadius pelletKnockback

-- Homing missile -------------------------------------------------------------

-- | missileSpeed = cruise (max) speed the propulsion builds to; missileLaunchSpeed
-- = the speed it pops out of the tube at; missileResponsiveness = how briskly it
-- accelerates / how hard it homes (lower = slower build, but more gravity droop:
-- droop ≈ gravity / missileResponsiveness).
missileSpeed, missileResponsiveness, missileLaunchSpeed, missileKnockback :: Float
missileSpeed = 750
missileResponsiveness = 3
missileLaunchSpeed = 10
missileKnockback = 380

missileDamage :: Int
missileDamage = 3

missileHitRadius :: Float
missileHitRadius = 20

-- | A homing missile: every frame its propulsion accelerates it toward a
-- constant cruise speed; the direction is the target while it's alive, otherwise
-- the missile's current heading (so it keeps powering on, just no longer
-- tracking). Detonates (damage + knockback) on any overlap; despawns on the
-- ground / off-screen.
homingMissile :: EntityId -> Script ()
homingMissile target = fly
  where
    fly = do
      me <- getMyPos
      es <- getEnemies
      case [tid | (tid, p) <- es, vectorDistance me p <= missileHitRadius] of
        tid : _ -> do
          damage tid missileDamage
          v <- getMyVel
          push tid (impulseAlong missileKnockback v)
          despawnSelf
        [] -> do
          mtp <- getEntityPos target
          v   <- getMyVel
          let dir = case mtp of
                      Just tp -> aimUnit me tp v   -- steer toward the live target
                      Nothing -> headingUnit v     -- no target: hold current heading
          steer missileResponsiveness (dir |* missileSpeed)   -- always accelerate to cruise
          if onLand me || offScreen me then despawnSelf else yield >> fly

    -- unit direction toward the target (falls back to heading if it's on top of us)
    aimUnit me tp v = let d = tp |-| me in if magnitude d < 1 then headingUnit v else vectorNormalize d
    -- unit heading from current velocity (up if essentially stationary)
    headingUnit v   = if magnitude v < 1 then Vector2 0 (-1) else vectorNormalize v

-- | An impulse of the given magnitude in the direction of travel @v@.
impulseAlong :: Float -> Vector2 -> Vector2
impulseAlong strength v
  | magnitude v < 1 = Vector2 0 0
  | otherwise       = vectorNormalize v |* strength

-- Repulsor wave --------------------------------------------------------------

-- | waveSpeed = how fast the wavefront expands (px/s); waveMaxRadius = how far it
-- reaches before fading; waveKnockback = how hard it shoves whatever it reaches.
waveSpeed, waveMaxRadius, waveKnockback :: Float
waveSpeed     = 900
waveMaxRadius = 550
waveKnockback = 1000

-- | A repulsion wave: a non-damaging pulse anchored at its origin whose radius
-- grows each frame. Any enemy the expanding front first reaches is shoved
-- radially outward exactly once (tracked in @hit@). Despawns at max radius.
-- Knockback and the drawn ring both use the fixed @origin@, so the wave's own
-- (ignored, gravity-bound) position never matters.
repulsorWave :: Vector2 -> Script ()
repulsorWave origin = expand 0 []
  where
    expand radius hit = do
      dt <- getDt
      let radius' = radius + waveSpeed * dt
      setRadius radius'                                   -- grow the visual / hit ring
      es <- getEnemies
      let fresh = [ (i, p) | (i, p) <- es, i `notElem` hit, vectorDistance origin p <= radius' ]
      mapM_ (\(i, p) -> push i (shoveFrom origin p waveKnockback)) fresh
      if radius' >= waveMaxRadius
        then despawnSelf
        else yield >> expand radius' (hit <> map fst fresh)

-- | An outward impulse of magnitude @strength@, pushing @target@ away from @from@.
shoveFrom :: Vector2 -> Vector2 -> Float -> Vector2
shoveFrom from target strength =
  let d = target |-| from
   in if magnitude d < 1 then Vector2 0 (negate strength) else vectorNormalize d |* strength

-- Tesla bolt -----------------------------------------------------------------

-- | teslaBounceRange = how far an arc can jump to the next trooper (px);
-- teslaHopDelay = seconds a segment waits before arcing onward; teslaArcLife =
-- how long each drawn arc lingers in total (a bit longer than the hop, so
-- consecutive arcs overlap on screen).
teslaBounceRange, teslaHopDelay, teslaArcLife :: Float
teslaBounceRange = 280
teslaHopDelay    = 0.1
teslaArcLife     = 0.22

teslaDamage :: Int
teslaDamage = 2

-- | How many times a chain may arc onward after the first strike.
teslaMaxBounces :: Int
teslaMaxBounces = 2

-- | The chain's next hop: the enemy nearest @from@ within @range@ that isn't
-- already in @excluded@ (Nothing ends the chain).
chainTarget :: Float -> [EntityId] -> Vector2 -> [(EntityId, Vector2)] -> Maybe (EntityId, Vector2)
chainTarget range excluded from es =
  nearestEnemy from [e | e@(i, p) <- es, i `notElem` excluded, vectorDistance from p <= range]

-- | One segment of a lightning chain, pinned where it struck. It damages its
-- target instantly (no knockback — crowd control is the repulsor's job); after
-- 'teslaHopDelay' it arcs onward to the nearest not-yet-struck enemy within
-- 'teslaBounceRange' (re-evaluated live, so a trooper that died mid-chain is
-- skipped) — until the chain has bounced 'teslaMaxBounces' times. @struck@ is
-- who earlier segments already hit. The segment lingers to 'teslaArcLife' so
-- consecutive arcs overlap, then despawns.
teslaBolt :: EntityId -> [EntityId] -> Script ()
teslaBolt target struck = do
  damage target teslaDamage
  linger teslaHopDelay
  when (length struck < teslaMaxBounces) $ do
    me <- getMyPos
    es <- getEnemies
    case chainTarget teslaBounceRange (target : struck) me es of
      Just (tid, tpos) -> fire tpos (TeslaBolt me tid (target : struck))
      Nothing          -> pure ()
  linger (teslaArcLife - teslaHopDelay)
  despawnSelf
  where
    -- like 'wait', but hard-holds position each frame (a bolt ignores gravity)
    linger remaining
      | remaining <= 0 = pure ()
      | otherwise = do
          setVel (Vector2 0 0)
          dt <- getDt
          yield
          linger (remaining - dt)

-- Boomerang ------------------------------------------------------------------

-- | boomerangForwardReach = how far it travels along its throw axis (px, max at
-- mid-flight); boomerangLateralReach = how far it swings perpendicular (the S);
-- boomerangFlightTime = seconds for the whole out-and-back; boomerangHitRadius =
-- its (large) hitbox; boomerangKnockback = how hard it flings what it carves.
boomerangForwardReach, boomerangLateralReach, boomerangFlightTime, boomerangHitRadius, boomerangKnockback :: Float
boomerangForwardReach = 600
boomerangLateralReach = 320
boomerangFlightTime   = 2.0
boomerangHitRadius    = 42
boomerangKnockback    = 320

boomerangDamage :: Int
boomerangDamage = 2

-- | A large boomerang flung along @axis@ that traces a wide S and returns to
-- where it was thrown. Position would be @axis·R·sin(πt) + perp·L·sin(2πt)@; we
-- emit its derivative as the velocity each frame (kinematic), so it integrates
-- to that path and lands back at the origin (both sines vanish at t=0 and t=T).
-- @side@ (±1) mirrors the S. It carves every enemy it sweeps over once (tracked
-- in @hit@): damage + knockback along its travel. Despawns when the flight ends.
boomerang :: Vector2 -> Float -> Script ()
boomerang axis side = fly 0 []
  where
    Vector2 ax ay = axis
    perp = Vector2 (negate ay * side) (ax * side)          -- axis rotated 90° (sign = S side)
    fly elapsed hit = do
      let ph  = elapsed / boomerangFlightTime
          fwd = axis |* (boomerangForwardReach * (pi / boomerangFlightTime) * cos (pi * ph))
          lat = perp |* (boomerangLateralReach * (2 * pi / boomerangFlightTime) * cos (2 * pi * ph))
          v   = fwd |+| lat
      setVel v                                             -- kinematic: drive the analytic path
      me <- getMyPos
      es <- getEnemies
      let fresh = [ (i, p) | (i, p) <- es, i `notElem` hit, vectorDistance me p <= boomerangHitRadius ]
      mapM_ (\(i, _) -> damage i boomerangDamage >> push i (impulseAlong boomerangKnockback v)) fresh
      dt <- getDt
      if elapsed + dt >= boomerangFlightTime
        then despawnSelf
        else yield >> fly (elapsed + dt) (hit <> map fst fresh)
