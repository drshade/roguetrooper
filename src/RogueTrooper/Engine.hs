-- | The script interpreter and per-frame simulation.
--
-- The engine is a generic substrate: it answers queries from world state, lets
-- scripts emit 'Effect's (spawn / damage / despawn), and folds those effects
-- back into the world. It has no per-projectile collision or lifetime logic —
-- a projectile script decides its own hits and when to despawn.
module RogueTrooper.Engine
  ( World (..)
  , moveEntity
  , runEntityFrame
  , applyEffects
  , resolveTowerHits
  , spawnTick
  , step
  ) where

import Control.Monad.Free (Free (..))
import Raylib.Types        (Vector2, pattern Vector2)
import Raylib.Util.Math    (vectorDistance, (|*), (|-|))
import RogueTrooper.Aim    (nearestInBox, seekToward)
import RogueTrooper.Script (ScriptF (..))
import RogueTrooper.Types

-- | An enemy within this distance of the tower counts as having reached it.
towerHitRadius :: Float
towerHitRadius = 28

-- | Read-only per-frame context available to scripts via queries, plus the
-- content-provided projectile factory (so the engine never imports behaviours).
data World = World
  { dt           :: Float                              -- ^ seconds elapsed this frame
  , aimTarget    :: Vector2                            -- ^ current aim position (mouse)
  , towerPos     :: Vector2                            -- ^ the defended tower's position
  , enemyList    :: [(EntityId, Vector2, Vector2)]     -- ^ enemies visible to scripts (id, pos, velocity)
  , mkProjectile :: ProjectileType -> Vector2 -> Bullet -- ^ content factory: type + origin -> bullet
  , mkEnemy      :: Vector2 -> Entity                  -- ^ content factory: position -> enemy
  }

-- | Carry out a movement: move the entity's box toward @target@ by
-- @speed * dt@, recording the resulting velocity.
moveEntity :: Float -> Float -> Vector2 -> Entity -> Entity
moveEntity speed dt' target ent = ent { box = b' { center = newCenter }, vel = v }
  where
    b'        = ent.box
    newCenter = seekToward speed dt' b'.center target
    v         = if dt' <= 0 then Vector2 0 0 else (newCenter |-| b'.center) |* (1 / dt')

-- | Run one entity's script for a single frame: execute queries and movement
-- (updating the entity) and accumulate emitted 'Effect's until the script
-- suspends at 'Yield' or finishes. Returns the entity (with its resumed
-- continuation) and the effects it wants applied to the world.
runEntityFrame :: World -> Entity -> (Entity, [Effect])
runEntityFrame world ent0 = go ent0 [] ent0.script
  where
    go ent effs s = case s of
      Pure ()                       -> (ent { script = pure () }, reverse effs)
      Free (Yield next)             -> (ent { script = next }, reverse effs)
      Free (GetDt k)                -> go ent effs (k world.dt)
      Free (GetAimPos k)            -> go ent effs (k world.aimTarget)
      Free (GetMyPos k)             -> go ent effs (k ent.box.center)
      Free (GetMyId k)              -> go ent effs (k ent.eid)
      Free (GetTowerPos k)          -> go ent effs (k world.towerPos)
      Free (GetEnemies k)           -> go ent effs (k [(i, p) | (i, p, _) <- world.enemyList])
      Free (GetTargetInBox k)       -> go ent effs (k (targetInBox ent.box world.enemyList))
      Free (MoveToward speed target next) -> go (moveEntity speed world.dt target ent) effs next
      Free (Fire origin pt next)    -> go ent (Spawn pt origin : effs) next
      Free (Hit tid amt next)       -> go ent (Damage tid amt : effs) next
      Free (Expire tid next)        -> go ent (Despawn tid : effs) next

    -- nearest enemy (position + velocity) inside a box (reusing the tested selector)
    targetInBox b enemies = nearestInBox b [((p, v), p) | (_, p, v) <- enemies]

-- | Fold a frame's emitted effects into the world: damage kills the named enemy
-- (and scores), despawn removes the named entity, spawn adds an assembled bullet
-- with a fresh id.
applyEffects :: (ProjectileType -> Vector2 -> Bullet) -> [Effect] -> GameState -> GameState
applyEffects mk effs gs0 = foldl' apply gs0 effs
  where
    apply gs (Damage tid amt) =
      let reduce e        = if e.eid == tid then e { hp = e.hp - amt } else e
          reduced         = map reduce gs.enemies
          (dead, alive)   = partition (\e -> e.hp <= 0) reduced
       in gs { enemies = alive, score = gs.score + length dead }
    apply gs (Despawn tid) =
      gs { enemies = filter (\e -> e.eid /= tid) gs.enemies
         , bullets = filter (\b -> b.entity.eid /= tid) gs.bullets
         }
    apply gs (Spawn pt origin) =
      let b0   = mk pt origin
          ent0 = b0.entity
          b    = b0 { entity = ent0 { eid = EntityId gs.nextId } }
       in gs { bullets = gs.bullets <> [b], nextId = gs.nextId + 1 }

-- | A simple deterministic LCG step (glibc constants).
nextSeed :: Int -> Int
nextSeed s = (1103515245 * s + 12345) `mod` 2147483648

-- | Spawn a new enemy at a random top-of-screen position when the spawn timer
-- elapses; otherwise count the timer down. Deterministic given the seed.
spawnTick :: World -> GameState -> GameState
spawnTick world gs
  | gs.spawnTimer - world.dt > 0 = gs { spawnTimer = gs.spawnTimer - world.dt }
  | otherwise =
      let s'    = nextSeed gs.seed
          x     = 50 + fromIntegral (s' `mod` 1180)   -- x in [50, 1230)
          enemy = (world.mkEnemy (Vector2 x 0)) { eid = EntityId gs.nextId }
       in gs { enemies    = enemy : gs.enemies
             , nextId     = gs.nextId + 1
             , seed       = s'
             , spawnTimer = gs.spawnInterval
             }

-- | Remove enemies that have reached the tower, dealing 1 HP of tower damage
-- per reaching enemy.
resolveTowerHits :: GameState -> GameState
resolveTowerHits gs = gs { enemies = survivors, towerHp = gs.towerHp - length hits }
  where
    (hits, survivors) = partition reached gs.enemies
    reached e = vectorDistance e.box.center gs.tower <= towerHitRadius

-- | Advance the whole simulation one frame: run every entity's script, collect
-- their effects, fold them into the world, then resolve tower hits.
step :: World -> GameState -> GameState
step world gs =
  let (turret', tEff) = runEntityFrame world gs.turret
      enemyR          = map (runEntityFrame world) gs.enemies
      bulletR         = map runBullet gs.bullets
      runBullet b     = let (e', eff) = runEntityFrame world b.entity in (b { entity = e' }, eff)
      effs            = tEff <> concatMap snd enemyR <> concatMap snd bulletR
      gs1             = gs { turret  = turret'
                           , enemies = map fst enemyR
                           , bullets = map fst bulletR
                           }
   in spawnTick world (resolveTowerHits (applyEffects world.mkProjectile effs gs1))
