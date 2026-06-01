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
  , step
  ) where

import Control.Monad.Free (Free (..))
import Raylib.Types        (Vector2)
import Raylib.Util.Math    (vectorDistance)
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
  , enemyList    :: [(EntityId, Vector2)]              -- ^ enemies visible to scripts
  , mkProjectile :: ProjectileType -> Vector2 -> Bullet -- ^ content factory: type + origin -> bullet
  }

-- | Carry out a movement: move the entity's box toward @target@ by
-- @entity.speed * dt@.
moveEntity :: Float -> Vector2 -> Entity -> Entity
moveEntity dt' target ent = ent { box = b' }
  where
    b  = ent.box
    b' = b { center = seekToward ent.speed dt' b.center target }

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
      Free (GetAimPos k)            -> go ent effs (k world.aimTarget)
      Free (GetMyPos k)             -> go ent effs (k ent.box.center)
      Free (GetMyId k)              -> go ent effs (k ent.eid)
      Free (GetTowerPos k)          -> go ent effs (k world.towerPos)
      Free (GetEnemies k)           -> go ent effs (k world.enemyList)
      Free (GetTargetInBox k)       -> go ent effs (k (targetInBox ent.box world.enemyList))
      Free (MoveToward target next) -> go (moveEntity world.dt target ent) effs next
      Free (Fire pt next)           -> go ent (Spawn pt ent.box.center : effs) next
      Free (Hit tid next)           -> go ent (Damage tid : effs) next
      Free (Expire tid next)        -> go ent (Despawn tid : effs) next

    -- nearest enemy position inside a box (reusing the tested selector)
    targetInBox b enemies = nearestInBox b [(p, p) | (_, p) <- enemies]

-- | Fold a frame's emitted effects into the world: damage kills the named enemy
-- (and scores), despawn removes the named entity, spawn adds an assembled bullet
-- with a fresh id.
applyEffects :: (ProjectileType -> Vector2 -> Bullet) -> [Effect] -> GameState -> GameState
applyEffects mk effs gs0 = foldl' apply gs0 effs
  where
    apply gs (Damage tid) =
      let (killed, survivors) = partition (\e -> e.eid == tid) gs.enemies
       in gs { enemies = survivors, score = gs.score + length killed }
    apply gs (Despawn tid) =
      gs { enemies = filter (\e -> e.eid /= tid) gs.enemies
         , bullets = filter (\b -> b.entity.eid /= tid) gs.bullets
         }
    apply gs (Spawn pt origin) =
      let b0   = mk pt origin
          ent0 = b0.entity
          b    = b0 { entity = ent0 { eid = EntityId gs.nextId } }
       in gs { bullets = gs.bullets <> [b], nextId = gs.nextId + 1 }

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
   in resolveTowerHits (applyEffects world.mkProjectile effs gs1)
