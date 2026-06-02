-- | The script interpreter and per-frame simulation.
--
-- The engine is a generic substrate. Each entity's script run is a pure
-- function to a list of 'Event's. 'step' folds every entity's events into the
-- world, then a single physics integration pass applies gravity and moves
-- everyone by their (persistent) velocity. The engine has no per-projectile
-- collision or lifetime logic.
module RogueTrooper.Engine
  ( World (..)
  , runEntityFrame
  , applyEvents
  , integrate
  , resolveTowerHits
  , spawnTick
  , step
  ) where

import           Control.Monad.Free (Free (..))
import qualified Data.Map.Strict    as Map
import           Raylib.Types        (Vector2, pattern Vector2)
import           Raylib.Util.Math    (vectorDistance, (|*), (|+|), (|-|))
import           RogueTrooper.Aim    (nearestInBox)
import           RogueTrooper.Script (ScriptF (..))
import           RogueTrooper.Types

-- | An enemy within this distance of the tower counts as having reached it.
towerHitRadius :: Float
towerHitRadius = 28

-- | Read-only per-frame context available to scripts via queries, plus the
-- content-provided factories and physics constants (so the engine imports no
-- content).
data World = World
  { dt           :: Float                              -- ^ seconds elapsed this frame
  , aimTarget    :: Vector2                            -- ^ current aim position (mouse)
  , towerPos     :: Vector2                            -- ^ the defended tower's position
  , gravity      :: Vector2                            -- ^ acceleration applied to airborne physical entities
  , groundLevel  :: Float                              -- ^ y of the ground line
  , enemyList    :: [(EntityId, Vector2, Vector2)]     -- ^ enemies visible to scripts (id, pos, velocity)
  , mkProjectile :: ProjectileType -> Vector2 -> Entity -- ^ content factory: type + origin -> projectile
  , mkEnemy      :: Vector2 -> Entity                  -- ^ content factory: position -> enemy
  }

-- | Run one entity's script for a single frame, producing the events it wants
-- applied. The entity is not modified here — movement becomes 'Steer'/'Impulse'
-- velocity events (integration moves it), 'Yield' stores the resumed
-- continuation, and the effect verbs become world events.
runEntityFrame :: World -> Entity -> [Event]
runEntityFrame world ent = go [] ent.script
  where
    go evs s = case s of
      Pure ()                       -> reverse (SetScript ent.eid (pure ()) : evs)
      Free (Yield next)             -> reverse (SetScript ent.eid next : evs)
      Free (GetDt k)                -> go evs (k world.dt)
      Free (GetAimPos k)            -> go evs (k world.aimTarget)
      Free (GetMyPos k)             -> go evs (k ent.box.center)
      Free (GetMyVel k)             -> go evs (k ent.vel)
      Free (GetMyId k)              -> go evs (k ent.eid)
      Free (GetTowerPos k)          -> go evs (k world.towerPos)
      Free (GetGravity k)           -> go evs (k world.gravity)
      Free (GetEnemies k)           -> go evs (k [(i, p) | (i, p, _) <- world.enemyList])
      Free (GetTargetInBox k)       -> go evs (k (targetInBox ent.box world.enemyList))
      Free (DoSteer resp tgt next)  -> go (Steer ent.eid resp tgt : evs) next
      Free (DoSetVel v next)        -> go (SetVel ent.eid v : evs) next
      Free (DoPush tid dv next)     -> go (Impulse tid dv : evs) next
      Free (Fire origin pt next)    -> go (Spawn pt origin : evs) next
      Free (Hit tid amt next)       -> go (Damage tid amt : evs) next
      Free (Expire tid next)        -> go (Despawn tid : evs) next

    targetInBox b enemies = nearestInBox b [((p, v), p) | (_, p, v) <- enemies]

-- | Fold a frame's events into the world: ease/impulse velocities, store
-- continuations, spawn projectiles (fresh ids), damage (kill + score enemies at
-- 0 HP), and despawn entities.
applyEvents :: Float -> (ProjectileType -> Vector2 -> Entity) -> [Event] -> GameState -> GameState
applyEvents dt' mk evs gs0 = foldl' apply gs0 evs
  where
    apply gs (Steer i resp tgt) =
      gs { entities = Map.adjust (\e -> e { vel = e.vel |+| ((tgt |-| e.vel) |* min 1 (resp * dt')) }) i gs.entities }
    apply gs (SetVel i v) =
      gs { entities = Map.adjust (\e -> e { vel = v }) i gs.entities }
    apply gs (Impulse i dv) =
      gs { entities = Map.adjust (\e -> e { vel = e.vel |+| dv }) i gs.entities }
    apply gs (SetScript i k) = gs { entities = Map.adjust (\e -> e { script = k }) i gs.entities }
    apply gs (Despawn i)     = gs { entities = Map.delete i gs.entities }
    apply gs (Spawn pt origin) =
      let i = EntityId gs.nextId
          e = (mk pt origin) { eid = i }
       in gs { entities = Map.insert i e gs.entities, nextId = gs.nextId + 1 }
    apply gs (Damage i amt) =
      case Map.lookup i gs.entities of
        Nothing -> gs
        Just e ->
          let e' = e { hp = e.hp - amt }
           in if e'.hp <= 0
                then gs { entities = Map.delete i gs.entities
                        , score    = gs.score + (if e.kind == Enemy then 1 else 0)
                        }
                else gs { entities = Map.insert i e' gs.entities }

-- | Physics integration: apply gravity to airborne physical entities, move
-- everyone by velocity, and rest landed enemies on the ground (clamp + stop
-- vertical motion). The turret/scanbox is a non-physical reticle — no gravity,
-- no ground clamp.
integrate :: Vector2 -> Float -> Float -> GameState -> GameState
integrate g ground dt' gs = gs { entities = Map.map move gs.entities }
  where
    move e =
      let Vector2 _ y       = e.box.center
          airborne          = e.kind /= Turret && y < ground
          v1                = if airborne then e.vel |+| (g |* dt') else e.vel
          Vector2 cx cy     = e.box.center |+| (v1 |* dt')
          (center', vel')
            | e.kind == Enemy && cy >= ground =
                let Vector2 vx _ = v1 in (Vector2 cx ground, Vector2 vx 0)  -- land + stop falling
            | otherwise = (Vector2 cx cy, v1)
       in e { box = e.box { center = center' }, vel = vel' }

-- | Remove enemies that have reached the tower, dealing 1 HP of tower damage each.
resolveTowerHits :: GameState -> GameState
resolveTowerHits gs =
  gs { entities = Map.difference gs.entities hits
     , towerHp  = gs.towerHp - Map.size hits
     }
  where
    hits      = Map.filter reached gs.entities
    reached e = e.kind == Enemy && vectorDistance e.box.center gs.tower <= towerHitRadius

-- | A simple deterministic LCG step (glibc constants).
nextSeed :: Int -> Int
nextSeed s = (1103515245 * s + 12345) `mod` 2147483648

-- | Spawn a new enemy at a random top-of-screen position when the spawn timer
-- elapses; otherwise count it down. Deterministic given the seed.
spawnTick :: World -> GameState -> GameState
spawnTick world gs
  | gs.spawnTimer - world.dt > 0 = gs { spawnTimer = gs.spawnTimer - world.dt }
  | otherwise =
      let s'    = nextSeed gs.seed
          x     = 50 + fromIntegral (s' `mod` 1180)   -- x in [50, 1230)
          i     = EntityId gs.nextId
          enemy = (world.mkEnemy (Vector2 x 0)) { eid = i }
       in gs { entities   = Map.insert i enemy gs.entities
             , nextId     = gs.nextId + 1
             , seed       = s'
             , spawnTimer = gs.spawnInterval
             }

-- | Advance the whole simulation one frame: collect every entity's events, fold
-- them into the world (velocity persists between frames), integrate physics,
-- then resolve tower hits and spawning.
step :: World -> GameState -> GameState
step world gs =
  let events = concatMap (runEntityFrame world) (Map.elems gs.entities)
      folded = applyEvents world.dt world.mkProjectile events gs
   in spawnTick world (resolveTowerHits (integrate world.gravity world.groundLevel world.dt folded))
