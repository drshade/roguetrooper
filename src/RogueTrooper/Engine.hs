-- | The script interpreter and per-frame simulation.
--
-- The engine is a generic substrate. Each entity's script run is a pure
-- function to a list of 'Event's — it never mutates the entity (not even its
-- own position). 'step' folds every entity's events into the world, then a
-- single integration pass moves everyone by their velocity. The engine has no
-- per-projectile collision or lifetime logic.
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
import           RogueTrooper.Aim    (nearestInBox, seekToward)
import           RogueTrooper.Script (ScriptF (..))
import           RogueTrooper.Types

-- | An enemy within this distance of the tower counts as having reached it.
towerHitRadius :: Float
towerHitRadius = 28

-- | Read-only per-frame context available to scripts via queries, plus the
-- content-provided factories (so the engine never imports behaviours).
data World = World
  { dt           :: Float                              -- ^ seconds elapsed this frame
  , aimTarget    :: Vector2                            -- ^ current aim position (mouse)
  , towerPos     :: Vector2                            -- ^ the defended tower's position
  , enemyList    :: [(EntityId, Vector2, Vector2)]     -- ^ enemies visible to scripts (id, pos, velocity)
  , mkProjectile :: ProjectileType -> Vector2 -> Entity -- ^ content factory: type + origin -> projectile
  , mkEnemy      :: Vector2 -> Entity                  -- ^ content factory: position -> enemy
  }

-- | Run one entity's script for a single frame, producing the events it wants
-- applied. The entity is not modified here — @MoveToward@ becomes a 'SetVel'
-- (integration moves it later), 'Yield' stores the resumed continuation, and the
-- effect verbs become world events. 'getMyPos' therefore returns the frame-start
-- position throughout the run.
runEntityFrame :: World -> Entity -> [Event]
runEntityFrame world ent = go [] ent.script
  where
    go evs s = case s of
      Pure ()                             -> reverse (SetScript ent.eid (pure ()) : evs)
      Free (Yield next)                   -> reverse (SetScript ent.eid next : evs)
      Free (GetDt k)                      -> go evs (k world.dt)
      Free (GetAimPos k)                  -> go evs (k world.aimTarget)
      Free (GetMyPos k)                   -> go evs (k ent.box.center)
      Free (GetMyId k)                    -> go evs (k ent.eid)
      Free (GetTowerPos k)                -> go evs (k world.towerPos)
      Free (GetEnemies k)                 -> go evs (k [(i, p) | (i, p, _) <- world.enemyList])
      Free (GetTargetInBox k)             -> go evs (k (targetInBox ent.box world.enemyList))
      Free (MoveToward speed target next) -> go (SetVel ent.eid (velToward speed target) : evs) next
      Free (Fire origin pt next)          -> go (Spawn pt origin : evs) next
      Free (Hit tid amt next)             -> go (Damage tid amt : evs) next
      Free (Expire tid next)              -> go (Despawn tid : evs) next

    -- velocity that, integrated over this frame, lands at the (clamped) seek target
    velToward speed target =
      let pos  = ent.box.center
          newP = seekToward speed world.dt pos target
       in if world.dt <= 0 then Vector2 0 0 else (newP |-| pos) |* (1 / world.dt)

    targetInBox b enemies = nearestInBox b [((p, v), p) | (_, p, v) <- enemies]

-- | Fold a frame's events into the world: set velocities/continuations, spawn
-- projectiles (assigning fresh ids), deal damage (killing + scoring enemies at
-- 0 HP), and despawn entities.
applyEvents :: (ProjectileType -> Vector2 -> Entity) -> [Event] -> GameState -> GameState
applyEvents mk evs gs0 = foldl' apply gs0 evs
  where
    apply gs (SetVel i v)    = gs { entities = Map.adjust (\e -> e { vel = v }) i gs.entities }
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

-- | Move every entity by its current velocity (the only place positions change).
integrate :: Float -> GameState -> GameState
integrate dt' gs = gs { entities = Map.map move gs.entities }
  where
    move e = e { box = e.box { center = e.box.center |+| (e.vel |* dt') } }

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
-- them into the world (velocities reset first so a non-moving entity stops),
-- integrate positions, then resolve tower hits and spawning.
step :: World -> GameState -> GameState
step world gs =
  let events = concatMap (runEntityFrame world) (Map.elems gs.entities)
      zeroed = gs { entities = Map.map (\e -> e { vel = Vector2 0 0 }) gs.entities }
      folded = applyEvents world.mkProjectile events zeroed
   in spawnTick world (resolveTowerHits (integrate world.dt folded))
