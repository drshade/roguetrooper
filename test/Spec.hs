module Main where

import qualified Data.Map.Strict     as Map
import           RogueTrooper.Aim        (boxContains, nearestInBox, seekToward)
import           RogueTrooper.Behaviours (boomerang, boomerangCompanion, chainTarget,
                                          enemyBehaviour, flameParticle, flamethrowerTurret,
                                          groundLevel, homingMissile, launchToHit,
                                          missileCompanion, missionDirector, nearestEnemy,
                                          pellet, repulsorCompanion, repulsorWave,
                                          shotgunCompanion, straightBullet, teslaBolt,
                                          teslaCompanion, turretBehaviour, wait)
import           RogueTrooper.Engine     (World (..), applyEvents, integrate,
                                          resolveTowerHits, step)
import           RogueTrooper.Script     (Script, spawnEnemyAt)
import           RogueTrooper.Types       (Box (..), BoxShape (..), Entity (..),
                                          EntityId (..), EntityKind (..), Event (..),
                                          GameState (..), ProjectileType (..))
import           Raylib.Types            (Vector2, pattern Vector2)
import           Test.Hspec

-- | Assert two vectors are equal within a small tolerance.
shouldBeCloseTo :: Vector2 -> Vector2 -> Expectation
shouldBeCloseTo (Vector2 ax ay) (Vector2 bx by) =
  (abs (ax - bx) < eps && abs (ay - by) < eps) `shouldBe` True
  where
    eps = 1e-4

-- Entity builders ------------------------------------------------------------

mkEnemyAt :: EntityId -> Vector2 -> Entity
mkEnemyAt i p = Entity i Enemy (Box p (Circle 12)) (Vector2 0 0) 3 enemyBehaviour

turretAt :: EntityId -> Vector2 -> Float -> Entity
turretAt i p r = Entity i Turret (Box p (Circle r)) (Vector2 0 0) 1 turretBehaviour

bulletAt :: EntityId -> Vector2 -> Vector2 -> Entity
bulletAt i p v = Entity i (Projectile (StraightBullet v)) (Box p (Circle 4)) v 1 straightBullet

pelletAt :: EntityId -> Vector2 -> Vector2 -> Entity
pelletAt i p v = Entity i (Projectile (Pellet v)) (Box p (Circle 2)) v 1 pellet

directorWith :: Script () -> Entity
directorWith scr = Entity (EntityId 5) Director (Box (Vector2 0 0) (Circle 0)) (Vector2 0 0) 1 scr

companionAt :: EntityId -> Vector2 -> Script () -> Entity
companionAt i p scr = Entity i Companion (Box p (Circle 10)) (Vector2 0 0) 1 scr

testProjectile :: ProjectileType -> Vector2 -> Entity
testProjectile pt origin = case pt of
  StraightBullet v    -> Entity (EntityId 0) (Projectile pt) (Box origin (Circle 4)) v 1 straightBullet
  Pellet v            -> Entity (EntityId 0) (Projectile pt) (Box origin (Circle 2)) v 1 pellet
  HomingMissile tid v -> Entity (EntityId 0) (Projectile pt) (Box origin (Circle 6)) v 1 (homingMissile tid)
  RepulsorWave o      -> Entity (EntityId 0) (Projectile pt) (Box origin (Circle 0)) (Vector2 0 0) 1 (repulsorWave o)
  Flame v r           -> Entity (EntityId 0) (Projectile pt) (Box origin (Circle 5)) v 1 (flameParticle v r)
  Boomerang axis side -> Entity (EntityId 0) (Projectile pt) (Box origin (Circle 42)) (Vector2 0 0) 1 (boomerang axis side)
  TeslaBolt _ tid struck -> Entity (EntityId 0) (Projectile pt) (Box origin (Circle 6)) (Vector2 0 0) 1 (teslaBolt tid struck)

testEnemyFactory :: Vector2 -> Entity
testEnemyFactory p = Entity (EntityId 0) Enemy (Box p (Circle 12)) (Vector2 0 0) 3 enemyBehaviour

-- GameState / World builders -------------------------------------------------

mkGameState :: [Entity] -> Vector2 -> Int -> GameState
mkGameState es towerP hp =
  GameState { entities = Map.fromList [(e.eid, e) | e <- es]
            , tower = towerP, towerHp = hp, score = 0, nextId = 100 }

testWorld :: Float -> Vector2 -> Vector2 -> World
testWorld dt' aim towerP =
  World { dt = dt', aimTarget = aim, towerPos = towerP
        , gravity = Vector2 0 1000, groundLevel = groundLevel, enemyList = []
        , mkProjectile = testProjectile, mkEnemy = testEnemyFactory }

-- Accessors / helpers --------------------------------------------------------

centerOf :: EntityId -> GameState -> Vector2
centerOf i gs = case Map.lookup i gs.entities of
  Just e  -> e.box.center
  Nothing -> error ("entity missing: " <> show i)

velOf :: EntityId -> GameState -> Maybe Vector2
velOf i gs = (.vel) <$> Map.lookup i gs.entities

enemiesOf :: GameState -> [Entity]
enemiesOf gs = [e | e <- Map.elems gs.entities, e.kind == Enemy]

projectilesOf :: GameState -> [Entity]
projectilesOf gs = [e | e <- Map.elems gs.entities, isProjectile e.kind]
  where
    isProjectile (Projectile _) = True
    isProjectile _              = False

stepN :: World -> Int -> GameState -> GameState
stepN w n gs = iterate (step w) gs !! n

main :: IO ()
main = hspec $ do
  describe "boxContains" $ do
    describe "Circle" $ do
      it "contains a point inside the radius" $
        boxContains (Box (Vector2 0 0) (Circle 5)) (Vector2 3 0) `shouldBe` True
      it "excludes a point outside the radius" $
        boxContains (Box (Vector2 0 0) (Circle 5)) (Vector2 6 0) `shouldBe` False
    describe "Rect (half-width, half-height)" $ do
      it "contains a point within both half-extents" $
        boxContains (Box (Vector2 0 0) (Rect 4 2)) (Vector2 3 1) `shouldBe` True
      it "excludes a point beyond the height half-extent" $
        boxContains (Box (Vector2 0 0) (Rect 4 2)) (Vector2 0 3) `shouldBe` False
    describe "Oval (x radius, y radius)" $ do
      it "contains a point inside the ellipse" $
        boxContains (Box (Vector2 0 0) (Oval 4 2)) (Vector2 1 1) `shouldBe` True
      it "excludes a point outside the ellipse" $
        boxContains (Box (Vector2 0 0) (Oval 4 2)) (Vector2 3 1.5) `shouldBe` False

  describe "seekToward" $ do
    it "moves toward the target by speed*dt when the target is far" $
      seekToward 10 0.3 (Vector2 0 0) (Vector2 10 0) `shouldBeCloseTo` Vector2 3 0
    it "clamps to the target without overshooting when within reach" $
      seekToward 100 1 (Vector2 0 0) (Vector2 2 0) `shouldBe` Vector2 2 0

  describe "nearestInBox" $ do
    let box = Box (Vector2 0 0) (Circle 5)
    it "returns Nothing when nothing is inside the box" $
      nearestInBox box [(1 :: Int, Vector2 10 0)] `shouldBe` Nothing
    it "returns the entity nearest the box centre" $
      nearestInBox box [(1 :: Int, Vector2 4 0), (2, Vector2 1 0), (3, Vector2 3 0)] `shouldBe` Just 2
    it "breaks ties by list order (earliest wins)" $
      nearestInBox box [(1 :: Int, Vector2 2 0), (2, Vector2 0 2)] `shouldBe` Just 1

  describe "launchToHit (ballistic firing solution)" $ do
    it "produces a launch that lands exactly on the target under gravity" $
      case launchToHit 40 10 (Vector2 0 0) (Vector2 100 50) of
        Just (Vector2 vx vy) ->
          let t = 100 / vx
              y = vy * t + 0.5 * 10 * t * t
           in abs (y - 50) `shouldSatisfy` (< 0.01)
        Nothing -> expectationFailure "expected a firing solution"
    it "returns Nothing when the target is out of range" $
      launchToHit 5 10 (Vector2 0 0) (Vector2 100000 0) `shouldBe` Nothing

  describe "integrate" $ do
    let g = Vector2 0 1000 :: Vector2
    it "moves a non-physical entity by velocity * dt (no gravity)" $ do
      let e   = (turretAt (EntityId 1) (Vector2 0 0) 10) { vel = Vector2 100 50 }
          gs' = integrate g groundLevel 0.1 (mkGameState [e] (Vector2 0 0) 10)
      centerOf (EntityId 1) gs' `shouldBeCloseTo` Vector2 10 5
    it "applies gravity to an airborne enemy (it falls)" $ do
      let e   = mkEnemyAt (EntityId 1) (Vector2 0 100)
          gs' = integrate g groundLevel 0.1 (mkGameState [e] (Vector2 0 0) 10)
      centerOf (EntityId 1) gs' `shouldBeCloseTo` Vector2 0 110
    it "lands an enemy on the ground, clamping it and stopping the fall" $ do
      let e   = (mkEnemyAt (EntityId 1) (Vector2 0 635)) { vel = Vector2 0 1000 }
          gs' = integrate g groundLevel 0.1 (mkGameState [e] (Vector2 0 0) 10)
      centerOf (EntityId 1) gs' `shouldBeCloseTo` Vector2 0 groundLevel
      velOf (EntityId 1) gs' `shouldBe` Just (Vector2 0 0)

  describe "applyEvents" $ do
    let world = testWorld 0.1 (Vector2 0 0) (Vector2 0 0)
        e1 = mkEnemyAt (EntityId 1) (Vector2 100 100)
        e2 = mkEnemyAt (EntityId 2) (Vector2 200 200)
        gs = mkGameState [e1, e2] (Vector2 0 0) 10
    it "Impulse adds to an entity's velocity" $
      velOf (EntityId 1) (applyEvents world [Impulse (EntityId 1) (Vector2 5 0)] gs)
        `shouldBe` Just (Vector2 5 0)
    it "SetVel hard-sets an entity's velocity" $
      velOf (EntityId 1) (applyEvents world [SetVel (EntityId 1) (Vector2 9 9)] gs)
        `shouldBe` Just (Vector2 9 9)
    it "Steer eases velocity toward the target (full step at responsiveness*dt >= 1)" $
      velOf (EntityId 1) (applyEvents world [Steer (EntityId 1) 10 (Vector2 100 0)] gs)
        `shouldBe` Just (Vector2 100 0)
    it "lethal Damage kills the named enemy and scores" $ do
      let gs' = applyEvents world [Damage (EntityId 1) 3] gs
      map (.eid) (enemiesOf gs') `shouldBe` [EntityId 2]
      gs'.score `shouldBe` 1
    it "non-lethal Damage reduces HP without scoring" $ do
      let gs' = applyEvents world [Damage (EntityId 1) 1] gs
      ((.hp) <$> Map.lookup (EntityId 1) gs'.entities) `shouldBe` Just 2
      gs'.score `shouldBe` 0
    it "Despawn removes the named entity" $
      Map.member (EntityId 2) (applyEvents world [Despawn (EntityId 2)] gs).entities `shouldBe` False
    it "Spawn adds a projectile with a fresh id" $ do
      let gs' = applyEvents world [Spawn (StraightBullet (Vector2 5 5)) (Vector2 0 0)] (mkGameState [] (Vector2 0 0) 10)
      length (projectilesOf gs') `shouldBe` 1
      gs'.nextId `shouldBe` 101
    it "SpawnEnemy adds an enemy via the factory" $
      length (enemiesOf (applyEvents world [SpawnEnemy (Vector2 300 0)] (mkGameState [] (Vector2 0 0) 10))) `shouldBe` 1

  describe "turret scanbox" $ do
    let gs0   = mkGameState [turretAt (EntityId 0) (Vector2 0 0) 10] (Vector2 0 0) 10
        world = testWorld 0.1 (Vector2 100 0) (Vector2 0 0)
    it "tracks the crosshair (moves toward it, no gravity)" $
      centerOf (EntityId 0) (step world gs0) `shouldSatisfy` (\(Vector2 x y) -> x > 0 && y == 0)

  describe "enemy paratrooper" $ do
    let towerP = Vector2 640 620
        world  = testWorld 0.1 (Vector2 0 0) towerP
    it "falls under gravity while airborne" $
      centerOf (EntityId 1) (step world (mkGameState [mkEnemyAt (EntityId 1) (Vector2 300 100)] towerP 10))
        `shouldSatisfy` (\(Vector2 x y) -> x == 300 && y > 100)
    it "walks toward the tower once on the ground" $
      centerOf (EntityId 1) (step world (mkGameState [mkEnemyAt (EntityId 1) (Vector2 300 groundLevel)] towerP 10))
        `shouldSatisfy` (\(Vector2 x _) -> x > 300)

  describe "resolveTowerHits" $
    it "removes an enemy that reached the tower and deals 1 HP" $ do
      let towerP = Vector2 640 620
          gs'    = resolveTowerHits (mkGameState [mkEnemyAt (EntityId 1) towerP] towerP 10)
      length (enemiesOf gs') `shouldBe` 0
      gs'.towerHp `shouldBe` 9

  describe "straightBullet" $ do
    let enemyP       = Vector2 500 500
        towerP       = Vector2 640 620
        worldWith es = (testWorld 0.016 (Vector2 0 0) towerP) { enemyList = es }
    it "damages an overlapping enemy (non-lethal single hit) and despawns itself" $ do
      let gs  = mkGameState [mkEnemyAt (EntityId 1) enemyP, bulletAt (EntityId 2) enemyP (Vector2 100 0)] towerP 10
          gs' = step (worldWith [(EntityId 1, enemyP, Vector2 0 0)]) gs
      ((.hp) <$> Map.lookup (EntityId 1) gs'.entities) `shouldBe` Just 1
      gs'.score `shouldBe` 0
      Map.member (EntityId 2) gs'.entities `shouldBe` False
    it "despawns when it hits the ground" $ do
      let gs  = mkGameState [bulletAt (EntityId 2) (Vector2 100 650) (Vector2 0 0)] towerP 10
          gs' = step (worldWith []) gs
      Map.member (EntityId 2) gs'.entities `shouldBe` False

  describe "pellet (shotgun round)" $ do
    let enemyP       = Vector2 500 500
        towerP       = Vector2 640 620
        worldWith es = (testWorld 0.016 (Vector2 0 0) towerP) { enemyList = es }
    it "deals only 1 damage on hit (weaker than a primary bullet) and despawns" $ do
      let gs  = mkGameState [mkEnemyAt (EntityId 1) enemyP, pelletAt (EntityId 2) enemyP (Vector2 100 0)] towerP 10
          gs' = step (worldWith [(EntityId 1, enemyP, Vector2 0 0)]) gs
      ((.hp) <$> Map.lookup (EntityId 1) gs'.entities) `shouldBe` Just 2
      Map.member (EntityId 2) gs'.entities `shouldBe` False

  describe "turret firing (primary straight bullet)" $ do
    let towerP       = Vector2 640 620
        turretE      = turretAt (EntityId 0) (Vector2 100 100) 60
        worldWith es = (testWorld 0.016 (Vector2 100 100) towerP) { enemyList = es }
    it "fires a ballistic bullet toward the scanbox (player-aimed)" $ do
      let gs' = step (worldWith []) (mkGameState [turretE] towerP 10)
      length (projectilesOf gs') `shouldBe` 1
    it "fires regardless of whether an enemy is in the scanbox (no auto-lock)" $ do
      let gs' = step (worldWith [(EntityId 1, Vector2 110 110, Vector2 0 0)])
                     (mkGameState [turretE, mkEnemyAt (EntityId 1) (Vector2 110 110)] towerP 10)
      length (projectilesOf gs') `shouldBe` 1

  describe "flamethrowerTurret (primary)" $ do
    let towerP  = Vector2 640 620
        turretE = Entity (EntityId 0) Turret (Box (Vector2 640 360) (Circle 0)) (Vector2 0 0) 1 (flamethrowerTurret 5150)
        world   = testWorld 0.016 (Vector2 640 360) towerP
    it "sprays a puff of several flame particles toward the crosshair" $ do
      let gs' = step world (mkGameState [turretE] towerP 10)
      length (projectilesOf gs') `shouldSatisfy` (> 1)

  describe "flameParticle" $ do
    let towerP       = Vector2 640 620
        worldWith es = (testWorld 0.016 (Vector2 0 0) towerP) { enemyList = es }
        flame i p v  = Entity i (Projectile (Flame v 450)) (Box p (Circle 5)) v 1 (flameParticle v 450)
    it "burns an overlapping enemy a little, then despawns (no knockback)" $ do
      let enemyP = Vector2 500 500
          gs     = mkGameState [mkEnemyAt (EntityId 1) enemyP, flame (EntityId 2) enemyP (Vector2 700 0)] towerP 10
          gs'    = step (worldWith [(EntityId 1, enemyP, Vector2 0 0)]) gs
      ((.hp) <$> Map.lookup (EntityId 1) gs'.entities) `shouldBe` Just 2   -- 1 damage
      Map.member (EntityId 2) gs'.entities `shouldBe` False               -- particle spent
    it "flies straight: gravity does not accumulate (vy stays bounded)" $ do
      let gs0 = mkGameState [flame (EntityId 2) (Vector2 300 300) (Vector2 700 0)] towerP 10
          gs3 = stepN (worldWith []) 3 gs0
      velOf (EntityId 2) gs3 `shouldSatisfy` maybe False (\(Vector2 vx vy) -> vx == 700 && abs vy < 25)

  describe "homingMissile" $ do
    let towerP       = Vector2 640 620
        worldWith es = (testWorld 0.016 (Vector2 0 0) towerP) { enemyList = es }
        missile i p tgt v = Entity i (Projectile (HomingMissile tgt v)) (Box p (Circle 6)) v 1 (homingMissile tgt)
    it "detonates on an overlapping enemy: damage, kill, despawn" $ do
      let enemyP = Vector2 500 500
          gs  = mkGameState [mkEnemyAt (EntityId 1) enemyP, missile (EntityId 2) enemyP (EntityId 1) (Vector2 100 0)] towerP 10
          gs' = step (worldWith [(EntityId 1, enemyP, Vector2 0 0)]) gs
      Map.member (EntityId 1) gs'.entities `shouldBe` False   -- 3 dmg vs 3 hp
      gs'.score `shouldBe` 1
      Map.member (EntityId 2) gs'.entities `shouldBe` False   -- missile spent
    it "homes toward its target (steers toward it)" $ do
      let enemyP = Vector2 500 200
          gs  = mkGameState [mkEnemyAt (EntityId 1) enemyP, missile (EntityId 2) (Vector2 300 400) (EntityId 1) (Vector2 0 0)] towerP 10
          gs' = step (worldWith [(EntityId 1, enemyP, Vector2 0 0)]) gs
      velOf (EntityId 2) gs' `shouldSatisfy` maybe False (\(Vector2 vx _) -> vx > 0)
    it "keeps accelerating along its heading when its target is gone" $ do
      -- target id 99 isn't in the world; the missile keeps thrusting (speeds up)
      -- along its current heading rather than coasting/stalling
      let m   = missile (EntityId 2) (Vector2 300 300) (EntityId 99) (Vector2 200 0)
          gs' = step (worldWith []) (mkGameState [m] towerP 10)
      velOf (EntityId 2) gs' `shouldSatisfy` maybe False (\(Vector2 vx _) -> vx > 200)

  describe "nearestEnemy" $ do
    it "returns Nothing when there are no enemies" $
      nearestEnemy (Vector2 0 0) [] `shouldBe` Nothing
    it "returns the enemy closest to the origin point" $
      nearestEnemy (Vector2 0 0) [(EntityId 1, Vector2 5 0), (EntityId 2, Vector2 1 0)]
        `shouldBe` Just (EntityId 2, Vector2 1 0)

  describe "shotgunCompanion" $ do
    let towerP       = Vector2 640 620
        comp         = companionAt (EntityId 0) (Vector2 500 624) (shotgunCompanion 3131)
        worldWith es = (testWorld 0.016 (Vector2 0 0) towerP) { enemyList = es }
    it "fires a burst of several pellets at the closest enemy" $ do
      let gs' = step (worldWith [(EntityId 1, Vector2 400 400, Vector2 0 0)])
                     (mkGameState [comp, mkEnemyAt (EntityId 1) (Vector2 400 400)] towerP 10)
      length (projectilesOf gs') `shouldSatisfy` (> 1)   -- a burst, exact count is tunable
    it "holds fire when there are no enemies" $ do
      let gs' = step (worldWith []) (mkGameState [comp] towerP 10)
      length (projectilesOf gs') `shouldBe` 0

  describe "missileCompanion" $ do
    let towerP       = Vector2 640 620
        comp         = companionAt (EntityId 0) (Vector2 500 624) (missileCompanion 7777)
        worldWith es = (testWorld 0.016 (Vector2 0 0) towerP) { enemyList = es }
    it "launches a single homing missile at an enemy" $ do
      let gs' = step (worldWith [(EntityId 1, Vector2 400 400, Vector2 0 0)])
                     (mkGameState [comp, mkEnemyAt (EntityId 1) (Vector2 400 400)] towerP 10)
      length (projectilesOf gs') `shouldBe` 1
    it "holds fire when there are no enemies" $ do
      let gs' = step (worldWith []) (mkGameState [comp] towerP 10)
      length (projectilesOf gs') `shouldBe` 0

  describe "repulsorWave" $ do
    let towerP       = Vector2 640 620
        origin       = Vector2 300 300
        worldWith es = (testWorld 0.1 (Vector2 0 0) towerP) { enemyList = es }
        wave i       = Entity i (Projectile (RepulsorWave origin)) (Box origin (Circle 0)) (Vector2 0 0) 1 (repulsorWave origin)
    it "shoves an enemy radially outward without dealing damage" $ do
      let enemyP = Vector2 340 300   -- 40px right of the origin, within one frame's reach
          gs     = mkGameState [mkEnemyAt (EntityId 1) enemyP, wave (EntityId 2)] towerP 10
          gs'    = step (worldWith [(EntityId 1, enemyP, Vector2 0 0)]) gs
      ((.hp) <$> Map.lookup (EntityId 1) gs'.entities) `shouldBe` Just 3   -- no damage
      velOf (EntityId 1) gs' `shouldSatisfy` maybe False (\(Vector2 vx _) -> vx > 0)  -- pushed outward (right)
    it "does not reach an enemy beyond the wavefront yet" $ do
      let enemyP = Vector2 800 300   -- 500px away; front only reaches 90px this frame
          gs     = mkGameState [mkEnemyAt (EntityId 1) enemyP, wave (EntityId 2)] towerP 10
          gs'    = step (worldWith [(EntityId 1, enemyP, Vector2 0 0)]) gs
      velOf (EntityId 1) gs' `shouldSatisfy` maybe False (\(Vector2 vx _) -> vx == 0)  -- not yet shoved

  describe "repulsorCompanion" $ do
    let towerP       = Vector2 640 620
        comp         = companionAt (EntityId 0) (Vector2 500 624) repulsorCompanion
        worldWith es = (testWorld 0.016 (Vector2 0 0) towerP) { enemyList = es }
    it "emits a repulsion wave when enemies are present" $ do
      let gs' = step (worldWith [(EntityId 1, Vector2 400 400, Vector2 0 0)])
                     (mkGameState [comp, mkEnemyAt (EntityId 1) (Vector2 400 400)] towerP 10)
      length (projectilesOf gs') `shouldBe` 1
    it "holds when there are no enemies" $ do
      let gs' = step (worldWith []) (mkGameState [comp] towerP 10)
      length (projectilesOf gs') `shouldBe` 0

  describe "boomerang" $ do
    let towerP       = Vector2 640 620
        worldWith es = (testWorld 0.016 (Vector2 0 0) towerP) { enemyList = es }
        boom i p axis side = Entity i (Projectile (Boomerang axis side)) (Box p (Circle 42)) (Vector2 0 0) 1 (boomerang axis side)
    it "carves an enemy it sweeps over (damage) on the first frame" $ do
      let p   = Vector2 300 300
          gs  = mkGameState [mkEnemyAt (EntityId 1) p, boom (EntityId 2) p (Vector2 0 (-1)) 1] towerP 10
          gs' = step (worldWith [(EntityId 1, p, Vector2 0 0)]) gs
      ((.hp) <$> Map.lookup (EntityId 1) gs'.entities) `shouldBe` Just 1   -- 2 damage
    it "expires after its flight (returns home and is gone)" $ do
      let gs  = mkGameState [boom (EntityId 2) (Vector2 300 300) (Vector2 0 (-1)) 1] towerP 10
          gs' = stepN (worldWith []) 130 gs
      length (projectilesOf gs') `shouldBe` 0

  describe "boomerangCompanion" $ do
    let towerP       = Vector2 640 620
        comp         = companionAt (EntityId 0) (Vector2 500 624) boomerangCompanion
        worldWith es = (testWorld 0.016 (Vector2 0 0) towerP) { enemyList = es }
    it "flings a boomerang when enemies are present" $ do
      let gs' = step (worldWith [(EntityId 1, Vector2 400 400, Vector2 0 0)])
                     (mkGameState [comp, mkEnemyAt (EntityId 1) (Vector2 400 400)] towerP 10)
      length (projectilesOf gs') `shouldBe` 1
    it "holds when there are no enemies" $ do
      let gs' = step (worldWith []) (mkGameState [comp] towerP 10)
      length (projectilesOf gs') `shouldBe` 0

  describe "chainTarget (tesla hop selection)" $ do
    it "picks the nearest enemy within bounce range" $
      chainTarget 100 [] (Vector2 0 0) [(EntityId 1, Vector2 50 0), (EntityId 2, Vector2 30 0)]
        `shouldBe` Just (EntityId 2, Vector2 30 0)
    it "excludes enemies the chain already struck" $
      chainTarget 100 [EntityId 2] (Vector2 0 0) [(EntityId 1, Vector2 50 0), (EntityId 2, Vector2 30 0)]
        `shouldBe` Just (EntityId 1, Vector2 50 0)
    it "returns Nothing when every candidate is out of range or struck" $
      chainTarget 100 [EntityId 1] (Vector2 0 0) [(EntityId 1, Vector2 50 0), (EntityId 2, Vector2 500 0)]
        `shouldBe` Nothing

  describe "teslaBolt" $ do
    let towerP       = Vector2 640 620
        worldWith es = (testWorld 0.1 (Vector2 0 0) towerP) { enemyList = es }
        boltAt i p tgt struck =
          Entity i (Projectile (TeslaBolt (Vector2 500 624) tgt struck)) (Box p (Circle 6)) (Vector2 0 0) 1 (teslaBolt tgt struck)
        aP = Vector2 300 300
        a  = mkEnemyAt (EntityId 1) aP
    it "strikes its target for 2 damage" $ do
      let gs  = mkGameState [a, boltAt (EntityId 9) aP (EntityId 1) []] towerP 10
          gs' = step (worldWith [(EntityId 1, aP, Vector2 0 0)]) gs
      ((.hp) <$> Map.lookup (EntityId 1) gs'.entities) `shouldBe` Just 1
    it "arcs to the nearest trooper in bounce range after the hop delay" $ do
      let bP  = Vector2 400 300   -- 100px from the strike, within bounce range
          gs  = mkGameState [a, mkEnemyAt (EntityId 2) bP, boltAt (EntityId 9) aP (EntityId 1) []] towerP 10
          es  = [(EntityId 1, aP, Vector2 0 0), (EntityId 2, bP, Vector2 0 0)]
          gs2 = stepN (worldWith es) 2 gs
          gs3 = step (worldWith es) gs2
      length (projectilesOf gs2) `shouldBe` 2                               -- second segment spawned
      ((.hp) <$> Map.lookup (EntityId 2) gs3.entities) `shouldBe` Just 1    -- ...and it struck B
    it "does not arc onward once the chain has used its 2 bounces" $ do
      let bP  = Vector2 400 300
          gs  = mkGameState [a, mkEnemyAt (EntityId 2) bP, boltAt (EntityId 9) aP (EntityId 1) [EntityId 98, EntityId 99]] towerP 10
          es  = [(EntityId 1, aP, Vector2 0 0), (EntityId 2, bP, Vector2 0 0)]
          gs3 = stepN (worldWith es) 3 gs
      length (projectilesOf gs3) `shouldSatisfy` (<= 1)
      ((.hp) <$> Map.lookup (EntityId 2) gs3.entities) `shouldBe` Just 3    -- B untouched
    it "does not arc to a trooper beyond bounce range" $ do
      let bP  = Vector2 800 300   -- 500px away, beyond bounce range
          gs  = mkGameState [a, mkEnemyAt (EntityId 2) bP, boltAt (EntityId 9) aP (EntityId 1) []] towerP 10
          es  = [(EntityId 1, aP, Vector2 0 0), (EntityId 2, bP, Vector2 0 0)]
          gs2 = stepN (worldWith es) 2 gs
      length (projectilesOf gs2) `shouldBe` 1
    it "expires after its arc life" $ do
      let gs  = mkGameState [a, boltAt (EntityId 9) aP (EntityId 1) []] towerP 10
          gs' = stepN (worldWith [(EntityId 1, aP, Vector2 0 0)]) 5 gs
      length (projectilesOf gs') `shouldBe` 0

  describe "teslaCompanion" $ do
    let towerP       = Vector2 640 620
        comp         = companionAt (EntityId 0) (Vector2 500 624) (teslaCompanion 9090)
        worldWith es = (testWorld 0.016 (Vector2 0 0) towerP) { enemyList = es }
    it "strikes an enemy within its long range with a bolt" $ do
      let gs' = step (worldWith [(EntityId 1, Vector2 400 400, Vector2 0 0)])
                     (mkGameState [comp, mkEnemyAt (EntityId 1) (Vector2 400 400)] towerP 10)
      length (projectilesOf gs') `shouldBe` 1
    it "holds fire when no enemy is within range" $ do
      let farP = Vector2 1300 50   -- ~980px from the coil, beyond its 750 range
          gs'  = step (worldWith [(EntityId 1, farP, Vector2 0 0)])
                      (mkGameState [comp, mkEnemyAt (EntityId 1) farP] towerP 10)
      length (projectilesOf gs') `shouldBe` 0
    it "holds when there are no enemies" $ do
      let gs' = step (worldWith []) (mkGameState [comp] towerP 10)
      length (projectilesOf gs') `shouldBe` 0

  describe "wait + spawnEnemyAt (sequential mission scripting)" $ do
    let world = testWorld 0.1 (Vector2 0 0) (Vector2 640 620)
        scr   = wait 1.0 >> spawnEnemyAt (Vector2 100 0)
        gsW   = mkGameState [directorWith scr] (Vector2 640 620) 10
    it "does not spawn before the wait elapses" $
      length (enemiesOf (stepN world 1 gsW)) `shouldBe` 0
    it "spawns once the wait elapses" $
      length (enemiesOf (stepN world 12 gsW)) `shouldBe` 1

  describe "missionDirector" $
    it "drops a stream of troopers over time" $
      -- after ~3.5s, the first round (10) has dropped several troopers (still falling)
      length (enemiesOf (stepN (testWorld 0.1 (Vector2 0 0) (Vector2 640 620)) 35
                               (mkGameState [directorWith missionDirector] (Vector2 640 620) 10)))
        `shouldSatisfy` (>= 3)
