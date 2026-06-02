module Main where

import qualified Data.Map.Strict     as Map
import           RogueTrooper.Aim        (boxContains, nearestInBox, seekToward)
import           RogueTrooper.Behaviours (enemyBehaviour, groundLevel, launchToHit,
                                          predictLead, straightBullet, turretBehaviour)
import           RogueTrooper.Engine     (World (..), applyEvents, integrate,
                                          resolveTowerHits, spawnTick, step)
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

testProjectile :: ProjectileType -> Vector2 -> Entity
testProjectile pt@(StraightBullet v) origin =
  Entity (EntityId 0) (Projectile pt) (Box origin (Circle 4)) v 1 straightBullet

testEnemyFactory :: Vector2 -> Entity
testEnemyFactory p = Entity (EntityId 0) Enemy (Box p (Circle 12)) (Vector2 0 0) 3 enemyBehaviour

-- GameState / World builders -------------------------------------------------

mkGameState :: [Entity] -> Vector2 -> Int -> GameState
mkGameState es towerP hp =
  GameState { entities = Map.fromList [(e.eid, e) | e <- es]
            , tower = towerP, towerHp = hp, score = 0, nextId = 100
            , spawnTimer = 999, spawnInterval = 999, seed = 1 }

testWorld :: Float -> Vector2 -> Vector2 -> World
testWorld dt' aim towerP =
  World { dt = dt', aimTarget = aim, towerPos = towerP
        , gravity = Vector2 0 1000, groundLevel = groundLevel, enemyList = []
        , mkProjectile = testProjectile, mkEnemy = testEnemyFactory }

-- Accessors ------------------------------------------------------------------

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

  describe "predictLead" $
    it "leads along the target's velocity by the bullet travel time" $
      predictLead (Vector2 0 0) (Vector2 100 0) (Vector2 0 50) 100
        `shouldSatisfy` (\(Vector2 x y) -> x == 100 && y > 45 && y < 65)

  describe "launchToHit (ballistic firing solution)" $ do
    it "produces a launch that lands exactly on the target under gravity" $
      case launchToHit 40 10 (Vector2 0 0) (Vector2 100 50) of
        Just (Vector2 vx vy) ->
          let t = 100 / vx                       -- time to reach the target's x
              y = vy * t + 0.5 * 10 * t * t       -- ballistic y at that time
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
      let e   = mkEnemyAt (EntityId 1) (Vector2 0 100)   -- well above the ground
          gs' = integrate g groundLevel 0.1 (mkGameState [e] (Vector2 0 0) 10)
      centerOf (EntityId 1) gs' `shouldBeCloseTo` Vector2 0 110
    it "lands an enemy on the ground, clamping it and stopping the fall" $ do
      let e   = (mkEnemyAt (EntityId 1) (Vector2 0 635)) { vel = Vector2 0 1000 }
          gs' = integrate g groundLevel 0.1 (mkGameState [e] (Vector2 0 0) 10)
      centerOf (EntityId 1) gs' `shouldBeCloseTo` Vector2 0 groundLevel
      velOf (EntityId 1) gs' `shouldBe` Just (Vector2 0 0)

  describe "applyEvents" $ do
    let e1 = mkEnemyAt (EntityId 1) (Vector2 100 100)
        e2 = mkEnemyAt (EntityId 2) (Vector2 200 200)
        gs = mkGameState [e1, e2] (Vector2 0 0) 10
    it "Impulse adds to an entity's velocity" $
      velOf (EntityId 1) (applyEvents 0.1 testProjectile [Impulse (EntityId 1) (Vector2 5 0)] gs)
        `shouldBe` Just (Vector2 5 0)
    it "Steer eases velocity toward the target (full step at responsiveness*dt >= 1)" $
      velOf (EntityId 1) (applyEvents 0.1 testProjectile [Steer (EntityId 1) 10 (Vector2 100 0)] gs)
        `shouldBe` Just (Vector2 100 0)
    it "lethal Damage kills the named enemy and scores" $ do
      let gs' = applyEvents 0.1 testProjectile [Damage (EntityId 1) 3] gs
      map (.eid) (enemiesOf gs') `shouldBe` [EntityId 2]
      gs'.score `shouldBe` 1
    it "non-lethal Damage reduces HP without scoring" $ do
      let gs' = applyEvents 0.1 testProjectile [Damage (EntityId 1) 1] gs
      ((.hp) <$> Map.lookup (EntityId 1) gs'.entities) `shouldBe` Just 2
      gs'.score `shouldBe` 0
    it "Despawn removes the named entity" $
      Map.member (EntityId 2) (applyEvents 0.1 testProjectile [Despawn (EntityId 2)] gs).entities `shouldBe` False
    it "Spawn adds a projectile with a fresh id" $ do
      let gs' = applyEvents 0.1 testProjectile [Spawn (StraightBullet (Vector2 5 5)) (Vector2 0 0)] (mkGameState [] (Vector2 0 0) 10)
      length (projectilesOf gs') `shouldBe` 1
      gs'.nextId `shouldBe` 101

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

  describe "resolveTowerHits" $ do
    let towerP = Vector2 640 620
    it "removes an enemy that reached the tower and deals 1 HP" $ do
      let gs' = resolveTowerHits (mkGameState [mkEnemyAt (EntityId 1) towerP] towerP 10)
      length (enemiesOf gs') `shouldBe` 0
      gs'.towerHp `shouldBe` 9

  describe "straightBullet" $ do
    let enemyP       = Vector2 500 500
        towerP       = Vector2 640 620
        worldWith es = (testWorld 0.016 (Vector2 0 0) towerP) { enemyList = es }
    it "damages an overlapping enemy (non-lethal single hit) and despawns itself" $ do
      let gs  = mkGameState [mkEnemyAt (EntityId 1) enemyP, bulletAt (EntityId 2) enemyP (Vector2 100 0)] towerP 10
          gs' = step (worldWith [(EntityId 1, enemyP, Vector2 0 0)]) gs
      ((.hp) <$> Map.lookup (EntityId 1) gs'.entities) `shouldBe` Just 1   -- 2 dmg vs 3 HP
      gs'.score `shouldBe` 0
      Map.member (EntityId 2) gs'.entities `shouldBe` False
    it "despawns when it hits the ground" $ do
      let gs  = mkGameState [bulletAt (EntityId 2) (Vector2 100 650) (Vector2 0 0)] towerP 10
          gs' = step (worldWith []) gs
      Map.member (EntityId 2) gs'.entities `shouldBe` False

  describe "turret firing" $
    it "fires a ballistic projectile toward the scanbox each shot" $ do
      let towerP  = Vector2 640 620
          turretE = turretAt (EntityId 0) (Vector2 100 100) 60
          gs'     = step (testWorld 0.016 (Vector2 100 100) towerP) (mkGameState [turretE] towerP 10)
      length (projectilesOf gs') `shouldBe` 1

  describe "spawnTick (periodic enemy spawning)" $ do
    let world = testWorld 0.1 (Vector2 0 0) (Vector2 0 0)
    it "counts the timer down when a spawn is not yet due" $ do
      let gs' = spawnTick world ((mkGameState [] (Vector2 0 0) 10) { spawnTimer = 2, spawnInterval = 3 })
      length (enemiesOf gs') `shouldBe` 0
      gs'.spawnTimer `shouldSatisfy` (\t -> t > 1.89 && t < 1.91)
    it "spawns an enemy at the top and resets the timer when due" $ do
      let gs' = spawnTick world ((mkGameState [] (Vector2 0 0) 10) { spawnTimer = 0.05, spawnInterval = 3 })
      length (enemiesOf gs') `shouldBe` 1
      gs'.spawnTimer `shouldBe` 3
      map (\e -> let Vector2 _ y = e.box.center in y) (enemiesOf gs') `shouldBe` [0]
