module Main where

import qualified Data.Map.Strict     as Map
import           RogueTrooper.Aim        (boxContains, nearestInBox, seekToward)
import           RogueTrooper.Behaviours (enemyBehaviour, groundLevel, predictLead,
                                          straightBullet, turretBehaviour)
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
bulletAt i p tgt = Entity i (Projectile (StraightBullet tgt)) (Box p (Circle 4)) (Vector2 0 0) 1 (straightBullet tgt)

-- | A projectile factory for tests (inert projectile).
testProjectile :: ProjectileType -> Vector2 -> Entity
testProjectile pt origin = Entity (EntityId 0) (Projectile pt) (Box origin (Circle 4)) (Vector2 0 0) 1 (pure ())

-- | An enemy factory for tests.
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
  World { dt = dt', aimTarget = aim, towerPos = towerP, enemyList = []
        , mkProjectile = testProjectile, mkEnemy = testEnemyFactory }

-- Accessors ------------------------------------------------------------------

centerOf :: EntityId -> GameState -> Vector2
centerOf i gs = case Map.lookup i gs.entities of
  Just e  -> e.box.center
  Nothing -> error ("entity missing: " <> show i)

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
    it "ignores entities outside the box" $
      nearestInBox box [(1 :: Int, Vector2 9 0), (2, Vector2 2 0)] `shouldBe` Just 2
    it "breaks ties by list order (earliest wins)" $
      nearestInBox box [(1 :: Int, Vector2 2 0), (2, Vector2 0 2)] `shouldBe` Just 1

  describe "predictLead" $ do
    it "aims at the target's current position when it is stationary" $
      predictLead (Vector2 0 0) (Vector2 100 0) (Vector2 0 0) 100 `shouldBeCloseTo` Vector2 100 0
    it "leads along the target's velocity by the bullet travel time" $
      predictLead (Vector2 0 0) (Vector2 100 0) (Vector2 0 50) 100
        `shouldSatisfy` (\(Vector2 x y) -> x == 100 && y > 45 && y < 65)

  describe "integrate" $
    it "moves each entity by velocity * dt" $ do
      let e   = (mkEnemyAt (EntityId 1) (Vector2 0 0)) { vel = Vector2 100 50 }
          gs' = integrate 0.1 (mkGameState [e] (Vector2 0 0) 10)
      centerOf (EntityId 1) gs' `shouldBeCloseTo` Vector2 10 5

  describe "turret script (velocity intent + integration)" $ do
    let gs0   = mkGameState [turretAt (EntityId 0) (Vector2 0 0) 10] (Vector2 0 0) 10
        world = testWorld 0.1 (Vector2 100 0) (Vector2 0 0)
    it "moves the turret box toward the aim position (without overshooting)" $
      centerOf (EntityId 0) (step world gs0)
        `shouldSatisfy` (\(Vector2 x y) -> x > 0 && x <= 100 && y == 0)
    it "resumes and keeps moving across frames" $
      let g1 = step world gs0
          g2 = step world g1
          Vector2 x1 _ = centerOf (EntityId 0) g1
          Vector2 x2 _ = centerOf (EntityId 0) g2
       in (x2 > x1) `shouldBe` True

  describe "enemy script (parachute then advance)" $ do
    let towerP = Vector2 640 (groundLevel + 20)
        world  = testWorld 0.1 (Vector2 0 0) towerP
        afterStep p = centerOf (EntityId 1) (step world (mkGameState [mkEnemyAt (EntityId 1) p] towerP 10))
    it "descends straight down while airborne" $
      afterStep (Vector2 300 (groundLevel - 200))
        `shouldSatisfy` (\(Vector2 x y) -> x == 300 && y > groundLevel - 200)
    it "advances toward the tower once landed" $
      afterStep (Vector2 300 (groundLevel + 10))
        `shouldSatisfy` (\(Vector2 x y) -> x > 300 && y > groundLevel + 10)

  describe "applyEvents" $ do
    let e1 = mkEnemyAt (EntityId 1) (Vector2 100 100)
        e2 = mkEnemyAt (EntityId 2) (Vector2 200 200)
        gs = mkGameState [e1, e2] (Vector2 0 0) 10
    it "SetVel sets an entity's velocity" $
      ((.vel) <$> Map.lookup (EntityId 1) (applyEvents testProjectile [SetVel (EntityId 1) (Vector2 7 7)] gs).entities)
        `shouldBe` Just (Vector2 7 7)
    it "lethal Damage kills the named enemy and scores" $ do
      let gs' = applyEvents testProjectile [Damage (EntityId 1) 3] gs
      map (.eid) (enemiesOf gs') `shouldBe` [EntityId 2]
      gs'.score `shouldBe` 1
    it "non-lethal Damage reduces HP without scoring" $ do
      let gs' = applyEvents testProjectile [Damage (EntityId 1) 1] gs
      ((.hp) <$> Map.lookup (EntityId 1) gs'.entities) `shouldBe` Just 2
      gs'.score `shouldBe` 0
    it "Despawn removes the named entity" $
      Map.member (EntityId 2) (applyEvents testProjectile [Despawn (EntityId 2)] gs).entities `shouldBe` False
    it "Spawn adds a projectile with a fresh id" $ do
      let gs' = applyEvents testProjectile [Spawn (StraightBullet (Vector2 5 5)) (Vector2 0 0)] (mkGameState [] (Vector2 0 0) 10)
      length (projectilesOf gs') `shouldBe` 1
      gs'.nextId `shouldBe` 101

  describe "resolveTowerHits" $ do
    let towerP = Vector2 640 660
    it "removes an enemy that reached the tower and deals 1 HP" $ do
      let gs' = resolveTowerHits (mkGameState [mkEnemyAt (EntityId 1) towerP] towerP 10)
      length (enemiesOf gs') `shouldBe` 0
      gs'.towerHp `shouldBe` 9
    it "keeps a distant enemy and leaves HP unchanged" $ do
      let gs' = resolveTowerHits (mkGameState [mkEnemyAt (EntityId 1) (Vector2 300 100)] towerP 10)
      length (enemiesOf gs') `shouldBe` 1
      gs'.towerHp `shouldBe` 10

  describe "step (full frame)" $
    it "removes an enemy that has reached the tower and deals 1 HP" $ do
      let towerP = Vector2 640 660
          world  = testWorld 0.016 (Vector2 0 0) towerP
          gs'    = step world (mkGameState [mkEnemyAt (EntityId 1) towerP] towerP 10)
      length (enemiesOf gs') `shouldBe` 0
      gs'.towerHp `shouldBe` 9

  describe "straightBullet" $ do
    let enemyP       = Vector2 500 500
        worldWith es = (testWorld 0.016 (Vector2 0 0) (Vector2 0 0)) { enemyList = es }
    it "damages an overlapping enemy, scores, and despawns itself" $ do
      let gs  = mkGameState [mkEnemyAt (EntityId 1) enemyP, bulletAt (EntityId 2) enemyP (Vector2 9999 500)] (Vector2 0 0) 10
          gs' = step (worldWith [(EntityId 1, enemyP, Vector2 0 0)]) gs
      length (enemiesOf gs') `shouldBe` 0
      gs'.score `shouldBe` 1
      Map.member (EntityId 2) gs'.entities `shouldBe` False
    it "despawns when it flies off-screen without hitting anything" $ do
      let gs  = mkGameState [bulletAt (EntityId 2) (Vector2 (-100) (-100)) (Vector2 (-9999) (-100))] (Vector2 0 0) 10
          gs' = step (worldWith []) gs
      Map.member (EntityId 2) gs'.entities `shouldBe` False

  describe "turret firing" $ do
    let towerP  = Vector2 640 700
        turretE = turretAt (EntityId 0) (Vector2 100 100) 60
    it "fires a projectile toward the scanbox each shot (no target needed)" $ do
      let gs' = step (testWorld 0.016 (Vector2 100 100) towerP) (mkGameState [turretE] towerP 10)
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
