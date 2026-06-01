module Main where

import RogueTrooper.Aim        (boxContains, nearestInBox, seekToward)
import RogueTrooper.Behaviours (enemyBehaviour, groundLevel, straightBullet, turretBehaviour)
import RogueTrooper.Engine     (World (..), applyEffects, resolveTowerHits, step)
import RogueTrooper.Types      (Box (..), BoxShape (..), Bullet (..), Effect (..), Entity (..),
                                EntityId (..), GameState (..), ProjectileType (..))
import Raylib.Types            (Vector2, pattern Vector2)
import Test.Hspec

-- | Assert two vectors are equal within a small tolerance.
shouldBeCloseTo :: Vector2 -> Vector2 -> Expectation
shouldBeCloseTo (Vector2 ax ay) (Vector2 bx by) =
  (abs (ax - bx) < eps && abs (ay - by) < eps) `shouldBe` True
  where
    eps = 1e-4

-- | A turret that does nothing (so step leaves it untouched in enemy tests).
noopTurret :: Entity
noopTurret = Entity { eid = EntityId 0, box = Box (Vector2 0 0) (Circle 1), speed = 0, script = pure () }

-- | An enemy at a position, running the real enemy behaviour.
mkEnemy :: Vector2 -> Entity
mkEnemy p = Entity { eid = EntityId 9, box = Box p (Circle 12), speed = 100, script = enemyBehaviour }

-- | A minimal game state with the given enemies, tower position, and HP.
mkGameState :: [Entity] -> Vector2 -> Int -> GameState
mkGameState es towerP hp =
  GameState { turret = noopTurret, enemies = es, bullets = [], tower = towerP, towerHp = hp, score = 0, nextId = 100 }

-- | A trivial projectile factory for tests (inert bullet).
testBullet :: ProjectileType -> Vector2 -> Bullet
testBullet pt origin = Bullet pt (Entity (EntityId 0) (Box origin (Circle 4)) 600 (pure ()))

-- | A world with the given dt, aim, and tower; no visible enemies.
testWorld :: Float -> Vector2 -> Vector2 -> World
testWorld dt' aim towerP =
  World { dt = dt', aimTarget = aim, towerPos = towerP, enemyList = [], mkProjectile = testBullet }

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
    it "stays put when already at the target" $
      seekToward 10 0.1 (Vector2 5 5) (Vector2 5 5) `shouldBe` Vector2 5 5

  describe "nearestInBox" $ do
    let box = Box (Vector2 0 0) (Circle 5)
    it "returns Nothing when nothing is inside the box" $
      nearestInBox box [(1 :: Int, Vector2 10 0)] `shouldBe` Nothing
    it "returns the only entity inside the box" $
      nearestInBox box [(1 :: Int, Vector2 1 0)] `shouldBe` Just 1
    it "returns the entity nearest the box centre" $
      nearestInBox box [(1 :: Int, Vector2 4 0), (2, Vector2 1 0), (3, Vector2 3 0)]
        `shouldBe` Just 2
    it "ignores entities outside the box" $
      nearestInBox box [(1 :: Int, Vector2 9 0), (2, Vector2 2 0)] `shouldBe` Just 2
    it "breaks ties by list order (earliest wins)" $
      nearestInBox box [(1 :: Int, Vector2 2 0), (2, Vector2 0 2)] `shouldBe` Just 1

  describe "turret script (resumable, entity interpreter)" $ do
    let turretE = Entity { eid = EntityId 0, box = Box (Vector2 0 0) (Circle 10), speed = 100, script = turretBehaviour }
        gs0     = (mkGameState [] (Vector2 0 0) 10) { turret = turretE }
        world   = testWorld 0.1 (Vector2 100 0) (Vector2 0 0)
    it "moves the turret box toward the aim position in one frame" $
      (step world gs0).turret.box.center `shouldBeCloseTo` Vector2 10 0
    it "resumes the continuation across frames, continuing to move" $
      let g1 = step world gs0   -- (0,0)  -> (10,0)
          g2 = step world g1    -- (10,0) -> (20,0)
       in g2.turret.box.center `shouldBeCloseTo` Vector2 20 0
    it "leaves speed, shape, tower and hp untouched" $ do
      let g1 = step world gs0
      g1.turret.speed `shouldBe` 100
      g1.turret.box.shape `shouldBe` Circle 10
      g1.towerHp `shouldBe` 10

  describe "enemy script (parachute then advance)" $ do
    let towerP = Vector2 640 (groundLevel + 20)
        world  = testWorld 0.1 (Vector2 0 0) towerP
        afterStep p =
          case (step world (mkGameState [mkEnemy p] towerP 10)).enemies of
            (e : _) -> e.box.center
            []      -> error "enemy unexpectedly removed"
    it "descends straight down while airborne" $
      afterStep (Vector2 300 (groundLevel - 200))
        `shouldSatisfy` (\(Vector2 x y) -> x == 300 && y > groundLevel - 200)
    it "advances toward the tower once landed" $
      afterStep (Vector2 300 (groundLevel + 10))
        `shouldSatisfy` (\(Vector2 x y) -> x > 300 && y > groundLevel + 10)

  describe "resolveTowerHits" $ do
    let towerP = Vector2 640 660
    it "removes an enemy that reached the tower and deals 1 HP" $ do
      let gs' = resolveTowerHits (mkGameState [mkEnemy towerP] towerP 10)
      length gs'.enemies `shouldBe` 0
      gs'.towerHp `shouldBe` 9
    it "keeps a distant enemy and leaves HP unchanged" $ do
      let gs' = resolveTowerHits (mkGameState [mkEnemy (Vector2 300 100)] towerP 10)
      length gs'.enemies `shouldBe` 1
      gs'.towerHp `shouldBe` 10

  describe "step (full frame)" $
    it "removes an enemy that has reached the tower and deals 1 HP" $ do
      let towerP = Vector2 640 660
          world  = testWorld 0.1 (Vector2 0 0) towerP
          gs'    = step world (mkGameState [mkEnemy towerP] towerP 10)
      length gs'.enemies `shouldBe` 0
      gs'.towerHp `shouldBe` 9

  describe "applyEffects" $ do
    let e1 = (mkEnemy (Vector2 100 100)) { eid = EntityId 1 }
        e2 = (mkEnemy (Vector2 200 200)) { eid = EntityId 2 }
        gs = mkGameState [e1, e2] (Vector2 0 0) 10
    it "Damage removes the named enemy and increments the score" $ do
      let gs' = applyEffects testBullet [Damage (EntityId 1)] gs
      map (.eid) gs'.enemies `shouldBe` [EntityId 2]
      gs'.score `shouldBe` 1
    it "Despawn removes the named entity" $ do
      let gs' = applyEffects testBullet [Despawn (EntityId 2)] gs
      map (.eid) gs'.enemies `shouldBe` [EntityId 1]
    it "Spawn adds a projectile with a fresh id" $ do
      let gs' = applyEffects testBullet [Spawn (StraightBullet (Vector2 5 5)) (Vector2 0 0)] (mkGameState [] (Vector2 0 0) 10)
      length gs'.bullets `shouldBe` 1
      gs'.nextId `shouldBe` 101

  describe "straightBullet" $ do
    let enemyP        = Vector2 500 500
        mkBulletAt p tgt =
          Bullet (StraightBullet tgt)
            (Entity { eid = EntityId 2, box = Box p (Circle 4), speed = 600, script = straightBullet tgt })
        worldWith es = (testWorld 0.016 (Vector2 0 0) (Vector2 0 0)) { enemyList = es }
    it "damages an enemy within range, scores, and despawns itself" $ do
      let enemy = (mkEnemy enemyP) { eid = EntityId 1 }
          gs    = (mkGameState [enemy] (Vector2 0 0) 10) { bullets = [mkBulletAt enemyP (Vector2 9999 500)] }
          gs'   = step (worldWith [(EntityId 1, enemyP)]) gs
      map (.eid) gs'.enemies `shouldBe` []
      gs'.score `shouldBe` 1
      length gs'.bullets `shouldBe` 0
    it "despawns when it flies off-screen without hitting anything" $ do
      let gs  = (mkGameState [] (Vector2 0 0) 10) { bullets = [mkBulletAt (Vector2 (-100) (-100)) (Vector2 (-9999) (-100))] }
          gs' = step (worldWith []) gs
      length gs'.bullets `shouldBe` 0

  describe "turret firing" $ do
    let towerP   = Vector2 640 700
        turretE  = Entity { eid = EntityId 0, box = Box (Vector2 100 100) (Circle 60), speed = 320, script = turretBehaviour }
        mkGs es  = (mkGameState es towerP 10) { turret = turretE }
        worldWith es = (testWorld 0.016 (Vector2 100 100) towerP) { enemyList = es }
    it "fires a projectile from the tower when an enemy is inside the turret box" $ do
      let enemy = (mkEnemy (Vector2 110 110)) { eid = EntityId 1 }
          gs'   = step (worldWith [(EntityId 1, Vector2 110 110)]) (mkGs [enemy])
      map (\b -> b.entity.box.center) gs'.bullets `shouldBe` [towerP]
    it "holds fire when no enemy is in the box" $ do
      let enemy = (mkEnemy (Vector2 900 900)) { eid = EntityId 1 }
          gs'   = step (worldWith [(EntityId 1, Vector2 900 900)]) (mkGs [enemy])
      length gs'.bullets `shouldBe` 0
