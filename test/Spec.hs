module Main where

import RogueTrooper.Aim        (boxContains, nearestInBox, seekToward)
import RogueTrooper.Behaviours (enemyBehaviour, groundLevel, turretBehaviour)
import RogueTrooper.Engine     (World (..), resolveTowerHits, step)
import RogueTrooper.Types      (Box (..), BoxShape (..), Entity (..), GameState (..))
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
noopTurret = Entity { box = Box (Vector2 0 0) (Circle 1), speed = 0, script = pure () }

-- | An enemy at a position, running the real enemy behaviour.
mkEnemy :: Vector2 -> Entity
mkEnemy p = Entity { box = Box p (Circle 12), speed = 100, script = enemyBehaviour }

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
        -- (1,1): (1/4)^2 + (1/2)^2 = 0.3125 < 1
        boxContains (Box (Vector2 0 0) (Oval 4 2)) (Vector2 1 1) `shouldBe` True
      it "excludes a point outside the ellipse" $
        -- (3,1.5): (3/4)^2 + (1.5/2)^2 = 1.125 > 1
        boxContains (Box (Vector2 0 0) (Oval 4 2)) (Vector2 3 1.5) `shouldBe` False

  describe "seekToward" $ do
    it "moves toward the target by speed*dt when the target is far" $
      -- maxDistance = 10 * 0.3 = 3, so (0,0) -> (3,0)
      seekToward 10 0.3 (Vector2 0 0) (Vector2 10 0) `shouldBeCloseTo` Vector2 3 0
    it "clamps to the target without overshooting when within reach" $
      -- maxDistance = 100, target only 2 away -> snaps to target exactly
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
    let gs0 = GameState
                { turret  = Entity { box = Box (Vector2 0 0) (Circle 10), speed = 100, script = turretBehaviour }
                , enemies = []
                , tower   = Vector2 0 0
                , towerHp = 10
                }
        world = World { dt = 0.1, aimTarget = Vector2 100 0, towerPos = Vector2 0 0 }
    it "moves the turret box toward the aim position in one frame" $
      -- maxDistance = 100 * 0.1 = 10, aim far at (100,0) -> moves to (10,0).
      -- (That this terminates at all proves the script suspends at yield rather
      -- than looping forever.)
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
    let tower = Vector2 640 (groundLevel + 20)
        world = World { dt = 0.1, aimTarget = Vector2 0 0, towerPos = tower }
        afterStep p =
          case (step world (GameState noopTurret [mkEnemy p] tower 10)).enemies of
            (e : _) -> e.box.center
            []      -> error "enemy unexpectedly removed"
    it "descends straight down while airborne" $
      -- well above ground: moves down (y up), x unchanged. speed*dt = 10.
      afterStep (Vector2 300 (groundLevel - 200))
        `shouldSatisfy` (\(Vector2 x y) -> x == 300 && y > groundLevel - 200)
    it "advances toward the tower once landed" $
      -- on the ground and left of the tower: moves right and down toward it.
      afterStep (Vector2 300 (groundLevel + 10))
        `shouldSatisfy` (\(Vector2 x y) -> x > 300 && y > groundLevel + 10)

  describe "resolveTowerHits" $ do
    let tower = Vector2 640 660
        gsWith es = GameState noopTurret es tower 10
    it "removes an enemy that reached the tower and deals 1 HP" $ do
      let gs' = resolveTowerHits (gsWith [mkEnemy tower])
      length gs'.enemies `shouldBe` 0
      gs'.towerHp `shouldBe` 9
    it "keeps a distant enemy and leaves HP unchanged" $ do
      let gs' = resolveTowerHits (gsWith [mkEnemy (Vector2 300 100)])
      length gs'.enemies `shouldBe` 1
      gs'.towerHp `shouldBe` 10

  describe "step (full frame)" $
    it "removes an enemy that has reached the tower and deals 1 HP" $ do
      let tower = Vector2 640 660
          world = World 0.1 (Vector2 0 0) tower
          gs'   = step world (GameState noopTurret [mkEnemy tower] tower 10)
      length gs'.enemies `shouldBe` 0
      gs'.towerHp `shouldBe` 9
