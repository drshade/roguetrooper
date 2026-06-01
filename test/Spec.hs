module Main where

import RogueTrooper.Aim    (boxContains, nearestInBox, seekTurretBox)
import RogueTrooper.Engine (stepAim)
import RogueTrooper.Types  (Box (..), BoxShape (..), GameState (..))
import Raylib.Types        (Vector2, pattern Vector2)
import Test.Hspec

-- | Assert two vectors are equal within a small tolerance.
shouldBeCloseTo :: Vector2 -> Vector2 -> Expectation
shouldBeCloseTo (Vector2 ax ay) (Vector2 bx by) =
  (abs (ax - bx) < eps && abs (ay - by) < eps) `shouldBe` True
  where
    eps = 1e-4

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

  describe "seekTurretBox" $ do
    it "moves toward the target by speed*dt when the target is far" $
      -- maxDistance = 10 * 0.3 = 3, so (0,0) -> (3,0)
      seekTurretBox 10 0.3 (Vector2 0 0) (Vector2 10 0) `shouldBeCloseTo` Vector2 3 0
    it "clamps to the target without overshooting when within reach" $
      -- maxDistance = 100, target only 2 away -> snaps to target exactly
      seekTurretBox 100 1 (Vector2 0 0) (Vector2 2 0) `shouldBe` Vector2 2 0
    it "stays put when already at the target" $
      seekTurretBox 10 0.1 (Vector2 5 5) (Vector2 5 5) `shouldBe` Vector2 5 5

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

  describe "stepAim" $ do
    let gs = GameState
               { turret    = Box (Vector2 0 0) (Circle 10)
               , seekSpeed = 100
               , tower     = Vector2 0 0
               , towerHp   = 10
               }
    it "seeks the turret box toward the aim target by speed*dt" $
      -- maxDistance = 100 * 0.1 = 10, target far at (100,0) -> turret moves to (10,0)
      (stepAim 0.1 (Vector2 100 0) gs).turret.center `shouldBeCloseTo` Vector2 10 0
    it "leaves seek speed, shape, tower and hp untouched" $ do
      let gs' = stepAim 0.1 (Vector2 100 0) gs
      gs'.seekSpeed `shouldBe` 100
      gs'.turret.shape `shouldBe` Circle 10
      gs'.towerHp `shouldBe` 10
