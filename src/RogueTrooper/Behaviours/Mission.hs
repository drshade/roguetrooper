-- | Mission director scripts: stage rounds by dropping troopers over time.
-- Content only — sequential scripts built on the DSL + 'wait'.
module RogueTrooper.Behaviours.Mission
  ( missionDirector
  ) where

import           Raylib.Types        (pattern Vector2)
import           RogueTrooper.Behaviours.Common (wait)
import           RogueTrooper.Script (Script, spawnEnemyAt, yield)

-- | Seconds between trooper drops within a round, and between rounds.
dropInterval, betweenRounds :: Float
dropInterval = 0.6
betweenRounds = 5

-- | The mission: a sequence of rounds, each dropping N troopers paced over time
-- with a gap between rounds, then idle. Carries its own RNG seed for drop
-- positions, so the mission is deterministic.
missionDirector :: Script ()
missionDirector = runRounds 12345 [3, 5, 8]
  where
    runRounds :: Int -> [Int] -> Script ()
    runRounds _    []         = forever yield          -- mission complete
    runRounds seed (n : rest) = do
      seed' <- dropTroopers seed n
      wait betweenRounds
      runRounds seed' rest

    dropTroopers :: Int -> Int -> Script Int
    dropTroopers seed 0 = pure seed
    dropTroopers seed k = do
      let (x, seed') = randomTopX seed
      spawnEnemyAt (Vector2 x 0)
      wait dropInterval
      dropTroopers seed' (k - 1)

-- | Deterministic random x along the top of the screen, advancing the seed.
randomTopX :: Int -> (Float, Int)
randomTopX s =
  let s' = (1103515245 * s + 12345) `mod` 2147483648
   in (50 + fromIntegral (s' `mod` 1180), s')
