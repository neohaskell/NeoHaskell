module Int (
  toFloat,
  fromInt64,
  toInt64,
  getRandom,
  getRandomBetween,
) where

import Basics
import Prelude qualified
import System.Random qualified as Random
import Task (Task)
import Task qualified


-- * Int to Float / Float to Int


-- | Convert an integer into a float. Useful when mixing @Int@ and @Float@
-- values like this:
--
-- > halfOf :: Int -> Float
-- > halfOf number =
-- >   toFloat number / 2
toFloat :: Int -> Float
toFloat =
  Prelude.fromIntegral


-- | Convert an Int64 to an Int safely
-- Clamps to maxBound if value exceeds Int range (prevents silent truncation)
fromInt64 :: Int64 -> Int
fromInt64 value =
  if value > Prelude.fromIntegral (Prelude.maxBound :: Int)
    then Prelude.maxBound
    else
      if value < Prelude.fromIntegral (Prelude.minBound :: Int)
        then Prelude.minBound
        else Prelude.fromIntegral value


-- | Convert an Int to an Int64
toInt64 :: Int -> Int64
toInt64 = Prelude.fromIntegral


-- * Random Number Generation


-- | Generate a random integer within the full range of Int.
--
-- >>> randomValue <- Int.getRandom
-- >>> -- randomValue is now a random Int
getRandom :: Task _ Int
getRandom = do
  value <- Task.fromIO Random.randomIO
  Task.yield value


-- | Generate a random integer between two bounds (inclusive).
-- The first parameter is the lower bound, the second is the upper bound.
--
-- >>> diceRoll <- Int.getRandomBetween 1 6
-- >>> -- diceRoll is now a random number between 1 and 6 (inclusive)
--
-- >>> percentChance <- Int.getRandomBetween 0 100
-- >>> -- percentChance is now a random percentage
getRandomBetween :: Int -> Int -> Task _ Int
getRandomBetween minValue maxValue = do
  let lowerBound = min minValue maxValue
  let upperBound = max minValue maxValue
  value <- Task.fromIO (Random.randomRIO (lowerBound, upperBound))
  Task.yield value
