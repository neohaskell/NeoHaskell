module Int (
  toFloat,
  fromInt64,
  toInt64,
  getRandom,
  getRandomBetween,
  powerOf,
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


-- | Convert an Int64 to an Int safely.
-- Clamps to minBound/maxBound if value exceeds Int range (prevents silent truncation).
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


-- * Exponentiation


-- | Raise a base to the power of an exponent.
--
-- Computes @base^exponent@ using integer arithmetic.
-- Negative exponents are treated as 0 (returns 1).
--
-- >>> 3 |> Int.powerOf 2
-- 8
-- -- 2^3 = 8
--
-- >>> 10 |> Int.powerOf 2
-- 1024
-- -- 2^10 = 1024
--
-- >>> 0 |> Int.powerOf 5
-- 1
-- -- 5^0 = 1
--
-- >>> (-1) |> Int.powerOf 2
-- 1
-- -- Negative exponent treated as 0
powerOf :: Int -> Int -> Int
powerOf base exponent = go (max 0 exponent) 1
  where
    go 0 acc = acc
    go n acc = go (n - 1) (acc * base)
