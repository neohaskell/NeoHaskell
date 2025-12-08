module Float (
  toInt,
) where

import Basics
import Prelude qualified


-- * Float to Int conversion


-- | Convert a float to an integer by truncating towards zero.
--
-- >>> Float.toInt 3.7 == 3
-- >>> Float.toInt 3.2 == 3
-- >>> Float.toInt (-3.7) == -3
-- >>> Float.toInt (-3.2) == -3
toInt :: Float -> Int
toInt =
  Prelude.truncate