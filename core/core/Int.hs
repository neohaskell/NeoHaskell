module Int (
  toFloat,
) where

import Basics
import Prelude qualified


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
