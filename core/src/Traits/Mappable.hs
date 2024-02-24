module Traits.Mappable (Mappable (..)) where


-- | # Mappable
--
-- This trait specifies that some type contains a value that can be
-- transformed using a function.
class Mappable context where
  -- | # map
  --
  -- This function is used to transform the value inside of the context
  -- using a function.
  map :: (input -> output) -> context input -> context output