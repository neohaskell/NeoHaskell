module Traits.Applicable where

import Traits.Mappable


-- | # Applicable
--
-- This trait specifies that some value contains a function that can be
-- applied to the contents of another value of the same type
class Mappable context => Applicable context where
  -- | # apply
  --
  -- This function is used to apply the function inside of the context
  -- to the value inside of another context.
  apply :: context (input -> output) -> context input -> context output