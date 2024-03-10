module Traits.Defaultable (Defaultable (..), with) where

import Bool
import HaskellCompatibility.Syntax
import Int
import String


-- | The `Defaultable` trait defines that a type has a default value.
-- It is useful to define default values for fields in records, or
-- to define default values for properties in a schema.
class Defaultable someType where
  -- | The default value for the type.
  defaultValue :: someType


with :: (Defaultable someType) => someType
with = defaultValue


instance Defaultable String where
  defaultValue = ""


instance Defaultable Bool where
  defaultValue = False


instance Defaultable Int where
  defaultValue = 0