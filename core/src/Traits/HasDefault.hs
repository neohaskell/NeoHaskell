module Traits.HasDefault (HasDefault (..)) where


-- | The `HasDefault` trait defines that a type has a default value.
-- It is useful to define default values for fields in records, or
-- to define default values for properties in a schema.
class HasDefault someType where
  -- | The default value for the type.
  defaultValue :: someType
