{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Optional documentation metadata for types.
--
-- All fields have defaults - implement only what you need.
--
-- Usage:
--
-- @
-- instance Documented AddItem where
--   description = "Add an item to a shopping cart"
--   examples =
--     [ AddItem { cartId = exampleUuid, productId = "SKU-123", quantity = 2 }
--     ]
-- @
module Documented (
  Documented (..),
) where

import Array (Array)
import Array qualified
import Basics
import Text (Text)
import TypeName qualified


-- | Optional documentation metadata for types.
--
-- All fields have defaults - implement only what you need:
--
-- - 'name': Human-readable name. Defaults to type name via TypeName.reflect.
-- - 'description': Description of what this type represents. Defaults to empty.
-- - 'examples': Example values for documentation. Defaults to empty array.
-- - 'deprecated': Whether this type is deprecated. Defaults to False.
class (TypeName.Inspectable value) => Documented value where
  -- | Human-readable name.
  -- Defaults to type name via TypeName.reflect.
  name :: Text
  default name :: Text
  name = TypeName.reflect @value

  -- | Description of what this type represents.
  -- Defaults to empty string.
  description :: Text
  default description :: Text
  description = ""

  -- | Example values for documentation.
  -- Defaults to empty array.
  examples :: Array value
  default examples :: Array value
  examples = Array.empty

  -- | Whether this type is deprecated.
  -- Defaults to False.
  deprecated :: Bool
  default deprecated :: Bool
  deprecated = False
