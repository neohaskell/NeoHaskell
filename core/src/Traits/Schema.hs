module Traits.Schema (
  Schema (..),
  SchemaDescription (..),
  SchemaProperty (..),
  SchemaBuilder (..),
) where

import Reflect qualified
import Types


data SchemaDescription = SchemaDescription
  { name :: String,
    description :: String,
    properties :: Array SchemaProperty
  }


data SchemaProperty = SchemaProperty
  { name :: String,
    description :: String,
    shorthand :: Optional String
  }


-- | `SchemaBuilder` is a data type that allows building a `Schema` for a record in a monadic way.
-- It allows writing do notation to build a `Schema` for a record. At the same time, it ensures that
-- the `Schema` is built correctly by allowing to constrain the functions that operate within it to
-- HasField constraints.
--
-- Example:
--
-- ```haskell
-- data User = User
--   { name :: String,
--     age :: Int
--   }
--
-- instance Schema User where
--   schema = do
--     name "User"
--     description "Defines a user"
--     property #name do
--       description "The name of the user"
--       shorthand "n"
--     property #age do
--       description "The age of the user"
--       shorthand "a"
-- ```
-- The monad will accumulate the SchemaDescription type, and the HasField constraints will ensure that
-- the properties are correctly defined.
data SchemaBuilder record a = SchemaBuilder
  deriving (Reflect.Shape, Reflect.TypeInfo)


-- TODO: Implement this using a Writer monad (figure out a Writer monad) ((Probably now is the time to figure out the `Ref` DSL to allow creating and modifying variables))

class Schema someType where
  schema :: SchemaBuilder someType ()