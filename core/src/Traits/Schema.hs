module Traits.Schema (
  Schema (..),
  SchemaDescription (..),
  SchemaProperty (..),
  SchemaBuilder (..),
) where

import Debug (todo)
import Record
import Reflect qualified
import Types


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
--   schema = record "User" do
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
data SchemaBuilder someType a = SchemaBuilder
  deriving (Reflect.Shape, Reflect.TypeInfo)


class SchemaDescriptor builder where
  description :: String -> builder someType ()
  name :: String -> builder someType ()
  shorthand :: String -> builder someType ()


data RecordSchema = SchemaDescription
  { recordName :: String,
    recordDescription :: String,
    recordProperties :: Array PropertySchema
  }


data RecordSchemaBuilder someType a = RecordSchemaBuilder
  deriving (Reflect.Shape, Reflect.TypeInfo)


record ::
  String ->
  RecordSchemaBuilder someType () ->
  SchemaBuilder someType RecordSchema
record name builder = todo


data PropertySchema = PropertySchema
  { propertyName :: String,
    propertyDescription :: String,
    propertyShorthand :: Optional String
  }
  deriving (Reflect.Shape, Reflect.TypeInfo)


data PropertySchemaBuilder someType a = PropertySchemaBuilder
  deriving (Reflect.Shape, Reflect.TypeInfo)


property ::
  HasField name someType fieldType =>
  Label name ->
  PropertySchemaBuilder someType () ->
  RecordSchemaBuilder someType PropertySchema
property name builder = todo


-- TODO: Implement this using a Writer monad (figure out a Writer monad) ((Probably now is the time to figure out the `Ref` DSL to allow creating and modifying variables))

class Schema someType where
  schema :: SchemaBuilder someType ()