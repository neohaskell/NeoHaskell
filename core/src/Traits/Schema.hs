module Traits.Schema (
  Schema (..),
  SchemaBuilder (..),
) where

import Accumulator (Accumulator, AccumulatorDsl (..))
import Data.Coerce (coerce)
import Debug (todo)
import Dsl (Dsl (..))
import HaskellCompatibility.Syntax
import Label
import Operators
import Optional (Optional (None))
import Record
import Reflect qualified
import Traits.Defaultable (Defaultable (..))
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
  { recordName :: Optional String,
    recordDescription :: Optional String,
    recordProperties :: Optional (Array PropertySchema)
  }


data RecordSchemaBuilder someType a = RecordSchemaBuilder
  deriving (Reflect.Shape, Reflect.TypeInfo)


instance Defaultable RecordSchema where
  defaultValue =
    SchemaDescription
      { recordName = Nothing,
        recordDescription = Nothing,
        recordProperties = Nothing
      }


record ::
  String ->
  RecordSchemaBuilder someType () ->
  SchemaBuilder someType RecordSchema
record name builder = todo


data PropertySchema = PropertySchema
  { propertyName :: Optional String,
    propertyDescription :: Optional String,
    propertyShorthand :: Optional String
  }
  deriving (Reflect.Shape, Reflect.TypeInfo)


instance Defaultable PropertySchema where
  defaultValue =
    PropertySchema
      { propertyName = None,
        propertyDescription = None,
        propertyShorthand = None
      }


data PropertySchemaDsl someType a = PropertySchemaDsl
  { value :: (AccumulatorDsl PropertySchema a)
  }


instance Dsl (PropertySchemaDsl someType) where
  andThen callback self =
    self.value
      |> andThen (callback .> getField #value)
      |> PropertySchemaDsl
  yield value =
    yield value
      |> PropertySchemaDsl


property ::
  HasField name someType fieldType =>
  Label name ->
  PropertySchemaDsl someType () ->
  RecordSchemaBuilder someType PropertySchema
property name builder = todo


class Schema someType where
  schema :: SchemaBuilder someType ()