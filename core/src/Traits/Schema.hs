module Traits.Schema (
  Schema (..),
  SchemaDefinition (..),
  SchemaDescriptor (..),
  RecordSchema (..),
  PropertySchema (..),
  shorthand,
  property,
  record,
  definition,
) where

import Accumulator qualified
import Array (Array)
import Array qualified
import HaskellCompatibility.Syntax
import Label
import Operators
import Optional (Optional (..))
import Record
import Reflect qualified
import String (String)
import Traits.Defaultable (Defaultable (..))
import Traits.Dsl (Dsl (..), (>>))


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
data SchemaDefinition
  = RecordSchemaDefinition RecordSchema
  | NoSchemaDefinition


class SchemaDescriptor builder where
  description :: String -> builder someType ()
  named :: String -> builder someType ()


data SchemaBuilder someType result = SchemaBuilder
  {value :: Accumulator.AccumulatorDsl SchemaDefinition result}


instance Dsl (SchemaBuilder someType) where
  andThen callback self =
    self.value
      |> andThen (\recordSchema -> (callback recordSchema).value)
      |> SchemaBuilder
  yield value =
    yield value
      |> SchemaBuilder


data RecordSchema = RecordSchema
  { recordName :: Optional String,
    recordDescription :: Optional String,
    recordProperties :: Optional (Array PropertySchema)
  }


data RecordSchemaBuilder someType result = RecordSchemaBuilder
  {value :: Accumulator.AccumulatorDsl RecordSchema result}


instance Dsl (RecordSchemaBuilder someType) where
  andThen callback self =
    self.value
      |> andThen (\recordSchema -> (callback recordSchema).value)
      |> RecordSchemaBuilder
  yield value =
    yield value
      |> RecordSchemaBuilder


instance Defaultable RecordSchema where
  defaultValue =
    RecordSchema
      { recordName = Optional.None,
        recordDescription = Optional.None,
        recordProperties = Optional.None
      }


record ::
  forall someType.
  (Reflect.TypeInfo someType) =>
  RecordSchemaBuilder someType () ->
  SchemaBuilder someType ()
record builder = do
  let recordName = Reflect.inspectTypeName @someType
  let recordSchema = do
        named recordName
        builder
  let x = Accumulator.accumulate recordSchema.value
  let result = do
        Accumulator.update (\_ -> RecordSchemaDefinition x)
  SchemaBuilder result


instance SchemaDescriptor RecordSchemaBuilder where
  description txt = RecordSchemaBuilder do
    let addDescription recordSchema = recordSchema {recordDescription = Some txt}
    Accumulator.update addDescription


  named txt = RecordSchemaBuilder do
    let addName recordSchema = recordSchema {recordName = Some txt}
    Accumulator.update addName


data PropertySchema = PropertySchema
  { propertyName :: Optional String,
    propertyDescription :: Optional String,
    propertyShorthand :: Optional String
  }


instance Defaultable PropertySchema where
  defaultValue =
    PropertySchema
      { propertyName = None,
        propertyDescription = None,
        propertyShorthand = None
      }


data PropertySchemaDsl someType a = PropertySchemaDsl
  { value :: (Accumulator.AccumulatorDsl PropertySchema a)
  }


instance Dsl (PropertySchemaDsl someType) where
  andThen callback self =
    self.value
      |> andThen (\propertySchema -> (callback propertySchema).value)
      |> PropertySchemaDsl
  yield value =
    yield value
      |> PropertySchemaDsl


property ::
  forall name someType fieldType.
  ( HasField name someType fieldType,
    Label.Literal name
  ) =>
  Label name ->
  PropertySchemaBuilder fieldType () ->
  RecordSchemaBuilder someType ()
property nameLabel builder = do
  let nameValue = Label.toString nameLabel
  let addProperty recordSchema = do
        let builderWithName = do
              named nameValue
              builder
        let newPropertySchema = Accumulator.accumulate (builderWithName.value)
        let props = recordSchema.recordProperties ?? []
        recordSchema {recordProperties = Some (props |> Array.push newPropertySchema)}
  RecordSchemaBuilder (Accumulator.update addProperty)


data PropertySchemaBuilder someType result = PropertySchemaBuilder
  {value :: Accumulator.AccumulatorDsl PropertySchema result}


instance Dsl (PropertySchemaBuilder someType) where
  andThen callback self =
    self.value
      |> andThen (\propertySchema -> (callback propertySchema).value)
      |> PropertySchemaBuilder
  yield value =
    yield value
      |> PropertySchemaBuilder


instance SchemaDescriptor PropertySchemaBuilder where
  description txt = PropertySchemaBuilder do
    let addDescription propertySchema = propertySchema {propertyDescription = Some txt}
    Accumulator.update addDescription
  named txt = PropertySchemaBuilder do
    let addName propertySchema = propertySchema {propertyName = Some txt}
    Accumulator.update addName


shorthand :: String -> PropertySchemaBuilder someType ()
shorthand txt = PropertySchemaBuilder do
  let addShorthand propertySchema = propertySchema {propertyShorthand = Some txt}
  Accumulator.update addShorthand


class Reflect.TypeInfo someType => Schema someType where
  schema :: SchemaBuilder someType ()


definition :: forall name. Label.Literal name => Label name
definition = Label.Label