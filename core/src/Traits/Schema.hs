module Traits.Schema (
  Schematized (..),
  SchemaDefinition (..),
  SchemaDescriptor (..),
  RecordSchema (..),
  PropertySchema (..),
  PrimitiveSchema (..),
  shorthand,
  property,
  record,
  definition,
  Schema,
  getDefinition,
) where

import Accumulator qualified
import Array (Array)
import Array qualified
import Bool (Bool)
import Char (Char)
import Debug.ToDo (todo)
import HaskellCompatibility.Syntax
import Int (Int)
import Label
import Operators
import Optional (Optional (..))
import Record
import Reflect qualified
import String (String)
import Traits.Defaultable (Defaultable (..))
import Traits.Dsl (Dsl (..), (>>))
import Void (Void)


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
  | PrimitiveSchemaDefinition PrimitiveSchema
  | NoSchemaDefinition


instance Defaultable SchemaDefinition where
  defaultValue = NoSchemaDefinition


data PrimitiveSchema
  = StringSchema
  | IntSchema
  | FloatSchema
  | BooleanSchema
  | NullSchema
  | IdSchema


instance Schematized String where
  schema_impl = schemaOf (PrimitiveSchemaDefinition StringSchema)


instance Schematized Int where
  schema_impl = schemaOf (PrimitiveSchemaDefinition IntSchema)


-- instance Schematized Float where
--   schema_impl = schemaOf (PrimitiveSchemaDefinition FloatSchema)

instance Schematized Bool where
  schema_impl = schemaOf (PrimitiveSchemaDefinition BooleanSchema)


instance Schematized Void where
  schema_impl = schemaOf (PrimitiveSchemaDefinition NullSchema)


class SchemaDescriptor builder where
  description :: String -> builder someType ()
  named :: String -> builder someType ()


type Schema someType = SchemaBuilder someType ()


data SchemaBuilder someType result = SchemaBuilder
  {value :: Accumulator.AccumulatorDsl SchemaDefinition result}


schemaOf :: SchemaDefinition -> SchemaBuilder someType ()
schemaOf value = SchemaBuilder (Accumulator.update \_ -> value)


getDefinition :: SchemaBuilder someType () -> SchemaDefinition
getDefinition builder = Accumulator.accumulate builder.value


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


  named :: forall k (someType :: k). String -> RecordSchemaBuilder someType ()
  named txt = RecordSchemaBuilder do
    let addName recordSchema = recordSchema {recordName = Some txt}
    Accumulator.update addName


data PropertySchema = PropertySchema
  { propertyName :: Optional String,
    propertyDescription :: Optional String,
    propertyShorthand :: Optional Char,
    propertySchema :: Optional SchemaDefinition
  }


instance Defaultable PropertySchema where
  defaultValue =
    PropertySchema
      { propertyName = None,
        propertyDescription = None,
        propertyShorthand = None,
        propertySchema = None
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
    Label.Literal name,
    Schematized fieldType
  ) =>
  Label name ->
  PropertySchemaBuilder fieldType () ->
  RecordSchemaBuilder someType ()
property nameLabel builder = do
  let nameValue = Label.toString nameLabel
  let addProperty recordSchema = do
        let schema = getDefinition (schema_impl @fieldType)
        case schema of
          PrimitiveSchemaDefinition primitiveSchema -> do
            let builderWithName = do
                  named nameValue
                  typed primitiveSchema
                  builder
            let newPropertySchema = Accumulator.accumulate (builderWithName.value)
            let props = recordSchema.recordProperties ?? []
            recordSchema {recordProperties = Some (props |> Array.push newPropertySchema)}
          _ -> todo

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


typed :: PrimitiveSchema -> PropertySchemaBuilder someType ()
typed schema = PropertySchemaBuilder do
  let addSchema propertySchema = propertySchema {propertySchema = Some (PrimitiveSchemaDefinition schema)}
  Accumulator.update addSchema


class (Reflect.TypeInfo someType) => Schematized someType where
  schema_impl :: SchemaBuilder someType ()


definition :: forall name. (Label.Literal name) => Label name
definition = Label.Label