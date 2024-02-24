{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module JSON (decodeSchema) where

-- TODO: Cleanup Lazy imports into its own data types

import Array qualified
import Core (todo)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonType
import Dsl
import HaskellCompatibility.Conversion qualified as Convert
import HaskellCompatibility.Syntax
import Operators
import Record
import String
import Traits.Schema qualified as Schema


decodeSchema :: Schema.SchemaDefinition -> Aeson.Value -> AesonType.Parser a
decodeSchema schema value = case schema of
  Schema.NoSchemaDefinition -> todo
  Schema.RecordSchemaDefinition recordSchema -> decodeRecordSchema recordSchema value


decodeRecordSchema :: Schema.RecordSchema -> Aeson.Value -> AesonType.Parser a
decodeRecordSchema recordSchema =
  Aeson.withObject (Convert.toLegacy "Expected JSON Object") <| \obj -> do
    let properties = recordSchema.recordProperties ?? []
    -- Dynamically construct the record based on its properties
    fields <- properties |> Array.applyToEach (decodePropertySchema obj)
    -- Here, `constructRecord` is a placeholder for actually constructing the Haskell value
    -- This part is highly dependent on your record types and how you map them from the schema
    return $ constructRecord fields


decodePropertySchema :: Aeson.Object -> Schema.PropertySchema -> AesonType.Parser (String, Aeson.Value)
decodePropertySchema obj propertySchema = do
  let name = propertySchema.propertyName
  let isOptional = propertySchema.propertyOptional
  if isOptional
    then fmap ((,) name) <$> obj .:? name
    else fmap ((,) name) <$> obj .: name