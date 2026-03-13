module Schema.JsonSchema
  ( -- * Conversion
    toJsonSchema
  ) where

import Array qualified
import Basics
import Json qualified
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Schema (FieldSchema (..), Schema (..))
import Text (Text)
import Text qualified


-- | Convert a NeoHaskell Schema to a JSON Schema Aeson Value.
--
-- Follows JSON Schema draft-07 semantics. See conversion rules below.
{-# INLINEABLE toJsonSchema #-}
toJsonSchema :: Schema -> Json.Value
toJsonSchema schema = case schema of
  SNull ->
    Json.object [("type", Json.toJSON ("null" :: Text))]

  SBool ->
    Json.object [("type", Json.toJSON ("boolean" :: Text))]

  SInt ->
    Json.object [("type", Json.toJSON ("integer" :: Text))]

  SNumber ->
    Json.object [("type", Json.toJSON ("number" :: Text))]

  SText ->
    Json.object [("type", Json.toJSON ("string" :: Text))]

  SArray innerSchema -> do
    let items = toJsonSchema innerSchema
    Json.object
      [ ("type", Json.toJSON ("array" :: Text))
      , ("items", items)
      ]

  SOptional innerSchema ->
    toJsonSchema innerSchema

  SObject fields -> do
    let properties =
          fields
            |> Array.map (\field -> (field.fieldName, fieldPropertySchema field))
            |> Array.toLinkedList
    let requiredNames =
          fields
            |> Array.takeIf (\field -> field.fieldRequired)
            |> Array.map (\field -> field.fieldName)
            |> Array.toLinkedList
    Json.object
      [ ("type", Json.toJSON ("object" :: Text))
      , ("properties", Json.object properties)
      , ("required", Json.toJSON requiredNames)
      , ("additionalProperties", Json.toJSON False)
      ]

  SEnum variants -> do
    let enumValues = variants |> Array.toLinkedList
    Json.object
      [ ("type", Json.toJSON ("string" :: Text))
      , ("enum", Json.toJSON enumValues)
      ]

  SUnion variants -> do
    let oneOf =
          variants
            |> Array.map (\(ctorName, variantSchema) -> do
                let innerSchema = toJsonSchema variantSchema
                Json.object
                  [ ("type", Json.toJSON ("object" :: Text))
                  , ("properties", Json.object
                      [ ("tag", Json.object [("const", Json.toJSON ctorName)])
                      , ("contents", innerSchema)
                      ])
                  , ("required", Json.toJSON (["tag", "contents"] :: [Text]))
                  , ("additionalProperties", Json.toJSON False)
                  ])
            |> Array.toLinkedList
    Json.object [("oneOf", Json.toJSON oneOf)]

  SRef refName ->
    Json.object [("$ref", Json.toJSON (Text.append "#/definitions/" refName))]


-- | Build a JSON Schema for an object property, including "description" when non-empty.
fieldPropertySchema :: FieldSchema -> Json.Value
fieldPropertySchema field = do
  let baseSchema = toJsonSchema field.fieldSchema
  if Text.isEmpty field.fieldDescription
    then baseSchema
    else addDescription field.fieldDescription baseSchema


-- | Insert a "description" key into a JSON Schema Value.
-- If the value is a JSON object, inserts the key. Otherwise returns the value unchanged.
addDescription :: Text -> Json.Value -> Json.Value
addDescription desc value = case value of
  Aeson.Object km ->
    Aeson.Object (AesonKeyMap.insert (AesonKey.fromText "description") (Json.toJSON desc) km)
  _ ->
    value
