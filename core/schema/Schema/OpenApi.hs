{-# LANGUAGE OverloadedStrings #-}

-- | OpenAPI 3.0 spec generation from NeoHaskell Schema types.
--
-- This module converts library-agnostic Schema representations to OpenAPI
-- specifications using the openapi3 package.
--
-- Usage:
--
-- @
-- import Schema qualified
-- import Schema.OpenApi qualified as OpenApi
--
-- -- Convert a Schema to OpenAPI Schema
-- let schema = Schema.SObject [...]
-- let openApiSchema = OpenApi.toOpenApiSchema schema
--
-- -- Generate full OpenAPI spec
-- let apiInfo = ApiInfo "My API" "1.0.0" "API description"
-- let spec = OpenApi.toOpenApiSpec apiInfo cmdSchemas querySchemas
-- @
module Schema.OpenApi (
  toOpenApiSchema,
  toOpenApiSpec,
) where

import Array qualified
import Basics
import Data.Monoid qualified as GhcMonoid
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Lens qualified as OpenApiLens
import Control.Lens qualified as Lens
import Data.Aeson qualified as Json
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import LinkedList qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Schema (FieldSchema (..), Schema (..))
import Service.Application.Types (ApiInfo (..))
import Service.Transport (EndpointSchema (..))
import Text (Text)
import Text qualified


-- | Convert NeoHaskell Schema to OpenAPI Schema.
--
-- This handles all Schema constructors and produces a corresponding
-- OpenAPI schema representation.
toOpenApiSchema :: Schema -> OpenApi.Schema
toOpenApiSchema schema = case schema of
  SNull -> do
    GhcMonoid.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiNull)
  
  SBool -> do
    GhcMonoid.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiBoolean)
  
  SInt -> do
    GhcMonoid.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiInteger)
  
  SNumber -> do
    GhcMonoid.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiNumber)
  
  SText -> do
    GhcMonoid.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiString)
  
  SArray itemSchema -> do
    let itemsSchema = toOpenApiSchema itemSchema
    GhcMonoid.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiArray)
      |> Lens.set OpenApiLens.items (Just (OpenApi.OpenApiItemsObject (OpenApi.Inline itemsSchema)))
  
  SOptional innerSchema -> do
    toOpenApiSchema innerSchema
      |> Lens.set OpenApiLens.nullable (Just True)
  
  SObject fields -> do
    let properties = fields
          |> Array.map (\field -> (field.fieldName, OpenApi.Inline (toOpenApiSchema field.fieldSchema)))
          |> Array.toLinkedList
          |> InsOrdHashMap.fromList
    let required = fields
          |> Array.takeIf (\field -> field.fieldRequired)
          |> Array.map (\field -> field.fieldName)
          |> Array.toLinkedList
    GhcMonoid.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiObject)
      |> Lens.set OpenApiLens.properties properties
      |> Lens.set OpenApiLens.required required
  
  SEnum variants -> do
    let enumValues = variants
          |> Array.map (\name -> Text.toLower name)
          |> Array.toLinkedList
    GhcMonoid.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiString)
      |> Lens.set OpenApiLens.enum_ (Just (enumValues |> LinkedList.map (\t -> Json.toJSON t)))
  
  SUnion variants -> do
    let variantSchemas = variants
          |> Array.map (\(_name, variantSchema) -> toOpenApiSchema variantSchema)
          |> Array.toLinkedList
    GhcMonoid.mempty
      |> Lens.set OpenApiLens.oneOf (Just (variantSchemas |> LinkedList.map OpenApi.Inline))
  
  SRef refName -> do
    -- Note: Schema references in OpenAPI are handled at the usage site via Referenced type,
    -- not as a field on Schema itself. We encode the reference as the schema title.
    GhcMonoid.mempty
      |> Lens.set OpenApiLens.title (Just refName)
      |> Lens.set OpenApiLens.description (Just [fmt|Reference to: {refName}|])


-- | Generate OpenAPI 3.0 specification from endpoint schemas.
--
-- Creates a complete OpenAPI spec with:
-- - Info section (title, version, description)
-- - Paths for /commands/{name} and /queries/{name}
-- - Request/response schemas
-- - Standard HTTP status codes
toOpenApiSpec ::
  ApiInfo ->
  Map Text EndpointSchema ->
  Map Text EndpointSchema ->
  OpenApi.OpenApi
toOpenApiSpec apiInfo commandSchemas querySchemas = do
  let info = GhcMonoid.mempty
        |> Lens.set OpenApiLens.title apiInfo.apiTitle
        |> Lens.set OpenApiLens.version apiInfo.apiVersion
        |> Lens.set OpenApiLens.description (Just apiInfo.apiDescription)
  
  let commandPaths = commandSchemas
        |> Map.entries
        |> Array.map (\(name, schema) -> makeCommandPath name schema)
        |> Array.map (\(path, item) -> (Text.toLinkedList path, item))
        |> Array.toLinkedList
        |> InsOrdHashMap.fromList
  
  let queryPaths = querySchemas
        |> Map.entries
        |> Array.map (\(name, schema) -> makeQueryPath name schema)
        |> Array.map (\(path, item) -> (Text.toLinkedList path, item))
        |> Array.toLinkedList
        |> InsOrdHashMap.fromList
  
  let allPaths = InsOrdHashMap.union commandPaths queryPaths
  
  GhcMonoid.mempty
    |> Lens.set OpenApiLens.info info
    |> Lens.set OpenApiLens.paths allPaths


-- | Create OpenAPI path item for a command endpoint.
--
-- Commands use POST /commands/{name} with a request body.
makeCommandPath :: Text -> EndpointSchema -> (Text, OpenApi.PathItem)
makeCommandPath name schema = do
  let path = [fmt|/commands/{name}|]
  let requestBody = case schema.requestSchema of
        Nothing -> Nothing
        Just reqSchema ->
          Just (OpenApi.Inline (GhcMonoid.mempty
            |> Lens.set OpenApiLens.required (Just True)
            |> Lens.set OpenApiLens.content (InsOrdHashMap.fromList
                  [ ("application/json", GhcMonoid.mempty
                      |> Lens.set OpenApiLens.schema (Just (OpenApi.Inline (toOpenApiSchema reqSchema)))
                    )
                  ])
          ))
  
  let responseContent = InsOrdHashMap.fromList
        [ ("application/json", GhcMonoid.mempty
            |> Lens.set OpenApiLens.schema (Just (OpenApi.Inline (toOpenApiSchema schema.responseSchema)))
          )
        ]
  
  let responses = OpenApi.Responses
        { OpenApi._responsesDefault = Nothing
        , OpenApi._responsesResponses = InsOrdHashMap.fromList
            [ (200, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Success"
                |> Lens.set OpenApiLens.content responseContent
              ))
            , (400, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Bad Request"
              ))
            , (401, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Unauthorized"
              ))
            , (500, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Internal Server Error"
              ))
            ]
        }
  
  let operation = GhcMonoid.mempty
        |> Lens.set OpenApiLens.summary (Just name)
        |> Lens.set OpenApiLens.description (Just schema.description)
        |> Lens.set OpenApiLens.deprecated (Just schema.deprecated)
        |> Lens.set OpenApiLens.requestBody requestBody
        |> Lens.set OpenApiLens.responses responses
  
  let pathItem = GhcMonoid.mempty
        |> Lens.set OpenApiLens.post (Just operation)
  
  (path, pathItem)


-- | Create OpenAPI path item for a query endpoint.
--
-- Queries use GET /queries/{name} with no request body.
makeQueryPath :: Text -> EndpointSchema -> (Text, OpenApi.PathItem)
makeQueryPath name schema = do
  let path = [fmt|/queries/{name}|]
  
  let responseContent = InsOrdHashMap.fromList
        [ ("application/json", GhcMonoid.mempty
            |> Lens.set OpenApiLens.schema (Just (OpenApi.Inline (toOpenApiSchema schema.responseSchema)))
          )
        ]
  
  let responses = OpenApi.Responses
        { OpenApi._responsesDefault = Nothing
        , OpenApi._responsesResponses = InsOrdHashMap.fromList
            [ (200, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Success"
                |> Lens.set OpenApiLens.content responseContent
              ))
            , (401, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Unauthorized"
              ))
            , (403, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Forbidden"
              ))
            , (500, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Internal Server Error"
              ))
            ]
        }
  
  let operation = GhcMonoid.mempty
        |> Lens.set OpenApiLens.summary (Just name)
        |> Lens.set OpenApiLens.description (Just schema.description)
        |> Lens.set OpenApiLens.deprecated (Just schema.deprecated)
        |> Lens.set OpenApiLens.responses responses
  
  let pathItem = GhcMonoid.mempty
        |> Lens.set OpenApiLens.get (Just operation)
  
  (path, pathItem)
