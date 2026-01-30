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

import Array (Array)
import Array qualified
import Basics
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Lens qualified as OpenApiLens
import Lens.Micro qualified as Lens
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Schema (FieldSchema (..), Schema (..))
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
    OpenApi.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiNull)
  
  SBool -> do
    OpenApi.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiBoolean)
  
  SInt -> do
    OpenApi.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiInteger)
  
  SNumber -> do
    OpenApi.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiNumber)
  
  SText -> do
    OpenApi.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiString)
  
  SArray itemSchema -> do
    let itemsSchema = toOpenApiSchema itemSchema
    OpenApi.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiArray)
      |> Lens.set OpenApiLens.items (Just (OpenApi.OpenApiItemsObject (OpenApi.Inline itemsSchema)))
  
  SOptional innerSchema -> do
    toOpenApiSchema innerSchema
  
  SObject fields -> do
    let properties = fields
          |> Array.map (\field -> (field.fieldName, OpenApi.Inline (toOpenApiSchema field.fieldSchema)))
          |> Array.toLinkedList
          |> Map.fromList
    let required = fields
          |> Array.filter (\field -> field.fieldRequired)
          |> Array.map (\field -> field.fieldName)
          |> Array.toLinkedList
    OpenApi.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiObject)
      |> Lens.set OpenApiLens.properties properties
      |> Lens.set OpenApiLens.required required
  
  SEnum variants -> do
    let enumValues = variants
          |> Array.map (\name -> Text.toLower name)
          |> Array.toLinkedList
    OpenApi.mempty
      |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiString)
      |> Lens.set OpenApiLens.enum_ (Just (enumValues |> map (\t -> OpenApi.toJSON t)))
  
  SUnion variants -> do
    let variantSchemas = variants
          |> Array.map (\(name, variantSchema) -> toOpenApiSchema variantSchema)
          |> Array.toLinkedList
    OpenApi.mempty
      |> Lens.set OpenApiLens.oneOf (Just (variantSchemas |> map OpenApi.Inline))
  
  SRef refName -> do
    OpenApi.mempty
      |> Lens.set OpenApiLens.ref (Just (OpenApi.Reference refName))


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
  let info = OpenApi.mempty
        |> Lens.set OpenApiLens.title apiInfo.apiTitle
        |> Lens.set OpenApiLens.version apiInfo.apiVersion
        |> Lens.set OpenApiLens.description (Just apiInfo.apiDescription)
  
  let commandPaths = commandSchemas
        |> Map.entries
        |> Array.map (\(name, schema) -> makeCommandPath name schema)
        |> Array.toLinkedList
        |> Map.fromList
  
  let queryPaths = querySchemas
        |> Map.entries
        |> Array.map (\(name, schema) -> makeQueryPath name schema)
        |> Array.toLinkedList
        |> Map.fromList
  
  let allPaths = Map.merge commandPaths queryPaths
  
  OpenApi.mempty
    |> Lens.set OpenApiLens.info info
    |> Lens.set OpenApiLens.openapi "3.0.0"
    |> Lens.set OpenApiLens.paths (OpenApi.Paths allPaths)


-- | Create OpenAPI path item for a command endpoint.
--
-- Commands use POST /commands/{name} with a request body.
makeCommandPath :: Text -> EndpointSchema -> (Text, OpenApi.PathItem)
makeCommandPath name schema = do
  let path = [fmt|/commands/{name}|]
  let requestBody = case schema.requestSchema of
        Nothing -> Nothing
        Just reqSchema -> do
          let content = Map.fromList
                [ ("application/json", OpenApi.mempty
                    |> Lens.set OpenApiLens.schema (Just (OpenApi.Inline (toOpenApiSchema reqSchema)))
                  )
                ]
          Just (OpenApi.Inline (OpenApi.mempty
            |> Lens.set OpenApiLens.required (Just True)
            |> Lens.set OpenApiLens.content content
          ))
  
  let responseContent = Map.fromList
        [ ("application/json", OpenApi.mempty
            |> Lens.set OpenApiLens.schema (Just (OpenApi.Inline (toOpenApiSchema schema.responseSchema)))
          )
        ]
  
  let responses = OpenApi.Responses
        { OpenApi._responsesDefault = Nothing
        , OpenApi._responsesResponses = Map.fromList
            [ ("200", OpenApi.Inline (OpenApi.mempty
                |> Lens.set OpenApiLens.description "Success"
                |> Lens.set OpenApiLens.content responseContent
              ))
            , ("400", OpenApi.Inline (OpenApi.mempty
                |> Lens.set OpenApiLens.description "Bad Request"
              ))
            , ("401", OpenApi.Inline (OpenApi.mempty
                |> Lens.set OpenApiLens.description "Unauthorized"
              ))
            , ("500", OpenApi.Inline (OpenApi.mempty
                |> Lens.set OpenApiLens.description "Internal Server Error"
              ))
            ]
        }
  
  let operation = OpenApi.mempty
        |> Lens.set OpenApiLens.summary (Just name)
        |> Lens.set OpenApiLens.description (Just schema.description)
        |> Lens.set OpenApiLens.deprecated (Just schema.deprecated)
        |> Lens.set OpenApiLens.requestBody requestBody
        |> Lens.set OpenApiLens.responses responses
  
  let pathItem = OpenApi.mempty
        |> Lens.set OpenApiLens.post (Just operation)
  
  (path, pathItem)


-- | Create OpenAPI path item for a query endpoint.
--
-- Queries use GET /queries/{name} with no request body.
makeQueryPath :: Text -> EndpointSchema -> (Text, OpenApi.PathItem)
makeQueryPath name schema = do
  let path = [fmt|/queries/{name}|]
  
  let responseContent = Map.fromList
        [ ("application/json", OpenApi.mempty
            |> Lens.set OpenApiLens.schema (Just (OpenApi.Inline (toOpenApiSchema schema.responseSchema)))
          )
        ]
  
  let responses = OpenApi.Responses
        { OpenApi._responsesDefault = Nothing
        , OpenApi._responsesResponses = Map.fromList
            [ ("200", OpenApi.Inline (OpenApi.mempty
                |> Lens.set OpenApiLens.description "Success"
                |> Lens.set OpenApiLens.content responseContent
              ))
            , ("401", OpenApi.Inline (OpenApi.mempty
                |> Lens.set OpenApiLens.description "Unauthorized"
              ))
            , ("403", OpenApi.Inline (OpenApi.mempty
                |> Lens.set OpenApiLens.description "Forbidden"
              ))
            , ("500", OpenApi.Inline (OpenApi.mempty
                |> Lens.set OpenApiLens.description "Internal Server Error"
              ))
            ]
        }
  
  let operation = OpenApi.mempty
        |> Lens.set OpenApiLens.summary (Just name)
        |> Lens.set OpenApiLens.description (Just schema.description)
        |> Lens.set OpenApiLens.deprecated (Just schema.deprecated)
        |> Lens.set OpenApiLens.responses responses
  
  let pathItem = OpenApi.mempty
        |> Lens.set OpenApiLens.get (Just operation)
  
  (path, pathItem)


-- | ApiInfo type for spec generation.
--
-- This is re-exported from Service.Application but defined here
-- to avoid circular dependencies.
data ApiInfo = ApiInfo
  { apiTitle :: Text
  , apiVersion :: Text
  , apiDescription :: Text
  }
