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
import Maybe qualified
import Schema (FieldSchema (..), Schema (..))
import Service.Application.Types (ApiInfo (..))
import Service.Transport (EndpointSchema (..))
import Set (Set)
import Set qualified
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
          |> Array.map (\(name, variantSchema) -> 
               toOpenApiSchema variantSchema
                 |> Lens.set OpenApiLens.title (Just name)
             )
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
-- - Tags for grouping endpoints (entity names for commands, "Queries" for queries,
--   "Authentication" for OAuth2, "Files" for file upload)
-- - Paths for all endpoint types
-- - Request/response schemas
-- - Standard HTTP status codes
toOpenApiSpec ::
  ApiInfo ->
  Map Text EndpointSchema ->  -- ^ Command schemas (tagged by entity name)
  Map Text EndpointSchema ->  -- ^ Query schemas (tagged "Queries")
  Map Text EndpointSchema ->  -- ^ OAuth2 schemas (tagged "Authentication")
  Map Text EndpointSchema ->  -- ^ File upload schemas (tagged "Files")
  OpenApi.OpenApi
toOpenApiSpec apiInfo commandSchemas querySchemas oauth2Schemas fileSchemas = do
  let info = GhcMonoid.mempty
        |> Lens.set OpenApiLens.title apiInfo.apiTitle
        |> Lens.set OpenApiLens.version apiInfo.apiVersion
        |> Lens.set OpenApiLens.description (Just apiInfo.apiDescription)

  -- Collect unique entity names from commands for tags
  let commandEntityNames = commandSchemas
        |> Map.values
        |> Array.foldl (\acc schema ->
             case schema.entityName of
               Just name -> Set.insert name acc
               Nothing -> acc
           ) Set.empty

  -- Build list of all tag names
  let hasQueries = not (Map.isEmpty querySchemas)
  let hasOAuth2 = not (Map.isEmpty oauth2Schemas)
  let hasFiles = not (Map.isEmpty fileSchemas)

  let allTagNames = commandEntityNames
        |> Set.toArray
        |> (\names -> if hasQueries then Array.prepend "Queries" names else names)
        |> (\names -> if hasOAuth2 then Array.prepend "Authentication" names else names)
        |> (\names -> if hasFiles then Array.prepend "Files" names else names)

  -- Generate OpenAPI tag definitions
  let tags = allTagNames
        |> Array.map (\tagName -> GhcMonoid.mempty
             |> Lens.set OpenApiLens.name tagName
             |> Lens.set OpenApiLens.description (Just [fmt|#{tagName} endpoints|])
           )
        |> Array.toLinkedList

  -- Generate paths for each endpoint type
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

  let oauth2Paths = oauth2Schemas
        |> Map.entries
        |> Array.map (\(name, schema) -> makeOAuth2Path name schema)
        |> Array.map (\(path, item) -> (Text.toLinkedList path, item))
        |> Array.toLinkedList
        |> InsOrdHashMap.fromList

  let filePaths = fileSchemas
        |> Map.entries
        |> Array.map (\(name, schema) -> makeFilePath name schema)
        |> Array.map (\(path, item) -> (Text.toLinkedList path, item))
        |> Array.toLinkedList
        |> InsOrdHashMap.fromList

  let allPaths = commandPaths
        |> InsOrdHashMap.union queryPaths
        |> InsOrdHashMap.union oauth2Paths
        |> InsOrdHashMap.union filePaths

  GhcMonoid.mempty
    |> Lens.set OpenApiLens.info info
    |> Lens.set OpenApiLens.tags tags
    |> Lens.set OpenApiLens.paths allPaths


-- | Create OpenAPI path item for a command endpoint.
--
-- Commands use POST /commands/{name} with a request body.
makeCommandPath :: Text -> EndpointSchema -> (Text, OpenApi.PathItem)
makeCommandPath name schema = do
  let kebabName = name |> Text.toKebabCase
  let path = [fmt|/commands/#{kebabName}|]
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
            , (403, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Forbidden"
              ))
            , (500, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Internal Server Error"
              ))
            ]
        }
  
  -- Assign tag from entity name (or "API" as fallback)
  let tag = schema.entityName |> Maybe.withDefault "API"

  let operation = GhcMonoid.mempty
        |> Lens.set OpenApiLens.summary (Just name)
        |> Lens.set OpenApiLens.description (Just schema.description)
        |> Lens.set OpenApiLens.deprecated (Just schema.deprecated)
        |> Lens.set OpenApiLens.tags [tag]
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
  let kebabName = name |> Text.toKebabCase
  let path = [fmt|/queries/#{kebabName}|]
  
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
  
  -- All queries use the fixed "Queries" tag
  let operation = GhcMonoid.mempty
        |> Lens.set OpenApiLens.summary (Just name)
        |> Lens.set OpenApiLens.description (Just schema.description)
        |> Lens.set OpenApiLens.deprecated (Just schema.deprecated)
        |> Lens.set OpenApiLens.tags ["Queries"]
        |> Lens.set OpenApiLens.responses responses

  let pathItem = GhcMonoid.mempty
        |> Lens.set OpenApiLens.get (Just operation)

  (path, pathItem)


-- | Create OpenAPI path item for an OAuth2 endpoint.
--
-- OAuth2 endpoints use path parameters for provider name:
-- - GET /connect/{provider}
-- - GET /callback/{provider}
-- - DELETE /disconnect/{provider}
makeOAuth2Path :: Text -> EndpointSchema -> (Text, OpenApi.PathItem)
makeOAuth2Path name schema = do
  let path = case name of
        "connect" -> "/connect/{provider}"
        "callback" -> "/callback/{provider}"
        "disconnect" -> "/disconnect/{provider}"
        _ -> [fmt|/oauth2/#{name}|]

  let providerParam = GhcMonoid.mempty
        |> Lens.set OpenApiLens.name "provider"
        |> Lens.set OpenApiLens.in_ OpenApi.ParamPath
        |> Lens.set OpenApiLens.required (Just True)
        |> Lens.set OpenApiLens.schema (Just (OpenApi.Inline (GhcMonoid.mempty
             |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiString)
             |> Lens.set OpenApiLens.description (Just "OAuth2 provider identifier (e.g., google, github)")
           )))

  let responses = OpenApi.Responses
        { OpenApi._responsesDefault = Nothing
        , OpenApi._responsesResponses = InsOrdHashMap.fromList
            [ (302, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Redirect to OAuth2 provider or callback URL"
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
        |> Lens.set OpenApiLens.summary (Just [fmt|OAuth2 #{name}|])
        |> Lens.set OpenApiLens.description (Just schema.description)
        |> Lens.set OpenApiLens.tags ["Authentication"]
        |> Lens.set OpenApiLens.parameters [OpenApi.Inline providerParam]
        |> Lens.set OpenApiLens.responses responses

  let pathItem = case name of
        "disconnect" -> GhcMonoid.mempty |> Lens.set OpenApiLens.delete (Just operation)
        _ -> GhcMonoid.mempty |> Lens.set OpenApiLens.get (Just operation)

  (path, pathItem)


-- | Create OpenAPI path item for a file endpoint.
--
-- File endpoints:
-- - POST /files/upload (multipart/form-data)
-- - GET /files/{fileRef}
makeFilePath :: Text -> EndpointSchema -> (Text, OpenApi.PathItem)
makeFilePath name schema = do
  case name of
    "upload" -> makeFileUploadPath schema
    "download" -> makeFileDownloadPath schema
    _ -> ([fmt|/files/#{name}|], GhcMonoid.mempty)


-- | Create path item for POST /files/upload
makeFileUploadPath :: EndpointSchema -> (Text, OpenApi.PathItem)
makeFileUploadPath schema = do
  let path = "/files/upload"

  let requestBody = Just (OpenApi.Inline (GhcMonoid.mempty
        |> Lens.set OpenApiLens.required (Just True)
        |> Lens.set OpenApiLens.content (InsOrdHashMap.fromList
              [ ("multipart/form-data", GhcMonoid.mempty
                  |> Lens.set OpenApiLens.schema (Just (OpenApi.Inline (GhcMonoid.mempty
                       |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiObject)
                       |> Lens.set OpenApiLens.properties (InsOrdHashMap.fromList
                            [ ("file", OpenApi.Inline (GhcMonoid.mempty
                                |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiString)
                                |> Lens.set OpenApiLens.format (Just "binary")
                                |> Lens.set OpenApiLens.description (Just "File to upload")
                              ))
                            ])
                       |> Lens.set OpenApiLens.required ["file"]
                     )))
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
                |> Lens.set OpenApiLens.description "File uploaded successfully"
                |> Lens.set OpenApiLens.content responseContent
              ))
            , (400, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Bad Request"
              ))
            , (401, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Unauthorized"
              ))
            , (413, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Payload Too Large"
              ))
            , (500, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Internal Server Error"
              ))
            ]
        }

  let operation = GhcMonoid.mempty
        |> Lens.set OpenApiLens.summary (Just "Upload file")
        |> Lens.set OpenApiLens.description (Just schema.description)
        |> Lens.set OpenApiLens.tags ["Files"]
        |> Lens.set OpenApiLens.requestBody requestBody
        |> Lens.set OpenApiLens.responses responses

  let pathItem = GhcMonoid.mempty
        |> Lens.set OpenApiLens.post (Just operation)

  (path, pathItem)


-- | Create path item for GET /files/{fileRef}
makeFileDownloadPath :: EndpointSchema -> (Text, OpenApi.PathItem)
makeFileDownloadPath schema = do
  let path = "/files/{fileRef}"

  let fileRefParam = GhcMonoid.mempty
        |> Lens.set OpenApiLens.name "fileRef"
        |> Lens.set OpenApiLens.in_ OpenApi.ParamPath
        |> Lens.set OpenApiLens.required (Just True)
        |> Lens.set OpenApiLens.schema (Just (OpenApi.Inline (GhcMonoid.mempty
             |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiString)
             |> Lens.set OpenApiLens.description (Just "Unique file reference from upload response")
           )))

  let responses = OpenApi.Responses
        { OpenApi._responsesDefault = Nothing
        , OpenApi._responsesResponses = InsOrdHashMap.fromList
            [ (200, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "File content"
                |> Lens.set OpenApiLens.content (InsOrdHashMap.fromList
                     [ ("application/octet-stream", GhcMonoid.mempty
                         |> Lens.set OpenApiLens.schema (Just (OpenApi.Inline (GhcMonoid.mempty
                              |> Lens.set OpenApiLens.type_ (Just OpenApi.OpenApiString)
                              |> Lens.set OpenApiLens.format (Just "binary")
                            )))
                       )
                     ])
              ))
            , (401, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Unauthorized"
              ))
            , (403, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Forbidden - not file owner"
              ))
            , (404, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "File not found or expired"
              ))
            , (500, OpenApi.Inline (GhcMonoid.mempty
                |> Lens.set OpenApiLens.description "Internal Server Error"
              ))
            ]
        }

  let operation = GhcMonoid.mempty
        |> Lens.set OpenApiLens.summary (Just "Download file")
        |> Lens.set OpenApiLens.description (Just schema.description)
        |> Lens.set OpenApiLens.tags ["Files"]
        |> Lens.set OpenApiLens.parameters [OpenApi.Inline fileRefParam]
        |> Lens.set OpenApiLens.responses responses

  let pathItem = GhcMonoid.mempty
        |> Lens.set OpenApiLens.get (Just operation)

  (path, pathItem)
