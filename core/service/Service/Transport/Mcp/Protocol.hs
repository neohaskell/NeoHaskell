module Service.Transport.Mcp.Protocol (
  -- * Server state
  ServerState (..),
  newServerState,
  -- * Request processing
  handleRequest,
) where

import Appendable ((<>))
import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Json qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Schema.JsonSchema qualified
import Service.Auth qualified as Auth
import Service.Transport (EndpointHandler, EndpointSchema (..), QueryEndpointHandler)
import Service.Transport.Mcp.JsonRpc (JsonRpcRequest (..), JsonRpcResponse (..))
import Service.Transport.Mcp.JsonRpc qualified as JsonRpc
import Service.Transport.Mcp.Response qualified as Response
import Service.Query.Pagination (QueryPageRequest (..))
import Task (Task)
import Task qualified
import Array qualified
import Data.Text qualified as GhcText
import Text (Text)
import Text qualified


-- | MCP protocol version.
mcpProtocolVersion :: Text
mcpProtocolVersion = "2025-03-26"


-- | Resource URI prefix.
resourceUriPrefix :: Text
resourceUriPrefix = "neohaskell://queries/"


-- | Mutable server state tracking initialization and cached responses.
data ServerState = ServerState
  { initReceived :: ConcurrentVar Bool
    -- ^ Whether 'initialize' request was received
  , initialized :: ConcurrentVar Bool
    -- ^ Whether 'notifications/initialized' was received (full handshake complete)
  , serverName :: Text
  , serverVersion :: Text
  , commandEndpoints :: Map Text EndpointHandler
  , queryEndpoints :: Map Text QueryEndpointHandler
  , cachedToolsList :: Json.Value
  , cachedResourcesList :: Json.Value
  }


-- | Create initial server state with pre-computed cached responses.
newServerState ::
  Text ->
  Text ->
  Map Text EndpointHandler ->
  Map Text QueryEndpointHandler ->
  Map Text EndpointSchema ->
  Map Text EndpointSchema ->
  Task Text ServerState
newServerState serverName serverVersion commandEndpoints queryEndpoints commandSchemas querySchemas = do
  initReceivedVar <- ConcurrentVar.containing False
  initializedVar <- ConcurrentVar.containing False
  let cachedTools = buildToolsList commandSchemas
  let cachedResources = buildResourcesList querySchemas
  Task.yield ServerState
    { initReceived = initReceivedVar
    , initialized = initializedVar
    , serverName = serverName
    , serverVersion = serverVersion
    , commandEndpoints = commandEndpoints
    , queryEndpoints = queryEndpoints
    , cachedToolsList = cachedTools
    , cachedResourcesList = cachedResources
    }


-- | Process a single JSON-RPC request and produce a response.
-- Returns Nothing for notifications (no response expected).
handleRequest :: ServerState -> JsonRpcRequest -> Task Text (Maybe JsonRpcResponse)
handleRequest state request = do
  let requestId = request.id
  -- Check initialization state
  isInitialized <- ConcurrentVar.peek state.initialized
  case request.method of
    "initialize" -> do
      _ <- ConcurrentVar.swap True state.initReceived
      let result = Json.object
            [ "protocolVersion" Json..= mcpProtocolVersion
            , "capabilities" Json..= Json.object
                [ "tools" Json..= Json.object []
                , "resources" Json..= Json.object []
                , "prompts" Json..= Json.object []
                , "logging" Json..= Json.object []
                ]
            , "serverInfo" Json..= Json.object
                [ "name" Json..= state.serverName
                , "version" Json..= state.serverVersion
                ]
            ]
      Task.yield (Just (JsonRpc.successResponse requestId result))

    "notifications/initialized" -> do
      wasInitReceived <- ConcurrentVar.peek state.initReceived
      if wasInitReceived
        then do
          _ <- ConcurrentVar.swap True state.initialized
          Task.yield Nothing
        else
          Task.yield Nothing

    _ -> do
      if isInitialized
        then handleMethod state request
        else Task.yield (Just (JsonRpc.errorResponse requestId (JsonRpc.invalidRequest "Server not initialized")))


-- | Handle a method after initialization is confirmed.
handleMethod :: ServerState -> JsonRpcRequest -> Task Text (Maybe JsonRpcResponse)
handleMethod state request = do
  let requestId = request.id
  case request.method of
    "ping" ->
      Task.yield (Just (JsonRpc.successResponse requestId (Json.object [])))

    "notifications/cancelled" ->
      Task.yield Nothing

    "tools/list" ->
      Task.yield (Just (JsonRpc.successResponse requestId state.cachedToolsList))

    "tools/call" ->
      handleToolsCall state request

    "resources/list" ->
      Task.yield (Just (JsonRpc.successResponse requestId state.cachedResourcesList))

    "resources/read" ->
      handleResourcesRead state request

    "prompts/list" ->
      Task.yield (Just (JsonRpc.successResponse requestId (Json.object ["prompts" Json..= Json.toJSON ([] :: [Json.Value])])))

    "prompts/get" ->
      Task.yield (Just (JsonRpc.errorResponse requestId (JsonRpc.invalidParams "Prompt not found")))

    _ -> do
      let methodName = request.method
      Task.yield (Just (JsonRpc.errorResponse requestId (JsonRpc.methodNotFound [fmt|Unknown method: #{methodName}|])))


-- | Handle tools/call requests.
handleToolsCall :: ServerState -> JsonRpcRequest -> Task Text (Maybe JsonRpcResponse)
handleToolsCall state request = do
  let requestId = request.id
  case request.params of
    Nothing ->
      Task.yield (Just (JsonRpc.errorResponse requestId (JsonRpc.invalidParams "Missing params")))
    Just paramsVal -> do
      case extractToolCallParams paramsVal of
        Nothing ->
          Task.yield (Just (JsonRpc.errorResponse requestId (JsonRpc.invalidParams "Missing name in params")))
        Just (toolName, arguments) -> do
          case Map.get toolName state.commandEndpoints of
            Nothing ->
              Task.yield (Just (JsonRpc.errorResponse requestId (JsonRpc.invalidParams [fmt|Unknown tool: #{toolName}|])))
            Just handler -> do
              let callTask = do
                    requestContext <- Auth.anonymousContext
                    let argBytes = Json.encodeText arguments |> Text.toBytes
                    resultVar <- ConcurrentVar.new
                    handler requestContext argBytes (\(commandResponse, _responseBytes) -> do
                      let callToolResult = Response.toCallToolResult commandResponse
                      ConcurrentVar.set callToolResult resultVar
                      )
                    callToolResultVal <- ConcurrentVar.get resultVar
                    Task.yield (Just (JsonRpc.successResponse requestId callToolResultVal))
              callTask
                |> Task.recover (\_ ->
                  Task.yield (Just (JsonRpc.errorResponse requestId JsonRpc.internalError))
                  )


-- | Extract tool name and arguments from tools/call params.
extractToolCallParams :: Json.Value -> Maybe (Text, Json.Value)
extractToolCallParams value =
  case value of
    Aeson.Object obj -> do
      case AesonKeyMap.lookup (AesonKey.fromText "name") obj of
        Just (Aeson.String name) -> do
          let args = case AesonKeyMap.lookup (AesonKey.fromText "arguments") obj of
                Just a -> a
                Nothing -> Json.object []
          Just (name, args)
        _ -> Nothing
    _ -> Nothing


-- | Handle resources/read requests.
handleResourcesRead :: ServerState -> JsonRpcRequest -> Task Text (Maybe JsonRpcResponse)
handleResourcesRead state request = do
  let requestId = request.id
  case request.params of
    Nothing ->
      Task.yield (Just (JsonRpc.errorResponse requestId (JsonRpc.invalidParams "Missing params")))
    Just paramsVal -> do
      case extractResourceUri paramsVal of
        Nothing ->
          Task.yield (Just (JsonRpc.errorResponse requestId (JsonRpc.invalidParams "Missing or invalid uri in params")))
        Just uri -> do
          case GhcText.stripPrefix resourceUriPrefix uri of
            Nothing ->
              Task.yield (Just (JsonRpc.errorResponse requestId (JsonRpc.invalidParams [fmt|Invalid resource URI: #{uri}|])))
            Just queryNameWithParams -> do
              -- Strip query parameters (e.g., "cart-summary?id=..." -> "cart-summary")
              let queryName = case GhcText.breakOn "?" queryNameWithParams of
                    (name, _) -> name
              case Map.get queryName state.queryEndpoints of
                Nothing ->
                  Task.yield (Just (JsonRpc.errorResponse requestId (JsonRpc.invalidParams [fmt|Unknown resource: #{uri}|])))
                Just queryHandler -> do
                  let queryTask = do
                        let defaultPageRequest = QueryPageRequest { limit = 100, offset = 0 }
                        queryResult <- queryHandler Nothing Nothing defaultPageRequest
                          |> Task.mapError (\_ -> "Query execution failed" :: Text)
                        let resourceContent = Response.toResourceContent uri queryResult
                        Task.yield (Just (JsonRpc.successResponse requestId resourceContent))
                  queryTask
                    |> Task.recover (\_ ->
                      Task.yield (Just (JsonRpc.errorResponse requestId JsonRpc.internalError))
                      )


-- | Extract URI from resources/read params.
extractResourceUri :: Json.Value -> Maybe Text
extractResourceUri value =
  case value of
    Aeson.Object obj ->
      case AesonKeyMap.lookup (AesonKey.fromText "uri") obj of
        Just (Aeson.String uri) -> Just uri
        _ -> Nothing
    _ -> Nothing


-- | Build the tools/list response from command schemas.
buildToolsList :: Map Text EndpointSchema -> Json.Value
buildToolsList commandSchemas = do
  let tools = commandSchemas
        |> Map.entries
        |> Array.map (\(name, schema) ->
          Json.object
            [ "name" Json..= name
            , "description" Json..= schema.description
            , "inputSchema" Json..= case schema.requestSchema of
                Just reqSchema -> Schema.JsonSchema.toJsonSchema reqSchema
                Nothing -> Json.object []
            ]
          )
        |> Array.toLinkedList
  Json.object ["tools" Json..= Json.toJSON tools]


-- | Build the resources/list response from query schemas.
buildResourcesList :: Map Text EndpointSchema -> Json.Value
buildResourcesList querySchemas = do
  let resources = querySchemas
        |> Map.entries
        |> Array.map (\(name, schema) ->
          Json.object
            [ "uri" Json..= (resourceUriPrefix <> name)
            , "name" Json..= (name |> Text.toKebabCase)
            , "description" Json..= schema.description
            ]
          )
        |> Array.toLinkedList
  Json.object ["resources" Json..= Json.toJSON resources]
