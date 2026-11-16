-- | # Service.Application.Transports
--
-- Transport runtime for the Application.
--
-- This module handles running transports with their combined endpoints.
module Service.Application.Transports (
  -- * Runtime
  WebRuntimeConfig (..),
  EndpointRegistry (..),
  runTransports,
  guardQueryEndpoint,
  guardQueryByIdEndpoint,
) where

import Array qualified
import Basics
import Log qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Maybe qualified
import Result (Result (..))
import Service.AccessControl qualified as AccessControl
import Service.ServiceDefinition.Core (TransportValue (..))
import Service.Transport (Transport (..), QueryEndpointHandler, QueryByIdEndpointHandler, QueryByIdEndpoint (..), EndpointHandler, Endpoints (..), EndpointSchema (..))
import Service.Transport.Web (WebTransport (..), AuthEnabled, OAuth2Config, FileUploadEnabled (..), CorsConfig, HealthCheckConfig, IntegrationStatus)
import Service.Transport.Web qualified as Web
import Service.Transport.Web.Readiness (ReadinessConfig)
import Service.Application.Types (ApiInfo)
import Service.Query.Subscriber (QuerySubscriber)
import Service.Query.Subscriber qualified as Subscriber
import Task (Task)
import Task qualified
import Text (Text)
import Unsafe.Coerce qualified as GhcUnsafe


-- | Optional application settings applied when WebTransport starts.
data WebRuntimeConfig = WebRuntimeConfig
  { authEnabled :: Maybe AuthEnabled
  , oauth2Config :: Maybe OAuth2Config
  , fileUploadEnabled :: Maybe FileUploadEnabled
  , apiInfo :: Maybe ApiInfo
  , corsConfig :: Maybe CorsConfig
  , healthCheck :: Maybe HealthCheckConfig
  , integrationStatus :: Maybe IntegrationStatus
  , readinessConfig :: Maybe ReadinessConfig
  , bindHost :: Maybe Text
  , bindPort :: Maybe Int
  }


-- | Endpoint handlers and schemas collected for transport startup.
--
-- Command entries are grouped by transport name. Query entries are shared
-- across transports, while query-by-ID entries are used only by WebTransport.
data EndpointRegistry = EndpointRegistry
  { commandEndpointsByTransport :: Map Text (Map Text EndpointHandler)
  , commandSchemasByTransport :: Map Text (Map Text EndpointSchema)
  , queryEndpoints :: Map Text QueryEndpointHandler
  , queryByIdEndpoints :: Map Text QueryByIdEndpoint
  , querySchemas :: Map Text EndpointSchema
  }


-- | Run each transport once with its command endpoints and shared query endpoints.
--
-- WebTransport applies the optional web settings and guards query endpoints
-- with the supplied subscriber readiness. Generic transports omit query-by-ID
-- endpoints.
runTransports ::
  WebRuntimeConfig ->
  QuerySubscriber ->
  EndpointRegistry ->
  Map Text TransportValue ->
  Task Text Unit
runTransports webConfig subscriber endpointRegistry transportsMap = do
  transportsMap
    |> Map.entries
    |> Task.forEach \(transportName, transportVal) -> do
        let commandEndpointsForTransport =
              endpointRegistry.commandEndpointsByTransport
                |> Map.get transportName
                |> Maybe.withDefault Map.empty

        let commandSchemasForTransport =
              endpointRegistry.commandSchemasByTransport
                |> Map.get transportName
                |> Maybe.withDefault Map.empty

        let endpoints =
              Endpoints
                { transport = transportVal
                , commandEndpoints = commandEndpointsForTransport
                , queryEndpoints = endpointRegistry.queryEndpoints
                , queryByIdEndpoints = endpointRegistry.queryByIdEndpoints
                , commandSchemas = commandSchemasForTransport
                , querySchemas = endpointRegistry.querySchemas
                }

        let commandCount = Map.length commandEndpointsForTransport
        let sharedQueryCount = Map.length endpointRegistry.queryEndpoints
        let transportQueryCount = sharedQueryCount
        Log.withScope [("component", "Transport"), ("transportName", transportName)] do
          Log.info [fmt|Starting transport: #{transportName} with #{commandCount} commands and #{transportQueryCount} queries (shared query endpoints: #{sharedQueryCount} total)|]
            |> Task.ignoreError

        -- Handle WebTransport specially to configure auth, OAuth2, and file uploads
        case transportName of
          "WebTransport" -> runWebTransport webConfig subscriber endpoints
          _ -> runGenericTransport endpoints


-- | Run WebTransport with JWT authentication, OAuth2 provider routes, and file uploads.
--
-- SAFETY: We use unsafeCoerce to cast the existentially-typed transport to WebTransport.
-- This is safe because:
-- 1. We guard on transportName == "WebTransport" before calling this function
-- 2. Transport names are derived from type names via TypeName, ensuring consistency
-- 3. WebTransport is the only transport that registers with name "WebTransport"
-- If the type system allowed Typeable on the existential, we'd use cast instead.
runWebTransport ::
  WebRuntimeConfig ->
  QuerySubscriber ->
  Endpoints TransportValue ->
  Task Text Unit
runWebTransport webConfig subscriber endpoints = do
  case endpoints.transport of
    TransportValue transport -> do
      let guardedQueryEndpoints =
            endpoints.queryEndpoints
              |> Map.entries
              |> Array.map
                ( \(queryName, handler) ->
                    ( queryName,
                      guardQueryEndpoint
                        ( Subscriber.readinessOfQuery subscriber queryName
                            |> Task.mapError (\_ -> AccessControl.StorageError "Query readiness unavailable")
                        )
                        handler
                    )
                )
              |> Map.fromArray
      let guardedQueryByIdEndpoints =
            endpoints.queryByIdEndpoints
              |> Map.entries
              |> Array.map
                ( \(queryName, endpoint) ->
                    ( queryName,
                      endpoint
                        { byIdHandler =
                            guardQueryByIdEndpoint
                              ( Subscriber.readinessOfQuery subscriber queryName
                                  |> Task.mapError (\_ -> AccessControl.StorageError "Query readiness unavailable")
                              )
                              endpoint.byIdHandler
                        }
                    )
                )
              |> Map.fromArray
      -- Cast the existentially-typed transport to WebTransport
      let baseWebTransport :: WebTransport = GhcUnsafe.unsafeCoerce transport
      -- Configure auth, OAuth2, file uploads, and API info on the WebTransport itself
      -- ADR-0019: Coordinate maxBodySize with file upload limit when file uploads are enabled
      let coordinatedMaxBodySize :: Int
          coordinatedMaxBodySize = case webConfig.fileUploadEnabled of
            Nothing -> baseWebTransport.maxBodySize
            Just (FileUploadEnabled {maxRequestBodyBytes}) ->
              -- Use max to respect explicit user configuration while ensuring file uploads work
              max baseWebTransport.maxBodySize (fromIntegral maxRequestBodyBytes)
      let webTransportWithConfig = baseWebTransport
            { authEnabled = webConfig.authEnabled
            , oauth2Config = webConfig.oauth2Config
            , fileUploadEnabled = webConfig.fileUploadEnabled
            , apiInfo = webConfig.apiInfo
            , maxBodySize = coordinatedMaxBodySize
            , corsConfig = webConfig.corsConfig
            , healthCheck = webConfig.healthCheck
            , integrationStatus = webConfig.integrationStatus
            , readinessConfig = webConfig.readinessConfig
            , readinessProbe = Just (Subscriber.readinessOf subscriber)
            }
            |> Web.applyBindOverrides webConfig.bindHost webConfig.bindPort
      -- Reject an unusable host or port here, before any socket is opened:
      -- the error names the setting and reaches Application.run's caller.
      case Web.validateBindHost webTransportWithConfig.host of
        Just reason -> Task.throw [fmt|Application.withHost: #{reason}|]
        Nothing -> pass
      case Web.validateBindPort webTransportWithConfig.port of
        Just reason -> Task.throw [fmt|Application.withPort: #{reason}|]
        Nothing -> pass
      -- Build endpoints with the configured transport
      let webEndpoints :: Endpoints WebTransport =
            Endpoints
              { transport = webTransportWithConfig,
                commandEndpoints = endpoints.commandEndpoints,
                queryEndpoints = guardedQueryEndpoints,
                queryByIdEndpoints = guardedQueryByIdEndpoints,
                commandSchemas = endpoints.commandSchemas,
                querySchemas = endpoints.querySchemas
              }
      let runnableTransport = assembleTransport webEndpoints
      runTransport webTransportWithConfig runnableTransport


guardQueryEndpoint ::
  Task AccessControl.QueryEndpointError (Maybe Subscriber.Readiness) ->
  QueryEndpointHandler ->
  QueryEndpointHandler
guardQueryEndpoint queryReadiness handler userClaims maybeExpr pageRequest = do
  before <- queryReadiness |> Task.asResult
  response <- handler userClaims maybeExpr pageRequest
  after <- queryReadiness |> Task.asResult
  case (before, after) of
    (Ok Nothing, Ok Nothing) -> Task.yield response
    (Ok (Just Subscriber.Ready), Ok (Just Subscriber.Ready)) -> Task.yield response
    _ -> Task.throw (AccessControl.StorageError "Query projection unavailable")


guardQueryByIdEndpoint ::
  Task AccessControl.QueryEndpointError (Maybe Subscriber.Readiness) ->
  QueryByIdEndpointHandler ->
  QueryByIdEndpointHandler
guardQueryByIdEndpoint queryReadiness handler userClaims queryId = do
  before <- queryReadiness |> Task.asResult
  response <- handler userClaims queryId
  after <- queryReadiness |> Task.asResult
  case (before, after) of
    (Ok Nothing, Ok Nothing) -> Task.yield response
    (Ok (Just Subscriber.Ready), Ok (Just Subscriber.Ready)) -> Task.yield response
    _ -> Task.throw (AccessControl.StorageError "Query projection unavailable")


-- | Run a generic transport without auth.
runGenericTransport ::
  Endpoints TransportValue ->
  Task Text Unit
runGenericTransport endpoints = do
  case endpoints.transport of
    TransportValue transport -> do
      let typedEndpoints =
            Endpoints
              { transport = transport,
                commandEndpoints = endpoints.commandEndpoints,
                queryEndpoints = endpoints.queryEndpoints,
                queryByIdEndpoints = Map.empty,
                commandSchemas = endpoints.commandSchemas,
                querySchemas = endpoints.querySchemas
              }
      let runnableTransport = assembleTransport typedEndpoints
      runTransport transport runnableTransport
