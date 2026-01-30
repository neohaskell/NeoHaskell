{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Application (
  -- * Application Type
  Application (..),

  -- * ServiceRunner Type
  ServiceRunner (..),

  -- * Auth Setup Type
  WebAuthSetup (..),

  -- * OAuth2 Setup Type
  OAuth2Setup (..),

  -- * Construction
  new,

  -- * Configuration
  withEventStore,
  withQueryObjectStore,
  withQuery,
  withQueryEndpoint,
  withServiceRunner,
  withService,
  withTransport,
  withOutbound,
  withOutboundLifecycle,
  withInbound,
  withAuth,
  withAuthOverrides,
  withSecretStore,
  withOAuth2StateKey,
  withOAuth2Provider,
  withFileUpload,

  -- * File Upload Setup Type
  FileUploadSetup (..),

  -- * Inspection
  isEmpty,
  hasQueryRegistry,
  hasServiceRunners,
  serviceRunnerCount,
  hasTransports,
  transportCount,
  hasQueryEndpoints,
  queryEndpointCount,
  hasQueryDefinitions,
  queryDefinitionCount,
  hasEventStore,

  -- * Running
  run,
  runWith,
  runWithAsync,
) where

import Array (Array)
import Array qualified
import AsyncTask qualified
import Auth.Config (AuthOverrides)
import Auth.Config qualified
import Auth.Discovery qualified as Discovery
import Auth.Jwks qualified as Jwks
import Auth.OAuth2.Client qualified as OAuth2Client
import Auth.OAuth2.Provider (OAuth2ProviderConfig (..), ValidatedOAuth2ProviderConfig (..))
import Auth.OAuth2.Types (OAuth2Error (..), Provider (..))
import Auth.OAuth2.RateLimiter qualified as RateLimiter
import Auth.OAuth2.Routes qualified as OAuth2Routes
import Auth.OAuth2.StateToken qualified as StateToken
import Auth.OAuth2.TransactionStore.InMemory qualified as InMemoryTransactionStore
import Auth.SecretStore (SecretStore)
import Auth.SecretStore.InMemory qualified as InMemorySecretStore

import Basics
import Console qualified
import Environment qualified
import Control.Concurrent.Async qualified as GhcAsync
import Data.Either qualified as GhcEither
import Default (Default (..))
import GHC.TypeLits qualified as GHC
import IO qualified
import Integration qualified
import Integration.Lifecycle qualified as Lifecycle
import Json qualified
import Map (Map)
import Map qualified
import Record qualified
import LinkedList (LinkedList)
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Command.Core (NameOf)
import Service.Entity.Core (Entity (..), EventOf)
import Service.Event ()
import Service.EventStore (EventStore (..), EventStoreConfig (..))
import Service.Query.Core (EntitiesOf, Query)
import Service.Query.Definition (QueryDefinition (..), WireEntities)
import Service.Query.Definition qualified as Definition
import Service.QueryObjectStore.Core (QueryObjectStoreConfig (..))
import Service.QueryObjectStore.InMemory qualified as InMemory
import Service.Query.Registry (QueryRegistry)
import Service.Query.Registry qualified as Registry
import Service.Query.Subscriber qualified as Subscriber
import Service.ServiceDefinition.Core (ServiceRunner (..), TransportValue (..))
import Service.ServiceDefinition.Core qualified as ServiceDefinition
import Service.Application.Integrations qualified as Integrations
import Service.Application.Transports qualified as Transports
import Service.Integration.Dispatcher qualified as Dispatcher
import Service.Integration.Types (OutboundRunner, OutboundLifecycleRunner)
import Service.Transport (Transport (..), QueryEndpointHandler)
import Service.Transport.Web qualified as Web
import Path qualified
import Service.EventStore.Postgres.Internal (PostgresEventStore (..))
import Service.FileUpload.BlobStore.Local qualified as LocalBlobStore
import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..))
import Service.FileUpload.Core (FileUploadConfig (..), FileStateStoreBackend (..), InternalFileUploadConfig (..))
import Service.FileUpload.FileStateStore.Postgres qualified as PostgresFileStore
import Service.FileUpload.Web (FileUploadSetup (..))
import Service.FileUpload.Web qualified as FileUpload
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)
import TypeName qualified


-- | Configuration for WebTransport authentication.
-- This is stored in the Application and converted to WebTransport.AuthEnabled at runtime.
--
-- When auth is enabled, all endpoints require a valid JWT by default.
-- Permission checks should be done in the command's decide method, not at the transport layer.
--
-- Example:
--
-- @
-- app = Application.new
--   |> Application.withTransport WebTransport.server
--   |> Application.withAuth "https://auth.example.com"
-- @
data WebAuthSetup = WebAuthSetup
  { authServerUrl :: Text,
    -- ^ OAuth provider URL (e.g., "https://auth.example.com")
    authOverrides :: AuthOverrides
    -- ^ Optional overrides for auth configuration
  }


-- | Configuration for OAuth2 provider integration.
--
-- This is stored in the Application and converted to WebTransport.OAuth2Config at runtime.
-- OAuth2 routes require JWT authentication to be enabled (via withAuth).
--
-- SECURITY: The HMAC key is loaded from an environment variable at runtime.
-- This ensures the key is persistent across application restarts.
--
-- Example:
--
-- @
-- let ouraProvider = OAuth2ProviderConfig
--       { provider = Provider { name = "oura", ... }
--       , clientId = ClientId "your-client-id"
--       , ...
--       }
--
-- app = Application.new
--   |> Application.withTransport WebTransport.server
--   |> Application.withAuth "https://auth.example.com"
--   |> Application.withOAuth2StateKey "OAUTH2_STATE_KEY"
--   |> Application.withOAuth2Provider ouraProvider
-- @
data OAuth2Setup = OAuth2Setup
  { -- | Environment variable name containing the HMAC secret (min 32 bytes)
    hmacKeyEnvVar :: Text
  , -- | Configured OAuth2 providers
    providers :: Array OAuth2ProviderConfig
  }


-- | Application combines multiple Services and Queries with shared infrastructure.
--
-- The Application type provides a builder pattern for configuring:
--
-- * EventStore configuration (how to connect to the event store)
-- * QueryDefinitions (declarative query registrations, wired at runtime)
-- * QueryRegistry (maps entity names to query updaters)
-- * ServiceRunners (functions that run services with a shared EventStore)
-- * Transports (servers that expose commands to external clients)
-- * QueryEndpoints (HTTP endpoints for serving query data)
--
-- Example usage:
--
-- @
-- app = Application.new
--   |> Application.withEventStore postgresConfig
--   |> Application.withTransport WebTransport.server
--   |> Application.withService myCartService
--   |> Application.withQuery \@CartSummary
--
-- Application.run app
-- @
-- | A type-erased wrapper for QueryObjectStoreConfig.
--
-- This allows storing the config in Application without knowing the concrete type.
data QueryObjectStoreConfigValue = forall config. (QueryObjectStoreConfig config) => QueryObjectStoreConfigValue config


data Application = Application
  { eventStoreCreator :: Maybe (Task Text (EventStore Json.Value)),
    queryObjectStoreConfig :: Maybe QueryObjectStoreConfigValue,
    queryDefinitions :: Array QueryDefinition,
    queryRegistry :: QueryRegistry,
    serviceRunners :: Array ServiceRunner,
    transports :: Map Text TransportValue,
    queryEndpoints :: Map Text QueryEndpointHandler,
    outboundRunners :: Array OutboundRunner,
    outboundLifecycleRunners :: Array OutboundLifecycleRunner,
    inboundIntegrations :: Array Integration.Inbound,
    webAuthSetup :: Maybe WebAuthSetup,
    oauth2Setup :: Maybe OAuth2Setup,
    fileUploadConfig :: Maybe FileUploadConfig,
    secretStore :: Maybe SecretStore
  }


-- | Create a new empty Application.
--
-- By default, queries use in-memory storage. Use 'withQueryObjectStore' to
-- configure a different storage backend.
new :: Application
new =
  Application
    { eventStoreCreator = Nothing,
      queryObjectStoreConfig = Nothing,
      queryDefinitions = Array.empty,
      queryRegistry = Registry.empty,
      serviceRunners = Array.empty,
      transports = Map.empty,
      queryEndpoints = Map.empty,
      outboundRunners = Array.empty,
      outboundLifecycleRunners = Array.empty,
      inboundIntegrations = Array.empty,
      webAuthSetup = Nothing,
      oauth2Setup = Nothing,
      fileUploadConfig = Nothing,
      secretStore = Nothing
    }


-- | Configure the EventStore for the Application.
--
-- The EventStore is created when 'run' is called.
--
-- Example:
--
-- @
-- let postgresConfig = PostgresEventStore
--       { host = "localhost", port = 5432, ... }
--
-- app = Application.new
--   |> Application.withEventStore postgresConfig
-- @
withEventStore ::
  (EventStoreConfig config) =>
  config ->
  Application ->
  Application
withEventStore config app =
  app {eventStoreCreator = Just (createEventStore config)}


-- | Check if an EventStore has been configured.
hasEventStore :: Application -> Bool
hasEventStore app = case app.eventStoreCreator of
  Nothing -> False
  Just _ -> True


-- | Configure the QueryObjectStore for the Application.
--
-- The QueryObjectStore is used to store query read models. If not configured,
-- queries will use in-memory storage by default.
--
-- Example:
--
-- @
-- let postgresConfig = PostgresQueryObjectStore
--       { connectionString = "..." }
--
-- app = Application.new
--   |> Application.withQueryObjectStore postgresConfig
--   |> Application.withQuery \@CartSummary
-- @
withQueryObjectStore ::
  (QueryObjectStoreConfig config) =>
  config ->
  Application ->
  Application
withQueryObjectStore config app =
  app {queryObjectStoreConfig = Just (QueryObjectStoreConfigValue config)}


-- | Register a query type with automatic wiring.
--
-- This is the declarative way to add queries to an Application. At runtime,
-- when 'runWith' is called, the query infrastructure is automatically created:
--
-- * QueryObjectStore for storing query instances (using the configured backend)
-- * EntityFetcher for reconstructing entity state (for each entity in EntitiesOf query)
-- * QueryUpdater for handling entity events (for each entity)
-- * HTTP endpoint at @GET /queries/{query-name}@
--
-- The query name is derived from 'NameOf query' in kebab-case.
-- All entities listed in 'EntitiesOf query' are automatically wired.
--
-- The QueryObjectStore backend is determined by 'withQueryObjectStore'. If not
-- configured, in-memory storage is used by default.
--
-- Example:
--
-- @
-- app = Application.new
--   |> Application.withTransport WebTransport.server
--   |> Application.withService cartService
--   |> Application.withQuery \@CartSummary
-- @
withQuery ::
  forall query queryName entities.
  ( Query query,
    Json.ToJSON query,
    Json.FromJSON query,
    queryName ~ NameOf query,
    entities ~ EntitiesOf query,
    GHC.KnownSymbol queryName,
    WireEntities entities query
  ) =>
  Application ->
  Application
withQuery app = do
  let storeFactory = case app.queryObjectStoreConfig of
        Just (QueryObjectStoreConfigValue config) -> createQueryObjectStore config
        Nothing -> InMemory.new |> Task.mapError toText
  let definition = Definition.createDefinitionWithStore @query storeFactory
  app {queryDefinitions = app.queryDefinitions |> Array.push definition}


-- | Check if any query definitions have been registered.
hasQueryDefinitions :: Application -> Bool
hasQueryDefinitions app = not (Array.isEmpty app.queryDefinitions)


-- | Get the number of query definitions registered.
queryDefinitionCount :: Application -> Int
queryDefinitionCount app = Array.length app.queryDefinitions


-- | Check if the Application is empty (no configurations set).
isEmpty :: Application -> Bool
isEmpty app =
  Array.isEmpty app.queryDefinitions
    && Registry.isEmpty app.queryRegistry
    && Array.isEmpty app.serviceRunners
    && Map.length app.transports == 0


-- | Check if a QueryRegistry has been configured.
hasQueryRegistry :: Application -> Bool
hasQueryRegistry app = not (Registry.isEmpty app.queryRegistry)


-- | Add a Transport to the Application.
--
-- Transports define how commands are exposed to external clients (e.g., HTTP, gRPC).
-- Multiple transports can be configured, and they will all be started when the
-- Application runs.
--
-- Example:
--
-- @
-- app = Application.new
--   |> Application.withTransport WebTransport.server
-- @
withTransport ::
  forall transport transportName.
  ( Transport transport,
    transportName ~ NameOf transport,
    Record.KnownHash transportName,
    GHC.KnownSymbol transportName
  ) =>
  transport ->
  Application ->
  Application
withTransport transport app = do
  let transportNameText =
        GHC.symbolVal (Record.Proxy @transportName)
          |> Text.fromLinkedList
  app {transports = app.transports |> Map.set transportNameText (TransportValue transport)}


-- | Check if any transports have been configured.
hasTransports :: Application -> Bool
hasTransports app = Map.length app.transports > 0


-- | Get the number of transports configured.
transportCount :: Application -> Int
transportCount app = Map.length app.transports


-- | Add a query endpoint to the Application.
--
-- Query endpoints expose read models via HTTP GET /queries/{query-name}.
-- The handler is a Task that returns JSON text when called.
--
-- Example:
--
-- @
-- cartSummaryStore <- InMemory.new
-- app = Application.new
--   |> Application.withQueryEndpoint "cart-summary" (Endpoint.createQueryEndpoint cartSummaryStore)
-- @
withQueryEndpoint ::
  Text ->
  QueryEndpointHandler ->
  Application ->
  Application
withQueryEndpoint queryName handler app = do
  let updatedEndpoints = app.queryEndpoints |> Map.set queryName handler
  app {queryEndpoints = updatedEndpoints}


-- | Check if any query endpoints have been configured.
hasQueryEndpoints :: Application -> Bool
hasQueryEndpoints app = Map.length app.queryEndpoints > 0


-- | Get the number of query endpoints configured.
queryEndpointCount :: Application -> Int
queryEndpointCount app = Map.length app.queryEndpoints


-- | Add a ServiceRunner to the Application.
--
-- ServiceRunners are functions that run services with a shared EventStore.
-- Multiple runners can be added and they will all be executed when the
-- Application is run.
withServiceRunner ::
  ServiceRunner ->
  Application ->
  Application
withServiceRunner runner app =
  app {serviceRunners = app.serviceRunners |> Array.push runner}


-- | Add a Service to the Application.
--
-- This converts the Service to a ServiceRunner and adds it to the Application.
-- The Service will use the Application's shared EventStore instead of creating
-- its own.
--
-- Example:
--
-- @
-- app = Application.new
--   |> Application.withService myCartService
--   |> Application.withService myOrderService
-- @
withService ::
  forall cmds commandTransportNames event entity.
  ( event ~ ServiceDefinition.ServiceEventType cmds,
    entity ~ ServiceDefinition.ServiceEntityType cmds,
    Json.FromJSON event,
    Json.ToJSON event,
    Json.FromJSON entity,
    Json.ToJSON entity
  ) =>
  ServiceDefinition.Service cmds commandTransportNames ->
  Application ->
  Application
withService service app = do
  let runner = ServiceDefinition.toServiceRunner service
  app |> withServiceRunner runner


-- | Check if any ServiceRunners have been configured.
hasServiceRunners :: Application -> Bool
hasServiceRunners app = not (Array.isEmpty app.serviceRunners)


-- | Get the number of ServiceRunners configured.
serviceRunnerCount :: Application -> Int
serviceRunnerCount app = Array.length app.serviceRunners


-- | Run the application using the configured EventStore.
--
-- This is the primary way to run an Application. It:
-- 1. Creates the EventStore from the configured settings
-- 2. Wires all query definitions
-- 3. Rebuilds queries from historical events
-- 4. Starts live subscription for new events
-- 5. Runs all services
--
-- Example:
--
-- @
-- Application.new
--   |> Application.withEventStore postgresConfig
--   |> Application.withTransport WebTransport.server
--   |> Application.withService myService
--   |> Application.withQuery \@CartSummary
--   |> Application.run
-- @
run :: Application -> Task Text Unit
run app = case app.eventStoreCreator of
  Nothing -> Task.throw "No EventStore configured. Use withEventStore to configure one."
  Just creator -> do
    eventStore <- creator
    runWith eventStore app


-- | Run application with a provided EventStore.
--
-- Use this when you need to provide your own EventStore instance.
-- For most cases, prefer using 'run' with 'withEventStore'.
--
-- This function:
-- 1. Wires all query definitions (creates stores, updaters, endpoints)
-- 2. Creates a query subscriber with combined registries
-- 3. Rebuilds all queries from historical events
-- 4. Starts live subscription for new events
-- 5. Collects all command endpoints from all services
-- 6. Runs each transport once with combined endpoints
runWith :: EventStore Json.Value -> Application -> Task Text Unit
runWith eventStore app = do
  -- 1. Wire all query definitions and collect registries + endpoints
  wiredQueries <-
    app.queryDefinitions
      |> Task.mapArray (\def -> def.wireQuery eventStore)

  -- 2. Combine all registries from query definitions with the manual registry
  let combinedRegistry =
        wiredQueries
          |> Array.reduce (\(reg, _) acc -> mergeRegistries reg acc) app.queryRegistry

  -- 3. Combine all query endpoints from definitions with manual endpoints
  let combinedQueryEndpoints =
        wiredQueries
          |> Array.reduce (\(_, (name, handler)) acc -> acc |> Map.set name handler) app.queryEndpoints

  -- 4. Create query subscriber with combined registry
  subscriber <- Subscriber.new eventStore combinedRegistry

  -- 5. Rebuild all queries from historical events
  Subscriber.rebuildAll subscriber

  -- 6. Start live subscription
  Subscriber.start subscriber

  -- 7. Collect command endpoints from all services, grouped by transport
  endpointsByTransportMaps <-
    app.serviceRunners
      |> Task.mapArray (\runner -> runner.getEndpointsByTransport eventStore app.transports)

  -- 8. Merge all endpoints by transport name
  let mergeEndpointMaps serviceEndpoints acc =
        serviceEndpoints
          |> Map.entries
          |> Array.reduce
              ( \(transportName, cmdEndpoints) innerAcc ->
                  case innerAcc |> Map.get transportName of
                    Nothing -> innerAcc |> Map.set transportName cmdEndpoints
                    Just existing -> innerAcc |> Map.set transportName (Map.merge cmdEndpoints existing)
              )
              acc

  let combinedEndpointsByTransport =
        endpointsByTransportMaps
          |> Array.reduce mergeEndpointMaps Map.empty

  -- 9. Flatten all command endpoints for integration dispatch
  let combinedCommandEndpoints =
        combinedEndpointsByTransport
          |> Map.values
          |> Array.reduce (\cmdMap acc -> Map.merge cmdMap acc) Map.empty

  -- 10. Initialize auth if configured
  maybeAuthEnabled <- case app.webAuthSetup of
    Nothing -> Task.yield Nothing
    Just (WebAuthSetup serverUrl overrides) -> do
      Console.print [fmt|[Auth] Discovering auth config from #{serverUrl}...|]
      authConfig <-
        Discovery.discoverConfig serverUrl overrides
          |> Task.mapError (\err -> [fmt|Auth discovery failed: #{toText err}|])
      Console.print [fmt|[Auth] Starting JWKS manager...|]
      jwksManager <- Jwks.startManager authConfig
      Console.print [fmt|[Auth] Auth initialized successfully|]
      Task.yield
        ( Just
            Web.AuthEnabled
              { Web.jwksManager = jwksManager,
                Web.authConfig = authConfig
              }
        )

  -- 11. Initialize OAuth2 if configured
  (maybeOAuth2Config, actionContext) <- case app.oauth2Setup of
    Nothing -> do
      -- Apps without OAuth2 get context with empty provider registry
      emptyStore <- case app.secretStore of
        Just store -> Task.yield store
        Nothing -> InMemorySecretStore.new
      Task.yield
        ( Nothing
        , Integration.ActionContext
            { Integration.secretStore = emptyStore
            , Integration.providerRegistry = Map.empty
            }
        )
    Just (OAuth2Setup envVarName providerConfigs) -> do
      -- OAuth2 routes require JWT authentication to be enabled
      case app.webAuthSetup of
        Nothing -> Task.throw "OAuth2 requires authentication to be enabled. Call withAuth before configuring OAuth2 providers."
        Just _ -> pass
      Console.print [fmt|[OAuth2] Loading HMAC key from environment variable #{envVarName}...|]
      -- Load HMAC key from environment (declarative config, runtime loading)
      hmacKey <- loadHmacKeyFromEnv envVarName
      Console.print [fmt|[OAuth2] Initializing OAuth2 provider routes...|]
      -- Create in-memory transaction store for PKCE verifiers + userId
      transactionStore <- InMemoryTransactionStore.new
      -- Get or create SecretStore for token storage
      tokenStore <- case app.secretStore of
        Just store -> do
          Console.print [fmt|[OAuth2] Using custom SecretStore|]
          Task.yield store
        Nothing -> do
          Console.print [fmt|[OAuth2] ============================================================|]
          Console.print [fmt|[OAuth2] WARNING: Using in-memory SecretStore (development only)|]
          Console.print [fmt|[OAuth2] |]
          Console.print [fmt|[OAuth2] OAuth2 tokens will be LOST on restart and cannot be|]
          Console.print [fmt|[OAuth2] shared across multiple instances.|]
          Console.print [fmt|[OAuth2] |]
          Console.print [fmt|[OAuth2] For production, configure a persistent SecretStore:|]
          Console.print [fmt|[OAuth2]   Application.withSecretStore mySecretStore|]
          Console.print [fmt|[OAuth2] ============================================================|]
          InMemorySecretStore.new
      -- Create rate limiters for abuse prevention
      connectRateLimiter <- RateLimiter.new RateLimiter.defaultConnectConfig
      callbackRateLimiter <- RateLimiter.new RateLimiter.defaultCallbackConfig
      -- Build provider map from array, detecting duplicates
      providerMap <- buildProviderMap providerConfigs
      let actionContext =
            Integration.ActionContext
              { Integration.secretStore = tokenStore
              , Integration.providerRegistry = providerMap
              }
      -- Create route handlers with loaded HMAC key and rate limiters
      let routeDeps =
            OAuth2Routes.OAuth2RouteDeps
              { OAuth2Routes.hmacKey = hmacKey,
                OAuth2Routes.transactionStore = transactionStore,
                OAuth2Routes.secretStore = tokenStore,
                OAuth2Routes.providers = providerMap,
                OAuth2Routes.connectRateLimiter = connectRateLimiter,
                OAuth2Routes.callbackRateLimiter = callbackRateLimiter
              }
      let routes = OAuth2Routes.createRoutes routeDeps
      -- Create action dispatcher that decodes JSON and dispatches to command handlers
      -- The JSON is expected to contain { "commandType": "CommandName", "commandData": {...} }
      let dispatchAction :: Text -> Task Text Unit
          dispatchAction actionJson = do
            case Json.decodeText actionJson of
              Err err -> Task.throw [fmt|Failed to decode OAuth2 action JSON: #{err}|]
              Ok (payload :: Integration.CommandPayload) -> do
                Dispatcher.dispatchCommand combinedCommandEndpoints payload
      let providerCount = Array.length providerConfigs
      Console.print [fmt|[OAuth2] Initialized #{providerCount} provider(s)|]
      Task.yield (Just Web.OAuth2Config {Web.routes = routes, Web.dispatchAction = dispatchAction}, actionContext)

  -- 12. Start integration subscriber for outbound integrations with command dispatch
  maybeDispatcher <-
    Integrations.startIntegrationSubscriber
      eventStore
      app.outboundRunners
      app.outboundLifecycleRunners
      combinedCommandEndpoints
      actionContext

  -- 13. Start inbound integration workers (timers, webhooks, etc.)
  inboundWorkers <- Integrations.startInboundWorkers app.inboundIntegrations combinedCommandEndpoints

  -- 14. Initialize file uploads if configured
  maybeFileUploadEnabled <- case app.fileUploadConfig of
    Nothing -> Task.yield Nothing
    Just config -> do
      Console.print "[FileUpload] Initializing file upload..."
      setup <- initializeFileUpload config
      let routes = FileUpload.createRoutes setup
      Console.print "[FileUpload] File uploads enabled"
      Task.yield (Just Web.FileUploadEnabled {Web.fileUploadRoutes = routes})

  -- Define cleanup actions that must always run
  let cleanupJwksManager = case maybeAuthEnabled of
        Just (Web.AuthEnabled manager _) -> do
          Console.print "[Auth] Stopping JWKS manager..."
            |> Task.ignoreError
          Jwks.stopManager manager
            |> Task.ignoreError
        Nothing -> pass

  let cleanupDispatcher = case maybeDispatcher of
        Just dispatcher -> do
          Console.print "[Integration] Shutting down outbound dispatcher..."
            |> Task.ignoreError
          Dispatcher.shutdown dispatcher
        Nothing -> pass

  let cleanupAll = do
        cleanupJwksManager
        cleanupDispatcher

  -- 15. Run each transport once with combined endpoints from all services
  -- When transports complete (or fail), cancel inbound workers for clean shutdown
  -- Use Task.finally to ensure cleanup always runs even if runTransports fails
  result <-
    Transports.runTransports app.transports combinedEndpointsByTransport combinedQueryEndpoints maybeAuthEnabled maybeOAuth2Config maybeFileUploadEnabled
      |> Task.finally cleanupAll
      |> Task.asResult

  -- 16. Cancel all inbound workers on shutdown
  Console.print "[Integration] Shutting down inbound workers..."
    |> Task.ignoreError
  let shutdownTimeoutMs = 5000
  let awaitWorkerShutdown worker = do
        let awaitTask = AsyncTask.waitFor worker
        let timeoutTask =
              AsyncTask.sleep shutdownTimeoutMs
                |> Task.andThen (\_ -> Task.throw [fmt|timeout|])
        Task.fromIOResult do
          raceResult <- GhcAsync.race (Task.runResult awaitTask) (Task.runResult timeoutTask)
          case raceResult of
            GhcEither.Left result -> IO.yield result
            GhcEither.Right result -> IO.yield result
        |> Task.asResult
  inboundWorkers
    |> Task.forEach \worker -> do
        AsyncTask.cancel worker
          |> Task.ignoreError
        awaitResult <- awaitWorkerShutdown worker
        case awaitResult of
          Ok _ -> Task.yield unit
          Err err ->
            Console.print [fmt|[Integration] Inbound worker shutdown timed out or failed: #{err}|]
              |> Task.ignoreError

  -- Re-raise any transport error
  case result of
    Err err -> Task.throw err
    Ok _ -> Task.yield unit


-- | Merge two QueryRegistries by combining their updaters.
mergeRegistries :: QueryRegistry -> QueryRegistry -> QueryRegistry
mergeRegistries source target = do
  -- Get all entity names from source and merge their updaters into target
  -- This is a simplified merge - in practice we'd iterate over all entries
  -- For now, we rely on the fact that each query definition registers for different entities
  -- A proper implementation would use Registry.merge or similar
  source |> Registry.mergeInto target


-- | Run application with provided EventStore, non-blocking.
--
-- Launches the application in a background task and returns immediately.
-- Errors are logged to the console rather than silently discarded.
runWithAsync :: EventStore Json.Value -> Application -> Task Text Unit
runWithAsync eventStore app = do
  let taskWithErrorLogging :: Task Text Unit
      taskWithErrorLogging = do
        result <- runWith eventStore app |> Task.asResult
        case result of
          Err err -> Console.print [fmt|[Application] Error: Application failed: #{err}|]
          Ok _ -> Task.yield unit
  AsyncTask.run taskWithErrorLogging
    |> Task.mapError toText
    |> discard
  Task.yield unit


-- | Register an outbound integration for an entity type.
--
-- Outbound integrations react to entity events and can trigger external effects
-- or emit commands to other services (Process Manager pattern).
--
-- The integration function receives the current entity state (reconstructed via
-- event replay from the event store) and the new event, and returns actions to
-- execute.
--
-- Example:
--
-- @
-- app = Application.new
--   |> Application.withService cartService
--   |> Application.withOutbound \@CartEntity \@CartEvent cartIntegrations
-- @
withOutbound ::
  forall entity event.
  ( Json.FromJSON entity
  , Json.FromJSON event
  , Json.FromJSON (EventOf entity)
  , TypeName.Inspectable entity
  , Default entity
  , Entity entity
  , EventOf entity ~ event
  ) =>
  (entity -> event -> Integration.Outbound) ->
  Application ->
  Application
withOutbound integrationFn app = do
  let (runners, lifecycleRunners, inbounds) =
        Integrations.withOutbound @entity @event
          integrationFn
          (app.outboundRunners, app.outboundLifecycleRunners, app.inboundIntegrations)
  app
    { outboundRunners = runners
    , outboundLifecycleRunners = lifecycleRunners
    , inboundIntegrations = inbounds
    }


-- | Register an outbound integration with lifecycle management.
--
-- Use this for integrations that need expensive resources like:
--
-- * Database connection pools
-- * Embedded interpreters (Lua, Python, etc.)
-- * gRPC channels
-- * WebSocket subscriptions
--
-- Workers are created per-entity. When a worker is idle for too long,
-- it is reaped and cleanup is called. The next event for that entity
-- will spawn a fresh worker with initialize called again.
--
-- Example:
--
-- @
-- app = Application.new
--   |> Application.withService cartService
--   |> Application.withOutboundLifecycle \@CartEntity postgresNotifier
--
-- postgresNotifier :: Lifecycle.OutboundConfig PostgresPool
-- postgresNotifier = Lifecycle.OutboundConfig
--   { initialize = \\streamId -> Postgres.createPool connectionString
--   , processEvent = \\pool event -> Postgres.notify pool event >> Task.yield []
--   , cleanup = \\pool -> Postgres.closePool pool
--   }
-- @
withOutboundLifecycle ::
  forall (entity :: Type) state.
  ( TypeName.Inspectable entity
  ) =>
  Lifecycle.OutboundConfig state ->
  Application ->
  Application
withOutboundLifecycle config app = do
  let (runners, lifecycleRunners, inbounds) =
        Integrations.withOutboundLifecycle @entity
          config
          (app.outboundRunners, app.outboundLifecycleRunners, app.inboundIntegrations)
  app
    { outboundRunners = runners
    , outboundLifecycleRunners = lifecycleRunners
    , inboundIntegrations = inbounds
    }


-- | Register an inbound integration worker.
--
-- Inbound integrations listen to external sources (timers, webhooks, message queues)
-- and translate external events into domain commands.
--
-- When the Application runs, each inbound worker is started in its own background task.
-- Workers emit commands that are dispatched to the appropriate service handlers.
--
-- Example:
--
-- @
-- app = Application.new
--   |> Application.withService cartService
--   |> Application.withInbound periodicCartCreator
-- @
withInbound ::
  Integration.Inbound ->
  Application ->
  Application
withInbound inboundIntegration app = do
  let (runners, lifecycleRunners, inbounds) =
        Integrations.withInbound
          inboundIntegration
          (app.outboundRunners, app.outboundLifecycleRunners, app.inboundIntegrations)
  app
    { outboundRunners = runners
    , outboundLifecycleRunners = lifecycleRunners
    , inboundIntegrations = inbounds
    }


-- | Enable JWT authentication for WebTransport.
--
-- This is the simple version that uses default settings.
-- For advanced configuration, use 'withAuthOverrides'.
--
-- Example:
--
-- @
-- let commandAuth = Map.fromArray
--       [ ("AddItem", Authenticated)
--       , ("Checkout", RequireAllPermissions (Array.fromLinkedList ["checkout"]))
--       ]
--
-- app = Application.new
--   |> Application.withTransport WebTransport.server
--   |> Application.withService cartService
--   |> Application.withAuth "https://auth.example.com"
-- @
withAuth ::
  Text ->
  Application ->
  Application
withAuth authServerUrl app =
  app
    { webAuthSetup =
        Just
          WebAuthSetup
            { authServerUrl = authServerUrl,
              authOverrides = Auth.Config.defaultOverrides
            }
    }


-- | Enable JWT authentication with custom configuration.
--
-- This version allows overriding auth settings like audience, clock skew, etc.
-- Permission checks should be done in the command's decide method.
--
-- Example:
--
-- @
-- let overrides = Auth.Config.defaultOverrides
--       { audience = Just "my-api"
--       , permissionsClaim = Just "scope"
--       }
--
-- app = Application.new
--   |> Application.withTransport WebTransport.server
--   |> Application.withAuthOverrides "https://auth.example.com" overrides
-- @
withAuthOverrides ::
  Text ->
  AuthOverrides ->
  Application ->
  Application
withAuthOverrides authServerUrl overrides app =
  app
    { webAuthSetup =
        Just
          WebAuthSetup
            { authServerUrl = authServerUrl,
              authOverrides = overrides
            }
    }


-- | Configure a custom SecretStore for token storage.
--
-- By default, OAuth2 uses an in-memory SecretStore which is suitable for
-- development. For production, provide a persistent implementation
-- (e.g., Vault, AWS Secrets Manager, encrypted database).
--
-- @
-- -- Development (default - in-memory):
-- app = Application.new
--   |> Application.withOAuth2StateKey "OAUTH2_STATE_KEY"
--   |> Application.withOAuth2Provider ouraConfig
--
-- -- Production (custom implementation):
-- vaultStore <- MyVault.newSecretStore vaultConfig
-- app = Application.new
--   |> Application.withSecretStore vaultStore
--   |> Application.withOAuth2StateKey "OAUTH2_STATE_KEY"
--   |> Application.withOAuth2Provider ouraConfig
-- @
withSecretStore ::
  SecretStore ->
  Application ->
  Application
withSecretStore store app = app {secretStore = Just store}


-- | Initialize OAuth2 with an HMAC key loaded from environment.
--
-- The HMAC key is used to sign state tokens for CSRF protection.
-- It is loaded from the specified environment variable at runtime.
-- The key MUST be at least 32 bytes (256 bits) for security.
--
-- @
-- app = Application.new
--   |> Application.withOAuth2StateKey "OAUTH2_STATE_KEY"
--   |> Application.withOAuth2Provider ouraConfig
-- @
withOAuth2StateKey ::
  -- | Environment variable name containing the HMAC secret
  Text ->
  Application ->
  Application
withOAuth2StateKey envVarName app = do
  let existingProviders = case app.oauth2Setup of
        Just existing -> existing.providers
        Nothing -> Array.empty
  app {oauth2Setup = Just OAuth2Setup {hmacKeyEnvVar = envVarName, providers = existingProviders}}


-- | Add an OAuth2 provider to the Application.
--
-- This enables OAuth2 provider integration routes:
--
-- * @GET /connect/{provider}@ - Initiate OAuth2 flow
-- * @GET /callback/{provider}@ - OAuth2 callback
-- * @POST /disconnect/{provider}@ - Disconnect provider
--
-- Requires 'withOAuth2StateKey' to be called first.
-- Requires JWT authentication to be enabled (via withAuth).
--
-- Multiple providers can be added by calling this function multiple times:
--
-- @
-- app = Application.new
--   |> Application.withTransport WebTransport.server
--   |> Application.withAuth "https://auth.example.com"
--   |> Application.withOAuth2StateKey "OAUTH2_STATE_KEY"
--   |> Application.withOAuth2Provider ouraConfig
--   |> Application.withOAuth2Provider githubConfig
-- @
withOAuth2Provider ::
  OAuth2ProviderConfig ->
  Application ->
  Application
withOAuth2Provider providerConfig app = do
  case app.oauth2Setup of
    Nothing ->
      -- withOAuth2StateKey not called - this is a configuration error
      panic "withOAuth2Provider requires withOAuth2StateKey to be called first"
    Just existingSetup -> do
      let updatedProviders = existingSetup.providers |> Array.push providerConfig
      app {oauth2Setup = Just existingSetup {providers = updatedProviders}}


-- | Enable file uploads for WebTransport with declarative configuration.
--
-- This adds file upload and download routes:
--
-- * @POST /files/upload@ - Upload a file (multipart form)
-- * @GET /files/{fileRef}@ - Download a file
--
-- The configuration is declarative - actual IO (creating directories,
-- connecting to databases) is deferred to 'Application.run'.
--
-- Example:
--
-- @
-- let fileUploadConfig = FileUploadConfig
--       { blobStoreDir = "./uploads"
--       , stateStoreBackend = InMemoryStateStore  -- or PostgresStateStore {...}
--       , maxFileSizeBytes = 10485760  -- 10 MB
--       , pendingTtlSeconds = 21600    -- 6 hours
--       , cleanupIntervalSeconds = 900 -- 15 minutes
--       , allowedContentTypes = Nothing  -- All types allowed
--       , storeOriginalFilename = True
--       }
--
-- app = Application.new
--   |> Application.withTransport WebTransport.server
--   |> Application.withFileUpload fileUploadConfig
-- @
withFileUpload ::
  FileUploadConfig ->
  Application ->
  Application
withFileUpload config app =
  app {fileUploadConfig = Just config}


-- | Initialize file upload from declarative configuration.
--
-- This performs the IO required to set up file uploads:
--
-- * Creates the blob store directory if it doesn't exist
-- * Initializes the state store connection (PostgreSQL or in-memory)
--
-- This is called internally by 'Application.run' - users should not call this directly.
initializeFileUpload :: FileUploadConfig -> Task Text FileUploadSetup
initializeFileUpload fileConfig = do
  -- Extract config fields (needed for fmt quasiquoter compatibility)
  let blobDir = fileConfig.blobStoreDir

  -- 1. Create blob store from directory path
  blobStorePath <- case Path.fromText blobDir of
    Just path -> Task.yield path
    Nothing -> Task.throw [fmt|Invalid blob store directory path: #{blobDir}|]

  blobStore <- LocalBlobStore.createBlobStore LocalBlobStoreConfig
    { rootDir = blobStorePath
    }

  -- 2. Create state store based on backend configuration
  stateStore <- case fileConfig.stateStoreBackend of
    InMemoryStateStore -> do
      inMemoryStore <- FileUpload.newInMemoryFileStateStore
      Task.yield (FileUpload.inMemoryFileStateStore inMemoryStore)

    PostgresStateStore {pgHost, pgPort, pgDatabase, pgUser, pgPassword} -> do
      let postgresConfig = PostgresEventStore
            { host = pgHost
            , port = pgPort
            , databaseName = pgDatabase
            , user = pgUser
            , password = pgPassword
            }
      PostgresFileStore.new postgresConfig

  -- 3. Build internal config
  let internalConfig = InternalFileUploadConfig
        { pendingTtlSeconds = fileConfig.pendingTtlSeconds
        , cleanupIntervalSeconds = fileConfig.cleanupIntervalSeconds
        , maxFileSizeBytes = fileConfig.maxFileSizeBytes
        , allowedContentTypes = fileConfig.allowedContentTypes
        , storeOriginalFilename = fileConfig.storeOriginalFilename
        }

  -- 4. Return the setup
  Task.yield FileUploadSetup
    { config = internalConfig
    , blobStore = blobStore
    , stateStore = stateStore
    }


-- | Load HMAC key from environment variable.
--
-- This is called at runtime when the application starts.
loadHmacKeyFromEnv :: Text -> Task Text StateToken.HmacKey
loadHmacKeyFromEnv envVarName = do
  keyText <- Environment.getVariable envVarName
    |> Task.mapError (\_ -> [fmt|Environment variable #{envVarName} not found. OAuth2 requires a 32+ byte HMAC secret.|])
  case StateToken.mkHmacKey keyText of
    Err err -> Task.throw [fmt|Invalid HMAC key in #{envVarName}: #{err}|]
    Ok key -> Task.yield key


-- | Build provider map from array, validating endpoints and failing on duplicates.
--
-- SECURITY: Each provider's endpoints are validated once at startup for:
-- * HTTPS requirement (tokens contain secrets)
-- * No private/loopback IPs (SSRF protection)
-- * DNS resolution check (rebinding attack prevention)
--
-- This validation is done ONCE here, not on every token request (performance at scale).
--
-- If validation fails, the application startup fails with a detailed error message
-- indicating which provider failed and why.
buildProviderMap :: Array OAuth2ProviderConfig -> Task Text (Map Text ValidatedOAuth2ProviderConfig)
buildProviderMap configs = do
  let go ::
        Map Text ValidatedOAuth2ProviderConfig ->
        LinkedList OAuth2ProviderConfig ->
        Task Text (Map Text ValidatedOAuth2ProviderConfig)
      go acc remaining =
        case remaining of
          [] -> Task.yield acc
          (cfg : rest) -> do
            let providerName = cfg.provider.name
            case Map.get providerName acc of
              Just _ -> Task.throw [fmt|Duplicate OAuth2 provider configuration: '#{providerName}' is registered multiple times. Each provider name must be unique.|]
              Nothing -> do
                -- Log validation attempt for debugging
                Console.print [fmt|[OAuth2] Validating provider '#{providerName}' endpoints...|]
                  |> Task.ignoreError
                -- Validate provider endpoints (HTTPS, SSRF protection, DNS check)
                validationResult <- OAuth2Client.validateProvider cfg.provider |> Task.asResult
                case validationResult of
                  Err oauthError -> do
                    -- Log detailed error before failing
                    let errorDetail = formatOAuth2ValidationError providerName cfg.provider oauthError
                    Console.print [fmt|[OAuth2] ERROR: #{errorDetail}|]
                      |> Task.ignoreError
                    Task.throw errorDetail
                  Ok validatedProvider -> do
                    Console.print [fmt|[OAuth2] Provider '#{providerName}' validated successfully|]
                      |> Task.ignoreError
                    let validatedConfig =
                          ValidatedOAuth2ProviderConfig
                            { provider = cfg.provider
                            , validatedProvider = validatedProvider
                            , clientId = cfg.clientId
                            , clientSecret = cfg.clientSecret
                            , redirectUri = cfg.redirectUri
                            , scopes = cfg.scopes
                            , onSuccess = cfg.onSuccess
                            , onFailure = cfg.onFailure
                            , onDisconnect = cfg.onDisconnect
                            , successRedirectUrl = cfg.successRedirectUrl
                            , failureRedirectUrl = cfg.failureRedirectUrl
                            }
                    go (acc |> Map.set providerName validatedConfig) rest
  go Map.empty (configs |> Array.toLinkedList)


-- | Format OAuth2 validation error with helpful debugging information.
formatOAuth2ValidationError :: Text -> Provider -> OAuth2Error -> Text
formatOAuth2ValidationError providerName provider oauthError = do
  let authorizeUrl = provider.authorizeEndpoint
  let tokenUrl = provider.tokenEndpoint
  case oauthError of
    EndpointValidationFailed errMsg ->
      [fmt|Provider '#{providerName}' endpoint validation failed: #{errMsg}. Check that authorize endpoint (#{authorizeUrl}) and token endpoint (#{tokenUrl}) use HTTPS and resolve to public IPs.|]
    NetworkError errMsg ->
      [fmt|Provider '#{providerName}' network error during validation: #{errMsg}. Check network connectivity and DNS resolution.|]
    InvalidGrant errMsg ->
      [fmt|Provider '#{providerName}' unexpected InvalidGrant during validation: #{errMsg}|]
    InvalidClient errMsg ->
      [fmt|Provider '#{providerName}' unexpected InvalidClient during validation: #{errMsg}|]
    ScopeDenied errMsg ->
      [fmt|Provider '#{providerName}' unexpected ScopeDenied during validation: #{errMsg}|]
    TokenRequestFailed errMsg ->
      [fmt|Provider '#{providerName}' unexpected TokenRequestFailed during validation: #{errMsg}|]
    MalformedResponse errMsg ->
      [fmt|Provider '#{providerName}' unexpected MalformedResponse during validation: #{errMsg}|]
    InvalidState errMsg ->
      [fmt|Provider '#{providerName}' unexpected InvalidState during validation: #{errMsg}|]
    InvalidPkceVerifier errMsg ->
      [fmt|Provider '#{providerName}' unexpected InvalidPkceVerifier during validation: #{errMsg}|]
    InvalidRedirectUri errMsg ->
      [fmt|Provider '#{providerName}' unexpected InvalidRedirectUri during validation: #{errMsg}|]
