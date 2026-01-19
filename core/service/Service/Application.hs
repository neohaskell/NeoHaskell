{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Application (
  -- * Application Type
  Application (..),

  -- * ServiceRunner Type
  ServiceRunner (..),

  -- * Auth Setup Type
  WebAuthSetup (..),

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

import Basics
import Console qualified
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
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Command.Core (NameOf)
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
    webAuthSetup :: Maybe WebAuthSetup
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
      webAuthSetup = Nothing
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
  Application
    { eventStoreCreator = app.eventStoreCreator
    , queryObjectStoreConfig = app.queryObjectStoreConfig
    , queryRegistry = app.queryRegistry
    , serviceRunners = app.serviceRunners
    , transports = app.transports
    , queryEndpoints = updatedEndpoints
    , queryDefinitions = app.queryDefinitions
    , outboundRunners = app.outboundRunners
    , outboundLifecycleRunners = app.outboundLifecycleRunners
    , inboundIntegrations = app.inboundIntegrations
    , webAuthSetup = app.webAuthSetup
    }


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

  -- 10. Start integration subscriber for outbound integrations with command dispatch
  maybeDispatcher <- Integrations.startIntegrationSubscriber eventStore app.outboundRunners app.outboundLifecycleRunners combinedCommandEndpoints

  -- 11. Start inbound integration workers (timers, webhooks, etc.)
  inboundWorkers <- Integrations.startInboundWorkers app.inboundIntegrations combinedCommandEndpoints

  -- 12. Initialize auth if configured
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

  -- 13. Run each transport once with combined endpoints from all services
  -- When transports complete (or fail), cancel inbound workers for clean shutdown
  -- Use Task.finally to ensure cleanup always runs even if runTransports fails
  result <-
    Transports.runTransports app.transports combinedEndpointsByTransport combinedQueryEndpoints maybeAuthEnabled
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
-- The integration function receives the current entity state and the new event,
-- and returns actions to execute. Currently, the entity is reconstructed with
-- default values - full event replay will be added in a future version.
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
  , TypeName.Inspectable entity
  , Default entity
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
