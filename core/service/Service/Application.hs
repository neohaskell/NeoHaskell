{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.Application (
  -- * Application Type
  Application (..),

  -- * ServiceRunner Type
  ServiceRunner (..),

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
  withInbound,

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
import Basics
import Console qualified
import Default (Default (..), defaultValue)
import GHC.TypeLits qualified as GHC
import Integration qualified
import Json qualified
import Map (Map)
import Map qualified
import Record qualified
import Maybe (Maybe (..), withDefault)
import Result (Result (..))
import Service.Command.Core (NameOf)
import Service.Event (Event (..))
import Service.EventStore (EventStore (..), EventStoreConfig (..))
import Service.EventStore.Core (Error)
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
import Service.Transport (Transport (..), QueryEndpointHandler, EndpointHandler, Endpoints (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)
import TypeName qualified


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


-- | A type-erased outbound integration runner.
--
-- This wraps an integration function with JSON decoding so it can process
-- raw events from the event store.
data OutboundRunner = OutboundRunner
  { entityTypeName :: Text
  , processEvent :: Event Json.Value -> Task Text (Array Integration.CommandPayload)
  }


data Application = Application
  { eventStoreCreator :: Maybe (Task Text (EventStore Json.Value)),
    queryObjectStoreConfig :: Maybe QueryObjectStoreConfigValue,
    queryDefinitions :: Array QueryDefinition,
    queryRegistry :: QueryRegistry,
    serviceRunners :: Array ServiceRunner,
    transports :: Map Text TransportValue,
    queryEndpoints :: Map Text QueryEndpointHandler,
    outboundRunners :: Array OutboundRunner,
    inboundIntegrations :: Array Integration.Inbound
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
      inboundIntegrations = Array.empty
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
    , inboundIntegrations = app.inboundIntegrations
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
  startIntegrationSubscriber eventStore app.outboundRunners combinedCommandEndpoints

  -- 11. Start inbound integration workers (timers, webhooks, etc.)
  startInboundWorkers app.inboundIntegrations combinedCommandEndpoints

  -- 12. Run each transport once with combined endpoints from all services
  runTransports app.transports combinedEndpointsByTransport combinedQueryEndpoints


-- | Start subscription for outbound integrations.
--
-- Subscribes to all events and processes them through registered OutboundRunners.
-- Commands emitted by integrations are dispatched to the appropriate service handlers.
startIntegrationSubscriber ::
  EventStore Json.Value ->
  Array OutboundRunner ->
  Map Text EndpointHandler ->
  Task Text Unit
startIntegrationSubscriber (EventStore {subscribeToAllEvents}) runners commandEndpoints = do
  let processIntegrationEvent :: Event Json.Value -> Task Text Unit
      processIntegrationEvent rawEvent = do
        runners
          |> Task.forEach \runner -> do
              result <- runner.processEvent rawEvent |> Task.asResult
              case result of
                Ok commands -> do
                  commands
                    |> Task.forEach \payload -> do
                        dispatchCommand commandEndpoints payload
                Err err ->
                  Console.print [fmt|[Integration] Error processing event: #{err}|]
                    |> Task.ignoreError
  if Array.isEmpty runners
    then Task.yield unit
    else do
      subscribeToAllEvents processIntegrationEvent
        |> Task.mapError (toText :: Error -> Text)
        |> Task.map (\_ -> unit)


-- | Dispatch a command payload to the appropriate service handler.
dispatchCommand ::
  Map Text EndpointHandler ->
  Integration.CommandPayload ->
  Task Text Unit
dispatchCommand commandEndpoints payload = do
  let cmdType = payload.commandType
  case Map.get cmdType commandEndpoints of
    Just handler -> do
      let cmdBytes = Json.encodeText payload.commandData |> Text.toBytes
      let responseCallback _ = Task.yield unit
      handler cmdBytes responseCallback
        |> Task.mapError (\err -> [fmt|Command dispatch failed for #{cmdType}: #{err}|])
    Nothing -> do
      Console.print [fmt|[Integration] No handler found for command: #{cmdType}|]
        |> Task.ignoreError


-- | Start all inbound integration workers.
--
-- Each worker runs in its own background task, emitting commands that are
-- dispatched to the appropriate service handlers. Workers run indefinitely.
startInboundWorkers ::
  Array Integration.Inbound ->
  Map Text EndpointHandler ->
  Task Text Unit
startInboundWorkers inbounds commandEndpoints = do
  if Array.isEmpty inbounds
    then Task.yield unit
    else do
      let workerCount = Array.length inbounds
      Console.print [fmt|[Integration] Starting #{workerCount} inbound worker(s)|]
      inbounds
        |> Task.forEach \inboundIntegration -> do
            -- Start each worker in a background task
            let workerWithErrorHandling :: Task Text Unit
                workerWithErrorHandling = do
                  let emitCommand payload = do
                        dispatchCommand commandEndpoints payload
                          |> Task.mapError (\err -> Integration.PermanentFailure err)
                  result <- Integration.runInbound inboundIntegration emitCommand
                    |> Task.mapError toText
                    |> Task.asResult
                  case result of
                    Err err -> Console.print [fmt|[Integration] Inbound worker error: #{err}|]
                    Ok _ -> Task.yield unit
            -- Run worker in background
            AsyncTask.run workerWithErrorHandling
              |> Task.mapError toText
              |> discard


-- | Run all transports with their combined endpoints.
--
-- Each transport is started exactly once with all command and query endpoints
-- from all services that use that transport.
runTransports ::
  Map Text TransportValue ->
  Map Text (Map Text EndpointHandler) ->
  Map Text QueryEndpointHandler ->
  Task Text Unit
runTransports transportsMap endpointsByTransport queryEndpoints = do
  transportsMap
    |> Map.entries
    |> Task.forEach \(transportName, transportVal) -> do
        let commandEndpointsForTransport =
              endpointsByTransport
                |> Map.get transportName
                |> Maybe.withDefault Map.empty

        let commandCount = Map.length commandEndpointsForTransport
        let queryCount = Map.length queryEndpoints
        Console.print [fmt|Starting transport: #{transportName} with #{commandCount} commands and #{queryCount} queries|]

        case transportVal of
          TransportValue transport -> do
            let endpoints =
                  Endpoints
                    { transport = transport
                    , commandEndpoints = commandEndpointsForTransport
                    , queryEndpoints = queryEndpoints
                    }
            let runnableTransport = assembleTransport endpoints
            runTransport transport runnableTransport


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
  let typeName = TypeName.reflect @entity
  let runner = OutboundRunner
        { entityTypeName = typeName
        , processEvent = \rawEvent -> do
            -- Decode the event from JSON
            case Json.decode @event rawEvent.event of
              Err _ -> Task.yield Array.empty  -- Skip events that don't match
              Ok decodedEvent -> do
                -- Use default entity as placeholder - full entity reconstruction
                -- via event replay will be implemented in a future version.
                -- Integrations should use entityId from the event when needed.
                let entity = defaultValue @entity
                -- Call the integration function
                let outbound = integrationFn entity decodedEvent
                let actions = Integration.getActions outbound
                -- Execute all actions and collect command payloads
                actions
                  |> Task.mapArray (\action -> do
                      result <- Integration.runAction action
                        |> Task.mapError toText
                      case result of
                        Just payload -> Task.yield [payload]
                        Nothing -> Task.yield []
                    )
                  |> Task.map Array.flatten
        }
  app {outboundRunners = app.outboundRunners |> Array.push runner}


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
withInbound inboundIntegration app =
  app {inboundIntegrations = app.inboundIntegrations |> Array.push inboundIntegration}
