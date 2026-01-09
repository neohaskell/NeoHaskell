module Service.Application (
  -- * Application Type
  Application,

  -- * ServiceRunner Type
  ServiceRunner (..),

  -- * Construction
  new,

  -- * Configuration
  withQueryRegistry,
  withQueryEndpoint,
  withServiceRunner,
  withService,
  withTransport,

  -- * Inspection
  isEmpty,
  hasQueryRegistry,
  hasServiceRunners,
  serviceRunnerCount,
  hasTransports,
  transportCount,
  hasQueryEndpoints,
  queryEndpointCount,

  -- * Running
  runWith,
  runWithAsync,
) where

import Array (Array)
import Array qualified
import AsyncTask qualified
import Basics
import GHC.TypeLits qualified as GHC
import Json qualified
import Map (Map)
import Map qualified
import Record qualified
import Service.Command.Core (NameOf)
import Service.EventStore (EventStore)
import Service.Query.Registry (QueryRegistry)
import Service.Query.Registry qualified as Registry
import Service.Query.Subscriber qualified as Subscriber
import Service.ServiceDefinition.Core (ServiceRunner (..), TransportValue (..))
import Service.ServiceDefinition.Core qualified as ServiceDefinition
import Service.Transport (Transport, QueryEndpointHandler)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


-- | Application combines multiple Services and Queries with shared infrastructure.
--
-- The Application type provides a builder pattern for configuring:
--
-- * QueryRegistry (maps entity names to query updaters)
-- * ServiceRunners (functions that run services with a shared EventStore)
-- * Transports (servers that expose commands to external clients)
-- * QueryEndpoints (HTTP endpoints for serving query data)
--
-- Example usage:
--
-- @
-- app = Application.new
--   |> Application.withQueryRegistry myRegistry
--   |> Application.withTransport WebTransport.server
--   |> Application.withServiceRunner myServiceRunner
--   |> Application.withQueryEndpoint "cart-summary" cartSummaryEndpoint
-- @
data Application = Application
  { queryRegistry :: QueryRegistry,
    serviceRunners :: Array ServiceRunner,
    transports :: Map Text TransportValue,
    queryEndpoints :: Map Text QueryEndpointHandler
  }


-- | Create a new empty Application.
new :: Application
new =
  Application
    { queryRegistry = Registry.empty,
      serviceRunners = Array.empty,
      transports = Map.empty,
      queryEndpoints = Map.empty
    }


-- | Add a QueryRegistry to the Application.
--
-- The QueryRegistry maps entity names to query updaters, which are called
-- when events are processed to update read models.
withQueryRegistry ::
  QueryRegistry ->
  Application ->
  Application
withQueryRegistry registry app =
  app {queryRegistry = registry}


-- | Check if the Application is empty (no configurations set).
isEmpty :: Application -> Bool
isEmpty app =
  Registry.isEmpty app.queryRegistry
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
withQueryEndpoint queryName handler app =
  app {queryEndpoints = app.queryEndpoints |> Map.set queryName handler}


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


-- | Run application with a provided EventStore.
--
-- This function:
-- 1. Creates a query subscriber
-- 2. Rebuilds all queries from historical events
-- 3. Starts live subscription for new events
-- 4. Runs all service runners with the shared event store and transports
runWith :: EventStore Json.Value -> Application -> Task Text Unit
runWith eventStore app = do
  -- 1. Create query subscriber
  subscriber <- Subscriber.new eventStore app.queryRegistry

  -- 2. Rebuild all queries from historical events
  Subscriber.rebuildAll subscriber

  -- 3. Start live subscription
  Subscriber.start subscriber

  -- 4. Run all services with shared event store, transports, and query endpoints
  app.serviceRunners
    |> Task.forEach \runner ->
      runner.runWithEventStore eventStore app.transports app.queryEndpoints


-- | Run application with provided EventStore, non-blocking.
--
-- Launches the application in a background task and returns immediately.
runWithAsync :: EventStore Json.Value -> Application -> Task Text Unit
runWithAsync eventStore app = do
  AsyncTask.run (runWith eventStore app)
    |> Task.mapError toText
    |> discard
  Task.yield unit
