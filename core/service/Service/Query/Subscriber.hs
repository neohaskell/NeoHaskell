module Service.Query.Subscriber (
  QuerySubscriber (..),
  Readiness (..),
  RebuildOptions (..),
  QueryRebuildError (..),
  new,
  start,
  stop,
  rebuildAll,
  rebuildFrom,
  rebuildAllAsync,
  readinessOf,
  readinessOfQuery,
  rebuildOptionsDefault,
) where

import Array (Array)
import Array qualified
import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Log qualified
import Json qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Maybe qualified
import Result (Result (..))
import Service.Event (Event (..))
import Service.Event.EntityName (EntityName)
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.StreamPosition (StreamPosition (..))
import Service.EventStore (EventStore (..))
import Service.EventStore.Core (Error, Limit (..), ReadAllMessage (..), SubscriptionId)
import Service.Query.Registry (QueryRegistry, QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Stream qualified
import Task (Task)
import Task qualified
import Text (Text)
import ToText (toText)


-- | The QuerySubscriber listens to events from the EventStore and
-- dispatches them to registered QueryUpdaters.
data QuerySubscriber = QuerySubscriber
  { eventStore :: EventStore Json.Value,
    registry :: QueryRegistry,
    lastProcessedPosition :: ConcurrentVar (Maybe StreamPosition),
    subscriptionId :: ConcurrentVar (Maybe SubscriptionId),
    -- | Per-query readiness state, keyed by queryName.
    queryReadiness :: ConcurrentVar (Map Text Readiness)
  }


-- | Create a new QuerySubscriber.
new :: EventStore Json.Value -> QueryRegistry -> Task Text QuerySubscriber
new eventStore registry = do
  lastProcessedPosition <- ConcurrentVar.containing Nothing
  subscriptionId <- ConcurrentVar.containing Nothing
  queryReadiness <- ConcurrentVar.containing Map.empty
  Task.yield
    QuerySubscriber
      { eventStore,
        registry,
        lastProcessedPosition,
        subscriptionId,
        queryReadiness
      }


-- | Rebuild all queries from the beginning of the event store.
-- Called on application startup before starting live subscription.
rebuildAll :: QuerySubscriber -> Task Text Unit
rebuildAll subscriber = do
  Log.withScope [("component", "QuerySubscriber")] do
    Log.info "Starting query rebuild from event store..."
      |> Task.ignoreError

  -- Read all events from the beginning (use large limit)
  messageStream <-
    subscriber.eventStore.readAllEventsForwardFrom (StreamPosition 0) (Limit 9223372036854775807)
      |> Task.mapError (toText :: Error -> Text)

  -- Process each message incrementally via Stream.consume
  messageStream
    |> Stream.consume
      ( \_ message -> do
          case message of
            AllEvent rawEvent -> do
              processEvent subscriber rawEvent
              -- Update last processed position
              case rawEvent.metadata.globalPosition of
                Just pos -> subscriber.lastProcessedPosition |> ConcurrentVar.modify (\_ -> Just pos)
                Nothing -> pass
            _ -> pass
          Task.yield unit
      )
      unit

  -- Log completion
  maybeLastPos <- ConcurrentVar.peek subscriber.lastProcessedPosition
  Log.withScope [("component", "QuerySubscriber")] do
    case maybeLastPos of
      Just pos ->
        Log.info [fmt|Query rebuild complete. Last position: #{pos}|]
          |> Task.ignoreError
      Nothing ->
        Log.info "Query rebuild complete. No events found."
          |> Task.ignoreError

  Task.yield unit


-- | Start live subscription for query updates.
-- Should be called after rebuildAll completes.
start :: QuerySubscriber -> Task Text Unit
start subscriber = do
  maybeStartPosition <- ConcurrentVar.peek subscriber.lastProcessedPosition

  let startPosition = case maybeStartPosition of
        Just (StreamPosition pos) -> StreamPosition (pos + 1)
        Nothing -> StreamPosition 0

  Log.withScope [("component", "QuerySubscriber")] do
    Log.info [fmt|Starting query subscriber from position #{startPosition}|]
      |> Task.ignoreError

  subId <-
    subscriber.eventStore.subscribeToAllEventsFromPosition
      startPosition
      (processEventHandler subscriber)
      |> Task.mapError (toText :: Error -> Text)

  subscriber.subscriptionId |> ConcurrentVar.modify (\_ -> Just subId)

  Task.yield unit


-- | Stop the live subscription.
-- This unsubscribes from the event store, allowing graceful shutdown.
stop :: QuerySubscriber -> Task Text Unit
stop subscriber = do
  maybeSubId <- ConcurrentVar.peek subscriber.subscriptionId
  case maybeSubId of
    Just subId -> do
      subscriber.eventStore.unsubscribe subId
        |> Task.mapError (toText :: Error -> Text)
      subscriber.subscriptionId |> ConcurrentVar.modify (\_ -> Nothing)
    Nothing -> Task.yield unit


-- | Handler wrapper for subscription callback.
-- Processes the event and updates lastProcessedPosition, mirroring rebuildAll behavior.
processEventHandler :: QuerySubscriber -> Event Json.Value -> Task Text Unit
processEventHandler subscriber rawEvent = do
  processEvent subscriber rawEvent
  -- Update last processed position (same logic as rebuildAll)
  case rawEvent.metadata.globalPosition of
    Just pos -> subscriber.lastProcessedPosition |> ConcurrentVar.modify (\_ -> Just pos)
    Nothing -> pass


-- | Readiness state of a query rebuild.
data Readiness
  = Rebuilding
  | Ready
  | Failed Text
  deriving (Eq, Show, Generic)


-- | Options controlling a per-query rebuild.
data RebuildOptions = RebuildOptions
  { chunkSize :: Int
    -- ^ Events per fetch (default: 1000).
  , timeout :: Int
    -- ^ Per-query rebuild timeout in seconds (default: 300).
  , logProgress :: Bool
    -- ^ Emit a log message after each chunk (default: True).
  , deleteStaleHashFirst :: Bool
    -- ^ Delete rows with mismatched query_hash before replaying (default: True).
  }
  deriving (Eq, Show)


-- | Errors produced during a query rebuild.
data QueryRebuildError
  = RebuildTimeout Text
    -- ^ Rebuild took longer than the configured timeout.
  | UpdaterException Text
    -- ^ QueryUpdater returned Err during replay.
  | HashMismatchReplay Text
    -- ^ Hash mismatch forced a replay, but the replay itself failed.
  | CheckpointFetchFailed Text
    -- ^ Could not read the resume position from the object store.
  | EventStoreFailed Text
    -- ^ EventStore.readFrom returned Err.
  deriving (Eq, Show, Generic)


-- | Default rebuild options.
rebuildOptionsDefault :: RebuildOptions
rebuildOptionsDefault = RebuildOptions
  { chunkSize = 1000
  , timeout = 300
  , logProgress = True
  , deleteStaleHashFirst = True
  }


-- | Resumable per-query rebuild from a given StreamPosition.
--
-- Reads events from the EventStore in chunks, applies them to registered
-- query updaters for the named query, and tracks progress.
rebuildFrom
  :: QuerySubscriber
  -> Text
  -> StreamPosition
  -> RebuildOptions
  -> Task QueryRebuildError Unit
rebuildFrom subscriber queryName startPosition options = do
  -- Mark query as rebuilding.
  subscriber.queryReadiness
    |> ConcurrentVar.modify (Map.set queryName Rebuilding)
  -- Read events from the event store in chunks.
  let chunkLimit = Limit (fromIntegral options.chunkSize)
  messageStream <-
    subscriber.eventStore.readAllEventsForwardFrom startPosition chunkLimit
      |> Task.mapError (\err -> EventStoreFailed (toText err))
  -- Process events, routing only to updaters for this queryName.
  let updaters = getUpdatersForQuery queryName subscriber.registry
  result <-
    processEventsForQuery updaters options messageStream
      |> Task.asResult
  case result of
    Err err -> do
      subscriber.queryReadiness
        |> ConcurrentVar.modify (Map.set queryName (Failed (toText (show err))))
      Task.throw err
    Ok _ -> do
      -- Mark query as ready.
      subscriber.queryReadiness
        |> ConcurrentVar.modify (Map.set queryName Ready)
      Task.yield unit


-- | Process events from a stream for a specific set of updaters.
processEventsForQuery
  :: Array QueryUpdater
  -> RebuildOptions
  -> Stream.Stream (ReadAllMessage Json.Value)
  -> Task QueryRebuildError Unit
processEventsForQuery updaters options messageStream = do
  messageStream
    |> Stream.consume
        (\_ message -> do
          case message of
            AllEvent rawEvent -> do
              applyEvent updaters rawEvent
                |> Task.mapError (\err -> UpdaterException err)
            _ -> pass
          Task.yield unit)
        unit


-- | Apply an event to a set of updaters.
applyEvent
  :: Array QueryUpdater
  -> Event Json.Value
  -> Task Text Unit
applyEvent updaters rawEvent =
  updaters
    |> Task.forEach \updater -> do
        result <- updater.updateQuery rawEvent |> Task.asResult
        case result of
          Ok _ -> pass
          Err err ->
            Log.warn [fmt|Query updater #{updater.queryName} failed: #{err}|]
              |> Task.ignoreError


-- | Get all QueryUpdaters across all entities that belong to the named query.
getUpdatersForQuery :: Text -> QueryRegistry -> Array QueryUpdater
getUpdatersForQuery queryName registry =
  let allNames = Registry.getEntityNames registry
  in allNames
      |> Array.flatMap (\entityName ->
          Registry.getUpdatersForEntity entityName registry
            |> Array.takeIf (\updater -> updater.queryName == queryName))


-- | Spawn async rebuild for all registered queries, updating readiness states.
--
-- Returns Ok Unit when all queries have completed (either Ready or Failed).
-- Per-query failures are recorded in readiness state, not propagated as errors.
rebuildAllAsync
  :: QuerySubscriber
  -> RebuildOptions
  -> Task QueryRebuildError Unit
rebuildAllAsync subscriber options = do
  let queryNames = getAllQueryNames subscriber.registry
  queryNames
    |> Task.forEach (\queryName -> do
        result <-
          rebuildFrom subscriber queryName (StreamPosition 0) options
            |> Task.asResult
        case result of
          Ok _ -> pass
          Err err ->
            Log.withScope [("component", "QuerySubscriber"), ("queryName", queryName)] do
              Log.warn [fmt|Query rebuild failed: #{toText (show err)}|]
                |> Task.ignoreError)
  Task.yield unit


-- | Fetch the aggregate readiness state of all registered queries.
--
-- Returns:
-- - Ready    if all registered queries are Ready (or none are registered)
-- - Rebuilding if any query is Rebuilding
-- - Failed   if any query has Failed (and none are Rebuilding)
readinessOf
  :: QuerySubscriber
  -> Task Text Readiness
readinessOf subscriber = do
  readinessMap <- ConcurrentVar.peek subscriber.queryReadiness
  let states = readinessMap |> Map.values
  Task.yield (aggregateReadiness states)


-- | Fetch the readiness state for a specific named query.
--
-- Returns Nothing if the query is not registered.
readinessOfQuery
  :: QuerySubscriber
  -> Text
  -> Task Text (Maybe Readiness)
readinessOfQuery subscriber queryName = do
  readinessMap <- ConcurrentVar.peek subscriber.queryReadiness
  Task.yield (readinessMap |> Map.get queryName)


-- | Aggregate per-query readiness values into a single Readiness.
aggregateReadiness :: Array Readiness -> Readiness
aggregateReadiness states =
  states
    |> Array.reduce
        (\state acc ->
          case acc of
            Failed reason -> Failed reason
            Rebuilding ->
              case state of
                Failed reason -> Failed reason
                _ -> Rebuilding
            Ready ->
              case state of
                Rebuilding -> Rebuilding
                Failed reason -> Failed reason
                Ready -> Ready)
        Ready


-- | Get all unique query names from the registry.
getAllQueryNames :: QueryRegistry -> Array Text
getAllQueryNames registry =
  let allNames = Registry.getEntityNames registry
  in allNames
      |> Array.flatMap (\entityName ->
          Registry.getUpdatersForEntity entityName registry
            |> Array.map (\updater -> updater.queryName))
      |> Array.unique


-- | Process a single raw event through all relevant query updaters.
processEvent :: QuerySubscriber -> Event Json.Value -> Task Text Unit
processEvent subscriber rawEvent = do
  let entityName = rawEvent.entityName
  let updaters = Registry.getUpdatersForEntity entityName subscriber.registry

  Log.withScope [("component", "QuerySubscriber")] do
    case rawEvent.metadata.globalPosition of
      Just pos ->
        Log.debug [fmt|Processing event at position #{toText pos}|] |> Task.ignoreError
      Nothing ->
        Log.debug "Processing event (no position)" |> Task.ignoreError

  -- Run all updaters for this entity type
  updaters
    |> Task.forEach \updater -> do
      let updaterName = updater.queryName
      result <- updater.updateQuery rawEvent |> Task.asResult
      case result of
        Ok _ -> pass
        Err errText ->
          Log.withScope [("component", "QuerySubscriber"), ("queryName", updaterName)] do
            Log.warn [fmt|Query updater failed: #{errText}|]
              |> Task.ignoreError
