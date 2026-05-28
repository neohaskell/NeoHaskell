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

import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Log qualified
import Json qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Event (Event (..))
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
    subscriptionId :: ConcurrentVar (Maybe SubscriptionId)
  }


-- | Create a new QuerySubscriber.
new :: EventStore Json.Value -> QueryRegistry -> Task Text QuerySubscriber
new eventStore registry = do
  lastProcessedPosition <- ConcurrentVar.containing Nothing
  subscriptionId <- ConcurrentVar.containing Nothing
  Task.yield
    QuerySubscriber
      { eventStore,
        registry,
        lastProcessedPosition,
        subscriptionId
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
-- Stub — not implemented.
rebuildFrom
  :: QuerySubscriber
  -> Text
  -> StreamPosition
  -> RebuildOptions
  -> Task QueryRebuildError Unit
rebuildFrom _ _ _ _ = panic "not implemented: Service.Query.Subscriber.rebuildFrom"


-- | Spawn async rebuild for all registered queries, updating readiness states.
--
-- Stub — not implemented.
rebuildAllAsync
  :: QuerySubscriber
  -> RebuildOptions
  -> Task QueryRebuildError Unit
rebuildAllAsync _ _ = panic "not implemented: Service.Query.Subscriber.rebuildAllAsync"


-- | Fetch the aggregate readiness state of all registered queries.
--
-- Stub — not implemented.
readinessOf
  :: QuerySubscriber
  -> Task Text Readiness
readinessOf _ = panic "not implemented: Service.Query.Subscriber.readinessOf"


-- | Fetch the readiness state for a specific named query.
--
-- Stub — not implemented.
readinessOfQuery
  :: QuerySubscriber
  -> Text
  -> Task Text (Maybe Readiness)
readinessOfQuery _ _ = panic "not implemented: Service.Query.Subscriber.readinessOfQuery"


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
