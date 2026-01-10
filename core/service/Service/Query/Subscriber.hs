module Service.Query.Subscriber (
  QuerySubscriber (..),
  new,
  start,
  stop,
  rebuildAll,
) where

import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Console qualified
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
  Console.print "Starting query rebuild from event store..."
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
  case maybeLastPos of
    Just pos ->
      Console.print [fmt|Query rebuild complete. Last position: #{pos}|]
        |> Task.ignoreError
    Nothing ->
      Console.print "Query rebuild complete. No events found."
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

  Console.print [fmt|Starting query subscriber from position #{startPosition}|]
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


-- | Process a single raw event through all relevant query updaters.
processEvent :: QuerySubscriber -> Event Json.Value -> Task Text Unit
processEvent subscriber rawEvent = do
  let entityName = rawEvent.entityName
  let updaters = Registry.getUpdatersForEntity entityName subscriber.registry

  -- Run all updaters for this entity type
  updaters
    |> Task.forEach \updater -> do
      let updaterName = updater.queryName
      result <- updater.updateQuery rawEvent |> Task.asResult
      case result of
        Ok _ -> pass
        Err errText ->
          Console.print [fmt|Warning: Query updater #{updaterName} failed: #{errText}|]
            |> Task.ignoreError
