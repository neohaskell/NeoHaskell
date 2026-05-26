{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
module Service.Query.Subscriber (
  QuerySubscriber (..),
  new,
  start,
  stop,
  rebuildAll,
  rebuildFrom,
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
import Result (Result (..))
import Service.Event (Event (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.StreamPosition (StreamPosition (..))
import Service.EventStore (EventStore (..))
import Service.EventStore.Core (Error, Limit (..), ReadAllMessage (..), SubscriptionId)
import Service.Query.Registry (QueryRegistry, QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Service.Query.Checkpoint (CheckpointStore (..))
import Stream qualified
import Task (Task)
import Task qualified
import Text (Text)
import ToText (toText)


-- | The QuerySubscriber listens to events from the EventStore and
-- dispatches them to registered QueryUpdaters.
data QuerySubscriber = QuerySubscriber
  { eventStore :: EventStore Json.Value
  , registry :: QueryRegistry
  , lastProcessedPosition :: ConcurrentVar (Maybe StreamPosition)
  , subscriptionId :: ConcurrentVar (Maybe SubscriptionId)
  , ready :: ConcurrentVar Bool
  }


-- | Create a new QuerySubscriber.
new :: EventStore Json.Value -> QueryRegistry -> Task Text QuerySubscriber
new eventStore registry = do
  lastProcessedPosition <- ConcurrentVar.containing Nothing
  subscriptionId <- ConcurrentVar.containing Nothing
  ready <- ConcurrentVar.containing False
  Task.yield
    QuerySubscriber
      { eventStore
      , registry
      , lastProcessedPosition
      , subscriptionId
      , ready
      }


-- | Internal helper: determine which queries need processing.
-- Compares checkpoint positions against the message stream position.
needsProcessing :: Map Text StreamPosition -> ReadAllMessage Json.Value -> Array Text
needsProcessing checkpoints message =
  case message of
    AllEvent rawEvent ->
      case rawEvent.metadata.globalPosition of
        Nothing -> Array.empty
        Just eventPosition ->
          checkpoints
            |> Map.entries
            |> Array.reduce
              ( \(queryName, checkpointPosition) matchingQueryNames ->
                  if checkpointPosition < eventPosition
                    then matchingQueryNames |> Array.push queryName
                    else matchingQueryNames
              )
              Array.empty
    _ -> Array.empty


-- | Rebuild queries from a specific stream position using checkpoint data.
--
-- Loads persisted checkpoints from the CheckpointStore, hydrates them,
-- and processes events in chunks. Checkpoints are persisted after each chunk.
-- The subscriber's 'ready' flag is guaranteed to be set on every exit path.
rebuildFrom :: QuerySubscriber -> CheckpointStore -> StreamPosition -> Task Text Unit
rebuildFrom subscriber checkpointStore fromPosition = do
  subscriber.ready |> ConcurrentVar.modify (\_ -> False)

  -- Hydrate persisted checkpoints before rebuilding
  persistedCheckpoints <- checkpointStore.getPositions

  let (StreamPosition rawFromPosition) = fromPosition
  let defaultCheckpoint = StreamPosition (rawFromPosition - 1)

  let cleanup = do
        subscriber.ready |> ConcurrentVar.modify (\_ -> True)
        Log.withScope [("component", "QuerySubscriber")] do
          Log.info "Query rebuild finished (ready flag set)."
            |> Task.ignoreError

  -- Wrap the chunk loop with a finalizer so ready is reset on every exit path
  Task.finally cleanup do
    rebuildChunkLoop subscriber checkpointStore defaultCheckpoint fromPosition persistedCheckpoints


rebuildChunkLoop ::
  QuerySubscriber ->
  CheckpointStore ->
  StreamPosition ->
  StreamPosition ->
  Map Text StreamPosition ->
  Task Text Unit
rebuildChunkLoop subscriber checkpointStore defaultCheckpoint readPosition checkpoints = do
  messageStream <-
    subscriber.eventStore.readAllEventsForwardFrom readPosition (Limit 1000)
      |> Task.mapError (toText :: Error -> Text)

  (eventsReplayed, maybeLastPosition, nextCheckpoints) <-
    messageStream
      |> Stream.consume
        ( \(replayedCount, lastGlobalPosition, checkpointMap) message -> do
            case message of
              AllEvent rawEvent -> do
                let updatersForEntity = Registry.getUpdatersForEntity rawEvent.entityName subscriber.registry
                let checkpointMapWithDefaults =
                      updatersForEntity
                        |> Array.reduce
                          ( \updater acc ->
                              if acc |> Map.contains updater.queryName
                                then acc
                                else acc |> Map.set updater.queryName defaultCheckpoint
                          )
                          checkpointMap
                let queryNamesNeedingProcessing = needsProcessing checkpointMapWithDefaults message
                successfulQueryNames <-
                  processUpdatersForEvent rawEvent updatersForEntity queryNamesNeedingProcessing
                let updatedCheckpointMap =
                      case rawEvent.metadata.globalPosition of
                        Just globalPosition ->
                          successfulQueryNames
                            |> Array.reduce
                              (\queryName acc -> acc |> Map.set queryName globalPosition)
                              checkpointMapWithDefaults
                        Nothing -> checkpointMapWithDefaults
                case rawEvent.metadata.globalPosition of
                  Just globalPosition -> do
                    subscriber.lastProcessedPosition
                      |> ConcurrentVar.modify (\_ -> Just globalPosition)
                    Task.yield (replayedCount + 1, Just globalPosition, updatedCheckpointMap)
                  Nothing ->
                    Task.yield (replayedCount + 1, lastGlobalPosition, updatedCheckpointMap)
              _ ->
                Task.yield (replayedCount, lastGlobalPosition, checkpointMap)
        )
        (0, Nothing, checkpoints)

  if eventsReplayed == 0
    then do
      Log.withScope [("component", "QuerySubscriber")] do
        Log.info "Query rebuild from checkpoint complete. No more events."
          |> Task.ignoreError
      Task.yield unit
    else do
      -- Persist updated checkpoints after each chunk
      nextCheckpoints
        |> Map.entries
        |> Task.forEach (\(queryName, pos) -> checkpointStore.setPosition queryName pos)

      logRebuildChunkProgress subscriber eventsReplayed maybeLastPosition
      let nextReadPosition =
            case maybeLastPosition of
              Just (StreamPosition globalPosition) -> StreamPosition (globalPosition + 1)
              Nothing ->
                case readPosition of
                  StreamPosition currentPosition ->
                    StreamPosition (currentPosition + fromIntegral eventsReplayed)
      rebuildChunkLoop subscriber checkpointStore defaultCheckpoint nextReadPosition nextCheckpoints


processUpdatersForEvent ::
  Event Json.Value ->
  Array QueryUpdater ->
  Array Text ->
  Task Text (Array Text)
processUpdatersForEvent rawEvent updatersForEntity queryNamesNeedingProcessing =
  processUpdatersForEventAtIndex rawEvent updatersForEntity queryNamesNeedingProcessing 0 Array.empty


processUpdatersForEventAtIndex ::
  Event Json.Value ->
  Array QueryUpdater ->
  Array Text ->
  Int ->
  Array Text ->
  Task Text (Array Text)
processUpdatersForEventAtIndex rawEvent updatersForEntity queryNamesNeedingProcessing updaterIndex successfulQueryNames =
  case updatersForEntity |> Array.get updaterIndex of
    Nothing ->
      Task.yield successfulQueryNames
    Just updater -> do
      let updaterName = updater.queryName
      if queryNamesNeedingProcessing |> Array.contains updaterName
        then do
          result <- updater.updateQuery rawEvent |> Task.asResult
          case result of
            Ok _ ->
              processUpdatersForEventAtIndex
                rawEvent
                updatersForEntity
                queryNamesNeedingProcessing
                (updaterIndex + 1)
                (successfulQueryNames |> Array.push updaterName)
            Err errText -> do
              Log.withScope [("component", "QuerySubscriber"), ("queryName", updaterName)] do
                Log.warn [fmt|Query updater failed for #{updaterName}: #{errText}|]
                  |> Task.ignoreError
              -- Failed updaters are NOT added to successfulQueryNames,
              -- so their checkpoint won't advance and they'll be retried next chunk.
              processUpdatersForEventAtIndex
                rawEvent
                updatersForEntity
                queryNamesNeedingProcessing
                (updaterIndex + 1)
                successfulQueryNames
        else
          processUpdatersForEventAtIndex
            rawEvent
            updatersForEntity
            queryNamesNeedingProcessing
            (updaterIndex + 1)
            successfulQueryNames


-- | Rebuild all queries from the beginning of the event store.
-- Called on application startup before starting live subscription.
--
-- This is the legacy no-checkpoint path. For checkpoint-aware rebuild,
-- use 'rebuildFrom' with a CheckpointStore.
rebuildAll :: QuerySubscriber -> Task Text Unit
rebuildAll subscriber = do
  subscriber.ready |> ConcurrentVar.modify (\_ -> False)

  let cleanup = do
        subscriber.ready |> ConcurrentVar.modify (\_ -> True)
        Log.withScope [("component", "QuerySubscriber")] do
          Log.info "Query rebuild finished (ready flag set)."
            |> Task.ignoreError

  Task.finally cleanup do
    Log.withScope [("component", "QuerySubscriber")] do
      Log.info "Starting query rebuild from event store..."
        |> Task.ignoreError

    -- Read all events from the beginning (use large limit)
    messageStream <-
      subscriber.eventStore.readAllEventsForwardFrom (StreamPosition 0) (Limit 9223372036854775807)
        |> Task.mapError (toText :: Error -> Text)

    -- Process each message incrementally via Stream.consume
    let handler = \_ message -> do
          case message of
            AllEvent rawEvent -> do
              processEvent subscriber rawEvent
              case rawEvent.metadata.globalPosition of
                Just pos -> subscriber.lastProcessedPosition |> ConcurrentVar.modify (\_ -> Just pos)
                Nothing -> pass
            _ -> pass
          Task.yield unit
    messageStream |> Stream.consume handler unit
    maybeLastPos <- ConcurrentVar.peek subscriber.lastProcessedPosition
    Log.withScope [("component", "QuerySubscriber")] do
      case maybeLastPos of
        Just pos ->
          Log.info [fmt|Query rebuild complete. Last position: #{pos}|]
            |> Task.ignoreError
        Nothing ->
          Log.info "Query rebuild complete. No events found."
            |> Task.ignoreError


logRebuildChunkProgress ::
  QuerySubscriber ->
  Int ->
  Maybe StreamPosition ->
  Task Text Unit
logRebuildChunkProgress subscriber eventsReplayed maybeLastPosition = do
  maybeHeadPosition <- readHeadPosition subscriber
  Log.withScope [("component", "QuerySubscriber")] do
    case (maybeLastPosition, maybeHeadPosition) of
      (Just (StreamPosition lastPosition), Just (StreamPosition headPosition)) -> do
        let lagFromHead =
              if headPosition > lastPosition
                then headPosition - lastPosition
                else 0
        Log.info
          [fmt|Query rebuild chunk complete. eventsReplayed=#{eventsReplayed}, lagFromHead=#{lagFromHead}|]
          |> Task.ignoreError
      _ ->
        Log.info
          [fmt|Query rebuild chunk complete. eventsReplayed=#{eventsReplayed}, lagFromHead=unknown|]
          |> Task.ignoreError


readHeadPosition :: QuerySubscriber -> Task Text (Maybe StreamPosition)
readHeadPosition subscriber = do
  headStream <-
    subscriber.eventStore.readAllEventsBackwardFrom (StreamPosition 9223372036854775807) (Limit 1)
      |> Task.mapError (toText :: Error -> Text)
  readHeadPositionFromStream headStream


readHeadPositionFromStream :: Stream.Stream (ReadAllMessage Json.Value) -> Task Text (Maybe StreamPosition)
readHeadPositionFromStream stream = do
  maybeMessage <- Stream.readNext stream
  case maybeMessage of
    Nothing -> Task.yield Nothing
    Just message ->
      case message of
        AllEvent rawEvent -> Task.yield rawEvent.metadata.globalPosition
        _ -> readHeadPositionFromStream stream


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
