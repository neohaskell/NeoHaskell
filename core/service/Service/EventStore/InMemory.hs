module Service.EventStore.InMemory (
  new,
) where

import Array qualified
import ConcurrentVar qualified
import Core
import DurableChannel qualified
import Lock qualified
import Map qualified
import Maybe qualified
import Service.Event
import Service.Event.EventMetadata qualified as EventMetadata
import Service.EventStore.Core
import Task qualified
import Uuid qualified


new :: Task Error (EventStore eventType)
new = do
  store <- newEmptyStreamStore
  let eventStore =
        EventStore
          { insert = insertWithNotification store,
            readStreamForwardFrom = readStreamForwardFromImpl store,
            readStreamBackwardFrom = readStreamBackwardFromImpl store,
            readAllStreamEvents = readAllStreamEventsImpl store,
            readAllEventsForwardFrom = readAllEventsForwardFromImpl store,
            readAllEventsBackwardFrom = readAllEventsBackwardFromImpl store,
            readAllEventsForwardFromFiltered = readAllEventsForwardFromFilteredImpl store,
            readAllEventsBackwardFromFiltered = readAllEventsBackwardFromFilteredImpl store,
            subscribeToAllEvents = subscribeToAllEventsImpl store,
            subscribeToAllEventsFromPosition = subscribeToAllEventsFromPositionImpl store,
            subscribeToAllEventsFromStart = subscribeToAllEventsFromStartImpl store,
            subscribeToEntityEvents = subscribeToEntityEventsImpl store,
            subscribeToStreamEvents = subscribeToStreamEventsImpl store,
            unsubscribe = unsubscribeImpl store,
            truncateStream = truncateStreamImpl store
          }
  Task.yield eventStore


-- PRIVATE

fromInsertionPayload :: StreamPosition -> InsertionPayload eventType -> Array (StoredEvent eventType)
fromInsertionPayload (StreamPosition globalPosition) payload =
  payload.insertions
    |> Array.indexed
    |> Array.map \(index, insertion) -> do
      let newMetadata = insertion.metadata {EventMetadata.globalPosition = Just (StreamPosition (globalPosition + fromIntegral index))}
      StoredEvent
        { entityName = payload.entityName,
          streamId = payload.streamId,
          metadata = newMetadata,
          event = insertion.event
        }


insertWithNotification ::
  StreamStore eventType ->
  InsertionPayload eventType ->
  Task Error InsertionSuccess
insertWithNotification store event = do
  finalEvent <- insertImpl store event
  -- Notify subscribers AFTER the event has been successfully stored and locks are released
  -- FIXME: Now finalEvent is not an event, but rather a position
  -- notifySubscribers store finalEvent
  Task.yield finalEvent


data StreamStore eventType = StreamStore
  { globalStream :: (DurableChannel (StoredEvent eventType)),
    streams :: ConcurrentVar (Map (EntityName, StreamId) (DurableChannel (StoredEvent eventType))),
    globalLock :: Lock,
    subscriptions :: ConcurrentVar (Map SubscriptionId (Subscription eventType))
  }


data SubscriptionType
  = AllEvents
  | EntityEvents EntityName
  | StreamEvents EntityName StreamId
  deriving (Eq, Show)


data Subscription eventType = Subscription
  { subscriptionType :: SubscriptionType,
    handler :: StoredEvent eventType -> Task Error Unit
  }


newEmptyStreamStore :: Task _ (StreamStore eventType)
newEmptyStreamStore = do
  globalLock <- Lock.new
  globalStream <- DurableChannel.new
  streams <- ConcurrentVar.containing Map.empty
  subscriptions <- ConcurrentVar.containing Map.empty
  Task.yield StreamStore {globalLock, streams, globalStream, subscriptions}


-- | Idempotent stream creation.
ensureStream :: EntityName -> StreamId -> StreamStore eventType -> Task _ (DurableChannel (StoredEvent eventType))
ensureStream entityName streamId store = do
  let modifier streamMap = do
        case streamMap |> Map.get (entityName, streamId) of
          Just channel -> do
            Task.yield (streamMap, channel)
          Nothing -> do
            channel <- DurableChannel.new
            Task.yield (streamMap |> Map.set (entityName, streamId) channel, channel)
  store.streams
    |> ConcurrentVar.modifyReturning modifier
    |> Lock.with store.globalLock


insertImpl ::
  StreamStore eventType ->
  InsertionPayload eventType ->
  Task Error InsertionSuccess
insertImpl store payload = do
  let entityName = payload.entityName
  let streamId = payload.streamId
  let expectedPosition =
        payload.insertions
          |> Array.map (\i -> i.metadata.localPosition)
          |> Array.maximum
          |> Maybe.flatten
          |> Maybe.withDefault (StreamPosition 0)
  channel <- store |> ensureStream entityName streamId

  let appendCondition :: Array (StoredEvent eventType) -> Bool
      appendCondition events = do
        let currentLength = Array.length events
        let (StreamPosition pos) = expectedPosition
        pos == fromIntegral currentLength

  -- First, get the global index from the global stream
  globalIndex <-
    store.globalStream
      |> DurableChannel.writeWithIndex (\index -> payload |> fromInsertionPayload (fromIntegral index |> StreamPosition))

  -- Now create the event with the correct global position
  let globalPosition = StreamPosition (fromIntegral globalIndex)
  let finalEvents = payload |> fromInsertionPayload globalPosition

  -- Write to the individual stream with the correctly positioned event
  hasWritten <-
    channel |> DurableChannel.checkAndWrite appendCondition finalEvents

  if hasWritten
    then do
      let finalEvent = finalEvents |> Array.last |> Maybe.getOrDie
      Task.yield
        InsertionSuccess
          { localPosition = finalEvent.metadata.localPosition |> Maybe.withDefault (StreamPosition 0),
            globalPosition = finalEvent.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0)
          }
    else
      (InsertionError ConsistencyCheckFailed)
        |> Task.throw


readStreamForwardFromImpl ::
  StreamStore eventType ->
  EntityName ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Array (StoredEvent eventType))
readStreamForwardFromImpl store entityName streamId position (Limit (limit)) = do
  channel <- store |> ensureStream entityName streamId
  channel
    |> DurableChannel.getAndTransform \events ->
      events
        |> Array.dropWhile (\event -> event.metadata.localPosition < Just position)
        |> Array.take (fromIntegral limit)


readStreamBackwardFromImpl ::
  StreamStore eventType ->
  EntityName ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Array (StoredEvent eventType))
readStreamBackwardFromImpl store entityName streamId position (Limit (limit)) = do
  channel <- store |> ensureStream entityName streamId
  channel
    |> DurableChannel.getAndTransform \events ->
      events
        |> Array.takeIf (\event -> event.metadata.localPosition < Just position)
        |> Array.reverse
        |> Array.take (fromIntegral limit)


readAllStreamEventsImpl ::
  StreamStore eventType ->
  EntityName ->
  StreamId ->
  Task Error (Array (StoredEvent eventType))
readAllStreamEventsImpl store entityName streamId = do
  channel <- store |> ensureStream entityName streamId
  channel
    |> DurableChannel.getAndTransform unchanged


readAllEventsForwardFromImpl ::
  StreamStore eventType ->
  StreamPosition ->
  Limit ->
  Task Error (Array (StoredEvent eventType))
readAllEventsForwardFromImpl store (StreamPosition (position)) (Limit (limit)) = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents |> Array.drop (fromIntegral position) |> Array.take (fromIntegral limit) |> Task.yield


readAllEventsBackwardFromImpl ::
  StreamStore eventType ->
  StreamPosition ->
  Limit ->
  Task Error (Array (StoredEvent eventType))
readAllEventsBackwardFromImpl store (StreamPosition (position)) (Limit (limit)) = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents
    |> Array.takeIf
      ( \event -> do
          let (StreamPosition eventPos) = event.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0)
          eventPos <= position
      )
    |> Array.reverse
    |> Array.take (fromIntegral limit)
    |> Task.yield


readAllEventsForwardFromFilteredImpl ::
  StreamStore eventType ->
  StreamPosition ->
  Limit ->
  Array EntityName ->
  Task Error (Array (StoredEvent eventType))
readAllEventsForwardFromFilteredImpl store (StreamPosition (position)) (Limit (limit)) entityNames = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents
    |> Array.takeIf
      ( \event -> do
          let (StreamPosition eventPos) = event.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0)
          eventPos >= position
      )
    |> Array.takeIf (\event -> entityNames |> Array.any (\entityName -> entityName == event.entityName))
    |> Array.take (fromIntegral limit)
    |> Task.yield


readAllEventsBackwardFromFilteredImpl ::
  StreamStore eventType ->
  StreamPosition ->
  Limit ->
  Array EntityName ->
  Task Error (Array (StoredEvent eventType))
readAllEventsBackwardFromFilteredImpl store (StreamPosition (position)) (Limit (limit)) entityNames = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents
    |> Array.takeIf
      ( \event -> do
          let (StreamPosition eventPos) = event.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0)
          eventPos <= position
      )
    |> Array.reverse
    |> Array.takeIf (\event -> entityNames |> Array.any (\entityName -> entityName == event.entityName))
    |> Array.take (fromIntegral limit)
    |> Task.yield


-- SUBSCRIPTION IMPLEMENTATIONS

subscribeToAllEventsImpl ::
  StreamStore eventType ->
  (StoredEvent eventType -> Task Error Unit) ->
  Task Error SubscriptionId
subscribeToAllEventsImpl store handler = do
  subscriptionId <- generateSubscriptionId
  let subscription = Subscription {subscriptionType = AllEvents, handler}
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock
  Task.yield subscriptionId


subscribeToAllEventsFromPositionImpl ::
  StreamStore eventType ->
  StreamPosition ->
  (StoredEvent eventType -> Task Error Unit) ->
  Task Error SubscriptionId
subscribeToAllEventsFromPositionImpl store fromPosition handler = do
  subscriptionId <- generateSubscriptionId
  let subscription = Subscription {subscriptionType = AllEvents, handler}

  -- First, add the subscription for future events
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock

  -- Then, deliver historical events from the specified position
  deliverHistoricalEvents store fromPosition handler

  Task.yield subscriptionId


subscribeToAllEventsFromStartImpl ::
  StreamStore eventType ->
  (StoredEvent eventType -> Task Error Unit) ->
  Task Error SubscriptionId
subscribeToAllEventsFromStartImpl store handler = do
  subscriptionId <- generateSubscriptionId
  let subscription = Subscription {subscriptionType = AllEvents, handler}

  -- First, add the subscription for future events
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock

  -- Then, deliver ALL historical events from the very beginning (position -1)
  deliverHistoricalEventsFromStart store handler subscriptionId

  Task.yield subscriptionId


subscribeToEntityEventsImpl ::
  StreamStore eventType ->
  EntityName ->
  (StoredEvent eventType -> Task Error Unit) ->
  Task Error SubscriptionId
subscribeToEntityEventsImpl store entityName handler = do
  subscriptionId <- generateSubscriptionId
  let subscription = Subscription {subscriptionType = EntityEvents entityName, handler}
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock
  Task.yield subscriptionId


subscribeToStreamEventsImpl ::
  StreamStore eventType ->
  EntityName ->
  StreamId ->
  (StoredEvent eventType -> Task Error Unit) ->
  Task Error SubscriptionId
subscribeToStreamEventsImpl store entityName streamId handler = do
  subscriptionId <- generateSubscriptionId
  let subscription = Subscription {subscriptionType = StreamEvents entityName streamId, handler}
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock
  Task.yield subscriptionId


unsubscribeImpl ::
  StreamStore eventType ->
  SubscriptionId ->
  Task Error Unit
unsubscribeImpl store subscriptionId = do
  store.subscriptions
    |> ConcurrentVar.modify (Map.remove subscriptionId)
    |> Lock.with store.globalLock


-- SUBSCRIPTION HELPER FUNCTIONS

generateSubscriptionId :: Task _ SubscriptionId
generateSubscriptionId = do
  uuid <- Uuid.generate
  uuid |> toText |> SubscriptionId |> Task.yield


-- notifySubscribers :: forall err. (Show err) => StreamStore eventType -> Event -> Task err Unit
-- notifySubscribers store event = do
--   -- Get subscriptions snapshot without locks
--   allSubscriptions <- ConcurrentVar.peek store.subscriptions
--   let relevantSubscriptions =
--         allSubscriptions
--           |> Map.entries
--           |> Array.takeIf (\(_, subscription) -> shouldNotify subscription.subscriptionType event)

--   -- Notify each subscriber in fire-and-forget manner using async tasks
--   relevantSubscriptions
--     |> Task.mapArray (\(_, subscription) -> AsyncTask.run (notifySubscriber subscription event))
--     |> discard

-- shouldNotify :: SubscriptionType -> Event -> Bool
-- shouldNotify subscriptionType event =
--   case subscriptionType of
--     AllEvents -> True
--     EntityEvents entityName -> event.entityName == entityName
--     StreamEvents entityName streamId -> event.entityName == entityName && event.streamId == streamId

-- notifySubscriber :: Subscription -> Event -> Task _ Unit
-- notifySubscriber subscription event = do
--   -- Execute subscriber handler and catch any errors to prevent failures from affecting the event store
--   result <- subscription.handler event |> Task.asResult
--   case result of
--     Ok _ -> Task.yield ()
--     Err _ -> Task.yield () -- Silently ignore subscriber errors to maintain store reliability

deliverHistoricalEvents ::
  StreamStore eventType -> StreamPosition -> (StoredEvent eventType -> Task Error Unit) -> Task _ Unit
deliverHistoricalEvents store fromPosition handler = do
  -- Read all events from the specified position onwards
  let (StreamPosition startPos) = fromPosition
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged

  let historicalEvents =
        allGlobalEvents
          |> Array.takeIf
            ( \event -> do
                let (StreamPosition eventPos) = event.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0)
                eventPos > startPos
            )

  -- Deliver each historical event to the subscriber synchronously
  historicalEvents
    |> Task.mapArray (\event -> notifySubscriberSafely handler event)
    |> discard


deliverHistoricalEventsFromStart ::
  StreamStore eventType -> (StoredEvent eventType -> Task Error Unit) -> SubscriptionId -> Task _ Unit
deliverHistoricalEventsFromStart store handler _subscriptionId = do
  -- Read ALL events from the very beginning (no position filter)
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged

  -- Deliver each historical event to the subscriber synchronously
  allGlobalEvents
    |> Task.mapArray (\event -> notifySubscriberSafely handler event)
    |> discard


notifySubscriberSafely :: (StoredEvent eventType -> Task Error Unit) -> StoredEvent eventType -> Task _ Unit
notifySubscriberSafely handler event = do
  -- Execute subscriber handler and catch any errors
  result <- handler event |> Task.asResult
  case result of
    Ok _ -> Task.yield ()
    Err _ -> Task.yield () -- Silently ignore subscriber errors


truncateStreamImpl :: StreamStore eventType -> EntityName -> StreamId -> StreamPosition -> Task Error Unit
truncateStreamImpl store entityName streamId position = do
  let streams = store.streams
  let foo ::
        Map (EntityName, StreamId) (DurableChannel (StoredEvent eventType)) ->
        Task Never (Map (EntityName, StreamId) (DurableChannel (StoredEvent eventType)), Maybe Error)
      foo streamMap = do
        let key = (entityName, streamId)
        case streamMap |> Map.get key of
          Nothing -> Task.yield (streamMap, StreamNotFound entityName streamId |> Just)
          Just chan -> do
            chan |> DurableChannel.modify (Array.dropWhile (\p -> p.metadata.localPosition < Just position))
            Task.yield (streamMap, Nothing)

  maybeErr <- streams |> ConcurrentVar.modifyReturning foo
  case maybeErr of
    Nothing -> pass
    Just err -> Task.throw err