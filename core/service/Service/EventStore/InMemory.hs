module Service.EventStore.InMemory (
  new,
) where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import DurableChannel qualified
import Lock qualified
import Map qualified
import Service.Event
import Service.Event qualified as Event
import Service.EventStore.Core
import Task qualified
import Uuid qualified


new :: Task Error EventStore
new = do
  store <- newEmptyStreamStore
  let eventStore =
        EventStore
          { appendToStream = appendToStreamWithNotification store,
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

appendToStreamWithNotification ::
  StreamStore ->
  InsertionEvent ->
  Task Error Event
appendToStreamWithNotification store event = do
  finalEvent <- appendToStreamImpl store event
  -- Notify subscribers AFTER the event has been successfully stored and locks are released
  notifySubscribers store finalEvent
  Task.yield finalEvent


data StreamStore = StreamStore
  { globalStream :: (DurableChannel Event),
    streams :: ConcurrentVar (Map (EntityId, StreamId) (DurableChannel Event)),
    globalLock :: Lock,
    subscriptions :: ConcurrentVar (Map SubscriptionId Subscription)
  }


data SubscriptionType
  = AllEvents
  | EntityEvents EntityId
  | StreamEvents EntityId StreamId
  deriving (Eq, Show)


data Subscription = Subscription
  { subscriptionType :: SubscriptionType,
    handler :: Event -> Task Error Unit
  }


newEmptyStreamStore :: Task _ StreamStore
newEmptyStreamStore = do
  globalLock <- Lock.new
  globalStream <- DurableChannel.new
  streams <- ConcurrentVar.containing Map.empty
  subscriptions <- ConcurrentVar.containing Map.empty
  Task.yield StreamStore {globalLock, streams, globalStream, subscriptions}


-- | Idempotent stream creation.
ensureStream :: EntityId -> StreamId -> StreamStore -> Task _ (DurableChannel Event)
ensureStream entityId streamId store = do
  let modifier streamMap = do
        case streamMap |> Map.get (entityId, streamId) of
          Just channel -> do
            Task.yield (streamMap, channel)
          Nothing -> do
            channel <- DurableChannel.new
            Task.yield (streamMap |> Map.set (entityId, streamId) channel, channel)
  store.streams
    |> ConcurrentVar.modifyReturning modifier
    |> Lock.with store.globalLock


appendToStreamImpl ::
  StreamStore ->
  InsertionEvent ->
  Task Error Event
appendToStreamImpl store event = do
  let id = event.id
  let localPosition = event.localPosition
  let entityId = event.entityId
  let streamId = event.streamId
  let expectedPosition = localPosition
  channel <- store |> ensureStream entityId streamId

  let appendCondition :: Array Event -> Bool
      appendCondition events = do
        let currentLength = Array.length events
        let (StreamPosition pos) = expectedPosition
        pos == currentLength

  -- First, get the global index from the global stream
  globalIndex <-
    store.globalStream
      |> DurableChannel.writeWithIndex (\index -> event |> Event.fromInsertionEvent (StreamPosition index))

  -- Now create the event with the correct global position
  let globalPosition = StreamPosition globalIndex
  let finalEvent = Event {id, streamId, entityId, localPosition, globalPosition}

  -- Write to the individual stream with the correctly positioned event
  hasWritten <-
    channel |> DurableChannel.checkAndWrite appendCondition finalEvent

  if hasWritten
    then do
      Task.yield finalEvent
    else
      (ConcurrencyConflict streamId expectedPosition)
        |> Task.throw


readStreamForwardFromImpl ::
  StreamStore ->
  EntityId ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Array Event)
readStreamForwardFromImpl store entityId streamId position (Limit (limit)) = do
  channel <- store |> ensureStream entityId streamId
  channel
    |> DurableChannel.getAndTransform \events ->
      events
        |> Array.dropWhile (\event -> event.localPosition < position)
        |> Array.take limit


readStreamBackwardFromImpl ::
  StreamStore ->
  EntityId ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Array Event)
readStreamBackwardFromImpl store entityId streamId position (Limit (limit)) = do
  channel <- store |> ensureStream entityId streamId
  channel
    |> DurableChannel.getAndTransform \events ->
      events
        |> Array.takeIf (\event -> event.localPosition < position)
        |> Array.reverse
        |> Array.take limit


readAllStreamEventsImpl ::
  StreamStore ->
  EntityId ->
  StreamId ->
  Task Error (Array Event)
readAllStreamEventsImpl store entityId streamId = do
  channel <- store |> ensureStream entityId streamId
  channel
    |> DurableChannel.getAndTransform unchanged


readAllEventsForwardFromImpl ::
  StreamStore ->
  StreamPosition ->
  Limit ->
  Task Error (Array Event)
readAllEventsForwardFromImpl store (StreamPosition (position)) (Limit (limit)) = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents |> Array.drop position |> Array.take limit |> Task.yield


readAllEventsBackwardFromImpl ::
  StreamStore ->
  StreamPosition ->
  Limit ->
  Task Error (Array Event)
readAllEventsBackwardFromImpl store (StreamPosition (position)) (Limit (limit)) = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents
    |> Array.takeIf
      ( \event -> do
          let (StreamPosition eventPos) = event.globalPosition
          eventPos <= position
      )
    |> Array.reverse
    |> Array.take limit
    |> Task.yield


readAllEventsForwardFromFilteredImpl ::
  StreamStore ->
  StreamPosition ->
  Limit ->
  Array EntityId ->
  Task Error (Array Event)
readAllEventsForwardFromFilteredImpl store (StreamPosition (position)) (Limit (limit)) entityIds = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents
    |> Array.takeIf
      ( \event -> do
          let (StreamPosition eventPos) = event.globalPosition
          eventPos >= position
      )
    |> Array.takeIf (\event -> entityIds |> Array.any (\entityId -> entityId == event.entityId))
    |> Array.take limit
    |> Task.yield


readAllEventsBackwardFromFilteredImpl ::
  StreamStore ->
  StreamPosition ->
  Limit ->
  Array EntityId ->
  Task Error (Array Event)
readAllEventsBackwardFromFilteredImpl store (StreamPosition (position)) (Limit (limit)) entityIds = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents
    |> Array.takeIf
      ( \event -> do
          let (StreamPosition eventPos) = event.globalPosition
          eventPos <= position
      )
    |> Array.reverse
    |> Array.takeIf (\event -> entityIds |> Array.any (\entityId -> entityId == event.entityId))
    |> Array.take limit
    |> Task.yield


-- SUBSCRIPTION IMPLEMENTATIONS

subscribeToAllEventsImpl ::
  StreamStore ->
  (Event -> Task Error Unit) ->
  Task Error SubscriptionId
subscribeToAllEventsImpl store handler = do
  subscriptionId <- generateSubscriptionId
  let subscription = Subscription {subscriptionType = AllEvents, handler}
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock
  Task.yield subscriptionId


subscribeToAllEventsFromPositionImpl ::
  StreamStore ->
  StreamPosition ->
  (Event -> Task Error Unit) ->
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
  StreamStore ->
  (Event -> Task Error Unit) ->
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
  StreamStore ->
  EntityId ->
  (Event -> Task Error Unit) ->
  Task Error SubscriptionId
subscribeToEntityEventsImpl store entityId handler = do
  subscriptionId <- generateSubscriptionId
  let subscription = Subscription {subscriptionType = EntityEvents entityId, handler}
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock
  Task.yield subscriptionId


subscribeToStreamEventsImpl ::
  StreamStore ->
  EntityId ->
  StreamId ->
  (Event -> Task Error Unit) ->
  Task Error SubscriptionId
subscribeToStreamEventsImpl store entityId streamId handler = do
  subscriptionId <- generateSubscriptionId
  let subscription = Subscription {subscriptionType = StreamEvents entityId streamId, handler}
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock
  Task.yield subscriptionId


unsubscribeImpl ::
  StreamStore ->
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


notifySubscribers :: forall err. (Show err) => StreamStore -> Event -> Task err Unit
notifySubscribers store event = do
  -- Get subscriptions snapshot without locks
  allSubscriptions <- ConcurrentVar.peek store.subscriptions
  let relevantSubscriptions =
        allSubscriptions
          |> Map.entries
          |> Array.takeIf (\(_, subscription) -> shouldNotify subscription.subscriptionType event)

  -- Notify each subscriber in fire-and-forget manner using async tasks
  relevantSubscriptions
    |> Task.mapArray (\(_, subscription) -> AsyncTask.run (notifySubscriber subscription event))
    |> discard


shouldNotify :: SubscriptionType -> Event -> Bool
shouldNotify subscriptionType event =
  case subscriptionType of
    AllEvents -> True
    EntityEvents entityId -> event.entityId == entityId
    StreamEvents entityId streamId -> event.entityId == entityId && event.streamId == streamId


notifySubscriber :: Subscription -> Event -> Task _ Unit
notifySubscriber subscription event = do
  -- Execute subscriber handler and catch any errors to prevent failures from affecting the event store
  result <- subscription.handler event |> Task.asResult
  case result of
    Ok _ -> Task.yield ()
    Err _ -> Task.yield () -- Silently ignore subscriber errors to maintain store reliability


deliverHistoricalEvents :: StreamStore -> StreamPosition -> (Event -> Task Error Unit) -> Task _ Unit
deliverHistoricalEvents store fromPosition handler = do
  -- Read all events from the specified position onwards
  let (StreamPosition startPos) = fromPosition
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged

  let historicalEvents =
        allGlobalEvents
          |> Array.takeIf
            ( \event -> do
                let (StreamPosition eventPos) = event.globalPosition
                eventPos > startPos
            )

  -- Deliver each historical event to the subscriber synchronously
  historicalEvents
    |> Task.mapArray (\event -> notifySubscriberSafely handler event)
    |> discard


deliverHistoricalEventsFromStart :: StreamStore -> (Event -> Task Error Unit) -> SubscriptionId -> Task _ Unit
deliverHistoricalEventsFromStart store handler _subscriptionId = do
  -- Read ALL events from the very beginning (no position filter)
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged

  -- Deliver each historical event to the subscriber synchronously
  allGlobalEvents
    |> Task.mapArray (\event -> notifySubscriberSafely handler event)
    |> discard


notifySubscriberSafely :: (Event -> Task Error Unit) -> Event -> Task _ Unit
notifySubscriberSafely handler event = do
  -- Execute subscriber handler and catch any errors
  result <- handler event |> Task.asResult
  case result of
    Ok _ -> Task.yield ()
    Err _ -> Task.yield () -- Silently ignore subscriber errors


truncateStreamImpl :: StreamStore -> EntityId -> StreamId -> StreamPosition -> Task Error Unit
truncateStreamImpl store entityId streamId position = do
  let streams = store.streams
  let foo ::
        Map (EntityId, StreamId) (DurableChannel Event) ->
        Task Never (Map (EntityId, StreamId) (DurableChannel Event), Maybe Error)
      foo streamMap = do
        let key = (entityId, streamId)
        case streamMap |> Map.get key of
          Nothing -> Task.yield (streamMap, StreamNotFound entityId streamId |> Just)
          Just chan -> do
            chan |> DurableChannel.modify (Array.dropWhile (\p -> p.localPosition < position))
            Task.yield (streamMap, Nothing)

  maybeErr <- streams |> ConcurrentVar.modifyReturning foo
  case maybeErr of
    Nothing -> pass
    Just err -> Task.throw err