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
import Maybe qualified
import Service.Event
import Service.Event.EventMetadata qualified as EventMetadata
import Service.EventStore.Core
import Stream (Stream)
import Stream qualified
import Task qualified
import Uuid qualified


new :: (Show eventType) => Task Error (EventStore eventType)
new = do
  store <- newEmptyStreamStore
  let eventStore =
        EventStore
          { insert = insertImpl store,
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

fromInsertionPayload :: StreamPosition -> Int -> InsertionPayload eventType -> Array (Event eventType)
fromInsertionPayload (StreamPosition globalPosition) streamLength payload =
  payload.insertions
    |> Array.indexed
    |> Array.map \(index, insertion) -> do
      let globalPos = StreamPosition (globalPosition + fromIntegral index)
      let localPos = case insertion.metadata.localPosition of
            Just pos -> pos
            Nothing -> StreamPosition (fromIntegral streamLength + fromIntegral index)
      let newMetadata =
            insertion.metadata
              { EventMetadata.globalPosition = Just globalPos,
                EventMetadata.localPosition = Just localPos
              }
      Event
        { entityName = payload.entityName,
          streamId = payload.streamId,
          metadata = newMetadata,
          event = insertion.event
        }


data StreamStore eventType = StreamStore
  { globalStream :: (DurableChannel (Event eventType)),
    streams :: ConcurrentVar (Map (EntityName, StreamId) (DurableChannel (Event eventType))),
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
    handler :: Event eventType -> Task Text Unit
  }


newEmptyStreamStore :: Task _ (StreamStore eventType)
newEmptyStreamStore = do
  globalLock <- Lock.new
  globalStream <- DurableChannel.new
  streams <- ConcurrentVar.containing Map.empty
  subscriptions <- ConcurrentVar.containing Map.empty
  Task.yield StreamStore {globalLock, streams, globalStream, subscriptions}


-- | Idempotent stream creation.
ensureStream :: EntityName -> StreamId -> StreamStore eventType -> Task _ (DurableChannel (Event eventType))
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
  (Show eventType) =>
  StreamStore eventType ->
  InsertionPayload eventType ->
  Task Error InsertionSuccess
insertImpl store payload = do
  let insertionsCount = payload.insertions |> Array.length

  if insertionsCount <= 0
    then Task.throw (InsertionError EmptyPayload)
    else pass

  if insertionsCount > 100
    then Task.throw (InsertionError PayloadTooLarge)
    else pass

  let entityName = payload.entityName
  let streamId = payload.streamId
  let expectedPosition =
        payload.insertions
          |> Array.map (\i -> i.metadata.localPosition |> Maybe.withDefault (StreamPosition 0))
          |> Array.first
          |> Maybe.withDefault (StreamPosition 0)
  channel <- store |> ensureStream entityName streamId

  let appendCondition :: Array (Event eventType) -> Bool
      appendCondition events =
        let currentLength = Array.length events
            (StreamPosition pos) = expectedPosition
            positionMatches = fromIntegral pos == currentLength
        in  case payload.insertionType of
              AnyStreamState -> True
              StreamCreation -> currentLength == 0 && positionMatches
              ExistingStream -> currentLength > 0 && positionMatches
              InsertAfter (StreamPosition afterPos) ->
                fromIntegral afterPos < currentLength && positionMatches

  result <-
    Lock.with store.globalLock do
      currentStreamEvents <- channel |> DurableChannel.getAndTransform unchanged
      let consistencyCheckPassed = appendCondition currentStreamEvents
      let streamLength = Array.length currentStreamEvents

      if consistencyCheckPassed
        then do
          globalIndex <-
            store.globalStream
              |> DurableChannel.writeWithIndex
                (\index -> payload |> fromInsertionPayload (fromIntegral index |> StreamPosition) streamLength)

          let globalPosition = StreamPosition (fromIntegral globalIndex)
          let finalEvents = payload |> fromInsertionPayload globalPosition streamLength
          channel |> DurableChannel.checkAndWrite appendCondition finalEvents |> discard

          finalEvents |> Task.forEach (notifySubscribers store)
          let finalEvent = finalEvents |> Array.last |> Maybe.getOrDie
          Task.yield
            ( Ok
                InsertionSuccess
                  { localPosition = finalEvent.metadata.localPosition |> Maybe.withDefault (StreamPosition 0),
                    globalPosition = finalEvent.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0)
                  }
            )
        else do
          Task.yield (Err ConsistencyCheckFailed)

  case result of
    Ok success -> Task.yield success
    Err errorType -> (InsertionError errorType) |> Task.throw


readStreamForwardFromImpl ::
  StreamStore eventType ->
  EntityName ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadStreamMessage eventType))
readStreamForwardFromImpl store entityName streamId position (Limit (limit)) = do
  channel <- store |> ensureStream entityName streamId
  events <-
    channel
      |> DurableChannel.getAndTransform \events ->
        events
          |> Array.dropWhile (\event -> event.metadata.localPosition < Just position)
          |> Array.take (fromIntegral limit)
  let items = events |> Array.map StreamEvent
  Stream.fromArray items


readStreamBackwardFromImpl ::
  StreamStore eventType ->
  EntityName ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadStreamMessage eventType))
readStreamBackwardFromImpl store entityName streamId position (Limit (limit)) = do
  channel <- store |> ensureStream entityName streamId
  events <-
    channel
      |> DurableChannel.getAndTransform \events ->
        events
          |> Array.takeIf (\event -> event.metadata.localPosition < Just position)
          |> Array.reverse
          |> Array.take (fromIntegral limit)
  let items = events |> Array.map StreamEvent
  Stream.fromArray items


readAllStreamEventsImpl ::
  StreamStore eventType ->
  EntityName ->
  StreamId ->
  Task Error (Stream (ReadStreamMessage eventType))
readAllStreamEventsImpl store entityName streamId = do
  channel <- store |> ensureStream entityName streamId
  events <-
    channel
      |> DurableChannel.getAndTransform unchanged
  let items = events |> Array.map StreamEvent
  Stream.fromArray items


readAllEventsForwardFromImpl ::
  StreamStore eventType ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadAllMessage eventType))
readAllEventsForwardFromImpl store (StreamPosition (position)) (Limit (limit)) = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  let items =
        allGlobalEvents
          |> Array.drop (fromIntegral position)
          |> Array.take (fromIntegral limit)
          |> Array.map AllEvent
  Stream.fromArray items


readAllEventsBackwardFromImpl ::
  StreamStore eventType ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadAllMessage eventType))
readAllEventsBackwardFromImpl store (StreamPosition (position)) (Limit (limit)) = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  let items =
        allGlobalEvents
          |> Array.takeIf
            ( \event -> do
                let (StreamPosition eventPos) = event.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0)
                eventPos <= position
            )
          |> Array.reverse
          |> Array.take (fromIntegral limit)
          |> Array.map AllEvent
  Stream.fromArray items


readAllEventsForwardFromFilteredImpl ::
  StreamStore eventType ->
  StreamPosition ->
  Limit ->
  Array EntityName ->
  Task Error (Stream (ReadAllMessage eventType))
readAllEventsForwardFromFilteredImpl store (StreamPosition (position)) (Limit (limit)) entityNames = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  let items =
        allGlobalEvents
          |> Array.takeIf
            ( \event -> do
                let (StreamPosition eventPos) = event.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0)
                eventPos >= position
            )
          |> Array.takeIf (\event -> entityNames |> Array.any (\entityName -> entityName == event.entityName))
          |> Array.take (fromIntegral limit)
          |> Array.map AllEvent
  Stream.fromArray items


readAllEventsBackwardFromFilteredImpl ::
  StreamStore eventType ->
  StreamPosition ->
  Limit ->
  Array EntityName ->
  Task Error (Stream (ReadAllMessage eventType))
readAllEventsBackwardFromFilteredImpl store (StreamPosition (position)) (Limit (limit)) entityNames = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  let items =
        allGlobalEvents
          |> Array.takeIf
            ( \event -> do
                let (StreamPosition eventPos) = event.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0)
                eventPos <= position
            )
          |> Array.reverse
          |> Array.takeIf (\event -> entityNames |> Array.any (\entityName -> entityName == event.entityName))
          |> Array.take (fromIntegral limit)
          |> Array.map AllEvent
  Stream.fromArray items


-- SUBSCRIPTION IMPLEMENTATIONS

subscribeToAllEventsImpl ::
  StreamStore eventType ->
  (Event eventType -> Task Text Unit) ->
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
  (Event eventType -> Task Text Unit) ->
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
  (Event eventType -> Task Text Unit) ->
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
  (Event eventType -> Task Text Unit) ->
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
  (Event eventType -> Task Text Unit) ->
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


notifySubscribers :: (Show err) => StreamStore eventType -> Event eventType -> Task err Unit
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


shouldNotify :: SubscriptionType -> Event eventType -> Bool
shouldNotify subscriptionType event =
  case subscriptionType of
    AllEvents -> True
    EntityEvents entityName -> event.entityName == entityName
    StreamEvents entityName streamId -> event.entityName == entityName && event.streamId == streamId


notifySubscriber :: Subscription eventType -> Event eventType -> Task _ Unit
notifySubscriber subscription event = do
  -- Execute subscriber handler and catch any errors to prevent failures from affecting the event store
  result <- subscription.handler event |> Task.asResult
  case result of
    Ok _ -> Task.yield unit
    Err _ -> Task.yield unit -- Silently ignore subscriber errors to maintain store reliability


deliverHistoricalEvents ::
  StreamStore eventType -> StreamPosition -> (Event eventType -> Task Text Unit) -> Task _ Unit
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
  StreamStore eventType -> (Event eventType -> Task Text Unit) -> SubscriptionId -> Task _ Unit
deliverHistoricalEventsFromStart store handler _subscriptionId = do
  -- Read ALL events from the very beginning (no position filter)
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged

  -- Deliver each historical event to the subscriber synchronously
  allGlobalEvents
    |> Task.mapArray (\event -> notifySubscriberSafely handler event)
    |> discard


notifySubscriberSafely :: (Event eventType -> Task Text Unit) -> Event eventType -> Task _ Unit
notifySubscriberSafely handler event = do
  -- Execute subscriber handler and catch any errors
  result <- handler event |> Task.asResult
  case result of
    Ok _ -> Task.yield unit
    Err _ -> Task.yield unit -- Silently ignore subscriber errors


truncateStreamImpl :: StreamStore eventType -> EntityName -> StreamId -> StreamPosition -> Task Error Unit
truncateStreamImpl store entityName streamId position = do
  let streams = store.streams
  let performTruncation ::
        Map (EntityName, StreamId) (DurableChannel (Event eventType)) ->
        Task Never (Map (EntityName, StreamId) (DurableChannel (Event eventType)), Maybe Error)
      performTruncation streamMap = do
        let key = (entityName, streamId)
        case streamMap |> Map.get key of
          Nothing -> Task.yield (streamMap, StreamNotFound entityName streamId |> Just)
          Just chan -> do
            chan |> DurableChannel.modify (Array.dropWhile (\p -> p.metadata.localPosition < Just position))
            Task.yield (streamMap, Nothing)

  maybeErr <- streams |> ConcurrentVar.modifyReturning performTruncation
  case maybeErr of
    Nothing -> pass
    Just err -> Task.throw err