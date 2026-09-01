module Service.EventStore.Simple (
  SimpleEventStore (..),
  new,
) where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Directory qualified
import DurableChannel qualified
import File qualified
import Json qualified
import LinkedList qualified
import Lock qualified
import Log qualified
import Map qualified
import Maybe qualified
import Path qualified
import Service.Event
import Service.Event.EntityName qualified as EntityName
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core
import Stream (Stream)
import Stream qualified
import Task qualified
import Text qualified
import Uuid qualified


data SimpleEventStore = SimpleEventStore
  { basePath :: Path,
    persistent :: Bool
  }
  deriving (Eq, Show, Generic)


instance EventStoreConfig SimpleEventStore where
  createEventStore config = new config


new :: SimpleEventStore -> Task Text (EventStore Json.Value)
new config = do
  store <- newEmptyStreamStore
  insertionLock <- Lock.new
  Task.when config.persistent do
    loadEventsFromDisk config.basePath store
      |> Task.mapError toText
  let eventStore =
        EventStore
          { insert = insertImpl config store insertionLock,
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
            truncateStream = truncateStreamImpl config store,
            close = closeImpl
          }
  Task.yield eventStore


closeImpl :: Task Text Unit
closeImpl = Task.yield unit


-- PRIVATE


-- | Sanitize a text value for safe use in file paths.
-- Strips path traversal characters: /, .., \, and null bytes.
-- Note: POSIX-focused. Does not handle Windows-reserved characters
-- (:, *, ?, ", <, >, |) since NeoHaskell targets CLI tools on POSIX systems.
sanitizePath :: Text -> Text
sanitizePath input =
  input
    |> Text.replace "/" "_"
    |> Text.replace "\\" "_"
    |> Text.replace ".." "_"
    |> Text.replace "\0" "_"


-- | Build the file path for a stream's JSONL file.
buildEventFilePath :: Path -> EntityName -> StreamId -> Path
buildEventFilePath basePath entityName streamId = do
  let sanitizedEntity = entityName |> EntityName.toText |> sanitizePath
  let sanitizedStream = streamId |> StreamId.toText |> sanitizePath
  let entityPath = Path.fromText sanitizedEntity |> Maybe.getOrDie
  let streamFile = Path.fromText [fmt|#{sanitizedStream}.jsonl|] |> Maybe.getOrDie
  Path.joinPaths [basePath, entityPath, streamFile]


-- | Write events to a JSONL file with error handling.
-- Uses the provided write function (appendText for persist, writeText for rewrite).
writeEventsToFile ::
  (Path -> Text -> Task Text Unit) ->
  SimpleEventStore ->
  EntityName ->
  StreamId ->
  Array (Event Json.Value) ->
  Task _ Unit
writeEventsToFile writeFn config entityName streamId events = do
  let filePath = buildEventFilePath config.basePath entityName streamId
  let entityDir = do
        let sanitizedEntity = entityName |> EntityName.toText |> sanitizePath
        let entityDirPath = Path.fromText sanitizedEntity |> Maybe.getOrDie
        Path.joinPaths [config.basePath, entityDirPath]
  let jsonLines =
        events
          |> Array.map (\event -> Json.encodeText event ++ "\n")
          |> Text.concat
  result <-
    ( do
        Directory.createIfMissing entityDir |> Task.mapError toText
        writeFn filePath jsonLines
    )
      |> Task.asResult
  case result of
    Ok _ -> Task.yield unit
    Err _err -> do
      Log.withScope [("component", "EventStore.Simple")] do
        Log.warn [fmt|Failed to write events for #{EntityName.toText entityName}/#{StreamId.toText streamId}|]
          |> Task.ignoreError
      Task.yield unit


-- | Persist events to a JSONL file on disk.
-- Failures are logged but do not throw (in-memory state is already updated).
persistEvents :: SimpleEventStore -> EntityName -> StreamId -> Array (Event Json.Value) -> Task Never Unit
persistEvents config entityName streamId events =
  writeEventsToFile (\path text -> File.appendText path text |> Task.mapError toText) config entityName streamId events


-- | Load all events from JSONL files on disk into the stream store.
loadEventsFromDisk :: Path -> StreamStore -> Task _ Unit
loadEventsFromDisk basePath store = do
  -- Ensure base directory exists
  Directory.createIfMissing basePath
    |> Task.mapError (\_ -> "Failed to create base directory" :: Text)

  -- Walk the base directory to find all files
  allFiles <-
    Directory.walk basePath
      |> Task.mapError (\_ -> "Failed to walk events directory" :: Text)

  let jsonlFiles = allFiles |> Array.takeIf (\filePath -> filePath |> Path.endsWith ".jsonl")

  -- Collect all events with their global positions for proper ordering
  allEvents <- ConcurrentVar.containing (Array.empty :: Array (Event Json.Value))

  jsonlFiles |> Task.forEach \relativeFilePath -> do
    let fullPath = Path.joinPaths [basePath, relativeFilePath]
    fileResult <- File.readText fullPath |> Task.asResult
    case fileResult of
      Err _ -> do
        Log.warn [fmt|Failed to read events file: #{Path.toText relativeFilePath}|]
          |> Task.ignoreError
      Ok contents -> do
        let fileLines = contents |> Text.lines |> Array.takeIf (\line -> Text.isEmpty line |> not)
        fileLines |> Task.forEach \line -> do
          case Json.decodeText @(Event Json.Value) line of
            Ok event -> do
              allEvents |> ConcurrentVar.modify (Array.push event)
            Err err -> do
              Log.warn [fmt|Failed to decode event line: #{err}|]
                |> Task.ignoreError

  -- Sort events by global position and replay them
  loadedEvents <- ConcurrentVar.peek allEvents
  let sortedEvents =
        loadedEvents
          |> Array.toLinkedList
          |> LinkedList.sortBy (\event -> event.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0))
          |> Array.fromLinkedList

  -- Replay each event into the in-memory store
  sortedEvents |> Task.forEach \event -> do
    channel <- store |> ensureStream event.entityName event.streamId
    channel |> DurableChannel.write (Array.wrap event) |> Task.ignoreError
    store.globalStream |> DurableChannel.write (Array.wrap event) |> Task.ignoreError

  Log.withScope [("component", "EventStore.Simple")] do
    Log.debug [fmt|Loaded #{toText (Array.length sortedEvents)} events from disk|]
      |> Task.ignoreError


fromInsertionPayload :: StreamPosition -> Int -> InsertionPayload Json.Value -> Array (Event Json.Value)
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


data StreamStore = StreamStore
  { globalStream :: (DurableChannel (Event Json.Value)),
    streams :: ConcurrentVar (Map (EntityName, StreamId) (DurableChannel (Event Json.Value))),
    globalLock :: Lock,
    subscriptions :: ConcurrentVar (Map SubscriptionId Subscription)
  }


data SubscriptionType
  = AllEvents
  | EntityEvents EntityName
  | StreamEvents EntityName StreamId
  deriving (Eq, Show)


data Subscription = Subscription
  { subscriptionType :: SubscriptionType,
    handler :: Event Json.Value -> Task Text Unit
  }


newEmptyStreamStore :: Task _ StreamStore
newEmptyStreamStore = do
  globalLock <- Lock.new
  globalStream <- DurableChannel.new
  streams <- ConcurrentVar.containing Map.empty
  subscriptions <- ConcurrentVar.containing Map.empty
  Task.yield StreamStore {globalLock, streams, globalStream, subscriptions}


-- | Idempotent stream creation.
ensureStream :: EntityName -> StreamId -> StreamStore -> Task _ (DurableChannel (Event Json.Value))
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
  SimpleEventStore ->
  StreamStore ->
  Lock.Lock ->
  InsertionPayload Json.Value ->
  Task Error InsertionSuccess
insertImpl config store insertionLock payload = do
  let insertionsCount = payload.insertions |> Array.length

  Task.when (insertionsCount <= 0) do
    Task.throw (InsertionError EmptyPayload)

  Task.when (insertionsCount > 100) do
    Task.throw (InsertionError PayloadTooLarge)

  let entityName = payload.entityName
  let streamId = payload.streamId
  let expectedPosition =
        payload.insertions
          |> Array.map (\i -> i.metadata.localPosition |> Maybe.withDefault (StreamPosition 0))
          |> Array.first
          |> Maybe.withDefault (StreamPosition 0)
  channel <- store |> ensureStream entityName streamId

  let appendCondition :: Array (Event Json.Value) -> Bool
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

  -- The critical section only covers the append + consistency check plus a
  -- snapshot of the subscribers active at commit time. Subscriber notification
  -- and disk persistence run AFTER the lock is released so a subscriber handler
  -- that reads back from the store (taking globalLock via ensureStream) cannot
  -- deadlock the insert. See #625.
  --
  -- The subscriber snapshot is captured inside the lock so concurrent
  -- subscribe/unsubscribe between commit and dispatch cannot drop an event
  -- (handler removed after commit) or deliver an event older than the
  -- subscription (handler added after commit).
  (result, eventsToNotify, subscribersSnapshot) <-
    Lock.with insertionLock do
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

            let finalEvent = finalEvents |> Array.last |> Maybe.getOrDie
            Log.withScope [("component", "EventStore.Simple")] do
              Log.debug [fmt|Inserted #{toText (Array.length finalEvents)} event(s) for #{toText entityName}/#{toText streamId}|]
                |> Task.ignoreError

            subscribersSnapshot <- ConcurrentVar.peek store.subscriptions
            Task.yield
              ( Ok
                  InsertionSuccess
                    { localPosition = finalEvent.metadata.localPosition |> Maybe.withDefault (StreamPosition 0),
                      globalPosition = finalEvent.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0)
                    },
                finalEvents,
                subscribersSnapshot
              )
          else do
            Log.withScope [("component", "EventStore.Simple")] do
              Log.warn [fmt|Consistency check failed for #{toText entityName}/#{toText streamId}|]
                |> Task.ignoreError
            Task.yield (Err ConsistencyCheckFailed, Array.empty, Map.empty)

  -- Notify subscribers after releasing the lock. Each insert still waits for
  -- its own notifications to drain before returning, preserving ordering: two
  -- sequential inserts deliver in order.
  eventsToNotify
    |> Task.forEach (notifySubscribers subscribersSnapshot)
    |> Task.ignoreError

  -- Persist to disk if persistent mode is enabled. Done outside the lock so a
  -- slow disk write does not block readers and never crosses an async boundary.
  Task.when (config.persistent && not (Array.isEmpty eventsToNotify)) do
    persistEvents config entityName streamId eventsToNotify |> Task.ignoreError

  case result of
    Ok success -> Task.yield success
    Err errorType -> (InsertionError errorType) |> Task.throw


readStreamForwardFromImpl ::
  StreamStore ->
  EntityName ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadStreamMessage Json.Value))
readStreamForwardFromImpl store entityName streamId position (Limit (limit)) = do
  Log.debug [fmt|Reading stream forward: #{toText entityName}/#{toText streamId}|] |> Task.ignoreError
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
  StreamStore ->
  EntityName ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadStreamMessage Json.Value))
readStreamBackwardFromImpl store entityName streamId position (Limit (limit)) = do
  Log.debug [fmt|Reading stream backward: #{toText entityName}/#{toText streamId}|] |> Task.ignoreError
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
  StreamStore ->
  EntityName ->
  StreamId ->
  Task Error (Stream (ReadStreamMessage Json.Value))
readAllStreamEventsImpl store entityName streamId = do
  Log.debug [fmt|Reading all stream events: #{toText entityName}/#{toText streamId}|] |> Task.ignoreError
  channel <- store |> ensureStream entityName streamId
  events <-
    channel
      |> DurableChannel.getAndTransform unchanged
  let items = events |> Array.map StreamEvent
  Stream.fromArray items


readAllEventsForwardFromImpl ::
  StreamStore ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadAllMessage Json.Value))
readAllEventsForwardFromImpl store (StreamPosition (position)) (Limit (limit)) = do
  Log.debug "Reading global event stream forward" |> Task.ignoreError
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  let items =
        allGlobalEvents
          |> Array.drop (fromIntegral position)
          |> Array.take (fromIntegral limit)
          |> Array.map AllEvent
  Stream.fromArray items


readAllEventsBackwardFromImpl ::
  StreamStore ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadAllMessage Json.Value))
readAllEventsBackwardFromImpl store (StreamPosition (position)) (Limit (limit)) = do
  Log.debug "Reading global event stream backward" |> Task.ignoreError
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
  StreamStore ->
  StreamPosition ->
  Limit ->
  Array EntityName ->
  Task Error (Stream (ReadAllMessage Json.Value))
readAllEventsForwardFromFilteredImpl store (StreamPosition (position)) (Limit (limit)) entityNames = do
  Log.debug "Reading global event stream forward (filtered)" |> Task.ignoreError
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
  StreamStore ->
  StreamPosition ->
  Limit ->
  Array EntityName ->
  Task Error (Stream (ReadAllMessage Json.Value))
readAllEventsBackwardFromFilteredImpl store (StreamPosition (position)) (Limit (limit)) entityNames = do
  Log.debug "Reading global event stream backward (filtered)" |> Task.ignoreError
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
  StreamStore ->
  (Event Json.Value -> Task Text Unit) ->
  Task Error SubscriptionId
subscribeToAllEventsImpl store handler = do
  subscriptionId <- generateSubscriptionId
  let subscription = Subscription {subscriptionType = AllEvents, handler}
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock
  Log.debug [fmt|Subscription created: #{toText subscriptionId}|] |> Task.ignoreError
  Task.yield subscriptionId


subscribeToAllEventsFromPositionImpl ::
  StreamStore ->
  StreamPosition ->
  (Event Json.Value -> Task Text Unit) ->
  Task Error SubscriptionId
subscribeToAllEventsFromPositionImpl store fromPosition handler = do
  subscriptionId <- generateSubscriptionId
  coordinator <- newSimpleReplayCoordinator
  let orderedHandler = handleSimpleLiveEvent store handler coordinator
  let subscription = Subscription {subscriptionType = AllEvents, handler = orderedHandler}

  -- Register before replay. The wrapped handler buffers overlap by global
  -- position until the background replay has drained and switched atomically
  -- to live mode.
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock
  Log.debug [fmt|Subscription created: #{toText subscriptionId}|] |> Task.ignoreError

  let replayTask = do
        replayResult <- runSimpleReplay store fromPosition handler coordinator |> Task.asResult
        case replayResult of
          Ok _ -> Task.yield unit
          Err _ ->
            Log.warn "Positional subscription replay failed"
              |> Task.ignoreError
  _replayTask <-
    AsyncTask.run replayTask
      |> Task.mapError StorageFailure
  Task.yield subscriptionId


subscribeToAllEventsFromStartImpl ::
  StreamStore ->
  (Event Json.Value -> Task Text Unit) ->
  Task Error SubscriptionId
subscribeToAllEventsFromStartImpl store handler =
  subscribeToAllEventsFromPositionImpl store (StreamPosition 0) handler


subscribeToEntityEventsImpl ::
  StreamStore ->
  EntityName ->
  (Event Json.Value -> Task Text Unit) ->
  Task Error SubscriptionId
subscribeToEntityEventsImpl store entityName handler = do
  subscriptionId <- generateSubscriptionId
  let subscription = Subscription {subscriptionType = EntityEvents entityName, handler}
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock
  Log.debug [fmt|Subscription created: #{toText subscriptionId}|] |> Task.ignoreError
  Task.yield subscriptionId


subscribeToStreamEventsImpl ::
  StreamStore ->
  EntityName ->
  StreamId ->
  (Event Json.Value -> Task Text Unit) ->
  Task Error SubscriptionId
subscribeToStreamEventsImpl store entityName streamId handler = do
  subscriptionId <- generateSubscriptionId
  let subscription = Subscription {subscriptionType = StreamEvents entityName streamId, handler}
  store.subscriptions
    |> ConcurrentVar.modify (Map.set subscriptionId subscription)
    |> Lock.with store.globalLock
  Log.debug [fmt|Subscription created: #{toText subscriptionId}|] |> Task.ignoreError
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


-- | Dispatch one event to every subscription in the given snapshot.
-- The snapshot must be taken inside the critical section so concurrent
-- subscribe/unsubscribe between commit and dispatch cannot deliver an
-- event to a handler that wasn't active at commit time (or drop one
-- that was).
notifySubscribers ::
  Map SubscriptionId Subscription ->
  Event Json.Value ->
  Task Never Unit
notifySubscribers allSubscriptions event = do
  let relevantSubscriptions =
        allSubscriptions
          |> Map.entries
          |> Array.takeIf (\(_, subscription) -> shouldNotify subscription.subscriptionType event)

  -- Notify all subscribers and wait for completion to ensure ordered delivery
  -- This guarantees that when insert N completes, all its notifications have been
  -- delivered, so insert N+1's notifications will always come after
  let notificationTasks :: Array (Task Text Unit) =
        relevantSubscriptions
          |> Array.map (\(_, subscription) -> notifySubscriber subscription event)
  notificationTasks |> AsyncTask.runAllIgnoringErrors


shouldNotify :: SubscriptionType -> Event Json.Value -> Bool
shouldNotify subscriptionType event =
  case subscriptionType of
    AllEvents -> True
    EntityEvents entityName -> event.entityName == entityName
    StreamEvents entityName streamId -> event.entityName == entityName && event.streamId == streamId


notifySubscriber :: Subscription -> Event Json.Value -> Task Text Unit
notifySubscriber subscription event = do
  -- Execute subscriber handler and catch any errors to prevent failures from affecting the event store
  result <- subscription.handler event |> Task.asResult
  case result of
    Ok _ -> Task.yield unit
    Err err -> do
      Log.warn [fmt|Subscriber notification failed: #{toText err}|]
        |> Task.ignoreError
      Task.yield unit


data SimpleReplayPhase
  = SimpleReplaying
  | SimpleLive
  deriving (Eq, Show)


data SimpleReplayState = SimpleReplayState
  { replayPhase :: SimpleReplayPhase,
    replayHighWater :: Maybe StreamPosition,
    replayInbox :: Map StreamPosition (Event Json.Value),
    replayOverflow :: Bool
  }


newtype SimpleReplayCoordinator = SimpleReplayCoordinator
  { replayState :: ConcurrentVar SimpleReplayState
  }


-- | Maximum overlap retained before rereading from the durable channel.
simpleReplayInboxCapacity :: Int
simpleReplayInboxCapacity = 1000


-- | Allocate ordering state for one positional subscription.
newSimpleReplayCoordinator :: Task w SimpleReplayCoordinator
newSimpleReplayCoordinator = do
  replayState <-
    ConcurrentVar.containing
      SimpleReplayState
        { replayPhase = SimpleReplaying,
          replayHighWater = Nothing,
          replayInbox = Map.empty,
          replayOverflow = False
        }
  Task.yield SimpleReplayCoordinator {replayState}


handleSimpleLiveEvent
  :: StreamStore
  -> (Event Json.Value -> Task Text Unit)
  -> SimpleReplayCoordinator
  -> Event Json.Value
  -> Task Text Unit
handleSimpleLiveEvent _store handler coordinator event = do
  delivery <- coordinator.replayState
    |> ConcurrentVar.modifyReturning \state -> do
        case event.metadata.globalPosition of
          Nothing -> Task.yield (state, Nothing)
          Just position -> do
            let duplicate = case state.replayHighWater of
                  Just highWater -> position <= highWater
                  Nothing -> False
            case duplicate of
              True -> Task.yield (state, Nothing)
              False -> case state.replayPhase of
                SimpleReplaying ->
                  case state.replayOverflow || Map.length state.replayInbox >= simpleReplayInboxCapacity of
                    True -> Task.yield (state {replayOverflow = True}, Nothing)
                    False ->
                      Task.yield
                        ( state {replayInbox = state.replayInbox |> Map.set position event}
                        , Nothing
                        )
                SimpleLive ->
                  Task.yield (state {replayHighWater = Just position}, Just event)
  case delivery of
    Just eventToDeliver -> notifySubscriberSafely handler eventToDeliver
    Nothing -> Task.yield unit


processSimpleReplayEvent
  :: (Event Json.Value -> Task Text Unit)
  -> SimpleReplayCoordinator
  -> Event Json.Value
  -> Task w Unit
processSimpleReplayEvent handler coordinator event = do
  state <- ConcurrentVar.peek coordinator.replayState
  case event.metadata.globalPosition of
    Nothing -> Task.yield unit
    Just position -> do
      let duplicate = case state.replayHighWater of
            Just highWater -> position <= highWater
            Nothing -> False
      case duplicate of
        True -> Task.yield unit
        False -> do
          -- Do not hold the coordinator while user code runs: live callbacks
          -- must remain able to enter the bounded inbox during replay.
          notifySubscriberSafely handler event
          coordinator.replayState
            |> ConcurrentVar.modify
              ( \current ->
                  case current.replayHighWater of
                    Just highWater -> current {replayHighWater = Just (max highWater position)}
                    Nothing -> current {replayHighWater = Just position}
              )


-- | Snapshot events at or after the inclusive global position.
eventsFromPosition :: StreamStore -> StreamPosition -> Task w (Array (Event Json.Value))
eventsFromPosition store (StreamPosition fromPosition) = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  let startIndex = max 0 (fromIntegral fromPosition)
  Task.yield (allGlobalEvents |> Array.drop startIndex)


-- | Atomically begin one overflow-recovery read.
clearSimpleOverflow :: SimpleReplayCoordinator -> Task w (Bool, Maybe StreamPosition)
clearSimpleOverflow coordinator =
  coordinator.replayState
    |> ConcurrentVar.modifyReturning \state ->
        Task.yield
          ( state {replayOverflow = False}
          , (state.replayOverflow, state.replayHighWater)
          )


stabilizeSimpleReplay
  :: StreamStore
  -> (Event Json.Value -> Task Text Unit)
  -> SimpleReplayCoordinator
  -> Task w Unit
stabilizeSimpleReplay store handler coordinator = do
  (overflowed, highWater) <- clearSimpleOverflow coordinator
  case overflowed of
    True -> do
      let catchUpPosition = case highWater of
            Just (StreamPosition position) -> StreamPosition (position + 1)
            Nothing -> StreamPosition 0
      catchUpEvents <- eventsFromPosition store catchUpPosition
      catchUpEvents |> Task.forEach (processSimpleReplayEvent handler coordinator)
      stabilizeSimpleReplay store handler coordinator
    False -> do
      entries <- coordinator.replayState
        |> ConcurrentVar.modifyReturning \state -> do
            let pendingEntries = state.replayInbox |> Map.entries
            case Array.isEmpty pendingEntries of
              True ->
                Task.yield
                  ( state
                      { replayPhase = SimpleLive
                      , replayInbox = Map.empty
                      , replayOverflow = False
                      }
                  , pendingEntries
                  )
              False ->
                Task.yield
                  ( state {replayInbox = Map.empty}
                  , pendingEntries
                  )
      case Array.isEmpty entries of
        True -> Task.yield unit
        False -> do
          entries
            |> Task.forEach (\(_, pendingEvent) -> processSimpleReplayEvent handler coordinator pendingEvent)
          stabilizeSimpleReplay store handler coordinator


runSimpleReplay
  :: StreamStore
  -> StreamPosition
  -> (Event Json.Value -> Task Text Unit)
  -> SimpleReplayCoordinator
  -> Task Text Unit
runSimpleReplay store fromPosition handler coordinator = do
  historicalEvents <- eventsFromPosition store fromPosition
  historicalEvents |> Task.forEach (processSimpleReplayEvent handler coordinator)
  stabilizeSimpleReplay store handler coordinator


notifySubscriberSafely :: (Event Json.Value -> Task Text Unit) -> Event Json.Value -> Task _ Unit
notifySubscriberSafely handler event = do
  -- Execute subscriber handler and catch any errors
  result <- handler event |> Task.asResult
  case result of
    Ok _ -> Task.yield unit
    Err err -> do
      Log.warn [fmt|Subscriber notification failed: #{toText err}|]
        |> Task.ignoreError
      Task.yield unit


truncateStreamImpl :: SimpleEventStore -> StreamStore -> EntityName -> StreamId -> StreamPosition -> Task Error Unit
truncateStreamImpl config store entityName streamId position = do
  let streams = store.streams
  let performTruncation ::
        Map (EntityName, StreamId) (DurableChannel (Event Json.Value)) ->
        Task Never (Map (EntityName, StreamId) (DurableChannel (Event Json.Value)), Maybe Error)
      performTruncation streamMap = do
        let key = (entityName, streamId)
        case streamMap |> Map.get key of
          Nothing -> Task.yield (streamMap, StreamNotFound entityName streamId |> Just)
          Just chan -> do
            chan |> DurableChannel.modify (Array.dropWhile (\p -> p.metadata.localPosition < Just position))
            Task.yield (streamMap, Nothing)

  maybeErr <- streams |> ConcurrentVar.modifyReturning performTruncation
  case maybeErr of
    Nothing -> do
      -- Rewrite JSONL file if persistent
      Task.when config.persistent do
        rewriteStreamFile config store entityName streamId
    Just err -> Task.throw err


-- | Rewrite a stream's JSONL file with only the remaining events after truncation.
rewriteStreamFile :: SimpleEventStore -> StreamStore -> EntityName -> StreamId -> Task _ Unit
rewriteStreamFile config store entityName streamId = do
  channel <- store |> ensureStream entityName streamId
  remainingEvents <- channel |> DurableChannel.getAndTransform unchanged
  writeEventsToFile (\path text -> File.writeText path text |> Task.mapError toText) config entityName streamId remainingEvents
