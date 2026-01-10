{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.EventStore.Core (
  EventStore (..),
  Error (..),
  Limit (..),
  SubscriptionId (..),
  ReadAllMessage (..),
  ReadStreamMessage (..),
  ToxicContents (..),
  EventStoreConfig (..),
  collectAllEvents,
  collectStreamEvents,
  streamMessageToAllMessage,
  castEventStore,
) where

import Array (Array)
import Array qualified
import Basics
import Console qualified
import Json qualified
import Maybe (Maybe (..))
import Maybe qualified
import Result (Result (..))
import Service.Event (
  EntityName,
  Event (..),
  Insertion (..),
  InsertionFailure,
  InsertionPayload (..),
  InsertionSuccess,
  StreamPosition (..),
 )
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.StreamId (StreamId)
import Stream (Stream)
import Stream qualified
import Task (Task)
import Text (Text)


newtype Limit = Limit Int64
  deriving (Eq, Show, Ord, Generic)


newtype SubscriptionId = SubscriptionId Text
  deriving (Eq, Show, Ord, Generic)


data Error
  = StreamNotFound EntityName StreamId
  | EventNotFound StreamId StreamPosition
  | StorageFailure Text -- Generic storage errors with message
  | SubscriptionNotFound SubscriptionId
  | SubscriptionError SubscriptionId Text -- Subscription-specific errors
  | TruncationError EntityName StreamId StreamPosition Text -- Stream truncation errors with context
  | InsertionError InsertionFailure
  | ReadingAllError Text
  deriving (Eq, Show)


data ToxicContents = ToxicContents
  { locator :: Text,
    metadata :: Json.Value,
    globalPosition :: StreamPosition,
    localPosition :: StreamPosition,
    additionalInfo :: Text
  }
  deriving (Eq, Show)


data ReadAllMessage eventType
  = ReadingStarted
  | AllEvent (Event eventType)
  | ToxicAllEvent ToxicContents
  | Checkpoint StreamPosition
  | Terminated Text
  | CaughtUp
  | FellBehind
  deriving (Eq, Show)


data ReadStreamMessage eventType
  = StreamReadingStarted
  | StreamEvent (Event eventType)
  | ToxicStreamEvent ToxicContents
  | StreamCheckpoint StreamPosition
  | StreamTerminated Text
  | StreamCaughtUp
  | StreamFellBehind
  deriving (Eq, Show, Generic)


streamMessageToAllMessage :: ReadStreamMessage eventType -> ReadAllMessage eventType
streamMessageToAllMessage message =
  case message of
    StreamReadingStarted -> ReadingStarted
    StreamEvent event -> AllEvent event
    ToxicStreamEvent contents -> ToxicAllEvent contents
    StreamCheckpoint position -> Checkpoint position
    StreamTerminated reason -> Terminated reason
    StreamCaughtUp -> CaughtUp
    StreamFellBehind -> FellBehind


collectAllEvents :: Array (ReadAllMessage eventType) -> Array (Event eventType)
collectAllEvents messages = do
  let extractEvent message =
        case message of
          AllEvent evt -> Array.wrap evt
          _ -> Array.empty
  messages |> Array.flatMap extractEvent


collectStreamEvents :: Array (ReadStreamMessage eventType) -> Array (Event eventType)
collectStreamEvents messages = do
  let extractEvent message =
        case message of
          StreamEvent evt -> Array.wrap evt
          _ -> Array.empty
  messages |> Array.flatMap extractEvent


-- | An interface for the operations of an event store
data EventStore eventType = EventStore
  { -- | Insert an event in a stream
    --   Returns the local and global stream position if successful, or an error on conflict or failure.
    insert :: InsertionPayload eventType -> Task Error InsertionSuccess,
    -- | Read events from a stream in forward direction starting from a given revision.
    --   Returns a stream of ReadStreamMessage or an Error.
    readStreamForwardFrom ::
      EntityName -> StreamId -> StreamPosition -> Limit -> Task Error (Stream (ReadStreamMessage eventType)),
    -- | Read events from a stream in backward direction starting from a given revision.
    --   Useful for looking at recent events.
    readStreamBackwardFrom ::
      EntityName -> StreamId -> StreamPosition -> Limit -> Task Error (Stream (ReadStreamMessage eventType)),
    -- | Read all events from a specific stream for a given entity.
    --   Returns a stream of ReadStreamMessage or an Error.
    readAllStreamEvents :: EntityName -> StreamId -> Task Error (Stream (ReadStreamMessage eventType)),
    -- | Read events from the global stream in forward direction starting from a given global position.
    --   This reads across all streams and entities.
    readAllEventsForwardFrom :: StreamPosition -> Limit -> Task Error (Stream (ReadAllMessage eventType)),
    -- | Read events from the global stream in backward direction starting from a given global position.
    --   This reads across all streams and entities in reverse order.
    readAllEventsBackwardFrom :: StreamPosition -> Limit -> Task Error (Stream (ReadAllMessage eventType)),
    -- | Read events from the global stream in forward direction starting from a given global position,
    --   but only for specific entities. Useful for event sourcing projections that only care about certain entities.
    readAllEventsForwardFromFiltered ::
      StreamPosition -> Limit -> Array EntityName -> Task Error (Stream (ReadAllMessage eventType)),
    -- | Read events from the global stream in backward direction starting from a given global position,
    --   but only for specific entities. Useful for filtered historical analysis and debugging.
    readAllEventsBackwardFromFiltered ::
      StreamPosition -> Limit -> Array EntityName -> Task Error (Stream (ReadAllMessage eventType)),
    -- | Subscribe to all events in the event store. The subscriber function will be called
    --   for every event that gets appended. Returns a SubscriptionId for unsubscribing.
    subscribeToAllEvents :: ((Event eventType) -> Task Text Unit) -> Task Error SubscriptionId,
    -- | Subscribe to all events from a specific global position onwards. This allows catching up
    --   from historical events and then receiving new events as they are appended.
    subscribeToAllEventsFromPosition ::
      StreamPosition -> ((Event eventType) -> Task Text Unit) -> Task Error SubscriptionId,
    -- | Subscribe to all events from the very beginning of the event store. This delivers ALL
    --   historical events first, then continues with new events as they are appended.
    subscribeToAllEventsFromStart :: ((Event eventType) -> Task Text Unit) -> Task Error SubscriptionId,
    -- | Subscribe to events for a specific entity. The subscriber function will be called
    --   only for events belonging to the specified entity.
    subscribeToEntityEvents :: EntityName -> ((Event eventType) -> Task Text Unit) -> Task Error SubscriptionId,
    -- | Subscribe to events for a specific stream within an entity. The subscriber function
    --   will be called only for events in the specified entity and stream.
    subscribeToStreamEvents ::
      EntityName -> StreamId -> ((Event eventType) -> Task Text Unit) -> Task Error SubscriptionId,
    -- | Unsubscribe from event notifications. After this call, the subscriber function
    --   will no longer be called for new events.
    unsubscribe :: SubscriptionId -> Task Error Unit,
    -- | Removes all the events up to a position
    truncateStream :: EntityName -> StreamId -> StreamPosition -> Task Error Unit
  }


class EventStoreConfig config where
  createEventStore :: config -> Task Text (EventStore Json.Value)


-- | Wrap an EventStore Json.Value with typed serialization/deserialization.
-- This allows multiple services with different event types to share a single EventStore.
castEventStore ::
  forall eventType.
  (Json.FromJSON eventType, Json.ToJSON eventType) =>
  EventStore Json.Value ->
  EventStore eventType
castEventStore rawStore =
  EventStore
    { insert = \payload -> do
        let rawPayload = encodeInsertionPayload payload
        rawStore.insert rawPayload,
      readStreamForwardFrom = \entityName streamId position limit -> do
        rawStream <- rawStore.readStreamForwardFrom entityName streamId position limit
        rawStream |> Stream.mapStream decodeReadStreamMessage,
      readStreamBackwardFrom = \entityName streamId position limit -> do
        rawStream <- rawStore.readStreamBackwardFrom entityName streamId position limit
        rawStream |> Stream.mapStream decodeReadStreamMessage,
      readAllStreamEvents = \entityName streamId -> do
        rawStream <- rawStore.readAllStreamEvents entityName streamId
        rawStream |> Stream.mapStream decodeReadStreamMessage,
      readAllEventsForwardFrom = \position limit -> do
        rawStream <- rawStore.readAllEventsForwardFrom position limit
        rawStream |> Stream.mapStream decodeReadAllMessage,
      readAllEventsBackwardFrom = \position limit -> do
        rawStream <- rawStore.readAllEventsBackwardFrom position limit
        rawStream |> Stream.mapStream decodeReadAllMessage,
      readAllEventsForwardFromFiltered = \position limit entityNames -> do
        rawStream <- rawStore.readAllEventsForwardFromFiltered position limit entityNames
        rawStream |> Stream.mapStream decodeReadAllMessage,
      readAllEventsBackwardFromFiltered = \position limit entityNames -> do
        rawStream <- rawStore.readAllEventsBackwardFromFiltered position limit entityNames
        rawStream |> Stream.mapStream decodeReadAllMessage,
      subscribeToAllEvents = \callback -> do
        rawStore.subscribeToAllEvents (handleSubscriptionRawEvent callback),
      subscribeToAllEventsFromPosition = \position callback -> do
        rawStore.subscribeToAllEventsFromPosition position (handleSubscriptionRawEvent callback),
      subscribeToAllEventsFromStart = \callback -> do
        rawStore.subscribeToAllEventsFromStart (handleSubscriptionRawEvent callback),
      subscribeToEntityEvents = \entityName callback -> do
        rawStore.subscribeToEntityEvents entityName (handleSubscriptionRawEvent callback),
      subscribeToStreamEvents = \entityName streamId callback -> do
        rawStore.subscribeToStreamEvents entityName streamId (handleSubscriptionRawEvent callback),
      unsubscribe = rawStore.unsubscribe,
      truncateStream = rawStore.truncateStream
    }
 where
  encodeInsertionPayload :: InsertionPayload eventType -> InsertionPayload Json.Value
  encodeInsertionPayload payload =
    InsertionPayload
      { streamId = payload.streamId,
        entityName = payload.entityName,
        insertionType = payload.insertionType,
        insertions = payload.insertions |> Array.map encodeInsertion
      }

  encodeInsertion :: Insertion eventType -> Insertion Json.Value
  encodeInsertion insertion =
    Insertion
      { id = insertion.id,
        event = Json.encode insertion.event,
        metadata = insertion.metadata
      }

  decodeEvent :: Event Json.Value -> Maybe (Event eventType)
  decodeEvent rawEvent =
    case decodeEventData rawEvent.event of
      Just decoded ->
        Just
          Event
            { entityName = rawEvent.entityName,
              streamId = rawEvent.streamId,
              event = decoded,
              metadata = rawEvent.metadata
            }
      Nothing -> Nothing

  decodeEventData :: Json.Value -> Maybe eventType
  decodeEventData jsonValue =
    case Json.decode jsonValue of
      Ok decoded -> Just decoded
      Err _ -> Nothing

  decodeReadAllMessage :: ReadAllMessage Json.Value -> ReadAllMessage eventType
  decodeReadAllMessage message =
    case message of
      ReadingStarted -> ReadingStarted
      AllEvent rawEvent ->
        case decodeEvent rawEvent of
          Just decoded -> AllEvent decoded
          Nothing -> ToxicAllEvent (rawEventToToxicContents rawEvent)
      ToxicAllEvent contents -> ToxicAllEvent contents
      Checkpoint position -> Checkpoint position
      Terminated reason -> Terminated reason
      CaughtUp -> CaughtUp
      FellBehind -> FellBehind

  decodeReadStreamMessage :: ReadStreamMessage Json.Value -> ReadStreamMessage eventType
  decodeReadStreamMessage message =
    case message of
      StreamReadingStarted -> StreamReadingStarted
      StreamEvent rawEvent ->
        case decodeEvent rawEvent of
          Just decoded -> StreamEvent decoded
          Nothing -> ToxicStreamEvent (rawEventToToxicContents rawEvent)
      ToxicStreamEvent contents -> ToxicStreamEvent contents
      StreamCheckpoint position -> StreamCheckpoint position
      StreamTerminated reason -> StreamTerminated reason
      StreamCaughtUp -> StreamCaughtUp
      StreamFellBehind -> StreamFellBehind

  rawEventToToxicContents :: Event Json.Value -> ToxicContents
  rawEventToToxicContents rawEvent =
    ToxicContents
      { locator = [fmt|{rawEvent.entityName}/{rawEvent.streamId}|],
        metadata = Json.encode rawEvent.metadata,
        globalPosition = rawEvent.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0),
        localPosition = rawEvent.metadata.localPosition |> Maybe.withDefault (StreamPosition 0),
        additionalInfo = [fmt|Failed to decode event data in castEventStore|]
      }

  handleSubscriptionRawEvent ::
    ((Event eventType) -> Task Text Unit) ->
    Event Json.Value ->
    Task Text Unit
  handleSubscriptionRawEvent callback rawEvent =
    case decodeEvent rawEvent of
      Just typedEvent -> callback typedEvent
      Nothing -> do
        let entity = rawEvent.entityName
        let stream = rawEvent.streamId
        let globalPos = rawEvent.metadata.globalPosition |> Maybe.withDefault (StreamPosition 0)
        let localPos = rawEvent.metadata.localPosition |> Maybe.withDefault (StreamPosition 0)
        let eventPayload = Json.encodeText rawEvent.event
        Console.print
          [fmt|[EventStore] Warning: Failed to decode subscription event
  entityName: #{entity}
  streamId: #{stream}
  globalPosition: #{globalPos}
  localPosition: #{localPos}
  payload: #{eventPayload}|]