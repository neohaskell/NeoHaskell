module Service.EventStore.Postgres.Internal (
  Config (..),
  Ops (..),
  new,
  defaultOps,
  Sessions.Connection (..),
) where

import Array qualified
import AsyncTask qualified
import Core
import Default ()
import Hasql.Connection qualified as Hasql
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Session qualified as Session
import Json qualified
import Maybe qualified
import Service.Event
import Service.Event.EntityName qualified as EntityName
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core
import Service.EventStore.Postgres.Internal.Sessions qualified as Sessions
import Stream (Stream)
import Stream qualified
import Task qualified
import Text qualified


data Config = Config
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text,
    port :: Int
  }
  deriving (Eq, Ord, Show)


toConnectionSettings :: Config -> LinkedList ConnectionSetting.Setting
toConnectionSettings cfg = do
  let params =
        ConnectionSettingConnection.params
          [ Param.host cfg.host,
            Param.port (fromIntegral cfg.port),
            Param.dbname cfg.databaseName,
            Param.user cfg.user,
            Param.password cfg.password
          ]
  [params |> ConnectionSetting.connection]


data Ops = Ops
  { acquire :: Config -> Task Text Sessions.Connection,
    initializeTable :: Sessions.Connection -> Task Text Unit
  }


defaultOps :: Ops
defaultOps = do
  let acquire cfg =
        toConnectionSettings cfg
          |> Hasql.acquire
          |> Task.fromIOEither
          |> Task.mapError toText
          |> Task.map Sessions.Connection

  let initializeTable connection = do
        res <-
          Sessions.createEventsTableSession
            |> Sessions.run connection
            |> Task.mapError toText
            |> Task.asResult
        case res of
          Ok _ -> Task.yield unit
          Err err ->
            if err |> Text.contains "\"2714\""
              then Task.yield unit
              else Task.throw err

  Ops {acquire, initializeTable}


new ::
  (Json.Encodable eventType) =>
  Ops ->
  Config ->
  Task Text (EventStore eventType)
new ops cfg = do
  connection <- ops.acquire cfg
  ops.initializeTable connection
  let eventStore =
        EventStore
          { insert = insertImpl ops cfg 0,
            readStreamForwardFrom = readStreamForwardFromImpl,
            readStreamBackwardFrom = readStreamBackwardFromImpl,
            readAllStreamEvents = readAllStreamEventsImpl,
            readAllEventsForwardFrom = readAllEventsForwardFromImpl,
            readAllEventsBackwardFrom = readAllEventsBackwardFromImpl,
            readAllEventsForwardFromFiltered = readAllEventsForwardFromFilteredImpl,
            readAllEventsBackwardFromFiltered = readAllEventsBackwardFromFilteredImpl,
            subscribeToAllEvents = subscribeToAllEventsImpl,
            subscribeToAllEventsFromPosition = subscribeToAllEventsFromPositionImpl,
            subscribeToAllEventsFromStart = subscribeToAllEventsFromStartImpl,
            subscribeToEntityEvents = subscribeToEntityEventsImpl,
            subscribeToStreamEvents = subscribeToStreamEventsImpl,
            unsubscribe = unsubscribeImpl,
            truncateStream = truncateStreamImpl
          }
  Task.yield eventStore


data PostgresStoreError
  = SessionError Session.SessionError
  | ConnectionAcquisitionError Text
  | CoreInsertionError InsertionFailure


insertImpl ::
  (Json.Encodable eventType) =>
  Ops ->
  Config ->
  Int ->
  InsertionPayload eventType ->
  Task Error InsertionSuccess
insertImpl ops cfg consistencyRetryCount payload = do
  res <- insertGo ops cfg payload |> Task.asResult
  case res of
    Ok success ->
      Task.yield success
    Err (ConnectionAcquisitionError err) ->
      Task.throw (StorageFailure err)
    Err (CoreInsertionError err) ->
      Task.throw (InsertionError err)
    Err (SessionError err) -> do
      let isEventsUniqueKeyViolation err =
            (err |> toText |> Text.contains "\"2627\"")
              && (err |> toText |> Text.toLower |> Text.contains "uk_events_stream")
      if (err |> isEventsUniqueKeyViolation) && (payload.insertionType != AnyStreamState)
        then
          Task.throw (InsertionError ConsistencyCheckFailed)
        else
          if (err |> isEventsUniqueKeyViolation) && (payload.insertionType == AnyStreamState) && (consistencyRetryCount < 100)
            then do
              AsyncTask.sleep (consistencyRetryCount + 1)
              insertImpl ops cfg (consistencyRetryCount + 1) payload
            else
              Task.throw (InsertionError (InsertionFailed "Insertion failed after 100 retries"))


insertGo ::
  (Json.Encodable eventType) =>
  Ops ->
  Config ->
  InsertionPayload eventType ->
  Task PostgresStoreError InsertionSuccess
insertGo ops cfg payload = do
  conn <- ops.acquire cfg |> Task.mapError ConnectionAcquisitionError

  let payloadEventIds =
        payload.insertions
          |> Array.map (\i -> i.metadata.eventId)

  alreadyExistingIds <-
    Sessions.selectExistingIdsSession payloadEventIds
      |> Sessions.run conn
      |> Task.mapError SessionError

  let insertions =
        payload.insertions
          |> Array.dropIf
            ( \i ->
                alreadyExistingIds
                  |> Array.contains i.metadata.eventId
            )

  let insertionsCount = insertions |> Array.length

  if insertionsCount > 100
    then Task.throw (CoreInsertionError PayloadTooLarge)
    else pass

  latestPositions <-
    Sessions.selectLatestEventInStream payload.entityName payload.streamId
      |> Sessions.run conn
      |> Task.mapError SessionError

  if insertionsCount <= 0
    then do
      let (globalPosition, localPosition) =
            latestPositions |> Maybe.withDefault (StreamPosition 0, StreamPosition 0)
      Task.yield InsertionSuccess {localPosition, globalPosition}
    else do
      let offset =
            case payload.insertionType of
              InsertAfter (StreamPosition pos) -> pos + 1
              _ ->
                latestPositions
                  |> Maybe.map (\(_, StreamPosition localPos) -> localPos + 1)
                  |> Maybe.withDefault 0
      let insertionRecords =
            insertions |> Array.indexed |> Array.map \(idx, i) -> do
              Sessions.EventInsertionRecord
                { eventId = i.metadata.eventId,
                  localPosition = offset + fromIntegral idx,
                  inlinedStreamId = payload.streamId |> StreamId.toText,
                  entity = payload.entityName |> EntityName.toText,
                  eventData = Json.encode i.event,
                  metadata = Json.encode i.metadata
                }
      Sessions.insertRecordsIntoStream insertionRecords
        |> Sessions.run conn
        |> Task.mapError SessionError

      case insertionRecords |> Array.last of
        Nothing ->
          "The impossible happened: no insertions were available during insertion"
            |> InsertionFailed
            |> CoreInsertionError
            |> Task.throw
        Just lastInsertion -> do
          lastEventPositions <-
            Sessions.selectInsertedEvent (lastInsertion.eventId)
              |> Sessions.run conn
              |> Task.mapError SessionError

          let (globalPosition, localPosition) =
                lastEventPositions
                  |> Maybe.withDefault (StreamPosition 0, StreamPosition 0)

          Task.yield (InsertionSuccess {localPosition, globalPosition})


readStreamForwardFromImpl :: EntityName -> StreamId -> StreamPosition -> Limit -> Task Error (Stream (Event eventType))
readStreamForwardFromImpl _ _ _ _ = do
  _ <- panic "Postgres.readStreamForwardFromImpl - Not implemented yet"
  Stream.fromArray Array.empty


readStreamBackwardFromImpl :: EntityName -> StreamId -> StreamPosition -> Limit -> Task Error (Stream (Event eventType))
readStreamBackwardFromImpl _ _ _ _ = do
  _ <- panic "Postgres.readStreamBackwardFromImpl - Not implemented yet"
  Stream.fromArray Array.empty


readAllStreamEventsImpl :: EntityName -> StreamId -> Task Error (Stream (Event eventType))
readAllStreamEventsImpl _ _ = do
  _ <- panic "Postgres.readAllStreamEventsImpl - Not implemented yet"
  Stream.fromArray Array.empty


readAllEventsForwardFromImpl :: StreamPosition -> Limit -> Task Error (Stream (Event eventType))
readAllEventsForwardFromImpl _ _ = do
  _ <- panic "Postgres.readAllEventsForwardFromImpl - Not implemented yet"
  Stream.fromArray Array.empty


readAllEventsBackwardFromImpl :: StreamPosition -> Limit -> Task Error (Stream (Event eventType))
readAllEventsBackwardFromImpl _ _ = do
  _ <- panic "Postgres.readAllEventsBackwardFromImpl - Not implemented yet"
  Stream.fromArray Array.empty


-- data RelativePosition
--   = Start
--   | End
--   deriving (Eq, Show, Ord, Generic)

-- data ReadDirection
--   = Forwards
--   | Backwards
--   deriving (Eq, Show, Ord, Generic)

-- toPostgresDirection :: Maybe RelativePosition -> Maybe ReadDirection -> Text
-- toPostgresDirection pos dir = do
--   case (pos, dir) of
--     (Just Start, _) -> "ASC"
--     (Just End, _) -> "DESC"
--     (Nothing, Just Forwards) -> "ASC"
--     (Nothing, Just Backwards) -> "DESC"
--     (Nothing, Nothing) -> "ASC"

-- toPostgresGlobalPositionComparison :: Maybe ReadDirection -> Text
-- toPostgresGlobalPositionComparison dir =
--   case dir of
--     Just Backwards -> "<"
--     _ -> ">="

-- toPostgresPosition :: Maybe RelativePosition -> Maybe ReadDirection -> Int64
-- toPostgresPosition pos dir =
--   case (pos, dir) of
--     (Just Start, _) -> 0
--     (Just End, _) -> maxValue
--     (Nothing, Just Forwards) -> 0
--     (Nothing, Just Backwards) -> maxValue
--     (Nothing, Nothing) -> 0

-- toPostgresEntityFilters :: Maybe (Array EntityName) -> Text
-- toPostgresEntityFilters maybeEntityNames = do
--   let entityNames = maybeEntityNames |> Maybe.withDefault Array.empty
--   let entityNamesText = entityNames |> Array.map EntityName.toText |> Text.joinWith ", "
--   if (entityNames |> Array.length) == 0
--     then ""
--     else [fmt| AND Entity = ANY (#{entityNamesText})|]

-- performReadAllStreamEvents ::
--   Ops ->
--   Config ->
--   Maybe RelativePosition ->
--   Maybe ReadDirection ->
--   Maybe (Array EntityName) ->
--   StreamId ->
--   Task PostgresStoreError (Array (Event eventType))
-- performReadAllStreamEvents ops cfg relative readDirection entityNames streamId = do
--   conn <- ops.acquire cfg |> Task.mapError ConnectionAcquisitionError
--   let direction = toPostgresDirection relative readDirection
--   let positionFilter = toPostgresGlobalPositionComparison readDirection
--   let position = toPostgresPosition relative readDirection
--   let entityFilters = toPostgresEntityFilters entityNames
--   Task.yield (panic "performReadAllStreamEvents: Not implemented yet")

readAllEventsForwardFromFilteredImpl ::
  StreamPosition -> Limit -> Array EntityName -> Task Error (Stream (Event eventType))
readAllEventsForwardFromFilteredImpl _ _ _ = do
  _ <- panic "Postgres.readAllEventsForwardFromFilteredImpl - Not implemented yet"
  Stream.fromArray Array.empty


readAllEventsBackwardFromFilteredImpl ::
  StreamPosition -> Limit -> Array EntityName -> Task Error (Stream (Event eventType))
readAllEventsBackwardFromFilteredImpl _ _ _ = do
  _ <- panic "Postgres.readAllEventsBackwardFromFilteredImpl - Not implemented yet"
  Stream.fromArray Array.empty


subscribeToAllEventsImpl :: (Event eventType -> Task Error Unit) -> Task Error SubscriptionId
subscribeToAllEventsImpl _ = panic "Postgres.subscribeToAllEventsImpl - Not implemented yet" |> Task.yield


subscribeToAllEventsFromPositionImpl ::
  StreamPosition -> (Event eventType -> Task Error Unit) -> Task Error SubscriptionId
subscribeToAllEventsFromPositionImpl _ _ = panic "Postgres.subscribeToAllEventsFromPositionImpl - Not implemented yet" |> Task.yield


subscribeToAllEventsFromStartImpl :: (Event eventType -> Task Error Unit) -> Task Error SubscriptionId
subscribeToAllEventsFromStartImpl _ = panic "Postgres.subscribeToAllEventsFromStartImpl - Not implemented yet" |> Task.yield


subscribeToEntityEventsImpl :: EntityName -> (Event eventType -> Task Error Unit) -> Task Error SubscriptionId
subscribeToEntityEventsImpl _ _ = panic "Postgres.subscribeToEntityEventsImpl - Not implemented yet" |> Task.yield


subscribeToStreamEventsImpl ::
  EntityName -> StreamId -> (Event eventType -> Task Error Unit) -> Task Error SubscriptionId
subscribeToStreamEventsImpl _ _ _ = panic "Postgres.subscribeToStreamEventsImpl - Not implemented yet" |> Task.yield


unsubscribeImpl :: SubscriptionId -> Task Error Unit
unsubscribeImpl _ = panic "Postgres.unsubscribeImpl - Not implemented yet" |> Task.yield


truncateStreamImpl :: EntityName -> StreamId -> StreamPosition -> Task Error Unit
truncateStreamImpl _ _ _ = panic "Postgres.truncateStreamImpl - Not implemented yet" |> Task.yield
