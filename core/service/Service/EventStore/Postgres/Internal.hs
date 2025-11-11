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
import Json qualified
import Maybe qualified
import Result qualified
import Service.Event
import Service.Event.EntityName qualified as EntityName
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core
import Service.EventStore.Postgres.Internal.Core
import Service.EventStore.Postgres.Internal.PostgresEventRecord (PostgresEventRecord (..))
import Service.EventStore.Postgres.Internal.Sessions qualified as Sessions
import Stream (Stream)
import Stream qualified
import Task qualified
import Text qualified
import Var qualified


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
  ( Json.Encodable eventType,
    Json.Decodable eventType
  ) =>
  Ops ->
  Config ->
  Task Text (EventStore eventType)
new ops cfg = do
  connection <- ops.acquire cfg
  ops.initializeTable connection
  let eventStore =
        EventStore
          { insert = insertImpl ops cfg 0,
            readStreamForwardFrom = readStreamForwardFromImpl ops cfg,
            readStreamBackwardFrom = readStreamBackwardFromImpl ops cfg,
            readAllStreamEvents = readAllStreamEventsImpl ops cfg,
            readAllEventsForwardFrom = readAllEventsForwardFromImpl ops cfg,
            readAllEventsBackwardFrom = readAllEventsBackwardFromImpl ops cfg,
            readAllEventsForwardFromFiltered = readAllEventsForwardFromFilteredImpl ops cfg,
            readAllEventsBackwardFromFiltered = readAllEventsBackwardFromFilteredImpl ops cfg,
            subscribeToAllEvents = subscribeToAllEventsImpl,
            subscribeToAllEventsFromPosition = subscribeToAllEventsFromPositionImpl,
            subscribeToAllEventsFromStart = subscribeToAllEventsFromStartImpl,
            subscribeToEntityEvents = subscribeToEntityEventsImpl,
            subscribeToStreamEvents = subscribeToStreamEventsImpl,
            unsubscribe = unsubscribeImpl,
            truncateStream = truncateStreamImpl
          }
  Task.yield eventStore


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
  Task.when (payload.insertions |> Array.isEmpty) do
    Task.throw (CoreInsertionError EmptyPayload)

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


readStreamForwardFromImpl ::
  (Json.Decodable eventType) =>
  Ops ->
  Config ->
  EntityName ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadStreamMessage eventType))
readStreamForwardFromImpl ops cfg entityName streamId streamPosition limit = do
  readingRefs <- newReadingRefs
  stream <- Stream.new
  let relative = FromAndAfter streamPosition |> Just
  let readDirection = Just Forwards

  performReadStreamEvents ops cfg stream readingRefs entityName streamId limit relative readDirection
    |> Task.mapError (toText .> StorageFailure)


readStreamBackwardFromImpl ::
  (Json.Decodable eventType) =>
  Ops ->
  Config ->
  EntityName ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadStreamMessage eventType))
readStreamBackwardFromImpl ops cfg entityName streamId streamPosition limit = do
  readingRefs <- newReadingRefs
  stream <- Stream.new
  let relative = Before streamPosition |> Just
  let readDirection = Just Backwards

  performReadStreamEvents ops cfg stream readingRefs entityName streamId limit relative readDirection
    |> Task.mapError (toText .> StorageFailure)


readAllStreamEventsImpl ::
  (Json.Decodable eventType) =>
  Ops ->
  Config ->
  EntityName ->
  StreamId ->
  Task Error (Stream (ReadStreamMessage eventType))
readAllStreamEventsImpl ops cfg entityName streamId = do
  readingRefs <- newReadingRefs
  stream <- Stream.new
  let limit = Limit maxValue  -- Read all events
  let relative = Just Start
  let readDirection = Just Forwards

  performReadStreamEvents ops cfg stream readingRefs entityName streamId limit relative readDirection
    |> Task.mapError (toText .> StorageFailure)


readAllEventsForwardFromImpl ::
  (Json.Decodable eventType) =>
  Ops ->
  Config ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadAllMessage eventType))
readAllEventsForwardFromImpl ops config streamPosition limit = do
  readingRefs <- newReadingRefs
  stream <- Stream.new
  -- FIXME: pass relative properly
  let relative = FromAndAfter streamPosition |> Just
  let readDirection = Just Forwards
  performReadAllStreamEvents ops config stream readingRefs limit relative readDirection Nothing
    |> Task.mapError (toText .> ReadingAllError)


readAllEventsBackwardFromImpl ::
  (Json.Decodable eventType) =>
  Ops ->
  Config ->
  StreamPosition ->
  Limit ->
  Task Error (Stream (ReadAllMessage eventType))
readAllEventsBackwardFromImpl ops config streamPosition limit = do
  readingRefs <- newReadingRefs
  stream <- Stream.new
  -- FIXME: pass relative properly
  let relative = Before streamPosition |> Just
  let readDirection = Just Backwards
  performReadAllStreamEvents ops config stream readingRefs limit relative readDirection Nothing
    |> Task.mapError (toText .> ReadingAllError)


readAllEventsForwardFromFilteredImpl ::
  (Json.Decodable eventType) =>
  Ops ->
  Config ->
  StreamPosition ->
  Limit ->
  Array EntityName ->
  Task Error (Stream (ReadAllMessage eventType))
readAllEventsForwardFromFilteredImpl ops config streamPosition limit entityNames = do
  readingRefs <- newReadingRefs
  stream <- Stream.new
  -- FIXME: pass relative properly
  let relative = FromAndAfter streamPosition |> Just
  let readDirection = Just Forwards
  performReadAllStreamEvents ops config stream readingRefs limit relative readDirection (Just entityNames)
    |> Task.mapError (toText .> ReadingAllError)


readAllEventsBackwardFromFilteredImpl ::
  (Json.Decodable eventType) =>
  Ops ->
  Config ->
  StreamPosition ->
  Limit ->
  Array EntityName ->
  Task Error (Stream (ReadAllMessage eventType))
readAllEventsBackwardFromFilteredImpl ops config streamPosition limit entityNames = do
  readingRefs <- newReadingRefs
  stream <- Stream.new
  -- FIXME: pass relative properly
  let relative = Before streamPosition |> Just
  let readDirection = Just Backwards
  performReadAllStreamEvents ops config stream readingRefs limit relative readDirection (Just entityNames)
    |> Task.mapError (toText .> ReadingAllError)


data ReadingRefs = ReadingRefs
  { notifiedReadingRef :: Var Bool,
    cancellationRef :: Var Bool
  }


newReadingRefs :: Task Error ReadingRefs
newReadingRefs = do
  notifiedReadingRef <- Var.new False
  cancellationRef <- Var.new False
  Task.yield ReadingRefs {notifiedReadingRef, cancellationRef}


performReadAllStreamEvents ::
  forall eventType.
  (Json.Decodable eventType) =>
  Ops ->
  Config ->
  Stream (ReadAllMessage eventType) ->
  ReadingRefs ->
  Limit ->
  Maybe RelativePosition ->
  Maybe ReadDirection ->
  Maybe (Array EntityName) ->
  Task PostgresStoreError (Stream (ReadAllMessage eventType))
performReadAllStreamEvents
  ops
  cfg
  stream
  (ReadingRefs {notifiedReadingRef, cancellationRef})
  (Limit limit)
  relative
  readDirection
  entityNames = do
    conn <- ops.acquire cfg |> Task.mapError ConnectionAcquisitionError

    breakLoopRef <- Var.new False
    remainingLimitRef <- Var.new limit
    let shouldKeepReading = do
          shouldBreak <- Var.get breakLoopRef
          isCancelled <- Var.get cancellationRef
          remainingLimit <- Var.get remainingLimitRef
          Task.yield (not shouldBreak && not isCancelled && remainingLimit > 0)

    positionRef <- Var.new (toPostgresPosition relative readDirection)

    Task.while (shouldKeepReading) do
      selectSession <- Sessions.selectEventBatch positionRef relative readDirection entityNames
      records <-
        selectSession
          |> Sessions.run conn
          |> Task.mapError
            SessionError

      hasNotifiedReadingStarted <- Var.get notifiedReadingRef
      Task.unless (hasNotifiedReadingStarted) do
        stream |> Stream.writeItem ReadingStarted
        notifiedReadingRef |> Var.set True

      Task.when (records |> Array.isEmpty) do
        breakLoopRef |> Var.set True

      records |> Task.forEach \record -> do
        let evt :: Result Text (ReadAllMessage eventType) = do
              event <- Json.decode record.eventData
              m <- Json.decode record.metadata
              let streamId = record.inlinedStreamId |> StreamId.fromText
              let metadata =
                    m
                      { EventMetadata.globalPosition = Just (StreamPosition record.globalPosition)
                      }
              Event
                { entityName = record.entityName |> EntityName,
                  streamId,
                  event,
                  metadata
                }
                |> AllEvent
                |> Result.Ok
        remainingLimit <- Var.get remainingLimitRef
        if remainingLimit <= 0
          then
            pass
          else do
            case evt of
              Ok goodEvent -> do
                stream |> Stream.writeItem goodEvent
              Err err -> do
                let entityName = record.entityName
                let streamId = record.inlinedStreamId
                let locator = [fmt|#{entityName}#{streamId}|]
                let toxicEvt =
                      ToxicContents
                        { locator = locator,
                          metadata = record.metadata,
                          globalPosition = (StreamPosition record.globalPosition),
                          localPosition = (StreamPosition record.localPosition),
                          additionalInfo = err
                        }
                        |> ToxicAllEvent
                stream |> Stream.writeItem toxicEvt
                positionRef |> Var.set record.globalPosition
            Var.decrement remainingLimitRef

      Task.when (Array.length records < batchSize) do
        breakLoopRef |> Var.set True

    stream |> Stream.end
    Task.yield stream


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


performReadStreamEvents ::
  forall eventType.
  (Json.Decodable eventType) =>
  Ops ->
  Config ->
  Stream (ReadStreamMessage eventType) ->
  ReadingRefs ->
  EntityName ->
  StreamId ->
  Limit ->
  Maybe RelativePosition ->
  Maybe ReadDirection ->
  Task PostgresStoreError (Stream (ReadStreamMessage eventType))
performReadStreamEvents
  ops
  cfg
  stream
  (ReadingRefs {notifiedReadingRef, cancellationRef})
  entityName
  streamId
  (Limit limit)
  relative
  readDirection = do
    conn <- ops.acquire cfg |> Task.mapError ConnectionAcquisitionError

    breakLoopRef <- Var.new False
    remainingLimitRef <- Var.new limit
    offsetRef <- Var.new (0 :: Int64)

    let shouldKeepReading = do
          shouldBreak <- Var.get breakLoopRef
          isCancelled <- Var.get cancellationRef
          remainingLimit <- Var.get remainingLimitRef
          Task.yield (not shouldBreak && not isCancelled && remainingLimit > 0)

    -- Determine starting position based on relative position
    let streamPosition =
          case relative of
            Just (FromAndAfter (StreamPosition p)) -> p
            Just (Before (StreamPosition p)) -> p
            Just Start -> 0
            Just End -> maxValue
            Nothing -> 0

    positionRef <- Var.new streamPosition

    Task.while (shouldKeepReading) do
      selectSession <- Sessions.selectStreamEventBatch positionRef entityName streamId relative readDirection
      records <-
        selectSession
          |> Sessions.run conn
          |> Task.mapError SessionError

      hasNotifiedReadingStarted <- Var.get notifiedReadingRef
      Task.unless (hasNotifiedReadingStarted) do
        stream |> Stream.writeItem StreamReadingStarted
        notifiedReadingRef |> Var.set True

      Task.when (records |> Array.isEmpty) do
        breakLoopRef |> Var.set True

      records |> Task.forEach \record -> do
        let evt :: Result Text (ReadStreamMessage eventType) = do
              event <- Json.decode record.eventData
              m <- Json.decode record.metadata
              let metadata =
                    m
                      { EventMetadata.globalPosition = Just (StreamPosition record.globalPosition),
                        EventMetadata.localPosition = Just (StreamPosition record.localPosition)
                      }
              Event
                { entityName,
                  streamId,
                  event,
                  metadata
                }
                |> StreamEvent
                |> Result.Ok

        remainingLimit <- Var.get remainingLimitRef
        if remainingLimit <= 0
          then
            pass
          else do
            case evt of
              Ok goodEvent -> do
                stream |> Stream.writeItem goodEvent
                -- Update position for next batch
                positionRef |> Var.set record.localPosition
              Err err -> do
                let locator = [fmt|#{EntityName.toText entityName}#{StreamId.toText streamId}|]
                let toxicEvt =
                      ToxicContents
                        { locator = locator,
                          metadata = record.metadata,
                          globalPosition = StreamPosition record.globalPosition,
                          localPosition = StreamPosition record.localPosition,
                          additionalInfo = err
                        }
                        |> ToxicStreamEvent
                stream |> Stream.writeItem toxicEvt
                positionRef |> Var.set record.localPosition
            Var.decrement remainingLimitRef

      -- Check if we got fewer records than batch size (means we're at the end)
      Task.when (Array.length records < batchSize) do
        breakLoopRef |> Var.set True

      -- Update offset for next batch if continuing
      currentOffset <- Var.get offsetRef
      offsetRef |> Var.set (currentOffset + fromIntegral batchSize)

    stream |> Stream.end
    Task.yield stream
