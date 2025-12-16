module Service.EventStore.Postgres.Internal (
  PostgresEventStore (..),
  Ops (..),
  new,
  defaultOps,
  Sessions.Connection (..),
  SubscriptionStore.SubscriptionStore (..),
  withConnection,
) where

import Array (Array)
import Array qualified
import AsyncTask qualified
import Basics
import Default ()
import Hasql.Connection qualified as Hasql
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting qualified as Hasql
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Pool qualified as HasqlPool
import Hasql.Pool.Config qualified as HasqlPoolConfig
import Json qualified
import LinkedList (LinkedList)
import Maybe (Maybe (..))
import Maybe qualified
import Result (Result (..))
import Service.Event
import Service.Event.EntityName qualified as EntityName
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core
import Service.EventStore.Postgres.Internal.Core
import Service.EventStore.Postgres.Internal.Notifications qualified as Notifications
import Service.EventStore.Postgres.Internal.PostgresEventRecord (PostgresEventRecord (..))
import Service.EventStore.Postgres.Internal.Sessions qualified as Sessions
import Service.EventStore.Postgres.Internal.SubscriptionStore (SubscriptionStore)
import Service.EventStore.Postgres.Internal.SubscriptionStore qualified as SubscriptionStore
import Stream (Stream)
import Stream qualified
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)
import Unsafe.Coerce qualified as GHC
import Var (Var)
import Var qualified


data PostgresEventStore = PostgresEventStore
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text,
    port :: Int
  }
  deriving (Eq, Ord, Show)


instance EventStoreConfig PostgresEventStore where
  newEventStoreConstructor =
    EventStoreConstructor
      ( new
          @(Text) -- Horrible hack to get through FromJSON/ToJSON
          defaultOps
          |> GHC.unsafeCoerce
      )


toConnectionPoolSettings :: LinkedList Hasql.Setting -> HasqlPoolConfig.Config
toConnectionPoolSettings settings = do
  [HasqlPoolConfig.staticConnectionSettings settings] |> HasqlPoolConfig.settings


toConnectionSettings :: PostgresEventStore -> LinkedList Hasql.Setting
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


data Ops eventType = Ops
  { acquire :: PostgresEventStore -> Task Text Sessions.Connection,
    release :: Sessions.Connection -> Task Text Unit,
    initializeTable :: Sessions.Connection -> Task Text Unit,
    initializeSubscriptions :: SubscriptionStore eventType -> PostgresEventStore -> Task Text Unit
  }


defaultOps ::
  (Json.FromJSON eventType) =>
  Ops eventType
defaultOps = do
  let acquire cfg = do
        toConnectionSettings cfg
          |> toConnectionPoolSettings
          |> HasqlPool.acquire
          |> Task.fromIO
          |> Task.map (Sessions.Connection)

  let initializeTable connection = do
        Sessions.createEventsTableSession
          |> Sessions.run connection
          |> Task.mapError toText

  let initializeSubscriptions subscriptionStore cfg = do
        connection <- Hasql.acquire (toConnectionSettings cfg) |> Task.fromIOEither |> Task.mapError toText
        Sessions.createEventNotificationTriggerFunctionSession
          |> Sessions.runConnection connection
          |> Task.mapError toText

        Sessions.createEventNotificationTriggerSession
          |> Sessions.runConnection connection
          |> Task.mapError toText

        subscriptionStore |> Notifications.connectTo connection

  let release connection = do
        case connection of
          Sessions.MockConnection ->
            pass
          Sessions.Connection conn ->
            HasqlPool.release conn
              |> Task.fromIO @Unit

  Ops {acquire, initializeTable, initializeSubscriptions, release}


withConnection ::
  PostgresEventStore ->
  (Sessions.Connection -> Task PostgresStoreError result) ->
  Ops eventType ->
  Task PostgresStoreError result
withConnection cfg callback ops = do
  connection <- ops.acquire cfg |> Task.mapError ConnectionAcquisitionError
  result <- callback connection |> Task.asResult
  ops.release connection |> Task.mapError ConnectionReleaseError
  case result of
    Ok res -> Task.yield res
    Err err -> Task.throw err


withConnectionAndError ::
  (Show result) =>
  PostgresEventStore -> (Sessions.Connection -> Task Error result) -> Ops eventType -> Task Error result
withConnectionAndError cfg callback ops = do
  connection <- ops.acquire cfg |> Task.mapError (ConnectionAcquisitionError .> toText .> StorageFailure)
  result <- callback connection |> Task.asResult
  ops.release connection |> Task.mapError (ConnectionReleaseError .> toText .> StorageFailure)
  case result of
    Ok res -> do
      Task.yield res
    Err err ->
      Task.throw err


new ::
  ( Json.ToJSON eventType,
    Json.FromJSON eventType
  ) =>
  Ops eventType ->
  PostgresEventStore ->
  Task Text (EventStore eventType)
new ops cfg =
  ops
    |> withConnection
      cfg
      ( \connection -> do
          ops.initializeTable connection |> Task.mapError (TableInitializationError)
          subscriptionStore <- SubscriptionStore.new |> Task.mapError (toText .> SubscriptionInitializationError)
          ops.initializeSubscriptions subscriptionStore cfg |> Task.mapError (SubscriptionInitializationError)
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
                    subscribeToAllEvents = subscribeToAllEventsImpl ops cfg subscriptionStore,
                    subscribeToAllEventsFromPosition = subscribeToAllEventsFromPositionImpl ops cfg subscriptionStore,
                    subscribeToAllEventsFromStart = subscribeToAllEventsFromStartImpl ops cfg subscriptionStore,
                    subscribeToEntityEvents = subscribeToEntityEventsImpl ops cfg subscriptionStore,
                    subscribeToStreamEvents = subscribeToStreamEventsImpl ops cfg subscriptionStore,
                    unsubscribe = unsubscribeImpl subscriptionStore,
                    truncateStream = truncateStreamImpl ops cfg
                  }
          Task.yield eventStore
      )
    |> Task.mapError toText


insertImpl ::
  forall eventType.
  ( Json.FromJSON eventType,
    Json.ToJSON eventType
  ) =>
  Ops eventType ->
  PostgresEventStore ->
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
    Err (ConnectionReleaseError err) ->
      Task.throw (StorageFailure err)
    Err (TableInitializationError err) ->
      Task.throw (StorageFailure err)
    Err (SubscriptionInitializationError err) ->
      Task.throw (StorageFailure err)
    Err (CoreInsertionError err) ->
      Task.throw (InsertionError err)
    Err (SubscriptionStoreError identifier err) ->
      Task.throw (SubscriptionError (SubscriptionId identifier) (toText err))
    Err (SessionError err) -> do
      let isEventsUniqueKeyViolation err =
            (err |> toText |> Text.contains "\"23505\"")
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
              Task.throw (InsertionError (InsertionFailed (toText err)))


insertGo ::
  (Json.ToJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
  InsertionPayload eventType ->
  Task PostgresStoreError InsertionSuccess
insertGo ops cfg payload =
  ops |> withConnection cfg \conn -> do
    Task.when (payload.insertions |> Array.isEmpty) do
      Task.throw (CoreInsertionError EmptyPayload)

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

    -- Check stream existence constraint for StreamCreation
    case payload.insertionType of
      StreamCreation -> do
        case latestPositions of
          Just _ -> do
            -- Stream already exists, cannot create
            Task.throw (CoreInsertionError ConsistencyCheckFailed)
          Nothing -> do
            -- Stream doesn't exist, can proceed
            pass
      _ -> do
        pass

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
                    globalPosition = Nothing,
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
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
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
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
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
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
  EntityName ->
  StreamId ->
  Task Error (Stream (ReadStreamMessage eventType))
readAllStreamEventsImpl ops cfg entityName streamId = do
  readingRefs <- newReadingRefs
  stream <- Stream.new
  let limit = Limit maxValue -- Read all events
  let relative = Just Start
  let readDirection = Just Forwards

  performReadStreamEvents ops cfg stream readingRefs entityName streamId limit relative readDirection
    |> Task.mapError (toText .> StorageFailure)


readAllEventsForwardFromImpl ::
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
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
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
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
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
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
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
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
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
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
  entityNames =
    ops |> withConnection cfg \conn -> do
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

        Task.when (records |> Array.isEmpty) do
          breakLoopRef |> Var.set True

        -- Only notify reading started if we have records to process
        Task.unless (records |> Array.isEmpty) do
          hasNotifiedReadingStarted <- Var.get notifiedReadingRef
          Task.unless (hasNotifiedReadingStarted) do
            stream |> Stream.writeItem ReadingStarted
            notifiedReadingRef |> Var.set True

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
                  -- Update position for next batch
                  -- For forward: increment by 1 (since we use >= comparison)
                  -- For backward: decrement by 1 (since we use <= comparison for global position)
                  case readDirection of
                    Just Backwards -> positionRef |> Var.set (record.globalPosition - 1)
                    _ -> positionRef |> Var.set (record.globalPosition + 1)
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
                  -- Update position for next batch
                  -- For forward: increment by 1 (since we use >= comparison)
                  -- For backward: decrement by 1 (since we use <= comparison for global position)
                  case readDirection of
                    Just Backwards -> positionRef |> Var.set (record.globalPosition - 1)
                    _ -> positionRef |> Var.set (record.globalPosition + 1)
              Var.decrement remainingLimitRef

        Task.when (Array.length records < batchSize) do
          breakLoopRef |> Var.set True

      stream |> Stream.end
      Task.yield stream


subscribeToAllEventsImpl ::
  Ops eventType ->
  PostgresEventStore ->
  SubscriptionStore eventType ->
  (Event eventType -> Task Text Unit) ->
  Task Error SubscriptionId
subscribeToAllEventsImpl ops cfg store callback = do
  ops |> withConnectionAndError cfg \conn -> do
    currentMaxPosition <-
      Sessions.selectMaxGlobalPosition
        |> Sessions.run conn
        |> Task.mapError (SessionError .> toText .> StorageFailure)
    store
      |> SubscriptionStore.addGlobalSubscriptionFromPosition currentMaxPosition callback
      |> Task.mapError
        ( \err ->
            SubscriptionStoreError "global" err
              |> toText
              |> SubscriptionError (SubscriptionId "global")
        )


subscribeToAllEventsFromPositionImpl ::
  forall eventType.
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
  SubscriptionStore eventType ->
  StreamPosition ->
  (Event eventType -> Task Text Unit) ->
  Task Error SubscriptionId
subscribeToAllEventsFromPositionImpl ops cfg store startPosition callback = do
  -- Catch up loop: read historical events and process them
  let catchUp currentPosition =
        ops |> withConnectionAndError cfg \conn -> do
          -- Get current max position
          maxPosition <-
            Sessions.selectMaxGlobalPosition
              |> Sessions.run conn
              |> Task.mapError (toText .> StorageFailure)

          case maxPosition of
            Nothing -> do
              -- No events in database, just subscribe from now
              Task.yield currentPosition
            Just maxPos -> do
              if maxPos <= currentPosition
                then do
                  -- No new events, we're caught up
                  Task.yield currentPosition
                else do
                  -- Read events from currentPosition to maxPos
                  let limit = Limit maxValue -- Read all available events
                  eventStream <-
                    readAllEventsForwardFromImpl ops cfg currentPosition limit
                      |> Task.mapError (\_ -> StorageFailure "Failed to read events during catch-up")

                  -- Collect all events from the stream
                  events <- eventStream |> Stream.toArray |> Task.mapError (toText .> StorageFailure)

                  -- Process each event serially through the callback
                  events
                    |> collectAllEvents
                    |> Task.forEach (\event -> callback event |> Task.asResult |> discard)

                  -- Check if more events were added while processing
                  catchUp maxPos

  -- Start catch-up from the requested position
  finalPosition <- catchUp startPosition

  -- Now subscribe from the final position onwards
  store
    |> SubscriptionStore.addGlobalSubscriptionFromPosition (Just finalPosition) callback
    |> Task.mapError (\err -> SubscriptionError (SubscriptionId "fromPosition") (err |> toText))


subscribeToAllEventsFromStartImpl ::
  forall eventType.
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
  SubscriptionStore eventType ->
  (Event eventType -> Task Text Unit) ->
  Task Error SubscriptionId
subscribeToAllEventsFromStartImpl ops cfg store callback = do
  -- Subscribe from the very beginning (position 0)
  subscribeToAllEventsFromPositionImpl ops cfg store (StreamPosition 0) callback


subscribeToEntityEventsImpl ::
  Ops eventType ->
  PostgresEventStore ->
  SubscriptionStore eventType ->
  EntityName ->
  (Event eventType -> Task Text Unit) ->
  Task Error SubscriptionId
subscribeToEntityEventsImpl ops cfg store entityName callback =
  ops |> withConnectionAndError cfg \conn -> do
    currentMaxPosition <-
      Sessions.selectMaxGlobalPosition
        |> Sessions.run conn
        |> Task.mapError (toText .> StorageFailure)
    store
      |> SubscriptionStore.addEntitySubscriptionFromPosition entityName currentMaxPosition callback
      |> Task.mapError (\err -> SubscriptionError (SubscriptionId "entity") (err |> toText))


subscribeToStreamEventsImpl ::
  forall eventType.
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
  SubscriptionStore eventType ->
  EntityName ->
  StreamId ->
  (Event eventType -> Task Text Unit) ->
  Task Error SubscriptionId
subscribeToStreamEventsImpl ops cfg store entityName streamId callback =
  ops |> withConnectionAndError cfg \conn -> do
    -- Subscribe to the stream-specific notification channel
    connection <-
      Hasql.acquire (toConnectionSettings cfg)
        |> Task.fromIOEither
        |> Task.mapError (\err -> SubscriptionError (SubscriptionId "stream") (err |> toText))
    Notifications.subscribeToStream connection streamId
      |> Task.mapError StorageFailure

    currentMaxPosition <-
      Sessions.selectMaxGlobalPosition
        |> Sessions.run conn
        |> Task.mapError (toText .> StorageFailure)
    store
      |> SubscriptionStore.addStreamSubscriptionFromPosition entityName streamId currentMaxPosition callback
      |> Task.mapError (\err -> SubscriptionError (SubscriptionId "stream") (err |> toText))


unsubscribeImpl :: SubscriptionStore eventType -> SubscriptionId -> Task Error Unit
unsubscribeImpl store id =
  store
    |> SubscriptionStore.removeSubscription id
    |> Task.mapError (\err -> SubscriptionError id (err |> toText))


truncateStreamImpl :: Ops eventType -> PostgresEventStore -> EntityName -> StreamId -> StreamPosition -> Task Error Unit
truncateStreamImpl ops cfg entityName streamId truncateBefore = do
  res <-
    truncateStreamGo ops cfg entityName streamId truncateBefore
      |> Task.asResult
  case res of
    Ok _ ->
      Task.yield unit
    Err err ->
      Task.throw (StorageFailure (toText err))


truncateStreamGo ::
  Ops eventType -> PostgresEventStore -> EntityName -> StreamId -> StreamPosition -> Task PostgresStoreError Unit
truncateStreamGo ops cfg entityName streamId truncateBefore =
  ops |> withConnection cfg \conn -> do
    Sessions.truncateStreamSession entityName streamId truncateBefore
      |> Sessions.run conn
      |> Task.mapError SessionError


performReadStreamEvents ::
  forall eventType.
  (Json.FromJSON eventType) =>
  Ops eventType ->
  PostgresEventStore ->
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
  readDirection =
    ops |> withConnection cfg \conn -> do
      breakLoopRef <- Var.new False
      remainingLimitRef <- Var.new limit

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
      -- Track if this is the first batch to use relative position correctly
      isFirstBatchRef <- Var.new True

      Task.while (shouldKeepReading) do
        isFirstBatch <- Var.get isFirstBatchRef
        -- Use the original relative position for the first batch, then switch to position-based
        currentRelative <-
          if isFirstBatch
            then Task.yield relative
            else do
              -- For subsequent batches, use FromAndAfter with current position
              -- The position has already been incremented, so this avoids duplicates
              currentPos <- Var.get positionRef
              Task.yield (Just (FromAndAfter (StreamPosition currentPos)))
        selectSession <- Sessions.selectStreamEventBatch positionRef entityName streamId currentRelative readDirection
        records <-
          selectSession
            |> Sessions.run conn
            |> Task.mapError SessionError

        -- Mark that we've completed the first batch
        Task.when (isFirstBatch) do
          isFirstBatchRef |> Var.set False

        Task.when (records |> Array.isEmpty) do
          breakLoopRef |> Var.set True

        -- Only notify reading started if we have records to process
        Task.unless (records |> Array.isEmpty) do
          hasNotifiedReadingStarted <- Var.get notifiedReadingRef
          Task.unless (hasNotifiedReadingStarted) do
            stream |> Stream.writeItem StreamReadingStarted
            notifiedReadingRef |> Var.set True

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
                  -- For forward: increment by 1 (since we use >= comparison)
                  -- For backward: keep same position (since we use < comparison which already excludes it)
                  case readDirection of
                    Just Backwards -> positionRef |> Var.set record.localPosition
                    _ -> positionRef |> Var.set (record.localPosition + 1)
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
                  -- Update position for next batch
                  -- For forward: increment by 1 (since we use >= comparison)
                  -- For backward: keep same position (since we use < comparison which already excludes it)
                  case readDirection of
                    Just Backwards -> positionRef |> Var.set record.localPosition
                    _ -> positionRef |> Var.set (record.localPosition + 1)
              Var.decrement remainingLimitRef

        -- Check if we got fewer records than batch size (means we're at the end)
        Task.when (Array.length records < batchSize) do
          breakLoopRef |> Var.set True

      stream |> Stream.end
      Task.yield stream
