module Service.EventStore.Postgres.Internal (
  Config (..),
  Ops (..),
  new,
  defaultOps,
  Sessions.Connection (..),
) where

import Array qualified
import Core
import Default ()
import Hasql.Connection qualified as Hasql
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Json qualified
import Maybe qualified
import Service.Event
import Service.Event.EntityName qualified as EntityName
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core
import Service.EventStore.Postgres.Internal.Sessions qualified as Sessions
import Task qualified


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

  let initializeTable connection =
        Sessions.createEventsTableSession
          |> Sessions.run connection
          |> Task.mapError toText
          |> discard

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
          { insert = insertImpl ops cfg,
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


insertImpl ::
  (Json.Encodable eventType) =>
  Ops ->
  Config ->
  InsertionPayload eventType ->
  Task Error InsertionSuccess
insertImpl ops cfg payload = do
  conn <- ops.acquire cfg |> Task.mapError (toText .> InsertionFailed .> InsertionError)
  let payloadEventIds =
        payload.insertions
          |> Array.map (\i -> i.metadata.eventId)

  alreadyExistingIds <-
    Sessions.selectExistingIdsSession payloadEventIds
      |> Sessions.run conn
      |> Task.mapError (toText .> InsertionFailed .> InsertionError)

  let insertions =
        payload.insertions
          |> Array.dropIf
            ( \i ->
                alreadyExistingIds
                  |> Array.contains i.metadata.eventId
            )

  let insertionsCount = insertions |> Array.length

  if insertionsCount > 100
    then Task.throw (InsertionError PayloadTooLarge)
    else pass

  latestPositions <-
    Sessions.selectLatestEventInStream payload.entityName payload.streamId
      |> Sessions.run conn
      |> Task.mapError (toText .> InsertionFailed .> InsertionError)

  if insertionsCount <= 0
    then do
      let (latestGlobalPos, latestLocalPos) =
            latestPositions |> Maybe.withDefault (StreamPosition 0, StreamPosition 0)
      Task.yield InsertionSuccess {localPosition = latestLocalPos, globalPosition = latestGlobalPos}
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
        |> Task.mapError (toText .> InsertionFailed .> InsertionError)

      Task.throw (InsertionError (InsertionFailed (toText insertionRecords)))


readStreamForwardFromImpl :: EntityName -> StreamId -> StreamPosition -> Limit -> Task Error (Array (Event eventType))
readStreamForwardFromImpl _ _ _ _ = panic "Postgres.readStreamForwardFromImpl - Not implemented yet" |> Task.yield


readStreamBackwardFromImpl :: EntityName -> StreamId -> StreamPosition -> Limit -> Task Error (Array (Event eventType))
readStreamBackwardFromImpl _ _ _ _ = panic "Postgres.readStreamBackwardFromImpl - Not implemented yet" |> Task.yield


readAllStreamEventsImpl :: EntityName -> StreamId -> Task Error (Array (Event eventType))
readAllStreamEventsImpl _ _ = panic "Postgres.readAllStreamEventsImpl - Not implemented yet" |> Task.yield


readAllEventsForwardFromImpl :: StreamPosition -> Limit -> Task Error (Array (Event eventType))
readAllEventsForwardFromImpl _ _ = panic "Postgres.readAllEventsForwardFromImpl - Not implemented yet" |> Task.yield


readAllEventsBackwardFromImpl :: StreamPosition -> Limit -> Task Error (Array (Event eventType))
readAllEventsBackwardFromImpl _ _ = panic "Postgres.readAllEventsBackwardFromImpl - Not implemented yet" |> Task.yield


readAllEventsForwardFromFilteredImpl ::
  StreamPosition -> Limit -> Array EntityName -> Task Error (Array (Event eventType))
readAllEventsForwardFromFilteredImpl _ _ _ = panic "Postgres.readAllEventsForwardFromFilteredImpl - Not implemented yet" |> Task.yield


readAllEventsBackwardFromFilteredImpl ::
  StreamPosition -> Limit -> Array EntityName -> Task Error (Array (Event eventType))
readAllEventsBackwardFromFilteredImpl _ _ _ = panic "Postgres.readAllEventsBackwardFromFilteredImpl - Not implemented yet" |> Task.yield


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

{-
EXAMPLE C# IMPLEMENTATION WITH MSSQLSERVER

```cs
public class MsSqlEventStore<EventInterface>(
  public AsyncResult<InsertionSuccess, InsertionFailure> Insert(InsertionPayload<EventInterface> payload)
  {
    return Go();

    async Task<Result<InsertionSuccess, InsertionFailure>> Go()
    {
      try
      {
        await using var connection = new SqlConnection(connectionString);
        var payloadEventIds = payload.Insertions.Select(i => i.Metadata.EventId).ToArray();
        var insertionDefaultCorrelationId = Guid.NewGuid().ToString();
        var consistencyRetryCount = 0;

        while (true)
        {
          try
          {
            var eventIdParams = new DynamicParameters();
            foreach (var t in payloadEventIds.Select((id, idx) => (id, idx)))
            {
              eventIdParams.Add($"@EventId{t.idx}", t.id);
            }

            var alreadyExistingIds = await connection.QueryAsync<Guid>(
              $"""
               SELECT EventId
               FROM Events
               WHERE EventId IN ({string.Join(',', payloadEventIds.Select((_, i) => $"@EventId{i}"))})
               """,
              eventIdParams);

            var insertions = payload.Insertions.Where(i => !alreadyExistingIds.Contains(i.Metadata.EventId)).ToArray();

            if (insertions.Length > 100)
            {
              return new InsertionFailure.PayloadTooLarge();
            }

            var latestStreamEvent = await connection.QuerySingleOrDefaultAsync<EventExistenceRecord>(
              """
              SELECT TOP 1 GlobalPosition, StreamPosition
              FROM Events
              WHERE Swimlane = @Swimlane AND InlinedStreamId = @StreamId
              ORDER BY StreamPosition DESC
              """,
              new { payload.Swimlane, StreamId = payload.StreamId.StreamId() });

            if (insertions.Length == 0)
            {
              return new InsertionSuccess(
                (ulong?)latestStreamEvent?.GlobalPosition ?? 0UL,
                latestStreamEvent?.StreamPosition ?? 0);
            }

            var offset = payload.InsertionType switch
            {
              InsertAfter(var pos) => pos + 1,
              _ => latestStreamEvent?.StreamPosition switch
              {
                { } le => le + 1,
                null => 0
              }
            };

            var insertionRecords = insertions
              .Select((i, idx) => serializer(i.Event)
                .Apply(et => new EventInsertionRecord(
                  i.Metadata.EventId,
                  offset + idx,
                  payload.StreamId.StreamId(),
                  payload.Swimlane,
                  et.typeName,
                  et.data,
                  Encoding.UTF8.GetBytes(
                    JsonConvert.SerializeObject(
                      new StoredMetadata(
                        i.Metadata.RelatedUserSub,
                        i.Metadata.CorrelationId ?? insertionDefaultCorrelationId,
                        i.Metadata.CausationId,
                        i.Metadata.CreatedAt))))))
              .ToArray();

            var valueParams = insertionRecords
              .Select((_, i) =>
                $"(@EventId{i}, @StreamPosition{i}, @InlinedStreamId, @Swimlane, @EventType{i}, @EventData{i}, @Metadata{i})")
              .Apply(strings => string.Join(", ", strings));

            var values = insertionRecords
              .SelectMany<EventInsertionRecord, KeyValuePair<string, object>>((r, i) =>
              [
                new KeyValuePair<string, object>($"EventId{i}", r.EventId),
                new KeyValuePair<string, object>($"StreamPosition{i}", r.StreamPosition),
                new KeyValuePair<string, object>($"EventType{i}", r.EventType),
                new KeyValuePair<string, object>($"EventData{i}", r.EventData),
                new KeyValuePair<string, object>($"Metadata{i}", r.Metadata)
              ])
              .Append(new KeyValuePair<string, object>("InlinedStreamId", payload.StreamId.StreamId()))
              .Append(new KeyValuePair<string, object>("Swimlane", payload.Swimlane))
              .ToDictionary();

            var insertionSql =
              $"""
               INSERT INTO Events (EventId, StreamPosition, InlinedStreamId, Swimlane, EventType, EventData, Metadata)
               VALUES {valueParams};
               """;

            await connection.ExecuteAsync(insertionSql, values);

            var lastEvent = await connection.QuerySingleOrDefaultAsync<EventExistenceRecord>(
              """
              SELECT TOP 1 GlobalPosition, StreamPosition
              FROM Events
              WHERE EventId = @EventId
              """,
              new { insertionRecords.Last().EventId });

            return new InsertionSuccess((ulong)(lastEvent?.GlobalPosition ?? 0), lastEvent?.StreamPosition ?? 0);
          }
          catch (SqlException ex) when (
            ex.Number == 2627
            && ex.Message.Contains("UK_Events_Stream")
            && payload.InsertionType is not AnyStreamState)
          {
            return new InsertionFailure.ConsistencyCheckFailed();
          }
          catch (SqlException ex) when (
            ex.Number == 2627
            && ex.Message.Contains("UK_Events_Stream")
            && payload.InsertionType is AnyStreamState
            && consistencyRetryCount < 100)
          {
            // try again
            consistencyRetryCount++;
            await Task.Delay(consistencyRetryCount + 1);
          }
        }
      }
      catch
      {
        return new InsertionFailure.InsertionFailed();
      }
    }
  }

-}