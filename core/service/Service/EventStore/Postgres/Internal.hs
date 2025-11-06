module Service.EventStore.Postgres.Internal (
  Config (..),
  Connection (..),
  Ops (..),
  new,
  defaultOps,
) where

import Array qualified
import Core
import Hasql.Connection qualified as Hasql
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Session qualified as Session
import Result qualified
import Service.Event
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


data Connection
  = Connection Hasql.Connection
  | MockConnection


data Ops = Ops
  { acquire :: Config -> Task Text Connection,
    initializeTable :: Connection -> Task Text Unit
  }


defaultOps :: Ops
defaultOps = do
  let acquireImpl cfg = do
        connection <-
          cfg
            |> toConnectionSettings
            |> Hasql.acquire
            |> Task.fromIOEither
            |> Task.mapError toText
        Task.yield (Connection connection)
  let initializeTableImpl connection = do
        let session = Sessions.createEventsTableSession
        case connection of
          MockConnection -> pass
          Connection conn -> do
            result <- Session.run session conn |> Task.fromIO |> Task.map Result.fromEither
            case result of
              Result.Err _ ->
                -- FIXME: Add logging saying that the table already exists
                pass
              Result.Ok _ ->
                -- FIXME: Add logging saying that the table was created
                pass
  Ops
    { acquire = acquireImpl,
      initializeTable = initializeTableImpl
    }


new :: Ops -> Config -> Task Text (EventStore eventType)
new ops cfg = do
  connection <- ops.acquire cfg
  ops.initializeTable connection
  let eventStore =
        EventStore
          { insert = insertImpl connection,
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


insertImpl :: Connection -> InsertionPayload eventType -> Task Error InsertionSuccess
insertImpl _connection payload = do
  let insertionsCount = payload.insertions |> Array.length

  -- Validate payload is not empty
  if insertionsCount <= 0
    then Task.throw (InsertionError EmptyPayload)
    else pass

  -- Validate payload is not too large
  if insertionsCount > 100
    then Task.throw (InsertionError PayloadTooLarge)
    else pass

  -- TODO: Implement the following steps based on the C# implementation (lines 435-577):
  -- 1. Extract event IDs from payload insertions
  -- 2. Generate default correlation ID for events without one
  -- 3. Initialize retry counter for AnyStreamState retries (max 100)
  -- 4. Enter retry loop (infinite with explicit breaks/returns)
  -- 5. Query for existing event IDs in database (idempotency check)
  --    SQL: SELECT EventId FROM Events WHERE EventId IN (...)
  -- 6. Filter out events that already exist in database
  -- 7. Validate filtered insertions don't exceed 100 events
  -- 8. Query latest stream event to get current stream state
  --    SQL: SELECT TOP 1 GlobalPosition, LocalPosition FROM Events
  --         WHERE Entity = @Entity AND InlinedStreamId = @StreamId
  --         ORDER BY LocalPosition DESC
  -- 9. If no new insertions after filtering, return current stream positions
  -- 10. Calculate offset (starting LocalPosition) based on InsertionType:
  --     - InsertAfter pos -> pos + 1
  --     - Otherwise -> latestStreamEvent.LocalPosition + 1 (or 0 if no events)
  -- 11. Build insertion records with:
  --     - EventId, LocalPosition (offset + index), InlinedStreamId, Entity
  --     - EventType (serialized), EventData (serialized), Metadata (serialized as JSON)
  -- 12. Build parameterized SQL INSERT statement with all events
  --     SQL: INSERT INTO Events (EventId, LocalPosition, InlinedStreamId, Entity, EventType, EventData, Metadata)
  --          VALUES (@EventId0, @LocalPosition0, ...), (@EventId1, @LocalPosition1, ...), ...
  -- 13. Execute INSERT statement (GlobalPosition auto-increments via BIGSERIAL)
  -- 14. Query the last inserted event to get its GlobalPosition and LocalPosition
  --     SQL: SELECT TOP 1 GlobalPosition, LocalPosition FROM Events WHERE EventId = @LastEventId
  -- 15. Return InsertionSuccess with positions from last event
  -- 16. Catch unique constraint violations on UK_Events_Stream (Entity, InlinedStreamId, LocalPosition):
  --     - If InsertionType is NOT AnyStreamState -> return ConsistencyCheckFailed
  --     - If InsertionType IS AnyStreamState and retries < 100 -> increment retry, delay, loop again
  -- 17. Catch all other exceptions -> return InsertionFailed
  --
  -- Key differences from InMemory implementation:
  -- - Uses SQL queries instead of in-memory data structures
  -- - Relies on database UNIQUE constraints for consistency checking
  -- - GlobalPosition is auto-generated by BIGSERIAL (not manually calculated)
  -- - Retry logic uses exception handling instead of optimistic locking
  -- - All events inserted in single SQL statement for atomicity

  -- For now, throw error indicating this needs SQL implementation
  Task.throw (InsertionError InsertionFailed)


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
using System.Runtime.CompilerServices;
using System.Text;
using Dapper;
using DeFuncto;
using DeFuncto.Extensions;
using Microsoft.Data.SqlClient;
using Newtonsoft.Json;
using Nvx.ConsistentAPI.Store.Events;
using Nvx.ConsistentAPI.Store.Store;
using static DeFuncto.Prelude;

namespace Nvx.ConsistentAPI.Store.MsSql;

public class MsSqlEventStore<EventInterface>(
  string connectionString,
  Func<string, byte[], Option<(EventInterface evt, StrongId streamId)>> deserializer,
  Func<EventInterface, (string typeName, byte[] data)> serializer)
  : EventStore<EventInterface> where EventInterface : HasSwimlane, HasEntityId
{
  private const string EventStoreTableCreationSql =
    """
    CREATE TABLE Events (
        EventId UNIQUEIDENTIFIER NOT NULL,
        GlobalPosition BIGINT IDENTITY(1,1) NOT NULL,
        StreamPosition BIGINT NOT NULL,
        InlinedStreamId NVARCHAR(4000) NOT NULL,
        Swimlane NVARCHAR(255) NOT NULL,
        EventType NVARCHAR(255) NOT NULL,
        EventData VARBINARY(MAX) NOT NULL,
        Metadata VARBINARY(MAX) NULL,
        CONSTRAINT PK_Events PRIMARY KEY (GlobalPosition),
        CONSTRAINT UK_Events_EventId UNIQUE (EventId),
        CONSTRAINT UK_Events_Stream UNIQUE (Swimlane, InlinedStreamId, StreamPosition)
    );
    """;

  private const int BatchSize = 500;

  private readonly TimeSpan pollingDelay = TimeSpan.FromMilliseconds(500);

  public async Task Initialize(CancellationToken cancellationToken = default)
  {
    await using var connection = new SqlConnection(connectionString);
    try
    {
      await connection.ExecuteAsync(EventStoreTableCreationSql, cancellationToken);
    }
    catch (SqlException e) when (e.Number == 2714)
    {
      // Ignore if the table already exists.
    }
  }

  public async IAsyncEnumerable<ReadAllMessage<EventInterface>> Read(
    ReadAllRequest request = default,
    [EnumeratorCancellation] CancellationToken cancellationToken = default)
  {
    await using var connection = new SqlConnection(connectionString);
    var hasNotifiedReadingStarted = false;

    var direction =
      request.Relative switch
      {
        RelativePosition.Start => "ASC",
        RelativePosition.End => "DESC",
        _ => request.Direction switch
        {
          ReadDirection.Forwards => "ASC",
          ReadDirection.Backwards => "DESC",
          _ => "ASC"
        }
      };

    var positionFilter =
      request.Direction switch
      {
        ReadDirection.Backwards => "GlobalPosition < @Position",
        _ => "GlobalPosition >= @Position"
      };

    var position =
      request.Relative switch
      {
        RelativePosition.Start => 0L,
        RelativePosition.End => long.MaxValue,
        _ => request.Direction switch
        {
          ReadDirection.Forwards => (long)(request.Position ?? 0L),
          ReadDirection.Backwards => (long)(request.Position ?? long.MaxValue),
          _ => 0L
        }
      };

    var swimlaneFilters = request.Swimlanes is null || request.Swimlanes.Length == 0
      ? ""
      : $" AND Swimlane IN ({string.Join(", ", request.Swimlanes.Select(s => $"'{s}'"))})";

    while (!cancellationToken.IsCancellationRequested)
    {
      var query =
        $"""
           SELECT TOP {BatchSize} EventId, GlobalPosition, StreamPosition, InlinedStreamId, Swimlane, EventType, EventData, Metadata
           FROM Events
           WHERE {positionFilter}{swimlaneFilters}
           ORDER BY GlobalPosition {direction};
         """;
      var records = await connection
        .QueryAsync<FullEventRecord>(
          query,
          new
          {
            Position = (long?)position
          })
        .Map(r => r.ToArray());

      if (!hasNotifiedReadingStarted)
      {
        yield return new ReadAllMessage<EventInterface>.ReadingStarted();
        hasNotifiedReadingStarted = true;
      }

      if (records.Length == 0)
      {
        break;
      }

      foreach (var record in records)
      {
        var evt =
          from e in deserializer(record.EventType, record.EventData)
          from m in DeserializeMetadata(record.Metadata)
          select new ReadAllMessage<EventInterface>.AllEvent(
            record.Swimlane,
            e.streamId,
            e.evt,
            new StoredEventMetadata(
              record.EventId,
              m.EmittedBy,
              m.CorrelationId,
              m.CausationId,
              m.EmittedAt,
              (ulong)record.GlobalPosition,
              record.StreamPosition)) as ReadAllMessage<EventInterface>;
        yield return evt.DefaultValue(ReadAllMessage<EventInterface> () =>
          new ReadAllMessage<EventInterface>.ToxicAllEvent(
            $"{record.Swimlane}{record.InlinedStreamId}",
            record.Metadata,
            (ulong)record.GlobalPosition,
            record.StreamPosition));
        position = record.GlobalPosition;
      }

      if (records.Length < BatchSize)
      {
        break;
      }
    }
  }

  public async IAsyncEnumerable<ReadAllMessage<EventInterface>> Subscribe(
    SubscribeAllRequest request = default,
    [EnumeratorCancellation] CancellationToken cancellationToken = default)
  {
    var currentPosition = (long?)request.Position ?? await GetMaxGlobalPosition();
    currentPosition++;
    var hasNotifiedReadingStarted = false;
    var lastNotifiedSyncStatus = NotifiedSyncStatus.None;

    var messageBatch = new List<ReadAllMessage<EventInterface>>(BatchSize);
    while (!cancellationToken.IsCancellationRequested)
    {
      var readRequest = new ReadAllRequest(
        (ulong?)currentPosition,
        request.Position == 0 && !hasNotifiedReadingStarted ? RelativePosition.Start : null,
        ReadDirection.Forwards,
        request.Swimlanes ?? []);
      messageBatch.Clear();

      try
      {
        await foreach (var message in Read(readRequest, cancellationToken))
        {
          messageBatch.Add(message);
        }
      }
      catch
      {
        // Ignore exceptions.
      }

      if (messageBatch.Count == 0)
      {
        if (lastNotifiedSyncStatus != NotifiedSyncStatus.CaughtUp)
        {
          yield return new ReadAllMessage<EventInterface>.CaughtUp();
          lastNotifiedSyncStatus = NotifiedSyncStatus.CaughtUp;
        }

        await Task.Delay(pollingDelay, cancellationToken);
        continue;
      }

      if (messageBatch.Count == BatchSize && lastNotifiedSyncStatus != NotifiedSyncStatus.Behind)
      {
        yield return new ReadAllMessage<EventInterface>.FellBehind();
        lastNotifiedSyncStatus = NotifiedSyncStatus.Behind;
      }

      foreach (var message in messageBatch)
      {
        if (!hasNotifiedReadingStarted)
        {
          yield return new ReadAllMessage<EventInterface>.ReadingStarted();
          hasNotifiedReadingStarted = true;
        }

        if (message is not ReadAllMessage<EventInterface>.ReadingStarted)
        {
          yield return message;
        }

        currentPosition = message switch
        {
          ReadAllMessage<EventInterface>.AllEvent e => (long)e.Metadata.GlobalPosition + 1,
          ReadAllMessage<EventInterface>.ToxicAllEvent te => (long)te.GlobalPosition + 1,
          _ => currentPosition
        };
      }
    }
  }

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

  public async IAsyncEnumerable<ReadStreamMessage<EventInterface>> Read(
    ReadStreamRequest request,
    [EnumeratorCancellation] CancellationToken cancellationToken = default)
  {
    await using var connection = new SqlConnection(connectionString);
    var direction = request.Direction == ReadDirection.Forwards ? "ASC" : "DESC";
    var positionFilter = request.Position switch
    {
      RelativePosition.Start => string.Empty,
      RelativePosition.End => string.Empty,
      _ => request.Direction switch
      {
        ReadDirection.Forwards => "AND StreamPosition >= @StreamPosition",
        ReadDirection.Backwards => "AND StreamPosition <= @StreamPosition",
        _ => string.Empty
      }
    };

    var streamPosition = request.Position switch
    {
      RelativePosition.Start => 0,
      RelativePosition.End => long.MaxValue,
      _ => request.StreamPosition
    };

    var offset = 0;
    var hasNotifiedReadingStarted = false;

    while (true)
    {
      var query =
        $"""
            SELECT EventId, GlobalPosition, StreamPosition, InlinedStreamId, Swimlane, EventType, EventData, Metadata
            FROM Events
            WHERE Swimlane = @Swimlane AND InlinedStreamId = @StreamId {positionFilter}
            ORDER BY GlobalPosition {direction}
            OFFSET @Offset ROWS
            FETCH NEXT @Count ROWS ONLY;
         """;

      var records = await connection
        .QueryAsync<FullEventRecord>(
          query,
          new
          {
            request.Swimlane,
            StreamId = request.Id.StreamId(),
            StreamPosition = streamPosition,
            Count = BatchSize,
            Offset = offset
          })
        .Map(r => r.ToArray());

      if (!hasNotifiedReadingStarted)
      {
        yield return new ReadStreamMessage<EventInterface>.ReadingStarted();
        hasNotifiedReadingStarted = true;
      }

      foreach (var record in records)
      {
        yield return (
            from e in deserializer(record.EventType, record.EventData)
            from m in DeserializeMetadata(record.Metadata)
            select new ReadStreamMessage<EventInterface>.SolvedEvent(
              record.Swimlane,
              e.streamId,
              e.evt,
              new StoredEventMetadata(
                record.EventId,
                m.EmittedBy,
                m.CorrelationId,
                m.CausationId,
                m.EmittedAt,
                (ulong)record.GlobalPosition,
                record.StreamPosition)) as ReadStreamMessage<EventInterface>)
          .DefaultValue(ReadStreamMessage<EventInterface> () => new ReadStreamMessage<EventInterface>.ToxicEvent(
            $"{record.Swimlane}{request.Id.StreamId()}",
            record.EventData,
            record.Metadata,
            (ulong)record.GlobalPosition,
            record.StreamPosition));
      }

      if (records.Length < BatchSize)
      {
        break;
      }

      // Technical debt: If the stream gets truncating during a read, this will repeat events.
      offset += BatchSize;
    }
  }

  public async IAsyncEnumerable<ReadStreamMessage<EventInterface>> Subscribe(
    SubscribeStreamRequest request,
    [EnumeratorCancellation] CancellationToken cancellationToken = default)
  {
    var position = request.IsFromStart ? -1 : await GetMaxStreamPosition(request.Swimlane, request.Id.StreamId()) + 1;

    var messageBatch = new List<ReadStreamMessage<EventInterface>>(BatchSize);

    var hasNotifiedReadingStarted = false;

    while (!cancellationToken.IsCancellationRequested)
    {
      var readRequest =
        new ReadStreamRequest(
          request.Swimlane,
          request.Id,
          request.IsFromStart && !hasNotifiedReadingStarted ? RelativePosition.Start : null,
          ReadDirection.Forwards,
          position);

      messageBatch.Clear();

      try
      {
        await foreach (var message in Read(readRequest, cancellationToken))
        {
          messageBatch.Add(message);
        }
      }
      catch
      {
        // Ignore exceptions.
      }

      if (!hasNotifiedReadingStarted)
      {
        yield return new ReadStreamMessage<EventInterface>.ReadingStarted();
        hasNotifiedReadingStarted = true;
      }

      if (messageBatch.Count == 0)
      {
        await Task.Delay(pollingDelay, cancellationToken);
        continue;
      }

      foreach (var message in messageBatch)
      {
        position = message switch
        {
          ReadStreamMessage<EventInterface>.SolvedEvent e => e.Metadata.StreamPosition + 1,
          ReadStreamMessage<EventInterface>.ToxicEvent te => te.StreamPosition + 1,
          _ => position
        };

        yield return message;
      }
    }
  }

  public async Task TruncateStream(string swimlane, StrongId id, long truncateBefore)
  {
    await using var connection = new SqlConnection(connectionString);
    await connection.ExecuteAsync(
      """
      DELETE FROM Events
      WHERE Swimlane = @Swimlane AND InlinedStreamId = @StreamId AND StreamPosition < @TruncateBefore
      """,
      new { Swimlane = swimlane, StreamId = id.StreamId(), TruncateBefore = truncateBefore });
  }

  private async Task<long> GetMaxGlobalPosition()
  {
    await using var connection = new SqlConnection(connectionString);
    return await connection.QueryFirstAsync<long>("SELECT COALESCE(MAX(GlobalPosition), -1) FROM Events");
  }

  private async Task<long> GetMaxStreamPosition(string swimlane, string inlinedStreamId)
  {
    await using var connection = new SqlConnection(connectionString);
    return await connection.QueryFirstAsync<long>(
      """
      SELECT COALESCE(MAX(StreamPosition), -1)
      FROM Events
      WHERE Swimlane = @Swimlane AND InlinedStreamId = @InlinedStreamId
      """,
      new { Swimlane = swimlane, InlinedStreamId = inlinedStreamId });
  }

  private static Option<StoredMetadata> DeserializeMetadata(byte[] metadata)
  {
    try
    {
      return Optional(JsonConvert.DeserializeObject<StoredMetadata>(Encoding.UTF8.GetString(metadata)));
    }
    catch
    {
      return None;
    }
  }

  private enum NotifiedSyncStatus
  {
    CaughtUp,
    Behind,
    None
  }

  private record FullEventRecord(
    Guid EventId,
    long GlobalPosition,
    long StreamPosition,
    string InlinedStreamId,
    string Swimlane,
    string EventType,
    byte[] EventData,
    byte[] Metadata);

  private record EventInsertionRecord(
    Guid EventId,
    long StreamPosition,
    string InlinedStreamId,
    string Swimlane,
    string EventType,
    byte[] EventData,
    byte[] Metadata);

  private record StoredMetadata(
    string? EmittedBy,
    string CorrelationId,
    string? CausationId,
    DateTime EmittedAt);

  private record EventExistenceRecord(long GlobalPosition, long StreamPosition);
}
```

-}