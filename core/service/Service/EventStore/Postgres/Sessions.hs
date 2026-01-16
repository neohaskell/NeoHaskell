module Service.EventStore.Postgres.Sessions where

import Array qualified
import Bytes qualified
import Contravariant.Extras qualified as Contravariant
import Core
import Data.Functor.Contravariant ((>$<))
import Data.Semigroup ((<>))
import Data.Tuple (fst, snd)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Connection qualified as HasqlConnection
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pool qualified as HasqlPool
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))
import Hasql.Statement qualified as Hasql
import Hasql.TH qualified as TH
import Json qualified
import Mappable qualified
import Maybe qualified
import Result qualified
import Service.Event (EntityName (..), Event (..), StreamId (..), StreamPosition (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.EventStore.Postgres.Core
import Service.EventStore.Postgres.PostgresEventRecord (PostgresEventRecord)
import Service.EventStore.Postgres.PostgresEventRecord qualified as PostgresEventRecord
import Task qualified
import Text qualified
import Uuid qualified
import Var qualified


data Connection
  = Connection HasqlPool.Pool
  | MockConnection


data EventInsertionRecord = EventInsertionRecord
  { eventId :: Uuid,
    globalPosition :: Maybe Int64,
    localPosition :: Int64,
    inlinedStreamId :: Text,
    entity :: Text,
    eventData :: Json.Value,
    metadata :: Json.Value
  }
  deriving (Eq, Ord, Show, Generic)


instance Json.FromJSON EventInsertionRecord


insertionRecordToEvent ::
  (Json.FromJSON eventType) =>
  EventInsertionRecord ->
  Result Text (Event eventType)
insertionRecordToEvent record = do
  event <- Json.decode record.eventData
  rawMetadata <- Json.decode record.metadata
  let metadata =
        rawMetadata
          { EventMetadata.globalPosition = record.globalPosition |> Maybe.map StreamPosition,
            localPosition = record.localPosition |> StreamPosition |> Just
          }
  let entityName = EntityName record.entity
  let streamId = StreamId record.inlinedStreamId
  Event {event, metadata, entityName, streamId} |> Ok


run ::
  (Default result) =>
  Connection ->
  Session.Session result ->
  Task HasqlPool.UsageError result
run connection session = do
  case connection of
    MockConnection ->
      Task.yield defaultValue
    Connection pool -> do
      result <- HasqlPool.use pool session |> Task.fromIO |> Task.map Result.fromEither
      case result of
        Result.Err err -> do
          Task.throw err
        Result.Ok res -> do
          Task.yield res


runConnection ::
  (Default result) =>
  HasqlConnection.Connection ->
  Session.Session result ->
  Task Session.SessionError result
runConnection connection session = do
  result <- Session.run session connection |> Task.fromIO |> Task.map Result.fromEither
  case result of
    Result.Err err -> do
      Task.throw err
    Result.Ok res -> do
      Task.yield res


createEventsTableSession :: Session.Session Unit
createEventsTableSession =
  Session.sql
    [fmt|
            CREATE TABLE IF NOT EXISTS Events (
                eventId UUID NOT NULL,
                globalPosition BIGSERIAL NOT NULL,
                localPosition BIGINT NOT NULL,
                inlinedStreamId VARCHAR(4000) NOT NULL,
                entity VARCHAR(255) NOT NULL,
                eventData JSONB NOT NULL,
                metadata JSONB NULL,
                CONSTRAINT PK_Events PRIMARY KEY (GlobalPosition),
                CONSTRAINT UK_Events_EventId UNIQUE (EventId),
                CONSTRAINT UK_Events_Stream UNIQUE (Entity, InlinedStreamId, LocalPosition)
            );
            DO $$
            BEGIN
              IF NOT EXISTS (SELECT 1 FROM Events LIMIT 1) THEN
                ALTER SEQUENCE events_globalposition_seq MINVALUE 0 RESTART WITH 0;
              END IF;
            END $$;
          |]


dropEventsTableSession :: Session.Session Unit
dropEventsTableSession =
  Session.sql
    [fmt|
            DROP TABLE IF EXISTS Events
          |]


createEventNotificationTriggerFunctionSession :: Session.Session Unit
createEventNotificationTriggerFunctionSession =
  Session.sql
    [fmt|
            CREATE OR REPLACE FUNCTION notify_event_inserted() RETURNS trigger AS $$
            BEGIN
              PERFORM pg_notify(
                NEW.InlinedStreamId,
                json_build_object(
                  'eventId', NEW.eventId,
                  'globalPosition', NEW.globalPosition,
                  'localPosition', NEW.localPosition,
                  'inlinedStreamId', NEW.inlinedStreamId,
                  'eventData', NEW.eventData,
                  'metadata', NEW.metadata
                )::text
              );
              PERFORM pg_notify(
                'global',
                json_build_object(
                  'eventId', NEW.eventId,
                  'globalPosition', NEW.globalPosition,
                  'localPosition', NEW.localPosition,
                  'inlinedStreamId', NEW.inlinedStreamId,
                  'entity', NEW.entity,
                  'eventData', NEW.eventData,
                  'metadata', NEW.metadata
                )::text
              );
              RETURN NEW;
            END;
            $$ LANGUAGE plpgsql;
          |]


createEventNotificationTriggerSession :: Session.Session Unit
createEventNotificationTriggerSession =
  Session.sql
    [fmt|
            DO $$
            BEGIN
              IF NOT EXISTS (
                SELECT 1 FROM pg_trigger
                WHERE tgname = 'notify_event_insert'
              ) THEN
                CREATE TRIGGER notify_event_insert
                AFTER INSERT ON Events
                FOR EACH ROW
                EXECUTE FUNCTION notify_event_inserted();
              END IF;
            END $$;
          |]


selectExistingIdsSession :: Array Uuid -> Session.Session (Array Uuid)
selectExistingIdsSession ids = do
  let s :: Hasql.Statement (Vector UUID.UUID) (Vector UUID.UUID) =
        [TH.vectorStatement|
    SELECT EventId :: uuid
    FROM Events
    WHERE EventId = ANY ($1 :: uuid[])
  |]
  let params = toLegacyUuids ids
  Session.statement params s
    |> Mappable.map fromLegacyUuids


toLegacyUuids :: Array Uuid -> Vector UUID.UUID
toLegacyUuids ids =
  ids
    |> Array.map (Uuid.toLegacy)
    |> Array.unwrap


fromLegacyUuids :: Vector UUID.UUID -> Array Uuid
fromLegacyUuids legacyIds =
  legacyIds
    |> Array.fromLegacy
    |> Array.map (Uuid.fromLegacy)


selectLatestEventInStream ::
  EntityName ->
  StreamId ->
  Session.Session (Maybe (StreamPosition, StreamPosition))
selectLatestEventInStream (EntityName entityName) (StreamId streamIdText) = do
  let s :: Hasql.Statement (Text, Text) (Maybe (Int64, Int64)) =
        [TH.maybeStatement|
    SELECT GlobalPosition :: int8, LocalPosition :: int8
    FROM Events
    WHERE Entity = $1 :: text AND InlinedStreamId = $2 :: text
    ORDER BY LocalPosition DESC
    LIMIT 1
  |]
  let params = (entityName, streamIdText)
  Session.statement params s
    |> Mappable.map
      ( Maybe.map \(globalPos, localPos) ->
          (StreamPosition globalPos, StreamPosition localPos)
      )


selectMaxGlobalPosition :: Session.Session (Maybe StreamPosition)
selectMaxGlobalPosition = do
  let s :: Hasql.Statement () (Maybe Int64) =
        [TH.singletonStatement|
    SELECT MAX(GlobalPosition) :: int8?
    FROM Events
  |]
  Session.statement () s
    |> Mappable.map (Maybe.map StreamPosition)


selectInsertedEvent ::
  Uuid ->
  Session.Session (Maybe (StreamPosition, StreamPosition))
selectInsertedEvent eventId = do
  let s :: Hasql.Statement UUID.UUID (Maybe (Int64, Int64)) =
        [TH.maybeStatement|
    SELECT GlobalPosition :: int8, LocalPosition :: int8
    FROM Events
    WHERE EventId = $1 :: uuid
    ORDER BY LocalPosition DESC
    LIMIT 1
  |]
  let params = Uuid.toLegacy eventId
  Session.statement params s
    |> Mappable.map
      ( Maybe.map \(globalPos, localPos) ->
          (StreamPosition globalPos, StreamPosition localPos)
      )


insertRecordsIntoStream ::
  Array EventInsertionRecord ->
  Session.Session Unit
insertRecordsIntoStream events = do
  let statement :: Statement (Vector EventInsertionRecord) Unit =
        Statement sql encoder decoder True
       where
        sql =
          "INSERT INTO Events (EventId, LocalPosition, InlinedStreamId, Entity, EventData, Metadata) SELECT * FROM UNNEST ($1, $2, $3, $4, $5, $6)"
        encoder =
          extractFields
            >$< Contravariant.contrazip6
              (Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.uuid))))
              (Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.int8))))
              (Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.text))))
              (Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.text))))
              (Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.jsonb))))
              (Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.jsonb))))
        decoder =
          Decoders.noResult
        extractFields vec =
          Vector.unzip6
            ( vec |> Mappable.map \(record :: EventInsertionRecord) ->
                ( Uuid.toLegacy record.eventId,
                  record.localPosition,
                  record.inlinedStreamId,
                  record.entity,
                  record.eventData,
                  record.metadata
                )
            )
  let vectorEvents = events |> Array.unwrap
  Session.statement vectorEvents statement


selectEventBatch ::
  Var Int64 ->
  Maybe RelativePosition ->
  Maybe ReadDirection ->
  Maybe (Array EntityName) ->
  Task PostgresStoreError (Session.Session (Array (PostgresEventRecord)))
selectEventBatch positionRef relative readDirection entityNames = do
  let direction = toPostgresDirection relative readDirection
  let positionComparison = toPostgresGlobalPositionComparison readDirection
  position <- Var.get positionRef
  let entityFilters = toPostgresEntityFilters entityNames
  let positionFilter :: Text = [fmt|GlobalPosition #{positionComparison} #{position}|]
  let query :: Text =
        [fmt|
            SELECT EventId, GlobalPosition, LocalPosition, InlinedStreamId, Entity, EventData, Metadata
            FROM Events
            WHERE #{positionFilter}#{entityFilters}
            ORDER BY GlobalPosition #{direction}
            LIMIT #{batchSize}
          |]
  let encoder = Encoders.noParams
  let decoder = Decoders.rowVector PostgresEventRecord.rowDecoder
  let statement :: Statement () (Array PostgresEventRecord) =
        Statement (query |> Text.toBytes |> Bytes.unwrap) encoder decoder True
          |> Mappable.map Array.fromLegacy
  Session.statement unit statement |> Task.yield


selectStreamEventBatch ::
  Var Int64 ->
  EntityName ->
  StreamId ->
  Maybe RelativePosition ->
  Maybe ReadDirection ->
  Task PostgresStoreError (Session.Session (Array (PostgresEventRecord)))
selectStreamEventBatch positionRef (EntityName entityName) (StreamId streamIdText) relative readDirection = do
  let direction = toPostgresDirection relative readDirection
  position <- Var.get positionRef
  let positionComparison = toPostgresLocalPositionComparison readDirection
  -- Build query with parameterized values ($1, $2) for user-controlled inputs
  -- Direction and position filter use safe internal enum values (not user input)
  let positionFilter :: Text
      positionFilter =
        case relative of
          Just Start -> ""
          Just End -> ""
          _ -> [fmt| AND LocalPosition #{positionComparison} #{position}|]
  let query :: Text =
        [fmt|
            SELECT EventId, GlobalPosition, LocalPosition, InlinedStreamId, Entity, EventData, Metadata
            FROM Events
            WHERE Entity = $1 AND InlinedStreamId = $2#{positionFilter}
            ORDER BY LocalPosition #{direction}
            LIMIT #{batchSize}
          |]
  let encoder =
        (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
          <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
  let decoder = Decoders.rowVector PostgresEventRecord.rowDecoder
  let params = (entityName, streamIdText)
  let statement :: Statement (Text, Text) (Array PostgresEventRecord) =
        Statement (query |> Text.toBytes |> Bytes.unwrap) encoder decoder True
          |> Mappable.map Array.fromLegacy
  Session.statement params statement |> Task.yield


truncateStreamSession ::
  EntityName ->
  StreamId ->
  StreamPosition ->
  Session.Session Unit
truncateStreamSession (EntityName entityName) (StreamId streamIdText) (StreamPosition truncateBefore) = do
  let s :: Hasql.Statement (Text, Text, Int64) Unit =
        [TH.resultlessStatement|
    DELETE FROM Events
    WHERE Entity = $1 :: text
      AND InlinedStreamId = $2 :: text
      AND LocalPosition < $3 :: int8
  |]
  let params = (entityName, streamIdText, truncateBefore)
  Session.statement params s