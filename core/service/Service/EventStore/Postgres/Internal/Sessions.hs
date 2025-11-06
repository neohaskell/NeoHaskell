module Service.EventStore.Postgres.Internal.Sessions where

import Array qualified
import Core
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Hasql
import Hasql.TH qualified as TH
import Mappable qualified
import Result qualified
import Service.Event (EntityName, StreamId, StreamPosition)
import Task qualified
import Uuid qualified


data Connection
  = Connection Hasql.Connection
  | MockConnection


run ::
  (Default result) =>
  Connection ->
  Session.Session result ->
  Task Session.SessionError result
run connection session = do
  case connection of
    MockConnection ->
      Task.yield defaultValue
    Connection conn -> do
      result <- Session.run session conn |> Task.fromIO |> Task.map Result.fromEither
      case result of
        Result.Err err ->
          Task.throw err
        Result.Ok res ->
          Task.yield res


createEventsTableSession :: Session.Session Unit
createEventsTableSession =
  Session.sql
    [fmt|
            CREATE TABLE IF NOT EXISTS Events (
                EventId UUID NOT NULL,
                GlobalPosition BIGSERIAL NOT NULL,
                LocalPosition BIGINT NOT NULL,
                InlinedStreamId VARCHAR(4000) NOT NULL,
                Entity VARCHAR(255) NOT NULL,
                EventType VARCHAR(255) NOT NULL,
                EventData BYTEA NOT NULL,
                Metadata BYTEA NULL,
                CONSTRAINT PK_Events PRIMARY KEY (GlobalPosition),
                CONSTRAINT UK_Events_EventId UNIQUE (EventId),
                CONSTRAINT UK_Events_Stream UNIQUE (Entity, InlinedStreamId, LocalPosition)
            )
          |]


selectExistingIdsSession :: Array Uuid -> Session.Session (Array Uuid)
selectExistingIdsSession ids = do
  let s :: Hasql.Statement (Vector UUID.UUID) (Vector UUID.UUID) =
        [TH.vectorStatement|
    SELECT EventId :: uuid
    FROM Events
    WHERE EventId IN ($1 :: uuid[])
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
  Session.Session (StreamPosition, StreamPosition)
selectLatestEventInStream = panic "selectLatestEventInStream : not implemented"
