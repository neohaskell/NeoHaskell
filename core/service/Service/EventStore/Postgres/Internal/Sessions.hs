module Service.EventStore.Postgres.Internal.Sessions where

import Core
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Result qualified
import Task qualified


data Connection
  = Connection Hasql.Connection
  | MockConnection


run :: (Default result) => Session.Session result -> Connection -> Task Session.SessionError result
run session connection = do
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
