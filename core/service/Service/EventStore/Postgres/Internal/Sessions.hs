module Service.EventStore.Postgres.Internal.Sessions where

import Core
import Hasql.Session qualified as Session


createEventsTableSession :: Session.Session Unit
createEventsTableSession =
  Session.sql
    [fmt|
            CREATE TABLE Events (
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
