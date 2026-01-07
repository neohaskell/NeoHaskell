module Service.EventStore.Postgres (
  PostgresEventStore (..),
  new,
) where

import Core
import Json qualified
import Service.EventStore.Core
import Service.EventStore.Postgres.Internal (PostgresEventStore (..))
import Service.EventStore.Postgres.Internal qualified as Internal


new ::
  PostgresEventStore ->
  Task Text (EventStore Json.Value)
new config = Internal.new Internal.defaultOps config