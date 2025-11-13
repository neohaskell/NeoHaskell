module Service.EventStore.Postgres (
  Config (..),
  new,
) where

import Core
import Json qualified
import Service.EventStore.Core
import Service.EventStore.Postgres.Internal (Config (..))
import Service.EventStore.Postgres.Internal qualified as Internal


new ::
  (Json.FromJSON eventType, Json.ToJSON eventType) =>
  Config ->
  Task Text (EventStore eventType)
new config = Internal.new Internal.defaultOps config