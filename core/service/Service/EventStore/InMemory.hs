module Service.EventStore.InMemory (
  new,
) where

import Core.Prelude
import Json qualified
import Service.EventStore.Core
import Service.EventStore.Simple qualified as Simple
import Task qualified


new :: Task Error (EventStore Json.Value)
new = Simple.new Simple.inMemory |> Task.mapError StorageFailure
