module Service.EventStore.InMemory (
  new,
) where

import Core
import Json qualified
import Maybe qualified
import Path qualified
import Service.EventStore.Core
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.EventStore.Simple qualified as Simple
import Task qualified


new :: Task Error (EventStore Json.Value)
new = do
  let config =
        SimpleEventStore
          { basePath = Path.fromText ".neo/events" |> Maybe.getOrDie,
            persistent = False
          }
  Simple.new config |> Task.mapError (\t -> StorageFailure t)