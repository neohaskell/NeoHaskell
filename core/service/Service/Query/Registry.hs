module Service.Query.Registry (
  QueryRegistry,
  QueryUpdater (..),
  empty,
  isEmpty,
  register,
  getUpdatersForEntity,
) where

import Array (Array)
import Array qualified
import Basics
import Json qualified
import Map (Map)
import Map qualified
import Maybe qualified
import Service.Event (Event)
import Service.Event.EntityName (EntityName)
import Task (Task)
import Text (Text)


-- | A function that updates a query when an entity changes.
-- Receives the raw event (Event Json.Value) and handles deserialization,
-- entity reconstruction, and query update internally.
data QueryUpdater = QueryUpdater
  { queryName :: Text,
    updateQuery :: Event Json.Value -> Task Text Unit
  }


-- | Maps entity names to their associated query updaters.
newtype QueryRegistry = QueryRegistry (Map EntityName (Array QueryUpdater))


-- | Create an empty QueryRegistry.
empty :: QueryRegistry
empty = QueryRegistry Map.empty


-- | Check if the QueryRegistry has no registered updaters.
isEmpty :: QueryRegistry -> Bool
isEmpty (QueryRegistry registry) = Map.length registry == 0


-- | Register a QueryUpdater for an entity type.
-- Multiple updaters can be registered for the same entity
-- (when multiple queries depend on the same entity).
register :: EntityName -> QueryUpdater -> QueryRegistry -> QueryRegistry
register entityName updater (QueryRegistry registry) = do
  let currentUpdaters = registry |> Map.get entityName |> Maybe.withDefault Array.empty
  let newUpdaters = currentUpdaters |> Array.push updater
  QueryRegistry (registry |> Map.set entityName newUpdaters)


-- | Get all QueryUpdaters registered for a given entity type.
getUpdatersForEntity :: EntityName -> QueryRegistry -> Array QueryUpdater
getUpdatersForEntity entityName (QueryRegistry registry) =
  registry |> Map.get entityName |> Maybe.withDefault Array.empty
