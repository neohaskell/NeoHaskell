module Service.QueryObjectStore.Core (
  QueryObjectStore (..),
  Error (..),
  QueryObjectStoreConfig (..),
) where

import Array (Array)
import Basics
import Json qualified
import Maybe (Maybe)
import Task (Task)
import Text (Text)
import Uuid (Uuid)


-- | Errors that can occur during QueryObjectStore operations.
data Error
  = -- | Generic storage failure
    StorageError Text
  | -- | JSON serialization/deserialization failure
    SerializationError Text
  deriving (Eq, Show)


-- | Storage interface for query instances (read models).
--
-- Similar in pattern to SnapshotCache, but specifically designed for storing
-- and retrieving query instances. Each query instance is identified by a Uuid.
--
-- The key operation is 'atomicUpdate' which enables safe concurrent updates
-- to query instances when multiple entity events arrive simultaneously.
--
-- Example usage:
--
-- @
-- -- Get a specific query instance
-- maybeUserOrders <- queryStore.get userId
--
-- -- Atomically update a query instance
-- queryStore.atomicUpdate userId \\maybeExisting ->
--   case maybeExisting of
--     Just existing -> Just (existing { orderCount = existing.orderCount + 1 })
--     Nothing -> Just (UserOrders { userId = userId, orderCount = 1 })
--
-- -- Get all query instances (for HTTP endpoint)
-- allUserOrders <- queryStore.getAll
-- @
data QueryObjectStore query = QueryObjectStore
  { -- | Get a query instance by its ID.
    --
    -- Returns Nothing if no query instance exists with the given ID.
    get :: Uuid -> Task Error (Maybe query),
    -- | Atomically update a query instance.
    --
    -- The update function receives the current value (or Nothing if not exists)
    -- and returns the new value (or Nothing to delete the entry).
    --
    -- This operation is atomic to handle concurrent updates safely.
    -- When multiple entity events arrive that affect the same query instance,
    -- this ensures updates are applied sequentially without data loss.
    --
    -- To delete an entry, return Nothing from the update function.
    atomicUpdate :: Uuid -> (Maybe query -> Maybe query) -> Task Error Unit,
    -- | Get all query instances.
    --
    -- Used primarily for the HTTP endpoint: GET /queries/{query-name}
    -- Returns all stored query instances as an array.
    getAll :: Task Error (Array query)
  }


-- | Configuration typeclass for creating QueryObjectStore instances.
class QueryObjectStoreConfig config where
  createQueryObjectStore ::
    (Json.FromJSON query, Json.ToJSON query) =>
    config ->
    Task Text (QueryObjectStore query)
