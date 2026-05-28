module Service.QueryObjectStore.Postgres (
  PostgresQueryObjectStoreConfig (..),
  QueryObjectStoreError (..),
  createQueryObjectStore,
) where

import Basics
import Json qualified
import Service.QueryObjectStore.Core (QueryObjectStore (..))
import Service.QueryObjectStore.Core qualified as Core
import Task (Task)
import Task qualified
import Text (Text)
import ToText (toText)


-- | Errors produced by the Postgres-backed QueryObjectStore.
data QueryObjectStoreError
  = ConnectionFailed Text
    -- ^ Unable to acquire Postgres connection.
  | StatementFailed Text
    -- ^ Hasql statement execution failed.
  | DecodingFailed Text
    -- ^ Hasql result decoder failed.
  deriving (Eq, Show, Generic)


-- | Configuration for the Postgres-backed QueryObjectStore.
data PostgresQueryObjectStoreConfig = PostgresQueryObjectStoreConfig
  { host :: Text
  , databaseName :: Text
  , user :: Text
  , password :: Text
  , port :: Int
  }
  deriving (Eq, Show)


instance Core.QueryObjectStoreConfig PostgresQueryObjectStoreConfig where
  createQueryObjectStore config =
    newFromConfig config
      |> Task.mapError toText


-- | Create a Postgres-backed QueryObjectStore from the given config.
--
-- This is the public API used by tests. Delegates to the internal stub.
createQueryObjectStore
  :: forall query.
     (Json.FromJSON query, Json.ToJSON query)
  => PostgresQueryObjectStoreConfig
  -> Task QueryObjectStoreError (QueryObjectStore query)
createQueryObjectStore config = newFromConfig config


-- | Internal stub. Throws a sentinel so that every test fails against this stub.
newFromConfig
  :: forall query.
     (Json.FromJSON query, Json.ToJSON query)
  => PostgresQueryObjectStoreConfig
  -> Task QueryObjectStoreError (QueryObjectStore query)
newFromConfig _ = panic "not implemented: Service.QueryObjectStore.Postgres.createQueryObjectStore"
