module Service.QueryObjectStore.Postgres (
  PostgresQueryObjectStoreConfig (..),
  QueryObjectStoreError (..),
  createQueryObjectStore,
) where

import Array (Array)
import Array qualified
import Basics
import Data.Functor.Contravariant ((>$<))
import Data.UUID qualified as UUID
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pool (Pool)
import Hasql.Pool qualified as HasqlPool
import Hasql.Pool.Config qualified as HasqlPoolConfig
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))
import Json qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..), QueryObjectStoreConfig (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Bytes qualified
import ToText (toText)
import Uuid (Uuid)
import Uuid qualified


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


instance QueryObjectStoreConfig PostgresQueryObjectStoreConfig where
  createQueryObjectStore config =
    newFromConfig config
      |> Task.mapError toText


-- | Create a Postgres-backed QueryObjectStore from the given config.
--
-- Exported as public API; tests import this directly.
createQueryObjectStore
  :: forall query.
     (Json.FromJSON query, Json.ToJSON query)
  => PostgresQueryObjectStoreConfig
  -> Task QueryObjectStoreError (QueryObjectStore query)
createQueryObjectStore = newFromConfig


-- | Internal implementation.
newFromConfig
  :: forall query.
     (Json.FromJSON query, Json.ToJSON query)
  => PostgresQueryObjectStoreConfig
  -> Task QueryObjectStoreError (QueryObjectStore query)
newFromConfig config = do
  pool <- acquirePool config
  Task.yield
    QueryObjectStore
      { get = getImpl pool
      , atomicUpdate = atomicUpdateImpl pool
      , getAll = getAllImpl pool
      }


-- | Acquire a Hasql connection pool and verify connectivity.
acquirePool :: PostgresQueryObjectStoreConfig -> Task QueryObjectStoreError Pool
acquirePool cfg = do
  let params =
        ConnectionSettingConnection.params
          [ Param.host cfg.host
          , Param.port (fromIntegral cfg.port)
          , Param.dbname cfg.databaseName
          , Param.user cfg.user
          , Param.password cfg.password
          ]
  let settings = [params |> ConnectionSetting.connection]
  let poolConfig =
        [ HasqlPoolConfig.staticConnectionSettings settings
        , HasqlPoolConfig.agingTimeout 300
        , HasqlPoolConfig.idlenessTimeout 60
        ]
          |> HasqlPoolConfig.settings
  pool <- HasqlPool.acquire poolConfig
    |> Task.fromIO
  pingResult <- runPool pool pingSession |> Task.asResult
  case pingResult of
    Err err -> Task.throw (ConnectionFailed (toText (show err)))
    Ok _ -> Task.yield pool


-- | Run a Hasql session on the pool.
runPool
  :: Pool
  -> Session.Session a
  -> Task QueryObjectStoreError a
runPool pool session = do
  ioResult <- HasqlPool.use pool session |> Task.fromIO
  case Result.fromEither ioResult of
    Err err -> Task.throw (StatementFailed (toText (show err)))
    Ok val -> Task.yield val


-- | Trivial connectivity check.
pingSession :: Session.Session ()
pingSession = Session.sql "SELECT 1"


-- | Get a single query instance by UUID.
getImpl
  :: forall query.
     (Json.FromJSON query)
  => Pool
  -> Uuid
  -> Task Error (Maybe query)
getImpl pool uuid = do
  let rawUuid = Uuid.toLegacy uuid
  result <- runPool pool (selectJsonSession rawUuid)
    |> Task.mapError (\e -> StorageError (toText (show e)))
  case result of
    Nothing -> Task.yield Nothing
    Just jsonVal ->
      case Json.decode jsonVal of
        Err err -> Task.throw (SerializationError err)
        Ok val -> Task.yield (Just val)


-- | Atomically update a query instance.
atomicUpdateImpl
  :: forall query.
     (Json.FromJSON query, Json.ToJSON query)
  => Pool
  -> Uuid
  -> (Maybe query -> Maybe query)
  -> Task Error Unit
atomicUpdateImpl pool uuid updateFn = do
  let rawUuid = Uuid.toLegacy uuid
  maybeJson <- runPool pool (selectJsonSession rawUuid)
    |> Task.mapError (\e -> StorageError (toText (show e)))
  maybeExisting <- case maybeJson of
    Nothing -> Task.yield Nothing
    Just jsonVal ->
      case Json.decode jsonVal of
        Err err -> Task.throw (SerializationError err)
        Ok val -> Task.yield (Just val)
  case updateFn maybeExisting of
    Nothing ->
      runPool pool (deleteSession rawUuid)
        |> Task.mapError (\e -> StorageError (toText (show e)))
    Just newVal -> do
      let encoded = Json.encode newVal
      runPool pool (upsertSession rawUuid encoded)
        |> Task.mapError (\e -> StorageError (toText (show e)))


-- | Get all query instances.
getAllImpl
  :: forall query.
     (Json.FromJSON query)
  => Pool
  -> Task Error (Array query)
getAllImpl pool = do
  jsonValues <- runPool pool selectAllSession
    |> Task.mapError (\e -> StorageError (toText (show e)))
  let buildResult =
        jsonValues
          |> Array.reduce
              (\jsonVal acc ->
                case acc of
                  Err e -> Err e
                  Ok arr ->
                    case Json.decode jsonVal of
                      Err err -> Err (SerializationError err)
                      Ok val -> Ok (Array.push val arr))
              (Ok Array.empty)
  case buildResult of
    Err e -> Task.throw e
    Ok arr -> Task.yield arr


-- | SELECT state_json ... WHERE instance_uuid = ?
selectJsonSession :: UUID.UUID -> Session.Session (Maybe Json.Value)
selectJsonSession rawUuid = do
  let sql = "SELECT state_json FROM query_object_store WHERE instance_uuid = $1 LIMIT 1"
  let encoder = Encoders.param (Encoders.nonNullable Encoders.uuid)
  let decoder = Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.jsonb))
  let stmt = Statement (sql |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  Session.statement rawUuid stmt


-- | DELETE FROM query_object_store WHERE instance_uuid = ?
deleteSession :: UUID.UUID -> Session.Session ()
deleteSession rawUuid = do
  let sql = "DELETE FROM query_object_store WHERE instance_uuid = $1"
  let encoder = Encoders.param (Encoders.nonNullable Encoders.uuid)
  let decoder = Decoders.noResult
  let stmt = Statement (sql |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  Session.statement rawUuid stmt


-- | UPSERT into query_object_store.
upsertSession :: UUID.UUID -> Json.Value -> Session.Session ()
upsertSession rawUuid jsonVal = do
  let sql =
        "INSERT INTO query_object_store (instance_uuid, state_json, updated_at) \
        \VALUES ($1, $2, now()) \
        \ON CONFLICT (instance_uuid) DO UPDATE \
        \SET state_json = EXCLUDED.state_json, updated_at = now()"
  let encoder =
        (fst >$< Encoders.param (Encoders.nonNullable Encoders.uuid))
          <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.jsonb))
  let decoder = Decoders.noResult
  let stmt = Statement (sql |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  Session.statement (rawUuid, jsonVal) stmt


-- | SELECT all state_json values.
selectAllSession :: Session.Session (Array Json.Value)
selectAllSession = do
  let sql = "SELECT state_json FROM query_object_store"
  let encoder = Encoders.noParams
  let decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.jsonb))
  let stmt = Statement (sql |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  rows <- Session.statement () stmt
  rows |> Array.fromLinkedList |> pure
