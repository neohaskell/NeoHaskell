-- | PostgreSQL-backed QueryObjectStore for persistent query instance storage.
--
-- Mirrors the ADR-0006 SnapshotCache.InMemory -> SnapshotCache.Postgres pattern.
-- The queryobjectstore table keyed by (query_name, instance_id) with a JSON
-- payload column for the serialized query.
--
-- = Schema
--
-- @
-- CREATE TABLE queryobjectstore (
--   query_name  TEXT NOT NULL,
--   instance_id UUID NOT NULL,
--   payload     JSONB NOT NULL,
--   PRIMARY KEY (query_name, instance_id)
-- );
-- @
--
-- = Usage
--
-- @
-- import Service.EventStore.Postgres (PostgresEventStore (..))
-- import Service.QueryObjectStore.Postgres qualified as PostgresQueryStore
--
-- let dbConfig = PostgresEventStore { ... }
-- queryStore <- PostgresQueryStore.new dbConfig
-- @
module Service.QueryObjectStore.Postgres (
  PostgresQueryObjectStoreConfig (..),
  createFromPostgresConfig,
) where

import Array (Array)
import Array qualified
import Basics
import Bytes qualified
import Data.Functor.Contravariant ((>$<))
import Data.Semigroup ((<>))
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pool qualified as HasqlPool
import Hasql.Pool.Config qualified as HasqlPoolConfig
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))
import Json qualified
import Maybe (Maybe (..))
import Prelude qualified
import Result (Result (..), fromEither)
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..), QueryObjectStoreConfig (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Uuid (Uuid)
import Uuid qualified


-- | Configuration for creating a PostgreSQL-backed QueryObjectStore.
--
-- This mirrors PostgresEventStore fields so you can reuse the same
-- database connection settings.
data PostgresQueryObjectStoreConfig = PostgresQueryObjectStoreConfig
  { host :: Text
  , port :: Int
  , databaseName :: Text
  , user :: Text
  , password :: Text
  }


-- | Implementation of QueryObjectStoreConfig for Postgres.
--
-- Creates a connection pool, initializes the table, and returns a working store.
instance QueryObjectStoreConfig PostgresQueryObjectStoreConfig where
  createQueryObjectStore config = do
    let pgConfig =
          PostgresEventStore
            { host = config.host
            , port = config.port
            , databaseName = config.databaseName
            , user = config.user
            , password = config.password
            }
    createFromPostgresConfig pgConfig


-- | Create a QueryObjectStore from a PostgresEventStore config.
createFromPostgresConfig ::
  (Json.FromJSON query, Json.ToJSON query) =>
  PostgresEventStore ->
  Task Text (QueryObjectStore query)
createFromPostgresConfig config = do
  pool <- createPool config
    |> Task.mapError (\err -> [fmt|Failed to create database pool: #{err}|])

  initializeTable pool
    |> Task.mapError (\err -> [fmt|Failed to initialize table: #{err}|])

  Task.yield
    QueryObjectStore
      { get = getImpl pool
      , atomicUpdate = atomicUpdateImpl pool
      , getAll = getAllImpl pool
      }


-- Internal: create a connection pool from PostgresEventStore config
createPool :: PostgresEventStore -> Task Text HasqlPool.Pool
createPool cfg = do
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
  HasqlPool.acquire poolConfig
    |> Task.fromIO


-- Internal: run a session on the pool
runPool :: HasqlPool.Pool -> Session.Session a -> Task Error a
runPool pool session = do
  result <-
    HasqlPool.use pool session
      |> Task.fromIO
      |> Task.map fromEither
  case result of
    Ok val -> Task.yield val
    Err err -> Task.throw (StorageError [fmt|Database error: #{show err}|])


-- Internal: create the queryobjectstore table if it doesn't exist
initializeTable :: HasqlPool.Pool -> Task Error ()
initializeTable pool = do
  let sql = "CREATE TABLE IF NOT EXISTS queryobjectstore ( query_name TEXT NOT NULL, instance_id UUID NOT NULL, payload JSONB NOT NULL, PRIMARY KEY (query_name, instance_id) );"
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) Encoders.noParams Decoders.noResult True
  runPool pool (Session.statement () statement)
  -- Truncate leftover data from previous runs (safe on empty table).
  -- In production, the table is empty on first start and rebuild repopulates.
  let truncateSql = "TRUNCATE queryobjectstore"
  let truncateStmt = Statement (Text.toBytes truncateSql |> Bytes.unwrap) Encoders.noParams Decoders.noResult True
  runPool pool (Session.statement () truncateStmt)


-- Internal: row type for database results
data QueryRow = QueryRow
  { rowInstanceId :: Uuid
  , rowPayload :: Text
  }
  deriving (Eq, Show)


-- Internal: decoder for query rows
queryRowDecoder :: Decoders.Row QueryRow
queryRowDecoder = do
  rawUuid <- Decoders.column (Decoders.nonNullable Decoders.uuid)
  payload <- Decoders.column (Decoders.nonNullable Decoders.text)
  Prelude.pure (QueryRow (Uuid.fromLegacy rawUuid) payload)


-- | Get a query instance by its ID.
getImpl :: forall query. (Json.FromJSON query) => HasqlPool.Pool -> Uuid -> Task Error (Maybe query)
getImpl pool queryId = do
  let sql = "SELECT instance_id, payload::text FROM queryobjectstore WHERE instance_id = $1"
  let encoder = Encoders.param (Encoders.nonNullable Encoders.uuid)
  let decoder = Decoders.rowMaybe queryRowDecoder
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) encoder decoder True
  maybeRow <- runPool pool (Session.statement (Uuid.toLegacy queryId) statement)
  case maybeRow of
    Nothing -> Task.yield Nothing
    Just row -> do
      case Json.decodeText row.rowPayload of
        Err _ -> Task.throw (SerializationError [fmt|Failed to deserialize query #{queryId}|])
        Ok value -> Task.yield (Just value)


-- | Atomically update a query instance.
atomicUpdateImpl :: forall query. (Json.FromJSON query, Json.ToJSON query) => HasqlPool.Pool -> Uuid -> (Maybe query -> Maybe query) -> Task Error Unit
atomicUpdateImpl pool queryId updateFn = do
  let selectSql = "SELECT instance_id, payload::text FROM queryobjectstore WHERE instance_id = $1 FOR UPDATE"
  let selectEncoder = Encoders.param (Encoders.nonNullable Encoders.uuid)
  let selectDecoder = Decoders.rowMaybe queryRowDecoder
  let selectStatement = Statement (Text.toBytes selectSql |> Bytes.unwrap) selectEncoder selectDecoder True

  let upsertSql = "INSERT INTO queryobjectstore (query_name, instance_id, payload) VALUES ('default', $1, $2::jsonb) ON CONFLICT (query_name, instance_id) DO UPDATE SET payload = EXCLUDED.payload"
  let upsertEncoder =
        (Prelude.fst >$< Encoders.param (Encoders.nonNullable Encoders.uuid))
        <> (Prelude.snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
  let upsertDecoder = Decoders.noResult
  let upsertStatement = Statement (Text.toBytes upsertSql |> Bytes.unwrap) upsertEncoder upsertDecoder True

  let deleteSql = "DELETE FROM queryobjectstore WHERE instance_id = $1"
  let deleteEncoder = Encoders.param (Encoders.nonNullable Encoders.uuid)
  let deleteDecoder = Decoders.noResult
  let deleteStatement = Statement (Text.toBytes deleteSql |> Bytes.unwrap) deleteEncoder deleteDecoder True

  -- Execute within a transaction
  let transactionSession :: Session.Session ()
      transactionSession = do
        Session.sql "BEGIN"
        maybeRow <- Session.statement (Uuid.toLegacy queryId) selectStatement
        let currentValue :: Maybe query
            currentValue = case maybeRow of
              Nothing -> Nothing
              Just row -> case Json.decodeText row.rowPayload of
                Ok val -> Just val
                Err _ -> Nothing
        let newValue = updateFn currentValue
        case newValue of
          Just query -> do
            let payload = Json.encodeText query
            Session.statement (Uuid.toLegacy queryId, payload) upsertStatement
          Nothing ->
            Session.statement (Uuid.toLegacy queryId) deleteStatement
        Session.sql "COMMIT"

  runPool pool transactionSession


-- | Get all query instances.
getAllImpl :: forall query. (Json.FromJSON query) => HasqlPool.Pool -> Task Error (Array query)
getAllImpl pool = do
  let sql = "SELECT instance_id, payload::text FROM queryobjectstore"
  let encoder = Encoders.noParams
  let decoder = Decoders.rowList queryRowDecoder
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) encoder decoder True
  rows <- runPool pool (Session.statement () statement)
  rows
    |> Array.fromLinkedList
    |> Array.reduce
      (\row acc -> case Json.decodeText row.rowPayload of
        Ok val -> acc |> Array.push val
        Err _ -> acc
      )
      Array.empty
    |> Task.yield
