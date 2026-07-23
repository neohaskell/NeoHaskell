module Service.QueryObjectStore.Postgres (
  PostgresQueryObjectStoreConfig (..),
  QueryObjectStoreError (..),
  CheckpointStore (..),
  newFromConfig,
  createCheckpointStore,
) where

import Array (Array)
import Array qualified
import Basics
import Data.Functor.Contravariant ((>$<))
import Data.Semigroup ((<>))
import Data.Tuple (fst, snd)
import Data.UUID qualified as UUID
import Default (Default)
import Default qualified
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pool (Pool)
import Hasql.Pool qualified as HasqlPool
import Hasql.Session qualified as Session
import Hasql.Statement (Statement (..))
import Json qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Result qualified
import Service.Infra.Postgres.ConnectionConfig qualified as ConnectionConfig
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..))
import Service.QueryObjectStore.Core qualified as Core
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
    -- | Connection-pool size for the QueryObjectStore pool. Defaults to 4,
    -- sized for Azure Flexible Server B1ms (~35 usable connections); see
    -- ADR-0060 for the aggregate budget. Raise per deployment tier via this
    -- field.
  , poolSize :: Int
    -- WI-5 (#684): optional TLS hardening. Both default to off.
  , sslMode :: ConnectionConfig.SslMode
  , sslRootCert :: Maybe Text
  }
  deriving (Eq)


-- | Default config: connection-field placeholders plus the ADR-0060 B1ms
-- default pool size of 4. Use record-update syntax to set the real
-- connection fields, e.g. @def { host = "...", databaseName = "..." }@.
instance Default PostgresQueryObjectStoreConfig where
  def =
    PostgresQueryObjectStoreConfig
      { host = ""
      , databaseName = ""
      , user = ""
      , password = ""
      , port = 5432
      , poolSize = 4
      , sslMode = ConnectionConfig.SslModeUnset
      , sslRootCert = Nothing
      }


instance Core.QueryObjectStoreConfig PostgresQueryObjectStoreConfig where
  createQueryObjectStore config queryName =
    newFromConfig config queryName
      |> Task.mapError toText


-- | Create a Postgres-backed QueryObjectStore from the given config.
--
-- The @queryName@ (@NameOf query@) is threaded into every row this store
-- reads/writes, so multiple queries over the same entity persist independently
-- under the @(query_name, instance_uuid)@ primary key (#734 / ADR-0070).
--
-- Use qualified at call sites: @PostgresQueryObjectStore.newFromConfig cfg name@.
newFromConfig
  :: forall query.
     (Json.FromJSON query, Json.ToJSON query)
  => PostgresQueryObjectStoreConfig
  -> Text
  -> Task QueryObjectStoreError (QueryObjectStore query)
newFromConfig config queryName = do
  pool <- acquirePool config
  initializeTable pool
    |> Task.andThen (\_ ->
        Task.yield
          QueryObjectStore
            { get = getImpl pool queryName
            , atomicUpdate = atomicUpdateImpl pool queryName
            , getAll = getAllImpl pool queryName
            })


-- | Acquire a Hasql connection pool and verify connectivity.
acquirePool :: PostgresQueryObjectStoreConfig -> Task QueryObjectStoreError Pool
acquirePool cfg = do
  let size = cfg.poolSize
  case size > 0 of
    False ->
      Task.throw (ConnectionFailed [fmt|poolSize must be > 0, got #{size}|])
    True ->
      case ConnectionConfig.toConnectionParams
             ConnectionConfig.ConnectionParams
               { host = cfg.host
               , databaseName = cfg.databaseName
               , user = cfg.user
               , password = cfg.password
               , port = cfg.port
               , sslMode = cfg.sslMode
               , sslRootCert = cfg.sslRootCert
               } of
        Err portErr -> Task.throw (ConnectionFailed portErr)
        Ok settings -> do
          let poolConfig = ConnectionConfig.toPoolConfig cfg.poolSize settings
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


-- | Initialize the query_object_store table if it doesn't exist.
--
-- Schema per ADR-0059: includes query_name (sharding key), instance_uuid,
-- query_hash (schema evolution), position (resume point), state_json, updated_at.
-- Primary key is (query_name, instance_uuid) to support per-tenant/per-instance sharding.
initializeTable :: Pool -> Task QueryObjectStoreError ()
initializeTable pool = do
  runPool pool createTableSession
    |> Task.mapError (\e -> StatementFailed (toText (show e)))


-- | Create the query_object_store table — ADR-0059 schema.
--
-- Uses CREATE TABLE IF NOT EXISTS so this is additive and idempotent:
-- safe to run on every startup without destroying persisted query state.
--
-- PRIMARY KEY (query_name, instance_uuid) supports per-query sharding.
-- query_hash enables hash-mismatch detection for schema evolution (H5).
-- position enables CAS-on-position for lost-write prevention (H2).
--
-- Note: If a pre-existing deployment has a stale schema (e.g., missing
-- columns), a one-time manual migration is required. This PR's scope is
-- correct fresh creation; schema evolution is out of scope.
--
-- Test suites that need a clean table per run should TRUNCATE or DROP/recreate
-- via their own setup hooks — production code must never do a DROP.
createTableSession :: Session.Session ()
createTableSession =
  Session.sql
    [fmt|
      CREATE TABLE IF NOT EXISTS query_object_store (
        query_name    TEXT        NOT NULL,
        instance_uuid UUID        NOT NULL,
        query_hash    TEXT        NOT NULL,
        position      BIGINT      NOT NULL,
        state_json    JSONB       NOT NULL,
        updated_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
        PRIMARY KEY (query_name, instance_uuid)
      );
    |]


-- | Get a single query instance by UUID.
--
-- Looks up state under the store's own query name (@NameOf query@), so distinct
-- queries over the same entity never read each other's rows — the collision the
-- @"__trait__"@ sentinel used to cause (#734 / ADR-0070). The name matches the
-- one @Subscriber.rebuildFrom@ writes on the checkpoint pathway, so trait state
-- and checkpoints now share one namespace per query, as the
-- @(query_name, instance_uuid)@ PK intends.
getImpl
  :: forall query.
     (Json.FromJSON query)
  => Pool
  -> Text
  -> Uuid
  -> Task Error (Maybe query)
getImpl pool queryName uuid = do
  let rawUuid = Uuid.toLegacy uuid
  result <- runPool pool (selectJsonSession queryName rawUuid)
    |> Task.mapError (\e -> StorageError (toText (show e)))
  case result of
    Nothing -> Task.yield Nothing
    Just jsonVal ->
      case Json.decode jsonVal of
        Err err -> Task.throw (SerializationError err)
        Ok val -> Task.yield (Just val)


-- | Atomically update a query instance.
--
-- Uses INSERT ... ON CONFLICT (query_name, instance_uuid) DO UPDATE, keyed by
-- the store's own query name (@NameOf query@). Concurrent updates to distinct
-- queries over the same entity therefore never overwrite each other — the
-- lost-write / cross-query collision the @"__trait__"@ sentinel caused (#734).
atomicUpdateImpl
  :: forall query.
     (Json.FromJSON query, Json.ToJSON query)
  => Pool
  -> Text
  -> Uuid
  -> (Maybe query -> Maybe query)
  -> Task Error Unit
atomicUpdateImpl pool queryName uuid updateFn = do
  let rawUuid = Uuid.toLegacy uuid
  maybeJson <- runPool pool (selectJsonSession queryName rawUuid)
    |> Task.mapError (\e -> StorageError (toText (show e)))
  maybeExisting <- case maybeJson of
    Nothing -> Task.yield Nothing
    Just jsonVal ->
      case Json.decode jsonVal of
        Err err -> Task.throw (SerializationError err)
        Ok val -> Task.yield (Just val)
  case updateFn maybeExisting of
    Nothing ->
      runPool pool (deleteSession queryName rawUuid)
        |> Task.mapError (\e -> StorageError (toText (show e)))
    Just newVal -> do
      let encoded = Json.encode newVal
      runPool pool (upsertSession queryName rawUuid encoded)
        |> Task.mapError (\e -> StorageError (toText (show e)))


-- | Get all instances stored under the store's own query name (@NameOf query@).
--
-- Returns only this query's rows, not every query's rows sharing the table —
-- the @getAll@ leakage the @"__trait__"@ sentinel caused (#734).
getAllImpl
  :: forall query.
     (Json.FromJSON query)
  => Pool
  -> Text
  -> Task Error (Array query)
getAllImpl pool queryName = do
  jsonValues <- runPool pool (selectAllSession queryName)
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


-- | SELECT state_json ... WHERE query_name = ? AND instance_uuid = ?
selectJsonSession :: Text -> UUID.UUID -> Session.Session (Maybe Json.Value)
selectJsonSession queryName rawUuid = do
  let sql = "SELECT state_json FROM query_object_store WHERE query_name = $1 AND instance_uuid = $2 LIMIT 1"
  let encoder =
        (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
          <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.uuid))
  let decoder = Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.jsonb))
  let stmt = Statement (sql |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  Session.statement (queryName, rawUuid) stmt


-- | DELETE FROM query_object_store WHERE query_name = ? AND instance_uuid = ?
deleteSession :: Text -> UUID.UUID -> Session.Session ()
deleteSession queryName rawUuid = do
  let sql = "DELETE FROM query_object_store WHERE query_name = $1 AND instance_uuid = $2"
  let encoder =
        (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
          <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.uuid))
  let decoder = Decoders.noResult
  let stmt = Statement (sql |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  Session.statement (queryName, rawUuid) stmt


-- | UPSERT into query_object_store for the trait's @atomicUpdate@.
--
-- Note: the trait API carries no position, so this write does NOT apply
-- CAS-on-position. (Doing so with a hardcoded position = 0 would make
-- every conflict branch a silent no-op — the bug fixed in this revision.)
-- Hazard H2 (lost-write CAS) is enforced on the checkpoint pathway via
-- 'CheckpointStore' helpers, which carry a real advancing position.
-- query_hash is stored alongside state to keep the row shape consistent
-- with checkpoint rows, but is empty for these per-instance rows — only the
-- checkpoint marker (nil UUID) carries a real hash.
upsertSession :: Text -> UUID.UUID -> Json.Value -> Session.Session ()
upsertSession queryName rawUuid jsonVal = do
  let sql =
        "INSERT INTO query_object_store \
        \  (query_name, instance_uuid, query_hash, position, state_json, updated_at) \
        \VALUES ($1, $2, '', 0, $3, now()) \
        \ON CONFLICT (query_name, instance_uuid) DO UPDATE \
        \SET state_json  = EXCLUDED.state_json, \
        \    updated_at  = now()"
  let encoder =
        ((\(a, _, _) -> a) >$< Encoders.param (Encoders.nonNullable Encoders.text))
          <> ((\(_, b, _) -> b) >$< Encoders.param (Encoders.nonNullable Encoders.uuid))
          <> ((\(_, _, c) -> c) >$< Encoders.param (Encoders.nonNullable Encoders.jsonb))
  let decoder = Decoders.noResult
  let stmt = Statement (sql |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  Session.statement (queryName, rawUuid, jsonVal) stmt


-- | SELECT all per-instance state_json values for a query name.
--
-- Excludes the reserved nil-UUID checkpoint marker row (see
-- 'writeCheckpointSession'): trait rows and the checkpoint marker now share one
-- query_name (#734 / ADR-0070), so the marker's placeholder @'{}'@ state must
-- not leak into getAll — it is not a query instance.
selectAllSession :: Text -> Session.Session (Array Json.Value)
selectAllSession queryName = do
  let sql =
        "SELECT state_json FROM query_object_store \
        \WHERE query_name = $1 \
        \  AND instance_uuid <> '00000000-0000-0000-0000-000000000000'"
  let encoder = Encoders.param (Encoders.nonNullable Encoders.text)
  let decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.jsonb))
  let stmt = Statement (sql |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  rows <- Session.statement queryName stmt
  rows |> Array.fromLinkedList |> pure


-- ---------------------------------------------------------------------------
-- Checkpoint helpers (Defect 2 / ADR-0059 §Internal helpers)
-- ---------------------------------------------------------------------------

-- | Operations for hash-aware checkpoint management.
--
-- This is separate from the QueryObjectStore trait (which handles
-- generic per-instance state). Checkpoint operations carry the
-- query_name and query_hash explicitly, which the trait API does not.
--
-- Created by createCheckpointStore from a live Pool; tests may supply
-- a mock CheckpointStore without a real Postgres connection.
data CheckpointStore = CheckpointStore
  { -- | Fetch the minimum persisted position for a query whose hash matches.
    --
    -- Returns Nothing when no rows for (queryName, queryHash) exist —
    -- i.e., the query has never been persisted or the hash has changed.
    resumeFromCheckpoint :: Text -> Text -> Task QueryObjectStoreError (Maybe Int64)
    -- | Delete all rows for a query whose hash does NOT match queryHash.
    --
    -- Called before a full replay when a schema change is detected.
  , deleteStaleHash :: Text -> Text -> Task QueryObjectStoreError Unit
    -- | Persist (query_name, query_hash, position) for checkpoint resume.
    --
    -- Uses CAS-on-position (ADR-0059 §"Concurrency & persistence", hazard H2):
    -- the row is only updated if the incoming position is strictly greater than
    -- the stored position. A nil UUID is used as the checkpoint marker row key,
    -- keeping checkpoint rows separate from per-instance trait rows.
  , writeCheckpoint :: Text -> Text -> Int64 -> Task QueryObjectStoreError Unit
  }


-- | Create a CheckpointStore backed by the given Postgres Pool.
createCheckpointStore :: Pool -> CheckpointStore
createCheckpointStore pool =
  CheckpointStore
    { resumeFromCheckpoint = resumeFromCheckpointImpl pool
    , deleteStaleHash = deleteStaleHashImpl pool
    , writeCheckpoint = writeCheckpointImpl pool
    }


-- | SELECT MIN(position) FROM query_object_store WHERE query_name = ? AND query_hash = ?
resumeFromCheckpointImpl :: Pool -> Text -> Text -> Task QueryObjectStoreError (Maybe Int64)
resumeFromCheckpointImpl pool queryName queryHash = do
  let sql = "SELECT MIN(position) FROM query_object_store WHERE query_name = $1 AND query_hash = $2"
  let encoder =
        (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
          <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
  let decoder = Decoders.singleRow (Decoders.column (Decoders.nullable Decoders.int8))
  let stmt = Statement (sql |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  runPool pool (Session.statement (queryName, queryHash) stmt)


-- | DELETE FROM query_object_store WHERE query_name = ? AND query_hash != ?
deleteStaleHashImpl :: Pool -> Text -> Text -> Task QueryObjectStoreError Unit
deleteStaleHashImpl pool queryName queryHash = do
  let sql = "DELETE FROM query_object_store WHERE query_name = $1 AND query_hash != $2"
  let encoder =
        (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
          <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
  let decoder = Decoders.noResult
  let stmt = Statement (sql |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  runPool pool (Session.statement (queryName, queryHash) stmt)


-- | Persist a checkpoint row for (query_name, query_hash, position).
--
-- The nil UUID is the checkpoint marker key — one row per query, isolated by
-- that reserved UUID from the trait's per-instance rows, which share the same
-- query_name since #734 (getAll filters the marker out).
writeCheckpointImpl :: Pool -> Text -> Text -> Int64 -> Task QueryObjectStoreError Unit
writeCheckpointImpl pool queryName queryHash position =
  runPool pool (writeCheckpointSession queryName queryHash position)


-- | INSERT ... ON CONFLICT DO UPDATE with CAS-on-position.
--
-- Only advances the stored position when the incoming position is strictly
-- greater, preventing lost-write regression (ADR-0059 hazard H2).
writeCheckpointSession :: Text -> Text -> Int64 -> Session.Session ()
writeCheckpointSession queryName queryHash position = do
  let sql =
        "INSERT INTO query_object_store \
        \  (query_name, instance_uuid, query_hash, position, state_json, updated_at) \
        \VALUES ($1, '00000000-0000-0000-0000-000000000000'::uuid, $2, $3, '{}'::jsonb, now()) \
        \ON CONFLICT (query_name, instance_uuid) DO UPDATE \
        \SET position   = EXCLUDED.position, \
        \    query_hash = EXCLUDED.query_hash, \
        \    updated_at = now() \
        \WHERE query_object_store.position < EXCLUDED.position"
  let encoder =
        ((\(a, _, _) -> a) >$< Encoders.param (Encoders.nonNullable Encoders.text))
          <> ((\(_, b, _) -> b) >$< Encoders.param (Encoders.nonNullable Encoders.text))
          <> ((\(_, _, c) -> c) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
  let decoder = Decoders.noResult
  let stmt = Statement (sql |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  Session.statement (queryName, queryHash, position) stmt
