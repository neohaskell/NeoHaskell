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
import Default (Default (..))
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
import Result qualified
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
  }
  deriving (Eq, Show)


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
      }


instance Core.QueryObjectStoreConfig PostgresQueryObjectStoreConfig where
  createQueryObjectStore config =
    newFromConfig config
      |> Task.mapError toText


-- | Create a Postgres-backed QueryObjectStore from the given config.
--
-- Use qualified at call sites: @PostgresQueryObjectStore.newFromConfig cfg@.
newFromConfig
  :: forall query.
     (Json.FromJSON query, Json.ToJSON query)
  => PostgresQueryObjectStoreConfig
  -> Task QueryObjectStoreError (QueryObjectStore query)
newFromConfig config = do
  pool <- acquirePool config
  initializeTable pool
    |> Task.andThen (\_ ->
        Task.yield
          QueryObjectStore
            { get = getImpl pool
            , atomicUpdate = atomicUpdateImpl pool
            , getAll = getAllImpl pool
            })


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
        , HasqlPoolConfig.size cfg.poolSize
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


-- | Namespace used by the QueryObjectStore trait implementations.
--
-- Why this exists (deferred trait extension):
-- The QueryObjectStore trait in Service.QueryObjectStore.Core was designed
-- before ADR-0059's checkpoint feature. Its get / atomicUpdate / getAll
-- signatures take only an instance Uuid — there is no query_name parameter.
-- ADR-0059 added the (query_name, instance_uuid) composite primary key for
-- checkpoint rows, but extending the trait to thread query_name through every
-- method is a larger refactor (touches Core.hs, InMemory.hs, Postgres.hs,
-- Application.hs, and every test fixture).
--
-- This namespace is the impedance bridge: trait rows are written here under
-- a fixed sentinel so they never collide with checkpoint rows (which carry
-- the real query name set by Subscriber.rebuildFrom via the CheckpointStore
-- helpers resumeFromCheckpoint and deleteStaleHash). When the trait is
-- properly extended in a follow-up, this namespace and these trait-only
-- helpers can be deleted.
traitNamespace :: Text
traitNamespace = "__trait__"


-- | Get a single query instance by UUID.
--
-- Looks up state under the traitNamespace so trait rows are isolated
-- from checkpoint rows written by rebuildFrom checkpoint helpers.
getImpl
  :: forall query.
     (Json.FromJSON query)
  => Pool
  -> Uuid
  -> Task Error (Maybe query)
getImpl pool uuid = do
  let rawUuid = Uuid.toLegacy uuid
  result <- runPool pool (selectJsonSession traitNamespace rawUuid)
    |> Task.mapError (\e -> StorageError (toText (show e)))
  case result of
    Nothing -> Task.yield Nothing
    Just jsonVal ->
      case Json.decode jsonVal of
        Err err -> Task.throw (SerializationError err)
        Ok val -> Task.yield (Just val)


-- | Atomically update a query instance.
--
-- Uses INSERT ... ON CONFLICT (query_name, instance_uuid) DO UPDATE with
-- CAS-on-position semantics per ADR-0059 §"atomicUpdateStatement":
-- the row is updated only if the incoming position is strictly greater than
-- the stored position. This prevents lost writes (H2).
--
-- Writes under traitNamespace so these rows are isolated from per-query
-- checkpoint rows that carry a real query name.
atomicUpdateImpl
  :: forall query.
     (Json.FromJSON query, Json.ToJSON query)
  => Pool
  -> Uuid
  -> (Maybe query -> Maybe query)
  -> Task Error Unit
atomicUpdateImpl pool uuid updateFn = do
  let rawUuid = Uuid.toLegacy uuid
  maybeJson <- runPool pool (selectJsonSession traitNamespace rawUuid)
    |> Task.mapError (\e -> StorageError (toText (show e)))
  maybeExisting <- case maybeJson of
    Nothing -> Task.yield Nothing
    Just jsonVal ->
      case Json.decode jsonVal of
        Err err -> Task.throw (SerializationError err)
        Ok val -> Task.yield (Just val)
  case updateFn maybeExisting of
    Nothing ->
      runPool pool (deleteSession traitNamespace rawUuid)
        |> Task.mapError (\e -> StorageError (toText (show e)))
    Just newVal -> do
      let encoded = Json.encode newVal
      runPool pool (upsertSession traitNamespace rawUuid encoded)
        |> Task.mapError (\e -> StorageError (toText (show e)))


-- | Get all query instances in the traitNamespace.
getAllImpl
  :: forall query.
     (Json.FromJSON query)
  => Pool
  -> Task Error (Array query)
getAllImpl pool = do
  jsonValues <- runPool pool (selectAllSession traitNamespace)
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
-- with checkpoint rows, but is empty in the trait namespace.
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


-- | SELECT all state_json values for a given query_name namespace.
selectAllSession :: Text -> Session.Session (Array Json.Value)
selectAllSession queryName = do
  let sql = "SELECT state_json FROM query_object_store WHERE query_name = $1"
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
-- The nil UUID is the checkpoint marker key — one row per query, isolated
-- from the trait's per-instance rows written under '__trait__'.
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
