-- | PostgreSQL-backed FileStateStore for production use.
--
-- This module provides a persistent FileStateStore implementation that
-- survives server restarts, unlike the in-memory store.
--
-- = Usage
--
-- @
-- import Service.EventStore.Postgres (PostgresEventStore (..))
-- import Service.FileUpload.FileStateStore.Postgres qualified as PostgresFileStore
--
-- -- Use the same config as your event store
-- let dbConfig = PostgresEventStore
--       { host = "localhost"
--       , port = 5432
--       , databaseName = "myapp"
--       , user = "myuser"
--       , password = "mypassword"
--       }
--
-- stateStore <- PostgresFileStore.new dbConfig
--
-- -- Use in FileUploadSetup
-- let setup = FileUploadSetup
--       { config = ...
--       , blobStore = ...
--       , stateStore = stateStore
--       }
-- @
--
-- = Schema
--
-- The module creates a @file_upload_state@ table if it doesn't exist.
-- See 'initializeTable' for the schema definition.
module Service.FileUpload.FileStateStore.Postgres (
  -- * Initialization
  new,
  newWithCleanup,

  -- * Types
  Pool,
  PostgresFileStoreError (..),

  -- * Low-level access (for cleanup workers)
  -- | These functions require a pool obtained from 'newWithCleanup'.
  -- Do NOT create pools per-call - that causes connection leaks.
  findExpiredPendingFiles,
  deleteFileState,
) where

import Basics
import Bytes qualified
import Core
import Data.Functor.Contravariant ((>$<))
import Data.Semigroup ((<>))
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Pool (Pool)
import Hasql.Pool qualified as HasqlPool
import Hasql.Pool.Config qualified as HasqlPoolConfig
import Hasql.Pool.Observation (ConnectionStatus (..), ConnectionTerminationReason (..), Observation (..))
import Hasql.Session qualified as Session
import Log qualified
import Prelude qualified
import Hasql.Statement (Statement (..))

import Result qualified
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.Core (
  BlobKey (..),
  FileRef (..),
  FileUploadEvent (..),
  OwnerHash (..),
 )
import Service.FileUpload.Lifecycle (FileUploadState (..))
import Service.FileUpload.Lifecycle qualified as Lifecycle
import Service.FileUpload.Web (FileStateStore (..))
import Task qualified
import Text qualified


-- ==========================================================================
-- Errors
-- ==========================================================================

-- | Errors that can occur when using PostgreSQL FileStateStore
data PostgresFileStoreError
  = PoolError HasqlPool.UsageError
  | DeserializationError Text
  deriving (Eq, Show)


-- ==========================================================================
-- Initialization
-- ==========================================================================

-- | Create a new PostgreSQL-backed FileStateStore.
--
-- Uses the same connection config as the PostgresEventStore, so you can
-- share database settings across your application.
--
-- Creates the file_upload_state table if it doesn't exist.
--
-- @
-- import Service.EventStore.Postgres (PostgresEventStore (..))
-- import Service.FileUpload.FileStateStore.Postgres qualified as PostgresFileStore
--
-- let dbConfig = PostgresEventStore { ... }
-- stateStore <- PostgresFileStore.new dbConfig
-- @
new :: PostgresEventStore -> Task Text FileStateStore
new config = do
  (store, _) <- newWithCleanup config
  Task.yield store


-- | Create a PostgreSQL-backed FileStateStore with access to the connection pool.
--
-- Use this when you need the pool for cleanup operations. The returned pool
-- should be reused for all cleanup calls - never create pools per-call.
--
-- @
-- (stateStore, pool) <- PostgresFileStore.newWithCleanup dbConfig
--
-- -- Use stateStore for normal operations
-- let setup = FileUploadSetup { stateStore = stateStore, ... }
--
-- -- Use pool for cleanup worker (runs periodically)
-- cleanupWorker = do
--   expired <- PostgresFileStore.findExpiredPendingFiles pool currentTime
--   for_ expired $ \\(fileRef, blobKey) -> do
--     blobStore.delete blobKey
--     PostgresFileStore.deleteFileState pool fileRef
-- @
newWithCleanup :: PostgresEventStore -> Task Text (FileStateStore, HasqlPool.Pool)
newWithCleanup config = do
  -- Create connection pool
  pool <- createPool config
    |> Task.mapError (\err -> [fmt|Failed to create database pool: #{show err}|])

  -- Create table if not exists
  initResult <- initializeTable pool
    |> Task.asResult
  case initResult of
    Err err -> Task.throw [fmt|Failed to initialize file state table: #{show err}|]
    Ok _ -> pass

  -- Return the FileStateStore implementation and the pool
  let store = FileStateStore
        { getState = getStateImpl pool
        , setState = setStateImpl pool
        , updateState = updateStateImpl pool
        }
  Task.yield (store, pool)


-- | Create a connection pool from PostgresEventStore config
createPool :: PostgresEventStore -> Task PostgresFileStoreError HasqlPool.Pool
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
        , HasqlPoolConfig.observationHandler logPoolObservation
        ]
          |> HasqlPoolConfig.settings
  HasqlPool.acquire poolConfig
    |> Task.fromIO


-- | Log connection pool lifecycle events for observability.
-- Only logs termination events to avoid overhead under high load.
-- See ADR-0027 for rationale.
logPoolObservation :: Observation -> Prelude.IO ()
logPoolObservation observation = case observation of
  ConnectionObservation _uuid status -> case status of
    TerminatedConnectionStatus reason -> case reason of
      AgingConnectionTerminationReason ->
        ((Log.debug "[Pool] Connection terminated (aging timeout)" |> Task.ignoreError :: Task Text Unit) |> Task.runOrPanic)
      IdlenessConnectionTerminationReason ->
        ((Log.debug "[Pool] Connection terminated (idleness timeout)" |> Task.ignoreError :: Task Text Unit) |> Task.runOrPanic)
      NetworkErrorConnectionTerminationReason err ->
        ((Log.critical [fmt|[Pool] Connection terminated (network error: #{show err})|] |> Task.ignoreError :: Task Text Unit) |> Task.runOrPanic)
      ReleaseConnectionTerminationReason ->
        Prelude.pure ()
      InitializationErrorTerminationReason err ->
        ((Log.critical [fmt|[Pool] Connection terminated (init error: #{show err})|] |> Task.ignoreError :: Task Text Unit) |> Task.runOrPanic)
    _ -> Prelude.pure ()


-- | Create the file_upload_state table if it doesn't exist.
initializeTable :: HasqlPool.Pool -> Task PostgresFileStoreError ()
initializeTable pool = do
  let session = createTableSession
  runPool pool session


-- ==========================================================================
-- FileStateStore Implementation
-- ==========================================================================

-- | Get file state by FileRef
getStateImpl :: HasqlPool.Pool -> FileRef -> Task Text (Maybe FileUploadState)
getStateImpl pool (FileRef fileRefText) = do
  let session = selectStateSession fileRefText
  result <- runPool pool session
    |> Task.mapError (\err -> [fmt|Database error: #{show err}|])
  case result of
    Nothing -> Task.yield Nothing
    Just row -> do
      case deserializeState row of
        Err err -> Task.throw [fmt|Failed to deserialize state: #{err}|]
        Ok state -> Task.yield (Just state)


-- | Set file state (upsert)
setStateImpl :: HasqlPool.Pool -> FileRef -> FileUploadState -> Task Text ()
setStateImpl pool (FileRef fileRefText) state = do
  let row = serializeState fileRefText state
  let session = upsertStateSession row
  runPool pool session
    |> Task.mapError (\err -> [fmt|Database error: #{show err}|])


-- | Update state by applying an event atomically.
-- Uses SELECT FOR UPDATE to prevent race conditions between concurrent updates.
-- Returns an error if the existing row has corrupted state (rather than masking it).
updateStateImpl :: HasqlPool.Pool -> FileRef -> FileUploadEvent -> Task Text ()
updateStateImpl pool (FileRef fileRefText) event = do
  let session = updateStateAtomicSession fileRefText event
  result <- runPool pool session
    |> Task.mapError (\err -> [fmt|Database error: #{show err}|])
  case result of
    Err updateErr -> Task.throw updateErr
    Ok _ -> Task.yield ()


-- ==========================================================================
-- Cleanup Queries (for external cleanup workers)
-- ==========================================================================

-- | Find all pending files that have expired.
-- Returns list of (FileRef, BlobKey) for cleanup.
--
-- IMPORTANT: Use the pool from 'newWithCleanup'. Do NOT create pools per-call.
--
-- @
-- (stateStore, pool) <- PostgresFileStore.newWithCleanup dbConfig
-- -- ... later in cleanup worker ...
-- expired <- PostgresFileStore.findExpiredPendingFiles pool currentTime
-- for_ expired $ \\(fileRef, blobKey) -> do
--   blobStore.delete blobKey
--   PostgresFileStore.deleteFileState pool fileRef
-- @
findExpiredPendingFiles ::
  HasqlPool.Pool ->
  Int64 ->  -- Current epoch time
  Task Text [(FileRef, BlobKey)]
findExpiredPendingFiles pool currentTime = do
  let session = selectExpiredPendingSession currentTime
  runPool pool session
    |> Task.mapError (\err -> [fmt|Database error: #{show err}|])


-- | Delete a file state record.
--
-- IMPORTANT: Use the pool from 'newWithCleanup'. Do NOT create pools per-call.
deleteFileState :: HasqlPool.Pool -> FileRef -> Task Text ()
deleteFileState pool (FileRef fileRefText) = do
  let session = deleteStateSession fileRefText
  runPool pool session
    |> Task.mapError (\err -> [fmt|Database error: #{show err}|])


-- ==========================================================================
-- Internal: Database Row Type
-- ==========================================================================

-- | Database row representation
data FileStateRow = FileStateRow
  { fileRef :: Text
  , status :: Text  -- "initial", "pending", "confirmed", "deleted"
  , blobKey :: Maybe Text
  , ownerHash :: Maybe Text
  , filename :: Maybe Text
  , contentType :: Maybe Text
  , sizeBytes :: Maybe Int64
  , uploadedAt :: Maybe Int64
  , expiresAt :: Maybe Int64
  , confirmedByRequestId :: Maybe Text
  }
  deriving (Generic, Eq, Show)


-- | Serialize FileUploadState to database row
serializeState :: Text -> FileUploadState -> FileStateRow
serializeState fileRefText state = case state of
  Initial -> FileStateRow
    { fileRef = fileRefText
    , status = "initial"
    , blobKey = Nothing
    , ownerHash = Nothing
    , filename = Nothing
    , contentType = Nothing
    , sizeBytes = Nothing
    , uploadedAt = Nothing
    , expiresAt = Nothing
    , confirmedByRequestId = Nothing
    }
  Pending pending -> FileStateRow
    { fileRef = fileRefText
    , status = "pending"
    , blobKey = Just (unwrapBlobKey pending.metadata.blobKey)
    , ownerHash = Just (unwrapOwnerHash pending.ownerHash)
    , filename = Just pending.metadata.filename
    , contentType = Just pending.metadata.contentType
    , sizeBytes = Just pending.metadata.sizeBytes
    , uploadedAt = Just pending.metadata.uploadedAt
    , expiresAt = Just pending.expiresAt
    , confirmedByRequestId = Nothing
    }
  Confirmed confirmed -> FileStateRow
    { fileRef = fileRefText
    , status = "confirmed"
    , blobKey = Just (unwrapBlobKey confirmed.metadata.blobKey)
    , ownerHash = Just (unwrapOwnerHash confirmed.ownerHash)
    , filename = Just confirmed.metadata.filename
    , contentType = Just confirmed.metadata.contentType
    , sizeBytes = Just confirmed.metadata.sizeBytes
    , uploadedAt = Just confirmed.metadata.uploadedAt
    , expiresAt = Nothing
    , confirmedByRequestId = Just confirmed.confirmedByRequestId
    }
  Deleted -> FileStateRow
    { fileRef = fileRefText
    , status = "deleted"
    , blobKey = Nothing
    , ownerHash = Nothing
    , filename = Nothing
    , contentType = Nothing
    , sizeBytes = Nothing
    , uploadedAt = Nothing
    , expiresAt = Nothing
    , confirmedByRequestId = Nothing
    }


-- | Deserialize database row to FileUploadState
deserializeState :: FileStateRow -> Result Text FileUploadState
deserializeState row = case row.status of
  "initial" -> Ok Initial
  "deleted" -> Ok Deleted
  "pending" -> do
    blobKey <- requireField "blob_key" row.blobKey
    ownerHash <- requireField "owner_hash" row.ownerHash
    filename <- requireField "filename" row.filename
    contentType <- requireField "content_type" row.contentType
    sizeBytes <- requireField "size_bytes" row.sizeBytes
    uploadedAt <- requireField "uploaded_at" row.uploadedAt
    expiresAt <- requireField "expires_at" row.expiresAt
    Ok (Pending Lifecycle.PendingFile
      { metadata = Lifecycle.FileMetadata
          { ref = FileRef row.fileRef
          , filename = filename
          , contentType = contentType
          , sizeBytes = sizeBytes
          , blobKey = BlobKey blobKey
          , uploadedAt = uploadedAt
          }
      , ownerHash = OwnerHash ownerHash
      , expiresAt = expiresAt
      })
  "confirmed" -> do
    blobKey <- requireField "blob_key" row.blobKey
    ownerHash <- requireField "owner_hash" row.ownerHash
    filename <- requireField "filename" row.filename
    contentType <- requireField "content_type" row.contentType
    sizeBytes <- requireField "size_bytes" row.sizeBytes
    uploadedAt <- requireField "uploaded_at" row.uploadedAt
    confirmedBy <- requireField "confirmed_by_request_id" row.confirmedByRequestId
    Ok (Confirmed Lifecycle.ConfirmedFile
      { metadata = Lifecycle.FileMetadata
          { ref = FileRef row.fileRef
          , filename = filename
          , contentType = contentType
          , sizeBytes = sizeBytes
          , blobKey = BlobKey blobKey
          , uploadedAt = uploadedAt
          }
      , ownerHash = OwnerHash ownerHash
      , confirmedByRequestId = confirmedBy
      })
  other -> Err [fmt|Unknown status: #{other}|]


-- | Helper to require a field
requireField :: Text -> Maybe a -> Result Text a
requireField fieldName maybeVal = case maybeVal of
  Nothing -> Err [fmt|Missing required field: #{fieldName}|]
  Just val -> Ok val


-- | Unwrap BlobKey
unwrapBlobKey :: BlobKey -> Text
unwrapBlobKey (BlobKey t) = t


-- | Unwrap OwnerHash
unwrapOwnerHash :: OwnerHash -> Text
unwrapOwnerHash (OwnerHash t) = t


-- ==========================================================================
-- Internal: SQL Sessions
-- ==========================================================================

-- | Run a session on the pool
runPool :: HasqlPool.Pool -> Session.Session a -> Task PostgresFileStoreError a
runPool pool session = do
  result <- HasqlPool.use pool session
    |> Task.fromIO
    |> Task.map Result.fromEither
  case result of
    Err err -> Task.throw (PoolError err)
    Ok val -> Task.yield val


-- | Create table if not exists
createTableSession :: Session.Session ()
createTableSession =
  Session.sql
    [fmt|
      CREATE TABLE IF NOT EXISTS file_upload_state (
        file_ref VARCHAR(255) PRIMARY KEY,
        status VARCHAR(20) NOT NULL,
        blob_key VARCHAR(500),
        owner_hash VARCHAR(255),
        filename VARCHAR(1000),
        content_type VARCHAR(255),
        size_bytes BIGINT,
        uploaded_at BIGINT,
        expires_at BIGINT,
        confirmed_by_request_id VARCHAR(255),
        created_at TIMESTAMPTZ DEFAULT NOW(),
        updated_at TIMESTAMPTZ DEFAULT NOW()
      );

      CREATE INDEX IF NOT EXISTS idx_file_upload_state_status
        ON file_upload_state(status);

      CREATE INDEX IF NOT EXISTS idx_file_upload_state_expires
        ON file_upload_state(expires_at)
        WHERE status = 'pending';

      CREATE INDEX IF NOT EXISTS idx_file_upload_state_owner
        ON file_upload_state(owner_hash);
    |]


-- | Select state by file_ref
selectStateSession :: Text -> Session.Session (Maybe FileStateRow)
selectStateSession fileRefText = do
  let sql =
        "SELECT file_ref, status, blob_key, owner_hash, filename, content_type, \
        \size_bytes, uploaded_at, expires_at, confirmed_by_request_id \
        \FROM file_upload_state WHERE file_ref = $1"
  let encoder = Encoders.param (Encoders.nonNullable Encoders.text)
  let decoder = Decoders.rowMaybe rowDecoder
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) encoder decoder True
  Session.statement fileRefText statement


-- | Row decoder for FileStateRow
rowDecoder :: Decoders.Row FileStateRow
rowDecoder = do
  fileRef <- Decoders.column (Decoders.nonNullable Decoders.text)
  status <- Decoders.column (Decoders.nonNullable Decoders.text)
  blobKey <- Decoders.column (Decoders.nullable Decoders.text)
  ownerHash <- Decoders.column (Decoders.nullable Decoders.text)
  filename <- Decoders.column (Decoders.nullable Decoders.text)
  contentType <- Decoders.column (Decoders.nullable Decoders.text)
  sizeBytes <- Decoders.column (Decoders.nullable Decoders.int8)
  uploadedAt <- Decoders.column (Decoders.nullable Decoders.int8)
  expiresAt <- Decoders.column (Decoders.nullable Decoders.int8)
  confirmedByRequestId <- Decoders.column (Decoders.nullable Decoders.text)
  pure FileStateRow
    { fileRef = fileRef
    , status = status
    , blobKey = blobKey
    , ownerHash = ownerHash
    , filename = filename
    , contentType = contentType
    , sizeBytes = sizeBytes
    , uploadedAt = uploadedAt
    , expiresAt = expiresAt
    , confirmedByRequestId = confirmedByRequestId
    }


-- | Upsert state (INSERT ... ON CONFLICT UPDATE)
upsertStateSession :: FileStateRow -> Session.Session ()
upsertStateSession row = do
  let sql =
        "INSERT INTO file_upload_state \
        \(file_ref, status, blob_key, owner_hash, filename, content_type, \
        \size_bytes, uploaded_at, expires_at, confirmed_by_request_id, updated_at) \
        \VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, NOW()) \
        \ON CONFLICT (file_ref) DO UPDATE SET \
        \status = EXCLUDED.status, \
        \blob_key = EXCLUDED.blob_key, \
        \owner_hash = EXCLUDED.owner_hash, \
        \filename = EXCLUDED.filename, \
        \content_type = EXCLUDED.content_type, \
        \size_bytes = EXCLUDED.size_bytes, \
        \uploaded_at = EXCLUDED.uploaded_at, \
        \expires_at = EXCLUDED.expires_at, \
        \confirmed_by_request_id = EXCLUDED.confirmed_by_request_id, \
        \updated_at = NOW()"
  let encoder =
        ((\r -> r.fileRef) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\r -> r.status) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\r -> r.blobKey) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\r -> r.ownerHash) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\r -> r.filename) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\r -> r.contentType) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\r -> r.sizeBytes) >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((\r -> r.uploadedAt) >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((\r -> r.expiresAt) >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((\r -> r.confirmedByRequestId) >$< Encoders.param (Encoders.nullable Encoders.text))
  let decoder = Decoders.noResult
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) encoder decoder True
  Session.statement row statement


-- | Atomically update state by applying an event.
-- Uses a transaction with SELECT FOR UPDATE to prevent race conditions.
-- Returns an error if the existing row has corrupted/invalid state.
updateStateAtomicSession :: Text -> FileUploadEvent -> Session.Session (Result Text ())
updateStateAtomicSession fileRefText event = do
  -- Start transaction
  Session.sql "BEGIN"
  
  -- Ensure row exists (INSERT ... ON CONFLICT DO NOTHING) so SELECT FOR UPDATE has something to lock.
  -- This prevents race conditions for new file_refs where no row exists yet.
  ensureRowExistsSession fileRefText
  
  -- Get current state with SELECT FOR UPDATE (locks the row)
  maybeRow <- selectForUpdateSession fileRefText
  
  -- Apply event to get new state
  case maybeRow of
    Nothing -> do
      -- This shouldn't happen after ensureRowExistsSession, but handle gracefully
      Session.sql "ROLLBACK"
      pure (Err [fmt|Row not found after insert for file_ref: #{fileRefText}|])
    Just row -> do
      case deserializeState row of
        Err deserializeErr -> do
          -- Corrupted state - rollback and report error, don't overwrite with initialState
          Session.sql "ROLLBACK"
          pure (Err [fmt|Corrupted state for file_ref #{fileRefText}: #{deserializeErr}|])
        Ok currentState -> do
          let newState = Lifecycle.update event currentState
          let newRow = serializeState fileRefText newState
          -- Upsert new state
          upsertStateSession newRow
          -- Commit transaction
          Session.sql "COMMIT"
          pure (Ok ())


-- | Ensure a row exists for the given file_ref.
-- Uses INSERT ... ON CONFLICT DO NOTHING to create an initial state row if missing.
-- This allows SELECT FOR UPDATE to acquire a lock even for new file_refs.
ensureRowExistsSession :: Text -> Session.Session ()
ensureRowExistsSession fileRefText = do
  let initialRow = serializeState fileRefText Lifecycle.initialState
  let sql =
        "INSERT INTO file_upload_state \
        \(file_ref, status, blob_key, owner_hash, filename, content_type, \
        \size_bytes, uploaded_at, expires_at, confirmed_by_request_id, updated_at) \
        \VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, NOW()) \
        \ON CONFLICT (file_ref) DO NOTHING"
  let encoder =
        ((\r -> r.fileRef) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\r -> r.status) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\r -> r.blobKey) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\r -> r.ownerHash) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\r -> r.filename) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\r -> r.contentType) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\r -> r.sizeBytes) >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((\r -> r.uploadedAt) >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((\r -> r.expiresAt) >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((\r -> r.confirmedByRequestId) >$< Encoders.param (Encoders.nullable Encoders.text))
  let decoder = Decoders.noResult
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) encoder decoder True
  Session.statement initialRow statement


-- | Select state with FOR UPDATE lock (for atomic updates)
selectForUpdateSession :: Text -> Session.Session (Maybe FileStateRow)
selectForUpdateSession fileRefText = do
  let sql =
        "SELECT file_ref, status, blob_key, owner_hash, filename, content_type, \
        \size_bytes, uploaded_at, expires_at, confirmed_by_request_id \
        \FROM file_upload_state WHERE file_ref = $1 FOR UPDATE"
  let encoder = Encoders.param (Encoders.nonNullable Encoders.text)
  let decoder = Decoders.rowMaybe rowDecoder
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) encoder decoder True
  Session.statement fileRefText statement


-- | Select expired pending files for cleanup
selectExpiredPendingSession :: Int64 -> Session.Session [(FileRef, BlobKey)]
selectExpiredPendingSession currentTime = do
  let sql =
        "SELECT file_ref, blob_key FROM file_upload_state \
        \WHERE status = 'pending' AND expires_at < $1 AND blob_key IS NOT NULL"
  let encoder = Encoders.param (Encoders.nonNullable Encoders.int8)
  let decoder = Decoders.rowList do
        fileRef <- Decoders.column (Decoders.nonNullable Decoders.text)
        blobKey <- Decoders.column (Decoders.nonNullable Decoders.text)
        pure (FileRef fileRef, BlobKey blobKey)
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) encoder decoder True
  Session.statement currentTime statement


-- | Delete a file state record
deleteStateSession :: Text -> Session.Session ()
deleteStateSession fileRefText = do
  let sql = "DELETE FROM file_upload_state WHERE file_ref = $1"
  let encoder = Encoders.param (Encoders.nonNullable Encoders.text)
  let decoder = Decoders.noResult
  let statement = Statement (Text.toBytes sql |> Bytes.unwrap) encoder decoder True
  Session.statement fileRefText statement
