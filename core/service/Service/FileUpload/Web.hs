-- | Web transport integration for file uploads.
--
-- This module provides route handlers for file upload and download
-- that can be integrated into WebTransport.
--
-- = Usage in Application
--
-- @
-- app = Application.new
--   |> Application.withTransport WebTransport.server
--   |> Application.withFileUpload fileUploadConfig
-- @
module Service.FileUpload.Web (
  -- * Configuration
  FileUploadSetup (..),
  defaultFileUploadSetup,

  -- * Route Handlers
  FileUploadRoutes (..),
  createRoutes,

  -- * State Management
  FileStateStore (..),
  InMemoryFileStateStore,
  newInMemoryFileStateStore,
  inMemoryFileStateStore,

  -- * Content Deduplication
  computeContentHash,
  handleUploadImpl,

  -- * Cleanup
  startCleanupWorker,
  cleanupExpiredFiles,
) where

import Array qualified
import AsyncTask (AsyncTask)
import AsyncTask qualified
import Basics
import Bytes (Bytes)
import Bytes qualified
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import ConcurrentVar qualified
import Crypto.Hash qualified as Hash
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as Encoding
import Data.ByteString qualified as BS
import Log qualified
import DateTime qualified

import Map (Map)
import Maybe (Maybe (..))
import Result (Result (..))
import Service.FileUpload.BlobStore (BlobStore (..))
import Service.FileUpload.Core (
  ContentHash (..),
  FileAccessError (..),
  FileDeletedData (..),
  FileDeletionReason (..),
  FileRef (..),
  InternalFileUploadConfig (..),
  FileUploadEvent (..),
  FileUploadedData (..),
  FileConfirmedData (..),
  OwnerHash (..),
 )
import Service.FileUpload.Lifecycle (FileUploadState (..))
import Service.FileUpload.Lifecycle qualified as Lifecycle
import Service.FileUpload.Resolver (ResolvedFile (..))
import Service.FileUpload.Resolver qualified as Resolver
import Service.FileUpload.Routes (UploadResponse (..), UploadRequest (..), UploadError (..))
import Service.FileUpload.Routes qualified as Routes
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified



-- ==========================================================================
-- Configuration
-- ==========================================================================

-- | Internal configuration for file upload routes (after initialization).
--
-- This is created by 'initializeFileUpload' from a 'FileUploadConfig'.
-- Users should not create this directly - use 'FileUploadConfig' instead.
data FileUploadSetup = FileUploadSetup
  { config :: InternalFileUploadConfig
  -- ^ Upload limits and allowed content types
  , blobStore :: BlobStore
  -- ^ Where to store file content
  , stateStore :: FileStateStore
  -- ^ Where to track file lifecycle state
  }


-- | Default file upload configuration.
-- Uses 10MB max file size, 6 hour pending TTL, all content types allowed.
defaultFileUploadSetup :: BlobStore -> FileStateStore -> FileUploadSetup
defaultFileUploadSetup blobStore stateStore =
  FileUploadSetup
    { config = InternalFileUploadConfig
        { pendingTtlSeconds = 21600 -- 6 hours
        , cleanupIntervalSeconds = 900 -- 15 minutes
        , maxFileSizeBytes = 10485760 -- 10 MB
        , allowedContentTypes = Nothing -- All types allowed
        , storeOriginalFilename = True
        }
    , blobStore = blobStore
    , stateStore = stateStore
    }


-- ==========================================================================
-- State Store Abstraction
-- ==========================================================================

-- | Interface for storing file upload state.
-- This allows different backends (in-memory, event-sourced, database).
data FileStateStore = FileStateStore
  { getState :: FileRef -> Task Text (Maybe FileUploadState)
  -- ^ Get the current state for a file reference
  , setState :: FileRef -> FileUploadState -> Task Text ()
  -- ^ Set the state for a file reference
  , updateState :: FileRef -> FileUploadEvent -> Task Text ()
  -- ^ Apply an event to update state
  , findByContentHash :: OwnerHash -> ContentHash -> Task Text (Maybe FileRef)
  -- ^ Find an existing non-deleted file with matching owner and content hash
  }


-- ==========================================================================
-- In-Memory State Store
-- ==========================================================================

-- | In-memory file state store for development/testing.
type InMemoryFileStateStore = ConcurrentMap FileRef FileUploadState


-- | Create a new in-memory file state store.
newInMemoryFileStateStore :: Task err InMemoryFileStateStore
newInMemoryFileStateStore = ConcurrentMap.new


-- | Create a FileStateStore backed by a ConcurrentMap.
inMemoryFileStateStore :: InMemoryFileStateStore -> FileStateStore
inMemoryFileStateStore store =
  FileStateStore
    { getState = \fileRef -> do
        ConcurrentMap.get fileRef store
          |> Task.mapError (\_ -> "Failed to read state")
    , setState = \fileRef state -> do
        ConcurrentMap.set fileRef state store
          |> Task.mapError (\_ -> "Failed to write state")
    , updateState = \fileRef event -> do
        -- Use atomic update to prevent race conditions
        let updateFn maybeState = 
              let currentState = case maybeState of
                    Nothing -> Lifecycle.initialState
                    Just s -> s
              in Lifecycle.update event currentState
        ConcurrentMap.update fileRef updateFn store
          |> Task.mapError (\_ -> "Failed to update state")
    , findByContentHash = \ownerHash contentHash -> do
        -- NOTE: O(n) scan — acceptable for dev/testing only. Production uses Postgres with indexed lookup.
        allEntries <- ConcurrentMap.entries store
          |> Task.mapError (\_ -> "Failed to read state map")
        let entryList = allEntries |> Array.toLinkedList
        Task.yield (findMatchingEntry ownerHash contentHash entryList)
    }


-- | Search a list of (FileRef, FileUploadState) entries for a matching
-- owner + content hash in Pending or Confirmed state.
-- Returns the first matching FileRef, or Nothing.
--
-- NOTE: ConcurrentMap does NOT have a findFirst function.
-- We use ConcurrentMap.entries to get all entries as an Array,
-- convert to a linked list, and recurse manually.
findMatchingEntry :: OwnerHash -> ContentHash -> [(FileRef, FileUploadState)] -> Maybe FileRef
{-# INLINE findMatchingEntry #-}
findMatchingEntry ownerHash contentHash entryList =
  case entryList of
    [] -> Nothing
    (fileRef, state) : rest ->
      case state of
        Pending pending ->
          if pending.ownerHash == ownerHash && pending.metadata.contentHash == contentHash
            then Just fileRef
            else findMatchingEntry ownerHash contentHash rest
        Confirmed confirmed ->
          if confirmed.ownerHash == ownerHash && confirmed.metadata.contentHash == contentHash
            then Just fileRef
            else findMatchingEntry ownerHash contentHash rest
        _ -> findMatchingEntry ownerHash contentHash rest


-- ==========================================================================
-- Route Handlers
-- ==========================================================================

-- | File upload route handlers.
data FileUploadRoutes = FileUploadRoutes
  { handleUpload :: Text -> Text -> Text -> Bytes -> Task Text UploadResponse
  -- ^ Handle POST /files/upload
  -- Parameters: ownerHash, filename, contentType, content
  , handleDownload :: Text -> FileRef -> Task FileAccessError (Bytes, Text, Text)
  -- ^ Handle GET /files/:fileRef
  -- Returns (content, contentType, filename)
  , resolveFileRefs :: Text -> [FileRef] -> Task FileAccessError (Map FileRef ResolvedFile)
  -- ^ Resolve multiple file references for command context
  , confirmFile :: FileRef -> Text -> Task Text ()
  -- ^ Confirm a file after successful command execution
  }


-- | Create file upload route handlers from configuration.
createRoutes :: FileUploadSetup -> FileUploadRoutes
createRoutes setup =
  let stateStoreImpl = setup.stateStore
      blobStoreImpl = setup.blobStore
      configImpl = setup.config
  in FileUploadRoutes
      { handleUpload = \ownerHashText filename contentType content -> do
          -- Multipart parsing is done by caller (WebTransport)
          handleUploadImpl configImpl blobStoreImpl stateStoreImpl ownerHashText filename contentType content

      , handleDownload = \ownerHashText fileRef -> do
          let ownerHash = OwnerHash ownerHashText
          handleDownloadImpl blobStoreImpl stateStoreImpl ownerHash fileRef

      , resolveFileRefs = \ownerHashText refs -> do
          let ownerHash = OwnerHash ownerHashText
          Resolver.resolveFileRefs
            blobStoreImpl
            stateStoreImpl.getState
            ownerHash
            refs

      , confirmFile = \fileRef requestId -> do
          confirmFileImpl stateStoreImpl fileRef requestId
      }


-- ==========================================================================
-- Implementation
-- ==========================================================================

-- | Handle file upload.
-- The caller (WebTransport) handles multipart parsing and provides all parameters.
handleUploadImpl ::
  InternalFileUploadConfig ->
  BlobStore ->
  FileStateStore ->
  Text ->  -- ownerHash
  Text ->  -- filename
  Text ->  -- contentType
  Bytes -> -- content
  Task Text UploadResponse
handleUploadImpl config blobStore stateStore ownerHash filename contentType content = Log.withScope [("component", "FileUpload")] do
  Log.info "Processing file upload"
    |> Task.ignoreError

  -- 1. Compute content hash BEFORE any I/O
  let contentHash = computeContentHash content
  let ownerHashValue = OwnerHash ownerHash

  -- 2. Check for existing upload with same content (dedup)
  existingRef <- stateStore.findByContentHash ownerHashValue contentHash

  case existingRef of
    Just fileRef -> do
      -- Duplicate detected — return existing FileRef without storing blob or emitting event
      Log.info "Duplicate upload detected, returning existing FileRef"
        |> Task.ignoreError
      existingState <- stateStore.getState fileRef
        |> Task.mapError (\_ -> "Failed to retrieve file state")
      case existingState of
        Just (Pending pendingFile) -> do
          Task.yield UploadResponse
            { fileRef = fileRef
            , blobKey = pendingFile.metadata.blobKey
            , filename = pendingFile.metadata.filename
            , contentType = pendingFile.metadata.contentType
            , sizeBytes = pendingFile.metadata.sizeBytes
            , expiresAt = DateTime.fromEpochSeconds pendingFile.expiresAt
            }
        Just (Confirmed confirmedFile) -> do
          now <- DateTime.now |> Task.mapError (\_ -> "Failed to get time")
          Task.yield UploadResponse
            { fileRef = fileRef
            , blobKey = confirmedFile.metadata.blobKey
            , filename = confirmedFile.metadata.filename
            , contentType = confirmedFile.metadata.contentType
            , sizeBytes = confirmedFile.metadata.sizeBytes
            , expiresAt = now  -- Already confirmed, no meaningful expiry
            }
        _ -> do
          -- Deleted or Initial — treat as no match, fall through to normal flow
          normalUploadFlow config blobStore stateStore ownerHash filename contentType content contentHash

    Nothing -> do
      -- No duplicate — normal upload flow
      normalUploadFlow config blobStore stateStore ownerHash filename contentType content contentHash


-- | Normal upload flow (no dedup match found).
-- Extracted from the original handleUploadImpl.
normalUploadFlow ::
  InternalFileUploadConfig ->
  BlobStore ->
  FileStateStore ->
  Text ->  -- ownerHash
  Text ->  -- filename
  Text ->  -- contentType
  Bytes -> -- content
  ContentHash ->
  Task Text UploadResponse
normalUploadFlow config blobStore stateStore ownerHash filename contentType content contentHash = do
  let request = UploadRequest
        { filename = filename
        , contentType = contentType
        , content = content
        , ownerHash = ownerHash
        }

  -- Call the core upload handler (validates + stores blob)
  response <- Routes.handleUpload config blobStore request
    |> Task.mapError uploadErrorToText

  -- Record the pending state (with contentHash)
  now <- DateTime.now |> Task.mapError (\_ -> "Failed to get time")
  let event = FileUploaded FileUploadedData
        { fileRef = response.fileRef
        , ownerHash = OwnerHash ownerHash
        , contentHash = contentHash
        , filename = response.filename
        , contentType = response.contentType
        , sizeBytes = response.sizeBytes
        , blobKey = response.blobKey
        , uploadedAt = DateTime.toEpochSeconds now
        , expiresAt = DateTime.toEpochSeconds response.expiresAt
        }

  -- Update state, cleanup blob on failure to avoid orphaned blobs
  stateUpdateResult <- stateStore.updateState response.fileRef event
    |> Task.asResult

  case stateUpdateResult of
    Ok _ -> do
      Log.info "File uploaded successfully"
        |> Task.ignoreError
      Task.yield response
    Err stateErr -> do
      -- Check if this is a constraint violation (concurrent dedup race)
      if Text.contains "23505" stateErr || Text.contains "duplicate key" stateErr
        then do
          -- Race condition: another upload of same content won the insert.
          -- Retry findByContentHash to get the winner's FileRef.
          Log.info "Constraint violation on insert, retrying dedup lookup"
            |> Task.ignoreError
          -- Clean up our blob since the other upload already stored one
          _ <- blobStore.delete response.blobKey |> Task.asResult
          retryRef <- stateStore.findByContentHash (OwnerHash ownerHash) contentHash
          case retryRef of
            Just existingFileRef -> do
              existingState <- stateStore.getState existingFileRef
                |> Task.mapError (\err -> [fmt|Failed to get state after retry: #{err}|])
              case existingState of
                Just (Pending pendingFile) ->
                  Task.yield UploadResponse
                    { fileRef = existingFileRef
                    , blobKey = pendingFile.metadata.blobKey
                    , filename = pendingFile.metadata.filename
                    , contentType = pendingFile.metadata.contentType
                    , sizeBytes = pendingFile.metadata.sizeBytes
                    , expiresAt = DateTime.fromEpochSeconds pendingFile.expiresAt
                    }
                Just (Confirmed confirmedFile) -> do
                  retryNow <- DateTime.now |> Task.mapError (\_ -> "Failed to get time")
                  Task.yield UploadResponse
                    { fileRef = existingFileRef
                    , blobKey = confirmedFile.metadata.blobKey
                    , filename = confirmedFile.metadata.filename
                    , contentType = confirmedFile.metadata.contentType
                    , sizeBytes = confirmedFile.metadata.sizeBytes
                    , expiresAt = retryNow
                    }
                _ ->
                  Task.throw "Upload failed due to a temporary conflict. Please retry."
            Nothing ->
              Task.throw "Upload failed due to a temporary conflict. Please retry."
        else do
          -- Not a constraint violation - original error handling
          Log.warn "State update failed, cleaning up blob"
            |> Task.ignoreError
          deleteResult <- blobStore.delete response.blobKey
            |> Task.asResult
          case deleteResult of
            Ok _ -> pass
            Err _ ->
              Log.critical "Failed to cleanup blob after state update failure"
                |> Task.ignoreError
          Task.throw "Failed to record upload state"


-- | Convert upload error to text.
uploadErrorToText :: UploadError -> Text
uploadErrorToText err = case err of
  FileTooLarge maxB actualB -> [fmt|File too large: #{actualB} bytes exceeds limit of #{maxB} bytes|]
  InvalidContentType ct _ -> [fmt|Invalid content type: #{ct}|]
  InvalidFilename msg -> [fmt|Invalid filename: #{msg}|]
  StorageFailure msg -> [fmt|Storage failure: #{msg}|]


-- | Compute SHA-256 content hash of file bytes.
-- Returns a 64-character lowercase hex string (Base16 encoding).
-- Follows the same pattern as Auth.OAuth2.TransactionStore.TransactionKey
-- but uses hex encoding instead of base64url for VARCHAR(64) compatibility.
computeContentHash :: Bytes -> ContentHash
computeContentHash content = do
  let contentBytes = Bytes.unwrap content
  let hashResult = Hash.hash contentBytes :: Hash.Digest Hash.SHA256
  let hashBytes = BA.convert hashResult :: BS.ByteString
  let encoded = Encoding.convertToBase Encoding.Base16 hashBytes
  let hashText = Bytes.fromLegacy encoded |> Text.fromBytes
  ContentHash hashText
{-# INLINE computeContentHash #-}


-- | Handle file download.
handleDownloadImpl ::
  BlobStore ->
  FileStateStore ->
  OwnerHash ->
  FileRef ->
  Task FileAccessError (Bytes, Text, Text)
handleDownloadImpl blobStore stateStore ownerHash fileRef = Log.withScope [("component", "FileUpload")] do
  Log.debug "Processing file download"
    |> Task.ignoreError
  -- Get file state
  maybeState <- stateStore.getState fileRef
    |> Task.mapError (\err -> StorageError err)

  case maybeState of
    Nothing -> Task.throw (FileNotFound fileRef)
    Just state -> case state of
      Initial -> Task.throw (FileNotFound fileRef)
      Deleted -> Task.throw (FileIsDeleted fileRef)
      Pending pendingFile -> do
        -- Check ownership
        if pendingFile.ownerHash != ownerHash
          then Task.throw (NotOwner fileRef)
          else do
            -- Check expiry
            now <- DateTime.now |> Task.mapError (\_ -> StorageError "Failed to get time")
            let nowEpoch = DateTime.toEpochSeconds now
            if nowEpoch > pendingFile.expiresAt
              then Task.throw (FileExpired fileRef)
              else do
                let meta = pendingFile.metadata
                content <- blobStore.retrieve meta.blobKey
                  |> Task.mapError (\_ -> BlobMissing fileRef)
                Task.yield (content, meta.contentType, meta.filename)
      Confirmed confirmedFile -> do
        -- Check ownership
        if confirmedFile.ownerHash != ownerHash
          then Task.throw (NotOwner fileRef)
          else do
            let meta = confirmedFile.metadata
            content <- blobStore.retrieve meta.blobKey
              |> Task.mapError (\_ -> BlobMissing fileRef)
            Task.yield (content, meta.contentType, meta.filename)


-- | Confirm a file after successful command execution.
confirmFileImpl ::
  FileStateStore ->
  FileRef ->
  Text ->
  Task Text ()
confirmFileImpl stateStore fileRef requestId = Log.withScope [("component", "FileUpload")] do
  Log.debug "Confirming file"
    |> Task.ignoreError
  now <- DateTime.now |> Task.mapError (\_ -> "Failed to get time")
  let nowEpoch = DateTime.toEpochSeconds now
  let event = FileConfirmed FileConfirmedData
        { fileRef = fileRef
        , confirmedByRequestId = requestId
        , confirmedAt = nowEpoch
        }
  stateStore.updateState fileRef event


-- ==========================================================================
-- Cleanup
-- ==========================================================================

-- | Start a background worker that periodically cleans up expired pending files.
-- Returns the AsyncTask handle for the worker (can be cancelled on shutdown).
startCleanupWorker ::
  InternalFileUploadConfig ->
  FileStateStore ->
  BlobStore ->
  InMemoryFileStateStore ->
  Task Text (AsyncTask Text ())
startCleanupWorker config stateStore blobStore stateMap = do
  let intervalMs = fromIntegral config.cleanupIntervalSeconds * 1000
  AsyncTask.run do
    cleanupLoop intervalMs stateStore blobStore stateMap


-- | Internal cleanup loop - runs forever until cancelled.
cleanupLoop ::
  Int ->
  FileStateStore ->
  BlobStore ->
  InMemoryFileStateStore ->
  Task Text ()
cleanupLoop intervalMs stateStore blobStore stateMap = do
  -- Wait for the configured interval
  AsyncTask.sleep intervalMs
  
  -- Run cleanup
  cleanupResult <- cleanupExpiredFiles stateStore blobStore stateMap
    |> Task.asResult
  Log.withScope [("component", "FileUpload")] do
    case cleanupResult of
      Result.Err err -> 
        Log.warn [fmt|Cleanup error: #{err}|] 
          |> Task.ignoreError
      Result.Ok count -> 
        if count > 0
          then Log.info [fmt|Cleaned up #{count} expired files|] 
            |> Task.ignoreError
          else pass
  
  -- Loop forever
  cleanupLoop intervalMs stateStore blobStore stateMap


-- | Clean up expired pending files.
-- Returns the number of files cleaned up.
cleanupExpiredFiles ::
  FileStateStore ->
  BlobStore ->
  InMemoryFileStateStore ->
  Task Text Int
cleanupExpiredFiles stateStore blobStore stateMap = do
  now <- DateTime.now |> Task.mapError (\_ -> "Failed to get time")
  let nowEpoch = DateTime.toEpochSeconds now
  
  -- Use a counter to track deleted files
  counter <- ConcurrentVar.containing 0
    |> Task.mapError (\_ -> "Failed to create counter")
  
  -- Process entries in chunks (100 at a time) to avoid memory issues with large maps
  -- Each entry is processed independently - failures are logged but don't abort the run
  let processEntry fileRef state = do
        case state of
          Pending pendingFile -> do
            if nowEpoch > pendingFile.expiresAt
              then do
                -- Try to delete the blob (best effort, log failures)
                deleteResult <- blobStore.delete pendingFile.metadata.blobKey
                  |> Task.asResult
                case deleteResult of
                  Err _ -> do
                    -- Log blob deletion failure but continue
                    Log.withScope [("component", "FileUpload")] do
                      Log.warn "Failed to delete blob, skipping state update"
                        |> Task.ignoreError
                  Ok _ -> do
                    -- Blob deleted successfully, update state to Deleted
                    let event = FileDeleted FileDeletedData
                          { fileRef = fileRef
                          , deletedAt = nowEpoch
                          , reason = Orphaned  -- TTL expired without confirmation
                          }
                    stateUpdateResult <- stateStore.updateState fileRef event
                      |> Task.asResult
                    case stateUpdateResult of
                      Err _ -> 
                        Log.withScope [("component", "FileUpload")] do
                          Log.warn "Failed to update state after blob deletion"
                            |> Task.ignoreError
                      Ok _ -> do
                        -- Successfully cleaned up, increment counter (ignore counter errors)
                        _ <- ConcurrentVar.modify (\n -> n + 1) counter
                          |> Task.asResult
                        pass
              else pass  -- Not expired yet
          _ -> pass  -- Skip non-pending files
  
  -- Process in chunks of 100 entries
  ConcurrentMap.forEachChunked 100 processEntry stateMap
    |> Task.mapError (\_ -> "Failed to iterate state map")
  
  -- Return count of deleted files
  ConcurrentVar.get counter
    |> Task.mapError (\_ -> "Failed to read counter")
