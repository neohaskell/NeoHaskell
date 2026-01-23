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
) where

import Basics
import Bytes (Bytes)
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import DateTime qualified
import Map (Map)
import Maybe (Maybe (..))
import Service.FileUpload.BlobStore (BlobStore (..))
import Service.FileUpload.Core (
  FileAccessError (..),
  FileRef (..),
  FileUploadConfig (..),
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



-- ==========================================================================
-- Configuration
-- ==========================================================================

-- | Configuration for file upload routes.
data FileUploadSetup = FileUploadSetup
  { config :: FileUploadConfig
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
    { config = FileUploadConfig
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
        maybeState <- ConcurrentMap.get fileRef store
          |> Task.mapError (\_ -> "Failed to read state")
        let currentState = case maybeState of
              Nothing -> Lifecycle.initialState
              Just s -> s
        let newState = Lifecycle.update event currentState
        ConcurrentMap.set fileRef newState store
          |> Task.mapError (\_ -> "Failed to write state")
    }


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
  FileUploadConfig ->
  BlobStore ->
  FileStateStore ->
  Text ->  -- ownerHash
  Text ->  -- filename
  Text ->  -- contentType
  Bytes -> -- content
  Task Text UploadResponse
handleUploadImpl config blobStore stateStore ownerHash filename contentType content = do
  let request = UploadRequest
        { filename = filename
        , contentType = contentType
        , content = content
        , ownerHash = ownerHash
        }
  
  -- Call the core upload handler
  response <- Routes.handleUpload config blobStore request
    |> Task.mapError uploadErrorToText
  
  -- Record the pending state
  now <- DateTime.now |> Task.mapError (\_ -> "Failed to get time")
  let event = FileUploaded FileUploadedData
        { fileRef = response.fileRef
        , ownerHash = OwnerHash ownerHash
        , filename = response.filename
        , contentType = response.contentType
        , sizeBytes = response.sizeBytes
        , blobKey = response.blobKey
        , uploadedAt = DateTime.toEpochSeconds now
        , expiresAt = DateTime.toEpochSeconds response.expiresAt
        }
  stateStore.updateState response.fileRef event
  
  Task.yield response


-- | Convert upload error to text.
uploadErrorToText :: UploadError -> Text
uploadErrorToText err = case err of
  FileTooLarge maxB actualB -> [fmt|File too large: #{actualB} bytes exceeds limit of #{maxB} bytes|]
  InvalidContentType ct _ -> [fmt|Invalid content type: #{ct}|]
  InvalidFilename msg -> [fmt|Invalid filename: #{msg}|]
  StorageFailure msg -> [fmt|Storage failure: #{msg}|]


-- | Handle file download.
handleDownloadImpl ::
  BlobStore ->
  FileStateStore ->
  OwnerHash ->
  FileRef ->
  Task FileAccessError (Bytes, Text, Text)
handleDownloadImpl blobStore stateStore ownerHash fileRef = do
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
confirmFileImpl stateStore fileRef requestId = do
  now <- DateTime.now |> Task.mapError (\_ -> "Failed to get time")
  let nowEpoch = DateTime.toEpochSeconds now
  let event = FileConfirmed FileConfirmedData
        { fileRef = fileRef
        , confirmedByRequestId = requestId
        , confirmedAt = nowEpoch
        }
  stateStore.updateState fileRef event
