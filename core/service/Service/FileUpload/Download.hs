module Service.FileUpload.Download (
  -- * Response Type
  DownloadResponse (..),

  -- * Error Type (re-exported from Core)
  DownloadError,

  -- * Handler
  handleDownload,
) where

import Basics
import Bytes (Bytes)
import DateTime qualified
import Maybe (Maybe (..))
import Service.FileUpload.BlobStore (BlobStore (..), BlobStoreError)
import Service.FileUpload.BlobStore qualified as BlobStore
import Service.FileUpload.Core (FileAccessError (..), FileRef (..), OwnerHash (..))
import Service.FileUpload.Lifecycle (
  ConfirmedFile (..),
  FileMetadata (..),
  FileUploadState (..),
  PendingFile (..),
 )
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- ==========================================================================
-- Response Type
-- ==========================================================================

-- | Successful download response containing file content and metadata
data DownloadResponse = DownloadResponse
  { content :: Bytes
  , contentType :: Text
  , filename :: Text
  }
  deriving (Generic, Eq, Show)


-- ==========================================================================
-- Error Types
-- ==========================================================================

-- | Type alias for download errors (uses shared FileAccessError)
type DownloadError = FileAccessError


-- ==========================================================================
-- Handler
-- ==========================================================================

-- | Handle a file download request
-- Validates ownership and file state before retrieving content
handleDownload ::
  forall stateError.
  (Show stateError) =>
  BlobStore ->
  (FileRef -> Task stateError (Maybe FileUploadState)) ->
  OwnerHash ->
  FileRef ->
  Task DownloadError DownloadResponse
handleDownload blobStore getState requestOwner fileRef = do
  -- Get file state (preserve original error for diagnostics)
  maybeState <-
    getState fileRef
      |> Task.mapError (\stateErr -> StateLookupFailed fileRef (show stateErr |> Text.fromLinkedList))

  case maybeState of
    Nothing -> Task.throw (FileNotFound fileRef)
    Just state -> downloadFromState blobStore requestOwner fileRef state


-- | Download from a specific file state
downloadFromState ::
  BlobStore ->
  OwnerHash ->
  FileRef ->
  FileUploadState ->
  Task DownloadError DownloadResponse
downloadFromState blobStore requestOwner fileRef state = do
  case state of
    Initial ->
      Task.throw (FileNotFound fileRef)
    Deleted ->
      Task.throw (FileIsDeleted fileRef)
    Pending pendingFile ->
      downloadPending blobStore requestOwner fileRef pendingFile
    Confirmed confirmedFile ->
      downloadConfirmed blobStore requestOwner fileRef confirmedFile


-- | Download a pending file (checks expiration)
downloadPending ::
  BlobStore ->
  OwnerHash ->
  FileRef ->
  PendingFile ->
  Task DownloadError DownloadResponse
downloadPending blobStore requestOwner fileRef pendingFile = do
  -- Check ownership
  if pendingFile.ownerHash != requestOwner
    then Task.throw (NotOwner fileRef)
    else do
      -- Check expiration
      now <- DateTime.now |> Task.mapError (\_ -> StorageError "Failed to get current time")
      let nowEpoch = DateTime.toEpochSeconds now
      if nowEpoch > pendingFile.expiresAt
        then Task.throw (FileExpired fileRef)
        else retrieveContent blobStore fileRef pendingFile.metadata


-- | Download a confirmed file
downloadConfirmed ::
  BlobStore ->
  OwnerHash ->
  FileRef ->
  ConfirmedFile ->
  Task DownloadError DownloadResponse
downloadConfirmed blobStore requestOwner fileRef confirmedFile = do
  -- Check ownership
  if confirmedFile.ownerHash != requestOwner
    then Task.throw (NotOwner fileRef)
    else retrieveContent blobStore fileRef confirmedFile.metadata


-- | Retrieve content from blob store
retrieveContent ::
  BlobStore ->
  FileRef ->
  FileMetadata ->
  Task DownloadError DownloadResponse
retrieveContent blobStore fileRef metadata = do
  content <-
    blobStore.retrieve metadata.blobKey
      |> Task.mapError (mapBlobError fileRef)

  Task.yield
    DownloadResponse
      { content = content
      , contentType = metadata.contentType
      , filename = metadata.filename
      }


-- | Map blob store errors to download errors
mapBlobError :: FileRef -> BlobStoreError -> DownloadError
mapBlobError fileRef blobErr = case blobErr of
  BlobStore.NotFound _ -> BlobMissing fileRef
  BlobStore.StorageError msg -> StorageError msg
  BlobStore.InvalidBlobKey msg -> StorageError [fmt|Invalid blob key: #{msg}|]
