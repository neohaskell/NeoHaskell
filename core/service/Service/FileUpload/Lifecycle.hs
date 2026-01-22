module Service.FileUpload.Lifecycle (
  -- * State Types
  FileUploadState (..),
  PendingFile (..),
  ConfirmedFile (..),
  FileMetadata (..),

  -- * State Machine
  update,
  initialState,

  -- * Validation Helpers
  canConfirm,
  isExpired,
  canDelete,
  getOwnerHash,
  getBlobKey,
  getMetadata,
) where

import Basics
import Maybe (Maybe (..))
import Service.FileUpload.Core (
  BlobKey (..),
  FileRef (..),
  FileUploadEvent (..),
  OwnerHash (..),
 )
import Text (Text)


-- ==========================================================================
-- Lifecycle Metadata (different from Core.FileMetadata - includes ref and blobKey)
-- ==========================================================================

-- | Full metadata for lifecycle state tracking
data FileMetadata = FileMetadata
  { ref :: FileRef
  , filename :: Text
  , contentType :: Text
  , sizeBytes :: Int64
  , blobKey :: BlobKey
  , uploadedAt :: Int64
  }
  deriving (Generic, Eq, Show)


-- ==========================================================================
-- State Types
-- ==========================================================================

-- | A file in pending state (uploaded but not yet confirmed by a command)
data PendingFile = PendingFile
  { metadata :: FileMetadata
  , ownerHash :: OwnerHash
  , expiresAt :: Int64
  }
  deriving (Generic, Eq, Show)


-- | A file in confirmed state (successfully used by a command)
data ConfirmedFile = ConfirmedFile
  { metadata :: FileMetadata
  , ownerHash :: OwnerHash
  , confirmedByRequestId :: Text
  }
  deriving (Generic, Eq, Show)


-- | File upload lifecycle state machine
data FileUploadState
  = Initial -- No events yet
  | Pending PendingFile -- Uploaded, awaiting confirmation
  | Confirmed ConfirmedFile -- Confirmed by a command
  | Deleted -- File has been deleted
  deriving (Generic, Eq, Show)


-- ==========================================================================
-- State Machine
-- ==========================================================================

-- | Initial state for a new file upload stream
initialState :: FileUploadState
initialState = Initial


-- | Apply an event to transition state
update :: FileUploadEvent -> FileUploadState -> FileUploadState
update event state = case event of
  FileUploaded {fileRef, ownerHash, filename, contentType, sizeBytes, blobKey, expiresAt, uploadedAt} ->
    Pending
      PendingFile
        { metadata =
            FileMetadata
              { ref = fileRef
              , filename = filename
              , contentType = contentType
              , sizeBytes = sizeBytes
              , blobKey = blobKey
              , uploadedAt = uploadedAt
              }
        , ownerHash = ownerHash
        , expiresAt = expiresAt
        }
  FileConfirmed {confirmedByRequestId} ->
    case state of
      Pending pending ->
        Confirmed
          ConfirmedFile
            { metadata = pending.metadata
            , ownerHash = pending.ownerHash
            , confirmedByRequestId = confirmedByRequestId
            }
      Confirmed _ -> state -- Idempotent: already confirmed
      _ -> state -- Invalid transition: ignore
  FileDeleted {} ->
    case state of
      Pending _ -> Deleted
      Confirmed _ -> Deleted
      Deleted -> Deleted -- Idempotent
      Initial -> Initial -- Invalid transition: ignore


-- ==========================================================================
-- Validation Helpers
-- ==========================================================================

-- | Check if file can be confirmed at the given time
-- Returns True only for Pending state before expiry
canConfirm :: FileUploadState -> Int64 -> Bool
canConfirm state currentTime = case state of
  Pending pending -> currentTime < pending.expiresAt
  _ -> False


-- | Check if file has expired at the given time
-- Returns True only for Pending state at or after expiry
isExpired :: FileUploadState -> Int64 -> Bool
isExpired state currentTime = case state of
  Pending pending -> currentTime >= pending.expiresAt
  _ -> False


-- | Check if file can be deleted
-- Returns True for Pending and Confirmed states
canDelete :: FileUploadState -> Bool
canDelete state = case state of
  Pending _ -> True
  Confirmed _ -> True
  _ -> False


-- | Get owner hash if available
getOwnerHash :: FileUploadState -> Maybe OwnerHash
getOwnerHash state = case state of
  Pending pending -> Just pending.ownerHash
  Confirmed confirmed -> Just confirmed.ownerHash
  _ -> Nothing


-- | Get blob key if available
getBlobKey :: FileUploadState -> Maybe BlobKey
getBlobKey state = case state of
  Pending pending -> Just pending.metadata.blobKey
  Confirmed confirmed -> Just confirmed.metadata.blobKey
  _ -> Nothing


-- | Get metadata if available
getMetadata :: FileUploadState -> Maybe FileMetadata
getMetadata state = case state of
  Pending pending -> Just pending.metadata
  Confirmed confirmed -> Just confirmed.metadata
  _ -> Nothing
