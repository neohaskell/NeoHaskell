module Service.FileUpload.Core (
  -- * Opaque References
  FileRef (..),
  BlobKey (..),
  OwnerHash (..),

  -- * File Metadata
  FileMetadata (..),
  ResolvedFile (..),

  -- * Events
  FileUploadEvent (..),
  FileUploadedData (..),
  FileConfirmedData (..),
  FileDeletedData (..),
  FileDeletionReason (..),

  -- * Configuration
  FileUploadConfig (..),
) where

import Array (Array)
import Basics
import Json qualified
import Maybe (Maybe)
import Text (Text)


-- ==========================================================================
-- Opaque References (with redacted Show instances for security)
-- ==========================================================================

-- | Opaque reference to an uploaded file (used in command payloads)
-- Show instance is redacted to prevent leaking file identifiers in logs
newtype FileRef = FileRef Text
  deriving (Generic, Eq, Ord)

instance Show FileRef where
  show _ = "FileRef <REDACTED>"

instance Json.FromJSON FileRef
instance Json.ToJSON FileRef


-- | Internal storage key for blob store
-- Show instance is redacted to prevent leaking storage paths in logs
newtype BlobKey = BlobKey Text
  deriving (Generic, Eq, Ord)

instance Show BlobKey where
  show _ = "BlobKey <REDACTED>"

instance Json.FromJSON BlobKey
instance Json.ToJSON BlobKey


-- | Pseudonymous owner identity (HMAC of user ID)
-- Show instance is redacted to prevent leaking user information in logs
newtype OwnerHash = OwnerHash Text
  deriving (Generic, Eq, Ord)

instance Show OwnerHash where
  show _ = "OwnerHash <REDACTED>"

instance Json.FromJSON OwnerHash
instance Json.ToJSON OwnerHash


-- ==========================================================================
-- File Metadata
-- ==========================================================================

-- | Metadata about an uploaded file
data FileMetadata = FileMetadata
  { filename :: Text
  , contentType :: Text
  , sizeBytes :: Int64
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileMetadata
instance Json.ToJSON FileMetadata


-- | Resolved file with full metadata (available in RequestContext.files)
data ResolvedFile = ResolvedFile
  { ref :: FileRef
  , metadata :: FileMetadata
  , blobKey :: BlobKey
  , uploadedAt :: Int64
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON ResolvedFile
instance Json.ToJSON ResolvedFile


-- ==========================================================================
-- File Lifecycle Events
-- ==========================================================================

-- | Data for FileUploaded event
data FileUploadedData = FileUploadedData
  { fileRef :: FileRef
  , ownerHash :: OwnerHash
  , filename :: Text
  , contentType :: Text
  , sizeBytes :: Int64
  , blobKey :: BlobKey
  , expiresAt :: Int64
  , uploadedAt :: Int64
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileUploadedData
instance Json.ToJSON FileUploadedData


-- | Data for FileConfirmed event
data FileConfirmedData = FileConfirmedData
  { fileRef :: FileRef
  , confirmedByRequestId :: Text
  , confirmedAt :: Int64
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileConfirmedData
instance Json.ToJSON FileConfirmedData


-- | Data for FileDeleted event
data FileDeletedData = FileDeletedData
  { fileRef :: FileRef
  , reason :: FileDeletionReason
  , deletedAt :: Int64
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileDeletedData
instance Json.ToJSON FileDeletedData


-- | File lifecycle events (event-sourced state)
-- Each constructor carries a dedicated record type to avoid partial field selectors
data FileUploadEvent
  = FileUploaded FileUploadedData
  | FileConfirmed FileConfirmedData
  | FileDeleted FileDeletedData
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileUploadEvent
instance Json.ToJSON FileUploadEvent


-- | Reason why a file was deleted
data FileDeletionReason
  = Orphaned -- TTL expired without confirmation
  | UserRequested -- Explicit deletion by owner
  | AdminPurge -- System cleanup
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileDeletionReason
instance Json.ToJSON FileDeletionReason


-- ==========================================================================
-- Configuration
-- ==========================================================================

-- | Configuration for file uploads
data FileUploadConfig = FileUploadConfig
  { pendingTtlSeconds :: Int64 -- How long before unconfirmed files are cleaned up (default: 21600 = 6 hours)
  , cleanupIntervalSeconds :: Int64 -- How often the cleaner runs (default: 900 = 15 minutes)
  , maxFileSizeBytes :: Int64 -- Maximum upload size (default: 10485760 = 10 MB)
  , allowedContentTypes :: Maybe (Array Text) -- MIME type allowlist (Nothing = all allowed)
  , storeOriginalFilename :: Bool -- Whether to store filename in events (default: True)
  }
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileUploadConfig
instance Json.ToJSON FileUploadConfig
