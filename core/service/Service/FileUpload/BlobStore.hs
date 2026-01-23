module Service.FileUpload.BlobStore (
  -- * BlobStore Interface
  BlobStore (..),

  -- * Error Types
  BlobStoreError (..),

  -- * BlobKey Re-export
  BlobKey (..),
) where

import Basics
import Bytes (Bytes)
import Service.FileUpload.Core (BlobKey (..))
import Task (Task)
import Text (Text)


-- ==========================================================================
-- Error Types
-- ==========================================================================

-- | Errors that can occur when interacting with the blob store
data BlobStoreError
  = NotFound BlobKey -- Requested blob does not exist
  | StorageError Text -- Backend storage error
  | InvalidBlobKey Text -- Blob key validation failed
  deriving (Generic, Eq)

instance Show BlobStoreError where
  show err = case err of
    NotFound _ -> "NotFound: blob does not exist"
    StorageError msg -> [fmt|StorageError: #{msg}|]
    InvalidBlobKey msg -> [fmt|InvalidBlobKey: #{msg}|]


-- ==========================================================================
-- BlobStore Interface
-- ==========================================================================

-- | Interface for blob storage backends
-- Implementations: Local (filesystem), S3 (future), GCS (future)
data BlobStore = BlobStore
  { store :: BlobKey -> Bytes -> Task BlobStoreError Unit
  -- ^ Store bytes under the given key (overwrites if exists)
  , retrieve :: BlobKey -> Task BlobStoreError Bytes
  -- ^ Retrieve bytes for the given key
  , delete :: BlobKey -> Task BlobStoreError Unit
  -- ^ Delete blob (idempotent - succeeds if already deleted)
  , exists :: BlobKey -> Task BlobStoreError Bool
  -- ^ Check if blob exists
  }
