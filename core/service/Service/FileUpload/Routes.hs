module Service.FileUpload.Routes (
  -- * Request/Response Types
  UploadRequest (..),
  UploadResponse (..),
  UploadError (..),

  -- * Handlers
  handleUpload,
) where

import Array (Array)
import Array qualified
import Basics
import Bytes (Bytes)
import Bytes qualified
import Data.Aeson qualified as GhcAeson
import DateTime (DateTime)
import DateTime qualified
import Json qualified
import Maybe (Maybe (..))
import Service.FileUpload.BlobStore (BlobStore (..), BlobStoreError (..))
import Service.FileUpload.Core (BlobKey (..), FileRef (..), FileUploadConfig (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Uuid qualified


-- ==========================================================================
-- Request/Response Types
-- ==========================================================================

-- | Upload request data (parsed from multipart form)
data UploadRequest = UploadRequest
  { filename :: Text
  -- ^ Original filename (may be empty)
  , contentType :: Text
  -- ^ MIME content type
  , content :: Bytes
  -- ^ File content
  , ownerHash :: Text
  -- ^ Hash of owner identity (for ownership checks)
  }
  deriving (Generic, Eq, Show)


-- | Successful upload response
-- Note: blobKey is internal and intentionally omitted from JSON serialization
data UploadResponse = UploadResponse
  { fileRef :: FileRef
  -- ^ Opaque reference for use in commands
  , blobKey :: BlobKey
  -- ^ Internal key where blob is stored (NOT exposed to clients via JSON)
  , filename :: Text
  -- ^ Original filename
  , contentType :: Text
  -- ^ MIME content type
  , sizeBytes :: Int64
  -- ^ File size in bytes
  , expiresAt :: DateTime
  -- ^ When the pending upload expires
  }
  deriving (Generic, Eq, Show)


-- | Custom ToJSON instance that omits internal blobKey field
instance Json.ToJSON UploadResponse where
  toJSON response = GhcAeson.object
    [ ("fileRef", GhcAeson.toJSON response.fileRef)
    , ("filename", GhcAeson.toJSON response.filename)
    , ("contentType", GhcAeson.toJSON response.contentType)
    , ("sizeBytes", GhcAeson.toJSON response.sizeBytes)
    , ("expiresAt", GhcAeson.toJSON response.expiresAt)
    ]


-- | Upload errors
data UploadError
  = FileTooLarge
      { maxBytes :: Int64
      , actualBytes :: Int64
      }
  | InvalidContentType
      { contentType :: Text
      , allowedTypes :: Array Text
      }
  | InvalidFilename Text
  | StorageFailure Text
  deriving (Generic, Eq, Show)


instance Json.ToJSON UploadError


instance Json.FromJSON UploadError


-- ==========================================================================
-- Handlers
-- ==========================================================================

-- | Handle file upload request
-- 1. Validate content type and size
-- 2. Generate FileRef and BlobKey
-- 3. Store blob in BlobStore
-- 4. Return response with metadata
--
-- Note: Event emission (FileUploaded) is handled by the caller (Transport layer)
-- to allow for transaction coordination with EventStore
handleUpload ::
  FileUploadConfig ->
  BlobStore ->
  UploadRequest ->
  Task UploadError UploadResponse
handleUpload config blobStore request = do
  -- 1. Validate file size
  let actualSize = fromIntegral (Bytes.length request.content) :: Int64
  if actualSize > config.maxFileSizeBytes
    then Task.throw FileTooLarge
      { maxBytes = config.maxFileSizeBytes
      , actualBytes = actualSize
      }
    else pass

  -- 2. Validate content type (if restrictions are configured)
  case config.allowedContentTypes of
    Just allowedTypes -> do
      if Array.contains request.contentType allowedTypes
        then pass
        else Task.throw InvalidContentType
          { contentType = request.contentType
          , allowedTypes = allowedTypes
          }
    Nothing -> pass  -- All types allowed

  -- 3. Sanitize and truncate filename (max 255 chars for filesystem compatibility)
  let maxFilenameLength = 255
  let sanitizedFilename = request.filename
        |> Text.left maxFilenameLength

  -- 4. Generate FileRef and BlobKey (both random UUIDs)
  fileRefUuid <- Uuid.generate
  blobKeyUuid <- Uuid.generate

  let fileRef = FileRef [fmt|file_#{Uuid.toText fileRefUuid}|]
  let blobKey = BlobKey (Uuid.toText blobKeyUuid)

  -- 5. Store blob in BlobStore
  _ <- blobStore.store blobKey request.content
    |> Task.mapError (\e -> StorageFailure (blobStoreErrorToText e))

  -- 6. Calculate expiration time
  now <- DateTime.now |> Task.mapError (\_ -> StorageFailure "Failed to get current time")
  let expiresAt = DateTime.addSeconds config.pendingTtlSeconds now

  -- 7. Return response
  Task.yield UploadResponse
    { fileRef = fileRef
    , blobKey = blobKey
    , filename = sanitizedFilename
    , contentType = request.contentType
    , sizeBytes = actualSize
    , expiresAt = expiresAt
    }


-- ==========================================================================
-- Helpers
-- ==========================================================================

-- | Convert BlobStoreError to Text for error messages
blobStoreErrorToText :: BlobStoreError -> Text
blobStoreErrorToText err = case err of
  NotFound (BlobKey key) -> [fmt|Blob not found: #{key}|]
  StorageError msg -> [fmt|Storage error: #{msg}|]
  InvalidBlobKey msg -> [fmt|Invalid blob key: #{msg}|]
