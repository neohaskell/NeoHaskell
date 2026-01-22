module Service.FileUpload.BlobStore.Local (
  -- * Configuration
  LocalBlobStoreConfig (..),

  -- * Constructor
  createBlobStore,
) where

import Basics
import Bytes (Bytes)
import Directory qualified
import File qualified
import Maybe (Maybe (..))
import Path (Path)
import Path qualified
import Result (Result (..))
import Service.FileUpload.BlobStore (BlobStore (..), BlobStoreError (..))
import Service.FileUpload.Core (BlobKey (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- ==========================================================================
-- Configuration
-- ==========================================================================

-- | Configuration for local filesystem blob store
data LocalBlobStoreConfig = LocalBlobStoreConfig
  { rootDir :: Path
  -- ^ Root directory where blobs are stored
  }
  deriving (Generic, Eq, Show)


-- ==========================================================================
-- Validation
-- ==========================================================================

-- | Validate blob key to prevent path traversal attacks
validateBlobKey :: BlobKey -> Result BlobStoreError Unit
validateBlobKey (BlobKey key) = do
  if Text.contains ".." key
    then Err (InvalidBlobKey "Path traversal not allowed")
    else
      if Text.contains "/" key
        then Err (InvalidBlobKey "Slashes not allowed in blob key")
        else
          if Text.startsWith "/" key
            then Err (InvalidBlobKey "Absolute paths not allowed")
            else Ok ()


-- | Get path for a blob key within the root directory
getBlobPath :: Path -> BlobKey -> Result BlobStoreError Path
getBlobPath rootDir blobKey@(BlobKey key) = do
  case validateBlobKey blobKey of
    Err e -> Err e
    Ok () -> case Path.fromText key of
      Nothing -> Err (InvalidBlobKey "Invalid path characters")
      Just keyPath -> Ok (rootDir |> Path.append keyPath)


-- ==========================================================================
-- Constructor
-- ==========================================================================

-- | Create a local filesystem blob store
createBlobStore :: LocalBlobStoreConfig -> Task Text BlobStore
createBlobStore config = do
  -- Ensure root directory exists
  _ <- Directory.createIfMissing config.rootDir
    |> Task.mapError (\e -> [fmt|Failed to create blob store root: #{show e}|])

  Task.yield
    BlobStore
      { store = storeImpl config.rootDir
      , retrieve = retrieveImpl config.rootDir
      , delete = deleteImpl config.rootDir
      , exists = existsImpl config.rootDir
      }


-- ==========================================================================
-- Implementation
-- ==========================================================================

storeImpl :: Path -> BlobKey -> Bytes -> Task BlobStoreError Unit
storeImpl rootDir blobKey bytes = do
  case getBlobPath rootDir blobKey of
    Err e -> Task.throw e
    Ok path -> do
      -- Get temp path for atomic write
      case Path.fromText [fmt|#{keyText}.tmp|] of
        Nothing -> Task.throw (InvalidBlobKey "Failed to create temp path")
        Just tempKeyPath -> do
          let tempPath = rootDir |> Path.append tempKeyPath

          -- Write to temp file first
          _ <- File.writeBytes tempPath bytes
            |> Task.mapError (\e -> StorageError [fmt|Write failed: #{show e}|])

          -- Atomic rename to final path
          File.rename tempPath path
            |> Task.mapError (\e -> StorageError [fmt|Rename failed: #{show e}|])
  where
    (BlobKey keyText) = blobKey


retrieveImpl :: Path -> BlobKey -> Task BlobStoreError Bytes
retrieveImpl rootDir blobKey = do
  case getBlobPath rootDir blobKey of
    Err e -> Task.throw e
    Ok path -> do
      fileExists <-
        File.exists path
          |> Task.mapError (\e -> StorageError [fmt|Exists check failed: #{show e}|])

      if fileExists
        then
          File.readBytes path
            |> Task.mapError (\e -> StorageError [fmt|Read failed: #{show e}|])
        else Task.throw (NotFound blobKey)


deleteImpl :: Path -> BlobKey -> Task BlobStoreError Unit
deleteImpl rootDir blobKey = do
  case getBlobPath rootDir blobKey of
    Err e -> Task.throw e
    Ok path -> do
      -- Idempotent delete - succeeds even if file doesn't exist
      File.deleteIfExists path
        |> Task.mapError (\e -> StorageError [fmt|Delete failed: #{show e}|])


existsImpl :: Path -> BlobKey -> Task BlobStoreError Bool
existsImpl rootDir blobKey = do
  case getBlobPath rootDir blobKey of
    Err e -> Task.throw e
    Ok path -> do
      File.exists path
        |> Task.mapError (\e -> StorageError [fmt|Exists check failed: #{show e}|])
