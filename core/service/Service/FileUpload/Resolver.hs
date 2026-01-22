module Service.FileUpload.Resolver (
  -- * Types
  ResolvedFile (..),
  ResolveError (..),

  -- * Resolution Functions
  resolveFileRef,
  resolveFileRefs,
) where

import Basics
import DateTime (DateTime)
import DateTime qualified
import Int qualified
import Json qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Service.FileUpload.BlobStore (BlobStore (..))
import Service.FileUpload.Core (BlobKey (..), FileRef (..), OwnerHash (..))
import Service.FileUpload.Lifecycle (FileUploadState (..))
import Service.FileUpload.Lifecycle qualified as Lifecycle
import Task (Task)
import Task qualified
import Text (Text)


-- ==========================================================================
-- Types
-- ==========================================================================

-- | A resolved file with validated metadata
-- This is what commands see in RequestContext.files
data ResolvedFile = ResolvedFile
  { ref :: FileRef
  -- ^ The original file reference
  , filename :: Text
  -- ^ Original filename
  , contentType :: Text
  -- ^ MIME content type
  , sizeBytes :: Int
  -- ^ File size in bytes
  , uploadedAt :: DateTime
  -- ^ When the file was uploaded
  , blobKey :: BlobKey
  -- ^ Internal key for blob retrieval (not exposed to commands)
  }
  deriving (Generic, Eq, Show)


instance Json.ToJSON ResolvedFile


instance Json.FromJSON ResolvedFile


-- | Errors that can occur during file resolution
data ResolveError
  = FileNotFound FileRef
  -- ^ No file upload exists for this reference
  | NotOwner FileRef
  -- ^ File exists but belongs to a different user
  | FileExpired FileRef
  -- ^ File upload has expired (pending TTL exceeded)
  | FileDeleted FileRef
  -- ^ File has been deleted
  | BlobMissing FileRef
  -- ^ File metadata exists but blob is missing from storage
  | StorageError Text
  -- ^ Error accessing blob store
  deriving (Generic, Eq, Show)


instance Json.ToJSON ResolveError


instance Json.FromJSON ResolveError


-- ==========================================================================
-- Resolution Functions
-- ==========================================================================

-- | Resolve a single file reference
--
-- Validates:
-- 1. File exists in event store
-- 2. File is not deleted
-- 3. File is not expired (if pending)
-- 4. Owner matches the requesting user
-- 5. Blob exists in storage
resolveFileRef ::
  forall err.
  BlobStore ->
  (FileRef -> Task err (Maybe FileUploadState)) ->
  OwnerHash ->
  FileRef ->
  Task ResolveError ResolvedFile
resolveFileRef blobStore getState requestOwner fileRef = do
  -- 1. Fetch file state
  maybeState <- getState fileRef
    |> Task.mapError (\_ -> StorageError "Failed to fetch file state")

  case maybeState of
    Nothing -> Task.throw (FileNotFound fileRef)
    Just state -> resolveFromState blobStore requestOwner fileRef state


-- | Resolve multiple file references
-- Returns a Map from FileRef to ResolvedFile
-- Fails fast on first error
resolveFileRefs ::
  forall err.
  BlobStore ->
  (FileRef -> Task err (Maybe FileUploadState)) ->
  OwnerHash ->
  [FileRef] ->
  Task ResolveError (Map FileRef ResolvedFile)
resolveFileRefs blobStore getState requestOwner refs = do
  -- Resolve each ref and accumulate into map
  resolveAll refs Map.empty
  where
    resolveAll :: [FileRef] -> Map FileRef ResolvedFile -> Task ResolveError (Map FileRef ResolvedFile)
    resolveAll [] acc = Task.yield acc
    resolveAll (ref : rest) acc = do
      resolved <- resolveFileRef blobStore getState requestOwner ref
      let newAcc = Map.set ref resolved acc
      resolveAll rest newAcc


-- ==========================================================================
-- Internal Helpers
-- ==========================================================================

-- | Resolve file from its state
resolveFromState ::
  BlobStore ->
  OwnerHash ->
  FileRef ->
  FileUploadState ->
  Task ResolveError ResolvedFile
resolveFromState blobStore requestOwner fileRef state = case state of
  Initial -> Task.throw (FileNotFound fileRef)

  Deleted -> Task.throw (FileDeleted fileRef)

  Pending pendingFile -> do
    let meta = Lifecycle.getMetadata (Pending pendingFile)
    case meta of
      Nothing -> Task.throw (FileNotFound fileRef)
      Just metadata -> do
        let ownerHash = pendingFile.ownerHash
        let expiresAt = pendingFile.expiresAt
        let blobKey = metadata.blobKey

        -- Check ownership
        if ownerHash != requestOwner
          then Task.throw (NotOwner fileRef)
          else pass

        -- Check expiry
        now <- DateTime.now |> Task.mapError (\_ -> StorageError "Failed to get current time")
        let expiresAtDateTime = DateTime.fromEpochSeconds expiresAt
        if now >= expiresAtDateTime
          then Task.throw (FileExpired fileRef)
          else pass

        -- Check blob exists
        blobExists <- blobStore.exists blobKey
          |> Task.mapError (\_ -> StorageError "Failed to check blob existence")
        if not blobExists
          then Task.throw (BlobMissing fileRef)
          else pass

        -- Build resolved file
        Task.yield ResolvedFile
          { ref = fileRef
          , filename = metadata.filename
          , contentType = metadata.contentType
          , sizeBytes = Int.fromInt64 metadata.sizeBytes
          , uploadedAt = DateTime.fromEpochSeconds metadata.uploadedAt
          , blobKey = blobKey
          }

  Confirmed confirmedFile -> do
    let meta = Lifecycle.getMetadata (Confirmed confirmedFile)
    case meta of
      Nothing -> Task.throw (FileNotFound fileRef)
      Just metadata -> do
        let ownerHash = confirmedFile.ownerHash
        let blobKey = metadata.blobKey

        -- Check ownership
        if ownerHash != requestOwner
          then Task.throw (NotOwner fileRef)
          else pass

        -- Confirmed files don't expire, but check blob exists
        blobExists <- blobStore.exists blobKey
          |> Task.mapError (\_ -> StorageError "Failed to check blob existence")
        if not blobExists
          then Task.throw (BlobMissing fileRef)
          else pass

        -- Build resolved file
        Task.yield ResolvedFile
          { ref = fileRef
          , filename = metadata.filename
          , contentType = metadata.contentType
          , sizeBytes = Int.fromInt64 metadata.sizeBytes
          , uploadedAt = DateTime.fromEpochSeconds metadata.uploadedAt
          , blobKey = blobKey
          }
