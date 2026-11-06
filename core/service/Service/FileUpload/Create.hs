-- | Create application-owned files that remain available until deletion.
--
-- Call 'create' with the application's file upload setup and a request whose
-- owner and creation key come from an authorized document job.
module Service.FileUpload.Create (
  create,
) where

import Array qualified
import Basics
import Bytes qualified
import DateTime qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.FileUpload.BlobStore (BlobStore (..))
import Service.FileUpload.Core (
  BlobKey (..),
  ContentHash (..),
  CreateFileRequest (..),
  FileCreation (..),
  FileCreationError (..),
  FileRef (..),
  InternalFileUploadConfig (..),
  OwnerHash (..),
 )
import Service.FileUpload.Lifecycle (FileUploadState (..))
import Service.FileUpload.Lifecycle qualified as Lifecycle
import Service.FileUpload.Web (FileStateStore (..), FileUploadSetup (..))
import Service.FileUpload.Web qualified as Web
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Uuid qualified


-- | Create or recover a retained file for one owner and logical job key.
create :: FileUploadSetup -> CreateFileRequest -> Task FileCreationError FileRef
create setup request = do
  validateRequest setup.config request
  let ownerHash = OwnerHash request.owner
  let fileSize = fromIntegral (Bytes.length request.content) :: Int64
  let contentHash = Web.computeContentHash request.content
  let effectiveFilename =
        if setup.config.storeOriginalFilename
          then Text.left 255 request.filename
          else ""
  candidate <- newFileCreation ownerHash request.creationKey effectiveFilename request.contentType fileSize contentHash
  canonical <-
    setup.stateStore.reserveCreation candidate
      |> Task.mapError (\_ -> FileCreationStateFailure)
  validateCanonicalMetadata canonical effectiveFilename request.contentType
  currentState <-
    setup.stateStore.getState canonical.fileRef
      |> Task.mapError (\_ -> FileCreationStateFailure)
  case currentState of
    Just Deleted -> Task.throw FileCreationDeleted
    Just (Retained retained) -> recoverRetained setup request canonical retained fileSize contentHash
    Nothing -> createUnpublished setup request canonical fileSize contentHash
    Just Initial -> createUnpublished setup request canonical fileSize contentHash
    Just _ -> Task.throw FileCreationConflict


validateRequest :: InternalFileUploadConfig -> CreateFileRequest -> Task FileCreationError ()
validateRequest config request = do
  Task.when
    (Text.isEmpty request.owner || Text.length request.owner > 255 || Text.any (== '\NUL') request.owner)
    (Task.throw InvalidFileOwner)
  Task.when
    (Text.isEmpty request.creationKey || Text.length request.creationKey > 255 || Text.any (== '\NUL') request.creationKey)
    (Task.throw InvalidCreationKey)
  Task.when
    ( Text.length request.filename
        > 1000
        || Text.any (\character -> character < ' ' || character == '\DEL') request.filename
    )
    (Task.throw InvalidFileMetadata)
  Task.when
    ( Text.isEmpty request.contentType
        || Text.length request.contentType
        > 255
        || Text.any (\character -> character < ' ' || character == '\DEL') request.contentType
    )
    (Task.throw InvalidFileMetadata)
  let fileSize = fromIntegral (Bytes.length request.content) :: Int64
  Task.when (fileSize > config.maxFileSizeBytes) (Task.throw FileCreationTooLarge)
  case config.allowedContentTypes of
    Nothing -> Task.yield ()
    Just allowedTypes ->
      Task.unless
        (Array.contains request.contentType allowedTypes)
        (Task.throw InvalidFileMetadata)


newFileCreation :: OwnerHash -> Text -> Text -> Text -> Int64 -> ContentHash -> Task FileCreationError FileCreation
newFileCreation ownerHash creationKey filename contentType sizeBytes contentHash = do
  fileRefUuid <-
    Uuid.generate
      |> Task.mapError (\_ -> FileCreationStateFailure)
  blobKeyUuid <-
    Uuid.generate
      |> Task.mapError (\_ -> FileCreationStateFailure)
  now <-
    DateTime.now
      |> Task.mapError (\_ -> FileCreationStateFailure)
  Task.yield
    FileCreation
      { ownerHash = ownerHash,
        creationKey = creationKey,
        fileRef = FileRef [fmt|file_#{Uuid.toText fileRefUuid}|],
        blobKey = BlobKey (Uuid.toText blobKeyUuid),
        filename = filename,
        contentType = contentType,
        sizeBytes = sizeBytes,
        contentHash = contentHash,
        createdAt = DateTime.toEpochSeconds now
      }


validateCanonicalMetadata :: FileCreation -> Text -> Text -> Task FileCreationError ()
validateCanonicalMetadata canonical filename contentType = do
  Task.when
    (canonical.filename != filename || canonical.contentType != contentType)
    (Task.throw FileCreationConflict)


createUnpublished ::
  FileUploadSetup -> CreateFileRequest -> FileCreation -> Int64 -> ContentHash -> Task FileCreationError FileRef
createUnpublished setup request canonical fileSize contentHash = do
  validateContentFingerprint canonical fileSize contentHash
  setup.blobStore.store canonical.blobKey request.content
    |> Task.mapError (\_ -> FileCreationStorageFailure)
  publishResult <-
    setup.stateStore.publishRetained canonical
      |> Task.asResult
  case publishResult of
    Ok _ -> Task.yield canonical.fileRef
    Err _ -> do
      currentState <-
        setup.stateStore.getState canonical.fileRef
          |> Task.mapError (\_ -> FileCreationStateFailure)
      case currentState of
        Just (Retained retained) -> do
          validateRetainedState canonical retained
          Task.yield canonical.fileRef
        Just Deleted -> Task.throw FileCreationDeleted
        _ -> Task.throw FileCreationStateFailure


recoverRetained ::
  FileUploadSetup ->
  CreateFileRequest ->
  FileCreation ->
  Lifecycle.RetainedFile ->
  Int64 ->
  ContentHash ->
  Task FileCreationError FileRef
recoverRetained setup request canonical retained fileSize contentHash = do
  validateRetainedState canonical retained
  blobExists <-
    setup.blobStore.exists canonical.blobKey
      |> Task.mapError (\_ -> FileCreationStorageFailure)
  if blobExists
    then Task.yield canonical.fileRef
    else do
      validateContentFingerprint canonical fileSize contentHash
      setup.blobStore.store canonical.blobKey request.content
        |> Task.mapError (\_ -> FileCreationStorageFailure)
      currentState <-
        setup.stateStore.getState canonical.fileRef
          |> Task.mapError (\_ -> FileCreationStateFailure)
      case currentState of
        Just (Retained currentRetained) -> do
          validateRetainedState canonical currentRetained
          Task.yield canonical.fileRef
        Just Deleted -> Task.throw FileCreationDeleted
        _ -> Task.throw FileCreationStateFailure


validateRetainedState :: FileCreation -> Lifecycle.RetainedFile -> Task FileCreationError ()
validateRetainedState canonical retained = do
  let metadata = retained.metadata
  Task.unless
    ( retained.ownerHash
        == canonical.ownerHash
        && metadata.ref
        == canonical.fileRef
        && metadata.blobKey
        == canonical.blobKey
        && metadata.filename
        == canonical.filename
        && metadata.contentType
        == canonical.contentType
        && metadata.sizeBytes
        == canonical.sizeBytes
        && metadata.contentHash
        == canonical.contentHash
        && metadata.uploadedAt
        == canonical.createdAt
    )
    (Task.throw FileCreationStateFailure)


validateContentFingerprint :: FileCreation -> Int64 -> ContentHash -> Task FileCreationError ()
validateContentFingerprint canonical fileSize contentHash = do
  Task.unless
    (canonical.sizeBytes == fileSize && canonical.contentHash == contentHash)
    (Task.throw FileCreationConflict)
