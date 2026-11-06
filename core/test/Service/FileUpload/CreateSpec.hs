-- | Observable behavior for application-owned retained file creation.
-- Run 'spec' in the nhcore-test-service suite to exercise the public creation contract.
module Service.FileUpload.CreateSpec where

import Array qualified
import AsyncTask qualified
import Bytes qualified
import Control.Concurrent.MVar qualified as GhcMVar
import Core
import Directory qualified
import Service.FileUpload.BlobStore (BlobStore (..), BlobStoreError (..))
import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..), createBlobStore)
import Service.FileUpload.Core (
  BlobKey (..),
  CreateFileRequest (..),
  FileCreation (..),
  FileCreationError (..),
  FileDeletedData (..),
  FileDeletionReason (..),
  FileRef (..),
  FileUploadEvent (..),
  InternalFileUploadConfig (..),
  OwnerHash (..),
 )
import Service.FileUpload.Create qualified as Create
import Service.FileUpload.Lifecycle (FileMetadata (..), FileUploadState (..), PendingFile (..), RetainedFile (..))
import Service.FileUpload.Web (
  FileStateStore (..),
  FileUploadSetup (..),
  inMemoryFileStateStore,
  newInMemoryFileStateStore,
 )
import Service.FileUpload.Web qualified as Web
import System.Timeout qualified as GhcTimeout
import Task qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Service.FileUpload.Create" do
    it "retains the first successful bytes and returns the canonical ref for ready retries" \_ -> do
      withCreateEnv \env -> do
        let original = Bytes.pack [0x25, 0x50, 0x44, 0x46, 0x0d, 0x0a, 0x00]
        let regenerated = Bytes.pack [0x25, 0x50, 0x44, 0x46, 0x0d, 0x0a, 0x01, 0x02]
        first <- createFile env.setup (request "owner-a" "job-1" "report.pdf" original)
        retry <- createFile env.setup (request "owner-a" "job-1" "report.pdf" regenerated)
        retry |> shouldBe first
        state <- env.setup.stateStore.getState first
        case state of
          Just (Retained retained) -> do
            retained.metadata.filename |> shouldBe "report.pdf"
            retained.metadata.contentType |> shouldBe "application/pdf"
            retained.metadata.uploadedAt |> shouldSatisfy (> 0)
            restored <- env.setup.blobStore.retrieve retained.metadata.blobKey |> Task.mapError toText
            restored |> shouldBe original
          _ -> fail "Expected a retained file after creation"

    it "rejects changed metadata for the same owner and creation key" \_ -> do
      withCreateEnv \env -> do
        let content = "pdf bytes" |> Text.toBytes
        _ <- createFile env.setup (request "owner-a" "job-2" "report.pdf" content)
        result <- Create.create env.setup (request "owner-a" "job-2" "renamed.pdf" content) |> Task.asResult
        result |> shouldBe (Err FileCreationConflict)

    it "rejects changed bytes for an unpublished reservation before writing" \_ -> do
      withCreateEnv \env -> do
        let original = Bytes.pack [0x01, 0x02, 0x03]
        let changed = Bytes.pack [0x01, 0x02, 0x04]
        let creation = reservedCreation "owner-a" "job-3" original
        _ <- env.setup.stateStore.reserveCreation creation
        result <- Create.create env.setup (request "owner-a" "job-3" "report.pdf" changed) |> Task.asResult
        result |> shouldBe (Err FileCreationConflict)
        exists <- env.setup.blobStore.exists creation.blobKey |> Task.mapError toText
        exists |> shouldBe False
        state <- env.setup.stateStore.getState creation.fileRef
        state |> shouldBe Nothing

    it "scopes a creation key to its owner" \_ -> do
      withCreateEnv \env -> do
        let content = "pdf bytes" |> Text.toBytes
        first <- createFile env.setup (request "owner-a" "job-4" "report.pdf" content)
        second <- createFile env.setup (request "owner-b" "job-4" "report.pdf" content)
        (first != second) |> shouldBe True

    it "rejects a retry after the retained file is deleted" \_ -> do
      withCreateEnv \env -> do
        let content = "pdf bytes" |> Text.toBytes
        fileRef <- createFile env.setup (request "owner-a" "job-5" "report.pdf" content)
        env.setup.stateStore.updateState fileRef (deleteEvent fileRef)
        result <- Create.create env.setup (request "owner-a" "job-5" "report.pdf" content) |> Task.asResult
        result |> shouldBe (Err FileCreationDeleted)
        state <- env.setup.stateStore.getState fileRef
        state |> shouldBe (Just Deleted)

    it "restores a missing retained blob only from the original bytes" \_ -> do
      withCreateEnv \env -> do
        let original = Bytes.pack [0x25, 0x50, 0x44, 0x46, 0x0d, 0x0a, 0x00]
        let changed = Bytes.pack [0x25, 0x50, 0x44, 0x46, 0x0d, 0x0a, 0x01]
        fileRef <- createFile env.setup (request "owner-a" "job-6" "report.pdf" original)
        state <- env.setup.stateStore.getState fileRef
        case state of
          Just (Retained retained) -> do
            env.setup.blobStore.delete retained.metadata.blobKey |> Task.mapError toText
            changedResult <- Create.create env.setup (request "owner-a" "job-6" "report.pdf" changed) |> Task.asResult
            changedResult |> shouldBe (Err FileCreationConflict)
            originalRetry <- createFile env.setup (request "owner-a" "job-6" "report.pdf" original)
            originalRetry |> shouldBe fileRef
            restored <- env.setup.blobStore.retrieve retained.metadata.blobKey |> Task.mapError toText
            restored |> shouldBe original
          _ -> fail "Expected a retained file before removing its blob"

    it "propagates retained blob existence errors" \_ -> do
      withCreateEnv \env -> do
        let content = "pdf bytes" |> Text.toBytes
        _ <- createFile env.setup (request "owner-a" "job-7" "report.pdf" content)
        let failingBlobStore =
              env.setup.blobStore
                { exists = \_ -> Task.throw (StorageError "injected exists failure")
                }
        let setup = env.setup {blobStore = failingBlobStore}
        result <- Create.create setup (request "owner-a" "job-7" "report.pdf" content) |> Task.asResult
        result |> shouldBe (Err FileCreationStorageFailure)

    it "rejects filename control characters before storage" \_ -> do
      withCreateEnv \env -> do
        let content = "pdf bytes" |> Text.toBytes
        let badFilename = request "owner-a" "job-8" "bad\NULname.pdf" content
        let badContentType = requestWithContentType "owner-a" "job-9" "report.pdf" "application/\DELpdf" content
        filenameResult <- Create.create env.setup badFilename |> Task.asResult
        contentTypeResult <- Create.create env.setup badContentType |> Task.asResult
        filenameResult |> shouldBe (Err InvalidFileMetadata)
        contentTypeResult |> shouldBe (Err InvalidFileMetadata)

    it "rejects NUL in owner and creation key before PostgreSQL storage" \_ -> do
      withCreateEnv \env -> do
        let content = "pdf bytes" |> Text.toBytes
        ownerResult <- Create.create env.setup (request "owner\NULa" "job-10a" "report.pdf" content) |> Task.asResult
        keyResult <- Create.create env.setup (request "owner-a" "job\NUL10b" "report.pdf" content) |> Task.asResult
        ownerResult |> shouldBe (Err InvalidFileOwner)
        keyResult |> shouldBe (Err InvalidCreationKey)

    it "keeps the reservation after a blob write failure and recovers on retry" \_ -> do
      withCreateEnv \env -> do
        let content = Bytes.pack [0x25, 0x50, 0x44, 0x46, 0x0d, 0x0a, 0x00]
        (recordingStore, reservedCreation) <- recordReservation env.setup.stateStore
        let failingBlobStore =
              env.setup.blobStore
                { store = \_ _ -> Task.throw (StorageError "injected initial store failure")
                }
        let failingSetup = env.setup {blobStore = failingBlobStore, stateStore = recordingStore}
        failed <- Create.create failingSetup (request "owner-a" "job-17" "report.pdf" content) |> Task.asResult
        failed |> shouldBe (Err FileCreationStorageFailure)
        reserved <- Task.fromIO (GhcMVar.readMVar reservedCreation)
        stateBeforeRetry <- env.setup.stateStore.getState reserved.fileRef
        stateBeforeRetry |> shouldBe Nothing
        blobBeforeRetry <- env.setup.blobStore.exists reserved.blobKey |> Task.mapError toText
        blobBeforeRetry |> shouldBe False
        retried <- createFile env.setup (request "owner-a" "job-17" "report.pdf" content)
        retried |> shouldBe reserved.fileRef
        stateAfterRetry <- env.setup.stateStore.getState retried
        stateAfterRetry |> shouldSatisfy (\current -> case current of Just (Retained _) -> True; _ -> False)

    it "reports reservation failure and recovers on retry" \_ -> do
      withCreateEnv \env -> do
        let content = "pdf bytes" |> Text.toBytes
        let failingStateStore =
              env.setup.stateStore
                { reserveCreation = \_ -> Task.throw "injected reservation failure"
                }
        let failingSetup = env.setup {stateStore = failingStateStore}
        failed <- Create.create failingSetup (request "owner-a" "job-18" "report.pdf" content) |> Task.asResult
        failed |> shouldBe (Err FileCreationStateFailure)
        retried <- createFile env.setup (request "owner-a" "job-18" "report.pdf" content)
        stateAfterRetry <- env.setup.stateStore.getState retried
        stateAfterRetry |> shouldSatisfy (\current -> case current of Just (Retained _) -> True; _ -> False)

    it "recovers after cancellation between storing canonical bytes and publishing retained state" \_ -> do
      withCreateEnv \env -> do
        stored <- Task.fromIO (GhcMVar.newEmptyMVar :: IO (GhcMVar.MVar ()))
        releasePublication <- Task.fromIO (GhcMVar.newEmptyMVar :: IO (GhcMVar.MVar ()))
        (recordingStore, reservedCreation) <- recordReservation env.setup.stateStore
        let originalBlobStore = env.setup.blobStore
        let blockedBlobStore =
              originalBlobStore
                { store = \blobKey content -> do
                    originalBlobStore.store blobKey content
                    GhcMVar.putMVar stored () |> Task.fromIO
                    GhcMVar.takeMVar releasePublication |> Task.fromIO
                    Task.yield ()
                }
        let blockedSetup = env.setup {blobStore = blockedBlobStore, stateStore = recordingStore}
        let content = Bytes.pack [0x25, 0x50, 0x44, 0x46, 0x0d, 0x0a, 0x00]
        createTask <-
          AsyncTask.run
            (Create.create blockedSetup (request "owner-a" "job-19" "report.pdf" content) |> Task.mapError toText)
        let cleanup = do
              _ <- GhcMVar.tryPutMVar releasePublication () |> Task.fromIO
              AsyncTask.requestThreadTermination createTask
        Task.finally cleanup do
          storeStart <- Task.fromIO (GhcTimeout.timeout 5000000 (GhcMVar.takeMVar stored))
          case storeStart of
            Nothing -> fail "CreateFile did not reach the blob store"
            Just () -> pass
          canonical <- Task.fromIO (GhcMVar.readMVar reservedCreation)
          blobExists <- originalBlobStore.exists canonical.blobKey |> Task.mapError toText
          blobExists |> shouldBe True
          stateBeforeRetry <- env.setup.stateStore.getState canonical.fileRef
          stateBeforeRetry |> shouldBe Nothing
          AsyncTask.requestThreadTermination createTask
          outcome <- AsyncTask.waitOutcome createTask :: Task Text (AsyncTask.Outcome Text FileRef)
          case outcome of
            AsyncTask.Interrupted -> pass
            _ -> fail "Expected cancellation before retained publication"
          retried <- createFile env.setup (request "owner-a" "job-19" "report.pdf" content)
          retried |> shouldBe canonical.fileRef
          finalState <- env.setup.stateStore.getState retried
          finalState |> shouldSatisfy (\current -> case current of Just (Retained _) -> True; _ -> False)

    it "enforces configured size and content type limits" \_ -> do
      withCreateEnv \env -> do
        let restrictedConfig =
              env.setup.config
                { maxFileSizeBytes = 3,
                  allowedContentTypes = Just (Array.fromLinkedList ["application/pdf"])
                }
        let setup = env.setup {config = restrictedConfig}
        let content = Bytes.pack [0x01, 0x02, 0x03, 0x04]
        oversized <- Create.create setup (request "owner-a" "job-11" "report.pdf" content) |> Task.asResult
        wrongType <-
          Create.create setup (requestWithContentType "owner-a" "job-12" "report.pdf" "text/plain" (Bytes.pack [0x01]))
            |> Task.asResult
        oversized |> shouldBe (Err FileCreationTooLarge)
        wrongType |> shouldBe (Err InvalidFileMetadata)

    it "keeps retained files when expired pending uploads are cleaned up" \_ -> do
      withCreateEnv \env -> do
        let content = "retained document" |> Text.toBytes
        retainedRef <- createFile env.setup (request "owner-a" "job-13" "report.pdf" content)
        let expiredRef = FileRef "expired-pending-file"
        let expiredBlobKey = BlobKey "expired-pending-blob"
        env.setup.blobStore.store expiredBlobKey ("expired upload" |> Text.toBytes) |> Task.mapError toText
        env.setup.stateStore.setState
          expiredRef
          ( Pending
              PendingFile
                { metadata =
                    FileMetadata
                      { ref = expiredRef,
                        filename = "temporary.txt",
                        contentType = "text/plain",
                        sizeBytes = 14,
                        blobKey = expiredBlobKey,
                        uploadedAt = 1,
                        contentHash = Web.computeContentHash ("expired upload" |> Text.toBytes)
                      },
                  ownerHash = OwnerHash "owner-a",
                  expiresAt = 1
                }
          )
        cleaned <- Web.cleanupExpiredFiles env.setup.stateStore env.setup.blobStore env.stateMap
        cleaned |> shouldBe 1
        pendingState <- env.setup.stateStore.getState expiredRef
        pendingState |> shouldBe (Just Deleted)
        retainedState <- env.setup.stateStore.getState retainedRef
        retainedState |> shouldSatisfy (\current -> case current of Just (Retained _) -> True; _ -> False)
        retainedBlobExists <- do
          case retainedState of
            Just (Retained retained) -> env.setup.blobStore.exists retained.metadata.blobKey |> Task.mapError toText
            _ -> Task.yield False
        retainedBlobExists |> shouldBe True

    it "recovers after a publication failure without discarding the canonical blob" \_ -> do
      withCreateEnv \env -> do
        let content = Bytes.pack [0x25, 0x50, 0x44, 0x46, 0x0d, 0x0a, 0x00]
        let creation = reservedCreation "owner-a" "job-14" content
        _ <- env.setup.stateStore.reserveCreation creation
        let failingStore =
              env.setup.stateStore
                { publishRetained = \_ -> Task.throw "injected publication failure"
                }
        let failingSetup = env.setup {stateStore = failingStore}
        firstResult <- Create.create failingSetup (request "owner-a" "job-14" "report.pdf" content) |> Task.asResult
        firstResult |> shouldBe (Err FileCreationStateFailure)
        blobAfterFailure <- env.setup.blobStore.exists creation.blobKey |> Task.mapError toText
        blobAfterFailure |> shouldBe True
        stateAfterFailure <- env.setup.stateStore.getState creation.fileRef
        stateAfterFailure |> shouldBe Nothing
        retried <- createFile env.setup (request "owner-a" "job-14" "report.pdf" content)
        retried |> shouldBe creation.fileRef

    it "accepts a concurrent publication whose acknowledgement was lost" \_ -> do
      withCreateEnv \env -> do
        let content = Bytes.pack [0x25, 0x50, 0x44, 0x46, 0x0d, 0x0a, 0x00]
        let stateStore =
              env.setup.stateStore
                { publishRetained = \creation -> do
                    env.setup.stateStore.publishRetained creation
                    Task.throw "injected lost acknowledgement"
                }
        let setup = env.setup {stateStore = stateStore}
        created <- createFile setup (request "owner-a" "job-15" "report.pdf" content)
        state <- env.setup.stateStore.getState created
        state |> shouldSatisfy (\current -> case current of Just (Retained _) -> True; _ -> False)

    it "returns one canonical ref for concurrent equal calls" \_ -> do
      withCreateEnv \env -> do
        let content = "same pdf bytes" |> Text.toBytes
        (first, second) <-
          AsyncTask.runConcurrently
            ( createFile env.setup (request "owner-a" "job-10" "report.pdf" content),
              createFile env.setup (request "owner-a" "job-10" "report.pdf" content)
            )
        first |> shouldBe second
        state <- env.setup.stateStore.getState first
        state |> shouldSatisfy (\current -> case current of Just (Retained _) -> True; _ -> False)


data CreateEnv = CreateEnv
  { setup :: FileUploadSetup,
    stateMap :: Web.InMemoryFileStateStore
  }


recordReservation :: FileStateStore -> Task Text (FileStateStore, GhcMVar.MVar FileCreation)
recordReservation original = do
  canonicalCreation <- Task.fromIO (GhcMVar.newEmptyMVar :: IO (GhcMVar.MVar FileCreation))
  let stateStore =
        original
          { reserveCreation = \candidate -> do
              canonical <- original.reserveCreation candidate
              Task.fromIO (GhcMVar.putMVar canonicalCreation canonical)
              Task.yield canonical
          }
  Task.yield (stateStore, canonicalCreation)


withCreateEnv :: (CreateEnv -> Task Text a) -> Task Text a
withCreateEnv action = Directory.withTemporaryFolder toText \tempDir -> do
  blobStore <- createBlobStore LocalBlobStoreConfig {rootDir = tempDir} |> Task.mapError toText
  stateMap <- newInMemoryFileStateStore
  let stateStore = inMemoryFileStateStore stateMap
  let config =
        InternalFileUploadConfig
          { maxFileSizeBytes = 10485760,
            allowedContentTypes = Nothing,
            pendingTtlSeconds = 3600,
            cleanupIntervalSeconds = 900,
            storeOriginalFilename = True
          }
  let setup = FileUploadSetup {config, blobStore, stateStore}
  let env = CreateEnv {setup, stateMap}
  action env


request :: Text -> Text -> Text -> Bytes.Bytes -> CreateFileRequest
request ownerText key fileName content = requestWithContentType ownerText key fileName "application/pdf" content


requestWithContentType :: Text -> Text -> Text -> Text -> Bytes.Bytes -> CreateFileRequest
requestWithContentType ownerText key fileName typeText content =
  CreateFileRequest
    { owner = ownerText,
      creationKey = key,
      filename = fileName,
      contentType = typeText,
      content = content
    }


createFile :: FileUploadSetup -> CreateFileRequest -> Task Text FileRef
createFile setup requestValue = Create.create setup requestValue |> Task.mapError toText


reservedCreation :: Text -> Text -> Bytes.Bytes -> FileCreation
reservedCreation ownerText key content =
  FileCreation
    { ownerHash = OwnerHash ownerText,
      creationKey = key,
      fileRef = FileRef [fmt|file_reserved_#{key}|],
      blobKey = BlobKey [fmt|blob_reserved_#{key}|],
      filename = "report.pdf",
      contentType = "application/pdf",
      sizeBytes = fromIntegral (Bytes.length content),
      contentHash = Web.computeContentHash content,
      createdAt = 1700000000
    }


deleteEvent :: FileRef -> FileUploadEvent
deleteEvent fileRef =
  FileDeleted
    FileDeletedData
      { fileRef,
        reason = UserRequested,
        deletedAt = 1700000100
      }
