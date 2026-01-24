-- | Shared specification tests for FileStateStore implementations.
--
-- Both InMemory and PostgreSQL FileStateStore implementations should
-- pass these tests.
--
-- Usage:
--
-- @
-- -- In InMemorySpec.hs
-- import Test.Service.FileUpload.FileStateStore qualified as FileStateStoreSpec
--
-- spec = do
--   describe "InMemoryFileStateStore" do
--     let newStore = do
--           stateMap <- FileUpload.newInMemoryFileStateStore
--           Task.yield (FileUpload.inMemoryFileStateStore stateMap)
--     FileStateStoreSpec.spec newStore
-- @
module Test.Service.FileUpload.FileStateStore (
  spec,
) where

import Core
import Service.FileUpload.Core (
  BlobKey (..),
  FileConfirmedData (..),
  FileDeletedData (..),
  FileDeletionReason (..),
  FileRef (..),
  FileUploadEvent (..),
  FileUploadedData (..),
  OwnerHash (..),
 )
import Service.FileUpload.Lifecycle (FileUploadState (..))
import Service.FileUpload.Lifecycle qualified as Lifecycle
import Service.FileUpload.Web (FileStateStore (..))
import Task qualified
import Test
import Uuid qualified


spec :: Task Text FileStateStore -> Spec Unit
spec newStore = do
  describe "FileStateStore Specification" do
    -- ==========================================================================
    -- getState
    -- ==========================================================================
    describe "getState" do
      it "returns Nothing for non-existent file reference" \_ -> do
        store <- newStore
        let fileRef = FileRef "nonexistent_ref"
        result <- store.getState fileRef
        result |> shouldBe Nothing

      it "returns state after setState" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        let state = Lifecycle.initialState
        store.setState fileRef state
        result <- store.getState fileRef
        result |> shouldBe (Just state)

    -- ==========================================================================
    -- setState
    -- ==========================================================================
    describe "setState" do
      it "stores Initial state" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        store.setState fileRef Lifecycle.initialState
        result <- store.getState fileRef
        result |> shouldBe (Just Lifecycle.initialState)

      it "stores Pending state with all metadata" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        let state = mkPendingState fileRef
        store.setState fileRef state
        result <- store.getState fileRef
        result |> shouldBe (Just state)

      it "stores Confirmed state with all metadata" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        let state = mkConfirmedState fileRef
        store.setState fileRef state
        result <- store.getState fileRef
        result |> shouldBe (Just state)

      it "stores Deleted state" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        store.setState fileRef Deleted
        result <- store.getState fileRef
        result |> shouldBe (Just Deleted)

      it "overwrites existing state (upsert)" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        let state1 = mkPendingState fileRef
        let state2 = Deleted
        store.setState fileRef state1
        store.setState fileRef state2
        result <- store.getState fileRef
        result |> shouldBe (Just state2)

    -- ==========================================================================
    -- updateState
    -- ==========================================================================
    describe "updateState" do
      it "applies FileUploaded event to create Pending state" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        let event = mkFileUploadedEvent fileRef 1700000000 1700003600
        store.updateState fileRef event
        result <- store.getState fileRef
        case result of
          Just (Pending pending) -> do
            pending.metadata.ref |> shouldBe fileRef
            pending.expiresAt |> shouldBe 1700003600
          _ -> fail "Expected Pending state"

      it "applies FileConfirmed event to transition Pending -> Confirmed" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        -- First create pending state
        let uploadEvent = mkFileUploadedEvent fileRef 1700000000 1700003600
        store.updateState fileRef uploadEvent
        -- Then confirm it
        let confirmEvent = FileConfirmed FileConfirmedData
              { fileRef = fileRef
              , confirmedByRequestId = "request-123"
              , confirmedAt = 1700000100
              }
        store.updateState fileRef confirmEvent
        result <- store.getState fileRef
        case result of
          Just (Confirmed confirmed) -> do
            confirmed.confirmedByRequestId |> shouldBe "request-123"
          _ -> fail "Expected Confirmed state"

      it "applies FileDeleted event to transition to Deleted" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        -- First create pending state
        let uploadEvent = mkFileUploadedEvent fileRef 1700000000 1700003600
        store.updateState fileRef uploadEvent
        -- Then delete it
        let deleteEvent = FileDeleted FileDeletedData
              { fileRef = fileRef
              , reason = Orphaned
              , deletedAt = 1700003700
              }
        store.updateState fileRef deleteEvent
        result <- store.getState fileRef
        result |> shouldBe (Just Deleted)

      it "handles events idempotently" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        -- Apply same event twice
        let event = mkFileUploadedEvent fileRef 1700000000 1700003600
        store.updateState fileRef event
        store.updateState fileRef event
        result <- store.getState fileRef
        case result of
          Just (Pending _) -> pass
          _ -> fail "Expected Pending state after idempotent updates"

    -- ==========================================================================
    -- Multiple files
    -- ==========================================================================
    describe "multiple files" do
      it "stores different states for different file refs" \_ -> do
        store <- newStore
        fileRef1 <- generateFileRef
        fileRef2 <- generateFileRef
        let state1 = mkPendingState fileRef1
        let state2 = Deleted
        store.setState fileRef1 state1
        store.setState fileRef2 state2
        result1 <- store.getState fileRef1
        result2 <- store.getState fileRef2
        result1 |> shouldBe (Just state1)
        result2 |> shouldBe (Just state2)

      it "does not affect other files when updating one" \_ -> do
        store <- newStore
        fileRef1 <- generateFileRef
        fileRef2 <- generateFileRef
        let state1 = mkPendingState fileRef1
        let state2 = mkPendingState fileRef2
        store.setState fileRef1 state1
        store.setState fileRef2 state2
        -- Update only fileRef1
        store.setState fileRef1 Deleted
        result1 <- store.getState fileRef1
        result2 <- store.getState fileRef2
        result1 |> shouldBe (Just Deleted)
        result2 |> shouldBe (Just state2)


-- ==========================================================================
-- Test Helpers
-- ==========================================================================

-- | Generate a unique FileRef for testing
generateFileRef :: Task Text FileRef
generateFileRef = do
  uuid <- Uuid.generate
  Task.yield (FileRef [fmt|file_#{Uuid.toText uuid}|])


-- | Create a Pending state for testing
mkPendingState :: FileRef -> FileUploadState
mkPendingState fileRef =
  Pending Lifecycle.PendingFile
    { metadata = Lifecycle.FileMetadata
        { ref = fileRef
        , filename = "test-file.txt"
        , contentType = "text/plain"
        , sizeBytes = 1024
        , blobKey = BlobKey "blob_test"
        , uploadedAt = 1700000000
        }
    , ownerHash = OwnerHash "owner_test"
    , expiresAt = 1700003600
    }


-- | Create a Confirmed state for testing
mkConfirmedState :: FileRef -> FileUploadState
mkConfirmedState fileRef =
  Confirmed Lifecycle.ConfirmedFile
    { metadata = Lifecycle.FileMetadata
        { ref = fileRef
        , filename = "confirmed-file.txt"
        , contentType = "text/plain"
        , sizeBytes = 2048
        , blobKey = BlobKey "blob_confirmed"
        , uploadedAt = 1700000000
        }
    , ownerHash = OwnerHash "owner_confirmed"
    , confirmedByRequestId = "request_abc"
    }


-- | Create a FileUploaded event for testing
mkFileUploadedEvent :: FileRef -> Int64 -> Int64 -> FileUploadEvent
mkFileUploadedEvent (FileRef refText) uploadedAtTime expiresAtTime =
  FileUploaded FileUploadedData
    { fileRef = FileRef refText
    , ownerHash = OwnerHash [fmt|owner-#{refText}|]
    , filename = [fmt|file-#{refText}.txt|]
    , contentType = "text/plain"
    , sizeBytes = 100
    , blobKey = BlobKey [fmt|blob-#{refText}|]
    , expiresAt = expiresAtTime
    , uploadedAt = uploadedAtTime
    }
