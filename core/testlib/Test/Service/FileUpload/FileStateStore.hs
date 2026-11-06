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
  ContentHash (..),
  FileCreation (..),
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
    -- findByContentHash
    -- ==========================================================================
    describe "findByContentHash" do
      it "returns Nothing for non-existent owner+hash combination" \_ -> do
        store <- newStore
        result <- store.findByContentHash (OwnerHash "owner1") (ContentHash "hash1")
        result |> shouldBe Nothing

      it "returns FileRef for Pending file matching owner+hash" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        let (FileRef refText) = fileRef
        let event = mkFileUploadedEvent fileRef 1700000000 1700003600
        store.updateState fileRef event
        result <- store.findByContentHash (OwnerHash [fmt|owner-#{refText}|]) (ContentHash "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789")
        result |> shouldBe (Just fileRef)

      it "returns FileRef for Confirmed file matching owner+hash" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        let (FileRef refText) = fileRef
        let uploadEvent = mkFileUploadedEvent fileRef 1700000000 1700003600
        store.updateState fileRef uploadEvent
        let confirmEvent = FileConfirmed FileConfirmedData
              { fileRef = fileRef
              , confirmedByRequestId = "request-123"
              , confirmedAt = 1700000100
              }
        store.updateState fileRef confirmEvent
        result <- store.findByContentHash (OwnerHash [fmt|owner-#{refText}|]) (ContentHash "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789")
        result |> shouldBe (Just fileRef)

      it "returns Nothing for Deleted file matching owner+hash" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        let (FileRef refText) = fileRef
        let uploadEvent = mkFileUploadedEvent fileRef 1700000000 1700003600
        store.updateState fileRef uploadEvent
        let deleteEvent = FileDeleted FileDeletedData
              { fileRef = fileRef
              , reason = UserRequested
              , deletedAt = 1700003700
              }
        store.updateState fileRef deleteEvent
        result <- store.findByContentHash (OwnerHash [fmt|owner-#{refText}|]) (ContentHash "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789")
        result |> shouldBe Nothing

      it "returns Nothing when owner matches but hash does not" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        let (FileRef refText) = fileRef
        let event = mkFileUploadedEvent fileRef 1700000000 1700003600
        store.updateState fileRef event
        result <- store.findByContentHash (OwnerHash [fmt|owner-#{refText}|]) (ContentHash "completely-different-hash")
        result |> shouldBe Nothing

      it "returns Nothing when hash matches but owner does not" \_ -> do
        store <- newStore
        fileRef <- generateFileRef
        let event = mkFileUploadedEvent fileRef 1700000000 1700003600
        store.updateState fileRef event
        result <- store.findByContentHash (OwnerHash "other-owner") (ContentHash "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789")
        result |> shouldBe Nothing


    describe "retained creation" do
      it "reserves one canonical file identity per owner and creation key" \_ -> do
        store <- newStore
        let first = mkFileCreation "owner-a" "job-1" "first"
        let competing = mkFileCreation "owner-a" "job-1" "second"
        firstResult <- store.reserveCreation first
        secondResult <- store.reserveCreation competing
        firstResult |> shouldBe first
        secondResult |> shouldBe first
        state <- store.getState first.fileRef
        state |> shouldBe Nothing

      it "scopes creation keys to their owner" \_ -> do
        store <- newStore
        let first = mkFileCreation "owner-a" "job-1" "first"
        let otherOwner = mkFileCreation "owner-b" "job-1" "second"
        firstResult <- store.reserveCreation first
        otherResult <- store.reserveCreation otherOwner
        firstResult.fileRef |> shouldBe first.fileRef
        otherResult.fileRef |> shouldBe otherOwner.fileRef
        (otherResult.fileRef != firstResult.fileRef) |> shouldBe True

      it "publishes retained metadata and keeps the deletion tombstone" \_ -> do
        store <- newStore
        let creation = mkFileCreation "owner-a" "job-1" "retained"
        _ <- store.reserveCreation creation
        store.publishRetained creation
        state <- store.getState creation.fileRef
        case state of
          Just (Retained retained) -> do
            retained.ownerHash |> shouldBe creation.ownerHash
            retained.metadata.ref |> shouldBe creation.fileRef
            retained.metadata.blobKey |> shouldBe creation.blobKey
            retained.metadata.filename |> shouldBe creation.filename
            retained.metadata.contentType |> shouldBe creation.contentType
            retained.metadata.sizeBytes |> shouldBe creation.sizeBytes
            retained.metadata.contentHash |> shouldBe creation.contentHash
          _ -> fail "Expected retained file state"
        store.updateState creation.fileRef (mkDeleteEvent creation.fileRef)
        repeatedReservation <- store.reserveCreation (mkFileCreation "owner-a" "job-1" "replacement")
        repeatedReservation |> shouldBe creation
        publishResult <- store.publishRetained creation |> Task.asResult
        case publishResult of
          Err _ -> pass
          Ok _ -> fail "Expected the deleted state to reject publication"
        stateAfterRetry <- store.getState creation.fileRef
        stateAfterRetry |> shouldBe (Just Deleted)

-- ==========================================================================
-- Test Helpers
-- ==========================================================================

-- | Generate a unique FileRef for testing
generateFileRef :: Task Text FileRef
generateFileRef = do
  uuid <- Uuid.generate
  Task.yield (FileRef [fmt|file_#{Uuid.toText uuid}|])


-- | Create a Pending state for testing.
-- OwnerHash is derived from fileRef to ensure unique (owner_hash, content_hash)
-- pairs per file, avoiding Postgres partial unique index violations.
mkPendingState :: FileRef -> FileUploadState
mkPendingState fileRef = do
  let (FileRef refText) = fileRef
  Pending Lifecycle.PendingFile
    { metadata = Lifecycle.FileMetadata
        { ref = fileRef
        , filename = "test-file.txt"
        , contentType = "text/plain"
        , sizeBytes = 1024
        , blobKey = BlobKey "blob_test"
        , uploadedAt = 1700000000
        , contentHash = ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
        }
    , ownerHash = OwnerHash [fmt|owner-#{refText}|]
    , expiresAt = 1700003600
    }


-- | Create a Confirmed state for testing.
-- OwnerHash is derived from fileRef to ensure unique (owner_hash, content_hash)
-- pairs per file, avoiding Postgres partial unique index violations.
mkConfirmedState :: FileRef -> FileUploadState
mkConfirmedState fileRef = do
  let (FileRef refText) = fileRef
  Confirmed Lifecycle.ConfirmedFile
    { metadata = Lifecycle.FileMetadata
        { ref = fileRef
        , filename = "confirmed-file.txt"
        , contentType = "text/plain"
        , sizeBytes = 2048
        , blobKey = BlobKey "blob_confirmed"
        , uploadedAt = 1700000000
        , contentHash = ContentHash "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"
        }
    , ownerHash = OwnerHash [fmt|owner-confirmed-#{refText}|]
    , confirmedByRequestId = "request_abc"
    }


-- | Create a FileUploaded event for testing
mkFileUploadedEvent :: FileRef -> Int64 -> Int64 -> FileUploadEvent
mkFileUploadedEvent (FileRef refText) uploadedAtTime expiresAtTime =
  FileUploaded FileUploadedData
    { fileRef = FileRef refText
    , ownerHash = OwnerHash [fmt|owner-#{refText}|]
    , contentHash = ContentHash "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789"
    , filename = [fmt|file-#{refText}.txt|]
    , contentType = "text/plain"
    , sizeBytes = 100
    , blobKey = BlobKey [fmt|blob-#{refText}|]
    , expiresAt = expiresAtTime
    , uploadedAt = uploadedAtTime
    }


mkFileCreation :: Text -> Text -> Text -> FileCreation
mkFileCreation ownerText key suffix = FileCreation
  { ownerHash = OwnerHash ownerText
  , creationKey = key
  , fileRef = FileRef [fmt|file_#{suffix}|]
  , blobKey = BlobKey [fmt|blob_#{suffix}|]
  , filename = "document.pdf"
  , contentType = "application/pdf"
  , sizeBytes = 4
  , contentHash = ContentHash "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789"
  , createdAt = 1700000000
  }


mkDeleteEvent :: FileRef -> FileUploadEvent
mkDeleteEvent fileRef = FileDeleted FileDeletedData
  { fileRef = fileRef
  , reason = UserRequested
  , deletedAt = 1700000100
  }

