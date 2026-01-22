module Service.FileUpload.LifecycleSpec where

import Core
import Service.FileUpload.Core (
  BlobKey (..),
  FileDeletionReason (..),
  FileRef (..),
  FileUploadEvent (..),
  OwnerHash (..),
 )
import Service.FileUpload.Lifecycle (
  ConfirmedFile (..),
  FileMetadata (..),
  FileUploadState (..),
  PendingFile (..),
  canConfirm,
  canDelete,
  getBlobKey,
  getOwnerHash,
  isExpired,
  update,
 )
import Test


spec :: Spec Unit
spec = do
  describe "Service.FileUpload.Lifecycle" do
    -- ==========================================================================
    -- State Transitions
    -- ==========================================================================
    describe "State Transitions" do
      describe "FileUploaded event" do
        it "creates Pending state from initial state" \_ -> do
          let event = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let state = update event Initial
          case state of
            Pending pending -> do
              pending.metadata.ref |> shouldBe (FileRef "ref-1")
              pending.expiresAt |> shouldBe 1700003600
            _ -> fail "Expected Pending state"

        it "sets all metadata fields correctly" \_ -> do
          let event = FileUploaded
                { fileRef = FileRef "ref-1"
                , ownerHash = OwnerHash "owner-abc"
                , filename = "test.pdf"
                , contentType = "application/pdf"
                , sizeBytes = 1024
                , blobKey = BlobKey "blob-xyz"
                , expiresAt = 1700003600
                , uploadedAt = 1700000000
                }
          let state = update event Initial
          case state of
            Pending pending -> do
              pending.ownerHash |> shouldBe (OwnerHash "owner-abc")
              pending.metadata.filename |> shouldBe "test.pdf"
              pending.metadata.contentType |> shouldBe "application/pdf"
              pending.metadata.sizeBytes |> shouldBe 1024
            _ -> fail "Expected Pending state"

      describe "FileConfirmed event" do
        it "transitions Pending to Confirmed" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let confirmEvent = FileConfirmed
                { fileRef = FileRef "ref-1"
                , confirmedByRequestId = "request-123"
                , confirmedAt = 1700000100
                }
          let state = update confirmEvent pendingState
          case state of
            Confirmed confirmed -> do
              confirmed.confirmedByRequestId |> shouldBe "request-123"
            _ -> fail "Expected Confirmed state"

        it "preserves metadata from Pending state" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let confirmEvent = FileConfirmed
                { fileRef = FileRef "ref-1"
                , confirmedByRequestId = "request-123"
                , confirmedAt = 1700000100
                }
          let state = update confirmEvent pendingState
          case state of
            Confirmed confirmed -> do
              confirmed.metadata.ref |> shouldBe (FileRef "ref-1")
            _ -> fail "Expected Confirmed state"

        it "is idempotent - confirming already Confirmed is no-op" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let confirmEvent1 = FileConfirmed
                { fileRef = FileRef "ref-1"
                , confirmedByRequestId = "request-123"
                , confirmedAt = 1700000100
                }
          let confirmedState = update confirmEvent1 pendingState
          let confirmEvent2 = FileConfirmed
                { fileRef = FileRef "ref-1"
                , confirmedByRequestId = "request-456"
                , confirmedAt = 1700000200
                }
          let state = update confirmEvent2 confirmedState
          case state of
            Confirmed confirmed -> do
              -- Should keep original confirmation
              confirmed.confirmedByRequestId |> shouldBe "request-123"
            _ -> fail "Expected Confirmed state"

        it "does not transition from Initial state" \_ -> do
          let confirmEvent = FileConfirmed
                { fileRef = FileRef "ref-1"
                , confirmedByRequestId = "request-123"
                , confirmedAt = 1700000100
                }
          let state = update confirmEvent Initial
          state |> shouldBe Initial

        it "does not transition from Deleted state" \_ -> do
          let state = update (mkConfirmEvent "ref-1") Deleted
          state |> shouldBe Deleted

      describe "FileDeleted event" do
        it "transitions Pending to Deleted" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let deleteEvent = FileDeleted
                { fileRef = FileRef "ref-1"
                , reason = Orphaned
                , deletedAt = 1700003700
                }
          let state = update deleteEvent pendingState
          state |> shouldBe Deleted

        it "transitions Confirmed to Deleted" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let confirmEvent = mkConfirmEvent "ref-1"
          let confirmedState = update confirmEvent pendingState
          let deleteEvent = FileDeleted
                { fileRef = FileRef "ref-1"
                , reason = UserRequested
                , deletedAt = 1700004000
                }
          let state = update deleteEvent confirmedState
          state |> shouldBe Deleted

        it "is idempotent - deleting Deleted is no-op" \_ -> do
          let deleteEvent = FileDeleted
                { fileRef = FileRef "ref-1"
                , reason = AdminPurge
                , deletedAt = 1700005000
                }
          let state = update deleteEvent Deleted
          state |> shouldBe Deleted

    -- ==========================================================================
    -- Validation Helpers
    -- ==========================================================================
    describe "Validation Helpers" do
      describe "canConfirm" do
        it "returns True for Pending state before expiry" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let currentTime = 1700000100  -- Before expiry
          canConfirm pendingState currentTime |> shouldBe True

        it "returns False for Pending state after expiry" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let currentTime = 1700004000  -- After expiry
          canConfirm pendingState currentTime |> shouldBe False

        it "returns False for Initial state" \_ -> do
          canConfirm Initial 1700000000 |> shouldBe False

        it "returns False for Confirmed state" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let confirmedState = update (mkConfirmEvent "ref-1") pendingState
          canConfirm confirmedState 1700000100 |> shouldBe False

        it "returns False for Deleted state" \_ -> do
          canConfirm Deleted 1700000000 |> shouldBe False

      describe "isExpired" do
        it "returns True for Pending state after expiresAt" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let currentTime = 1700003601  -- Just after expiry
          isExpired pendingState currentTime |> shouldBe True

        it "returns False for Pending state before expiresAt" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let currentTime = 1700003599  -- Just before expiry
          isExpired pendingState currentTime |> shouldBe False

        it "returns True at exactly expiresAt" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let currentTime = 1700003600  -- Exactly at expiry
          isExpired pendingState currentTime |> shouldBe True

        it "returns False for non-Pending states" \_ -> do
          isExpired Initial 1700000000 |> shouldBe False
          isExpired Deleted 1700000000 |> shouldBe False

      describe "canDelete" do
        it "returns True for Pending state" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          canDelete pendingState |> shouldBe True

        it "returns True for Confirmed state" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let confirmedState = update (mkConfirmEvent "ref-1") pendingState
          canDelete confirmedState |> shouldBe True

        it "returns False for Deleted state" \_ -> do
          canDelete Deleted |> shouldBe False

        it "returns False for Initial state" \_ -> do
          canDelete Initial |> shouldBe False

      describe "getOwnerHash" do
        it "returns ownerHash for Pending state" \_ -> do
          let uploadEvent = FileUploaded
                { fileRef = FileRef "ref-1"
                , ownerHash = OwnerHash "owner-xyz"
                , filename = "test.txt"
                , contentType = "text/plain"
                , sizeBytes = 100
                , blobKey = BlobKey "blob-1"
                , expiresAt = 1700003600
                , uploadedAt = 1700000000
                }
          let pendingState = update uploadEvent Initial
          getOwnerHash pendingState |> shouldBe (Just (OwnerHash "owner-xyz"))

        it "returns ownerHash for Confirmed state" \_ -> do
          let uploadEvent = FileUploaded
                { fileRef = FileRef "ref-1"
                , ownerHash = OwnerHash "owner-xyz"
                , filename = "test.txt"
                , contentType = "text/plain"
                , sizeBytes = 100
                , blobKey = BlobKey "blob-1"
                , expiresAt = 1700003600
                , uploadedAt = 1700000000
                }
          let pendingState = update uploadEvent Initial
          let confirmedState = update (mkConfirmEvent "ref-1") pendingState
          getOwnerHash confirmedState |> shouldBe (Just (OwnerHash "owner-xyz"))

        it "returns Nothing for Initial and Deleted states" \_ -> do
          getOwnerHash Initial |> shouldBe Nothing
          getOwnerHash Deleted |> shouldBe Nothing

      describe "getBlobKey" do
        it "returns blobKey for Pending state" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          getBlobKey pendingState |> shouldBe (Just (BlobKey "blob-ref-1"))

        it "returns blobKey for Confirmed state" \_ -> do
          let uploadEvent = mkFileUploadedEvent "ref-1" 1700000000 1700003600
          let pendingState = update uploadEvent Initial
          let confirmedState = update (mkConfirmEvent "ref-1") pendingState
          getBlobKey confirmedState |> shouldBe (Just (BlobKey "blob-ref-1"))

        it "returns Nothing for Initial and Deleted states" \_ -> do
          getBlobKey Initial |> shouldBe Nothing
          getBlobKey Deleted |> shouldBe Nothing


-- ==========================================================================
-- Test Helpers
-- ==========================================================================

mkFileUploadedEvent :: Text -> Int64 -> Int64 -> FileUploadEvent
mkFileUploadedEvent refId uploadedAt expiresAt =
  FileUploaded
    { fileRef = FileRef refId
    , ownerHash = OwnerHash [fmt|owner-#{refId}|]
    , filename = [fmt|file-#{refId}.txt|]
    , contentType = "text/plain"
    , sizeBytes = 100
    , blobKey = BlobKey [fmt|blob-#{refId}|]
    , expiresAt = expiresAt
    , uploadedAt = uploadedAt
    }

mkConfirmEvent :: Text -> FileUploadEvent
mkConfirmEvent refId =
  FileConfirmed
    { fileRef = FileRef refId
    , confirmedByRequestId = "request-default"
    , confirmedAt = 1700000100
    }


-- Types and functions imported from Service.FileUpload.Core and Service.FileUpload.Lifecycle
