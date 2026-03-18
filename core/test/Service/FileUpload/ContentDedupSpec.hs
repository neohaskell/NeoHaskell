module Service.FileUpload.ContentDedupSpec where

import Bytes qualified
import Core
import Data.Char (isDigit)
import Directory qualified
import Log qualified
import "nhcore" Path qualified
import Service.FileUpload.BlobStore (BlobStore)
import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..), createBlobStore)
import Service.FileUpload.Core (
  BlobKey (..),
  ContentHash (..),
  FileConfirmedData (..),
  FileDeletedData (..),
  FileDeletionReason (..),
  FileRef (..),
  FileUploadEvent (..),
  InternalFileUploadConfig (..),
  OwnerHash (..),
 )
import Service.FileUpload.Lifecycle (FileUploadState (..))
import Service.FileUpload.Lifecycle qualified as Lifecycle
import Service.FileUpload.Routes (UploadResponse (..))
import Service.FileUpload.Web (
  FileStateStore (..),
  computeContentHash,
  handleUploadImpl,
  inMemoryFileStateStore,
  newInMemoryFileStateStore,
 )
import Task qualified
import Test
import Text qualified
import Uuid qualified


spec :: Spec Unit
spec = do
  describe "Service.FileUpload.ContentDedup" do
    -- ==========================================================================
    -- computeContentHash — Determinism
    -- ==========================================================================
    describe "computeContentHash" do
      describe "Determinism" do
        it "same input produces same hash — deterministic across two calls" \_ -> do
          let content = "hello world" |> Text.toBytes
          let hash1 = computeContentHash content
          let hash2 = computeContentHash content
          hash1 |> shouldBe hash2

        it "same binary content in different calls produces identical ContentHash" \_ -> do
          let content = Bytes.replicate 100 0x42
          let hash1 = computeContentHash content
          let hash2 = computeContentHash content
          hash1 |> shouldBe hash2

      describe "Different Inputs Produce Different Hashes" do
        it "different text content produces different hashes" \_ -> do
          let hashA = computeContentHash ("hello" |> Text.toBytes)
          let hashB = computeContentHash ("world" |> Text.toBytes)
          hashA |> shouldNotBe hashB

        it "content differing by one byte produces different hashes" \_ -> do
          let hashA = computeContentHash (Bytes.pack [0x00, 0x01, 0x02])
          let hashB = computeContentHash (Bytes.pack [0x00, 0x01, 0x03])
          hashA |> shouldNotBe hashB

        it "empty vs non-empty content produces different hashes" \_ -> do
          let hashA = computeContentHash ("" |> Text.toBytes)
          let hashB = computeContentHash ("a" |> Text.toBytes)
          hashA |> shouldNotBe hashB

      describe "Edge Cases" do
        it "empty content produces a valid 64-char hex hash" \_ -> do
          let (ContentHash hashText) = computeContentHash ("" |> Text.toBytes)
          Text.length hashText |> shouldBe 64

        it "empty content produces the known SHA-256 of empty string" \_ -> do
          let result = computeContentHash ("" |> Text.toBytes)
          result |> shouldBe (ContentHash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

        it "binary content with null bytes produces valid 64-char hash" \_ -> do
          let (ContentHash hashText) = computeContentHash (Bytes.pack [0x00, 0x00, 0x00])
          Text.length hashText |> shouldBe 64

        it "large content (10KB) produces valid 64-char hash" \_ -> do
          let (ContentHash hashText) = computeContentHash (Bytes.replicate 10240 0xFF)
          Text.length hashText |> shouldBe 64

        it "hash output is lowercase hex only — no uppercase letters" \_ -> do
          let (ContentHash hashText) = computeContentHash ("test content" |> Text.toBytes)
          hashText |> shouldSatisfy (\t -> Text.all (\c -> isDigit c || (c >= 'a' && c <= 'f')) t)

        it "known SHA-256 test vector: 'hello' produces expected hash" \_ -> do
          let result = computeContentHash ("hello" |> Text.toBytes)
          result |> shouldBe (ContentHash "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")

    -- ==========================================================================
    -- findByContentHash — In-Memory FileStateStore
    -- ==========================================================================
    describe "findByContentHash (in-memory)" do
      describe "Happy Path" do
        it "returns matching FileRef for Pending file with same owner+hash" \_ -> do
          stateMap <- newInMemoryFileStateStore
          let store = inMemoryFileStateStore stateMap
          let fileRef = FileRef "file-pending-1"
          let pendingState = mkPendingStateWith fileRef (OwnerHash "owner1") (ContentHash "hash1")
          store.setState fileRef pendingState
          result <- store.findByContentHash (OwnerHash "owner1") (ContentHash "hash1")
          result |> shouldBe (Just fileRef)

        it "returns matching FileRef for Confirmed file with same owner+hash" \_ -> do
          stateMap <- newInMemoryFileStateStore
          let store = inMemoryFileStateStore stateMap
          let fileRef = FileRef "file-confirmed-1"
          let confirmedState = mkConfirmedStateWith fileRef (OwnerHash "owner1") (ContentHash "hash1")
          store.setState fileRef confirmedState
          result <- store.findByContentHash (OwnerHash "owner1") (ContentHash "hash1")
          result |> shouldBe (Just fileRef)

      describe "No Match Cases" do
        it "returns Nothing when no files exist in store" \_ -> do
          stateMap <- newInMemoryFileStateStore
          let store = inMemoryFileStateStore stateMap
          result <- store.findByContentHash (OwnerHash "owner1") (ContentHash "hash1")
          result |> shouldBe Nothing

        it "returns Nothing when owner matches but hash does not" \_ -> do
          stateMap <- newInMemoryFileStateStore
          let store = inMemoryFileStateStore stateMap
          let fileRef = FileRef "file-hash-mismatch"
          let pendingState = mkPendingStateWith fileRef (OwnerHash "owner1") (ContentHash "hashA")
          store.setState fileRef pendingState
          result <- store.findByContentHash (OwnerHash "owner1") (ContentHash "hashB")
          result |> shouldBe Nothing

        it "returns Nothing when hash matches but owner does not — cross-owner isolation" \_ -> do
          stateMap <- newInMemoryFileStateStore
          let store = inMemoryFileStateStore stateMap
          let fileRef = FileRef "file-owner-mismatch"
          let pendingState = mkPendingStateWith fileRef (OwnerHash "owner1") (ContentHash "hash1")
          store.setState fileRef pendingState
          result <- store.findByContentHash (OwnerHash "owner2") (ContentHash "hash1")
          result |> shouldBe Nothing

        it "returns Nothing when matching file is in Deleted state" \_ -> do
          stateMap <- newInMemoryFileStateStore
          let store = inMemoryFileStateStore stateMap
          let fileRef = FileRef "file-deleted-1"
          let pendingState = mkPendingStateWith fileRef (OwnerHash "owner1") (ContentHash "hash1")
          store.setState fileRef pendingState
          store.setState fileRef Deleted
          result <- store.findByContentHash (OwnerHash "owner1") (ContentHash "hash1")
          result |> shouldBe Nothing

        it "returns Nothing when matching file is in Initial state" \_ -> do
          stateMap <- newInMemoryFileStateStore
          let store = inMemoryFileStateStore stateMap
          let fileRef = FileRef "file-initial-1"
          store.setState fileRef Initial
          result <- store.findByContentHash (OwnerHash "owner1") (ContentHash "hash1")
          result |> shouldBe Nothing

      describe "Multiple Files" do
        it "returns correct FileRef when multiple files exist with different hashes" \_ -> do
          stateMap <- newInMemoryFileStateStore
          let store = inMemoryFileStateStore stateMap
          let file1Ref = FileRef "file-multi-1"
          let file2Ref = FileRef "file-multi-2"
          let ownerHash = OwnerHash "owner1"
          store.setState file1Ref (mkPendingStateWith file1Ref ownerHash (ContentHash "hash1"))
          store.setState file2Ref (mkPendingStateWith file2Ref ownerHash (ContentHash "hash2"))
          result <- store.findByContentHash ownerHash (ContentHash "hash2")
          result |> shouldBe (Just file2Ref)

        it "different owners with same content hash are isolated — returns correct owner's FileRef" \_ -> do
          stateMap <- newInMemoryFileStateStore
          let store = inMemoryFileStateStore stateMap
          let file1Ref = FileRef "file-iso-1"
          let file2Ref = FileRef "file-iso-2"
          store.setState file1Ref (mkPendingStateWith file1Ref (OwnerHash "owner1") (ContentHash "hashX"))
          store.setState file2Ref (mkPendingStateWith file2Ref (OwnerHash "owner2") (ContentHash "hashX"))
          result <- store.findByContentHash (OwnerHash "owner1") (ContentHash "hashX")
          result |> shouldBe (Just file1Ref)

    -- ==========================================================================
    -- handleUploadImpl — Dedup Integration Tests
    -- ==========================================================================
    describe "handleUploadImpl" do
      describe "First Upload (No Dedup, Normal Flow)" do
        it "first upload of content returns Ok with a non-empty FileRef" \_ -> do
          withTestDedupEnv \env -> do
            let content = "hello dedup world" |> Text.toBytes
            result <- env.stateStore.findByContentHash (OwnerHash "owner1") (computeContentHash content)
            result |> shouldBe Nothing
            -- Actually do the upload
            uploadResult <- uploadFile env "owner1" "file.txt" "text/plain" content
              |> Task.asResult
            case uploadResult of
              Ok response -> do
                let (FileRef refText) = response.fileRef
                (Text.length refText > 0) |> shouldBe True
              Err err -> fail [fmt|Expected success, got: #{err}|]

        it "first upload creates Pending state with correct contentHash" \_ -> do
          withTestDedupEnv \env -> do
            let content = "first upload content" |> Text.toBytes
            response <- uploadFile env "owner1" "file.txt" "text/plain" content
            maybeState <- env.stateStore.getState response.fileRef
            case maybeState of
              Just (Pending pending) ->
                pending.metadata.contentHash |> shouldBe (computeContentHash content)
              _ -> fail "Expected Pending state after first upload"

      describe "Duplicate Detection (Dedup Kicks In)" do
        it "second upload of same content by same owner returns existing FileRef" \_ -> do
          withTestDedupEnv \env -> do
            let content = "duplicate content" |> Text.toBytes
            response1 <- uploadFile env "owner1" "invoice.pdf" "text/plain" content
            response2 <- uploadFile env "owner1" "invoice.pdf" "text/plain" content
            response2.fileRef |> shouldBe response1.fileRef

        it "duplicate upload returns same blobKey as original" \_ -> do
          withTestDedupEnv \env -> do
            let content = "duplicate blobkey content" |> Text.toBytes
            response1 <- uploadFile env "owner1" "file.txt" "text/plain" content
            response2 <- uploadFile env "owner1" "file.txt" "text/plain" content
            response2.blobKey |> shouldBe response1.blobKey

        it "duplicate upload returns original metadata (filename from first upload)" \_ -> do
          withTestDedupEnv \env -> do
            let content = "duplicate filename content" |> Text.toBytes
            _ <- uploadFile env "owner1" "invoice.pdf" "text/plain" content
            response2 <- uploadFile env "owner1" "copy.pdf" "text/plain" content
            response2.filename |> shouldBe "invoice.pdf"

        it "duplicate upload returns matching sizeBytes from original" \_ -> do
          withTestDedupEnv \env -> do
            let content = "duplicate sizeBytes content" |> Text.toBytes
            response1 <- uploadFile env "owner1" "file.txt" "text/plain" content
            response2 <- uploadFile env "owner1" "file.txt" "text/plain" content
            response2.sizeBytes |> shouldBe response1.sizeBytes

      describe "Dedup with Confirmed State" do
        it "duplicate detected against Confirmed file returns existing FileRef" \_ -> do
          withTestDedupEnv \env -> do
            let content = "confirmed dedup content" |> Text.toBytes
            response1 <- uploadFile env "owner1" "file.txt" "text/plain" content
            -- Confirm the file
            let confirmEvent = FileConfirmed FileConfirmedData
                  { fileRef = response1.fileRef
                  , confirmedByRequestId = "req-1"
                  , confirmedAt = 1700000100
                  }
            env.stateStore.updateState response1.fileRef confirmEvent
            -- Upload same content again
            response2 <- uploadFile env "owner1" "file.txt" "text/plain" content
            response2.fileRef |> shouldBe response1.fileRef

      describe "Deleted File Does NOT Dedup (Deletion Boundary)" do
        it "upload after deletion creates new FileRef — deletion boundary respected" \_ -> do
          withTestDedupEnv \env -> do
            let content = "deletion boundary content" |> Text.toBytes
            response1 <- uploadFile env "owner1" "file.txt" "text/plain" content
            -- Delete the file
            let deleteEvent = FileDeleted FileDeletedData
                  { fileRef = response1.fileRef
                  , reason = UserRequested
                  , deletedAt = 1700003700
                  }
            env.stateStore.updateState response1.fileRef deleteEvent
            -- Upload same content again
            response2 <- uploadFile env "owner1" "file.txt" "text/plain" content
            response2.fileRef |> shouldNotBe response1.fileRef

        it "upload after deletion creates new Pending state for the new FileRef" \_ -> do
          withTestDedupEnv \env -> do
            let content = "deletion boundary state content" |> Text.toBytes
            response1 <- uploadFile env "owner1" "file.txt" "text/plain" content
            let deleteEvent = FileDeleted FileDeletedData
                  { fileRef = response1.fileRef
                  , reason = UserRequested
                  , deletedAt = 1700003700
                  }
            env.stateStore.updateState response1.fileRef deleteEvent
            response2 <- uploadFile env "owner1" "file.txt" "text/plain" content
            maybeState <- env.stateStore.getState response2.fileRef
            case maybeState of
              Just (Pending _) -> pass
              _ -> fail "Expected new Pending state after re-upload post-deletion"

      describe "Cross-Owner Isolation (No Dedup Across Owners)" do
        it "same content uploaded by different owner creates new FileRef" \_ -> do
          withTestDedupEnv \env -> do
            let content = "cross-owner content" |> Text.toBytes
            response1 <- uploadFile env "owner1" "file.txt" "text/plain" content
            response2 <- uploadFile env "owner2" "file.txt" "text/plain" content
            response2.fileRef |> shouldNotBe response1.fileRef

        it "cross-owner uploads create separate Pending states" \_ -> do
          withTestDedupEnv \env -> do
            let content = "cross-owner state content" |> Text.toBytes
            response1 <- uploadFile env "owner1" "file.txt" "text/plain" content
            response2 <- uploadFile env "owner2" "file.txt" "text/plain" content
            maybeState1 <- env.stateStore.getState response1.fileRef
            maybeState2 <- env.stateStore.getState response2.fileRef
            case (maybeState1, maybeState2) of
              (Just (Pending _), Just (Pending _)) -> pass
              _ -> fail "Expected both to be Pending states"

      describe "Edge Cases" do
        it "empty file content deduplicates correctly" \_ -> do
          withTestDedupEnv \env -> do
            let content = "" |> Text.toBytes
            response1 <- uploadFile env "owner1" "empty.txt" "text/plain" content
            response2 <- uploadFile env "owner1" "empty.txt" "text/plain" content
            response2.fileRef |> shouldBe response1.fileRef

        it "different content by same owner creates separate FileRefs" \_ -> do
          withTestDedupEnv \env -> do
            let contentA = "content A" |> Text.toBytes
            let contentB = "content B" |> Text.toBytes
            response1 <- uploadFile env "owner1" "a.txt" "text/plain" contentA
            response2 <- uploadFile env "owner1" "b.txt" "text/plain" contentB
            response2.fileRef |> shouldNotBe response1.fileRef

        it "dedup response has valid non-empty metadata fields" \_ -> do
          withTestDedupEnv \env -> do
            let content = "metadata check content" |> Text.toBytes
            _ <- uploadFile env "owner1" "meta.txt" "text/plain" content
            response2 <- uploadFile env "owner1" "meta.txt" "text/plain" content
            (Text.length response2.filename > 0) |> shouldBe True
            (Text.length response2.contentType > 0) |> shouldBe True
            (response2.sizeBytes > 0) |> shouldBe True


-- ==========================================================================
-- Test Infrastructure
-- ==========================================================================

data TestDedupEnv = TestDedupEnv
  { blobStore :: BlobStore
  , stateStore :: FileStateStore
  , config :: InternalFileUploadConfig
  , tempDir :: Path
  }


withTestDedupEnv :: (TestDedupEnv -> Task Text a) -> Task Text a
withTestDedupEnv action = do
  -- Create unique temp directory
  uuid <- Uuid.generate
  let uniqueId = Uuid.toText uuid
  let tempDirText = [fmt|/tmp/neohaskell-dedup-test-#{uniqueId}|]

  tempDir <- case Path.fromText tempDirText of
    Just p -> Task.yield p
    Nothing -> Task.throw "Failed to create temp path"

  -- Create directory
  _ <- Directory.createIfMissing tempDir
    |> Task.mapError (\e -> [fmt|Failed to create temp directory: #{show e}|])

  -- Create blob store
  let blobStoreConfig = LocalBlobStoreConfig { rootDir = tempDir }
  blobStore <- createBlobStore blobStoreConfig

  -- Create in-memory state store
  stateMap <- newInMemoryFileStateStore
  let stateStore = inMemoryFileStateStore stateMap

  -- Create config with permissive settings
  let config = InternalFileUploadConfig
        { maxFileSizeBytes = 10485760  -- 10MB
        , allowedContentTypes = Nothing  -- all types allowed
        , pendingTtlSeconds = 3600  -- 1 hour
        , cleanupIntervalSeconds = 900
        , storeOriginalFilename = True
        }

  let env = TestDedupEnv
        { blobStore = blobStore
        , stateStore = stateStore
        , config = config
        , tempDir = tempDir
        }

  -- Run action
  result <- action env |> Task.asResult

  -- Cleanup (best effort, log failures for CI debugging)
  cleanupResult <- Directory.removeRecursive tempDir
    |> Task.mapError (\_ -> [fmt|Failed to remove temp directory: #{tempDirText}|] :: Text)
    |> Task.asResult
  case cleanupResult of
    Ok _ -> pass
    Err cleanupErr ->
      Log.warn [fmt|Test cleanup failed: #{cleanupErr}|]
        |> Task.ignoreError

  -- Return result
  case result of
    Ok a -> Task.yield a
    Err e -> Task.throw e


-- | Helper to upload a file using handleUploadImpl
uploadFile ::
  TestDedupEnv ->
  Text ->  -- ownerHash
  Text ->  -- filename
  Text ->  -- contentType
  Bytes ->  -- content
  Task Text UploadResponse
uploadFile env ownerHash filename contentType content =
  handleUploadImpl env.config env.blobStore env.stateStore ownerHash filename contentType content


-- | Create a Pending state with specific owner and content hash
mkPendingStateWith :: FileRef -> OwnerHash -> ContentHash -> FileUploadState
mkPendingStateWith fileRef ownerHash contentHash =
  Pending Lifecycle.PendingFile
    { metadata = Lifecycle.FileMetadata
        { ref = fileRef
        , filename = "test-file.txt"
        , contentType = "text/plain"
        , sizeBytes = 1024
        , blobKey = BlobKey "blob_test"
        , uploadedAt = 1700000000
        , contentHash = contentHash
        }
    , ownerHash = ownerHash
    , expiresAt = 1700003600
    }


-- | Create a Confirmed state with specific owner and content hash
mkConfirmedStateWith :: FileRef -> OwnerHash -> ContentHash -> FileUploadState
mkConfirmedStateWith fileRef ownerHash contentHash =
  Confirmed Lifecycle.ConfirmedFile
    { metadata = Lifecycle.FileMetadata
        { ref = fileRef
        , filename = "confirmed-file.txt"
        , contentType = "text/plain"
        , sizeBytes = 2048
        , blobKey = BlobKey "blob_confirmed"
        , uploadedAt = 1700000000
        , contentHash = contentHash
        }
    , ownerHash = ownerHash
    , confirmedByRequestId = "request_abc"
    }
