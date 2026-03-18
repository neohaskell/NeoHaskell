module Service.FileUpload.CoreSpec where

import Core
import Json qualified
import Result qualified
import Service.FileUpload.Core (
  BlobKey (..),
  ContentHash (..),
  FileConfirmedData (..),
  FileDeletedData (..),
  FileDeletionReason (..),
  FileMetadata (..),
  FileRef (..),
  FileStateStoreBackend (..),
  FileUploadConfig (..),
  FileUploadEvent (..),
  FileUploadedData (..),
  InternalFileUploadConfig (..),
  OwnerHash (..),
  ResolvedFile (..),
 )
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Service.FileUpload.Core" do
    -- ==========================================================================
    -- FileRef Type
    -- ==========================================================================
    describe "FileRef" do
      it "Show instance does not reveal the actual reference" \_ -> do
        -- FileRef contains opaque identifiers that shouldn't leak to logs
        let fileRef = FileRef "secret-file-id-12345"
        let shown = toText fileRef
        shown |> shouldSatisfy (\t -> not (Text.contains "secret-file-id" t))
        shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

      it "can be created from Text" \_ -> do
        let fileRef = FileRef "test-ref"
        -- Should compile and exist
        fileRef |> shouldBe (FileRef "test-ref")

      it "supports Eq comparison" \_ -> do
        let ref1 = FileRef "ref-a"
        let ref2 = FileRef "ref-a"
        let ref3 = FileRef "ref-b"
        ref1 |> shouldBe ref2
        ref1 |> shouldNotBe ref3

      it "supports Ord for use in Maps" \_ -> do
        let ref1 = FileRef "aaa"
        let ref2 = FileRef "bbb"
        (ref1 < ref2) |> shouldBe True

    -- ==========================================================================
    -- BlobKey Type
    -- ==========================================================================
    describe "BlobKey" do
      it "Show instance does not reveal the actual key" \_ -> do
        -- BlobKey is internal storage key, shouldn't leak
        let blobKey = BlobKey "internal-storage-path-xyz"
        let shown = toText blobKey
        shown |> shouldSatisfy (\t -> not (Text.contains "internal-storage" t))
        shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

      it "can be created from Text" \_ -> do
        let blobKey = BlobKey "storage-key"
        blobKey |> shouldBe (BlobKey "storage-key")

    -- ==========================================================================
    -- OwnerHash Type
    -- ==========================================================================
    describe "OwnerHash" do
      it "Show instance does not reveal the actual hash" \_ -> do
        -- OwnerHash is pseudonymous identity, shouldn't leak
        let ownerHash = OwnerHash "hashed-user-id-abc123"
        let shown = toText ownerHash
        shown |> shouldSatisfy (\t -> not (Text.contains "hashed-user" t))
        shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

    -- ==========================================================================
    -- FileMetadata Type
    -- ==========================================================================
    describe "FileMetadata" do
      it "can be constructed with all fields" \_ -> do
        let metadata = FileMetadata
              { filename = "document.pdf"
              , contentType = "application/pdf"
              , sizeBytes = 1024
              }
        metadata.filename |> shouldBe "document.pdf"
        metadata.contentType |> shouldBe "application/pdf"
        metadata.sizeBytes |> shouldBe 1024

    -- ==========================================================================
    -- ResolvedFile Type
    -- ==========================================================================
    describe "ResolvedFile" do
      it "can be constructed with all fields" \_ -> do
        let resolved = ResolvedFile
              { ref = FileRef "ref-123"
              , metadata = FileMetadata
                  { filename = "test.txt"
                  , contentType = "text/plain"
                  , sizeBytes = 100
                  }
              , blobKey = BlobKey "blob-key-456"
              , uploadedAt = 1700000000
              }
        resolved.ref |> shouldBe (FileRef "ref-123")
        resolved.metadata.filename |> shouldBe "test.txt"
        resolved.blobKey |> shouldBe (BlobKey "blob-key-456")

    -- ==========================================================================
    -- FileUploadEvent Types
    -- ==========================================================================
    describe "FileUploadEvent" do
      it "FileUploaded event contains required fields" \_ -> do
        let eventData = FileUploadedData
              { fileRef = FileRef "ref-1"
              , ownerHash = OwnerHash "owner-hash"
              , contentHash = ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
              , filename = "file.txt"
              , contentType = "text/plain"
              , sizeBytes = 500
              , blobKey = BlobKey "blob-1"
              , expiresAt = 1700003600  -- 6 hours later
              , uploadedAt = 1700000000
              }
        let event = FileUploaded eventData
        case event of
          FileUploaded uploaded -> do
            uploaded.fileRef |> shouldBe (FileRef "ref-1")
            uploaded.sizeBytes |> shouldBe 500
            uploaded.contentHash |> shouldBe (ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890")
          _ -> fail "Expected FileUploaded event"

      it "FileConfirmed event contains required fields" \_ -> do
        let eventData = FileConfirmedData
              { fileRef = FileRef "ref-1"
              , confirmedByRequestId = "request-uuid-123"
              , confirmedAt = 1700000100
              }
        let event = FileConfirmed eventData
        case event of
          FileConfirmed confirmed -> do
            confirmed.fileRef |> shouldBe (FileRef "ref-1")
            confirmed.confirmedByRequestId |> shouldBe "request-uuid-123"
          _ -> fail "Expected FileConfirmed event"

      it "FileDeleted event contains required fields" \_ -> do
        let eventData = FileDeletedData
              { fileRef = FileRef "ref-1"
              , reason = Orphaned
              , deletedAt = 1700010000
              }
        let event = FileDeleted eventData
        case event of
          FileDeleted deleted -> do
            deleted.fileRef |> shouldBe (FileRef "ref-1")
            deleted.reason |> shouldBe Orphaned
          _ -> fail "Expected FileDeleted event"

    -- ==========================================================================
    -- ContentHash Type
    -- ==========================================================================
    describe "ContentHash" do
      describe "Construction" do
        it "can be created from a 64-char hex string" \_ -> do
          let ch = ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
          ch |> shouldBe (ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890")

        it "can be created from empty text" \_ -> do
          let ch = ContentHash ""
          ch |> shouldBe (ContentHash "")

        it "supports Eq comparison — equal values" \_ -> do
          let ch1 = ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
          let ch2 = ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
          ch1 |> shouldBe ch2

        it "supports Eq comparison — different values" \_ -> do
          let ch1 = ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
          let ch2 = ContentHash "bbbbbb1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
          ch1 |> shouldNotBe ch2

        it "supports Ord for use in Maps — less than" \_ -> do
          let ch1 = ContentHash "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          let ch2 = ContentHash "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
          (ch1 < ch2) |> shouldBe True

      describe "Redacted Show Instance" do
        it "Show instance does not reveal the actual hash" \_ -> do
          let ch = ContentHash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          let shown = toText ch
          shown |> shouldSatisfy (\t -> not (Text.contains "e3b0c44298fc" t))

        it "Show instance contains REDACTED marker" \_ -> do
          let ch = ContentHash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          let shown = toText ch
          shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

        it "Prevents accidental exposure in string interpolation" \_ -> do
          let ch = ContentHash "secret-hash-value"
          let msg = [fmt|Hash: #{ch}|]
          msg |> shouldSatisfy (\s -> not (Text.contains "secret-hash-value" s))

      describe "JSON Serialization" do
        it "Round-trips a 64-char hex ContentHash through JSON" \_ -> do
          let x = ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
          Json.decodeText (Json.encodeText x) |> shouldBe (Ok x)

        it "Round-trips empty ContentHash through JSON" \_ -> do
          let x = ContentHash ""
          Json.decodeText (Json.encodeText x) |> shouldBe (Ok x)

        it "Decodes from valid JSON string" \_ -> do
          Json.decodeText @ContentHash "\"somehashvalue\"" |> shouldSatisfy Result.isOk

        it "Rejects non-string JSON (number)" \_ -> do
          Json.decodeText @ContentHash "12345" |> shouldSatisfy Result.isErr

        it "Rejects JSON object" \_ -> do
          Json.decodeText @ContentHash "{}" |> shouldSatisfy Result.isErr

        it "Rejects JSON null" \_ -> do
          Json.decodeText @ContentHash "null" |> shouldSatisfy Result.isErr

    -- ==========================================================================
    -- FileUploadedData with contentHash
    -- ==========================================================================
    describe "FileUploadedData with contentHash" do
      it "FileUploaded event contains contentHash field" \_ -> do
        let eventData = FileUploadedData
              { fileRef = FileRef "ref-1"
              , ownerHash = OwnerHash "owner-hash"
              , contentHash = ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
              , filename = "file.txt"
              , contentType = "text/plain"
              , sizeBytes = 500
              , blobKey = BlobKey "blob-1"
              , expiresAt = 1700003600
              , uploadedAt = 1700000000
              }
        eventData.contentHash |> shouldBe (ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890")

      it "FileUploadedData with contentHash round-trips through JSON" \_ -> do
        let eventData = FileUploadedData
              { fileRef = FileRef "ref-1"
              , ownerHash = OwnerHash "owner-hash"
              , contentHash = ContentHash "abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890"
              , filename = "file.txt"
              , contentType = "text/plain"
              , sizeBytes = 500
              , blobKey = BlobKey "blob-1"
              , expiresAt = 1700003600
              , uploadedAt = 1700000000
              }
        Json.decodeText (Json.encodeText eventData) |> shouldBe (Ok eventData)

    -- ==========================================================================
    -- FileDeletionReason Type
    -- ==========================================================================
    describe "FileDeletionReason" do
      it "has Orphaned variant for TTL expiry" \_ -> do
        let reason = Orphaned
        reason |> shouldBe Orphaned

      it "has UserRequested variant for explicit deletion" \_ -> do
        let reason = UserRequested
        reason |> shouldBe UserRequested

      it "has AdminPurge variant for system cleanup" \_ -> do
        let reason = AdminPurge
        reason |> shouldBe AdminPurge

    -- ==========================================================================
    -- FileUploadConfig Type (User-facing declarative config)
    -- ==========================================================================
    describe "FileUploadConfig" do
      it "can be constructed with in-memory state store" \_ -> do
        let config = FileUploadConfig
              { blobStoreDir = "./uploads"
              , stateStoreBackend = InMemoryStateStore
              , maxFileSizeBytes = 10485760  -- 10 MB
              , pendingTtlSeconds = 21600  -- 6 hours
              , cleanupIntervalSeconds = 900  -- 15 minutes
              , allowedContentTypes = Just ["application/pdf", "image/png"]
              , storeOriginalFilename = True
              }
        config.blobStoreDir |> shouldBe "./uploads"
        config.maxFileSizeBytes |> shouldBe 10485760

      it "can be constructed with postgres state store" \_ -> do
        let config = FileUploadConfig
              { blobStoreDir = "./uploads"
              , stateStoreBackend = PostgresStateStore
                  { pgHost = "localhost"
                  , pgPort = 5432
                  , pgDatabase = "testdb"
                  , pgUser = "testuser"
                  , pgPassword = "testpass"
                  }
              , maxFileSizeBytes = 10485760
              , pendingTtlSeconds = 21600
              , cleanupIntervalSeconds = 900
              , allowedContentTypes = Nothing
              , storeOriginalFilename = True
              }
        config.blobStoreDir |> shouldBe "./uploads"

    -- ==========================================================================
    -- InternalFileUploadConfig Type (Runtime config after initialization)
    -- ==========================================================================
    describe "InternalFileUploadConfig" do
      it "can be constructed with all configuration options" \_ -> do
        let config = InternalFileUploadConfig
              { pendingTtlSeconds = 21600  -- 6 hours
              , cleanupIntervalSeconds = 900  -- 15 minutes
              , maxFileSizeBytes = 10485760  -- 10 MB
              , allowedContentTypes = Just ["application/pdf", "image/png"]
              , storeOriginalFilename = True
              }
        config.pendingTtlSeconds |> shouldBe 21600
        config.maxFileSizeBytes |> shouldBe 10485760

      it "allowedContentTypes can be Nothing to allow all" \_ -> do
        let config = InternalFileUploadConfig
              { pendingTtlSeconds = 21600
              , cleanupIntervalSeconds = 900
              , maxFileSizeBytes = 10485760
              , allowedContentTypes = Nothing
              , storeOriginalFilename = True
              }
        config.allowedContentTypes |> shouldBe Nothing

    -- ==========================================================================
    -- JSON Serialization
    -- ==========================================================================
    describe "JSON Serialization" do
      it "FileRef roundtrips through JSON" \_ -> do
        let fileRef = FileRef "test-ref-json"
        let encoded = Json.encodeText fileRef
        let decoded = Json.decodeText @FileRef encoded
        decoded |> shouldBe (Ok fileRef)

      it "FileMetadata roundtrips through JSON" \_ -> do
        let metadata = FileMetadata
              { filename = "test.pdf"
              , contentType = "application/pdf"
              , sizeBytes = 2048
              }
        let encoded = Json.encodeText metadata
        let decoded = Json.decodeText @FileMetadata encoded
        decoded |> shouldBe (Ok metadata)

      it "FileDeletionReason roundtrips through JSON" \_ -> do
        let checkRoundtrip reason = do
              let encoded = Json.encodeText reason
              let decoded = Json.decodeText @FileDeletionReason encoded
              decoded == Ok reason
        checkRoundtrip Orphaned |> shouldBe True
        checkRoundtrip UserRequested |> shouldBe True
        checkRoundtrip AdminPurge |> shouldBe True


-- Types imported from Service.FileUpload.Core
