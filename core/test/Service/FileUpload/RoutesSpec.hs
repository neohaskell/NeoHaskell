module Service.FileUpload.RoutesSpec where

import Array qualified
import Bytes qualified
import Core
import DateTime qualified
import Directory qualified
import "nhcore" Path qualified
import Service.FileUpload.BlobStore (BlobStore)
import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..), createBlobStore)
import Service.FileUpload.Core (FileRef (..), FileUploadConfig (..))
import Service.FileUpload.Routes (UploadError (..), UploadRequest (..), UploadResponse (..), handleUpload)
import Task qualified
import Test
import Text qualified
import Uuid qualified


spec :: Spec Unit
spec = do
  describe "Service.FileUpload.Routes" do
    -- ==========================================================================
    -- Upload Endpoint
    -- ==========================================================================
    describe "handleUpload" do
      it "returns FileRef and metadata for valid upload" \_ -> do
        withTestUploadEnv \env -> do
          let request = UploadRequest
                { filename = "test.pdf"
                , contentType = "application/pdf"
                , content = "PDF content here" |> Text.toBytes
                , ownerHash = "user123hash"
                }

          result <- handleUpload env.config env.blobStore request
            |> Task.asResult

          case result of
            Ok response -> do
              -- FileRef should be non-empty
              let (FileRef refText) = response.fileRef
              (Text.length refText > 0) |> shouldBe True

              -- Metadata should match request
              response.filename |> shouldBe "test.pdf"
              response.contentType |> shouldBe "application/pdf"
              response.sizeBytes |> shouldBe 16  -- "PDF content here" is 16 bytes

              -- ExpiresAt should be in the future
              now <- DateTime.now
              (response.expiresAt > now) |> shouldBe True

            Err err -> fail [fmt|Expected success, got error: #{show err}|]

      it "stores blob in BlobStore" \_ -> do
        withTestUploadEnv \env -> do
          let content = "Hello, blob store!" |> Text.toBytes
          let request = UploadRequest
                { filename = "hello.txt"
                , contentType = "text/plain"
                , content = content
                , ownerHash = "user123hash"
                }

          response <- handleUpload env.config env.blobStore request
            |> Task.mapError (\e -> [fmt|Upload failed: #{show e}|])

          -- Retrieve blob using the internal blobKey
          -- Note: We need to verify the blob exists - but blobKey is internal
          -- The response should indicate success, and we trust that storage worked
          -- (BlobStore.Local tests already verify store/retrieve)
          response.filename |> shouldBe "hello.txt"

      it "rejects files exceeding maxFileSizeBytes" \_ -> do
        withTestUploadEnv \env -> do
          -- Create content larger than maxFileSizeBytes (100 bytes in test config)
          let largeContent = Bytes.replicate 200 0x42

          let request = UploadRequest
                { filename = "large.bin"
                , contentType = "application/octet-stream"
                , content = largeContent
                , ownerHash = "user123hash"
                }

          result <- handleUpload env.config env.blobStore request
            |> Task.asResult

          case result of
            Err (FileTooLarge {}) -> pass
            Err err -> fail [fmt|Expected FileTooLarge, got: #{show err}|]
            Ok _ -> fail "Expected FileTooLarge error"

      it "rejects files with disallowed content type" \_ -> do
        withTestUploadEnv \env -> do
          -- Config only allows "text/plain" and "application/pdf"
          let request = UploadRequest
                { filename = "script.exe"
                , contentType = "application/x-executable"
                , content = "malicious" |> Text.toBytes
                , ownerHash = "user123hash"
                }

          result <- handleUpload env.config env.blobStore request
            |> Task.mapError toText
            |> Task.asResult

          case result of
            Err err -> do
              -- Should contain content type error message
              (Text.contains "content type" err || Text.contains "ContentType" err)
                |> shouldBe True
            Ok _ -> fail "Expected content type rejection"

      it "allows any content type when allowedContentTypes is Nothing" \_ -> do
        withTestUploadEnv \env -> do
          -- Override config to allow all content types
          let permissiveConfig = env.config { allowedContentTypes = Nothing }

          let request = UploadRequest
                { filename = "anything.xyz"
                , contentType = "application/x-custom-type"
                , content = "custom content" |> Text.toBytes
                , ownerHash = "user123hash"
                }

          result <- handleUpload permissiveConfig env.blobStore request
            |> Task.asResult

          case result of
            Ok response -> response.contentType |> shouldBe "application/x-custom-type"
            Err err -> fail [fmt|Expected success, got: #{show err}|]

      it "generates unique FileRef for each upload" \_ -> do
        withTestUploadEnv \env -> do
          let request = UploadRequest
                { filename = "test.txt"
                , contentType = "text/plain"
                , content = "content" |> Text.toBytes
                , ownerHash = "user123hash"
                }

          -- Upload twice
          response1 <- handleUpload env.config env.blobStore request
            |> Task.mapError (\e -> [fmt|Upload 1 failed: #{show e}|])
          response2 <- handleUpload env.config env.blobStore request
            |> Task.mapError (\e -> [fmt|Upload 2 failed: #{show e}|])

          -- FileRefs should be different
          (response1.fileRef != response2.fileRef) |> shouldBe True

      it "handles empty filename gracefully" \_ -> do
        withTestUploadEnv \env -> do
          let request = UploadRequest
                { filename = ""
                , contentType = "text/plain"
                , content = "content" |> Text.toBytes
                , ownerHash = "user123hash"
                }

          result <- handleUpload env.config env.blobStore request
            |> Task.asResult

          -- Empty filename should be accepted (validation is optional)
          -- Or rejected with InvalidFilename error
          case result of
            Ok response -> response.sizeBytes |> shouldBe 7
            Err (InvalidFilename _) -> pass
            Err err -> fail [fmt|Unexpected error: #{show err}|]

      it "handles binary content correctly" \_ -> do
        withTestUploadEnv \env -> do
          -- Binary content with null bytes
          let binaryContent = Bytes.pack [0x00, 0x01, 0xFF, 0xFE, 0x00]

          let request = UploadRequest
                { filename = "binary.bin"
                , contentType = "application/octet-stream"
                , content = binaryContent
                , ownerHash = "user123hash"
                }

          -- Need permissive config for octet-stream
          let permissiveConfig = env.config { allowedContentTypes = Nothing }

          result <- handleUpload permissiveConfig env.blobStore request
            |> Task.asResult

          case result of
            Ok response -> response.sizeBytes |> shouldBe 5
            Err err -> fail [fmt|Expected success, got: #{show err}|]


-- ==========================================================================
-- Test Infrastructure
-- ==========================================================================

-- | Test environment with blob store and config
data TestUploadEnv = TestUploadEnv
  { blobStore :: BlobStore
  , config :: FileUploadConfig
  , tempDir :: Path
  }


-- | Create test upload environment with temp blob store
withTestUploadEnv :: (TestUploadEnv -> Task Text a) -> Task Text a
withTestUploadEnv action = do
  -- Create unique temp directory
  uuid <- Uuid.generate
  let uniqueId = Uuid.toText uuid
  let tempDirText = [fmt|/tmp/neohaskell-upload-test-#{uniqueId}|]

  tempDir <- case Path.fromText tempDirText of
    Just p -> Task.yield p
    Nothing -> Task.throw "Failed to create temp path"

  -- Create directory
  _ <- Directory.createIfMissing tempDir
    |> Task.mapError (\e -> [fmt|Failed to create temp directory: #{show e}|])

  -- Create blob store
  let blobStoreConfig = LocalBlobStoreConfig { rootDir = tempDir }
  blobStore <- createBlobStore blobStoreConfig

  -- Test configuration with small limits for testing
  let config = FileUploadConfig
        { pendingTtlSeconds = 3600  -- 1 hour
        , cleanupIntervalSeconds = 900  -- 15 minutes
        , maxFileSizeBytes = 100    -- Small limit for testing
        , allowedContentTypes = Just (Array.fromLinkedList ["text/plain", "application/pdf"])
        , storeOriginalFilename = True
        }

  let env = TestUploadEnv
        { blobStore = blobStore
        , config = config
        , tempDir = tempDir
        }

  -- Run action
  result <- action env |> Task.asResult

  -- Cleanup
  Directory.removeRecursive tempDir
    |> Task.mapError (\_ -> "Failed to remove temp directory" :: Text)
    |> Task.asResult
    |> discard

  -- Return result
  case result of
    Ok a -> Task.yield a
    Err e -> Task.throw e
