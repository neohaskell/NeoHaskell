module Service.FileUpload.BlobStore.LocalSpec where

import Array qualified
import AsyncTask qualified
import Bytes qualified
import Core
import Directory qualified
import "nhcore" Path qualified
import Result qualified
import Service.FileUpload.BlobStore (BlobStore (..), BlobStoreError (..))
import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..), createBlobStore)
import Service.FileUpload.Core (BlobKey (..))
import Task qualified
import Test
import Text qualified
import Uuid qualified


spec :: Spec Unit
spec = do
  describe "Service.FileUpload.BlobStore.Local" do
    -- ==========================================================================
    -- Basic Operations
    -- ==========================================================================
    describe "Basic Operations" do
      it "store and retrieve roundtrips bytes correctly" \_ -> do
        withTempBlobStore \blobStore -> do
          let blobKey = BlobKey "test-blob-1"
          let content = "Hello, World!" |> Text.toBytes
          
          -- Store
          blobStore.store blobKey content
            |> Task.mapError toText
            |> discard
          
          -- Retrieve
          retrieved <- blobStore.retrieve blobKey
            |> Task.mapError toText
          
          retrieved |> shouldBe content

      it "store overwrites existing blob" \_ -> do
        withTempBlobStore \blobStore -> do
          let blobKey = BlobKey "overwrite-test"
          let content1 = "First content" |> Text.toBytes
          let content2 = "Second content" |> Text.toBytes
          
          -- Store first
          blobStore.store blobKey content1
            |> Task.mapError toText
            |> discard
          
          -- Overwrite
          blobStore.store blobKey content2
            |> Task.mapError toText
            |> discard
          
          -- Retrieve should return second content
          retrieved <- blobStore.retrieve blobKey
            |> Task.mapError toText
          
          retrieved |> shouldBe content2

      it "retrieve returns NotFound for non-existent blob" \_ -> do
        withTempBlobStore \blobStore -> do
          let blobKey = BlobKey "non-existent"
          
          result <- blobStore.retrieve blobKey
            |> Task.asResult
          
          case result of
            Err (NotFound _) -> pass
            Err err -> fail [fmt|Expected NotFound, got: #{toText err}|]
            Ok _ -> fail "Expected NotFound error"

      it "delete removes blob" \_ -> do
        withTempBlobStore \blobStore -> do
          let blobKey = BlobKey "delete-test"
          let content = "To be deleted" |> Text.toBytes
          
          -- Store
          blobStore.store blobKey content
            |> Task.mapError toText
            |> discard
          
          -- Delete
          blobStore.delete blobKey
            |> Task.mapError toText
            |> discard
          
          -- Retrieve should fail
          result <- blobStore.retrieve blobKey
            |> Task.asResult
          
          case result of
            Err (NotFound _) -> pass
            _ -> fail "Expected NotFound after delete"

      it "delete is idempotent - deleting non-existent blob succeeds" \_ -> do
        withTempBlobStore \blobStore -> do
          let blobKey = BlobKey "never-existed"
          
          result <- blobStore.delete blobKey
            |> Task.mapError toText
            |> Task.asResult
          
          result |> shouldSatisfy Result.isOk

      it "exists returns True for existing blob" \_ -> do
        withTempBlobStore \blobStore -> do
          let blobKey = BlobKey "exists-test"
          let content = "I exist" |> Text.toBytes
          
          -- Store
          blobStore.store blobKey content
            |> Task.mapError toText
            |> discard
          
          -- Check exists
          doesExist <- blobStore.exists blobKey
            |> Task.mapError toText
          
          doesExist |> shouldBe True

      it "exists returns False for non-existent blob" \_ -> do
        withTempBlobStore \blobStore -> do
          let blobKey = BlobKey "does-not-exist"
          
          doesExist <- blobStore.exists blobKey
            |> Task.mapError toText
          
          doesExist |> shouldBe False

    -- ==========================================================================
    -- Large Files
    -- ==========================================================================
    describe "Large Files" do
      it "handles 1MB file" \_ -> do
        withTempBlobStore \blobStore -> do
          let blobKey = BlobKey "large-file"
          -- Create 1MB of data
          let content = Bytes.replicate (1024 * 1024) 0x42  -- 1MB of 'B'
          
          -- Store
          blobStore.store blobKey content
            |> Task.mapError toText
            |> discard
          
          -- Retrieve
          retrieved <- blobStore.retrieve blobKey
            |> Task.mapError toText
          
          Bytes.length retrieved |> shouldBe (1024 * 1024)

    -- ==========================================================================
    -- Binary Data
    -- ==========================================================================
    describe "Binary Data" do
      it "handles binary content with null bytes" \_ -> do
        withTempBlobStore \blobStore -> do
          let blobKey = BlobKey "binary-test"
          let content = Bytes.pack [0x00, 0x01, 0x02, 0xFF, 0xFE, 0x00, 0x00]
          
          -- Store
          blobStore.store blobKey content
            |> Task.mapError toText
            |> discard
          
          -- Retrieve
          retrieved <- blobStore.retrieve blobKey
            |> Task.mapError toText
          
          retrieved |> shouldBe content

      it "handles empty content" \_ -> do
        withTempBlobStore \blobStore -> do
          let blobKey = BlobKey "empty-file"
          let content = Bytes.empty
          
          -- Store empty file
          blobStore.store blobKey content
            |> Task.mapError toText
            |> discard
          
          -- Retrieve
          retrieved <- blobStore.retrieve blobKey
            |> Task.mapError toText
          
          Bytes.length retrieved |> shouldBe 0

    -- ==========================================================================
    -- Path Safety
    -- ==========================================================================
    describe "Path Safety" do
      it "rejects blob keys with path traversal attempts" \_ -> do
        withTempBlobStore \blobStore -> do
          let maliciousKey = BlobKey "../../../etc/passwd"
          let content = "malicious" |> Text.toBytes
          
          result <- blobStore.store maliciousKey content
            |> Task.asResult
          
          case result of
            Err (InvalidBlobKey _) -> pass
            Err err -> fail [fmt|Expected InvalidBlobKey, got: #{toText err}|]
            Ok _ -> fail "Expected error for path traversal"

      it "rejects blob keys with absolute paths" \_ -> do
        withTempBlobStore \blobStore -> do
          let maliciousKey = BlobKey "/etc/passwd"
          let content = "malicious" |> Text.toBytes
          
          result <- blobStore.store maliciousKey content
            |> Task.asResult
          
          case result of
            Err (InvalidBlobKey _) -> pass
            _ -> fail "Expected error for absolute path"

      it "rejects blob keys with slashes" \_ -> do
        withTempBlobStore \blobStore -> do
          let maliciousKey = BlobKey "subdir/file"
          let content = "content" |> Text.toBytes
          
          result <- blobStore.store maliciousKey content
            |> Task.asResult
          
          case result of
            Err (InvalidBlobKey _) -> pass
            _ -> fail "Expected error for path with slash"

    -- ==========================================================================
    -- Concurrent Access
    -- ==========================================================================
    describe "Concurrent Access" do
      it "handles concurrent reads to same blob" \_ -> do
        withTempBlobStore \blobStore -> do
          let blobKey = BlobKey "concurrent-read"
          let content = "shared content" |> Text.toBytes
          
          -- Store once
          blobStore.store blobKey content
            |> Task.mapError toText
            |> discard
          
          -- Read concurrently
          results <- Array.initialize 10 identity
            |> Array.map (\_ -> do
                AsyncTask.run (blobStore.retrieve blobKey |> Task.mapError toText)
              )
            |> Task.mapArray identity
            |> Task.andThen (\tasks ->
                tasks |> Array.map AsyncTask.waitFor |> Task.mapArray identity
              )
          
          -- All should succeed with same content
          results |> Array.length |> shouldBe 10
          -- Check no result differs from expected content
          let allMatch = not (results |> Array.any (\r -> r != content))
          allMatch |> shouldBe True

      it "handles concurrent writes to different blobs" \_ -> do
        withTempBlobStore \blobStore -> do
          -- Write to 10 different blobs concurrently
          tasks <- Array.initialize 10 identity
            |> Array.map (\i -> do
                let blobKey = BlobKey [fmt|blob-#{i}|]
                let fileContent = [fmt|content-#{i}|] |> Text.toBytes
                AsyncTask.run (blobStore.store blobKey fileContent |> Task.mapError toText)
              )
            |> Task.mapArray identity
          
          -- Wait for all
          tasks |> Array.map AsyncTask.waitFor |> Task.mapArray identity |> discard
          
          -- Verify all exist
          existResults <- Array.initialize 10 identity
            |> Array.map (\i -> do
                let blobKey = BlobKey [fmt|blob-#{i}|]
                blobStore.exists blobKey |> Task.mapError toText
              )
            |> Task.mapArray identity
          
          -- All should be True
          let allExist = not (existResults |> Array.any (\exists -> not exists))
          allExist |> shouldBe True


-- ==========================================================================
-- Test Infrastructure
-- ==========================================================================

-- | Create a temporary blob store for testing
withTempBlobStore :: (BlobStore -> Task Text a) -> Task Text a
withTempBlobStore action = do
  -- Create unique temp directory
  uuid <- Uuid.generate
  let uniqueId = Uuid.toText uuid
  let tempDirText = [fmt|/tmp/neohaskell-blobstore-test-#{uniqueId}|]
  
  tempDir <- case Path.fromText tempDirText of
    Just p -> Task.yield p
    Nothing -> Task.throw "Failed to create temp path"
  
  -- Create directory
  _ <- Directory.createIfMissing tempDir
    |> Task.mapError (\e -> [fmt|Failed to create temp directory: #{show e}|])
  
  -- Create blob store
  let config = LocalBlobStoreConfig { rootDir = tempDir }
  blobStore <- createBlobStore config
  
  -- Run action
  result <- action blobStore |> Task.asResult
  
  -- Cleanup
  Directory.removeRecursive tempDir
    |> Task.mapError (\_ -> "Failed to remove temp directory" :: Text)
    |> Task.asResult
    |> discard
  
  -- Return result
  case result of
    Ok a -> Task.yield a
    Err e -> Task.throw e


-- Types and functions imported from Service.FileUpload.BlobStore and Service.FileUpload.BlobStore.Local
