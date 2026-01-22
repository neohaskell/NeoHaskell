module Service.FileUpload.ResolverSpec where

import Bytes qualified
import Core
import DateTime qualified
import Directory qualified
import Map qualified
import "nhcore" Path qualified
import Service.FileUpload.BlobStore (BlobStore (..))
import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..), createBlobStore)
import Service.FileUpload.Core (BlobKey (..), FileRef (..), OwnerHash (..))
import Service.FileUpload.Lifecycle (ConfirmedFile (..), FileMetadata (..), FileUploadState (..), PendingFile (..))
import Service.FileUpload.Resolver (ResolveError (..), ResolvedFile (..), resolveFileRef, resolveFileRefs)
import Task qualified
import Test
import Uuid qualified


spec :: Spec Unit
spec = do
  describe "Service.FileUpload.Resolver" do
    -- ==========================================================================
    -- Single File Resolution
    -- ==========================================================================
    describe "resolveFileRef" do
      it "resolves valid pending file for matching owner" \_ -> do
        withTestResolverEnv \env -> do
          -- Create a pending file state
          let fileRef = FileRef "file_test123"
          let blobKey = BlobKey "blob_abc"
          let ownerHash = OwnerHash "owner_xyz"
          now <- DateTime.now
          let expiresAt = DateTime.addSeconds 3600 now  -- Expires in 1 hour

          let meta = FileMetadata
                { ref = fileRef
                , filename = "test.pdf"
                , contentType = "application/pdf"
                , sizeBytes = 1024
                , blobKey = blobKey
                , uploadedAt = DateTime.toEpochSeconds now
                }
          let state = Pending PendingFile
                { metadata = meta
                , ownerHash = ownerHash
                , expiresAt = DateTime.toEpochSeconds expiresAt
                }

          -- Store blob in BlobStore
          let content = Bytes.replicate 1024 0x42
          _ <- env.blobStore.store blobKey content
            |> Task.mapError (\e -> [fmt|Store failed: #{show e}|])

          -- Create state lookup function
          let getState :: FileRef -> Task Text (Maybe FileUploadState)
              getState ref = do
                if ref == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          -- Resolve with matching owner
          result <- resolveFileRef env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Ok resolved -> do
              resolved.ref |> shouldBe fileRef
              resolved.filename |> shouldBe "test.pdf"
              resolved.contentType |> shouldBe "application/pdf"
              resolved.sizeBytes |> shouldBe 1024
            Err err -> fail [fmt|Expected success, got: #{show err}|]

      it "resolves valid confirmed file for matching owner" \_ -> do
        withTestResolverEnv \env -> do
          let fileRef = FileRef "file_confirmed"
          let blobKey = BlobKey "blob_confirmed"
          let ownerHash = OwnerHash "owner_xyz"
          now <- DateTime.now

          let meta = FileMetadata
                { ref = fileRef
                , filename = "confirmed.pdf"
                , contentType = "application/pdf"
                , sizeBytes = 2048
                , blobKey = blobKey
                , uploadedAt = DateTime.toEpochSeconds now
                }
          let state = Confirmed ConfirmedFile
                { metadata = meta
                , ownerHash = ownerHash
                , confirmedByRequestId = "req_123"
                }

          -- Store blob
          let content = Bytes.replicate 2048 0x42
          _ <- env.blobStore.store blobKey content
            |> Task.mapError (\e -> [fmt|Store failed: #{show e}|])

          let getState ref =
                if ref == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          result <- resolveFileRef env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Ok resolved -> resolved.filename |> shouldBe "confirmed.pdf"
            Err err -> fail [fmt|Expected success, got: #{show err}|]

      it "rejects file with non-matching owner" \_ -> do
        withTestResolverEnv \env -> do
          let fileRef = FileRef "file_other"
          let blobKey = BlobKey "blob_other"
          let fileOwner = OwnerHash "owner_alice"
          let requestOwner = OwnerHash "owner_bob"  -- Different!
          now <- DateTime.now
          let expiresAt = DateTime.addSeconds 3600 now

          let meta = FileMetadata
                { ref = fileRef
                , filename = "private.pdf"
                , contentType = "application/pdf"
                , sizeBytes = 512
                , blobKey = blobKey
                , uploadedAt = DateTime.toEpochSeconds now
                }
          let state = Pending PendingFile
                { metadata = meta
                , ownerHash = fileOwner
                , expiresAt = DateTime.toEpochSeconds expiresAt
                }

          -- Store blob
          let content = Bytes.replicate 512 0x42
          _ <- env.blobStore.store blobKey content
            |> Task.mapError (\e -> [fmt|Store failed: #{show e}|])

          let getState ref =
                if ref == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          result <- resolveFileRef env.blobStore getState requestOwner fileRef
            |> Task.asResult

          case result of
            Err (NotOwner _) -> pass
            Err err -> fail [fmt|Expected NotOwner, got: #{show err}|]
            Ok _ -> fail "Expected NotOwner error"

      it "rejects expired pending file" \_ -> do
        withTestResolverEnv \env -> do
          let fileRef = FileRef "file_expired"
          let blobKey = BlobKey "blob_expired"
          let ownerHash = OwnerHash "owner_xyz"
          now <- DateTime.now
          let expiredAt = DateTime.addSeconds (-3600) now  -- Expired 1 hour ago

          let meta = FileMetadata
                { ref = fileRef
                , filename = "expired.pdf"
                , contentType = "application/pdf"
                , sizeBytes = 256
                , blobKey = blobKey
                , uploadedAt = DateTime.toEpochSeconds (DateTime.addSeconds (-7200) now)
                }
          let state = Pending PendingFile
                { metadata = meta
                , ownerHash = ownerHash
                , expiresAt = DateTime.toEpochSeconds expiredAt
                }

          -- Store blob (even though expired)
          let content = Bytes.replicate 256 0x42
          _ <- env.blobStore.store blobKey content
            |> Task.mapError (\e -> [fmt|Store failed: #{show e}|])

          let getState ref =
                if ref == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          result <- resolveFileRef env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Err (FileExpired _) -> pass
            Err err -> fail [fmt|Expected FileExpired, got: #{show err}|]
            Ok _ -> fail "Expected FileExpired error"

      it "rejects deleted file" \_ -> do
        withTestResolverEnv \env -> do
          let fileRef = FileRef "file_deleted"
          let ownerHash = OwnerHash "owner_xyz"

          let state = Deleted

          let getState ref =
                if ref == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          result <- resolveFileRef env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Err (FileDeleted _) -> pass
            Err err -> fail [fmt|Expected FileDeleted, got: #{show err}|]
            Ok _ -> fail "Expected FileDeleted error"

      it "rejects non-existent file reference" \_ -> do
        withTestResolverEnv \env -> do
          let fileRef = FileRef "file_nonexistent"
          let ownerHash = OwnerHash "owner_xyz"

          let getState _ = Task.yield Nothing

          result <- resolveFileRef env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Err (FileNotFound _) -> pass
            Err err -> fail [fmt|Expected FileNotFound, got: #{show err}|]
            Ok _ -> fail "Expected FileNotFound error"

      it "rejects file when blob is missing" \_ -> do
        withTestResolverEnv \env -> do
          let fileRef = FileRef "file_nobob"
          let blobKey = BlobKey "blob_missing"  -- Not stored!
          let ownerHash = OwnerHash "owner_xyz"
          now <- DateTime.now
          let expiresAt = DateTime.addSeconds 3600 now

          let meta = FileMetadata
                { ref = fileRef
                , filename = "missing.pdf"
                , contentType = "application/pdf"
                , sizeBytes = 128
                , blobKey = blobKey
                , uploadedAt = DateTime.toEpochSeconds now
                }
          let state = Pending PendingFile
                { metadata = meta
                , ownerHash = ownerHash
                , expiresAt = DateTime.toEpochSeconds expiresAt
                }

          -- Deliberately NOT storing the blob

          let getState ref =
                if ref == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          result <- resolveFileRef env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Err (BlobMissing _) -> pass
            Err err -> fail [fmt|Expected BlobMissing, got: #{show err}|]
            Ok _ -> fail "Expected BlobMissing error"

    -- ==========================================================================
    -- Multiple File Resolution
    -- ==========================================================================
    describe "resolveFileRefs" do
      it "resolves multiple valid files" \_ -> do
        withTestResolverEnv \env -> do
          let fileRef1 = FileRef "file_1"
          let fileRef2 = FileRef "file_2"
          let blobKey1 = BlobKey "blob_1"
          let blobKey2 = BlobKey "blob_2"
          let ownerHash = OwnerHash "owner_xyz"
          now <- DateTime.now
          let expiresAt = DateTime.addSeconds 3600 now

          let meta1 = FileMetadata
                { ref = fileRef1
                , filename = "file1.txt"
                , contentType = "text/plain"
                , sizeBytes = 100
                , blobKey = blobKey1
                , uploadedAt = DateTime.toEpochSeconds now
                }
          let state1 = Pending PendingFile
                { metadata = meta1
                , ownerHash = ownerHash
                , expiresAt = DateTime.toEpochSeconds expiresAt
                }

          let meta2 = FileMetadata
                { ref = fileRef2
                , filename = "file2.txt"
                , contentType = "text/plain"
                , sizeBytes = 200
                , blobKey = blobKey2
                , uploadedAt = DateTime.toEpochSeconds now
                }
          let state2 = Pending PendingFile
                { metadata = meta2
                , ownerHash = ownerHash
                , expiresAt = DateTime.toEpochSeconds expiresAt
                }

          -- Store blobs
          _ <- env.blobStore.store blobKey1 (Bytes.replicate 100 0x41)
            |> Task.mapError (\e -> [fmt|Store failed: #{show e}|])
          _ <- env.blobStore.store blobKey2 (Bytes.replicate 200 0x42)
            |> Task.mapError (\e -> [fmt|Store failed: #{show e}|])

          let getState ref
                | ref == fileRef1 = Task.yield (Just state1)
                | ref == fileRef2 = Task.yield (Just state2)
                | otherwise = Task.yield Nothing

          result <- resolveFileRefs env.blobStore getState ownerHash [fileRef1, fileRef2]
            |> Task.asResult

          case result of
            Ok resolvedMap -> do
              Map.length resolvedMap |> shouldBe 2
              case Map.get fileRef1 resolvedMap of
                Just r1 -> r1.filename |> shouldBe "file1.txt"
                Nothing -> fail "fileRef1 not in map"
              case Map.get fileRef2 resolvedMap of
                Just r2 -> r2.filename |> shouldBe "file2.txt"
                Nothing -> fail "fileRef2 not in map"
            Err err -> fail [fmt|Expected success, got: #{show err}|]

      it "returns empty map for empty input" \_ -> do
        withTestResolverEnv \env -> do
          let ownerHash = OwnerHash "owner_xyz"
          let getState _ = Task.yield Nothing

          result <- resolveFileRefs env.blobStore getState ownerHash []
            |> Task.asResult

          case result of
            Ok resolvedMap -> Map.length resolvedMap |> shouldBe 0
            Err err -> fail [fmt|Expected empty map, got: #{show err}|]

      it "fails if any file fails to resolve" \_ -> do
        withTestResolverEnv \env -> do
          let fileRef1 = FileRef "file_valid"
          let fileRef2 = FileRef "file_invalid"
          let blobKey1 = BlobKey "blob_valid"
          let ownerHash = OwnerHash "owner_xyz"
          now <- DateTime.now
          let expiresAt = DateTime.addSeconds 3600 now

          let meta1 = FileMetadata
                { ref = fileRef1
                , filename = "valid.txt"
                , contentType = "text/plain"
                , sizeBytes = 100
                , blobKey = blobKey1
                , uploadedAt = DateTime.toEpochSeconds now
                }
          let state1 = Pending PendingFile
                { metadata = meta1
                , ownerHash = ownerHash
                , expiresAt = DateTime.toEpochSeconds expiresAt
                }

          -- Store only first blob
          _ <- env.blobStore.store blobKey1 (Bytes.replicate 100 0x41)
            |> Task.mapError (\e -> [fmt|Store failed: #{show e}|])

          let getState ref
                | ref == fileRef1 = Task.yield (Just state1)
                | otherwise = Task.yield Nothing  -- fileRef2 not found

          result <- resolveFileRefs env.blobStore getState ownerHash [fileRef1, fileRef2]
            |> Task.asResult

          case result of
            Err (FileNotFound ref) -> ref |> shouldBe fileRef2
            Err err -> fail [fmt|Expected FileNotFound, got: #{show err}|]
            Ok _ -> fail "Expected failure for invalid file"


-- ==========================================================================
-- Test Infrastructure
-- ==========================================================================

data TestResolverEnv = TestResolverEnv
  { blobStore :: BlobStore
  , tempDir :: Path
  }


withTestResolverEnv :: (TestResolverEnv -> Task Text a) -> Task Text a
withTestResolverEnv action = do
  -- Create unique temp directory
  uuid <- Uuid.generate
  let uniqueId = Uuid.toText uuid
  let tempDirText = [fmt|/tmp/neohaskell-resolver-test-#{uniqueId}|]

  tempDir <- case Path.fromText tempDirText of
    Just p -> Task.yield p
    Nothing -> Task.throw "Failed to create temp path"

  -- Create directory
  _ <- Directory.createIfMissing tempDir
    |> Task.mapError (\e -> [fmt|Failed to create temp directory: #{show e}|])

  -- Create blob store
  let blobStoreConfig = LocalBlobStoreConfig { rootDir = tempDir }
  blobStore <- createBlobStore blobStoreConfig

  let env = TestResolverEnv
        { blobStore = blobStore
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
