module Service.FileUpload.DownloadSpec where

import Bytes qualified
import Core
import DateTime qualified
import Text qualified
import Directory qualified
import "nhcore" Path qualified
import Service.FileUpload.BlobStore (BlobStore (..))
import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..), createBlobStore)
import Service.FileUpload.Core (BlobKey (..), FileRef (..), OwnerHash (..))
import Service.FileUpload.Download (DownloadError (..), DownloadResponse (..), handleDownload)
import Service.FileUpload.Lifecycle (ConfirmedFile (..), FileMetadata (..), FileUploadState (..), PendingFile (..))
import Task qualified
import Test
import Uuid qualified


-- | Type alias for state getter function used in tests
-- Using Text as error type since it's Show-able and simple for tests
type StateGetter = FileRef -> Task Text (Maybe FileUploadState)


spec :: Spec Unit
spec = do
  describe "Service.FileUpload.Download" do
    describe "handleDownload" do
      it "downloads valid pending file for matching owner" \_ -> do
        withTestDownloadEnv \env -> do
          let fileRef = FileRef "file_download1"
          let blobKey = BlobKey "blob_download1"
          let ownerHash = OwnerHash "owner_xyz"
          let content = "This is the file content!" |> Text.toBytes
          now <- DateTime.now
          let expiresAt = DateTime.addSeconds 3600 now

          -- Store blob
          _ <- env.blobStore.store blobKey content
            |> Task.mapError (\e -> [fmt|Store failed: #{show e}|])

          let meta = FileMetadata
                { ref = fileRef
                , filename = "document.pdf"
                , contentType = "application/pdf"
                , sizeBytes = 25
                , blobKey = blobKey
                , uploadedAt = DateTime.toEpochSeconds now
                }
          let state = Pending PendingFile
                { metadata = meta
                , ownerHash = ownerHash
                , expiresAt = DateTime.toEpochSeconds expiresAt
                }

          let getState :: StateGetter
              getState ref' =
                if ref' == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          result <- handleDownload env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Ok response -> do
              response.content |> shouldBe content
              response.contentType |> shouldBe "application/pdf"
              response.filename |> shouldBe "document.pdf"
            Err err -> fail [fmt|Expected success, got: #{show err}|]

      it "downloads confirmed file for matching owner" \_ -> do
        withTestDownloadEnv \env -> do
          let fileRef = FileRef "file_confirmed_dl"
          let blobKey = BlobKey "blob_confirmed_dl"
          let ownerHash = OwnerHash "owner_abc"
          let content = Bytes.replicate 100 0x42
          now <- DateTime.now

          -- Store blob
          _ <- env.blobStore.store blobKey content
            |> Task.mapError (\e -> [fmt|Store failed: #{show e}|])

          let meta = FileMetadata
                { ref = fileRef
                , filename = "confirmed.txt"
                , contentType = "text/plain"
                , sizeBytes = 100
                , blobKey = blobKey
                , uploadedAt = DateTime.toEpochSeconds now
                }
          let state = Confirmed ConfirmedFile
                { metadata = meta
                , ownerHash = ownerHash
                , confirmedByRequestId = "req_123"
                }

          let getState :: StateGetter
              getState ref' =
                if ref' == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          result <- handleDownload env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Ok response -> do
              Bytes.length response.content |> shouldBe 100
              response.contentType |> shouldBe "text/plain"
            Err err -> fail [fmt|Expected success, got: #{show err}|]

      it "rejects download for non-matching owner" \_ -> do
        withTestDownloadEnv \env -> do
          let fileRef = FileRef "file_private"
          let blobKey = BlobKey "blob_private"
          let fileOwner = OwnerHash "owner_alice"
          let requestOwner = OwnerHash "owner_bob"
          let content = "Secret content" |> Text.toBytes
          now <- DateTime.now
          let expiresAt = DateTime.addSeconds 3600 now

          -- Store blob
          _ <- env.blobStore.store blobKey content
            |> Task.mapError (\e -> [fmt|Store failed: #{show e}|])

          let meta = FileMetadata
                { ref = fileRef
                , filename = "private.txt"
                , contentType = "text/plain"
                , sizeBytes = 14
                , blobKey = blobKey
                , uploadedAt = DateTime.toEpochSeconds now
                }
          let state = Pending PendingFile
                { metadata = meta
                , ownerHash = fileOwner
                , expiresAt = DateTime.toEpochSeconds expiresAt
                }

          let getState :: StateGetter
              getState ref' =
                if ref' == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          result <- handleDownload env.blobStore getState requestOwner fileRef
            |> Task.asResult

          case result of
            Err (NotOwner _) -> pass
            Err err -> fail [fmt|Expected NotOwner, got: #{show err}|]
            Ok _ -> fail "Expected NotOwner error"

      it "rejects download for expired pending file" \_ -> do
        withTestDownloadEnv \env -> do
          let fileRef = FileRef "file_expired_dl"
          let blobKey = BlobKey "blob_expired_dl"
          let ownerHash = OwnerHash "owner_xyz"
          let content = "Old content" |> Text.toBytes
          now <- DateTime.now
          let expiredAt = DateTime.addSeconds (-3600) now  -- Expired 1 hour ago

          -- Store blob
          _ <- env.blobStore.store blobKey content
            |> Task.mapError (\e -> [fmt|Store failed: #{show e}|])

          let meta = FileMetadata
                { ref = fileRef
                , filename = "expired.txt"
                , contentType = "text/plain"
                , sizeBytes = 11
                , blobKey = blobKey
                , uploadedAt = DateTime.toEpochSeconds (DateTime.addSeconds (-7200) now)
                }
          let state = Pending PendingFile
                { metadata = meta
                , ownerHash = ownerHash
                , expiresAt = DateTime.toEpochSeconds expiredAt
                }

          let getState :: StateGetter
              getState ref' =
                if ref' == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          result <- handleDownload env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Err (FileExpired _) -> pass
            Err err -> fail [fmt|Expected FileExpired, got: #{show err}|]
            Ok _ -> fail "Expected FileExpired error"

      it "rejects download for deleted file" \_ -> do
        withTestDownloadEnv \env -> do
          let fileRef = FileRef "file_deleted_dl"
          let ownerHash = OwnerHash "owner_xyz"

          let state = Deleted

          let getState :: StateGetter
              getState ref' =
                if ref' == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          result <- handleDownload env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Err (FileDeleted _) -> pass
            Err err -> fail [fmt|Expected FileDeleted, got: #{show err}|]
            Ok _ -> fail "Expected FileDeleted error"

      it "rejects download for non-existent file" \_ -> do
        withTestDownloadEnv \env -> do
          let fileRef = FileRef "file_nonexistent_dl"
          let ownerHash = OwnerHash "owner_xyz"

          let getState :: StateGetter
              getState _ = Task.yield Nothing

          result <- handleDownload env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Err (FileNotFound _) -> pass
            Err err -> fail [fmt|Expected FileNotFound, got: #{show err}|]
            Ok _ -> fail "Expected FileNotFound error"

      it "rejects download when blob is missing" \_ -> do
        withTestDownloadEnv \env -> do
          let fileRef = FileRef "file_nobob_dl"
          let blobKey = BlobKey "blob_missing_dl"  -- Not stored!
          let ownerHash = OwnerHash "owner_xyz"
          now <- DateTime.now
          let expiresAt = DateTime.addSeconds 3600 now

          -- Deliberately NOT storing the blob

          let meta = FileMetadata
                { ref = fileRef
                , filename = "missing.txt"
                , contentType = "text/plain"
                , sizeBytes = 10
                , blobKey = blobKey
                , uploadedAt = DateTime.toEpochSeconds now
                }
          let state = Pending PendingFile
                { metadata = meta
                , ownerHash = ownerHash
                , expiresAt = DateTime.toEpochSeconds expiresAt
                }

          let getState :: StateGetter
              getState ref' =
                if ref' == fileRef
                  then Task.yield (Just state)
                  else Task.yield Nothing

          result <- handleDownload env.blobStore getState ownerHash fileRef
            |> Task.asResult

          case result of
            Err (BlobMissing _) -> pass
            Err err -> fail [fmt|Expected BlobMissing, got: #{show err}|]
            Ok _ -> fail "Expected BlobMissing error"


-- ==========================================================================
-- Test Infrastructure
-- ==========================================================================

data TestDownloadEnv = TestDownloadEnv
  { blobStore :: BlobStore
  , tempDir :: Path
  }


withTestDownloadEnv :: (TestDownloadEnv -> Task Text a) -> Task Text a
withTestDownloadEnv action = do
  -- Create unique temp directory
  uuid <- Uuid.generate
  let uniqueId = Uuid.toText uuid
  let tempDirText = [fmt|/tmp/neohaskell-download-test-#{uniqueId}|]

  tempDir <- case Path.fromText tempDirText of
    Just p -> Task.yield p
    Nothing -> Task.throw "Failed to create temp path"

  -- Create directory
  _ <- Directory.createIfMissing tempDir
    |> Task.mapError (\e -> [fmt|Failed to create temp directory: #{show e}|])

  -- Create blob store
  let blobStoreConfig = LocalBlobStoreConfig { rootDir = tempDir }
  blobStore <- createBlobStore blobStoreConfig

  let env = TestDownloadEnv
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
