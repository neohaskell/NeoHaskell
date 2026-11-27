---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Applications can now store uploaded files in S3-compatible object storage (such as AWS S3, Cloudflare R2, Backblaze B2, or MinIO) as well as on local disk through `EventModel.withFileUpload` and `Application.withFileUpload`. `FileUploadConfig` now takes a typed `blobStore` backend configuration instead of a local `blobStoreDir` text field, matching how `withEventStore` selects between `SimpleEventStore` and `PostgresEventStore`.

The new `Service.FileUpload.BlobStore.S3` module signs path-style `PUT`, `GET`, `HEAD`, and `DELETE` requests with AWS Signature Version 4 and validates endpoint URLs, bucket names, and header-bound fields at startup before sending requests. Shared upload limits (`maxFileSizeBytes`, `pendingTtlSeconds`, and `cleanupIntervalSeconds`) are validated before initializing the selected blob store, and backend errors stop startup without falling back to local disk.

## Migration

Apps that do not call `withFileUpload` or construct `FileUploadConfig` need no changes.

Apps that construct `FileUploadConfig` must replace the `blobStoreDir :: Text` field with `blobStore :: blobStore`, where `blobStore` implements `BlobStoreConfig` from `Service.FileUpload.BlobStore`.

To keep local filesystem storage:

1. Import `Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..))`, and `Path qualified` when converting a dynamic `Text` path.
2. Convert your directory path to `Path` (use `[path|./uploads|]` from `Core` for a literal path or `Path.fromText config.uploadDir` for a configured `Text` path) and pass `LocalBlobStoreConfig { rootDir = ... }` in `blobStore`. Ensure the configured directory is not empty or whitespace, because startup rejects blank `rootDir` values.

Before:

```haskell
makeFileUploadConfig :: AppConfig -> FileUploadConfig
makeFileUploadConfig config =
  FileUploadConfig
    { blobStoreDir = config.uploadDir
    , stateStoreBackend = InMemoryStateStore
    , maxFileSizeBytes = 10485760
    , pendingTtlSeconds = 21600
    , cleanupIntervalSeconds = 900
    , allowedContentTypes = Nothing
    , storeOriginalFilename = True
    }
```

After (local disk):

```haskell
makeFileUploadConfig :: AppConfig -> FileUploadConfig LocalBlobStoreConfig
makeFileUploadConfig config = do
  let uploadDir = config.uploadDir
  let rootDir = case Path.fromText uploadDir of
        Just path -> path
        Nothing -> panic [fmt|Invalid upload directory path: #{uploadDir}|]
  FileUploadConfig
    { blobStore = LocalBlobStoreConfig { rootDir }
    , stateStoreBackend = InMemoryStateStore
    , maxFileSizeBytes = 10485760
    , pendingTtlSeconds = 21600
    , cleanupIntervalSeconds = 900
    , allowedContentTypes = Nothing
    , storeOriginalFilename = True
    }
```

To switch file uploads to S3-compatible storage, import `Service.FileUpload.BlobStore.S3 (S3Config (..))`, mark your secret key field with `Config.secret` in `defineConfig`, and build `FileUploadConfig S3Config`:

```haskell
makeFileUploadConfig :: AppConfig -> FileUploadConfig S3Config
makeFileUploadConfig config =
  FileUploadConfig
    { blobStore =
        S3Config
          { endpoint = config.s3Endpoint
          , bucket = config.s3Bucket
          , region = config.s3Region
          , accessKeyId = config.s3AccessKeyId
          , secretAccessKey = config.s3SecretAccessKey
          }
    , stateStoreBackend = InMemoryStateStore
    , maxFileSizeBytes = 10485760
    , pendingTtlSeconds = 21600
    , cleanupIntervalSeconds = 900
    , allowedContentTypes = Nothing
    , storeOriginalFilename = True
    }
```

The S3 bucket must already exist before starting the application, and `endpoint` must use `https://` unless the host is `127.0.0.1`, `[::1]`, or `localhost`. Because `S3Config` omits `Show` and JSON instances to keep credentials out of logs, remove any `show` or JSON serialization of `FileUploadConfig S3Config`. Existing uploaded files on local disk are not copied to S3 automatically; migrate stored objects separately if switching an existing deployment from local disk to S3.

### Verify

Build the application and run its file upload tests to confirm `POST /files/upload` and `GET /files/{fileRef}` store and return uploaded bytes. In a `neo` project, run `neo build` and `neo test --ci`. In this repository, run `nix develop --command .agents/skills/tdd/scripts/test-match "BlobStore.S3" nhcore-test-service` and, with PostgreSQL available locally, `nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict`.

## Agent prompt

```text
Update my NeoHaskell application for the parameterized FileUploadConfig API.

1. Search the project for every use of FileUploadConfig, blobStoreDir, EventModel.withFileUpload, and Application.withFileUpload. If the project does not use file uploads, report that no changes are required.

2. Update every type signature that writes bare FileUploadConfig to include its backend parameter, such as FileUploadConfig LocalBlobStoreConfig or FileUploadConfig S3Config.

3. For each FileUploadConfig record that keeps local filesystem storage:
   - Add `import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..))`, plus `import Path qualified` when converting a Text variable.
   - Replace `blobStoreDir = "./uploads"` (or a literal path) with `blobStore = LocalBlobStoreConfig { rootDir = [path|./uploads|] }` (`path` is re-exported by `Core`).
   - Replace `blobStoreDir = config.uploadDir` (or a Text variable) by binding `let uploadDir = config.uploadDir`, converting with `Path.fromText uploadDir` into `rootDir`, and setting `blobStore = LocalBlobStoreConfig { rootDir }`. Make sure the configured upload directory default is non-empty.

4. Preserve all existing stateStoreBackend, maxFileSizeBytes, pendingTtlSeconds, cleanupIntervalSeconds, allowedContentTypes, and storeOriginalFilename settings, as well as existing EventModel.withFileUpload and Application.withFileUpload registrations (`@()` partial type applications continue to work). If switching an environment to S3 only when explicitly requested by the user, use `S3Config` from `Service.FileUpload.BlobStore.S3`, mark secret credential fields with `Config.secret`, avoid calling `show` on `FileUploadConfig S3Config`, and note that existing local files are not migrated automatically.

5. Build the application and run its test suite (`neo build` and `neo test --ci`, or the project's Cabal build and Hurl/Hspec upload tests). Report every modified file, the test results, and any upload path or storage migration choice that requires human review.
```
