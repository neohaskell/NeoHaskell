# Storage

Event storage, query storage, and file upload storage have separate configuration. `withEventStore` selects the accepted event history. `withQueryObjectStore` selects stored read models. `withFileUpload` selects both the blob store backend and the file lifecycle state store. A Postgres event store does not implicitly make query rows or file uploads persistent.

## Event history

`SimpleEventStore` has `basePath :: Path` and `persistent :: Bool`. With persistence enabled, it loads events from disk and writes event files. A generated starter project sets `persistent = False`, so its events disappear when that process ends.

`PostgresEventStore` configures Postgres event storage. Its fields are `host`, `databaseName`, `user`, `password`, `port`, `poolSize`, `sslMode`, `sslRootCert`, and `erasableApplicationScope`. `sslRootCert` is `Maybe Text`; use `Nothing` when no certificate path is needed. Set `erasableApplicationScope` to a stable, nonempty identifier for the application before using protected-value erasure.

If your application constructs a custom `EventStore` record, add its `eraseErasableKey` field. A store without protected-value erasure support must set `eraseErasableKey = \_ -> Task.throw (StorageFailure "Protected-value erasure is unsupported by this event store")`. Add `import Task qualified` and `import Service.EventStore (Error (StorageFailure))` if those imports are absent. The current compiler rejects a constructor that omits this field. Build the application, then call the field in a test and check that an unsupported store returns `StorageFailure`. For the one-reference request and result contract of a supporting store, see [protected-value erasure](advanced-erasure.md).

The event store supports insertion preconditions, ordered reads, and subscriptions. The command executor uses the target stream's history and insertion result. One successful append does not establish a transaction across commands, services, or application instances.

For application-owned key deletion, read [protected-value erasure](advanced-erasure.md).

## Query rows

For a model-backed application, call `EventModel.withQueryObjectStore` before the Features that declare query Slices. Each `query @Row` Slice then registers against the selected store.

For a legacy application assembled directly with `Application`, `Application.new` defaults to in-memory query storage. Call `Application.withQueryObjectStore` before `Application.withQuery @Row`, which captures the configured store factory at registration. This manual registration path remains supported and is not deprecated.

`PostgresQueryObjectStoreConfig` has the same eight field names. The store distinguishes query types through the query name and instance identifier.

Configuring Postgres event storage without `withQueryObjectStore` still leaves query rows in memory. A query result observed before restart does not prove that the row will survive restart.

## Credentials and caches

OAuth2 credentials belong in a separate `SecretStore`. The shipped in-memory implementation is for development. Applications that need durable tokens supply their own implementation through `withSecretStore`. Read [external OAuth2](advanced-oauth2.md).

A snapshot cache supports entity reconstruction. It does not replace the event history or choose the query backend.

## File uploads

`EventModel.withFileUpload` and `Application.withFileUpload` accept a `config -> FileUploadConfig blobStore` factory for any `blobStore` type with a `BlobStoreConfig` instance from `Service.FileUpload.BlobStore`. Use `@()` when the configuration is static, or register `withConfig @AppConfig` so the factory receives the loaded application configuration at startup.

`FileUploadConfig` from `Service.FileUpload.Core` has `blobStore`, `stateStoreBackend`, `maxFileSizeBytes`, `pendingTtlSeconds`, `cleanupIntervalSeconds`, `allowedContentTypes`, and `storeOriginalFilename`. Startup validates `maxFileSizeBytes`, `pendingTtlSeconds`, and `cleanupIntervalSeconds` as positive integers with `cleanupIntervalSeconds < pendingTtlSeconds` before calling `BlobStoreConfig.createBlobStore`. Backend initialization failures stop startup without falling back to local disk.

Shipped `BlobStoreConfig` implementations:

- `LocalBlobStoreConfig` from `Service.FileUpload.BlobStore.Local` has `rootDir :: Path`. Startup rejects an empty or whitespace-only directory path before creating missing folders.
- `S3Config` from `Service.FileUpload.BlobStore.S3` has `endpoint`, `bucket`, `region`, `accessKeyId`, and `secretAccessKey`. Requests use path-style URLs signed with AWS Signature Version 4. The bucket must already exist and use a valid DNS-style bucket name of 3 to 63 lowercase letters, digits, hyphens, and dots. Endpoints must include only scheme, host, and optional port. HTTPS is required unless the host is `127.0.0.1`, `[::1]`, or `localhost`. `S3Config` omits `Show` so credentials are not printed to logs.

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module AgentGuidance.FileUploadStorage where

import Core
import Service.Application (Application)
import Service.Application qualified as Application
import Service.EventModel (EventModelAction)
import Service.EventModel qualified as EventModel
import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..))
import Service.FileUpload.BlobStore.S3 (S3Config (..))
import Service.FileUpload.Core (FileStateStoreBackend (..), FileUploadConfig (..))

localUploadConfig :: FileUploadConfig LocalBlobStoreConfig
localUploadConfig =
  FileUploadConfig
    { blobStore = LocalBlobStoreConfig {rootDir = [path|./uploads|]}
    , stateStoreBackend = InMemoryStateStore
    , maxFileSizeBytes = 10485760
    , pendingTtlSeconds = 21600
    , cleanupIntervalSeconds = 900
    , allowedContentTypes = Nothing
    , storeOriginalFilename = True
    }

s3UploadConfig :: FileUploadConfig S3Config
s3UploadConfig =
  FileUploadConfig
    { blobStore =
        S3Config
          { endpoint = "https://s3.eu-central-1.amazonaws.com"
          , bucket = "app-uploads"
          , region = "eu-central-1"
          , accessKeyId = "app-access-key-id"
          , secretAccessKey = "app-secret-access-key"
          }
    , stateStoreBackend = InMemoryStateStore
    , maxFileSizeBytes = 10485760
    , pendingTtlSeconds = 21600
    , cleanupIntervalSeconds = 900
    , allowedContentTypes = Nothing
    , storeOriginalFilename = True
    }

s3UploadConfiguration :: EventModelAction
s3UploadConfiguration =
  EventModel.withFileUpload @() (\_ -> s3UploadConfig)

localUploadApplication :: Application
localUploadApplication =
  Application.new
    |> Application.withFileUpload @() (\_ -> localUploadConfig)
```
