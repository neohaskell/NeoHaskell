# NeoHaskell File Uploads (ADR-0011) — Implementation Plan

This document is the concrete, implementable plan for ADR-0011 (`docs/decisions/0011-file-upload-architecture.md`). It is written to be compatible with existing NeoHaskell conventions and patterns (RequestContext, EventStore, SnapshotCache, SecretStore, integrations/timers).

**Scope**: First iteration is **local file storage only** (development). Cloud backends (S3, GCS) are out of scope for v1.

## 1. Executive Summary

### Bottom line

Implement file uploads as a two-phase flow: `POST /files/upload` produces a short-lived `FileRef`, and later commands reference that `FileRef` in JSON. The runtime resolves `FileRef` values into `RequestContext.files` and confirms them atomically via event store optimistic concurrency, with a 6-hour orphan cleanup process.

### First iteration scope

- **In scope**: Local filesystem `BlobStore`, proxy multipart upload, full lifecycle, testbed example
- **Out of scope**: S3/GCS backends, signed direct upload URLs, production hardening

### Action plan (high level)

1. Add new `Service.FileUpload.*` modules (types, lifecycle, blob store interface, local backend, routes, cleaner).
2. Extend `Service.Auth.RequestContext` with `files :: Map FileRef ResolvedFile`.
3. Extend `Service.Transport.Web` routing to include `POST /files/upload` and `GET /files/:id`.
4. Extend command execution pipeline to (a) extract file refs, (b) resolve them into `ctx.files`, (c) confirm them idempotently after successful command append.
5. Add projection/index + cleaner job for orphan cleanup.
6. Add unit/integration tests and testbed example.

## 2. Security Architecture (EU-grade / GDPR)

This section follows the same security "shape" as ADR-0010: tamper-evident tokens, redacted logs, atomic one-time-ish semantics, and multi-instance correctness.

### 2.1 GDPR posture

- Right to deletion: provide a complete deletion path that removes blobs and writes a `FileDeleted` audit event.
- Audit trail: every file lifecycle step is an event (`FileUploaded`, `FileConfirmed`, `FileDeleted`).
- Access control: all operations scoped to `RequestContext.user` via `ownerHash` checks.
- No PII in logs: never log raw user ids, filenames (by default), blob bytes, or tokens.
- Data minimization: store only metadata necessary for access control and operations.

### 2.2 Pseudonymous identity (avoid PII in event store)

Do not store raw `UserClaims.sub` in file events. Store a stable pseudonymous hash:

- `ownerHash = HMAC-SHA256(FILEUPLOAD_USER_HASH_KEY, userSub)`
- `UserHash` type should have redacted `Show` (pattern: `Auth.SecretStore.TokenKey` in `core/auth/Auth/SecretStore.hs`).

This keeps ownership checks possible while reducing PII exposure in logs/analytics.

### 2.3 Data minimization (metadata)

Required fields for `FileUploaded`:

- `fileRef` (opaque)
- `ownerHash` (pseudonymous)
- `sizeBytes`
- `contentType`
- `blobKey` (random; not derivable from `fileRef`)
- `expiresAt` (pending TTL)
- `uploadedAt`

Optional fields (default off):

- original filename (treat as personal data)

### 2.4 Access control model

Rules:

- Upload: authenticated user required in production (anonymous allowed in dev).
- Resolve/confirm: only the owner can confirm (unless a privileged system context is explicitly introduced later).
- Delete: only owner (or admin) can delete.

Enforcement points:

- `/files/upload` route validates authentication (using existing `Auth.Middleware.checkAuth` pattern).
- FileRef resolution checks `ownerHash` against the current request's user.

### 2.5 Logging and redaction

- Never log request body.
- Never log blob bytes.
- Never log raw user identifiers.
- Avoid logging filenames; treat as PII.

Log fields to include:

- `requestId` (already in `RequestContext`)
- `fileRef`
- `ownerHash` (safe)
- `operation` (`upload|confirm|delete|cleanup`)
- `result` and a stable `errorCode`

### 2.6 Right to deletion (complete removal)

Deletion semantics:

- Deletion is idempotent.
- After appending `FileDeleted`, delete blob from primary storage.

## 3. Performance Architecture

### 3.1 First iteration: proxy streaming multipart

For the first iteration (local storage only):

- Client sends `POST /files/upload` with multipart/form-data.
- Server parses multipart and streams to local blob store.
- No signed URLs (that's for cloud backends in v2).

### 3.2 Multipart streaming library choice (WAI)

Use `wai-extra`:

- Module: `Network.Wai.Parse`
- Function: `parseRequestBodyEx`
- Enforce limits via `ParseRequestBodyOptions` (max file size, total bytes, headers, number of files).

Plan-level critical snippet (shape only):

```haskell
import Network.Wai.Parse qualified as WaiParse

let options = WaiParse.defaultParseRequestBodyOptions
      { WaiParse.prboMaxNumFiles = Just 1
      , WaiParse.prboMaxFileSize = Just (fromIntegral config.maxFileSizeBytes)
      , WaiParse.prboMaxFilesSize = Just (fromIntegral config.maxFileSizeBytes)
      , WaiParse.prboMaxHeaderLines = Just 16
      , WaiParse.prboMaxHeaderLineLength = Just 4096
      }

(params, files) <- WaiParse.parseRequestBodyEx options backEnd request
```

For proxy streaming, use:

- `tempFileBackEnd` (writes to temp file without holding bytes in memory), then move into blob store.

### 3.3 Warp settings (DoS / throughput)

- Keep existing `maxBodySize` limit in `WebTransport` for JSON commands.
- For multipart uploads, apply separate limits using `wai-extra` options.

### 3.4 Fast FileRef resolution

A naive approach (loading upload state by replaying per-file stream) is acceptable at low scale.

Plan for first iteration:

- Simple entity fetch via existing `EntityFetcher` pattern.
- Add an in-process TTL cache using `ConcurrentMap` (`core/concurrency/ConcurrentMap.hs`) for "hot" refs to avoid repeated reads.

Future optimization (v2):

- CQRS projection/index of uploads keyed by `fileRef` for O(1) lookups.

### 3.5 Single-instance for v1

First iteration assumes single instance (local development):

- Local blob store is not shared.
- Cleaner runs in-process.

Multi-instance considerations deferred to v2 (cloud backends).

## 4. Correctness Guarantees

### 4.1 Lifecycle state machine

States: `Pending -> Confirmed -> Deleted` (ADR-0011).

Events:

- `FileUploaded`
- `FileConfirmed`
- `FileDeleted`

### 4.2 Atomic FileRef consumption (no double-use)

Guarantee: at most one concurrent confirmation transition from Pending happens.

Mechanism:

- Each upload is its own event stream keyed by `fileRef`.
- Confirm attempts append `FileConfirmed` using optimistic concurrency (append-at-expected-position).
- This works across multiple instances because concurrency control is in Postgres/event store.

### 4.3 Exactly-once confirmation (idempotent)

Use `RequestContext.requestId` as idempotency key:

- `FileConfirmed` carries `confirmedByRequestId`.
- Reducer treats repeated confirmations for the same requestId as success.

### 4.4 Orphan cleanup safety (no deleting in-flight)

Constraints:

- Orphans are only deleted after TTL (default 6 hours).
- Cleaner must not delete a ref that becomes Confirmed concurrently.

Algorithm:

1. List expired Pending uploads from entity fetcher or projection.
2. For each ref:
   - attempt to append `FileDeleted { reason = Orphaned }` with expected state Pending.
   - only after that append succeeds, delete blob from blob store.
3. If append fails (already confirmed/deleted), skip.

### 4.5 Failure boundaries and compensating actions

No distributed transaction between Postgres and blob store.

Rules:

- If blob write succeeds but `FileUploaded` append fails: delete blob best-effort, return error.
- If `FileDeleted` append succeeds but blob delete fails: keep retrying deletion; emit alerts.

### 4.6 Graceful degradation

Blob store unavailable:

- `/files/upload` fails fast with 503.
- Command execution: resolution fails with 503 or 404 depending on root cause; do not silently ignore missing files.

## 5. Module-by-Module Implementation Details

### 5.1 New module layout

```text
core/service/Service/FileUpload/
  Core.hs           -- FileRef, ResolvedFile, FileUploadConfig, events
  Lifecycle.hs      -- Pure reducer, state machine
  BlobStore.hs      -- BlobStore interface
  BlobStore/
    Local.hs        -- Filesystem backend (v1)
  Routes.hs         -- Upload/download endpoint handlers
  Cleaner.hs        -- Orphan garbage collector
```

Future (v2):

```text
  BlobStore/
    S3.hs           -- AWS S3 backend
    GCS.hs          -- Google Cloud Storage backend
  Projection.hs     -- CQRS read model for fast lookups
```

### 5.2 `Service.FileUpload.Core`

Responsibilities:

- Types: `FileRef`, `ResolvedFile`, `FileUploadConfig`, `FileUploadEvent`, `FileDeletionReason`.
- JSON instances for `FileRef` and upload responses.
- Redacted `Show` instances for secrets/identifiers (pattern: `Auth.SecretStore.TokenKey`).

Types (following NeoHaskell conventions):

```haskell
-- | Opaque reference to an uploaded file (used in command payloads)
newtype FileRef = FileRef Text
  deriving (Generic, Eq, Ord)

instance Show FileRef where
  show _ = "FileRef <REDACTED>"

instance Json.FromJSON FileRef
instance Json.ToJSON FileRef


-- | Resolved file with metadata (available in RequestContext.files)
data ResolvedFile = ResolvedFile
  { ref :: FileRef
  , filename :: Text
  , contentType :: Text
  , sizeBytes :: Int64
  , uploadedAt :: UTCTime
  , blobKey :: Text  -- Internal: for blob store access
  }
  deriving (Generic)

instance Json.FromJSON ResolvedFile
instance Json.ToJSON ResolvedFile


-- | File lifecycle events
data FileUploadEvent
  = FileUploaded
      { fileRef :: FileRef
      , ownerHash :: Text
      , filename :: Text
      , contentType :: Text
      , sizeBytes :: Int64
      , blobKey :: Text
      , expiresAt :: UTCTime
      , uploadedAt :: UTCTime
      }
  | FileConfirmed
      { fileRef :: FileRef
      , confirmedByRequestId :: Uuid
      , confirmedAt :: UTCTime
      }
  | FileDeleted
      { fileRef :: FileRef
      , reason :: FileDeletionReason
      , deletedAt :: UTCTime
      }
  deriving (Generic)

instance Json.FromJSON FileUploadEvent
instance Json.ToJSON FileUploadEvent


data FileDeletionReason
  = Orphaned      -- TTL expired without confirmation
  | UserRequested -- Explicit deletion by owner
  | AdminPurge    -- System cleanup
  deriving (Generic, Eq, Show)

instance Json.FromJSON FileDeletionReason
instance Json.ToJSON FileDeletionReason


-- | Configuration for file uploads
data FileUploadConfig = FileUploadConfig
  { pendingTtl :: Duration          -- Default: 6 hours
  , cleanupInterval :: Duration     -- Default: 15 minutes
  , maxFileSizeBytes :: Int64       -- Default: 10 MB
  , allowedContentTypes :: Maybe (Array Text)  -- Nothing = all allowed
  , storeOriginalFilename :: Bool   -- Default: True (for v1 simplicity)
  }
```

### 5.3 `Service.FileUpload.BlobStore`

Define backend interface (simplified for v1 - no signed URLs):

```haskell
data BlobStore = BlobStore
  { store :: BlobKey -> Bytes -> Task Error ()
  , retrieve :: BlobKey -> Task Error Bytes
  , delete :: BlobKey -> Task Error ()
  , exists :: BlobKey -> Task Error Bool
  }

data Error
  = NotFound BlobKey
  | StorageError Text
  deriving (Eq, Show)

class BlobStoreConfig config where
  createBlobStore :: config -> Task Text BlobStore
```

### 5.4 `Service.FileUpload.BlobStore.Local`

```haskell
data LocalBlobStoreConfig = LocalBlobStoreConfig
  { rootDir :: Path
  }

instance BlobStoreConfig LocalBlobStoreConfig where
  createBlobStore config = do
    Directory.createIfMissing config.rootDir
    Task.yield BlobStore
      { store = \blobKey bytes -> do
          let path = config.rootDir |> Path.join blobKey
          -- Atomic write: write to temp, then rename
          let tempPath = path <> ".tmp"
          File.writeBytes tempPath bytes
          File.rename tempPath path

      , retrieve = \blobKey -> do
          let path = config.rootDir |> Path.join blobKey
          fileExists <- File.exists path
          if fileExists
            then File.readBytes path
            else Task.throw (NotFound blobKey)

      , delete = \blobKey -> do
          let path = config.rootDir |> Path.join blobKey
          File.deleteIfExists path

      , exists = \blobKey -> do
          let path = config.rootDir |> Path.join blobKey
          File.exists path
      }
```

### 5.5 `Service.FileUpload.Lifecycle`

Responsibilities:

- Pure reducer from events to state (`Pending|Confirmed|Deleted`).
- Validation helpers: `canConfirm`, `isExpired`, `canDelete`, etc.
- Idempotency handling: confirmation with same requestId should succeed.

```haskell
data FileUploadState
  = Pending PendingFile
  | Confirmed ConfirmedFile
  | Deleted
  deriving (Generic, Eq, Show)

data PendingFile = PendingFile
  { metadata :: ResolvedFile
  , ownerHash :: Text
  , expiresAt :: UTCTime
  }

data ConfirmedFile = ConfirmedFile
  { metadata :: ResolvedFile
  , ownerHash :: Text
  , confirmedByRequestId :: Uuid
  }

-- | Apply event to state
update :: FileUploadEvent -> FileUploadState -> FileUploadState
update event state = case event of
  FileUploaded {..} ->
    Pending PendingFile
      { metadata = ResolvedFile
          { ref = fileRef
          , filename = filename
          , contentType = contentType
          , sizeBytes = sizeBytes
          , uploadedAt = uploadedAt
          , blobKey = blobKey
          }
      , ownerHash = ownerHash
      , expiresAt = expiresAt
      }
  FileConfirmed {confirmedByRequestId} ->
    case state of
      Pending pending -> Confirmed ConfirmedFile
        { metadata = pending.metadata
        , ownerHash = pending.ownerHash
        , confirmedByRequestId = confirmedByRequestId
        }
      _ -> state  -- Idempotent
  FileDeleted {} -> Deleted

-- | Check if file can be confirmed
canConfirm :: FileUploadState -> UTCTime -> Bool
canConfirm state now = case state of
  Pending pending -> now < pending.expiresAt
  _ -> False

-- | Check if file is expired
isExpired :: FileUploadState -> UTCTime -> Bool
isExpired state now = case state of
  Pending pending -> now >= pending.expiresAt
  _ -> False
```

### 5.6 `Service.FileUpload.Routes`

Add WAI handler(s) for:

- `POST /files/upload` - Upload a file
- `GET /files/:id` - Download a file

Route behavior for upload:

1. Authenticate user (if auth enabled).
2. Parse multipart request using `wai-extra`.
3. Validate content type and size.
4. Generate `fileRef` (random UUID-based) and `blobKey`.
5. Store blob in `BlobStore`.
6. Append `FileUploaded` event.
7. Return JSON with `fileRef`, `filename`, `contentType`, `sizeBytes`, `expiresAt`.

Route behavior for download:

1. Authenticate user (if auth enabled).
2. Resolve `fileRef` to `ResolvedFile`.
3. Check ownership (ownerHash matches).
4. Retrieve blob from `BlobStore`.
5. Return with appropriate `Content-Type` header.

### 5.7 `Service.FileUpload.Cleaner`

Implementation patterns to reuse:

- periodic loop: `core/service/Integration/Timer.hs`
- background workers started by `Service.Application` via `withInbound`.

Cleaner logic:

1. Query all file upload streams for Pending state.
2. Filter to expired uploads.
3. For each expired upload:
   - Attempt to append `FileDeleted { reason = Orphaned }` with optimistic concurrency.
   - If append succeeds, delete blob from `BlobStore`.
   - If append fails (already confirmed/deleted), skip.

### 5.8 Extend `Service.Auth.RequestContext`

File: `core/service/Service/Auth.hs`

Add:

```haskell
data RequestContext = RequestContext
  { user :: Maybe UserClaims
  , files :: Array ResolvedFile  -- NEW: resolved file references
  }
```

Initialize `files` to empty array in:

- `anonymousContext`
- `authenticatedContext`

### 5.9 Extend `Service.Transport.Web` routing

File: `core/service/Service/Transport/Web.hs`

Add path cases:

```haskell
case Wai.pathInfo request of
  ["files", "upload"] -> handleFileUpload ...
  ["files", fileRefText] -> handleFileDownload fileRefText ...
  ["commands", name] -> ... existing ...
  ["queries", name] -> ... existing ...
  _ -> notFound
```

### 5.10 FileRef extraction and resolution in command execution

Where:

- Command path: transport decodes JSON into `command`, then handler executes.
- Inject FileRef resolution *after decoding* but *before decide*.

Extraction strategy:

- Extend the existing Template Haskell command derivation (`Service.CommandExecutor.TH`) to also derive `getFileRefsImpl` by inspecting record fields typed as `FileRef`, `Maybe FileRef`, `Array FileRef`.

Resolution pipeline:

1. Extract file refs from the decoded command value.
2. Resolve each ref:
   - Fetch file upload entity.
   - Check ownerHash matches request user.
   - Check not expired/deleted.
   - Check blob exists.
3. Build `ResolvedFile` values.
4. Set `ctx.files = resolvedFiles`.
5. Call `decide`.
6. If command accepted and domain events appended successfully, confirm each ref idempotently.

### 5.11 Extend `Service.Application`

Add builder function:

```haskell
withBlobStore :: (BlobStoreConfig config) => config -> Application -> Application
withFileUpload :: FileUploadConfig -> Application -> Application
```

Wire cleaner as inbound integration when file upload is configured.

## 6. Data Flow Diagrams (ASCII)

### 6.1 Proxy upload (v1)

```
Client                 NeoHaskell                   Local BlobStore
  | POST /files/upload     |                            |
  |----------------------->|                            |
  | multipart/form-data    |                            |
  |                        | parse multipart            |
  |                        | generate fileRef + blobKey |
  |                        | store blob                 |
  |                        |--------------------------->|
  |                        | append FileUploaded        |
  |<-----------------------|                            |
  | { fileRef, filename,   |                            |
  |   contentType, size,   |                            |
  |   expiresAt }          |                            |
```

### 6.2 Command using FileRef

```
Client                         NeoHaskell
  | POST /commands/x               |
  |------------------------------->|
  | { attachment: fileRef }        |
  |                                | extract fileRef(s)
  |                                | resolve -> ctx.files
  |                                | decide(cmd, entity, ctx)
  |                                | append domain events
  |                                | confirm fileRef(s)
  |<-------------------------------|
```

### 6.3 File download

```
Client                         NeoHaskell                   Local BlobStore
  | GET /files/{fileRef}           |                            |
  |------------------------------->|                            |
  |                                | resolve fileRef             |
  |                                | check ownership             |
  |                                | retrieve blob               |
  |                                |<---------------------------|
  |<-------------------------------|                            |
  | bytes + Content-Type           |                            |
```

### 6.4 Orphan cleanup

```
Cleaner                     EventStore                  Local BlobStore
  | list pending file uploads      |                            |
  |------------------------------->|                            |
  | filter expired                 |                            |
  | for each: append FileDeleted   |                            |
  |------------------------------->|                            |
  | if ok: delete blobKey          |--------------------------->|
  | if conflict: skip              |                            |
```

## 7. Configuration Reference

### 7.1 File upload config

`FileUploadConfig`:

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `pendingTtl` | Duration | 6 hours | How long before unconfirmed files are cleaned up |
| `cleanupInterval` | Duration | 15 minutes | How often the cleaner runs |
| `maxFileSizeBytes` | Int64 | 10 MB | Maximum upload size |
| `allowedContentTypes` | Maybe (Array Text) | Nothing | MIME type allowlist (Nothing = all) |
| `storeOriginalFilename` | Bool | True | Whether to store filename in events |

### 7.2 Blob store config (Local)

`LocalBlobStoreConfig`:

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `rootDir` | Path | required | Directory to store blobs |

### 7.3 Secrets

- `FILEUPLOAD_USER_HASH_KEY` (HMAC key; required if auth enabled)

## 8. Testing Strategy

### 8.1 Unit tests

- Reducer tests for `FileUpload.Lifecycle`:
  - Pending -> Confirmed
  - Pending -> Deleted (orphan)
  - Confirmed -> Deleted
  - Idempotent confirm (same requestId)
  - Reject confirm when expired/deleted

- Security tests:
  - Redacted `Show` instances
  - ownerHash mismatch rejects

### 8.2 Integration tests (Hurl)

```hurl
# tests/files/upload.hurl

# Upload a file
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,test-document.pdf;

HTTP/1.1 200
[Captures]
file_ref: jsonpath "$.fileRef"
[Asserts]
jsonpath "$.filename" == "test-document.pdf"
jsonpath "$.contentType" == "application/pdf"
jsonpath "$.expiresAt" exists
```

```hurl
# tests/files/use-in-command.hurl

# Upload file first
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,test.txt;

HTTP/1.1 200
[Captures]
file_ref: jsonpath "$.fileRef"

# Use in command
POST http://localhost:8080/commands/upload-document
{
  "title": "Test Document",
  "file": "{{file_ref}}"
}

HTTP/1.1 200
[Asserts]
jsonpath "$.events[0].type" == "DocumentCreated"
```

```hurl
# tests/files/download.hurl

# Upload then download
POST http://localhost:8080/files/upload
[MultipartFormData]
file: file,test.txt;

HTTP/1.1 200
[Captures]
file_ref: jsonpath "$.fileRef"

GET http://localhost:8080/files/{{file_ref}}

HTTP/1.1 200
[Asserts]
header "Content-Type" contains "text/plain"
```

### 8.3 Local blob store tests

- Store + retrieve roundtrip
- Delete + exists check
- Concurrent writes to same key
- Large file handling

## 9. Rollout Plan

### 9.1 Phased implementation

**Phase 1: Types & BlobStore** (PR A, Small)
- `Service.FileUpload.Core` - Types and events
- `Service.FileUpload.BlobStore` - Interface
- `Service.FileUpload.BlobStore.Local` - Filesystem backend
- `Service.FileUpload.Lifecycle` - State machine
- Unit tests

**Phase 2: Upload Endpoint** (PR B, Medium)
- `Service.FileUpload.Routes` - Upload handler
- Extend `Service.Transport.Web` routing
- Multipart parsing
- Integration tests for upload

**Phase 3: RequestContext & Commands** (PR C, Medium)
- Extend `RequestContext` with `files`
- FileRef extraction in command execution
- FileRef resolution pipeline
- Automatic confirmation on command success
- Integration tests for commands with files

**Phase 4: Download & Cleanup** (PR D, Medium)
- Download endpoint
- `Service.FileUpload.Cleaner` - Orphan garbage collector
- Extend `Service.Application` with `withBlobStore`, `withFileUpload`
- End-to-end integration tests

**Phase 5: Testbed Example** (PR E, Small)
- `DocumentEntity` with file attachment
- `UploadDocument` command
- Hurl tests demonstrating full flow

### 9.2 Compatibility

- Extending `RequestContext` is backwards compatible.
- Commands without `FileRef` are unaffected.
- No database migrations required (files are event-sourced).

## 10. Monitoring and Alerting

### 10.1 Metrics (v1 - basic)

- `uploads_created_total`
- `uploads_confirmed_total`
- `uploads_deleted_total{reason}`
- `uploads_rejected_total{errorCode}`
- `blobstore_errors_total{op}`

### 10.2 Logs

- Structured logs with `requestId`, `fileRef`, `ownerHash`
- Never log: blob bytes, raw user ids, filenames (configurable)

### 10.3 Alerts (v1 - basic)

- Elevated `blobstore_errors_total`
- Orphan backlog growth (expired pending count)

---

## Future Work (v2)

Out of scope for first iteration:

- **Cloud backends**: S3, GCS with signed direct upload URLs
- **Signed URLs**: Client uploads directly to cloud storage
- **CQRS projection**: Fast O(1) FileRef resolution
- **Multi-instance cleaner**: Postgres advisory lock or leader election
- **Streaming content access**: Lazy `Task` for large file content
- **Resumable uploads**: For very large files
- **Virus scanning integration**: Scan before confirmation

---

## Effort Estimate

**First iteration (local storage only):**

| Phase | Effort | Dependencies |
|-------|--------|--------------|
| Phase 1: Types & BlobStore | 2-3 hours | None |
| Phase 2: Upload Endpoint | 3-4 hours | Phase 1 |
| Phase 3: RequestContext & Commands | 4-6 hours | Phase 2 |
| Phase 4: Download & Cleanup | 3-4 hours | Phase 3 |
| Phase 5: Testbed Example | 1-2 hours | Phase 4 |

#### Total: ~15-20 hours (2-3 days)
