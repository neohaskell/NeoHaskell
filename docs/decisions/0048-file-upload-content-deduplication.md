# ADR-0048: File Upload Content Deduplication via SHA-256

## Status

Proposed

## Context

### Current State

NeoHaskell's file upload system (ADR-0011, ADR-0012) implements a two-phase upload flow: files are uploaded first via `POST /files/upload`, then referenced in commands via `FileRef`. The framework stores blobs in a `BlobStore`, tracks lifecycle state in a `FileStateStore`, and emits `FileUploaded` events for each upload.

Today, **every upload creates a new blob, a new `FileRef`, and a new `FileUploaded` event** — even when the same user uploads the exact same file content multiple times. This causes three concrete problems:

1. **Wasted storage**: Duplicate blobs accumulate in the `BlobStore`. A user who uploads the same PDF 5 times creates 5 identical blobs.

2. **Duplicate events**: Each upload emits a `FileUploaded` event, triggering downstream integrations (e.g., PDF transcription, virus scanning) for content that has already been processed. This wastes compute and can cause confusing duplicate results.

3. **Confusing UX**: The user receives a different `FileRef` each time, even though the content is identical. If they accidentally upload the same file twice, they now have two references to manage.

The existing codebase already uses SHA-256 hashing in the OAuth2 module (`Auth.OAuth2.TransactionStore.TransactionKey`) via the `crypton` package, so the cryptographic primitive is available and proven.

### Use Cases

- **Accidental re-upload**: Jess uploads `invoice.pdf` to her app, realizes she forgot to fill in a form field, and re-uploads the same file. Today: two blobs, two events, two integration runs. After dedup: the framework returns the existing `FileRef` with HTTP 200 — no new blob, no new event, no wasted processing.

- **Batch import with retries**: Jess's app processes a batch of documents. If the batch fails partway through and retries, previously-uploaded files are deduplicated automatically. The retry is idempotent.

- **Re-upload after deletion**: Jess deletes a file and later uploads the same content again. This is treated as a new upload (new intent), not a dedup match. The framework respects the deletion boundary.

- **Multi-user isolation**: Two different users upload the same file. Each gets their own `FileRef` — dedup is scoped per-owner, preserving privacy and ownership boundaries.

### Design Goals

1. **Transparent to Jess**: Deduplication happens inside the framework. Jess's upload code doesn't change. She calls the same endpoint, gets the same response shape. The only observable difference is that duplicate uploads are faster and return HTTP 200 instead of 201.

2. **Per-owner scoping**: Dedup is keyed on `(OwnerHash, ContentHash)`, not just `ContentHash`. Two different users uploading the same file each get their own `FileRef`. This preserves ownership isolation and avoids cross-user information leakage.

3. **Constant-memory hashing**: SHA-256 is computed over the file bytes in a single pass. The file content is already fully in memory (as `Bytes`) at the point where hashing occurs, so no streaming infrastructure is needed — but the hash computation itself uses constant auxiliary memory.

4. **Race-condition safe**: A database unique constraint on `(owner_hash, content_hash)` handles concurrent uploads of the same content by the same user. The first insert wins; the second gets a constraint violation and falls back to returning the existing `FileRef`.

5. **Minimal code change**: ~75 lines across 4 existing files. No new modules, no new dependencies, no new configuration options.

### GitHub Issue

- [#588: File Upload: Content-addressable deduplication via SHA-256](https://github.com/neohaskell/NeoHaskell/issues/588)

## Decision

### 1. Content Hash Type: `ContentHash` Newtype

Add a `ContentHash` newtype to `Service.FileUpload.Core`, following the same pattern as the existing `OwnerHash` newtype.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Raw `Text` field | Rejected | No type safety — could accidentally pass an `OwnerHash` where a `ContentHash` is expected. Violates the newtype-everything pattern established by `FileRef`, `BlobKey`, `OwnerHash`. |
| `Hash` (generic name) | Rejected | Ambiguous — hash of what? Doesn't communicate that this is a content-addressable hash. |
| `FileHash` | Rejected | Could be confused with a hash of the filename or file metadata. |
| `ContentHash` | **Chosen** | Self-documenting: "hash of the content." Follows the `XxxHash` naming pattern of `OwnerHash`. Familiar to anyone who has used content-addressable storage (Git, Docker, IPFS). |

```haskell
-- | SHA-256 hash of file content for deduplication.
-- Show instance is redacted to prevent leaking content fingerprints in logs.
newtype ContentHash = ContentHash Text
  deriving (Generic, Eq, Ord)

instance Show ContentHash where
  show _ = "ContentHash <REDACTED>"

instance Json.FromJSON ContentHash
instance Json.ToJSON ContentHash
```

**Module placement**: `core/service/Service/FileUpload/Core.hs` — alongside `OwnerHash`, `FileRef`, `BlobKey`.

**Why redacted Show?** Content hashes are fingerprints. While not secret per se, they could be used to confirm whether a specific file exists in the system. Redacting them in logs follows the same security posture as `OwnerHash` and `FileRef`.

### 2. Hash Computation: SHA-256 via `crypton`

The content hash is computed using the same `Crypto.Hash` module already used in `Auth.OAuth2.TransactionStore.TransactionKey`. The implementation follows the identical pattern: hash the bytes, base64url-encode the digest.

```haskell
-- In Service.FileUpload.Web (or a helper used by Web.hs)
computeContentHash :: Bytes -> ContentHash
computeContentHash content = do
  let contentBytes = Bytes.unwrap content
  let hashResult = Hash.hash contentBytes :: Hash.Digest Hash.SHA256
  let hashBytes = BA.convert hashResult :: BS.ByteString
  let encoded = Encoding.convertToBase Encoding.Base64URLUnpadded hashBytes
  let hashText = Bytes.fromLegacy encoded |> Text.fromBytes
  ContentHash hashText
{-# INLINE computeContentHash #-}
```

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| MD5 | Rejected | Cryptographically broken. Collision attacks are practical. Not suitable for content-addressable storage. |
| SHA-1 | Rejected | Collision attacks demonstrated (SHAttered, 2017). Git is migrating away from SHA-1 for this reason. |
| SHA-256 | **Chosen** | Industry standard for content-addressable storage. Used by Docker, IPFS, and many dedup systems. Already available in nhcore via `crypton`. No known practical collision attacks. |
| BLAKE3 | Rejected | Faster than SHA-256 but requires adding a new dependency (`blake3`). SHA-256 is fast enough for file upload sizes (sub-millisecond for 10 MB). |

### 3. Event Data Extension: `contentHash` Field on `FileUploadedData`

Add a `contentHash` field to the `FileUploadedData` record so that the content hash is persisted in the event stream. This enables future features like content-based search and audit trails.

```haskell
data FileUploadedData = FileUploadedData
  { fileRef :: FileRef
  , ownerHash :: OwnerHash
  , contentHash :: ContentHash  -- NEW
  , filename :: Text
  , contentType :: Text
  , sizeBytes :: Int64
  , blobKey :: BlobKey
  , expiresAt :: Int64
  , uploadedAt :: Int64
  }
  deriving (Generic, Eq, Show)
```

**JSON compatibility**: Adding a new field to a record with generic `FromJSON`/`ToJSON` instances is backward-compatible for serialization (new field is present in new events) but requires care for deserialization of old events (the field won't exist). Since `FileUploadedData` uses generic `FromJSON`, old events without `contentHash` will fail to deserialize. This is acceptable because:

1. File upload events are consumed by the `FileStateStore` state machine, not replayed from an event store.
2. The `FileStateStore` (both in-memory and Postgres) stores materialized state, not raw events.
3. If backward compatibility with old serialized events is needed, a `Maybe ContentHash` field or a custom `FromJSON` instance with a default can be used instead.

### 4. Lifecycle State Extension: `contentHash` in `FileMetadata`

Add `contentHash` to the `FileMetadata` record in `Lifecycle.hs` so that the content hash is available in the materialized state for dedup lookups.

```haskell
data FileMetadata = FileMetadata
  { ref :: FileRef
  , filename :: Text
  , contentType :: Text
  , sizeBytes :: Int64
  , blobKey :: BlobKey
  , uploadedAt :: Int64
  , contentHash :: ContentHash  -- NEW
  }
  deriving (Generic, Eq, Show)
```

The `update` function in `Lifecycle.hs` already constructs `FileMetadata` from `FileUploadedData`, so it will naturally pick up the new field.

### 5. FileStateStore Interface Extension: `findByContentHash`

Add a `findByContentHash` method to the `FileStateStore` record in `Web.hs`:

```haskell
data FileStateStore = FileStateStore
  { getState :: FileRef -> Task Text (Maybe FileUploadState)
  , setState :: FileRef -> FileUploadState -> Task Text ()
  , updateState :: FileRef -> FileUploadEvent -> Task Text ()
  , findByContentHash :: OwnerHash -> ContentHash -> Task Text (Maybe FileRef)  -- NEW
  }
```

**Why `OwnerHash -> ContentHash -> Task Text (Maybe FileRef)` and not just `ContentHash -> ...`?**

Dedup is scoped per-owner. The lookup key is `(OwnerHash, ContentHash)`. This ensures:
- User A's files are never returned for User B's uploads
- No cross-user information leakage ("does this file exist in the system?")
- The database index is efficient (composite key)

**Return type is `Maybe FileRef`**, not `Maybe FileUploadState`. The caller only needs to know "does a non-deleted match exist, and if so, what's its `FileRef`?" The full state is not needed for the dedup decision.

### 6. Dedup Logic in `handleUploadImpl`

The dedup check is inserted into `handleUploadImpl` in `Web.hs`, **after** validation but **before** blob storage:

```haskell
handleUploadImpl config blobStore stateStore ownerHash filename contentType content = do
  -- 1. Validate (existing: size, content type)
  -- ... existing validation code ...

  -- 2. Compute content hash (NEW)
  let contentHash = computeContentHash content

  -- 3. Check for existing upload with same content (NEW)
  let ownerHashValue = OwnerHash ownerHash
  existingRef <- stateStore.findByContentHash ownerHashValue contentHash

  case existingRef of
    Just fileRef -> do
      -- Duplicate detected — return existing FileRef without storing blob or emitting event
      Log.info "Duplicate upload detected, returning existing FileRef"
        |> Task.ignoreError
      existingState <- stateStore.getState fileRef
      case existingState of
        Just (Pending pendingFile) -> do
          -- Return response matching the existing upload
          Task.yield UploadResponse
            { fileRef = fileRef
            , blobKey = pendingFile.metadata.blobKey
            , filename = pendingFile.metadata.filename
            , contentType = pendingFile.metadata.contentType
            , sizeBytes = pendingFile.metadata.sizeBytes
            , expiresAt = DateTime.fromEpochSeconds pendingFile.expiresAt
            }
        Just (Confirmed confirmedFile) -> do
          -- Return response matching the existing upload
          now <- DateTime.now |> Task.mapError (\_ -> "Failed to get time")
          Task.yield UploadResponse
            { fileRef = fileRef
            , blobKey = confirmedFile.metadata.blobKey
            , filename = confirmedFile.metadata.filename
            , contentType = confirmedFile.metadata.contentType
            , sizeBytes = confirmedFile.metadata.sizeBytes
            , expiresAt = now  -- Already confirmed, no meaningful expiry
            }
        _ -> do
          -- Deleted or Initial — treat as no match, fall through to normal flow
          normalUploadFlow config blobStore stateStore ownerHash filename contentType content contentHash

    Nothing -> do
      -- No duplicate — normal upload flow
      normalUploadFlow config blobStore stateStore ownerHash filename contentType content contentHash
```

**Dedup decision matrix:**

| Existing State | Action | Rationale |
|---------------|--------|-----------|
| `Pending` | Return existing `FileRef` (HTTP 200) | Same content, same owner, still active — reuse it |
| `Confirmed` | Return existing `FileRef` (HTTP 200) | Same content, same owner, already confirmed — reuse it |
| `Deleted` | Create new upload (normal flow) | User deleted the file — re-upload is a new intent |
| `Initial` | Create new upload (normal flow) | Should not occur in practice; treat as no match |
| No match | Create new upload (normal flow) | First upload of this content by this owner |

### 7. Postgres Implementation: `findByContentHash` + Schema Changes

#### New Column and Index

Add `content_hash` column to the `file_upload_state` table and a unique constraint on `(owner_hash, content_hash)`:

```sql
-- Add column (nullable for backward compatibility with existing rows)
ALTER TABLE file_upload_state
  ADD COLUMN IF NOT EXISTS content_hash VARCHAR(255);

-- Unique constraint for dedup (only for non-deleted files)
CREATE UNIQUE INDEX IF NOT EXISTS idx_file_upload_state_content_dedup
  ON file_upload_state(owner_hash, content_hash)
  WHERE status IN ('pending', 'confirmed')
    AND content_hash IS NOT NULL;
```

**Why a partial unique index instead of a full unique constraint?**

- Deleted files should not block re-uploads of the same content
- Old rows without `content_hash` (NULL) should not conflict
- The `WHERE status IN ('pending', 'confirmed')` clause ensures only active files participate in dedup

#### Query Implementation

```haskell
findByContentHashImpl :: HasqlPool.Pool -> OwnerHash -> ContentHash -> Task Text (Maybe FileRef)
findByContentHashImpl pool (OwnerHash ownerHashText) (ContentHash contentHashText) = do
  let sql =
        "SELECT file_ref FROM file_upload_state \
        \WHERE owner_hash = $1 AND content_hash = $2 \
        \AND status IN ('pending', 'confirmed') \
        \LIMIT 1"
  -- ... encoder/decoder/statement ...
```

### 8. In-Memory Implementation: `findByContentHash`

For the in-memory `FileStateStore` (used in development/testing), `findByContentHash` iterates the `ConcurrentMap` and checks for a matching `(ownerHash, contentHash)` pair in non-deleted states:

```haskell
findByContentHash = \ownerHash contentHash -> do
  -- Iterate entries, find first match with matching owner + content hash
  -- in Pending or Confirmed state
  ConcurrentMap.findFirst
    (\fileRef state -> case state of
      Pending pending ->
        if pending.ownerHash == ownerHash
          then pending.metadata.contentHash == contentHash
          else False
      Confirmed confirmed ->
        if confirmed.ownerHash == ownerHash
          then confirmed.metadata.contentHash == contentHash
          else False
      _ -> False
    )
    stateMap
  |> Task.mapError (\_ -> "Failed to search state map")
```

### 9. Public API Summary

No new public API surfaces for Jess. The deduplication is entirely internal to the framework. The only observable changes are:

| What | Before | After |
|------|--------|-------|
| Duplicate upload response | HTTP 201 (new FileRef) | HTTP 200 (existing FileRef) |
| Duplicate upload blob storage | New blob stored | No blob stored |
| Duplicate upload event | `FileUploaded` emitted | No event emitted |
| `FileUploadedData` record | No `contentHash` field | Has `contentHash` field |
| `FileMetadata` (Lifecycle) | No `contentHash` field | Has `contentHash` field |
| `FileStateStore` record | 3 methods | 4 methods (`findByContentHash` added) |
| Postgres schema | No `content_hash` column | `content_hash` column + partial unique index |

### 10. Type Definitions Summary

```haskell
-- Core.hs — NEW type
newtype ContentHash = ContentHash Text
  deriving (Generic, Eq, Ord)

-- Core.hs — MODIFIED record (one new field)
data FileUploadedData = FileUploadedData
  { fileRef :: FileRef
  , ownerHash :: OwnerHash
  , contentHash :: ContentHash  -- NEW
  , filename :: Text
  , contentType :: Text
  , sizeBytes :: Int64
  , blobKey :: BlobKey
  , expiresAt :: Int64
  , uploadedAt :: Int64
  }
  deriving (Generic, Eq, Show)

-- Lifecycle.hs — MODIFIED record (one new field)
data FileMetadata = FileMetadata
  { ref :: FileRef
  , filename :: Text
  , contentType :: Text
  , sizeBytes :: Int64
  , blobKey :: BlobKey
  , uploadedAt :: Int64
  , contentHash :: ContentHash  -- NEW
  }
  deriving (Generic, Eq, Show)

-- Web.hs — MODIFIED record (one new method)
data FileStateStore = FileStateStore
  { getState :: FileRef -> Task Text (Maybe FileUploadState)
  , setState :: FileRef -> FileUploadState -> Task Text ()
  , updateState :: FileRef -> FileUploadEvent -> Task Text ()
  , findByContentHash :: OwnerHash -> ContentHash -> Task Text (Maybe FileRef)  -- NEW
  }

-- Web.hs — NEW function
computeContentHash :: Bytes -> ContentHash
```

## Consequences

### Positive

- **Zero effort for Jess**: Deduplication is invisible. Jess's upload code doesn't change. She gets faster responses for duplicate uploads and avoids wasted integration processing — without writing a single line of dedup logic.

- **Storage savings**: Duplicate blobs are never stored. For apps with frequent re-uploads (document management, batch processing), this can significantly reduce storage costs.

- **Idempotent uploads**: Uploading the same file twice produces the same `FileRef`. This makes retry logic trivial — if a network error occurs during upload, Jess can safely retry without worrying about duplicates.

- **No wasted integration processing**: Duplicate uploads don't emit `FileUploaded` events, so downstream integrations (PDF transcription, virus scanning, thumbnail generation) are not triggered for content that has already been processed.

- **Race-condition safe**: The Postgres partial unique index on `(owner_hash, content_hash)` handles concurrent uploads atomically. The first insert wins; concurrent duplicates get a constraint violation and fall back to returning the existing `FileRef`.

- **Privacy preserved**: Per-owner scoping ensures that dedup never leaks information across users. User A cannot discover that User B has uploaded the same file.

### Negative

- **Schema migration required**: Existing Postgres deployments need an `ALTER TABLE` to add the `content_hash` column. This is a non-breaking change (nullable column), but requires a migration step.

- **Slight upload latency increase**: Computing SHA-256 adds ~0.5ms for a 10 MB file. The database lookup for `findByContentHash` adds one additional query per upload. Both are negligible compared to blob storage I/O.

- **`FileUploadedData` breaking change**: Adding `contentHash` to the record breaks deserialization of old `FileUploadedData` values if they were serialized with generic `FromJSON`. This is mitigated by the fact that file upload events are consumed by the `FileStateStore` state machine (materialized state), not replayed from raw event streams.

- **In-memory dedup is O(n)**: The in-memory `findByContentHash` implementation scans all entries. This is acceptable for development/testing but would not scale for production (which uses Postgres with an indexed lookup).

### Risks

1. **Hash collision**: Two different files produce the same SHA-256 hash, causing incorrect dedup. The probability is astronomically low (~1 in 2^128 for a collision attack, effectively impossible for random files). SHA-256 has no known practical collision attacks.

2. **Stale dedup match**: A file is in `Pending` state and about to expire. A duplicate upload matches it and returns the existing `FileRef`. The cleanup worker then deletes the pending file, leaving the user with a dead reference. This is mitigated by the existing TTL mechanism — the user would have had the same problem with a non-dedup upload if they waited too long.

3. **Content-type mismatch**: Same content uploaded with different content types (e.g., `application/octet-stream` vs `application/pdf`). The dedup returns the original `FileRef` with the original content type. This is correct behavior — the content is identical, and the original metadata is preserved.

### Mitigations

1. **Hash collision**: SHA-256 is the industry standard for content-addressable storage. The risk is theoretical, not practical. If a stronger guarantee is ever needed, the `ContentHash` newtype can be extended to include file size as an additional discriminator.

2. **Stale dedup match**: The dedup check verifies the state is `Pending` or `Confirmed` before returning. If the file is about to expire, the user gets a valid `FileRef` that will expire on schedule — same as any other pending upload. The user can re-upload after expiry.

3. **Content-type mismatch**: Document that dedup is content-based, not metadata-based. If Jess needs different metadata for the same content, she should use different owner contexts or delete the original first.

## References

- [#588: File Upload: Content-addressable deduplication via SHA-256](https://github.com/neohaskell/NeoHaskell/issues/588)
- [ADR-0011: File Upload Architecture](0011-file-upload-architecture.md) — original file upload design
- [ADR-0012: PostgreSQL FileStateStore](0012-postgres-file-state-store.md) — Postgres state store implementation
- [core/service/Service/FileUpload/Core.hs](../../core/service/Service/FileUpload/Core.hs) — `OwnerHash`, `FileRef`, `FileUploadedData` types
- [core/service/Service/FileUpload/Web.hs](../../core/service/Service/FileUpload/Web.hs) — `handleUploadImpl`, `FileStateStore` interface
- [core/service/Service/FileUpload/Lifecycle.hs](../../core/service/Service/FileUpload/Lifecycle.hs) — `FileMetadata`, state machine
- [core/service/Service/FileUpload/FileStateStore/Postgres.hs](../../core/service/Service/FileUpload/FileStateStore/Postgres.hs) — Postgres implementation
- [core/auth/Auth/OAuth2/TransactionStore/TransactionKey.hs](../../core/auth/Auth/OAuth2/TransactionStore/TransactionKey.hs) — existing SHA-256 pattern in nhcore
