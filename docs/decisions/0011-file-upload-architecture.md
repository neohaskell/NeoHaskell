# ADR-0011: File Upload Architecture for Commands

## Status

Proposed

## Context

NeoHaskell needs a way for developers ("Jess" - our target persona, a time-constrained junior developer) to accept file uploads in commands without understanding multipart parsing, temporary storage, or blob management.

### Current State

- Commands receive `RequestContext` containing user info, request ID, and timestamp (ADR-0009)
- Commands have a uniform signature: `decide :: Command -> Maybe Entity -> RequestContext -> Decision Event`
- No mechanism exists for commands to access uploaded files
- Jess would need to manually handle multipart parsing, temporary storage, and cleanup

### Requirements

- **Newcomer-friendly**: Jess should be able to accept file uploads with minimal ceremony
- **Event-sourced**: File lifecycle must be trackable and auditable
- **Uniform signature**: The `decide` function signature must not change (no 4th parameter)
- **Pluggable storage**: Support local development and production cloud storage (S3, GCS)
- **Automatic cleanup**: Orphaned uploads (never referenced in a command) must be garbage collected

### The "Email Attachment" Mental Model

The design follows the familiar pattern of email attachments:

1. **Upload first**: User uploads files (like attaching to a draft email)
2. **Reference later**: Command includes references to uploaded files (like sending the email)
3. **Files travel with context**: The framework resolves references before `decide` is called

This two-phase approach is intuitive for developers familiar with modern web applications where file uploads are typically handled separately from form submissions, then linked via IDs or URLs.

### Relationship to Existing ADRs

| ADR | Relationship |
|-----|-------------|
| ADR-0003 | Establishes the `decide` signature we must preserve |
| ADR-0009 | Establishes `RequestContext` as the mechanism for request-scoped data |
| ADR-0010 | Similar pattern: external resources (tokens) accessed via framework abstractions |

## Decision

Implement file uploads as a two-phase process with files accessible via `RequestContext`, following the principle that the framework handles infrastructure while domain logic remains pure.

### 1. Type Naming

| Type | Purpose | User-Facing? |
|------|---------|--------------|
| `FileRef` | Opaque reference to an uploaded file (used in command payloads) | Yes |
| `ResolvedFile` | Hydrated file with metadata and content access (available in `decide`) | Yes |
| `BlobStore` | Storage backend interface (Local, S3, GCS) | Yes (config) |
| `FileUpload` | Internal: tracks file lifecycle in event store | No |

**Naming rationale:**

- `FileRef` over `FileId` or `UploadId`: emphasizes it's a reference to be resolved, not an entity ID
- `ResolvedFile` over `File` or `UploadedFile`: clarifies the two-phase nature (ref → resolved)
- `BlobStore` over `FileStore` or `ObjectStore`: industry-standard term for binary storage

### 2. Module Structure

**New modules:**

```text
core/service/Service/
  FileUpload/
    Core.hs           -- FileRef, ResolvedFile, FileUpload types
    BlobStore.hs      -- BlobStore interface
    BlobStore/
      Local.hs        -- Filesystem backend (development)
      S3.hs           -- AWS S3 backend (production)
      GCS.hs          -- Google Cloud Storage backend (production)
    Routes.hs         -- Upload endpoint handlers
    Lifecycle.hs      -- Event-sourced file state machine
    Cleaner.hs        -- Orphan file garbage collector
```

**Modified modules:**

- `Service/Auth.hs` - Add `files :: Map FileRef ResolvedFile` to `RequestContext`
- `Service/Transport/Web.hs` - Add `/upload` route handling
- `Service/Application.hs` - Add `withBlobStore` builder, wire cleaner

### 3. RequestContext Extension

```haskell
data RequestContext = RequestContext
  { user :: Maybe UserClaims
  , requestId :: Uuid
  , timestamp :: UTCTime
  , files :: Map FileRef ResolvedFile  -- NEW: resolved file references
  }
```

**Why extend RequestContext (not a 4th parameter)?**

1. **Uniform signature**: ALL commands keep the same `decide` signature
2. **Explicit context**: Type signature shows what's available without API changes
3. **Backwards compatible**: Existing commands ignore `files` field (like `user`)
4. **Jess-friendly**: No new concepts—just another field on the context they already know

Commands access files naturally:

```haskell
-- In decide function
decide :: UploadDocument -> Maybe DocumentEntity -> RequestContext -> Decision DocumentEvent
decide cmd entity ctx = do
  case Map.lookup cmd.attachment ctx.files of
    Nothing -> Decider.reject "Attachment not found or expired"
    Just file -> do
      -- file.name, file.contentType, file.size, file.content available
      Decider.acceptExisting [DocumentUploaded { ... }]
```

### 4. File Lifecycle (Event-Sourced)

Files follow a state machine tracked via events:

```
┌─────────┐     ┌───────────┐     ┌─────────┐
│ Pending │────▶│ Confirmed │────▶│ Deleted │
└─────────┘     └───────────┘     └─────────┘
     │                                  ▲
     └──────────────────────────────────┘
              (orphan cleanup)
```

**States:**

| State | Meaning | Transitions |
|-------|---------|-------------|
| `Pending` | Uploaded, not yet referenced in a successful command | → Confirmed, → Deleted |
| `Confirmed` | Referenced in at least one successful command | → Deleted |
| `Deleted` | Blob removed from storage, metadata retained for audit | Terminal |

**Events:**

```haskell
data FileUploadEvent
  = FileUploaded
      { fileRef :: FileRef
      , uploadedBy :: Maybe UserId
      , filename :: Text
      , contentType :: Text
      , sizeBytes :: Int
      , blobKey :: Text
      , expiresAt :: UTCTime  -- Pending expiry (e.g., now + 6 hours)
      }
  | FileConfirmed
      { fileRef :: FileRef
      , confirmedBy :: Uuid  -- Command's request ID that used it
      , confirmedAt :: UTCTime
      }
  | FileDeleted
      { fileRef :: FileRef
      , reason :: FileDeletionReason  -- Orphaned | UserRequested | AdminPurge
      , deletedAt :: UTCTime
      }
```

### 5. Two-Phase Upload Flow

**Phase 1: Upload (before command)**

```
POST /upload
Content-Type: multipart/form-data

file=@document.pdf
```

Response:

```json
{
  "fileRef": "file_abc123...",
  "filename": "document.pdf",
  "contentType": "application/pdf",
  "sizeBytes": 102400,
  "expiresAt": "2026-01-22T18:00:00Z"
}
```

**Phase 2: Reference in command**

```
POST /commands/upload-document
Content-Type: application/json

{
  "documentId": "550e8400-e29b-41d4-a716-446655440000",
  "attachment": "file_abc123..."
}
```

The framework:

1. Parses command JSON, finds `FileRef` fields
2. Resolves each `FileRef` to `ResolvedFile` (or fails if expired/not found)
3. Populates `ctx.files` map
4. Calls `decide` with hydrated context
5. On success, emits `FileConfirmed` event

### 6. BlobStore Interface

```haskell
data BlobStore = BlobStore
  { store :: BlobContent -> Task BlobStore.Error BlobKey
  , retrieve :: BlobKey -> Task BlobStore.Error BlobContent
  , delete :: BlobKey -> Task BlobStore.Error ()
  , generateSignedUrl :: BlobKey -> Duration -> Task BlobStore.Error Url  -- For direct download
  }

data BlobContent = BlobContent
  { bytes :: ByteString
  , contentType :: Text
  }
```

**Implementations:**

| Backend | Use Case | Notes |
|---------|----------|-------|
| `BlobStore.Local` | Development | Stores in configurable directory |
| `BlobStore.S3` | Production (AWS) | Uses pre-signed URLs for uploads/downloads |
| `BlobStore.GCS` | Production (GCP) | Uses signed URLs for uploads/downloads |

### 7. Automatic Orphan Cleanup

A background process runs periodically (default: every 15 minutes):

1. Query files in `Pending` state where `expiresAt < now`
2. For each orphan:
   - Delete blob from `BlobStore`
   - Emit `FileDeleted { reason = Orphaned }`

**Configuration:**

```haskell
data FileUploadConfig = FileUploadConfig
  { pendingTtl :: Duration        -- Default: 6 hours
  , cleanupInterval :: Duration   -- Default: 15 minutes
  , maxFileSize :: Int            -- Default: 10 MB
  , allowedContentTypes :: Maybe (Array Text)  -- Nothing = allow all
  }
```

### 8. Application Wiring

```haskell
Application.new
  |> Application.withEventStore postgresConfig
  |> Application.withBlobStore (BlobStore.Local.new "./uploads")
  |> Application.withFileUpload fileUploadConfig
  |> Application.withService documentService
  |> Application.run
```

For production:

```haskell
  |> Application.withBlobStore (BlobStore.S3.new s3Config)
```

### 9. ResolvedFile Type

```haskell
data ResolvedFile = ResolvedFile
  { ref :: FileRef
  , filename :: Text
  , contentType :: Text
  , sizeBytes :: Int
  , uploadedAt :: UTCTime
  , content :: Task BlobStore.Error ByteString  -- Lazy: only fetched if accessed
  , downloadUrl :: Task BlobStore.Error Url     -- Signed URL for direct access
  }
```

**Why lazy content?**

- Commands may only need metadata (filename, size) for validation
- Large files shouldn't be loaded into memory unless necessary
- Signed URLs enable direct browser downloads without proxying

## Consequences

### Positive

- **Jess writes ~5 lines** to accept file uploads: add `FileRef` field to command, use `ctx.files`
- **Uniform signature preserved**: No changes to existing commands
- **Event-sourced audit trail**: Full lifecycle visibility for compliance
- **Automatic cleanup**: No orphaned blobs accumulating in storage
- **Cloud-ready**: Same code works with local files or S3/GCS

### Negative

- **Two-phase UX**: Frontend must upload files before submitting command (minor)
- **RequestContext grows**: Another field to potentially confuse newcomers
- **Cleanup complexity**: Background process adds operational consideration
- **Storage costs**: Confirmed files persist until explicitly deleted

### Risks

| Risk | Mitigation |
|------|------------|
| Large file memory pressure | Lazy content loading, streaming support in Phase 2 |
| Orphan cleanup deletes in-flight file | 6-hour TTL provides ample buffer; extend if needed |
| FileRef collision | FileRef includes timestamp + random component |
| Blob storage failure | Retry with exponential backoff, dead-letter queue |

## Alternatives Considered

### Alternative 1: Single-Request Multipart Upload

**Approach:** Command endpoint accepts `multipart/form-data` with JSON payload + file parts.

**Rejected because:**

- Couples transport (multipart parsing) with domain logic
- Different signature for file-accepting commands vs regular commands
- Jess must learn multipart handling
- Harder to test—can't use simple JSON in tests/examples
- Breaks uniform command flow established in ADR-0003

### Alternative 2: Base64 Encoding in JSON

**Approach:** Encode file content as base64 string in JSON payload.

**Rejected because:**

- ~33% size overhead
- Large files (>1MB) cause JSON parsing performance issues
- Memory pressure: entire file loaded into command payload
- Can't support streaming or resumable uploads
- Poor developer experience: "why is my request so slow?"

### Alternative 3: External File Service

**Approach:** Separate microservice handles files; NeoHaskell commands receive URLs.

**Rejected because:**

- Breaks "the framework handles it" promise
- Jess must deploy and configure additional infrastructure
- Cross-service coordination complexity
- Eventual consistency issues between file service and event store
- Not newcomer-friendly

### Alternative 4: Fourth Parameter to `decide`

**Approach:** Change signature to `decide :: Command -> Maybe Entity -> RequestContext -> FileContext -> Decision Event`.

**Rejected because:**

- **Breaks uniform signature**: Every existing command must change
- **Confuses newcomers**: "When do I use RequestContext vs FileContext?"
- **API churn**: All command TH derivation must update
- **Unnecessary**: RequestContext is already the "request-scoped data" container
- **Violates Least Astonishment**: Why are files special compared to user identity?

## Implementation Plan

### Phase 1: Foundation (PR A, Small)

- `FileRef`, `ResolvedFile`, `FileUploadEvent` types
- `BlobStore` interface + `BlobStore.Local` implementation
- `FileUpload.Lifecycle` state machine

### Phase 2: Upload Endpoint (PR B, Medium)

- `POST /upload` route in WebTransport
- Multipart parsing (WAI)
- Event emission for `FileUploaded`

### Phase 3: RequestContext Integration (PR C, Medium)

- Extend `RequestContext` with `files` field
- FileRef detection in command JSON
- Resolution pipeline before `decide`
- `FileConfirmed` emission on success

### Phase 4: Cleanup & Cloud Backends (PR D, Medium)

- `FileUpload.Cleaner` background process
- `BlobStore.S3` implementation
- `BlobStore.GCS` implementation
- Configuration via `Application.withFileUpload`

### Phase 5: Testbed Example (PR E, Small)

- `DocumentEntity` with file attachment
- `UploadDocument` command
- Hurl tests for upload flow
- Documentation examples

## Security Checklist

- [ ] Validate file size before accepting upload
- [ ] Validate content type against allowlist (if configured)
- [ ] Scan for malware before confirming (integration point)
- [ ] Generate unpredictable FileRef values (not sequential)
- [ ] Signed URLs expire quickly (default: 15 minutes)
- [ ] BlobStore keys are not derivable from FileRef
- [ ] Rate limit upload endpoint per user
- [ ] Log file operations without logging content

## References

- [ADR-0003: Command Abstraction and Flow](0003-command-abstraction-and-flow.md) - Establishes decide signature
- [ADR-0009: JWT Authentication Middleware](0009-jwt-authentication-middleware.md) - RequestContext design
- [ADR-0010: OAuth2 Provider Integration](0010-oauth2-provider-architecture.md) - Similar two-phase external resource pattern
- [Service/Auth.hs](../../core/service/Service/Auth.hs) - RequestContext implementation
- [Testbed/Cart/Commands/AddItem.hs](../../testbed/src/Testbed/Cart/Commands/AddItem.hs) - Current command pattern
