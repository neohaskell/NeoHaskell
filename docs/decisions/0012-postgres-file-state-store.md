# ADR-0012: PostgreSQL FileStateStore

## Status

Accepted

## Context

ADR-0011 introduced file uploads for commands with a two-phase design: files are uploaded first, then referenced in commands. The `FileStateStore` tracks file lifecycle state (Pending → Confirmed → Deleted) to:

1. Prevent duplicate uploads from creating orphan blobs
2. Enable garbage collection of expired pending files
3. Track which files have been confirmed by successful commands
4. Provide audit trails for compliance (GDPR right-to-delete)

### The Problem

The current implementation uses `InMemoryFileStateStore` backed by `ConcurrentMap`. This works well for development and testing, but has critical production issues:

**Issue 1: State Loss on Restart**

When the server restarts (deploy, crash, scaling), all file state is lost:

```text
Before restart:
  file_abc123 → Pending (blob exists in S3)
  file_def456 → Confirmed (blob exists in S3)

After restart:
  [empty state]

Result:
  - Confirmed files become inaccessible (FileNotFound)
  - Pending files can never be confirmed
  - Orphaned blobs accumulate in storage (cost + compliance risk)
```

**Issue 2: No Horizontal Scaling**

In-memory state cannot be shared across multiple server instances. Each instance has its own state, leading to inconsistent behavior:

```text
Instance A: Receives upload → file_abc123 = Pending
Instance B: Receives command referencing file_abc123 → FileNotFound!
```

**Issue 3: GDPR Compliance**

Without persistent file tracking, we cannot:

- Enumerate all files associated with a user (for data export requests)
- Reliably delete all files for a user (right to be forgotten)
- Prove files were deleted (audit requirement)

### Requirements

The solution must:

1. **Persist across restarts**: File state survives server restart/deployment
2. **Scale horizontally**: Multiple instances share the same state
3. **Reuse existing infrastructure**: Leverage the same PostgreSQL database used by `EventStore.Postgres`
4. **Implement existing interface**: Drop-in replacement for `InMemoryFileStateStore`
5. **Support cleanup queries**: Find expired pending files efficiently
6. **Enable compliance queries**: List files by owner, find files to delete

### Relationship to Existing ADRs

| ADR | Relationship |
|-----|-------------|
| ADR-0004 | Provides the PostgreSQL patterns we follow (connection pooling, trait pattern) |
| ADR-0011 | Defines the `FileStateStore` interface this implements |
| ADR-0006 | Similar pattern: persistent backend for previously in-memory state |

## Decision

Implement a PostgreSQL-backed `FileStateStore` using the same database and connection pool as `EventStore.Postgres`.

### 1. Module Structure

```text
core/service/Service/
  FileUpload/
    Web.hs              -- Existing: FileStateStore trait definition
    FileStateStore/     -- NEW: implementations directory
      Postgres.hs       -- PostgreSQL implementation
```

This follows the established pattern from `EventStore/` and `SnapshotCache/`: a flat structure with one level of nesting for implementation variants.

### 2. Database Schema

A single table stores file state, with nullable columns for state-dependent data:

```sql
CREATE TABLE IF NOT EXISTS file_upload_state (
    -- Primary identifier
    file_ref            VARCHAR(255) PRIMARY KEY,
    
    -- Lifecycle state: 'initial', 'pending', 'confirmed', 'deleted'
    status              VARCHAR(20) NOT NULL,
    
    -- Storage reference (NULL for initial/deleted states)
    blob_key            VARCHAR(500),
    
    -- Ownership (NULL for initial/deleted states)
    owner_hash          VARCHAR(255),
    
    -- File metadata (NULL for initial/deleted states)
    filename            VARCHAR(1000),
    content_type        VARCHAR(255),
    size_bytes          BIGINT,
    
    -- Timestamps (Unix epoch seconds for consistency with nhcore DateTime)
    uploaded_at         BIGINT,       -- When the file was uploaded
    expires_at          BIGINT,       -- For pending files, when they expire
    
    -- For confirmed files: which request confirmed it
    confirmed_by_request_id VARCHAR(255),
    
    -- Audit timestamps
    created_at          TIMESTAMPTZ DEFAULT NOW(),
    updated_at          TIMESTAMPTZ DEFAULT NOW()
);

-- Index for status-based queries
CREATE INDEX IF NOT EXISTS idx_file_upload_state_status 
    ON file_upload_state (status);

-- Index for cleanup: find expired pending files
CREATE INDEX IF NOT EXISTS idx_file_upload_state_expires 
    ON file_upload_state (expires_at) 
    WHERE status = 'pending';

-- Index for ownership queries: find files by owner
CREATE INDEX IF NOT EXISTS idx_file_upload_state_owner 
    ON file_upload_state (owner_hash);
```

**Design rationale:**

- **`file_ref` as primary key**: Direct lookup by file reference (the common case)
- **`status` as VARCHAR**: More readable in queries than enum/int; supports 'initial', 'pending', 'confirmed', 'deleted'
- **Nullable metadata columns**: State-dependent data (only populated for pending/confirmed states)
- **Unix epoch timestamps**: Consistent with NeoHaskell's `DateTime.toEpochSeconds` pattern
- **Partial indexes**: Cleanup query only scans pending files via partial index on `expires_at`
- **VARCHAR lengths**: Reasonable limits that match validation at the application layer

### 3. PostgreSQL FileStateStore Implementation

```haskell
-- Service/FileUpload/FileStateStore/Postgres.hs
module Service.FileUpload.FileStateStore.Postgres (
  new,
  newWithCleanup,
  Pool,
  PostgresFileStoreError (..),
  findExpiredPendingFiles,
  deleteFileState,
) where

import Core
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.Web (FileStateStore (..))
import Hasql.Pool (Pool)

-- | Create a PostgreSQL-backed FileStateStore.
-- Initializes the table schema on first use.
new :: PostgresEventStore -> Task Text FileStateStore

-- | Create with access to the connection pool for cleanup operations.
-- Use this when you need the pool for findExpiredPendingFiles/deleteFileState.
newWithCleanup :: PostgresEventStore -> Task Text (FileStateStore, Pool)

-- | Find expired pending files for cleanup (requires pool from newWithCleanup).
findExpiredPendingFiles :: Pool -> Int64 -> Task Text [(FileRef, BlobKey)]

-- | Delete a file state record (requires pool from newWithCleanup).
deleteFileState :: Pool -> FileRef -> Task Text ()
```

**Key implementation details:**

1. **Reuses EventStore config**: Accepts `PostgresEventStore` config for consistent database settings
2. **Connection pool management**: Creates a pool internally; exposes it via `newWithCleanup` for cleanup workers
3. **Idempotent schema**: Table and indexes use `IF NOT EXISTS` for safe repeated initialization
4. **Atomic updates**: Uses `BEGIN/COMMIT` transactions with `SELECT FOR UPDATE` for race-free state updates
5. **Upsert semantics**: Uses PostgreSQL's `INSERT ... ON CONFLICT DO UPDATE` for set operations
6. **State mapping**: Serializes `FileUploadState` ADT to/from database row representation

### 4. Interface Compatibility

The PostgreSQL implementation provides the same `FileStateStore` interface:

```haskell
data FileStateStore = FileStateStore
  { getState :: FileRef -> Task Text (Maybe FileUploadState)
  , setState :: FileRef -> FileUploadState -> Task Text ()
  , updateState :: FileRef -> FileUploadEvent -> Task Text ()
  }
```

**`getState`**: Single-row SELECT by `file_ref`, maps row to `FileUploadState`

**`setState`**: UPSERT (INSERT or UPDATE) the full state

**`updateState`**: Atomic read-modify-write using transactions:
- Runs in a `BEGIN/COMMIT` block with `SELECT FOR UPDATE` to lock the row
- Reads current state (or uses `initialState` if not found)
- Applies the event via `Lifecycle.update` to compute new state
- Upserts the new state

This ensures concurrent updates to the same file reference are serialized, matching the atomicity guarantee of the in-memory `ConcurrentMap.update` implementation.

### 5. Cleanup Query Enhancement

The PostgreSQL implementation provides efficient cleanup through direct SQL queries:

```sql
-- Find expired pending files for cleanup
SELECT file_ref, blob_key 
FROM file_upload_state 
WHERE status = 'pending' 
  AND expires_at < $1  -- current epoch time
  AND blob_key IS NOT NULL;
```

The cleanup functions are exported separately (not part of `FileStateStore` interface):

```haskell
-- Get the store and pool together
(stateStore, pool) <- PostgresFileStore.newWithCleanup dbConfig

-- In cleanup worker (runs periodically)
expired <- PostgresFileStore.findExpiredPendingFiles pool currentTime
for_ expired \(fileRef, blobKey) -> do
  blobStore.delete blobKey
  PostgresFileStore.deleteFileState pool fileRef
```

**Important**: Always use `newWithCleanup` and reuse the returned pool. Do NOT create pools per-call, as that causes connection leaks.

### 6. Application Wiring

```haskell
-- Current (in-memory)
stateStoreMap <- FileUpload.newInMemoryFileStateStore
let stateStore = FileUpload.inMemoryFileStateStore stateStoreMap
let setup = FileUpload.defaultFileUploadSetup blobStore stateStore

-- New (PostgreSQL) - basic usage
import Service.FileUpload.FileStateStore.Postgres qualified as PostgresFileStore

stateStore <- PostgresFileStore.new postgresConfig  -- Same config as EventStore
let setup = FileUpload.defaultFileUploadSetup blobStore stateStore

-- New (PostgreSQL) - with cleanup support
(stateStore, pool) <- PostgresFileStore.newWithCleanup postgresConfig
let setup = FileUpload.defaultFileUploadSetup blobStore stateStore
-- Pass `pool` to your cleanup worker
```

The change is a single-line swap: replace `inMemoryFileStateStore stateStoreMap` with `PostgresFileStore.new config`.

### 7. Migration Strategy

For existing deployments moving from in-memory to PostgreSQL:

**Scenario A: Fresh deployment**
- No migration needed; table is created on first startup
- Files uploaded before the switch are lost (acceptable for dev/staging)

**Scenario B: Production with existing files**

1. **Pre-migration**: Deploy with both stores active (write to both, read from in-memory)
2. **Migration window**: Short downtime to:
   - Stop accepting uploads
   - Export in-memory state to PostgreSQL
   - Switch to PostgreSQL-only
3. **Post-migration**: Verify all pending files are accessible

The migration script:

```haskell
-- One-time migration: export in-memory state to PostgreSQL
migrateToPostgres :: 
  InMemoryFileStateStore -> 
  FileStateStore ->  -- Postgres store
  Task Text Int
migrateToPostgres memStore pgStore = do
  -- ConcurrentMap.forEachChunked handles iteration
  counter <- ConcurrentVar.containing 0
  ConcurrentMap.forEachChunked 100 
    (\fileRef state -> do
      pgStore.setState fileRef state
      ConcurrentVar.modify (\n -> n + 1) counter
    ) 
    memStore
  ConcurrentVar.get counter
```

**Note**: For most deployments, Scenario A is recommended. The in-memory store is intended for development; production should start with PostgreSQL from day one.

## Consequences

### Positive

1. **Production-ready**: File state survives restarts, enabling reliable file uploads in production

2. **Horizontal scaling**: Multiple server instances share state through PostgreSQL

3. **Infrastructure reuse**: Same database, same connection pool as EventStore—no new dependencies

4. **Efficient cleanup**: Index-backed queries replace full table scans for finding expired files

5. **Compliance-ready**: Owner-based indexes enable GDPR data export and deletion queries

6. **Drop-in replacement**: Implements existing `FileStateStore` interface; existing code unchanged

7. **Audit trail**: Full history preserved in database (soft delete with `deleted_at` timestamp)

### Negative

1. **Database dependency**: File uploads now require PostgreSQL (acceptable since EventStore already requires it)

2. **Latency increase**: Network round-trip to database vs in-memory lookup (mitigated by connection pooling)

3. **Schema migration**: Future changes to state representation require database migrations

4. **Storage growth**: Deleted file records accumulate (mitigated by periodic hard-delete job for old deleted records)

### Risks

| Risk | Mitigation |
|------|------------|
| Connection pool exhaustion | File operations are fast (simple queries); EventStore pool is sized for event operations which are heavier |
| Schema drift between in-memory and Postgres | Share `FileUploadState` type; Postgres implementation maps to/from row representation |
| Partial failure (blob stored, state not) | Existing cleanup process handles orphaned blobs via TTL expiration |
| Clock skew affecting expiration | Use database server time for `expires_at` comparisons, not application time |

## Alternatives Considered

### Alternative 1: SQLite

**Approach**: Use SQLite for a simpler, embedded database.

**Rejected because**:
- Doesn't support horizontal scaling (single file)
- Adds new dependency (SQLite library)
- Connection handling differs from PostgreSQL patterns already in use
- Would need separate backup/monitoring strategy

### Alternative 2: Redis

**Approach**: Use Redis for fast in-memory persistence with disk snapshots.

**Rejected because**:
- Adds new infrastructure dependency
- Requires separate Redis deployment and monitoring
- Different consistency model (eventual vs immediate)
- Overkill for file metadata (we don't need sub-millisecond access)

### Alternative 3: File-based JSON

**Approach**: Persist state as JSON files on disk.

**Rejected because**:
- No atomic transactions (partial writes on crash)
- No indexes for cleanup/owner queries
- Doesn't scale horizontally without shared filesystem
- Manual concurrency control needed

### Alternative 4: Event-source the file state

**Approach**: Store `FileUploadEvent`s in the EventStore and project to state.

**Rejected because**:
- Overkill: file lifecycle is simple (3 states) and doesn't benefit from event replay
- Adds complexity: separate projections for cleanup queries
- Already have EventStore overhead for domain events; file metadata is operational concern
- A simple table is more debuggable for operations team

## Implementation Plan

### Phase 1: Core Implementation (PR A, Small)

- `FileStateStore.Postgres` module
- Table schema with indexes
- Basic CRUD operations (get/set/update)
- Unit tests with test database

### Phase 2: Cleanup Integration (PR B, Small)

- Add `findExpiredPending` method
- Update `cleanupExpiredFiles` to use efficient query
- Integration tests for cleanup flow

### Phase 3: Documentation & Migration (PR C, Small)

- Update `FileUpload.Web` module docs
- Add migration guide to README
- Update testbed to demonstrate PostgreSQL config

## References

- [ADR-0004: EventStore Abstraction](0004-eventstore-abstraction.md) - PostgreSQL patterns
- [ADR-0006: Entity Snapshot Cache](0006-entity-snapshot-cache.md) - Similar persistent backend pattern
- [ADR-0011: File Upload Architecture](0011-file-upload-architecture.md) - FileStateStore interface definition
- [Service/FileUpload/Web.hs](../../core/service/Service/FileUpload/Web.hs) - Current FileStateStore interface
- [Service/EventStore/Postgres/Internal.hs](../../core/service/Service/EventStore/Postgres/Internal.hs) - Connection pooling patterns
