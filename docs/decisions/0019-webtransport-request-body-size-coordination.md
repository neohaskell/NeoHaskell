# ADR-0019: WebTransport Request Body Size Coordination

## Status

Proposed

## Context

When uploading files larger than 1MB through NeoHaskell's file upload system, Warp rejects the request before it reaches the file upload handler:

```
Request body exceeds maximum size of 1048576 bytes
```

This occurs despite `FileUploadConfig.maxFileSizeBytes` being set to 10MB (or higher).

### Current State

Two independent size limits exist:

| Configuration | Default | Location | Purpose |
|--------------|---------|----------|---------|
| `WebTransport.maxBodySize` | 1MB (1048576 bytes) | `Service/Transport/Web.hs:121` | Warp-level DoS protection |
| `FileUploadConfig.maxFileSizeBytes` | 10MB (10485760 bytes) | `Service/FileUpload/Core.hs:372` | File upload validation |

The problem is that these limits are **not coordinated**:

1. `Application.withFileUpload` stores `FileUploadConfig` in the application
2. In `Application.runWith`, the file upload setup is initialized
3. In `Transports.runWebTransport`, the WebTransport is configured with auth, OAuth2, and file upload routes
4. **But `maxBodySize` is never updated** to match `maxFileSizeBytes`

```haskell
-- Transports.hs:101-106
let webTransportWithConfig = baseWebTransport
      { authEnabled = maybeAuth
      , oauth2Config = maybeOAuth2
      , fileUploadEnabled = maybeFileUpload  -- Routes only!
      , apiInfo = maybeApiInfo
      }
      -- maxBodySize stays at 1MB default
```

### The Request Flow

```
Client POST /files/upload (5MB file)
    │
    ▼
Warp receives request
    │
    ▼
readBodyWithLimit checks maxBodySize (1MB)
    │
    ▼
❌ REJECTED: "Request body exceeds maximum size of 1048576 bytes"
    │
    (File upload handler never reached)
    │
    (maxFileSizeBytes check never runs)
```

### Relationship to Existing ADRs

| ADR | Relationship |
|-----|-------------|
| ADR-0011 | Defines `FileUploadConfig` and its `maxFileSizeBytes` field |
| ADR-0014 | WebTransport OpenAPI integration (where body limits matter for schema) |

## Decision

### 1. Update `FileUploadEnabled` to Include Max Size

Add `maxRequestBodyBytes` to `FileUploadEnabled` so the size limit can be propagated to the transport layer:

```haskell
-- Service/Transport/Web.hs
data FileUploadEnabled = FileUploadEnabled
  { fileUploadRoutes :: FileUploadRoutes
  , maxRequestBodyBytes :: Int64  -- NEW
  }
```

### 2. Populate `maxRequestBodyBytes` in `Application.runWith`

When creating `maybeFileUploadEnabled`, include the configured max file size:

```haskell
-- Application.hs, in runWith
let maybeFileUploadEnabled = case maybeFileUploadSetup of
      Nothing -> Nothing
      Just setup -> Just Web.FileUploadEnabled
        { Web.fileUploadRoutes = FileUpload.createRoutes setup
        , Web.maxRequestBodyBytes = setup.config.maxFileSizeBytes
        }
```

### 3. Coordinate Body Size in `runWebTransport`

When file uploads are enabled, update `maxBodySize` to match:

```haskell
-- Transports.hs, in runWebTransport
let webTransportWithConfig = baseWebTransport
      { authEnabled = maybeAuth
      , oauth2Config = maybeOAuth2
      , fileUploadEnabled = maybeFileUpload
      , apiInfo = maybeApiInfo
      -- Coordinate body size with file upload limit
      , maxBodySize = case maybeFileUpload of
          Nothing -> baseWebTransport.maxBodySize
          Just fileUpload -> max baseWebTransport.maxBodySize (fromIntegral fileUpload.maxRequestBodyBytes)
      }
```

### 4. Type Conversion Consideration

`WebTransport.maxBodySize` is `Int` while `FileUploadConfig.maxFileSizeBytes` is `Int64`. The conversion uses `fromIntegral` which is safe because:

- File sizes exceeding `maxBound :: Int` (2^63-1 on 64-bit, 2^31-1 on 32-bit) are impractical
- On 32-bit systems, this limits uploads to ~2GB which is reasonable for HTTP body limits
- A future ADR could unify both fields to `Int64` for consistency

### 5. Behavior Summary

After this change:

| Scenario | `maxBodySize` Used |
|----------|-------------------|
| No file uploads | User-configured or default (1MB) |
| File uploads enabled | `max(user-configured, maxFileSizeBytes)` |

The `max` ensures:
- If user explicitly sets a larger `maxBodySize`, it's respected
- If file uploads need more space, the limit is automatically raised

## Consequences

### Positive

- File uploads work out of the box without manual `maxBodySize` configuration
- Existing applications with file uploads will immediately work with larger files
- The framework maintains "Jess-friendly" promise: less configuration ceremony
- Defense in depth: Warp limit + file upload validation

### Negative

- `FileUploadEnabled` grows by one field
- Implicit behavior change: enabling file uploads now affects global body limit
- Users who explicitly want a lower body limit for non-file endpoints must configure separately (future enhancement)

### Trade-offs

**Implicit coordination vs explicit configuration:**

We chose implicit coordination because:
1. The vast majority of users want "file uploads just work"
2. The alternative (documentation saying "also set maxBodySize") leads to confusing errors
3. Taking the `max` is safe: it only raises limits, never lowers them

**Single global limit vs per-endpoint limits:**

The current design uses one global body limit. Per-endpoint limits (e.g., 1MB for commands, 10MB for uploads) would be more flexible but:
1. Adds complexity to transport configuration
2. Requires significant refactoring of request handling
3. Can be addressed in a future ADR if needed

### Migration Path

**No migration required.** This is a purely additive change:

- Existing applications without file uploads: no behavior change
- Existing applications with file uploads: body limit automatically raised to match `maxFileSizeBytes`
- Users who customized `maxBodySize`: their value is respected (via `max`)

## Implementation Notes

### Files to Modify

1. `core/service/Service/Transport/Web.hs` - Add `maxRequestBodyBytes` to `FileUploadEnabled`
2. `core/service/Service/Application.hs` - Pass `maxFileSizeBytes` when creating `FileUploadEnabled`
3. `core/service/Service/Application/Transports.hs` - Coordinate `maxBodySize` in `runWebTransport`

### Test Cases

1. **File upload with default settings:** Upload 5MB file with default `maxFileSizeBytes` (10MB) should succeed
2. **File upload exceeding limit:** Upload 15MB file with default limit should fail with clear error
3. **Custom body size honored:** If user sets `maxBodySize = 50MB`, that should be respected
4. **No file uploads:** Without `withFileUpload`, body limit stays at user-configured value

## References

- [Issue #359](https://github.com/neohaskell/neohaskell/issues/359)
- [ADR-0011: File Upload Architecture](0011-file-upload-architecture.md)
- [ADR-0014: WebTransport OpenAPI Integration](0014-webtransport-openapi-integration.md)
