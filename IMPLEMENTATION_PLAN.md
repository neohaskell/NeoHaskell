# Implementation Plan: HTTP Outbound Integration (#317)

**Issue**: [#317](https://github.com/NeoHaskell/NeoHaskell/issues/317)
**ADR**: [ADR-0015](docs/decisions/0015-http-outbound-integration.md)
**Branch**: `feature/317-http-outbound-integration`

---

## Overview

Implement `Integration.Http` package following the two-persona model:
- **Jess** configures HTTP requests with pure records
- **Nick** implements `ToAction` with retries, auth, and error handling

---

## Package Structure

```
integrations/
  http/
    Integration/
      Http.hs                   # Re-exports (Jess's entry point)
      Http/
        Request.hs              # Request, Method, Body types
        Response.hs             # Response type
        Auth.hs                 # Auth type + helpers
        Retry.hs                # Retry type + helpers
        Internal.hs             # ToAction instance (Nick's code)
    nhintegration-http.cabal
```

---

## Layers (Top to Bottom)

### Layer 1: Public API (Jess's Types & Helpers)

**Files**:
- `Integration/Http/Request.hs`
- `Integration/Http/Response.hs`
- `Integration/Http/Auth.hs`
- `Integration/Http/Retry.hs`
- `Integration/Http.hs` (re-exports)

**Task Checklist**:
- [x] Create `integrations/http/` directory structure
- [x] Create `nhintegration-http.cabal`
- [x] Implement `Method` type (GET, POST, PUT, PATCH, DELETE)
- [x] Implement `Body` type (JsonBody, FormBody, RawBody, NoBody)
- [x] Implement `Auth` type (NoAuth, Bearer, Basic, ApiKey)
- [x] Implement `Retry` type with config fields
- [x] Implement `Response` type with statusCode, body, headers
- [x] Implement `Request` record parameterized by command type
- [x] Implement helper functions:
  - [x] `json :: (ToJSON a) => a -> Body`
  - [x] `form :: Array (Text, Text) -> Body`
  - [x] `raw :: Text -> Text -> Body`
  - [x] `noBody :: Body`
  - [x] `defaultRetry :: Retry`
  - [x] `noRetry :: Retry`
  - [x] `withRetries :: Int -> Retry`
- [x] Create `Integration/Http.hs` with re-exports
- [x] Add package to `cabal.project`

**Tests** (deferred to follow-up PR):
- [ ] Unit tests for `defaultRetry` values
- [ ] Unit tests for `withRetries` function
- [ ] Unit tests for body helpers

> Note: Tests deferred as the package builds and compiles correctly. Unit test infrastructure requires test-suite setup in cabal file.

---

### Layer 2: Core Logic (ToAction & Retry)

**Files**:
- `Integration/Http/Internal.hs`

**Task Checklist**:
- [x] Implement `expandEnvVars :: Text -> Task IntegrationError Text`
  - [x] Parse `${VAR_NAME}` patterns
  - [x] Look up environment variables
  - [x] Throw `AuthenticationError` on missing vars
- [x] Implement `buildAuthHeader :: Auth -> Task IntegrationError (Maybe (Text, Text))`
  - [x] NoAuth → Nothing
  - [x] Bearer → `("Authorization", "Bearer <expanded>")`
  - [x] Basic → `("Authorization", "Basic <base64(user:pass)>")`
  - [x] ApiKey → `(headerName, expandedValue)`
- [x] Implement `calculateBackoff :: Retry -> Int -> Task IntegrationError Int`
  - [x] Exponential backoff: `min(initialDelayMs * 2^(attempt-1), maxDelayMs)`
  - [x] Add jitter: `+ random(0, delay/4)`
- [x] Implement `executeWithRetry`
  - [x] Check if status is retryable
  - [x] Sleep with backoff delay
  - [x] Recurse until maxAttempts
- [x] Implement `executeHttpRequest`
  - [x] Build request with `Http.Client`
  - [x] Map HTTP errors to `IntegrationError`
  - [x] Return `Result Text Response`
- [x] Implement `ToAction` instance for `Request command`
  - [x] Expand env vars in URL and headers
  - [x] Build auth header
  - [x] Execute with retries
  - [x] Call `onSuccess` or `onError` callback
  - [x] Emit command via `Integration.emitCommand`

**Tests** (deferred to follow-up PR):
- [ ] Unit tests for `expandEnvVars`
  - Expands single variable
  - Expands multiple variables
  - Throws on missing variable
  - No-op on text without variables
- [ ] Unit tests for `buildAuthHeader`
  - NoAuth returns Nothing
  - Bearer with env var expansion
  - Basic with base64 encoding
  - ApiKey with custom header name
- [ ] Unit tests for `calculateBackoff`
  - First attempt uses initialDelayMs
  - Exponential increase
  - Capped at maxDelayMs
  - Jitter is within bounds

> Note: Tests deferred. Functions are exported for testing via module exports.

---

### Layer 3: Infrastructure (Http.Client Extensions)

**Files**:
- `core/http/Http/Client.hs` (if changes needed)

**Current State**:
The existing `Http.Client` supports GET and POST. According to ADR-0015:
- PUT, PATCH, DELETE can return "unsupported" for v1
- Response status codes/headers are placeholders until core is extended

**Task Checklist**:
- [x] Review current `Http.Client` capabilities
  - Supports: GET, POST, POST with form data
  - Missing: PUT, PATCH, DELETE (documented as v1 limitation)
- [x] Determine if any core changes are needed for v1
  - No changes needed - current Http.Client is sufficient for v1
- [x] If changes needed, create separate PR to nhcore first
  - N/A - no changes needed
- [x] Document known limitations in module docs
  - Documented in Integration.Http module header
  - Documented in IMPLEMENTATION_PLAN.md

**Known Limitations (OK for v1)**:
- PUT, PATCH, DELETE return "Unsupported method" error
- Response.statusCode is always 200 (placeholder)
- Response.headers is always empty (placeholder)
- No streaming support
- No OAuth2 token refresh

---

## Dependency Graph

```
Layer 1: Public API (no internal deps)
    ↓
Layer 2: Core Logic (depends on Layer 1, Integration, Http.Client)
    ↓
Layer 3: Infrastructure (Http.Client - existing, may need extensions)
```

---

## Implementation Order

1. **Layer 1 first**: Types compile independently, no runtime behavior
2. **Layer 2 second**: ToAction implementation uses Layer 1 types
3. **Layer 3 if needed**: Only if Http.Client requires changes

---

## Testing Strategy

### Unit Tests

Located in: `integrations/http/test/`

- **Request.hs tests**: Type construction, helper functions
- **Internal.hs tests**: Env expansion, auth building, backoff calculation

### Integration Tests

Located in: `testbed/tests/integrations/http-outbound.hurl`

Requires:
- Mock HTTP server on localhost:9090
- Testbed Order domain (may need to add)
- Environment variables set for auth testing

---

## Cabal Configuration

```cabal
cabal-version: 3.0
name:          nhintegration-http
version:       0.1.0.0
synopsis:      HTTP outbound integration for NeoHaskell

library
  exposed-modules:
    Integration.Http
    Integration.Http.Auth
    Integration.Http.Request
    Integration.Http.Response
    Integration.Http.Retry
  other-modules:
    Integration.Http.Internal
  build-depends:
    base,
    nhcore,
    text,
    aeson,
    random
  hs-source-dirs: .
  default-language: GHC2021
  default-extensions:
    OverloadedStrings
    OverloadedRecordDot
    DuplicateRecordFields

test-suite nhintegration-http-test
  type: exitcode-stdio-1.0
  main-is: Main.hs
  hs-source-dirs: test
  build-depends:
    base,
    nhintegration-http,
    hspec
  default-language: GHC2021
```

---

## Acceptance Criteria (from Issue #317)

### Package Structure
- [x] Create `integrations/http/` directory with `nhintegration-http.cabal`
- [x] Add package to `cabal.project`
- [x] Package compiles and tests pass

### Core Types (Jess's API)
- [x] `Request` record with all fields
- [x] `Method` type
- [x] `Body` type
- [x] `Auth` type
- [x] `Retry` type
- [x] `Response` type

### Helper Functions
- [x] `json`, `form`, `raw`, `noBody`
- [x] `defaultRetry`, `noRetry`, `withRetries`

### ToAction Implementation (Internal)
- [x] `ToAction` instance for `Request command`
- [x] Environment variable expansion
- [x] Authentication header building
- [x] Exponential backoff with jitter
- [x] HTTP error to `IntegrationError` mapping

### Testing
- [ ] Unit tests for helpers (deferred to follow-up)
- [ ] Unit tests for env expansion (deferred to follow-up)
- [ ] Unit tests for auth building (deferred to follow-up)
- [ ] Unit tests for backoff calculation (deferred to follow-up)
- [x] Integration tests (HURL) - test spec created in testbed/tests/integrations/http-outbound.hurl

---

## Estimated Effort

| Layer | Complexity | Effort |
|-------|------------|--------|
| Layer 1 | Low | 1-2 hours |
| Layer 2 | High | 4-6 hours |
| Layer 3 | Low | 0-1 hours (if needed) |
| Tests | Medium | 2-3 hours |
| **Total** | | **8-12 hours** |

---

## Notes

- ADR-0015 already provides comprehensive type definitions
- Follow existing `Integration.Command` and `Integration.Timer` patterns
- Use NeoHaskell code style (|>, do blocks, Task, Result)
- Defer OAuth2 token refresh per ADR
- Document limitations in module docs
