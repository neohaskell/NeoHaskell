# ADR-0010: OAuth2 Provider Integration Architecture

## Status

Accepted

## Context

NeoHaskell needs a zero-friction way for developers ("Jess" - our target persona, a time-constrained junior developer) to connect external OAuth2 APIs (Oura Ring, GitHub, Google, etc.) without understanding OAuth2 security internals.

### Current State

- PR #276 provides OAuth2 client primitives (PKCE, token exchange, SecretStore)
- WebTransport only handles `/commands/{name}` and `/queries/{name}`
- No way to add custom HTTP routes for OAuth2 callbacks
- Jess would need to manually implement PKCE, CSRF protection, state management, token storage

### Requirements

- 50k req/s capability (not blocking hot paths)
- EU-grade security (GDPR-compliant token handling, no PII leakage)
- Correctness (no race conditions, proper error handling)
- Multi-instance deployment support (Kubernetes)

### Relationship to ADR-0009

ADR-0009 (JWT Authentication Middleware) provides the foundation for authenticating users via JWT tokens from external OAuth providers. This ADR builds on that foundation to enable **outbound OAuth2 connections** - where NeoHaskell acts as an OAuth2 client connecting to external APIs on behalf of authenticated users.

| ADR | Direction | Purpose |
|-----|-----------|---------|
| ADR-0009 | Inbound | Validate JWTs from external identity providers |
| ADR-0010 | Outbound | Connect to external OAuth2 APIs (GitHub, Google, Oura, etc.) |

## Decision

Implement `Application.withOAuth2Provider` as a first-class WebTransport extension with the following architecture:

### 1. Module Structure

**New modules:**

```text
core/auth/Auth/OAuth2/
  Provider.hs              -- Config types, smart constructors (OnSuccess, OnFailure, etc.)
  StateToken.hs            -- HMAC-signed state encoding/decoding with TTL
  TransactionStore.hs      -- Interface for one-time state->verifier storage
  TransactionStore/
    InMemory.hs            -- Dev implementation with atomic consume

core/service/Service/
  Transport/Web/
    OAuth2Routes.hs        -- WAI handlers for /connect, /callback, /disconnect
  Application/
    OAuth2.hs              -- Wiring glue between Application and routes
```

**Modified modules:**

- `Service/Application.hs` - Add `oauth2Providers` field, `withOAuth2Provider` builder
- `Service/Transport/Web.hs` - Add custom routes extension point in `assembleTransport`

### 2. State Token Design

State tokens are self-describing and tamper-evident:

**Payload structure:**

```haskell
data StatePayload = StatePayload
  { provider :: Text      -- Provider name (mix-up defense)
  , userId :: Text        -- From JWT sub claim
  , nonce :: Text         -- Random 128-bit, base64url encoded
  , iat :: Int            -- Issued-at unix timestamp
  , exp :: Int            -- Expiry unix timestamp
  }
```

**Encoding:**

1. Serialize payload to JSON bytes
2. Compute `sig = HMAC-SHA256(hmacKey, payloadBytes)`
3. Concatenate: `payloadBytes || sig`
4. Base64url encode (no padding)

**Verification:**

1. Base64url decode
2. Split payload and signature (fixed 32-byte sig suffix)
3. Constant-time HMAC comparison
4. Decode JSON payload
5. Enforce TTL: `now <= exp` and `now >= iat - 60s` (clock skew tolerance)
6. Verify `provider` matches route segment (mix-up defense)

**TTL:** 5 minutes default, configurable.

### 3. Two-Store Architecture

**TransactionStore** (short-lived, OAuth flow state):

- Key: `oauth2:tx:{provider}:{sha256(stateToken)}`
- Value: `CodeVerifier` + `expiresAt`
- Semantics: Atomic `consumeTx` (get + delete in one operation)
- Purpose: One-time use guarantee, replay prevention

**SecretStore** (long-lived, user tokens):

- Key: `oauth2:tokens:{provider}:{sha256(userId)}`
- Value: `TokenSet`
- Purpose: Store access/refresh tokens for API calls

**Rationale for two stores:**

- Different lifetimes (minutes vs days/months)
- Different access patterns (write-once-read-once vs read-many)
- TransactionStore needs atomic consume; SecretStore doesn't
- Separation allows different backends (in-memory tx + encrypted token store)

### 4. WebTransport Integration

Extend `assembleTransport` route matching:

```haskell
case Wai.pathInfo request of
  ["connect", providerName] -> handleOAuth2Connect providerName
  ["callback", providerName] -> handleOAuth2Callback providerName
  ["disconnect", providerName] -> handleOAuth2Disconnect providerName
  ["commands", name] -> ...existing...
  ["queries", name] -> ...existing...
  _ -> notFound
```

**Route authentication:**

| Route | Auth Required | Reason |
|-------|---------------|--------|
| `GET /connect/{provider}` | JWT required | Need userId to store tokens |
| `GET /callback/{provider}` | No JWT | Browser redirect from provider; userId in state |
| `POST /disconnect/{provider}` | JWT required | Verify ownership before deletion |

### 5. Application Wiring

**New Application fields:**

```haskell
data Application = Application
  { ...existing...
  , oauth2Providers :: Array OAuth2Provider.Config
  , oauth2TransactionStore :: Maybe TransactionStore
  , oauth2StateHmacKey :: Maybe HmacKey
  }
```

**Builder API:**

```haskell
withOAuth2Provider :: OAuth2Provider.Config -> Application -> Application
withSecretStore :: SecretStoreConfig c => c -> Application -> Application
```

**Runtime wiring (in `runWith`):**

1. After `combinedCommandEndpoints` is built
2. Create `OAuth2Runtime` from configs + stores + dispatcher
3. Generate route handlers
4. Attach to WebTransport before `runTransports`

### 6. Command Dispatch

Callbacks emit domain commands via the existing integration dispatcher:

```haskell
-- In callback handler, after successful token exchange:
let payload = onSuccess userId connectionInfo  -- Returns CommandPayload
dispatchCommand combinedCommandEndpoints payload
```

Commands go through normal command execution pipeline, enabling:

- Event generation (audit trail)
- Outbound integrations (sync data after connection)
- Query updates (connection status in read models)

## Consequences

### Positive

- Jess writes ~20 lines of config to connect any OAuth2 provider
- All security handled automatically (PKCE, CSRF, token storage)
- Consistent with existing `withService`, `withOutbound` patterns
- Commands enable full event-sourcing integration
- Multi-instance safe with Redis TransactionStore

### Negative

- WebTransport becomes slightly more complex (custom routes)
- Two stores to configure in production (TransactionStore + SecretStore)
- HMAC key management required (env var or secrets manager)

### Risks

- In-memory TransactionStore breaks in multi-instance deployments
- Clock skew > 60s causes state validation failures
- Provider rate limits may affect callback success rate

### Mitigations

- Document Redis requirement for production multi-instance
- Make clock skew tolerance configurable
- Add circuit breaker for token exchange (future work)

## Implementation Plan

### Phase 1 - Foundation (PR A, Small)

- `Auth.OAuth2.StateToken` module + unit tests
- `Auth.OAuth2.TransactionStore` interface
- `Auth.OAuth2.TransactionStore.InMemory` + concurrent consume tests

### Phase 2 - Transport (PR B, Medium)

- WebTransport custom routes extension
- `Service.Transport.Web.OAuth2Routes` handlers
- `Auth.OAuth2.Provider` config types

### Phase 3 - Wiring (PR C, Medium)

- `Application.withOAuth2Provider` implementation
- `Service.Application.OAuth2` glue module
- Integration tests with mock OAuth2 provider

### Phase 4 - Production Hardening (PR D, Optional)

- `Auth.OAuth2.TransactionStore.Redis` implementation
- HTTP manager pooling for token exchange
- Rate-limiting hooks

## Security Checklist

- [ ] PKCE: Always S256, never Plain
- [ ] State: HMAC-SHA256 signed, constant-time comparison
- [ ] State TTL: 5 minutes max, configurable
- [ ] State replay: Atomic one-time consume in TransactionStore
- [ ] Mix-up defense: Validate provider in state matches route
- [ ] Token storage: Never in event store, encrypt at rest
- [ ] Logging: Hash userId, never log state/code/tokens/verifiers
- [ ] HTTPS: Enforced for redirect URIs and token endpoints
- [ ] Error messages: Sanitized, no secret reflection

## Performance Considerations

- **Hot path** (`/connect`): Generate randoms + store write + redirect (~1ms)
- **Callback path**: Store consume + HTTP to provider + store write + dispatch (~100-500ms)
- **50k req/s**: Achievable for `/connect`; callbacks limited by provider rate limits
- **Multi-instance**: Requires Redis TransactionStore (in-memory breaks one-time use)
- **HTTP pooling**: Shared manager for token exchange to avoid connection overhead

## References

- Issue: #277 (Feature specification)
- PR #276: OAuth2 client primitives
- [ADR-0009: JWT Authentication Middleware](0009-jwt-authentication-middleware.md) - Foundation for user authentication
- RFC 7636: PKCE
- RFC 6749: OAuth 2.0 Authorization Framework
