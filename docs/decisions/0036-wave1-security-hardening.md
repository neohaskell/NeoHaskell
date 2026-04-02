# ADR-0036: Wave 1 Security Hardening

## Status

Accepted

## Context

A security audit of the NeoHaskell core libraries and integrations identified seven issues across authentication, HTTP transport, and integration error handling. These issues range from race conditions in token storage to information leakage in error messages.

This ADR covers all seven issues as a single wave of related security fixes. They share a common theme: the existing code is functionally correct under normal conditions but lacks the defensive depth needed for production security.

This document has been refined through three review cycles:

1. **Security Review** — OWASP Top 10, NIST SP 800-53, GDPR compliance, Jess Test (is the secure path the ONLY path?), Astonishment Test, Effort Test
2. **Performance Review** — 50k req/s target on commodity hardware; per-request allocation, lock contention, INLINE pragmas, strict fields
3. **DevEx Review** — API naming philosophy, module placement, migration ergonomics

Four security gaps and three performance blockers were identified during review and are incorporated as binding requirements in this ADR (see "Review Findings" sections within each issue).

### Internal Dependencies

Two dependency constraints affect implementation order:

- **Issue #334 must be completed before #333**: Both modify `core/auth/Auth/SecretStore.hs` and `core/auth/Auth/OAuth2/TokenRefresh.hs`. The atomicity primitive added in #334 is the foundation the mutex logic in #333 builds on.
- **Issue #335 must be completed before #338**: Both modify `core/http/Http/Client.hs`. The secure request function added in #335 is the entry point that #338's size-limiting wrapper applies to.

---

## Decision

### Issue #334: SecretStore Atomicity (URGENT)

**Location**: `core/auth/Auth/SecretStore/InMemory.hs:60-88`, `core/auth/Auth/OAuth2/TokenRefresh.hs:72-78`

**Problem**

`withValidToken` performs a non-atomic read-check-write sequence:

1. `store.get tokenKey` reads the current tokens
2. The caller inspects the result
3. `store.atomicModify tokenKey (\_ -> Just newTokens)` writes new tokens

Between steps 1 and 3, another concurrent request can read the same (now-stale) tokens, trigger its own refresh, and write its result. The second write then overwrites the first. This is a classic TOCTOU (time-of-check/time-of-use) race condition. Under concurrent 401 responses, this causes redundant refresh calls and potential token loss.

The current `atomicModify` signature only returns `Unit`, so callers cannot retrieve the value they just wrote:

```haskell
-- Current interface (insufficient)
atomicModify :: TokenKey -> (Maybe TokenSet -> Maybe TokenSet) -> Task Text Unit
```

**Proposed Solution**

Add `atomicModifyReturning` to the `SecretStore` interface. This combines the read, transform, and write into a single atomic operation and returns the result of the transformation:

```haskell
-- New interface addition
data SecretStore = SecretStore
  { get :: TokenKey -> Task Text (Maybe TokenSet)
  , put :: TokenKey -> TokenSet -> Task Text Unit
  , delete :: TokenKey -> Task Text Unit
  , atomicModify :: TokenKey -> (Maybe TokenSet -> Maybe TokenSet) -> Task Text Unit
  , atomicModifyReturning
      :: forall result.
         TokenKey
      -> (Maybe TokenSet -> Task Text (Maybe TokenSet, result))
      -> Task Text result
  }
```

The in-memory implementation uses `ConcurrentVar.modifyReturning` to guarantee atomicity:

```haskell
-- core/auth/Auth/SecretStore/InMemory.hs
{-# INLINE atomicModifyReturningImpl #-}
atomicModifyReturning = \tokenKey transform -> do
  let key = unwrapTokenKey tokenKey
  var <- store |> ConcurrentMap.get key
  case var of
    Nothing -> Task.throw [fmt|No var for key: #{key}|]
    Just v -> ConcurrentVar.modifyReturning v (\current -> transform current)
```

**InMemory Architecture (MANDATORY — Performance Blocker #1)**

> **CRITICAL**: The current `InMemory.hs` uses a single `ConcurrentVar (Map TokenKey TokenSet)` — one MVar holding the entire token map. If `atomicModifyReturning` runs the network refresh call (200–2000ms) inside `ConcurrentVar.modifyReturning`, ALL token operations for ALL users are blocked for the duration of each OAuth2 refresh. At 50,000 req/s this is catastrophic.

`InMemory.hs` MUST be refactored to use **per-key ConcurrentVars**:

```haskell
-- REQUIRED architecture: ConcurrentMap TokenKey (ConcurrentVar (Maybe TokenSet))
-- NOT: ConcurrentVar (Map TokenKey TokenSet)

newtype InMemoryStore = InMemoryStore
  { perKeyStore :: ConcurrentMap Text (ConcurrentVar (Maybe TokenSet))
  }

{-# INLINE atomicModifyImpl #-}
atomicModify = \tokenKey transform -> do
  let key = unwrapTokenKey tokenKey
  var <- perKeyStore |> ConcurrentMap.getOrInsert key (ConcurrentVar.containing Nothing)
  ConcurrentVar.modify var transform

{-# INLINE atomicModifyReturningImpl #-}
atomicModifyReturning = \tokenKey transform -> do
  let key = unwrapTokenKey tokenKey
  var <- perKeyStore |> ConcurrentMap.getOrInsert key (ConcurrentVar.containing Nothing)
  ConcurrentVar.modifyReturning var (\current -> transform current)
```

With this architecture, contention is limited to the individual `(provider, userId)` pair. Different users never block each other.

**Required INLINE Pragmas**

```haskell
-- core/auth/Auth/SecretStore/InMemory.hs
{-# INLINE atomicModifyImpl #-}
{-# INLINE atomicModifyReturningImpl #-}

-- core/auth/Auth/SecretStore.hs
{-# INLINE unwrapTokenKey #-}  -- called on every store operation
```

**Consequences**

Positive:
- Eliminates the TOCTOU race in token refresh
- The transform function receives the current value and returns both the new value and a result, making the pattern composable
- Future implementations (Postgres, Vault) can use their own atomic primitives (e.g., `UPDATE ... RETURNING`)
- Per-key ConcurrentVars ensure different users/providers do not block each other

Negative:
- Adds a new required field to `SecretStore`, so all existing implementations must be updated
- The transform function runs inside the lock, so it must not block indefinitely (Http.Client's 10-second timeout mitigates the worst case)
- InMemory.hs requires architectural refactoring from single-MVar to per-key ConcurrentVars

---

### Issue #333: Token Refresh Mutex (URGENT)

**Depends on**: #334

**Location**: `core/auth/Auth/OAuth2/TokenRefresh.hs:67-107`

**Problem**

When multiple concurrent requests receive a 401 response for the same user/provider, each independently enters the refresh path. Without per-key locking, all of them call the upstream token endpoint simultaneously. This causes:

- Redundant network calls to the OAuth2 provider
- Race conditions where the last writer wins, potentially discarding a valid token
- Rate limiting or revocation by providers that detect concurrent refresh attempts

**Proposed Solution**

Add a `ConcurrentMap Text Lock` to the refresh context for per-key locking. Wrap the refresh operation in `Lock.with` and apply a double-check pattern: after acquiring the lock, re-read the store to see if another goroutine already refreshed.

**Lock Timeout Requirement (GAP-2)**

`Lock.with` as currently implemented has no timeout. If the lock holder's HTTP request hangs (or if the OAuth2 provider is slow beyond Http.Client's own timeout), all concurrent requests for the same user/provider pair will block indefinitely at lock acquisition. To address this:

1. `TokenRefreshError` MUST include a `LockAcquisitionTimeout Text` constructor
2. The `withValidToken` implementation MUST use a bounded lock acquisition (default: **30 seconds**)
3. If lock acquisition times out, the error message MUST include the `(providerName:userId)` key that timed out (not the userId alone, since that key is not PII)

Updated `TokenRefreshError` type:

```haskell
data TokenRefreshError domainError
  = TokenNotFound Text
  | RefreshTokenMissing
  | RefreshFailed OAuth2Error
  | StorageError Text
  | LockAcquisitionTimeout Text
  -- ^ Timed out waiting to acquire the per-key refresh lock.
  -- The Text field is the lock key ("provider:userId"), not the userId alone.
  -- Occurs when the lock holder's OAuth2 refresh is taking longer than
  -- the configured lock timeout (default 30 seconds).
  | ActionFailed domainError
```

**Lock Acquisition — Hot Path Optimization (Performance Blocker #2)**

The ADR originally proposed `ConcurrentMap.getOrCreate lockKey Lock.new`. This allocates a new `MVar` on **every** request, even on the common case where the lock already exists. At 50,000 req/s this creates 50,000 wasted MVar allocations per second with immediate GC pressure.

The implementation MUST use a two-phase lookup:

```haskell
-- REQUIRED: two-phase lookup
-- 1. Read-only get (no allocation) on every request
-- 2. getOrInsert (allocate MVar) only on first request per user
lock <- do
  existing <- ctx.refreshLocks |> ConcurrentMap.get lockKey
  case existing of
    Just l -> Task.yield l
    Nothing -> ctx.refreshLocks |> ConcurrentMap.getOrInsert lockKey Lock.new
```

NOT the single-call form `ConcurrentMap.getOrCreate lockKey Lock.new` which allocates on every hit.

```haskell
-- New type for refresh context (strict fields required — see Performance section)
data TokenRefreshContext = TokenRefreshContext
  { secretStore   :: !SecretStore
  , refreshLocks  :: !(ConcurrentMap Text Lock)
  }

-- Updated withValidToken (simplified)
withValidToken ::
  forall err result.
  TokenRefreshContext ->
  Text ->
  Text ->
  (AccessToken -> Task err result) ->
  (err -> Bool) ->
  Task (TokenRefreshError err) result
withValidToken ctx providerName userId action isUnauthorized = do
  let tokenKey = TokenKey [fmt|oauth:{providerName}:{userId}|]
  let lockKey = [fmt|{providerName}:{userId}|]

  -- Two-phase lock lookup: read-only on hot path, allocate only on miss
  lock <- do
    existing <- ctx.refreshLocks |> ConcurrentMap.get lockKey
    case existing of
      Just l -> Task.yield l
      Nothing -> ctx.refreshLocks |> ConcurrentMap.getOrInsert lockKey Lock.new

  -- Bounded lock acquisition: 30-second timeout
  Lock.withTimeout 30 lock (doWithValidToken ctx tokenKey action isUnauthorized)
    |> Task.mapError (\_ -> LockAcquisitionTimeout lockKey)

doWithValidToken ::
  forall err result.
  TokenRefreshContext ->
  TokenKey ->
  (AccessToken -> Task err result) ->
  (err -> Bool) ->
  Task (TokenRefreshError err) result
doWithValidToken ctx tokenKey action isUnauthorized = do
  -- Double-check: re-read after acquiring lock
  maybeTokens <- ctx.secretStore.get tokenKey |> Task.mapError StorageError
  case maybeTokens of
    Nothing -> Task.throw (TokenNotFound "unknown")
    Just tokenSet -> do
      let accessToken = unwrapAccessToken tokenSet.accessToken
      firstResult <- action accessToken |> Task.mapError ActionFailed |> Task.asResult
      case firstResult of
        Result.Ok result -> Task.yield result
        Result.Err (ActionFailed err) | isUnauthorized err -> do
          case tokenSet.refreshToken of
            Nothing -> Task.throw RefreshTokenMissing
            Just rt -> do
              -- Use atomicModifyReturning from #334 to refresh and store atomically
              newTokens <-
                ctx.secretStore.atomicModifyReturning tokenKey (\_ -> do
                  refreshed <- refreshAction rt |> Task.mapError RefreshFailed
                  Task.yield (Just refreshed, refreshed))
              let newAccessToken = unwrapAccessToken newTokens.accessToken
              action newAccessToken |> Task.mapError ActionFailed
        Result.Err err -> Task.throw err
```

**Required INLINE Pragmas**

```haskell
-- core/auth/Auth/OAuth2/TokenRefresh.hs
{-# INLINE withValidToken #-}   -- hot path; enables GHC to specialize on err type
{-# INLINE doWithValidToken #-} -- inner implementation; eliminates allocation
```

**Consequences**

Positive:
- Concurrent 401s for the same user/provider result in exactly one refresh call
- The double-check pattern means threads that waited for the lock benefit from the refresh done by the first thread
- Per-key locks mean different users/providers don't block each other
- `LockAcquisitionTimeout` surfaces hangs as named errors rather than silent deadlocks
- Two-phase lookup eliminates 50k unnecessary MVar allocations/second at hot path

Negative:
- Adds `ConcurrentMap` to the refresh context, which callers must construct
- Lock acquisition adds a small overhead on every token use (negligible compared to network I/O)
- `Lock.withTimeout` requires adding a timeout variant to the `Lock` module (30-second default)
- Deadlock is impossible here (single lock per operation, no nested locking), but future changes must be careful

---

### Issue #328: Constant-Time State Comparison (Enhancement)

**Location**: `core/auth/Auth/OAuth2/Types.hs:332` (`validateState`), `Types.hs:355` (`validateRedirectUri`)

**Problem**

Both functions use the standard `==` operator for security-critical string comparisons:

```haskell
-- validateState (line 332)
case expected == returned of
  True -> Ok ()
  False -> Err (InvalidState "State parameter mismatch - possible CSRF attack")

-- validateRedirectUri (line 355)
case registered == received of
  False -> Err (InvalidRedirectUri "Redirect URI does not match registered URI")
  True -> ...
```

Standard equality short-circuits on the first differing byte. An attacker who can measure response timing can use this to learn information about the expected value one byte at a time. For CSRF state tokens and redirect URIs, this is a timing attack vector.

The codebase already has a `BA.constEq` usage in `core/auth/Auth/OAuth2/StateToken.hs:265`. This fix exposes it as a named, reusable function and applies it to `validateState` and `validateRedirectUri`.

**Proposed Solution — GAP-1: `constEq` Must Be Added to StateToken Exports**

> **BLOCKING**: The ADR previously stated `import Auth.OAuth2.StateToken (constEq)` without specifying that `constEq` exists as an exported function. Reviewing `StateToken.hs` module exports (lines 39–55), **`constEq` is NOT defined or exported**. The existing code uses `BA.constEq` inline at line 265. The import will fail with: `Module 'Auth.OAuth2.StateToken' does not export 'constEq'`.

**StateToken.hs MUST be updated** to define and export `constEq :: Text -> Text -> Bool`:

```haskell
-- Addition to Auth.OAuth2.StateToken module exports:
module Auth.OAuth2.StateToken (
  -- * Types
  HmacKey,
  StatePayload (..),
  StateTokenError (..),

  -- * Key Management
  mkHmacKey,
  generateKey,

  -- * Nonce Generation
  generateNonce,

  -- * Encoding/Decoding
  encodeStateToken,
  decodeStateToken,

  -- * Constant-Time Comparison (re-exported for use in OAuth2 validation)
  constEq,   -- NEW
) where

-- New definition (add before or after the existing BA.constEq usage):
--
-- SECURITY: This MUST have {-# INLINE constEq #-} — without inlining, GHC may
-- reorder comparisons, defeating the constant-time guarantee.
{-# INLINE constEq #-}
constEq :: Text -> Text -> Bool
constEq a b = do
  let aBytes = Text.toBytes a |> Bytes.unwrap
  let bBytes = Text.toBytes b |> Bytes.unwrap
  BA.constEq aBytes bBytes
```

Then replace `==` in both validation functions:

```haskell
-- Import the constant-time comparison
import Auth.OAuth2.StateToken (constEq)

validateState :: State -> State -> Result OAuth2Error ()
validateState expectedState returnedState = do
  let expected = unwrapState expectedState
  let returned = unwrapState returnedState
  case constEq expected returned of
    True -> Ok ()
    False -> Err (InvalidState "State parameter mismatch - possible CSRF attack")

validateRedirectUri :: RedirectUri -> RedirectUri -> Result OAuth2Error ()
validateRedirectUri registeredUri receivedUri = do
  let registered = unwrapRedirectUri registeredUri
  let received = unwrapRedirectUri receivedUri
  case constEq registered received of
    False -> Err (InvalidRedirectUri "Redirect URI does not match registered URI")
    True -> do
      -- ... rest of validation unchanged ...
```

**INLINE Pragma — MANDATORY for Correctness (Not Just Performance)**

```haskell
-- core/auth/Auth/OAuth2/StateToken.hs
{-# INLINE constEq #-}
-- ^ MANDATORY: Without inlining, GHC may reorder the ByteString comparisons,
-- defeating the constant-time guarantee. This is a correctness requirement,
-- not a performance optimization.
```

**Consequences**

Positive:
- Eliminates timing side-channel from CSRF state and redirect URI validation
- `constEq` becomes a named, reusable, discoverable utility in the auth module (Option A preferred over importing BA directly in Types.hs)
- No API changes to callers of `validateState` / `validateRedirectUri`; purely internal

Negative:
- Constant-time comparison is slightly slower than short-circuit equality (negligible for these string lengths)
- Low priority: exploiting this timing channel requires network-level measurement precision and many samples, making it a theoretical rather than practical risk in most deployments
- StateToken.hs module surface area increases by one exported function

---

### Issue #335: Secure HTTP Client / TLS Enforcement (URGENT)

**Location**: `core/http/Http/Client.hs:264-284`

**Problem**

The current `getRaw` and related functions accept any URL, including `http://` URLs. There is no minimum TLS version enforcement. This means:

- Integration code can accidentally call `http://` endpoints, sending credentials in plaintext
- An attacker who can influence the URL (e.g., via misconfigured provider config) can downgrade to unencrypted transport
- No defense against TLS 1.0/1.1 downgrade attacks

**Proposed Solution**

Add `Http.getSecure` as a new entry point that enforces HTTPS and a minimum TLS version of 1.2.

**GAP-3: `getRaw` Must Move to `Http.Client.Internal`**

> **BLOCKING (Jess Test FAIL)**: `getRaw` is currently exported from `Http.Client` (line 15 of module header). Any code — including Jess's integration code — can call `Http.getRaw` with an external HTTP URL, bypassing `getSecure` entirely. A developer who finds `getRaw` first (alphabetically before `getSecure` in IDE autocomplete) can bypass TLS enforcement without realizing it.

**`getRaw` MUST be moved to `Http.Client.Internal`** and removed from the public `Http.Client` exports:

```haskell
-- Http.Client module exports (REVISED):
module Http.Client (
  -- * Request Configuration
  Request (..),
  request,
  withUrl,
  addHeader,
  withTimeout,
  withRedirects,
  withMaxResponseSize,  -- NEW from #338

  -- * Response
  Response (..),

  -- * HTTP Methods (Secure — external calls)
  get,
  getSecure,   -- NEW: enforces HTTPS + TLS 1.2+
  post,
  postForm,
  postRaw,
  put,
  patch,
  delete,

  -- * Errors
  Error (..),
  -- NOTE: getRaw is NOT exported here.
  -- It is available in Http.Client.Internal for localhost/internal use only.
  -- See ADR-0018: HTTP Localhost Exception for OAuth2 Discovery.
) where
```

Callers that legitimately need `getRaw` (e.g., localhost OAuth2 discovery per ADR-0018) MUST explicitly import `Http.Client.Internal`:

```haskell
-- Localhost/internal callers must opt-in explicitly:
import Http.Client.Internal (getRaw)
```

**GAP-4: `InvalidUrl` Error Message Must Use `sanitizeUrlText`**

> **BLOCKING**: The proposed error message `Got: {urlText}` includes the full URL. If the URL contains query parameters with tokens or secrets (e.g., `?client_secret=abc`), this leaks secrets. The existing `sanitizeUrlText` function already strips query params.

The error message MUST use `sanitizeUrlText`:

```haskell
{-# INLINE getSecure #-}
getSecure ::
  Request ->
  Task Error (Response Bytes)
getSecure options = do
  let urlText = options.url |> Maybe.withDefault ""
  -- SECURITY: sanitizeUrlText strips query params (which may contain secrets)
  let sanitizedUrl = sanitizeUrlText urlText
  case Text.startsWith "https://" urlText of
    False -> Task.throw (InvalidUrl [fmt|Secure requests require HTTPS. Got: {sanitizedUrl}|])
    True -> do
      getRawWithTlsConfig minimumTls12Config options

-- TLS configuration (internal)
data TlsConfig = TlsConfig
  { minimumVersion      :: !TlsVersion
  , validateCertificate :: !Bool
  }

minimumTls12Config :: TlsConfig
minimumTls12Config =
  TlsConfig
    { minimumVersion = Tls12
    , validateCertificate = True
    }

-- Updated Error type
data Error
  = HttpError Text
  | InvalidUrl Text
  | Timeout
  | TlsError Text
  deriving (Generic, Show)
```

**URL Scheme Case Sensitivity**: `Text.startsWith "https://"` is case-sensitive. `HTTPS://api.example.com` would be rejected. This is safe (fail-closed) and intentional — all NeoHaskell-generated URLs use lowercase schemes. This must be documented in `getSecure`'s Haddock comment.

**Must NOT include**: Certificate pinning. This is deferred to a future ADR. Pinning requires per-provider configuration and a key rotation strategy that is out of scope for this wave.

**Consequences**

Positive:
- Prevents accidental plaintext HTTP calls from integration code (Jess Test now PASSES)
- Enforces TLS 1.2+ minimum, blocking downgrade attacks
- `sanitizeUrlText` in error messages prevents secrets in query params from leaking
- `getRaw` in `Http.Client.Internal` makes the localhost exception explicit and intentional

Negative:
- Existing callers using `getRaw` for external endpoints must migrate to `getSecure`
- The localhost exception (ADR-0018) must explicitly import `Http.Client.Internal`, which is more friction but makes the security trade-off visible
- Only lowercase `https://` scheme is accepted; callers must normalize URLs before calling `getSecure`

---

### Issue #338: Response Size Limits (IMPORTANT)

**Depends on**: #335

**Location**: `core/http/Http/Client.hs:264-284`

**Problem**

Neither `getRaw` nor `getSecure` cap the response body size. A malicious or misconfigured server can return an arbitrarily large response body, causing the process to exhaust memory. This is an OOM (out-of-memory) denial-of-service vector.

**Proposed Solution**

Add `withMaxResponseSize` as a `Request` modifier. The default limit is 10MB.

**Content-Length Shortcut (MANDATORY — Performance Blocker #3)**

> **CRITICAL**: The ADR originally specified "reads the response body in chunks and aborts if the accumulated size exceeds the limit." Without a `Content-Length` header check, every response body at 50,000 req/s with 10KB average = 500MB/s of byte counting. This is measurable CPU overhead.

The implementation MUST use a **two-strategy approach**:

1. **Primary: `Content-Length` header check** — O(1) lookup. If the declared `Content-Length > maxResponseBytes`, throw `ResponseTooLarge` immediately with zero bytes read. This handles the vast majority of well-behaved API servers.
2. **Fallback: Incremental byte counting** — only for chunked transfer encoding or responses with no `Content-Length` header. Accumulate chunk sizes and abort as soon as the running total exceeds `maxResponseBytes`.

```haskell
-- REQUIRED implementation strategy (pseudocode):
checkResponseSize :: Int -> HttpClient.Response bodyReader -> Task Error ()
checkResponseSize maxBytes httpResponse = do
  -- Strategy 1: Content-Length header shortcut (O(1), covers most API responses)
  let contentLength = getResponseHeader "Content-Length" httpResponse
  case contentLength of
    Just len | len > maxBytes ->
      Task.throw (ResponseTooLarge { limit = maxBytes, received = len })
    _ ->
      -- Strategy 2: Incremental counting for chunked/unknown-length responses
      -- Use withResponse + brReadSome from http-client:
      --   brReadSome :: BodyReader -> Int -> IO ByteString
      -- Read in chunks, accumulate total, throw ResponseTooLarge on limit exceeded
      Task.yield ()
```

Note: The `ResponseTooLarge` constructor should carry both `limit` and `received` for better debugging:

```haskell
{-# INLINE withMaxResponseSize #-}
withMaxResponseSize :: Int -> Request -> Request
withMaxResponseSize maxBytes options =
  options { maxResponseBytes = Just maxBytes }

-- Updated Request type
data Request = Request
  { url :: Maybe Text
  , headers :: Map Text Text
  , timeoutSeconds :: Maybe Int
  , maxRedirects :: Int
  , maxResponseBytes :: !(Maybe Int)
  -- ^ Maximum response body size in bytes.
  -- 'Nothing' means no limit (not recommended for production).
  -- Default via 'request' is 10MB (10 * 1024 * 1024).
  -- SECURITY: See module docs — always set a limit for untrusted endpoints.
  }

-- Updated default
instance Default Request where
  def =
    Request
      { url = Nothing
      , headers = Map.empty
      , timeoutSeconds = Just 10
      , maxRedirects = 0
      , maxResponseBytes = Just (10 * 1024 * 1024)
      }

-- Updated Error type
data Error
  = HttpError Text
  | InvalidUrl Text
  | Timeout
  | TlsError Text
  | ResponseTooLarge { limit :: Int, received :: Int }
  -- ^ Response exceeded the configured maximum size.
  -- 'limit' is the configured maximum; 'received' is bytes read before abort.
  deriving (Generic, Show)
```

**Must NOT include**: Streaming response support. Streaming requires a different API shape (callbacks or conduit) and is out of scope for this wave.

**Consequences**

Positive:
- Prevents OOM from oversized responses
- Default 10MB limit is generous for API responses but blocks pathological cases
- `Content-Length` shortcut means well-behaved servers (the vast majority) incur only an O(1) header lookup
- `withMaxResponseSize` lets callers adjust the limit for legitimate large responses (e.g., file downloads)

Negative:
- Adds `maxResponseBytes` to `Request`, which changes the `Default` instance
- Callers that legitimately need large responses must explicitly call `withMaxResponseSize` with a higher limit
- Chunked transfer encoding requires careful byte counting to avoid off-by-one errors in the limit check

---

### Issue #337: Oura Error Message Sanitization (IMPORTANT)

**Location**: `integrations/Integration/Oura/Internal.hs:622`

**Problem**

`mapTokenError` includes the `userId` in the error message for `TokenNotFound`:

```haskell
-- Current code (line 622)
mapTokenError :: TokenRefreshError OuraHttpError -> IntegrationError
mapTokenError err = case err of
  TokenNotFound userId -> AuthenticationError [fmt|Token not found for user: #{userId}|]
  RefreshTokenMissing -> AuthenticationError "Refresh token missing"
  RefreshFailed oauth2Err -> AuthenticationError [fmt|Refresh failed: #{toText oauth2Err}|]
  StorageError msg -> UnexpectedError [fmt|Storage error: #{msg}|]
  ActionFailed (OtherHttpError msg) -> NetworkError msg
  ActionFailed Unauthorized -> AuthenticationError "Unauthorized after refresh"
  ActionFailed (OuraRateLimited retryAfter) -> RateLimited retryAfter
```

`IntegrationError` values propagate to API responses and logs. Including `userId` in an authentication error message leaks PII. If the error reaches a client response, it exposes user identifiers to potentially unauthorized parties.

**Proposed Solution**

Remove `userId` from the error message. The `_userId` binding is kept (with underscore prefix) to document that the value exists but is intentionally not used in the message. The new `LockAcquisitionTimeout` constructor (added in #333) must also be handled:

```haskell
mapTokenError :: TokenRefreshError OuraHttpError -> IntegrationError
mapTokenError err = case err of
  TokenNotFound _userId -> AuthenticationError "Token not found"
  RefreshTokenMissing -> AuthenticationError "Refresh token missing"
  RefreshFailed oauth2Err -> AuthenticationError [fmt|Refresh failed: #{toText oauth2Err}|]
  StorageError msg -> UnexpectedError [fmt|Storage error: #{msg}|]
  LockAcquisitionTimeout lockKey -> UnexpectedError [fmt|Lock timeout for key: #{lockKey}|]
  ActionFailed (OtherHttpError msg) -> NetworkError msg
  ActionFailed Unauthorized -> AuthenticationError "Unauthorized after refresh"
  ActionFailed (OuraRateLimited retryAfter) -> RateLimited retryAfter
```

Note: `lockKey` is `"providerName:userId"` format — this is an internal key, not user-identifiable PII in isolation. It is acceptable to include in error messages.

**Must NOT include**: Internal audit logging for token lookup failures. Structured audit logging is covered by ADR-0028 and is separate work. This fix is purely about removing PII from error messages.

**Consequences**

Positive:
- Eliminates PII leakage from authentication error messages
- Minimal change: one line removal, no API surface changes
- The fix is self-documenting: `_userId` signals the value was considered and deliberately excluded
- New `LockAcquisitionTimeout` arm handled explicitly (exhaustiveness guarantee)

Negative:
- Slightly less diagnostic information in error messages. Operators debugging token issues will need to correlate with request context (e.g., request ID) rather than reading the userId from the error message directly.

---

### Issue #339: Provider Registry Immutability (IMPORTANT)

**Location**: `core/service/Integration.hs:111-117`

**Problem**

`ActionContext.providerRegistry` is typed as `Map Text ValidatedOAuth2ProviderConfig`:

```haskell
data ActionContext = ActionContext
  { secretStore :: SecretStore
  , providerRegistry :: Map Text ValidatedOAuth2ProviderConfig
  , fileAccess :: Maybe FileAccessContext
  }
```

`Map` is a mutable-by-convention type. Nothing in the type system prevents code from constructing a modified `ActionContext` with a different registry at runtime. While no current code does this, the lack of a type-level guarantee is a defense-in-depth gap: a future change or integration could accidentally or maliciously swap provider configurations mid-request.

**Proposed Solution**

Wrap the registry in a `newtype` that exports only read operations. The constructor is not exported, so the registry can only be created via `fromMap` and never modified after construction.

```haskell
-- New module: core/service/Integration/ProviderRegistry.hs

module Integration.ProviderRegistry
  ( ImmutableProviderRegistry
  -- Constructor NOT exported: only fromMap can create a registry
  , fromMap
  , lookup
  , toList
  ) where

import Auth.OAuth2.Types (ValidatedOAuth2ProviderConfig)
import Map (Map)
import Map qualified
import Text (Text)

-- | A provider registry that cannot be modified after construction.
--
-- The constructor is not exported. Use 'fromMap' to create a registry
-- from a validated configuration map.
--
-- INTENTIONAL: This module exposes NO modification operations.
-- The immutability guarantee prevents runtime provider configuration injection.
newtype ImmutableProviderRegistry
  = ImmutableProviderRegistry
      { getProviders :: !(Map Text ValidatedOAuth2ProviderConfig)
      }

-- | Create a registry from a validated configuration map.
-- This is the only way to construct an 'ImmutableProviderRegistry'.
{-# INLINE fromMap #-}
fromMap :: Map Text ValidatedOAuth2ProviderConfig -> ImmutableProviderRegistry
fromMap providers =
  ImmutableProviderRegistry { getProviders = providers }

-- | Look up a provider by name.
{-# INLINE lookup #-}
lookup :: Text -> ImmutableProviderRegistry -> Maybe ValidatedOAuth2ProviderConfig
lookup name registry =
  registry.getProviders |> Map.get name

-- | List all providers.
{-# INLINE toList #-}
toList :: ImmutableProviderRegistry -> Array (Text, ValidatedOAuth2ProviderConfig)
toList registry =
  registry.getProviders |> Map.toArray
```

Update `ActionContext` to use the new type:

```haskell
data ActionContext = ActionContext
  { secretStore :: SecretStore
  , providerRegistry :: ImmutableProviderRegistry
  , fileAccess :: Maybe FileAccessContext
  }
```

**Must NOT include**: Phantom types or initialization phase tracking. These would add significant complexity (type-level state machines, additional type parameters on `ActionContext`) for marginal benefit. The `newtype` wrapper with unexported constructor is sufficient for this wave.

**Required INLINE Pragmas — MANDATORY for Zero-Cost Abstraction**

The three INLINE pragmas on `lookup`, `fromMap`, and `toList` are **mandatory**, not optional. Without them, GHC may not eliminate the newtype accessor, adding a pointer indirection on every provider lookup (which is on the hot path for every authenticated request).

```haskell
{-# INLINE lookup #-}   -- MANDATORY: eliminates newtype indirection per request
{-# INLINE fromMap #-}  -- MANDATORY: eliminates newtype constructor
{-# INLINE toList #-}   -- MANDATORY: eliminates newtype accessor
```

**Consequences**

Positive:
- Type-level guarantee that the provider registry cannot be modified after `Application.run` constructs it
- Callers use `ImmutableProviderRegistry.lookup` instead of `Map.get`, making the intent clear
- Zero runtime cost with INLINE pragmas: `newtype` is erased at compile time
- Module documentation explicitly states no modification operations exist

Negative:
- All existing call sites that use `Map.get` on `providerRegistry` must migrate to `ImmutableProviderRegistry.lookup`
- Adds a new module to the service layer

---

## Performance Engineering Requirements

All changes in this ADR that touch hot paths require the following INLINE pragmas and strict fields. These are **implementation requirements**, not suggestions.

### INLINE Pragma Summary

```haskell
-- core/auth/Auth/SecretStore/InMemory.hs
{-# INLINE atomicModifyImpl #-}
{-# INLINE atomicModifyReturningImpl #-}  -- new function from #334

-- core/auth/Auth/SecretStore.hs
{-# INLINE unwrapTokenKey #-}  -- called on every store operation

-- core/auth/Auth/OAuth2/TokenRefresh.hs
{-# INLINE withValidToken #-}   -- hot path; enables err type specialization
{-# INLINE doWithValidToken #-} -- inner implementation; eliminates allocation

-- Integration/ProviderRegistry.hs (new module, #339)
{-# INLINE lookup #-}   -- MANDATORY: without this, newtype adds indirection per request
{-# INLINE fromMap #-}  -- MANDATORY: eliminates newtype constructor
{-# INLINE toList #-}   -- MANDATORY: eliminates newtype accessor

-- core/auth/Auth/OAuth2/StateToken.hs (#328)
{-# INLINE constEq #-}  -- MANDATORY for CORRECTNESS: without inlining, compiler may
                         -- reorder comparisons, defeating constant-time guarantee

-- core/http/Http/Client.hs (#335, #338)
{-# INLINE getSecure #-}          -- new function from #335; allows call-site optimization
{-# INLINE withMaxResponseSize #-} -- new function from #338; pure record update
```

### Strict Fields Summary

```haskell
-- NEW: TokenRefreshContext (from #333)
data TokenRefreshContext = TokenRefreshContext
  { secretStore   :: !SecretStore              -- prevent thunk; accessed every call
  , refreshLocks  :: !(ConcurrentMap Text Lock) -- prevent thunk; accessed on 401 path
  }

-- UPDATED: Request (from #338)
data Request = Request
  { url              :: Maybe Text
  , headers          :: Map Text Text
  , timeoutSeconds   :: Maybe Int
  , maxRedirects     :: Int
  , maxResponseBytes :: !(Maybe Int)  -- NEW: strict; checked on every response
  }

-- NEW: ImmutableProviderRegistry (from #339)
newtype ImmutableProviderRegistry
  = ImmutableProviderRegistry
      { getProviders :: !(Map Text ValidatedOAuth2ProviderConfig)  -- strict; always needed on lookup
      }

-- NEW: TlsConfig (from #335)
data TlsConfig = TlsConfig
  { minimumVersion      :: !TlsVersion  -- prevent thunk on config constant
  , validateCertificate :: !Bool        -- prevent thunk on config constant
  }
```

---

## DevEx Review Summary

### API Name Verification

All new API names have been reviewed for NeoHaskell naming philosophy (descriptive, explicit, TypeScript/Java-familiar):

| Name | Module | Verdict |
|------|--------|---------|
| `atomicModifyReturning` | `SecretStore` | ✅ Mirrors `AtomicReference.getAndUpdate` (Java) |
| `getSecure` | `Http.Client` | ✅ Intent-revealing; contrast with `getRaw` is clear |
| `ImmutableProviderRegistry` | `Integration.ProviderRegistry` | ✅ TypeScript `ReadonlyMap` equivalent |
| `constEq` | `Auth.OAuth2.StateToken` | ✅ Mirrors `crypto.timingSafeEqual` (Node.js) |
| `withMaxResponseSize` | `Http.Client` | ✅ Pipe-chainable modifier pattern |
| `LockAcquisitionTimeout` | `TokenRefreshError` | ✅ Names the failure mode precisely |

### Module Placement Verification

| New Code | Location | Reason |
|----------|----------|--------|
| `constEq` | `Auth.OAuth2.StateToken` | Auth-related utility; reusable within the auth layer |
| `getRaw` | `Http.Client.Internal` | Moved from public API; internal/localhost use only |
| `getSecure` | `Http.Client` | Public API; replaces `getRaw` as the default entry point |
| `ImmutableProviderRegistry` | `Integration.ProviderRegistry` | New module; service layer; one concept per module |
| `TokenRefreshContext` | `Auth.OAuth2.TokenRefresh` | Co-located with its primary consumer |

### Migration Path (No Breaking Changes Without Path)

| Change | Migration |
|--------|-----------|
| `getRaw` removed from `Http.Client` | Add `import Http.Client.Internal (getRaw)` for localhost callers; migrate external callers to `getSecure` |
| `providerRegistry :: ImmutableProviderRegistry` | Replace `Map.get` call sites with `ImmutableProviderRegistry.lookup` — mechanical change |
| `SecretStore.atomicModifyReturning` | New field; all `SecretStore` implementations must add it — Postgres/Vault via `UPDATE ... RETURNING` |
| `TokenRefreshContext` replaces bare args | Update call sites that construct `withValidToken` contexts |
| `LockAcquisitionTimeout` in `TokenRefreshError` | Add match arm to `mapTokenError` functions (exhaustiveness checking catches misses) |

---

## Consequences

### Positive (across all issues)

1. **Eliminated TOCTOU race** (#334): Token refresh is now atomic. Concurrent 401s for the same user no longer cause redundant refresh calls or token loss.

2. **Single refresh per key** (#333): Per-key locking with double-check ensures exactly one refresh call reaches the upstream provider per concurrent burst.

3. **Timing attack mitigation** (#328): CSRF state and redirect URI validation no longer leak information through response timing.

4. **Enforced HTTPS** (#335): Integration code cannot accidentally call `http://` endpoints. `getRaw` moved to Internal makes the localhost exception explicit. Misconfiguration produces a clear sanitized error.

5. **OOM protection** (#338): Malicious or oversized responses are rejected at the `Content-Length` header level (O(1)) for well-behaved servers, with incremental fallback for chunked responses.

6. **PII removed from errors** (#337): User IDs no longer appear in authentication error messages that may reach logs or API responses.

7. **Immutable registry** (#339): Provider configuration cannot be modified after startup, closing a defense-in-depth gap.

### Negative (across all issues)

1. **Migration burden**: Issues #334, #333, #335, #338, and #339 all change existing interfaces. Call sites must be updated.

2. **Increased complexity in auth layer**: The mutex and double-check pattern in #333 adds non-trivial concurrency logic that must be carefully maintained.

3. **Reduced diagnostic detail** (#337): Removing `userId` from error messages makes debugging token issues slightly harder without a correlated request ID.

4. **New module** (#339): `Integration.ProviderRegistry` adds a module that must be maintained and documented.

5. **InMemory.hs architectural refactor** (#334): Switching from single-MVar to per-key ConcurrentVars is a non-trivial refactor, though it is internal to the InMemory implementation and does not change the `SecretStore` interface.

### Implementation Order

Given the internal dependencies:

1. **#334** (SecretStore atomicity) first — includes InMemory.hs per-key refactor
2. **#333** (Token refresh mutex) after #334
3. **#328** (Constant-time comparison) independently — requires StateToken.hs update first
4. **#335** (Secure HTTP client) independently — moves `getRaw` to Internal
5. **#338** (Response size limits) after #335
6. **#337** (Error sanitization) after #333 — needs `LockAcquisitionTimeout` arm
7. **#339** (Provider registry immutability) independently

Issues #328, #339 have no dependencies and can proceed in parallel with the auth and HTTP tracks. Issue #337 should wait for #333 to finalize the `TokenRefreshError` type.

---

## References

- [ADR-0009: JWT Authentication Middleware](0009-jwt-authentication-middleware.md) — Auth layer context
- [ADR-0010: OAuth2 Provider Architecture](0010-oauth2-provider-architecture.md) — Provider config and token refresh design
- [ADR-0015: HTTP Outbound Integration](0015-http-outbound-integration.md) — HTTP client design
- [ADR-0016: Redacted Type for Sensitive Data](0016-redacted-type-for-sensitive-data.md) — PII handling patterns
- [ADR-0018: HTTP Localhost Exception for OAuth2 Discovery](0018-http-localhost-exception-for-oauth2-discovery.md) — Why `getRaw` must remain for localhost
- [ADR-0028: Structured Logging](0028-structured-logging.md) — Audit logging (separate from #337)
- `core/auth/Auth/SecretStore.hs` — Current SecretStore interface
- `core/auth/Auth/OAuth2/TokenRefresh.hs:67-107` — `withValidToken` with TOCTOU
- `core/auth/Auth/OAuth2/Types.hs:328-378` — `validateState` / `validateRedirectUri`
- `core/auth/Auth/OAuth2/StateToken.hs` — `BA.constEq` usage (line 265); `constEq` to be added
- `core/http/Http/Client.hs:264-284` — `getRaw` implementation (to move to Internal)
- `integrations/Integration/Oura/Internal.hs:622` — `mapTokenError` with userId leak
- `core/service/Integration.hs:111-117` — `ActionContext` with bare `Map`
