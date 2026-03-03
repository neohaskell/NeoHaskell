# ADR-0036: Wave 1 Security Hardening

## Status

Proposed

## Context

A security audit of the NeoHaskell core libraries and integrations identified seven issues across authentication, HTTP transport, and integration error handling. These issues range from race conditions in token storage to information leakage in error messages.

This ADR covers all seven issues as a single wave of related security fixes. They share a common theme: the existing code is functionally correct under normal conditions but lacks the defensive depth needed for production security.

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
atomicModifyReturning = \tokenKey transform -> do
  let key = unwrapTokenKey tokenKey
  var <- store |> ConcurrentMap.getOrCreate key ConcurrentVar.new
  ConcurrentVar.modifyReturning var (\current -> transform current)
```

**Consequences**

Positive:
- Eliminates the TOCTOU race in token refresh
- The transform function receives the current value and returns both the new value and a result, making the pattern composable
- Future implementations (Postgres, Vault) can use their own atomic primitives (e.g., `UPDATE ... RETURNING`)

Negative:
- Adds a new required field to `SecretStore`, so all existing implementations must be updated
- The transform function runs inside the lock, so it must not block indefinitely

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

```haskell
-- New type for refresh context
data TokenRefreshContext = TokenRefreshContext
  { secretStore :: SecretStore
  , refreshLocks :: ConcurrentMap Text Lock
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

  -- Acquire per-key lock before any refresh attempt
  lock <- ctx.refreshLocks |> ConcurrentMap.getOrCreate lockKey Lock.new
  Lock.with lock (doWithValidToken ctx tokenKey action isUnauthorized)

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

**Consequences**

Positive:
- Concurrent 401s for the same user/provider result in exactly one refresh call
- The double-check pattern means threads that waited for the lock benefit from the refresh done by the first thread
- Per-key locks mean different users/providers don't block each other

Negative:
- Adds `ConcurrentMap` to the refresh context, which callers must construct
- Lock acquisition adds a small overhead on every token use (negligible compared to network I/O)
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

The codebase already has a constant-time comparison in `core/auth/Auth/OAuth2/StateToken.hs:265`. This fix applies the same pattern to `validateState` and `validateRedirectUri`.

**Proposed Solution**

Replace `==` with `constEq` in both validation functions:

```haskell
-- Import the existing constant-time comparison
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

**Consequences**

Positive:
- Eliminates timing side-channel from CSRF state and redirect URI validation
- Uses an existing, tested `constEq` implementation rather than introducing new crypto code
- No API changes; purely internal

Negative:
- Constant-time comparison is slightly slower than short-circuit equality (negligible for these string lengths)
- Low priority: exploiting this timing channel requires network-level measurement precision and many samples, making it a theoretical rather than practical risk in most deployments

---

### Issue #335: Secure HTTP Client / TLS Enforcement (URGENT)

**Location**: `core/http/Http/Client.hs:264-284`

**Problem**

The current `getRaw` and related functions accept any URL, including `http://` URLs. There is no minimum TLS version enforcement. This means:

- Integration code can accidentally call `http://` endpoints, sending credentials in plaintext
- An attacker who can influence the URL (e.g., via misconfigured provider config) can downgrade to unencrypted transport
- No defense against TLS 1.0/1.1 downgrade attacks

**Proposed Solution**

Add `Http.getSecure` as a new entry point that enforces HTTPS and a minimum TLS version of 1.2. The existing `getRaw` remains for internal use cases (e.g., localhost OAuth2 discovery per ADR-0018).

```haskell
-- New secure entry point
getSecure ::
  Request ->
  Task Error (Response Bytes)
getSecure options = do
  let urlText = options.url |> Maybe.withDefault ""
  case Text.startsWith "https://" urlText of
    False -> Task.throw (InvalidUrl [fmt|Secure requests require HTTPS. Got: {urlText}|])
    True -> do
      getRawWithTlsConfig minimumTls12Config options

-- TLS configuration (internal)
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

All integration code that calls external OAuth2 or API endpoints should migrate from `getRaw` to `getSecure`. The `getRaw` function remains available for the localhost exception documented in ADR-0018.

**Must NOT include**: Certificate pinning. This is deferred to a future ADR. Pinning requires per-provider configuration and a key rotation strategy that is out of scope for this wave.

**Consequences**

Positive:
- Prevents accidental plaintext HTTP calls from integration code
- Enforces TLS 1.2+ minimum, blocking downgrade attacks
- Clear error message when `http://` is used, making misconfiguration obvious

Negative:
- Existing callers using `getRaw` for external endpoints must migrate to `getSecure`
- The localhost exception (ADR-0018) must continue to use `getRaw` directly, which requires callers to be aware of the distinction

---

### Issue #338: Response Size Limits (IMPORTANT)

**Depends on**: #335

**Location**: `core/http/Http/Client.hs:264-284`

**Problem**

Neither `getRaw` nor `getSecure` cap the response body size. A malicious or misconfigured server can return an arbitrarily large response body, causing the process to exhaust memory. This is an OOM (out-of-memory) denial-of-service vector.

**Proposed Solution**

Add `withMaxResponseSize` as a `Request` modifier. The default limit is 10MB. The implementation reads the response body in chunks and aborts if the accumulated size exceeds the limit.

```haskell
-- Request modifier
withMaxResponseSize :: Int -> Request -> Request
withMaxResponseSize maxBytes options =
  options { maxResponseBytes = Just maxBytes }

-- Updated Request type
data Request = Request
  { url :: Maybe Text
  , headers :: Map Text Text
  , timeoutSeconds :: Maybe Int
  , maxRedirects :: Int
  , maxResponseBytes :: Maybe Int
  -- ^ Maximum response body size in bytes.
  -- 'Nothing' means no limit (not recommended for production).
  -- Default via 'request' is 10MB (10 * 1024 * 1024).
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

-- Error case
data Error
  = HttpError Text
  | InvalidUrl Text
  | Timeout
  | TlsError Text
  | ResponseTooLarge Int
  -- ^ Response exceeded the configured maximum size (bytes).
  deriving (Generic, Show)
```

The implementation reads the response body incrementally and throws `ResponseTooLarge` as soon as the limit is exceeded, rather than buffering the entire body first.

**Must NOT include**: Streaming response support. Streaming requires a different API shape (callbacks or conduit) and is out of scope for this wave.

**Consequences**

Positive:
- Prevents OOM from oversized responses
- Default 10MB limit is generous for API responses but blocks pathological cases
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

Remove `userId` from the error message. The `_userId` binding is kept (with underscore prefix) to document that the value exists but is intentionally not used in the message:

```haskell
mapTokenError :: TokenRefreshError OuraHttpError -> IntegrationError
mapTokenError err = case err of
  TokenNotFound _userId -> AuthenticationError "Token not found"
  RefreshTokenMissing -> AuthenticationError "Refresh token missing"
  RefreshFailed oauth2Err -> AuthenticationError [fmt|Refresh failed: #{toText oauth2Err}|]
  StorageError msg -> UnexpectedError [fmt|Storage error: #{msg}|]
  ActionFailed (OtherHttpError msg) -> NetworkError msg
  ActionFailed Unauthorized -> AuthenticationError "Unauthorized after refresh"
  ActionFailed (OuraRateLimited retryAfter) -> RateLimited retryAfter
```

**Must NOT include**: Internal audit logging for token lookup failures. Structured audit logging is covered by ADR-0028 and is separate work. This fix is purely about removing PII from error messages.

**Consequences**

Positive:
- Eliminates PII leakage from authentication error messages
- Minimal change: one line, no API surface changes
- The fix is self-documenting: `_userId` signals the value was considered and deliberately excluded

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
newtype ImmutableProviderRegistry
  = ImmutableProviderRegistry
      { getProviders :: Map Text ValidatedOAuth2ProviderConfig
      }

-- | Create a registry from a validated configuration map.
-- This is the only way to construct an 'ImmutableProviderRegistry'.
fromMap :: Map Text ValidatedOAuth2ProviderConfig -> ImmutableProviderRegistry
fromMap providers =
  ImmutableProviderRegistry { getProviders = providers }

-- | Look up a provider by name.
lookup :: Text -> ImmutableProviderRegistry -> Maybe ValidatedOAuth2ProviderConfig
lookup name registry =
  registry.getProviders |> Map.get name

-- | List all providers.
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

**Consequences**

Positive:
- Type-level guarantee that the provider registry cannot be modified after `Application.run` constructs it
- Callers use `ImmutableProviderRegistry.lookup` instead of `Map.get`, making the intent clear
- No runtime cost: `newtype` is erased at compile time

Negative:
- All existing call sites that use `Map.get` on `providerRegistry` must migrate to `ImmutableProviderRegistry.lookup`
- Adds a new module to the service layer

---

## Consequences

### Positive (across all issues)

1. **Eliminated TOCTOU race** (#334): Token refresh is now atomic. Concurrent 401s for the same user no longer cause redundant refresh calls or token loss.

2. **Single refresh per key** (#333): Per-key locking with double-check ensures exactly one refresh call reaches the upstream provider per concurrent burst.

3. **Timing attack mitigation** (#328): CSRF state and redirect URI validation no longer leak information through response timing.

4. **Enforced HTTPS** (#335): Integration code cannot accidentally call `http://` endpoints. Misconfiguration produces a clear error rather than silent plaintext transmission.

5. **OOM protection** (#338): Malicious or oversized responses are rejected before they exhaust memory.

6. **PII removed from errors** (#337): User IDs no longer appear in authentication error messages that may reach logs or API responses.

7. **Immutable registry** (#339): Provider configuration cannot be modified after startup, closing a defense-in-depth gap.

### Negative (across all issues)

1. **Migration burden**: Issues #334, #333, #335, #338, and #339 all change existing interfaces. Call sites must be updated.

2. **Increased complexity in auth layer**: The mutex and double-check pattern in #333 adds non-trivial concurrency logic that must be carefully maintained.

3. **Reduced diagnostic detail** (#337): Removing `userId` from error messages makes debugging token issues slightly harder without a correlated request ID.

4. **New module** (#339): `Integration.ProviderRegistry` adds a module that must be maintained and documented.

### Implementation Order

Given the internal dependencies:

1. **#334** (SecretStore atomicity) first
2. **#333** (Token refresh mutex) after #334
3. **#328** (Constant-time comparison) independently
4. **#335** (Secure HTTP client) independently
5. **#338** (Response size limits) after #335
6. **#337** (Error sanitization) independently
7. **#339** (Provider registry immutability) independently

Issues #328, #337, and #339 have no dependencies and can proceed in parallel with the auth and HTTP tracks.

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
- `core/http/Http/Client.hs:264-284` — `getRaw` implementation
- `integrations/Integration/Oura/Internal.hs:622` — `mapTokenError` with userId leak
- `core/service/Integration.hs:111-117` — `ActionContext` with bare `Map`
