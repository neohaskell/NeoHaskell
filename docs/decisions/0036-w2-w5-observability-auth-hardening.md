# ADR-0036: W2+W5 Observability and Auth Hardening

## Status

Proposed

## Context

Two clusters of production-readiness issues have accumulated across the service and auth layers. Left unaddressed, they cause silent data loss, misleading logs, and token-related outages in production.

**Wave 2 (Silent Failures / Observability)** covers eight issues where errors are swallowed, logs are absent or misleading, and the LISTEN/NOTIFY connection can silently drop without recovery. Operators have no visibility into what the service is doing or why it failed.

**Wave 5 (Auth Hardening)** covers six issues where OAuth2 tokens expire without proactive refresh, multiple concurrent requests trigger a thundering herd of token fetches, and there is no encrypted-at-rest secret store or GDPR audit trail for auth events.

These two waves are batched into a single ADR because they share a cross-cutting theme: the system currently fails silently and without recovery. Both waves add structured observability and self-healing behaviour to existing infrastructure rather than introducing new domain concepts.

### Use Cases

**W2-A: LISTEN/NOTIFY Reliability (#397, #405)**
- A PostgreSQL LISTEN/NOTIFY connection drops due to a network blip. The service should detect the drop, reconnect with exponential backoff, and replay any missed notifications. Currently it silently stops receiving events.
- After a clean shutdown, the service logs "exited unexpectedly" (#405). Operators see a false alarm on every deploy.

**W2-B: Structured Error Propagation (#399, #246, #247, #401, #323, #254)**
- A command handler fails to insert an event. The `CommandExecutor` swallows the error and returns success (#399). The caller has no idea the write failed.
- The service runner catches a worker crash but logs nothing and continues (#246). Operators cannot tell which worker died or why.
- `runWith` has no error handling at all (#247). Any exception propagates as an unstructured panic.
- Failure points throughout the service layer emit no structured log entries (#401). Correlation IDs are absent.
- The event dispatcher sends every event to every handler regardless of type (#323). Handlers receive events they never subscribed to.
- The inbound worker's backoff timer never resets after a successful poll (#254). After one failure the worker permanently polls at the maximum interval.

**W5-A: Token Lifecycle Management (#297, #326, #341, #296)**
- A cached OAuth2 token expires mid-request. The service returns 401 to the caller instead of refreshing transparently (#297, #341).
- Ten concurrent requests all find an expired token and simultaneously trigger a refresh (#326). The token provider receives ten parallel refresh calls, often rate-limiting the service.
- There is no component that owns the token lifecycle (#296). Each call site manages its own refresh logic inconsistently.

**W5-B: Compliance and Persistence (#336, #294)**
- Auth events (token issued, token refreshed, token revoked) are not logged (#336). GDPR audit requirements cannot be met.
- Secrets are stored in memory only (#294). A process restart loses all cached tokens. There is no encrypted-at-rest backend.

### Requirements

**W2 Requirements**
- LISTEN/NOTIFY must reconnect automatically after a connection drop, with configurable backoff
- Keepalive pings must detect a stale connection before the OS TCP timeout fires
- Clean shutdown must not log "exited unexpectedly"
- `CommandExecutor` must surface insert errors as `Result` failures, not silently discard them
- Service runner must log structured errors when a worker crashes, including the worker name and exception
- `runWith` must catch and log all exceptions with a structured entry before re-raising
- All failure points must emit a structured log entry with a correlation ID
- The event dispatcher must filter events by handler subscription, not broadcast all events
- The inbound worker backoff must reset to the minimum interval after a successful poll

**W5 Requirements**
- `SecretStore` must track token TTL and expiration timestamps
- A `TokenManager` must own the token lifecycle: proactive refresh before expiry, single-flight deduplication of concurrent refresh calls
- Token refresh must be triggered proactively (e.g. 60 seconds before expiry) rather than reactively on 401
- Concurrent callers waiting for a refresh must all receive the same new token without triggering multiple refresh requests
- Auth events must be written to a structured audit log with timestamp, actor, and event type
- A `PostgresSecretStore` must persist tokens encrypted at rest using AES-256-GCM

## Decision

### W2-A: LISTEN/NOTIFY Keepalive and Reconnect

Introduce a `NotificationConnection` type that wraps the raw PostgreSQL connection with a keepalive loop and a reconnect supervisor. The supervisor runs in a separate `AsyncTask` and monitors the connection via a heartbeat channel. On drop, it reconnects with exponential backoff capped at a configurable maximum.

The false "exited unexpectedly" log (#405) is fixed by distinguishing clean shutdown signals from unexpected exits in `Internal.hs`.

### W2-B: Structured Error Propagation

`CommandExecutor` wraps the insert in a `Result`-returning function. Any database error becomes a `CommandError` and is returned to the caller. No silent discard.

The service runner gains a `WorkerSupervisor` that catches exceptions from each worker, logs a structured entry (worker name, exception, timestamp, correlation ID), and decides whether to restart or halt based on a configurable restart policy.

`runWith` gains a top-level exception handler that logs a structured `ServicePanic` entry before re-raising.

Structured logging uses the `Logger` from ADR-0028. Every failure site calls `Logger.error` with a `LogEntry` record containing at minimum `correlationId`, `component`, and `message`.

The dispatcher gains a subscription registry. Each handler registers the event types it handles. The dispatcher routes each event only to handlers whose subscription matches the event's type tag.

The inbound worker resets its backoff counter to the initial interval on every successful poll.

### W5-A: TokenManager with Single-Flight Refresh

Introduce `Auth.OAuth2.TokenManager`, a long-lived component that:

1. Holds the current token and its expiry in a `ConcurrentMap`
2. Runs a background `AsyncTask` that wakes 60 seconds before expiry and calls the refresh endpoint
3. Uses a `Lock` to ensure only one refresh runs at a time; concurrent callers block on the lock and receive the refreshed token when it completes

`SecretStore` gains `expiresAt :: Timestamp` and `ttl :: Duration` fields. The `TokenManager` reads these to schedule proactive refresh.

### W5-B: Audit Logging and PostgresSecretStore

`Auth.OAuth2.TokenRefresh` emits a structured `AuthAuditEvent` on every token issuance, refresh, and revocation. The audit event includes `actorId`, `eventType`, `tokenId` (hashed), and `occurredAt`.

`Auth.SecretStore.Postgres` implements the `SecretStore` typeclass backed by a PostgreSQL table. Secrets are encrypted with AES-256-GCM before write and decrypted on read. The encryption key is loaded from the application config, never hardcoded.

### Type Definitions

```haskell
-- W2-A: LISTEN/NOTIFY

data NotificationConnection = NotificationConnection
  { rawConnection :: !PostgresConnection
  , heartbeatChannel :: !(Channel HeartbeatSignal)  -- Bounded size 10
  , reconnectConfig :: !ReconnectConfig
  }

data ReconnectConfig = ReconnectConfig
  { initialBackoff :: !Duration
  , maxBackoff :: !Duration
  , backoffMultiplier :: !Double
  }

-- Smart constructor with validation
mkReconnectConfig :: Duration -> Duration -> Double -> Result Text ReconnectConfig

data HeartbeatSignal
  = HeartbeatOk
  | HeartbeatFailed Text

-- W2-B: Command and Worker errors

data CommandError
  = InsertFailed Text
  | SerializationFailed Text
  | UnexpectedCommandError Text

data WorkerCrash = WorkerCrash
  { workerName :: !Text
  , exception :: !Text
  , correlationId :: !Text
  , occurredAt :: !Timestamp
  }

data RestartPolicy
  = AlwaysRestart
  | RestartUpTo Int
  | NeverRestart

-- W2-B: Dispatcher subscription

data HandlerSubscription event = HandlerSubscription
  { handledTypes :: !(Array TypeTag)
  , handler :: event -> Task Text Unit
  }

-- Dispatcher sealed at startup, uses Map for O(1) routing

-- W5-A: Token lifecycle

data TokenEntry = TokenEntry
  { accessToken :: !(Redacted Text)
  , refreshToken :: !(Maybe (Redacted Text))
  , expiresAt :: !Timestamp
  , ttl :: !Duration
  }

data TokenManagerConfig = TokenManagerConfig
  { proactiveRefreshLeadTime :: !Duration
  , maxRefreshRetries :: !Int
  , refreshBackoff :: !ReconnectConfig
  }

-- Smart constructor with validation
mkTokenManagerConfig :: Duration -> Int -> ReconnectConfig -> Result Text TokenManagerConfig

data TokenRefreshResult
  = TokenRefreshed TokenEntry
  | TokenRefreshFailed Text

-- W5-B: Audit and encrypted store

data AuthAuditEvent = AuthAuditEvent
  { actorId :: !Text  -- Pseudonymous internal ID
  , eventType :: !AuthEventType
  , tokenId :: !Text  -- SHA-256 hash of token
  , occurredAt :: !Timestamp
  }

data AuthEventType
  = TokenIssued
  | TokenRefreshed
  | TokenRevoked

data EncryptionConfig = EncryptionConfig
  { keyId :: !Text
  , algorithm :: !Text  -- "AES-256-GCM"
  , key :: !(Redacted Text)  -- Wrapped to prevent logging
  }
```

### Instances/Functions Required

**W2-A** (`core/service/Service/EventStore/Postgres/Notifications.hs`, `Internal.hs`)
- `Notifications.connect :: ReconnectConfig -> PostgresPool -> Task Text NotificationConnection`
  - Creates bounded heartbeat channel (size 10)
- `Notifications.listen :: NotificationConnection -> Channel -> Task Text Unit`
- `Notifications.startKeepalive :: NotificationConnection -> Task Text Unit`
- `Internal.isCleanShutdown :: ShutdownReason -> Bool`

**W2-B** (`core/service/Service/CommandExecutor.hs`, `Application.hs`, `Integration/Dispatcher.hs`)
- `CommandExecutor.execute :: Command event -> Task CommandError Unit` (replaces fire-and-forget)
  - {-# INLINE #-} pragma for performance
- `Application.WorkerSupervisor.supervise :: RestartPolicy -> Array (Task Text Unit) -> Task Text Unit`
- `Application.runWith :: Application -> Task Text Unit` (gains top-level exception handler)
- `Dispatcher.register :: HandlerSubscription event -> Dispatcher event -> Dispatcher event`
- `Dispatcher.dispatch :: event -> Dispatcher event -> Task Text Unit`
  - {-# INLINE #-} pragma for O(1) performance
- `Integration.InboundWorker.resetBackoff :: WorkerState -> WorkerState`

**W5-A** (`core/auth/Auth/SecretStore.hs`, new `core/auth/Auth/OAuth2/TokenManager.hs`)
- `SecretStore.get :: TokenKey -> Task Text (Maybe TokenSet)` (unchanged interface)
- `SecretStore.atomicModify :: TokenKey -> (Maybe TokenSet -> Maybe TokenSet) -> Task Text Unit`
- `TokenManager.new :: SecretStore -> TokenManagerConfig -> Task Text TokenManager`
- `TokenManager.getToken :: TokenManager -> Text -> Text -> (RefreshToken -> Task OAuth2Error TokenSet) -> Task (TokenRefreshError Text) TokenSet`
  - {-# INLINE #-} on `needsProactiveRefresh` for hot path
- `TokenManager.startRefreshLoop :: TokenManager -> Text -> Text -> (RefreshToken -> Task OAuth2Error TokenSet) -> Task Text Unit`

**W5-B** (`core/auth/Auth/OAuth2/TokenRefresh.hs`, new `core/auth/Auth/SecretStore/Postgres.hs`)
- `TokenRefresh.withAuditLog :: Logger -> TokenRefresh -> TokenRefresh`
- `PostgresSecretStore.new :: EncryptionConfig -> PostgresPool -> Task Text SecretStore`
- `PostgresSecretStore.encrypt :: EncryptionConfig -> Text -> Task Text ByteString`
  - Fresh 96-bit random IV per encryption
- `PostgresSecretStore.decrypt :: EncryptionConfig -> ByteString -> Task Text Text`
  - In-memory cache layer for performance

## Consequences

### Positive

- Operators gain full visibility into service failures. Every error site emits a structured log entry with a correlation ID, making incidents traceable.
- Dispatcher registration is sealed at startup. No runtime registration allowed.
- LISTEN/NOTIFY connections self-heal. A network blip no longer silently stops event delivery.
- `CommandExecutor` failures surface immediately as typed errors. Callers can handle or escalate them rather than assuming success.
- Token expiry is handled proactively. End users no longer see 401 errors caused by a stale cached token.
- JWT tokens are validated after every refresh (signature, issuer, audience).
- Concurrent token refresh is deduplicated. The thundering herd problem is eliminated at the source.
- Auth events are auditable. GDPR compliance requirements can be met without additional instrumentation.
- Correlation IDs are sanitized at HTTP boundary (newlines stripped).
- Secrets survive process restarts when `PostgresSecretStore` is configured.
- The dispatcher no longer sends irrelevant events to handlers. Handler logic is simpler and more predictable.
- All hot-path data types use strict fields to prevent space leaks.

- Dispatcher uses Map-based O(log n) routing instead of O(n) handler scan.
### Negative

- `CommandExecutor.execute` is a breaking change for callers that currently ignore the return value. All call sites must be updated to handle `Task CommandError Unit`.
- `TokenManager` adds a background `AsyncTask` per OAuth2 provider. Long-running services with many providers will have more goroutine-equivalent overhead.
- `PostgresSecretStore` requires a migration to create the secrets table and manage the encryption key. Key rotation is not in scope for this ADR.
- The dispatcher subscription registry adds a small amount of per-handler registration boilerplate.

### Risks

- **Risk**: IV reuse in AES-256-GCM would be catastrophic.
  - **Mitigation**: Generate fresh random 96-bit IV per encryption using cryptographically secure RNG. Store IV with ciphertext.
- **Risk**: The encryption key for `PostgresSecretStore` must be kept out of source control and rotated periodically. If the key is lost, all stored tokens are unrecoverable.
  - **Mitigation**: Load the key exclusively from the application config (environment variable or secrets manager). Document key rotation as a follow-up ADR.

- **Risk**: The `TokenManager` refresh loop could mask a permanently broken refresh endpoint by retrying indefinitely.
  - **Mitigation**: `TokenManagerConfig.maxRefreshRetries` caps the retry count. After exhaustion, `getToken` returns a `TokenRefreshFailed` error and the manager stops the loop.

- **Risk**: Changing `CommandExecutor` to return `Result` errors could cause callers to silently ignore the new error type if they pattern-match only on the success case.
  - **Mitigation**: The `CommandError` type is non-empty and the compiler will warn on incomplete pattern matches when `-Wincomplete-patterns` is enabled (already set in the project).

- **Risk**: The dispatcher subscription registry could be misconfigured, causing a handler to never receive events it expects.

- **Risk**: Smart constructors not used could allow invalid configurations.
  - **Mitigation**: Export only smart constructors (mkReconnectConfig, mkTokenManagerConfig) that validate invariants.
  - **Mitigation**: Add a startup check that logs a warning if a registered handler has an empty subscription list.

## References

- [Issue #397](https://github.com/neohaskell/NeoHaskell/issues/397) - LISTEN/NOTIFY keepalive and reconnect
- [Issue #405](https://github.com/neohaskell/NeoHaskell/issues/405) - False "exited unexpectedly" log on clean shutdown
- [Issue #399](https://github.com/neohaskell/NeoHaskell/issues/399) - CommandExecutor swallows insert errors
- [Issue #246](https://github.com/neohaskell/NeoHaskell/issues/246) - Service runner failures silently ignored
- [Issue #247](https://github.com/neohaskell/NeoHaskell/issues/247) - No error handling in runWith
- [Issue #401](https://github.com/neohaskell/NeoHaskell/issues/401) - No structured logging at failure points
- [Issue #323](https://github.com/neohaskell/NeoHaskell/issues/323) - Dispatcher sends all events to all handlers
- [Issue #254](https://github.com/neohaskell/NeoHaskell/issues/254) - Inbound worker backoff never resets
- [Issue #297](https://github.com/neohaskell/NeoHaskell/issues/297) - Token TTL/expiration in SecretStore
- [Issue #326](https://github.com/neohaskell/NeoHaskell/issues/326) - OAuth2 thundering herd / single-flight
- [Issue #341](https://github.com/neohaskell/NeoHaskell/issues/341) - No proactive token refresh
- [Issue #296](https://github.com/neohaskell/NeoHaskell/issues/296) - No TokenManager for auto refresh
- [Issue #336](https://github.com/neohaskell/NeoHaskell/issues/336) - No GDPR audit logging for auth
- [Issue #294](https://github.com/neohaskell/NeoHaskell/issues/294) - No PostgresSecretStore with encryption at rest
- [ADR-0028: Structured Logging](0028-structured-logging.md) - Logger used by all W2 failure sites
- [ADR-0010: OAuth2 Provider Architecture](0010-oauth2-provider-architecture.md) - Foundation for W5 token management
- [ADR-0016: Redacted Type for Sensitive Data](0016-redacted-type-for-sensitive-data.md) - Used for token fields in TokenEntry
