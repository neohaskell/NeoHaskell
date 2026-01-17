# JWT Auth Middleware — Implementation Plan (ADR-0009)

This plan implements `docs/decisions/0009-jwt-authentication-middleware.md` with explicit design for **50,000 requests/second**.

At this QPS, the only viable design is:

- **No network I/O in the request path**
- **Lock-free-ish reads of cached keys**
- **Coalesced refresh** (avoid thundering herds)
- **Stale-while-revalidate** with circuit breaking

---

## 1. Architecture Overview

### 1.1 Hot path vs control plane

**Hot path (per request):**
- Read-only access to a snapshot of cached keys
- JWT parsing + signature verification
- Claim validation + `AuthOptions` authorization

**Control plane (background):**
- OIDC discovery (`.well-known/openid-configuration`)
- JWKS fetch + cache refresh
- Backoff + jitter + circuit breaker
- Key rotation overlap management

This separation is what keeps tail latency stable at 50k req/s.

### 1.2 Key caching strategy (stale-while-revalidate)

- Maintain **current keys** and **retired keys**.
- **Current keys** are used first.
- **Retired keys** are kept for a configured overlap window so tokens signed before rotation remain valid.
- When refresh fails, keep serving **stale** keys up to a configured `maxStaleSeconds`.

### 1.3 Concurrency primitives (NeoHaskell)

- `AtomicVar` — stores a single immutable **snapshot** of JWKS state; request path uses `AtomicVar.peek` (non-blocking).
- `Channel` (bounded) — refresh trigger queue; request path uses `Channel.tryWriteWithTimeout` with `0ms` to avoid blocking.
- `AsyncTask` — runs background refresh loop.
- `ConcurrentMap` — optional: track per-`kid` "missing key cooldown" to avoid repeated refresh enqueues when a new key appears.

### 1.4 Configuration defaults (reasonable for most IdPs)

- Scheduled refresh interval: **15 minutes** (industry default seen across Kafka/Dapr/etc.)
- Missing-key expedited refresh cooldown: **60 seconds** per `kid`
- Backoff: **50ms initial**, exponential, cap **5s**, jitter ±25%
- Circuit breaker:
  - trip after **5 failures**
  - open for **30s**, then half-open with **1 trial request**
- Stale serving (`stale-if-error`): allow stale keys for **24h** (configurable)
- Clock skew: default **60s** (from ADR)

### 1.5 Algorithm recommendation

**IMPORTANT**: At 50k req/s, algorithm choice significantly impacts CPU requirements:

| Algorithm | Verify Time (p50) | CPU cores for 50k req/s |
|-----------|-------------------|-------------------------|
| ES256     | ~250µs            | ~12-15 cores            |
| EdDSA     | ~100µs            | ~5-8 cores              |
| RS256     | ~1,000µs          | ~50+ cores              |

**Recommendation**: Strongly prefer **ES256 or EdDSA** for this throughput target. RS256 is supported but requires significantly more compute resources.

---

## 2. Component Design

> Naming follows ADR-0009; new modules are introduced only where necessary for correctness/performance.

### 2.1 `Auth.Claims` (public)
**Responsibility:** `UserClaims` type exposed to application code.

**Concurrency:** none.

### 2.2 `Auth.Options` (public)
**Responsibility:** `AuthOptions` DSL for endpoint requirements.

**Concurrency:** none.

### 2.3 `Auth.Error` (public-ish)
Split error space into:

- `AuthError` (request-facing): token missing/malformed/expired/insufficient permissions, auth infra unavailable, etc.
- `DiscoveryError` / `JwksError` (control-plane): discovery/JWKS fetch/parse failures.

**Concurrency:** none.

**Security (RFC 6750 alignment):**
- Error responses use generic messages to prevent information leakage
- Do NOT expose internal details like "kid missing" vs "signature invalid" in response body
- Use `WWW-Authenticate: Bearer error="invalid_token"` header format

### 2.4 `Auth.Config` (internal)
**Responsibility:** immutable settings discovered (issuer/jwksUri) + overrides.

Add operational settings:
- `refreshIntervalSeconds`
- `missingKidCooldownSeconds`
- `maxStaleSeconds`
- `circuitBreaker` thresholds
- `requestTimeoutMs` for discovery/JWKS fetch
- `allowedAlgorithms` — explicit allowlist (MUST include, see RFC 8725)

**Concurrency:** config is immutable.

### 2.5 `Auth.Discovery` (internal)
**Responsibility:**
- Fetch discovery document
- Parse `{ issuer, jwks_uri }`

**Concurrency:** called only by background manager.

**Stability:** implement timeouts; convert errors into typed `DiscoveryError`.

**HTTP caching:** Cache discovery results; respect `Cache-Control` / `max-age` headers.

### 2.6 `Auth.Jwks` (internal, NEW)
This is the core for 50k req/s.

#### State model
Store key material in an `AtomicVar` for hot path, and refresh state in a separate `AtomicVar` to reduce contention.

```haskell
-- Auth/Jwks.hs

data KeyMaterial = KeyMaterial
  { currentKeysByKid :: Map Text Jose.JWK,
    retiredKeysByKid :: Map Text Jose.JWK,
    currentFetchedAt :: Int64,        -- Unix timestamp (seconds)
    retiredExpiresAt :: Int64         -- Unix timestamp (seconds)
  }

-- Tracks cooldown for missing kids to prevent herd
-- (bounded size with LRU eviction; only used on misses)
data MissingKidCache = MissingKidCache
  { nextAllowedByKid :: Map Text Int64,
    insertionOrder :: Array Text,      -- For LRU eviction
    maxEntries :: Int
  }

-- Simplified circuit breaker (no halfOpenTrialInFlight for single worker)
data Circuit = Circuit
  { consecutiveFailures :: Int,
    nextAttemptAt :: Int64             -- Unix timestamp (seconds)
  }

-- Refresh state (separate from hot-path key material)
data RefreshState = RefreshState
  { missingKids :: MissingKidCache,
    circuit :: Circuit,
    lastSuccessAt :: Maybe Int64,
    lastError :: Maybe Text,
    refreshInProgress :: Bool          -- Single-flight guard
  }

-- Hot-path snapshot (read by request path)
newtype KeySnapshot = KeySnapshot KeyMaterial

-- Full manager state
data JwksManager = JwksManager
  { keySnapshot :: AtomicVar KeySnapshot,    -- Hot path reads this
    refreshState :: AtomicVar RefreshState,  -- Control plane uses this
    refreshChannel :: Channel RefreshSignal
  }
```

**Time representation:** All timestamps are **Int64** representing **seconds since Unix epoch (UTC)**. Never use monotonic time for JWT `exp`/`nbf` comparisons.

#### Refresh trigger channel
- Create `Channel RefreshSignal` with a small bounded capacity (e.g. 4).
- Request path enqueues with `Channel.tryWriteWithTimeout 0`.
- Background loop reads and refreshes; multiple signals coalesce naturally.

#### Refresh worker (single-flight)
Only one refresh executes at a time because only one loop performs the fetch.

Guard with `refreshInProgress :: Bool` inside `RefreshState`:
- Background loop sets it before fetching and clears after
- This prevents overlapping refresh runs

#### Missing-key expedited refresh
On `kid` miss:

1. Read `KeySnapshot` from `AtomicVar.peek`.
2. If `missingKids.nextAllowedByKid[kid] <= now`:
   - Update the `missingKids` map with `nextAllowed = now + cooldown` using `AtomicVar.modify`.
   - Apply LRU eviction if `maxEntries` exceeded (prevents memory pressure under `kid` spray attack).
   - Enqueue refresh signal (non-blocking).
3. Otherwise do nothing (refresh already scheduled recently).

This is the Kafka-style missing-key cooldown that prevents a thundering herd during rotation.

#### Key rotation correctness (overlap) — FIXED

**CRITICAL**: Do NOT blindly move all `currentKeysByKid → retiredKeysByKid` on every refresh.

On successful JWKS refresh with fetched keys `newKeys`:

```haskell
rotateKeys :: KeyMaterial -> Map Text Jose.JWK -> Int64 -> Int64 -> KeyMaterial
rotateKeys old newKeys now overlapSeconds =
  let
    -- New current = exactly what the IdP published
    newCurrent = newKeys
    -- New retired = (old current + old retired) minus keys still in newKeys
    -- This preserves keys that were recently removed from publication
    combinedOld = Map.union old.currentKeysByKid old.retiredKeysByKid
    newRetired = Map.difference combinedOld newKeys
  in
    KeyMaterial
      { currentKeysByKid = newCurrent,
        retiredKeysByKid = newRetired,
        currentFetchedAt = now,
        retiredExpiresAt = now + overlapSeconds
      }
```

This ensures:
- Keys still published by IdP are always in `currentKeysByKid`
- Keys recently removed are moved to `retiredKeysByKid` for overlap window
- No false 401s during routine refresh when IdP publishes overlapping keys

Request path tries:
1. current keys
2. retired keys (only if `now <= retiredExpiresAt`)

#### Stale serving and max staleness
When refresh fails:
- Keep serving `currentKeysByKid` until `now > currentFetchedAt + maxStaleSeconds`.
- After that point, protected endpoints must return **503** (auth infra unavailable).

#### HTTP caching for JWKS fetch
- Respect `Cache-Control` / `max-age` headers from JWKS endpoint
- Use `If-None-Match` / `ETag` to minimize bandwidth on unchanged keys
- Enforce TLS validation; reject unsafe redirects

### 2.7 `Auth.Jwt` (internal)
**Responsibility:** verify token using cached key material.

**Performance rules (hot path):**
- Parse JWT exactly once; avoid repeated Base64 decoding.
- Extract `kid` early; if missing, short-circuit.
- Key lookup before doing any expensive crypto.

**RFC 8725 JOSE header hardening (CRITICAL):**
- **Explicit algorithm allowlist**: Reject `alg=none` and any algorithm not in `allowedAlgorithms`
- **`crit` header handling**: Reject tokens with `crit` headers containing unsupported extensions
- **`typ` header validation**: If configured, require specific type (e.g., `at+jwt` for access tokens)
- **Key/algorithm binding**: Verify JWK is compatible with claimed `alg`:
  - RSA key required for RS256/RS384/RS512
  - EC key with correct curve for ES256 (P-256), ES384 (P-384), ES512 (P-521)
  - OKP key for EdDSA

**Correctness rules (validation order):**
1. Parse JWT format
2. Reject `alg` not in allowlist (fail fast, no crypto)
3. Reject unknown/unsupported `crit` headers
4. Validate `typ` if required
5. Extract `kid` (if missing, short-circuit)
6. Key lookup (verify key type matches `alg`)
7. Signature verification (CPU-heavy step)
8. Issuer validation (exact match, not "contains")
9. Audience validation (`aud` can be string or array; handle both)
10. Time claims (`exp`/`nbf`) with clock skew
11. Claims extraction

This ordering avoids CPU waste on obviously invalid tokens.

### 2.8 `Auth.Middleware` (internal)
**Responsibility:**
- apply `AuthOptions` to request
- produce `Maybe UserClaims`
- map errors to HTTP responses

**Concurrency:** request-local logic + `AtomicVar.peek`.

**`kid` miss policy (CRITICAL):**

```haskell
handleKidMiss :: RefreshState -> Int64 -> AuthError
handleKidMiss state now =
  let
    keysAreFresh = case state.lastSuccessAt of
      Just lastSuccess -> now - lastSuccess < freshnessThresholdSeconds
      Nothing -> False
    circuitHealthy = state.circuit.consecutiveFailures < circuitTripThreshold
  in
    if keysAreFresh && circuitHealthy
      then InvalidToken  -- 401: Keys are fresh, kid genuinely missing (forged or wrong token)
      else AuthInfraUnavailable  -- 503: Infra problem, not auth problem
```

This avoids returning incorrect 401s during transient rotation/outage.

### 2.9 `Service.Transport.Web` integration
**Responsibility:** call auth check before handler dispatch for both commands and queries.

**Concurrency:** unchanged; keep auth check synchronous but non-blocking.

---

## 3. Data Flow

### 3.1 Command request lifecycle

1. Route match in `Service/Transport/Web.hs`
2. If endpoint `AuthOptions == Everyone` → skip auth entirely
3. Extract `Authorization` header
4. Parse `Bearer <token>` prefix
5. Parse JWT header (extract `kid`, `alg`)
6. **RFC 8725 checks**: reject disallowed `alg`, unknown `crit`, validate `typ`
7. **Key/alg binding check**: verify key type matches algorithm
8. `Auth.Jwks.getKey`:
   - `AtomicVar.peek keySnapshot`
   - lookup in `currentKeysByKid`, then in `retiredKeysByKid`
   - if missing: apply `kid` miss policy (401 vs 503 based on health)
   - optionally schedule refresh (cooldown)
9. Verify signature (CPU-heavy)
10. Validate claims: issuer (exact match)/audience (string or array)/time
11. Extract `UserClaims`
12. Enforce permissions (`RequireAllPermissions` / `RequireAnyPermission`)
13. Call command handler with `Maybe UserClaims`

### 3.2 Query request lifecycle

Query endpoints use **default-deny** when auth is configured, and support **keyed + paginated** access only.

#### Single instance: `GET /queries/{name}/{id}`

1. Route match `GET /queries/{query-name}/{id}`
2. Lookup endpoint config for query
3. **Default-deny**: If auth configured and no explicit `withPublicQuery`, require auth
4. If public: call `PublicQueryHandler` with query ID
5. If protected: validate JWT (same as commands), call `AuthedQueryHandler` with `UserClaims` + query ID
6. Return single instance or 404

#### Paginated list: `GET /queries/{name}?cursor=&limit=`

1. Route match `GET /queries/{query-name}`
2. Verify list endpoint is enabled (`withQueryList`)
3. Parse pagination params; enforce max limit (default: 100)
4. Auth check (default-deny when auth configured)
5. If public: call `PublicListHandler` with `QueryParams`
6. If protected: call `AuthedListHandler` with `UserClaims` + `QueryParams`
7. Return paginated response with `nextCursor`

**Query handler types (split by auth requirement):**

```haskell
-- Query response with pagination
data QueryResponse = QueryResponse
  { items :: Text,         -- JSON array of results
    nextCursor :: Maybe Text,
    hasMore :: Bool
  }

-- Pagination parameters (enforced by framework)
data QueryParams = QueryParams
  { cursor :: Maybe Text,  -- Opaque cursor for pagination
    limit :: Int           -- Capped at maxQueryLimit (default: 100)
  }

-- Public handlers (no auth)
type PublicQueryHandler = Text -> Task Text QueryResponse        -- Keyed access by ID
type PublicListHandler = QueryParams -> Task Text QueryResponse  -- Paginated list

-- Authenticated handlers (UserClaims guaranteed present)
type AuthedQueryHandler = UserClaims -> Text -> Task Text QueryResponse        -- Keyed access
type AuthedListHandler = UserClaims -> QueryParams -> Task Text QueryResponse  -- Paginated list
```

**CRITICAL**: Protected handlers receive `UserClaims` directly (not `Maybe`). This prevents security bugs from "forgot to check" patterns.

**Query scoping**: Use `claims.sub` or `claims.tenantId` as part of the query lookup key—NOT post-fetch filtering via `getAll`.

### 3.3 Refresh lifecycle

- A background `AsyncTask` runs:
  - periodic tick (every `refreshIntervalSeconds`)
  - and/or reacts to `RefreshSignal` from the `Channel`

Refresh attempt:
- Check `refreshInProgress`; if true, skip (single-flight)
- If circuit open (`now < nextAttemptAt`): skip attempt, sleep until window expires
- Set `refreshInProgress = True`
- Fetch JWKS (with `If-None-Match` if we have an ETag)
- On success:
  - Apply key rotation logic (see 2.6)
  - Reset circuit (`consecutiveFailures = 0`)
  - Update `lastSuccessAt`
- On failure:
  - Increment `consecutiveFailures`
  - If >= threshold, set `nextAttemptAt = now + openDurationSeconds`
  - Apply exponential backoff with jitter for next scheduled attempt
- Set `refreshInProgress = False`

---

## 4. Error Handling Matrix

| Failure mode | Where | Behavior | HTTP Status | Notes |
|---|---|---|---|---|
| Discovery down at startup | startup | start in degraded mode | N/A | `Everyone` endpoints OK; protected endpoints 503 until keys loaded |
| JWKS down at startup | startup | start in degraded mode | N/A | background retries with backoff |
| Discovery/JWKS timeout | refresh worker | record failure; backoff | N/A | enforce timeouts always |
| JWKS unavailable during runtime | refresh worker | serve stale keys; refresh keeps retrying | N/A | until `maxStaleSeconds` exceeded |
| Circuit open | refresh worker | fail-fast; no network calls | N/A | avoids cascading failures |
| No keys ever loaded | request | protected endpoints fail | **503** | fail closed when auth required |
| Keys loaded but `kid` missing + keys fresh | request | schedule refresh (cooldown) | **401** | likely forged or wrong token |
| Keys loaded but `kid` missing + keys stale/unhealthy | request | schedule refresh (cooldown) | **503** | infra problem, not auth problem |
| Token malformed | request | no refresh | **401** | |
| `alg=none` or disallowed algorithm | request | reject immediately | **401** | RFC 8725 |
| Unknown `crit` header | request | reject immediately | **401** | RFC 8725 |
| Key type mismatch with `alg` | request | reject | **401** | prevents alg confusion |
| Signature invalid (key present) | request | no refresh | **401** | |
| Issuer mismatch | request | no refresh | **401** | exact match required |
| Audience mismatch | request | no refresh | **401** | handle string or array |
| exp/nbf invalid | request | no refresh | **401** | clock skew applied |
| Permissions insufficient | request | no refresh | **403** | |

---

## 5. Performance Budget

### 5.1 Target per-request overhead (p50)

- Header parse + route: **< 10µs**
- `AtomicVar.peek` + `Map.get kid`: **< 5µs**
- JWT header parse + RFC 8725 checks: **< 20µs**
- Claim validation + extraction (excluding signature): **< 50µs**
- Signature verification:
  - EdDSA: **< 100µs** (recommended)
  - ES256: **< 250µs** (recommended)
  - RS256: **< 1,000µs** (CPU-heavy at 50k req/s)

### 5.2 Cache efficiency targets

- Key lookup hit ratio: **> 99.99%**
- Refresh signal rate: **near zero** during steady state

### 5.3 Allocation rules (hot path)

- Never allocate `Jose.JWKSet` per request.
- Keep key maps as immutable structures swapped wholesale.
- Parse token once; avoid repeated splitting/decoding.

### 5.4 Connection pooling

JWKS/discovery calls are rare, but still:
- enforce timeouts
- reuse connections if `Http.Client` supports it via underlying manager
- use `If-None-Match` / `ETag` to minimize bandwidth

If `Network.HTTP.Simple.httpJSON` does not reuse connections adequately in practice, extend `Http.Client` to manage a shared `Manager`.

---

## 6. Security & Compliance

### 6.1 RFC 8725 compliance (JOSE hardening)

| Requirement | Implementation |
|---|---|
| Reject `alg=none` | Explicit allowlist check before any crypto |
| Algorithm allowlist | `allowedAlgorithms` in config; default: `["ES256", "ES384", "ES512", "EdDSA", "RS256", "RS384", "RS512"]` |
| Reject unknown `crit` | Parse and reject if any `crit` value not understood |
| Key/alg binding | Verify JWK `kty` + `crv` matches claimed `alg` |
| `typ` validation | Optional; if configured, enforce exact match |

### 6.2 GDPR & data protection

| Requirement | Implementation |
|---|---|
| **Never log tokens** | Tokens (including malformed ones) must NEVER be written to logs |
| **Never persist tokens** | Tokens exist only in memory during request processing |
| **Claims are personal data** | If logging `sub`, use separate audit log with strict retention |
| **Pseudonymization** | If `kid` or `sub` must be logged, hash/truncate |
| **Debug log safety** | Debug logs cannot be enabled in production without safeguards |

**Logging rules:**
- Log: request ID, timestamp, auth result (success/failure), error category (not details)
- Do NOT log: token, `sub`, `email`, `name`, full `kid`, raw claims

### 6.3 Error response security

All auth errors return generic messages to prevent information leakage:

```haskell
-- Good: Generic error
respondWithAuthError InvalidToken = (401, "Authentication failed")
respondWithAuthError AuthInfraUnavailable = (503, "Service temporarily unavailable")

-- Bad: Leaks internal state
respondWithAuthError (KidNotFound kid) = (401, "Key {kid} not found")  -- NEVER DO THIS
```

### 6.4 Audit requirements

For regulatory compliance, maintain an audit trail of:
- Authentication failures (rate, categories)
- Circuit breaker state changes
- Key refresh events (success/failure)

**Do NOT include** in audit trail:
- Token contents
- Personal data (sub, email)
- Specific `kid` values (hash if needed)

---

## 7. Testing Strategy

### 7.1 Unit tests
- `Auth.Jwt`:
  - valid token accepted
  - malformed token rejected
  - `alg=none` rejected (RFC 8725)
  - unknown `crit` rejected (RFC 8725)
  - key/alg mismatch rejected
  - issuer/audience mismatches (test both string and array `aud`)
  - exp/nbf with skew
- `Auth.Jwks`:
  - key rotation overlap keeps old tokens valid
  - rotation with overlapping keys does NOT cause false rejects
  - missing-key cooldown prevents repeated refresh scheduling
  - LRU eviction in `MissingKidCache` under `kid` spray
  - stale-if-error behavior up to `maxStaleSeconds`
  - circuit breaker transitions (closed → open → closed)
  - `kid` miss policy: 401 when fresh, 503 when unhealthy

### 7.2 Concurrency tests
Use `AsyncTask.runAllIgnoringErrors` to simulate:
- many parallel `getKey` lookups
- concurrent refresh signals

Assert:
- no deadlocks
- refresh count stays bounded (singleflight)
- request path doesn't block during refresh

### 7.3 Integration tests
- Fake OIDC server serving:
  - discovery
  - JWKS (with `ETag` support)
  - rotated JWKS on demand (with overlapping keys)

Validate:
- requests succeed before + after rotation
- requests with old tokens succeed during overlap
- rotation with overlapping keys causes no false rejects
- refresh failures do not break requests until staleness limit exceeded
- `kid` miss returns 503 (not 401) when JWKS endpoint is down

### 7.4 Query auth tests
- Verify default-deny when `withAuth` is configured
- Verify `withPublicQuery` allows unauthenticated access
- Verify protected query handlers receive `UserClaims` (not `Maybe`)
- Verify pagination enforces max limit
- Verify list endpoints require explicit `withQueryList`
- Verify keyed access works: `GET /queries/{name}/{id}`

### 7.5 Security tests
- Verify `alg=none` tokens rejected
- Verify algorithm confusion attacks rejected (RS256 key with ES256 alg)
- Verify tokens never appear in logs (even on parse failure)
- Verify error responses don't leak internal details
- Verify query endpoints don't expose cross-user data (scoping)

### 7.6 Load/bench tests
- micro-benchmark verify-only path
- measure throughput on ES256 vs RS256 vs EdDSA
- compute required cores for 50k req/s
- verify no memory growth under `kid` spray attack
- verify paginated queries stay bounded (no `getAll` hot path)

---

## 8. Phased Implementation

### Phase 1 — Public API + basic validation (Short)
- Implement modules from ADR: `Auth.Claims`, `Auth.Options`, `Auth.Error`.
- Implement `Auth.Jwt.validateToken` that validates given a key set.
- **Include RFC 8725 hardening from the start** (algorithm allowlist, `crit` rejection, key/alg binding).

### Phase 2 — JWKS manager (Medium)
- Implement `Auth.Jwks`:
  - Split state: `AtomicVar KeySnapshot` + `AtomicVar RefreshState`
  - bounded `Channel RefreshSignal`
  - background `AsyncTask` refresh loop with single-flight guard
  - missing-key cooldown with LRU eviction
  - backoff + jitter + simplified circuit breaker
  - **Correct key rotation logic** (preserve overlapping keys)
  - HTTP caching (`ETag`, `If-None-Match`)

### Phase 3 — Command middleware integration (Medium)
- Implement `Auth.Middleware.checkAuth` using `Auth.Jwks.getKey` + `Auth.Jwt`.
- Implement `kid` miss policy (401 vs 503 based on health).
- Wire into `Service/Transport/Web.hs` for command endpoints.

### Phase 4 — Query auth integration (Medium)
- Implement split handler types:
  - `PublicQueryHandler` / `PublicListHandler`
  - `AuthedQueryHandler` / `AuthedListHandler`
- Implement **default-deny** policy when `withAuth` is configured.
- Add query registration API:
  - `Application.withPublicQuery` (explicit public)
  - `Application.withQueryList` (enable paginated list)
  - `Application.withQueryAuth` (custom auth)
- Add `QueryParams` with cursor-based pagination.
- Enforce max limit (default: 100).
- Wire into `Service/Transport/Web.hs`:
  - `GET /queries/{name}/{id}` (keyed access)
  - `GET /queries/{name}?cursor=&limit=` (paginated, opt-in)

### Phase 5 — Application wiring (Short)
- Update `Application.withAuth` / `withAuthOverrides` to start JWKS manager at runtime.
- Implement startup policy (default degraded start).

### Phase 6 — Hardening + observability (Medium)
- Add metrics counters (refresh success/failure, key-miss count, circuit open time, **time since last refresh**).
- Add GDPR-compliant audit logging.
- Add benchmarks for ES256/EdDSA/RS256.
- Document "cores required" guidance for each algorithm at target QPS.

---

## 9. Risk Mitigation

| Risk | Mitigation |
|---|---|
| Thundering herd on rotation | bounded refresh channel + missing-key cooldown + single refresh loop + LRU eviction |
| OIDC/JWKS outage | stale-while-revalidate + circuit breaker + backoff + `kid` miss policy (503 vs 401) |
| Rejecting valid tokens during rotation | correct key rotation logic preserves overlapping keys; retain retired keys for overlap window |
| Algorithm confusion attacks | RFC 8725 compliance: explicit allowlist + key/alg binding |
| Clock skew issues | configurable `clockSkewSeconds` applied to `exp`/`nbf` |
| Hot path allocation creep | strict parsing, single decode, immutable snapshot swap |
| Crypto CPU saturation | benchmark early; document algorithm cost; recommend ES256/EdDSA |
| GDPR violations | never log tokens; pseudonymize claims; separate audit trail |
| Information leakage | generic error responses; RFC 6750 alignment |
| Memory pressure under attack | bounded `MissingKidCache` with LRU eviction |
| Accidental query exposure | default-deny when auth configured; require explicit `withPublicQuery` |
| `getAll` DoS / data leakage | no unbounded list endpoints; keyed + paginated access only |
| "Forgot to check auth" bugs | split handler types: `AuthedQueryHandler` receives `UserClaims` directly (not `Maybe`) |
| Unbounded query responses | framework-enforced max limit on pagination |

---

## 10. Escalation Triggers

Revisit architecture if:

- Must support **RS256** at sustained 50k rps on limited cores → consider edge/proxy JWT verification offload
- Multi-tenant / **multiple issuers** in same process → requires per-issuer caches
- **Replay protection** required → needs `jti` tracking / stateful verification

**Note on audit trails**: Domain events already provide immutable audit trails via event sourcing. Auth decisions (who accessed what, when) are naturally captured in command events. Operational metrics (JWKS refresh, circuit breaker) use standard observability, not the event store.

---

## 11. Multitenancy Design Notes (Future ADR)

This implementation is **single-tenant-safe**. For multitenancy:

| Aspect | Design Decision |
|--------|-----------------|
| Tenancy scope | Per-command and per-query (not per-application) |
| Type family | Both commands and queries use `IsMultiTenant` pattern |
| Tenant enforcement | Tenant-scoped endpoints require `tenantId` in claims; reject with 403 if missing |
| Global endpoints | Ignore `tenantId` even if present (prevents accidental scoping) |
| Auth ordering | Auth must come BEFORE multitenancy (provides trusted `tenantId`) |

**Why auth before multitenancy:**
1. Multitenancy enforcement depends on **trusted tenant identity** from JWT claims
2. Auth without multitenancy is useful for single-tenant apps and internal tools
3. Multitenancy without auth is not enforceable (tenant would be attacker-controlled)

The `tenantId` field in `UserClaims` is extracted but **not enforced** until the multitenancy ADR is implemented.
