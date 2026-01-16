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
- `ConcurrentMap` — optional: track per-`kid` “missing key cooldown” to avoid repeated refresh enqueues when a new key appears.

### 1.4 Configuration defaults (reasonable for most IdPs)

- Scheduled refresh interval: **15 minutes** (industry default seen across Kafka/Dapr/etc.)
- Missing-key expedited refresh cooldown: **60 seconds** per `kid`
- Backoff: **50ms initial**, exponential, cap **5s**, jitter ±25%
- Circuit breaker:
  - trip after **5 failures**
  - open for **30s**, then half-open with **1 trial request**
- Stale serving (`stale-if-error`): allow stale keys for **24h** (configurable)
- Clock skew: default **60s** (from ADR)

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

- `AuthError` (request-facing): token missing/malformed/expired/insufficient permissions, etc.
- `DiscoveryError` / `JwksError` (control-plane): discovery/JWKS fetch/parse failures.

**Concurrency:** none.

### 2.4 `Auth.Config` (internal)
**Responsibility:** immutable settings discovered (issuer/jwksUri) + overrides.

Add operational settings:
- `refreshIntervalSeconds`
- `missingKidCooldownSeconds`
- `maxStaleSeconds`
- `circuitBreaker` thresholds
- `requestTimeoutMs` for discovery/JWKS fetch

**Concurrency:** config is immutable.

### 2.5 `Auth.Discovery` (internal)
**Responsibility:**
- Fetch discovery document
- Parse `{ issuer, jwks_uri }`

**Concurrency:** called only by background manager.

**Stability:** implement timeouts; convert errors into typed `DiscoveryError`.

### 2.6 `Auth.Jwks` (internal, NEW)
This is the core for 50k req/s.

#### State model
Store a single snapshot in an `AtomicVar`.

```haskell
-- Auth/Jwks.hs

data KeyMaterial = KeyMaterial
  { currentKeysByKid :: Map Text Jose.JWK,
    retiredKeysByKid :: Map Text Jose.JWK,
    currentExpiresAt :: Int,
    retiredExpiresAt :: Int
  }

-- Tracks cooldown for missing kids to prevent herd
-- (bounded size; only used on misses)
data MissingKidCache = MissingKidCache
  { nextAllowedByKid :: Map Text Int,
    maxEntries :: Int
  }

-- Minimal circuit breaker
-- (kept small; no external dep)
data Circuit = Circuit
  { consecutiveFailures :: Int,
    openUntil :: Int,
    halfOpenTrialInFlight :: Bool
  }

-- Single snapshot used by request path

data State = State
  { keys :: KeyMaterial,
    missingKids :: MissingKidCache,
    circuit :: Circuit,
    lastSuccessAt :: Maybe Int,
    lastError :: Maybe Text
  }
```

#### Refresh trigger channel
- Create `Channel RefreshSignal` with a small bounded capacity (e.g. 4).
- Request path enqueues with `Channel.tryWriteWithTimeout 0`.
- Background loop reads and refreshes; multiple signals coalesce naturally.

#### Refresh worker (single-flight)
Only one refresh executes at a time because only one loop performs the fetch.

If you add a second mechanism (e.g., scheduled timer + signals), enforce singleflight via `AtomicVar`:

- `refreshInProgress :: Bool` inside state
- background loop sets it before fetching and clears after

This prevents overlapping refresh runs.

#### Missing-key expedited refresh
On `kid` miss:

1. Read `State` from `AtomicVar.peek`.
2. If `missingKids.nextAllowedByKid[kid] <= now`:
   - Update the `missingKids` map with `nextAllowed = now + cooldown` using `AtomicVar.modify`.
   - Enqueue refresh signal (non-blocking).
3. Otherwise do nothing (refresh already scheduled recently).

This is the Kafka-style missing-key cooldown that prevents a thundering herd during rotation.

#### Key rotation correctness (overlap)
On successful JWKS refresh:

- Move `currentKeysByKid` → `retiredKeysByKid`
- Install new `currentKeysByKid`
- Set `retiredExpiresAt = now + overlapSeconds`

Request path tries:
1. current keys
2. retired keys (only if `now <= retiredExpiresAt`)

This ensures tokens signed with old keys remain valid during rotation overlap.

#### Stale serving and max staleness
When refresh fails:
- Keep serving `currentKeysByKid` until `now > currentExpiresAt + maxStaleSeconds`.
- After that point, protected endpoints must return **503** (auth infra unavailable).

### 2.7 `Auth.Jwt` (internal)
**Responsibility:** verify token using cached key material.

**Performance rules (hot path):**
- Parse JWT exactly once; avoid repeated Base64 decoding.
- Extract `kid` early; if missing, short-circuit.
- Key lookup before doing any expensive crypto.

**Correctness rules:**
- Validate in order:
  1. format
  2. `alg` allowed
  3. `kid` present
  4. key lookup
  5. signature
  6. issuer/audience
  7. time claims (`exp`/`nbf`) with skew
  8. claims extraction

This ordering avoids CPU waste on obviously invalid tokens.

### 2.8 `Auth.Middleware` (internal)
**Responsibility:**
- apply `AuthOptions` to request
- produce `Maybe UserClaims`
- map errors to HTTP responses

**Concurrency:** request-local logic + `AtomicVar.peek`.

### 2.9 `Service.Transport.Web` integration
**Responsibility:** call auth check before handler dispatch.

**Concurrency:** unchanged; keep auth check synchronous but non-blocking.

---

## 3. Data Flow

### 3.1 Request lifecycle

1. Route match in `Service/Transport/Web.hs`
2. If endpoint `AuthOptions == Everyone` → skip auth entirely
3. Extract `Authorization` header
4. Parse `Bearer <token>` prefix
5. Parse JWT header (extract `kid`, `alg`)
6. `Auth.Jwks.getKey`:
   - `AtomicVar.peek state`
   - lookup in `currentKeysByKid`, then in `retiredKeysByKid`
   - if missing: optionally schedule refresh (cooldown) and fail 401/503 as appropriate
7. Verify signature (CPU-heavy)
8. Validate claims: issuer/audience/time
9. Extract `UserClaims`
10. Enforce permissions (`RequireAllPermissions` / `RequireAnyPermission`)
11. Call command handler with `Maybe UserClaims`

### 3.2 Refresh lifecycle

- A background `AsyncTask` runs:
  - periodic tick (every `refreshIntervalSeconds`)
  - and/or reacts to `RefreshSignal` from the `Channel`

Refresh attempt:
- If circuit open and not half-open: skip attempt, sleep until open window expires
- Fetch discovery (optional; can be periodic) and JWKS
- On success: swap keys + set expiry timestamps
- On failure: update circuit + schedule backoff with jitter

---

## 4. Error Handling Matrix

| Failure mode | Where | Behavior | Notes |
|---|---|---|---|
| Discovery down at startup | startup | start in degraded mode | `Everyone` endpoints OK; protected endpoints 503 until keys loaded |
| JWKS down at startup | startup | start in degraded mode | background retries with backoff |
| Discovery/JWKS timeout | refresh worker | record failure; backoff | enforce timeouts always |
| JWKS unavailable during runtime | refresh worker | serve stale keys; refresh keeps retrying | until `maxStaleSeconds` exceeded |
| Circuit open | refresh worker | fail-fast; no network calls | avoids cascading failures |
| No keys ever loaded | request | **503** for protected endpoints | fail closed when auth required |
| Keys loaded but `kid` missing in cache | request | **401** + schedule refresh (cooldown) | key miss indicates rotation or forged kid |
| Token malformed | request | **401** | no refresh |
| Signature invalid (key present) | request | **401** | do not refresh |
| Issuer/audience mismatch | request | **401** | do not refresh |
| exp/nbf invalid | request | **401** | clock skew applied |
| Permissions insufficient | request | **403** | do not refresh |

---

## 5. Performance Budget

### 5.1 Target per-request overhead (p50)

- Header parse + route: **< 10µs**
- `AtomicVar.peek` + `Map.get kid`: **< 5µs**
- JWT header parse: **< 15µs**
- Claim validation + extraction (excluding signature): **< 50µs**
- Signature verification:
  - ES256: **< 250µs**
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

If `Network.HTTP.Simple.httpJSON` does not reuse connections adequately in practice, extend `Http.Client` to manage a shared `Manager`.

---

## 6. Testing Strategy

### 6.1 Unit tests
- `Auth.Jwt`:
  - valid token accepted
  - malformed token rejected
  - issuer/audience mismatches
  - exp/nbf with skew
- `Auth.Jwks`:
  - key rotation overlap keeps old tokens valid
  - missing-key cooldown prevents repeated refresh scheduling
  - stale-if-error behavior up to `maxStaleSeconds`
  - circuit breaker transitions (closed → open → half-open → closed)

### 6.2 Concurrency tests
Use `AsyncTask.runAllIgnoringErrors` to simulate:
- many parallel `getKey` lookups
- concurrent refresh signals

Assert:
- no deadlocks
- refresh count stays bounded (singleflight)
- request path doesn’t block during refresh

### 6.3 Integration tests
- Fake OIDC server serving:
  - discovery
  - JWKS
  - rotated JWKS on demand

Validate:
- requests succeed before + after rotation
- requests with old tokens succeed during overlap
- refresh failures do not break requests until staleness limit exceeded

### 6.4 Load/bench tests
- micro-benchmark verify-only path
- measure throughput on ES256 vs RS256
- compute required cores for 50k req/s

---

## 7. Phased Implementation

### Phase 1 — Public API + basic validation (Short)
- Implement modules from ADR: `Auth.Claims`, `Auth.Options`, `Auth.Error`.
- Implement `Auth.Jwt.validateToken` that validates given a key set.

### Phase 2 — JWKS manager (Medium)
- Implement `Auth.Jwks`:
  - `AtomicVar State`
  - bounded `Channel RefreshSignal`
  - background `AsyncTask` refresh loop
  - missing-key cooldown
  - backoff + jitter + circuit breaker

### Phase 3 — Middleware integration (Medium)
- Implement `Auth.Middleware.checkAuth` using `Auth.Jwks.getKey` + `Auth.Jwt`.
- Wire into `Service/Transport/Web.hs` before handler dispatch.

### Phase 4 — Application wiring (Short)
- Update `Application.withAuth` / `withAuthOverrides` to start JWKS manager at runtime.
- Implement startup policy (default degraded start).

### Phase 5 — Hardening + observability (Medium)
- Add metrics counters (refresh success/failure, key-miss count, circuit open time).
- Add benchmarks.

---

## 8. Risk Mitigation

- **Thundering herd on rotation:** bounded refresh channel + missing-key cooldown + single refresh loop.
- **OIDC/JWKS outage:** stale-while-revalidate + circuit breaker + backoff.
- **Rejecting valid tokens during rotation:** retain retired keys for overlap window.
- **Clock skew issues:** configurable `clockSkewSeconds` applied to `exp`/`nbf`.
- **Hot path allocation creep:** strict parsing, single decode, immutable snapshot swap.
- **Crypto CPU saturation:** benchmark early; document algorithm cost (RS256 may require large CPU).

