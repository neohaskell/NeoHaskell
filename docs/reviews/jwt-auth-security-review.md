# JWT Authentication Middleware - Comprehensive Security Review

**Branch:** `feature/jwt-auth-middleware`  
**Review Date:** 2026-01-17  
**Reviewers:** Oracle Agent Swarm (5 parallel deep reviews)  
**Target:** 50k req/s, EU deployment readiness

---

## Executive Summary

The JWT authentication middleware implementation is **architecturally sound** with good separation between hot path (lock-free key lookups) and control plane (background refresh). The RFC 8725 JOSE hardening is correctly implemented for the core validation path.

However, the review identified **critical issues** that must be addressed before production deployment, particularly around:
1. SSRF protection gaps (DNS resolution not implemented)
2. Concurrency safety (refresh lock can be stranded on exceptions)
3. Performance blockers for 50k req/s target (per-request clock IO, crypto bottleneck)

### Issue Summary by Severity

| Severity | Count | Categories |
|----------|-------|------------|
| **CRITICAL** | 7 | SSRF bypass, concurrency races, base64 padding, crypto throughput |
| **HIGH** | 8 | URI comparison, exception safety, staleness semantics, error fidelity |
| **MEDIUM** | 12 | Token trimming, DoS limits, logging, GDPR minimization |
| **LOW** | 2 | Clock skew, allocation micro-optimizations |

---

## 1. CORRECTNESS & TYPE-SAFETY

### CRITICAL Issues

#### 1.1 ~~SSRF Bypass via Bracketed IPv6 Literals~~ FIXED
**File:** `core/auth/Auth/UrlValidation.hs`  
**Issue:** `isPrivateOrLoopback` parses IPv6 via `readMaybe @IP.IPv6 host`, but `Network.URI` commonly represents IPv6 hosts as `"[::1]"` in the authority. If `URI.uriRegName` includes brackets, `readMaybe` fails and the function returns `False`, allowing loopback/private IPv6.

**Fix:** ~~Normalize `host` before parsing (strip `[`/`]` when present) and add explicit tests for `https://[::1]/...`, `https://[fc00::1]/...`, `https://[fe80::1]/...`.~~

**Status:** FIXED - Added `stripIPv6Brackets` helper function to normalize bracketed IPv6 addresses before parsing.

#### 1.2 SSRF via DNS Resolution Gap
**File:** `core/auth/Auth/UrlValidation.hs`  
**Issue:** Current logic only blocks when the hostname *itself* is an IP literal (or `localhost`). Any DNS name that resolves to `127.0.0.1`, `10.0.0.0/8`, `::1`, etc. will pass validation.

**Fix:** Either implement DNS resolution + IP range check on resolved addresses, or rename/document the function as "blocks literal private IPs" to avoid false sense of safety.

#### 1.3 ~~Base64url Padding Mismatch Risk~~ VERIFIED OK
**File:** `core/auth/Auth/Jwt.hs`  
**Issue:** JWT uses base64url **without padding**. If `Data.ByteString.Base64.URL.decode` expects padding, valid tokens will be incorrectly rejected as `TokenMalformed`.

**Fix:** ~~Ensure using an unpadded base64url decoder (or explicitly add correct padding before decoding) and keep it strict.~~

**Status:** VERIFIED - `Data.ByteString.Base64.URL.decode` correctly handles unpadded JWT segments and rejects invalid input. No change needed.

### HIGH Issues

#### 1.4 ~~URI Comparison via `show` (False Mismatches)~~ FIXED
**File:** `core/auth/Auth/Jwt.hs:312, 326`  
**Issue:** Issuer/audience comparison uses `show audUri == GhcText.unpack expectedAud`. `show` is not stable for URI equality (encoding, trailing slashes vary).

**Fix:** ~~Compare via single canonical representation, or restrict config to Text form and only accept `JWT.string` prism (fail closed on URI form).~~

**Status:** FIXED - Added `stringOrUriMatchesText` helper that uses consistent comparison via `stringOrUriToText`.

#### 1.5 ~~Exception-Unsafe Refresh Lock~~ FIXED
**File:** `core/auth/Auth/Jwks.hs:235-260`  
**Issue:** `requestRefresh` sets `refreshInProgress = True` but only clears it after `refreshKeys`. If an IO exception escapes (HTTP, JSON, async cancellation), the lock stays True forever.

**Fix:** ~~Use `Task.finally`/bracket pattern to always clear `refreshInProgress`.~~

**Status:** FIXED - Added `Task.finally` to Task module and used it in `requestRefresh` to guarantee lock release.

### MEDIUM Issues

- **Token whitespace not trimmed:** `extractToken` doesn't trim whitespace around extracted token
- ~~**Issuer/audience mismatch loses detail:**~~ FIXED - `mapJoseError` now includes expected values from config
- ~~**Staleness not set on on-demand refresh failure:**~~ FIXED - All refresh paths now mark stale on failure via shared `performRefreshWithLock`

---

## 2. CONCURRENCY SAFETY

### CRITICAL Issues

#### 2.1 ~~Background vs On-Demand Refresh Race~~ FIXED
**File:** `core/auth/Auth/Jwks.hs`  
**Issue:** `requestRefresh` guards via `refreshState`, but `refreshLoop` calls `refreshKeys` directly with no check of `refreshInProgress`. This causes redundant fetches and state races with stale marking.

**Fix:** ~~Use one shared refresh gate for *all* refreshes (background + on-demand).~~

**Status:** FIXED - Added `refreshWithGate` and `performRefreshWithLock` functions. Both background and on-demand refresh now use the same gate with `Task.finally` for cleanup.

#### 2.2 ~~Stale Flag Race Condition~~ FIXED
**File:** `core/auth/Auth/Jwks.hs`  
**Issue:** `markKeysStale` can race with successful `refreshKeys`. Can end up with "fresh keys but stale flag" or stale snapshot overwriting fresh snapshot.

**Fix:** ~~Make stale updates monotonic with `staleAt`/`snapshotVersion` timestamp; only mark stale if not racing with newer successful refresh.~~

**Status:** FIXED - Added `snapshotVersion` field to `KeySnapshot` and `markKeysStaleIfVersion` that only marks stale if version hasn't changed since refresh started.

#### 2.3 ~~Cancellation Can Strand Refresh Lock~~ FIXED
**File:** `core/auth/Auth/Jwks.hs:235-260`  
**Issue:** No `finally`/bracket around refresh body. If request thread is cancelled between acquiring lock and releasing it, `refreshInProgress` stays True forever.

**Fix:** ~~Wrap refresh with exception-safe cleanup.~~

**Status:** FIXED - All refresh paths now use `Task.finally` via `performRefreshWithLock`.

### HIGH Issues

- **Snapshot strictness:** Thunk-heavy snapshots forced on hot path cause latency spikes
- **Stop behavior inconsistency:** `refreshLoop` continues current refresh after `isRunning` flips to False
- **Memory ordering:** `AtomicVar.peek` must be atomic read with appropriate barriers

### Stress Test Recommendations

1. **Concurrent refresh collision test:** Run `refreshLoop` + N request threads triggering missing `kid`
2. **Cancellation wedge test:** Cancel `requestRefresh` at random points; assert lock always clears
3. **Stale-vs-fresh race test:** Force background fail while on-demand succeeds; assert correct last-writer-wins
4. **Snapshot strictness test:** Generate large JWKS sets; track p99 latency for thunk forcing

---

## 3. PERFORMANCE (50k req/s)

### CRITICAL Blockers

#### 3.1 Per-Request Clock IO
**File:** `core/auth/Auth/Jwks.hs:222-229`  
**Issue:** `checkStaleness` calls `getCurrentSeconds` (via `getPOSIXTime`) on every request. This syscall overhead is significant at 50k req/s.

**Fix:** Make staleness a cached boolean in `KeySnapshot` updated by refresh loop (or via 1s ticking clock var).

#### 3.2 Asymmetric Crypto Throughput Ceiling
**Issue:** 50k asymmetric signature verifications/sec (RS256/ES256) is not realistic on a single node. Crypto will dominate and cap throughput well below target.

**Fix:** Add verification-result cache (bounded LRU + short TTL) or move auth to edge gateway.

#### 3.3 Header Parsing Allocations
**File:** `core/auth/Auth/Jwt.hs:67-108`  
**Issue:** `parseHeader` does `Text.splitOn`, UTF8 encode, base64 decode, and `Aeson.decodeStrict` into full Map per request. At high RPS, this creates GC pressure.

**Fix:** Decode only first segment with tight scan for `.`; parse JSON into Object (lookup fields without building Map).

### HIGH Issues

- **`checkCritHeaders` is O(m*n):** Allocates `unsupported` array each request
- **`extractToken` lowercases entire header:** Avoidable allocation
- **`Map.get` is O(log n):** Fine for small JWKS but not constant-time

### Benchmark Recommendations

1. **Microbench hot path excluding crypto:** `extractToken`, `parseHeader`, `AtomicVar.peek` + `Map.get`, `checkStaleness`
2. **End-to-end with/without crypto:** Measure framework overhead ceiling vs real throughput
3. **Allocation/GC analysis:** Run with `+RTS -s`, compare bytes allocated per request
4. **Crypto reality check:** Measure `verifyJWT` throughput per core for RS256/ES256/EdDSA

---

## 4. EU COMPLIANCE & GDPR

### CRITICAL Issues

#### 4.1 SSRF via DNS Rebinding
**Issue:** Same as correctness section - validates URL but doesn't resolve DNS. Hostname resolving to private IP passes validation.

#### 4.2 Redirect-Based SSRF Pivot
**File:** `core/http/Http/Client.hs`  
**Issue:** HTTP client likely follows redirects. Even if initial URL is HTTPS+public, redirect could send to internal IP/host.

**Fix:** Disable redirects for discovery/JWKS, or re-validate each redirect target.

#### 4.3 Exception Escape in Discovery/JWKS Fetch
**Issue:** `Http.Client.get` is `Task.fromIO` without explicit exception capture. TLS/parse/timeout exceptions can escape typed error handling.

### HIGH Issues

#### 4.4 Data Residency Cannot Be Assured
**Issue:** `discoverConfig` accepts any HTTPS URL. Need explicit allowlist for "EU-hosted IdP domains only".

### MEDIUM Issues

- **Token policy hardening:** Require `exp`, cap max token lifetime, configurable `typ`
- **DoS protections:** Max JWT size, max header/claims size, rate limiting on auth failures
- **JWKS rotation robustness:** Retain previous keys for grace window during rotation
- **Rate limiting:** No rate limiting on auth failures (brute force / CPU DoS)

### PASS: Correctly Implemented

- TLS enforcement for discovery/JWKS URLs
- SSRF baseline (blocks localhost, private IPv4, key IPv6 ranges)
- Algorithm allowlist excludes `none` and HS* families
- Strict base64url decoding
- Generic client error messages (no info leakage)
- 30-second timeouts on HTTP requests

---

## 5. ARCHITECTURE & DESIGN

### CRITICAL Issues

- **SSRF protection incomplete relative to stated contract** (documented above)

### HIGH Issues

#### 5.1 Custom Auth Failures Mapped to TokenMalformed
**File:** `core/auth/Auth/Middleware.hs:140-142`  
**Issue:** `Custom` maps `AuthOptionsError` into `TokenMalformed`, conflating parse failures with policy failures.

**Fix:** Add distinct `AuthError` constructor (e.g., `CustomAuthFailed Text`).

#### 5.2 Malformed `crit` Headers Silently Dropped
**File:** `core/auth/Auth/Jwt.hs:92-99`  
**Issue:** Non-string `crit` entries become `""` and are dropped. For RFC 8725 hardening, malformed should fail closed.

**Fix:** Return `TokenMalformed` for non-string `crit` values.

### MEDIUM Issues

- **JWT validation pipeline duplication:** Three functions repeat same sequence; extract internal helper
- **`WWW-Authenticate` header on all errors:** Should only be on 401s per spec
- **Mixed NeoHaskell/base containers:** Consider isolating interop parsing

### PASS: Well-Designed Aspects

- Clear module boundaries, no circular dependencies
- Hot path vs control plane separation is real (lock-free reads, background refresh)
- `KeySnapshot` redundancy is intentional for performance
- Security posture is "fail fast, fail closed" in right spots
- Public API is minimal (internals hidden)

---

## 6. ACTION PLAN

### Phase 1: Critical Fixes (1-2 days)

| Priority | Task | File(s) | Effort |
|----------|------|---------|--------|
| CRITICAL | Fix bracketed IPv6 parsing in SSRF check | `UrlValidation.hs` | 1h |
| CRITICAL | Add exception-safe refresh lock (finally/bracket) | `Jwks.hs` | 2h |
| CRITICAL | Verify/fix base64url padding handling | `Jwt.hs` | 1h |
| CRITICAL | Share refresh gate between background + on-demand | `Jwks.hs` | 2h |
| CRITICAL | Make stale updates monotonic (timestamp-based) | `Jwks.hs` | 2h |
| HIGH | Fix URI comparison (canonical form) | `Jwt.hs` | 1h |
| HIGH | Preserve issuer/audience in error details | `Jwt.hs` | 30m |

### Phase 2: Performance Optimization (1 day)

| Priority | Task | File(s) | Effort |
|----------|------|---------|--------|
| CRITICAL | Remove per-request clock IO from staleness check | `Jwks.hs` | 2h |
| HIGH | Optimize header parsing (avoid full Map construction) | `Jwt.hs` | 4h |
| MEDIUM | Convert `supportedCritHeaders` to Set for O(1) lookup | `Jwt.hs` | 1h |

### Phase 3: EU Hardening (1-2 days)

| Priority | Task | File(s) | Effort |
|----------|------|---------|--------|
| MEDIUM | Add DoS limits (max JWT size, rate limiting) | `Middleware.hs`, `Jwt.hs` | 4h |
| MEDIUM | Add IdP domain allowlist option | `Config.hs`, `Discovery.hs` | 2h |

### Phase 4: Optional Enhancements (3+ days)

| Priority | Task | Effort |
|----------|------|--------|
| MEDIUM | Implement DNS resolution for full SSRF protection | 1d |
| MEDIUM | Add token verification cache for 50k req/s | 1-2d |
| LOW | Add token introspection/revocation support | 2-3d |

---

## 7. TESTING RECOMMENDATIONS

### Unit Tests to Add

1. **SSRF bypass tests:** `https://[::1]/`, `https://[fc00::1]/`, `https://[fe80::1]/`
2. **Base64url padding tests:** Tokens with/without padding
3. **URI form issuer/audience tests:** Both string and URI forms
4. **Malformed crit header tests:** Non-string values in crit array

### Concurrency Tests to Add

1. **Concurrent refresh collision:** Background + N on-demand refreshes
2. **Cancellation wedge:** Cancel at random points; assert lock clears
3. **Stale-vs-fresh race:** Force specific timing; assert correct winner

### Performance Benchmarks to Add

1. **Hot path microbenchmarks:** Isolate each component
2. **End-to-end with/without crypto:** Measure ceiling
3. **GC pressure analysis:** Bytes allocated per request

---

## 8. CONCLUSION

The JWT authentication middleware has a **solid architectural foundation** with correct RFC 8725 hardening in the validation path. The hot path / control plane separation is well-designed for performance.

**Before production deployment, address:**
1. SSRF protection gaps (Phase 1)
2. Concurrency safety issues (Phase 1)
3. Performance blockers if 50k req/s is a hard requirement (Phase 2)
4. EU compliance gaps if deploying in regulated markets (Phase 3)

**Estimated total effort:** 4-6 days for Phases 1-3.

---

*Generated by Oracle Agent Swarm - 5 parallel deep reviews*
*Reviews: Correctness, Performance, Concurrency, EU Compliance, Architecture*
