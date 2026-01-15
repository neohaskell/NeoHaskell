# Integration Pattern Audit Report

**Date**: 2026-01-15  
**Branch**: `feat/integration`  
**PR**: #255  
**Methodology**: 7-agent parallel review swarm (Oracle-based)

## Executive Summary

The Integration pattern implementation is **well-designed architecturally** but has **critical issues that must be fixed before merge**. A comprehensive 7-agent review identified:

| Severity | Count |
|----------|-------|
| **Blocker** | 11 |
| **High** | 18 |
| **Medium** | 15 |
| **Low** | 4 |

### Review Agents Deployed

1. **Concurrency Correctness** - STM invariants, race conditions, atomicity
2. **Dispatcher Lifecycle** - Worker spawn/reuse/teardown, resource leaks
3. **Channel + Backpressure** - Bounded queue semantics, timeout safety
4. **Performance + Scalability** - 50k events/s target, contention analysis
5. **Fault Tolerance** - Error propagation, cleanup, degradation modes
6. **Security + Resource Limits** - Abuse cases, injection, DoS vectors
7. **NeoHaskell Standards** - Code style, API ergonomics, documentation

---

## Blocker Issues (11)

These **must be fixed before merge**.

### 1. SQL Injection via StreamId Interpolation

| Field | Value |
|-------|-------|
| **Location** | `core/service/Service/EventStore/Postgres/Sessions.hs:354` |
| **Review** | Security |
| **Evidence** | `InlinedStreamId = '#{streamIdText}'` - string interpolation in SQL |
| **Impact** | Data exfiltration, corruption, DoS |
| **Fix** | Parameterize query using `$1`, `$2` like `truncateStreamSession` does |

### 2. `ConcurrentVar.set` Deadlocks

| Field | Value |
|-------|-------|
| **Location** | `core/concurrency/ConcurrentVar.hs:45` |
| **Review** | Concurrency Correctness |
| **Evidence** | Uses `putMVar` on already-full MVar (created via `containing`) |
| **Impact** | Dispatcher thread hangs permanently |
| **Fix** | Implement `set` via `swapMVar` or add separate `swap` function |

### 3. Reaper Deletes Wrong Worker After Concurrent Replacement

| Field | Value |
|-------|-------|
| **Location** | `core/service/Service/Integration/Dispatcher.hs:54-74` |
| **Review** | Concurrency Correctness |
| **Evidence** | Reaper snapshots worker, dispatch replaces it, reaper removes the NEW worker |
| **Impact** | Worker routing breaks, "one worker per entity" violated |
| **Fix** | Add `ConcurrentMap.removeIfM` or store worker ID and remove conditionally |

### 4. `Stop` Messages Can Be Dropped

| Field | Value |
|-------|-------|
| **Location** | `core/service/Service/Integration/Dispatcher.hs:276` |
| **Review** | Channel, Lifecycle, Fault Tolerance |
| **Evidence** | `writeWorkerMessageWithTimeout` logs and discards on timeout |
| **Impact** | Orphaned workers, resource leaks, cleanup never runs |
| **Fix** | Use blocking write for Stop, or cancel AsyncTask as fallback |

### 5. TBQueue STM Thrash Under Backpressure

| Field | Value |
|-------|-------|
| **Location** | `core/concurrency/Channel.hs:36` |
| **Review** | Performance |
| **Evidence** | Bounded channels use STM TBQueue; `registerDelay` per write; STM retry on full |
| **Impact** | Wakeup storms, throughput collapse at 50k/s |
| **Fix** | Use non-STM queue (Unagi) with external backpressure, or reduce STM scope |

### 6. Reaper Scans Entire Map in Single STM Transaction

| Field | Value |
|-------|-------|
| **Location** | `core/service/Service/Integration/Dispatcher.hs:554` + `ConcurrentMap.hs:275` |
| **Review** | Performance |
| **Evidence** | `ConcurrentMap.entries` traverses whole map in one `atomically` |
| **Impact** | Global stalls, dispatch conflicts, periodic "stop-the-world" |
| **Fix** | Batch/chunk iteration, or maintain separate index of active keys |

### 7. Async Exceptions Kill Workers Silently

| Field | Value |
|-------|-------|
| **Location** | `core/service/Service/Integration/Dispatcher.hs:406,436` |
| **Review** | Fault Tolerance |
| **Evidence** | Worker loops via `AsyncTask.run`, never awaited, no exception boundary |
| **Impact** | Dead workers stay in map, events queue then drop silently |
| **Fix** | Wrap worker loop in exception boundary, log + remove/respawn on failure |

### 8. Pattern Matching Outside `case` in AtomicVar

| Field | Value |
|-------|-------|
| **Location** | `core/concurrency/AtomicVar.hs:52,73,83,94` |
| **Review** | Standards |
| **Evidence** | `peek (AtomicVar tvar) = ...` |
| **Impact** | NeoHaskell style violation |
| **Fix** | Use `case atomicVar of AtomicVar tvar -> ...` |

### 9. Pattern Matching Outside `case` in ConcurrentMap

| Field | Value |
|-------|-------|
| **Location** | `core/concurrency/ConcurrentMap.hs:54+` |
| **Review** | Standards |
| **Evidence** | `get key (ConcurrentMap stmMap) = ...` |
| **Impact** | NeoHaskell style violation |
| **Fix** | Use `case concurrentMap of ConcurrentMap stmMap -> ...` |

### 10. Unqualified `pure` in STM Blocks

| Field | Value |
|-------|-------|
| **Location** | `core/concurrency/ConcurrentMap.hs:102,106,141,144,148,183,186,190` |
| **Review** | Standards |
| **Evidence** | `pure (existingValue, Just candidate)` |
| **Impact** | NeoHaskell forbids `pure`/`return` |
| **Fix** | Add STM yield helper or use qualified alternative |

### 11. Docs Suggest `unsafePerformIO` Pattern

| Field | Value |
|-------|-------|
| **Location** | `core/concurrency/AtomicVar.hs:64` |
| **Review** | Standards |
| **Evidence** | Example shows `unsafePerformIO (atomically ...)` |
| **Impact** | Encourages dangerous patterns, subtle concurrency bugs |
| **Fix** | Replace with safe example using `ConcurrentMap.getOrInsertIfM` |

---

## High Priority Issues (18)

### Resource Leaks

| Issue | Location | Review |
|-------|----------|--------|
| Stateless workers never reaped | `Dispatcher.hs:303` | Lifecycle |
| Worker crashes undetected | `Dispatcher.hs:406,436` | Lifecycle |
| Lifecycle cleanup skipped on crash | `Dispatcher.hs:551` | Fault Tolerance |
| Outbound dispatcher not tied to app shutdown | `Integrations.hs:196` | Fault Tolerance |

### Data Loss

| Issue | Location | Review |
|-------|----------|--------|
| "Drain on Stop" not implemented | `Dispatcher.hs:416-423,454-465` | Lifecycle |
| Backpressure drops events silently | `Dispatcher.hs:276` | Fault Tolerance |
| Missing command handler ignored | `Dispatcher.hs:518` | Fault Tolerance |
| Race between reaper and dispatch loses events | `Dispatcher.hs:55-75` | Channel |

### Performance

| Issue | Location | Review |
|-------|----------|--------|
| Multiple sync ops per lifecycle event | `Dispatcher.hs:55-74` | Performance |
| Wasted worker pre-spawn under races | `Dispatcher.hs:321-333` | Performance |
| Head-of-line blocking at dispatcher | `Dispatcher.hs:82-85` | Channel |

### Security

| Issue | Location | Review |
|-------|----------|--------|
| Unbounded stateless workers (DoS) | `Dispatcher.hs:174` | Security |
| Unbounded lifecycle workers under burst | `Dispatcher.hs:176` | Security |
| StreamId unvalidated (any text) | `StreamId.hs:42` | Security |

### API/Style

| Issue | Location | Review |
|-------|----------|--------|
| CommandPayload constructor exposed | `Integration.hs:57` | Standards |
| `pure` in dispatcher STM predicate | `Dispatcher.hs:372` | Standards |
| Tuple pattern matching in lambdas | `Dispatcher.hs:561,596,601` | Standards |
| IntegrationError flattened to Text | `Integrations.hs:96` | Fault Tolerance |

---

## Medium Priority Issues (15)

| Issue | Location | Review |
|-------|----------|--------|
| Timer thread churn with `registerDelay` | `Channel.hs:49-61` | Channel |
| Config defaults cause blocking (5s timeout) | `Dispatcher.hs:113-120` | Performance |
| JSON alloc per command dispatch | `Dispatcher.hs:527-529` | Performance |
| `getOrInsertIfM` allows duplicate candidate construction | `ConcurrentMap.hs:82` | Concurrency |
| No per-worker processing timeout | `Dispatcher.hs:413,451` | Security |
| Drop policy exploitable | `Dispatcher.hs:276` | Security |
| Log injection risk | `Integrations.hs:80,84` | Security |
| PG LISTEN identifier issues | `Notifications.hs:43` | Security |
| No retry/backoff for outbound | `Dispatcher.hs:477` | Fault Tolerance |
| Shutdown doesn't block dispatch | `Dispatcher.hs:590-602` | Lifecycle |
| Qualified import name wrong (STMMap) | `ConcurrentMap.hs:26` | Standards |
| Base imports not Ghc-qualified | `Integration.hs:73-74` | Standards |
| Inbound constructor unnecessarily exported | `Integration.hs:51` | Standards |
| `getOrInsertIfM` expands STM read-set | `Dispatcher.hs:70-75` | Performance |
| Cardinality tradeoffs undocumented | `Dispatcher.hs:23-26` | Performance |

---

## Low Priority Issues (4)

| Issue | Location | Review |
|-------|----------|--------|
| No channel close semantics | `Channel.hs` | Channel |
| Check-then-act race window (optimization) | `Dispatcher.hs:55` | Concurrency |
| AtomicVar peekSTM example misleading | `AtomicVar.hs:62` | Concurrency |
| runAction export could be misused | `Integration.hs` | Standards |

---

## Prioritized Fix Plan

### Phase 1: Critical Security & Correctness

**Must complete before any merge consideration.**

- [ ] **1.1** Fix SQL injection in `Sessions.hs` - parameterize query
- [ ] **1.2** Add `ConcurrentVar.swap` function, document `set` vs `swap`
- [ ] **1.3** Make reaper removal conditional (only remove if same worker)
- [ ] **1.4** Add StreamId validation (max length + allowed charset)

### Phase 2: Resource Safety

**Required for production readiness.**

- [ ] **2.1** Guarantee `Stop` delivery (blocking write or AsyncTask.cancel fallback)
- [ ] **2.2** Add worker supervision (exception boundaries, crash detection)
- [ ] **2.3** Add stateless worker reaping or max worker cap
- [ ] **2.4** Implement proper drain-on-stop semantics
- [ ] **2.5** Tie outbound dispatcher to app shutdown lifecycle

### Phase 3: Performance

**Required for 50k events/s target.**

- [ ] **3.1** Reduce STM transactions per event (consolidate or use non-STM hot path)
- [ ] **3.2** Batch/chunk reaper iteration instead of full-map STM scan
- [ ] **3.3** Add spawn reservation pattern to prevent duplicate worker creation
- [ ] **3.4** Review timeout defaults (5s may be too long for high-throughput)

### Phase 4: Standards Compliance

**Required for code quality.**

- [ ] **4.1** Fix pattern matching in AtomicVar (use `case` expressions)
- [ ] **4.2** Fix pattern matching in ConcurrentMap (use `case` expressions)
- [ ] **4.3** Remove `pure` usage, add STM yield helper if needed
- [ ] **4.4** Fix AtomicVar docs (remove unsafePerformIO example)
- [ ] **4.5** Make CommandPayload/Inbound opaque exports
- [ ] **4.6** Fix tuple pattern matching in Dispatcher lambdas

### Phase 5: Observability & Polish

**Nice to have for production operations.**

- [ ] **5.1** Add structured logging for channel timeouts, worker lifecycle
- [ ] **5.2** Add metrics points (worker count, drop rate, queue depth)
- [ ] **5.3** Document backpressure semantics in Integration.hs
- [ ] **5.4** Add per-event processing timeout option

---

## Appendix: Detailed Findings by Agent

### A. Concurrency Correctness Agent

**Summary**: 2 Blockers, 1 High, 1 Medium, 2 Low

Key invariant violations:
- `ConcurrentVar.set` assumes empty MVar (wrong)
- Reaper remove is not conditional on current map value
- "Stop" delivery is best-effort, not guaranteed

### B. Dispatcher Lifecycle Agent

**Summary**: 1 Blocker, 3 High, 2 Medium

Worker state machine gaps:
- No transition from Active to Crashed (undetected)
- Stateless workers have no reaping at all
- "Drain on Stop" comment is aspirational, not implemented

### C. Channel + Backpressure Agent

**Summary**: 1 Blocker, 2 High, 1 Medium, 1 Low

Backpressure audit:
- Per-entity bounded queue applies pressure correctly
- But slow entity can block entire intake (head-of-line)
- Stop can be dropped, breaking cleanup guarantees

### D. Performance + Scalability Agent

**Summary**: 2 Blockers, 2 High, 3 Medium, 1 Low

Hot path analysis (per lifecycle event):
1. `ConcurrentMap.get` (STM tx #1)
2. `AtomicVar.peek` (readTVarIO)
3. Possibly `ConcurrentMap.getOrInsertIfM` (STM tx #2 with expanded read-set)
4. `ConcurrentVar.set` for lastActivityTime (MVar op - DEADLOCK)
5. `Channel.tryWriteWithTimeout` (STM tx #3 with registerDelay)

Top bottlenecks:
1. TBQueue STM contention on hot entities
2. Whole-map reaper scan
3. Pre-spawn worker churn
4. Multiple sync ops per event

### E. Fault Tolerance Agent

**Summary**: 2 Blockers, 4 High, 2 Medium

Failure mode matrix:
| Failure | Behavior | Caller Visibility | Cleanup |
|---------|----------|-------------------|---------|
| Worker crash | Thread dies | None | None |
| Channel full | Drop + log | None (returns success) | None |
| Shutdown | Best-effort Stop | Partial | Weak |

### F. Security + Resource Limits Agent

**Summary**: 1 Blocker, 3 High, 4 Medium

Abuse cases:
| Vector | Current Mitigation | Needed |
|--------|-------------------|--------|
| Many entities | None (stateless) / Idle reap (lifecycle) | Global cap + rate limit |
| Malicious StreamId | None | Validation + sanitization |
| SQL injection | None | Parameterized queries |

### G. NeoHaskell Standards Agent

**Summary**: 3 Blockers, 3 High, 3 Medium, 1 Low

API critique:
- `CommandPayload(..)` should be opaque
- `Inbound(..)` should be opaque
- All other exports are appropriate

Documentation gaps:
- AtomicVar needs safe example
- Dispatcher needs backpressure semantics docs
- Integration.hs needs CommandPayload invariant docs

---

## Conclusion

The Integration pattern is architecturally sound with excellent two-persona design. However, the implementation has critical concurrency bugs (`ConcurrentVar.set` deadlock, reaper race), a SQL injection vulnerability, and several resource leak paths. 

**Recommendation**: Fix Phase 1 (security/correctness) before any merge. Phases 2-3 are required for production. Phase 4 is required for code quality standards.

Estimated effort:
- Phase 1: 1-2 days
- Phase 2: 2-3 days
- Phase 3: 2-3 days
- Phase 4: 1 day
- Phase 5: 1-2 days (optional)
