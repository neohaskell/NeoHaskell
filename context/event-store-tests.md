# Event Store Test Suite – Correctness & Performance

This suite validates the integrity, concurrency, throughput, and resilience of a high-performance, fault-tolerant Event Store for critical real-time systems (e.g., aviation sensors). It is split into core tests, critical extensions, and extreme edge cases.

---

## Core Test Cases (C1–C7)

### C1. Write-After-Write Ordering

- **Goal**: Ensure total ordering per stream.
- **Description**: Append 1M events sequentially to the same stream. Verify that all events are stored in correct order with no gaps or duplicates.

### C2. Version Conflict Racing

- **Goal**: Test optimistic concurrency control.
- **Description**: Two writers attempt to append at the same version simultaneously. Exactly one should succeed.

### C3. High-Concurrency Hammer

- **Goal**: Detect deadlocks and performance degradation under massive concurrency.
- **Description**: Multiple threads append concurrently to many streams. The system should remain responsive and lock-free.

### C4. Idempotent Write Replay

- **Goal**: Prevent duplicated writes under retry conditions.
- **Description**: The same event is appended 10 times using the same idempotency key. Only one event should be persisted.

### C5. Sustained Write Throughput

- **Goal**: Measure p99 latency and throughput under load.
- **Description**: Append 50,000 events per second for 60 seconds. Track memory and latency metrics under load.

### C6. Read Consistency and Order

- **Goal**: Ensure consistent reads after writes.
- **Description**: After writing a series of ordered events, read them and verify that order and content are preserved.

### C7. Soak Test (72 Hours)

- **Goal**: Validate long-term stability under production-like load.
- **Description**: Simulate real workload for 72 hours. Monitor memory, file descriptors, garbage collection, and system health.

---

## Critical Additional Cases (A1–A7)

### A1. Read-Not-Starved Under Write Surge

- **Goal**: Prevent read starvation during intense write loads.
- **Description**: Continuously monitor read latency during 50k/s write flood. Read operations must remain under SLA.

### A2. Cross-Stream Causal Order

- **Goal**: Ensure causal consistency across streams.
- **Description**: Events sharing the same causation ID must appear in the same order in all affected streams.

### A3. Subscribe Hot Path Integrity

- **Goal**: Validate the global event subscription feed (`subscribeAll`).
- **Description**: No duplicates or missing events must occur even under high throughput.

### A4. Snapshot and Catch-up Race

- **Goal**: Prevent loss during proactor bootstrapping.
- **Description**: When restoring from snapshot + replay, ensure no events are skipped during the race window.

### A5. Write-Ahead Log Flush Bound

- **Goal**: Measure true I/O fsync latency.
- **Description**: Log p99 time for `fsync()` per event write; it must remain under 2ms.

### A6. Cluster Replication Lag

- **Goal**: Confirm that replicas stay in sync under load.
- **Description**: Followers must apply events from the leader within a bounded time (e.g., ≤100ms).

### A7. Backpressure Stability

- **Goal**: Avoid overload collapse.
- **Description**: When clients send requests 10× over capacity, the system must throttle gracefully (e.g., 429s) without crashing.

---

## Extreme Validation Cases (A8–A13)

### A8. Variable Event Size Sweep

- **Goal**: Validate performance and memory stability under varying payload sizes.
- **Description**: Append events ranging from 0B to 4MB. Check p99 latency and resident memory.

### A9. High-Cardinality Streams

- **Goal**: Ensure scalability with many streams.
- **Description**: Create and use 10 million streams (1 event each). Index and reads must remain fast.

### A10. Delete/Truncate Integrity

- **Goal**: Ensure logical deletes (truncation) are respected.
- **Description**: After truncation, old events must be inaccessible to readers and projectors.

### A11. GC / Fragmentation Drift

- **Goal**: Detect heap fragmentation over time.
- **Description**: Repeatedly create/delete streams for 24 hours and monitor heap/RSS growth.

### A12. Split-Brain Merge Correctness

- **Goal**: Reconcile conflicting writes post-partition.
- **Description**: Partition a cluster, write on both sides, then rejoin. Verify no data loss or duplication.

### A13. QuickCheck/Fuzz Concurrency

- **Goal**: Expose concurrency bugs via randomized testing.
- **Description**: Generate random interleaved operations (append, read, truncate) with QuickCheck. Check invariants.

---
