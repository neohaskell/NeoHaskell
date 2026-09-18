# Performance design review: Preserve command decision concurrency semantics
Spec: docs/changes/858-command-executor-revisions.md | Capabilities: commands, event-store | Date: 2026-09-18

| # | Checklist | Finding | Grounding | Verdict |
|---|-----------|---------|-----------|---------|
| 1 | P1 | `CommandExecutor` and event persistence are hot paths. The normal path retains the already-fetched stream position; the additional fetch/re-decision work occurs only after an optimistic-concurrency conflict, which is the correctness path under contention. | kept: the affected functions are command intake and persistence, but the spec does not claim a speedup; nightly measurement remains the right place to measure contention cost. | informational |
| 2 | P2 | No new exported polymorphic function or cross-module specialization requirement is introduced; the implementation is internal to the existing executor. | demoted: no pragma or specialization change is justified by the contract. | informational |
| 3 | P3 | The retry path refetches typed state and position and re-runs the existing decision; the design introduces no lazy opt-outs, recursive accumulator, or container-thunk pattern. | demoted: no new laziness risk is specified. | informational |
| 4 | P4 | No event or API codec changes are proposed. | demoted: serialization remains on the existing derived/hand-written paths. | informational |
| 5 | P5 | The design adds no text packing, repeated encoding, or new per-request formatting. It preserves the existing bounded retry policy rather than adding an unbounded loop. | kept: retry work is bounded and is triggered by a real conflict; no allocation optimization is required without a profile. | informational |
| 6 | P6 | PostgreSQL contention is intentionally coordinated by the existing optimistic-concurrency mechanism; the production design adds no global lock or whole-map `ConcurrentVar`. | demoted: test barriers are test-only and do not affect runtime contention. | informational |
| 7 | P7 | The spec makes no “faster” claim and adds no benchmark criterion. Any conflict-rate or retry-cost impact belongs in the nightly benchmark harness. | kept: no PR benchmark budget entry is warranted for this correctness-only change. | informational |

**Blockers:** 0 — reviewed; no performance blocker found. Measurement note: verify contention/retry behavior in the nightly benchmark suite if the implementation changes the hot-path profile materially.
