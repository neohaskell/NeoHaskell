# Performance design review: Thread the query name through the QueryObjectStore factory

Spec: docs/changes/003-thread-query-name-through-query-object-store.md | Capabilities: queries (perf-sensitive), service-wiring | Date: 2026-07-23

Design-time review of the approved contract delta (no code yet). Committed on
the PR branch; gated at PR-ready by `./dev spec-check --reviews-pr`.

## Hot-path placement summary

- **Factory surface** (`createDefinitionWithStore`, `createQueryObjectStore`,
  `newFromConfig`): runs **once per query at application boot** (wiring). The
  added `Text` parameter has no per-request cost.
- **Store ops** (`get` / `atomicUpdate` / `getAll`): on the query path
  (rebuild + live apply + `GET /queries/{name}`, budget <0.2ms). The real
  `query_name` is captured **once** in the operation closures at store
  construction (`get = getImpl pool queryName`), so no per-call allocation of
  the name; each call still passes it via the same typed Hasql parameter.

| # | Checklist | Finding | Grounding | Verdict |
|---|-----------|---------|-----------|---------|
| 1 | P1 Hot-path placement | Factory changes are one-time boot wiring — zero request-path impact. Store ops keep the same parameterized, PK-indexed statements. | kept | informational (no regression) |
| 2 | P1 `getAll` selectivity | `getAll` changes from `WHERE query_name = '__trait__'` (scans **all** queries' rows, O(total)) to `WHERE query_name = $1` (only this query's rows). `get`/`atomicUpdate` gain PK selectivity — the `(query_name, instance_uuid)` leading column now discriminates instead of being the constant `'__trait__'`. | kept (Q1 measurable: smaller result set + better index selectivity; Q2 reachable: multi-query `getAll`; Q3 framework-absorbed; Q4 proportional — incidental to the correctness fix) | advisory (positive: improvement) |
| 3 | P5 Allocation | `query_name` is captured once in the closure, not rebuilt per call. No new `pack`/`unpack`, no `[fmt\|…\|]` in a loop, no unfused `Array.map` chain. SQL-bytes construction per call is pre-existing and unchanged. | kept | informational |
| 4 | P2 Specialisation | No new cross-module polymorphic hot-path dispatch. The per-query-type JSON codec (`Json.encode`/`decode`) is unchanged; threading a `Text` adds no typeclass indirection. | demoted (failed Q2: path unchanged) | informational |
| 5 | P3 Laziness / P4 Serialization / P6 Contention | No `~` opt-outs (`Text` is strict under global `Strict`); no hand-written hot codec introduced; no new `ConcurrentVar`-over-`Map` writer contention (in-memory backend is untouched and ignores the name; Postgres uses a connection pool). | kept | informational (N/A) |
| 6 | P7 Evidence discipline | The spec makes no unverified "faster" claim — the `getAll` win is a consequence of the correctness fix. No new perf-tagged entrypoint is introduced, so no new `telemetry/bench-budgets.json` entry is required; the existing nightly Postgres-store benches (`./dev bench`, `POSTGRES_AVAILABLE=true`) already cover this path. | kept | informational |

**Blockers:** 0 — no plan amendment required. The change is neutral-to-positive
on the 50k req/s budget: startup-only factory cost, no new per-request
allocation, and a strictly narrower `getAll` scan. Measurement (not required for
this PR) remains the nightly bench harness's job, not a PR gate.
