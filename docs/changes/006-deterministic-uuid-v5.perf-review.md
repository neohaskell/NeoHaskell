# Performance design review: Deterministic UUID v5 in `Uuid` and the `Decision` monad

Spec: docs/changes/006-deterministic-uuid-v5.md | Capabilities: core-primitives, commands (perf-sensitive) | Date: 2026-08-12

Design-time review of the approved contract delta (no code yet). Committed on
the PR branch; gated at PR-ready by `./dev spec-check --reviews-pr`.

## Hot-path placement summary

The promised surface is three pure, total, monomorphic functions. Two of them
land on the **command intake** path (budget <1ms) — but only for commands that
opt in by calling them:

- **`Uuid.generateV5 ns name`** — called from a user command's `getEntityId`
  (once per command intake) and/or from `decide`. Cost is **O(|name|)**: one
  UTF-8 encode of `name`, one `[Word8]` list build, and one SHA-1 over
  `16 + |name|` bytes, i.e. `ceil((16 + |name| + 9) / 64)` compression blocks.
  A natural key of realistic size (a repo path, an OAuth subject — tens of
  bytes) is 1–2 blocks, and the cost grows linearly from there. No wall-clock
  figure is asserted — nothing here was measured, so the review states the cost
  *shape* and leaves timing to the bench harness (P7). **The bound is the
  caller's**: this is
  a pure function with no length limit of its own, so a caller that derives
  from an untrusted, unbounded `name` (a request body field, an imported file)
  must impose its own cap — the HTTP transport's body limit constrains only the
  web path, not the API. No measured figure is claimed here; see P7.
- **`Decider.generateDeterministicUuid`** — `Uuid.generateV5 … |> Return`. It
  adds no `Decision` constructor and no `runDecision` case, so the interpreter
  loop that every command already pays for is bit-for-bit unchanged. Commands
  that do not call it pay exactly nothing.
- **`Bytes.unpack`** — a prelude gap-fill; on the intake path only as
  `generateV5`'s internal step.

Existing commands are unaffected: no signature they use changes, and no
executor code path is modified.

| # | Checklist | Finding | Grounding | Verdict |
|---|-----------|---------|-----------|---------|
| 1 | P1 Hot-path placement | `generateV5` sits on command intake when a command derives its stream id from a natural key. One UTF-8 encode + one `[Word8]` build + one SHA-1 over a short input (1–2 compression blocks for a realistic key) — a fixed, input-proportional amount of work on a path budgeted at <1ms, with no wall-clock figure claimed. `generateDeterministicUuid` adds a `Return` node to a `Decision` the executor already walks. Zero cost for commands that don't call either. | kept | informational (no regression) |
| 2 | P5 Allocation — the `[Word8]` hop | `Data.UUID.V5.generateNamed :: UUID -> [Word8] -> UUID` demands a lazy cons list, so `Text.toBytes \|> Bytes.unpack` allocates one cons cell per name byte per call. This is **forced by the upstream package API**, not a design choice: `uuid` exposes no `ByteString` entry point. The list is consumed immediately and strictly by `generateNamed`'s SHA-1 fold and never retained, so it is short-lived nursery garbage — tens of cells for a realistic key, collected in the minor GC that intake already triggers. | kept (Q1: bounded by key length, negligible at tier; Q2: reachable on intake; Q3: framework-absorbed — users never see `[Word8]`; Q4: no fix proposed, so nothing to be disproportionate about) | informational |
| 3 | P5 Allocation — double derivation | A command that derives the **same** UUID in both `getEntityId` and `decide` pays the encode+SHA-1 twice per request. The two hooks are invoked independently with no shared context, so **no reuse of the computed value is possible** — not by the framework (nothing to memoize into) and not by the caller. A shared helper (`cartId cmd = Uuid.generateV5 ns cmd.key`) can only keep the two derivations *identical*, which is a correctness benefit, not a saved computation. The honest statement is therefore "the second derivation is unavoidable and costs another O(\|name\|) hash", not "derive once and reuse". The testbed demo (C8) derives distinct values in the two hooks, so it does not even hit this shape. | kept (Q1: exactly 2× the single-derivation cost, which is O(\|name\|) and unmeasured — bounded and immaterial at realistic key sizes; Q3: **not** framework-absorbable) | informational (no action available) |
| 4 | P2 Specialisation / INLINE pragmas | All three functions are **monomorphic** — no typeclass constraints, so `INLINABLE` has nothing to specialise and P2 does not apply. The abandoned `feat/uuid-v5-decision` branch carried `{-# INLINE generateV5 #-}` and `{-# INLINE generateDeterministicUuid #-}`; **do not port them.** `generateV5`'s body calls SHA-1 — inlining duplicates code at every call site and unlocks no fusion or RULES. `generateDeterministicUuid` is a small non-recursive wrapper whose unfolding GHC will inline at `-O` on its own merits. Adding either pragma is exactly the premature-optimization cascade the grounding filter rejects: compile time and code size for no measured win. | kept (Q4: pragmas on code no profile has shown hot) | **advisory — plan amendment: omit both `INLINE` pragmas the prior branch carried** |
| 5 | P3 Laziness escapes | `[Word8]` is an imported Prelude list, precisely the case global `Strict` does **not** cover — flagged deliberately. No retention risk: it is produced and consumed inside one function body, never stored in a field, a container, or an accumulator, and `generateNamed` forces it fully. No `~` opt-outs are introduced. `Uuid` and `Text` fields are strict under global `Strict`. | kept | informational |
| 6 | P4 Serialization | No hand-written `ToJSON` on a hot codec. `Uuid` already has its instances; the C8 testbed demo command derives JSON via `Generic`, which emits a real `toEncoding` (the `toJSON`-only pitfall does not arise). | demoted (failed Q2: no new codec on an exercised hot path) | informational |
| 7 | P6 Contention | No shared mutable state introduced — three pure functions. No `ConcurrentVar`/`TVar`-over-`Map`, no new writer serialisation. Determinism means concurrent callers need no coordination at all. | demoted (failed Q2: path does not exist) | informational (N/A) |
| 8 | P7 Evidence discipline | The spec makes **no** performance claim, so nothing needs measurement to stand up — and this review makes none either: the cost is stated as a shape (O(\|name\|), 1–2 SHA-1 blocks for a realistic key), not as a measured number for a stated runtime and hardware. Input length is the only cost driver. Through the **web transport** it is bounded by the request-body size limit (ADR-0019); through any other caller it is **not bounded by the framework**, so an untrusted unbounded name is the caller's cap to impose (finding 1). No new `telemetry/bench-budgets.json` entry is proposed: a pure function with no shared state and no framework-owned input bound has no meaningful budget to defend, and the existing command-intake benches cover the path it sits on. If a consumer reports deriving over large payloads, that is the trigger to measure and add one. | kept | informational |

**Blockers:** 0.

**Plan amendment (advisory, finding 4):** implement `Uuid.generateV5` and
`Decider.generateDeterministicUuid` **without** the `{-# INLINE #-}` pragmas the
abandoned branch carried. Everything else in the approved contract stands as
specified; no criterion changes.

The change is neutral on the 50k req/s budget: opt-in, pure, O(|name|) with
allocation bounded by key length and immediately collected, and zero cost to
every command that does not use it. The one caveat a consumer must carry is that
the input bound is theirs, not the framework's. Measurement stays the nightly
bench harness's job, not a PR gate.
