---
name: neohaskell-performance-design-review
description: Design-time performance review of an approved contract-delta spec, before implementation. Runs when the spec's touches list intersects perf-sensitive capabilities. Produces the committed review record; measurement lives in the nightly bench harness, not here.
---

# Performance design review (risk-tiered, design-time)

Rebuilt 2026-07-08 (Phase 5; predecessors archived in
`docs/archive/2026-07-ai-artifacts/claude-skills/` — note the archive's
blanket-INLINE/UNPACK advice was CORRECTED against GHC reality during
salvage; this file is the fixed version). Target: ~50k req/s event-sourcing
services. Hot-path budgets: command intake <1ms, event apply <0.5ms, query
<0.2ms, event persistence <1ms.

## When this runs (mechanical, not judgment)

`./dev spec-check --plan docs/changes/NNN-slug.md` → `design_reviews`
contains `"perf"` (spec `touches:` ∩ `perf-sensitive` capabilities). After
spec approval, before implementation. Untagged specs skip this. This review
is per-PR *assurance by reasoning*; per-night *measurement* is `./dev bench`
(nightly-bench.yml) — never conflate the two, and never demand benchmarks
in a PR.

## Compiler context (facts the review must not contradict)

- `Strict` is ON globally: let-bindings, args, and constructor fields are
  already strict. Never advise "add strictness" / bangs / `foldl'`.
- `Strict` does NOT cover: imported types (Prelude `Maybe`/lists/tuples stay
  lazy), nested patterns, or values inside containers (`Map k v` holds lazy
  `v`s). These are where leaks actually hide.
- At `-O` GHC unboxes small strict fields itself
  (`-funbox-small-strict-fields`): `UNPACK` on a word-sized field of a
  single-constructor type is noise. `UNPACK` earns its place only on
  multi-word strict fields or where the unfolding shows re-boxing at lazy
  call sites — and that claim needs a profile.

## Checklist (numbered — the record cites these IDs)

- **P1 Hot-path placement:** which promised functions sit on command intake /
  event apply / query / persistence? Budget impact estimate for each. Not on
  a hot path → the rest of this list demotes to informational.
- **P2 Specialisation:** exported polymorphic (typeclass-constrained)
  function called cross-module on a hot path → needs `INLINABLE` (GHC cannot
  specialise across modules without it). `INLINE` only for small functions
  that unlock fusion/RULES — never on >~10-line bodies.
- **P3 Laziness escapes:** `~` opt-outs in hot-path types need a justifying
  comment; hot-path data relying on Prelude `Maybe`/list under the false
  assumption `Strict` covers it; recursive accumulators building non-flat
  structures (thunk-graph retention).
- **P4 Serialization:** hand-written `ToJSON` on a hot codec (events, API
  responses, projections) must define `toEncoding`, not just `toJSON`
  (the default `toEncoding` via `toJSON` gives zero speedup; Generic/TH
  derivation emits a real one).
- **P5 Allocation:** `pack`/`unpack` or `encodeUtf8`/`decodeUtf8` round-trips
  more than once per request; `[fmt|…|]` inside tight loops; unfused
  `Array.map f |> Array.map g` chains; constants rebuilt inside loops.
- **P6 Contention:** `ConcurrentVar`/`TVar` wrapping a whole `Map` with
  multiple writer threads serialises all writers — push toward
  `ConcurrentMap` (stm-containers) or per-key sharding. Single-writer or
  read-mostly state: the plain var is correct, the swap is overkill.
- **P7 Evidence discipline:** any "this is faster" claim in the spec beyond
  removing the named anti-patterns requires measurement (a criterion/
  tasty-bench number or a profile) — otherwise downgrade the claim and note
  it for the nightly harness. New perf-tagged surface should propose a
  bench-budgets entry (`telemetry/bench-budgets.json`).

## Grounding filter (mandatory)

Same 4-question discipline as the security review: (1) measurable impact on
the 50k req/s budget given the workload tier? (2) is the path actually
exercised? (3) can the framework absorb the fix so users never see a pragma?
(4) proportional — no pragma cascades on code no profile has shown hot?
Findings failing any question are demoted to `informational` with the failed
question named. Premature-optimization cascades (INLINE+UNPACK+SPECIALIZE
everywhere) are a net negative: compile time, code size, lost fusion.

## Output (the committed record)

Write `docs/changes/NNN-slug.perf-review.md` on the PR branch (same shape as
the security record: checklist-ID table, grounding column, verdict, blocker
count + resolution). Blockers amend the plan or park the run; zero-finding
reviews still commit the record.
