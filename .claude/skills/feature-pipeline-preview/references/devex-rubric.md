# DevEx review quality rubric

The acceptance bar for a phase-6 DevEx review. Eight yes/no questions, grounded in patterns merged across `core/core/Redacted.hs`, `core/decimal/Decimal.hs`, `core/core/Uuid.hs`, `core/core/Result.hs`, `core/core/Array.hs`, and the Karpathy guidelines.

The review-quality agent applies every question to the DevEx review produced by phase 6.1 against the ADR. A finding survives only if it would be made by a senior reviewer reading the ADR cold. The DevEx review PASSES when every question lands `yes`.

## Contents

- [How to use this rubric](#how-to-use-this-rubric)
- [The eight checks](#the-eight-checks)
- [Karpathy carrier rules](#karpathy-carrier-rules)
- [Exemplar references](#exemplar-references)

## How to use this rubric

The review-quality agent reads the ADR's "Public API" section and the DevEx review produced by phase 6.1. For each rubric question, the agent records:

- `verdict`: one of `pass`, `fail`, `n/a`.
- `evidence`: one or two lines pointing at the ADR signature or DevEx-review paragraph that supports the verdict.

If every question is `pass` or `n/a`, the produce step passes. If any is `fail`, the agent surfaces the failures verbatim, refuses to mark phase 6 complete, and the pipeline halts at this step until the producer fixes the review.

## The eight checks

1. **Naming convention — conversions.** Every type-conversion function in the ADR's public API is named `fromX` or `toX`, never `xToY` or `convertX` (Karpathy "match existing style"; ref Decimal `fromCents`/`toCents`, Uuid `fromText`/`toText`).
2. **Naming convention — predicates and queries.** Every boolean predicate is named `is*` or uses plural form (e.g. `contains`); every query returns the value directly without an `xOrError` wrapper (Result `isOk`/`isErr`; Text `isPascalCase`).
3. **Subject-first arguments / pipe-friendliness.** Every operation on a value takes that value as the first argument so the call site reads `value |> Module.action` (Decimal, Text, Stream).
4. **No boolean blindness.** No function takes two-or-more `Bool` parameters; no function returns `Bool` where `Maybe a` / `Result e a` is semantically correct. `Decimal.divide` returns `Maybe` for the zero-divisor case; the ADR follows that pattern.
5. **API surface is grouped by category.** The ADR's "Public API" section organises declarations by purpose (Construction / Query / Transform / Errors), mirroring `core/core/Array.hs`. No flat dump of signatures.
6. **Documentation by example.** Every public function in the ADR carries a one-line doc-comment-style example that compiles. The DevEx review confirms this for each function (Redacted's `@` block pattern, Decimal `decimal` examples).
7. **Type-parameter discipline.** No type parameter is named with a single letter; no function carries more than three type parameters before its first arrow. Names are descriptive (`element`, `result`), never `a`/`b`/`c` (NeoHaskell style guide).
8. **Jess affordance.** Every public function passes the four Jess tests (autocomplete / shrug / default / 15-minute) recorded in the DevEx review with a one-line rationale per test. A `fail` on any Jess test is a `fail` on this check.

## Karpathy carrier rules

The review also enforces the Karpathy guidelines as cross-cutting:

- **Simplicity first.** The ADR proposes the minimum public surface the feature needs. If the review identifies a function that is "useful but speculative", the check fails — the function must be deferred or justified by a concrete caller in the ADR's Context section.
- **Surgical changes.** No public-API addition outside the feature's scope. If the ADR proposes touching adjacent modules' public API without naming why, the check fails.
- **Goal-driven.** Every public function in the ADR has a one-line user-facing goal stated in the review ("Jess can do X in Y seconds"). If a function exists without a stated user goal, the check fails.

These three rules apply to the review's overall verdict, not to the eight per-question checks above. A DevEx review that passes the eight checks but violates a carrier rule is still a `fail`.

## Exemplar references

When uncertain, consult these merged exemplars (lines as of 2026-05):

- `core/core/Redacted.hs:51-69` — doc-by-example pattern.
- `core/decimal/Decimal.hs:84-99, 163-170` — `fromX`/`toX` + `Maybe` for failure modes.
- `core/core/Uuid.hs:40-70` — minimal public API, conversion symmetry.
- `core/core/Array.hs:1-68` — grouped-by-category export list.
- `core/core/Result.hs:82` — subject-first transform functions.
