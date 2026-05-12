# Test spec design quality rubric

The acceptance bar for a phase-8 test specification. Eight yes/no questions, grounded in patterns merged across `core/test/DecimalSpec.hs`, `core/test/TextSpec.hs`, `core/test/StreamSpec.hs`, `core/test/RedactedSpec.hs`, and the Karpathy guidelines. The review-quality agent applies every question to the test spec produced by phase 8.1.

The test spec passes when every question lands `yes`. The standard is "phase 9 can write Haskell tests from this spec without making coverage decisions".

## Contents

- [How to use this rubric](#how-to-use-this-rubric)
- [The eight checks](#the-eight-checks)
- [Karpathy carrier rules](#karpathy-carrier-rules)
- [Exemplar references](#exemplar-references)

## How to use this rubric

The review-quality agent reads the produced test spec at `docs/architecture/<adr-number>-<slug>-tests.md` plus the architecture doc at `docs/architecture/<adr-number>-<slug>.md`. For each question:

- `verdict`: `pass`, `fail`, or `n/a` with a one-line justification.
- `evidence`: spec section + line range, or architecture-doc cite when the spec is missing coverage.

If every question is `pass` or `n/a`, the agent runs `pipeline.py complete 8`. Any `fail` causes the agent to surface failures verbatim and stop the pipeline.

## The eight checks

1. **Coverage per public function.** Every public function listed in the architecture doc has at least three test cases in the spec — minimum 1 happy-path + 2 non-happy (edge / error). Functions with zero or one or two cases fail this check (DecimalSpec: 6+ cases for `decimal`/`fromCents`/`toCents` group; TextSpec averages 2.7 cases per function — that is the borderline floor).
2. **Edge-to-happy ratio.** The whole spec maintains an aggregate ratio of non-happy to happy of at least 1:1; the spec narrates the ratio with an explicit count line. Target stretch is 3:1 (per existing skill docs), but 1:1 is the hard floor. Below 1:1 fails this check.
3. **Round-trip tests for serializable types.** Every type that derives or hand-writes `ToJSON` / `FromJSON` (per the architecture doc) has at least one round-trip test case named explicitly (DecimalSpec lines 80-101 pattern: "round-trips correctly"). Missing round-trips for serializable types fail this check.
4. **Error / variant exhaustion.** Every constructor of every public error ADT (per the architecture doc's "Errors" section) is exercised in at least one test case. Every constructor of every public sum type is pattern-matched in at least one test case (StreamSpec lines 65-89 and 288-305: every variant tested). Missing constructors fail.
5. **Property invariants for laws.** Operations the architecture doc identifies as commutative, associative, idempotent, or round-tripping have at least one property-based test case named after the law. Operations that do not satisfy any law are explicitly recorded as `properties: none — <reason>`. Silent absence of property cases when laws apply fails this check.
6. **Boundary cases.** Every numeric / sized / indexed input is tested at: zero, one, the documented maximum, and one above the maximum (or the next-higher representable value). Text inputs are tested with empty and unicode-multibyte at minimum. Missing boundary cases fail this check.
7. **Each error condition reproduced.** For every error case listed in the architecture doc's "Errors" section, there is a test case whose stated purpose is to provoke that error. The test description names the trigger condition (e.g. "divides by zero", "rejects empty handle"). Generic "error handling" cases fail this check.
8. **Layout matches existing specs.** The spec organises cases as `describe "Module" > describe "Function" > it "specific case"` (DecimalSpec, TextSpec, StreamSpec). Flat lists or cases grouped by concept rather than by function fail this check.

## Karpathy carrier rules

- **Think before coding.** Each non-trivial test case has a one-line "what this case proves" rationale in the spec. Cases without rationale fail the carrier rule — the spec author has not articulated the goal.
- **Simplicity first.** No test case exists only to "cover a code path" — every case has a behavioural meaning. Coverage-driven cases that duplicate behavioural ones fail this carrier rule.
- **Surgical changes.** The spec touches only the new feature's functions. If the spec proposes adding tests to unrelated existing modules, the carrier rule fails — those are out of scope.
- **Goal-driven.** Each test case's expected outcome is stated as a concrete value or predicate (`returns Nothing` / `length == 0` / `error has constructor X`), never as "behaves correctly".

A spec that passes the eight checks but violates a carrier rule is still a `fail`.

## Exemplar references

- `core/test/DecimalSpec.hs:19-45, 59-74, 80-101` — happy/edge/JSON round-trip layout.
- `core/test/StreamSpec.hs:65-89, 288-305` — variant-exhaustion and error-condition coverage.
- `core/test/TextSpec.hs:1-304` — `describe`/`describe`/`it` nesting and case density (≈2.7 per function).
