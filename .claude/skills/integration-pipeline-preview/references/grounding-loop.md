# The grounding loop

A mandatory pass that runs after every security review (ADR + impl) and after every performance review (ADR + impl). Its job is to keep review intensity proportional to the feature's blast radius and to keep the recommendations inside Jess's 15-minute pocket.

The grounding pass is the load-bearing safeguard against "cascade reviews" where a simple feature (e.g. adding a `Mock` integration for tests) acquires unwarranted caching layers, SHA verification chains, constant-time compares for public IDs, `UNPACK` pragmas on cold paths, or `stm-containers` swaps for a `TVar` no concurrent writer touches.

## Contents

- [Inputs](#inputs)
- [The 4-question filter (run on every finding)](#the-4-question-filter-run-on-every-finding)
- [Blast-radius matrix by feature tier](#blast-radius-matrix-by-feature-tier)
- [Overkill catalogue (auto-demote unless justified)](#overkill-catalogue-auto-demote-unless-justified)
- [The Jess override](#the-jess-override)
- [Output contract](#output-contract)
- [Worked examples](#worked-examples)

## Inputs

The grounding pass receives:

1. The raw findings list emitted by the preceding deep-audit step.
2. The feature classification at `.integration-pipeline/classification.json` (one of: `trivial`, `simple`, `moderate`, `complex`, `security-critical`).
3. The Jess persona (`references/jess-persona.md`).
4. The relevant methodology digest (`references/security-methodology.md` or `references/performance-methodology.md`).
5. The diff scope: which files, modules, and trust boundaries the feature actually touches.

## The 4-question filter (run on every finding)

For each finding the deep-audit produced, the grounding pass answers four yes/no questions. A finding survives only if every answer is "yes". If any answer is "no", the finding is **demoted** to `informational` and excluded from the blocking set; the grounding record explains *why* it was demoted, citing the question that failed.

1. **Blast-radius**: given the diff's reachable surface, can a realistic attacker / a realistic production workload convert this finding into a concrete impact (data exfil, integrity loss, DoS, EoP, or measurable latency/allocation regression on the 50k req/s budget)?
2. **Reachability**: does the new code actually exercise the code path the finding describes, in the workload tier the feature targets? A finding on a hot-path control flow that this feature never touches is out of scope.
3. **Jess affordance**: is the recommended mitigation something Jess can apply in 15 minutes from autocomplete + the ADR's "Public API" block, *or* something the framework can absorb so Jess never sees it?
4. **Proportionality**: would a senior reviewer, looking at the diff blind, agree the recommended fix matches the feature's tier (per the matrix below)? A `security-critical` finding on a `trivial` feature triggers a sanity recheck of the classification before recording.

## Blast-radius matrix by feature tier

| Tier | Definition | Active security sections | Active performance sections | Default verdict on borderline findings |
| --- | --- | --- | --- | --- |
| `trivial` | Pure renames, doc fixes, test fixtures, mock implementations, internal-only refactors. | none (only A06 design smells) | none (only obvious anti-patterns) | demote |
| `simple` | New value type, new pure function, new module with no external IO. | A06, sections 5, 8 | sections 1–6 (eyeball only) | demote unless directly applicable |
| `moderate` | New protocol integration, new query/command, new persistence schema. | A03, A04, A05, A06, A09, A10; sections 2, 5, 6, 8 | sections 1–7 | keep specific findings, demote cascades |
| `complex` | New event-store layer, new auth flow, new networked service. | full Top-10 + STRIDE + SSDF PW.1/5/7/9 | full digest + profiling required | keep all justified findings |
| `security-critical` | Anything emitting/comparing secrets, signing, key material, multi-tenant authorisation. | every section, no skipping | full digest + profiling required | keep all justified findings |

The classification is produced by phase 2 (`02-classify-feature/`) — an opus `01-decide` step emits the JSON, a script `02-persist` step writes `.integration-pipeline/classification.json`. If the grounding pass disagrees with the classifier, it records its rebuttal and asks the maintainer to re-run classification — it does not silently override.

## Overkill catalogue (auto-demote unless justified)

The following recommendations are demoted by default unless the diff matches the listed justifying condition. The grounding record names the catalogue entry that fired.

| Recommendation | Auto-justify only if… |
| --- | --- |
| Constant-time compare (`constEq`) | the compared value is a long-lived attacker-submittable secret per [security §6](./security-methodology.md#6-constant-time-comparison-for-secrets). |
| CSPRNG (`Crypto.Random`) | the value gates access, seeds crypto, or is a session/CSRF/OAuth token per [security §7](./security-methodology.md#7-cryptographically-secure-randomness). |
| `ScrubbedBytes` / memory zeroization | the secret is long-lived in process memory per [security §8](./security-methodology.md#8-secret-handling-20252026-best-practice). |
| SHA-pinning a dependency | the dep is fetched at build time outside a hash-verified lockfile per [security §3](./security-methodology.md#3-slsa-v11-supply-chain). |
| Caching layer | a profile shows the underlying call as a top-N cost centre per [perf §7](./performance-methodology.md#7-profiling-first-vs-eyeball-review). |
| `{-# INLINE #-}` | function is small, in a hot path, and unlocks fusion/`RULES` per [perf §1](./performance-methodology.md#1-inline--inlinable--specialize). |
| `{-# UNPACK #-}` | field is in a sum-type, or is being passed only to strict consumers per [perf §2](./performance-methodology.md#2-unpack-with-strict-fields). |
| `stm-containers` / sharded map swap | multi-writer contention on a hot map is shown or strongly likely per [perf §8](./performance-methodology.md#8-stm--ioref--tvar-contention). |
| Hand-written `toEncoding` | the codec is on a serialisation hot path per [perf §4](./performance-methodology.md#4-toencoding-vs-tojson-aeson). |
| Custom error ADT replacing `Text` errors | the error is consumed by user-facing code, not internal plumbing. |

## The Jess override

Even when a finding survives the 4-question filter, the grounding pass applies one final check: would the recommended mitigation expose Jess to a knob, pragma, or vocabulary item she would not normally encounter? If yes, the grounding pass rewrites the recommendation into one of:

1. **Framework absorption** — move the mitigation into a default the framework applies for every user, so Jess never sees it.
2. **Smart constructor** — change the public API so the unsafe shape is unrepresentable, so Jess cannot reach the bad path.
3. **Deferred** — record the finding as a follow-up issue against the framework, not a blocker on Jess's feature.

If none of the three is feasible, the finding is recorded as a blocker, but the grounding pass attaches a `framework-debt` note for the maintainer.

## Output contract

The grounding pass emits a structured record per finding, written by `03-record/SKILL.md` (or `04-record/SKILL.md` for the impl reviews) to `.integration-pipeline/findings-{phase}.json`:

```json
{
  "id": "sec-04-002",
  "source_finding_id": "sec-deep-007",
  "severity_input": "high",
  "severity_after_grounding": "informational",
  "grounding_outcome": "demoted",
  "reasons": ["fails Q1 blast-radius: no attacker path"],
  "catalogue_match": "constant-time-compare",
  "jess_override": "framework-absorption",
  "blocker": false
}
```

Aggregate fields: `total_findings`, `blockers`, `kept`, `demoted`, `framework_debt`. The pipeline only blocks on `blockers > 0`.

## Worked examples

### Example A — "Add a `Mock` HTTP integration for tests"

Classifier emits `trivial`. Deep-audit returns 6 findings: missing `INLINE`, no `toEncoding`, no `ScrubbedBytes` on the mock's fake API key, no `constEq` on the mock's expected-vs-received header check, no SHA-pin on the test fixture URL, no `stm-containers` for the mock's call-log `TVar (Map ...)`. The grounding pass demotes all six: tier is `trivial` so no hot-path or secret applies, the "fake API key" never leaves the test process, the mock's `TVar` has one writer (the test harness), the fixture URL is local. Result: 0 blockers, 6 informational, 0 framework-debt. Feature ships.

### Example B — "Add HMAC-signed state tokens for OAuth"

Classifier emits `security-critical`. Deep-audit returns: missing `constEq` on the HMAC comparison, `Show` derived on the token type, no rate-limit, `System.Random` used to seed the HMAC key. Grounding pass keeps all four (Q1–Q4 all yes), applies Jess override "smart constructor" to the `Show` finding (the framework should provide a `HmacKey` newtype with hand-written `Show`, recorded as `framework-debt`), and records 4 blockers. The feature stops here until fixes land.
