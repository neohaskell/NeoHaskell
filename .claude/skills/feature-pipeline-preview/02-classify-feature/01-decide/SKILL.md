---
name: 01-decide
description: Judges the feature's complexity tier from the ADR, issue, and diff scope, with explicit rationale.
kind: leaf
executor: opus
model: claude-opus-4-7
---

# Decide complexity tier

Returns the single tier that best describes the feature's blast radius. The output drives every later grounding pass — `trivial` features skip cryptographic and hot-path checks entirely; `security-critical` features cannot skip them. A miscalled tier compounds across phases.

## Inputs

- `feature_name` — string, the human-readable feature name from `pipeline.py get feature_name`.
- `issue_text` — string, the GitHub issue body if available (may be empty).
- `module_path` — string, the planned module location from `pipeline.py get module_path`.
- `adr_path` — optional, the path to the ADR if phase 3 has already produced one.
- `diff_scope` — optional list of file paths the feature touches, if known.

The orchestrator gathers what is available. Any missing field is `""` or `[]` — never invent content.

## The five tiers

| Tier | The feature… | Grounding intensity |
| --- | --- | --- |
| `trivial` | Adds no new trust boundary, no external IO, no new dependency, no new control flow. Examples: a `Mock` integration used only in tests, a pure rename, a doc fix, a test fixture, an internal-only refactor. | Skips cryptographic, hot-path, and authorisation rubrics entirely. |
| `simple` | Adds a new pure value type or pure function with no IO and no dependency. Example: a `Decimal` type, a `Uuid` newtype. | Eyeball-only security/perf checks; rejects every cascade recommendation by default. |
| `moderate` | Adds a new query / command / decider / serialiser / persistence schema, or a new in-process integration. Touches external IO OR adds a dependency, but not both. | Keeps specific findings; demotes cascade recommendations unless directly applicable. |
| `complex` | Adds a new event-store layer, new networked subsystem, schema migration, replication, queue, or contention-sensitive concurrent structure. Touches both external IO AND a new dependency. | Full Top-10 + STRIDE + SSDF; profiling evidence required for any speedup claim. |
| `security-critical` | Produces, compares, signs, verifies, or stores a value whose secrecy gates access. Touches authn / authz, multi-tenant isolation, HMAC / JWT / OAuth / session / API-key handling. | Every methodology section active; no demotions. |

## Decision algorithm

State assumptions, define the verifiable goal, then execute:

1. **Read every input present.** If the ADR exists at `adr_path`, read it end-to-end and consult its "Decision drivers" and "Public API" sections. If only `feature_name` + `issue_text` are present, work from those.
2. **Collect six signals.** For each, record `true` / `false` / `unknown`:
   - `touches_secrets` — does the feature produce, compare, sign, verify, or store a secret value (API key, token, HMAC, password, OAuth state, session id, share-link)?
   - `touches_auth` — does it touch authentication, authorisation, multi-tenant isolation, or `canAccess` / `canView` semantics?
   - `external_io` — does it perform network, file-system, or process IO?
   - `adds_dep` — does it add a new cabal / npm / nix dependency?
   - `is_test_only` — is the feature scoped to test code (Mock, fixture, stub, harness)? `Mock` integrations are test-only.
   - `is_pure_change` — is the entire diff a pure rename, doc edit, internal refactor, comment fix, or formatting change?
3. **Apply the cascade.** First rule that fires wins. Do not short-circuit early; finish reading inputs first.
   - Rule 1 — `touches_secrets ∨ touches_auth` ⇒ `security-critical`. No exceptions, even for test-only code that handles real secrets.
   - Rule 2 — `is_pure_change ∨ is_test_only` ⇒ `trivial`. **This rule overrides keyword matches like "integration" or "http" when the feature is a mock or fixture.** A "Mock HTTP integration for tests" is `trivial`.
   - Rule 3 — event-store / migration / networked-subsystem / multi-writer-concurrent-structure language present, OR (`external_io ∧ adds_dep`) ⇒ `complex`.
   - Rule 4 — query / command / decider / serialiser / integration vocabulary, OR `external_io`, OR `adds_dep` ⇒ `moderate`.
   - Rule 5 — otherwise ⇒ `simple`.
4. **Sanity-check.** Re-read the chosen tier's row in the table. If the description does not fit the feature, name the mismatch and pick again. If two tiers fit equally, pick the lower-intensity one (the grounding pass can always promote findings; cascade demotions are harder to undo).

## Output

Emit exactly one JSON document on stdout, no preamble, no trailing commentary:

```json
{
  "tier": "trivial",
  "rationale": "<one to three sentences naming the rule that fired and the decisive signals>",
  "signals": {
    "touches_secrets": false,
    "touches_auth": false,
    "external_io": false,
    "adds_dep": false,
    "is_test_only": true,
    "is_pure_change": false
  }
}
```

The persist step will read this JSON verbatim. Any deviation (preamble, multiple JSON blocks, missing field) breaks the pipeline.

## Refusals

- All inputs are empty or contradictory → refuse: "insufficient evidence to classify"; do not invent a tier.
- A signal is `unknown` AND it would flip the rule that fires → refuse: "signal `<name>` cannot be determined from inputs; ask the maintainer".
- The chosen tier contradicts the explicit issue body (e.g. issue says "this stores OAuth tokens" but no auth/secret signal was set) → refuse: name the contradiction and stop.
