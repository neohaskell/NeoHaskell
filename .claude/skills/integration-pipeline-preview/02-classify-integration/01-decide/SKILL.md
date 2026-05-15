---
name: 01-decide
description: Judges the integration's complexity tier from the design draft, issue, and diff scope, with explicit rationale.
kind: leaf
executor: opus
model: claude-opus-4-7
---

# Decide complexity tier

Returns the single tier that best describes the integration's blast radius. The output drives every later grounding pass — `trivial` integrations skip cryptographic and hot-path checks entirely; `security-critical` integrations cannot skip them. A miscalled tier compounds across phases.

## Inputs

- `integration_name` — string, the human-readable integration name from `pipeline.py get integration_name`.
- `issue_text` — string, the GitHub issue body if available (may be empty).
- `module_path` — string, the planned module location from `pipeline.py get module_path` (typically under `integrations/Integration/<Name>.hs`).
- `design_path` — optional, the path to the design draft if phase 3 has already produced one (defaults to `.integration-pipeline/integration-design.md`).
- `diff_scope` — optional list of file paths the integration touches, if known.

The orchestrator gathers what is available. Any missing field is `""` or `[]` — never invent content.

## The five tiers

| Tier | The integration… | Grounding intensity |
| --- | --- | --- |
| `trivial` | Adds no new trust boundary, no external IO, no new dependency, no new control flow. Examples: a `Mock` outbound integration used only in tests, a pure rename, a doc fix, an internal-only refactor. | Skips cryptographic, hot-path, and authorisation rubrics entirely. |
| `simple` | Pure helper module supporting an integration (e.g. shared request shaping, pure decoders). Rare for outbound integrations — most touch IO. | Eyeball-only security/perf checks; rejects every cascade recommendation by default. |
| `moderate` | A vanilla outbound integration: HTTP / queue / file-storage call, with retries and error handling but no auth secret or webhook signing. Touches external IO OR adds a dependency, but not both. | Keeps specific findings; demotes cascade recommendations unless directly applicable. |
| `complex` | Adds a streaming / WebSocket / long-lived-connection integration, schema migration, or contention-sensitive concurrent state. Touches both external IO AND a new dependency. | Full Top-10 + STRIDE + SSDF; profiling evidence required for any speedup claim. |
| `security-critical` | Stores or sends a secret (API key, OAuth token, webhook signing secret, bearer); verifies a webhook HMAC; enforces tenant isolation; mediates authentication. | Every methodology section active; no demotions. |

## Decision algorithm

State assumptions, define the verifiable goal, then execute:

1. **Read every input present.** If the design draft exists at `design_path`, read it end-to-end and consult its "Decision drivers", "Public API", and "Trust boundary" sections. If only `integration_name` + `issue_text` are present, work from those.
2. **Collect six signals.** For each, record `true` / `false` / `unknown`:
   - `touches_secrets` — does the integration produce, compare, sign, verify, or store a secret value (API key, token, HMAC, password, OAuth state, session id, webhook signing secret)?
   - `touches_auth` — does it touch authentication, authorisation, multi-tenant isolation, or `canAccess` / `canView` semantics?
   - `external_io` — does it perform network, file-system, or process IO? (Outbound integrations almost always do.)
   - `adds_dep` — does it add a new cabal / nix dependency?
   - `is_test_only` — is the integration scoped to test code (Mock, fixture, stub, harness)?
   - `is_pure_change` — is the entire diff a pure rename, doc edit, internal refactor, comment fix, or formatting change?
3. **Apply the cascade.** First rule that fires wins. Do not short-circuit early; finish reading inputs first.
   - Rule 1 — `touches_secrets ∨ touches_auth` ⇒ `security-critical`. No exceptions, even for test-only code that handles real secrets.
   - Rule 2 — `is_pure_change ∨ is_test_only` ⇒ `trivial`. **This rule overrides keyword matches like "integration" or "http" when the change is a mock or fixture.** A "Mock Stripe integration for tests" is `trivial`.
   - Rule 3 — streaming / long-lived / migration / multi-writer-concurrent-structure language present, OR (`external_io ∧ adds_dep`) ⇒ `complex`.
   - Rule 4 — request/response / outbound-call / queue-publish / file-upload vocabulary, OR `external_io`, OR `adds_dep` ⇒ `moderate`.
   - Rule 5 — otherwise ⇒ `simple`.
4. **Sanity-check.** Re-read the chosen tier's row in the table. If the description does not fit the integration, name the mismatch and pick again. If two tiers fit equally, pick the lower-intensity one (the grounding pass can always promote findings; cascade demotions are harder to undo).

## Output

Emit exactly one JSON document on stdout, no preamble, no trailing commentary:

```json
{
  "tier": "moderate",
  "rationale": "<one to three sentences naming the rule that fired and the decisive signals>",
  "signals": {
    "touches_secrets": false,
    "touches_auth": false,
    "external_io": true,
    "adds_dep": false,
    "is_test_only": false,
    "is_pure_change": false
  }
}
```

The persist step will read this JSON verbatim. Any deviation (preamble, multiple JSON blocks, missing field) breaks the pipeline.

## Refusals

- All inputs are empty or contradictory → refuse: "insufficient evidence to classify"; do not invent a tier.
- Any signal remains `unknown` at emit time → refuse: "signal `<name>` cannot be determined from inputs; ask the maintainer". The JSON output schema is boolean-only; `unknown` is a scratchpad state for the decision algorithm and must never appear in the emitted JSON.
- The chosen tier contradicts the explicit issue body (e.g. issue says "this stores OAuth tokens" but no auth/secret signal was set) → refuse: name the contradiction and stop.
