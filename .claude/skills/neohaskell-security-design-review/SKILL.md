---
name: neohaskell-security-design-review
description: Design-time security review of an approved contract-delta spec, before implementation. Runs when the spec's touches list intersects security-sensitive capabilities. Produces a LOCAL-only review record (gitignored, never pushed — a security review maps attack surface); enforced on the dev machine, not in CI.
---

# Security design review (risk-tiered, design-time)

Rebuilt 2026-07-08 (Phase 5; predecessors archived in
`docs/archive/2026-07-ai-artifacts/claude-skills/`). NeoHaskell serves
national-level European infrastructure: security compliance is a hard
requirement, and the review record is the audit-trail artifact — but it is kept
**local, never pushed** (a public repo would hand the attack-surface analysis to
attackers). The audit trail lives on the maintainer's machine / secure store, not
the public git history. See [ADR-0069](../../../docs/decisions/0069-security-reviews-are-local.md).

## When this runs (mechanical, not judgment)

`./dev spec-check --plan docs/changes/NNN-slug.md` → `design_reviews`
contains `"security"`. That is the ONLY trigger: it fires exactly when the
spec's `touches:` intersects a capability tagged `security-sensitive` in
`codemap/capabilities.yaml`. Untagged specs skip this entirely — gates scale
with blast radius. Timing: after the maintainer approves the spec (the
draft-PR gate), before any implementation.

## Inputs

1. The approved spec (`docs/changes/NNN-slug.md`) — contract delta + criteria.
2. The plan fragment (resolved `touches:`/`files:`/`uses:` — `./dev pipeline status`).
3. The touched capabilities' `responsibility:` lines (`codemap/capabilities.yaml`).

This reviews the *design*: the promised API, data flows, and trust boundaries.
Do not review code — none exists yet; findings amend the plan cheaply now
instead of the diff expensively later.

## Checklist (numbered — the record cites these IDs)

- **S1 Trust boundaries (STRIDE):** for every new boundary, data flow, or
  store the spec introduces, one question per STRIDE letter. No new boundary
  → S1 collapses to Tampering + Information-disclosure only.
- **S2 Parse, don't validate:** does the promised API take raw
  `Text`/`Int`/`Bytes` where a newtype + smart constructor would make the
  illegal state unrepresentable? Does anything return `Maybe`/`Task err ()`
  solely to report "input was invalid"?
- **S3 Secrets:** any value whose secrecy gates access → `Redacted` wrap or
  hand-written `Show` printing `<REDACTED>`; never `deriving (Show)` /
  derived `ToJSON` on a secret-bearing type; no secret in `[fmt|…|]`, error
  values, or logs. (Existing patterns: `Redacted`, OAuth2 `ClientSecret` /
  `AccessToken` / `HmacKey` hand-written Shows.)
- **S4 Comparison & randomness:** attacker-submittable long-lived secret
  compared → `constEq` (keeps its `{-# INLINE #-}` pragma); value that gates
  access / seeds crypto → `Crypto.Random`, never `System.Random`. A public
  ID needs neither.
- **S5 Authorization surface:** new queries define both `canAccess` and
  `canView`; `publicAccess` only with a justifying comment; new commands get
  `RequestContext` threaded (the type system forces this — verify the spec
  doesn't design around it).
- **S6 SQL / data access:** Hasql typed `Statement`/`Encoders`/`Decoders`
  only; entity names and stream IDs sanitized; never string concatenation.
- **S7 Supply chain:** `new-dependency: true` in the spec header → the dep
  must land in the hash-verified lockfile (`flake.lock` / cabal pin); any
  build step fetching a mutable URL or unpinned action is a blocker.
- **S8 Error surface:** no internals (connection strings, raw SQL, stack
  traces, tokens) in production-facing error messages; generic messages for
  401/403/500.

## Grounding filter (mandatory — proportionality is load-bearing)

Every finding must survive all four questions, or it is demoted to
`informational` with the failed question named:

1. **Blast radius** — can a realistic attacker, given the spec's reachable
   surface, convert this to concrete impact (exfil, integrity loss, DoS, EoP)?
2. **Reachability** — does the promised design actually exercise the path?
3. **User affordance** — is the mitigation absorbable by the framework
   (default, smart constructor) so downstream users never see a knob? Prefer
   that shape; if only a knob works, record `framework-debt`.
4. **Proportionality** — would a senior reviewer agree the fix matches the
   change's tier? No `constEq` on public IDs, no CSPRNG for retry jitter, no
   `Redacted` on values already in public logs. Control-cascades on a change
   touching no secret and no boundary are security theater — reject them.

## Output (the LOCAL record — never commit)

Write `docs/changes/NNN-slug.security-review.md` on your local working tree. It
is **gitignored** (ADR-0069) — do NOT commit or push it; a security review maps
attack surface and must never enter public git history. The pipeline enforces
its presence on your machine via `./dev spec-check --reviews-local` before the PR
flips to ready; CI never sees it (`--reviews-pr` gates only perf reviews). Format:

```markdown
# Security design review: <spec title>
Spec: docs/changes/NNN-slug.md | Capabilities: <security-sensitive ones> | Date: <date>

| # | Checklist | Finding | Grounding | Verdict |
|---|-----------|---------|-----------|---------|
| 1 | S3 | <what> | kept / demoted (failed Q1: <why>) | blocker / advisory / informational |

**Blockers:** N — <resolution: plan amended / escalated to maintainer>
```

Blockers either amend the plan (`./dev pipeline set plan.files …` + note in
the record) or park the run for the maintainer (`./dev pipeline park --label
human-rejected-spec --note …`). Zero-finding reviews still write the local record
— "reviewed, nothing found" is audit data — it just stays local like every other
security review.
