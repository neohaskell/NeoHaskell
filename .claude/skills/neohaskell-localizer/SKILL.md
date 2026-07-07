---
name: neohaskell-localizer
description: Route a request to exact code locations using the codemap — capability lookup, extension points for new code, signature grep, edit-set expansion. Use at PLAN time, before any implementation; execution must never search.
---

# NeoHaskell localizer

Localization is lookup, not search. The assets (all in `codemap/`, all CI-gated):
capability ontology, extension points, generated API signatures, symbol DB.

## Procedure (≤3 minutes, zero exploration)

1. **Classify**: does the request touch existing code, or need new code?
2. **Existing** → match against `codemap/capabilities.yaml`. Select capability
   IDs from the closed list — aliases bridge intent vocabulary ("serve a SPA"
   → `http-transport`). NEVER invent paths; never grep the tree at this stage.
3. **New** → match `triggers` in `codemap/extension-points.yaml`. The row
   gives create/register/tests locations and the driving skill.
4. **Symbols**: grep the owning capability's surface in
   `codemap/signatures/*.txt` — `nhcore-core`, `nhcore-service`,
   `nhcore-auth-config-testlib`, `nhintegrations`. One Read/grep; do not open
   source files to discover APIs.
5. **Edit-set expansion**: `./dev who-calls <symbol> [defining-module]` —
   who breaks if this changes, grouped by capability. testbed hits = update
   acceptance tests too. (Index: `./dev hiedb`; caveat: record-field
   accessors may not resolve via name-refs — fall back to grep for those.)
6. **Emit into the plan**: exact file paths, symbols to use (`uses:`), the
   `touches:` capability list (drives test-impact + Phase 5 risk gates via
   `security-sensitive`/`perf-sensitive` tags).

## Confidence rule

Ambiguous match, empty match, or two capabilities feel equally right →
**escalate model tier / ask** — never guess, never free-explore. If the miss
is a vocabulary gap, the fix is an alias PR to `capabilities.yaml`
(one line, `./dev codemap-check` gates it), not a workaround.

## Verification

`./dev codemap-check` must be green; map changes require the routing smoke
(`codemap/routing-smoke.yaml`, protocol in `codemap/README.md`, bar ≥8/10).
