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
6. **Resolve symbols (Phase 4)**: for every API the task will call, resolve
   the exact name NOW — `codemap/api-hot.md` first (the ~160 functions that
   dominate real usage, with call-site counts), then `./dev api "<type>"` for
   type-directed lookup ("what turns Text into Uuid" → `Uuid.fromText`), then
   `codemap/phrasebook.md` for verified usage examples. The executor
   transcribes these; it must never recall from training data.
7. **Emit into the plan**: exact file paths, `uses:` (resolved symbols), the
   `touches:` capability list (drives test-impact + Phase 5 risk gates via
   `security-sensitive`/`perf-sensitive` tags).

## Worked example (the plan-fragment contract)

Request: *"cart items disappear when two users add simultaneously"*

```yaml
touches: [event-store]            # step 1-2: capability lookup
files:                            # step 3: from owns-globs + signatures grep
  - core/service/Service/EventStore/Postgres/Internal.hs
  - core/testlib/Test/Service/EventStore/OptimisticConcurrency/Spec.hs
uses:                             # step 6: resolved via api-hot + ./dev api
  - EventStore.insert
  - Service.Event.InsertAfter     # InsertionType for concurrency control
  - Task.mapError
  - Test.Spec conventions from the OptimisticConcurrency suite
blast-radius: ./dev who-calls insert Service.EventStore.Core
risk: [perf-sensitive, security-sensitive]   # from capability tags
```

Every field is a lookup result, not a guess — that is the whole contract.

## Confidence rule

Ambiguous match, empty match, or two capabilities feel equally right →
**escalate model tier / ask** — never guess, never free-explore. If the miss
is a vocabulary gap, the fix is an alias PR to `capabilities.yaml`
(one line, `./dev codemap-check` gates it), not a workaround.

## Verification

`./dev codemap-check` must be green; map changes require the routing smoke
(`codemap/routing-smoke.yaml`, protocol in `codemap/README.md`, bar ≥8/10).
