# Telemetry schema v1 — FROZEN 2026-07-07

Every pipeline run emits **exactly one JSON line** appended to `telemetry/runs.jsonl`
(committed). Lines are emitted by `scripts/telemetry.py` — never hand-written.

## Run record

```json
{
  "schema": 1,
  "run_id": "2026-07-07-001",
  "request_ref": "issue#712 | adhoc:<slug>",
  "stages": {
    "<stage-name>": {
      "start": "2026-07-07T14:03:22Z",
      "stop": "2026-07-07T14:05:10Z",
      "model": "sonnet",
      "repair_rounds": 1
    }
  },
  "waiting_on_human_s": 340,
  "modules_rebuilt_after_restore": 4,
  "outcome": "ok",
  "failure_label": null
}
```

- `stages` — canonical stage names: `intake`, `localize`, `spec`, `design-review`,
  `plan`, `test-writing`, `implement`, `verify`, `pr`, `ci`. Add stages only by
  amending this schema (bump `schema`).
- `waiting_on_human_s` — total seconds parked on a human gate. Kept separate so
  work-time metrics stay honest.
- `modules_rebuilt_after_restore` — cache-health metric from `scripts/refresh-dev-cache`
  at run start (null if not refreshed).
- `outcome` — `ok | parked | failed | abandoned`.

## Failure-label taxonomy v1 (closed enum)

`wrong-intent | wrong-localization | dialect-violation | invented-api | test-failure |
spec-drift | flaky-infra | timeout | human-rejected-spec | human-rejected-pr | other`

`other` REQUIRES a `failure_note` field and must be re-classified (or the taxonomy
extended with a schema bump) at the weekly review. Free-text labels are forbidden —
they make trends unmeasurable.

## Golden-task record

Each run also archives under `telemetry/golden/<run_id>/` (local, gitignored —
retention/commit policy revisited when the retrospective miner lands in Phase 6):

```
request.md      # the original request, verbatim
spec.md         # the approved spec (contract delta)
final.diff      # the merged (or final) diff
verdict.md      # outcome + one-line cause
transcript.md   # session transcript or stage summaries (miner input)
```

## Inner-loop baseline (measured 2026-07-07, Apple Silicon, GHC 9.8.4, -O0 dev flavor)

| Measurement | Value | Target |
|---|---|---|
| ghcid error feedback after edit (`scripts/dev-loop`) | **0.55s** | <5s ✓ |
| ghcid recovery to "All good" | 1.9s | — |
| ghcid initial load (249 modules, warm .o) | ~60s (once per session) | — |
| `scripts/test-match "Text"` (nhcore-test-core) | **9.2s** | <30s ✓ |
| Full nhcore -O0 build (cold flavor) | 54s / 249 modules | — |
| Incremental: leaf module edit | 1 module / 4.9s | — |
| Incremental: comment-level edit to core/core/Text.hs | 6 modules / 6.2s | — |
| No-op `cabal build` overhead | 0.3s | — |

Notes: GHC recompilation checking is content-hash based (touch ≠ rebuild;
interface-preserving edits don't cascade). ghcid resolved via PATH, falling back
to `nix shell nixpkgs#ghcid` (not in the flake dev shell — adding it there is a
CI-cache decision deferred to Nick).
