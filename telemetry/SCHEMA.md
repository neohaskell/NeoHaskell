# Telemetry schema v2 — FROZEN 2026-07-08

v1 (frozen 2026-07-07) was bumped BEFORE the first real instrumented run —
the only v1 line in `runs.jsonl` is the Phase-1 schema-validation dummy —
to add per-stage `invented_api_events` (Phase 4 task 6). Extension always
requires a schema bump; this is that mechanism working as designed.

Every pipeline run emits **exactly one JSON line** appended to `telemetry/runs.jsonl`
(committed). Lines are emitted by `scripts/telemetry.py` — never hand-written.

## Run record

```json
{
  "schema": 2,
  "run_id": "2026-07-07-001",
  "request_ref": "issue#712 | adhoc:<slug>",
  "stages": {
    "<stage-name>": {
      "start": "2026-07-07T14:03:22Z",
      "stop": "2026-07-07T14:05:10Z",
      "model": "sonnet",
      "repair_rounds": 1,
      "invented_api_events": 0
    }
  },
  "waiting_on_human_s": 340,
  "modules_rebuilt_after_restore": 4,
  "outcome": "ok",
  "failure_label": null
}
```

- `run_id` — must match `^[A-Za-z0-9][A-Za-z0-9._-]*$` (enforced by the emitter;
  it is also the `telemetry/golden/<run_id>/` directory name). Convention:
  `YYYY-MM-DD-NNN`.
- `stages` — canonical stage names: `intake`, `localize`, `spec`, `design-review`,
  `plan`, `test-writing`, `implement`, `verify`, `pr`, `ci`. Add stages only by
  amending this schema (bump `schema`).
- `waiting_on_human_s` — total seconds parked on a human gate. Kept separate so
  work-time metrics stay honest.
- `modules_rebuilt_after_restore` — cache-health metric from `./dev refresh`
  at run start (null if not refreshed).
- `outcome` — `ok | parked | failed | abandoned`.

## Failure-label taxonomy v1 (closed enum)

`wrong-intent | wrong-localization | dialect-violation | invented-api | test-failure |
spec-drift | flaky-infra | timeout | human-rejected-spec | human-rejected-pr | other`

**invented-api counting (Phase 4):** `./dev check` reports
`invented-api-events=N` on typecheck failures — N is line-based and
approximate: `max(count of "not in scope" lines, count of GHC error-index
codes GHC-88464/GHC-76037)`, so a GHC rewording cannot silently zero the
metric (the error-index codes are stable by design; a `./dev doctor`
fixture self-test breaks loudly if BOTH stop matching). The pipeline runner
records N per stage via
`telemetry.py stage --name <s> --event stop --invented-api-events N`
(schema field `stages.<name>.invented_api_events`, default 0).

**Baseline (durable definition):** the invented-API baseline is the mean of
`sum(stages.*.invented_api_events)` over the **first 5 runs with
`outcome` in {ok, failed}** — parked and abandoned runs are excluded
(their stages are incomplete). The falling-trend claim (plan Phase 4) is
measured against that number. No historical replay.

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
| ghcid error feedback after edit (watcher + `./dev check`) | **0.55s** | <5s ✓ |
| ghcid recovery to "All good" | 1.9s | — |
| ghcid initial load (249 modules, warm .o) | ~60s (once per session) | — |
| `./dev test "Text"` (nhcore-test-core) | **9.2s** | <30s ✓ |
| Full nhcore -O0 build (cold flavor) | 54s / 249 modules | — |
| Incremental: leaf module edit | 1 module / 4.9s | — |
| Incremental: comment-level edit to core/core/Text.hs | 6 modules / 6.2s | — |
| No-op `cabal build` overhead | 0.3s | — |

Notes: GHC recompilation checking is content-hash based (touch ≠ rebuild;
interface-preserving edits don't cascade). All loop scripts self-provision the
pinned toolchain via `scripts/with-toolchain` (enters `nix develop` on demand;
ghcid ships in the flake dev shell) — callers never need to be inside the shell.
Wrapper overhead: ~0.4s warm eval. Caveat: if `dist-newstyle` was built under a
different environment than the wrapper's shell (config-hash mismatch), the first
ghcid load re-interprets all modules (minutes, one-time per flavor) instead of
loading warm `.o` files; reloads are sub-second either way.
