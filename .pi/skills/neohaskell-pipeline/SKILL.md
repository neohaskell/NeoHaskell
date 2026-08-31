---
name: neohaskell-pipeline
description: Orchestrate a NeoHaskell change end-to-end through the spec-gated pipeline - intake to merged PR with exactly two human gates. Use when implementing a feature, fixing a bug, or running any request that should produce a PR.
---

# The change pipeline (Phase 5)

> Restored as the authoritative change process by ADR-0076. The superseded
> queue-backed process was deleted; Git history is its recovery source.

Exactly **two human gates**: spec approval (draft PR) and final PR review.
Everything between them is mechanical or agent-run, resumable from
`.pipeline/state.json`, and telemetered. Stage names below are the telemetry
schema v4 canon (`telemetry/SCHEMA.md`) — state, telemetry lines, and this
skill share one vocabulary.

## Stage flow

```
intake ─ localize ─ spec ─▶ DRAFT PR ══ GATE 1 (maintainer) ══▶ design-review
        ─ plan ─ test-writing ─ implement ─ verify ─ pr ─ ci/review loop
        ══ GATE 2 (maintainer) ══▶ telemetry-only finalization ─ final checks ─ merged
```

**Telemetry transition invariant:** `./dev pipeline` owns resume state; telemetry
is explicit. On entry to every stage run `./dev telemetry stage --name <stage>
--event start`. Before every transition run `./dev telemetry stage --name
<stage> --event stop`, then `./dev pipeline advance`, then start `<next>`. Never
advance without that stop/start pair. Human waits use `./dev telemetry wait
--seconds <n>` and do not count against the stage time-box.

1. **intake** — `./dev pipeline init --run-id YYYY-MM-DD-NNN --request issue#N
   --branch <branch>`; `./dev telemetry start --run-id YYYY-MM-DD-NNN --request
   issue#N`; `./dev telemetry stage --name intake --event start`. Restate the request;
   ambiguity that changes the contract → one clarifying question NOW (cheap
   here, a wrong PR later).
2. **localize** — `neohaskell-localizer` skill. Output: capability IDs +
   `touches:`/`files:`/`uses:` lists → `./dev pipeline set plan.touches …`
   etc. The plan is now BINDING: resume never re-plans; a wrong plan parks
   the run (`wrong-localization`) and re-enters from intake, visibly.
   As you consult each aid (alias, capability, signature), log it:
   `./dev telemetry consult --asset <kind>:<name>` (e.g. `alias:http-transport`)
   — this feeds the miner's PRUNE of never-consulted assets.
3. **spec** — copy `docs/changes/TEMPLATE.md` → `NNN-slug.md` (next 3-digit
   number). Contract delta in signatures vocabulary; criteria C1…Cn each
   naming its proving test AND level (`unit|integration|acceptance` — a
   boundary-crossing behavior must declare integration/acceptance).
   `kind: bug` → C1 is the failing repro test, committed RED in the draft PR:
   the repro is the spec. ADR trigger flags honest (`./dev spec-check`
   cross-checks removals vs `breaking:`); triggered → write the ADR, link it
   — it's part of what the maintainer approves.
4. **GATE 1** — open a **draft PR** whose diff is the spec (+ADR, +red repro).
   Park: `./dev pipeline park` is NOT used here — waiting on the gate is
   `waiting_on_human_s`, not a failure. **How approval arrives (local-agent
   canonical flow):** the orchestrator remains in a persistent local agent
   session; hosted CI is never responsible for resuming it. Approval is
   delivered out-of-band — through RAMSYS/Discord or direct local interaction
   — and the orchestrator records it before resuming the local run. Recording
   IS the authorization: `./dev pipeline approve <gate> --by <who> --via
   <channel>` (e.g. `approve spec --by Nick --via discord`, or `--via local`)
   writes it into `.pipeline/state.json`, which is the authorization record;
   advancing past a gate is mechanically blocked without it, then `./dev
   pipeline advance`. A GitHub PR comment is optional *communication* only —
   never the mechanism that resumes local work.
5. **design-review** — `./dev spec-check --plan <spec>` → `design_reviews`.
   `security` → `neohaskell-security-design-review` skill; `perf` →
   `neohaskell-performance-design-review`. **Perf** records are committed to
   the PR branch (`NNN-slug.perf-review.md`); **security** records
   (`NNN-slug.security-review.md`) are **local-only — gitignored, never pushed**
   (they map attack surface; ADR-0069), enforced before PR-ready by
   `./dev spec-check --reviews-local` (CI's `--reviews-pr` gates only perf).
   Empty list → skip (stage recorded with ~0 duration; the skip is the
   risk-tiering working).
6. **plan** — order the work: which files in what sequence, which neighbor
   module each copy-adapts from (`neohaskell-implementer` discipline).
7. **test-writing** — tests FIRST, from the criteria table, red before any
   implementation. Never weaken an existing expectation. Under the harness
   configured by `.claude/settings.json`, the expectation-guard hook blocks it
   without the maintainer marker (`.claude/allow-expectation-edits`). Pi does
   not install those hooks: before continuing in Pi, run `python3
   .claude/hooks/expectation-guard.py --pr-diff
   <base-ref>`; CI runs the same census. New spec modules: register in the
   suite's `Main.hs` AND cabal `other-modules` (only `nhcore-test` is
   hspec-discovered).
8. **implement** — `neohaskell-implementer` skill; repair loop via
   `./dev check` (never `cabal build` in the loop); max 2 repair rounds per
   error, then the failure policy below. Log the aids you actually use
   (`./dev telemetry consult --asset hot-card:Text.toLower`,
   `phrasebook:task-validation`, …) so never-consulted assets surface as PRUNE.
9. **verify** — in order, no skipping:
   a. criteria tests green at their DECLARED levels (`./dev test "<pattern>" <suite>`)
   b. targeted regression: `./dev spec-check --plan <spec>` →
      `test_impact_globs` → run those suites
   c. `./dev lint` + `./dev spec-drift <spec>` (the promise check)
   d. PR-ready contract gates: `./dev spec-check --criteria-tests origin/main`,
      `./dev spec-check --reviews-local origin/main`, `./dev spec-check
      --reviews-pr origin/main`, and `./dev changelog --check origin/main`
   e. full suite once with mandatory dependencies: `./dev test-all
      --require-all`. A missing PostgreSQL or Hurl prerequisite is red, never a
      skipped-green result.
   f. acceptance as the user runs it: `./dev testbed`
10. **pr** — prepare the final substantive commit, then flip the draft to
    ready-for-review (this triggers the full CI matrix; drafts run only cheap
    checks). PR body: spec link, criteria → test mapping, review records. **Flip
    the ADR's `## Status` to `Implemented`** here (and the matching row in
    `docs/decisions/README.md`, then `./dev adr-website` to resync the landing
    page); run `./dev adr-check`. GATE 2 is the maintainer's normal review.
11. **ci → GATE 2 → telemetry-only finalization** — each CI round: watch
    checks → read every new bot comment → triage → push the
    fix → wait for re-review. Repeat until CodeRabbit has no outstanding
    actionable comments (its review state leaves `CHANGES_REQUESTED`) and the
    required check matrix is green on the substantive change. Generated
    artifacts (`codemap/**`, `CHANGELOG.md`) are regenerated, never hand-edited.
    Fix real findings; decline incorrect/generated-file findings with a reason
    on the PR.

    Once substantive CI and CodeRabbit are green, the maintainer performs Gate 2
    on that HEAD. Record the reviewed SHA before claiming success:

    ```sh
    ./dev pipeline approve ci --by <who> --via github-review \
      --head "$(git rev-parse HEAD)"
    ```

    **Finalization tail:** make no further product/doc changes. Stop the open
    stage, archive the now-truthful verdict, and finish telemetry:

    ```sh
    ./dev telemetry stage --name ci --event stop
    git diff origin/main...HEAD > /tmp/<run-id>.final.diff
    ./dev telemetry golden --run-id <run-id> --request-file <request.md> \
      --spec-file <spec.md> --diff-file /tmp/<run-id>.final.diff \
      --verdict "CI and Gate 2 satisfied" [--transcript-file <transcript.md>]
    ./dev telemetry finish --outcome ok
    ```

    Commit and push **only** `telemetry/runs.jsonl`, then wait for required
    checks and CodeRabbit. If GitHub invalidates the review, the maintainer may
    re-approve the metadata-only HEAD in GitHub, but the local `approve ci --head`
    record remains anchored to its substantive parent. Finally run `./dev
    pipeline complete --outcome ok`.

    `complete` requires the approved HEAD to be the direct parent of exactly one
    generated `telemetry/runs.jsonl` append; exact-HEAD equality cannot bypass
    this ordering. Any other delta requires reopening and Gate 2 again. It archives
    state under `.pipeline/completed/` and removes `state.json`. If the
    metadata-tail checks fail before `complete`, run `./dev telemetry reopen
    --run-id <run-id>`; this sanctioned command removes only the unmerged `ok`
    line, restores `.current-run.json`, and returns the same run to CI/Gate 2.
    Historical lines already on `origin/main` cannot reopen. Do not hand-edit the
    ledger. The golden archive is gitignored and local.
    Merge remains the maintainer's action. Post-merge `post-merge-guard.yml`
    marks a regression as a revert-candidate.

## Failure policy (time-boxes → retry → escalate → park)

Per-stage time-boxes, v1 defaults (wall-clock, excluding `waiting_on_human_s`;
the weekly telemetry review recalibrates from measured stage times):

| intake | localize | spec | design-review | plan | test-writing | implement | verify | pr | ci |
|---|---|---|---|---|---|---|---|---|---|
| 10m | 10m | 30m | 20m | 15m | 30m | 45m | 30m | 10m | 45m |

On breach: **retry once** (fresh attempt, same plan) → **escalate to the
next available model tier** (record the actual `model` per stage in telemetry)
→ **park**:
`./dev pipeline park --label <taxonomy> --note <one-liner>` + a structured
report comment on the PR/issue: stage, elapsed, label, last error verbatim,
what was tried. **A parked report beats a wrong PR** — parking is the
pipeline succeeding at honesty, not failing at work. Labels are the closed
taxonomy (SCHEMA.md); `other` requires `failure_note` and a weekly-review
reclassification.

**Closing a non-`ok` run carries the class-fix (Phase 6, enforced).** Stop the
open stage first; then close telemetry and resume state with the same outcome:

```sh
./dev telemetry stage --name <current-stage> --event stop
./dev telemetry finish --outcome <parked|failed> --failure-label <label> \
  --asset-delta <type>:<destination>
./dev pipeline complete --outcome <parked|failed>
```

The delta fixes the *class* (alias, phrasebook entry, hlint rule, hook, hot-card
line…) alongside the retry. `none:<reason>` is the honest escape when no asset
applies. The emitter refuses a missing delta; `complete` archives and releases
`state.json`, so parked/failed runs cannot block the next correction run.

## Resume contract

`./dev pipeline status` → resume at the recorded stage with the recorded
plan. Never re-derive `touches:`/`files:`/`uses:` on resume. If reality
contradicts the plan (file moved, API changed under you), park with
`wrong-localization` — the asset fix (alias, capability, extension point)
ships with the retry, per the failure→asset-delta protocol (ADR-0068). Log the
aids you consult while working (`./dev telemetry consult --asset
<kind>:<name>`) so the weekly miner can PRUNE what nothing uses.
