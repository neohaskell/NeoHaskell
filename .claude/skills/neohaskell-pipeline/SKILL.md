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
        ─ plan ─ test-writing ─ implement ─ verify ─ pr ══ GATE 2 ══▶ ci ─ merged
```

1. **intake** — `./dev pipeline init --run-id YYYY-MM-DD-NNN --request issue#N
   --branch <branch>`; `./dev telemetry start`. Restate the request;
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
   `waiting_on_human_s`, not a failure. **How approval arrives (server-local
   canonical flow):** the orchestrator runs in a persistent local Claude/tmux
   session on the server, so GitHub Actions is never responsible for resuming
   it. Approval is delivered out-of-band — through RAMSYS/Discord or direct
   local interaction — and the orchestrator records it and resumes the *same*
   tmux session in place. Recording IS the authorization: `./dev pipeline
   approve <gate> --by <who> --via <channel>` (e.g. `approve spec --by Nick
   --via discord`, or `--via local`) writes it into `.pipeline/state.json`,
   which is the authorization record; advancing past a gate is mechanically
   blocked without it, then `./dev pipeline advance`. A GitHub PR comment is
   optional *communication* only — never the mechanism that resumes local work.
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
   implementation. Never weaken an existing expectation: the
   expectation-guard hook blocks it without the maintainer marker
   (`.claude/allow-expectation-edits`). New spec modules: register in the
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
   d. full suite (`./dev test-all`) only here, once, at PR-ready
10. **pr** — flip the draft to ready-for-review (this re-triggers the full CI
    matrix; drafts run only the cheap checks). PR body: spec link, criteria →
    test mapping, review records. **Flip the ADR's `## Status` to
    `Implemented`** here (and the matching row in `docs/decisions/README.md`,
    then `./dev adr-website` to resync the landing page) — an ADR whose code has
    shipped must not still read `Proposed`/`Accepted`; `./dev adr-check` gates
    file↔index consistency, and `Implemented` is an accepted alias of
    `Accepted`. GATE 2 is the maintainer's normal review.
11. **ci** — **not a single pass: loop until the checks and the review bot both
    settle green.** Each round: watch checks → read every new bot comment →
    triage → push the fix → wait for re-review. Repeat until CodeRabbit reports
    no outstanding actionable comments (its review state leaves
    `CHANGES_REQUESTED`) *and* the check matrix is green. A round that changes
    generated artifacts (`codemap/**`, `CHANGELOG.md`) must re-run its
    generator, never hand-edit — hand-editing is what breaks `codemap-sync`.
    Triage rule: fix real findings; **decline with a stated reason** (as a reply
    on the comment, so the record is on the PR) when the finding is wrong or
    targets a generated file whose formatting the generator owns. Merge is the
    maintainer's.

Close-out (at **PR-ready**, before the merge — NOT after): **stop the open stage
first** (`./dev telemetry stage --name pr --event stop`) — `finish` snapshots
whatever is recorded, so a stage left open is written to `runs.jsonl` with a null
`stop` and stays wrong forever. Then `./dev telemetry
finish` (outcome `ok`) appends the run's line to `telemetry/runs.jsonl`, and
`./dev telemetry golden` writes `telemetry/golden/<run_id>/` (request.md,
spec.md, final.diff, verdict.md, transcript.md). `runs.jsonl` is **tracked**
(`.gitattributes merge=union`), so commit it **into the PR** — it lands on
`main` via the squash-merge; there is no post-merge job to commit it (and
`main` is push-protected). The golden archive is **gitignored** — a local
reference artifact, never pushed (like the security review). Do this once CI is
green and the maintainer has approved; if a post-merge regression flips it,
`post-merge-guard.yml` marks it a revert-candidate.

## Failure policy (time-boxes → retry → escalate → park)

Per-stage time-boxes, v1 defaults (wall-clock, excluding `waiting_on_human_s`;
the weekly telemetry review recalibrates from measured stage times):

| intake | localize | spec | design-review | plan | test-writing | implement | verify | pr | ci |
|---|---|---|---|---|---|---|---|---|---|
| 10m | 10m | 30m | 20m | 15m | 30m | 45m | 30m | 10m | 45m |

On breach: **retry once** (fresh attempt, same plan) → **escalate model
tier** (haiku→sonnet→opus; record `model` per stage in telemetry) → **park**:
`./dev pipeline park --label <taxonomy> --note <one-liner>` + a structured
report comment on the PR/issue: stage, elapsed, label, last error verbatim,
what was tried. **A parked report beats a wrong PR** — parking is the
pipeline succeeding at honesty, not failing at work. Labels are the closed
taxonomy (SCHEMA.md); `other` requires `failure_note` and a weekly-review
reclassification.

**Closing a non-`ok` run carries the class-fix (Phase 6, enforced).**
`./dev telemetry finish --outcome {parked,failed} --failure-label <l>
--asset-delta <type>:<destination>` — the delta is the fix for the *class*
(a new alias, phrasebook entry, hlint rule, hook, hot-card line…) that ships
alongside the retry. `none:<reason>` is the honest escape when no asset applies;
the emitter refuses to close a failed/parked run without one.

## Resume contract

`./dev pipeline status` → resume at the recorded stage with the recorded
plan. Never re-derive `touches:`/`files:`/`uses:` on resume. If reality
contradicts the plan (file moved, API changed under you), park with
`wrong-localization` — the asset fix (alias, capability, extension point)
ships with the retry, per the failure→asset-delta protocol (ADR-0068). Log the
aids you consult while working (`./dev telemetry consult --asset
<kind>:<name>`) so the weekly miner can PRUNE what nothing uses.
