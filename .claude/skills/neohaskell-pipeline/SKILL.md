---
name: neohaskell-pipeline
description: Orchestrate a NeoHaskell change end-to-end through the spec-gated pipeline - intake to merged PR with exactly two human gates. Use when implementing a feature, fixing a bug, or running any request that should produce a PR.
---

# The change pipeline (Phase 5)

Exactly **two human gates**: spec approval (draft PR) and final PR review.
Everything between them is mechanical or agent-run, resumable from
`.pipeline/state.json`, and telemetered. Stage names below are the telemetry
schema v2 canon (`telemetry/SCHEMA.md`) — state, telemetry lines, and this
skill share one vocabulary.

## Stage flow

```
intake ─ localize ─ spec ─▶ DRAFT PR ══ GATE 1 (maintainer) ══▶ design-review
        ─ plan ─ test-writing ─ implement ─ verify ─ pr ══ GATE 2 ══▶ ci ─ merged
```

1. **intake** — `./dev pipeline init --run-id YYYY-MM-DD-NNN --request issue#N
   --branch <branch>`; `scripts/telemetry.py` run start. Restate the request;
   ambiguity that changes the contract → one clarifying question NOW (cheap
   here, a wrong PR later).
2. **localize** — `neohaskell-localizer` skill. Output: capability IDs +
   `touches:`/`files:`/`uses:` lists → `./dev pipeline set plan.touches …`
   etc. The plan is now BINDING: resume never re-plans; a wrong plan parks
   the run (`wrong-localization`) and re-enters from intake, visibly.
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
   `waiting_on_human_s`, not a failure. The continue signal is a maintainer
   comment (`@claude` + instruction) — `claude.yml` ignores non-maintainers
   (author-association check). On approval: `./dev pipeline approve spec
   --by <who> --via pr-comment` (advance past `spec` is mechanically blocked
   without it), then `./dev pipeline advance`.
5. **design-review** — `./dev spec-check --plan <spec>` → `design_reviews`.
   `security` → `neohaskell-security-design-review` skill; `perf` →
   `neohaskell-performance-design-review`. Review records are committed to
   the PR branch (`NNN-slug.<kind>-review.md`). Empty list → skip (stage
   recorded with ~0 duration; the skip is the risk-tiering working).
6. **plan** — order the work: which files in what sequence, which neighbor
   module each copy-adapts from (`neohaskell-implementer` discipline).
7. **test-writing** — tests FIRST, from the criteria table, red before any
   implementation. Never weaken an existing expectation: the
   expectation-guard hook blocks it without the maintainer marker
   (`.pipeline/allow-expectation-edits`). New spec modules: register in the
   suite's `Main.hs` AND cabal `other-modules` (only `nhcore-test` is
   hspec-discovered).
8. **implement** — `neohaskell-implementer` skill; repair loop via
   `./dev check` (never `cabal build` in the loop); max 2 repair rounds per
   error, then the failure policy below.
9. **verify** — in order, no skipping:
   a. criteria tests green at their DECLARED levels (`./dev test "<pattern>" <suite>`)
   b. targeted regression: `./dev spec-check --plan <spec>` →
      `test_impact_globs` → run those suites
   c. `./dev lint` + `./dev spec-drift <spec>` (the promise check)
   d. full suite (`./dev test-all`) only here, once, at PR-ready
10. **pr** — flip the draft to ready-for-review (this re-triggers the full CI
    matrix; drafts run only the cheap checks). PR body: spec link, criteria →
    test mapping, review records. GATE 2 is the maintainer's normal review.
11. **ci** — watch checks; bot comments triaged (fix real findings; push
    back with evidence on wrong ones). Merge is the maintainer's.

After merge: `scripts/telemetry.py` finish (outcome `ok`), golden archive
(`telemetry/golden/<run_id>/`: request.md, spec.md, final.diff, verdict.md,
transcript.md).

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
`scripts/telemetry.py finish --outcome {parked,failed} --failure-label <l>
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
aids you consult while working (`scripts/telemetry.py consult --asset
<kind>:<name>`) so the weekly miner can PRUNE what nothing uses.
