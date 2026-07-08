---
name: neohaskell-retrospective-miner
description: Weekly retrospective mining of pipeline telemetry into ≤5 validated asset-delta recommendations. Use at the weekly telemetry review, once real runs have accumulated in telemetry/runs.jsonl. Reads the deterministic digest, proposes recommendations under a closed contract, and records the ones that pass.
---

# Retrospective miner (weekly learning loop)

The judgement half of Phase 6's learning loop (ADR-0068). The deterministic half —
gathering runs, computing the digest, and enforcing the recommendation contract —
is `./dev retrospect`; this skill decides *what to recommend*. **Activation
gate:** run this only once `telemetry/runs.jsonl` holds real runs (it has one
dummy line today); with no data there is nothing to mine.

## Procedure

1. **Digest.** `./dev retrospect --since <YYYY-MM-DD of the week>` → the markdown
   digest (outcomes, failure labels, stage times, invented-API trend vs the
   first-5-runs baseline, waiting-on-human, assets-consulted frequency, bench
   pointer). This is your evidence base — do not recall, transcribe.
2. **Find friction.** From the digest + the week's golden stage summaries
   (`telemetry/golden/<run_id>/`), name concrete friction: an invented API that
   recurred, a stage repeatedly over its time-box, a failure label clustering, an
   asset nothing consulted.
3. **Propose ≤5 recommendations**, each under the closed contract:
   `{week, friction, evidence:[run:stage,…], delta_type, destination, estimated_saving_s}`.
4. **Record the survivors.** `./dev retrospect --record '<json>'` per recommendation
   — the harness re-checks the contract and appends to
   `telemetry/recommendations.jsonl`, or rejects with the failed rule. Do not
   hand-edit that file.

## The contract (the harness enforces all of this — internalize it, don't fight it)

- **Cited friction + measured cost (rule a):** `friction` is a real observation;
  `estimated_saving_s` is a positive, measured number. No counterfactual "this
  might help" — those are hypotheses, not findings, and are discarded.
- **≥2 independent runs (rule b):** `evidence` must cite the same friction across
  **two or more distinct run_ids**. One-off friction is noise; act on recurrence.
- **Closed taxonomy (rule c):** `delta_type` ∈ `alias | extension-point |
  phrasebook | hot-card | hlint-rule | hook | cli-utility | skill-edit |
  telemetry-label | PRUNE | none`. A proposal that needs a *new* asset kind or a
  new CLI utility is an ontology change — surface it to the maintainer in the
  review, never `--record` it.
- **Validation over time (rule d):** an implemented delta is `validated` against
  its claimed metric over subsequent weeks; no movement makes it a `PRUNE`
  candidate in turn. The weekly review flips `status`/`validated`.
- **Removal is first-class (rule e):** the digest's "assets consulted" frequency
  plus the asset inventory surface never-consulted assets — recommend `PRUNE`.

## Destinations (delta_type → the real file the fix lands in)

`alias` → `codemap/capabilities.yaml` · `extension-point` → `codemap/extension-points.yaml`
· `phrasebook` → `codemap/phrasebook.md` · `hot-card` → `codemap/api-hot.md`
· `hlint-rule` → `.hlint.yaml` (skill: `neohaskell-dialect-rules`) · `hook` →
`.claude/hooks/*` · `cli-utility` → `scripts/*` (+ a `./dev` verb) · `skill-edit`
→ `.claude/skills/*/SKILL.md` · `telemetry-label` → `telemetry/SCHEMA.md` (a
schema bump) · `PRUNE` → delete the unused asset · `none` → the `destination`
field carries the justification.

## Output of the weekly review

The review picks **1–2** recorded recommendations to implement that week (the
maintainer's call); the rest stay `proposed` for re-evaluation. The instance-fix
and the class-fix ship together (`telemetry.py finish --asset-delta` already
recorded the class-fix for each failed run this week — the miner generalizes
across runs, it does not replace the per-run delta).
