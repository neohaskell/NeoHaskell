---
name: 02-classify-feature
description: Classifies the feature into a complexity tier that drives every later grounding pass.
kind: leaf
executor: script
---

# Classify feature

Emits the feature's tier (`trivial` / `simple` / `moderate` / `complex` / `security-critical`) to `.pipeline/classification.json`. The grounding loop reads this file to scale review intensity.

## Inputs

- `feature_name` — string, from pipeline state (auto-fetched).
- `issue_text` — string, the issue body (may be empty).
- `module_path` — string, from pipeline state.
- `touches` — JSON array of file globs (may be empty).
- `adds_dep` — boolean, does this feature add a new cabal/npm/nix dep?
- `external_io` — boolean, does it perform network/file/process IO?
- `touches_secrets` — boolean, does it produce/compare/store a secret value?
- `touches_auth` — boolean, does it touch auth or multi-tenant authorization?

The orchestrator gathers these from the ADR section "Decision drivers" and from the diff scope. Leave the booleans `false` if there is no evidence; the classifier prefers under-classifying to over-classifying.

## Plan (Karpathy 1 + 4)

1. Assemble the JSON payload from inputs → verify: valid JSON with the keys above.
2. Pipe payload into `classify-feature.py` → verify: stdout contains `"tier": "<one of the five>"`.
3. Persist the tier via `pipeline.py classify <tier> "<rationale>"` → verify: `.pipeline/classification.json` exists with the chosen tier.
4. Mark phase 2 complete → verify: `pipeline.py status` shows phase 2 done.

Assumptions:
- `pipeline.py` and `classify-feature.py` exist under `.claude/skills/feature-pipeline/scripts/`.
- The pipeline has been initialised (phase 1 complete).

If any assumption fails, refuse — do not invent a tier inline.

## Steps

1. Build the input JSON object from the gathered fields.
2. Run `printf '%s' "$payload" | python3 .claude/skills/feature-pipeline/scripts/classify-feature.py`. Capture `tier` and `rationale`.
3. Run `python3 .claude/skills/feature-pipeline/scripts/pipeline.py classify "$tier" "$rationale"`. Surface stderr on non-zero.
4. Run `python3 .claude/skills/feature-pipeline/scripts/pipeline.py complete 2`.

## Output

- `.pipeline/classification.json` exists with keys `tier`, `rationale`, `classified_at`.
- Phase 2 marked complete; pipeline now ready for phase 3.

## Refusals

- Pipeline not initialised → stop, point at phase 1.
- Classifier exits non-zero → surface stderr, do not write a fallback tier.
- The maintainer overrides the classifier with a hand-picked tier outside the cascade → record the override in the `rationale` field rather than silently substituting.
