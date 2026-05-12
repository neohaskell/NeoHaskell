---
name: 02-persist
description: Persists the classifier's JSON output to the pipeline state and marks phase 2 complete.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Persist classification

Reads the decide step's JSON output, calls `pipeline.py classify <tier> "<rationale>"`, then `pipeline.py complete 2`. The script does not interpret the classifier's decision — it only persists it.

## Inputs

- `decide_output` — the JSON document on stdout from `../01-decide/`. Must parse to an object with at least `tier` (one of `trivial`, `simple`, `moderate`, `complex`, `security-critical`) and `rationale` (string).

## Plan (Karpathy 1 + 4)

1. Parse the JSON → verify: `tier` and `rationale` are present, `tier` is one of the five valid values.
2. Run `pipeline.py classify "$tier" "$rationale"` → verify: exit 0; `.pipeline/classification.json` exists with the tier.
3. Run `pipeline.py complete 2` → verify: `pipeline.py status` shows phase 2 done.

Assumptions:
- The decide step produced exactly one JSON document with no preamble.
- The pipeline has been initialised (phase 1 complete).

If any assumption fails, refuse — do not invent a tier.

## Steps

1. Extract `tier` and `rationale` from the decide step's JSON. Use `jq -r '.tier'` and `jq -r '.rationale'` for a clean parse; fail with the parse error on stderr if either field is missing.
2. Invoke `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py classify "$tier" "$rationale"`. Surface stderr on non-zero exit.
3. Invoke `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 2`. Surface stderr on non-zero exit.

## Output

- `.pipeline/classification.json` exists with keys `tier`, `rationale`, `classified_at`.
- Phase 2 marked complete; the pipeline is ready for phase 3.

## Refusals

- JSON does not parse → refuse with the parser error.
- `tier` is not one of the five valid values → refuse and name the invalid value; do not coerce.
- `pipeline.py classify` exits non-zero → surface stderr; do not retry with a different tier.
