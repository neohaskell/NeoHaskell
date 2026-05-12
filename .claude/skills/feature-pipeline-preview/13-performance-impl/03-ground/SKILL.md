---
name: 03-ground
description: Applies the grounding loop to merged static and deep performance findings.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Ground Implementation Performance Findings

Filters merged static + deep performance findings through the 4-question grounding loop, the overkill catalogue, and the Jess override.

## Inputs

- stdin — JSON array of merged findings from step 2.
- `.pipeline/classification.json` — feature classification.

## Plan (Karpathy 1 + 4)

1. Read stdin → verify: array parses.
2. Read classification → verify: `tier` field exists.
3. Read `../../references/grounding-loop.md` and `../../references/jess-persona.md` → verify: both exist.
4. For every finding, apply the 4-question filter, the overkill catalogue, and the Jess override → verify: each finding has `grounding_outcome` and `severity_after_grounding`.

Assumptions:
- Grounding is mandatory — even zero findings emit `[]`.
- `file:line` references are preserved through grounding.
- The 50k req/s target frames every grounding decision.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Parse stdin findings; if empty, emit `[]` and exit 0.
2. Load classification and reference docs.
3. For each finding, ask the four grounding questions.
4. Cross-check against the overkill catalogue; demote or drop matches.
5. Apply the Jess override.
6. Annotate each finding with `grounding_outcome`, `severity_after_grounding`, `rationale`.
7. Emit the annotated JSON array on stdout.

## Output

JSON array on stdout, augmented with `grounding_outcome`, `severity_after_grounding`, `rationale`.

## Refusals

- stdin not valid JSON → refuse: "stdin is not a JSON array".
- Classification missing → refuse: "classification not found".
