---
name: 02-ground
description: Applies the grounding loop to raw security findings to filter overkill and surface real blockers.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Ground Security Findings

Filters raw security findings through the 4-question grounding loop, the overkill catalogue, and the Jess override to produce an audit-ready set.

## Inputs

- stdin — JSON array of raw findings from step 1 (threat-model output).
- `.integration-pipeline/classification.json` — feature classification, one of the five pipeline tiers (`trivial` / `simple` / `moderate` / `complex` / `security-critical`).

## Plan

1. Read stdin into a finding list → verify: array parses, every entry has `severity`, `rule`, `location`, `recommendation`.
2. Read `.integration-pipeline/classification.json` → verify: `tier` field exists.
3. Read `../../references/grounding-loop.md` and `../../references/jess-persona.md` → verify: both files exist.
4. For every finding, apply the 4-question filter, consult the overkill catalogue, and apply the Jess override → verify: each finding has `grounding_outcome` (`keep`/`demote`/`drop`/`framework-debt`) and `severity_after_grounding`.

Assumptions:
- The grounding pass is MANDATORY. Even on `trivial` features with zero findings, emit `[]` to stdout to create the audit-trail record.
- Findings classified `framework-debt` stay in the output with that outcome — they are not silently dropped.

If any assumption fails, refuse — do not guess.

## Steps

1. Parse stdin findings; if empty, emit `[]` and exit 0.
2. Load classification tier and the two reference docs.
3. For each finding, ask the four grounding questions from `../../references/grounding-loop.md`.
4. Cross-check against the overkill catalogue; demote or drop matches.
5. Apply the Jess override — if the issue would not bite a 15-minute newcomer, demote.
6. Annotate each finding with `grounding_outcome`, `severity_after_grounding`, and a short `rationale`.
7. Emit the annotated JSON array on stdout.

## Output

JSON array on stdout, one entry per original finding, augmented with `grounding_outcome`, `severity_after_grounding`, `rationale`.

## Refusals

- stdin not valid JSON → refuse: "stdin is not a JSON array of findings".
- `.integration-pipeline/classification.json` missing → refuse: "classification not found; run phase 02 first".
