> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 03-ground
description: Applies the grounding loop to merged static and deep security findings.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Ground Implementation Security Findings

Filters merged static + deep findings through the 4-question grounding loop, the overkill catalogue, and the Jess override.

## Inputs

- stdin — JSON array of merged findings from step 2.
- `.pipeline/classification.json` — feature classification.

## Plan

1. Read stdin → verify: array parses.
2. Read classification → verify: `tier` field exists.
3. Read `../../references/grounding-loop.md` and `../../references/jess-persona.md` → verify: both exist.
4. For every finding, apply the 4-question filter, the overkill catalogue, and the Jess override → verify: each finding has `grounding_outcome` and `severity_after_grounding`.

Assumptions:
- Grounding is mandatory — even zero findings emit `[]`.
- `file:line` references are preserved through grounding.

If any assumption fails, refuse — do not guess.

## Steps

1. Parse stdin findings; if empty, emit `[]` and exit 0.
2. Load classification and reference docs.
3. For each finding, ask the four grounding questions.
4. Cross-check against the overkill catalogue; demote or drop matches.
5. Apply the Jess override — if the issue would not bite a 15-minute newcomer at realistic load, demote.
6. Annotate each finding with `grounding_outcome` (`keep`/`demote`/`drop`/`framework-debt`), `severity_after_grounding`, and `rationale`.
7. Emit the annotated JSON array on stdout.

## Output

JSON array on stdout, augmented with `grounding_outcome`, `severity_after_grounding`, `rationale`.

## Refusals

- stdin not valid JSON → refuse: "stdin is not a JSON array".
- Classification missing → refuse: "classification not found".
