---
name: 01-hot-path-analysis
description: Analyses the ADR against the performance methodology to surface raw hot-path findings.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Hot-Path Analysis

Reads the ADR draft and asks every section of the performance methodology against it, emitting raw findings without filtering.

## Inputs

- `.pipeline/adr-draft.md` — the ADR produced in phase 3.
- `../../references/performance-methodology.md` — methodology digest.
- `../../references/nhcore-context.md` — nhcore performance context.

## Plan (Karpathy 1 + 4)

1. Read the ADR draft → verify: file exists and is non-empty.
2. Read the performance methodology and nhcore context → verify: both load.
3. For each numbered section of the methodology, ask its question against the ADR → verify: every section produces at least an explicit "not applicable" finding or a real finding.
4. Emit a JSON array of raw findings on stdout → verify: each entry has `severity`, `rule`, `location`, `recommendation`.

Assumptions:
- No filtering happens here — that is the grounding step's job.
- The 50k req/s target frames every severity decision (Blocking / Advisory / Informational).

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Load the ADR draft from `.pipeline/adr-draft.md`.
2. Load the performance methodology and nhcore context references.
3. For each methodology section (1-9), reason about the ADR section that maps to it; produce one or more findings.
4. For each finding, populate:
   - `severity`: `Blocking` / `Advisory` / `Informational`.
   - `rule`: short slug, e.g. `inline-pragmas`, `tojson-without-toencoding`, `tvar-map-contention`.
   - `location`: ADR section name or heading.
   - `recommendation`: a one-line fix.
5. Emit the JSON array on stdout.

## Output

JSON array of raw performance findings on stdout.

## Refusals

- `.pipeline/adr-draft.md` missing → refuse: "no ADR draft; run phase 03 first".
- Reference files missing → refuse: "performance methodology or nhcore context missing".
