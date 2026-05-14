---
name: 01-hot-path-analysis
description: Analyses the design draft against the performance methodology to surface raw hot-path findings.
kind: leaf
executor: opus
model: claude-opus-4-7
---

# Hot-Path Analysis

Reads the design draft and asks every section of the performance methodology against it, emitting raw findings without filtering.

## Review posture

Assume the artefact under review was produced by a language model (ChatGPT-class output). Treat plausible-looking claims as unverified, expect hallucinated APIs and missed constraints, and refuse to pass anything not directly traceable to the methodology in `../../references/performance-methodology.md`. Strict review is the default — benefit of the doubt goes to the rubric and to the source, never to the producer.

## Inputs

- `.integration-pipeline/integration-design.md` — the design draft produced in phase 3.
- `../../references/performance-methodology.md` — methodology digest.
- `../../references/nhcore-context.md` — nhcore performance context.

## Plan

1. Read the design draft → verify: file exists and is non-empty.
2. Read the performance methodology and nhcore context → verify: both load.
3. For each numbered section of the methodology, ask its question against the design draft → verify: every section produces at least an explicit "not applicable" finding or a real finding.
4. Emit a JSON array of raw findings on stdout → verify: each entry has `severity`, `rule`, `location`, `recommendation`.

Assumptions:
- No filtering happens here — that is the grounding step's job.
- The 50k req/s target frames every severity decision (Blocking / Advisory / Informational).

If any assumption fails, refuse — do not guess.

## Steps

1. Load the design draft from `.integration-pipeline/integration-design.md`.
2. Load the performance methodology and nhcore context references.
3. For each methodology section (1-9), reason about the design draft section that maps to it; produce one or more findings.
4. For each finding, populate:
   - `severity`: `Blocking` / `Advisory` / `Informational`.
   - `rule`: short slug, e.g. `inline-pragmas`, `tojson-without-toencoding`, `tvar-map-contention`.
   - `location`: design draft section name or heading.
   - `recommendation`: a one-line fix.
5. Emit the JSON array on stdout.

## Output

JSON array of raw performance findings on stdout.

## Refusals

- `.integration-pipeline/integration-design.md` missing → refuse: "no design draft; run phase 03 first".
- Reference files missing → refuse: "performance methodology or nhcore context missing".
