> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 02-deep-audit
description: Reads the implementation source and applies the performance methodology to produce file-line findings.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Deep Performance Audit

Reads changed source files plus siblings and applies every section of the performance methodology, producing raw findings with `file:line` locations.

## Review posture

Assume the artefact under review was produced by a language model (ChatGPT-class output). Treat plausible-looking claims as unverified, expect hallucinated APIs and missed constraints, and refuse to pass anything not directly traceable to the methodology in `../../references/performance-methodology.md` and the actual source lines. Strict review is the default — benefit of the doubt goes to the rubric and to the source, never to the producer.

## Inputs

- stdin — JSON array of static-scan findings from step 1.
- Changed source files under `module_path` and their siblings.
- `../../references/performance-methodology.md`
- `../../references/nhcore-context.md`

## Plan

1. Read stdin → verify: array parses.
2. Read changed files and siblings → verify: at least one file loaded.
3. Read the performance methodology and nhcore context → verify: both exist.
4. For each methodology section, ask its question against the code → verify: every section produces at least a "not applicable" or real finding with `file:line`.

Assumptions:
- Static-scan findings are folded in, not replaced.
- Every finding includes `file:line`; lines are 1-indexed.
- The 50k req/s target frames severity.

If any assumption fails, refuse — do not guess.

## Steps

1. Parse stdin into the static-scan finding list.
2. Resolve and load the changed files plus immediate siblings.
3. Load the performance methodology and nhcore context.
4. For each methodology section (1-9), walk the code and produce findings.
5. For each finding, populate `severity` (Blocking/Advisory/Informational), `rule` (e.g. `inline-pragmas`, `tojson-without-toencoding`, `tvar-map-contention`), `location` (`file:line`), `message`, `recommendation`.
6. Normalise every finding's location first: if a static-scan entry carries `file` and `line` instead of `location`, set `location = "<file>:<line>"` before dedup. After normalisation every entry has the same `location` shape (`file:line`).
7. Merge static-scan findings with deep findings (deduplicate by the normalised `rule` + `location` pair).
8. Emit the merged JSON array on stdout.

## Output

JSON array of merged static + deep performance findings on stdout.

## Refusals

- Methodology or context reference missing → refuse: "performance methodology or nhcore context missing".
- No changed files resolved → refuse: "no changed Haskell files".
