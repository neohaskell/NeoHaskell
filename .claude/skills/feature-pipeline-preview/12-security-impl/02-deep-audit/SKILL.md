---
name: 02-deep-audit
description: Reads the implementation source and applies the security methodology to produce file-line findings.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Deep Security Audit

Reads changed source files plus siblings and applies every section of the security methodology, producing raw findings with `file:line` locations.

## Inputs

- stdin — JSON array of static-scan findings from step 1.
- Changed source files under `module_path` and their siblings.
- `../../references/security-methodology.md`
- `../../references/nhcore-context.md`

## Plan (Karpathy 1 + 4)

1. Read stdin → verify: array parses (may be empty).
2. Read the changed files and their siblings → verify: at least one file loaded.
3. Read the security methodology and nhcore context → verify: both files exist.
4. For each methodology section, ask its question against the code → verify: every section produces at least an explicit "not applicable" finding or a real one with `file:line`.

Assumptions:
- Static-scan findings are folded into the output, not replaced.
- Every finding includes `file:line`; lines are 1-indexed.
- No filtering happens here — that is the grounding step's job.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Parse stdin into the static-scan finding list.
2. Resolve the changed file list and load the source files plus immediate siblings.
3. Load the security methodology and nhcore context.
4. For each methodology section (input validation, authn/authz, injection, secrets, deserialization, error handling, logging, dependencies, etc.), walk the code and produce findings.
5. For each finding, populate `severity` (Critical/High/Medium/Low/Informational), `rule`, `location` (`file:line`), `message`, `recommendation`.
6. Merge the static-scan findings with the deep findings (deduplicate by `rule` + `location`).
7. Emit the merged JSON array on stdout.

## Output

JSON array of merged static + deep findings on stdout.

## Refusals

- Methodology or context reference missing → refuse: "security methodology or nhcore context missing".
- No changed files resolved → refuse: "no changed Haskell files".
