---
name: 01-produce
description: Produces the DevEx review document for the integration design draft.
kind: leaf
executor: opus
model: claude-opus-4-7
---

# Produce DevEx review

Reads the design draft and the grounded security/performance findings, evaluates the API surface against the Jess persona, and writes `.integration-pipeline/devex-review.md`.

## Review posture

Assume the artefact under review was produced by a language model (ChatGPT-class output). Treat plausible-looking claims as unverified, expect hallucinated APIs and missed constraints, and refuse to pass anything not directly traceable to the rubric in `../../references/devex-rubric.md` and the design draft itself. Strict review is the default — benefit of the doubt goes to the rubric and to the source, never to the producer.

## Inputs

- `.integration-pipeline/integration-design.md` — the design draft from phase 3.
- `.integration-pipeline/findings-04.json` — grounded security findings.
- `.integration-pipeline/findings-05.json` — grounded performance findings.
- `../../references/jess-persona.md` — the persona profile.
- `../../references/devex-rubric.md` — the rubric the review will be judged against; the producer must satisfy it.

## Plan

1. Read the design draft, both findings files, the Jess persona, and the rubric → verify: all five exist.
2. Identify every public function / type / error in the design draft's Public API → verify: list is non-empty.
3. For each public API entry, record the eight DevEx rubric checks (naming-conversions, predicates, subject-first, no-boolean-blindness, grouped-by-category, doc-by-example, type-parameter-discipline, Jess-affordance) with a per-entry verdict → verify: every entry has eight verdicts.
4. Fold security and performance findings into a single "API-surface impact" section → verify: every blocking finding from phases 4/5 that touches the public API is named.
5. Write `.integration-pipeline/devex-review.md` → verify: file exists with the sections in the Output contract below.

Assumptions:
- The design draft uses the NeoHaskell design draft template and has Status: `Proposed` or `Accepted`.
- The Jess persona and DevEx rubric are unmodified upstream files; do not paraphrase them inline.

If any assumption fails, refuse — do not guess.

## Steps

1. Load the design draft, both findings files, the persona, and the rubric. Refuse on any missing input.
2. Walk the design draft's Public API. For each public entry, record naming, signature shape, doc example, Jess-test results.
3. Apply each of the eight DevEx rubric checks per entry. Record `pass` / `fail` / `n/a` plus a one-line rationale.
4. Cross-reference `findings-04.json` and `findings-05.json`: any finding whose `location` is on the public API is added to the "API-surface impact" section.
5. Write `.integration-pipeline/devex-review.md` with these sections:
   - `## Summary` — one paragraph; the headline verdict and finding count.
   - `## Per-entry verdicts` — table: entry × eight rubric checks × verdict.
   - `## API-surface impact` — security/perf findings that intersect the public surface.
   - `## Action items` — concrete edits the design draft needs before phase 6 can advance.
6. Print the path to the written file. Do not run `pipeline.py complete`. The review-quality step owns that call.

## Output

`.integration-pipeline/devex-review.md` exists with the four sections above. Stdout is a single line: `wrote .integration-pipeline/devex-review.md`.

## Refusals

- Any prerequisite input missing → refuse with the missing path.
- The design draft's Public API section is empty → refuse: "design has no public API to review".
- A required Jess-test result is "unknown" or "cannot tell" → refuse: the producer must answer every test.
