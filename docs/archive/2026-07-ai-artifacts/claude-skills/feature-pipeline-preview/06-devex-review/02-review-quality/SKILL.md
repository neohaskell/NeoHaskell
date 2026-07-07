> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 02-review-quality
description: Independently validates the DevEx review against the rubric and gates phase 6 completion.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Review DevEx-review quality

Reads the DevEx review and the ADR, applies every check in `../../references/devex-rubric.md`, records the per-check verdict to `.pipeline/devex-review-rubric.json`, and gates phase 6 completion on the result.

This step is the auto-gate that replaces the human PAUSE: when the rubric says `pass`, the pipeline advances automatically; when it says `fail`, the pipeline halts here with the rubric record as the audit trail.

## Review posture

Assume the artefact under review was produced by a language model (ChatGPT-class output). Treat plausible-looking claims as unverified, expect hallucinated APIs and missed constraints, and refuse to pass anything not directly traceable to the rubric in `../../references/devex-rubric.md` and the ADR itself. Strict review is the default — benefit of the doubt goes to the rubric and to the source, never to the producer.

## Inputs

- `.pipeline/devex-review.md` — produced by `../01-produce/`.
- `docs/decisions/<adr-number>-<slug>.md` — the ADR the review covers.
- `../../references/devex-rubric.md` — the eight-check rubric + three carrier rules.
- `../../references/jess-persona.md` — for cross-referencing the Jess affordance check.

## Plan

1. Load all four inputs → verify: every file exists; refuse on any missing.
2. For each of the eight rubric checks, decide `pass` / `fail` / `n/a` from the evidence in the review + ADR → verify: every check has a verdict + an evidence cite (file path + section/line).
3. For each of the three carrier rules, decide `pass` / `fail` → verify: every rule has a verdict.
4. Write the rubric record JSON to `.pipeline/devex-review-rubric.json` → verify: file exists with `checks`, `carriers`, `verdict`.
5. If `verdict == "pass"`, run `pipeline.py complete 6` and print `RUBRIC: pass`. If `verdict == "fail"`, print `RUBRIC: fail` plus the failing check names and refuse — do NOT mark phase 6 complete.

Assumptions:
- The reviewer never wrote the DevEx review (independence — different agent invocation).
- Verdicts are derived strictly from the rubric; the reviewer does not invent new criteria.
- Evidence cites must be unambiguous: a section heading + line range or quoted phrase.

If any assumption fails, refuse — do not guess.

## Steps

1. Load the DevEx review, the ADR, the rubric, and the Jess persona.
2. Walk each rubric check 1-8 in order. For each, scan the DevEx review for the relevant per-entry verdicts and cross-reference the ADR's Public API. Record `pass` / `fail` / `n/a` plus the supporting cite.
3. Walk the three carrier rules (Simplicity / Surgical / Goal-driven). Record verdicts.
4. Compose the rubric record:

   ```json
   {
     "phase": 6,
     "checks": [ {"id": "naming-conversions", "verdict": "pass", "evidence": "..."}, ... ],
     "carriers": [ {"rule": "simplicity-first", "verdict": "pass"}, ... ],
     "failing": [ ... ids of failing checks ... ],
     "verdict": "pass" | "fail"
   }
   ```

5. Write `.pipeline/devex-review-rubric.json`.
6. If every entry in `checks` and `carriers` is `pass` or `n/a`, run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 6` and print `RUBRIC: pass`.
7. Otherwise, print `RUBRIC: fail` followed by the failing check IDs and their cites, then refuse: name the producer step that must re-run and the concrete edits needed.

## Output

`.pipeline/devex-review-rubric.json` written. On `pass`: phase 6 marked complete; pipeline advances. On `fail`: pipeline stops; the rubric record names the failures.

## Refusals

- Any input file missing → refuse with the missing path.
- A rubric check cannot be decided from the review + ADR alone → refuse: "produce step did not record enough detail to decide check X"; the producer must amend the review.
- Verdict is `fail` → refuse to call `pipeline.py complete 6`; print the failure summary and stop.
