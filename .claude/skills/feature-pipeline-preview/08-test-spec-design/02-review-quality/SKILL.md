---
name: 02-review-quality
description: Independently validates the test specification against the rubric and gates phase 8 completion.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Review test-spec quality

Reads the produced test spec and the architecture doc, applies every check in `../../references/test-spec-rubric.md`, records the per-check verdict to `.pipeline/test-spec-rubric.json`, and gates phase 8 completion on the result.

This step is the auto-gate that replaces the human PAUSE: when the rubric says `pass`, the pipeline advances automatically; when it says `fail`, the pipeline halts here with the rubric record as the audit trail.

## Review posture

Assume the artefact under review was produced by a language model (ChatGPT-class output). Treat plausible-looking claims as unverified, expect hallucinated APIs and missed constraints, and refuse to pass anything not directly traceable to the rubric in `../../references/test-spec-rubric.md` and the architecture doc. Strict review is the default — benefit of the doubt goes to the rubric and to the source, never to the producer.

## Inputs

- `docs/architecture/<adr-number>-<slug>-tests.md` — produced by `../01-produce/`.
- `docs/architecture/<adr-number>-<slug>.md` — the source architecture doc.
- `../../references/test-spec-rubric.md` — the eight-check rubric + four carrier rules.

## Plan

1. Load all three inputs → verify: every file exists.
2. For each of the eight rubric checks, decide `pass` / `fail` / `n/a` from evidence in the spec vs the architecture doc → verify: every check has a verdict + an evidence cite.
3. For each of the four carrier rules, decide `pass` / `fail` → verify: every rule has a verdict.
4. Write the rubric record to `.pipeline/test-spec-rubric.json` → verify: file exists with `checks`, `carriers`, `verdict`.
5. If `verdict == "pass"`, run `pipeline.py complete 8` and print `RUBRIC: pass`. If `verdict == "fail"`, print `RUBRIC: fail` plus the failing check names and refuse — do NOT mark phase 8 complete.

Assumptions:
- The reviewer did not produce the spec (independence).
- Verdicts are derived strictly from the rubric; the reviewer does not invent new criteria.
- Coverage counts are computed from the spec's `## Coverage summary` table; the reviewer recomputes them by walking the per-function section if the table is missing.

If any assumption fails, refuse — do not guess.

## Steps

1. Load the test spec, the architecture doc, and the rubric.
2. Walk each rubric check 1-8 in order:
   - Check 1 (coverage per function): every public function from the architecture doc has ≥3 cases. Compute coverage by walking the spec's per-function sections; refuse if any function is missing or below the floor.
   - Check 2 (edge-to-happy ratio): aggregate non-happy ≥ aggregate happy. Recompute by counting case categories.
   - Check 3 (round-trip tests): every type the architecture doc marks `ToJSON`/`FromJSON` has a round-trip case in `## Round-trip suite`.
   - Check 4 (error / variant exhaustion): every error ADT constructor from the architecture doc has at least one case explicitly naming it.
   - Check 5 (property invariants): every operation the architecture doc identifies as commutative / associative / idempotent / round-tripping has a property case, OR an explicit `properties: none — <reason>` entry.
   - Check 6 (boundary cases): every numeric / sized input has zero, one, max, max+1 cases; every text input has empty + unicode-multibyte cases.
   - Check 7 (each error condition reproduced): every error case named in the architecture doc has a case whose description names the trigger.
   - Check 8 (layout matches existing specs): `describe`/`describe`/`it` nesting present; cases organised by function.
3. Walk the four carrier rules (Think / Simplicity / Surgical / Goal-driven). Record verdicts.
4. Compose the rubric record:

   ```json
   { "phase": 8, "checks": [...], "carriers": [...], "failing": [...], "verdict": "pass" | "fail" }
   ```

5. Write `.pipeline/test-spec-rubric.json`.
6. On `pass`: run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 8`; print `RUBRIC: pass`.
7. On `fail`: print `RUBRIC: fail` with the failing check IDs, their cites, and the concrete edits needed; refuse to mark phase 8 complete.

## Output

`.pipeline/test-spec-rubric.json` written. On `pass`: phase 8 marked complete. On `fail`: pipeline stops; rubric record names the failures.

## Refusals

- Any input file missing → refuse with the missing path.
- A coverage check cannot be decided because the spec uses ambiguous case descriptions → refuse: "spec is too vague to audit"; the producer must rewrite ambiguous cases.
- Verdict is `fail` → refuse to call `pipeline.py complete 8`; print the failure summary and stop.
