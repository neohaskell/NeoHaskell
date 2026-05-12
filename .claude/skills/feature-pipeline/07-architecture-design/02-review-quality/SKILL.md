---
name: 02-review-quality
description: Independently validates the architecture document against the rubric and gates phase 7 completion.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Review architecture-doc quality

Reads the produced architecture doc and the ADR, applies every check in `../../references/architecture-rubric.md`, records the per-check verdict to `.pipeline/architecture-rubric.json`, and gates phase 7 completion on the result.

This step is the auto-gate that replaces the human PAUSE: when the rubric says `pass`, the pipeline advances automatically; when it says `fail`, the pipeline halts here with the rubric record as the audit trail.

## Inputs

- `docs/architecture/<adr-number>-<slug>.md` — produced by `../01-produce/`.
- `docs/decisions/<adr-number>-<slug>.md` — the source ADR.
- `../../references/architecture-rubric.md` — the eight-check rubric + four Karpathy carrier rules.
- `../../references/nhcore-context.md` — to verify that every reused upstream symbol exists.

## Plan (Karpathy 1 + 4)

1. Load all four inputs → verify: every file exists.
2. For each of the eight rubric checks, decide `pass` / `fail` / `n/a` from evidence in the architecture doc vs the ADR → verify: every check has a verdict + an evidence cite (section + line).
3. For each of the four Karpathy carrier rules, decide `pass` / `fail` → verify: every rule has a verdict.
4. Write the rubric record to `.pipeline/architecture-rubric.json` → verify: file exists with `checks`, `carriers`, `verdict`.
5. If `verdict == "pass"`, run `pipeline.py complete 7` and print `RUBRIC: pass`. If `verdict == "fail"`, print `RUBRIC: fail` plus the failing check names and refuse — do NOT mark phase 7 complete.

Assumptions:
- The reviewer did not produce the architecture doc (independence).
- Verdicts are derived strictly from the rubric; the reviewer does not invent new criteria.
- Every reused upstream symbol named in the doc must exist in the repository — the reviewer spot-checks at least three by grep before declaring the relevant check `pass`.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Load the architecture doc, the ADR, the rubric, and the nhcore-context reference.
2. Walk each rubric check 1-8 in order:
   - Check 1 (module paths): grep the doc for any `TBD` / `?` / `TODO` token; refuse on hit.
   - Check 2 (type signatures): every public function in the doc has a non-placeholder signature.
   - Check 3 (every type used): every new type appears in at least one construction and one consumption site.
   - Check 4 (integration points): every "uses X" claim names a `Module.symbol` that exists — spot-check three by grep.
   - Check 5 (imports listed): every import line names the qualified module + symbol set.
   - Check 6 (errors enumerated): every error case from the ADR appears as a constructor in the doc.
   - Check 7 (concurrency & persistence): Hasql / EventStore / RequestContext decisions stated or explicit "no persistence".
   - Check 8 (visibility): every type/function tagged `exported` or `internal`.
3. Walk the four carrier rules (Think / Simplicity / Surgical / Goal-driven). Record verdicts.
4. Compose the rubric record:
   ```json
   { "phase": 7, "checks": [...], "carriers": [...], "failing": [...], "verdict": "pass" | "fail" }
   ```
5. Write `.pipeline/architecture-rubric.json`.
6. On `pass`: run `python3 .claude/skills/feature-pipeline/scripts/pipeline.py complete 7`; print `RUBRIC: pass`.
7. On `fail`: print `RUBRIC: fail` with the failing check IDs, their cites, and the concrete edits needed; refuse to mark phase 7 complete.

## Output

`.pipeline/architecture-rubric.json` written. On `pass`: phase 7 marked complete. On `fail`: pipeline stops; rubric record names the failures.

## Refusals

- Any input file missing → refuse with the missing path.
- Any "uses X" claim names a symbol that does not exist after grep → refuse: "architecture doc references missing symbol X"; the producer must amend.
- Verdict is `fail` → refuse to call `pipeline.py complete 7`; print the failure summary and stop.
