---
name: 01-pr-body
description: Writes the PR body to a file with a conventional-commit title, a checklist, and any hlint warnings captured from earlier phases.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# PR Body

Writes `.integration-pipeline/pr-body.md` with the PR title, description, checklist, references, and — if non-empty — the hlint warnings captured by phases 11 and 15.

## Inputs

- `.integration-pipeline/integration-design.md` — the design draft from phase 3.
- `.integration-pipeline/findings-04.json`, `.integration-pipeline/findings-05.json`, `.integration-pipeline/findings-12.json`, `.integration-pipeline/findings-13.json`
- `.integration-pipeline/classification.json` — contains the classification `tier` and `rationale` for the Summary section.
- Pipeline state — `issue_number`, `slug`, `module_name`, and `integration_name` come from `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py get <key>` (initialised in phase 01), not from `classification.json`.
- `.integration-pipeline/hlint.log` — the canonical hlint output from the most recent run (refreshed by phase 15.3); may be empty.

## Plan

1. Read the design draft, all four findings files, classification, and the hlint log → verify: all exist (the hlint log may be zero bytes).
2. Draft a conventional-commit title → verify: matches `<type>(<scope>): <subject>`.
3. Draft a body addressed to Jess covering the design draft, changes, security, performance, tests, hlint → verify: every section present; the hlint section reports either "clean" or surfaces the captured warnings.
4. Write `.integration-pipeline/pr-body.md` and a separate `.integration-pipeline/pr-title.txt` → verify: both exist.

Assumptions:
- The PR body is written for Jess (newcomer voice, community-writer tone).
- The issue number is sourced from pipeline state (`pipeline.py get issue_number`), set when the orchestrator ran `pipeline.py init --issue ...` in phase 01. It is **not** stored in `classification.json`.
- `.integration-pipeline/hlint.log` is the canonical record. Build-loop and final-verify both write it; the final-verify pass overwrites with the post-fix state. An empty file means hlint was clean; a non-empty file is surfaced verbatim into the PR body so the human reviewer (and CI's own hlint check) see exactly what hlint reported.

If any assumption fails, refuse — do not guess.

## Steps

1. Load the design draft, findings, classification, and hlint log.
2. Draft a conventional-commit title under 72 characters.
3. Draft body sections in this order: Summary, Design draft reference, Changes, Security (link to findings-04 + findings-12), Performance (findings-05 + findings-13), Tests, Hlint, Checklist. Append a final `Closes #<issue_number>` footer iff `pipeline.py get issue_number` is non-empty; otherwise omit the footer entirely.
4. Render the **Hlint** section as follows:
   - If `.integration-pipeline/hlint.log` is empty: a single line `Hlint clean.`.
   - Otherwise: a heading `### Hlint warnings`, a one-line preface noting the warnings did not block the pipeline and that CI's hlint check is the canonical gate, then the log contents inside a fenced ```` ``` ```` block. If the log exceeds 80 lines, embed the first 80 lines verbatim and append a single `...truncated; full log at .integration-pipeline/hlint.log` line; the limit is a hard 80-line count (use `head -n 80`), not an approximation.
5. Compute `N`, the warning count, as `grep -c "^[^ ]" .integration-pipeline/hlint.log` (each hlint warning starts on its own non-indented line; lines indented by spaces are continuation context for the preceding warning). If `grep` returns 0 for a non-empty file (e.g. the log is all continuation text), fall back to `wc -l < .integration-pipeline/hlint.log`.
6. Render the **Checklist** as follows. Use `- [x]` for items the pipeline can attest to, `- [ ]` for items pending CI/maintainer:
   - `[x] Design draft approved` (phase 3 was approved)
   - `[x] Security findings grounded` (phase 12 marked complete)
   - `[x] Performance findings grounded` (phase 13 marked complete)
   - `[x] Tests passing` (phase 15 build + test gates passed)
   - `Hlint:` followed by `clean ✓` when the log is empty, otherwise `<N> warning(s) — see the Hlint warnings section above` using the `N` from step 5 (no checkbox, since this is informational not gating).
7. Write `.integration-pipeline/pr-title.txt` with the title only.
8. Write `.integration-pipeline/pr-body.md` with the body.

## Output

`.integration-pipeline/pr-title.txt` and `.integration-pipeline/pr-body.md` written. The body contains either `Hlint clean.` or a verbatim warning section so the PR reviewer sees the hlint output without having to run it themselves.

## Refusals

- Any prerequisite file missing (other than the hlint log, which is allowed to be empty) → refuse: "prerequisite output missing".
- `.integration-pipeline/hlint.log` does not exist at all → refuse: "hlint was never run; phase 15 step 3 must complete first".
- Title would exceed 72 characters → refuse: "title too long; tighten the subject".
