> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 03-await-merge
description: Prints maintainer merge instructions and closes the pipeline.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Await Merge

Prints merge instructions to the maintainer and marks phase 18 complete. Approval is reserved for the maintainer — this leaf does **not** call `pipeline.py approve 18`. The pipeline parks at PAUSE state until the maintainer confirms the merge and runs `pipeline.py approve 18` manually.

## Inputs

- `pipeline.py get pr_url` → PR URL.
- `pipeline.py get pr_number` → PR number.

## Plan

1. Resolve `pr_url` and `pr_number` → verify: both set.
2. Print the maintainer instructions → verify: output emitted.
3. Mark phase 18 complete → verify: pipeline state updated.
4. Stop and wait for the maintainer to approve phase 18 manually → verify: phase 18 status is `awaiting_approval`, not `approved`.

Assumptions:
- The maintainer performs the merge manually and then runs `pipeline.py approve 18` themselves. This leaf must not auto-approve — that would collapse the final human gate.
- After the maintainer approval, no further phases run.

If any assumption fails, refuse — do not guess.

## Steps

1. `URL=$(python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py get pr_url)`.
2. `NUM=$(python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py get pr_number)`.
3. If either is empty, refuse.
4. Print: `Review at $URL (PR #$NUM), approve, merge. Then run pipeline.py approve 18 to close the pipeline.`
5. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 18`.
6. Stop here. Do **not** run `pipeline.py approve 18` — that command is reserved for the maintainer to invoke manually after the merge has actually landed.

## Output

Merge instructions printed; phase 18 marked complete and parked at `awaiting_approval`. The pipeline closes only after the maintainer runs `pipeline.py approve 18` themselves.

## Refusals

- `pr_url` or `pr_number` missing → refuse: "PR metadata missing; phase 16 incomplete".
- `pipeline.py` returns non-zero → refuse and surface stderr.
