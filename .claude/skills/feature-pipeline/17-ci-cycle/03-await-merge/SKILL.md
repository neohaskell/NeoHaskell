---
name: 03-await-merge
description: Prints maintainer merge instructions and closes the pipeline.
kind: leaf
executor: script
---

# Await Merge

Prints merge instructions to the maintainer, marks phase 17 complete, and approves to close the pipeline.

## Inputs

- `pipeline.py get pr_url` → PR URL.
- `pipeline.py get pr_number` → PR number.

## Plan (Karpathy 1 + 4)

1. Resolve `pr_url` and `pr_number` → verify: both set.
2. Print the maintainer instructions → verify: output emitted.
3. Mark phase 17 complete → verify: pipeline state updated.
4. Approve phase 17 → verify: pipeline closed.

Assumptions:
- This is the terminal step. After approval, no further phases run.
- The maintainer performs the merge manually.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. `URL=$(python3 .claude/skills/feature-pipeline/scripts/pipeline.py get pr_url)`.
2. `NUM=$(python3 .claude/skills/feature-pipeline/scripts/pipeline.py get pr_number)`.
3. If either is empty, refuse.
4. Print: `Review at $URL (PR #$NUM), approve, merge.`
5. Run `python3 .claude/skills/feature-pipeline/scripts/pipeline.py complete 17`.
6. Run `python3 .claude/skills/feature-pipeline/scripts/pipeline.py approve 17`.

## Output

Merge instructions printed; phase 17 complete and approved; pipeline closed.

## Refusals

- `pr_url` or `pr_number` missing → refuse: "PR metadata missing; phase 16 incomplete".
- `pipeline.py` returns non-zero → refuse and surface stderr.
