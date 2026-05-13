---
name: 03-record
description: Persists grounded performance findings to the pipeline state and marks phase 5 complete.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Record Performance Findings

Reads grounded findings on stdin, writes `.pipeline/findings-05.json` with aggregate counts, registers it via the pipeline state machine, and marks phase 5 complete. All work happens inside `scripts/record-findings.py` — this leaf just routes input into the script.

## Inputs

- stdin (or `--input <path>`) — JSON array of grounded findings from step 2. Each finding should carry at least `severity_after_grounding` and `grounding_outcome`.

## Plan (Karpathy 1 + 4)

1. Pipe the grounded-findings JSON into `record-findings.py` with `--phase 5 --severity-scheme performance` → verify: exit 0.
2. The script writes the file, calls `pipeline.py findings 5 ...` and `pipeline.py complete 5` internally → verify: stdout names the output path and the finding/blocker counts.

Assumptions:
- The script exists at `.claude/skills/feature-pipeline-preview/scripts/record-findings.py`.
- The pipeline is initialised and phase 5 is ready (dependencies satisfied).

If any assumption fails, refuse — do not inline a fallback recorder.

## Steps (Karpathy 2 + 3)

1. Invoke `python3 .claude/skills/feature-pipeline-preview/scripts/record-findings.py --phase 5 --severity-scheme performance`, piping in the grounded findings JSON on stdin (or pass `--input <path>` if the prior step wrote to a file).
2. Surface stdout (the success line) to the orchestrator.
3. On non-zero exit, surface stderr verbatim and stop — do not retry, do not reinterpret.

## Output

`.pipeline/findings-05.json` written with `total_findings`, `blockers`, `kept`, `demoted`, `framework_debt`, `findings`. Phase 5 marked complete; pipeline state advanced.

## Refusals

- Script not found → refuse with the missing path.
- Script exits 1 (input error) → surface stderr; the producer step must re-emit valid JSON.
- Script exits 2 (pipeline state error) → surface stderr; the orchestrator must resolve the prerequisite phase before re-running.
