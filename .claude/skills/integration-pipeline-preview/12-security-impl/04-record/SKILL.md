---
name: 04-record
description: Persists grounded implementation security findings and marks phase 12 complete.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Record Implementation Security Findings

Reads grounded findings on stdin, writes `.integration-pipeline/findings-12.json` with aggregate counts, registers it via the pipeline state machine, and marks phase 12 complete. All work happens inside `scripts/record-findings.py` — this leaf just routes input into the script.

## Inputs

- stdin (or `--input <path>`) — JSON array of grounded findings from step 3. Each finding should carry at least `severity_after_grounding` and `grounding_outcome`.

## Plan

1. Pipe the grounded-findings JSON into `record-findings.py` with `--phase 12 --severity-scheme security` → verify: exit 0.
2. The script stamps every finding with a `blocker` flag (true for severities `Critical` / `High`), computes aggregates, writes the file, then calls `pipeline.py findings 12 ...` and `pipeline.py complete 12` → verify: stdout names the output path and counts.

Assumptions:
- The script exists at `.claude/skills/integration-pipeline-preview/scripts/record-findings.py`.
- The pipeline is initialised and phase 12 is ready (depends on phase 11).

If any assumption fails, refuse — do not inline a fallback recorder.

## Steps

1. Invoke `python3 .claude/skills/integration-pipeline-preview/scripts/record-findings.py --phase 12 --severity-scheme security`, piping in the grounded findings JSON on stdin (or pass `--input <path>` if the prior step wrote to a file).
2. Surface stdout to the orchestrator.
3. On non-zero exit, surface stderr verbatim and stop — do not retry, do not reinterpret.

## Output

`.integration-pipeline/findings-12.json` written with per-finding `blocker` flags and the aggregate envelope (`total_findings`, `blockers`, `kept`, `demoted`, `framework_debt`). Phase 12 marked complete.

## Refusals

- Script not found → refuse with the missing path.
- Script exits 1 (input error) → surface stderr; the producer step must re-emit valid JSON.
- Script exits 2 (pipeline state error) → surface stderr; the orchestrator must resolve the prerequisite phase before re-running.
