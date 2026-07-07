> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 04-record
description: Persists grounded implementation performance findings and marks phase 13 complete.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Record Implementation Performance Findings

Reads grounded findings on stdin, writes `.integration-pipeline/findings-13.json` with aggregate counts, registers it via the pipeline state machine, and marks phase 13 complete. All work happens inside `scripts/record-findings.py` — this leaf just routes input into the script.

## Inputs

- stdin (or `--input <path>`) — JSON array of grounded findings from step 3. Each finding should carry at least `severity_after_grounding` and `grounding_outcome`.

## Plan

1. Pipe the grounded-findings JSON into `record-findings.py` with `--phase 13 --severity-scheme performance` → verify: exit 0.
2. The script stamps every finding with a `blocker` flag (true for severity `Blocking`), computes aggregates, writes the file, then calls `pipeline.py findings 13 ...` and `pipeline.py complete 13` → verify: stdout names the output path and counts.

Assumptions:
- The script exists at `.claude/skills/integration-pipeline-preview/scripts/record-findings.py`.
- The pipeline is initialised and phase 13 is ready (depends on phase 11).

If any assumption fails, refuse — do not inline a fallback recorder.

## Steps

1. Invoke `python3 .claude/skills/integration-pipeline-preview/scripts/record-findings.py --phase 13 --severity-scheme performance`, piping in the grounded findings JSON on stdin (or pass `--input <path>` if the prior step wrote to a file).
2. Surface stdout to the orchestrator.
3. On non-zero exit, surface stderr verbatim and stop — do not retry, do not reinterpret.

## Output

`.integration-pipeline/findings-13.json` written with per-finding `blocker` flags and the aggregate envelope (`total_findings`, `blockers`, `kept`, `demoted`, `framework_debt`). Phase 13 marked complete.

## Refusals

- Script not found → refuse with the missing path.
- Script exits 1 (input error) → surface stderr; the producer step must re-emit valid JSON.
- Script exits 2 (pipeline state error) → surface stderr; the orchestrator must resolve the prerequisite phase before re-running.
