> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 01-init-pipeline
description: Initialises the pipeline state machine for a NeoHaskell outbound integration.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Init pipeline

Bootstraps `.integration-pipeline/state.json` for the integration so subsequent phases can drive against it.

## Inputs

- `integration_name` — string, the human-readable integration name (e.g. "Stripe webhooks").
- `issue_number` — string, GitHub issue number (may be empty).
- `module_name` — string, Pascal-case module suffix used under `integrations/Integration/<Name>` (may be empty; pipeline.py derives from the first word of the name).
- `module_path` — string, path of the main module under `integrations/Integration/<Name>.hs` (may be empty).
- `test_path` — string, path of the test file under `integrations/test/<Name>Spec.hs` (may be empty).
- `branch_name` — string, git branch (default `integration/<slug>`).

## Plan

1. Validate inputs → verify: `integration_name` non-empty.
2. Run `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py init "$integration_name" [--issue …] [--module-name …] [--module …] [--test …] [--branch …]` → verify: exit code 0.
3. Mark complete via `pipeline.py complete 1` → verify: `pipeline.py status` shows phase 1 done.

Assumptions:
- `.claude/skills/integration-pipeline-preview/scripts/pipeline.py` exists.
- The runtime has a working Python 3.11+ available (project standard).

If any assumption fails, refuse — do not reimplement the state machine inline.

## Steps

1. Confirm `integration_name` is provided.
2. Invoke `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py init "$integration_name" --issue "$issue_number" --module-name "$module_name" --module "$module_path" --test "$test_path" --branch "$branch_name"`. Omit any `--…` flag whose value is empty.
3. If exit code ≠ 0, surface stderr verbatim and stop.
4. Run `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py complete 1` and verify it prints "Phase 1 ... completed.".

## Output

- File `.integration-pipeline/state.json` exists.
- File `.integration-pipeline/.gitignore` exists (so the state directory is never committed).
- `pipeline.py status` lists Phase 1 (Init pipeline) as `done`.

## Refusals

- `integration_name` empty → name the violation and stop.
- `pipeline.py` missing → refuse and ask the user to restore the skill.
- `init` exits non-zero (pipeline already initialised) → surface stderr; the user must `pipeline.py reset` before re-init.
