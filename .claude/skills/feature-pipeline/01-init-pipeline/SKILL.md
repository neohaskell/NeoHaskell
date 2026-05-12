---
name: 01-init-pipeline
description: Initialises the pipeline state machine for a NeoHaskell feature.
kind: leaf
executor: script
---

# Init pipeline

Bootstraps `.pipeline/state.json` for the feature so subsequent phases can drive against it.

## Inputs

- `feature_name` — string, the human-readable feature name.
- `issue_number` — string, GitHub issue number (may be empty).
- `module_path` — string, path of the main module (may be empty).
- `test_path` — string, path of the test file (may be empty).
- `branch_name` — string, git branch (default `feature/<slug>`).
- `adr_number` — string, 4-digit ADR number (may be empty).

## Plan (Karpathy 1 + 4)

1. Validate inputs → verify: `feature_name` non-empty.
2. Run `python3 .claude/skills/feature-pipeline/scripts/pipeline.py init "$feature_name" [--issue …] [--module …] [--test …] [--branch …] [--adr …]` → verify: exit code 0.
3. Mark complete via `pipeline.py complete 1` → verify: `pipeline.py status` shows phase 1 done.

Assumptions:
- `.claude/skills/feature-pipeline/scripts/pipeline.py` exists.
- The runtime has a working Python 3.11+ available (project standard).

If any assumption fails, refuse — do not reimplement the state machine inline.

## Steps

1. Confirm `feature_name` is provided.
2. Invoke `python3 .claude/skills/feature-pipeline/scripts/pipeline.py init "$feature_name" --issue "$issue_number" --module "$module_path" --test "$test_path" --branch "$branch_name" --adr "$adr_number"`. Omit any `--…` flag whose value is empty.
3. If exit code ≠ 0, surface stderr verbatim and stop.
4. Run `python3 .claude/skills/feature-pipeline/scripts/pipeline.py complete 1` and verify it prints "Phase 1 ... completed.".

## Output

- File `.pipeline/state.json` exists.
- File `.pipeline/.gitignore` exists (so the state directory is never committed).
- `pipeline.py status` lists Phase 1 (Init pipeline) as `done`.

## Refusals

- `feature_name` empty → name the violation and stop.
- `pipeline.py` missing → refuse and ask the user to restore the skill.
- `init` exits non-zero (pipeline already initialised) → surface stderr; the user must `pipeline.py reset` before re-init.
