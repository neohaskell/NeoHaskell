---
name: 02-apply
description: Applies the maintainer-approved findings-17 fixes, builds, tests, commits per finding, and pushes the branch.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Apply approved fixes

Invoked AFTER `pipeline.py approve 17`. Reads `.pipeline/findings-17.json` (which the maintainer may have edited between phase-17 PAUSE and approve), applies each `accept` finding, builds and tests after each, and pushes the branch.

## Inputs

- `.pipeline/findings-17.json` — possibly maintainer-edited

## Plan

1. Read findings-17.json. Filter to entries where `maintainer_decision == "accept"`. Verify: list captured.
2. For each accepted finding, in file-path order:
   1. Apply `proposed_fix` per the `kind` (edit / delete / rename / add). **Record the exact set of paths touched** for this fix (call it `<touched-paths>`); you need this list for step 5.
   2. Run `nix develop --command cabal build all` — must exit 0.
   3. Run `nix develop --command cabal test all` — must exit 0 with 0 failures.
   4. `git add <touched-paths>`; `git commit -m "<type>(<scope>): <rule>"` with a short body referencing the finding's `explanation`.
   5. If build or test fails: REVERT only the files this fix touched via `git restore -- <touched-paths>` (do NOT use `git checkout -- .` — that would also discard any unrelated uncommitted work). Then refuse with the finding's `rule` and surface the failure.
3. After all accepted findings are applied, `git push origin <branch>` to update the PR.
4. Call `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 17` (it may already be complete from step 1 — re-running is a no-op).

## Constraints

- Never apply findings with `maintainer_decision != "accept"` — `reject` and `defer` are skipped silently; surface deferred ones at the end so the maintainer can convert to follow-up issues.
- Never commit changes unrelated to the findings (no boy-scout sweep here).
- Never push without explicit build+test success first.
- One commit per finding — easier for a future reviewer to bisect.

## Output

The branch ends with N additional commits (one per accepted finding) pushed to the remote, updating the open PR.

## Refusals

- `.pipeline/findings-17.json` missing → "phase 17 review never ran"
- No entries with `maintainer_decision == "accept"` → no-op; report deferred/rejected counts
- Any single fix breaks build or test → revert that fix, surface the rule, do NOT proceed with later fixes
