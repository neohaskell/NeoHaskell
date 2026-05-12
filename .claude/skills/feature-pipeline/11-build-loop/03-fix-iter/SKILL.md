---
name: 03-fix-iter
description: Reads build and test logs, identifies the root cause, and edits implementation files to fix it.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Fix Iter

Reads the latest build and test logs and applies a targeted fix to the implementation. Test files are immutable.

## Inputs

- `.pipeline/build.log`
- `.pipeline/test.log`
- `docs/architecture/<adr-number>-<slug>.md` — architecture doc.

## Plan (Karpathy 1 + 4)

1. Read both logs → verify: at least one ends with a failure marker.
2. Identify the root cause (compile error, type mismatch, test failure, runtime exception) → verify: a single hypothesis is named.
3. Edit only implementation files → verify: no file under `core/test/` or `testbed/tests/` is touched.
4. Re-run `cabal build all` (or full loop on caller's next step) → verify: at least one previously-failing signal now passes.

Assumptions:
- Test files are immutable; only implementation files may change.
- The iteration counter caps at 10. After the 10th iteration with failure, refuse and surface for the maintainer.
- NeoHaskell style is strict — fixes must respect pipes, `do`+`let`, `case`...`of`, qualified imports.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Load the build and test logs and the architecture doc.
2. Locate the first failure in the logs and identify the responsible source file.
3. Form a single hypothesis for the root cause.
4. Edit the implementation file(s) to address the hypothesis.
5. Re-run `cabal build all > .pipeline/build.log 2>&1`.
6. If build passes, the caller will re-run tests on the next loop turn.
7. Increment the iteration counter via `python3 .claude/skills/feature-pipeline/scripts/pipeline.py iter 11`.
8. If the counter exceeds 10, refuse and escalate.

## Output

Edited implementation file(s); rebuilt `.pipeline/build.log`; iteration counter advanced.

## Refusals

- Iteration counter > 10 → refuse: "build loop exhausted; escalate to maintainer".
- Any test file modified → refuse: "tests are immutable".
- No identifiable failure in either log → refuse: "no failure found; loop should not be running".
