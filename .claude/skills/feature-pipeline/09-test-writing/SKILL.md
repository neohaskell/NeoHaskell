---
name: 09-test-writing
description: Translates the test spec into compiling hspec tests that all fail against stub implementations.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Test Writing

Translates the test spec into Haskell `hspec` tests and stub implementations such that everything compiles and every test fails.

## Inputs

- `docs/architecture/<adr-number>-<slug>-tests.md` — test spec.
- `docs/architecture/<adr-number>-<slug>.md` — architecture doc.

## Plan (Karpathy 1 + 4)

1. Read the test spec and architecture doc → verify: both exist.
2. Translate every spec case into an `hspec` test → verify: case count matches spec.
3. Create implementation stubs with `Task.throw` or `error` so the module compiles → verify: `cabal build all` succeeds.
4. Register the test suite in `nhcore.cabal` and confirm every test fails → verify: `cabal test` runs and every new test is red.

Assumptions:
- Tests follow NeoHaskell style: pipes, `do`+`let`, `case`...`of`, qualified imports, `[fmt|...|]`.
- Stubs use `Task.throw` for `Task` returns and `error "not implemented"` for pure ones.
- nhcore has `Strict` globally — never add `!` annotations.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Load the test spec and architecture doc.
2. Create the source module under the path specified by the architecture doc, with stub function bodies.
3. Create the test module under `core/test/...` mirroring the source path.
4. For each spec case, write an `hspec` `it` block referencing the spec case name.
5. Register the new test module in `nhcore.cabal` (and the source module if new).
6. Run `cabal build all` to confirm compilation.
7. Run `cabal test --test-show-details=streaming` to confirm every new test fails.
8. Run `python3 .claude/skills/feature-pipeline/scripts/pipeline.py complete 9`.

## Output

Test module and stub source module written, cabal updated, every new test compiling-but-failing, phase 9 marked complete.

## Refusals

- Test spec or architecture doc missing → refuse: "prerequisite phase output missing".
- Build fails after stubbing → refuse and surface the build error.
- Any new test passes against a stub → refuse: "test <name> passes against stub; spec is wrong".
