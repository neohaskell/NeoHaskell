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

- `.integration-pipeline/integration-tests.md` — test spec.
- `.integration-pipeline/integration-architecture.md` — architecture doc.

## Plan

1. Read the test spec and architecture doc → verify: both exist.
2. Translate every spec case into an `hspec` test → verify: case count matches spec.
3. Create implementation stubs with `Task.throw` or `error` so the module compiles → verify: `nix develop --command cabal build all` succeeds.
4. Register the test suite in `integrations/nhintegrations.cabal` and confirm every test fails → verify: `nix develop --command cabal test` runs and every new test is red.

Assumptions:
- Tests follow NeoHaskell style: pipes, `do`+`let`, `case`...`of`, qualified imports, `[fmt|...|]`.
- Stubs use `Task.throw` for `Task` returns and `error "not implemented"` for pure ones. Stubs must throw a sentinel value (e.g. `error "not implemented"` or a dedicated `NotImplemented` constructor) that **cannot** match the concrete domain error a real test expects — never use the same domain error constructor the spec asserts against.
- Error-path tests assert the exact domain error constructor (and any payload fields the architecture doc names), not a generic `shouldThrow`/anyException matcher. A test that only catches "any exception" is a false-green hazard against the stub above and must be rewritten before the suite is admitted.
- `nhintegrations` enables `Strict` globally (see its cabal common stanza) — never add `!` annotations.
- The source module is placed under `integrations/Integration/<module_name>/` (or `integrations/Integration/<module_name>.hs` for the top-level entry); the test module mirrors it under `integrations/test/<module_name>Spec.hs`.

If any assumption fails, refuse — do not guess.

## Steps

1. Load the test spec and architecture doc.
2. Create the source module under the path specified by the architecture doc, with stub function bodies.
3. Create the test module under `integrations/test/...` mirroring the source path.
4. For each spec case, write an `hspec` `it` block referencing the spec case name.
5. Register the new test module in `integrations/nhintegrations.cabal` (and the source module if new).
6. Run `nix develop --command cabal build all` to confirm compilation.
7. Run `nix develop --command cabal test --test-show-details=streaming` to confirm every new test fails.
8. Run `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py complete 9`.

## Output

Test module and stub source module written, cabal updated, every new test compiling-but-failing, phase 9 marked complete.

## Refusals

- Test spec or architecture doc missing → refuse: "prerequisite phase output missing".
- Build fails after stubbing → refuse and surface the build error.
- Any new test passes against a stub → refuse: "test <name> passes against stub; spec is wrong".
- Any error-path test uses a generic `shouldThrow`/anyException-style matcher instead of asserting the exact domain error constructor and payload → refuse: "test <name> would false-green against `Task.throw` / `error` stubs; assert the concrete error".
