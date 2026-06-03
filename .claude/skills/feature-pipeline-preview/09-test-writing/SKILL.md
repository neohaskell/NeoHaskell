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

## Plan

1. Read the test spec and architecture doc → verify: both exist.
2. Translate every spec case into an `hspec` test → verify: case count matches spec.
3. Create implementation stubs with `Task.throw` or `error` so the module compiles → verify: `nix develop --command cabal build all` succeeds.
4. Register the test suite in `nhcore.cabal` and confirm every test fails → verify: `nix develop --command cabal test` runs and every new test is red.

Assumptions:
- Tests follow NeoHaskell style: pipes, `do`+`let`, `case`...`of`, qualified imports, `[fmt|...|]`.
- Stubs use `Task.throw` for `Task` returns and `error "not implemented"` for pure ones. Stubs must throw a sentinel value (e.g. `error "not implemented"` or a dedicated `NotImplemented` constructor) that **cannot** match the concrete domain error a real test expects — never use the same domain error constructor the spec asserts against.
- Error-path tests assert the exact domain error constructor (and any payload fields the architecture doc names), not a generic `shouldThrow`/anyException matcher. A test that only catches "any exception" is a false-green hazard against the stub above and must be rewritten before the suite is admitted.
- nhcore has `Strict` globally — never add `!` annotations.

If any assumption fails, refuse — do not guess.

## Steps

1. Load the test spec and architecture doc.
2. Create the source module under the path specified by the architecture doc, with stub function bodies.
3. Create the test module under `core/test/...` mirroring the source path.
4. For each spec case, write an `hspec` `it` block referencing the spec case name.
5. Register the new test module in `nhcore.cabal` (and the source module if new).
6. Run `nix develop --command cabal build all` to confirm compilation.
7. Run `nix develop --command cabal test --test-show-details=streaming` to confirm every new test fails.
8. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 9`.

## Output

Test module and stub source module written, cabal updated, every new test compiling-but-failing, phase 9 marked complete.

## Refusals

- Test spec or architecture doc missing → refuse: "prerequisite phase output missing".
- Build fails after stubbing → refuse and surface the build error.
- Any new test passes against a stub → refuse: "test <name> passes against stub; spec is wrong".
- Any error-path test uses a generic `shouldThrow`/anyException-style matcher instead of asserting the exact domain error constructor and payload → refuse: "test <name> would false-green against `Task.throw` / `error` stubs; assert the concrete error".
- **Setup-error swallowing.** Any `case … of Err (ConnectionFailed _) -> pass` (or any `Err _ -> pass` shape on a fixture/setup call like `mkStore`, `createTestStore`, `InMemory.new`) → refuse: "tests must not absorb infrastructure failures; either make the dependency a hard prerequisite or mark the test `pending` with a rationale that names the missing fixture".
- **Mismatched name vs body.** If the test name contains `emits` / `reads in chunks` / `logs` / `deletes` / `writes` / `updates` / `replays` / `resumes`, the body's assertion must reference the same primitive. Otherwise → refuse: "test name promises a side effect the body never observes; either assert the effect or mark `pending` with a rationale".
- **Trivial-fixture error-path test.** For any `it "fails with <X>" \_ -> do …` whose setup is the canonical `Subscriber.new <InMemory>.new Registry.empty` or equivalent empty-fixture pattern AND whose body asserts `Err (<X> _) -> pass` → refuse: "error-path test needs a fixture that actually triggers <X>; empty registry + InMemory cannot reach the <X> branch".
- **Panicky-let non-behavioral test.** If a test body is just `let _x = builder ...` followed by `pass` → refuse: "test asserts no behavior; either add a meaningful assertion or remove the test".

Static checks for the four patterns above are encoded in `../../scripts/lint-test-patterns.py`. The phase 9 leaf SHOULD invoke it against every new spec file as a final guard before calling `pipeline.py complete 9`.
