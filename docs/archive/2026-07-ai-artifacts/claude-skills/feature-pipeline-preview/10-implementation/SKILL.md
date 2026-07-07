> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 10-implementation
description: Implements the feature per the architecture doc using nhcore-first, NeoHaskell style.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Implementation

Replaces the phase 9 stubs with a real implementation that follows the architecture doc exactly.

## Inputs

- `docs/architecture/<adr-number>-<slug>.md` — architecture doc.
- `docs/architecture/<adr-number>-<slug>-tests.md` — test spec (read-only reference).
- The stub module created in phase 9.

## Plan

1. Read the architecture doc and identify the stub module → verify: stubs exist.
2. Plan implementation per architecture decisions → verify: every public function in the doc has a planned body.
3. Implement using nhcore primitives first; fall back to Hackage only with a `Ghc` prefix as last resort → verify: imports list is auditable.
4. Confirm compilation → verify: `nix develop --command cabal build all` succeeds.

Assumptions:
- NeoHaskell style is strict: pipes (`|>`) over nesting, `do`+`let` over `let..in`/`where`, `case`...`of` over guards in arg lists, qualified imports, `[fmt|...|]` for strings, `Task`/`Result` not `IO`/`Either`, no `$`, no point-free.
- nhcore has `Strict` enabled globally — never add `!` annotations.
- Test files are immutable in this phase — never edit them.
- **Boy scout rule applies.** Any file this phase opens for writing is fair game for fixing pre-existing style debt (unqualified imports, point-free top-levels, wildcard params, raw `String`/`IO`/`Either`, `$`, `let..in`/`where`, `<>`/`++`). The rule does not apply to test bodies/assertions (still immutable) but it does apply to helpers, imports, and fixtures the phase legitimately touches. See `../references/nhcore-context.md#boy-scout-rule`.

If any assumption fails, refuse — do not guess.

## Steps

1. Load the architecture doc.
2. For each public function, replace the stub body with the real implementation.
3. Reach into nhcore (`Text`, `Array`, `Result`, `Task`, `EventStore`, etc.) before any external package.
4. If an external package is unavoidable, alias it with a `Ghc` prefix (e.g. `import qualified Data.Map.Strict as GhcMap`).
5. Run `nix develop --command cabal build all` to confirm compilation.
6. Do not run tests here — that is phase 11.
7. Verify no test files changed: `git diff --name-only HEAD -- 'core/test/' 'testbed/tests/'` (and any other test directories the architecture doc names) must return empty. Refuse if any match — `tests are immutable in phase 10`.
8. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 10`.

## Output

Stubs replaced with real implementations; `nix develop --command cabal build all` green; phase 10 marked complete.

## Refusals

- Architecture doc missing → refuse: "no architecture doc; run phase 07 first".
- Build fails → refuse and surface the error.
- Any test file changed → refuse: "tests are immutable in this phase".
- Any import of `Control.Concurrent.Async`, `Data.Either`, `Data.IORef`, `Data.Map`, `Data.Map.Strict`, `Data.Vector` (or any other `Control.*`/`Data.*` module) WITHOUT a `Ghc` prefix alias → refuse: "import must be aliased with `Ghc` prefix per nhcore convention (e.g. `import qualified Data.Map.Strict as GhcMap`)".
- The `Task.fromIO (… Task.runResult …)` round-trip pattern (or its inverse) detected anywhere → refuse: "stop escaping Task to use an IO library — add the missing primitive to nhcore (e.g. `AsyncTask.race`, `Task.timeout`) and use it instead. Document the new primitive in `core/concurrency/AsyncTask.hs` or `core/core/Task.hs` with a unit test before consuming it here."

Static checks for both patterns above live at `../../scripts/lint-imports.py`. The phase 10 leaf MUST invoke it against every source module it edits. If the script exits non-zero, the leaf MUST refuse and NOT call `pipeline.py complete 10` — surface the findings and stop.
