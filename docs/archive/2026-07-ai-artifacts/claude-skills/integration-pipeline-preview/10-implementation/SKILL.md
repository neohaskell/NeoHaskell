> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 10-implementation
description: Implements the integration under the `integrations/Integration/[Name]/` module tree per the architecture doc using nhcore-first, NeoHaskell style.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Implementation

Replaces the phase 9 stubs with a real implementation that follows the architecture doc exactly.

## Inputs

- `.integration-pipeline/integration-architecture.md` — architecture doc.
- `.integration-pipeline/integration-tests.md` — test spec (read-only reference).
- The stub module created in phase 9.

## Plan

1. Read the architecture doc and identify the stub module → verify: stubs exist.
2. Plan implementation per architecture decisions → verify: every public function in the doc has a planned body.
3. Implement using nhcore primitives first; fall back to Hackage only with a `Ghc` prefix as last resort → verify: imports list is auditable.
4. Confirm compilation → verify: `nix develop --command cabal build all` succeeds.

Assumptions:
- NeoHaskell style is strict: pipes (`|>`) over nesting, `do`+`let` over `let..in`/`where`, `case`...`of` over guards in arg lists, qualified imports, `[fmt|...|]` for strings, `Task`/`Result` not `IO`/`Either`, no `$`, no point-free.
- `nhintegrations` enables `Strict` globally (see its cabal common stanza) — never add `!` annotations.
- Source module path is `integrations/Integration/<module_name>/` and `integrations/Integration/<module_name>.hs`; never write integration code under `core/`.
- Test files are immutable in this phase — never edit them.

If any assumption fails, refuse — do not guess.

## Steps

1. Load the architecture doc.
2. For each public function, replace the stub body with the real implementation.
3. Reach into nhcore (`Text`, `Array`, `Result`, `Task`, `EventStore`, etc.) before any external package.
4. If an external package is unavoidable, alias it with a `Ghc` prefix (e.g. `import qualified Data.Map.Strict as GhcMap`).
5. Run `nix develop --command cabal build all` to confirm compilation.
6. Do not run tests here — that is phase 11.
7. Verify no test files changed: `git diff --name-only HEAD -- 'integrations/test/' 'core/test/' 'testbed/tests/'` (and any other test directories the architecture doc names) must return empty. Refuse if any match — `tests are immutable in phase 10`.
8. Run `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py complete 10`.

## Output

Stubs replaced with real implementations; `nix develop --command cabal build all` green; phase 10 marked complete.

## Refusals

- Architecture doc missing → refuse: "no architecture doc; run phase 07 first".
- Build fails → refuse and surface the error.
- Any test file changed → refuse: "tests are immutable in this phase".
