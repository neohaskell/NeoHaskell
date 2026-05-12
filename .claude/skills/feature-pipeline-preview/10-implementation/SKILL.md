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

## Plan (Karpathy 1 + 4)

1. Read the architecture doc and identify the stub module → verify: stubs exist.
2. Plan implementation per architecture decisions → verify: every public function in the doc has a planned body.
3. Implement using nhcore primitives first; fall back to Hackage only with a `Ghc` prefix as last resort → verify: imports list is auditable.
4. Confirm compilation → verify: `cabal build all` succeeds.

Assumptions:
- NeoHaskell style is strict: pipes (`|>`) over nesting, `do`+`let` over `let..in`/`where`, `case`...`of` over guards in arg lists, qualified imports, `[fmt|...|]` for strings, `Task`/`Result` not `IO`/`Either`, no `$`, no point-free.
- nhcore has `Strict` enabled globally — never add `!` annotations.
- Test files are immutable in this phase — never edit them.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Load the architecture doc.
2. For each public function, replace the stub body with the real implementation.
3. Reach into nhcore (`Text`, `Array`, `Result`, `Task`, `EventStore`, etc.) before any external package.
4. If an external package is unavoidable, alias it with a `Ghc` prefix (e.g. `import qualified Data.Map.Strict as GhcMap`).
5. Run `cabal build all` to confirm compilation.
6. Do not run tests here — that is phase 11.
7. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 10`.

## Output

Stubs replaced with real implementations; `cabal build all` green; phase 10 marked complete.

## Refusals

- Architecture doc missing → refuse: "no architecture doc; run phase 07 first".
- Build fails → refuse and surface the error.
- Any test file changed → refuse: "tests are immutable in this phase".
