---
name: neohaskell-implementer
description: Write NeoHaskell (not vanilla Haskell) code in this repo — copy-adapt discipline, the fast repair loop, and the dialect escape hatch. Use when implementing or modifying any .hs file.
---

# NeoHaskell implementer

Rebuilt 2026-07-07 (Phase 2 of the pipeline plan; predecessor archived in
`docs/archive/2026-07-ai-artifacts/claude-skills/`). The dialect rules below
are mechanically gated (hook → hlint → GHC); the working discipline
(copy-adapt, repair protocol, circuit breaker) is protocol — followed because
it works, verified at review.

## The one structural rule: copy-adapt, never blank-buffer

**No `.hs` file is ever created from scratch.** A model editing 200 lines of
real NeoHaskell produces NeoHaskell; a model facing an empty buffer produces
Stack Overflow Haskell. Before writing:

1. Find the nearest-neighbor module (same architectural shape: command,
   entity, query, transport feature, integration, Core primitive, spec).
2. Copy its skeleton — imports block, module header, structure.
3. Adapt. Your imports section should look like the neighbor's, not like
   your training data.

## Dialect (gates: edit hook → hlint → GHC)

The style table lives in `AGENTS.md` — read it. Enforcement layers you will
hit, in order:

1. **Edit hook** (~50ms): rejects `$`, `where` clauses, `Either`,
   `pure`/`return` usage, vanilla imports, unqualified open imports,
   `case-of-Bool` — on ADDED lines only, with the rule quoted.
2. **`./dev lint`** (seconds): dialect-first hlint — vanilla modules are
   restricted to their Core wrappers + grandfathered boundaries.
3. **GHC** via `./dev check`: `NoImplicitPrelude` — vanilla vocabulary mostly
   isn't in scope anyway.

**Escape hatch (deliberate two-step, use it rather than working around a ban):**
no Core wrapper exists for what you need and it's not core enough to build one?

- False positive on a line → `-- HOOK-ALLOW: <reason>` on that line.
- Real vanilla need → add your module to the relevant `within:` list in
  `.hlint.yaml` WITH a justification comment and a `belongs-in:` note (the
  Core module the wrapper would live in). Rule of three: the third exception
  for the same symbol means promote a primitive instead.
- NEVER reimplement a banned thing with allowed vocabulary — that is strictly
  worse than the exception.

## API discipline (Phase 4): transcribe, never recall

Your training data contains vanilla Haskell APIs; this repo doesn't. Before
calling ANY function you didn't just read:

1. The plan's `uses:` list (plan-time resolved symbols) — transcribe those.
2. `codemap/api-hot.md` — the frequency-ranked card of what this repo
   actually calls, with verified examples.
3. `./dev api "<type or name>"` — type-directed search over OUR surface
   ("Text -> Maybe Uuid" → `Uuid.fromText`). Use it the moment you feel
   yourself *remembering* an API instead of *reading* one.
4. `codemap/phrasebook.md` — doctest-verified usage patterns.

If `./dev check` reports `invented-api-events=N` ("not in scope"), that is a
hallucinated symbol: resolve via `./dev api`, and the pipeline records the
event (telemetry `invented-api` label) — do not guess twice.

## Repair loop (protocol, not suggestion)

```
./dev watch          # once per session (resident typechecker)
edit → wait ~2s → ./dev check          # 0.6s feedback
./dev test "pattern" [suite]           # targeted specs, ~4-9s, link-free
```

- Never spawn `cabal build` inside the loop.
- Max 2 repair rounds on the same error, then stop and escalate (report the
  error verbatim) — unbounded persistence is where wrong code comes from.
- Test suites: only `nhcore-test` is hspec-discovered; `-core`, `-auth`,
  `-service`, `-integration` need manual registration in their `Main.hs`
  AND the cabal `other-modules`.

## Non-negotiable (from AGENTS.md, mechanically enforced where possible)

- Every change ships with tests (happy path + error + boundary); bug fixes
  include a regression test that was red first.
- Never modify existing test expectations without maintainer approval.
- Early-exit sentinel guards in `Task` validation, not nested if/case pyramids.
- `if cond then a else b`, never `case cond of True -> ...`.
