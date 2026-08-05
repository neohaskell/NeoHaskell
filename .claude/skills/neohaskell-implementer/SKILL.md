---
name: neohaskell-implementer
description: Write NeoHaskell (not vanilla Haskell) code in this repo — copy-adapt discipline, the fast repair loop, and the dialect escape hatch. Use when implementing or modifying any .hs file.
---

# NeoHaskell implementer

Rebuilt 2026-07-07 (Phase 2 of the pipeline plan). The dialect rules below
are mechanically gated (hook → hlint → GHC); the working discipline
(copy-adapt, repair protocol, circuit breaker) is protocol — followed because
it works, verified at review.

## The one structural rule: copy-adapt, never blank-buffer

**No `.hs` file is ever created from scratch.** A model editing 200 lines of
real NeoHaskell produces NeoHaskell; a model facing an empty buffer produces
Stack Overflow Haskell. Before writing:

1. Copy-adapt from the **plan-resolved** nearest-neighbor module — the one the
   localizer pinned at plan time (pipeline step 6 records "which neighbor
   module each copy-adapts from"). Which module to copy is a "where things
   live" question, so implementation never searches the tree for one (AGENTS.md
   HARD RULE). Plan didn't pin one? The neighbor is a planning output, never an
   implementation-time lookup: **stop, park the run (`wrong-localization`), and
   return to planning** so the localizer pins it and records it at step 6.
   Resume never re-plans (AGENTS.md), so discovering or looking up a missing
   neighbor here is never allowed — fix the localization asset and re-enter.
   The neighbor shares the target's architectural shape (command, entity,
   query, transport feature, integration, Core primitive, spec).
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
3. `./dev api "<type or name>"` — hoogle type search: the NeoHaskell
   surface ranked top; vanilla (real dependency closure + boot libs) below
   with a disclaimer whenever it has results — the section is omitted when
   empty. Exit codes: 0 neo hit, 1 none, 2 usage, 3 vanilla-only (escape-
   hatch territory). Query in dialect types — respell vanilla vocabulary
   using the AGENTS.md style table (the single source of that mapping) and
   search again. Use it the moment you feel yourself *remembering* an API
   instead of *reading* one.
4. `codemap/phrasebook.md` — doctest-verified usage patterns (gate:
   test.yml `doctest` job).

The line the localizer's "execution never searches" doctrine draws:
**"search" means exploring the tree/source to find where things live —
plan-time only. `./dev api`, the hot card, and the phrasebook are LOOKUPS
and are allowed while implementing; recalling an API from training data
never is.**

If `./dev check` reports `invented-api-events=N` ("not in scope"), that is a
hallucinated symbol: resolve via `./dev api`, and the pipeline records the
count (per-stage `invented_api_events`, schema v2; `invented-api` failure
label when it kills the run) — do not guess twice.

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
