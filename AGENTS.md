# NeoHaskell — agent guide

<!-- Every claim in this file was verified against the repo on 2026-07-07.
     Governing rule: no agent-visible document without a CI check or a generation source.
     Localization assets are being rebuilt in codemap/ (see docs/plans/2026-07-07-continuous-generation-pipeline-plan.md, tracker #715).
     Old guidance docs live in docs/archive/2026-07-ai-artifacts/ — ARCHIVAL, do not use. -->

Newcomer-friendly Haskell dialect. Monorepo: core library (`core/` → `nhcore`), reference app + acceptance tests (`testbed/` → `nhtestbed`), outbound integrations (`integrations/` → `nhintegrations`), LSP (`lsp/`), Rust installer (`installer/` → `neo-install`), VSCode extension (`ide/`), Astro website (`website/`). Architecture: event-sourcing + CQRS in `core/service/`.

## Style (mandatory)

| Use | Never |
|---|---|
| `x \|> foo \|> bar` | `bar $ foo x`, `$` |
| `do let y = expr` | `let..in`, `where` |
| `case x of` | patterns in function head |
| `if cond then a else b` | `case cond of True -> …` |
| Early-exit sentinel guards in `Task` validation | nested if/case pyramids |
| `[fmt\|Hello #{name}!\|]` | `<>` / `++` for strings |
| `Result err val` | `Either` |
| `Task err val` | `IO` |
| `Task.yield v` | `pure`, `return` |
| `forall element result.` | single-letter type params |
| `import Foo (Foo); import Foo qualified` | unqualified imports |
| nhcore Core modules (`Text`, `Array`, `Char`, `File`, `Path`…) | raw `Data.*` / `System.*` / `Ghc*` imports |

## Build & test (commands verified 2026-07-07)

```bash
cabal build all                 # everything
cabal test nhcore-test-core     # core primitives only (no Postgres)
cabal test                      # all suites (Postgres needed: docker-compose up -d)
./scripts/run-doctest           # doctests
./testbed/scripts/run-tests.sh  # acceptance tests (auto-starts the app)
```

## Fast inner loop (measured 2026-07-07 — use this in repair loops, NOT cabal build)

Single entrypoint: **`./dev`** (no-args lists all verbs; same tools for humans and agents, deliberately):

```bash
./dev watch                  # start resident typecheck watcher → .ghcid-errors.txt (once per session)
./dev check                  # quick typecheck status (instant from watcher; one-shot fallback)
./dev test "pattern" [suite] # link-free hspec --match (~4-9s; default suite nhcore-test-core)
./dev refresh                # re-warm -O0 build after pull/switch; prints modules-rebuilt
./dev exec <cmd>             # any command with the pinned toolchain
```

- Repair-loop protocol: edit → wait ~2s → **`./dev check`** (measured: error feedback 0.6s, recovery 1.9s). Never spawn `cabal build` inside the loop.
- You do NOT need to be inside `nix develop`: every verb self-provisions the pinned toolchain (~0.4s warm overhead).
- Everything uses the dev flavor (`cabal.project.dev`, `-O0`); full nhcore -O0 build = 249 modules / ~54s on this machine.
- Pipeline telemetry: `scripts/telemetry.py` (schema: `telemetry/SCHEMA.md`, frozen v1). Every pipeline run emits one line to `telemetry/runs.jsonl`. Telemetry is pipeline-only: never emit lines for ad-hoc runs.
- These are the same commands humans use (README "Fast inner loop") — parity is deliberate; don't create agent-only variants.

- Test discovery: **only `nhcore-test` uses hspec-discover**; `nhcore-test-core`, `-auth`, `-service`, `-integration` register specs manually in their `Main.hs` — new spec modules must be added there AND to the cabal `other-modules`.
- Postgres-dependent specs self-gate on `POSTGRES_AVAILABLE=true`.
- ⚠ **hlint does NOT run in CI**, and the current `.hlint.yaml` does not encode NeoHaskell style — do not treat hlint output as style guidance until the Phase 2 rebuild lands.

## Non-negotiable

- Every change ships with tests (happy path + error + boundary); bug fixes include regression tests.
- Never modify existing test expectations without maintainer approval.
- Branch off `main`; never edit `main` directly (hook-enforced).
- ADRs live in `docs/decisions/NNNN-slug.md`.
