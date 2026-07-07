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

- Test discovery: **only `nhcore-test` uses hspec-discover**; `nhcore-test-core`, `-auth`, `-service`, `-integration` register specs manually in their `Main.hs` — new spec modules must be added there AND to the cabal `other-modules`.
- Postgres-dependent specs self-gate on `POSTGRES_AVAILABLE=true`.
- ⚠ **hlint does NOT run in CI**, and the current `.hlint.yaml` does not encode NeoHaskell style — do not treat hlint output as style guidance until the Phase 2 rebuild lands.

## Non-negotiable

- Every change ships with tests (happy path + error + boundary); bug fixes include regression tests.
- Never modify existing test expectations without maintainer approval.
- Branch off `main`; never edit `main` directly (hook-enforced).
- ADRs live in `docs/decisions/NNNN-slug.md`.
