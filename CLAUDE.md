# NeoHaskell

NeoHaskell is a newcomer-friendly Haskell dialect. Monorepo with core library (nhcore), testbed, transpiler, CLI, Rust installer, and Astro website. Event-sourcing + CQRS architecture.

## Where things live

| Task | Location |
|---|---|
| Core types (Text, Array, Result) | `core/core/` |
| Event-sourcing | `core/service/Service/` |
| Concurrency primitives | `core/concurrency/` |
| Tests | `core/test/`, `testbed/tests/` |
| CLI design | `cli/design/` |
| Transpiler design | `transpiler/design/` |
| Installer (Rust) | `installer/` |
| Website (Astro/Starlight) | `website/` |

## Universal style rules

- **Pipes over nesting**: `x |> foo |> bar` — not `bar $ foo x`
- **Do + let** for bindings — never `let..in` / `where`
- **`case ... of`** for pattern matching — never in function arg lists
- **Qualified imports**: `EventStore.new` style
- **String interpolation**: `[fmt|Hello #{name}!|]` — not `<>` / `++`
- **`Task err val`** instead of `IO`; **`Result error value`** instead of `Either`
- **`Task.yield` / `Result.ok`** instead of `pure` / `return`

## Forbidden

`let..in`, `where`, point-free style, single-letter type params, raw `IO`, `Either`, `<>`/`++` for string concat.

## Build / test

```bash
cabal build all                 # Build everything
cabal test                      # All tests
cabal test nhcore-test-core     # Core only (no Postgres needed)
hlint .                         # Lint (CI treats warnings as errors)
./scripts/run-doctest           # Doctests
```

Test suites needing PostgreSQL: `nhcore-test-service`, `nhcore-test-integration`, `nhcore-test`.

## Reference

- **Detailed conventions, full pipeline, all test patterns**: see [AGENTS.md](AGENTS.md)
- **Style enforcement**: invoke the `neohaskell-style-guide` skill
- **Feature implementation pipeline**: invoke the `neohaskell-feature-pipeline` skill (17 phases, run via `python3 .opencode/skills/neohaskell-feature-pipeline/pipeline.py`)
- **ADR drafting**: invoke `neohaskell-adr-architect` or `neohaskell-adr-template`

## Non-negotiable

- Every change ships with tests (happy path + error + boundary)
- Bug fixes include regression tests
- Never modify existing test expectations without maintainer approval
- Branch off `main`; never edit `main` directly (enforced by hook)
