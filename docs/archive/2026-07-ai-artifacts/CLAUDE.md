> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

# NeoHaskell

Newcomer-friendly Haskell dialect. Monorepo: core library (`nhcore`), testbed, outbound integrations, transpiler, LSP, CLI, Rust installer, Astro website. Architecture: event-sourcing + CQRS.

## Directory map

| Component | Location | Package |
|---|---|---|
| Core primitives (Text, Array, Result, Task) | `core/core/` | nhcore |
| Event-sourcing / CQRS | `core/service/Service/` | nhcore |
| Concurrency primitives | `core/concurrency/` | nhcore |
| Test utilities | `core/testlib/` | nhcore |
| Outbound integrations (Brevo, OpenRouter, …) | `integrations/Integration/` | nhintegrations |
| Testbed (reference app + acceptance tests) | `testbed/` | nhtestbed |
| Website (Astro/Starlight) | `website/` | — |

Deep reference for each package: the `AGENTS.md` file in that directory.

## Style (mandatory)

| Use | Never |
|---|---|
| `x \|> foo \|> bar` | `bar $ foo x`, `$` |
| `do let y = expr` | `let..in`, `where` |
| `case x of` | patterns in function head |
| `[fmt\|Hello #{name}!\|]` | `<>` / `++` for strings |
| `Result err val` | `Either` |
| `Task err val` | `IO` |
| `Task.yield v` | `pure`, `return` |
| `forall element result.` | single-letter type params |
| `import Foo (Foo); import Foo qualified` | unqualified imports |

Full enforcement → invoke `neohaskell-style-guide`.

## Build & test

```bash
cabal build all                 # everything
cabal test nhcore-test-core     # core only (no Postgres)
cabal test                      # all (Postgres needed for service/integration suites)
hlint .                         # lint — CI treats warnings as errors
./scripts/run-doctest           # doctests
./testbed/scripts/run-tests.sh  # acceptance tests (auto-starts app)
```

## Skills

| When you need to… | Skill |
|---|---|
| New feature end-to-end (ADR → PR) | `feature-pipeline-preview` |
| New outbound integration (design → PR) | `integration-pipeline-preview` |
| Write or review NeoHaskell code | `neohaskell-implementer` |
| Style / convention question | `neohaskell-style-guide` |
| Draft an ADR (interactive interview) | `neohaskell-adr-architect` |
| Draft an ADR (template only) | `neohaskell-adr-template` |
| DX / API ergonomics review | `neohaskell-devex-review` |
| Security review | `neohaskell-security-review` |
| Performance review | `neohaskell-performance-review` |
| Design test specification | `neohaskell-qa-designer` |
| Write docs, PR body, release notes | `neohaskell-community-writer` |
| Language syntax / DX tradeoff decision | `dx-council-lang` |

**Feature pipeline state:** `.pipeline/state.json`
Resume: `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py status`

**Integration pipeline state:** `.integration-pipeline/state.json`
Resume: `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py status`

ADRs live in `docs/decisions/NNNN-slug.md`.

## Non-negotiable

- Every change ships with tests (happy path + error + boundary)
- Bug fixes include regression tests
- Never modify existing test expectations without maintainer approval
- Branch off `main`; never edit `main` directly (enforced by hook)

## Test suites

| Suite | Scope | PostgreSQL | Auto-discovered |
|-------|-------|-----------|----------------|
| `nhcore-test-core` | Core primitives | No | Yes (hspec-discover) |
| `nhcore-test-auth` | Auth & JWT | No | Yes (hspec-discover) |
| `nhcore-test-service` | EventStore, Commands, Queries | Yes (mostly); Postgres-only specs (`PostgresSpec`) gate on `POSTGRES_AVAILABLE=true` and self-skip otherwise | **No** — manual registration in `core/test-service/Main.hs` |
| `nhcore-test-integration` | Integration tests | Yes | Yes (hspec-discover) |
| `nhcore-test` | All of the above | Yes | Yes (hspec-discover) |
