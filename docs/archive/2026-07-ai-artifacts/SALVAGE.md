# Salvage record — 2026-07-07

Claim-by-claim verification of every archived knowledge document, performed **before** archival
(Phase 0 of the [Continuous Generation pipeline plan](../../plans/2026-07-07-continuous-generation-pipeline-plan.md), tracker [#715](https://github.com/neohaskell/NeoHaskell/issues/715)).

Verdicts: **valid** (verified against repo state 2026-07-07) · **invalid** (verifiably false; correction given) · **unverifiable** (can't check cheaply).

The **valid** claims below are the seed corpus for `codemap/capabilities.yaml` (Phase 3).
The **invalid** claims are the rot inventory that motivated the governing rule:
*no agent-visible document without a CI check or a generation source.*

---

# Part 1 — Knowledge docs: claim verification

## 1. CLAUDE.md (root)

| Claim | Verdict | Evidence/Correction |
|---|---|---|
| Monorepo has core lib, testbed, integrations, website, LSP, Rust installer | valid | `core/`, `testbed/`, `integrations/`, `website/`, `lsp/neohaskell-lsp.cabal`, `installer/Cargo.toml` (binary `neo-install`) |
| Monorepo has "transpiler" and "CLI" components | invalid | No transpiler or CLI package exists. `cabal.project` packages: core, testbed, integrations, lsp. Unmentioned extra: `ide/` (VSCode extension) |
| Architecture: event-sourcing + CQRS | valid | `core/service/Service/{EventStore,Command,Query,Entity}` all present |
| Directory map (7 rows) | valid | All paths + package names confirmed (`nhcore`, `nhintegrations`, `nhtestbed`) |
| Style table (pipes, fmt, Result, Task, Task.yield…) | valid | Mechanisms exist: `fmt` quasiquoter `core/core/Basics.hs:960`, `Result.hs`, `Task.hs` `yield`:74 |
| `cabal test nhcore-test-core` / `cabal test` targets | valid | Suites defined in `core/nhcore.cabal` |
| `hlint .` — "CI treats warnings as errors" | **invalid** | hlint appears in NO workflow step; only a dev-shell comment at `.github/workflows/test.yml:157` |
| `./scripts/run-doctest` | valid | Exists |
| `./testbed/scripts/run-tests.sh` auto-starts app | valid | Line 50: `cabal run nhtestbed &` + wait loop |
| Pipeline state paths + resume commands | valid | Scripts existed; state dirs moved to `telemetry/archive/` in Phase 0 |
| ADRs in `docs/decisions/NNNN-slug.md` | valid | 0001–0030+ present |
| Branch-off-main enforced by hook | valid | `.claude/settings.json` PreToolUse hook |
| `nhcore-test-core` auto-discovered (hspec-discover) | **invalid** | `core/test-core/Main.hs` is manual registration |
| `nhcore-test-auth` auto-discovered | **invalid** | Manual: explicit `Hspec.describe` list |
| `nhcore-test-integration` auto-discovered | **invalid** | Manual: explicit list |
| `nhcore-test-service` manual registration | valid | Confirmed |
| `nhcore-test` auto-discovered | valid | `core/test/Main.hs` = `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}` — the ONLY auto-discovered suite |
| PostgresSpec gates on `POSTGRES_AVAILABLE` | valid | Confirmed in spec files |

## 2. core/AGENTS.md

| Claim | Verdict | Evidence/Correction |
|---|---|---|
| "9 hs-source-dirs in one library" | **invalid** | **18** hs-source-dirs (`nhcore.cabal:381`): adds auth, config, decimal, neoql, parser, layout, syntax, schema |
| Structure tree (10 dirs) | **invalid** (stale) | Omits 8 dirs (auth=24 modules, config=5, decimal, layout, neoql, parser, schema, syntax). Counts drifted: core 26→28, traits 8→9, concurrency 7→8, http 2→3, testlib 48→50; service actual = 95 |
| WHERE-TO-LOOK table (16 rows) | valid | Every file/dir exists incl. `http/Http/Client.hs` (`get`:288, `post`:493) |
| Core.hs "auto-imported in all NeoHaskell files" | **invalid** (nuance) | `NoImplicitPrelude` + explicit `import Core` convention — not automatic |
| Core.hs re-export list | valid | Array, Map, Maybe, Result, Task, Text, Basics, Channel, ConcurrentVar, DurableChannel, Lock, Uuid, Entity, Command, Query, Service, InsertionType |
| Traits table (8 traits) | valid | Mappable=Functor, Thenable=Monad (`andThen`,`yield`), Appendable=Semigroup (`++`), Combinable=Monoid (`empty`), Default, ToText confirmed. Omits `Documented.hs` |
| Collection = "31 methods" | **invalid** | 36 `*Impl` method signatures in `traits/Collection.hs` |
| `newtype Task err value = Task { runTask :: ExceptT err IO value }` | valid | Exact at `Task.hs:62-63` |
| Task functions (yield/throw/andThen/fromIO/runOrPanic) | valid | All present |
| `File.Error = NotFound \| NotWritable \| NotReadable` | valid | Exact at `system/File.hs:28` |
| Concurrency wrappers: AsyncTask=Async, Channel=Unagi, ConcurrentVar=MVar, Lock=MVar | valid | Confirmed via imports |
| ConcurrentMap = `TVar (Map k v)` | **invalid** | Uses `StmContainers.Map` (`concurrency/ConcurrentMap.hs:30`) |
| Anti-patterns table | valid | Array is vector-backed (`Array.hs:75`) |
| "All test suites use hspec-discover" | **invalid** | Only `nhcore-test` does |

## 3. core/service/AGENTS.md — highest quality of the set

| Claim | Verdict | Evidence/Correction |
|---|---|---|
| Structure tree | valid (incomplete) | Omits: FileUpload/, Infra/, TH/, Transport/, Event/, AccessControl.hs, Auth.hs, QueryObjectStore, ServiceDefinition, OutboundIntegration/, Trigger.hs, EventVariantOf.hs, EventStore/Simple.hs |
| Type families EntityOf/EventOf/NameOf/EntitiesOf | valid | `Entity/Core.hs:21,31`, `Command/Core.hs:63`, `Query/Core.hs:156` |
| Command retry "exponential backoff, max 10" | valid | `CommandExecutor/Core.hs:248` `maxRetries = 10`; 10ms doubling, cap 1s, jitter |
| Decider API acceptNew/acceptExisting/acceptAfter/reject | valid | `Decider.hs:13-16` (+ undocumented `acceptAny`) |
| Query pipeline (Subscriber → Updater → QueryOf.combine → QueryObjectStore; rebuildAll → start) | valid | All exports confirmed |
| Integration withOutbound/withInbound/withOutboundLifecycle | valid | `Application.hs:34-36` |
| WHERE-TO-LOOK rows | valid | All confirmed by grep |
| Application wiring example | valid | Matches `Application.hs:245` doc example |

## 4. testbed/AGENTS.md

| Claim | Verdict | Evidence/Correction |
|---|---|---|
| Structure tree | valid (incomplete) | Omits: Testbed/Config.hs, Document/, Examples/, tests/files/, tests/integrations/, Cart/Commands/CreateCartInternal.hs, Cart/Integrations/ |
| Launcher "11 lines total" | **invalid** | 12 lines |
| App.hs = fat wiring | valid | Confirmed |
| Queries use `deriveQuery` TH | valid | `Cart/Queries/CartSummary.hs:14,58` |
| periodicCartCreator timer inbound | valid | `Integrations.hs:12` |
| Hurl endpoints `localhost:8080/{commands,queries}/...` | valid | Confirmed in hurl files |
| `cabal run nhtestbed` | valid | `executable nhtestbed` at nhtestbed.cabal:86 |

## 5. integrations/AGENTS.md

| Claim | Verdict | Evidence/Correction |
|---|---|---|
| Structure tree | valid (incomplete) | Omits `Acs` and `AzureAI` integrations + their tests |
| Design ADRs 0008, 0015 | valid | Both exist in docs/decisions/ |
| Two-persona facade/Internal layout | valid | Brevo, OpenRouter, Http follow it; `ToAction` at `Integration.hs:51` |
| Usage example API | valid | `Brevo/Request.hs:82-123`; `Integration.hs:46-48,244` |
| `${VAR}` template → `AuthenticationError` | valid | `Http/Auth.hs:28`, `Http/Internal.hs:252,275` |
| `Config.field @(Redacted Text)` | valid | `core/config/Config.hs:133`, `core/core/Redacted.hs` |

## 6. website/AGENTS.md

| Claim | Verdict | Evidence/Correction |
|---|---|---|
| "Repository: `neohaskell/website`" | **invalid** | It's a monorepo directory |
| pnpm; dev (4321); build | valid | package.json scripts confirmed |
| Translations es/ru/hy/fr/ja auto-generated on push to main | valid (nuance) | `translate.yml` confirmed; **but** `astro.config.mjs:38` — non-English locales currently disabled ("docs revamp") |
| Sidebar structure | **invalid** (stale) | Actual adds: Why NeoHaskell?, Tutorial: Build NeoBank, Coming From…; "API Reference" → "Reference" |

## 7. context/TODO.md (dead planning doc, 2025-11-17)

| Claim | Verdict | Evidence/Correction |
|---|---|---|
| Event Store files exist | valid | All exist |
| `InsertionType = StreamCreation \| InsertAfter \| ExistingStream \| AnyStreamState` | valid | Exact at `Service/Event.hs:45-49` |
| "Status: Planning phase — awaiting implementation" | **invalid** (stale) | Phase-1 items shipped: Command/, CommandExecutor/ (planned as "CommandHandler"), Query/, SnapshotCache/, Application.hs |
| Proposed Command API (streamId/decide, AcceptCommand/RejectCommand) | **invalid** (superseded) | Shipped: `decideImpl`/`getEntityIdImpl` + Decider monad |
| ADRs in `docs/adr/` | **invalid** | Actual: `docs/decisions/` |

## 8. context/collections.md

| Claim | Verdict | Evidence/Correction |
|---|---|---|
| Qualified-import convention for collections | valid | Matches codebase |
| Standard API of ~40 functions all collections define | **invalid** | Array.hs defines only ~17 of them; the `Collection` trait uses different names entirely (`wrapImpl` not `yield`; 36 `*Impl` methods) — aspirational spec reality diverged from |

## 9. context/documentation.md

| Claim | Verdict | Evidence/Correction |
|---|---|---|
| Functions include doctests `-- >>>` | valid | Widely practiced (89 in Array.hs alone) |
| Property doctests `-- prop>` | **invalid** | Zero `prop>` occurrences under `core/` |
| "Markdown format instead of Haddock format" | **invalid** | Codebase uses Haddock markers (`-- \|`, `'foo'` links) throughout |

---

# Part 2 — Skills: salvage ranking (all 14 archived)

| Skill | Files | Salvage value | Destination |
|---|---|---|---|
| feature-pipeline-preview | 72 | **HIGH** | spec-gate flow; hlint rules + hooks (scripts); security checklist; performance checklist; QA rubrics |
| neohaskell-style-guide | 1 | **HIGH** | hlint rules + hook messages; copy-adapt implementer |
| neohaskell-implementer | 1 | **HIGH** | copy-adapt implementer |
| neohaskell-adr-template | 1 | **HIGH** | ADR template (reusable nearly verbatim) |
| neohaskell-qa-designer | 1 | **HIGH** | QA/test design (merge with newer test-spec-rubric.md) |
| neohaskell-security-review | 1 | **HIGH** | security checklist (merge under security-methodology.md's proportionality rules) |
| neohaskell-adr-architect | 1 | MEDIUM | ADR interview process (complements template) |
| neohaskell-devex-review | 1 | MEDIUM | API-shape rules; anti-pattern table is unique |
| integration-pipeline-preview | 67 | MEDIUM | portable-artifact pattern delta ONLY (near-clone of feature-pipeline-preview) |
| neohaskell-performance-review | 1 | MEDIUM | performance checklist — **with corrections** (see below) |
| neohaskell-community-writer | 1 | LOW–MED | PR-body template + label taxonomy only |
| dx-council-lang | 2 | LOW | none |
| dx-council-cli | 1 | LOW | none |
| neohaskell-feature-pipeline | 2 | LOW | **none — fully superseded** by feature-pipeline-preview (its own frontmatter says so; zero unique content) |

**Salvage-first order:**
1. `feature-pipeline-preview/references/` — grounding-loop.md (4-question demote filter, tier matrix, overkill catalogue), security-methodology.md (OWASP 2025/STRIDE/SLSA 1.1/SSDF + proportionality), performance-methodology.md (INLINABLE-vs-INLINE, UNPACK redundancy under `-funbox-strict-fields`, Strict footguns, TVar-Map contention), nhcore-context.md ("red-flag-but-already-handled" demote list — ideal hook-message copy), 3 review rubrics, jess-persona.md, phase-allowed-paths.md
2. `feature-pipeline-preview/scripts/` — sec-static-checks.py, perf-static-checks.py, lint-imports.py, lint-test-patterns.py (→ hlint rules + hooks); pipeline.py (514-line state machine) + verify-leaf.py (trust-but-verify) (→ spec-gate flow)
3. neohaskell-style-guide — 12-rule dialect table with correct/wrong pairs, ready-made as hlint rules + hook error messages
4. neohaskell-implementer — copy-adapt base (reuse-first, boy-scout, 10-iteration circuit breaker, event-sourcing scaffolds with EntityOf/EventOf)
5. feature-pipeline-preview orchestration invariants — rubric auto-gates (PAUSE only 3/16/17/18), per-phase diff allow-lists, draft-PR-at-ADR flow
6. ADR assets: adr-template + adr-architect + preview 03-adr-draft (note: 03-adr-draft's section list contradicts adr-template's — resolve at rebuild)
7. QA: qa-designer + test-spec-rubric.md (**ratio drift**: qa-designer says 3:1 edge:happy minimum; newer rubric says 1:1 hard floor / 3:1 stretch — rubric wins)
8. security-review — grounded mechanisms list (`Redacted`, hand-written Show on secrets, `canAccess`/`canView`, `constEq` with mandatory INLINE, Hasql typed statements)
9. performance-review — 50k req/s target, hot-path budgets (<1ms command, <0.5ms event apply, <0.2ms query); **corrections**: it advises UNPACK on primitive fields and blanket INLINE — performance-methodology.md documents UNPACK as redundant under `-funbox-strict-fields` and prefers INLINABLE; methodology + grounding-loop win
10. integration-pipeline-preview — portable-artifact pattern only (design doc travels in draft-PR body, never lands in docs/decisions/, empty-diff-as-signal)

**Rot found inside the skills themselves:**
- Both preview pipelines claim "CI's own hlint check is the canonical gate at merge time" — that backstop does not exist (hlint absent from CI)
- Old `neohaskell-feature-pipeline` gates on "hlint clean" — posture deliberately demoted in the preview version
- No skill anywhere encodes the early-exit **sentinel-guard** rule for Task validation — Phase 2 must add it from scratch
- Frontmatter model pins (claude-opus-4-7, per-leaf haiku/sonnet/opus) are environment-specific; re-decide at rebuild
