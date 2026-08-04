# Porting `neo` to NeoHaskell — design & phased plan

**Status:** draft for review
**Author:** drafted with Claude Code from a design conversation
**Scope:** move the NeoCLI tooling into the NeoHaskell monorepo and rewrite its logic in NeoHaskell (`nhc`), keeping only a thin Rust launcher. Replace regex-based Haskell inspection with a real GHC AST/session. Default to machine/CI output; the primary UX is the web IDE and AI agents.

---

## Table of contents

1. [Goals](#1-goals)
2. [Guiding decisions](#2-guiding-decisions)
3. [Target architecture](#3-target-architecture)
4. [Component mapping: Rust → `nhc`](#4-component-mapping-rust--nhc)
5. [The AST analyzer](#5-the-ast-analyzer)
6. [Code ↔ model sync](#6-code--model-sync)
7. [IDE backend on WebTransport](#7-ide-backend-on-webtransport)
8. [CI-first & structured output](#8-ci-first--structured-output)
9. [Testing strategy (the headline win)](#9-testing-strategy-the-headline-win)
10. [Phased implementation plan](#10-phased-implementation-plan)
11. [Risks & open questions](#11-risks--open-questions)
12. [What gets deleted](#12-what-gets-deleted)
13. [Appendix: current-state facts](#appendix-current-state-facts)

---

## 1. Goals

- **Deeper static checks** on user Haskell than regexes allow: incremental/correct entity evolution, thorough branch detection across event → update → combine/query, cross-module flow resolution.
- **Fast, deterministic CI**: move analysis correctness from slow real-nix e2e into in-process `hspec`/QuickCheck.
- **Single ecosystem**: `neo` lives in the NeoHaskell monorepo and is built by the same pinned toolchain as the code it analyzes.
- **IDE-first UX**: users drive `build`/`run`/`test`/model-editing from the web IDE (or instruct an AI agent); CLI defaults to machine-readable output.
- **Live introspection**: a long-lived process holds a warm GHC session for watch-mode builds and rich IDE feedback.

## 2. Guiding decisions

| # | Decision | Rationale |
|---|---|---|
| D1 | **Full port, Rust = launcher only** | The orchestration surface (command dispatch, transports) is absorbed by NeoHaskell's `Application` + transports, so it is *deleted*, not re-treaded. |
| D2 | **`nhc` = a NeoHaskell `Application`** on nhcore | Gets CLI + Web transports, config, event store, CQRS for free. |
| D3 | **Analyzer = live GHC session** (raw `ghc` API) + `ghc-lib-parser` as a syntax-only fast path | Watch mode + introspection want a warm session; monorepo pins one GHC so the version-decoupling `ghc-lib` exists for is unnecessary for the typed pass. |
| D4 | **Code always wins**; JSON persists a structural cache + presentation overlay | Code holds logic the model can't; model holds layout the code can't. Detect-and-prompt reconcile, code authoritative. |
| D5 | **IDE backend on WebTransport** (HTTP command/query); **query polling** for updates; SPA served by WebTransport | No hand-rolled JSON-RPC/registry/session; one small framework addition (static route [#707]). Real-time push (SSE/WS) deferred — polling covers current needs and fits detect-on-load reconcile (D4). |
| D6 | **CI-first, structured (JSON) default output** | Real consumers are the web UI and AI agents; they want data, not `[info]` prose. Deleting the interactive-TUI requirement is what makes the port tractable. |

## 3. Target architecture

```
┌──────────────────────────────────────────────────────────────┐
│ host machine                                                   │
│                                                                │
│   neo  (tiny Rust launcher, on PATH)                           │
│     └─ exec:  nix develop --command  nhc  <args…>              │
│                                                                │
│   ┌──────────── nix develop (pinned toolchain) ────────────┐  │
│   │  nhc  (NeoHaskell Application)                          │  │
│   │   ├─ CliTransport      → build / run / test / new / …   │  │
│   │   ├─ WebTransport (warp)                                │  │
│   │   │    ├─ GET  /queries/*   (reads)                     │  │
│   │   │    ├─ POST /commands/*  (writes: heal, generate…)   │  │
│   │   │    └─ static assets (SPA)         ← issue #707      │  │
│   │   │       (updates via client polling of queries)       │  │
│   │   ├─ GHC session (warm): parse + typecheck + watch      │  │
│   │   ├─ SimpleEventStore (model edits / heal history)      │  │
│   │   └─ generators + reconcile + nix/git/hurl drivers      │  │
│   └────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
```

**The Rust launcher** does only: locate the flake, enter `nix develop`, exec `nhc`, forward args + signals, friendly error if `nix` is missing, (optional) update check. Every subcommand — including `ide` — is passthrough, because `nhc ide` serves its own SPA. Target: a few hundred LOC.

## 4. Component mapping: Rust → `nhc`

| Current Rust | Fate | `nhc` home |
|---|---|---|
| `src/cli.rs`, `src/app.rs` (clap dispatch) | replace | `Application` + `CliTransport` / `optparse-applicative` |
| `src/inspect/` (regex parser, ~2.7k LOC) | **rewrite** | GHC-session analyzer (§5) |
| `src/ide/heal/diff.rs`, `apply.rs` | port (logic preserved) | `nhc` model-diff module (consumes same `ProjectInspection` shape) |
| `src/ide/rpc.rs`, `registry.rs`, `session.rs`, `server.rs`, `transport.rs` | **delete** | WebTransport does this |
| `src/ide/methods/*` | port as commands/queries | `POST /commands/*`, `GET /queries/*` |
| `src/ide/methods/heal_event_model.rs` (`claude -p` subprocess) | port + upgrade | `nhintegrations` OpenRouter LLM client (in-process) |
| `src/commands/ide.rs` (axum + rust-embed) | **delete** | WebTransport + static route (#707) |
| `src/reconcile/*` (cabal/cabal.project/flake/Spec.hs gen) | port | `nhc` generators (pure `Task` file I/O + templating) |
| `src/reconcile/dep_spec.rs` (npm-semver→cabal, 938 LOC) | port | pure module; becomes QuickCheck-testable |
| `src/interpret/*` (11 stderr regexes) | port, partially dissolve | GHC compile errors become structured diagnostics; nix/git stay stderr-regex |
| `src/subprocess/*` (nix/hurl/ghci wrappers) | port | `Task`-based process calls; build driven through GHC session where possible |
| `src/tui/*`, per-command ratatui FSMs | **delete** | CI-first output removes the need (D6) |
| `src/errors.rs` (miette) | port | typed `Task` errors + structured JSON error envelope |
| `src/network.rs` (git ls-remote, tarball, registry) | port | `Task` + `Http.Client` / git process calls |
| `src/skills.rs`, `commands/skills.rs` | port | `nhc skills setup` |

## 5. The AST analyzer

Replaces `src/inspect/parse.rs`. Two passes behind the **same `ProjectInspection` output contract** (so the downstream diff/heal logic ports without re-design).

### Pass 1 — syntactic (`ghc-lib-parser` + `ghc-lib-parser-ex`)
Version-decoupled, no session, no dependencies of the target code needed → trivially unit-testable (`String → ProjectInspection` fragment). Covers:

- event/command/query **declarations**, record **fields** (labels + surface types)
- decider/update/projection **functions** and their **branch structure** (`case`/pattern/guards)
- event → update → combine/query **wiring** by constructor name

### Pass 2 — typed (raw `ghc` API + `hie-bios`)
Uses the warm GHC session (§7) or `.hie` artifacts. Required for what syntax can't do reliably (aliases, imports, shadowing):

- **field-type resolution** to canonical types
- **incremental entity-change correctness**: follow record-construction/update across `update` branches, confirm each transition is additive and type-consistent

> Rule of thumb: **structure & shape → Pass 1; identity & types → Pass 2.**

### Isolation
All AST-shape matching lives behind one adapter module so a GHC-major bump is a one-file change. `ghc-exactprint` is pulled in only when we start doing model→code edits (§6).

## 6. Code ↔ model sync

**Ownership split:**

| Owned by **code** (authoritative) | Owned by **`event-model.json`** |
|---|---|
| events, commands, queries, integrations | node positions / layout |
| fields + types | chapters + chapter order |
| flow edges, branch structure | submodels / manual grouping |
|  | feature assignments, wave-order overrides |

**Operations:**

- **Generators** ("new event", "new entity", "new feature" — cf. the skills repo) **co-write structure into both** the `.hs` source (via AST code actions + `ghc-exactprint`, non-destructive) **and** `event-model.json`, in one step. New behaviour bodies are **stubs** (`reject "TODO"`) for the user/AI to fill.
- **On load**, the IDE compares the JSON's structural cache against a fresh projection from code and, on drift, **prompts directionally**:
  - "event model is outdated — heal from code?" (re-project structure; code wins)
  - "code is outdated — generate from event-model?" (apply AST code actions from model deltas)
- **Presentation overlay** (layout/grouping) is IDE-authoritative and reconciled by stable IDs.

Because code is authoritative and structure is a projection, **structural drift is detectable and always resolvable toward code** — the class of silent divergence the current heal pipeline fights largely goes away.

> This reverses the current "IDE fields are read-only" invariant: fields become editable *because* edits are now safe AST actions on source. Update memory `project_ide_code_model_field_sync.md` when committed.

## 7. IDE backend on WebTransport

WebTransport (`Service.Transport.Web`, warp under the hood) already provides `POST /commands/<kebab>`, `GET /queries/<kebab>` (with NeoQL `?q=` + pagination), JWT/OAuth2, CORS, `/health`, `/ready`, and OpenAPI/Scalar UI. The IDE RPC surface maps straight on:

| Today (hand-rolled JSON-RPC over WS) | On WebTransport |
|---|---|
| `workspace/readEventModel` | `GET /queries/event-model` |
| `workspace/writeEventModel` | `POST /commands/write-event-model` |
| `workspace/healEventModel` | `POST /commands/heal-event-model` |
| `workspace/relayoutEventModel` | `POST /commands/relayout-event-model` |

**One framework addition (in-tree, no new deps):**

1. **Static-asset route** to serve the SPA `dist/` with `index.html` fallback — tracked as **[neohaskell/NeoHaskell#707](https://github.com/neohaskell/NeoHaskell/issues/707)**. Cache-control is load-bearing: `immutable` for hashed bundles, `no-cache, must-revalidate` for `index.html`.

**Updates via polling, not push.** The two things the old WS pushed are handled with plain queries:

- **Heal progress** — heal is an **async job**: `POST /commands/heal-event-model` returns a `jobId`; the frontend polls `GET /queries/heal-status?job=…` (~500 ms while in flight) for `{state, progressLines, result?}`. This also avoids holding an HTTP request open for the 15-min timeout, which a synchronous streaming command would.
- **model-changed** — a cheap `GET /queries/event-model-version` returns a content hash; the frontend polls (~1–2 s, focus-gated) and refetches on change. Fits the detect-on-load reconcile model (D4).

Real-time push (SSE/websockets) is deferred; it only earns its keep for low-latency/high-frequency events (live multi-user collaboration, token-by-token LLM streaming), none of which are in scope.

**Frontend consequence:** `assets/ide/src/ipc/` moves from one WS + JSON-RPC envelopes to REST (`POST`/`GET`) + a small polling loop. Contained, and simpler than the hand-rolled client it replaces.

## 8. CI-first & structured output

- Default output is **machine-readable JSON** (as `inspect`/`validate` already are). Human `[info]/[ok]` text is a secondary tier; interactive TUI is dropped (D6).
- Watch mode is a long-lived `nhc` process driving builds **through the GHC session**, emitting structured diagnostics (`MsgEnvelope`) instead of scraping `cabal` stderr.

## 9. Testing strategy (the headline win)

| Behaviour | Today | After port | Layer |
|---|---|---|---|
| event/command/query extraction | slow real-nix e2e | pure `hspec` (source string → inspection) | unit |
| branch/flow detection | slow real-nix e2e | pure `hspec` | unit |
| incremental-entity-change correctness | **not checkable** | QuickCheck property over typed AST/`.hie` | unit |
| npm-semver → cabal translation | Rust unit | QuickCheck property | unit |
| AST code actions (model→code) | n/a | golden: `source + edit → source'` | unit |
| starter ↔ upstream API drift | flaky real-nix e2e | in-monorepo compile golden | integration |
| full user install flow | real-nix e2e | real-nix e2e (kept) | e2e |

The expensive real-nix layer stays for the genuine install path — it just stops being the *only* place analysis behaviour is observable.

## 10. Phased implementation plan

Each phase is independently landable. Per the repo's plan-thoroughness contract, **Tests** and **Verification** are specifications.

### Phase 0 — Scaffolding
Add the `nhc` package to the monorepo (`nhc/*.cabal` in `cabal.project`, `common common_cfg`, depends `nhcore`). Stub `Application` with a no-op CLI. Add the Rust launcher (`exec nix develop --command nhc "$@"`, nix-missing error, arg/signal passthrough).

- **Tests:** `launcher_passthrough_forwards_args`, `launcher_forwards_exit_code`, `launcher_errors_when_nix_absent` (spawn a fake `nhc`); `nhc_boots_and_prints_help`.
- **Verification:** `nix develop --command cabal build nhc`; `neo --help` == `nhc --help`; launcher binary runs on host outside the dev shell.
- **Regression:** existing Rust `neo` still builds until cut over.

### Phase 1 — AST analyzer behind `ProjectInspection`
Pass 1 (`ghc-lib-parser`) producing the existing `ProjectInspection` shape as JSON. Wire `nhc inspect` to emit it.

- **Tests (unit, pure):** one `hspec` per recognized shape — `parses_event_sum`, `parses_one_event_per_file`, `parses_command_decide_emitted_events`, `parses_query_subscribed_events`, `parses_combine_noop_branches`, `parses_record_fields_with_payload_module_hop`, `parses_integration_handle_arms`, `parses_aggregate_write_sets`. Port the fixtures in `parse.rs:1180+`/`mod.rs:431+` verbatim as the executable spec.
  - Edge cases: empty/whitespace module; Unicode identifiers; extensions that broke the regex parser (multiline records, comments inside `data`, qualified constructors); determinism (reordering decls → identical inspection); uncertainty resolves to *empty*, never *wrong*.
- **Tests (integration):** `inspect_matches_rust_on_testbed` — run both parsers over `nhtestbed`, assert identical `ProjectInspection` JSON.
- **Verification:** `nhc inspect | jq` on `nhtestbed` and a `neo new` project; diff against Rust `neo inspect`.
- **Regression:** downstream diff/heal consumes the JSON unchanged.

### Phase 2 — Typed pass + warm session
Raw `ghc` API + `hie-bios`; incremental-entity-change checker; field-type resolution.

- **Tests (unit/property):** `incremental_change_accepts_additive_update`, `..._rejects_type_narrowing`, `..._rejects_dropped_field`, `resolves_field_type_through_alias`, `resolves_qualified_constructor`. QuickCheck: generate additive vs destructive record evolutions, assert verdict.
- **Tests (integration):** session warms once and reloads on file change under `--watch`; `.hie`/session parity.
- **Verification:** introduce a deliberately incorrect entity evolution in a fixture; confirm a precise, LLM-actionable diagnostic with span.
- **Regression:** Phase-1 syntactic results unchanged.

### Phase 3 — Reconcile + generators
Port cabal / `cabal.project` / `flake.nix` / `tests/Spec.hs` generation and `dep_spec` translation. Add co-write generators (§6) with AST code actions.

- **Tests:** idempotence (`generate` twice → identical) for each artifact; `dep_spec` QuickCheck (npm range → cabal constraint round-trips); `new_event_cowrites_code_and_model`, `new_entity_cowrites`, golden `add_field_preserves_surrounding_source` (exact-print).
  - Edge cases: bad semver, bad git ref, conflicting dep specified two ways, empty dep set; `-Werror` cleanliness of generated `other-modules`; hspec-discover driver never clobbered.
- **Verification:** generated project compiles under nix; `cabal check`; run a generator, inspect the diff is minimal + `fourmolu`-clean.
- **Regression:** mirrors current `reconcile/mod.rs` idempotence tests + the test-suite-generation invariants (`project_test_suite_generation.md`).

### Phase 4 — IDE backend on WebTransport
Model read/write/heal/relayout as queries/commands. Land framework static route (#707). Heal as an async job (`jobId` + `heal-status` query). Heal via `nhintegrations` LLM. Port frontend `ipc/` to REST + polling.

- **Tests:** command/query handlers (unit, in-process — no nix); `read_event_model_not_found_is_success`; write is atomic/last-writer-wins; heal deterministic pre-pass short-circuits with no residuals; `heal_status_reports_progress_then_result`; `event_model_version_changes_on_edit` (polling signal); static route serves SPA + `index.html` fallback + correct cache headers.
  - Edge cases: malformed `event-model.json`; concurrent code-edit during model-write (watcher vs write arbitration); heal cancel via job id; LLM failure never drops the model on parse failure; polling a completed/expired jobId.
- **Verification:** `nhc ide`, load the SPA from WebTransport, edit model, observe autosave + live validation + SSE reload; screenshot via the existing critique harness.
- **Regression:** parity with current IDE RPC behaviour (`project_ide_*` memories, esp. autosave/validation/trace and node-handle contract).

### Phase 5 — Orchestration verbs + cutover
`build`/`run`/`test`/`lock`/`new`/`skills` as `Task`-based handlers driving nix/git/hurl (and GHC session for watch). Structured JSON output. Flip the Rust binary to pure launcher; delete the ported Rust.

- **Tests:** structured-output schema per command; `--ci` == default; watch `bail`/degrade rules; error-interpretation parity for nix/git stderr; GHC-diagnostic path for compile errors.
- **Verification:** full e2e against the nix-built launcher+`nhc` (mirror `tests/e2e.rs` groups D/F/G — build/run/test happy paths on a generated project).
- **Regression:** every assertion in `tests/integration_tests.rs` + `tests/e2e.rs` has a `nhc` equivalent before deleting the Rust.

## 11. Risks & open questions

- **AST code actions are HLS-level work.** Bounded and purely testable, but non-trivial; sequence them behind exact-print goldens.
- **Warm-session management** (package DB, reload) is the historically hard part of HLS. Mitigated by NeoHaskell's constrained project layout + `hie-bios`.
- **Concurrent edit arbitration** (editor writes `.hs` while IDE writes model) needs a debounce + write-lock so the watcher doesn't fight in-flight model→code writes.
- **`nix develop` startup latency** per one-shot CLI call (unchanged from today; acceptable, optimizable later).
- **Open:** does `nhc` event-source the model edits (SimpleEventStore) from day one, or start with plain file writes and add the event log later? Recommend: plain writes first, event-log in Phase 4+.
- **Open:** distribution of the launcher (nix-built static host binary vs shipped script).
- **Resolved:** push channel → **polling**, not SSE/WS (see §7). Revisit only if live collaboration or token streaming is added.

## 12. What gets deleted

Hand-rolled JSON-RPC (`rpc.rs`, `registry.rs`, `server.rs`, `session.rs`, `transport.rs`), the axum static server + rust-embed wiring (`commands/ide.rs`), all ratatui (`src/tui/*` + per-command FSMs), and the regex parser (`src/inspect/parse.rs`). Roughly the majority of the current ~26k Rust LOC, replaced by framework capabilities + a few hundred LOC launcher.

---

## Appendix: current-state facts

- **Rust `neo`**: ~25.9k LOC / 79 files. ~80% orchestration; genuine Haskell parsing ≈ `src/inspect/` (~2.7k, `parse.rs` 1,757). Parser is deliberately "dumb" (resolves uncertainty to empty).
- **CLI**: `new build run test lock ide inspect validate skills`; global `--ci`, `--verbose`; `OutputMode` enum splits CI/interactive.
- **IDE**: axum + hand-rolled JSON-RPC 2.0 over one WS at `/ws`; rust-embed SPA; source watcher → `sync_event_model` → broadcast `$/eventModelChanged`.
- **NeoHaskell**: `nhcore` prelude (`Task`, `Array`, `Text`, `Map`, `|>`); `SimpleEventStore` (in-memory + optional JSONL load-on-boot); `Application` builder; `Command`/`Decider`/`Entity`/`Query`; transports Web/Cli/Mcp; `nhintegrations` OpenRouter LLM + Agent. No websockets, no ghc-lib/haskell-src-exts yet. WebTransport router is a closed `case` on `pathInfo` ending in 404 (`Web.hs:555`, `:1026`); SwaggerUI is the static-content precedent (~`:1000`).
- **AST libs**: `ghc-lib-parser` (+`-ex`) for syntax (version-decoupled); raw `ghc` API / `.hie` for typed checks. Monorepo pins one GHC → raw API viable for the typed pass.
