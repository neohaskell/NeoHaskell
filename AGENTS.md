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

## Localization (Phase 3, live since 2026-07-08) — lookup, not search

Route requests via `codemap/` (CI-gated capability ontology + extension points) — use the `neohaskell-localizer` skill at plan time. Never explore the tree to find where things live:

- Existing code → `codemap/capabilities.yaml` (closed ID list; aliases bridge intent vocabulary)
- New code → `codemap/extension-points.yaml` (create/register/tests per kind)
- API discovery → grep `codemap/signatures/*.txt` (generated API surface; never open source files for this)
- Blast radius → `./dev who-calls <symbol> [module]` (capability-grouped; `./dev hiedb` builds the index)
- Validity → `./dev codemap-check` (CI-gated: ownership exactly-once, alias uniqueness, doc-ratchet)

## Dialect enforcement (Phase 2, live since 2026-07-07)

Three layers, in feedback order:
1. **Edit hook** (`.claude/hooks/dialect-guard.py`, ~50ms): rejects `$`, `where`-as-let-substitute (declaration `where` — module/class/instance/data/GADT/type-family — is fine), `Either`, `pure`/`return`, vanilla/unqualified imports, `case`-of-Bool — on added lines, quoting the rule. False positive? `-- HOOK-ALLOW: <reason>` on the line. Adding/changing rules → `neohaskell-dialect-rules` skill (case coverage is CI-enforced via `./dev doctor`).
2. **`./dev lint`** (seconds; CI gate in `checks.yml`): dialect-first `.hlint.yaml` — vanilla modules restricted to Core wrappers + grandfathered boundaries (generated 2026-07-07).
3. **GHC** (`./dev check`): `NoImplicitPrelude`.

**Escape hatch:** no Core wrapper for what you need? Add your module to the `.hlint.yaml` `within:` list with a justification + `belongs-in:` note. Rule of three: third exception for a symbol = promote a Core primitive. Never reimplement a banned thing with allowed vocabulary.

Implementing any `.hs` change? Use the `neohaskell-implementer` skill (copy-adapt discipline + repair protocol).

## Non-negotiable

- Every change ships with tests (happy path + error + boundary); bug fixes include regression tests.
- Never modify existing test expectations without maintainer approval.
- Branch off `main`; never edit `main` directly (hook-enforced).
- ADRs live in `docs/decisions/NNNN-slug.md`.
