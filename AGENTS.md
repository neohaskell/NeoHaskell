# NeoHaskell — agent guide

<!-- Every claim in this file was verified against the repo on 2026-07-07.
     Governing rule: no agent-visible document without a CI check or a generation source.
     Localization assets are being rebuilt in codemap/ (see docs/plans/2026-07-07-continuous-generation-pipeline-plan.md, tracker #715).
     Old guidance docs live in docs/archive/2026-07-ai-artifacts/ — ARCHIVAL, do not use. -->

Newcomer-friendly Haskell dialect. Monorepo: core library (`core/` → `nhcore`), reference app + acceptance tests (`testbed/` → `nhtestbed`), outbound integrations (`integrations/` → `nhintegrations`), Rust installer (`installer/` → `neo-install`), VSCode extension (`ide/`), Astro website (`website/`). Architecture: event-sourcing + CQRS in `core/service/`. (LSP package removed 2026-07-08 — unused.)

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
./dev doctest                   # doctest gate (CI: test.yml doctest job)
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
- Pipeline telemetry: `scripts/telemetry.py` (schema: `telemetry/SCHEMA.md`, frozen v2). Every pipeline run emits one line to `telemetry/runs.jsonl`. Telemetry is pipeline-only: never emit lines for ad-hoc runs.
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

## API knowledge (Phase 4) — transcribe, never recall

Training-data APIs don't exist here. Resolve symbols at **plan time** into a `uses:` list; execution transcribes:
- `codemap/api-hot.md` — frequency-ranked card (what this repo actually calls, with doctest examples; cut modules listed in the card trailer)
- `./dev api "Text -> Maybe Uuid"` — hoogle type search: NeoHaskell surface ranked top; vanilla (dependency closure + boot libs) below with disclaimer + escape-hatch guidance whenever it has results (omitted when empty; exit 3 = vanilla-only). Query in dialect types (per the style table above) to hit the surface directly
- `codemap/phrasebook.md` — doctest-verified usage patterns (gate: test.yml `doctest` job; thin coverage = the doc backlog, ratcheted via `undocumented_doctest_modules`)
- GHC "not in scope" in `./dev check` output = invented API — resolve via `./dev api`; pipeline runs record per-stage `invented_api_events` (telemetry schema v2)

## Change flow (Phase 5) — spec-gated, two human touchpoints

Any request that should end in a PR runs the `neohaskell-pipeline` skill. The shape (ADR-0067):

- **Spec first**: `docs/changes/NNN-slug.md` from `TEMPLATE.md` — promised API diff (signatures vocabulary), `touches:` capability IDs, criteria C1..Cn each naming its proving test + level (`unit|integration|acceptance`). Bugs: C1 = the failing repro, committed red. Validate: `./dev spec-check` (CI: checks.yml `spec` job).
- **Gate 1 = draft PR** (spec only; heavy CI skipped on drafts). Continue signal = maintainer `@claude` comment (claude.yml ignores non-maintainers). Record it: `./dev pipeline approve spec --by <who>` — advancing without it is refused.
- **Resume contract**: `.pipeline/state.json` via `./dev pipeline` (init/status/advance/set/approve/park/resume/validate). Resume never re-plans; plan wrong → park (`wrong-localization`) + fix the asset.
- **Risk-tiered design reviews** (post-approval, pre-implementation): `./dev spec-check --plan <spec>` routes to `neohaskell-security-design-review` / `neohaskell-performance-design-review` when `touches:` hits risk-tagged capabilities; review records (`NNN-slug.<kind>-review.md`) are committed next to the spec, and their presence is gated at PR-ready by `./dev spec-check --reviews-pr`.
- **Verification order**: criteria tests red → implement → green at declared levels → test-impact suites (from `--plan`) → `./dev lint` + `./dev spec-drift <spec>` → full suite once at PR-ready.
- **Failure policy**: per-stage time-boxes (skill has the table) → retry once → escalate tier → `./dev pipeline park --label <taxonomy>` + structured report. A parked report beats a wrong PR. Closing a failed/parked run records a class-fix — `telemetry.py finish … --asset-delta <type>:<dest>` (enforced; `none:<reason>` if none), per [ADR-0068](docs/decisions/0068-failure-asset-delta-and-learning-loop.md).
- **Expectation guard** (`.claude/hooks/expectation-guard.py`): removing/rewording an existing test expectation is blocked twice — locally by the hook (maintainer marker `.pipeline/allow-expectation-edits`) and in CI by the `expectations` census job (maintainer `expectations-approved` PR label, which the agent can't self-apply). Adding tests never needs either.
- **Benchmarks**: nightly only (`./dev bench` vs `telemetry/bench-budgets.json`, nightly-bench.yml) — never PR-blocking.

## Release tail + learning loop (Phase 6) — [ADR-0068](docs/decisions/0068-failure-asset-delta-and-learning-loop.md)

- **Definition of done**: criteria green at their declared levels (`./dev spec-check --criteria <spec>` emits them; the tier lint rejects an `acceptance` criterion with no `.hurl` test) + `./dev testbed` acceptance green + spec-drift trivial. Post-merge, `dod.yml` flags a `Test`-suite failure on `main` as a **revert-candidate** (notify-only, never auto-reverts).
- **Kill switch**: a maintainer comments `/revert` on a merged PR → `revert.yml` (OWNER/MEMBER-gated) runs `./dev revert <sha>` to open a revert PR. Never merges it.
- **Changelog**: generated from specs — `./dev changelog` (breaking = a removed signature line ⇒ mandatory migration note); `--check` gates it at PR-ready. Never hand-write `CHANGELOG.md`.
- **Learning loop**: closing a failed/parked run records a class-fix (`telemetry.py finish --asset-delta`, enforced); log consulted aids (`telemetry.py consult`); the weekly `./dev retrospect` digest + `neohaskell-retrospective-miner` skill turn recurring friction into ≤5 contract-validated recommendations (`telemetry/recommendations.jsonl`). **Activation** (first real weekly review, first miner report, archive sunset) waits on real runs accumulating.

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
