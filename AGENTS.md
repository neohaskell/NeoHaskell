# NeoHaskell — agent guide

<!-- Governing rule: no agent-visible document without a CI check or a generation
     source. `./dev doctor` validates registered `./dev` verbs named here. -->

Newcomer-friendly Haskell dialect. Monorepo: core library (`core/` → `nhcore`), reference app + acceptance tests (`testbed/` → `nhtestbed`), outbound integrations (`integrations/` → `nhintegrations`), Rust installer (`installer/` → `neo-install`), Astro website (`website/`). Architecture: event-sourcing + CQRS in `core/service/`. (LSP package removed 2026-07-08 — unused.)

This file is the **agent-specific contract**: the hard rule, dialect style, and the pipeline/verification gates. Two companion guides own the rest — do not duplicate them here:

- **`README.md`** — environment setup, the full build/test commands, Postgres, human contributor workflow.
- **`codemap/README.md`** — localization routing, API discovery (`codemap/api-hot.md`, `./dev api`, `phrasebook.md`), and codemap regeneration.

## HARD RULE

Under no circumstance you will begin exploring the codebase. Exploring the codebase without first exhausting the guiding principles and tools here (and in `codemap/README.md`) will result in immediate interruption and termination.

The only exception to this rule is if you COMPLETELY EXHAUST all the resources here and still haven't found what you were looking for. In that case, you will have to take a note to mention it in step 6 of the pipeline.

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

## Commands (pointers, not a duplicate)

- Repair loop: **`./dev watch`** once per session, then edit → wait ~2s → **`./dev check`**. Never spawn `cabal build` inside the loop. Full menu: run `./dev` with no args. Measured baselines + full build/test/Postgres commands: **README.md**.
- Localization + API discovery + codemap regeneration: **`codemap/README.md`** (use the `neohaskell-localizer` skill at plan time). Never explore the tree to find where things live. Training-data APIs don't exist here; GHC "not in scope" in `./dev check` = an invented API — resolve via `./dev api`.
- Implementing any `.hs` change? Use the `neohaskell-implementer` skill (copy-adapt discipline + repair protocol).

## Change flow (Phase 5) — spec-gated, two human touchpoints

Any request that should end in a PR runs the `neohaskell-pipeline` skill (ADR-0067).
(The pipeline-bootstrap PRs — which *build* this gate — are the one exemption; the spec gate applies to every subsequent change request.)

- **Spec first**: `docs/changes/NNN-slug.md` from `TEMPLATE.md` — promised API diff (signatures vocabulary), `touches:` capability IDs, criteria C1..Cn each naming its proving test + level (`unit|integration|acceptance`). Bugs: C1 = the failing repro, committed red. Validate: `./dev spec-check` (CI: checks.yml `spec` job).
- **Gate 1 = draft PR** (spec only; heavy CI skipped on drafts). Continue signal = maintainer `@claude` comment (claude.yml ignores non-maintainers). Record it: `./dev pipeline approve spec --by <who>` — advancing without it is refused.
- **Resume contract**: `.pipeline/state.json` via `./dev pipeline` (init/status/advance/set/approve/park/resume/validate). Resume never re-plans; plan wrong → park (`wrong-localization`) + fix the asset.
- **Risk-tiered design reviews** (post-approval, pre-implementation): `./dev spec-check --plan <spec>` routes to `neohaskell-security-design-review` / `neohaskell-performance-design-review` when `touches:` hits risk-tagged capabilities. **Perf** records (`NNN-slug.perf-review.md`) are committed next to the spec and gated at PR-ready by `./dev spec-check --reviews-pr`. **Security** records (`NNN-slug.security-review.md`) are **local-only — gitignored, never pushed** (a security review maps attack surface; [ADR-0069](docs/decisions/0069-security-reviews-are-local.md)); the pipeline enforces their local presence via `./dev spec-check --reviews-local` before flipping the PR to ready.
- **Verification order**: criteria tests red → implement → green at declared levels → test-impact suites (from `--plan`) → `./dev lint` + `./dev spec-drift <spec>` → full suite once at PR-ready.
- **Failure policy**: per-stage time-boxes (skill has the table) → retry once → escalate tier → `./dev pipeline park --label <taxonomy>` + structured report. A parked report beats a wrong PR. Closing a failed/parked run records a class-fix — `./dev telemetry finish … --asset-delta <type>:<dest>` (enforced; `none:<reason>` if none), per [ADR-0068](docs/decisions/0068-failure-asset-delta-and-learning-loop.md).
- **Expectation guard** (`.claude/hooks/expectation-guard.py`): removing/rewording an existing test expectation is blocked twice — locally by the hook (maintainer marker `.pipeline/allow-expectation-edits`) and in CI by the `expectations` census job (maintainer `expectations-approved` PR label, which the agent can't self-apply). Adding tests never needs either.
- **Benchmarks**: nightly only (`./dev bench` vs `telemetry/bench-budgets.json`, nightly-bench.yml) — never PR-blocking.

## Release tail + learning loop (Phase 6) — [ADR-0068](docs/decisions/0068-failure-asset-delta-and-learning-loop.md)

- **Definition of done** (three gates, all at spec/PR-ready): the **tier lint** binds each criterion's level to its test shape (`acceptance` ⇒ names a `.hurl`); `./dev spec-check --criteria-tests` proves every criterion's named test **exists** (a real `.hurl` or `*.hs` spec module); and the whole `Test` suite (all levels incl. the acceptance `test-hurl` job) + `./dev testbed` go green with spec-drift trivial. Post-merge, `dod.yml` flags a `Test`/`Test macOS` failure on `main` as a **revert-candidate** (notify-only).
- **Kill switch**: a maintainer comments `/revert` on a merged PR → `revert.yml` (OWNER/MEMBER-gated) runs `./dev revert <sha>` to open a revert PR. Never merges it.
- **Changelog**: generated from specs — `./dev changelog` (breaking = a removed signature line ⇒ mandatory migration note); `--check` gates it at PR-ready. Never hand-write `CHANGELOG.md`.
- **Learning loop**: closing a failed/parked run records a class-fix (`./dev telemetry finish --asset-delta`, enforced); an `ok` run that ships a class-fix records it via `--improvement <type>:<dest>` (optional). The deterministic weekly `./dev retrospect` digest (automated by `retrospect.yml`, schedule + dispatch) plus the `neohaskell-retrospective-miner` skill turn recurring friction into ≤5 contract-validated recommendations. **Activation** waits on real runs accumulating.

## Dialect enforcement (Phase 2, live since 2026-07-07)

Three layers, in feedback order:
1. **Edit hook** (`.claude/hooks/dialect-guard.py`, ~50ms): rejects `$`, `where`-as-let-substitute (declaration `where` — module/class/instance/data/GADT/type-family — is fine), `Either`, `pure`/`return`, vanilla/unqualified imports, `case`-of-Bool — on added lines, quoting the rule. False positive? `-- HOOK-ALLOW: <reason>` on the line. Adding/changing rules → `neohaskell-dialect-rules` skill.
2. **`./dev lint`** (seconds; CI gate in `checks.yml`): dialect-first `.hlint.yaml` — vanilla modules restricted to Core wrappers + grandfathered boundaries.
3. **GHC** (`./dev check`): `NoImplicitPrelude`.

**Escape hatch:** no Core wrapper for what you need? Add your module to the `.hlint.yaml` `within:` list with a justification + `belongs-in:` note. Rule of three: third exception for a symbol = promote a Core primitive. Never reimplement a banned thing with allowed vocabulary.

## Non-negotiable

- Every change ships with tests (happy path + error + boundary); bug fixes include regression tests.
- Never modify existing test expectations without maintainer approval.
- Branch off `main`; never edit `main` directly (hook-enforced).
- ADRs live in `docs/decisions/NNNN-slug.md`.
