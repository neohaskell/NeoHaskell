# NeoHaskell — agent guide

<!-- Governing rule: no agent-visible document without a CI check or a generation
     source. `./dev doctor` validates registered `./dev` verbs named here. -->

Newcomer-friendly Haskell dialect. Monorepo: core library (`core/` → `nhcore`), reference app + acceptance tests (`testbed/` → `nhtestbed`), outbound integrations (`integrations/` → `nhintegrations`), Rust installer (`installer/` → `neo-install`), Rust Neo CLI (`neo/` → the `neo` binary), Astro website (`website/`). Architecture: event-sourcing + CQRS in `core/service/`. (LSP package removed 2026-07-08 — unused.)

This file is the **agent-specific contract**: the hard rule, dialect style, and the pipeline/verification gates. Two companion guides own the rest — do not duplicate them here:

- **`README.md`** — environment setup, the full build/test commands, Postgres, human contributor workflow.
- **`codemap/README.md`** — localization routing, API discovery (`codemap/api-hot.md`, `./dev api`, `phrasebook.md`), and codemap regeneration.

Pi is the primary agent harness. Canonical project skills live in `.pi/skills/`;
`.claude/skills` is only a compatibility symlink to that single source.

## Rust `neo/**` — separate contract (do not apply Haskell rules here)

`neo/**` is the imported Rust Neo CLI, not NeoHaskell dialect code. Everything below in this file — the HARD RULE, the mandatory dialect style table, the spec-gated pipeline, dialect enforcement, and codemap localization — governs the Haskell trees (`core/`, `testbed/`, `integrations/`) and **does not apply under `neo/**`**. Route all `neo/**` work through **`neo/AGENTS.md`** and its skills: **`neo-cli-localizer`** (locate command/subsystem), **`neo-cli-implementer`** (Rust conventions + error/output contract + interactive-vs-CI), **`neo-cli-testing`** (unit/integration/e2e/smoke layers), **`neo-cli-ide`** (Vite frontend + embedded `dist/` sync + screen critique). Monorepo governance still binds `neo/**`: create each branch from its owning lower GitHub stack layer with `gh stack`, never edit `main` directly, keep ADRs and one-source-of-truth docs, ship with tests, and preserve the starter↔upstream cross-component gate. `./dev neo-skills-check` (run by `./dev doctor` + CI) keeps this routing honest.

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
| Named helpers with one abstraction level; orchestration reads top-to-bottom | nested control-flow pyramids, orchestration mixed with branch mechanics |
| Comments that explain constraints or intent | comments narrating what code does; extract a named function instead |
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

Any request that should end in a PR runs the `neohaskell-pipeline` skill
(ADR-0067, restored by ADR-0076). The pipeline-bootstrap PRs that build or
restore this gate are the one exemption; the spec gate applies to every
subsequent change request.

- **Spec first**: `docs/changes/NNN-slug.md` from `TEMPLATE.md` — promised API diff (signatures vocabulary), `touches:` capability IDs, and criteria C1..Cn using only typed proving-test locators plus level/boundary. Bugs: C1 = the failing repro, committed red; Gate 1 requires `./dev red-evidence` plus `./dev pipeline red-evidence` receipt verification. Validate: `./dev spec-check` (CI: checks.yml `spec` job).
- **Gate 1 = draft PR** (spec only; heavy CI skipped on drafts). A maintainer's explicit signal in a trusted channel authorizes continuation; record it with `./dev pipeline approve spec --by <who> --via <channel>`. The local record in `.pipeline/state.json` is the machine-enforced gate, and advancing without it is refused.
- **Resume contract**: `.pipeline/state.json` via `./dev pipeline` (init/status/advance/set/approve/park/resume/complete/validate). Resume never re-plans; plan wrong → park (`wrong-localization`) + fix the asset. `complete --outcome ok|parked|failed` archives terminal state and releases the next run (`ok` requires `ci`; `parked` requires a parked run).
- **Risk-tiered design reviews** (post-approval, pre-implementation): `./dev spec-check --plan <spec>` routes to `neohaskell-security-design-review` / `neohaskell-performance-design-review` when `touches:` hits risk-tagged capabilities. **Perf** records (`NNN-slug.perf-review.md`) are committed next to the spec and gated at PR-ready by `./dev spec-check --reviews-pr`. **Security** records (`NNN-slug.security-review.md`) are **local-only — gitignored, never pushed** (a security review maps attack surface; [ADR-0069](docs/decisions/0069-security-reviews-are-local.md)); the pipeline enforces their local presence via `./dev spec-check --reviews-local` before flipping the PR to ready.
- **Verification order**: criteria tests red → implement → green at declared levels → test-impact suites (from `--plan`) → `./dev lint` + `./dev spec-drift <spec>` → full suite once at PR-ready with `./dev test-all --require-all` (missing PostgreSQL/Hurl is red, never skipped green).
- **Gate 2 = final substantive review**: record it with `./dev pipeline approve ci --by <who> --via github-review --head "$(git rev-parse HEAD)"` before `telemetry finish --outcome ok`. Completion requires that approved HEAD to be the parent of one generated `telemetry/runs.jsonl`-only commit; exact-HEAD completion and every other delta are rejected.
- **Failure policy**: per-stage time-boxes (skill has the table) → retry once → escalate tier → `./dev pipeline park --label <taxonomy>` + structured report. A parked report beats a wrong PR. Closing a failed/parked run records a class-fix — `./dev telemetry finish … --asset-delta <type>:<dest>` (enforced; `none:<reason>` if none), per [ADR-0068](docs/decisions/0068-failure-asset-delta-and-learning-loop.md).
- **Expectation guard** (`.claude/hooks/expectation-guard.py`): removing/rewording an existing test expectation is blocked by the configured Claude hook (maintainer marker `.claude/allow-expectation-edits`) and by the CI `expectations` census (maintainer `expectations-approved` PR label, which the agent can't self-apply). Pi does not install Claude hooks: run `python3 .claude/hooks/expectation-guard.py --pr-diff <base-ref>` before continuing. Adding tests never needs an override.
- **Benchmarks**: nightly only (`./dev bench` vs `telemetry/bench-budgets.json`, nightly-bench.yml) — never PR-blocking.

## Release tail + learning loop (Phase 6) — [ADR-0068](docs/decisions/0068-failure-asset-delta-and-learning-loop.md)

- **Definition of done** (three gates, all at spec/PR-ready): the **tier lint** binds each criterion's level to an exact attested boundary; `./dev spec-check --criteria-tests` proves every locator resolves and `./dev spec-check --criteria-runtime` proves integration selectors execute their registered real fixtures; together they prove every criterion's named test **exists** (a real `.hurl` or `*.hs` spec module); and `./dev test-all --require-all` + `./dev testbed` go green with spec-drift trivial. Post-merge, `post-merge-guard.yml` flags a `Test`/`Test macOS` failure on `main` as a **revert-candidate** (notify-only).
- **Kill switch**: a maintainer comments `/revert` on a merged PR → `revert.yml` (OWNER/MEMBER-gated) runs `./dev revert <sha>` to open a revert PR. Never merges it.
- **Dependency PRs** ([ADR-0074](docs/decisions/0074-dependabot-auto-merge.md)): `dependabot-auto-merge.yml` enables GitHub's native auto-merge on Dependabot **patch/minor** PRs — GitHub holds them until every *required* check is green, so the workflow never judges CI itself. **Majors** (and any group containing one) are labelled `dependency-major` and never auto-merge; `dependabot-major-review.yml` (a `workflow_run` on the above — base-repo context is the only place Actions secrets exist for a Dependabot PR) posts Claude's breaking-change/migration analysis. That file always runs from the **default branch**, so it cannot be tested from a PR. Corollary: a CI gate that is not a required check is decoration — add it to branch protection.
- **Changelog**: generated from specs — `./dev changelog` (breaking = a removed signature line ⇒ mandatory migration note); `--check` gates it at PR-ready. Never hand-write `CHANGELOG.md`.
- **Learning loop**: closing a failed/parked run records a class-fix (`./dev telemetry finish --asset-delta`, enforced); an `ok` run that ships a class-fix records it via `--improvement <type>:<dest>` (optional). The deterministic weekly `./dev retrospect` digest (automated by `retrospect.yml`, schedule + dispatch) plus the `neohaskell-retrospective-miner` skill turn recurring friction into ≤5 contract-validated recommendations. **Activation** waits on real runs accumulating.

## Dialect enforcement (Phase 2, live since 2026-07-07)

Portable enforcement has two gates:
1. **`./dev lint`** (seconds; portable + CI gate in `checks.yml`): dialect-first `.hlint.yaml` plus a PR-diff syntax ratchet — vanilla modules are restricted to Core wrappers, and added `case … of True/False` is rejected in every harness.
2. **GHC** (`./dev check`): `NoImplicitPrelude`.

Claude compatibility adds optional earlier feedback through `.claude/hooks/dialect-guard.py` (~50ms). Pi does not depend on that hook. It rejects `$`, `where`-as-let-substitute (declaration `where` — module/class/instance/data/GADT/type-family — is fine), `Either`, `pure`/`return`, vanilla/unqualified imports, and `case`-of-Bool on added lines. False positive? Add `-- HOOK-ALLOW: <reason>` on that line. Adding/changing rules routes to `neohaskell-dialect-rules`.

**Escape hatch:** no Core wrapper for what you need? Add your module to the `.hlint.yaml` `within:` list with a justification + `belongs-in:` note. Rule of three: third exception for a symbol = promote a Core primitive. Never reimplement a banned thing with allowed vocabulary.

## Non-negotiable

- Every change ships with tests (happy path + error + boundary); bug fixes include regression tests.
- Never modify existing test expectations without maintainer approval.
- Branch off `main`; never edit `main` directly (check the branch before editing; the configured Claude hook also enforces it).
- ADRs live in `docs/decisions/NNNN-slug.md`.

## Project brain

Boot from `docs/`: `docs/charter.md` (mission, horizon, no-goals), `docs/decisions/` (ADRs). The charter governs priority disputes.
