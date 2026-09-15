# NeoHaskell — agent guide

<!-- Governing rule: no agent-visible document without a CI check or a generation
     source. `./dev doctor` validates registered `./dev` verbs named here. -->

Newcomer-friendly Haskell dialect. Monorepo: core library (`core/` → `nhcore`), reference app + acceptance tests (`testbed/` → `nhtestbed`), outbound integrations (`integrations/` → `nhintegrations`), Rust installer (`installer/` → `neo-install`), Rust Neo CLI (`neo/` → the `neo` binary), Astro website (`website/`). Architecture: event-sourcing + CQRS in `core/service/`. (LSP package removed 2026-07-08 — unused.)

This file is the **agent-specific contract**: the hard rule, dialect style, and the pipeline/verification gates. Two companion guides own the rest — do not duplicate them here:

- **`README.md`** — environment setup, the full build/test commands, Postgres, human contributor workflow.
- **`codemap/README.md`** — localization routing, API discovery (`codemap/api-hot.md`, `./dev api`, `phrasebook.md`), and codemap regeneration.

Codex is the primary agent harness. Canonical project skills live in `.agents/skills/`.
Codex loads this `AGENTS.md` and nested `AGENTS.md` contracts. Run the portable
`./dev` checks explicitly. Native hooks in `.codex/hooks.json` adapt Codex patch
events to shared guards, format Haskell edits, remind about tests, and warm Postgres.
Review and trust these project hooks through `/hooks` before relying on them;
portable checks remain required when a host does not load hooks.

### Consolidated Codex configuration

- Scoped instructions: `core/AGENTS.md`, `core/service/AGENTS.md`,
  `integrations/AGENTS.md`, `testbed/AGENTS.md`, `neo/AGENTS.md`, and
  `neo/assets/ide/AGENTS.md`. Scoped routing points back to the shared contract.
- Skills: `.agents/skills/` is the single source; no compatibility copies.
- Pull requests: use `neohaskell-pr` for naming, descriptions, and GitHub stack
  management. Lead PR descriptions with outcomes Jess understands; all release
  notes and migration instructions must pass its Jess comprehension review.
- Runtime: `.codex/config.toml` supplies sandbox defaults; `.codex/hooks.json`
  delegates to `scripts/guards/codex-hooks.py` and the shared dialect/expectation
  guards. Shell edits and other tool paths still require portable verification.
- Provisioning: `scripts/cloud-setup.sh` provisions cloud environments; local
  `./dev` commands select the pinned toolchain and `./dev watch` warms typechecking.
- Migration coverage: `./dev agent-config-check` verifies every former instruction
  entrypoint has a Codex destination and checks local Codex hooks.
  `./dev doctor` runs this check and its negative fixtures.

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
- **Expectation guard** (`scripts/guards/expectation-guard.py`): run `python3 scripts/guards/expectation-guard.py --pr-diff <base-ref>` before continuing. CI enforces the same census with the maintainer-only `expectations-approved` label. Codex patch hooks and direct edit-payload checks use the maintainer-created `.agents/allow-expectation-edits` marker. Adding tests never needs an override.
- **Benchmarks**: nightly only (`./dev bench` vs `telemetry/bench-budgets.json`, nightly-bench.yml) — never PR-blocking.

## Release tail + learning loop (Phase 6) — [ADR-0068](docs/decisions/0068-failure-asset-delta-and-learning-loop.md)

- **Definition of done** (three gates, all at spec/PR-ready): the **tier lint** binds each criterion's level to an exact attested boundary; `./dev spec-check --criteria-tests` proves every locator resolves and `./dev spec-check --criteria-runtime` proves integration selectors execute their registered real fixtures; together they prove every criterion's named test **exists** (a real `.hurl` or `*.hs` spec module); and `./dev test-all --require-all` + `./dev testbed` go green with spec-drift trivial. Post-merge, `post-merge-guard.yml` flags a `Test`/`Test macOS` failure on `main` as a **revert-candidate** (notify-only).
- **Kill switch**: a maintainer comments `/revert` on a merged PR → `revert.yml` (OWNER/MEMBER-gated) runs `./dev revert <sha>` to open a revert PR. Never merges it.
- **Dependency PRs** ([ADR-0074](docs/decisions/0074-dependabot-auto-merge.md)): `dependabot-auto-merge.yml` enables GitHub's native auto-merge on Dependabot **patch/minor** PRs — GitHub holds them until every *required* check is green, so the workflow never judges CI itself. **Majors** (and any group containing one) are labelled `dependency-major` and never auto-merge; maintainers review their breaking changes and migration steps manually. Corollary: a CI gate that is not a required check is decoration — add it to branch protection.
- **Releases**: use `.agents/skills/neohaskell-release/SKILL.md` locally before final review. Write reviewed `.changes/` fragments, with Jess-readable migration steps and an agent prompt for breaking changes. `./dev changelog --check` gates fragments. Main alone generates versions and permanent changelogs through verified release PRs; never hand-write `CHANGELOG.md`. Installation stays inactive until a verified manual first release. `./dev semantic-release --self-test` proves the release contracts.
- **Learning loop**: closing a failed/parked run records a class-fix (`./dev telemetry finish --asset-delta`, enforced); an `ok` run that ships a class-fix records it via `--improvement <type>:<dest>` (optional). The deterministic weekly `./dev retrospect` digest (automated by `retrospect.yml`, schedule + dispatch) plus the `neohaskell-retrospective-miner` skill turn recurring friction into ≤5 contract-validated recommendations. **Activation** waits on real runs accumulating.

## Dialect enforcement (Phase 2, live since 2026-07-07)

Portable enforcement has two gates:
1. **`./dev lint`** (seconds; portable + CI gate in `checks.yml`): dialect-first `.hlint.yaml` plus a PR-diff syntax ratchet — vanilla modules are restricted to Core wrappers, and added `case … of True/False` is rejected in every harness.
2. **GHC** (`./dev check`): `NoImplicitPrelude`.

The portable fragment checker `scripts/guards/dialect-guard.py` accepts JSON on stdin for direct diagnostics; `./dev lint` runs its PR-diff syntax ratchet. It rejects `$`, `where`-as-let-substitute (declaration `where` — module/class/instance/data/GADT/type-family — is fine), `Either`, `pure`/`return`, vanilla/unqualified imports, and `case`-of-Bool on added lines. False positive? Add `-- HOOK-ALLOW: <reason>` on that line. Adding/changing rules routes to `neohaskell-dialect-rules`.

**Escape hatch:** no Core wrapper for what you need? Add your module to the `.hlint.yaml` `within:` list with a justification + `belongs-in:` note. Rule of three: third exception for a symbol = promote a Core primitive. Never reimplement a banned thing with allowed vocabulary.

## Non-negotiable

- Every change ships with tests (happy path + error + boundary); bug fixes include regression tests.
- Never modify existing test expectations without maintainer approval.
- Branch off `main`; never edit `main` directly (check the branch before editing).
- ADRs live in `docs/decisions/NNNN-slug.md`.

## Project brain

Boot from `docs/`: `docs/charter.md` (mission, horizon, no-goals), `docs/decisions/` (ADRs). The charter governs priority disputes.
