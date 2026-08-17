# NeoHaskell — agent guide

<!-- Governing rule: no agent-visible document without a CI check or a generation
     source. `./dev doctor` validates registered `./dev` verbs named here. -->

Newcomer-friendly Haskell dialect. Monorepo: core library (`core/` → `nhcore`), reference app + acceptance tests (`testbed/` → `nhtestbed`), outbound integrations (`integrations/` → `nhintegrations`), Rust installer (`installer/` → `neo-install`), Rust Neo CLI (`neo/` → the `neo` binary), Astro website (`website/`). Architecture: event-sourcing + CQRS in `core/service/`. (LSP package removed 2026-07-08 — unused.)

This file is the **agent-specific contract**: the hard rule, dialect style, and the pipeline/verification gates. Two companion guides own the rest — do not duplicate them here:

- **`README.md`** — environment setup, the full build/test commands, Postgres, human contributor workflow.
- **`codemap/README.md`** — localization routing, API discovery (`codemap/api-hot.md`, `./dev api`, `phrasebook.md`), and codemap regeneration.

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

## Work intake

New work enters through the **bd issue queue**, not by an agent self-triggering
a skill. Run `bd ready` to see what's claimable, `bd show <id>` for details.
`bd prime` has the full workflow. The dispatcher pours the compound
**`change`** formula (`.beads/formulas/change.formula.toml`, one molecule,
intake through merge, human/gh:run/gh:pr gates in-formula) for whatever is
claimed; `release-publish` is the one other formula, kept separate since it
aggregates across changes on demand rather than running per change. See
`docs/processes/neohaskell-change.md` and
`docs/processes/neohaskell-agents.md` for the process and role roster this
implements.

Dolt remote sync is NOT enabled for this repo: never run `bd dolt push` or
push `refs/dolt/data`; issue data syncs only via the dispatcher host.

## Change flow (Phase 5) — spec-gated, two human touchpoints [DEPRECATED — moved to docs/legacy/neohaskell-pipeline/, rollback-only — see "Work intake" above]

Any request that should end in a PR runs the `neohaskell-pipeline` skill (ADR-0067) — **now at `docs/legacy/neohaskell-pipeline/SKILL.md`**.
(The pipeline-bootstrap PRs — which *build* this gate — are the one exemption; the spec gate applies to every subsequent change request.)

- **Spec first**: `docs/changes/NNN-slug.md` from `TEMPLATE.md` — promised API diff (signatures vocabulary), `touches:` capability IDs, criteria C1..Cn each naming its proving test + level (`unit|integration|acceptance`). Bugs: C1 = the failing repro, committed red. Validate: `./dev spec-check` (CI: checks.yml `spec` job).
- **Gate 1 = draft PR** (spec only; heavy CI skipped on drafts). Continue signal = maintainer `@claude` comment (claude.yml ignores non-maintainers). Record it: `./dev pipeline approve spec --by <who>` — advancing without it is refused.
- **Resume contract**: `.pipeline/state.json` via `./dev pipeline` (init/status/advance/set/approve/park/resume/validate). Resume never re-plans; plan wrong → park (`wrong-localization`) + fix the asset.
- **Risk-tiered design reviews** (post-approval, pre-implementation): `./dev spec-check --plan <spec>` routes to `neohaskell-security-design-review` / `neohaskell-performance-design-review` when `touches:` hits risk-tagged capabilities. **Perf** records (`NNN-slug.perf-review.md`) are committed next to the spec and gated at PR-ready by `./dev spec-check --reviews-pr`. **Security** records (`NNN-slug.security-review.md`) are **local-only — gitignored, never pushed** (a security review maps attack surface; [ADR-0069](docs/decisions/0069-security-reviews-are-local.md)); the pipeline enforces their local presence via `./dev spec-check --reviews-local` before flipping the PR to ready.
- **Verification order**: criteria tests red → implement → green at declared levels → test-impact suites (from `--plan`) → `./dev lint` + `./dev spec-drift <spec>` → full suite once at PR-ready.
- **Failure policy**: per-stage time-boxes (skill has the table) → retry once → escalate tier → `./dev pipeline park --label <taxonomy>` + structured report. A parked report beats a wrong PR. Closing a failed/parked run records a class-fix — `./dev telemetry finish … --asset-delta <type>:<dest>` (enforced; `none:<reason>` if none), per [ADR-0068](docs/decisions/0068-failure-asset-delta-and-learning-loop.md).
- **Expectation guard** (`.claude/hooks/expectation-guard.py`): removing/rewording an existing test expectation is blocked twice — locally by the hook (maintainer marker `.claude/allow-expectation-edits`) and in CI by the `expectations` census job (maintainer `expectations-approved` PR label, which the agent can't self-apply). Adding tests never needs either.
- **Benchmarks**: nightly only (`./dev bench` vs `telemetry/bench-budgets.json`, nightly-bench.yml) — never PR-blocking.

## Release tail + learning loop (Phase 6) — [ADR-0068](docs/decisions/0068-failure-asset-delta-and-learning-loop.md)

- **Definition of done** (three gates, all at spec/PR-ready): the **tier lint** binds each criterion's level to its test shape (`acceptance` ⇒ names a `.hurl`); `./dev spec-check --criteria-tests` proves every criterion's named test **exists** (a real `.hurl` or `*.hs` spec module); and the whole `Test` suite (all levels incl. the acceptance `test-hurl` job) + `./dev testbed` go green with spec-drift trivial. Post-merge, `post-merge-guard.yml` flags a `Test`/`Test macOS` failure on `main` as a **revert-candidate** (notify-only).
- **Kill switch**: a maintainer comments `/revert` on a merged PR → `revert.yml` (OWNER/MEMBER-gated) runs `./dev revert <sha>` to open a revert PR. Never merges it.
- **Dependency PRs** ([ADR-0074](docs/decisions/0074-dependabot-auto-merge.md)): `dependabot-auto-merge.yml` enables GitHub's native auto-merge on Dependabot **patch/minor** PRs — GitHub holds them until every *required* check is green, so the workflow never judges CI itself. **Majors** (and any group containing one) are labelled `dependency-major` and never auto-merge; `dependabot-major-review.yml` (a `workflow_run` on the above — base-repo context is the only place Actions secrets exist for a Dependabot PR) posts Claude's breaking-change/migration analysis. That file always runs from the **default branch**, so it cannot be tested from a PR. Corollary: a CI gate that is not a required check is decoration — add it to branch protection.
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

## Project brain

Boot from `docs/`: `docs/charter.md` (mission, horizon, no-goals), `docs/decisions/` (ADRs). The charter governs priority disputes.

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:970c3bf2 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.

## Agent Context Profiles

The managed Beads block is task-tracking guidance, not permission to override repository, user, or orchestrator instructions.

- **Conservative (default)**: Use `bd` for task tracking. Do not run git commits, git pushes, or Dolt remote sync unless explicitly asked. At handoff, report changed files, validation, and suggested next commands.
- **Minimal**: Keep tool instruction files as pointers to `bd prime`; use the same conservative git policy unless active instructions say otherwise.
- **Team-maintainer**: Only when the repository explicitly opts in, agents may close beads, run quality gates, commit, and push as part of session close. A current "do not commit" or "do not push" instruction still wins.

## Session Completion

This protocol applies when ending a Beads implementation workflow. It is subordinate to explicit user, repository, and orchestrator instructions.

1. **File issues for remaining work** - Create beads for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Handle git/sync by active profile**:
   ```bash
   # Conservative/minimal/default: report status and proposed commands; wait for approval.
   git status

   # Team-maintainer opt-in only, unless current instructions forbid it:
   git pull --rebase
   bd dolt push
   git push
   git status
   ```
5. **Hand off** - Summarize changes, validation, issue status, and any blocked sync/commit/push step

**Critical rules:**
- Explicit user or orchestrator instructions override this Beads block.
- Do not commit or push without clear authority from the active profile or the current user request.
- If a required sync or push is blocked, stop and report the exact command and error.
<!-- END BEADS INTEGRATION -->
