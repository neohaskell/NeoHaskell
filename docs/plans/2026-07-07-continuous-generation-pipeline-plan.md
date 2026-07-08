# Continuous Generation pipeline — phased build plan

**Date:** 2026-07-07
**Goal:** 4x development speed for NeoHaskell internal features/bugs: **Continuous Generation → Continuous Verification → Continuous Deployment**. Baseline: PRs take 2–3h and are wrong (#708). Target: **≤30-minute PRs that are correct.**
**Companion research:** [2026-07-07-prompt-to-plan-methodologies.md](2026-07-07-prompt-to-plan-methodologies.md)

## Decisions already made (do not relitigate)

- Codemap and all assets are **internal-only** for now; no user-facing exposure (revisit later).
- **No historical PR replay.** Telemetry is prospective — starts with the first instrumented run.
- Event-model specs are **only for user apps**. Internal NeoHaskell specs describe API/behavior contract deltas.
- Spec gate = pipeline opens a **draft PR** containing the spec; Nick approves in the PR and signals continue.
- Old AI artifacts are **archived, never deleted**.

## Success metrics

| Metric | Baseline | Target |
|---|---|---|
| PR lead time (work-time, excl. waiting-on-human) | 2–3h | ≤30m |
| PRs wrong after review | common (#708) | ~0; wrongness caught pre-review |
| Inner loop: edit → typecheck feedback | >60s | <5s |
| Inner loop: targeted test | >60s | <30s |
| Invented-API events (GHC "not in scope") per PR | unmeasured | measured, trending ↓ |
| Style violations reaching CI | unmeasured | 0 (caught at edit-hook/hlint layer) |

## Governing rules (the anti-rot constitution)

> **No agent-visible document without a CI check or a generation source.**
> Every asset in this plan is either compiler/script-generated (with a regenerate-and-diff sync check) or human-curated (with a structural validity check). Anything else gets archived.

> **Every pipeline tool is a human tool.** (Added 2026-07-07, panel-reviewed.) Same script, same environment, same build flavor for agents and humans — parity is what makes agent failures reproducible by a human in minutes. Non-interactive by default; TTY niceties never break the agent path. Telemetry records *pipeline runs only* — ad-hoc human use never emits. **Human-runnable is an acceptance criterion for every pipeline asset**, operationalized as: every asset registers a verb in `./dev`.

> **Every governing rule names its gate.** (Added 2026-07-07.) A rule without a named enforcement mechanism is a wish — rejected at plan review. Current pairings: doc-truth → `codemap check` + generated-file sync checks (Phase 3); verb registration → `./dev doctor` (CI: `checks.yml`, seconds, no toolchain); telemetry discipline → emitter validation in `scripts/telemetry.py`; dialect → hlint CI gate + edit hook (Phase 2); spec fidelity → signatures drift check (Phase 5); doc coverage → the ratchet (Phase 3).

---

## Phase 0 — Archive & truth reset

**Goal:** no agent-visible artifact makes a false claim; all old artifacts preserved in git with provenance; the vacuum never exists (replacement stub lands in the same PR).

**Inventory (scanned 2026-07-07):**

| Artifact | Kind | Disposition |
|---|---|---|
| `CLAUDE.md` (root) | knowledge | salvage → archive → replace with verified stub |
| `core/AGENTS.md`, `core/service/AGENTS.md`, `testbed/AGENTS.md`, `integrations/AGENTS.md`, `website/AGENTS.md` | knowledge | salvage → archive |
| `context/TODO.md`, `context/collections.md`, `context/documentation.md` | knowledge | salvage → archive |
| `opencode.jsonc`, `ocx.jsonc`, `ocx.lock` | tool config (retired tool) | archive whole, no salvage |
| `.claude/skills/` (14 skills; note duplicate pair `feature-pipeline-preview` / `neohaskell-feature-pipeline`) | knowledge | salvage → archive — **all 14, no quarantine-in-place**; replacements rebuilt in Phases 2–5 |
| `.pipeline/`, `.integration-pipeline/` | process state | run artifacts → move under `telemetry/archive/`, not doc archive |
| `.claude/hooks/`, `.claude/settings.json` | tool config | keep (extended in Phases 2/5) |
| `.github/workflows/claude.yml`, `claude-code-review.yml` | tool config | keep (reused in Phase 5) |
| `docs/plans/`, `docs/decisions/` | knowledge | keep — active |

**Tasks:**

1. **Salvage pass (before any move).** For each knowledge doc, extract claims into `docs/archive/2026-07-ai-artifacts/SALVAGE.md` with a verdict each: `valid` (verified against the repo/workflows today), `invalid`, `unverifiable`. Valid claims are the seed corpus for Phase 3's codemap bootstrap. Invalid claims form the rot taxonomy (informs which CI checks matter).
2. **Skill salvage ranking.** No keep-set — **all 14 skills are archived**; replacements are rebuilt from scratch in Phases 2–5. The triage's only job is salvage priority: which skills' content feeds the new artifacts first (expected high-value: `neohaskell-style-guide` → hlint rules + hook messages + stub style table; `neohaskell-implementer` → Phase 2 copy-adapt skill; feature pipeline → Phase 5 flow). Salvage verdicts recorded per skill in `SALVAGE.md` like the docs.
3. **Atomic archive PR** containing all of:
   - `git mv` archived files → `docs/archive/2026-07-ai-artifacts/` (mirroring original structure; history/blame preserved).
   - **Every archived file gets a banner prepended as its first lines:** `ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE. Superseded by codemap/ + CLAUDE.md. Scheduled for deletion once the pipeline is verified (Phase 6).` Same banner at the top of `MANIFEST.md`.
   - `MANIFEST.md`: original path, kind, reason, salvage verdict summary, where surviving content went, and the **sunset clause** (deleted after Phase 6 verification; `git mv` means git history preserves everything, so deletion loses nothing).
   - New minimal `CLAUDE.md` stub (~30 lines): style table (re-verified), build/test commands (each one executed before writing), pointer to `codemap/` (marked "coming — Phase 3"), pointer to the archive. **Every claim verified the day it's written.**
   - `codemap/` skeleton dir with a README placeholder (Phase 3's addressed home).
4. **Cross-reference check** (already verified: no skill references CLAUDE.md/AGENTS.md; re-verify for workflows and hooks before merge). Active plumbing is **not** archived: `.claude/hooks/`, `settings.json`, and the Claude workflows are functional infrastructure reused by Phases 2 and 5.

**Interim gap (accepted):** between Phase 0 and the Phase 2–5 rebuilds, agent runs operate on the verified stub alone — no skills. The stub's style table carries the essential dialect rules through the gap.

**Exit criteria:** single merged PR; no quarantined artifacts remain in agent-visible paths; every archived file carries the ARCHIVAL banner; every remaining agent-visible claim verified true; manifest complete with sunset clause; CI green.
**Size:** ~half a day. **Depends on:** nothing. **Blocks:** Phase 3 (salvage feeds bootstrap).

---

## Phase 1 — Fast inner loop + telemetry foundations

**Goal:** edit→typecheck <5s; targeted test <30s; every pipeline run measured from the very first one.

**Tasks:**

1. **Profile before optimizing** (~2h): one full and one incremental build with `-ddump-timings`; identify whether time goes to TH-splice recompilation cascades (Service layer is TH-heavy), linking, or cabal overhead. The profile decides which of the following matter most.
2. **Dev build config:** committed `cabal.project.dev` (or documented `cabal.project.local` recipe): `optimization: 0` for local packages, parallel `-j`. Never let agents build with `-O1` in the loop.
   **Worktree design (resolved):** pool + last-good snapshot restore. A **bake job** builds main with the dev flavor (`-O0`, same GHC version/flags as the loop — CI test-build artifacts are unusable, wrong flags) on every main merge or nightly, and snapshots `dist-newstyle`. Pool worktrees live at **stable absolute paths** (the cache embeds absolute paths in the package DB — restored elsewhere it silently rebuilds the world) and restore via hardlink rsync in seconds. Telemetry logs **modules-rebuilt-after-restore** as the cache-health metric; a spike means the bake went stale.
3. **Persistent typecheck loop:** `ghcid` session per worktree writing `--outputfile .ghcid-errors.txt`; `scripts/dev-loop` starts it; the agent's post-edit check = wait ~2s, read the file. No `cabal build` spawns inside the repair loop.
4. **Link-free targeted tests:** `scripts/test-match "<pattern>"` — runs hspec `--match` inside `cabal repl` (skips test-executable linking). Caveat to verify during profiling: TH modules may force object code; try `-fprefer-byte-code` (GHC 9.6+).
5. **Warm services:** `docker-compose up -d` for Postgres in the session-start hook; suites already gate on `POSTGRES_AVAILABLE`.
6. **Telemetry schema v1 (frozen before first run):** one JSON line per pipeline run appended to `telemetry/runs.jsonl` (committed):
   `{run_id, request_ref, stages: {name: {start, stop, model, repair_rounds}}, waiting_on_human_s, outcome, failure_label}`.
   Emitted by pipeline scripts automatically — never hand-written.
   **Failure-label taxonomy v1 (closed enum):** `wrong-intent | wrong-localization | dialect-violation | invented-api | test-failure | spec-drift | flaky-infra | timeout | human-rejected-spec | human-rejected-pr | other`. `other` requires a free-text note and must be re-classified (or the taxonomy extended) at the weekly review — free-text labels are forbidden because they make trends unmeasurable.
7. **Golden-task accumulation (free, forward-only):** each run archives `(request, spec, final diff, verdict, session transcript)` under `telemetry/golden/NNN/` (stage summaries instead of the full transcript when size-prohibitive). No replay of history; the eval suite builds itself from now on. The transcript capture is a prerequisite for the Phase 6 retrospective miner.

**Exit criteria:** measured typecheck <5s on a `core/core/` edit; `test-match` <30s; one dummy run emits a schema-valid telemetry line.
**Size:** 1–2 days. **Depends on:** nothing (parallel with Phase 0). **Blocks:** Phase 5 (loop + telemetry are its substrate).

---

## Phase 2 — Dialect enforcement

**Goal:** vanilla Haskell cannot survive to CI; feedback in milliseconds (hook) and seconds (hlint), not build-minutes.

**Tasks:**

1. **Invert `.hlint.yaml` to dialect-first.** Add `modules:` restrictions (Data.Text/Data.Map/Data.Char/Data.List… allowed only `within:` their Core wrapper), `functions:` restrictions (`($)`, `head/tail/fromJust/error/undefined`; `pure`/`return` with `within:` exceptions for Core Task internals), rewrite rules with teaching messages (`f $ x` → `x |> f`). Keep the existing ignore list (correct defense against vanilla-pushing default hints).
2. **Repo-wide triage run:** `hlint core integrations testbed lsp`; classify remaining default-hint noise; extend ignores in the same PR so the gate lands green.
3. **Add the missing CI step** to `test.yml` (hlint currently does **not** run in CI — CLAUDE.md's claim was false). Early, build-free, seconds-cheap.
4. **PreToolUse edit hook** (`.claude/hooks/`): deny-list regexes on `*.hs` Write/Edit payloads — `\$`, `\bwhere\b`, `\bEither\b`, `\bpure\b`, `\breturn\b`, `import Data\.`, `import .* \(\.\.\)`, `case .* of\s+True` — rejection message quotes the exact style rule. ~50ms feedback.
5. **Copy-adapt rule** into `neohaskell-implementer`: no `.hs` file is ever created from a blank buffer; new files start from the nearest-neighbor module or the extension-point template (Phase 3).
6. **Repair-loop policy:** hlint-on-changed-files → typecheck (ghcid file) → max 2 repair rounds → escalate model tier. Sampling variant allowed for boilerplate: 3 cheap candidates, keep survivors.
7. **Vanilla-exception escape hatch (balanced enforcement).** For vanilla Haskell with no Core wrapper that isn't core enough to become a primitive (e.g. a single `hFlush`):
   - **Central registry only:** exceptions are `.hlint.yaml` `within:` entries with a mandatory justification comment and a `belongs-in:` note (the Core module the word would live in if promoted). Inline HLINT pragmas are forbidden — un-enumerable exceptions can't be counted or governed.
   - **Every ban's error message teaches the hatch:** hook and hlint messages end with "no Core wrapper? register the exception in `.hlint.yaml` with a justification and continue." Deliberate two-step friction. Without a sanctioned exit, models reimplement the banned thing badly with allowed vocabulary — strictly worse than the exception.
   - **Rule-of-three promotion:** `check.py` counts registry entries per vanilla symbol; at 3 uses the retrospective miner emits a `promote-to-primitive` delta.
   - Exceptions touched by a PR are auto-listed in the PR body — visible at review, never a blocking gate.

**Exit criteria:** a probe edit containing `foo $ bar` is rejected by the hook locally AND fails CI if forced through; `hlint` green on the branch; hooks wired in `settings.json`.
**Size:** ~1 day (+ triage variance on 347 modules). **Depends on:** nothing (parallel with 0/1). **Blocks:** Phase 5's repair loop.

---

## Phase 3 — Localization assets (`codemap/`)

**Goal:** plan-time localization ≤3 minutes, no exploration; adding a module without a map entry fails CI.

**Layout:**

```
codemap/
├── capabilities.yaml        # curated ontology: id, owns globs, responsibility, aliases, tests, see,
│                            #   security-sensitive / perf-sensitive risk tags (drive Phase 5 review gates)
├── extension-points.yaml    # curated: kind → create/register/tests/skill columns
├── MAP.md                   # generated render (humans + one-shot agent read)
├── signatures/              # generated hoogle-format exports, committed, sync-checked
│   ├── nhcore-core.txt
│   ├── nhcore-service.txt
│   └── nhintegrations.txt
└── check.py                 # validity checker (CI)
```

**Tasks:**

1. **Bootstrap generator:** group `exposed-modules` from the cabal files by directory prefix + first Haddock line per module → `capabilities.yaml` draft. Seed responsibilities/aliases from Phase 0 salvage. One Nick curation pass (mostly aliases — the intent-vocabulary bridge, e.g. `SPA, static assets, frontend → http-transport`).
2. **`extension-points.yaml`:** ~6 rows for the architectural shapes (core primitive, service/transport feature, command machinery, integration, test suite, skill), each with create/register/tests/skill columns. Encodes "where do things that don't exist yet go."
3. **`check.py` + CI step:** (a) every exposed module matched by some `owns` glob — orphans fail (the anti-rot mechanism); (b) every glob matches ≥1 real file; (c) alias uniqueness; (d) referenced skills exist. Also enforces the doc-ratchet (below).
4. **Signatures:** `cabal haddock --haddock-hoogle` → split per namespace → commit; CI regenerates and `git diff --exit-code`; `scripts/refresh-codemap` for local regen.
5. **Doc-ratchet (module AND function level):** two counters in `codemap/.doc-ratchet` — modules missing a `-- |` header, and **exported functions missing docs** (haddock's per-module coverage output makes the count cheap); CI fails if either increases. Anti-filler constraint: a function doc only counts if it contains a doctest or a stated constraint/gotcha — "Returns the result" filler is negative-value index content and gets rejected in sweep review. Burn-down sweeps are prioritized by Phase 4's frequency ranking (hot functions first), making this ratchet the phrasebook's supply chain.
6. **hiedb:** add `-fwrite-ide-info -hiedir=.hie` to the shared ghc-options blocks; `scripts/refresh-hiedb` (build + index); `scripts/who-calls` / `scripts/where-defined` wrappers whose output is joined against `capabilities.yaml` (`12 refs — http-transport: 3, testbed: 9`). `.hie/`, `.hiedb` gitignored.
7. **Routing procedure** documented in the planning skill: capability match (closed enum, cheap model OK) → skeletons confirm at symbol level → hiedb expands edit set → **exact paths written into the plan; execution never searches**. Low confidence or empty match → escalate model tier, never retry the cheap one.
8. **Routing smoke set:** `codemap/routing-smoke.yaml` — 10 committed request→expected-capability pairs, ≥2 of them vocabulary-gap probes ("serve a SPA" class). `scripts/routing-smoke` runs them through the router; pass bar ≥8/10. Run on every PR that changes `capabilities.yaml`/`extension-points.yaml` (scripted gate, not a hard CI job — it calls an LLM).
9. **Manifest back-fill:** update `docs/archive/2026-07-ai-artifacts/MANIFEST.md` "where surviving content went" entries now that codemap destinations exist — the manifest must not stay born-stale.

**Exit criteria:** `check.py` green in CI; signatures committed + sync-checked; who-calls returns capability-tagged output; routing smoke set passes ≥8/10; MANIFEST destinations back-filled.
**Size:** 2–3 days. **Depends on:** Phase 0 (salvage). **Blocks:** Phases 4, 5.

---

## Phase 4 — API knowledge delivery

**Goal:** zero-exploration generation: the executor model receives everything it needs; invented-API rate measured and falling.

**Tasks:**

1. **Hot card (`codemap/api-hot.md`):** script counts qualified call sites across nhcore + testbed (`Text.toLower`, `Task.yield`, …), ranks functions by frequency, emits top-N per hot module — each entry = signature + one-line **doctest-mined example** (CI-verified, can't rot). Target 300–500 lines. Generated + committed + sync-checked.
2. **Phrasebook:** intent-organized entries ("building strings", "Task validation with sentinels", "collection transforms") mined from doctests; thin-coverage areas become ratcheted doc backlog.
3. **Local hoogle:** `scripts/hoogle-setup` builds a database over nhcore; query wrapper for type-directed search (`Text -> Maybe Int` → `Text.toInt`). Toolchain pinned via the Nix flake.
   **[Amended 2026-07-08 — superseded by ADR-0066.]** Shipped as `./dev api` with TWO databases (dialect ranked top, dependency-closure vanilla below with disclaimer + escape-hatch guidance), the nixpkgs hoogle binary, and `shell.withHoogle = false` — two upstream defects (haskell.nix hoogle wrapper overriding `--database`; haskell.nix-built hoogle 5.0.18.4 ignoring `--local`) are documented with reproductions in [ADR-0066](../decisions/0066-two-database-api-search.md). DBs are local artifacts smoke-checked at build; the neo DB indexes the CI-gated signatures directly.
4. **Plan-time symbol resolution:** planning skill consults signatures while writing tasks; every task carries `uses: [Text.split, Task.mapError, …]`. The executor transcribes; it does not recall.
5. **Permanent-context mini-index:** ~40 lines (module inventory + one-liners + "never import Data.*") into the CLAUDE.md stub.
   **[Amended 2026-07-08.]** Superseded: `codemap/MAP.md` (Phase 3) already IS the generated, gated inventory — duplicating ~40 lines of it into permanent context would violate the constitution (a second, hand-adjacent copy with no sync gate). Permanent context instead carries the 8-line "API knowledge (Phase 4)" pointer section in AGENTS.md; the style table there covers "never import Data.*".
6. **Invented-API telemetry:** the repair loop labels GHC "not in scope" events; counted per PR in `telemetry/runs.jsonl`. **Baseline definition:** the mean over the first 5 instrumented pipeline runs (no historical replay); the falling-trend claim is measured against that.

**Exit criteria:** hot card + phrasebook generated and sync-checked; a sample plan carries resolved symbols; invented-API events visible in telemetry.
**Size:** ~2 days. **Depends on:** Phase 3 (signatures). **Blocks:** Phase 5.

---

## Phase 5 — Pipeline restructure: spec gate + verification architecture

**Goal:** exactly two human gates (spec approval, PR review); test-first ordering enforced; spec drift detected mechanically.

**Tasks:**

1. **Spec template (contract delta)** — the draft PR's first commit, `docs/changes/NNN-slug.md`:
   - Promised public-API diff (signature form — same format as `codemap/signatures/`).
   - `touches:` — capability IDs from `capabilities.yaml`. This field is the mechanical join: it derives the test-impact set (via each capability's `tests:`) and lets routing be verified against the approved spec.
   - Numbered behavior criteria (`C1…Cn`), each naming the test that will prove it **and declaring its level (`unit | integration | acceptance`)** — a filesystem behavior must declare `integration`, so no mock can fake it.
   - User-impact clause: breaking? testbed effect? migration note?
   - **ADR section (mechanically triggered):** required when the spec contains a breaking signature diff, a new capability, a new extension-point row, or a new dependency — the ADR is part of the spec Nick reviews at the gate (template salvaged from the archived adr skills; lands in `docs/decisions/` on merge). Large specs additionally grow a design chapter; small fixes stay spec-only.
   - For bugs: the failing reproduction test **is** the spec — committed red in the draft PR.
2. **Draft-PR gate flow:** pipeline opens draft PR (spec only) → push notification → Nick approves → continue signal = PR comment command handled via existing `claude.yml`, **accepted only from the repo maintainer** (author-association check in the workflow — anyone else's comment is ignored) → pipeline resumes from `.pipeline/state.json` (the binding contract: plan, resolved paths/symbols, stage — resume never re-plans).
3. **Draft-PR CI guard:** workflow condition skipping the heavy build/test jobs on draft PRs / spec-only diffs, so parked specs don't burn CI. (Promoted from the risks table — it's work, not a hope.)
4. **Risk-tiered design review (security/performance) — after spec approval, before implementation.** When the spec's `touches:` intersects capabilities tagged `security-sensitive` / `perf-sensitive` in `capabilities.yaml`, an agent review of the *approved spec/design* runs as the first post-gate stage: numbered checklists (rebuilt from the archived `neohaskell-security-review` / `neohaskell-performance-review` skills), findings either amend the plan or escalate to Nick, and the **review record attaches to the PR** — the audit-trail artifact national-infrastructure compliance requires. Gates scale with blast radius: untagged capabilities skip this entirely.
5. **Verification order enforced by the pipeline:** repro/acceptance test red → implement → green → targeted regression via test-impact (derived from the spec's `touches:` capabilities and their `tests:` fields) → full suite only at PR-ready.
6. **Expectation-protection hook:** block edits to existing test expectation values unless the run carries an explicit maintainer-approval flag (mechanizes the standing rule).
7. **Spec-drift check:** at PR-ready, regenerate signatures and diff against the spec's promised API surface; mismatch = pipeline flags before Nick sees it.
8. **Failure policy:** per-stage time-boxes; on breach → retry once → escalate model tier → **park** with a structured report comment (labeled failure cause from the taxonomy). A parked report beats a wrong PR.
9. **Benchmark harness (nightly, non-blocking):** criterion/tasty-bench suites over perf-tagged capabilities, seeded from the existing EventStore volume/magnitude-style suites (`core/testlib/Test/Service/EventStore/`). Runs **nightly, never per-PR** — regressions produce a notification + telemetry entry and a next-morning fix task, not a PR blocker. Perf assurance per-PR is the design review (task 4); perf *measurement* is nightly.

**Exit criteria:** one feature and one bug shipped end-to-end through the new flow, ≤2 human touchpoints each, full telemetry lines recorded, spec-drift check exercised, risk-tiered design review exercised at least once on a tagged capability.
**Size:** 3–5 days. **Depends on:** Phases 1–4.

---

## Phase 6 — Release tail + learning loop

**Goal:** "done" = deployed-to-testbed green; every failure makes the pipeline better.

**Tasks:**

1. **Definition-of-done gate (criteria-based — covers no-API changes too):** done = every numbered criterion's named proving test green **at its declared level** (a filestore fix declaring `integration` cannot be satisfied by a mocked unit test) + testbed acceptance suite (`testbed/scripts/run-tests.sh`) green against the merged change on main + spec-drift check (passes trivially when the spec promised no API surface). The gate reads the spec's criteria table, not the API diff — internal fixes with zero public API are first-class. Failure = auto-revert candidate + notification.
2. **Kill switch:** bot-owned revert workflow (one command/label → revert PR). Drill it once.
3. **Release discipline (thin v1):** changelog entries generated from spec contract deltas. **Breaking-change definition is mechanical:** any removed or changed line in the exported-signatures diff = breaking; the spec's user-impact clause and the changelog derive from that diff, and a migration note is mandatory whenever it's non-empty.
4. **Failure→asset-delta protocol:** every parked/failed/reworked run must end with (a) a labeled cause in telemetry and (b) an asset PR — new alias, phrasebook entry, hlint rule, hook pattern, or golden task. The fix for the instance and the fix for the class ship together.
5. **Retrospective miner:** weekly, fused with the telemetry review: a frontier model reviews the week's runs (telemetry lines + stage summaries + tool-call stats, chunked transcript access on demand — never raw full transcripts by default) and produces **≤5 recommendations**. Contract per recommendation:
   `{friction observed, evidence pointer (run/stage), delta type, destination file, estimated saving}`
   where delta type is a **closed taxonomy**: `alias | extension-point | phrasebook | hot-card | hlint-rule | hook | cli-utility | skill-edit | telemetry-label | PRUNE`.
   Rules: (a) recommendations without a cited friction event and measured cost are discarded — LLM counterfactuals are hypotheses, not findings; (b) action only on friction recurring across ≥2 independent runs; (c) out-of-taxonomy proposals (new CLI utilities, new asset kinds) are ontology-expansion decisions for Nick, never automatic; (d) implemented deltas are validated against their claimed metric over subsequent runs — no movement = prune candidate; (e) usage accounting tracks which assets runs actually consult, so the miner can recommend **removal**, not just addition. Nick implements 1–2 per week.
6. **Weekly telemetry review:** 30 minutes; stage-time distribution, failure labels, invented-API trend, retrospective-miner recommendations triage; output = next week's asset deltas (1–2 picked, rest backlogged or discarded).
7. **Archive sunset:** once Phase 5 and Phase 6 exit criteria are met and the pipeline is verified working end-to-end, **delete `docs/archive/2026-07-ai-artifacts/`** in a dedicated PR (Nick approves). Safe by construction: the Phase 0 `git mv` preserved full history, so deletion loses nothing recoverable.

**Exit criteria:** revert drill executed; first weekly review held on real data; first retrospective-miner report produced under the recommendation contract; archive sunset executed (or explicitly deferred by Nick).
**Size:** 1–2 days + standing cadence. **Depends on:** Phase 5.

---

## Sequencing

```
Phase 0 (archive & truth reset)   ─┬─→ Phase 3 (codemap) ─→ Phase 4 (API knowledge) ─┐
Phase 1 (inner loop + telemetry)  ─┤                                                  ├─→ Phase 5 (spec gate + verification) ─→ Phase 6 (release + learning)
Phase 2 (dialect enforcement)     ─┘                                                  ┘
```

Phases 0, 1, 2 are independent and can run in parallel (0 first if serializing — it unblocks 3 and removes rot from every agent run that follows). Rough calendar: **2–3 weeks** with agent labor, Nick gating curation passes and approvals.

**Nick's time budget (the scarce resource — everything else is agent-runnable):**

| Phase | Nick's involvement | Est. |
|---|---|---|
| 0 | Approve skill triage split; review stub + manifest PR | ~1h |
| 1 | Worktree-pool decision; review scripts PR | ~30m |
| 2 | Review hlint rule set (it encodes his style authority) | ~45m |
| 3 | Curation pass on capabilities/aliases; approve extension-points | ~2h |
| 4 | Spot-check hot card + phrasebook | ~30m |
| 5 | Per-run: spec approval (~2m) + PR review (~5m); one-time flow review | ~1h + per-run |
| 6 | Weekly 30m telemetry review (standing) | 30m/wk |

## Risks

| Risk | Mitigation |
|---|---|
| Phase 0 knowledge vacuum | Archive + stub + codemap skeleton in one atomic PR |
| hlint triage noise across 347 modules | Triage and ignores land in the same PR as the CI gate, so it lands green |
| TH forces object code, defeating fast typecheck | Profiling task first; `-fprefer-byte-code`; worst case ghcid still beats cold builds |
| Draft-PR spec commits trigger full heavy CI | Workflow guard: skip build/test jobs on draft PRs or spec-only diffs |
| Approval latency dominates wall-clock | Push notification on gate-ready; telemetry splits waiting-on-human from work-time |
| Toolchain drift (hiedb/hoogle versions) | Pin in the Nix flake |
| New assets rot like the old ones | Governing rule: everything generated-or-gated; `check.py` + sync checks + ratchets in CI |
