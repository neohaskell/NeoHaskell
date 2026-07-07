# ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE

Everything in this directory is **superseded, unmaintained, and scheduled for deletion**
once the Continuous Generation pipeline is verified working end-to-end
([plan, Phase 6 "Archive sunset"](../../plans/2026-07-07-continuous-generation-pipeline-plan.md), tracker [#715](https://github.com/neohaskell/NeoHaskell/issues/715)).
All files were moved with `git mv`, so full history survives deletion.

**Governing rule going forward:**
> No agent-visible document without a CI check or a generation source.

These artifacts rotted because nothing enforced their truth. Their replacements
(root `AGENTS.md` stub, `codemap/`) are either generated from ground truth or CI-gated.

Salvage record (claim-by-claim verdicts, skills ranking): [`SALVAGE.md`](SALVAGE.md).

## Archived artifacts

| Original path | Kind | Reason | Salvage | Surviving content went to |
|---|---|---|---|---|
| `CLAUDE.md` | knowledge | False claims (hlint-in-CI, test auto-discovery, phantom transpiler/CLI components) | mostly valid | Style table + verified commands → root `AGENTS.md`; structure facts → `codemap/capabilities.yaml` (landed 2026-07-08) |
| `core/AGENTS.md` | knowledge | Maps 10 of 18 source dirs; stale counts; inverted test-discovery claim | WHERE-TO-LOOK + trait/Task/concurrency facts valid | `codemap/capabilities.yaml` (core-primitives, traits, concurrency, system capabilities — landed 2026-07-08) |
| `core/service/AGENTS.md` | knowledge | Incomplete tree (omits ~14 modules/dirs); otherwise highest quality of the set | nearly all valid | `codemap/capabilities.yaml` (event-store, commands, entities, queries, http-transport, service-wiring — landed 2026-07-08) |
| `testbed/AGENTS.md` | knowledge | Omits 2 newer bounded contexts | mostly valid | `codemap/capabilities.yaml` (testbed capability) + extension-points.yaml |
| `integrations/AGENTS.md` | knowledge | Omits Acs + AzureAI | mostly valid | `codemap/capabilities.yaml` (outbound-integrations) + new-outbound-integration extension point |
| `website/AGENTS.md` | knowledge | Wrong repo identity; stale sidebar; undocumented locale-disable | workflow claims valid | `codemap/capabilities.yaml` (website capability) |
| `context/TODO.md` | knowledge | Dead planning doc (2025-11-17); describes shipped work as "awaiting implementation"; superseded API designs | InsertionType + file-list facts | `codemap/capabilities.yaml` (event-store responsibility text) |
| `context/collections.md` | knowledge | Aspirational API spec reality diverged from (~23 of ~40 functions absent) | convention rules only | doc-ratchet rules (codemap/.doc-ratchet, landed 2026-07-08) |
| `context/documentation.md` | knowledge | `prop>` doctests unused; "no Haddock" contradicted by codebase | doctest rule | doc-ratchet counters (codemap/.doc-ratchet, landed 2026-07-08) |
| `opencode.jsonc`, `ocx.jsonc`, `ocx.lock` | tool config | Retired tool (opencode) — dead configs confuse agents | none | — |
| `.claude/skills/*` → `claude-skills/` (all 14 skills, 153 files) | knowledge | Full-sweep decision: no quarantine-in-place; all skills rebuilt from scratch in Phases 2–5 | see SALVAGE.md Part 2 | hlint rules + hooks (Phase 2), implementer skill (Phase 2), ADR template + spec-gate flow + review checklists (Phase 5), QA rubrics (Phase 5) |

## Not archived (active plumbing, kept in place)

`.claude/hooks/`, `.claude/settings.json` (branch guard, fourmolu hook — extended in Phases 2/5),
`.github/workflows/claude.yml` + `claude-code-review.yml` (reused for the Phase 5 continue signal),
`docs/plans/`, `docs/decisions/`.

## Untracked local state (not in git; moved on disk only)

`.pipeline/` → `telemetry/archive/pipeline-state/`, `.integration-pipeline/` → `telemetry/archive/integration-pipeline-state/`
(process state = run artifacts, not knowledge). `.claude/settings.local.json` and `uploads/` untouched (local, out of scope).

## Banner coverage

Every archived `.md`, `.py`, and `.jsonc` file carries an ARCHIVAL banner in its native comment syntax.
Exceptions (banner would be meaningless or break nothing worth preserving): 2 × `.gitignore`, 1 × `ocx.lock` —
covered by this manifest. Stale `__pycache__/` dirs (untracked) were deleted, not archived.
