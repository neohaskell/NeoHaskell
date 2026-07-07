# From user prompt to localized edits: survey of planning methodologies

**Date:** 2026-07-07
**Status:** Research findings (internet survey, three parallel sweeps: agent harnesses, academic pipelines, spec-driven + framework-specific tooling)
**Question:** How do existing harnesses/skills decompose a free-form user request into a proper plan with localized edits in a codebase? Is there specialized treatment for standardized codebases (Rails, Laravel, …)?

---

## TL;DR — the canonical pipeline

Across ~30 harnesses, skills, spec-driven frameworks, and academic systems, everything converges on the same shape:

```
user prompt
  → clarify intent            (questions, or "NEEDS CLARIFICATION" markers in the artifact)
  → ground in the repo        (localization: which files/symbols does this touch?)
  → spec                      (WHAT: current behavior vs. desired behavior — no implementation)
  → plan                      (HOW: ordered list of {file path, change instruction} pairs)
  → human gate                (approve/edit the artifact, often + context reset)
  → execute                   (single agent, planner/executor split, or subagent-per-task)
  → validate & select         (tests/oracle → feed failures back, or vote among candidates)
```

Nobody ships all stages; the interesting differences are *which stages get a first-class artifact* and *how localization works*.

## Five load-bearing ideas

1. **Localization is a separable, hierarchical funnel** (file → symbol → line) and the highest-ROI place to invest; represent the repo as tree/skeleton + typed AST entity graph (contain/import/invoke/inherit), not embeddings.
2. **Separate WHAT from HOW from WHERE**: behavioral spec (before/after), then plan, with file paths resolved *at plan time* and cached in the artifact so execution never re-searches.
3. **Traceability IDs** (requirement → task → test) are what make plans verifiable rather than decorative.
4. **The plan is a context-compression artifact** — the best harnesses reset context at the plan boundary and let the plan alone seed execution.
5. **Explicit plan graphs only earn their keep for propagating changes** (CodePlan's obligation graph + change-impact rules); for localized fixes, generate-many + execute + vote beats elaborate planning.

---

## Stage 1: Intent → Spec

Most harnesses collapse spec into plan; the ones that don't are the most instructive:

- **GitHub Copilot Workspace** ([user manual](https://github.com/githubnext/copilot-workspace-user-manual/blob/main/overview.md)) had the cleanest formulation: from the task it generates a **Current Spec** and **Proposed Spec** — two bulleted lists of *behavior*, before and after, with zero implementation detail. Both editable bullet-by-bullet before any plan exists. Design rationale (GitHub Next): natural-language intermediates are deliberate "control points" — cheap to correct before errors compound into code.
- **Spec-driven frameworks** ([GitHub Spec Kit](https://github.com/github/spec-kit), [AWS Kiro](https://kiro.dev/docs/specs/), [OpenSpec](https://github.com/Fission-AI/OpenSpec)) constrain the spec's *language* so it becomes referenceable and testable: numbered requirements (`FR-001`), EARS notation ("WHEN X THE SYSTEM SHALL Y"), RFC-2119 keywords + Given/When/Then scenarios. The constraint syntax exists so later artifacts can point back at requirement IDs.
- Ambiguity is handled *in the artifact*, not in chat: Spec Kit forces inline `[NEEDS CLARIFICATION: …]` markers that downstream commands refuse to proceed past.

Two structural extras worth stealing: a **constitution** (standing project principles checked as a pass/fail gate during planning, violations require written justification), and OpenSpec's **delta specs** for brownfield work — the change package only states `ADDED / MODIFIED / REMOVED` requirements against a living spec of current behavior, then merges on archive.

## Stage 2: Localization — the highest-leverage stage

The academic literature is unambiguous: swapping only the localizer moves end-to-end issue resolution by 4–12+ points with repair held fixed (LocAgent, CoSIL, OrcaLoca). The methodology spectrum, in increasing sophistication:

1. **Agentic grep/read** — Claude Code, Cline, Roo, OpenCode, Goose. No index; the plan artifact caches the result.
2. **Hierarchical funnel** ([Agentless](https://arxiv.org/abs/2407.01489), the reference design): repo serialized as a bare **file tree** → LLM picks suspicious files (intersected with an embedding channel) → files compressed into **skeletons** (signatures + headers only) → LLM picks classes/functions → full code of those → LLM picks line-level edit locations. Each level is a cheap compression of the repo appropriate to that decision.
3. **Static symbol graph under a token budget** ([Aider's repo map](https://aider.chat/2023/10/22/repomap.html)): tree-sitter extracts definitions *and references* → dependency graph → **PageRank personalized by the chat state** → top-ranked symbol signatures packed into ~1k tokens, sent with every request.
4. **AST entity graphs with typed edges** — LocAgent, RepoGraph, CoSIL, CodePlan, Sweep all independently converged on the same four edge families: **contain, import, invoke, inherit**. Sweep's variant plans in *entity space* rather than file space: retrieve high-recall candidates lexically + by embedding, expand one hop in the entity graph, then LLM-filter to high precision.
5. **Brute-force scan** ([CodeMonkeys](https://arxiv.org/abs/2501.14723)): a cheap model reads *every file* (~3M tokens) and classifies relevance — a 50× context reduction that costs <15% of the total budget.

Consensus: **cheap structure beats embeddings**. Tree/skeleton serialization for the coarse pass, AST entity graph for fine navigation; dense embeddings survive only as a secondary voting channel. The harness world's historical trend ran *away* from heavy indexing toward agentic search — with the plan file caching localization results so execution never re-searches.

## Stage 3: The plan artifact

Converged form: **a markdown file on disk containing explicit file paths**, persisted in a known location (`.cursor/plans/`, `.opencode/plans/`, `implementation_plan.md`, plan-mode plan files). Two harnesses enforce this at the permission layer — Roo's Architect mode and OpenCode's plan agent can *only* write markdown/the plans directory, so the plan is literally the sole writable artifact of the phase.

Rigor spectrum for the contents:

- Free-form prose (Claude Code plan mode) → checklists (Cursor, Goose) → **{file, NL instruction} pairs** (Copilot Workspace, Sweep) → fully-specified per-task contracts. The max-rigor end is [obra/superpowers' `writing-plans` skill](https://github.com/obra/superpowers/blob/main/skills/writing-plans/SKILL.md): every task lists `Create:/Modify: path:line-range`, exact interface signatures (Consumes/Produces), 2–5-minute atomic TDD steps with full code, the exact commit command, a zero-placeholder rule ("no TBD, no 'similar to Task N'"), plus a mandatory self-review pass checking spec coverage and cross-task signature consistency.
- Spec-driven frameworks add **ID-level traceability** as the load-bearing feature: Kiro tasks carry `_Requirements: 1.1, 3.2` trailers, BMAD tags subtasks `(AC: #1)`, Spec Kit tags tasks `[US1]` and marks `[P]` for parallelizable — the DAG is explicit, and verification becomes checkable rather than vibes.
- Academic counterpoint: for *issue-fixing*, the plan is usually just a list of edit locations plus rationale. The only system with a true plan artifact is [Microsoft's CodePlan](https://arxiv.org/abs/2309.12499) — an adaptive **obligation graph** where each edit is classified (signature change, method add/delete…) and change-may-impact rules over the dependency graph enqueue newly-affected blocks, iterating until the build oracle passes. That's the differentiator specifically for *propagating* multi-file changes (their benchmarks: 2–97 files), where trial-and-error baselines scored 0/6.

An insight that shows up in five independent places: **the plan doubles as a context-compression artifact, not just a review artifact.** Goose explicitly offers to wipe history and act on the plan alone; Claude Code has `showClearContextOnPlanAccept`; Amp's `/handoff` seeds a fresh thread from the plan; Cline's `/deep-planning` spawns a new task; BMAD "shards" the PRD+architecture into self-contained story files (each carrying pre-extracted `file_locations`) so the dev agent never reads the full corpus. This matches Cognition's ["Don't Build Multi-Agents"](https://cognition.com/blog/dont-build-multi-agents) argument that long-horizon work needs a compression layer — the plan *is* that layer.

## Stage 4: Execution topology

Three models, with a measurable result for one:

- **Single agent continues** with plan in context (most harnesses; Devin uniquely keeps re-planning mid-flight).
- **Planner/executor model split**: [Aider's architect/editor](https://aider.chat/2024/09/26/architect.html) — a reasoning model describes the change in free prose, a second model converts it to strict search/replace edits. Separating "solve the problem" from "conform to the edit format" lifted o1-preview from 79.7% → 85.0% on their benchmark.
- **Orchestrator + fresh subagent per task** (Roo's boomerang `new_task`, superpowers' subagent-driven-development, Kiro's dependency "waves" running independent tasks concurrently). Cognition argues against this for coupled work; tools that do it mitigate by making each task self-contained with the plan holding all shared state.

## Stage 5: Validation and selection

Academic recipe, uniform across systems: **generate many, execute, vote**. Sample N candidate patches (Agentless 40, MASAI 5, CodeMonkeys 10) → filter by regression tests → rank by whether an LLM-*generated reproduction test* flips fail→pass → majority-vote over normalized diffs. Reproduction tests are noisy (Agentless: only 94/300 flip correctly on ground-truth patches), so all systems layer fallbacks.

CodeMonkeys quantified the frontier: 69.8% of problems had a correct patch among candidates but only 57.4% got selected — **selection, not generation, is now the binding constraint**. Their "Barrel of Monkeys" ensemble: 80.8% coverage → 66.2% selected, beating the best single member (62.8%).

Harness-side, the split is between tools with a repair loop (Sweep runs your GitHub Actions and self-fixes; Aider auto-lints/tests with reflection; Copilot Workspace validates in a Codespace sandbox) and tools that hand validation back to the human (Cursor, Cline, Goose, OpenCode).

## Standardized codebases: yes, and it changes the mechanism

**Convention-over-configuration converts localization from a search problem into a lookup problem.** Four distinct exploitation mechanisms found in the wild:

1. **Directory structure as work-routing.** [claude-on-rails](https://github.com/obie/claude-on-rails) (Obie Fernandez) assigns each agent in a swarm a Rails directory 1:1 via `claude-swarm.yml` — a models agent owns `app/models`, a controllers agent owns routing/HTTP. "Where does this go" is pre-answered by the framework, so the architect agent only decomposes by MVC role. [rails_ai_agents](https://github.com/ThibautBaissac/rails_ai_agents) inverts the trick: 11 path-scoped rules auto-activate when the agent edits matching globs (`app/models/**` → ActiveRecord conventions, migrations → reversibility, policies → Pundit).
2. **Convention knowledge distributed through the package manager, versioned like code.** [Laravel Boost](https://laravel.com/docs/13.x/boost) (official) generates AI guidelines and skills from composer.json — per-package, *per-version* (Livewire 2 vs 3 vs 4 get different guidance), regenerated on `boost:update --discover`; third-party packages ship their own guideline files at `resources/boost/guidelines/`. Elixir's [usage_rules](https://hexdocs.pm/usage_rules/) does the same from hex deps (`mix usage_rules.sync` → AGENTS.md + generated skills). Tessl's Spec Registry ships 10,000+ version-accurate library "Usage Specs" as installable dependencies. The agent's "how we do things here" context is a build artifact, not hand-written docs.
3. **Runtime introspection replaces static search.** Boost's MCP server (~15 tools) exposes live schema, Eloquent models, routes, a tinker REPL, last error, browser logs; [Tidewave](https://tidewave.ai/) (José Valim/Dashbit) embeds the MCP server *in the running app* for Phoenix and Rails; [rails-ai-context](https://github.com/crisnahine/rails-ai-context) exposes 38 live tools. Standardized frameworks have standard introspection surfaces, so agents get ground truth instead of grepping.
4. **Training-data density as implicit convention.** Evil Martians ([greenfield→MVP in 4 weeks](https://evilmartians.com/chronicles/2-martians-greenfield-to-mvp-in-4-weeks-agentic-coding-on-rails)) and [NextLink Labs](https://nextlinklabs.com/resources/insights/claude-code-configurations-every-rails-engineer-should-use) make the same argument: 20 years of uniform public Rails code means "the AI's first guess about where code lives is usually correct" — Ruby cited cheapest per task ($0.36) because tokens aren't spent on file discovery. Corollary for CLAUDE.md: document only what's *not* inferable from convention, keep it under ~15KB.

Nuance: the planning *artifacts* stay generic — rails_ai_agents' pipeline is structurally a Spec Kit clone (constitution → spec → adversarial review → plan → data-model → contracts → tasks → validation-report, plus a "small-change mode" that warns if >3 criteria or >6 files). What conventions change is the **plan→code binding layer**. Generic tools must spend an entire phase deciding project layout and writing file paths into tasks; framework tools get that mapping free and spend the effort on rule enforcement and live verification instead.

**Gap nobody fills yet:** no surveyed tool drives planning through **generators** (`rails g model …`) as first-class plan steps — generators appear only as CLI-reference rules. Template-driven task emission is unexploited headroom.

Django is fragmented (several MCP servers, a Styleguide MCP, community CLAUDE.md guides; nothing at Boost's level). Shopify's [Roast](https://github.com/Shopify/roast) is adjacent: convention-oriented YAML workflow orchestration wrapping Claude Code steps ("non-determinism is the enemy of reliability") — the workflow-hardening layer for large-scale code operations, not a spec-planner.

### Implication for NeoHaskell

The event-sourcing/CQRS layout (commands/events/projections in fixed locations under `core/service/Service/`) is exactly the convention surface Boost and claude-on-rails exploit — a guideline/skill pack generated from cabal deps plus path-scoped rules, and `neo`-generator-driven plan steps, would be the Boost pattern applied to a language where nobody has done it yet. The existing `feature-pipeline-preview` is already the Spec-Kit-shaped artifact chain; the pieces it lacks relative to state of the art are requirement→task ID traceability and plan-time file-path resolution.

---
---

# Appendix A — Agent harnesses in detail

## A.1 Claude Code — Plan Mode (+ Ultraplan)

**Stages.** Injected system prompt defines 4 phases: (1) Initial Understanding — explore code, ask clarifying questions; (2) Design — develop implementation approaches; (3) Review — validate plan against original request, resolve ambiguities; (4) Final Plan — write recommended approach to a plan file "in scannable, executable format." Recent versions internally spawn an Explore agent (read-only search) then a Plan agent (architect) as subagents.

**Artifacts.** A markdown plan file in a dedicated plans folder (e.g. `~/.claude/plans/<name>.md`). No enforced schema. `ExitPlanMode` reads the file from disk and passes its contents forward — "the path towards spec always goes via the file system" (Ronacher).

**Localization.** Purely agentic: Read/Glob/Grep/Task tools; no index/embeddings. Docs recommend delegating exploration to subagents so file-read dumps stay out of main context.

**Validation.** Enforcement is a permission downgrade (writes blocked) + prompting, not tool removal. Hard user gate with multiple approval modes; `Ctrl+G` opens the plan in $EDITOR pre-approval. Ultraplan moves review to a browser with inline comments per section and iterated redrafts.

**Execution.** Same agent continues; optional `showClearContextOnPlanAccept` clears planning context so execution starts fresh with only the plan. Ultraplan adds cloud-execution → PR or "teleport" back to terminal.

**Skills note.** `anthropics/skills` contains **no** planning skill. The canonical community planning skill is **obra/superpowers `writing-plans`**: plan saved to `docs/superpowers/plans/YYYY-MM-DD-<feature>.md`; required header (Goal 1 sentence, Architecture 2-3 sentences, Tech Stack, Global Constraints); pre-task file-structure section mapping every created/modified file to one responsibility; per-task format = Files (`Create:/Modify: path:123-145/Test:`), Interfaces (Consumes/Produces exact signatures), checkbox steps of 2–5 min atomic TDD actions with full code and exact git commit command. Zero-placeholder rule. Mandatory self-review checklist (spec-coverage map, placeholder scan, cross-task type/signature consistency). Execution handoff: subagent-per-task with review between tasks (recommended) or inline.

Sources: [permission-modes docs](https://code.claude.com/docs/en/permission-modes) · [Ultraplan docs](https://code.claude.com/docs/en/ultraplan) · [common-workflows](https://code.claude.com/docs/en/common-workflows) · [Ronacher: What Is Plan Mode](https://lucumr.pocoo.org/2025/12/17/what-is-plan-mode/) · [how-claude-code-works plan-mode chapter](https://github.com/Windy3f3f3f3f/how-claude-code-works/blob/main/en/docs/10-plan-mode.md) · [obra/superpowers writing-plans](https://github.com/obra/superpowers/blob/main/skills/writing-plans/SKILL.md)

## A.2 GitHub Copilot Workspace (GitHub Next)

The most formalized pipeline of any tool: **Task → Spec → Plan → Implementation**, every artifact natural-language and editable.

- Spec = topic question + **Current Spec** and **Proposed Spec** (bulleted before/after behavior lists, success criteria only).
- Plan = list of files to modify (edited/created/deleted/moved/renamed), each with specific steps. Both artifacts editable bullet-by-bullet and regenerable.
- Localization: "LLM techniques and traditional code search"; user can steer by naming files in the task text.
- Validation: human gate at every stage + integrated Codespace terminal for build/lint/test.
- Execution: file-by-file per plan steps; repair loop = edit the plan steps for specific files and regenerate. Ends in PR/branch push.

Sources: [user manual overview](https://github.com/githubnext/copilot-workspace-user-manual/blob/main/overview.md) · [GitHub Next project page](https://githubnext.com/projects/copilot-workspace/) · [concept-to-code discussion](https://github.com/orgs/community/discussions/142971)

## A.3 Cursor — Plan Mode

Shift+Tab into Plan Mode: clarifying questions → codebase research → markdown plan (file paths, code references, editable to-do list) → user reviews/edits → explicit "Build" click. Plans saveable to `.cursor/plans/` for team sharing and as context for future agents. Semantic-embedding codebase index available; the plan pins results as explicit file paths so execution doesn't re-search. "Most new features at Cursor now begin with Agent writing a plan."

Sources: [Plan Mode docs](https://cursor.com/docs/agent/plan-mode) · [announcement](https://cursor.com/blog/plan-mode) · [practitioner walkthrough](https://engincanveske.substack.com/p/how-i-use-cursor-plan-mode-for-real)

## A.4 Cline — Plan/Act split + /deep-planning

Two-state toggle: Plan mode reads/searches/discusses, "cannot modify any files or execute commands"; Act has full tools. Guidance: small task → Act only; medium → Plan→Act; large → `/deep-planning` with 4 phases: Silent Investigation → Discussion (targeted questions) → Plan Creation (`implementation_plan.md`) → Task Creation (new task with trackable steps). Per-mode model config is first-class (reasoning model plans, cheaper model executes). Mode boundary is prompt-enforced (StateManager regenerates system prompt per mode).

Sources: [Plan & Act docs](https://docs.cline.bot/features/plan-and-act) · [deep-planning docs](https://docs.cline.bot/features/slash-commands/deep-planning) · [blog](https://cline.bot/blog/plan-smarter-code-faster-clines-plan-act-is-the-paradigm-for-agentic-coding) · [DeepWiki internals](https://deepwiki.com/cline/cline/3.4-plan-and-act-modes)

## A.5 Roo Code — Architect mode + Orchestrator

Architect mode's tool grant is the notable design: `read` + `mcp` + **`edit` restricted to markdown files only** — the plan artifact is enforced at the permission layer. Execution: manual switch to Code mode, or **Orchestrator (Boomerang) mode** — delegates subtasks via `new_task` to specialized modes, each a fresh context that returns a summary; plan-driven execution as subagent-per-step with the orchestrator holding the plan.

Sources: [Using modes](https://docs.roocode.com/basic-usage/using-modes) · [Custom modes](https://docs.roocode.com/features/custom-modes)

## A.6 Aider — architect/editor split + repo map

**Architect/editor:** (1) Architect model describes the solution in free prose, zero edit-format constraints; (2) Editor model converts to strict search/replace blocks. Rationale: a model "splits its attention between solving the coding problem and conforming to the edit format"; the split lifted o1-preview 79.7% → 85.0% on aider's benchmark.

**Repo map (reference implementation of static repo mapping):** tree-sitter parses every file into AST; extracts symbol definitions AND references; builds a graph (files = nodes, dependency/reference edges); **PageRank personalized by chat state** (files/identifiers in conversation up-weight related nodes); greedy selection of top-ranked symbols under `--map-tokens` (default 1k). Sent with every request: file paths + key symbol signatures. Auto-lint/auto-test after edits with reflection loop.

Sources: [architect/editor announcement](https://aider.chat/2024/09/26/architect.html) · [repo map w/ tree-sitter](https://aider.chat/2023/10/22/repomap.html) · [repomap docs](https://aider.chat/docs/repomap.html)

## A.7 Sweep AI

Issue-triggered pipeline: retrieval → planning (posted **as a comment on the GitHub issue** for human correction pre-coding) → per-file code changes → PR → CI validation loop. Formally: issue → (file, file-level NL instructions) pairs → code edits.

**Localization (most engineered of the harness group):** (a) custom lexical TF-IDF tokenizing camel/pascal/snake boundaries; (b) embedding search over a custom chunker; (c) **AST entity graph**: bipartite files↔entities graph, prune zero-in-degree nodes (680→102 edges in their example), traverse one degree out from retrieved files for recall, LLM-filter for precision. Plans in **entity space, not file space**. Context managed to ~10–15k tokens via keep-lists and CST-extracted function-level snippets.

**Multi-file coherence:** diffs propagated between sequential edits + topological sort of files by import order so signature changes reach callers. Runs your GitHub Actions and self-fixes failures before human PR review.

Sources: [Sweep planning blog](https://github.com/sweepai/sweep/blob/main/docs/pages/blogs/ai-code-planning.mdx) · [repo](https://github.com/sweepai/sweep) · [E2B founder interview](https://e2b.dev/blog/sweep-founders-share-learnings-from-building-an-ai-coding-assistant)

## A.8 Devin (Cognition)

**Interactive Planning** (Devin 2.0): on session start, interprets prompt, rapidly searches codebase, returns within seconds: relevant files + findings + preliminary plan. User edits/confirms; optional "wait for my approval" hard gate. Then autonomous plan-execute-observe-**re-plan** loop (plan mutable during execution — unique among surveyed tools). Two-layer localization: pre-computed **Devin Wiki** (auto-generated repo wiki, re-indexed every few hours, architecture diagrams) + on-demand **Devin Search** with citations.

**Architecture doctrine** ([Don't Build Multi-Agents](https://cognition.com/blog/dont-build-multi-agents)): parallel subagents fail because "actions carry implicit decisions, and conflicting decisions carry bad results"; recommended pattern = single linear agent + an LLM **compression layer** distilling history. The plan effectively serves as that persistent compressed context.

Sources: [Devin 2.0](https://cognition.com/blog/devin-2) · [Interactive Planning docs](https://docs.devin.ai/work-with-devin/interactive-planning) · [How Cognition uses Devin](https://cognition.com/blog/how-cognition-uses-devin-to-build-devin)

## A.9 Amp (Sourcegraph)

No formal plan mode; convention-driven. Recommended pattern: planning thread → `/handoff` drafts a new thread carrying only relevant context → execution thread ("one thread per task"). **Oracle tool** — a stronger reasoning model invoked as second opinion for planning/review/debugging. Librarian subagent for cross-repo research. Context hygiene over artifact formalism.

Sources: [Amp Owner's Manual](https://ampcode.com/manual)

## A.10 OpenCode

Two primary agents toggled with Tab: **build** (full tools) and **plan** (mutations denied). Notable: in plan mode edits are disabled **except for `.opencode/plans/*.md`** — a filesystem-level carve-out making the plan file the only writable artifact. Read-only subagents `@explore` (codebase) and `@scout` (external deps/docs) keep search out of primary context.

Sources: [Agents docs](https://opencode.ai/docs/agents/)

## A.11 Goose (Block)

CLI `/plan` command: clarifying-questions loop until enough context, then a comprehensive plan (structured chat message with per-step checkboxes). Two gates: plan approval, then — distinctive — goose asks whether to **clear message history and act on the plan alone** (explicit context reset). Dedicated planner model config: `GOOSE_PLANNER_PROVIDER`/`GOOSE_PLANNER_MODEL`.

Sources: [Goose docs: creating plans](https://block.github.io/goose/docs/guides/multi-model/creating-plans/) · [plan-mode article](https://dev.to/goose_oss/does-your-ai-agent-need-a-plan-4ic7)

---

# Appendix B — Academic pipelines (SWE-bench lineage)

## B.1 Agentless (Xia et al., UIUC, 2024)

Fixed 3-phase pipeline, no agent: **localization → repair → patch validation**.

- **Localization, hierarchical 3-level:** file level (LLM over `tree`-style repo structure format + embedding retrieval, intersected) → class/function level (files compressed into **skeleton format**: headers, fields, signatures, comments) → edit-location level (line numbers/functions/classes).
- **Repair:** ±10-line context per location; **Search/Replace diff format**; 4 location sets × 10 samples = 40 candidate patches.
- **Validation:** LLM-generated reproduction test (majority-voted; noisy — only 94/300 flip correctly on ground truth) + regression test selection + rank by fewest regressions → reproduction pass → **majority vote over normalized patches**.
- **Results:** SWE-bench Lite 32.0% at $0.70/issue; Verified 38.8%; 40.7%/50.8% with Claude 3.5 Sonnet. Beat contemporary agents at ¼ the cost.

Sources: [arXiv:2407.01489](https://arxiv.org/abs/2407.01489) · [github.com/OpenAutoCoder/Agentless](https://github.com/OpenAutoCoder/Agentless)

## B.2 CodePlan (Bairi et al., Microsoft Research, FSE 2024)

Repository-level coding as **planning**: seed edit specs + oracle Θ (build/type-check) → synthesize a multi-step chain of edits until Θ passes. "Seed specifications trigger other editing requirements… propagated across dependencies."

- **Repo representation:** dependency graph over code blocks (tree-sitter AST); edge families: ParentOf, Construct, Imports, BaseClassOf, Overrides, Calls, Instantiates, Uses (+ inverses). **Incrementally** recomputed after each edit.
- **Plan artifact:** explicit **plan graph** of obligations ⟨Block, Instruction, pending/completed⟩ — an adaptive change-propagation graph.
- **Loop:** extract fragment → spatial context (graph neighbors) → temporal context (linearized prior-edit paths with cause-effect statements) → LLM edit → classify change → **change-may-impact rules** enqueue affected blocks. Oracle errors become new seeds.
- **Results:** package migration (C#) + temporal edits (Python), 6 repos, 2–97 files: CodePlan 5/6 valid; oracle-guided-repair baselines 0/6.

Sources: [arXiv:2309.12499](https://arxiv.org/abs/2309.12499) · [github.com/microsoft/codeplan](https://github.com/microsoft/codeplan)

## B.3 AutoCodeRover (Zhang et al., NUS, ISSTA 2024)

Program as **AST, not bag of files**. Stratified API-driven search: `search_class`, `search_method_in_class`, `search_code_in_file`, etc.; per stratum the LLM picks API calls, then decides whether context suffices → continue or output method-level buggy locations (≤10 strata). Optional **SBFL** injection (spectrum-based fault localization from test runs) lifts results 22%. Patch retry loop with linter. Results: Lite 19% @1 ($0.43), later 46.2% Verified with Claude 3.5.

Sources: [arXiv:2404.05427](https://arxiv.org/abs/2404.05427) · [github.com/AutoCodeRoverSG/auto-code-rover](https://github.com/AutoCodeRoverSG/auto-code-rover)

## B.4 SWE-agent (Yang, Jimenez et al., Princeton, NeurIPS 2024)

Thesis: LMs need purpose-built **Agent-Computer Interfaces (ACI)**. Single ReAct loop; localization emergent. ACI guidelines: simple documented actions; compact operations; concise informative feedback; **guardrails** (edit command with integrated linter that rejects syntax-breaking edits: −3 pts without it; full ACI beats plain shell by +10.7 pts; 100-line file-viewer window beats larger/smaller). Successor **mini-swe-agent** (~100 lines, 65% Verified with modern models) shows scaffolding value is model-relative.

Sources: [arXiv:2405.15793](https://arxiv.org/abs/2405.15793) · [ACI background](https://swe-agent.com/0.7/background/aci/)

## B.5 MASAI (Arora et al., Microsoft, 2024)

Modular architecture — 5 sub-agents, each with own objective/strategy/temperature: Test Template Generator (learns repo-specific test invocation), Issue Reproducer (writes failing test), **Edit Localizer** (navigates repo incl. docs and existing tests), Fixer (samples 5 patches, CoT, no environment actions), Ranker (runs reproduction test on each patched repo, ranks by fail→pass flip). Results: 28.33% Lite (then joint SOTA), 75% localization rate.

Source: [arXiv:2406.11638](https://arxiv.org/abs/2406.11638)

## B.6 LocAgent (Chen et al., ACL 2025) — localization only

Directed heterogeneous code graph (nodes: directory/file/class/function; edges: **contain/import/invoke/inherit**) + sparse indexes (entity IDs, name dictionary, BM25 inverted index). Three unified tools: `SearchEntity`, `TraverseGraph` (type-aware BFS, compact tree-format subgraphs), `RetrieveEntity`. Results: file Acc@5 94.2%, function Acc@10 77.4% on Lite; fine-tuned Qwen2.5-Coder-32B matches at −86% cost; better localization → +12% Pass@10 downstream. Contributes **Loc-Bench** (fixes SWE-bench's 85%-bug-report skew).

Sources: [arXiv:2503.09089](https://arxiv.org/abs/2503.09089) · [github.com/gersteinlab/LocAgent](https://github.com/gersteinlab/LocAgent)

## B.7 RepoGraph (Ouyang et al., ICLR 2025) — plug-in module

Line-level graph nodes (def/ref) from tree-sitter; edges invoke/contain; project-local only. Used as k-hop **ego-graph** flattened into prompts (procedural frameworks) or as a `search_repograph()` action (agentic). Consistent ~2-pt lift on any host framework (Agentless 27.3→29.7, SWE-agent 18.3→20.3).

Sources: [arXiv:2410.14684](https://arxiv.org/abs/2410.14684) · [github.com/ozyyshr/RepoGraph](https://github.com/ozyyshr/RepoGraph)

## B.8 RepoUnderstander / LingmaAgent (Ma et al., Alibaba, 2024)

Hierarchical repository knowledge graph + function-call edges; **MCTS condensation** (UCT selection, BM25-correlated expansion, LLM CoT reward) to find issue-relevant subgraph; summary agent emits location descriptions + solution plan; then ReAct dynamic acquisition. 21.3% Lite; 38.3% with Claude 3.5 Sonnet.

Source: [arXiv:2406.01422](https://arxiv.org/abs/2406.01422)

## B.9 CodeMonkeys (Ehrlich et al., Stanford, 2025) — test-time scaling

- **Context:** cheap model (Qwen2.5-Coder-32B) reads every file (~2.94M tokens/problem), ranks relevance, packs 128k window (50.5× reduction, <15% of cost).
- **Generation:** 10 parallel (edit, test) candidate pairs; dual state machines iterate reproduction script and aider-style diff jointly on execution feedback (≤8 iterations).
- **Selection:** run all 10 tests × 10 edits, vote; top-3 tie-break via a selection state machine that writes new discriminating tests.
- **Results:** 57.4% Verified (~$4.60/issue); coverage 69.8% → **selection is the binding constraint**. Ensemble of leaderboard submissions: 80.8% coverage → 66.2% selected, beating best single member (62.8%).

Sources: [arXiv:2501.14723](https://arxiv.org/abs/2501.14723) · [github.com/ScalingIntelligence/codemonkeys](https://github.com/ScalingIntelligence/codemonkeys)

## B.10 Newer 2025–2026 work

- **CoSIL** ([arXiv:2503.22424](https://arxiv.org/abs/2503.22424)): no pre-built index — call graphs constructed dynamically by the LLM during search; pruner agent vetoes irrelevant expansions; function Top-1 43% vs Agentless 24.7%.
- **OrcaLoca** ([arXiv:2502.00350](https://arxiv.org/abs/2502.00350)): priority-based action scheduling + distance-aware context pruning; 65.3% function match on Lite.
- **SweRank/SweRank+** ([arXiv:2505.07849](https://arxiv.org/abs/2505.07849)): localization as retrieve-then-rerank with models trained on issue↔modified-function pairs mined from GitHub PRs; beats Claude-based agentic localizers at a fraction of the cost — the "distill the localizer into a small model" trend.
- **ARISE** (2026, [arXiv:2605.03117](https://arxiv.org/pdf/2605.03117)): statement-level graph nodes + intra-procedural def-use edges; data-flow slicing as an agent tool; SWE-agent +4.7 pts; line Recall@1 0.26→0.41 (line level is the current weak spot).
- **Survey: Agentic Software Issue Resolution with LLMs** (Dec 2025, [arXiv:2512.22256](https://arxiv.org/abs/2512.22256)): canonical five-phase workflow now standard — repo preprocessing → localization → repair → patch validation (reproduction + regression) → patch selection (voting/LLM-judge). Trends: RL-trained resolvers, specialized 32B models, multi-candidate generation + execution-based selection as default.

---

# Appendix C — Spec-driven frameworks & framework-specific tooling

## C.1 GitHub Spec Kit

Artifact chain: `/constitution` → `/specify` → `/clarify` → `/plan` → `/tasks` → `/implement`. 30+ agent integrations.

- `spec.md` — user stories with P1/P2/P3 priorities, each independently testable with Given/When/Then scenarios; `FR-001…` requirements in MUST language; `SC-001…` technology-agnostic success criteria; inline `[NEEDS CLARIFICATION]` markers.
- `plan.md` — Technical Context block; **Constitution Check gate before Phase 0 and re-check after Phase 1**; phased outputs (`research.md`, `data-model.md`, `contracts/`, `quickstart.md`); Complexity Tracking table where constitution violations must be justified.
- `tasks.md` — `[ID] [P?] [Story] Description`; `[P]` = parallelizable; **description must contain exact file paths**; phases Setup → Foundational → user stories by priority → Polish; per-story checkpoints for shippable increments.
- Plan→code mapping: plan.md picks one of three source-tree layouts; tasks hardcode real paths. **Localization decided once at plan time, not searched at implement time.**

Sources: [github.com/github/spec-kit](https://github.com/github/spec-kit) · [spec-driven.md](https://github.com/github/spec-kit/blob/main/spec-driven.md) · templates at `raw.githubusercontent.com/github/spec-kit/main/templates/`

## C.2 AWS Kiro

`.kiro/specs/{feature}/requirements.md` (user stories + **EARS notation** acceptance criteria numbered `1.1, 1.2…`), `design.md` (components, sequence diagrams, data models), `tasks.md` (checkboxes, max two hierarchy levels, each task ends `_Requirements: 1.1, 1.5` — full traceability). Bugfix Specs variant. Execution: per-task click, or bulk — **dependency graph groups independent tasks into "waves"** run concurrently. Phases gated by explicit user approval of each doc.

Sources: [kiro.dev/docs/specs](https://kiro.dev/docs/specs/) · [best practices](https://kiro.dev/docs/specs/best-practices/)

## C.3 BMAD-METHOD

Two phases: (1) Agentic Planning — Analyst → brief; PM → PRD; Architect → architecture; PO checklists. (2) Context-Engineered Development — Scrum Master **shards** PRD+architecture into self-contained story files; Dev implements one story at a time; QA reviews. Story file carries acceptance_criteria, **dev_notes** with previous_insights, extracted data models/API specs, **file_locations from the project structure doc**, testing requirements; subtasks tagged `(AC: #1)`; ≤1 dev-day per task. Sharding = the plan→code mapping mechanism; the Dev agent never re-reads the full corpus.

Sources: [github.com/bmad-code-org/bmad-method](https://github.com/bmad-code-org/bmad-method)

## C.4 claude-task-master (Taskmaster AI)

PRD → `parse-prd` → `tasks.json` (tagged task lists per branch/context; fields: id, title, description, status, priority, details, testStrategy, dependencies, recursive subtasks). `analyze-complexity` scores 1–10 and recommends expansion; `next` picks by dependency topology; dependency validation (no cycles). **No structural plan→code mapping** — localization left to the coding agent; verification = per-task testStrategy prose only.

Sources: [github.com/eyaltoledano/claude-task-master](https://github.com/eyaltoledano/claude-task-master)

## C.5 OpenSpec

Spec-anchored + brownfield-first. `openspec/specs/` = source of truth for **current** behavior by capability; `openspec/changes/` = change packages with `proposal.md`, `design.md`, `tasks.md`, and **delta specs** (`## ADDED / MODIFIED / REMOVED Requirements`). RFC-2119 + `#### Scenario:` Given/When/Then. Lifecycle: propose → implement → **archive** (CLI merges deltas into main specs, preserves decision history).

Sources: [github.com/Fission-AI/OpenSpec](https://github.com/Fission-AI/OpenSpec) · [openspec.dev](https://openspec.dev/)

## C.6 Tessl

Spec-as-source bet: 1:1 spec↔code-file mapping; generated code marked DO NOT EDIT; humans edit only specs. **Spec Registry**: 10,000+ version-accurate library "Usage Specs" installed as dependencies.

Sources: [tessl.io](https://tessl.io/blog/tessl-launches-spec-driven-framework-and-registry/) · [Fowler/Böckeler comparison](https://martinfowler.com/articles/exploring-gen-ai/sdd-3-tools.html) — three SDD rigor levels: spec-first (Kiro, Spec Kit) → spec-anchored (OpenSpec) → spec-as-source (Tessl).

## C.7 Laravel Boost (official)

Four legs, installed via composer: (1) **MCP server** ~15 tools — application info (versions, packages, Eloquent model list), DB schema/query, tinker execution, browser logs, last error, docs search; (2) **AI guidelines** — composable instruction files **selected per installed package AND version**, regenerated by `boost:update --discover`; third-party packages ship their own; app overrides via `.ai/guidelines/`; (3) **Agent Skills** auto-installed from composer.json detection, overridable; explicit guidelines-vs-skills doctrine (upfront broad conventions vs on-demand deep patterns — context-bloat control); (4) **Docs API** — 17,000+ embedded-search entries, version-filtered. Convention knowledge is **generated from the dependency graph**.

Sources: [laravel.com/docs/13.x/boost](https://laravel.com/docs/13.x/boost) · [announcement](https://laravel.com/blog/announcing-laravel-boost)

## C.8 Rails ecosystem

- **claude-on-rails** (Obie Fernandez): swarm of 7 agents mapped 1:1 to Rails directories via `claude-swarm.yml`; Architect coordinates; work routing = the MVC layout itself. [github.com/obie/claude-on-rails](https://github.com/obie/claude-on-rails)
- **rails_ai_agents**: fullest Rails SDD kit — 19 agents/26 commands/18 skills/14 rules/6 hooks; **11 path-scoped rules auto-activating on glob match**; full Spec-Kit-style pipeline + small-change mode (warns if >3 criteria or >6 files); fresh-context subagents per task. [github.com/ThibautBaissac/rails_ai_agents](https://github.com/ThibautBaissac/rails_ai_agents)
- **rails-ai-context**: 38 MCP tools exposing live schema/models/routes. [github.com/crisnahine/rails-ai-context](https://github.com/crisnahine/rails-ai-context)
- **rails-mcp-server** (maquina-app): version-matched Rails docs. [github.com/maquina-app/rails-mcp-server](https://github.com/maquina-app/rails-mcp-server)
- **NextLink Labs analysis**: Rails is "LLM catnip"; "the AI's first guess about where code lives is usually correct"; Ruby #1 cost-efficiency ($0.36/task); CLAUDE.md should document only what's not inferable from convention, <15KB. [Link](https://nextlinklabs.com/resources/insights/claude-code-configurations-every-rails-engineer-should-use)
- **Evil Martians**: conventions as context engineering; "AI Harnesses" open-source skill packs (Layered Rails). [Link](https://evilmartians.com/chronicles/2-martians-greenfield-to-mvp-in-4-weeks-agentic-coding-on-rails)
- **thoughtbot**: `rails-audit-thoughtbot` Claude Skill. [Link](https://thoughtbot.com/blog/ai-in-focus:a-new-claude-skill-for-rails-code-audits)

## C.9 Phoenix / Elixir

- **Tidewave** (Valim/Dashbit): MCP server embedded **in the running app** (Phoenix + Rails) — REPL, logs, DB, docs, routes in live app context. [tidewave.ai](https://tidewave.ai/)
- **usage_rules** (Zach Daniel): hex packages ship `usage-rules.md`; `mix usage_rules.sync` consolidates into AGENTS.md + generates skills; convention knowledge via the package manager (same pattern as Boost). [hexdocs.pm/usage_rules](https://hexdocs.pm/usage_rules/)

## C.10 Shopify Roast

Convention-oriented **workflow orchestration**: YAML workflows + markdown prompts; step types inferred by convention; `if:`/`each:` control flow; built-in CodingAgent tool wraps Claude Code so deterministic orchestration interleaves with agent steps. "Non-determinism is the enemy of reliability." Workflow-hardening for large-scale code operations, not a spec-planner.

Sources: [shopify.engineering/introducing-roast](https://shopify.engineering/introducing-roast) · [github.com/Shopify/roast](https://github.com/Shopify/roast)

## C.11 Django & community packs

Django: fragmented — several MCP servers (django-mcp-server, mcp-django, django-mcp), a Styleguide MCP, community CLAUDE.md guides; no Boost-level version-resolved generation. Community: [wshobson/agents](https://github.com/wshobson/agents) — 194 agents/158 skills across 5 harnesses; framework knowledge is persona-level ("django-pro") rather than structurally exploited.
