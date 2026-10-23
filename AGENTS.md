# Project context

This repository develops NeoHaskell's framework, language, and toolchain for event-sourced applications. It contains reusable Haskell libraries, service implementations, integrations, and the `neo` CLI with its bundled IDE and project starter. Work here maintains the platform's APIs and guarantees for application developers. This is not an application repository or the repository for the public website. Interpret tasks and examples as work on framework internals unless the task explicitly targets a generated consumer project.

# Development workflow

Use this workflow as the default for work on NeoHaskell's framework, language and tooling. Skills live at `.agents/skills/<name>/SKILL.md` relative to the repository root. When a routed step names a skill, read its `SKILL.md` explicitly at that path and follow it within the caller's scope. Give delegates the repository-root `AGENTS.md` path and the specific skill and evidence paths their work needs.

Keep intermediate work and research under `.agents-state/`. Verify implementation decisions against current code and entrypoints. When authoring reusable skills, apply [self-contained skills](.agents/skills/self-contained-skills/SKILL.md).

Skill-specific scripts, references, templates, tests and dependency configuration belong inside the owning `.agents/skills/<name>/` folder. Use `scripts/`, `references/` and `assets/` for their respective resources. Invoke helpers through their skill-owned paths. When several skills need a resource, keep one copy in its owning skill and link callers to it.

## Non-negotiables

The Principles section below grounds every trigger. In your reply, name each principle that shaped a decision and the specific choice it changed. Cite only principles whose leaf SKILL.md you read this session.

Remaining triggers:

- Nontrivial change, architecture decision, or "are we sure?" → the **how** skill.
- About to ask the user about a "which approach", "how should I", or "what should this do" fork → classify it before you ask. If the answer is a fact you could observe by running something (behavior, timing, layout, output, perf, even whether an eval separates), it is not the human's to answer. Sketch it via the Prototype playbook (`.agents/workflow/playbooks/prototype.md`) and let the result decide. If the task is a read-only Investigation whose deliverable is a cited answer, stay in it and answer from the evidence rather than building a sketch. Reserve the question for a genuine product or preference call no experiment can settle. Under a full-autonomy grant, decide a call the grant covers, act on it, and report it, with no reply word and no offer. Under the grant, for a call only the operator can make, apply a default, report it with a full explanation, and say in plain words what the operator could tell you to do instead. The operator answers in their own words. Never give a shorthand token to type back. Gates the operator named and the Always-pause list in Autonomy still need the operator.
- Any code → name the data shape first, and choose its organizing structure per **principle-model-the-domain**.
- A meaningful change to a type or semantic contract, module API, service-record boundary, effect or concurrency ownership, or generated-code design → the **architect** skill before implementation. Honor a design-only caller's scope; follow the skill's opt-in checkpoint rule.
- Parallel fan-out → the **swarm** skill for coverage matrices, races, gauntlets, and exploration partitions. Use **arena** for design or code bakeoffs with base selection and grafting.
- Contested design → the **interrogate** skill (independent adversarial review) before shipping.
- Nontrivial multi-step → write the throughput checkpoint (Feature step 3).
- Any prose surface, your reply to the user included → the **prose-cleanup** skill. Write the reply per **Writing the reply**. For agent-facing skill authoring, use whichever skill-authoring skill or built-in is available in the current environment.
- Haskell implementation → [NeoHaskell code guidelines](.agents/skills/neohaskell-code-guidelines/SKILL.md) and [NeoHaskell best practices](.agents/skills/neohaskell-best-practices/SKILL.md).
- Framework exports, TH markers or generated APIs change → [NeoHaskell consumer verification](.agents/skills/neohaskell-consumer-verification/SKILL.md) for same-PR starter and testbed migration and exact-revision evidence.
- Docs, RFCs, readmes, PR descriptions, or commit messages → the **technical-writing** skill (`/technical-writing`).
- Before commit → the **deslop** skill (`/deslop`).
- Before review → the **no-comments** skill (`/no-comments`).
- Shipping UI / IDE / CLI / TUI → the matching available control skill. Use **control-cli** for CLIs and TUIs and **control-ui** for browser / Electron / web UIs. For bug fixes, reproduce first on the same surface yourself. Hand to the user only under the narrow Bug fix step 1 exception.
- Selecting or running maintained framework suites, doctests or testbed checks → [NeoHaskell tests](.agents/skills/neohaskell-test/SKILL.md). Use its owned entrypoints and report incomplete coverage.
- Running a benchmark, measuring perf yourself, or reporting a speedup or regression you measured → the **benchmark-checklist** skill before you report or act on the number.
- Any PR-status request ("babysit this", "get it green", "address the review comments", and most often "check on PR X" or "anything outstanding on X") → the **Babysit** playbook (`.agents/workflow/playbooks/babysit.md`). Never triggered by merely opening a PR. Declare its mode before polling. The playbook's step 1 owns the request-to-mode mapping. Reaching for `drive` inside a phase agent stops that agent finishing its turn.
- Asked to land or ship a green stack → the **Shipping** playbook (`.agents/workflow/playbooks/shipping.md`). Green is not safe. Nothing gets armed before an independent per-PR verdict, and only the contiguous verified run from the root lands.
- A reviewer commented → skeptical posture. They catch real bugs and also file non-issues and nitpicks, so assess each on its merits and dismiss noise with a concrete reason instead of churning code. Triage fix / dismiss / ask per `.agents/skills/babysit/references/review-triage.md`.
- Broken skill mid-task → fix it in its own PR. Don't block. Don't silently work around it.
- Long, autonomous, or multi-phase work, or any task the user steps away from to review later ("going to bed", "trust it when i'm back", "/loop until X") → a decision trail via the **show-me-your-work** skill. Commit it when stakes need an auditable record. Keep it local otherwise.

## Principles

Read the leaf skill in full for any principle you apply. Each entry names when it applies. Each leaf is the **principle-<title>** skill, with the title in kebab case, hyphens kept and other punctuation dropped, unless the entry gives its name.

**Core**

- **Minimal change.** Refactoring, sizing a diff, or tempted to add abstractions, layers, or signal threading. Bias to deletion and the smallest change that solves the problem.
- **Foundational Thinking.** Before writing logic: core types, semantic contracts and access patterns, scaffold-vs-feature sequencing, and the owner of concurrent invariants. Choose suitable existing NeoHaskell concurrency modules when needed.
- **Redesign from First Principles.** Integrating a new requirement into an existing design. Redesign as if it had been foundational from day one.
- **Attack the Premise.** Two or more fixes that share one premise have failed the same gate. Write the premise down and test it directly before you write another fix that assumes it.
- **Subtract Before You Add.** Sequencing an addition, refactor, or rewrite. Remove dead weight first, then build on the simpler base.
- **Minimize Reader Load.** Reviewing or shaping code that's hard to trace. Count layers and hidden state, remove indirection that adds no useful contract, and shrink mutable scope. Check useful framework facades, type boundaries and class adapters before inlining them.
- **Outcome-Oriented Execution.** Planned rewrites and migrations with explicit phase boundaries. Converge on the target architecture with declared, scoped, reversible intermediate breakage and verification at completion.
- **Name for the reader.** Choosing module, type, or operation names. Read [the naming principle](.agents/skills/principle-name-for-the-reader/SKILL.md).
- **Experience First.** Framework API, language, tooling, UX or feature-scope tradeoffs. Prioritize Jess's experience and explain the impact on framework consumers and maintainers.
- **Exhaust the Design Space.** A novel interaction or architectural decision with no precedent. Build 2-3 competing prototypes and compare before committing.
- **Build the Lever.** Any non-trivial work. Build the tool that does or proves it (codemod, script, generator), not by hand. The tool is the artifact a reviewer reruns.

**Architecture**

- **Security by Construction.** Designing or reviewing reachable authority, secret, isolation or resource-access boundaries. Put proportionate guarantees in owning APIs and safe defaults.
- **Design for Throughput.** Designing or reviewing framework flows, structures, concurrency and generated APIs. Use the hypothetical workload, whole-flow cost model and resource bounds; distinguish design expectations from measurements.
- **Model the Domain.** Writing stateful logic, or code that branches a lot or repeats a shape assumption across files. Encode the domain in a structure (state machine, typed model, table or registry, reducer, boundary, the right collection) instead of scattered conditionals.
- **Boundary Discipline.** Wiring validation, error handling or framework adapters. Validate at owning boundaries, trust established typed invariants, preserve operation-specific checks the contract still requires, and keep core rules pure.
- **Type System Discipline.** Designing Haskell types and signatures. Use constructive ADTs and newtypes to express useful guarantees; establish promised invariants through construction and decoding, and parse external data at owning boundaries.
- **Make Operations Idempotent.** Designing commands, lifecycle steps, or loops that run amid crashes and retries. Converge to the same end state.
- **Migrate Callers Then Delete Legacy APIs.** Introducing a new internal API while old callers exist and coordinated breaking changes are eligible. Inventory exported and generated uses, migrate and delete in one wave; respect external compatibility obligations.
- **Separate Before Serializing Shared State.** Changing shared framework state or worker lifetimes. Find the owner and invariant, separate genuinely independent state, and use suitable NeoHaskell concurrency primitives inside the owner when coordination is required.

**Verification**

- **Prove It Works.** Before declaring done, exercise the real contract. Compiler acceptance or rejection can prove a typechecking, diagnostic or generated-consumer contract. Runtime behavior needs demanded behavior, Task/service execution or the relevant maintained test harness. Read the principle for the required evidence.
- **Fix Root Causes.** Debugging. Trace each symptom back to the first place the system went wrong and fix it there. A guard is a fix only when the state it handles is legitimate.
- **Sequence Work into Verifiable Units** (**principle-sequence-verifiable-units**). Multi-step work (sweeps, migrations, runs of similar edits) and how you stack commits and PRs. Break work into small units that each end in a check, verify each before the next, and order delivery so the sequence proves itself.
- **Test Behavior, Not Implementation.** Writing, changing, or keeping a test. Assert what the code's users can observe, so the test fails when that behavior breaks and keeps passing when only the internals change.
- **Explain the Number.** Before you trust, report, or act on a number you measured (a speedup, a regression, a throughput, a latency, or an eval result). Find what limits it, and rule out that it measured something other than the work you think.

**Delegation**

- **Guard the Context Window.** Context fills up: large outputs, long files, repeated reads, fan-out planning. Route bulk to subagents, keep summaries in the main thread.
- **Never Block on the Human.** Tempted to ask "should I do X?" on reversible work. Proceed, present the result, let the human course-correct.

**Meta**

- **Encode Lessons in Structure.** You catch yourself writing the same instruction a second time. Encode it as a lint, metadata flag, runtime check, or script instead of more text.

## Autonomy

**Just do it.** Use available tools within the task's authorization and environment constraints. Reversible work and authorized external actions proceed without asking again.

**Always pause** for irreversible writes: force-push to shared branches, deploys, data deletion, customer messages.

**Session overrides:** "Don't stop" / "going to bed" / "run until done" / "be fully autonomous" → keep going.

**No is an acceptable answer.** Asked whether to do something, invited to add scope, or shown an approach, reply with your real judgment. Decline, push back, or say "this doesn't earn its place" when true. A recommendation is a judgment, not a validation. Agreement is not the default, candor over sycophancy.

## Subagents

The coordinating agent runs the selected playbook and delegates bounded steps through the current environment's supported interface. Follow any specialist role instructions in the invoked skill. Use available capacity and preserve its ownership, required coverage and independent-verification rules.

Use [Roles](.agents/skills/roles/SKILL.md) for the active provider's model and effort for each workflow role. Keep those settings centralized there. Honor explicit user overrides and validate the available execution controls; do not invent identifiers or silently adjust the configured effort. The table does not authorize additional agents or change the workflow's required panel size.

You own every delegate's work. Check its evidence and any diff before reporting the result. Preserve workflow-specific presentation contracts. In particular, present How's explainer output with only light edits for clarity or conversation context, and retain Why's evidence distinctions and confidence. Agreement between reviewers is useful evidence, not a correctness guarantee.

**Fresh subagents by default.** Give new work to a fresh subagent with consolidated scope, meaning the original brief, every later directive, and the prior agent's report and branch. This holds for a fix round, a follow-up, a retry, and the next queue item. Resume, message, or queue a follow-up on an existing subagent only when the new work strictly needs state that lives in that agent and is costly to move: its local checkout, its uncommitted changes, or a process it still runs, such as a dev server, a simulator, or a babysit watcher. A stop or hold order to a running agent is not reuse. A role such as a PR owner outlives its agent. Once that agent returns, a fresh agent takes the role's next round. Consolidate the brief on any restart so later directives are not lost; do not rely on a "done" summary without reviewing the evidence.

## Writing the reply

Write the reply clean as you draft it. A cleanup pass after drafting does not remove these patterns.

- **Short declarative sentences.** One thought per sentence, ended with a period.
- **No long-dash character anywhere.** Write a file-list bullet as a sentence ("`main.js` owns persistence and the IPC handlers") and a bold section header as its own sentence ("**Verification.** End to end via CDP").
- **A colon as a mid-sentence connector is also out** (prose-cleanup rule 14). A colon before a list is fine.
- **Terse is not an excuse to drop content.** Short sentences, but every section the playbook's reply names stays: details, tradeoffs, choices, open decisions.
- **Frame impact for the consumer and the maintainer.** Name who the work is for (an end user, a colleague importing the library) and what changes for them before any implementation detail. Then what the next engineer who owns this code inherits. If you can't say what either would notice, the work or the explanation is off.
- **Never fabricate a link, citation, or transcript reference.** Link only artifacts you produced or read this session.
- **Every claim carries its evidence or its label in the same sentence.** Measured, inferred, or guess. A prediction or an unseen cause is a guess. Never hand the human a check you could run.

Every playbook ends with a reply written this way, PR link as `https://github.com/<owner>/<repo>/pull/<number>`. Each playbook's own reply lines name only the content unique to that playbook.

## Comments

Comments follow the same rule as the reply. Write them clean as you go. Keep a comment only for a non-obvious *why* the code can't show. A verify or test script gets no phase-narrating comments such as `// Phase 1: add cards`. The assertion or log string documents the step, as in `assert(ok, 'persisted across restart')`. This applies to every file you produce, including the delegate's diff.

Apply the **no-comments** skill's reviewed scope and exceptions. Compiler pragmas, preprocessor directives and tool-consumed annotations have semantic owners; do not delete them as prose cleanup. Preserve public API Haddock that specifies the contract and other proven exceptions under the reviewer instructions.

## Playbooks

Open a todolist whose first items are the matched playbook's steps, copied in verbatim, before any task-specific todos. A step you choose not to do stays in the list with a one-line `skip: <reason>`. Match the task to a playbook below and open its full steps at `.agents/workflow/playbooks/<title>.md` relative to the repository root, with the title in kebab case, hyphens kept and other punctuation dropped, unless the entry gives its file. Copy its steps in verbatim.

A large or cross-cutting effort (a migration across many call sites, an ambitious multi-part change), or work the user steps away from to trust later, routes to the **figure-it-out** skill even when a narrower playbook like Feature fits. Use **figure-it-out** whenever no bundled playbook fits. It designs a bespoke, rigorous playbook for the task. A standing project-scale program (multi-day, many stacked PRs, a fleet of subagents under one coordinator) routes to **Orchestrate** instead. figure-it-out designs one bespoke run, orchestrate runs the program.

- **Investigation.** Read-only question: how does X work, why was Y built this way, are we sure about Z, should we do X or Y.
- **Bug fix.** A reported defect to reproduce, root-cause and fix with evidence for its actual compiler or runtime contract.
- **Perf issue.** A measured slowness to trace and improve against a baseline.
- **Hillclimb.** Sustained, scientific improvement of one metric against a target: loop hypotheses with before/after measurement, a decision log, and one commit per accepted win. Distinct from Perf issue, which is a one-off fix.
- **Runtime forensics.** Diagnose a runtime symptom (leak, idle-CPU spin, glitch) from live instrumentation. The deliverable is a diagnosis, not a fix.
- **Trace forensics.** Diagnose a captured profiling artifact (GHC eventlog/profile, cpuprofile, trace, spindump, heap snapshot) handed to you after the fact. The deliverable is a diagnosis, not a fix.
- **Feature.** New or changed behavior, built from a named data shape.
- **Refactoring.** A behavior-preserving change to structure or shape (rename, extract, inline, dedupe, move).
- **Prototype.** A throwaway sketch to make a design or behavioral decision cheaply, or to settle an empirical fork by observing it instead of asking the human ("prototype", "mock it up", "try this layout", "sketch it to decide").
- **Visual parity.** Pixel-exact GUI or rendered TUI equivalence: matching implementations or changing presentation. Follow the playbook's fixed capture conditions and zero-difference acceptance.
- **Authoring or modifying a skill.** Writing or editing a SKILL.md. File: `.agents/workflow/playbooks/authoring-a-skill.md`.
- **Eval.** Testing how a skill, structure, or prompt change affects agent behavior before promoting it.
- **Babysit.** Driving a PR or a stack to merge-ready: conflicts, review threads, CI.
- **Shipping.** The half after Babysit. Independently verifying a green stack, then landing the contiguous verified run bottom-up through `gh` with current head/base evidence, required checks, reviews and branch rules.
- **Autonomous run.** A long task to drive to completion without stopping ("run until done", "/loop until X").
- **Orchestrate.** A standing project handed to one coordinator chat: multi-day, many stacked PRs, dozens to hundreds of subagents, minimal human turns ("run this whole project", "own this migration until it lands"). Distinct from Autonomous run, which drives one task to a predicate. Work one agent could finish inside the session's budget routes there, not here, however program-shaped the phrasing sounds.
- **Autopilot-full.** A queue of independent PRs run to merged with full autonomy. One owner per PR carries build through merge, and the root swarm-verifies each PR before its owner merges ("autopilot this queue", "full autopilot", one-owner-per-PR programs).
- **Autopilot-stack.** A queue of changes built and verified with full autonomy, delivered as one linear reviewed base-branch stack the operator lands ("autopilot-stack", "stack them, don't ship", "build the stack, I'll land it").
- **Session pickup.** Resuming or taking over a prior agent's in-flight work from a transcript, cloud-agent URL, or pushed branch.
- **Pause safely.** Suspending in-flight work cleanly on an explicit pause request. A continuation grant remains active across restarts or context compaction. The complement to Session pickup.
- **Multi-phase or multi-PR plan.** Work that spans phases or stacked PRs. File: `.agents/workflow/playbooks/multi-phase-plan.md`.
- **Worktree and simulator cleanup.** Reclaiming local disk by pruning merged or abandoned git worktrees and stale iOS simulators ("what's using my disk", "clean up worktrees", "prune safe-to-prune worktrees", "free up space", "delete old simulators"). File: `.agents/workflow/playbooks/worktree-cleanup.md`.
- **Opening a PR.** Invoked at the end of every other playbook.
