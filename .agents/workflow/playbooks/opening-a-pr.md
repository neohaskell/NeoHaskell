### Opening a PR

Invoked at the end of every other playbook. Apply the [task completion policy](../../skills/orchestrate/references/task-completion.md) to determine what follows PR creation.

**Worktree.** Work from a git worktree off main. Subagents inherit it. Multiple `Task` calls on the same branch each get their own worktree, or `git fetch && git reset --hard origin/<branch>` between them. Dirty branch with unrelated work: patch out, fresh worktree, apply. Snarled worktree: reset from main, redo minimally.

**Commits.** Commit liberally. Rebase into small, ordered commits before opening PRs. Each commit is a future PR: landable, ordered to tell the story. Amend when the fix belongs in a just-made commit. New commit when separable. Commit bodies don't restate the subject.

**PRs.** Run `/deslop` over the diff before commit and `/no-comments` before review. Write every PR title, description, and commit body with `/technical-writing` (every layer except Diátaxis, and apply the STE vocabulary rules: one word per action, keep articles, a plain verb over `-ing` where one works), then apply the **prose-cleanup** skill.

**Source-owned agent guidance.** For a PR that changes a consumer-facing framework contract, generated CLI behavior, or `docs/agent/`, complete the [local guidance review](../../skills/neohaskell-consumer-verification/SKILL.md#source-owned-agent-guidance) before review. Report its result in the PR verification section.

**Public feature evidence.** For a developed public NeoHaskell feature, name the working testbed example, maintained automated assertion, and exact run and test commands in the PR verification section. Follow [NeoHaskell consumer verification](../../skills/neohaskell-consumer-verification/SKILL.md#framework-changes-include-their-consumers). For internal-only work, state why this requirement does not apply.

**Release notes.** Before final substantive PR review, follow [neohaskell-release](../../skills/neohaskell-release/SKILL.md) for change fragments, breaking-change migrations and their validation.

**Titles.** Conventional Commits `type(scope): subject`, no trailing period. Type such as `feat`, `fix`, `docs`, `refactor`, `test`, `chore`, or `perf`. Scope is the area, such as `core`, `service`, or `neo`. Subject is short, imperative, and names a real symbol when one carries the change, for example `fix(service): preserve CommandExecutor request context`.

**Descriptions.** The PR body is a one-minute briefing, not a lab notebook, for a reviewer who has the diff. It says why the change exists, what it leaves out, what it could break, and how you proved it works. Write short, simple sentences with few identifiers, no walls of text. The body becomes the squash commit body, so keep that commit under about 40 lines. Use `##` headings, not bold lead-ins, in this order, and drop a section with nothing to say:

- `## Why`: the problem and approach, 1–3 short sentences. No SHAs, rebase genealogy, or "based on main" preamble.
- `## What changed`: 1–3 short bullets. Name a real symbol or path only when it carries the change. Name both sides of a rename or retarget.
- `## Scope`: always. 1–3 short items on what it covers and what it deliberately leaves out (a follow-up, a known gap). No symbols, paths, or file-by-file essay.
- `## Tradeoffs`: only rejected alternatives a reviewer would otherwise ask about. Skip when there was no real choice.
- `## Blast Radius`: 1–2 sentences on who or what it touches and why that is safe or risky. If main is red, state the cost of leaving it red.
- `## Verification`: 1–3 bullets, each a real run path and its outcome. Perf: one primary number with its unit, `before → after`, and a link to the arena or swarm directory for the rest. No sample-size methodology, swarm recitals, or metric tables.

After the sections, attach videos or screenshots when they prove a claim. Full SHAs, swarm or arena lane recitals, lever-correction essays, file-by-file checklists, and "CLEAN" verdicts go in a linked artifact, not the body.

**Forge.** Confirm the repository before the first PR operation. GitHub CLI (`gh`) is the default. Follow `AGENTS.md` for available PR tooling and keep the selected interface consistent for the operations it supports.

**Built-in PR tool.** When the run provides an integrated PR tool, use its supported interface for create, edit, retarget and ready state, preserving any tracking it supplies. Use `gh` for operations it does not cover, and for every PR operation when no integrated PR tool is available.

**Size and stacks.** Prefer narrow PRs with verifiable boundaries. Follow the [task completion policy](../../skills/orchestrate/references/task-completion.md#represent-dependencies-as-a-real-pr-stack) for dependency bases, current root/tip links, capability discovery, and topology ownership. Create a dependent PR with the supported `gh pr create --base <parent-branch>` operation when no integrated tool owns it. The topology owner can retarget with `gh pr edit <pr> --base <parent-branch>`. As a parent lands, reconcile its child through Shipping step 4. Preserve the existing authorization for history changes.

**Readiness.** Open PRs ready unless the user explicitly requests a draft. With an integrated PR tool, use its supported ready-state controls and verify the result. With `gh`, use `--draft` only for a requested draft. If an unintentionally drafted PR opens, mark it ready through the PR tool or run `gh pr ready <number>`. Preserve a requested draft. Read the PR back through the available tool or `gh pr view <number>` before you refer to its status.

**Continuation.** PR creation is a checkpoint under the [task completion policy](../../skills/orchestrate/references/task-completion.md). Finish the current implementation wave, then the task owner continues through Babysit and Shipping when landing is in scope. Do not wait for a second user request. A delegated worker returns its exact head and evidence to the coordinator. That return does not end the coordinator's task.

A subagent that opens a PR runs `interrogate`, `/deslop`, and `/no-comments` within the caller's scope, and reports the URL. It supervises or lands only when that ownership is explicitly assigned. Autopilot owners retain their assigned loops and delivery boundaries.
