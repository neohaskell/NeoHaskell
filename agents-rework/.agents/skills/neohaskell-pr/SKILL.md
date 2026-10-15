---
name: neohaskell-pr
description: Create, update, and manage NeoHaskell pull requests and GitHub stacks, including titles, descriptions, base branches, review follow-up, and verification. Use for PR work across the monorepo; lead descriptions with outcomes understandable to Jess.
---

# NeoHaskell pull requests

Own PR presentation and GitHub stack management across the monorepo. The
[change pipeline](../neohaskell-pipeline/SKILL.md) still owns its applicable
spec/approval gates; [Neo CLI guidance](../../../neo/AGENTS.md) owns Rust
verification. A PR-management request does not waive those gates or authorize
merging, publishing releases, or posting review comments to others.

## Establish the actual PR and stack

Before creating or editing a PR, inspect the current branch, working-tree state,
`gh stack view`, and the live PR's head/base/title/body/draft status. Review the
diff against the owning lower layer, not automatically against main.

- Bottom layer targets `main`; each higher layer targets the preceding branch.
  For example: `main <- hardcore-turkey <- semantic-releases` means the migration
  PR targets main and the release PR targets hardcore-turkey.
- If the lower branch has no PR, create that PR first; reuse existing PRs by head
  branch rather than creating duplicates. Use `gh stack` to create/link the stack.
- Verify the base/head of every affected PR and its changed-file list after
  linking or retargeting. The upper PR must exclude the lower layer's changes.
- A local validator that only accepts main is a tooling limitation. Preserve the
  correct GitHub stack; report the incompatible validation state before advancing
  its gate. Never retarget a dependent PR to main merely to satisfy a validator,
  silently rewrite pipeline evidence, or claim stale evidence is current.
- After a lower layer merges, inspect the live stack before syncing/rebasing.
  Preserve unrelated work; do not force-push or merge as an incidental metadata edit.

## Name the final change

Use `<type>(<optional-scope>): <imperative description>`. Classify the behavior
being delivered by the final PR, including when it is currently a spec-only draft.
Describe that scope honestly in the body; drafting a feature spec does not mean
its implementation exists. Rewrite both title and description when scope changes.

| Change | Title example | Why |
|---|---|---|
| Release workflows and automated publication | `ci(release): publish coordinated releases from main` | Changes how CI ships the product; it is not an application feature. |
| Canonical agent configuration migration | `chore(agents): consolidate repository configuration on Codex` | Repository maintenance and agent setup. |
| Add a user-facing CLI command | `feat(cli): add project validation` | New behavior users can invoke. |
| Fix duplicate events after reconnecting | `fix(events): prevent duplicate deliveries after reconnecting` | Repairs incorrect user-visible behavior. |
| Improve only the written installation guide | `docs(install): explain installation on macOS` | Documentation is the delivered change. |

Use `build` for build tooling/dependencies, `test` for tests, `refactor` for code
reorganization without behavior changes, and `perf` for performance changes.
Mixed PRs use the type of their primary delivered outcome and name material
secondary scope in the body. Automation can be new without being `feat`.

For breaking changes, use `!` (for example `feat(api)!: rename save`) or a
`BREAKING CHANGE:` footer, with honest spec metadata and migration fragments
prepared through [neohaskell-release](../neohaskell-release/SKILL.md). A title's type does not replace the
release-impact declaration or migration guidance.

## Write for Jess first, then the reviewer

Read [Writing for Jess](references/jess-writing.md) before writing PR descriptions
or editing release prose. Lead with one short paragraph stating the concrete
problem, resulting behavior, and why it matters. For internal tooling, describe
the contributor experience honestly rather than inventing an application benefit.

Then include only what helps a reviewer assess the change:

- **What changed:** material behavior and implementation decisions.
- **Verification:** commands actually run and results, with relevant limitations.
  Distinguish fixture checks from live execution and planned tests from passing ones.
- **Migration:** required user action, or an explicit statement when none is needed.
- **Stack/spec links:** dependencies, spec/ADR, and review records where applicable.

Small PRs may need only the opening paragraph and verification. Avoid boilerplate
headings with no content, chronological work logs, and unexplained internal names
in the opening. Link repository files with GitHub blob links at the reviewed head
(or PR file links), not local filesystem paths or ambiguous PR-relative paths.

Example for this repository's release automation:

> Each NeoHaskell release will explain what changed and how to update an existing
> app. Contributors write those notes with their local coding agent, and GitHub
> publishes the reviewed text after the release is merged. No paid AI service is
> needed during publication.

For a spec-only draft, begin "This PR proposes..." and state what has not yet
been implemented or exercised. Keep operational constraints and validation details
below the user-understandable introduction.

## Apply and verify

Use structured tool arguments or a temporary UTF-8 body file with `gh pr create`
/ `gh pr edit --body-file`. Preserve actual newlines and literal code examples;
never interpolate arbitrary Markdown into shell commands.

Before creating, marking ready, or merging, follow the applicable pipeline and
verification gates. A requested title/base/body correction can be performed
without rerunning product tests; re-read live metadata afterward and report the
actual result. Keep draft/ready state unless the task calls for changing it.

Review requests return findings unless posting them was explicitly requested.
When asked to address findings, verify each against the reviewed SHA, make fixes,
run relevant checks, and keep the PR description truthful. Do not treat a green
bot status, a draft's skipped jobs, or a requested wording change as merge approval.

Validation: `./dev neo-skills-check` checks discovery/frontmatter/references;
`./dev doctor` includes that check. These do not prove prose is understandable:
apply the Jess review in the linked guide before considering the text ready.
