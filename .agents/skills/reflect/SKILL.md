---
name: reflect
description: Spawn three parallel review subagents over the active transcript, surface learnings, and route each to a concrete edit on an existing skill. Use when the user says reflect.
---

# Reflect

Mine the current conversation for durable learnings, then route them into skill edits.

Invoke when the user says "reflect" or "/reflect". Skip when the conversation is trivial, off-topic, or already covered by an existing skill the parent followed correctly. One-offs are not learnings.

## 1. Locate this conversation's evidence

Use the active environment's transcript or conversation-history interface to identify this conversation. Read only its evidence. If a complete transcript is unavailable, pass a tight digest of the accessible session instead. State its coverage limits and do not invent missing turns or present the digest as a complete transcript. Keep any intermediate digests and review artifacts under `.agents-state`.

## 2. Spawn three reviewers in parallel

Delegate to three reviewers with independent judgment, tooling and divergent lenses. Follow the delegation instructions in `AGENTS.md`. Run them in parallel where capacity permits; if capacity is limited, batch the work without dropping a lens. Give them the session evidence and read access to the referenced context sources. They return findings without modifying files, skills or external records.

| Lens | Prompt template |
|---|---|
| Judgment | `references/judgment-reviewer.md` |
| Tooling | `references/tooling-reviewer.md` |
| Divergent | `references/divergent-reviewer.md` |

Pass each template verbatim, substituting the transcript path or digest where marked. Reviewers return findings in their response bodies. Pass along any evidence-coverage limits.

## 3. Synthesize

Delegate to a separate synthesizer following `AGENTS.md`. It may spot-verify citations using available read-only context tools. Pass `references/synthesizer.md` verbatim, with each reviewer's full output inlined where marked, plus any evidence-coverage limits. It returns an Accepted / Rejected / Backlog list without modifying files or external records.

## 4. Structural enforcement check

Move any Accepted item that a Haskell type, owning API, lint rule, script, metadata flag, or runtime check would enforce more reliably to Backlog. See the **encode-lessons-in-structure** principle skill.

## 5. Apply

Present the synthesizer's full Accepted / Rejected / Backlog output and wait for explicit approval before applying any Accepted edit. The user picks the subset and may redirect routings. Skill changes affect future runs that use those skills. Do not auto-apply.

File each Backlog item to your team's devex or backlog tracker without waiting. Only the Accepted list waits for approval.

Follow each approved row's Routing exactly:

- Trivial existing-skill edit (a one-line bullet, a tightened sentence, a stale fact corrected): the parent does it directly.
- Substantive existing-skill edit (a new section, a new pattern table, more than ~10 lines): use the skill-authoring capability available in the current environment and follow its applicable authoring and validation workflow.
- `tune description: <skill path>` (the skill exists but didn't trigger when it should have): use the available skill-authoring capability to improve the description and validate its triggering behavior with the facilities it provides.
- `new skill: <kebab-name>`: use the available skill-authoring capability. Do not invent the shape ad hoc.

If your environment ships a SKILL.md validator, run it on every touched skill before declaring done. Report any unavailable or unsupported checks; do not claim they passed.

## 6. Summarize for the user

Short list, no preamble:

- Edits applied: `<skill path>`. What changed, one line each.
- New skills created: `<skill path>`. One line each (rare).
- Backlog filed to the devex tracker: `<issue title>` (`<tags>`). One line each.
- Dropped: one line per rejected finding + reason from the synthesizer.
