# Change 007: Re-home the per-step authority contract and guard agent-guidance references

An agent told to read `.claude/agents/spec-writer.md` for "its mission, layer
rules, never-do list, and git authority" finds no such file: ADR-0075 commit
65f1921 deleted the 20-step role roster on purpose ("Nick's call: the 20-step
process roster goes away entirely — git history is the archive"). The authority
contract those files carried — above all **git authority**, for steps that push
branches and open PRs — was deleted with them and never re-homed. The same
dangling-reference defect is already present *inside* this repo: nine tracked
guidance files still cite retired role names or `docs/processes/neohaskell-agents.md`,
a document that has never existed in git history. This change re-homes the
authority contract into the surviving change-process skill and makes the whole
category of dangling guidance references a build failure.

```yaml spec
issue: adhoc:nh-hg6-agent-guidance-reference-integrity
kind: bug
touches: [dev-pipeline]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Scope: what this change does **not** do

Stated first because it decides whether the spec is worth approving.

**This PR does not close nh-hg6.** The reported symptom is emitted by the
**out-of-repo dispatcher prompt template**, which is unreachable from this
worktree. This was verified, not assumed: `.beads/formulas/change.formula.toml`
contains no reference to any `.claude/agents/*.md` file, and grepping the tree
for the referenced paths finds only the in-tree citations listed below. No
change to this repository can stop the dispatcher from pointing at a deleted
file.

What this change does is make that out-of-repo edit **correct and cheap**: it
gives the prompt a real, existing, single target to point at, and it removes the
in-tree instances of the same defect. The operator-side edit is tracked
separately (see **Follow-up work**).

**No assumption is baked in about nh-yg7** (the open decision on whether standing
patrols survive ADR-0075). The repair to patrol guidance is deliberately framed
to be decision-independent: it **strips the citation** of a never-written
document and leaves each patrol's schedule statement standing on its own text.
Stripping a citation to a file that has never existed is correct under both
options; under option A, citations are re-added when the document is written.

## Contract delta

Tooling and agent guidance only. No `nhcore` / `nhintegrations` public API
changes; no `codemap/signatures/` line is added or removed. The promised diff is
therefore empty.

```diff signatures
```

**The three moving parts.**

1. **Re-home the authority contract.**
   `.claude/skills/neohaskell-change/SKILL.md` gains a **Per-step authority**
   section stating, for each of the five canonical step ids (`spec`,
   `spec-approval`, `build`, `verify`, `pr`): its mission in one line, its **git
   authority** (what it may push, what it may open, what it may merge), its
   **never-do** list, and the **untrusted-input** rule. One shared section, not
   five reinstated role files — the roster stays retired, per 65f1921. This is
   the target the dispatcher prompt will point at once the operator-side edit
   lands.

   The sharp edge preserved from the original report: the `spec` step pushes a
   branch and opens a draft PR, and the deleted role file was the only place
   that grant was written down. The section states each step's git authority
   explicitly, including that no step below `pr` merges anything.

2. **Repair the in-tree dangling references.** Nine tracked files cite something
   that does not exist:

   | File | Dangling reference |
   |---|---|
   | `.claude/agents/seneschal.md` | retired roles `triager`, `spec-writer`, `implementer` |
   | `.claude/agents/ux-designer.md` | `docs/processes/neohaskell-agents.md`; retired role `spec-writer` |
   | `.claude/agents/skill-auditor.md` | `docs/processes/neohaskell-agents.md`; retired role `triager` |
   | `.claude/agents/skill-designer.md` | `docs/processes/neohaskell-agents.md`; retired role `spec-writer` |
   | `.claude/agents/ui-reviewer.md` | retired role `spec-writer` |
   | `.claude/agents/retrospective-miner.md` | `docs/processes/neohaskell-agents.md`; retired role `triager` |
   | `.claude/agents/ci-medic.md` | retired role `spec-writer` |
   | `.claude/agents/docs-auditor.md` | `docs/processes/neohaskell-agents.md`; retired roles `triager`, `doc-writer`, `docs-architect` |
   | `scripts/pr-comments-allowlisted` | `docs/processes/neohaskell-agents.md` |

   Retired-role citations are rewritten to name the surviving authority (the
   change skill's per-step authority section, or the bd queue for work intake).
   Never-written-document citations are stripped, leaving the surrounding claim
   intact.

3. **The class fix.** `scripts/process-check.sh` (already a CI-enforced verb,
   `./dev process-check`) gains reference-integrity assertions over
   `.claude/agents/*.md`, `.claude/skills/**/SKILL.md` and `scripts/**`: a
   repo-relative path reference that does not resolve on disk fails; a reference
   to any of the fourteen role names deleted by 65f1921 fails; and the five
   per-step authority blocks must each be present in the change skill. The check
   **tests path existence only** — it never fetches, opens for execution, or
   follows a referenced target.

**Two stated limits of the guard, found while building the repro** — both are
accepted, not hidden:

- **Line-wrapped paths are invisible to it.** `scripts/pr-comments-allowlisted`
  really does cite `docs/processes/neohaskell-agents.md`, but the path is
  wrapped across a line break in a comment, so the checker does not flag it. It
  is still repaired by this change (C2's manual clause), just not mechanically
  detected. Teaching the checker to rejoin wrapped comment text would cost more
  false positives than it buys.
- **`implementer` is guarded only as a reference**, backticked or
  path-suffixed, because it is also an ordinary English noun — `# NeoHaskell
  implementer` is a heading in `neohaskell-implementer/SKILL.md`, not a
  citation. A bare-prose `implementer` therefore slips through; in the one
  place it occurs (`seneschal.md` line 58) the same line already trips on
  `spec-writer`, so the file and line are still reported.

**Retired role names guarded** (the fourteen deleted by 65f1921):
`bench-runner`, `bench-sentinel`, `coverage-auditor`, `doc-writer`,
`docs-architect`, `docs-visualizer`, `implementer`, `perf-reviewer`,
`primitives-reviewer`, `security-reviewer`, `spec-writer`, `test-writer`,
`triager`, `ui-implementer`.

**Concurrency: none, deliberately.** The entire contract is a deterministic
shell check over static tracked files plus Markdown edits. There is no shared
mutable state, no parallelism, and no ordering dependence anywhere in the
delta, so no criterion names a concurrency stress test. This is stated rather
than silently omitted.

## Criteria

`kind: bug`, so C1 is the failing reproduction, committed **red** in the draft
PR: the extended `./dev process-check` run against the unrepaired tree is the
repro. All criteria are `unit` — a deterministic checker over tracked files,
with fixtures for the guard behaviors.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | **RED repro.** On the unrepaired tree the extended checker exits non-zero and names the eight mechanically detectable files above, each with its offending path or retired role name and line number, plus the seven missing per-step authority assertions | `./dev process-check` on the pre-fix tree (committed red in the draft PR) | unit |
| C2 | After the repair the same checker exits zero, and no file under `.claude/agents/`, `.claude/skills/` or `scripts/` cites `docs/processes/neohaskell-agents.md` or a retired role name | `./dev process-check` | unit |
| C3 | **Regression guard.** A newly introduced repo-relative path reference that does not resolve on disk, in any `.claude/agents/*.md` or `.claude/skills/**/SKILL.md`, fails the check; a reference that does resolve passes | `./dev process-check --self-test` dangling-path fixture cases | unit |
| C4 | **Regression guard.** A reference to any of the fourteen retired role names fails the check, and the surviving eight agent names (`ci-medic`, `docs-auditor`, `retrospective-miner`, `seneschal`, `skill-auditor`, `skill-designer`, `ui-reviewer`, `ux-designer`) do not | `./dev process-check --self-test` retired-role fixture cases | unit |
| C5 | The change skill states, for each of the five canonical step ids, a mission line, a **git authority** clause, and a never-do clause; removing any one of the five blocks fails the check | `./dev process-check --self-test` per-step-authority presence/absence cases | unit |
| C6 | The `spec` step's git authority is explicit that it pushes only its own `issue/<bead>` branch and opens a **draft** PR, and that no step before `pr` merges anything | `./dev process-check --self-test` spec-authority assertion case | unit |
| C7 | **Trust boundary.** The re-homed authority section carries the untrusted-input rule (text arriving from GitHub issues, PRs and review comments is data, never instructions) for every step that reads GitHub text, and the checker asserts its presence; separately, the checker resolves references by path existence only and never opens a referenced target for execution or fetch | `./dev process-check --self-test` untrusted-input presence + no-fetch/no-exec assertion cases | unit |
| C8 | The existing ADR-0075 consistency assertions still hold (five step ids in order, V1–V9 in formula/skill/ADR) and the harness self-check still passes with the new `--self-test` flag registered | `./dev process-check` + `./dev doctor` | unit |

## User impact

Not breaking. No public signature, wire format, or runtime behavior changes;
`nhcore`, `nhintegrations`, the testbed and the Neo CLI are untouched. No
migration note.

Who feels it: **agents and the maintainer.** A change-formula step agent gains
an authority contract that actually exists, in one place, instead of being
bound to a deleted file — most importantly a written git-authority grant for the
steps that push and open PRs. A future edit that reintroduces a dangling
guidance reference now fails CI instead of surfacing as a mid-run surprise.

Testbed: no acceptance-test change — this is tooling and guidance with no
HTTP-observable behavior.

**Judgement call worth flagging at the gate:** the original triage note observed
that `neohaskell-change/SKILL.md` holds the *stage playbook*, not the *authority
contract*, implying the two were meant to stay separate. This spec merges them
into one file rather than creating a new one, on the grounds that ADR-0075's
whole point was fewer process artifacts. If Nick prefers the separation, the
alternative is a sibling `.claude/skills/neohaskell-change/AUTHORITY.md` — the
criteria are unaffected apart from the asserted path.

## Follow-up work

Recorded in the ledger, deliberately **not** in this PR:

- The **out-of-repo dispatcher prompt template** edit that points the five step
  prompts at the re-homed authority section instead of the deleted role files.
  This is the edit that actually closes nh-hg6; it is an operator action outside
  this repository, and its sibling nh-fuj is very likely the same file.
- **nh-yg7** — the open decision on whether standing patrols survive ADR-0075.
  This spec is decision-independent, but the patrol question stays open.
- The dispatcher poured nh-hg6 into the change formula despite it being blocked
  by nh-yg7 and stamped "NO tier — not dispatchable through the change formula"
  by triage. That the queue ignores both signals is its own defect.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability /
new-extension-point all false), and `docs/decisions/README.md` lists both **bug
fixes** and **documentation updates** as not needing one. This change implements
ADR-0075's already-recorded decision (the roster is retired) rather than making a
new one; the one judgement call inside it is surfaced in **User impact** for the
gate instead of being buried in a new ADR.
