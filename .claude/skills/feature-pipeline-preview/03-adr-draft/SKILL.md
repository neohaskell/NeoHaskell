---
name: 03-adr-draft
description: Drafts the Architecture Decision Record for the feature in Status Proposed.
kind: leaf
executor: opus
model: claude-opus-4-7
---

# ADR draft

Produces `docs/decisions/NNNN-<slug>.md` matching the NeoHaskell ADR template, with Status: Proposed. The ADR is the input to every later phase (security, perf, devex, architecture, test spec).

## Inputs

- `adr_number` — 4-digit string. The orchestrator picks the next number with `ls docs/decisions/*.md | tail -1` before invoking this leaf.
- `feature_name` — string.
- `issue_number` — string.
- `module_path` — string.

## Plan (Karpathy 1 + 4)

1. Read the existing `docs/decisions/` directory → verify: the chosen ADR number is unused.
2. Read the `neohaskell-adr-template` skill if needed → verify: section list matches the template (Context, Decision drivers, Considered options, Decision outcome, Public API, Consequences).
3. Draft the ADR with every required section, every code example following the NeoHaskell style guide → verify: file written and contains `Status: Proposed`.
4. Mark phase 3 complete → verify: `pipeline.py status` shows phase 3 awaiting approval.

Assumptions:
- `docs/decisions/` exists.
- The maintainer will review the ADR before approving phase 3.

If any assumption fails (style guide unclear, ADR template missing), refuse and ask.

## Steps (Karpathy 2 + 3)

1. Compute the ADR path: `docs/decisions/<adr_number>-<slug>.md`. Refuse on file existing.
2. Draft each section — keep code examples short, name only the public API the feature introduces, reference `#<issue_number>` once at the top.
3. Apply the NeoHaskell style: pipes, do-blocks, `case ... of`, `Task`/`Result`, no `let..in`, no `where`, no `$`, no single-letter type params. Refuse if the chosen API forces any of these.
4. Apply the Jess persona check (`../references/jess-persona.md`): every public function reachable from the API block should be usable in 15 minutes from autocomplete alone. Refuse the design if it is not.
5. Write the file with `Status: Proposed`.
6. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 3`.

## Output

- `docs/decisions/<adr_number>-<slug>.md` exists, Status: Proposed, all template sections present.
- Phase 3 marked complete; pipeline now in `waiting_for_approval` for phase 3.

## Refusals

- ADR path already exists → stop, do not overwrite.
- Design forces NeoHaskell-style violations → surface the conflict; do not silently rewrite the API.
- Feature fails the Jess 15-minute test → surface the failure and request a redesign before drafting.
