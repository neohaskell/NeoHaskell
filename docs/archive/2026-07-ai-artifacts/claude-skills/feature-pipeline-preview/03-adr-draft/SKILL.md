> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 03-adr-draft
description: Drafts the ADR (Status Proposed) and opens a draft PR so the maintainer can review the ADR on GitHub.
kind: leaf
executor: opus
model: claude-opus-4-7
---

# ADR draft

Produces `docs/decisions/NNNN-<slug>.md` matching the NeoHaskell ADR template, with Status: Proposed, then commits it and opens a **draft PR** so the maintainer reviews the ADR on GitHub. The ADR is the input to every later phase (security, perf, devex, architecture, test spec); the draft PR is converted to ready for review at the PR gate (phase 16) — this phase never opens a second PR.

## Inputs

- `adr_number` — 4-digit string. The orchestrator derives the next number by parsing the 4-digit numeric prefix of every `docs/decisions/NNNN-*.md` file, taking the max, and adding one (zero-padded to 4 digits). Empty directory yields `0001`. Non-numeric or short prefixes are ignored.
- `feature_name` — string.
- `issue_number` — string.
- `module_path` — string.
- `branch_name` — string. The feature branch created in phase 1; the draft PR is opened from it.
- `slug` — string. The kebab-case slug used in the ADR filename.

## Plan

1. Read the existing `docs/decisions/` directory → verify: the chosen ADR number is unused.
2. Read the `neohaskell-adr-template` skill if needed → verify: section list matches the template (Context, Decision drivers, Considered options, Decision outcome, Public API, Consequences).
3. Draft the ADR with every required section, every code example following the NeoHaskell style guide → verify: file written and contains `Status: Proposed`.
4. Commit the ADR and open a draft PR → verify: `pipeline.py get pr_url` returns a URL.
5. Mark phase 3 complete → verify: `pipeline.py status` shows phase 3 awaiting approval.

Assumptions:
- `docs/decisions/` exists.
- The feature branch (`branch_name`) was created in phase 1; at this point the working tree contains only the ADR.
- `gh` is on PATH and the repository supports draft PRs.
- The maintainer will review the ADR in the draft PR before approving phase 3.

If any assumption fails (style guide unclear, ADR template missing, `gh` absent), refuse and ask.

## Steps

1. Compute the ADR path: `docs/decisions/<adr_number>-<slug>.md`. Refuse on file existing.
2. Draft each section — keep code examples short, name only the public API the feature introduces, reference `#<issue_number>` once at the top.
3. Apply the NeoHaskell style: pipes, do-blocks, `case ... of`, `Task`/`Result`, no `let..in`, no `where`, no `$`, no single-letter type params. Refuse if the chosen API forces any of these.
4. Apply the Jess persona check (`../references/jess-persona.md`): every public function reachable from the API block should be usable in 15 minutes from autocomplete alone. Refuse the design if it is not.
5. Write the file with `Status: Proposed`.
6. Open the draft PR so the maintainer can review the ADR on GitHub:
   1. `BRANCH=$(git branch --show-current)`. If `$BRANCH = main`, refuse: "cannot open a PR from main".
   2. `command -v gh >/dev/null 2>&1` — if non-zero, refuse: "gh not found".
   3. Stage and commit the ADR (plus any `docs/decisions/README.md` index update): `git add docs/decisions/` then `git commit -m "docs(adr): add ADR-<adr_number> <slug> (#<issue_number>)"`.
   4. Push the branch: `git push -u origin "$BRANCH"`.
   5. Write `.pipeline/draft-pr-body.md`: a short body stating the ADR `docs/decisions/<adr_number>-<slug>.md` is open for review, that implementation lands in later commits, that the PR is marked Ready for review at the pipeline's PR gate (phase 16), and a `Closes #<issue_number>` line.
   6. Open the draft PR (provisional title; phase 16 finalizes it): `URL=$(gh pr create --draft --title "<feature_name> (#<issue_number>)" --body-file .pipeline/draft-pr-body.md)`.
   7. `NUM=$(gh pr view --json number -q .number)`.
   8. Store both: `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py set pr_url "$URL"` and `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py set pr_number "$NUM"`.
7. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 3`.

## Output

- `docs/decisions/<adr_number>-<slug>.md` exists, Status: Proposed, all template sections present, committed.
- A **draft PR** is open against the feature branch; `pr_url` and `pr_number` are stored in pipeline state.
- Phase 3 marked complete; pipeline now in `waiting_for_approval` for phase 3 — the maintainer reviews the ADR in the draft PR.

## Refusals

- ADR path already exists → stop, do not overwrite.
- Current branch is `main` → refuse: "cannot open a PR from main".
- `gh` not on PATH → refuse: "gh not found".
- Design forces NeoHaskell-style violations → surface the conflict; do not silently rewrite the API.
- Feature fails the Jess 15-minute test → surface the failure and request a redesign before drafting.
