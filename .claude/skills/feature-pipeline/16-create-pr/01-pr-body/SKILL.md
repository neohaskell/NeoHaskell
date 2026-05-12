---
name: 01-pr-body
description: Writes the PR body to a file with a conventional-commit title and a checklist.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# PR Body

Writes `.pipeline/pr-body.md` with the PR title, description, checklist, and references.

## Inputs

- `.pipeline/adr-draft.md`
- `.pipeline/findings-04.json`, `.pipeline/findings-05.json`, `.pipeline/findings-12.json`, `.pipeline/findings-13.json`
- `.pipeline/classification.json` — contains issue number and slug.

## Plan (Karpathy 1 + 4)

1. Read the ADR, all four findings files, and classification → verify: all exist.
2. Draft a conventional-commit title → verify: matches `<type>(<scope>): <subject>`.
3. Draft a body addressed to Jess covering ADR, changes, security, performance, tests, hlint → verify: every section present.
4. Write `.pipeline/pr-body.md` and a separate `.pipeline/pr-title.txt` → verify: both exist.

Assumptions:
- The PR body is written for Jess (newcomer voice, community-writer tone).
- The issue number is in `.pipeline/classification.json` under `issue_number`.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Load the ADR, findings, and classification.
2. Draft a conventional-commit title under 72 characters.
3. Draft body sections: Summary, ADR reference, Changes, Security (link to findings-04 + findings-12), Performance (findings-05 + findings-13), Tests, Checklist, `Closes #<issue_number>`.
4. The checklist must include: ADR approved, security findings grounded, performance findings grounded, tests passing, hlint clean.
5. Write `.pipeline/pr-title.txt` with the title only.
6. Write `.pipeline/pr-body.md` with the body.

## Output

`.pipeline/pr-title.txt` and `.pipeline/pr-body.md` written.

## Refusals

- Any prerequisite file missing → refuse: "prerequisite output missing".
- Title would exceed 72 characters → refuse: "title too long; tighten the subject".
