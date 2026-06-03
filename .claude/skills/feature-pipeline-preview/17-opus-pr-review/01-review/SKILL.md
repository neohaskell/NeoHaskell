---
name: 01-review
description: Opus reads the PR diff against the NeoHaskell quality rubric, writes findings to .pipeline/findings-17.json, and surfaces a digest in conversation.
kind: leaf
executor: opus
model: claude-opus-4-7
---

# PR Review (Opus)

Opus reads the open PR diff and produces a structured findings file with proposed fixes. The maintainer reads the digest in-conversation, edits `.pipeline/findings-17.json` to accept/reject/defer each finding, then runs `pipeline.py approve 17` to release the apply step.

## Inputs

- `pipeline.py get pr_number` — the PR to review
- `gh pr diff <pr_number>` — the diff under review
- `git diff main..HEAD` — full branch delta as cross-check
- `docs/architecture/<adr_number>-<slug>.md` — architecture doc; check all named files exist in the tree
- `docs/decisions/<adr_number>-<slug>.md` — ADR for context
- `core/CLAUDE.md` — style table (cite when raising style findings)

## Plan

1. Read the diff and cross-reference it against the architecture doc.
2. Apply the rubric below to every file in the diff.
3. Emit `.pipeline/findings-17.json` — an array of findings with proposed fixes (schema below).
4. Surface a markdown digest of the findings in the orchestrator's conversation.
5. Call `pipeline.py complete 17`. STOP (PAUSE — maintainer reviews next).

## Findings schema

```json
{
  "rule": "<short-kebab-case-id>",
  "severity": "high" | "medium" | "low",
  "location": "<file:line[-line]>",
  "explanation": "<one paragraph: what's wrong + why it matters>",
  "proposed_fix": {
    "kind": "edit" | "delete" | "rename" | "add",
    "file": "<path>",
    "summary": "<one sentence: what to change>"
  },
  "maintainer_decision": "pending"
}
```

The maintainer sets `maintainer_decision` to `"accept"`, `"reject"`, or `"defer"` per entry before running approve.

## Rubric

### Category 1: NeoHaskell style violations

Cite `core/CLAUDE.md` for the exact rule. Common offenders:
- `$` used anywhere — should be `|>`
- `where` clauses — should be `do let`
- `let..in` — should be `do let`
- `<>` / `++` on strings — should be `[fmt|...|]`
- `Either` — should be `Result`
- `IO` in user code — should be `Task err val`
- `pure` / `return` — should be `Task.yield val`
- Unqualified imports of nhcore modules — should be `import Foo (Foo); import Foo qualified`
- `!` strictness annotations — nhcore has `Strict` globally; `!` is redundant
- Single-letter type params — should be `forall element result.` etc.

### Category 2: Hackage imports without `Ghc` prefix

Any import of `Control.Concurrent.Async`, `Data.Either`, `Data.IORef`, `Data.Map`, `Data.Map.Strict`, `Data.Vector`, etc. **must** be aliased with a `Ghc` prefix (e.g. `import qualified Data.Map.Strict as GhcMap`). Bare or differently-aliased Hackage imports are a finding.

### Category 3: `Task.fromIO (… Task.runResult …)` round-trip smell

Any code that does `Task.fromIO (someIoLib (Task.runResult task))` is escaping Task to use an IO library, then reconstructing Task. This means a needed nhcore primitive is missing. Propose extending nhcore (e.g. add `AsyncTask.race`) rather than perpetuating the escape pattern.

### Category 4: Dead code / unnecessary plumbing

- Aliases that wrap a single existing function without adding meaning (e.g. `nilUuid = Uuid.nil`)
- Redundant `Task.yield unit` after a step that already yields unit (`ConcurrentVar.modify`, `Task.forEach`, etc.)
- `pass` followed by `Task.yield unit`
- Public functions with no callers in the diff or elsewhere in the tree
- `case True/False` over `Bool` — should be `if/then/else`

### Category 5: Test anti-patterns

- **Setup-error swallowing:** `case … of Err (ConnectionFailed _) -> pass` or any `Err _ -> pass` on a fixture call — finding rule `test-swallows-infra-failure`
- **Mismatched name vs body:** test name says `emits` / `reads in chunks` / `logs` / `deletes` / `writes` / `updates` / `replays` / `resumes`, body doesn't assert the named primitive — rule `test-name-body-mismatch`
- **Trivial-fixture error-path:** `it "fails with <X>"` whose setup is `Subscriber.new <InMemory>.new Registry.empty` (or equivalent empty fixture) and body asserts `Err (<X> _) -> pass` — rule `test-trivial-fixture-error-path`
- **Panicky-let non-behavioral:** body is just `let _x = builder ... ; pass` — rule `test-panicky-let-non-behavioral`

### Category 6: Architecture-doc round-trip

For every file path named in the architecture doc's "Module map" or similar section, verify the file exists in the tree. Each missing file → finding rule `arch-doc-artifact-missing` with severity `high`.

### Category 7: Schema-migration smells

- `DROP TABLE` in any file under `core/service/` not matching `*Spec.hs`, `/testlib/`, `/test-service/` → rule `schema-drop-table-production`, severity `high`
- `CREATE TABLE` in any application-startup code path (not in a file matching `*/Migrations/*.sql`) → rule `schema-migration-in-app-code`, severity `medium`

## Output format

After writing `.pipeline/findings-17.json`, surface a markdown digest like:

```markdown
## Opus PR review — N findings

| # | Severity | Rule | Location | Proposed fix |
|---|----------|------|----------|--------------|
| 1 | high | ghc-async-in-task | core/service/.../Subscriber.hs:300-321 | Replace GhcAsync.race with AsyncTask.race |
| 2 | medium | redundant-yield-unit | core/service/.../Subscriber.hs:390 | Drop trailing Task.yield unit |
| ... | ... | ... | ... | ... |

Maintainer: edit `.pipeline/findings-17.json` to set `maintainer_decision` for each (accept / reject / defer), then run `pipeline.py approve 17`.
```

Then call `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 17` and STOP.

## Refusals

- PR number not set in pipeline state → "phase 16 didn't record a PR; cannot review"
- `gh pr diff` returns non-zero → surface and refuse
- Architecture doc missing → "phase 7 architecture doc missing; cannot do round-trip check"
