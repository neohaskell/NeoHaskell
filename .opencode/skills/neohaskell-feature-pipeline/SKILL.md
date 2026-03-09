---
name: neohaskell-feature-pipeline
description: "Orchestrates the full 17-phase NeoHaskell feature implementation pipeline. Use when implementing new features, types, or modules for NeoHaskell. Triggers: 'implement feature', 'new NeoHaskell feature', 'feature pipeline', 'implement issue #', 'add type to nhcore', 'run feature pipeline', 'start pipeline'."
tags:
  - pipeline
  - orchestration
  - feature
  - workflow
  - neohaskell
---

# NeoHaskell Feature Implementation Pipeline

This skill orchestrates the full 17-phase pipeline for implementing new features in NeoHaskell. It coordinates 7 specialized agents, enforces quality gates, and ensures every feature passes security, performance, and DevEx review before merging. Tests are designed BEFORE implementation (outside-in TDD).

## Script-Driven Execution (USE THIS)

This skill includes `pipeline.py` — a deterministic state machine that tracks pipeline state, enforces phase ordering and PAUSE gates, and generates agent-specific prompts. **Always use the script instead of interpreting the prose instructions below.**

### Quick Start

```bash
# Initialize a new pipeline
pipeline.py init "Feature Name" --issue 330 --module "core/path/Module.hs" --test "core/test/ModuleSpec.hs" --adr 0041

# Check status (human-readable)
pipeline.py status

# Get next phase(s) as JSON — includes agent, skills, category, and prompt
pipeline.py next

# Mark a phase complete
pipeline.py complete 1

# Approve a PAUSE-gated phase (maintainer action)
pipeline.py approve 1

# Set variables (PR number, session IDs)
pipeline.py set pr_number 450
pipeline.py set session_id.7 "ses_xxx"
```

### Orchestrator Workflow

1. Call `pipeline.py init` with feature details
2. Call `pipeline.py next` — returns JSON with phase number, agent, skills, category, and prompt
3. Delegate to the specified agent: `task(subagent_type=agent, load_skills=skills, prompt=prompt)`
4. After agent completes, store session ID: `pipeline.py set session_id.N <id>`
5. Mark phase done: `pipeline.py complete N`
6. For PAUSE phases, wait for maintainer to run `pipeline.py approve N`
7. Repeat from step 2 until `pipeline.py next` returns `{"status": "complete"}`

### Key Features

- **Deterministic ordering**: Dependencies enforced by the script, not LLM interpretation
- **PAUSE gates**: Phases with `pause_after: true` require explicit approval before continuing
- **Parallel phases**: `next` returns multiple phases when they can run simultaneously (e.g., phases 2+3, 10+11)
- **Session continuity**: `continue_session_id` in output tells you which previous session to resume
- **Variable substitution**: Prompts auto-fill with feature name, paths, issue number, etc.
- **Resumable**: State persists in `.pipeline/state.json` across sessions

---

## Variables (Set Before Starting)

| Variable | Description | Example |
|----------|-------------|---------|
| `FEATURE_NAME` | Human-readable feature name | `Decimal Type` |
| `ISSUE_NUMBER` | GitHub issue number | `330` |
| `MODULE_PATH` | Where the main module lives | `core/decimal/Decimal.hs` |
| `TEST_PATH` | Where tests live | `core/test/DecimalSpec.hs` |
| `ADR_TITLE` | ADR title (descriptive) | `Decimal Type for Financial Calculations` |
| `BRANCH_NAME` | Git branch name | `feature/decimal-type` |

**To determine the next ADR number:**
```bash
ls docs/decisions/*.md | tail -1
# Currently: 0040 → next is 0041
```

---

## Pipeline Overview

| Phase | Name | Agent | PAUSE? | Deliverable |
|-------|------|-------|--------|-------------|
| 1 | ADR Draft | `neohaskell-devex-lead` | **PAUSE** | `docs/decisions/NNNN-slug.md` |
| 2 | Security Review (ADR) | `neohaskell-security-architect` | | `security-notes.md` |
| 3 | Performance Review (ADR) | `neohaskell-performance-lead` | | `performance-notes.md` |
| 4 | DevEx Review | `neohaskell-devex-lead` | **PAUSE** | DevEx checklist (pass/fail) |
| 5 | Architecture Design | `neohaskell-devex-lead` | **PAUSE** | Architecture doc (module map, API signatures) |
| 6 | Test Spec Design | `neohaskell-qa-designer` | **PAUSE** | Test specification document |
| 7 | Test Suite Writing | `neohaskell-implementer` | | Test files (compile, all fail) |
| 8 | Implementation | `neohaskell-implementer` | | Source files |
| 9 | Build & Test Loop | `neohaskell-implementer` | | All tests pass, hlint clean |
| 10 | Security Review (Impl) | `neohaskell-security-architect` | **PAUSE** | `security-impl-notes.md` |
| 11 | Performance Review (Impl) | `neohaskell-performance-lead` | **PAUSE** | `performance-impl-notes.md` |
| 12 | Fix Review Notes | `neohaskell-implementer` | | Fixes applied |
| 13 | Final Build & Test | `neohaskell-implementer` | | Clean build, all tests pass |
| 14 | Create PR | `neohaskell-git-master` + `neohaskell-community-lead` | **PAUSE** | PR URL |
| 15 | Bot Review | _(wait for CI)_ | | CI results |
| 16 | Fix Bot Comments | `neohaskell-implementer` | | Fixes applied |
| 17 | Final Approval & Merge | _(human)_ | **PAUSE** | Maintainer merges PR |

---

## Phase-by-Phase Instructions

### Phase 1: ADR Draft

**Agent**: `neohaskell-devex-lead`
**Skills to load**: `neohaskell-adr-template`, `neohaskell-style-guide`

**Delegate with**:
```
task(category="unspecified-high", load_skills=["neohaskell-adr-template", "neohaskell-style-guide"],
  description="Draft ADR for {FEATURE_NAME}",
  prompt="TASK: Create ADR-{NEXT_NUMBER} for {FEATURE_NAME}.
    EXPECTED OUTCOME: Complete ADR file at docs/decisions/{NNNN}-{slug}.md with Status: Proposed.
    MUST DO: Follow ADR template exactly. Include type definitions, module placement, public API.
      All code examples must follow NeoHaskell style. Reference issue #{ISSUE_NUMBER}.
    MUST NOT DO: Set status to Accepted. Skip any ADR sections.
    CONTEXT: {any additional context about the feature}")
```

**Output**: ADR file created at `docs/decisions/NNNN-slug.md`

**⏸ PAUSE**: Report ADR file path to maintainer. Wait for approval before proceeding.
> "ADR-{NNNN} drafted at `docs/decisions/NNNN-slug.md`. Please review before I continue."

---

### Phase 2: Security Review (ADR)

**Agent**: `neohaskell-security-architect`
**Input**: The ADR file from Phase 1

**Delegate with**:
```
task(category="unspecified-high", load_skills=["neohaskell-style-guide"],
  description="Security review of ADR for {FEATURE_NAME}",
  prompt="TASK: Review ADR-{NNNN} for security implications.
    EXPECTED OUTCOME: Security assessment with OWASP/NIST/EU findings, risk ratings, and mitigations.
    REQUIRED TOOLS: read, glob, grep only.
    MUST DO: Evaluate against OWASP Top 10, NIST controls, EU/GDPR. Rate each finding Critical/High/Medium/Low/None.
      Apply the Jess Test to every recommendation.
    MUST NOT DO: Modify any files. Suggest security configurations users must enable.
    CONTEXT: ADR at docs/decisions/{NNNN}-{slug}.md. Feature: {FEATURE_NAME}.")
```

**Output**: Security assessment with risk ratings
**Blocking criteria**: Any Critical or High finding blocks the pipeline until resolved

---

### Phase 3: Performance Review (ADR)

**Agent**: `neohaskell-performance-lead`
**Input**: The ADR file from Phase 1

**Delegate with**:
```
task(category="unspecified-high", load_skills=["neohaskell-style-guide"],
  description="Performance review of ADR for {FEATURE_NAME}",
  prompt="TASK: Review ADR-{NNNN} for performance implications against 50k req/s target.
    EXPECTED OUTCOME: Performance assessment with impact ratings (blocking/advisory) and recommendations.
    REQUIRED TOOLS: read, glob, grep only.
    MUST DO: Evaluate serialization impact, hot path placement, memory characteristics, allocation patterns.
      Apply the 50k Test and the Jess Test.
    MUST NOT DO: Modify any files. Suggest performance tuning users must do.
    CONTEXT: ADR at docs/decisions/{NNNN}-{slug}.md. Feature: {FEATURE_NAME}.")
```

**Output**: Performance assessment with recommendations
**Blocking criteria**: Any finding estimated to degrade throughput below 50k req/s is blocking

**Note**: Phases 2 and 3 can run in parallel.

---

### Phase 4: DevEx Review

**Agent**: `neohaskell-devex-lead`
**Input**: ADR + security notes + performance notes

**Delegate with**:
```
task(category="unspecified-high", load_skills=["neohaskell-style-guide"],
  description="DevEx review of {FEATURE_NAME} ADR",
  prompt="TASK: Review ADR-{NNNN} for developer experience quality.
    EXPECTED OUTCOME: DevEx checklist with pass/fail for each criterion.
    MUST DO: Evaluate API intuitiveness, naming, pipe-friendliness, discoverability,
      consistency with existing nhcore patterns. Incorporate security and performance feedback.
    MUST NOT DO: Skip any checklist item.
    CONTEXT: ADR at docs/decisions/{NNNN}-{slug}.md.
      Security notes: {security findings summary}. Performance notes: {performance findings summary}.")
```

**Output**: DevEx review checklist

**⏸ PAUSE**: Report DevEx review results. Wait for maintainer decision.
> "DevEx review complete. {N} items pass, {M} need work. [summary]. Proceed?"

---

### Phase 5: Architecture Design

**Agent**: `neohaskell-devex-lead`
**Input**: Approved ADR + all review notes

**Delegate with**:
```
task(category="unspecified-high", load_skills=["neohaskell-style-guide"],
  description="Architecture design for {FEATURE_NAME}",
  prompt="TASK: Create detailed architecture document for {FEATURE_NAME}.
    EXPECTED OUTCOME: Module placement map, public API signatures with full type definitions,
      integration points with existing nhcore, dependency map.
    MUST DO: Specify exact file paths for all new modules. Define all type signatures.
      Show how new types integrate with EventStore/Command/Query if applicable.
    MUST NOT DO: Write implementation code. Change existing file structure without justification.
    CONTEXT: Approved ADR at docs/decisions/{NNNN}-{slug}.md.
      Module path: {MODULE_PATH}. Test path: {TEST_PATH}.")
```

**Output**: Architecture document with file paths, type signatures, integration points

**⏸ PAUSE**: Report architecture design. Wait for maintainer approval.
> "Architecture designed. New files: [list]. Public API: [key signatures]. Proceed to test design?"

---

### Phase 6: Test Spec Design

**Agent**: `neohaskell-qa-designer`
**Skills to load**: `neohaskell-style-guide`
**Input**: Architecture document from Phase 5

This is the **outside-in TDD entry point**. The QA designer reads the architecture document and produces a comprehensive test specification covering every public API function with happy paths, edge cases, error conditions, serialization round-trips, and property-based invariants.

**Delegate with**:
```
task(category="ultrabrain", load_skills=["neohaskell-style-guide"],
  description="Design test specification for {FEATURE_NAME}",
  prompt="TASK: Design a comprehensive test specification for {FEATURE_NAME}.
    EXPECTED OUTCOME: Structured test specification document with every test case enumerated
      in tabular format. Each test case must be specific enough to implement without ambiguity.
    REQUIRED TOOLS: read, glob, grep, write, edit.
    MUST DO: Read the architecture document to understand the public API. Read existing test files
      for similar modules to match patterns (e.g., DecimalSpec.hs, RedactedSpec.hs, OAuth2ClientSpec.hs).
      Apply the input analysis rubric to every function parameter. Cover: happy paths, edge cases
      (empty, boundary, unicode, special chars), error conditions (every error constructor),
      serialization round-trips, property-based invariants.
      Minimum 3 test cases per public function. Target 3:1 edge-to-happy ratio.
      Write the spec to a file.
    MUST NOT DO: Write Haskell test code. Skip any public API function. Use vague test descriptions.
    CONTEXT: Architecture doc: {architecture summary}. Module at {MODULE_PATH}. Test path: {TEST_PATH}.
      Reference test files: core/test/DecimalSpec.hs, core/test-core/RedactedSpec.hs,
      core/test/Auth/OAuth2/ClientSpec.hs for edge case patterns.")
```

**Output**: Test specification document with all test cases enumerated

**⏸ PAUSE**: Report test spec summary (total test count, categories). Wait for maintainer approval.
> "Test spec designed: {N} total test cases ({happy} happy paths, {edge} edge cases, {error} error conditions, {serial} serialization, {prop} property tests). Review spec before implementation?"

---

### Phase 7: Test Suite Writing

**Agent**: `neohaskell-implementer`
**Skills to load**: `neohaskell-style-guide`
**Input**: Test specification from Phase 6 + Architecture document from Phase 5

The implementer translates the QA designer's test specification into actual Haskell test code. Every row in the spec becomes an `it` block. No test cases are added or removed — the spec is the source of truth.

**Delegate with**:
```
task(category="unspecified-high", load_skills=["neohaskell-style-guide"],
  description="Write test suite for {FEATURE_NAME} from spec",
  prompt="TASK: Translate the test specification into Haskell test code for {FEATURE_NAME}.
    EXPECTED OUTCOME: Test files that compile but ALL tests fail (no implementation yet).
    MUST DO: Implement every test case from the test spec. Follow NeoHaskell test conventions exactly.
      Register tests in cabal file and test runner. Tests must compile with stub implementations.
      Each row in the spec = one `it` block. Do NOT add tests beyond the spec. Do NOT skip any.
    MUST NOT DO: Write implementation code. Modify existing tests. Add test cases not in the spec.
    CONTEXT: Test spec: {test_spec_summary}. Architecture doc: {architecture summary}.
      Test path: {TEST_PATH}. Module path: {MODULE_PATH}.")
```

**Output**: Test files created, all tests fail (red phase of TDD)

---

### Phase 8: Implementation

**Agent**: `neohaskell-implementer`
**Skills to load**: `neohaskell-style-guide`
**Input**: Architecture document + test files from Phase 7

**Delegate with (continue session from Phase 7)**:
```
task(session_id="{phase7_session_id}", load_skills=["neohaskell-style-guide"],
  description="Implement {FEATURE_NAME}",
  prompt="TASK: Implement {FEATURE_NAME} to make all tests pass.
    EXPECTED OUTCOME: All source files created, following NeoHaskell conventions exactly.
    MUST DO: Follow architecture document precisely. All code must follow NeoHaskell style guide.
      Use pipes, do-blocks, case expressions, if-then-else for Bools, descriptive type params,
      Task.yield, Result. Import nhcore modules first.
    MUST NOT DO: Modify any test files. Use where/let-in. Use single-letter type params. Use $.
    CONTEXT: Tests at {TEST_PATH}. Module at {MODULE_PATH}.")
```

**Output**: Implementation source files

---

### Phase 9: Build & Test Loop

**Agent**: `neohaskell-implementer`
**Input**: Implementation from Phase 8

**Delegate with (continue session from Phase 8)**:
```
task(session_id="{phase8_session_id}", load_skills=["neohaskell-style-guide"],
  description="Build and test {FEATURE_NAME}",
  prompt="TASK: Run build and test loop until all tests pass and hlint is clean.
    EXPECTED OUTCOME: `cabal build all` succeeds, `cabal test` passes, `hlint .` clean on changed files.
    MUST DO: Run `cabal build all && cabal test`. Fix compilation errors. Fix test failures.
      Run `hlint` on changed files. Max 10 iterations. Self-review against style guide before reporting.
    MUST NOT DO: Modify test expectations. Suppress type errors. Skip hlint.
    CONTEXT: Max 10 build iterations. If still failing after 10, report failure with details.")
```

**Output**: Clean build, all tests passing, hlint clean

**Failure protocol**: If 10 iterations fail, STOP and report:
1. What was attempted
2. Which tests still fail
3. What the errors are
4. Request human guidance

---

### Phase 10: Security Review (Implementation)

**Agent**: `neohaskell-security-architect`
**Input**: Implemented source files

**Delegate with**:
```
task(category="unspecified-high", load_skills=["neohaskell-style-guide"],
  description="Security review of {FEATURE_NAME} implementation",
  prompt="TASK: Review the implementation of {FEATURE_NAME} for security issues.
    EXPECTED OUTCOME: Code-level security findings with file:line references and pass/fail checklist.
    MUST DO: Check input validation, injection vectors, overflow/bounds handling,
      information disclosure in error messages. Verify 'parse, don't validate' patterns.
    MUST NOT DO: Modify any files.
    CONTEXT: Source files at {MODULE_PATH}. Tests at {TEST_PATH}.")
```

**Output**: Security implementation review with code-level findings

**⏸ PAUSE**: Report security findings. Wait for maintainer review.

---

### Phase 11: Performance Review (Implementation)

**Agent**: `neohaskell-performance-lead`
**Input**: Implemented source files

**Delegate with**:
```
task(category="unspecified-high", load_skills=["neohaskell-style-guide"],
  description="Performance review of {FEATURE_NAME} implementation",
  prompt="TASK: Review the implementation of {FEATURE_NAME} for performance issues.
    EXPECTED OUTCOME: Code-level performance findings with file:line references.
    MUST DO: Check INLINE pragmas, strict fields, serialization (toEncoding vs toJSON),
      allocation patterns, space leak potential. Check SPECIALIZE pragmas for polymorphic hot-path functions.
    MUST NOT DO: Modify any files.
    CONTEXT: Source files at {MODULE_PATH}. Tests at {TEST_PATH}. Target: 50k req/s.")
```

**Output**: Performance implementation review

**⏸ PAUSE**: Report performance findings. Wait for maintainer review.

**Note**: Phases 10 and 11 can run in parallel.

---

### Phase 12: Fix Review Notes

**Agent**: `neohaskell-implementer`
**Input**: Security + performance review notes

**Delegate with (continue session from Phase 9)**:
```
task(session_id="{phase9_session_id}", load_skills=["neohaskell-style-guide"],
  description="Fix review notes for {FEATURE_NAME}",
  prompt="TASK: Apply fixes from security and performance reviews.
    EXPECTED OUTCOME: All review findings addressed. Code compiles and tests pass.
    MUST DO: Address each finding. Add INLINE pragmas where recommended. Fix strict field annotations.
      Fix any security issues identified.
    MUST NOT DO: Modify test expectations. Ignore any Critical/High findings.
    CONTEXT: Security findings: {security_findings}. Performance findings: {performance_findings}.")
```

**Output**: Fixes applied

**Re-review trigger**: If security-relevant files changed, re-trigger Phase 10. If performance-relevant files changed (hot paths, serialization), re-trigger Phase 11.

---

### Phase 13: Final Build & Test

**Agent**: `neohaskell-implementer`

**Delegate with (continue session from Phase 12)**:
```
task(session_id="{phase12_session_id}", load_skills=["neohaskell-style-guide"],
  description="Final build and test for {FEATURE_NAME}",
  prompt="TASK: Final build and test verification.
    EXPECTED OUTCOME: `cabal build all` succeeds, ALL test suites pass, `hlint .` clean.
    MUST DO: Run full build and test. Run hlint on all changed files.
      Self-review all changed files against neohaskell-style-guide.
    MUST NOT DO: Modify test expectations.
    CONTEXT: This is the final verification before PR creation.")
```

**Output**: Clean build, all tests passing

---

### Phase 14: Create PR

**Agents**: `neohaskell-git-master` (branch + commit + PR) and `neohaskell-community-lead` (PR body)

**Step 1**: Get PR body from community lead:
```
task(category="writing", load_skills=[],
  description="Write PR body for {FEATURE_NAME}",
  prompt="TASK: Write the PR description for {FEATURE_NAME}.
    EXPECTED OUTCOME: PR body markdown with summary, changes list, and checklist.
    MUST DO: Include 'Closes #{ISSUE_NUMBER}'. List all changed files. Include checklist
      (ADR, security review, performance review, tests, hlint).
    MUST NOT DO: Include implementation details in the summary — keep it user-facing.
    CONTEXT: ADR: {NNNN}. Files changed: {list}. Tests: {test_count} passing.")
```

**Step 2**: Create PR with git master:
```
task(category="git", load_skills=["git-master"],
  description="Create PR for {FEATURE_NAME}",
  prompt="TASK: Create a PR from branch {BRANCH_NAME} to main.
    EXPECTED OUTCOME: PR created with the provided body. PR URL reported.
    MUST DO: Commit all changes with conventional commit format. Push branch. Create PR via gh.
    MUST NOT DO: Merge the PR. Force push.
    CONTEXT: Branch: {BRANCH_NAME}. PR body: {pr_body}.")
```

**⏸ PAUSE**: Report PR URL to maintainer.
> "PR created: {PR_URL}. Waiting for CI and review."

---

### Phase 15: Bot Review

**Action**: Wait for CI to complete. Check status with:
```bash
gh pr checks {PR_NUMBER} --watch
```

No agent needed — just monitor CI status.

---

### Phase 16: Fix Bot Comments

**Agent**: `neohaskell-implementer`
**Input**: CodeRabbit comments, CI failures

**Delegate with**:
```
task(category="unspecified-high", load_skills=["neohaskell-style-guide"],
  description="Fix bot comments on PR #{PR_NUMBER}",
  prompt="TASK: Address CodeRabbit and CI comments on PR #{PR_NUMBER}.
    EXPECTED OUTCOME: All bot comments resolved. CI passes.
    MUST DO: Read bot comments via `gh api`. Fix each issue. Push fixes. Tests remain immutable.
    MUST NOT DO: Modify test expectations. Dismiss bot reviews without fixing.
    CONTEXT: PR: {PR_URL}. Read comments with: gh api repos/neohaskell/NeoHaskell/pulls/{PR_NUMBER}/comments")
```

**Output**: Bot comments addressed, CI green

---

### Phase 17: Final Approval & Merge

**⏸ PAUSE**: Human gate. Maintainer reviews the PR, approves, and merges.
> "CI is green, all bot comments resolved. PR ready for final review and merge: {PR_URL}"

The pipeline ends here. Merging is a human action — Atlas does not merge PRs.

## Agent Coordination Reference

| Agent | Phases | Skills to Load | Write Access? |
|-------|--------|---------------|---------------|
| `neohaskell-devex-lead` | 1, 4, 5 | `neohaskell-adr-template`, `neohaskell-style-guide` | Yes (ADR files) |
| `neohaskell-qa-designer` | 6 | `neohaskell-style-guide` | Yes (test spec doc) |
| `neohaskell-security-architect` | 2, 10 | `neohaskell-style-guide` | No (read-only) |
| `neohaskell-performance-lead` | 3, 11 | `neohaskell-style-guide` | No (read-only) |
| `neohaskell-implementer` | 7, 8, 9, 12, 13, 16 | `neohaskell-style-guide` | Yes (source + tests) |
| `neohaskell-community-lead` | 14 (PR body) | — | No |
| `neohaskell-git-master` | 14 (git) | `git-master` | No (bash only) |

---

## Parallel Execution Opportunities

These phases can run in parallel to save time:

- **Phases 2 + 3**: Security and performance ADR reviews are independent
- **Phases 10 + 11**: Security and performance implementation reviews are independent
- **Phase 14**: Community lead (PR body) and git master (branch/commit) can prepare in parallel

---

## Failure Recovery Protocol

| Phase | If It Fails | Recovery Action |
|-------|-------------|-----------------|
| 1 (ADR) | ADR rejected | Revise ADR based on feedback, re-submit |
| 2-3 (Reviews) | Critical/High finding | Revise ADR to address finding, re-run review |
| 4 (DevEx) | Items fail checklist | Revise ADR/architecture, re-run DevEx review |
| 6 (Test Spec) | Insufficient coverage | QA designer revises spec, adds missing cases |
| 7 (Tests) | Tests don't compile | Fix test compilation errors (not expectations) |
| 9 (Build Loop) | 10 iterations exhausted | STOP. Report failure. Consult Oracle or ask human |
| 10-11 (Impl Reviews) | Critical findings | Fix in Phase 12, re-trigger affected review |
| 12 (Fix Notes) | Can't fix a finding | Escalate to maintainer with details |
| 16 (Bot Comments) | CI still failing | Debug with full build output. Max 5 attempts, then escalate |

---

## Deliverables Checklist

At pipeline completion, verify all deliverables exist:

- [ ] ADR file: `docs/decisions/NNNN-slug.md` (Status: Proposed → Accepted after merge)
- [ ] Test specification: test spec document reviewed and approved
- [ ] Source module: `{MODULE_PATH}`
- [ ] Test file: `{TEST_PATH}`
- [ ] All tests passing: `cabal test` green
- [ ] Lint clean: `hlint .` clean on changed files
- [ ] Security review: No Critical/High findings
- [ ] Performance review: 50k req/s target maintained
- [ ] PR created and ready for maintainer review
- [ ] README index updated in `docs/decisions/README.md`
- [ ] `nhcore.cabal` updated with new modules

---

## Session Continuity

**Critical**: The implementer agent runs across phases 7 → 8 → 9 → 12 → 13 → 16. Always use `session_id` to continue the same session:

```
phase7_result = task(...) → session_id = "ses_xxx"
phase8_result = task(session_id="ses_xxx", ...) → same session
phase9_result = task(session_id="ses_xxx", ...) → same session
phase12_result = task(session_id="ses_xxx", ...) → same session
phase13_result = task(session_id="ses_xxx", ...) → same session
phase16_result = task(session_id="ses_xxx", ...) → same session
```

This preserves full context across all implementation phases, saving tokens and avoiding re-exploration.
