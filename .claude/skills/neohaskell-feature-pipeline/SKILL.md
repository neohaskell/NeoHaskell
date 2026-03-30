---
name: neohaskell-feature-pipeline
description: Orchestrates NeoHaskell feature implementation using a structured 17-phase pipeline. Use when implementing new features, types, or modules for NeoHaskell. Triggers on requests like 'implement feature', 'new NeoHaskell feature', 'feature pipeline', 'implement issue #', 'add type to nhcore', 'run feature pipeline', or 'start pipeline'.
disable-model-invocation: true
---

# NeoHaskell Feature Implementation Pipeline

This skill orchestrates the full 17-phase pipeline for implementing new features in NeoHaskell. It coordinates specialized review phases, enforces quality gates, and ensures every feature passes security, performance, and DevEx review before merging. Tests are designed BEFORE implementation (outside-in TDD).

## Quick Reference

Run the pipeline script for state management:

```bash
python3 ${CLAUDE_SKILL_DIR}/scripts/pipeline.py <command>
```

**Commands:**
- `init "Feature Name" --issue N --module path --test path --adr NNNN` - Initialize pipeline
- `status` - Check current progress
- `next` - Get next phase(s) as JSON with prompts
- `complete N` - Mark phase N complete
- `approve N` - Approve a PAUSE-gated phase
- `set key value` - Set variables (pr_number, session_id.N)

## Pipeline Phases

| # | Phase | Focus | PAUSE? |
|---|-------|-------|--------|
| 1 | ADR Draft | Architecture decision record | **Yes** |
| 2 | Security Review (ADR) | OWASP/NIST evaluation | |
| 3 | Performance Review (ADR) | 50k req/s target analysis | |
| 4 | DevEx Review | API intuitiveness check | **Yes** |
| 5 | Architecture Design | Module map, API signatures | **Yes** |
| 6 | Test Spec Design | Comprehensive test specification | **Yes** |
| 7 | Test Suite Writing | Implement tests from spec (all fail) | |
| 8 | Implementation | Write source code | |
| 9 | Build & Test Loop | Iterate until green | |
| 10 | Security Review (Impl) | Code-level security audit | **Yes** |
| 11 | Performance Review (Impl) | INLINE pragmas, allocation patterns | **Yes** |
| 12 | Fix Review Notes | Address findings | |
| 13 | Final Build & Test | Clean build verification | |
| 14 | Create PR | Branch, commit, PR creation | **Yes** |
| 15 | Bot Review | Wait for CI | |
| 16 | Fix Bot Comments | Address CodeRabbit/CI issues | |
| 17 | Final Approval | Human merge | **Yes** |

## Workflow

1. **Initialize**: Run `pipeline.py init` with feature details
2. **Get Next Phase**: Run `pipeline.py next` for phase info and prompts
3. **Execute Phase**: Follow the phase instructions or delegate to appropriate agent
4. **Complete Phase**: Run `pipeline.py complete N`
5. **Handle PAUSE Gates**: Wait for maintainer to run `pipeline.py approve N`
6. **Repeat**: Continue until pipeline returns `{"status": "complete"}`

## Variables

Set these before starting:

| Variable | Description | Example |
|----------|-------------|---------|
| `FEATURE_NAME` | Human-readable name | `Decimal Type` |
| `ISSUE_NUMBER` | GitHub issue | `330` |
| `MODULE_PATH` | Main module location | `core/decimal/Decimal.hs` |
| `TEST_PATH` | Test file location | `core/test/DecimalSpec.hs` |
| `ADR_TITLE` | ADR description | `Decimal Type for Financial Calculations` |
| `BRANCH_NAME` | Git branch | `feature/decimal-type` |

**Find next ADR number:**
```bash
ls docs/decisions/*.md | tail -1
```

## Phase Details

### Phase 1: ADR Draft

Create the Architecture Decision Record at `docs/decisions/NNNN-slug.md`.

**Requirements:**
- Follow ADR template exactly (read `neohaskell-adr-template` skill)
- Include type definitions, module placement, public API
- All code examples must follow NeoHaskell style guide
- Reference the GitHub issue
- Set Status: Proposed

**PAUSE after**: Report ADR path to maintainer for review.

### Phases 2-3: Security & Performance Review (ADR)

Can run in parallel. Read-only analysis of the ADR.

**Security Review (Phase 2):**
- Evaluate against OWASP Top 10, NIST controls, EU/GDPR
- Rate findings: Critical/High/Medium/Low/None
- Any Critical/High finding blocks pipeline

**Performance Review (Phase 3):**
- Evaluate serialization impact, hot path placement
- Check memory characteristics, allocation patterns
- Target: 50k req/s maintained

### Phase 4: DevEx Review

Evaluate API quality:
- Intuitiveness and naming
- Pipe-friendliness
- Discoverability
- Consistency with existing nhcore patterns

**PAUSE after**: Report pass/fail count to maintainer.

### Phase 5: Architecture Design

Create detailed architecture document:
- Module placement map
- Public API signatures with full type definitions
- Integration points with existing nhcore
- Dependency map

**PAUSE after**: Report architecture to maintainer.

### Phase 6: Test Spec Design

Design comprehensive test specification (outside-in TDD entry point):
- Read architecture document for public API
- Match patterns from existing tests (DecimalSpec.hs, RedactedSpec.hs)
- Cover: happy paths, edge cases, error conditions, serialization, properties
- Minimum 3 test cases per public function
- Target 3:1 edge-to-happy ratio

**PAUSE after**: Report test count summary to maintainer.

### Phase 7: Test Suite Writing

Translate test spec to Haskell test code:
- Each row in spec = one `it` block
- Tests must compile but ALL fail (no implementation yet)
- Register in cabal file and test runner
- Do NOT add tests beyond the spec

### Phases 8-9: Implementation & Build Loop

Implement the feature and iterate until green:
- Follow architecture document precisely
- Use NeoHaskell style: pipes, do-blocks, case expressions
- Run `cabal build all && cabal test`
- Run `hlint` on changed files
- Max 10 iterations; escalate if still failing

### Phases 10-11: Security & Performance Review (Impl)

Can run in parallel. Code-level reviews with file:line references.

**Security Review (Phase 10):**
- Input validation, injection vectors
- Overflow/bounds handling
- Information disclosure in errors

**Performance Review (Phase 11):**
- INLINE pragmas placement
- Strict field annotations
- `toEncoding` vs `toJSON` for serialization
- SPECIALIZE pragmas for polymorphic hot paths

**PAUSE after each**: Report findings to maintainer.

### Phase 12: Fix Review Notes

Apply all security and performance fixes. If files changed significantly, re-trigger relevant reviews.

### Phase 13: Final Build & Test

Final verification:
- `cabal build all` succeeds
- All test suites pass
- `hlint .` clean on changed files

### Phase 14: Create PR

1. Write PR body with summary, changes list, checklist
2. Include `Closes #ISSUE_NUMBER`
3. Create branch, commit, push
4. Create PR via `gh pr create`

**PAUSE after**: Report PR URL to maintainer.

### Phases 15-16: Bot Review & Fixes

Wait for CI, then address any CodeRabbit or CI comments:
```bash
gh pr checks PR_NUMBER --watch
gh api repos/neohaskell/NeoHaskell/pulls/PR_NUMBER/comments
```

### Phase 17: Final Approval

Human gate. Maintainer reviews, approves, and merges the PR.

## Parallel Execution

These phases can run simultaneously:
- Phases 2 + 3 (Security + Performance ADR reviews)
- Phases 10 + 11 (Security + Performance implementation reviews)

## Failure Recovery

| Phase | Recovery |
|-------|----------|
| ADR rejected | Revise based on feedback, re-submit |
| Critical security finding | Revise ADR, re-run review |
| Build loop exhausted (10 iterations) | STOP, report details, request guidance |
| Can't fix a finding | Escalate to maintainer |
| CI still failing after 5 attempts | Escalate with full output |

## Deliverables Checklist

At completion, verify:
- [ ] ADR: `docs/decisions/NNNN-slug.md`
- [ ] Source module at `MODULE_PATH`
- [ ] Test file at `TEST_PATH`
- [ ] All tests passing
- [ ] hlint clean
- [ ] No Critical/High security findings
- [ ] 50k req/s target maintained
- [ ] PR ready for review
- [ ] `nhcore.cabal` updated with new modules

## Related Skills

- `neohaskell-adr-template` - ADR format and structure
- `neohaskell-style-guide` - Code style conventions
