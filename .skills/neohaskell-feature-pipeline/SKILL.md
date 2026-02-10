---
name: neohaskell-feature-pipeline
description: |
  17-phase feature implementation pipeline for NeoHaskell. Orchestrated by Atlas. 
  Use when implementing new features, types, or modules for the NeoHaskell project 
  following the full development lifecycle: ADR drafting, security review (OWASP/NIST/EU), 
  performance review (50k req/s target), DevEx lead review, architecture design, test-first 
  development, implementation, build/test loops, review cycles, PR creation, bot review, 
  and merge. Coordinates the neohaskell-devex-lead, neohaskell-security-architect, 
  neohaskell-performance-lead, and neohaskell-community-lead agents through a structured 
  pipeline with human checkpoints. Triggers: "implement feature", "new NeoHaskell feature", 
  "feature pipeline", "implement issue #", "add type to nhcore", "run feature pipeline", 
  "/neohaskell-feature-pipeline".
---

# NeoHaskell Feature Implementation Pipeline

A 17-phase pipeline for implementing features in NeoHaskell with full security, performance, and DevEx review.

## Variables (fill before starting)

| Variable | Description | Example |
|----------|-------------|---------|
| `FEATURE_NAME` | Human-readable feature name | "Decimal/Money Type" |
| `ISSUE_NUMBER` | GitHub issue number | 330 |
| `ISSUE_URL` | Full issue URL | `https://github.com/neohaskell/NeoHaskell/issues/330` |
| `MODULE_PATH` | Primary implementation file | `core/core/Decimal.hs` |
| `TEST_PATH` | Primary test file | `core/test-core/Decimal/DecimalSpec.hs` |
| `ADR_TITLE` | ADR document title | "Decimal Type for Financial Calculations" |
| `BRANCH_NAME` | Git branch name | `feature/decimal-type` |

## Pipeline Overview

| Phase | Name | Agent | Checkpoint |
|-------|------|-------|------------|
| 1 | ADR Draft | Atlas + neohaskell-devex-lead | |
| 2 | Security Review | neohaskell-security-architect | |
| 3 | Performance Review | neohaskell-performance-lead | |
| 4 | DevEx Lead Review | neohaskell-devex-lead | PAUSE |
| 5 | Architecture Design | neohaskell-devex-lead | PAUSE |
| 6 | Test Suite Definition | Implementation agent | |
| 7 | Implementation | Implementation agent | |
| 8 | Build & Test Loop | Implementation agent | |
| 9 | Security Impl Review | neohaskell-security-architect | |
| 10 | Performance Impl Review | neohaskell-performance-lead | |
| 11 | Fix Review Notes | Implementation agent | |
| 12 | Final Build & Test | Implementation agent | |
| 13 | Create PR | git-master | PAUSE |
| 14 | Wait for Bots | Human | PAUSE |
| 15 | Fix Bot Comments | Implementation agent | |
| 16 | Final Review | All review agents | |
| 17 | Merge | git-master | PAUSE |

## Phase Details

### PHASE 1: ADR Draft

Determine next ADR number:
```bash
ls docs/decisions/*.md | tail -1
```

Create `docs/decisions/{next_number}-{feature-slug}.md`:

```markdown
# ADR-{NUMBER}: {ADR_TITLE}

## Status
Proposed

## Context
[Problem this feature solves]

### Use Cases
- [Use case 1]
- [Use case 2]

### Requirements
- [Requirement 1]
- [Requirement 2]

## Decision
[Solution and approach]

### Type Definitions
[Key types and signatures — use NeoHaskell style: descriptive type params, Result not Either, Task not IO]

### Instances/Functions Required
[What needs implementing]

## Consequences

### Positive
- [Benefit 1]

### Negative
- [Tradeoff 1]

### Risks
- [Risk 1 and mitigation]
```

Update `docs/decisions/README.md` to add row to ADR index table.

**Delegate to**: `neohaskell-devex-lead` agent or `category="writing"` with domain context.

---

### PHASE 2: Security Review

Invoke `neohaskell-security-architect` agent to review the ADR from three perspectives:

**OWASP Top 10 Assessment:**
- A01 Broken Access Control
- A02 Cryptographic Failures
- A03 Injection
- A04 Insecure Design
- A08 Data Integrity

**NIST Assessment:**
- Data integrity controls
- Input validation
- Access controls

**EU Compliance Assessment:**
- GDPR implications
- Data storage/retention
- PII handling

Output: `security-notes.md` in the notepad directory.

---

### PHASE 3: Performance Review

Invoke `neohaskell-performance-lead` agent to review for 50k req/s target.

Review areas:
- Serialization impact (Aeson instances)
- Hot path placement (EventStore, Command/Query handling)
- Memory characteristics (strict fields, space leaks)
- Allocation patterns

Output: `performance-notes.md` in the notepad directory.

---

### PHASE 4: DevEx Lead Review — PAUSE

Invoke `neohaskell-devex-lead` agent to review ADR incorporating security and performance feedback.

Checklist:
- [ ] API is intuitive for NeoHaskell users
- [ ] Consistent with existing patterns (pipe operator, qualified imports, etc.)
- [ ] No breaking changes to existing code
- [ ] Documentation is clear
- [ ] Migration path documented (if needed)

Update ADR with refinements.

**PAUSE: Report ADR status and wait for human approval before continuing.**

---

### PHASE 5: Architecture Design — PAUSE

Define module location, abstraction layers, type signatures, and integration points.

Create `arch-doc.md` in the notepad directory with:
- Module placement in nhcore's source directory structure
- Public API (type signatures in NeoHaskell style)
- Integration with existing modules (Core re-exports, service layer connections)
- Dependencies on other nhcore modules

**PAUSE: Report architecture and wait for human approval.**

---

### PHASE 6: Test Suite Definition

Create test file at `{TEST_PATH}`.

Required test categories:
- Unit tests for core functionality
- Edge cases (null, negative, overflow, empty, boundary values)
- Serialization round-trips (ToJSON/FromJSON, if applicable)
- Property-based tests where applicable (QuickCheck)
- Integration with existing types (if the new type interacts with others)

Follow existing test patterns in `core/test-core/`.

**CRITICAL: Once tests are written, they are IMMUTABLE. Never modify tests during implementation.**

---

### PHASE 7: Implementation

Implement `{MODULE_PATH}` following NeoHaskell conventions:
- Pipe operator (`|>`) over nesting
- `do` blocks with `let` bindings, no `let..in` or `where`
- `case` for pattern matching, not function definitions
- Descriptive type parameters (`forall element result.`)
- Qualified imports (`Module.function` design)
- String interpolation with `[fmt|...|]`
- `Result` not `Either`, `Task` not `IO`
- INLINE pragmas on hot paths
- Strict fields on hot-path data types
- Export from Core module if appropriate

---

### PHASE 8: Build & Test Loop

Run until all tests pass (max 10 iterations):

```bash
cabal build all
cabal test
```

**If tests fail:** Fix implementation. NEVER modify tests.

---

### PHASE 9: Security Implementation Review

Invoke `neohaskell-security-architect` on the implemented code.

Checklist:
- [ ] Input validation (parse, don't validate)
- [ ] No injection vectors
- [ ] Overflow/bounds handling
- [ ] No information disclosure in error messages

Output: `security-impl-notes.md` in the notepad directory.

---

### PHASE 10: Performance Implementation Review

Invoke `neohaskell-performance-lead` on the implemented code.

Checklist:
- [ ] INLINE pragmas on hot paths
- [ ] No unnecessary allocations
- [ ] Strict fields where needed
- [ ] Efficient serialization (toEncoding over toJSON)
- [ ] Strict folds over event streams

Output: `performance-impl-notes.md` in the notepad directory.

---

### PHASE 11: Fix Review Notes

Apply fixes from security and performance reviews. Delegate to implementation agent.

---

### PHASE 12: Final Build & Test

```bash
cabal build all
cabal test
```

All tests must pass. Run `hlint .` for lint check.

---

### PHASE 13: Create PR — PAUSE

Use `git-master` skill for all git operations.

```bash
git checkout -b {BRANCH_NAME}
git add --all
git commit -m "feat(core): {FEATURE_NAME}

Closes #{ISSUE_NUMBER}

- [Change 1]
- [Change 2]
- [Change 3]"

git push -u origin {BRANCH_NAME}
gh pr create --title "feat(core): {FEATURE_NAME}" --body "$(cat <<'EOF'
Closes #{ISSUE_NUMBER}

## Summary
[Brief description]

## Changes
- [Change 1]
- [Change 2]

## Checklist
- [x] ADR created
- [x] Security review passed
- [x] Performance review passed
- [x] Tests written and passing
- [x] Documentation updated
EOF
)"
```

**PAUSE: Report PR URL and wait for bot reviews.**

---

### PHASE 14: Wait for Bots — PAUSE

Wait for:
- CodeRabbit AI review
- CI checks (Linux + macOS workflows)

**PAUSE: Report bot comments, ask if human wants to review before fixing.**

---

### PHASE 15: Fix Bot Comments

Address issues raised by bots. Tests remain IMMUTABLE.

---

### PHASE 16: Final Review

Verify:
- [ ] All bot comments addressed
- [ ] Tests still pass
- [ ] No security regressions
- [ ] Performance acceptable
- [ ] NeoHaskell style compliance

---

### PHASE 17: Merge — PAUSE

Wait for CI to pass.

**PAUSE: "CI passed. Merge PR? [y/n]"**

On approval:
```bash
gh pr merge --squash
```

## Deliverables Checklist

- [ ] `docs/decisions/XXXX-{feature-slug}.md` — ADR
- [ ] `{MODULE_PATH}` — Implementation
- [ ] `{TEST_PATH}` — Tests  
- [ ] PR merged to main
- [ ] Issue #{ISSUE_NUMBER} closed

## Agent Coordination Reference

| Agent | Phases | Role |
|-------|--------|------|
| `neohaskell-devex-lead` | 1, 4, 5 | API design, naming, module structure, ADR |
| `neohaskell-security-architect` | 2, 9 | Security review (OWASP/NIST/EU) |
| `neohaskell-performance-lead` | 3, 10 | Performance review (50k req/s target) |
| `neohaskell-community-lead` | 13 | PR description, release notes |
| `git-master` (skill) | 13, 17 | Git operations, PR creation, merge |

## NeoHaskell Style Quick Reference

| Rule | Example |
|------|---------|
| Pipe over nesting | `x \|> foo \|> bar` not `bar $ foo x` |
| Do blocks only | `do let y = ...` not `let y = ... in ...` |
| Case only | `case x of ...` not pattern matching in function defs |
| Descriptive types | `forall element result.` not `forall a b.` |
| Qualified imports | `Module.function` design |
| String interpolation | `[fmt\|Hello {name}!\|]` not `<>` |
| Result not Either | `Result error value` |
| Task not IO | `Task err val` |
| Strict hot paths | `!` fields, `{-# INLINE fn #-}` |
