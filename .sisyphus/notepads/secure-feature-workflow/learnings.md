# Learnings - Secure Feature Workflow

This notepad captures conventions, patterns, and wisdom discovered during workflow definition.

---

## [2026-01-26T16:35:00Z] Task 1: Workflow Entry Criteria

**Patterns Discovered:**
- Entry criteria must be concrete yes/no questions, not subjective evaluations
- Decision tree needs clear if/then logic with no ambiguous branches
- Three-tier approach prevents over-engineering: Full (security surface) / Standard (no security) / Fast-Path (bug fixes)
- Examples must use actual codebase features (Cart, Stock) not hypothetical scenarios

**Decision Tree Logic:**
- First branch: Bug fix vs new feature (determines Fast-Path eligibility)
- Second branch (bug fixes): Size, scope, security sensitivity (gates Fast-Path)
- Third branch (new features): Security surface check (gates Full vs Standard)
- No circular logic: Each path leads to exactly one workflow

**Entry Criteria Patterns:**
- Full Workflow: ANY criterion true (user input OR auth OR external data OR cross-domain)
- Standard Workflow: ALL criteria true (internal AND pure AND no security surface)
- Fast-Path: ALL criteria true (bug fix AND small AND no API change AND not security code)
- Upgrade criteria: Fast-Path → Full if ANY disqualifier (prevents under-engineering)

**Examples Mapping:**
- Cart → Full Workflow: Handles user input (cartId, quantity), cross-domain (Cart→Stock), security surface exists
- Stock query → Standard: Read-only, no user input processing, framework handles auth, no security surface
- Bug fix (error message) → Fast-Path: <50 lines, single file, no API change, not security code

**Key Insight:**
The decision tree must be deterministic - same feature always routes to same workflow. No "judgment calls" allowed.


## [2026-01-26T16:40:00Z] Tasks 2-4: Workflow Steps and Checklists

**Full Workflow Steps (Task 2):**
- 9-step process with clear purpose, inputs, outputs, exit criteria, recovery for each
- Build verification (Step 6) has 5 sub-checks: unit tests, integration tests, build, lint, doctests
- Security reviews at Step 2 (pre-review) and Step 7 (post-review) with agent invocations
- Tests become immutable after Step 4 (scope lock) - implementation must match tests
- Recovery procedures prevent circular loops (e.g., Step 8 → Step 7, not Step 7 → Step 8 → Step 7)

**Security Checklist (Task 3):**
- Mapped to OWASP Top 10 2021 (9 items), NIST CSF 2.0 (5 functions), MAGERIT v3 (8 safeguards)
- Each item is yes/no question with concrete example from codebase
- Skip criteria: pure utility + internal type + test helper = skip security review
- Examples reference actual files: Auth/Jwt.hs, Auth/UrlValidation.hs, Auth/OAuth2/StateToken.hs

**Performance Checklist (Task 4):**
- 50k req/s applies ONLY to: auth paths, event dispatching, HTTP hot paths, critical DB queries
- Skip criteria prevents over-engineering: if not hot path, skip performance review
- Performance patterns: lock-free reads, connection pooling, parallel processing, optimistic concurrency
- Target: <20μs per operation (50k req/s = 1 op per 20μs)
- Examples: JWT validation (YES - hot path), Cart query (NO - not hot path)

**Key Patterns:**
- Exit criteria must be measurable (commands to run, expected outputs)
- Recovery procedures must be concrete (not "figure it out")
- Agent invocations must specify agent name (neohaskell-security-architect, neohaskell-community-lead)
- Checklists must have skip criteria to prevent over-application


## [2026-01-26T16:45:00Z] Tasks 5-6: Workflow Variants

**Standard Workflow (Task 5):**
- Skips Steps 2, 7, 8 (security reviews) but keeps ALL other steps
- Step 6 (build verification) is NOT skipped - all 6a-6e checks required
- Justification template mandatory: "Security review skipped: [reason]"
- Valid reasons: pure utility, read-only query, internal type, test helper
- Example: CartSummary query (read-only, framework handles auth)

**Fast-Path Workflow (Task 6):**
- Skips Steps 1-3, 5, 7-8 (specification, security, unit test specs)
- Starts at Step 4 (regression test) - MANDATORY
- Step 6 (build verification) is NOT skipped - all 6a-6e checks required
- Upgrade criteria: ANY trigger (new API, security code, >50 lines, architectural) → Full Workflow
- Example: Fix typo in error message (Fast-Path), Fix auth bypass (upgrade to Full)

**Key Patterns:**
- Build verification (Step 6) is NEVER skipped in any workflow variant
- Regression test is mandatory for Fast-Path (captures bug behavior)
- Upgrade criteria prevent under-engineering (Fast-Path → Full when needed)
- Justification is mandatory for Standard Workflow (prevents lazy skipping)

**Workflow Comparison:**
- Full: All 9 steps (security surface exists)
- Standard: Steps 1, 3-6, 9 (no security surface, justified)
- Fast-Path: Steps 4, 6, 9 (bug fix, localized, not security-sensitive)


## [2026-01-26T16:50:00Z] Task 7: Recovery Procedures

**Recovery Procedure Patterns:**
- Each step has three components: Recovery (what to do), Rollback (how to undo), Escalation (when to ask for help)
- Recovery must be concrete (not "figure it out") - specific commands and actions
- Rollback uses git commands: checkout (discard), stash (save), reset (hard reset), diff (review)
- Escalation has timeouts: 24h (user unavailable), 2h (can't compile), 4h (stuck), 3+ rejections (architectural)

**Workflow Health Check:**
- Four checks before starting: build, test, lint, integration tests
- Prevents introducing new work on broken base
- All must pass before proceeding with workflow

**Circular Recovery Prevention:**
- Step 7 → Step 8 → Step 6 → Step 7 (correct: includes Step 6 verification)
- NOT Step 7 → Step 8 → Step 7 (incorrect: skips verification)
- Step 8 always re-runs Step 6 checks before looping to Step 7

**Rollback Strategies:**
- Steps 1-2: N/A (no code changes yet)
- Steps 3-5: `git checkout` (discard changes)
- Step 6: `git stash` (save for review)
- Step 7: `git reset --hard` (architectural changes)
- Step 8: `git diff` + selective revert
- Step 9: N/A (PR is WIP)

**Escalation Triggers:**
- Time-based: 24h (user), 2h (compile), 4h (stuck)
- Rejection-based: 3+ rejections (architectural guidance needed)
- Infrastructure-based: CI failure not code-related


## [2026-01-26T16:55:00Z] Task 8: Workflow Examples and Validation

**Cart Domain Walkthrough (Full Workflow):**
- All 9 steps mapped to actual implementation files
- Security checklist applied: OWASP (9 items), NIST (5 functions), MAGERIT (8 safeguards)
- Performance review skipped (not hot path) - demonstrates skip criteria working
- Files referenced: Core.hs, Commands/, Queries/, Integrations.hs, tests/

**Gaps Identified (Honest Assessment):**
- Gap 1: Security pre-review would not have caught issues (Cart is well-designed already)
  - Refinement: Workflow is validation, not creativity - prevents bad implementations
- Gap 2: Unit tests written AFTER implementation historically (violates TDD)
  - Refinement: Workflow emphasizes Step 5 BEFORE Step 6 - codifies best practices

**Bug Fix Example (Fast-Path):**
- Demonstrates Fast-Path correctly: Steps 4, 6, 9 only
- Regression test captures bug behavior (fails before fix, passes after)
- All Step 6 checks still required (no shortcuts on verification)
- Example: Fix error message "Entity not found" → "Cart not found"

**Validation Outcome:**
- Workflow maps to real implementation (Cart domain)
- Gaps are honest (not cherry-picked positives)
- Workflow prevents future mistakes (TDD emphasis)
- Skip criteria work (performance review skipped for Cart)

**Key Insight:**
Workflow is prescriptive (how to do it right) not descriptive (how it was done). Historical deviations (Gap 2) validate the need for the workflow.

