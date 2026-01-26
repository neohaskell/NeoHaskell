# Secure Feature Workflow

## Context

### Original Request
User wants a repeatable workflow plan for implementing NeoHaskell features with:
- Standards-based security (NIST CSF 2.0, OWASP Top 10, MAGERIT v3 safeguards)
- 50k req/s performance (where applicable - auth hot paths, event dispatching)
- Correctness guarantees (tests pass, compiles, lint clean)

The workflow should integrate specialized agents (security-architect, community-lead) and follow the existing outside-in TDD methodology documented in CLAUDE.md.

**User Clarification**: Security reviews should focus on industry standards (NIST, OWASP, MAGERIT).

### Metis Review Summary

**Key Findings**:
- 50k req/s is NOT universal - applies only to auth hot paths and event dispatching
- "EU-grade security" needs concrete definition - replaced with standards-based checklist (NIST CSF 2.0, OWASP Top 10, MAGERIT v3)
- User clarified: Focus on industry security standards, not developer UX approach
- Workflow needs tiered paths (full/standard/fast) to prevent over-engineering
- Missing explicit exclusions and applicability criteria
- Need measurable exit criteria for each step

**Applied Guardrails**:
- Feature boundary defined in testbed usage (Step 3)
- Test scope locked after Hurl tests written (Step 4 - tests become immutable requirements)
- Build verification steps added before security re-review
- Fast-path defined for bug fixes
- Applicability criteria for performance and security reviews

---

## Work Objectives

### Core Objective
Establish a repeatable, tiered workflow process for implementing NeoHaskell features with appropriate security, performance, and correctness guarantees based on feature requirements.

### Concrete Deliverables
- Workflow documentation in `.sisyphus/plans/secure-feature-workflow.md`
- Three workflow paths (Full/Standard/Fast) with clear entry criteria
- Security review checklist (replacing "EU-grade" with measurable criteria)
- Performance applicability checklist (when 50k req/s applies)
- Per-step exit criteria and failure recovery procedures

### Definition of Done
- [ ] Workflow document exists and is referenced by agents
- [ ] At least one example feature has been implemented using this workflow (validation)
- [ ] All three workflow paths have entry criteria and exit criteria
- [ ] Security and performance checklists are concrete and measurable
- [ ] Fast-path exists for bug fixes without over-engineering

### Must Have
- Tiered workflow paths (full/standard/fast) with explicit entry criteria
- Measurable security checklist mapped to NIST CSF 2.0, OWASP Top 10, MAGERIT v3
- Performance applicability check (auth/dispatch/hot-path vs. other)
- Build verification steps (build, lint, doctest, integration tests)
- Failure recovery procedures for each step
- Explicit "Must NOT Have" sections to prevent scope creep

### Must NOT Have (Guardrails)
- Universal 50k req/s requirement for ALL features (only hot paths)
- Ambiguous security terms without standards mapping (use NIST/OWASP/MAGERIT)
- Security review for features with no security surface (pure utilities)
- Test expectation modifications during implementation (tests are requirements)
- Workflow proceeding without written specification (Step 1 exit criteria)
- Premature abstractions flagged by security architect in Step 7

---

## Verification Strategy

### Manual QA Only (No Test Infrastructure Yet)

This is a workflow definition (process document), not executable code. Verification is manual review against completeness criteria.

**Evidence Required:**
- Document structure matches template
- All three paths (full/standard/fast) have entry criteria
- Each step has measurable exit criteria
- Security and performance checklists are concrete
- Examples reference actual NeoHaskell patterns (testbed/CLAUDE.md)

**Manual Verification Procedures**:

**For Workflow Structure:**
- [ ] Read through workflow document
- [ ] Verify each workflow path has entry criteria
- [ ] Verify each step has exit criteria and recovery procedures
- [ ] Verify checklists are measurable (yes/no questions, not "evaluate")

**For Security Checklist:**
- [ ] Read security checklist
- [ ] Verify items map to NIST CSF 2.0, OWASP Top 10, or MAGERIT v3 standards
- [ ] Verify no ambiguous terms like "EU-grade" without definition
- [ ] Verify each item is verifiable (not subjective judgment)

**For Performance Checklist:**
- [ ] Read performance applicability section
- [ ] Verify it references actual 50k req/s targets from codebase (JWT, JWKS, dispatcher)
- [ ] Verify it includes skip criteria for non-hot-path features

**For Example Feature Validation:**
- [ ] Pick one existing feature (e.g., Cart domain)
- [ ] Walk through workflow steps
- [ ] Verify workflow maps to actual implementation history
- [ ] Identify any gaps where workflow doesn't match reality

---

## Task Flow

All tasks are sequential (workflow definition is built step-by-step).

---

## TODOs

- [ ] 1. Document Workflow Entry Criteria (Three Paths)

  **What to do**:
  - Define entry criteria for **Full Workflow** (new features with security surface)
  - Define entry criteria for **Standard Workflow** (new features without security surface)
  - Define entry criteria for **Fast-Path** (bug fixes with regression tests)
  - Create decision tree to determine which path to follow

  **Must NOT do**:
  - Allow features to "upgrade" paths mid-workflow (choose path at start, commit)
  - Create ambiguous boundaries (use concrete checklist: "Does feature handle user input?" etc.)

  **Parallelizable**: NO (foundation for all other tasks)

  **References**:
  - `AGENTS.md:104-108` - Outside-in development, tests before code principles
  - `testbed/tests/scenarios/stock-reservation.hurl` - Example of complete feature with security surface (cross-domain coordination)
  - `testbed/tests/commands/create-cart.hurl` - Example of simple feature (single-domain command)
  - Metis finding: "Workflow too heavy for small features - add fast-path for bug fixes"

  **Acceptance Criteria**:
  - [ ] **Full Workflow** entry criteria:
    - Feature handles user input OR
    - Feature touches authentication/authorization OR
    - Feature processes external data OR
    - Feature has cross-domain coordination
  - [ ] **Standard Workflow** entry criteria:
    - Feature is internal utility OR
    - Feature is pure data transformation OR
    - Feature has no security surface
  - [ ] **Fast-Path** entry criteria:
    - Feature is bug fix with existing test coverage OR
    - Feature is refactoring with existing tests OR
    - Change is < 50 lines in single file
  - [ ] Decision tree diagram included in workflow document
  - [ ] Examples provided for each path

  **Manual Verification**:
  - [ ] Using existing features (Cart, Stock, Document):
    - Cart feature → Full Workflow (handles user input, cross-domain)
    - Stock query → Standard Workflow (read-only, no security surface)
    - Hypothetical "fix typo in error message" → Fast-Path
  - [ ] Read decision tree and verify it's unambiguous

  **Commit**: NO (groups with 2-4 as "workflow definition")

---

- [ ] 2. Define Full Workflow Steps (New Feature with Security)

  **What to do**:
  - Document all 9 steps of full workflow
  - For each step: purpose, inputs, outputs, exit criteria, recovery procedure
  - Include agent invocations (security-architect, community-lead)
  - Include build verification sub-steps

  **Must NOT do**:
  - Skip exit criteria (every step must have measurable success condition)
  - Leave recovery procedures undefined (what happens on failure?)
  - Apply 50k req/s requirement universally (use applicability check)

  **Parallelizable**: NO (depends on Task 1)

  **References**:
  - User's original request for 9-step workflow
  - `CLAUDE.md:35-57` - Outside-in development 5-step process
  - Metis finding: "Add build verification steps between TDD and security review"
  - `testbed/scripts/run-tests.sh` - Integration test runner pattern

  **Acceptance Criteria**:

  **Step 1: Evaluate Task Specifications**
  - [ ] Purpose: Write feature specification with acceptance criteria
  - [ ] Input: User requirement (natural language)
  - [ ] Output: Written specification document (`.sisyphus/specs/{feature}.md`)
  - [ ] Exit criteria: Specification has concrete deliverables, acceptance criteria, and "Must NOT Have" section
  - [ ] Recovery: If ambiguous, interview user for clarification

  **Step 2: Security & Performance Pre-Review**
  - [ ] Purpose: Threat modeling and performance applicability check
  - [ ] Input: Specification from Step 1
  - [ ] Agent: `neohaskell-security-architect`
  - [ ] Output: Security review report with threat model, performance applicability decision
  - [ ] Exit criteria: Security architect returns "APPROVED" or documented mitigation plan
  - [ ] Performance applicability checked (see Task 3 for checklist)
  - [ ] Recovery: If rejected, update specification and resubmit

  **Step 2.5: Define Explicit Exclusions (Metis addition)**
  - [ ] Purpose: Lock scope boundaries to prevent creep
  - [ ] Input: Specification with "Must Have"
  - [ ] Output: "Must NOT Have" section added to specification
  - [ ] Exit criteria: At least 3 explicit exclusions documented
  - [ ] Recovery: N/A (addition only, cannot fail)

  **Step 3: Implement Testbed Usage (As-If Existing)**
  - [ ] Purpose: Write usage code assuming feature already exists
  - [ ] Input: Specification with examples
  - [ ] Output: Testbed code in `testbed/src/Testbed/{Domain}/` (compiles even if tests fail)
  - [ ] Exit criteria: `cabal build nhtestbed` succeeds (implementation stubbed)
  - [ ] Recovery: If doesn't compile, add type stubs to make it compile

  **Step 4: Implement Hurl Integration Tests**
  - [ ] Purpose: Define API contract as executable specification
  - [ ] Input: Testbed usage from Step 3
  - [ ] Output: Hurl test files in `testbed/tests/{commands,queries,scenarios}/`
  - [ ] Exit criteria: Tests run and fail with "404 Not Found" or "Command not registered" (expected failure)
  - [ ] **Tests become immutable after this step** (scope lock)
  - [ ] Recovery: If tests can't run, check testbed server starts correctly

  **Step 5: Implement Unit Test Specs (Topmost Abstraction)**
  - [ ] Purpose: Test abstract interfaces before concrete implementations
  - [ ] Input: Testbed patterns from Step 3
  - [ ] Output: Hspec test files in `core/testlib/Test/{Module}/Spec.hs`
  - [ ] Exit criteria: Tests compile and fail with "not implemented" or similar
  - [ ] Recovery: If tests don't compile, stub implementations in `core/`

  **Step 6: Outside-In TDD Until Wired**
  - [ ] Purpose: Implement feature following compiler guidance
  - [ ] Input: Failing tests from Steps 4-5
  - [ ] Output: Feature implementation in `core/` with all tests passing
  - [ ] Exit criteria (ALL must pass):
    - [ ] 6a. All unit tests pass (`cabal test nhcore-test`)
    - [ ] 6b. All integration tests pass (`./testbed/scripts/run-tests.sh`)
    - [ ] 6c. Build succeeds (`cabal build all`)
    - [ ] 6d. Lint clean (`hlint .` → no warnings)
    - [ ] 6e. Doctests pass (`./scripts/run-doctest`)
  - [ ] Recovery: If any check fails, fix and re-run all checks

  **Step 7: Security & Performance Post-Review**
  - [ ] Purpose: Verify implementation meets security/performance requirements
  - [ ] Input: Implemented feature passing all checks
  - [ ] Agent: `neohaskell-security-architect`
  - [ ] Output: Security review report with implementation verification
  - [ ] Exit criteria: Security architect returns "APPROVED" or issues are documented
  - [ ] Recovery: If issues found, proceed to Step 8

  **Step 8: Fix Issues from Review**
  - [ ] Purpose: Address security architect's findings
  - [ ] Input: Issue list from Step 7
  - [ ] Output: Fixed implementation
  - [ ] Exit criteria: All issues resolved, re-run Step 6 checks
  - [ ] Recovery: Loop back to Step 7 if new issues introduced

  **Step 9: Generate PR via Community Lead**
  - [ ] Purpose: Create PR with appropriate context and documentation
  - [ ] Input: Completed feature with passing checks
  - [ ] Agent: `neohaskell-community-lead`
  - [ ] Output: GitHub PR with description, screenshots, tests
  - [ ] Exit criteria: PR created and CI passes
  - [ ] Recovery: If CI fails, fix and update PR

  **Manual Verification**:
  - [ ] Read through all 9 steps
  - [ ] Verify each step has purpose, inputs, outputs, exit criteria, recovery
  - [ ] Verify agent invocations are concrete (not "evaluate" but "invoke X agent")
  - [ ] Verify exit criteria are measurable (commands to run, expected outputs)

  **Commit**: NO (groups with 1, 3-4 as "workflow definition")

---

- [ ] 3. Define Security Review Checklist (Standards-Based: NIST, OWASP, MAGERIT)

  **What to do**:
  - Replace ambiguous "EU-grade security" with standards-based checklist
  - Map to NIST Cybersecurity Framework, OWASP Top 10, MAGERIT risk categories
  - Include measurable criteria (yes/no questions)
  - Reference existing security patterns from codebase

  **Must NOT do**:
  - Include subjective criteria ("is it secure enough?")
  - Reference undefined terms without explanation
  - Create checkbox compliance without technical verification

  **Parallelizable**: YES (with Task 4 - both are checklists)

  **References**:
  - `core/auth/Auth/Jwt.hs` - JWT validation with RFC 8725 hardening
  - `core/auth/Auth/UrlValidation.hs` - HTTPS enforcement, SSRF protection
  - `core/auth/Auth/OAuth2/StateToken.hs` - CSRF protection with HMAC-SHA256
  - `core/service/Service/Query/Auth.hs` - Authorization patterns (whitelist approach)
  - `core/http/Http/Client.hs` - URL sanitization to prevent secret leakage
  - NIST CSF 2.0: Identify, Protect, Detect, Respond, Recover functions
  - OWASP Top 10 2021: A01 (Broken Access Control), A02 (Cryptographic Failures), A03 (Injection), A05 (Security Misconfiguration), A07 (Identification & Auth Failures)
  - MAGERIT v3: Information assets, threats, vulnerabilities, safeguards

  **Acceptance Criteria**:

  **Security Checklist** (ALL must be YES or N/A with justification):

  **OWASP Top 10 Coverage**:
  - [ ] **A01 Broken Access Control**: Authorization checks enforced (default deny, explicit allow)?
  - [ ] **A02 Cryptographic Failures**: Sensitive data encrypted in transit (HTTPS) and at rest where applicable?
  - [ ] **A03 Injection**: All inputs validated and sanitized (SQL, command, path, CRLF)?
  - [ ] **A04 Insecure Design**: Threat modeling completed? Trust boundaries identified?
  - [ ] **A05 Security Misconfiguration**: Secure defaults used? No debug/verbose errors in production?
  - [ ] **A07 Identification & Auth Failures**: Authentication required? Session management secure (CSRF tokens, expiry)?
  - [ ] **A08 Software/Data Integrity**: Dependencies verified? No untrusted data deserialization?
  - [ ] **A09 Logging Failures**: Security events logged? No sensitive data in logs?
  - [ ] **A10 SSRF**: External requests validated (HTTPS-only, no private IPs)?

  **NIST CSF 2.0 Functions**:
  - [ ] **Identify (ID)**: Asset classification done? (public-facing vs internal, sensitive data identified)
  - [ ] **Protect (PR)**: Access control implemented? Data protection at rest/transit verified?
  - [ ] **Detect (DE)**: Security monitoring considered? (logging, anomaly detection for public endpoints)
  - [ ] **Respond (RS)**: Incident response plan exists? (what happens if vulnerability found?)
  - [ ] **Recover (RC)**: Rollback procedure defined? (can feature be safely disabled?)

  **MAGERIT v3 Safeguards**:
  - [ ] **[D.1] Backup copies**: Critical data has backup/recovery? (EventStore has replay capability)
  - [ ] **[D.2] Secure waste management**: Secrets properly erased? (no secrets in logs, redacted Show instances)
  - [ ] **[IA.1] User identification**: Authentication mechanism enforces identity? (JWT, OAuth2)
  - [ ] **[IA.2] User authentication**: Strong authentication used? (not plaintext passwords, proper token validation)
  - [ ] **[AC.1] Least privilege**: Users only access what they need? (permission-based access control)
  - [ ] **[AC.2] Access control enforcement**: Authorization checks cannot be bypassed?
  - [ ] **[MP.1] Encryption**: Cryptographic algorithms approved? (HMAC-SHA256 for CSRF, TLS 1.2+ for HTTPS)
  - [ ] **[MP.4] Secure communication**: External comms use secure channels? (HTTPS enforcement)

  **Applicability Criteria** (when to skip security review):
  - [ ] Feature is pure utility function (no IO, no user input)
  - [ ] Feature is internal type definition (no runtime behavior)
  - [ ] Feature is test helper (not production code)
  - If ALL above are YES → Skip security review, document: "Security review skipped: pure utility"

  **Manual Verification**:
  - [ ] Read security checklist
  - [ ] Verify each item is yes/no question (not subjective judgment)
  - [ ] Pick example from codebase: `Auth/OAuth2/Client.hs`
    - A01 Access Control: YES (OAuth2 enforces authorization)
    - A02 Crypto Failures: YES (HTTPS enforced, secrets redacted)
    - A03 Injection: YES (URL sanitized, SSRF checks)
    - A05 Misconfiguration: YES (secure defaults, URL validation)
    - A07 Auth Failures: YES (StateToken CSRF protection, token validation)
    - A10 SSRF: YES (UrlValidation.hs blocks private IPs, enforces HTTPS)
    - PR (Protect): YES (access control, data protection via HTTPS)
    - IA.1-2 (Identity/Auth): YES (JWT validation, OAuth2 flow)
    - MP.1 (Encryption): YES (HMAC-SHA256 for state tokens)
    - MP.4 (Secure Comm): YES (HTTPS enforcement)
  - [ ] Checklist matches actual implementation patterns

  **Commit**: NO (groups with 1-2, 4 as "workflow definition")

---

- [ ] 4. Define Performance Review Checklist (50k req/s Applicability)

  **What to do**:
  - Define when 50k req/s requirement applies (not universal)
  - Reference actual performance targets from codebase (JWT, JWKS, dispatcher)
  - Provide skip criteria for non-hot-path features
  - Include performance patterns (lock-free, connection pooling, concurrency)

  **Must NOT do**:
  - Apply 50k req/s to ALL features (over-engineering risk)
  - Include performance review without applicability check
  - Recommend premature optimization

  **Parallelizable**: YES (with Task 3 - both are checklists)

  **References**:
  - `core/auth/Auth/Jwks.hs` - Lock-free JWKS manager with 50k req/s target
  - ADR-0009 (JWT authentication) - 50k req/s for key lookups
  - ADR-0010 (OAuth2 provider) - 50k req/s for `/connect` endpoints
  - `core/service/Service/Integration/Dispatcher.hs` - 50k+ events/second for worker lookups
  - `core/concurrency/ConcurrentMap.hs` - Lock-free STM-based concurrent map
  - `core/service/Service/EventStore/Postgres/Internal.hs` - Connection pooling pattern
  - Metis finding: "50k req/s is NOT universal. It applies ONLY to authentication hot paths and event dispatching"

  **Acceptance Criteria**:

  **Performance Applicability Check** (50k req/s required if ANY are YES):
  - [ ] Feature touches authentication/authorization paths (JWT validation, permission checks)?
  - [ ] Feature touches event dispatching/processing (Integration.Dispatcher)?
  - [ ] Feature is HTTP hot path (called on every request)?
  - [ ] Feature is database query in critical path (EventStore, QueryObjectStore)?

  **If NO to all above**:
  - [ ] Document: "Performance review skipped: not in hot path"
  - [ ] Skip performance checklist
  - [ ] Default to correctness-first approach

  **If YES to any above** → Performance Checklist:
  - [ ] **Lock-Free Reads**: Hot path uses lock-free data structures (AtomicVar, ConcurrentMap)?
  - [ ] **Connection Pooling**: Database connections pooled (Hasql pool, not per-request)?
  - [ ] **Parallel Processing**: Independent operations use `AsyncTask.runConcurrently`?
  - [ ] **Optimistic Concurrency**: Writes use optimistic locking (event store position-based)?
  - [ ] **Chunked Processing**: Large operations use chunked iteration (ConcurrentMap.forEachChunked)?
  - [ ] **Background Refresh**: Cached data refreshes in background, not blocking requests?
  - [ ] **Backpressure**: Bounded channels used where unbounded would cause memory issues?

  **Performance Testing Criteria**:
  - [ ] Benchmark exists using Criterion (in `core/bench/` directory)
  - [ ] Benchmark measures hot path (target: <20μs for 50k req/s = 1 operation per 20μs)
  - [ ] Benchmark compared against similar existing feature (JWT validation, JWKS lookup)

  **Manual Verification**:
  - [ ] Read performance applicability check
  - [ ] Verify it references actual 50k req/s targets from codebase
  - [ ] Pick examples:
    - JWT validation → YES (auth hot path, has 50k req/s target, uses lock-free AtomicVar)
    - Cart query → NO (not hot path, skip performance review)
  - [ ] Read performance checklist
  - [ ] Verify items reference actual patterns (AtomicVar, ConcurrentMap, AsyncTask)
  - [ ] Verify skip criteria prevents over-engineering

  **Commit**: NO (groups with 1-3 as "workflow definition")

---

- [ ] 5. Define Standard Workflow (No Security Surface)

  **What to do**:
  - Document simplified workflow for features without security surface
  - Skip Steps 2, 7 (security reviews)
  - Keep Steps 3-6 (testbed usage, tests, TDD)
  - Add skip justification template

  **Must NOT do**:
  - Skip build verification (Steps 6a-6e still required)
  - Skip testing (Hurl and unit tests still required)
  - Allow "no security surface" without justification

  **Parallelizable**: YES (with Task 6 - both are workflow variants)

  **References**:
  - Metis finding: "Add standard workflow for new features without security surface"
  - `testbed/src/Testbed/Cart/Queries/CartSummary.hs` - Read-only query with no security surface

  **Acceptance Criteria**:

  **Standard Workflow Steps**:
  - [ ] Step 1: Evaluate Task Specifications (same as Full Workflow)
  - [ ] Step 2: SKIPPED (security review)
    - [ ] Justification template: "Security review skipped: [feature is pure utility | no user input | read-only query | other]"
  - [ ] Step 3: Implement Testbed Usage (same as Full Workflow)
  - [ ] Step 4: Implement Hurl Integration Tests (same as Full Workflow)
  - [ ] Step 5: Implement Unit Test Specs (same as Full Workflow)
  - [ ] Step 6: Outside-In TDD Until Wired (same as Full Workflow - ALL 6a-6e checks)
  - [ ] Step 7: SKIPPED (security re-review)
  - [ ] Step 8: SKIPPED (fix security issues)
  - [ ] Step 9: Generate PR via Community Lead (same as Full Workflow)

  **Entry Criteria** (revisited from Task 1):
  - [ ] Feature is internal utility (no external input) OR
  - [ ] Feature is pure data transformation (no IO) OR
  - [ ] Feature is read-only query with no auth requirements OR
  - [ ] Feature has explicitly no security surface (justified)

  **Skip Justification Examples**:
  - "Security review skipped: pure utility function with no IO"
  - "Security review skipped: read-only query accessing pre-authorized data"
  - "Security review skipped: internal type definition with no runtime behavior"

  **Manual Verification**:
  - [ ] Read standard workflow steps
  - [ ] Verify Steps 2, 7, 8 are explicitly marked SKIPPED
  - [ ] Verify Step 6 (build verification) is NOT skipped
  - [ ] Pick example: `CartSummary` query
    - Entry criteria: Read-only query, no auth in query itself (handled by framework)
    - Step 1: Specification → "Query that returns cart summaries"
    - Step 2: SKIPPED "read-only query accessing pre-authorized data"
    - Steps 3-6: Same as Full Workflow
    - Step 7: SKIPPED
    - Step 9: PR generation
  - [ ] Justification template is clear and mandatory

  **Commit**: NO (groups with 1-4, 6 as "workflow definition")

---

- [ ] 6. Define Fast-Path Workflow (Bug Fixes)

  **What to do**:
  - Document minimal workflow for bug fixes and refactoring
  - Start at Step 4 (Hurl regression test or update existing test)
  - Skip Steps 1-3 (specification, security pre-review, testbed usage)
  - Skip Steps 7-8 (security post-review)
  - Add "upgrade to Full Workflow" criteria

  **Must NOT do**:
  - Skip regression test (Step 4 is mandatory)
  - Skip build verification (Step 6 checks still required)
  - Allow "bug fix" to become "feature" without upgrading workflow

  **Parallelizable**: YES (with Task 5 - both are workflow variants)

  **References**:
  - Metis finding: "Add fast-path for bug fixes (skip Steps 2-3)"
  - Metis finding: "Bug fixes skip to Step 4 with regression test"

  **Acceptance Criteria**:

  **Fast-Path Workflow Steps**:
  - [ ] Step 1-3: SKIPPED (specification, security pre-review, testbed usage)
  - [ ] Step 4: Write Regression Test
    - [ ] If bug is in Hurl scenario, add failing Hurl test case
    - [ ] If bug is in core logic, add failing Hspec test case
    - [ ] Test captures bug behavior (fails before fix, passes after fix)
  - [ ] Step 5: SKIPPED (unit test specs - use Step 4 regression test instead)
  - [ ] Step 6: Fix Bug + TDD Until Wired (same verification as Full Workflow)
    - [ ] All checks pass (6a-6e)
  - [ ] Step 7-8: SKIPPED (security post-review)
  - [ ] Step 9: Generate PR via Community Lead (same as Full Workflow)

  **Entry Criteria** (revisited from Task 1):
  - [ ] Change is bug fix with clear reproduction steps AND
  - [ ] Change is < 50 lines in single file OR single module AND
  - [ ] Change does NOT modify public API (no type signature changes) AND
  - [ ] Change does NOT touch security-sensitive code (auth, validation, sanitization)

  **Upgrade to Full Workflow** (if ANY are YES):
  - [ ] Fix requires new public API?
  - [ ] Fix touches security-sensitive code?
  - [ ] Fix changes > 50 lines or multiple modules?
  - [ ] Fix reveals architectural problem?
  - If YES → Stop, upgrade to Full Workflow from Step 1

  **Manual Verification**:
  - [ ] Read fast-path workflow steps
  - [ ] Verify Steps 1-3, 5, 7-8 are explicitly marked SKIPPED
  - [ ] Verify Step 4 (regression test) is mandatory
  - [ ] Verify Step 6 (build verification) is NOT skipped
  - [ ] Read upgrade criteria
  - [ ] Pick hypothetical example: "Fix typo in error message for invalid cart ID"
    - Entry criteria: Bug fix, < 10 lines, no API change, not security-sensitive
    - Step 4: Add Hurl test expecting correct error message (fails)
    - Step 6: Fix typo, all checks pass
    - Step 9: PR generation
  - [ ] Pick counter-example: "Fix auth bypass in cart creation"
    - Entry criteria: Bug fix BUT touches security-sensitive code
    - Upgrade criteria: YES (touches auth) → Use Full Workflow

  **Commit**: NO (groups with 1-5 as "workflow definition")

---

- [ ] 7. Document Recovery Procedures (Failure Handling)

  **What to do**:
  - For each workflow step, define what to do on failure
  - Include rollback procedures where applicable
  - Define escalation path (when to ask for help)
  - Add "workflow health check" to verify prerequisites

  **Must NOT do**:
  - Leave failure states undefined ("if it fails, figure it out")
  - Skip rollback procedures (how to undo changes?)
  - Create circular recovery (Step X fails → go to Step Y → fails → go to Step X)

  **Parallelizable**: NO (depends on Tasks 2, 5, 6 - all workflow definitions)

  **References**:
  - Metis finding: "Add rollback strategy: If Step 7 fails, revert to Step 6 state"
  - Git workflow patterns (revert, reset, stash)

  **Acceptance Criteria**:

  **Per-Step Recovery Procedures**:

  **Step 1 Failure** (Specification ambiguous):
  - [ ] Recovery: Interview user for clarification
  - [ ] Rollback: N/A (no code changes yet)
  - [ ] Escalation: If user unavailable after 24h, park the work

  **Step 2 Failure** (Security architect rejects):
  - [ ] Recovery: Update specification addressing concerns, resubmit to Step 2
  - [ ] Rollback: N/A (no code changes yet)
  - [ ] Escalation: If 3+ rejections, escalate to devex-lead for architectural guidance

  **Step 3 Failure** (Testbed usage doesn't compile):
  - [ ] Recovery: Add type stubs to make it compile (implementation can be `error "not implemented"`)
  - [ ] Rollback: `git checkout testbed/` to discard changes
  - [ ] Escalation: If can't make it compile after 2h, architectural problem → escalate to oracle

  **Step 4 Failure** (Hurl tests can't run):
  - [ ] Recovery: Check testbed server starts (`cabal run nhtestbed`), verify endpoints registered
  - [ ] Rollback: `git checkout testbed/tests/` to discard test changes
  - [ ] Escalation: If server doesn't start, check logs in testbed console

  **Step 5 Failure** (Unit tests don't compile):
  - [ ] Recovery: Add stubs in `core/` to make tests compile
  - [ ] Rollback: `git checkout core/testlib/` to discard test changes
  - [ ] Escalation: If can't make tests compile, type mismatch → check testbed usage in Step 3

  **Step 6 Failure** (Build verification checks fail):
  - [ ] 6a failure (unit tests): Check test output, fix implementation
  - [ ] 6b failure (build): Check compiler errors, fix type errors
  - [ ] 6c failure (lint): Run `hlint --refactor` to auto-fix, or manually address
  - [ ] 6d failure (doctest): Fix examples in documentation
  - [ ] 6e failure (integration tests): Check Hurl output, fix implementation or test expectations
  - [ ] Rollback: `git stash` to save changes, `git stash pop` to restore after review
  - [ ] Escalation: If stuck for >4h, create draft PR and ask for review

  **Step 7 Failure** (Security architect identifies issues):
  - [ ] Recovery: Proceed to Step 8 (fix issues), then re-run Step 6 checks, then loop back to Step 7
  - [ ] Rollback: If issues require architectural changes, `git reset --hard` and return to Step 3
  - [ ] Escalation: If security architect rejects 3+ times, escalate to devex-lead for guidance

  **Step 8 Failure** (Fixes introduce new issues):
  - [ ] Recovery: Re-run Step 6 checks, if any fail, fix and re-check
  - [ ] Rollback: `git diff` to review changes, selectively revert problematic changes
  - [ ] Escalation: If fixes keep introducing new issues, architectural problem → escalate to oracle

  **Step 9 Failure** (CI fails on PR):
  - [ ] Recovery: Check CI logs, fix failing checks, push updates to PR branch
  - [ ] Rollback: N/A (PR is WIP, iterate until CI passes)
  - [ ] Escalation: If CI failure is infrastructure issue (not code), escalate to devex-lead

  **Workflow Health Check** (run before starting):
  - [ ] `cabal build all` succeeds (codebase compiles)
  - [ ] `cabal test` succeeds (all tests pass)
  - [ ] `hlint .` clean (no lint warnings)
  - [ ] `./testbed/scripts/run-tests.sh` succeeds (integration tests pass)
  - If any fail → Fix before starting workflow, don't introduce new work on broken base

  **Manual Verification**:
  - [ ] Read recovery procedures for each step
  - [ ] Verify each has recovery, rollback, escalation
  - [ ] Verify no circular recovery (A → B → A)
  - [ ] Read workflow health check
  - [ ] Simulate failure scenario: "Step 7 security review finds injection vulnerability"
    - Recovery: Proceed to Step 8, fix sanitization, re-run Step 6 checks, loop to Step 7
    - Rollback: If fix requires rewriting query interface, reset to Step 3
    - Escalation: If 3+ rejections, escalate to devex-lead

  **Commit**: NO (groups with 1-6 as "workflow definition")

---

- [ ] 8. Add Workflow Examples (Validation with Real Features)

  **What to do**:
  - Walk through workflow using existing Cart feature as example
  - Document what each step would have looked like historically
  - Identify gaps where workflow doesn't match reality
  - Refine workflow based on findings

  **Must NOT do**:
  - Invent history that didn't happen (use actual implementation order)
  - Skip gaps (if workflow doesn't match reality, document the mismatch)
  - Cherry-pick examples that fit workflow (use complete feature)

  **Parallelizable**: NO (depends on Tasks 1-7 - validates complete workflow)

  **References**:
  - `testbed/src/Testbed/Cart/` - Complete Cart domain implementation
  - `testbed/tests/scenarios/stock-reservation.hurl` - Multi-step workflow test
  - `CLAUDE.md:35-57` - Documented outside-in process for Cart feature

  **Acceptance Criteria**:

  **Example 1: Cart Domain (Full Workflow)**
  - [ ] Step 1 (Specification):
    - What: "Shopping cart with create, add item, view summary"
    - Deliverables: CreateCart command, AddItem command, CartSummary query
    - Must NOT Have: Checkout, payment, inventory management (separate domains)
  - [ ] Step 2 (Security Pre-Review):
    - Security checklist (OWASP/NIST/MAGERIT):
      - A01 Access Control: YES (Cart uses Auth.Context from framework)
      - A03 Injection: N/A (no external commands or file paths)
      - A05 Misconfiguration: YES (secure defaults, error messages sanitized)
      - A09 Logging: YES (no secrets in cart domain to leak)
      - PR (Protect): YES (CartEvent types enforce valid states)
      - IA.1-2 (Identity/Auth): YES (framework-level JWT validation)
    - Performance applicability: NO (not hot path, skip 50k req/s requirement)
  - [ ] Step 3 (Testbed Usage):
    - File: `testbed/src/Testbed/Cart/Core.hs` (CartEntity, CartEvent, update function)
    - Pattern: Event-sourced entity with pure update logic
  - [ ] Step 4 (Hurl Tests):
    - Files: `tests/commands/create-cart.hurl`, `tests/scenarios/stock-reservation.hurl`
    - Pattern: Captures for ID passing, retry for eventual consistency
  - [ ] Step 5 (Unit Tests):
    - Files: `core/testlib/Test/Service/CommandHandler/Execute/Spec.hs`
    - Pattern: Polymorphic tests against abstract EventStore interface
  - [ ] Step 6 (TDD Implementation):
    - Files: `core/service/Service/CommandExecutor/Core.hs`, `core/service/Service/EventStore/`
    - Pattern: Implement leaf modules (InMemory first, Postgres second)
  - [ ] Step 7 (Security Post-Review):
    - Verify: UUID generation cryptographically secure? (Check: uses `random` package)
    - Verify: Cart entity isolation? (Check: StreamId per cart, optimistic locking)
  - [ ] Step 8 (Fix Issues): N/A (hypothetical: if UUID generation weak, use `uuid` package)
  - [ ] Step 9 (PR Generation): Community lead creates PR with testbed examples, Hurl tests

  **Gaps Identified**:
  - [ ] Gap 1: Cart was implemented before workflow existed - would Step 2 have caught anything?
    - Analysis: Probably not - Cart has no security issues in actual implementation
    - Refinement: Workflow is validation, not creativity - good implementations pass automatically
  - [ ] Gap 2: Step 5 (unit tests) was written AFTER Step 6 (implementation) historically
    - Analysis: This violates TDD principle
    - Refinement: Workflow should emphasize testlib BEFORE core implementation

  **Example 2: Hypothetical Bug Fix (Fast-Path)**
  - [ ] Scenario: "Error message for non-existent cart says 'Entity not found', should say 'Cart not found'"
  - [ ] Entry criteria: Bug fix, <5 lines, no API change, not security-sensitive → Fast-Path
  - [ ] Step 4 (Regression Test):
    ```hurl
    POST http://localhost:8080/commands/add-item
    {"cartId": "00000000-0000-0000-0000-000000000000", "itemId": "item1", "amount": 1}
    HTTP 404
    [Asserts]
    jsonpath "$.error" == "Cart not found"  # Currently fails (says "Entity not found")
    ```
  - [ ] Step 6 (Fix): Update error message in `CommandExecutor/Core.hs` line X
  - [ ] Step 6 checks: All pass
  - [ ] Step 9 (PR): Community lead creates PR with regression test

  **Manual Verification**:
  - [ ] Read Example 1 (Cart domain)
  - [ ] Cross-reference with actual files in `testbed/src/Testbed/Cart/`
  - [ ] Verify workflow steps match actual implementation order (or identify gaps)
  - [ ] Read Example 2 (bug fix)
  - [ ] Verify it follows Fast-Path correctly (skips Steps 1-3, 5, 7-8)
  - [ ] Read gaps identified
  - [ ] Verify gaps are honest (not cherry-picked positives)

  **Commit**: YES
  - Message: `docs(workflow): add secure feature workflow with three paths`
  - Files: `.sisyphus/plans/secure-feature-workflow.md`
  - Pre-commit: N/A (markdown file, no build)

---

## Commit Strategy

| After Task | Message | Files | Verification |
|------------|---------|-------|--------------|
| 8 | `docs(workflow): add secure feature workflow with three paths` | `.sisyphus/plans/secure-feature-workflow.md` | Manual review |

---

## Success Criteria

### Verification Commands

```bash
# Verify workflow document exists
ls -lh .sisyphus/plans/secure-feature-workflow.md

# Verify workflow is well-formed (no syntax errors)
cat .sisyphus/plans/secure-feature-workflow.md

# Validate against real feature (manual check)
# Compare Cart domain implementation against workflow steps
```

### Final Checklist
- [ ] All "Must Have" present:
  - [ ] Three workflow paths (Full/Standard/Fast) with entry criteria
  - [ ] Security checklist replacing "EU-grade" with measurable criteria
  - [ ] Performance applicability check (50k req/s only where needed)
  - [ ] Build verification steps (6a-6e)
  - [ ] Recovery procedures for all steps
  - [ ] "Must NOT Have" sections in each step
- [ ] All "Must NOT Have" absent:
  - [ ] No universal 50k req/s requirement
  - [ ] No ambiguous "EU-grade security" without checklist
  - [ ] No workflow progression without written specification
  - [ ] No test expectation modifications during implementation
- [ ] Examples validate workflow (Cart domain walkthrough)
- [ ] Gaps identified and documented (honest assessment)
