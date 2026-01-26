# Decisions - Secure Feature Workflow

This notepad captures architectural and design decisions made during workflow definition.

---

## [2026-01-26T17:00:00Z] Workflow Definition Complete

**Decision: Three-Tier Workflow System**
- Full Workflow: Features with security surface (9 steps)
- Standard Workflow: Features without security surface (7 steps, skips security reviews)
- Fast-Path: Bug fixes (3 steps, minimal overhead)

**Decision: Standards-Based Security**
- Replaced ambiguous "EU-grade security" with concrete checklists
- Mapped to OWASP Top 10 2021, NIST CSF 2.0, MAGERIT v3
- Each item is yes/no question (not subjective judgment)

**Decision: Performance Applicability**
- 50k req/s applies ONLY to: auth paths, event dispatching, HTTP hot paths, critical DB queries
- Skip criteria prevents over-engineering for non-hot-path features
- Target: <20μs per operation

**Decision: Tests Immutable After Step 4**
- Scope lock: tests become requirements after Step 4
- Implementation must match tests (not vice versa)
- Prevents scope creep during implementation

**Decision: Build Verification Never Skipped**
- Step 6 (6a-6e checks) required in ALL workflow variants
- No shortcuts on quality gates
- Ensures correctness regardless of workflow path

**Rationale:**
These decisions address Metis findings and prevent over-engineering while maintaining quality standards.

