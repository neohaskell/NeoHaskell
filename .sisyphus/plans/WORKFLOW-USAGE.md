# Secure Feature Workflow - Quick Reference

## 📍 File Location
```
.sisyphus/plans/secure-feature-workflow.md
```

## 🚀 How to Use (3 Steps)

### 1️⃣ Open the workflow file
```bash
# View in editor
code .sisyphus/plans/secure-feature-workflow.md

# Or read specific sections
less .sisyphus/plans/secure-feature-workflow.md
```

### 2️⃣ Use the Decision Tree (Line 786)
Answer these questions to pick your workflow:

**Question 1: Is this a bug fix?**
- NO → Go to Question 3
- YES → Go to Question 2

**Question 2: Fast-Path eligible?** (All must be YES)
- [ ] Change < 50 lines in single file?
- [ ] No API changes?
- [ ] Not security-sensitive code?
→ **All YES = Fast-Path** | **Any NO = Full Workflow**

**Question 3: Security surface?** (Any YES triggers Full)
- [ ] Handles user input?
- [ ] Touches auth/authz?
- [ ] Processes external data?
- [ ] Cross-domain coordination?
→ **Any YES = Full Workflow** | **All NO = Standard Workflow**

### 3️⃣ Jump to Your Workflow Section

| Workflow | Line Range | When to Use |
|----------|-----------|-------------|
| **Fast-Path** | 1850-2045 | Bug fixes, <50 lines, no security |
| **Standard** | 1713-1848 | New features, no security surface |
| **Full** | 1073-1355 | New features WITH security surface |

## 📋 What You'll Find

**Decision Tree** (line 786)
- Visual flowchart to pick your path
- Clear yes/no criteria

**Entry Criteria** (lines 838-929)
- Detailed checklists for each workflow
- Concrete examples from codebase

**Full Workflow** (lines 1073-1355)
- All 9 steps with exit criteria
- Security & performance reviews
- Recovery procedures

**Security Checklist** (lines 1357-1547)
- OWASP Top 10 (9 items)
- NIST CSF 2.0 (5 functions)
- MAGERIT v3 (8 safeguards)
- Example: Auth.Jwt, Auth.OAuth2

**Performance Checklist** (lines 1549-1711)
- 50k req/s applicability check
- When to optimize vs when to skip
- Patterns: lock-free, pooling, concurrency

**Recovery Procedures** (lines 2047-2367)
- What to do when each step fails
- Rollback commands
- Escalation paths

**Real Example** (lines 2369-2748)
- Cart domain walkthrough
- All 9 steps applied to actual feature
- Gaps identified honestly

## 💡 Quick Examples

### Example 1: New Cart Feature
```
Decision: Handles user input + cross-domain → Full Workflow
Start at: Line 1073
Follow: All 9 steps
Security: Use checklist lines 1357-1547
```

### Example 2: Stock Query
```
Decision: Read-only, no security surface → Standard Workflow
Start at: Line 1713
Skip: Steps 2, 7, 8 (security reviews)
```

### Example 3: Fix Error Message
```
Decision: <10 lines, no API change → Fast-Path
Start at: Line 1850
Do: Step 4 (regression test), Step 6 (fix), Step 9 (PR)
```

## 🔧 Integration with Development

The workflow references:
- `testbed/` - Example app patterns
- `core/` - Implementation patterns
- `CLAUDE.md` - Outside-in TDD methodology
- `AGENTS.md` - NeoHaskell conventions

## 📊 Workflow Structure

```
Secure Feature Workflow (2,748 lines)
├── Context & Objectives (lines 1-780)
├── WORKFLOW DEFINITION (lines 782-2748)
│   ├── Decision Tree (line 786)
│   ├── Entry Criteria (lines 838-929)
│   ├── Full Workflow (lines 1073-1355)
│   ├── Security Checklist (lines 1357-1547)
│   ├── Performance Checklist (lines 1549-1711)
│   ├── Standard Workflow (lines 1713-1848)
│   ├── Fast-Path (lines 1850-2045)
│   ├── Recovery Procedures (lines 2047-2367)
│   └── Examples (lines 2369-2748)
```

## ⚡ Pro Tips

1. **Start with Decision Tree** - Always use it, never guess
2. **Copy Checklists** - Reset [ ] boxes for your feature
3. **Read Examples** - Cart domain shows real usage
4. **Use Recovery** - When stuck, see lines 2047-2367
5. **Adapt, Don't Skip** - Right-size workflow, don't bypass quality

## 📖 Additional Resources

- `learnings.md` - Key patterns discovered
- `decisions.md` - Design rationale
- `completion-summary.md` - Full implementation history
