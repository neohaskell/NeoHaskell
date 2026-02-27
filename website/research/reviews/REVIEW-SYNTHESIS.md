# Expert Review Synthesis: NeoHaskell Documentation Plan

> Synthesized from 7 expert reviews (Steve Klabnik, Martin Fowler, Julia Evans, Dan Abramov, Sarah Drasner, Tania Rascia, Kent C. Dodds) totaling ~340K of analysis across 4,221 lines.

---

## Consensus Classification

Changes are classified by agreement level:
- **UNANIMOUS** (6-7 reviewers) — Must change
- **STRONG** (4-5 reviewers) — Should change
- **NOTABLE** (2-3 reviewers) — Consider strongly
- **INDIVIDUAL** (1 reviewer, but critical expertise) — Evaluate carefully

---

## UNANIMOUS CONSENSUS — Must Change

### 1. Drop Layer 2 (Syntax Sidebar)

**Reviewers**: Steve, Julia, Dan, Tania, Kent (5/7 explicitly; Martin disagrees)

The collapsible "New NeoHaskell on this page" sidebar is overengineered and fights the implicit-first philosophy. Steve: "authoring and maintenance burden." Dan: "Layer 2 IS explicit teaching... shifts reader attention from 'I'm building a bank' to 'I'm learning a language.'" Julia agrees but adds: "replace with annotated code blocks, not nothing."

**Resolution**: Drop Layer 2. Replace with:
- Bold on first use + one-clause inline explanation (Steve)
- Annotated code blocks for first major code example in Parts 1-3 (Julia)
- Standalone syntax quick reference page (Steve, Julia)

**Martin's dissent**: "Keep it — compound unfamiliarity (syntax + concepts simultaneously) is real." Addressed by Julia's annotated code blocks which solve the same problem with a better mechanism.

### 2. Demote/Remove Bloom's Taxonomy Table

**Reviewers**: Steve, Julia, Dan, Tania, Kent (5/7)

Steve: "Use as retrospective validation, not design constraint." Tania: "Delete the table entirely — it's academic scaffolding." Kent: "The issue isn't the taxonomy — it's that it's applied without a testing dimension."

**Action**: Remove the Bloom's Taxonomy Progression table (lines 510-523). Keep the exercise progression concept but describe it in plain language, not academic framework.

### 3. Add Visual Diagrams (Non-Negotiable)

**Reviewers**: Steve, Julia, Dan, Sarah (4/7 explicitly)

Steve: "significant omission." Julia: "design visuals FIRST, then write prose to support them." Dan: "most valuable diagram shows how a single event flows through the entire system." Sarah: "event flow diagram design should be deliberate — clean SVG, not hand-drawn."

**Action**: Add to Phase 0:
- Event flow diagram (command → event → store → projection → read model)
- Event timeline visualization (static SVG, grows through tutorial)
- CRUD vs. Events comparison visual
Add "Visual Design" section to concept page template.

### 4. Exercise Solutions Must Be Visible

**Reviewers**: Julia, Tania, Sarah (3/7, but all newcomer-focused experts)

Julia: "Exercises without solutions are frustrating." Tania: "every tutorial I've ever written that didn't have 'stuck? here's the full code' sections had dramatically lower completion rates." Sarah: "graduated hints system."

**Action**: Every exercise must have:
- 2-3 progressive hints in `<details>` components
- Full solution behind "Show solution" toggle
- Link to tutorial companion repo

### 5. Elevate "Coming from CRUD" to First-Class Page

**Reviewers**: Dan, Tania, Sarah (3/7, all strongly)

Dan: "not a migration page — it's the core teaching document for the primary audience." Tania: "the MOST important page on the entire site." Sarah: "needs full expansion, not one table row."

**Action**: Move "Coming from CRUD" from subpage in Coming From section to standalone page in Core Concepts. Rename to "From CRUD to Events" or "Why Not Just Update the Record?" Structure with Dan's revised template (What You Might Expect → Why That Breaks → The Mental Model Shift → Side-by-Side Code → When CRUD Is Fine).

### 6. Add "Stuck Reader" Strategy

**Reviewers**: Julia, Tania, Sarah, Kent (4/7)

Julia: "What does a confused reader DO?" Tania: "the #1 reason people don't finish tutorials isn't that content is bad — they got stuck." Sarah: "emotional safety net." Kent: "design for failure, not just success."

**Action**: Add to every tutorial page:
- "If You're Stuck" section with escape hatches
- Complete code for each section (collapsible)
- Tutorial companion repo with tagged branches per part
- 3-5 "If you see X, check Y" troubleshooting items per checkpoint

### 7. Fade Alex Character to Second Person

**Reviewers**: Steve, Julia, Dan, Tania (4/7)

Steve: "let Alex fade by Part 2." Julia: "Alex should never be introduced as a named character." Tania: "Just use 'you' from the beginning." Dan: "second person is more immersive."

**Action**: Use Alex only on the tutorial index page as brief framing. Tutorial Part 1 onward uses "you." Product requirements framed as "The requirement:" not "Alex wants to..."

---

## STRONG CONSENSUS — Should Change

### 8. Integrate Testing into Tutorial (Not Phase 3)

**Reviewers**: Kent (primary), Dan (support)

Kent: "The plan treats testing the way most CRUD codebases treat testing — as a separate concern bolted on after the real work." Event sourcing's Given-When-Then pattern IS the testing grammar. Tutorial Part 2 should introduce the first test. By Part 6, reader should have 10-15 tests.

**Action**: 
- Add "Verify" as 7th exercise type
- Tutorial Part 2: first Given-When-Then test
- Tutorial Part 3: projection test
- Tutorial Part 5: cross-aggregate test
- Tutorial Part 6: property-based test
- Add `testing-event-sourced-systems.mdx` concept page (JIT with Part 2)
- Keep `guides/testing.mdx` for advanced patterns reference

### 9. Revise Concept Page Template

**Reviewers**: Dan, Julia, Tania, Sarah (4/7, each with different additions)

Dan: Add "What You Might Expect" and "Why That Breaks" at top — transforms information page into understanding page. Julia: Add "What Happens When..." section — makes invisible runtime behavior visible. Tania: Make template flexible, not rigid — some concepts need 2 sections, some need 5. Sarah: Use concept-name headings for searchability.

**Action**: Revised template:
```
## What You Might Expect (Dan)
## Why That Breaks (Dan)
## [Concept Name]: The Mental Model Shift (Sarah's searchable heading)
## What Happens When... (Julia — trace through concrete scenario)
## In NeoBank Terms
## The Full Picture (with code)
## How NeoHaskell Enforces This
## Testing This Concept (Kent)
## Going Deeper (optional)
```
Required sections: Mental Model Shift, In NeoBank Terms, Full Picture. All others optional based on concept needs (Tania's flexibility).

### 10. Add Delight/Emotional Design Principle

**Reviewers**: Julia, Sarah (2/7, but core expertise area)

Julia: "Add an 8th guiding principle about delight." Sarah: "emotional journey of the developer is designed for tutorial but not for anything else."

**Action**: Add 8th principle: "Delight is a teaching strategy. Surprise and 'wait, really?!' moments aren't decoration — they're how insights stick."

### 11. Streamline Phase 0

**Reviewers**: Tania, Steve (2/7)

Tania: "Gut Phase 0 to three tasks: CI, installation page, Tutorial Part 1." Steve: "Ship Tutorial Part 1. Watch someone use it. Fix everything."

**Action**: Reduce Phase 0 tasks. Move "Study reference docs" (already done), "Validate misconceptions" (can happen during writing), and "Create ARCHITECTURE.md" to Phase 1 or remove.

### 12. Add Starlight Component Strategy

**Reviewer**: Sarah (1/7, but deep platform expertise)

Sarah catalogs 6 underused Starlight components: Tabs, Asides, CardGrid, code block labels, Steps, Badges. "These aren't decorations — they're the difference between documentation that works and documentation that merely exists."

**Action**: Add "Starlight Component Strategy" section mapping each component to its purpose and usage location. Adopt from Phase 0, not as future enhancement.

### 13. Address Installation Friction

**Reviewers**: Sarah, Tania (2/7)

Sarah: "If Nix is the only path, this is the single highest-friction point." Tania: "most developers have never used Nix."

**Action**: 
- Offer zero-install path if possible (playground, Codespaces)
- If Nix required: design the Nix experience as if it's the product
- Add installation time estimates
- Use Starlight Tabs for platform-specific instructions

### 14. Add "Why Not CRUD?" Moment Before Teaching Events

**Reviewer**: Dan (1/7, but core expertise in mental model shifts)

Dan: "Show CRUD failure first. The transition happens because you made them feel the pain of mutation, not because you told them about events." Cites react.dev's "State as a Snapshot" pattern.

**Action**: Tutorial Part 1 should open with a brief CRUD example that fails (can't answer "what was the balance before the dispute?"), THEN introduce events as the solution. The "Why Not CRUD?" moment drives the Stage 0→1 transition.

### 15. Add "Thinking in Event Sourcing" Page

**Reviewer**: Dan (1/7, but cites react.dev's most important page)

Dan: "Thinking in React" is the most important page on react.dev. NeoHaskell needs an equivalent 5-step process: Identify events → Define commands → Design aggregates → Build projections → Connect bounded contexts.

**Action**: Add `concepts/thinking-in-events.mdx` to concept page schedule, linked from Tutorial Part 6 conclusion. Use non-banking domain for the worked example.

### 16. Move Disclaimer to Tutorial Conclusion

**Reviewer**: Dan (1/7)

Dan: "Leading with 'this isn't real' undermines the emotional strategy." The disclaimer at the top of Part 1 tells the reader to discount the experience.

**Action**: Move disclaimer from Part 1 intro to Part 6 conclusion, reframed as "what you'd add for production." If scope-setting needed early, do it with confidence, not defensiveness.

### 17. Add Progress Indicators and Time Estimates

**Reviewers**: Sarah, Tania, Julia (3/7)

Sarah: "You cannot improve what you don't measure, and you cannot encourage completion without making progress visible." Tania: "Knowing 'you're on Part 3 of 6' is not decoration — it's motivation."

**Action**: Implement tutorial progress tracker (Astro has this component). Add estimated reading time to each page. Add "What you'll learn" header and "Recap" section to each tutorial page.

---

## MARTIN FOWLER'S CRITICAL CORRECTIONS (Domain-Specific)

These are not consensus items — they're corrections from the only reviewer with deep event sourcing expertise. All should be applied.

### 18. WithdrawalDeclined Should NOT Be an Event

Martin: "Declined commands don't produce events, they return Result.err." This is a fundamental modeling error.

**Action**: Remove `WithdrawalDeclined` from Part 2's event list. The decline is a `Result.err` return from the command handler.

### 19. Bounded Contexts Introduced at Wrong Time/Reason

Martin: "Checking vs. savings is NOT a bounded context split — it's different aggregates in the same context." Delay bounded contexts until a genuine cross-context example (notifications, interest calculation).

**Action**: Part 4 teaches "Multiple Aggregates," not "Bounded Contexts." Move bounded context concept page to Part 5 (where cross-aggregate transfers genuinely need it) or to an advanced guide.

### 20. Rosetta Stone Conflates Entities with Patterns

Martin: "Aggregate ≠ Account, it's a consistency boundary."

**Action**: Fix Rosetta Stone table row: "My account" → "Account" → "Aggregate" should clarify that the aggregate is the consistency boundary, not the entity itself.

### 21. Projections Must Demonstrate Multiple Views

Martin: "Part 3 should build TWO different read models from same events (statement + monthly summary). This IS the CQRS insight."

**Action**: Revise Part 3 to build two projections from the same event stream, not just one.

### 22. Schema Evolution is Core, Not Advanced

Martin: "Relegating schema evolution to 'Advanced' is negligent."

**Action**: Move schema evolution from Misconception #9 ("Advanced guide") to a Core Concepts page, scheduled alongside Part 4 or Part 5.

### 23. Part 5 Saga Oversimplified

Martin: "Need full state machine: happy path → failure → compensation → process manager."

**Action**: Expand Part 5 scope to include failure handling and compensation, not just happy path.

### 24. Missing Critical Patterns

Martin flags: Idempotency, Snapshotting, GDPR/tombstones, Event ordering/causality.

**Action**: Add to concept page schedule or guides. At minimum, add to the plan as planned content.

### 25. Add Trade-offs / "When NOT to Use Event Sourcing"

Martin: "Plan oversells benefits without honest trade-offs."

**Action**: Add `concepts/trade-offs.mdx` — honest about when event sourcing adds unnecessary complexity.

### 26. Fix TransferInitiated/Completed

Martin: "Under-specified. AccountOpened savings variant should be same event with accountType field."

**Action**: Revise event list for Part 5. Use consistent event design.

---

## KENT C. DODDS' UNIQUE CONTRIBUTIONS

### 27. Add "Verify" Exercise Type

Write a test that proves the feature works. This creates a natural build→verify feedback loop.

### 28. Checkpoint Progression

Manual verification (Part 1) → semi-automated with test runner (Part 2) → automated tests as primary verification (Part 4) → reader writes their own verification (Part 6).

### 29. Litmus Tests for Doc Type Boundaries

| Section | Litmus Test |
|---------|-------------|
| Tutorial | "Does this page require previous pages?" |
| Concept | "Could the reader understand without a keyboard?" |
| Guide | "Does this solve a problem the reader already knows they have?" |
| Reference | "Would a reader come here to look up a forgotten detail?" |

### 30. Tutorial Companion Repo with Tagged Branches

Reader can `git checkout part-2-start` to get a clean starting point for any section.

### 31. Testing Trophy for Event Sourcing

Static Analysis (compiler) → Integration Tests (Given-When-Then) → E2E (Event Replay). Event sourcing makes integration tests trivially cheap.

---

## DISAGREEMENTS

| Topic | Pro | Con | Resolution |
|-------|-----|-----|-----------|
| Layer 2 | Martin: keep (compound unfamiliarity) | Everyone else: drop | Drop + annotated code blocks (Julia's mitigation) |
| Lesson granularity | Julia: 3-4 lessons per part (Svelte model, 48 lessons) | Others: 6 parts is fine | Adopt chapter/lesson structure (6 chapters, 3-4 lessons each) but don't mandate |
| JIT vs. foundational concepts | Steve: allow foundational pages before tutorial | Dan: explain in context, not standalone | Allow 2-3 foundational concept pages, but prioritize JIT |
| Alex character | Julia/Tania: never use, "you" from start | Steve: fade by Part 2 | Use Alex only in tutorial index, "you" from Part 1 onward |
| Bloom's | Tania: delete entirely | Steve/Kent: keep as retrospective | Remove table, keep difficulty progression concept in plain language |

---

## CHANGES TO APPLY TO DOCS-PLAN.md

### Priority 1 (Must — consensus items)
1. Add 8th guiding principle (delight)
2. Remove Layer 2 specification, add annotated code block approach
3. Remove Bloom's Taxonomy table
4. Revise concept page template (Dan/Julia/Tania/Sarah)
5. Fix event list (remove WithdrawalDeclined as event)
6. Fix bounded context timing (Part 4 → not BC)
7. Add "stuck reader" strategy
8. Add visual design requirements
9. Elevate "Coming from CRUD"
10. Fade Alex to "you"
11. Add exercise solutions requirement
12. Move disclaimer to Part 6

### Priority 2 (Should — strong consensus)
13. Add testing integration to tutorial
14. Add "Verify" exercise type
15. Streamline Phase 0
16. Add Starlight Component Strategy
17. Address installation friction
18. Add progress indicators and time estimates
19. Add "What you'll learn" + "Recap" to tutorial template
20. Add "Why Not CRUD?" moment to Part 1
21. Add "Thinking in Event Sourcing" page
22. Fix Rosetta Stone (aggregate ≠ entity)
23. Add multiple projections to Part 3

### Priority 3 (Should — individual critical)
24. Schema evolution → Core, not Advanced
25. Add Trade-offs concept page
26. Add missing patterns (idempotency, snapshotting, etc.)
27. Expand Part 5 saga detail
28. Fix TransferInitiated/Completed events
29. Add litmus tests for doc boundaries
30. Add tutorial companion repo
31. Add checkpoint progression (manual → automated)
32. Update "What We're NOT Doing" section (contradictions noted by Steve)
33. Add cross-reference matrix (Sarah)
