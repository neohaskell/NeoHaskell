---

# Review of the NeoHaskell Documentation Plan

**Reviewer: Kent C. Dodds**
**Date: February 2026**

---

## Opening

I've read the full DOCS-PLAN.md, the synthesis of 12 documentation sites, the React and Tailwind research files, and Steve Klabnik's review. Steve's review is excellent — particularly his points about the tutorial layer system being overengineered, checkpoint sections needing formalization, and the Bloom's taxonomy critique. I won't re-litigate those. I agree with most of his structural recommendations.

What I want to talk about is something Steve doesn't cover because it's not his domain: **testing**. And I don't just mean the `testing.mdx` guide listed on line 727. I mean the philosophical question this plan doesn't ask and absolutely must:

**How does a reader of these docs know their code works?**

This question runs through everything — the exercises, the compiler-as-teacher pattern, the checkpoint sections, the tutorial progression, the concept pages. It touches boundary definitions, exercise design, and the very mental model journey the plan describes. And the plan, as it stands, answers this question almost exclusively with "the compiler tells you." That's necessary. It's radically insufficient.

I also want to address something closer to my core work: what makes a tutorial *actually teach*, as opposed to creating the illusion of teaching. I've spent years at epicweb.dev watching people follow tutorials that produce working code but zero transferable understanding. The difference between a tutorial that works and a tutorial that teaches is how it handles verification, variation, and reader agency. This plan has strong instincts on all three but doesn't follow through.

Let me be specific.

---

## I. Testing Is an Afterthought, and It Shouldn't Be

**Responding to: The entire plan, but specifically `guides/testing.mdx` (line 727), Misconception #19 (line 93), and the Exercise Strategy (lines 491–523)**

The plan lists `testing.mdx` as one of 3-5 standalone guides to be written in Phase 3 (Weeks 6-8). It's on the same level as `deployment.mdx` and `postgresql-event-store.mdx`. The misconceptions table (line 93) addresses Misconception #19:

> "Testing event-sourced systems is hard" → "It's actually easier — replay events, assert projections"

That's the entire treatment. Nine words for what should be one of NeoHaskell's most powerful selling points.

**My critique:** This plan treats testing the way most CRUD codebases treat testing — as a separate concern bolted on after the "real" work is done. The irony is painful: the plan is building documentation for a paradigm (event sourcing) where testing is *architecturally native*, and it's burying testing in a Phase 3 guide.

**Why I feel strongly about this, from experience:**

When I built Testing Library, the core insight was that testing should reflect how users actually use software. Don't test implementation details — test behavior. Don't ask "did the internal state change?" Ask "does the user see the right thing?" This philosophy — test the outcome the user cares about, not the mechanism that produces it — maps *perfectly* onto event-sourced systems.

Think about what event sourcing gives you for testing:

1. **Given-When-Then is the native testing grammar.** Given these events happened → When this command is issued → Then these new events should be produced. That's not a testing framework layered on top. That's literally how event-sourced systems work. The test and the system share the same vocabulary.

2. **Projection testing IS integration testing.** You replay a sequence of events, run them through a projection, and assert the result. This is the Testing Trophy in action — testing at the integration layer gives you more confidence than testing individual functions. And event sourcing makes this trivially easy because you can construct any scenario just by building an event list. No database fixtures. No mock services. No setup/teardown ceremony.

3. **Command validation testing is property testing.** "No withdrawal should succeed when the balance is insufficient" is a property. "Every transfer should produce exactly two events" is a property. NeoHaskell, being a Haskell dialect, presumably has property-based testing libraries. This is a massive competitive advantage that the docs don't exploit.

4. **Event replay IS time-travel testing.** The plan lists "Historical replay" as a feature (line 123) and "Event replay, time-travel queries, full audit trail" as Part 6 content (line 135). But it doesn't connect this to testing. Event replay means you can take a production event stream, replay it in a test environment, and verify your projections produce the correct state. That's end-to-end testing with production data, and it's something CRUD systems can't do without enormous infrastructure. Event-sourced systems get it for free.

5. **The compiler is necessary but not sufficient for confidence.** The plan's Compiler-as-Teacher pattern (lines 527-559) is beautifully designed. But type checking is not the same as testing. The compiler can verify that your `MoneyDeposited` event has the right shape. It can't verify that your deposit handler produces the right events for a given command. It can't verify that your balance projection calculates correctly across edge cases. It can't verify that your transfer saga handles partial failures. Types reduce the space of possible bugs. Tests verify behavior in that remaining space.

**What's missing concretely:**

The tutorial teaches the reader to write event types, command handlers, projections, and sagas. At no point does the tutorial teach the reader to write a *test* for any of these things. The reader finishes the tutorial with a working NeoBank and zero tests. They have no idea how to verify that future changes don't break existing behavior. They have no way to express "this should always be true" in a way that runs automatically.

This is like teaching someone to cook without teaching them to taste the food.

**My specific recommendation:**

**Promote testing from a Phase 3 guide to a tutorial-integrated concern.** This doesn't mean adding a "Testing" tutorial part. It means introducing testing *inside* the existing tutorial parts, the way the plan already introduces language syntax implicitly.

Here's how:

1. **Tutorial Part 2 (Account Rules):** After the reader implements overdraft protection, show them how to write a Given-When-Then test for it:

```haskell
-- Given an account with $100
let events = [AccountOpened "checking", MoneyDeposited 100]

-- When a withdrawal of $150 is attempted  
let result = handleCommand (Withdraw 150) events

-- Then the withdrawal is declined
assert (result == Err InsufficientFunds)
```

This is three lines. It uses concepts the reader already knows (events, commands, results). It demonstrates that testing event-sourced systems is genuinely easy — not as a claim in a misconceptions table, but as lived experience. And it gives the reader their first taste of "I can verify my own code works."

2. **Tutorial Part 3 (Transaction History):** After building the bank statement projection, write a projection test:

```haskell
-- Given these transactions
let events = 
  [ MoneyDeposited 100
  , MoneyWithdrawn 30
  , MoneyDeposited 50
  ]

-- The balance projection should show $120
assert (balanceProjection events == 120)

-- The statement should have 3 entries
assert (length (statementProjection events) == 3)
```

This is the "Testing Trophy" in action — testing at the integration level (event list → projection → result) rather than unit-testing individual functions.

3. **Tutorial Part 5 (Transfers):** Write a test for the transfer saga that verifies both accounts are updated:

```haskell
-- Given two accounts
let checkingEvents = [AccountOpened "checking", MoneyDeposited 200]
let savingsEvents = [AccountOpened "savings", MoneyDeposited 50]

-- When $100 is transferred from checking to savings
let result = handleTransfer (Transfer "checking" "savings" 100) 
               checkingEvents savingsEvents

-- Then both accounts reflect the transfer
assert (balanceAfter result "checking" == 100)
assert (balanceAfter result "savings" == 150)
```

4. **Tutorial Part 6 (Audit Everything):** Introduce property-based testing for event replay consistency:

```haskell
-- For any sequence of valid events, replaying from scratch 
-- should produce the same balance as incremental application
property "replay consistency" do
  events <- generateValidEvents
  let incrementalBalance = foldl applyEvent emptyAccount events
  let replayedBalance = replay events
  assert (incrementalBalance == replayedBalance)
```

This progression mirrors the tutorial's own Bloom's-style cognitive progression (which Steve correctly says shouldn't be a design constraint, but works as a retrospective observation): from "copy this test" in Part 2, to "understand what this test verifies" in Part 3, to "see how tests catch cross-cutting bugs" in Part 5, to "express invariants as properties" in Part 6.

5. **Create a dedicated concept page: `testing-event-sourced-systems.mdx`** — triggered by Tutorial Part 2 (JIT, per the plan's own rule). This page should cover:
   - Given-When-Then as the native testing pattern
   - The Testing Trophy applied to event sourcing (why integration-level tests give the most confidence)
   - Property-based testing for event stream invariants
   - Event replay as production testing
   - How the compiler + tests together give maximum confidence

6. **The `guides/testing.mdx` page should still exist**, but as a comprehensive reference for testing patterns — not as the reader's first introduction to testing.

**The goal:** A reader who finishes the NeoBank tutorial should have 10-15 tests alongside their application code. They should have the muscle memory that "write the feature, write the test" is one motion, not two. And they should have experienced firsthand that testing event-sourced systems really IS easier than testing CRUD systems — because they've done both (the "Coming from CRUD" page can make this comparison explicit).

---

## II. The Exercises Don't Teach Readers to Verify Their Own Work

**Responding to: "Exercise Strategy" (lines 491–523) and "Exercise Placement" (lines 505–508)**

The plan defines six exercise types: Modify, Predict, Extend, Break, Compare, and Audit. These are good categories. The placement rules — 1 Modify + 1 Extend per tutorial page, 1 Predict or Break per concept page, 0 for guides — are reasonable.

**My critique:** None of these exercise types teach the reader *how to know they got the right answer.*

Look at the examples:

> **Modify**: "Change the deposit amount to $200. Run it. What's the new balance?"

Run it. See the output. Did it match? Okay, but how does the reader know the output is correct? They're comparing what they see on screen to what they expected. That's manual verification. It works for exercise 1. By exercise 12, the reader is skimming output and assuming it's right.

> **Extend**: "Add a `DailyWithdrawalLimitSet` event. Wire it through the handler so withdrawals are declined above the limit."

This is a great exercise. But how does the reader verify they wired it correctly? They can try a withdrawal above the limit and see if it's declined. But what about edge cases? What about withdrawals exactly at the limit? What about the limit being zero? The reader has no systematic way to verify correctness. They're relying on ad-hoc manual testing — precisely the approach event sourcing should liberate them from.

> **Audit**: "Replay all events from scratch. Does the derived balance match the current balance?"

This one is close! This is *almost* a test. But it's a manual test. The reader replays events in the REPL, eyeballs the balance, and compares. What if the exercise said: "Write a test that replays all events from scratch and asserts the derived balance matches the current balance. Run it. Does it pass?"

**Why this matters, from my teaching experience:**

At epicweb.dev, I've watched hundreds of developers do exercises. The pattern is always the same: the reader follows the instructions, gets the code to compile and produce some output, and moves on. They've demonstrated the ability to follow instructions. They haven't demonstrated understanding.

The difference shows up two weeks later when they try to build their own feature and can't. They knew how to follow the recipe but never learned how to check whether the dish tastes right.

The fix is simple and it's the same fix in every context: **make verification part of the exercise.** Don't just say "add this feature." Say "add this feature and write a test that proves it works." Now the reader has to understand what "works" means for this feature. That's the understanding you're actually trying to build.

**My specific recommendation:**

Add a seventh exercise type: **Verify**.

> **Verify**: "Write a test that proves [feature] works correctly. Run the tests. Do they pass?"

Then modify the exercise placement rules:

- **Tutorial pages**: 1 Modify + 1 Extend + 1 Verify per page
- **Concept pages**: 1 Predict or 1 Break per page

The Verify exercise appears after the Extend exercise. The reader extends the system, then proves their extension works. This creates a natural feedback loop: write code → write test → run test → see result.

Example progression:

| Part | Extend Exercise | Verify Exercise |
|------|----------------|-----------------|
| 1 | Add a `MoneyWithdrawn` event | Write a test: deposit $100, withdraw $30, assert balance is $70 |
| 2 | Add a `MinimumBalance` rule | Write a test: try to withdraw below minimum, assert it's declined |
| 3 | Create a `MonthlyStatement` projection | Write a test: given events across 3 months, assert the projection groups correctly |
| 4 | Add a `JointAccount` type | Write a test: both owners can deposit, assert shared balance |
| 5 | Handle partial transfer failure | Write a test: transfer with insufficient source funds, assert no events are produced |
| 6 | Build a `FraudAlert` projection | Write a property test: any account with 3+ declined withdrawals in 24 hours triggers an alert |

By Part 6, the reader has written 6 tests of increasing sophistication. They've experienced Given-When-Then testing, assertion testing, negative testing, property testing. They've done this naturally, in the context of features they built. And they have a test suite they can run with `neo test` to verify everything still works.

---

## III. The Compiler-as-Teacher Is a Form of Testing — the Plan Should Say So Explicitly

**Responding to: "Compiler-as-Teacher Pattern" (lines 527–559)**

Steve's review correctly notes that the plan should distinguish between compiler errors, linter warnings, and conventions. I agree. But I want to make a different, complementary point: **the Compiler-as-Teacher pattern is the plan's implicit acknowledgment that verification matters for learning.** The plan should make this explicit and then build on it.

**My analysis:**

The five-step Compiler-as-Teacher pattern is:

1. Show correct code
2. Instruct the reader to break it
3. Show the compiler error
4. Explain what the error means
5. Reader fixes it

This is, structurally, a test cycle:

1. Establish the expected behavior (working code)
2. Introduce a regression (break it)
3. Run the test (compile)
4. Read the test failure (error message)
5. Fix the regression (restore correct code)

The plan is teaching the reader the mechanics of a test-fix cycle without calling it that. This is a missed opportunity.

**Why this connection matters:**

If the plan explicitly names the Compiler-as-Teacher as "a form of testing" — specifically, static analysis testing — it creates a natural bridge to dynamic testing. The progression becomes:

1. **Static testing**: The compiler catches type errors, syntax violations, and linter rule violations. You've already experienced this in the Break-It exercises.
2. **Dynamic testing**: Tests catch behavioral errors that the compiler can't see. You'll learn this by writing tests for your NeoBank features.
3. **The confidence spectrum**: Compiler errors give you high confidence that your code is structurally correct. Tests give you high confidence that your code is behaviorally correct. Together, they give you maximum confidence.

This is the Testing Trophy applied to a type-safe, event-sourced system:

```
          ╱╲
         ╱  ╲          Static Analysis (Compiler)
        ╱────╲         — catches type errors, syntax violations
       ╱      ╲        — enforced automatically, zero effort
      ╱────────╲       
     ╱          ╲      Integration Tests (Given-When-Then)
    ╱────────────╲     — catches behavioral errors
   ╱              ╲    — event-sourcing makes these trivial
  ╱────────────────╲   
 ╱                  ╲  End-to-End Tests (Event Replay)
╱────────────────────╲ — catches system-level errors
                       — event store makes this free
```

In a traditional CRUD system, integration tests are expensive to write (mock the database, set up fixtures, handle state). In an event-sourced system, integration tests are cheap (construct an event list, run a function, assert the result). **This is one of event sourcing's biggest practical advantages**, and the plan barely mentions it.

**My specific recommendation:**

1. Add a paragraph to the Compiler-as-Teacher section explicitly naming it as "static testing" and positioning it as the first layer of a confidence strategy.

2. Add a subsection to the concept page template called "## Testing This Concept" — after "How NeoHaskell Enforces This." This section shows how to write a test for the concept the page just explained. This bridges the gap between "the compiler enforces structure" and "tests verify behavior."

3. In the tutorial, explicitly name the testing progression:

| Part | Compiler Teaching | Testing Teaching |
|------|-------------------|------------------|
| 1 | "The compiler catches type errors" | (implicit — verify output manually) |
| 2 | Break-It: use `let..in` instead of `do` | First Given-When-Then test |
| 3 | Break-It: unqualified import | Projection test |
| 4 | Break-It: `Either` instead of `Result` | (reinforcement — tests for new account type) |
| 5 | Break-It: `$` instead of `\|>` | Saga test (cross-aggregate) |
| 6 | Break-It: point-free style | Property-based test (replay consistency) |

This shows the reader two parallel confidence-building tools: the compiler (which catches structural errors for free) and tests (which catch behavioral errors with minimal effort in an event-sourced system).

---

## IV. The Boundary Between Tutorial, Guide, and Concept Is Clear in Theory but Blurred in Practice

**Responding to: "Site Structure (Diataxis-Informed)" (lines 271–339) and "Boundary Definitions (Kent's Rule)" (lines 330–339)**

I appreciate the plan naming the boundary table "Kent's Rule" — but the boundaries need more teeth.

The plan's boundary table says:

| Section | Linear? | Builds one project? | Standalone pages? | Reader goal |
|---------|---------|--------------------|--------------------|-------------|
| Tutorial | Yes | NeoBank | No | "Teach me everything in order" |
| Core Concepts | No | No | Yes | "Help me understand why" |
| Guides | No | No | Yes | "Help me do this specific thing" |

These are correct *definitions*. But the plan violates them in at least three places:

**Violation 1: The Testing Guide Is Actually a Tutorial**

`guides/testing.mdx` (line 727) is listed as a guide — a standalone, task-oriented, non-linear page. But if it's teaching someone how to test an event-sourced system for the first time, it's not a guide. It's a tutorial. A guide says "here's how to do this specific thing you already know you want to do." A tutorial says "let me teach you a new skill." If the reader has never written a test for event-sourced code, they need a tutorial — a progressive, step-by-step introduction. If they've written tests before and just need NeoHaskell-specific patterns, they need a guide.

The plan should acknowledge this and either:
- Make testing a tutorial part (my strong preference, as argued in Section I), or
- Split testing into a concept page ("Why Testing Event-Sourced Systems Is Different") and a guide ("How to Test in NeoHaskell"), or
- Accept that `guides/testing.mdx` is actually a teaching page and structure it accordingly (with progressive disclosure, exercises, and verification steps — things the plan explicitly says guides shouldn't have).

**Violation 2: The Using AI Guide Is Actually a Concept Page + a Guide**

`guides/using-ai.mdx` (line 612) has two purposes according to the plan:

> Copy-paste NeoHaskell prompt for AI tools, the 5 most common AI mistakes... how to spot wrong patterns, when to trust vs. override AI suggestions

The first half (copy-paste prompt, common mistakes) is a guide: task-oriented, standalone, "help me do this specific thing." The second half (when to trust vs. override AI) is a concept page: "help me understand why." These should be two pages or the plan should acknowledge the boundary blur.

**Violation 3: The Error Handling Guide Could Be Any of the Three Types**

`guides/error-handling.mdx` (line 731) is listed as a guide. But error handling in NeoHaskell — `Result Ok/Err`, `Task err val`, typed errors — is also a core concept that the tutorial introduces in Part 2. Is the guide a standalone "how to handle errors in NeoHaskell" for someone coming in without tutorial context? Or is it a deeper treatment of error handling patterns for someone who's done the tutorial? The answer determines whether it's a guide (task-oriented, standalone) or a concept extension (understanding-oriented, builds on tutorial knowledge).

**Why boundary clarity matters, from my experience:**

At epicweb.dev, I learned the hard way that boundary violations are contagious. Once one guide contains teaching content, every subsequent guide author feels permission to include teaching content. Once one concept page includes step-by-step instructions, concept pages start becoming tutorials. The result is a docs site where readers can't predict what they'll get when they click a link. That's a trust problem.

React.dev handles this well. The Learn section is strictly "why and when." The Reference section is strictly "what and how." You never find a `useState` tutorial in the Reference section, and you never find exact parameter types in the Learn section. The bidirectional links between them are explicit: "See the useState API reference" and "See the useState concept page." This separation survived thousands of pages because it was enforced at the structural level.

**My specific recommendation:**

1. Add a **litmus test** to each boundary definition that writers can use to self-check:

| Section | Litmus Test |
|---------|-------------|
| Tutorial | "Does this page require the reader to have completed previous pages?" If yes → tutorial. |
| Concept | "Could the reader understand this page without touching a keyboard?" If yes → concept. |
| Guide | "Does this page solve a problem the reader already knows they have?" If yes → guide. |
| Reference | "Would a reader come here to look up a specific detail they've forgotten?" If yes → reference. |

2. Apply the litmus test to every planned page and reclassify where needed:

- `guides/testing.mdx` → promote to tutorial integration (see Section I) + keep a `guides/testing-patterns.mdx` for advanced patterns
- `guides/using-ai.mdx` → split into `concepts/using-ai.mdx` (when to trust AI) and `guides/ai-prompt.mdx` (the actual prompt and patterns)
- `guides/error-handling.mdx` → clarify: is this for readers who've done the tutorial (concept extension) or readers who haven't (standalone guide)?

3. Add the litmus test to the plan's "Iteration Protocol — After Every Page" (lines 671-674), so every new page gets boundary-checked before publication.

---

## V. The Success Metrics Don't Measure the Thing That Matters Most: Reader Confidence

**Responding to: "Success Metrics" (lines 768-780)**

The plan lists six success metrics:

1. Time to first deposit (<10 minutes)
2. Tutorial completion rate (>50% through Part 4)
3. Discord question reduction
4. Contributor PRs
5. Search satisfaction
6. "I built a bank" effect

**My critique:** None of these metrics measure whether the reader can build something *on their own* after finishing the tutorial.

Time to first deposit measures the *tutorial* experience, not the reader's *capability*. Tutorial completion rate measures persistence, not understanding. "I built a bank" measures emotional response, not skill transfer.

The metric that actually matters is: **can the reader apply what they learned to a new domain?**

**Why this matters:**

This is the fundamental problem with "follow along" tutorials. The reader follows instructions, produces a working NeoBank, and feels accomplished. They try to build a todo app with event sourcing. They're stuck. The tutorial taught them how to build NeoBank, not how to build event-sourced systems.

The research synthesis identifies this implicitly. Pattern 5 (Teaching Unfamiliar Concepts Through Familiar Tasks) says the concept should "emerge naturally through usage." But "emerging" is not the same as "being understood." A reader can follow a tutorial where event sourcing emerges naturally and still not be able to apply event sourcing to a new domain. The emergence needs to be *named*, *practiced*, and *verified*.

The plan's second example domain (logistics/shipment tracking, line 172) is actually the right answer to this problem. It's listed as a Phase 4 "advanced guide" item. It should be listed as a *success metric validation exercise*: "Can a reader who completed the NeoBank tutorial build a shipment tracking system with event sourcing?" If yes, the tutorial worked. If no, the tutorial produced NeoBank expertise, not event-sourcing expertise.

**My specific recommendation:**

Add two metrics:

7. **Transfer rate**: After the tutorial, give readers a challenge: "Model a simple library system (borrow book, return book, overdue notice) using event sourcing. No instructions — just the domain description." Measure: what percentage can produce a working first event type within 30 minutes?

8. **Test coverage**: Do readers who complete the tutorial write tests for their own projects? If the tutorial integrates testing (per my Section I recommendation), this becomes measurable through GitHub analysis of NeoHaskell projects.

Also add a post-tutorial assessment: a single challenge page at the end of Part 6 that says:

> **Your final challenge: Build something that isn't a bank.**
>
> Pick a domain: a library, a restaurant reservation system, a project tracker. Model one aggregate with three events. Write a projection. Write one test.
>
> No hints. No step-by-step. If you can do this, you understand event sourcing.

This page is the actual test of whether the tutorial worked. And it gives the reader the confidence moment they need: "I did this without instructions. I actually understand this."

---

## VI. The Exercise Strategy Creates "Follow-Along" Learners, Not Independent Thinkers

**Responding to: "Exercise Strategy" (lines 491–523), specifically the Bloom's Taxonomy Progression (lines 510-523)**

Steve correctly critiques the Bloom's mapping as the wrong abstraction. I want to make a different point: even with the progression correct, **the exercise format itself creates dependency on instructions.**

Look at the exercise progression:

| Part | Exercise Pattern |
|------|-----------------|
| 1 | Copy-paste, change one value |
| 2 | Change behavior within existing structure |
| 3 | Build a similar feature using the same pattern |
| 4 | Identify which pattern to apply |
| 5 | Judge a given solution and find the flaw |
| 6 | Build something new combining multiple concepts |

Parts 1-4 all give the reader a specific task with a known correct answer. The reader either does it right or wrong, and they know which because the instructions implicitly define "right." Part 5 introduces ambiguity (find the flaw). Part 6 introduces creative freedom (build something new).

**The problem: the jump from Part 4 to Part 5 is too large.** For four consecutive parts, the reader has been operating in "follow instructions" mode. Suddenly in Part 5, they're expected to critically evaluate code. In Part 6, they're expected to create something novel. This is like training someone to drive in a parking lot for four days and then putting them on the highway.

**How testing fixes this:**

If the reader is writing tests from Part 2 onward (as I recommended in Section I), they're already practicing critical evaluation from Part 2. Writing a test requires asking: "What should this code do? How would I know if it's wrong? What are the edge cases?" These are analytical questions — they force the reader out of "follow along" mode and into "think for yourself" mode.

By Part 5, a reader who's been writing tests for three parts has already practiced evaluating correctness. The jump to "find the flaw in this code" is smaller because they've been finding flaws in their own code through testing.

By Part 6, a reader who's been writing tests has the tools to verify their creative work. "Build a FraudAlert projection" is less intimidating when you can write a test first that defines what FraudAlert should detect, and then implement until the test passes. This is TDD, and the reader learned it implicitly through the exercise structure.

**My specific recommendation:**

Restructure the exercise progression to interleave verification throughout:

| Part | Build Exercise | Verify Exercise | Critical Thinking |
|------|---------------|-----------------|-------------------|
| 1 | Change the deposit amount | Check: does the balance match? | (none — first encounter) |
| 2 | Add a MinimumBalance rule | Write a test for the rule | "What happens if the minimum is $0?" |
| 3 | Create MonthlyStatement projection | Write a test for the projection | "What if there are no events in a month?" |
| 4 | Add JointAccount type | Write a test for shared access | "Should AccountType be an event or a field?" |
| 5 | Handle partial transfer failure | Write a test for failure handling | "Find the bug in this transfer handler" |
| 6 | Build FraudAlert projection | Write a property test for fraud detection | Write tests first, then implement |

The "Critical Thinking" column is what the current plan calls exercises. But they're now the *third* activity, not the primary one. The reader builds, verifies, then thinks critically. By Part 6, they can write tests before implementation — the highest-confidence development pattern.

---

## VII. Misconception #19 Deserves a Full Concept Page, Not a One-Liner

**Responding to: Misconception #19 (line 93) and the Misconceptions Table generally**

> | 19 | "Testing event-sourced systems is hard" | It's actually easier — replay events, assert projections | Testing guide |

Nine words. For a misconception that, if properly addressed, is one of NeoHaskell's most compelling arguments for adoption.

**My critique:** This misconception is different from the others in the table. Misconceptions 1-18 are about event sourcing or NeoHaskell's design. Misconception 19 is about the *practical development experience* of working with event-sourced systems. It speaks directly to the developer audience's daily reality: "Will this make my job harder or easier?"

The answer — "easier, and here's proof" — should be one of the docs' strongest moments. Instead, it's delegated to a Phase 3 guide that might not exist for months.

**Why this resonates with my work:**

When I first introduced Testing Library, the main objection was: "Testing React components is hard." My response wasn't a sentence — it was a complete reimagining of what testing React components looks like. I showed people that if you test the way users interact with your component (click buttons, fill forms, read text), instead of testing implementation details (internal state, lifecycle methods, component instances), testing becomes *easier*. Not just "not harder" — genuinely easier.

The same argument applies to event sourcing. If you test the way your system *works* (given events → command → expected events), instead of testing implementation details (database state, ORM queries, mock repositories), testing becomes genuinely easier. This argument needs space. It needs examples. It needs side-by-side comparisons.

**My specific recommendation:**

1. Elevate Misconception #19's "Where to address" from "Testing guide" to "Tutorial (integrated) + Concept page + Guide." This is the only misconception that warrants three touchpoints because it affects the reader's daily development experience.

2. Create a concept page: `concepts/testing-event-sourced-systems.mdx` — triggered just-in-time by Tutorial Part 2 (when the first test is introduced). This page should include:

   - **Side-by-side comparison**: Testing a "transfer" feature in CRUD vs. event sourcing. Show the CRUD version with database setup, mock repositories, transaction management, and teardown. Show the event sourcing version with a simple event list and assertion. Let the difference speak for itself.
   
   - **The Given-When-Then pattern**: Name it explicitly. Show how event sourcing's architecture naturally maps to this testing grammar.
   
   - **The Testing Trophy for event sourcing**: Explain why integration tests give the most confidence, and why event sourcing makes integration tests cheap.
   
   - **Property-based testing**: Show how event stream invariants can be expressed as properties. This is where NeoHaskell's Haskell heritage becomes a selling point — QuickCheck-style testing for event streams.

3. The `guides/testing.mdx` page becomes a patterns reference: snapshot testing, event stream assertions, projection verification, saga testing, error scenario testing. This is for readers who understand *why* to test and need *how* patterns.

---

## VIII. The Plan Doesn't Teach Readers How to Debug When Tests Fail

**Responding to: The plan as a whole — this topic is absent**

The plan teaches: writing code, understanding concepts, breaking code (Compiler-as-Teacher), and reading error messages. It does not teach: what to do when your code compiles, the error messages are clear, and *the behavior is still wrong.*

**My critique:** In my experience teaching developers, the most common stuck point isn't "I don't understand the error message." It's "there is no error message — the code runs, but it does the wrong thing." The plan's entire verification strategy is compiler-centric: if it compiles, the compiler approved it. But correctly-typed code that produces wrong behavior is the most frustrating class of bug, and event sourcing has its own flavor of this:

- Events are applied in the wrong order → balance is incorrect
- A projection misses an event type → data is incomplete
- A command handler produces the wrong events → downstream projections break
- A saga doesn't handle partial failure → system reaches inconsistent state

These bugs all compile cleanly. They all pass type checking. They only surface when you *run the code and check the results.* This is precisely where tests catch what the compiler can't.

**My specific recommendation:**

Add a "Debugging" section to the tutorial — not as a separate part, but as a moment within Part 3 or Part 4 where the reader encounters a behavioral bug that the compiler doesn't catch.

Example scenario for Part 3 (Transaction History):

> Your balance projection shows $120, but you expected $150. The compiler is happy. What went wrong?
>
> Let's debug with events. Run `listEvents "checking"`:
>
> ```
> [AccountOpened, MoneyDeposited 100, MoneyDeposited 50, MoneyWithdrawn 30]
> ```
>
> Now run the projection step by step:
> ```
> balance after AccountOpened: $0
> balance after MoneyDeposited 100: $100
> balance after MoneyDeposited 50: $150
> balance after MoneyWithdrawn 30: $120
> ```
>
> The events are correct. The projection is correct. The expected value was wrong — you forgot about the withdrawal.
>
> **This is why event sourcing makes debugging easy.** Every state change is an event. You can replay them one at a time and see exactly where the state diverged from your expectation.

This teaches a meta-skill: debugging by replaying events. It also reinforces the core event-sourcing mental model: state is derived from events, and you can always reconstruct how you got here. And it does so through a scenario that requires no special tooling — just listing events and replaying them.

---

## IX. Where I Agree and Disagree with Steve

Steve's review makes 14 recommendations. Let me address the ones where my perspective differs or adds something.

### Agreement: Layer 2 Should Go
Steve says drop the syntax sidebar. I agree completely. It's a grammar lesson inserted into a cooking class. Inline explanations in the narrative are sufficient, and a standalone syntax reference page handles the lookup use case.

### Agreement: Checkpoints Need Formalization
Steve's proposed checkpoint format (command, expected output, success confirmation, common errors) is exactly right. I'd add one thing: **include a test that verifies the checkpoint.** After the manual verification, show the reader: "You can also verify this by running `neo test`. Here's what the test looks like." This normalizes testing as a verification mechanism alongside manual inspection.

### Partial Disagreement: Bloom's Taxonomy
Steve says demote Bloom's from design constraint to retrospective validation. I agree it shouldn't be a design constraint. But I think the issue isn't the taxonomy itself — it's that the taxonomy is applied without a *testing* dimension. A Bloom's-inspired progression where "Analyze" means "find the bug in this code" and "Create" means "write tests first, then implement" would actually be pedagogically sound. The problem isn't the framework; it's that the framework is applied only to building code, not to verifying code.

### Disagreement: JIT Concept Pages
Steve recommends relaxing the JIT rule to allow foundational concept pages (immutability, type inference) before the tutorial needs them. I'd go further: **testing** is a foundational concept that should have a pre-existing concept page.

Not a comprehensive testing guide — a short "Why NeoHaskell Makes Testing Easy" page that the Getting Started section can link to. It sets the expectation from page one: "You'll be writing tests in this ecosystem, and it'll be easier than you expect." This primes the reader for the testing integration in the tutorial, and it addresses Misconception #19 before the reader even starts.

### Building on Steve's Point: The Pacing Gap (Part 4 → Part 5)
Steve identifies the cognitive jump from single-aggregate to cross-aggregate operations. He recommends splitting Part 5. I agree, but I want to add: **the pacing gap is also a testing gap.** Parts 1-4 can be tested with single-event-stream assertions. Part 5 requires testing across two event streams with coordination. If the reader hasn't been writing tests through Parts 1-4, the testing challenge of Part 5 compounds the conceptual challenge.

If testing is integrated from Part 2, the testing progression naturally scaffolds the cross-aggregate challenge: "You've tested individual accounts. Now write a test that verifies both accounts after a transfer." The testing dimension softens the conceptual jump because the reader has a familiar tool (tests) to apply to the unfamiliar problem (cross-aggregate coordination).

---

## X. The "Verify Your Progress" Sections and the Checkpoint Format Should Teach Test-Thinking

**Responding to: Steve's Section VI and the plan's general approach to verification**

Steve recommends a formal "Verify Your Progress" template (his Section VI). I want to extend his recommendation with a specific evolution: **the checkpoint format should gradually transition from manual verification to automated tests.**

Here's what I mean:

**Part 1 checkpoint** (manual — reader is new to everything):
```markdown
### Verify Your Progress

Run your project and try this in the REPL:

    > deposit "alex-checking" 100
    MoneyDeposited { accountId = "alex-checking", amount = 100 }

    > balance "alex-checking" 
    $100.00

If you see this, you're on track.
```

**Part 2 checkpoint** (semi-automated — introduce the test runner):
```markdown
### Verify Your Progress

First, check manually:

    > withdraw "alex-checking" 150
    Err InsufficientFunds

Now run the tests we wrote in the exercise:

    neo test

You should see:

    ✓ deposit increases balance
    ✓ withdrawal with insufficient funds is declined
    2 tests passed

If any tests fail, check your `handleCommand` function.
```

**Part 4 checkpoint** (automated — tests are the primary verification):
```markdown
### Verify Your Progress

Run the test suite:

    neo test

You should see:

    ✓ checking account accepts deposits
    ✓ savings account accepts deposits
    ✓ overdraft protection works on checking
    ✓ savings accounts have separate balances
    ✓ monthly statement groups by month
    8 tests passed

All tests should pass. If they don't, the failing test name tells 
you which feature to fix.
```

**Part 6 checkpoint** (test-first — the reader writes the checkpoint):
```markdown
### Verify Your Progress

Before checking your solution, write a test for the fraud alert 
projection. What should it detect? What should it ignore?

Write your test, run `neo test`, and see if it passes.

Then compare your approach to the reference solution below.
```

This progression — manual → semi-automated → automated → self-directed — mirrors how professional developers work. By Part 6, the reader isn't waiting for the tutorial to tell them "you're on track." They know they're on track because their tests pass. That's *real* confidence. That's the confidence that survives beyond the tutorial.

---

## XI. The Second Example Domain Should Validate Teaching, Not Just Prove Generalization

**Responding to: "Second Example Domain" (line 172)**

> **Logistics / shipment tracking.** A package moves through locations, status changes are events, delivery is a projection. Different enough from banking to prove the pattern generalizes.

**My critique:** The plan positions the logistics domain as proof that event sourcing generalizes beyond banking. That's one purpose. But there's a more valuable purpose the plan doesn't identify: **the logistics domain validates whether the tutorial actually taught event sourcing, or just taught NeoBank.**

**Why this distinction matters:**

If a reader can build NeoBank by following instructions but can't model a shipment tracking system without instructions, the tutorial failed. It taught a specific implementation, not a transferable skill. The logistics domain shouldn't be buried in "Phase 4 — Polish and Feedback" as an advanced guide. It should be positioned as a post-tutorial assessment:

> You've built NeoBank. Now build something different. Here's a logistics domain. Apply what you learned.

This is different from "here's another tutorial." There are no step-by-step instructions. There's a domain description, a set of requirements, and the reader's own understanding. The reader has to make decisions: What are the events? What are the commands? What are the projections? What tests should I write?

**My specific recommendation:**

1. Keep the logistics domain for Phase 4, but reframe it. Instead of "advanced guide," make it a "Transfer Challenge" — a lightly-scaffolded exercise that tests whether the reader can apply event sourcing independently.

2. Structure it as:
   - Domain description (2 paragraphs)
   - Requirements (5 bullet points: "track package location," "notify on delivery," etc.)
   - Starter code (project scaffolding, no event/command/projection implementations)
   - Test specifications (Given-When-Then tests the reader should make pass)
   - Reference solution (behind a spoiler/expandable section)

3. The key innovation: **provide the tests, not the implementation.** Give the reader a set of failing tests:

```haskell
-- These tests should pass when you're done:
test "package shipped creates ShipmentCreated event" do ...
test "location scan updates package position" do ...
test "delivery creates DeliveryCompleted event" do ...
test "tracking projection shows current location" do ...
```

The reader's job is to make the tests pass. This is TDD as a teaching mechanism: the tests define the expected behavior, and the reader implements until they pass. This verifies understanding without requiring step-by-step instructions.

---

## XII. The Code Verification Infrastructure Verifies Docs, Not Reader Code

**Responding to: "Code Verification Infrastructure" (lines 236–267)**

The plan describes CI that extracts and compiles code blocks from documentation files. This is excellent — it prevents code examples from rotting. Steve doesn't address this section, but I want to make an observation the plan misses.

**My critique:** The code verification infrastructure tests *the docs*. It doesn't help readers test *their code*. These are different problems:

1. **Doc testing** (what the CI does): "Do the code examples in the documentation compile and produce the expected output?"
2. **Reader testing** (what the plan doesn't address): "Does the code the reader wrote while following the tutorial work correctly?"

The plan assumes these are the same thing. They're not. A reader who follows the tutorial exactly will produce code that matches the doc examples. But the exercises ask readers to *deviate* from the doc examples (Modify, Extend, Break). The reader who adds a `DailyWithdrawalLimitSet` event isn't following a code block — they're implementing a feature. How do they know it works?

**My specific recommendation:**

Extend the code verification infrastructure to include a **test suite for each tutorial part.** Not tests for the docs — tests that the reader runs against their own code.

Structure:

```
tutorial-code/
  part-1/
    src/         # starter code
    tests/       # tests that should pass after completing part 1
  part-2/
    src/         # builds on part 1
    tests/       # tests that should pass after completing part 2
  ...
```

Each tutorial part includes: "Run `neo test` to verify your implementation. All tests should pass." The tests are part of the tutorial project, not the documentation infrastructure. They test the reader's code, not the doc's code.

This also enables the exercise strategy I described in Section II: Verify exercises become "make this test pass" rather than "check the output manually."

---

## XIII. Quick Thoughts on Smaller Issues

### A. The "Compare" Exercise Type Is Underutilized

The exercise type "Compare: Write this same feature in [language you know]. Which version is clearer?" (line 501) is listed but never appears in the Bloom's progression table (lines 510-523). This is a missed opportunity, especially for the testing story. A Compare exercise in Part 2 could be:

> Write a test for overdraft protection in your current language (JavaScript, Python, Go). Now look at the NeoHaskell test you just wrote. Which one required more setup? Which one is more readable?

This makes the "testing is easier with event sourcing" argument concrete and personal.

### B. The FAQ Should Address "How Do I Test This?"

The plan mentions a FAQ addressing remaining misconceptions (line 646). It should include testing questions:

- "How do I test event handlers?" → Given-When-Then pattern
- "How do I test projections?" → Event list → projection → assertion
- "Do I need a test database?" → No — event lists replace fixtures
- "What about mocking?" → Event sourcing reduces the need for mocks dramatically

### C. The "Common Errors" Page Should Include Test Failures

`guides/common-errors.mdx` (line 732) is described as "error message as heading, fix as content." This should include common *test* failures, not just compiler errors:

- "Test fails with 'expected 3 events, got 2'" → Your command handler isn't emitting all expected events
- "Property test fails with counterexample" → Your invariant doesn't hold for edge cases
- "Replay produces different balance than projection" → Event ordering issue

### D. The Interactive Elements Should Include a Test Runner

The plan's Phase 2 interactive elements (lines 571-578) list "Run this code" buttons but not "Run these tests" buttons. If the plan can provide runnable code, it should also provide runnable tests. The "Run tests" button is more confidence-building than the "Run code" button because it provides pass/fail feedback, not just output.

### E. Open Question #8 Needs a Testing Dimension

Open Question #8 (line 791) asks whether the "compliance engine" claim is defensible. There's a testing angle: "compliance" implies auditability, which implies *verifiable correctness*. If the event store is the source of truth, can you write a test that replays all events and verifies consistency? If yes, the compliance claim has teeth. If no, it's marketing. The test is the proof.

---

## Summary of Recommendations

Ranked by impact on reader confidence and skill transfer:

1. **Integrate testing into the tutorial** (Parts 2-6), not as a separate guide but as a natural part of building NeoBank. Given-When-Then tests from Part 2, property tests by Part 6.

2. **Add a "Verify" exercise type** alongside Modify, Extend, etc. — every tutorial part asks readers to write a test that proves their code works.

3. **Evolve checkpoints from manual to automated** — Part 1 is manual REPL verification; by Part 4, `neo test` is the primary verification mechanism.

4. **Create a `testing-event-sourced-systems.mdx` concept page** that makes the case (with side-by-side comparisons) that event sourcing makes testing genuinely easier.

5. **Reclassify `guides/testing.mdx`** — it's not a guide if the reader has never tested event-sourced code before. Split into tutorial integration + patterns guide.

6. **Add boundary litmus tests** to prevent tutorial/guide/concept blur. Apply to every planned page.

7. **Name the Compiler-as-Teacher as "static testing"** and position it as the first layer of a testing confidence strategy.

8. **Elevate Misconception #19** from a one-liner to a first-class concept page with side-by-side CRUD-vs-event-sourcing testing comparisons.

9. **Add test-driven success metrics** — can readers apply event sourcing to a new domain? Do they write tests for their own projects?

10. **Reposition the second example domain** as a "Transfer Challenge" with provided tests and no step-by-step instructions, validating that the tutorial taught transferable skills.

11. **Extend code verification infrastructure** to include reader-facing test suites for each tutorial part.

12. **Add a post-tutorial challenge page** that asks readers to build something without instructions — the real test of whether the tutorial worked.

13. **Teach debugging through event replay** — add a scenario where the reader encounters a behavioral bug that compiles cleanly, and resolves it by replaying events.

14. **Include test failures in the Common Errors page** — not just compiler errors, but test failures and their meanings.

---

## Closing Thought

The fundamental question of documentation is the same as the fundamental question of testing: **how do you know it works?**

For testing, the answer is: write assertions about expected behavior, run them, and see if they pass. For documentation, the answer is: write content that claims to teach something, test it with real readers, and see if they learned it. The plan understands the second answer (its iteration protocol, user testing, and feedback mechanisms are strong). It doesn't apply the first answer to what it teaches.

Event sourcing is a paradigm that makes testing *architecturally natural*. Given-When-Then isn't a testing framework — it's how event-sourced systems work. Property-based testing isn't an advanced technique — it's a natural fit for event stream invariants. Event replay isn't a debugging tool — it's a production testing mechanism. These aren't features to document in a Phase 3 guide. They're selling points to demonstrate from Tutorial Part 2.

The plan says NeoHaskell's compiler is a teacher. That's true. But teachers don't just catch mistakes — they build confidence. The compiler catches structural mistakes. Tests build behavioral confidence. Together, they give the reader something no CRUD tutorial ever can: the certainty that their code is correct, not because they followed instructions, but because they proved it.

Ship Tutorial Part 1. Watch someone use it. Then — in Part 2 — ship the first test. Watch them write it. Watch the moment they realize: "Wait, testing this is *easy*?"

That's the moment that converts a CRUD-thinker into an event-sourcing advocate. Don't make them wait until Phase 3 for it.

— Kent C. Dodds