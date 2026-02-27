# Review of the NeoHaskell Documentation Plan

**Reviewer: Steve Klabnik**
**Date: February 2026**

---

## Preamble

I've read the full DOCS-PLAN.md, the research synthesis across 12 documentation sites, and the analysis of my own book. I want to be upfront: this is one of the most thoughtful documentation plans I've ever seen for a language that doesn't have a large user base yet. The ambition is right-sized, the audience model is concrete, and the banking domain is a genuinely inspired choice. What follows is not a demolition — it's the kind of review I wish someone had given me before I started the second edition of the Rust Book, when I'd already made commitments that were expensive to undo.

I'll be direct about what I think is wrong, what's missing, and what's overbuilt. I'll also correct a few things in the analysis of my work.

---

## I. The Tutorial Layer System Is Overengineered

**Responding to: "Tutorial Layer System" (lines 179–232)**

The plan proposes three simultaneous layers on every tutorial page: the narrative (Layer 1), a collapsible "New Syntax on This Page" aside (Layer 2), and inline deep-dive links to concept pages (Layer 3). The principle stated is: "If a reader skips every aside and every link, they still complete the tutorial and have working code."

**My critique:** This is a correct principle attached to an overcomplicated mechanism. The three-layer system creates a significant authoring burden, a testing burden, and — most critically — a *design* burden that will slow down the people writing the first pages.

**Why I feel strongly about this, from experience:**

When I wrote the Rust Book, I didn't have layers. I had one channel: the prose. When I needed to introduce a new concept, I said "here's what this does" in the flow of the text. When the concept had depth worth exploring, I said "we'll cover this in detail in Chapter X." That single forward-reference IS the deep-dive link, and it costs nothing to write.

The Rust Book's guessing game chapter (Chapter 2) introduces `match`, `Result`, `loop`, `crate` dependencies, user input, and random number generation. We didn't have a sidebar listing "New Rust on this page." We didn't need one. The reader encounters `match` in context, the prose says "this `match` expression compares our guess to the secret number," and we move on. By the time the reader hits Chapter 6 (Enums and Pattern Matching), they've already seen `match` work three times. They have intuition before they have formal understanding.

The three-layer system introduces three problems:

1. **It creates a meta-conversation the reader didn't ask for.** The aside that says "New NeoHaskell on this page: `|>` (pipe operator) — passes the left side as the last argument to the right side" is a grammar lesson hiding in a cooking class. The plan's own principle #7 says "No one reads a grammar chapter before cooking dinner." But a grammar sidebar IS a grammar chapter — it's just smaller and collapsible. The reader who opens it has been pulled out of the narrative. The reader who ignores it wonders whether they should have opened it.

2. **It doubles the maintenance surface.** Every code change in the tutorial now requires checking: does the narrative still work? Does the syntax aside still list the right constructs? Do the concept links still point to the right pages? This is three things to verify instead of one. For a small team, this is a tax that compounds.

3. **It conflates two different ideas: progressive disclosure and parallel channels.** Progressive disclosure (which the synthesis correctly identifies as a universal pattern) means "show simple first, offer depth on demand." That's one channel at two depths. The layer system is three parallel channels at the same depth. These are architecturally different. React's `<DeepDive>` sections work because they go deeper on the *same topic* the reader just encountered. The NeoHaskell syntax aside goes *sideways* to a different topic (language grammar) that the reader wasn't thinking about.

**My specific recommendation:**

Drop Layer 2 entirely. In the narrative, when a new construct appears for the first time, bold it and include a one-sentence parenthetical explanation inline. Example:

> We'll pipe the deposit amount through our handler using **`|>`** (which passes the left side to the function on the right — think Unix pipes):

That's it. One sentence. In the flow. The reader doesn't context-switch.

Keep Layer 3 (concept deep-dive links), but make them even more minimal than proposed. Don't put them at the end of paragraphs — put them in a small "Learn more" box at the bottom of each tutorial page section. This matches what the synthesis found in React's approach and doesn't interrupt the narrative at all.

If you still want a syntax reference, build it as a single standalone page: "NeoHaskell Syntax Quick Reference" in the Getting Started section. That's where the cheat sheet lives. The tutorial should not be the syntax reference; the syntax reference should be its own thing.

---

## II. Bloom's Taxonomy Mapping Is the Wrong Abstraction

**Responding to: "Exercise Strategy — Bloom's Taxonomy Progression" (lines 510–523)**

The plan maps each tutorial part to a Bloom's taxonomy level: Part 1 = Remember, Part 2 = Understand, Part 3 = Apply, and so on up to Part 6 = Create.

**My critique:** This is a pedagogically correct observation presented as a design constraint, and the difference matters enormously.

**Why this concerns me:**

The research analysis of the Rust Book (rust-book.md, line 381) attributes Bloom's alignment to our work: "Structure exercises to match the Rust Book's progression: Remember... Understand... Apply..." I want to correct the record: **we did not design the Rust Book's exercises around Bloom's taxonomy.** That taxonomy was not in our design documents, not in our discussions, and not in our heads. What we did was ask, at each point: "What would help this reader understand what they just learned?" Sometimes that was a "change this value and see what happens" exercise (which you could label "Remember"). Sometimes it was "design your own struct" (which you could label "Create"). But we arrived at those through empathy with the reader's position in the learning journey, not through a taxonomic framework.

The danger of adopting Bloom's as a *design constraint* is that it becomes prescriptive in ways that fight the content. Here's a concrete example from the plan:

> **Part 1 (Remember):** "Change the deposit amount to $200. Run it. What's the new balance?"
> **Part 4 (Analyze):** "Should `AccountType` be an event or a field on `AccountOpened`? Argue both sides."

Part 1's exercise is fine. Part 4's exercise is also fine. But the reason Part 4's exercise is good is NOT because "Analyze" is the right Bloom's level for tutorial part 4. It's good because by Part 4, the reader has enough context to reason about this design question. The taxonomy didn't tell you that — the progression of the tutorial told you that.

Now here's where the taxonomy actively hurts: what if the best exercise for Part 3 (mapped to "Apply") is actually a "Predict" exercise? The plan's own exercise types (line 497–505) include "Predict: What will this code output? Think first, then run it." Predicting output is a powerful learning tool for projections — you're literally asking the reader to do what a projection does (derive state from events, in their head, before the computer does it). But "Predict" doesn't map cleanly to "Apply" in Bloom's. If the writer is trying to hit the taxonomy target, they'll write a weaker "Replicate" exercise instead.

**My specific recommendation:**

Keep the Bloom's taxonomy table as a *retrospective validation tool*, not a *prospective design tool.* After writing all exercises, check: do they roughly progress in cognitive complexity? If Part 5's exercises are all "Remember" level, something went wrong. But don't start from the taxonomy when designing individual exercises. Start from: "What does this reader need to practice right now, given what they just learned?"

Remove the taxonomy labels from the tutorial progression table. Replace them with the simpler framing the plan already has in its Exercise Types list (Modify, Predict, Extend, Break, Compare, Audit). Those types are grounded in the actual activity, not an academic framework.

---

## III. The Just-in-Time Strategy Is Right but Too Rigid

**Responding to: "Just-in-Time Concept Page Strategy" (lines 372–427)**

The rule is stated as absolute: "No concept page exists until a tutorial page links to it."

**My critique:** The principle is correct — concept pages anchored to tutorial experience are dramatically better than abstract pre-written concept pages. But the absolutism creates a blind spot.

**Where the Rust Book's experience informs this:**

Chapter 4 of the Rust Book (Ownership) does something the plan's JIT rule would prohibit: it starts with a section on stack vs. heap memory. This section exists *before* any ownership code is shown. It's a concept page, essentially — a grounding explanation of background knowledge the reader needs before the new concept makes sense.

We wrote it because we tried writing the ownership chapter without it, and it didn't work. Readers kept asking "but why does it matter where the value is stored?" We needed to teach a prerequisite concept *before* the tutorial used it, or the tutorial didn't land.

NeoHaskell will face the same problem. The plan's Concept Page Schedule (lines 383–389) maps concept pages to tutorial parts. But what about concepts that the tutorial *assumes* but doesn't explicitly teach? For example:

- **Immutability.** The tutorial treats values as immutable from Part 1. A reader coming from JavaScript, where `let x = 5; x = 10;` is natural, may not understand *why* they can't reassign. The tutorial can't pause to teach immutability from first principles — it would break the narrative flow. But without a concept page on immutability available from the start, the reader has nowhere to go when they're confused.

- **Type inference.** The tutorial will use type inference from the first code example. Readers from Python or JavaScript may be confused about where the types "come from" if there are no annotations. A concept page on type inference should exist before Part 1, not be triggered by Part 2.

The synthesis research supports this. Pattern 5 (Teaching Unfamiliar Concepts Through Familiar Tasks) says "teach it by building something familiar, letting the concept emerge naturally." But Pattern 12 (Progressive Disclosure) says "mark advanced features as Optional or hide in expandable sections." These two patterns work together: the tutorial teaches through building, and concept pages exist *in the background* for readers who need the "why" earlier than the tutorial provides it.

**My specific recommendation:**

Modify the JIT rule to: "No concept page is *required reading* until a tutorial page links to it. But foundational concept pages (immutability, type inference, basic syntax orientation) may exist as *available resources* from the start." This preserves the intent (concept pages are anchored to experience) while allowing safety nets for readers who need earlier grounding.

Practically: write 2-3 "prerequisite concept" pages during Phase 0 — things like "Why Values Don't Change" (immutability) and "How NeoHaskell Knows Types" (type inference). These aren't linked from the tutorial's Layer 3 system; they're in the Concepts section for readers who go looking. They're short (500 words max), they use NeoBank examples even though the reader hasn't started the tutorial yet, and they exist as insurance.

---

## IV. The "Coming from Haskell" Page Leads with Differences When It Should Lead with Reassurance

**Responding to: "'Coming from Haskell' — Expanded Specification" (lines 460–488)**

The plan's structure for this page is: Opening → Full Rosetta Stone → "Why would you do this?" → Common AI mistakes → "What stayed the same."

**My critique:** "What stayed the same" is last. It should be first or second.

**Why this matters:**

The synthesis research explicitly identifies this pattern. Under "Coming From... (Migration Guides)" (Synthesis lines 715–720), it lists "Reassurance first" as the #1 best practice, citing Deno, Vue, and Elixir/Phoenix. The reasoning is clear: a Haskell developer arriving at this page is already skeptical. They know Haskell. They're evaluating whether NeoHaskell is worth their time. If the first thing they see is a table of 12 ways NeoHaskell differs from what they know, the emotional response is: "They changed everything. Why would I use this?"

But if they first see: "ADTs work the same. Type classes (called Traits) work the same. Records work the same. Deriving works the same. Modules work the same. GHC is under the hood" — the emotional response is: "Okay, this is Haskell. My knowledge transfers. Now show me what's different."

The Rust Book doesn't have a "Coming from C++" page, but the entire Chapter 4 follows this principle in miniature. Before teaching ownership, we say: "If you've heard the terms *shallow copy* and *deep copy* while working with other languages, the concept of copying the pointer, length, and capacity without copying the data probably sounds like making a shallow copy." We ground the new concept in what the reader already knows. We don't lead with "forget everything you know about memory management."

**My specific recommendation:**

Restructure the Coming from Haskell page as:

1. **"What Stayed the Same"** (first, and substantial — not a bullet list, but brief code examples showing Haskell code that works identically in NeoHaskell)
2. **Opening** ("NeoHaskell is a Haskell dialect, not a Haskell skin..." — this is well-written, keep it, but move it to position 2)
3. **The Rosetta Stone** (differences table)
4. **"Why would you do this?"** (rationale for each difference)
5. **Common AI mistakes** (practical, useful, keep as-is)

The restructured flow creates an emotional arc: comfort → honest framing → specific differences → reasoning → practical help. The current flow creates: confrontation → justification → belated comfort.

---

## V. The Plan Is Missing Visual Aids Almost Entirely

**Responding to: The plan as a whole, specifically the absence of diagrams in the tutorial and concept page designs**

The synthesis identifies visual aids as Pattern 9, used by 7/12 sites, and specifically calls out the Rust Book's SVG diagrams for stack/heap and ownership moves. The rust-book.md analysis recommends "Event Flow Diagrams" as a direct parallel (line 293). The plan mentions diagrams exactly zero times in the tutorial specification and concept page template.

**My critique:** This is a significant omission. The Rust Book's ownership diagrams are, by reader feedback, one of the most valuable parts of the entire book. They make invisible things visible.

**Why diagrams matter specifically for event sourcing:**

Event sourcing is an *architectural* concept. It describes how data flows through a system. The flow — Command → Aggregate → Event → Event Store → Projection → Read Model — is inherently spatial and sequential. Text descriptions of this flow are adequate. Diagrams of this flow are *immediately comprehensible.*

The Rust Book's ownership diagrams work because they show what happens in memory — something the reader can't see by running code. Event sourcing diagrams serve the same purpose: they show what happens to data as it flows through the system — something the reader can't see by looking at a single function.

Here's a specific example. The plan's Rosetta Stone (lines 138–146) maps developer language to banking terms to NeoHaskell terms. This is a table. It should also be a diagram:

```
[Developer says: "Do something"]
        ↓
   [Command: Deposit Slip]
        ↓
   [Aggregate: Account]    ← validates
        ↓
   [Event: Transaction]    ← immutable fact
        ↓
   [Event Store: Ledger]   ← append-only
        ↓
   [Projection: Statement] ← derived view
        ↓
[Developer asks: "What's my balance?"]
```

This diagram, shown once in Tutorial Part 1, would anchor *every subsequent concept* the reader encounters. "Where are we in the diagram?" becomes a spatial question the reader can answer at any point in the tutorial.

**My specific recommendation:**

Add a "Visual Aids" requirement to the tutorial design constraints (line 148). Specifically:

1. **One architectural diagram** appears in Tutorial Part 1 and is referenced in every subsequent part. This is the "event flow" diagram. It should be an SVG that can be incrementally highlighted (Part 1 highlights Command → Event, Part 3 highlights Projection → Read Model, etc.).

2. **The concept page template** (lines 393–418) should include a "## Visual" section between "The One-Sentence Version" and "In NeoBank Terms." Not every concept page needs a diagram, but the template should prompt the writer to consider one.

3. **Event timeline diagrams** should appear in Parts 3 and 6, showing the event stream as a visual timeline. This is the event-sourcing equivalent of the Rust Book's memory diagrams — it makes the append-only log *visible.*

The plan's Interactive Elements section (lines 567–583) mentions "Interactive transaction timeline" in Phase 2. Don't wait for interactivity. A static SVG timeline in Phase 1 is 80% of the value at 10% of the cost.

---

## VI. Checkpoint Sections Need to Be Formalized, Not Assumed

**Responding to: Tutorial design, specifically the absence of explicit checkpoint structure**

The synthesis identifies Checkpoint/Verification Sections as Pattern 4, used by 9 out of 12 sites. The plan's tutorial design constraints (line 148) say "Each part results in running code the reader wrote themselves" but never specifies a checkpoint format.

**My critique:** "Running code" is the output. A checkpoint is the *verification mechanism* — the specific commands the reader types, the exact output they should see, and what to do if they don't see it.

In the Rust Book, we show the full output of `cargo run` after every significant code change. We show the *exact* text the reader should see in their terminal. When the output changes because of an intentional error, we show that too. This isn't decoration — it's how readers confirm they're on the right track.

The plan's exercise strategy includes "Modify" and "Extend" exercises, and the Compiler-as-Teacher pattern includes showing expected error messages. But there's no equivalent of FastAPI's "Check it" sections or the Rust Book's terminal output blocks for the *happy path.* The reader writes the deposit handler, but nowhere does the plan say "run this, see this output, confirm this is what happened."

**My specific recommendation:**

Add a formal "Verify Your Progress" section template to the tutorial design constraints. Every tutorial part ends with:

```markdown
### Verify Your Progress

Run your project:
```bash
neo run
```

Then in the REPL:
```
> deposit "alex-checking" 100
MoneyDeposited { accountId = "alex-checking", amount = 100 }

> balance "alex-checking"
$100.00
```

**If you see this, you're on track.** Your account has one event, and the balance is derived from it.

**If you see an error:**
- `NotInScope: deposit` → make sure you exported the function in your module header
- `TypeMismatch` → check that your amount is an `Int`, not a `String`
```

This format — command, expected output, success confirmation, common errors — is directly from Stripe and FastAPI's patterns and it's the single most anxiety-reducing thing you can put in a tutorial.

---

## VII. The 20 Misconceptions List Is Excellent but the Validation Plan Is Too Vague

**Responding to: "The 20 Misconceptions (Design Before Structure)" (lines 69–96)**

The misconceptions list is one of the strongest parts of this plan. We didn't do this explicitly for the Rust Book, and I wish we had. Enumerating the specific wrong beliefs your readers hold, and designing content to address each one — that's rigorous pedagogy.

**My critique:** The validation plan at the bottom says:

> **Action**: Validate these with real developers before finalizing structure. Post in Discord, ask in the Haskell subreddit, talk to 5 people who looked at NeoHaskell and didn't adopt it.

This is too vague to be actionable, and the third item (talking to non-adopters) is the most valuable yet hardest to execute. The plan should specify *how* to validate, not just *that* you should validate.

**My specific recommendation:**

Replace the current Action item with a concrete validation protocol:

1. **Survey (quantitative):** Create a 5-minute survey with 20 true/false statements corresponding to the misconceptions. Post it in Discord, Haskell subreddit, and NeoHaskell's social channels. Target: 30+ responses. The statements that >60% answer incorrectly are confirmed misconceptions. The ones that <30% answer incorrectly might not need dedicated content.

2. **Interviews (qualitative):** Talk to 5 developers who *evaluated* NeoHaskell and decided not to use it. Ask: "What confused you? What would have helped?" These conversations will surface misconceptions you haven't thought of and confirm which ones from the list are actually blocking adoption. Don't ask leading questions — let them tell you what was hard.

3. **Observation (behavioral):** Give 3 developers the current Getting Started page and watch them use it. Don't help. Write down every place they pause, frown, or ask a question. Those pauses are misconceptions in action.

The plan already says "Test with someone who's never seen NeoHaskell (observe, don't help)" in Phase 1. That's the right instinct. Extend it to Phase 0 for misconception validation.

---

## VIII. The "Alex" Character Is a Risk the Plan Doesn't Acknowledge

**Responding to: "The Character" (lines 109–111)**

> Alex is the reader. Not a banker — a developer. Every part starts with what Alex (the developer) wants to build.

**My critique:** The Rust Book doesn't use a named character. Neither does the React tutorial, the FastAPI tutorial, the Go Tour, or the Svelte tutorial. The Django tutorial uses a "polls app" but no character. Laravel's tutorials don't use characters.

Named characters in technical tutorials can work (Stripe's guides occasionally reference personas), but they introduce a specific risk: **the reader stops identifying with Alex the moment Alex's situation doesn't match theirs.** If Alex is "building a bank," the reader who's evaluating NeoHaskell for a logistics platform has to constantly translate. The framing device becomes friction.

More practically: "Alex wants to..." is a sentence structure that gets repetitive across six tutorial parts. By Part 4, the reader doesn't care what Alex wants. They care what *they* want to build next.

**My specific recommendation:**

Don't remove Alex entirely — the character is useful for Part 1's opening to establish the scenario. But let Alex fade into the background by Part 2. Switch from "Alex wants to add overdraft protection" to "Next, we'll add overdraft protection." The reader should BE Alex, not READ ABOUT Alex. Second person ("you") is more immersive than third person ("Alex") for tutorials.

The plan's opening line of the tutorial (lines 165–170) is written in first person plural ("Let's prove it works by building a bank"). That's the right voice. The Alex framing should support this voice, not compete with it.

---

## IX. The Plan Contradicts Itself on Research

**Responding to: "What We're NOT Doing (And Why)" (lines 753–766)**

The first rejected approach is:

> **Analyzing 12 documentation sites** — Diminishing returns after 3. Time is better spent on learner research.

Yet the plan is being written alongside a SYNTHESIS.md that analyzes exactly 12 documentation sites. And that synthesis produced actionable insights — visual aids, checkpoint formats, progressive disclosure patterns, anti-patterns to avoid — that the plan incorporates (sometimes) and misses (other times, as I've noted above).

**My critique:** This contradiction suggests the plan was written before the research was complete, and the "What We're NOT Doing" section wasn't updated. That's fine — plans evolve. But it also means the plan hasn't fully absorbed the research findings. The synthesis identified specific patterns the plan doesn't adopt:

1. **"What you'll learn" upfront sections** (Synthesis Pattern 16, from Stripe/React/Astro/FastAPI). The plan's tutorial pages have no "learning objectives" header. Adding one would take 30 seconds per page and significantly help readers decide whether to read or skip.

2. **Progress tracking UI** (Synthesis recommendation under Tutorial best practices, lines 463–465). The plan never mentions visual progress indicators. For a 6-part tutorial, knowing "you're on part 3 of 6" reduces the "how much longer?" anxiety that causes mid-tutorial abandonment.

3. **Troubleshooting sections in reference docs** (Synthesis Pattern, under Reference, lines 673–677). The plan's reference strategy (lines 344–369) mentions auto-generated API docs and hand-written cross-cutting patterns, but no troubleshooting. The synthesis cites React, Svelte, and Deno as organizing troubleshooting by symptom: "I'm getting error X" → solution. This is the "Common Errors" page (line 645), but it should also be embedded in reference pages for specific modules.

**My specific recommendation:**

Remove or rewrite the "Analyzing 12 documentation sites" entry in the "What We're NOT Doing" section — it's empirically false, given that you did the analysis and it was valuable. Replace it with something honest, like: "We analyzed 12 sites but adopted selectively, not comprehensively."

Then do a deliberate pass through SYNTHESIS.md's "Must Adopt" recommendations (Synthesis lines 819–856) and check each one against the plan. I count at least three "Must Adopt" items that the plan handles only implicitly:

- **Checkpoint/Verification Sections** → no formal template in the plan
- **Quick Reference Tables** → mentioned in cheat-sheet.mdx but not in reference strategy  
- **Cross-Reference Density** → the Layer 3 links are cross-references, but the plan doesn't specify "See Also" sections at page bottoms, which the synthesis identifies as universal

---

## X. Correcting the Analysis of the Rust Book

**Responding to: research/rust-book.md**

The analysis is largely accurate and I appreciate its thoroughness. But I want to correct several points where it attributes patterns to us that are either wrong or overstated.

### Correction 1: We Didn't Use Bloom's Taxonomy

The analysis (line 381) presents a Bloom's alignment as if we designed it:

> **Bloom's Taxonomy Alignment:** Structure exercises to match the Rust Book's progression: Remember... Understand... Apply...

We didn't. Our exercises were designed around "what does the reader need to practice here?" The Bloom's mapping is a reasonable *post-hoc* analysis of what we ended up with, but presenting it as a deliberate strategy misrepresents our design process. The NeoHaskell plan's adoption of Bloom's as a design constraint (not just a validation lens) may stem from this misattribution.

### Correction 2: Chapter 2 Doesn't Teach Ownership Before Explaining It

The analysis says (line 139, paraphrased in multiple places):

> Chapter 2 uses ownership before explaining it.

This is imprecise. Chapter 2's guessing game doesn't exercise ownership semantics in any meaningful way. What Chapter 2 does is use `match`, `Result`, `loop`, external crates, and string parsing before formally explaining them. The reader encounters `match` and we say "we'll cover this in Chapter 6." Ownership isn't a concept that comes up in the guessing game — it's the topic of Chapter 4.

What IS true is that Chapter 2 uses Rust's general syntax — `let` bindings, function calls, method chaining — before Chapter 3 formally explains these constructs. This is the "implicit-first" strategy the NeoHaskell plan correctly identifies. But it's about *syntax*, not about ownership specifically.

### Correction 3: The "Interactive Variant" Isn't Ours

The analysis (line 224) mentions the Brown University enhanced version as if it's an official Rust project:

> The book promotes an enhanced version at https://rust-book.cs.brown.edu with quizzes, highlighting, visualizations.

This was created by Will Crichton's research group at Brown, not by the Rust Book team. We link to it as a useful resource, but we didn't build it and it wasn't part of our docs strategy. The NeoHaskell plan shouldn't treat this as evidence that the Rust Book team prioritized interactive elements — we didn't, and I'd actually say that's a gap in our work that NeoHaskell should learn from rather than repeat.

### Correction 4: Error Codes Aren't a Docs Decision

The analysis recommends adopting Rust's error code system (line 417):

> NeoHaskell errors should have: Error code [NH0042], Error message, Help text, Explanation

This is a compiler feature, not a documentation feature. Rust's `--explain` flag works because we invested heavily in the compiler's diagnostic infrastructure. The NeoHaskell plan's Compiler-as-Teacher pattern assumes friendly error messages exist. Open Question #5 (line 789) asks: "What does NeoHaskell's error output actually look like?" If the answer is "not friendly yet," the Compiler-as-Teacher pattern can't be the teaching mechanism — it becomes aspirational fiction.

**My recommendation:** Resolve Open Question #5 before committing to the Compiler-as-Teacher pattern's current design. If errors aren't friendly yet, the Break-It exercises should show *what the reader will actually see* (even if it's ugly) with a prose explanation, not a prettified mock error that doesn't match reality.

---

## XI. The 6-Part NeoBank Progression Has a Pacing Gap

**Responding to: "The Progression" (lines 128–136)**

The progression is:

1. First Transaction (deposit, events, balance as fold)
2. Account Rules (validation, declined commands)
3. Transaction History (projections, read models)
4. Multiple Accounts (multiple aggregates)
5. Transfers (cross-aggregate, sagas)
6. Audit Everything (replay, time-travel)

**My critique:** The cognitive jump from Part 4 to Part 5 is the steepest in the entire tutorial, and the plan doesn't acknowledge or mitigate it.

Parts 1–4 all operate within a single aggregate (one account, then multiple accounts that are structurally identical). The reader's mental model is: "I have a thing, events happen to it, I derive state from events." This is comfortable by Part 4.

Part 5 (Transfers) introduces cross-aggregate coordination — a fundamentally different architectural concept. Suddenly the reader must think about: two aggregates changing in response to one command, what happens if one succeeds and the other fails, sagas or process managers as coordination mechanisms. This is equivalent to the Rust Book's jump from single-threaded to concurrent programming (Chapters 13–16). We devoted an entire chapter to fearless concurrency because the mental model shift is that significant.

The plan gives cross-aggregate transfers one tutorial part. That's likely not enough. The "wow" moment listed is "Two bounded contexts, and it just works." But the reader's actual experience might be "Two bounded contexts, and I'm confused about which events belong where."

**My specific recommendation:**

Either split Part 5 into two parts (5a: "Same-account transfers" as a warm-up that keeps things in one aggregate, 5b: "Cross-account transfers" that introduces the real coordination problem) or add a concept interlude between Parts 4 and 5.

In the Rust Book, we placed the concurrency chapter *after* the reader had experience with iterators, closures, and smart pointers — building blocks they'd need. For Part 5, the reader needs to understand bounded contexts (introduced in Part 4) AND process managers (not yet introduced). The plan's concept page for "effects.mdx" is scheduled for Part 5, but effects are a prerequisite for understanding Part 5, not a companion concept. This is the same problem I described in Section III — sometimes you need the concept *before* the tutorial uses it.

---

## XII. The Compiler-as-Teacher Pattern Is Well-Designed but Needs a Fallback

**Responding to: "Compiler-as-Teacher Pattern" (lines 527–559)**

This section is one of the plan's strongest contributions. The five-step pattern (show correct code → instruct break → show error → explain → fix) directly mirrors what we do in the Rust Book, and the "Break-It Exercises by Convention" table is concrete and actionable. The placement rules (one per part, after correct usage, targeting AI-generated patterns) are smart constraints.

**My critique:** The pattern assumes the compiler produces the right error messages. If the compiler doesn't, the exercise becomes frustrating instead of illuminating.

The plan's table (lines 542–550) shows paraphrased expected errors like:

> "NeoHaskell uses do blocks for all bindings"
> "NeoHaskell uses |> for function piping"
> "Eta-reduce — NeoHaskell requires explicit lambdas"

These are *linter* messages, not compiler errors (the table itself distinguishes these in the "Enforced by" column — some are Compiler, some are Linter, some are Convention). This distinction matters because:

1. Compiler errors are blocking — the code won't compile. The reader MUST fix them.
2. Linter warnings are advisory — the code compiles but the linter complains. The reader MIGHT ignore them.
3. Convention enforcement — the code compiles and no tool complains, but the community norm is different. The reader WON'T know they're doing it wrong.

The Break-It exercises should be explicit about which category each belongs to. A reader who "breaks" their code by using `$` instead of `|>` and sees a linter warning (not a compilation failure) will have a different experience than the plan implies.

**My specific recommendation:**

Add a column to the Break-It table: "Enforcement level" with values "Compile error" / "Linter warning" / "Convention." Then adjust the exercise prose accordingly:

- For compile errors: "Try this. Your code won't compile. Here's the error."
- For linter warnings: "Try this. Your code compiles, but the linter will flag it. Here's the warning."
- For conventions: "This compiles and the linter accepts it, but NeoHaskell developers never write it this way. Here's why."

Each enforcement level teaches a different lesson. Compile errors teach "the language prevents this." Linter warnings teach "the community discourages this." Conventions teach "this works but isn't idiomatic." Conflating them all as "the compiler catches this" undermines the reader's trust when they discover that some of these "errors" are actually just suggestions.

---

## XIII. The "What We're NOT Doing" Section Gets One Thing Importantly Right and Should Double Down

**Responding to: "Separate 'Learn NeoHaskell' section before the tutorial" and "Formal grammar reference page" (lines 763–766)**

These two rejected approaches demonstrate excellent pedagogical judgment:

> **Separate "Learn NeoHaskell" section before the tutorial:** Forces readers through a grammar chapter before they have context. Language is learned by using it, not by studying it.

> **Formal grammar reference page:** NeoHaskell is a dialect, not a new language. A grammar spec signals "this is academic" instead of "this is practical."

I agree completely with both decisions. They reflect the same philosophy that made the Rust Book's Chapter 2 (guessing game before language fundamentals) effective.

**My critique:** The plan then partially contradicts both decisions by including `reading-neohaskell.mdx` ("5-minute annotated code walkthrough") and `cheat-sheet.mdx` in the Getting Started section (Phase 0 tasks, lines 600–601). These are, functionally, a grammar-before-the-tutorial section. They're just shorter.

Now — I think these pages are actually fine. A 5-minute annotated walkthrough is not the same as a grammar chapter. A cheat sheet is not the same as a formal reference. The contradiction is in the rhetoric, not the content. But the plan should acknowledge the tension: "We're not doing a grammar chapter, but we ARE providing a quick orientation for readers who want to preview the syntax before diving into the tutorial. These pages are optional and short."

**My specific recommendation:**

Add a note to the `reading-neohaskell.mdx` and `cheat-sheet.mdx` descriptions clarifying that they are *optional pre-reading*, not prerequisites. On the pages themselves, include a prominent callout:

> **You don't need to read this page before starting the tutorial.** It's here for readers who want a quick preview of NeoHaskell's syntax. If you prefer to learn by doing, skip straight to [Tutorial Part 1](/tutorial/01-first-transaction).

This maintains the "implicit-first" philosophy while serving readers who genuinely prefer a syntax orientation. It's the "multiple doors, one house" principle (Guiding Principle #6) applied correctly.

---

## XIV. Patterns the Research Identifies That the Plan Should Adopt

Beyond the issues I've already raised, here are research findings from SYNTHESIS.md that the plan doesn't address:

### A. Inline Glossary / Tooltips (Synthesis Pattern 10)

The plan has the Rosetta Stone table and concept links, but no inline tooltips. For a domain (event sourcing) with specialized vocabulary that readers encounter before formal definition, tooltips are high-value and low-cost. Starlight supports custom components; a `<Term>` component that shows a tooltip on hover would dramatically reduce the "what does 'projection' mean?" friction in early tutorial parts.

**Recommendation:** Add a `<Term>` Astro component to Phase 0 infrastructure. Use it for the first occurrence of: event, command, aggregate, projection, bounded context, event store. The tooltip text comes from the Rosetta Stone table.

### B. Platform-Specific Installation Tabs (Synthesis, Getting Started best practices)

The plan's installation page (`installation.mdx`) doesn't mention platform-specific instructions. Open Question #1 asks "Is Nix the only installation path?" but regardless of the answer, the *presentation* should use tabs (macOS / Linux / Windows / Docker). Starlight supports tabbed content natively.

**Recommendation:** Design the installation page with tabs from the start, even if the initial content is the same across platforms. The tab structure signals "we support your platform" before the reader reads a word.

### C. "See Also" Sections at Page Bottoms (Synthesis Pattern 11)

The plan's Layer 3 links are inline concept references. But the synthesis identifies "See Also" sections at page bottoms as a universal pattern (all 12 sites). These serve a different purpose: after the reader finishes a page, they need to know where to go next. "See Also" sections prevent the "dead end" feeling that causes readers to leave the docs entirely.

**Recommendation:** Add a "See Also" template to every tutorial and concept page:

```markdown
## Next Steps

- **Continue the tutorial:** [Part 3: Transaction History](/tutorial/03-transaction-history)
- **Go deeper:** [What is a Projection?](/concepts/projections)
- **Try it yourself:** [Guide: Testing Event-Sourced Systems](/guides/testing)
```

---

## XV. The Opening Line of the Tutorial Is Almost Perfect

**Responding to: "The Opening Line of the Tutorial" (lines 165–170)**

> *"Every bank in the world runs on the same idea: don't change the number, record what
> happened. A deposit isn't 'set balance to $150' — it's 'recorded: $50 deposited.'
> NeoHaskell is built on this same idea. Let's prove it works by building a bank.*
>
> *By the end of this tutorial, your NeoBank will handle accounts, transactions, transfers,
> and a complete audit trail. It'll feel like enterprise software. It was 200 lines of code."*

This is excellent. It's concrete, it's confident, and the final sentence is a genuine hook. Two small notes:

1. **"It was 200 lines of code"** — only works if it's true. If the actual line count is 350, this becomes a credibility problem. Verify the line count against real implementations before committing to a number. Better yet: write the tutorial code first, count the lines, then write this sentence.

2. **The opening sentence** assumes the reader knows how banks work at a ledger level. Most developers don't think about banking at the ledger level — they think about banking at the app level (log in, see balance, transfer money). The sentence "don't change the number, record what happened" is clear to someone who already understands event sourcing. For the target audience (CRUD-thinkers), consider grounding it even more concretely:

> *"When you deposit $50, your bank doesn't find your balance and change it to $150. It records a transaction: '$50 deposited.' Your balance is calculated from those transactions — every time."*

This version uses second person ("your bank"), uses a concrete number ($50 → $150), and explains the mechanism (calculated from transactions). It meets the reader at "I use a bank" rather than "I understand how banks work."

---

## Summary of Recommendations

Ranked by impact:

1. **Drop Tutorial Layer 2** (syntax sidebar). Integrate syntax explanations inline. Build a standalone syntax reference page instead.
2. **Add formal checkpoint/verification sections** to the tutorial template with exact commands and expected output.
3. **Add visual aids** — one architectural event-flow diagram in Part 1, referenced throughout. Event timeline diagrams in Parts 3 and 6.
4. **Restructure "Coming from Haskell"** to lead with "What Stayed the Same."
5. **Relax the JIT concept page rule** to allow foundational concept pages (immutability, type inference) before the tutorial needs them.
6. **Demote Bloom's Taxonomy** from design constraint to retrospective validation tool.
7. **Address the Part 4 → Part 5 pacing gap** by splitting Part 5 or adding a concept interlude.
8. **Add enforcement level** to the Compiler-as-Teacher Break-It table.
9. **Formalize misconception validation** with a specific protocol (survey + interviews + observation).
10. **Resolve the plan vs. research contradiction** in "What We're NOT Doing."
11. **Add "See Also" sections**, "What you'll learn" headers, and progress indicators to the tutorial template.
12. **Let Alex fade** from named character to second-person voice by Part 2.
13. **Verify the "200 lines of code" claim** and the "wow" moments against actual implementations.
14. **Add a `<Term>` tooltip component** for event sourcing vocabulary.

---

## Closing Thought

The parallel between NeoHaskell teaching event sourcing and Rust teaching ownership is genuinely apt. Both require the reader to *unlearn* a default assumption they don't know they're making. The CRUD developer doesn't think "I'm a CRUD developer" any more than the Java developer thinks "I rely on garbage collection." The assumption is invisible until something makes it visible.

The Rust Book's strategy for making ownership visible was: let the reader write code that feels natural, let the compiler reject it, and then explain *why* the rejection is the language helping them. Your Compiler-as-Teacher pattern gets this exactly right. The plan's overall "implicit-first" philosophy gets this exactly right. The banking domain gets this exactly right.

Where the plan overcomplicates things — layers, taxonomies, rigid rules — it's because the plan is trying to be as thorough as the research that informed it. But thoroughness in *planning* doesn't always produce clarity in *execution.* The Rust Book's second edition was better than the first not because we planned more, but because we wrote real pages, watched real people read them, and fixed what didn't work.

Ship Tutorial Part 1. Watch someone use it. Fix everything. Then write Part 2.

That's how good docs happen.

— Steve Klabnik
