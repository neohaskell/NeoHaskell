---

# Review of the NeoHaskell Documentation Plan

**Reviewer: Tania Rascia**
**Date: February 2026**

---

## Opening

I've written hundreds of tutorials over the past decade. My most popular posts — building a CRUD app in React, understanding authentication, getting started with Node — all share one trait: they got out of the reader's way. The reader came to *do* something, and I helped them do it. They didn't come to appreciate a pedagogical framework. They came to make something work on their screen.

I've read the full DOCS-PLAN.md, the cross-cutting research synthesis, the FastAPI and Laravel analyses, and Steve Klabnik's review. Steve's review is excellent — he speaks from the perspective of someone who *wrote* one of the best programming books ever made. My perspective is different. I speak from the perspective of someone who writes for the person Googling "how do I build a REST API" at 11pm on a Tuesday, who's already frustrated, already behind on a project, and who will close my tab in 90 seconds if I don't show them something that works.

That person is your most important reader. And I'm worried this plan doesn't understand their reality well enough.

What follows is a section-by-section critique. I agree with Steve on several points (particularly dropping Layer 2, demoting Bloom's, and the checkpoint formalization), so where he's already said it well, I'll build on his argument rather than restate it. Where I disagree with him or see something he missed, I'll say so.

---

## I. The Plan Has a Planning Problem

**Responding to: The plan as a whole — its ratio of meta-structure to actual content.**

Before this plan tells me what Tutorial Part 1 *says*, it tells me about:
- A 5-stage mental model journey (lines 53–60)
- 20 enumerated misconceptions with a validation protocol (lines 69–96)
- A Rosetta Stone table mapping developer terms to banking terms to NeoHaskell terms (lines 138–146)
- A three-layer tutorial system with design rules (lines 179–232)
- An exercise strategy with six exercise types (lines 492–505)
- A Bloom's taxonomy mapping (lines 510–523)
- A Compiler-as-Teacher pattern with a six-row table (lines 527–559)
- A concept page template with five mandatory sections (lines 393–418)
- Interactive element phases 1 through 3 (lines 567–583)

That's roughly 400 lines of planning infrastructure before the plan gets to its work phases.

**My critique:** This plan is so thorough about *how* to teach that it hasn't left room for the messy, human, iterative process of *actually teaching.* The best tutorials I've written didn't come from a framework. They came from me sitting down, writing the thing I wanted to explain, reading it back, and asking: "Would 2015-me have understood this?" If no, I rewrote it. If yes, I shipped it.

I've seen this pattern before. Teams spend weeks building taxonomies, templates, and review processes, and then burn out before the tutorial exists. The plan's own Phase 0 has *eight tasks* before anyone writes tutorial content — including "Study reference docs: Rust Book, Stripe, react.dev (teaching method)." You've already *done* that study. It produced 12 research documents and a synthesis. Doing it again in Phase 0 is procrastination dressed as preparation.

**Why this is specifically dangerous for a newcomer-focused project:**

When I'm writing a tutorial for beginners, every extra planning artifact is an artifact that's *not* a page someone can read. The reader doesn't benefit from your mental model journey chart. They benefit from a page that says "type this, run this, see this." Every hour spent refining the taxonomy is an hour not spent writing content that helps a real person.

The FastAPI analysis (fastapi.md, lines 76–89) identifies a key pattern: "Minimal Example First." Every FastAPI tutorial page starts with the *smallest possible working code.* The NeoHaskell plan starts with the smallest possible working *planning document.* These are not the same thing.

**My specific recommendation:**

Gut Phase 0 down to three tasks:
1. Set up CI that verifies code blocks compile.
2. Write the installation page.
3. Write Tutorial Part 1.

Everything else — misconception validation, sidebar configuration, architecture docs, cheat sheets — happens *after* Tutorial Part 1 exists. You will discover what your misconception list is missing the moment you try to write the first page. You will discover what your exercise strategy should be the moment you try to write the first exercise. The plan itself says "Writing early is research" (line 621). Then *act like it.* Make Phase 0 about writing, not about planning to write.

Steve says "Ship Tutorial Part 1. Watch someone use it. Fix everything. Then write Part 2." I agree so hard it hurts. But I'd go further: **ship an imperfect Tutorial Part 1 in Week 1, not a perfect one in Week 3.** The imperfect version will teach you things the perfect plan never will.

---

## II. The Tutorial Is Teaching Two Things at Once and Doesn't Admit It

**Responding to: Guiding Principle #7 (line 17) and the Tutorial Progression (lines 128–136)**

The plan says: "The tutorial teaches the language implicitly. Concept pages teach it explicitly."

This sounds great. It mirrors what FastAPI does with type hints (teach them by using them) and what the Rust Book's Chapter 2 does with `match` and `Result` (use them before explaining them). The synthesis correctly identifies this as a winning pattern.

**My critique:** FastAPI teaches *one* unfamiliar thing (type hints) through *one* familiar thing (building an API). The Rust Book's Chapter 2 teaches Rust *syntax* through a *guessing game* — a task so simple that syntax is the only new thing.

NeoHaskell's tutorial is teaching *two* unfamiliar things simultaneously:
1. **NeoHaskell syntax** — a Haskell dialect most readers have never seen
2. **Event sourcing** — an architectural pattern most readers have never used

And it's teaching them through a *banking domain* that, while universally understood at the ATM level, is not universally understood at the *ledger* level. (Steve makes this same observation about the opening line — most people think of banks as apps, not as ledgers.)

This means the reader of Tutorial Part 1 is simultaneously learning:
- What `do` blocks look like
- What `|>` means
- What type declarations look like
- What an event *is*
- What a command handler *is*
- What a fold over events *is*
- Why you'd derive balance from events instead of storing it

That's at least seven new concepts in a single tutorial page. The plan says "maximum 3-5 constructs per page" for the syntax aside (line 205), but that's counting only *syntax.* The domain concepts aren't counted. Asking a JavaScript developer to simultaneously parse `MoneyDeposited { accountId = "alex-checking", amount = 100 }` (unfamiliar syntax) while understanding *why* this is an event and not a database row (unfamiliar architecture) is a brutal cognitive load.

**Why this matters from my experience writing tutorials:**

When I wrote my React CRUD tutorial, I used a familiar domain (a simple note-taking app) specifically so that the *domain* wasn't a learning burden. The reader already knows what "add a note" means. All their cognitive energy goes to learning React.

When I wrote my Node/Express tutorial, I built a basic API — familiar enough that the reader isn't thinking about the domain at all. All their energy goes to Express routing and middleware.

The NeoHaskell tutorial is asking readers to spend cognitive energy on *three* simultaneous learning channels: syntax, architecture, and domain. That's one too many.

**My specific recommendation:**

Before Tutorial Part 1, add a "Tutorial Part 0" or a "Getting Started" bridge page that does ONE thing: gets the reader comfortable with NeoHaskell *syntax* in complete isolation from event sourcing. Not a grammar chapter — a 10-minute, hands-on, "let's write some NeoHaskell" page.

Something like:

> Let's write a small program before we start building NeoBank. Don't worry about understanding everything — just get the feel.

Show a 10-line program that uses `do`, `|>`, `Task.yield`, `Result.ok`, and a type declaration — but does something *simple* like processing a list of names or calculating something. No events, no commands, no aggregates. Just NeoHaskell syntax doing something basic.

Then Tutorial Part 1 can introduce event sourcing with less syntax friction, because the reader has already seen `do` blocks and `|>` once. They're not new anymore. They're *recent.*

I know this contradicts the plan's "No separate Learn NeoHaskell section" philosophy. But I'm not proposing a grammar chapter. I'm proposing a *warm-up exercise.* Athletes stretch before they run. Readers should type some NeoHaskell before they learn event sourcing in NeoHaskell. Five to ten minutes, max. It reduces the cognitive load of Part 1 enormously.

Steve recommends relaxing the JIT concept page rule for foundational concepts like immutability and type inference. I'm making a more practical version of the same argument: don't just have a *page* available — have a *hands-on warm-up* that builds physical familiarity with the syntax. Reading about syntax isn't the same as typing it.

---

## III. The "I'm Lost" Moment Is Completely Unaddressed

**Responding to: The tutorial design as a whole, and the absence of error recovery strategies**

Here's what happens in every tutorial ever written, including mine: the reader follows along, something goes wrong, and they can't figure out why. Their code doesn't compile. Their output doesn't match. They mistyped something three steps back and now everything is broken. 

At that moment, the reader makes a binary decision: **keep going** or **close the tab.** The quality of your documentation at that exact moment determines your conversion rate more than anything else.

The plan has no strategy for this moment.

Steve correctly identifies the need for checkpoint/verification sections and proposes a template (his Section VI). I agree with his template. But checkpoints catch the error. They don't *recover* from it.

**What the plan is missing:**

1. **"If you're stuck" escape hatches on every tutorial page.** When the reader's code doesn't match the expected output, they need immediate help. Not "go to Discord" — that's a multi-hour feedback loop. Something like a collapsible section: "Stuck? Click to see the complete code for this section." I do this in nearly every tutorial I write. It's the single most reader-friendly thing you can offer.

2. **Full source code for each tutorial part.** The plan mentions a NeoBank project the reader builds progressively, but doesn't mention providing downloadable/viewable source code for each stage. At the end of Part 2, the reader should be able to visit a GitHub link and see *exactly* what their project should look like — every file, every line. This is how the Astro tutorial works (the synthesis identifies it under "Checkpoint pages," SYNTHESIS.md lines 465–466). It's how React's tutorial works. It's how every tutorial I've ever written works. 

3. **A "Common Issues" section per tutorial page.** Not just in the reference docs. On the tutorial page itself. "If you see `ModuleNotFound`, you forgot to add the import on line 3." "If your balance shows 0, make sure your fold function starts with the initial value." These are not hypothetical problems — they are *inevitable* problems, and you know what they'll be because you've written the code.

4. **A reset mechanism.** "If you're completely lost, run `git checkout part-2-start` to get a clean starting point for this section." This requires maintaining a tutorial repo with tagged branches — extra work, but it's the difference between a reader spending 30 minutes debugging a typo and a reader closing your tab forever.

**Why this matters more than almost anything else in the plan:**

The plan's success metric #2 (line 773) is "Tutorial completion rate: >50% of people who start part 1 finish part 4." I've been writing tutorials for a decade, and I can tell you with confidence: the #1 reason people don't finish tutorials isn't that the content is bad. It's that they got stuck, couldn't figure out why, and left. Every tutorial I've ever written that *didn't* have "stuck? here's the full code" sections had dramatically lower completion rates than ones that did.

The plan's Exercise Strategy (lines 492–505) and Compiler-as-Teacher pattern (lines 527–559) assume the reader is in control and deliberately exploring. Those are great for the reader who's on the happy path. The reader who's lost isn't exploring — they're drowning. Your docs need a lifeguard.

**My specific recommendation:**

Add to the tutorial design constraints (line 148):
- Every tutorial part includes a "Stuck?" collapsible section at the end with the complete working code
- Every tutorial part links to a tagged branch/commit in a public tutorial-companion repo
- Every checkpoint section includes 3-5 "If you see [error], check [fix]" items
- The tutorial index page includes a link to the complete NeoBank source code with a note: "If you want to see the finished product before starting, here it is"

This last point is controversial — some people think showing the finished product spoils the tutorial. In my experience, it does the opposite. It gives the reader confidence that the tutorial actually leads somewhere, and it gives them a reference when they're lost.

---

## IV. The Exercises Are Written for the Teacher, Not the Student

**Responding to: Exercise Strategy (lines 492–523) and the exercise examples throughout**

Steve already addressed the Bloom's taxonomy issue — I agree it should be a validation tool, not a design tool. But I have a different concern about the exercises themselves: they're written from the perspective of someone who *already understands* the material.

Look at the exercise for Part 3 (line 518):

> "Create a `MonthlyStatement` projection that groups transactions by month."

And Part 4 (line 519):

> "Should `AccountType` be an event or a field on `AccountOpened`? Argue both sides."

And Part 5 (line 520):

> "This transfer handler doesn't check the source balance. What goes wrong? Fix it."

**My critique:** These are *exam questions*, not learning exercises. They test whether the reader has internalized the concept. But the reader just encountered the concept for the first time 5 minutes ago. They haven't internalized anything yet. They're still figuring out what a projection *is.* Asking them to "create a MonthlyStatement projection that groups transactions by month" is asking them to simultaneously: understand the projection pattern, understand how grouping works, design a new data structure, and implement it — all in a concept they learned on the same page.

**What exercises should actually look like for beginners:**

When I write a tutorial, my exercises follow a strict graduation:

**Level 1 (during the page): "Change one thing."** Not even "modify" — just *change a value.* "Change the deposit from $100 to $250 and re-run. What's the new balance?" This is what Part 1 does, and it's correct. But every subsequent part jumps to much harder exercises.

**Level 2 (end of page): "Do the same thing again with different data."** Not a *new* feature — the *same* feature with different inputs. "Create a second account called `alex-savings` and deposit $500 into it." This verifies the reader can replicate the pattern, not just copy-paste it.

**Level 3 (optional challenge, clearly marked): "Extend this."** NOW you can ask for the MonthlyStatement projection. But label it clearly: "Challenge (optional)." Don't make the reader feel like a failure if they skip it.

The plan's exercise types (Modify, Predict, Extend, Break, Compare, Audit) are a good taxonomy. But the *instances* of those types are pitched at the wrong difficulty level for a reader who's genuinely new to both NeoHaskell and event sourcing.

**A specific contrast:**

FastAPI's exercises (implicit in their tutorial structure — fastapi.md, lines 94–104) are dead simple: after showing how path parameters work, the implied exercise is "add another endpoint with a different path parameter." Same pattern, different data. The reader practices the mechanic, not the concept. The concept sinks in through *repetition of the mechanic.*

**My specific recommendation:**

For Parts 1-4, restructure exercises as:

1. **Mandatory "Do It Again"**: Same pattern the page taught, different data. Not optional. Not challenging. Pure repetition. "Create an event called `InterestAdded` that adds $5 to the account. Check the balance."

2. **Optional "Stretch Goal"**: The harder exercise currently listed. Clearly marked as optional with a label like "Ready for more?" or "Challenge." Includes a hint or a link to the solution.

3. **"Predict" exercises stay** — they're perfect for projections (Part 3) because you're literally asking the reader to BE a projection. But frame them with less academic language: "Before you run this, write down what you think the balance will be. Then run it."

For Parts 5-6, the current exercises are appropriately difficult because by that point the reader should have enough context. But even there, include hints or solution links for each exercise.

---

## V. The "Copy-Paste Must Work" Principle Needs Teeth

**Responding to: Guiding Principle #4 (line 14), Code Verification Infrastructure (lines 236–268)**

The plan says: "If it doesn't compile, it doesn't ship." It proposes CI that extracts and compiles code blocks. This is the right infrastructure.

**My critique:** CI catches compilation failures. It doesn't catch the nine other ways copy-paste fails for real beginners.

Here's what actually goes wrong when people copy-paste from tutorials. I know because I get the GitHub issues:

1. **Missing imports.** The code block shows the function but not the import at the top of the file. The reader pastes the function, gets `NotInScope`, and doesn't know why. The plan's Option B (code blocks as separate `.hs` files, line 266) helps with this if the files include imports. But the plan doesn't mandate that every code block be *complete and self-contained*, including imports.

2. **File path ambiguity.** The code block doesn't say which file it goes in. The reader has three files open and pastes it in the wrong one. The plan doesn't mention labeling code blocks with file paths.

3. **Missing context from the previous step.** Code block on page 3 assumes code from page 2 exists in the reader's project. But the reader skipped page 2's exercise, or did it differently. Now the code doesn't work and the error message is about something the reader didn't write.

4. **Invisible whitespace and formatting.** Copy-paste from web pages sometimes introduces invisible characters or weird indentation. In Haskell-family languages, where indentation is significant, this *will* cause errors the reader cannot diagnose.

5. **Platform differences.** The code works on the author's Mac but not on the reader's Windows machine because of path separators, line endings, or tool differences.

6. **Version mismatches.** The reader installed NeoHaskell 0.3 but the tutorial was written for 0.4 and a function signature changed.

**Why this is critical for a Haskell-family language specifically:**

Haskell is *notorious* for opaque error messages when something is wrong. Even with NeoHaskell's friendlier error messages (which, per Open Question #5, might not exist yet), a beginner encountering a type error because they pasted code into the wrong file is going to see something confusing. The friendliest error message in the world doesn't help if the *cause* is that the reader is in the wrong file.

**My specific recommendation:**

1. **Every code block must include a file path label.** Not `Account.hs`. A full relative path: `src/NeoBank/Account.hs`. This is what Stripe does (Laravel analysis, line 154: "Code blocks with copy buttons"). It's what every production docs site does.

2. **Every code block that modifies an existing file must show enough context** to locate the insertion point. Don't show 3 lines of new code — show 3 lines of existing code above, the new code, and 3 lines of existing code below. Use diff markers or highlighting to indicate what's new. This is how React's tutorial works.

3. **At minimum every other page, show the complete file.** Not just the new code — the entire file as it should look after this step. Put it in a collapsible section if it's long. This is the "source of truth" the reader can compare against when things go wrong.

4. **Add a "Common Copy-Paste Issues" section to the Getting Started page** that addresses platform-specific gotchas, editor configuration, and how to diagnose invisible character problems.

5. **Lock the tutorial to a specific NeoHaskell version** and say so prominently at the top: "This tutorial is written for NeoHaskell 0.x. If you're using a different version, some code may not work." Update the tutorial on major releases. Don't try to make it version-agnostic.

---

## VI. The Getting Started Section Is a Hallway, Not a Room

**Responding to: File Structure (lines 692–748), specifically `getting-started/`**

The plan's Getting Started section has four pages:
- `index.mdx` (overview)
- `installation.mdx` (Nix, tooling)
- `reading-neohaskell.mdx` (5-minute annotated walkthrough)
- `cheat-sheet.mdx` (quick reference)
- `first-events.mdx` (first event, store it, read it)

**My critique:** That's four pages before the reader reaches the tutorial. For a reader coming from "I want to try this" (the Developer persona), this is too much preamble. 

Think about what the FastAPI experience is: you land, you install, you write 5 lines, you run it, you see output. That's *one page.* The FastAPI "First Steps" page (fastapi.md, lines 76–89) goes from zero to running API in a single page load. The reader doesn't navigate away. They don't click "Next." They scroll down and the thing works.

The NeoHaskell Getting Started section asks the reader to:
1. Read an overview page (index.mdx)
2. Navigate to installation (installation.mdx) 
3. Navigate to reading-neohaskell (reading-neohaskell.mdx)
4. Navigate to a cheat sheet (cheat-sheet.mdx)
5. Navigate to first-events (first-events.mdx)

That's four page navigations before the reader has *done* anything. Each navigation is a drop-off point. Each one is a moment where the reader might decide "I'll come back to this later" and never come back.

**The specific problem with `reading-neohaskell.mdx` and `cheat-sheet.mdx`:**

Steve already noted (his Section XIII) that these pages partially contradict the "no grammar before the tutorial" principle, and he recommends adding a note that they're optional. I'd go further: **these pages don't need to exist as separate pages at all.** The annotated code walkthrough should be *embedded* in the installation page as a "Verify your install" section. The cheat sheet should be a standalone page in the reference section, not in Getting Started.

Here's why: the Getting Started section has one job — get the reader to a working, running example as fast as humanly possible. Everything that isn't installation or first working code is a detour. Detours belong elsewhere.

**My specific recommendation:**

Consolidate Getting Started to two pages:

1. **`getting-started/installation.mdx`** — Install NeoHaskell, run your first command, see "Hello NeoHaskell" output. Includes platform tabs (macOS / Linux / Windows), as the synthesis recommends. Ends with a "Verify" section that *is* the annotated code walkthrough — you run a small program and the page explains what each line does. One page, 10 minutes, done.

2. **`getting-started/first-events.mdx`** — Your first event. This IS Tutorial Part 1's warm-up. You define an event, store it, read it back. The page ends with "Ready to build something real? Let's build a bank →" linking to the tutorial.

Move the cheat sheet to `reference/cheat-sheet.mdx`. It's a reference document, not a Getting Started document. Link to it from the sidebar and from the tutorial for people who want a quick lookup.

This gives you: Install → First code → Tutorial. Three steps. Two page loads. Minimal drop-off surface.

---

## VII. The Concept Page Template Is Too Rigid for Real Content

**Responding to: Just-in-Time Concept Page Strategy — Concept Page Template (lines 393–418)**

Every concept page is supposed to follow:
1. The One-Sentence Version
2. In NeoBank Terms
3. The Full Picture
4. How NeoHaskell Enforces This
5. Going Deeper (optional)

**My critique:** This template assumes every concept needs five sections. Some don't.

"Immutability" needs two paragraphs and a code comparison. It doesn't need a "How NeoHaskell Enforces This" section because immutability is the default — there's nothing to enforce. It doesn't need "In NeoBank Terms" because immutability isn't specific to banking.

"Projections" might genuinely need all five sections because it's a deep, domain-specific concept with rich enforcement properties.

"Bounded Contexts" might need a *diagram* more than any prose section.

When I write tutorials, I never use templates for my content sections. Templates for *metadata* (title, description, prerequisites) are great. Templates for content structure produce formulaic writing that serves the template instead of the reader.

**What happens in practice with rigid templates:**

The writer encounters a concept that doesn't fit the template. They either:
1. Force-fit it, producing a section called "In NeoBank Terms" that feels contrived because the concept isn't really about banking, or
2. Skip sections, producing inconsistent pages that feel incomplete compared to pages that use all five sections.

Both outcomes are bad.

**My specific recommendation:**

Replace the five-section template with:
1. **Required metadata**: title, description, entry stage, exit stage, misconception addressed, tutorial anchor (these are all good — keep them)
2. **Required opening**: One sentence that shifts the mental model (the current "One-Sentence Version" — this is brilliant, keep it)
3. **Flexible body**: Write whatever structure serves this particular concept. Use NeoBank examples if they fit. Use a diagram if it's spatial. Use a code comparison if it's contrastive. Let the concept dictate the structure.
4. **Required closing**: "See Also" links to the tutorial, related concepts, and relevant guides.

This preserves the metadata consistency the plan wants while freeing writers to teach each concept in the way that concept demands.

---

## VIII. The Plan Underestimates How Scary Functional Programming Is to Its Target Audience

**Responding to: Audience Model — Developer (lines 25–31) and the plan's emotional tone throughout**

The Developer persona "comes from JavaScript/TypeScript, Python, Go" and "brings CRUD mental model, ORM habits."

**My critique:** The plan addresses the *intellectual* challenge of moving from CRUD to event sourcing but doesn't address the *emotional* challenge of encountering functional programming syntax for the first time.

I've taught JavaScript developers who panicked at arrow functions. I've seen Python developers freeze when they encounter TypeScript generics. The emotional response to unfamiliar syntax isn't "I don't understand this yet." It's "I'm not smart enough for this."

NeoHaskell code looks like this (from the plan's Rosetta Stone):
```
Module.yield
Result.ok / Result.err
case..of
Array.foldl
[fmt|Hello {name}!|]
```

To a JavaScript developer, `Array.foldl` is "that `reduce` thing I always have to Google." `case..of` looks like pattern matching they've never seen. `[fmt|Hello {name}!|]` is completely alien syntax. `Module.yield` isn't the `yield` they know from generators.

The plan doesn't have a strategy for managing the emotional impact of encountering this syntax. The three-layer system puts syntax explanations in a collapsible sidebar. But the emotional reaction happens *before the reader clicks the sidebar.* It happens the moment they see code that doesn't look like anything they've written before.

**How I handle this in my tutorials:**

When I teach React to someone who knows vanilla JavaScript, I start with a "This might look weird at first" moment. I show JSX — which looks like HTML inside JavaScript — and say: "This looks strange, but it's just JavaScript under the hood. You'll get used to it in about 15 minutes." That tiny acknowledgment — "this looks strange and that's okay" — prevents the reader from assuming they're too dumb for the material.

**My specific recommendation:**

1. **Add a "New to this kind of code?" callout to Tutorial Part 1.** Something like:
   
   > If NeoHaskell code looks unfamiliar, that's completely normal. If you're coming from JavaScript or Python, you'll notice some differences — no curly braces, no semicolons, types that look different. You'll get comfortable with the syntax within the first few pages. For now, just follow along and type it out. It'll click.

2. **Translate syntax to familiar concepts immediately, every time.** Don't just say what `|>` does — say what it *replaces:*
   
   > `deposit 100 |> processTransaction` — this is like `processTransaction(deposit(100))` in JavaScript, but written left to right so you can read it like a sentence.

3. **Show the JavaScript/Python equivalent in the first tutorial page.** Not on every page — just the first one. Let the reader see "this is what you'd write in JS, and here's the NeoHaskell version." Once. Early. It bridges the gap between "what I know" and "what I'm looking at."

4. **Use the Rosetta Stone table (lines 138–146) earlier than planned.** The plan says it "appears in Concepts, referenced from Tutorial Part 1." It should appear *in* Tutorial Part 1, prominently, before the first code block. "Here's a quick translation table you can reference as you go."

---

## IX. The "Coming From CRUD" Page Is More Important Than the Plan Realizes

**Responding to: "Coming From..." Section (lines 432–457)**

The plan lists six "Coming From" pages: JavaScript/TypeScript, Python, Go, Haskell, CRUD, and Event Sourcing. The CRUD page is one of six.

**My critique:** The CRUD page isn't one of six entries in a section. It's the *most important page on the entire site* for the target audience.

The Developer persona — the plan's primary audience — is defined as someone who "brings CRUD mental model" and "needs to unlearn mutation-first thinking." The entire NeoBank tutorial is predicated on moving the reader from CRUD thinking to event thinking. The "Coming From CRUD" page is where that transformation is *explicitly stated and visualized.*

But the plan gives it the same structural weight as "Coming From Go." It has the same three-section pattern as every other Coming From page:

```
## In [your language], you'd write...
## In NeoHaskell, the equivalent is...
## But here's what's actually different...
```

**This misses what the CRUD page needs to be.** The reader coming from CRUD isn't switching languages — they're switching *paradigms*. The JavaScript page shows syntax differences. The CRUD page shows a *worldview* difference. That requires a fundamentally different structure.

**My specific recommendation:**

Make the "Coming from CRUD" page a **first-class entry point**, not a subpage in the Coming From section. It should be linked from the homepage, from the Getting Started page, and from the top of Tutorial Part 1.

Structure it differently from the other Coming From pages:

```
## What you do today (show a typical CRUD operation)
## What's really happening (break down the problems with this approach)
## How NeoHaskell thinks about it differently (show the event-based version)
## You don't have to throw away what you know (reassurance)
## Ready to try it? → Tutorial Part 1
```

The key addition is section 4 — reassurance. The JavaScript developer switching from React to Vue knows they're switching tools. The CRUD developer switching to event sourcing thinks they're being told *everything they've ever built was wrong.* That's an emotional minefield. The page needs to say, explicitly: "CRUD works fine for many applications. Event sourcing is better for applications where history, auditability, and state derivation matter. You're not wrong for knowing CRUD. You're *expanding your toolkit.*"

This is something Steve's review doesn't touch, and it matters enormously. Steve writes for people who are already bought in to learning the new thing. I write for people who are *deciding whether to learn the new thing.* That decision is emotional as much as intellectual.

---

## X. The Disclaimers and Open Questions Reveal a Timing Problem

**Responding to: Open Questions (lines 783–792) and the Disclaimer (lines 156–160)**

The plan has 8 open questions, several of which are fundamental:

- "Is Nix the only installation path?" (line 783)
- "What's compilable today?" (line 785)
- "Does NeoHaskell have a Decimal/Money type?" (line 789)
- "Does NeoHaskell currently support sagas or process managers?" (line 790)

**My critique:** These aren't open questions. They're *blockers.* You cannot write Tutorial Part 1 until you know what's compilable today. You cannot write the installation page until you know the installation path. You cannot write a banking tutorial without knowing how the language handles money.

The plan presents these as "To Resolve During Phase 0," but Phase 0 also includes writing actual pages (reading-neohaskell.mdx, cheat-sheet.mdx). You can't write a code walkthrough if you don't know what code compiles.

**Why this matters for the beginner experience specifically:**

Beginners don't handle "this feature doesn't actually work yet" well. At all. If the tutorial says "deposit $100" and the reader discovers that decimal amounts aren't supported and they need to use integer cents, the tutorial has already lost them. If the tutorial says `neo new my-project` and the actual command is `nix develop` followed by something else, the tutorial is lying.

The FastAPI tutorial works because FastAPI is *finished software.* Every example in the tutorial runs exactly as shown. The Rust Book works because Rust is *stable.* The guessing game compiles on every version the book covers.

If NeoHaskell isn't at a point where a banking tutorial can be *honestly written* — where every code block actually compiles, every command actually runs, and every feature actually exists — then the plan is premature. This isn't a criticism of the plan's quality. It's a statement about sequencing.

**My specific recommendation:**

Before doing anything in this plan, answer the 8 open questions. All of them. Write them down as concrete facts:

- "NeoHaskell is installed via Nix. The command is `nix develop`."
- "The following features compile today: [list]."
- "Money is represented as `Int` (cents). There is no Decimal type."
- "Sagas are not supported yet. Tutorial Part 5 will use [alternative]."

Then scope the tutorial to *exactly what works today.* If that means the tutorial is 3 parts instead of 6, that's fine. A 3-part tutorial that works is infinitely better than a 6-part tutorial where parts 4-6 are aspirational.

The disclaimer on line 156 is honest and I appreciate it. But the disclaimer shouldn't be covering for features that don't exist. It should be scoping a tutorial that teaches real, working features.

---

## XI. The Plan's Relationship to Its Research Is Inverted

**Responding to: The plan's incorporation of synthesis findings, and "What We're NOT Doing (And Why)" (lines 753–766)**

Steve already noted the contradiction between "We're not analyzing 12 sites" and the existence of 12 site analyses. I want to make a different point.

The synthesis (SYNTHESIS.md) contains genuinely actionable recommendations. The FastAPI analysis (fastapi.md) identifies a specific pattern — "Recap, step by step" sections at the end of tutorial pages (fastapi.md, lines 249–258) — that directly addresses tutorial comprehension. The Laravel analysis (laravel.md) identifies the importance of multiple installation paths with platform-specific tabs (laravel.md, lines 33–38).

**My critique:** The plan cherry-picks from the research. It adopts the patterns that fit its existing design (compiler-as-teacher, progressive complexity, teaching through building) and quietly drops the ones that would require restructuring (recap sections, platform tabs, progress indicators, troubleshooting per page).

Here are the specific research findings the plan ignores or underweights:

1. **"Recap, step by step" sections** (from FastAPI). The plan has no recap mechanism. After a tutorial page, the reader moves to the next page. There's no "here's what you just did in 5 bullet points" moment. FastAPI uses these on *every* tutorial page and they are one of the most cited reasons beginners love FastAPI's docs. They serve a deep cognitive purpose: they let the reader verify their own understanding before moving on.

2. **"What you'll learn" upfront** (from Stripe, React, Astro, FastAPI — synthesis Pattern 16). Steve mentions this in his Section IX, but I want to emphasize *why* it matters for beginners: it gives the reader permission to skim. If I see "What you'll learn: define an event type, store it, derive a balance," I can scan the page for those three things. If the page is a wall of prose with no preview, I have to read linearly or risk missing something. Beginners are already anxious. Giving them a map reduces anxiety.

3. **Progress tracking** (from Astro, Svelte — synthesis lines 463-465). The plan has no visual progress indicator. For a 6-part tutorial, knowing "you're on Part 3 of 6" is not decoration — it's motivation. "I'm halfway there" keeps readers going. "I don't know how much more there is" causes readers to stop.

4. **Estimated reading/completion times** (from Stripe, FastAPI). The plan says "Part 1 must produce visible output within 10 minutes" (line 153) but doesn't expose this estimate to the reader. Saying "Part 1: ~10 minutes" at the top of the page sets expectations and prevents the "how long is this going to take?" anxiety.

**My specific recommendation:**

Do a systematic pass through the synthesis's "Must Adopt" recommendations (SYNTHESIS.md, lines 819-856) and for each one, document in the plan: "We're adopting this as [specific implementation]" or "We're deferring this because [reason]." Currently the plan absorbs research findings implicitly and inconsistently. Making the adoption decisions explicit ensures nothing important falls through the cracks.

At minimum, add to the tutorial page template:
- "What you'll build / What you'll learn" header
- "Time: ~N minutes" estimate
- "Recap" section at the end of each page
- Progress indicator in the sidebar or page header

---

## XII. Agreement with Steve, and Where I'd Push Further

Steve raises 14 points. I agree with most of them. Here's where I want to amplify or modify his recommendations:

### Drop Layer 2 — I agree, and I'd go even simpler

Steve recommends inline parenthetical explanations for new syntax and a standalone syntax reference page. I agree. But I'd add: the first three occurrences of any new syntax construct should include a brief parenthetical, not just the first. Beginners need repetition to build recognition. By the fourth time they see `|>`, they know what it does. The first time, they don't, even if you told them.

### Bloom's Taxonomy — I agree, and I'd delete the table entirely

Steve says demote it to a validation tool. I'd say delete the table from the plan document. It's academic scaffolding that will confuse anyone who joins the project later and sees it in the plan. "Are my exercises progressing in difficulty?" is a question any writer can answer intuitively. You don't need a taxonomy to answer it.

### Coming from Haskell restructuring — I agree

Steve's proposed ordering (What Stayed the Same → Opening → Rosetta Stone → Rationale → AI mistakes) is correct and well-argued. I'd add: the "What Stayed the Same" section should include *runnable code examples*, not just a bullet list. "Here's a function that works identically in Haskell and NeoHaskell. Try it." Proving similarity is more powerful than claiming it.

### Visual aids — I agree, and this is urgent

Steve calls the absence of diagrams a "significant omission." I'd escalate that to "critical omission." Event sourcing is an inherently spatial concept. The command → event → store → projection flow is a *pipeline*, and pipelines need to be drawn, not described. Create one SVG diagram. Put it on Tutorial Part 1. Reference it everywhere. This is non-negotiable for a beginner audience.

### The Alex character — I partially disagree with Steve

Steve recommends letting Alex fade after Part 1, switching to second person ("you"). I think Alex should never be introduced as a *named character* in the first place. Just use "you" from the beginning. "You're building a banking service. Your first feature: accept deposits." 

Named characters create distance. "Alex wants to..." is about someone else. "You want to..." is about the reader. Every tutorial I've written uses "you" because the reader is the protagonist, not a character called Alex.

The product requirements framing ("Alex wants to deposit money") can be preserved without the character: "The requirement: accept deposits into an account."

### The Part 4 → Part 5 gap — I agree, and I'd go further

Steve identifies the cognitive jump between single-aggregate and cross-aggregate operations. His recommendation (split Part 5 or add a concept interlude) is correct. I'd add: before cross-account transfers, show an *in-account* operation that prepares the reader for multiple operations on one aggregate. Something like "apply interest to all accounts" — a batch operation that still stays within one aggregate but introduces the idea of operating across data. This is a stepping stone.

---

## XIII. What I'd Cut

If I were advising this project, here's what I'd cut from the plan to make it more actionable:

1. **The 5-stage Mental Model Journey chart.** It's intellectually interesting but it doesn't help anyone write a page. Replace it with a simpler statement: "By the end of the tutorial, the reader should think in events instead of mutations."

2. **The 20 Misconceptions validation protocol.** Keep the misconceptions list — it's excellent. Cut the formal validation protocol with surveys and interviews. You'll discover which misconceptions are real by writing the tutorial and watching people use it. The formality will delay you.

3. **The Layer Interaction Map table (lines 223-231).** It duplicates information already in the tutorial progression table.

4. **The Exercise Strategy's Bloom's taxonomy table.** Already discussed.

5. **The Code Verification Infrastructure section's three options.** Just pick Option B (code blocks as separate files) and move on. The decision doesn't need three options and a recommendation — it needs a decision.

6. **Phase 0's "Study reference docs" task.** You've already studied them. The 12 research documents prove it.

7. **The Iteration Protocol's formal cadence.** "After Every Page," "After Every Phase," "Monthly" — these are ideal-state processes that no small team will actually follow. Replace with: "After writing each page, test it with one person. Fix what they struggled with."

8. **The Interactive Elements section's Phase 2 and 3.** They're aspirational and they distract from Phase 1, which is all that matters right now. Put them in a "Future Ideas" document and remove them from the plan.

---

## XIV. What I'd Add

1. **A "Getting Unstuck" page** in the Getting Started section. Links to the Discord, to the common errors page, to the tutorial companion repo, and to this sentence: "If you're confused, it's not you — it's the docs. File an issue and we'll fix it."

2. **A "Your NeoBank at Each Stage" page** — a visual gallery showing what the reader's project looks like at the end of each tutorial part. Screenshots of output, file structure diagrams, event stream examples. This is the map. It shows the reader where they are and where they're going.

3. **A "Before You Start" prerequisites section** on Tutorial Part 1 — not required reading, but an honest statement: "You should be comfortable with a terminal and a text editor. Experience with any programming language helps. No functional programming experience required."

4. **Estimated reading times on every page.** "~10 minutes" or "~20 minutes." Simple, helpful, universally appreciated.

5. **A "What You Built" section at the end of the tutorial** — a celebration moment. "You built a banking service with accounts, transactions, transfers, and a complete audit trail. Here's everything your NeoBank can do: [list]. You did this in ~200 lines of NeoHaskell."

---

## Summary of Recommendations (Ranked by Impact on Newcomers)

1. **Resolve the open questions before writing anything.** The tutorial can only teach what actually works today.
2. **Add "Stuck?" escape hatches** — complete code for each section, tutorial companion repo with tagged branches.
3. **Add a syntax warm-up** before Tutorial Part 1 — hands-on NeoHaskell without event sourcing.
4. **Consolidate Getting Started to 2 pages** — Installation and First Events. Move cheat sheet to Reference.
5. **Gut Phase 0 to three tasks** — CI, installation page, Tutorial Part 1. Everything else follows.
6. **Add "What you'll learn" headers, time estimates, and recap sections** to every tutorial page.
7. **Add one event-flow SVG diagram** in Tutorial Part 1, referenced throughout.
8. **Elevate "Coming from CRUD"** to a first-class entry point, not a subpage.
9. **Acknowledge the emotional reality** — NeoHaskell syntax looks unfamiliar, and that's okay.
10. **Restructure exercises** — Level 1 (change a value), Level 2 (repeat the pattern), Level 3 (extend, optional).
11. **Every code block gets a file path label and enough context** for the reader to know where it goes.
12. **Drop Layer 2, Bloom's table, Layer Interaction Map** — unnecessary planning infrastructure.
13. **Make concept page template flexible** — required metadata and opening, flexible body.
14. **Use "you" instead of "Alex"** from the beginning.
15. **Cut aspirational features** (interactive phases, formal iteration protocols) from the plan.

---

## Closing

Here's the thing about tutorial writing that's hard to learn from studying other tutorials: **the reader is always more confused than you think they are.** You, the writer, know the material. You've internalized it so deeply that you've forgotten what it felt like to not know it. Every assumption you make about what's "obvious" is a place where a reader gets lost.

This plan is written by someone who deeply understands NeoHaskell and event sourcing. That depth of understanding is both the plan's greatest strength (the domain choice, the mental model journey, the misconceptions list) and its greatest blind spot (the assumption that readers can absorb syntax and architecture simultaneously, the absence of error recovery strategies, the exercises pitched at the teacher's level instead of the student's).

The plan asks: "How do we teach event sourcing?" The better question is: "How do we make someone who's never seen functional programming feel confident enough to keep going after the first page?"

Answer that question, and the rest follows.

— Tania Rascia