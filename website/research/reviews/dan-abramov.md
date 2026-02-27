# Review of the NeoHaskell Documentation Plan

**Reviewer: Dan Abramov**
**Date: February 2026**

---

## Preface

I've read the full DOCS-PLAN.md, the synthesis of 12 documentation sites, the analysis of react.dev (which is a strange experience — reading someone else's analysis of decisions you remember making at 2 AM), and Steve Klabnik's review.

Steve's review is excellent. He's right about the layer system being overengineered, about Bloom's taxonomy being the wrong design constraint, about the visual aids gap, about checkpoint formalization, and about the Part 4→5 pacing cliff. I won't relitigate those points. Where I agree with Steve, I'll say so briefly and move on. Where I disagree or have a different angle, I'll explain why.

What I want to focus on is something Steve doesn't address much, because his expertise is in a different place: **how people form mental models, where those models go wrong, and what documentation can do about it.** The react.dev rewrite was, at its core, a project about fixing wrong mental models — models that had calcified over five years of class component documentation. That experience is directly relevant to NeoHaskell's challenge: getting CRUD developers to think in events instead of mutations.

I'm going to be direct about what I think this plan gets deeply right, and where it has gaps that will create the wrong kind of understanding — the kind where people can follow the tutorial but can't apply the ideas to their own problems.

---

## I. The Plan Gets the Central Insight Right — But Doesn't Follow Through on Its Hardest Implication

**Responding to: "The Mental Model Journey" (lines 49–66) and Guiding Principle #2**

The plan says: "The mental model IS the documentation." And then it defines a five-stage journey from "I store data in rows and update them" to "I can model any domain this way." This is correct. This is actually the hardest thing to get right in documentation, and most projects never even attempt it.

But here's what the plan misses: **mental model shifts don't happen because you showed someone new code. They happen because you showed someone what's wrong with their OLD model.**

When we rewrote react.dev, the single most impactful thing we did was not teaching Hooks. It was *explaining why the class component model was broken.* The page "State as a Snapshot" doesn't start by teaching you how `useState` works. It starts by showing you code where `setState` seems to "not work" — where you call it and then `console.log` the old value. The reader says "that's a bug." We say "no, that's how React actually works. Your mental model was wrong. Here's the correct model." The moment of productive confusion — "wait, my understanding was wrong?" — is where real learning happens.

The NeoHaskell plan has the stages right but doesn't identify the **productive confusion moments** that drive transitions between stages. The transition from Stage 0 ("I store data in rows") to Stage 1 ("Banks don't update balances") isn't triggered by showing someone event sourcing. It's triggered by showing someone a CRUD system that has a real, painful bug that event sourcing prevents.

**What's missing from the plan:** The "Key misconception addressed" metadata on each page (line 65) is a good idea, but misconceptions aren't addressed by stating the truth. They're addressed by making the reader *experience* the misconception's consequences. The plan's Compiler-as-Teacher pattern does this for NeoHaskell-specific conventions (using `$` instead of `|>`), but it doesn't do it for the central paradigm shift: CRUD → event sourcing.

**My specific recommendation:**

Before the tutorial teaches event sourcing, it needs a **"Why Not CRUD?"** moment — ideally in the first five minutes. Here's what I mean concretely:

Tutorial Part 1 should not begin with "let's create an event type." It should begin with something like:

> Here's a simple bank account. It stores a balance.
> ```
> type Account = { balance : Int }
> deposit amount account = account { balance = account.balance + amount }
> ```
> This works. Deposit $100, balance is $100. Deposit $50 more, balance is $150.
>
> Now answer this: **Someone deposited $100, then $50, then disputed the $50 deposit. What was their balance before the dispute?**
>
> You can't answer that. The balance is $150, but the *history* is gone. You overwrote it.

THEN you introduce events. The reader doesn't just see a new pattern — they see *why the old pattern fails.* The transition from Stage 0 to Stage 1 happens not because you told them about events, but because you made them feel the pain of mutation.

This is directly analogous to what we did on react.dev with "State as a Snapshot." We don't tell people "state is a snapshot." We show them code where treating state as mutable leads to a bug, let them feel the confusion, and then explain the model that makes the bug make sense. The "aha!" comes from the resolution of confusion, not from the presentation of new information.

The plan's opening line ("Every bank in the world runs on the same idea: don't change the number, record what happened") is great prose. But it's an *assertion*, not an *experience.* The reader nods along without internalizing it. To internalize it, they need to try the CRUD way first and see it fail.

---

## II. The Concept Page Template Is Missing the Most Important Section

**Responding to: "Concept Page Template" (lines 393–418)**

The template is:

1. The One-Sentence Version
2. In NeoBank Terms
3. The Full Picture
4. How NeoHaskell Enforces This
5. Going Deeper (optional)

**My critique:** This template produces *explanation* pages, not *understanding* pages. The difference matters enormously.

When we designed concept pages for react.dev, we discovered that the single most effective teaching structure wasn't "here's the concept, here's how it works, here's an example." It was:

1. **Here's what you probably think** (name the wrong model)
2. **Here's where that breaks** (show the failure)
3. **Here's the correct model** (the "aha!")
4. **Here's how to think about it** (the generalization)
5. **Challenges** (can you apply this model to a new situation?)

Our concept pages don't start with "the one-sentence version." They start with "you might think X, but actually Y." The page on "Responding to Events" doesn't open with "event handlers are functions." It opens with "in React, you don't attach listeners directly to the DOM." It names the wrong assumption, corrects it, and then teaches the right way.

The NeoHaskell plan's template is missing step 1 — "Here's what you probably think." The `misconception` field in the frontmatter metadata identifies which misconception the page addresses, but the template doesn't say *where in the page* the misconception gets surfaced and corrected. A writer following this template will write "Events are immutable facts" (The One-Sentence Version) without first writing "You might think you can just update the account balance directly" (the misconception).

This isn't a minor structural complaint. It's the difference between a page that *informs* and a page that *transforms*. Information pages produce readers who can recite definitions. Transformation pages produce readers who think differently.

**My specific recommendation:**

Revise the concept page template to:

```markdown
## What You Might Expect
[Name the CRUD/mutation assumption the reader likely holds. 
Use code if possible — show the "obvious" way that's wrong.]

## Why That Breaks
[Show the specific failure mode. Not abstract — concrete. 
"Try to answer this question about your data. You can't."]

## The Mental Model Shift  
[Now introduce the correct concept. This is where "The One-Sentence Version" goes, 
but it lands differently because the reader already felt the problem.]

## In NeoBank Terms
[Anchor to the banking domain]

## The Full Picture
[Complete explanation with code]

## How NeoHaskell Enforces This
[Compiler/linter support]

## Challenges
[Can the reader apply this model to a NEW situation? Not just replicate — apply.]
```

The addition of "What You Might Expect" and "Why That Breaks" at the top is what turns a reference page into a teaching page. It's also what makes the misconceptions list (lines 69–95) actually *do work* instead of just existing as metadata.

Steve suggested adding a "Visual" section to the template, and I agree with that — add it between "The Mental Model Shift" and "In NeoBank Terms." But the structural problem is more fundamental than missing diagrams. The template needs to be designed for cognitive change, not information delivery.

---

## III. The Tutorial Creates Procedural Following, Not Genuine Understanding — And the Plan Doesn't Have a Mechanism to Prevent This

**Responding to: "The Progression" (lines 128–136) and "Exercise Strategy" (lines 491–523)**

This is the thing that keeps me up at night about tutorials. When we launched the react.dev tic-tac-toe tutorial, we watched people complete it successfully, feel proud, and then be completely unable to build a simple counter component on their own. They had *followed* the tutorial. They had not *understood* the tutorial. Their fingers knew what to type. Their brain hadn't formed the model.

The NeoHaskell plan has this same risk, and it's amplified by the "implicit-first" strategy for teaching the language.

Here's the problem concretely. The plan says each tutorial part starts with "Alex's goal" (a product requirement). The reader builds toward that goal by writing code the tutorial dictates. At the end, they have working code. But at no point does the plan specify a moment where the reader has to **make a decision without the tutorial telling them what to decide.**

Look at the exercises:

- Part 1: "Change the deposit amount to $200. Run it. What's the new balance?" — This is a parameter change. Zero decisions.
- Part 2: "Add a `MinimumBalance` rule" — This is an extension, but the exercise tells you what to build. The reader doesn't decide what to build.
- Part 3: "Create a `MonthlyStatement` projection" — Same pattern. The exercise is "build this specific thing."

None of these exercises ask the reader to make a **design decision.** They all say "build X" and the reader figures out how. But the real skill of event sourcing isn't "how do I build X?" It's "should this be an event or a command? Should this be a projection or an aggregate? What are the events in this domain?"

When we rewrote react.dev, we added what I call **"think before you code" challenges.** These are inline challenges where the reader has to predict what code will do, or decide how to structure something, *before* seeing the answer. The key is: the challenge comes before the code, not after.

Our "Responding to Events" page has a challenge: "Which of these event handlers would you attach to a button?" with four options — some that work, some that don't. The reader has to think. If they get it wrong, that's the learning moment. They discover their mental model has a gap. We don't just show them the right answer afterward — we explain WHY the wrong answers don't work.

The NeoHaskell plan has "Predict" as an exercise type (line 498: "What will this code output? Think first, then run it"). But it's listed as an option, not a structural requirement. And it appears *after* the tutorial has shown the code, not before.

**My specific recommendation:**

Add a formal "Think First" challenge to every tutorial part, placed **before** the reader writes the implementation. The structure is:

1. State the problem (Alex's goal for this part)
2. **Challenge: "How would you model this?"** — Ask the reader to sketch the events, commands, or types they think they'll need. No code — just a list. "What events do you think an overdraft protection system needs?"
3. Show the actual implementation
4. **Debrief: "How did your model compare?"** — Discuss why the actual events differ from what the reader might have expected. This is where the mental model gets refined.

This is different from exercises that come after the tutorial section. Those test retention. "Think First" challenges test *understanding of the model.* The reader who can predict "I'll need a `WithdrawalDeclined` event" before seeing the implementation has internalized the event-sourcing model. The reader who can't has been following along without thinking.

On react.dev, these inline challenges were the single highest-value addition to the concept pages. Reader feedback consistently said "the challenges made me realize I didn't actually understand until I tried to think it through." That's the signal you want.

---

## IV. The "Implicit-First" Strategy Is Correct — But Has a Failure Mode the Plan Doesn't Address

**Responding to: Guiding Principle #7 and the entire Tutorial Layer System**

I agree strongly with implicit-first teaching. When we introduced Hooks on react.dev, we didn't start with "here's the hooks API." We started with "here's how to build a counter" and the reader encountered `useState` in context. The concept page for `useState` existed separately. The tutorial just *used* it.

The NeoHaskell plan does this for both the language syntax AND the event-sourcing concepts. That's doubly implicit — two unfamiliar things taught through doing. This is ambitious and, if it works, extremely effective. But it has a failure mode that we hit with Hooks and that the plan should prepare for.

**The failure mode: readers form wrong intermediate models that are never corrected.**

When we taught Hooks implicitly through the tic-tac-toe tutorial, many readers formed the model "useState is like this.state but shorter." That's wrong — state in React is a snapshot, not a mutable container — but the tutorial never forced them to confront that distinction because the tutorial's exercises didn't expose it. The reader completed the tutorial with a wrong model that happened to produce correct code for the specific tutorial problem.

We only fixed this by adding the "State as a Snapshot" concept page and making it part of the recommended learning path. The concept page explicitly names the wrong model and breaks it.

NeoHaskell faces the same risk, but doubled:

1. **Language syntax wrong model:** A JavaScript developer might think `|>` is just a different way to write function calls (like optional chaining). It is, but only technically. The deeper model is data transformation as a pipeline. If the tutorial doesn't surface this, the reader will use `|>` without understanding the compositional philosophy.

2. **Event sourcing wrong model:** A CRUD developer might think events are "just logging." They write `MoneyDeposited` and think it's like `console.log("deposited $100")` — a record of what happened, but not the *source of truth.* This is the most dangerous wrong model because it produces working code that breaks in subtle ways later. The reader builds NeoBank, thinks "events are logs," and then in their own project skips the event store because "I can just use a database and add logging."

Steve's recommendation to allow foundational concept pages before the tutorial needs them (his Section III) addresses part of this. But concept pages are passive. The reader has to choose to read them. What you need is an **active disambiguation moment** IN the tutorial.

**My specific recommendation:**

At the end of Tutorial Part 1, after the reader has their first event stored and their balance derived, add a **disambiguation exercise.** Something like:

> **Challenge: Which of these is true?**
>
> A) The event is a log entry. The balance is stored separately.
> B) The event IS the data. The balance is calculated from events.
> C) The event triggers a database update. The balance is in a table.
>
> Think about your answer before clicking.
>
> **Answer: B.** This is the key insight. There's no `balance` column anywhere. There's no separate data store. The events are all you have. The balance is calculated every time by replaying events. If you chose A or C, that's the CRUD model talking. Let's make this concrete:
>
> `[Show: delete all events, balance goes to 0. Restore events, balance comes back. There was never a separate "balance."]`

This is a five-minute exercise, but it catches the #1 wrong intermediate model (events-as-logging) at the earliest possible moment. Without it, you'll have readers who complete all six tutorial parts and still think of events as a logging layer on top of traditional state.

On react.dev, the analogous moment is the "what does this log?" challenge in the State page: you set state, log the variable, and the log shows the *old* value. That breaks the "setState mutates" model immediately. NeoHaskell needs an equivalent "no, really, there is no other data" moment.

---

## V. Where the Plan Risks Creating Cargo Cult Event Sourcing

**Responding to: The tutorial progression as a whole, and the exercise types**

"Cargo cult" is my biggest fear for this documentation. A reader who completes the tutorial and builds event-sourced systems by imitating the NeoBank patterns *without understanding when and why* to apply them is a cargo cult practitioner. They'll use event sourcing for a blog because that's the only pattern they know. They'll create events for every field change because "that's what NeoBank did." They'll build projections that mirror their database tables because they never understood that projections are for *different views* of the same data.

The plan is aware of this risk — it lists "I can model any domain this way — not just banking" as Stage 5 in the Mental Model Journey. And the second example domain (logistics/shipment tracking) in Phase 4 is a good step toward proving generality. But Phase 4 is too late. By Phase 4, the reader's patterns are already set.

The specific cargo cult risk I see:

**1. Every state change becomes an event, even when it shouldn't.**

The tutorial teaches: deposit → `MoneyDeposited` event. Withdrawal → `MoneyWithdrawn` event. Account creation → `AccountOpened` event. The reader's pattern becomes: "every action gets an event." But not every state change in a real system should be an event. Changing a user's email notification preferences probably shouldn't be event-sourced. The tutorial never shows the reader a case where an event is NOT the right answer.

**2. Projections are only used for "current state," not alternative views.**

The tutorial's projection (Part 3: Transaction History) derives a bank statement from events. This is the most obvious projection — essentially reconstructing the current state in a display format. But the power of projections is that you can derive *different views* from the same events: a fraud detection projection, a monthly summary projection, a customer segmentation projection. The tutorial only shows one projection from one event stream. The reader models projections as "how you read state" rather than "how you create multiple interpretations of history."

**3. The bounded context concept is undertaught.**

Part 4 introduces "Multiple Accounts" and the plan maps this to bounded contexts. But two accounts of different types in the same system are not really different bounded contexts — they're different aggregates in the same context. The plan's concept page schedule lists `bounded-contexts.mdx` for Part 4, but the tutorial example doesn't actually demonstrate a bounded context boundary (where one context explicitly does NOT have access to another context's events). The logistics example in Phase 4 would demonstrate this, but again, Phase 4 is too late.

**My specific recommendation:**

**For risk #1:** Add a "When NOT to Event-Source" section to the concepts area, linked from Tutorial Part 3 (after the reader has enough context to appreciate the distinction). Show a concrete example: "Should 'user updated their email notification preferences' be an event? It depends. Here's how to decide." This teaches the *judgment* of event sourcing, not just the *mechanics.*

**For risk #2:** In Tutorial Part 3, don't just build one projection. Build two different projections from the same event stream: a balance-over-time projection AND a "largest transactions" projection. The reader sees the same events producing different views. This is a five-minute addition that shifts the mental model from "projection = read state" to "projection = interpret history." This mirrors what we did on react.dev with rendering — we don't just show one component rendering. We show the SAME data rendered differently by different components. That's what makes the "UI as a function of state" model click.

**For risk #3:** Either introduce a genuine bounded context boundary in Part 4 (not just different account types, but a genuinely separate domain — e.g., a simple notification system that reacts to account events but has its own events and cannot modify accounts) or be honest that bounded contexts are an advanced topic and move them to an advanced guide. Half-teaching bounded contexts is worse than not teaching them, because the reader forms a weak model ("bounded context = different entity type") that will actively mislead them later.

---

## VI. The Interactive Elements Strategy Is Backwards

**Responding to: "Interactive Elements (Future, But Design For Them Now)" (lines 567–583)**

The plan puts interactive elements in three phases:
- Phase 1: Copy buttons, syntax highlighting, search (Starlight defaults)
- Phase 2: "Run this code" buttons, expandable sections, side-by-side comparisons, interactive timeline
- Phase 3: Embedded playground

**My critique:** This treats interactivity as a nice-to-have enhancement. In my experience with react.dev, interactivity is not an enhancement. It's the teaching mechanism.

The single most impactful feature of react.dev is not the writing quality. It's Sandpack — the inline runnable code editor on every page. Before Sandpack, we had the same concepts, the same explanations, the same structure. But readers didn't *engage* with the code. They read it. Reading code and running code are fundamentally different learning activities.

Here's what we learned: when a reader can modify code and see the result immediately, they naturally start experimenting. They change a value, see what happens. They break something, see the error. They fix it, feel accomplishment. This experimentation loop is where the deep learning happens — not from reading the prose, but from poking at the system and building intuitions.

The NeoHaskell plan puts this in Phase 3 ("ambitious"). But the plan also identifies "10 minutes to first deposit" as a success metric (line 771). How does a reader deposit anything in 10 minutes if they first have to install Nix, clone a repo, set up a development environment, and run a compiler? Those 10 minutes get eaten by tooling before any learning happens.

The plan says (line 583): "Structure the MDX so that code blocks tagged with `interactive` can be wired up later without rewriting content." That's good forward planning. But "later" means the first readers — the most important readers, the ones who form community opinions — get the non-interactive version.

Now, I understand the constraint. NeoHaskell compiles to native code. You can't run it in the browser easily. But there are intermediate options the plan doesn't explore.

**My specific recommendation:**

Promote interactive elements from "Phase 3 ambitious" to "Phase 1 minimum viable." Not a full embedded playground — that IS ambitious. But:

**1. Predict-and-reveal challenges.** These require zero runtime. Show code, ask "what do you think this outputs?", hide the answer behind a click. This is the most effective interactive element on react.dev and it requires only HTML. No compiler, no runtime, no playground. Just a `<details>` tag.

```markdown
**Challenge:** What's the balance after these three events?
```haskell
events = [MoneyDeposited 100, MoneyWithdrawn 30, MoneyDeposited 50]
```

<details>
<summary>Think first, then click to see the answer</summary>

The balance is $120. Each event is applied in order: 0 + 100 - 30 + 50 = 120.

</details>
```

**2. Side-by-side CRUD vs. Event Sourced comparisons.** These are static content with a toggle. Show "CRUD version" and "Event Sourced version" of the same operation. The toggle makes the comparison visceral — the reader can flip back and forth and see exactly what changed. Starlight supports tabs. Use them.

**3. If at all possible, a hosted REPL.** Not embedded in the page — a link to a hosted service with the tutorial code pre-loaded. "Click here to try this in the NeoHaskell Playground." Even if the playground is just a remote server running `neo repl`, it removes the installation barrier. The react.dev research analysis (line 211) notes "If NeoHaskell can run in the browser, use inline editors. If not, link to a hosted REPL with pre-loaded NeoBank examples." I second this strongly.

Phase 3's embedded playground is a long-term goal. But predict-and-reveal, side-by-side tabs, and a hosted REPL are Phase 1 achievable and they account for 70% of the pedagogical value of full interactivity.

---

## VII. The Rosetta Stone Is Good but Teaches Translation, Not Thinking

**Responding to: "The Rosetta Stone" (lines 137–146)**

| What Alex says | Banking term | NeoHaskell term | What it actually is |
|---|---|---|---|
| "Something happened" | Transaction | Event | An immutable record of a fact |
| "Do something" | Deposit slip | Command | A request that may be accepted or rejected |

This is a useful reference table. Steve is right that it should also be a diagram. But I want to raise a different concern: **translation tables help people translate, but they don't help people think natively.**

When we teach React, we don't provide a "jQuery → React Rosetta Stone." We don't say "jQuery's `$('.button').click(fn)` is React's `onClick={fn}`." That would teach people to write React code by translating from jQuery. They'd think in jQuery and write in React. That's not what we want. We want them to think in React.

The Rosetta Stone table encourages readers to think: "When I want to update a record (CRUD), I should create an event (NeoHaskell)." That's translation. What you want is for them to think: "What happened in the domain? That's the event." The starting point should be the domain, not the CRUD operation they would have written.

The banking domain is particularly susceptible to this because banking *actually works* via events (ledgers). So the Rosetta Stone's banking column accidentally provides the right mental model. But when the reader moves to their own domain — say, an inventory management system — they won't have a banking column to guide them. They'll fall back to the CRUD column: "I want to update stock quantity → I should create a `StockQuantityUpdated` event." That's CRUD with events, not event sourcing. The event should be `ItemsSold` or `ShipmentReceived`, not `StockQuantityUpdated`.

**My specific recommendation:**

Keep the Rosetta Stone table for reference. But add a conceptual warning, either in the table's introduction or in the concept page it links to:

> **Common mistake:** Don't name your events after the CRUD operation you would have done. `BalanceUpdated` is a CRUD event wearing an event-sourcing costume. `MoneyDeposited` is a domain event — it describes what happened in the business, not what happened in the database.
>
> Good events describe **what happened in the world.** Bad events describe **what you want to change in the system.**

And reinforce this in the Tutorial Part 4 exercise. When the reader adds a savings account, don't just have them create a `SavingsAccountOpened` event. Have them first consider what the wrong event would be (`AccountTypeChanged`? `AccountUpdated`?) and explain why it's wrong. This teaches the *thinking* behind event naming, not just the naming convention for bank accounts.

On react.dev, we do this with component design. We don't just show good components. We show components that work but are poorly designed, explain WHY they're poorly designed, and then show the better version. The contrast is where the thinking skill develops.

---

## VIII. The "Coming From CRUD" Page Is the Most Important Page in the Entire Documentation and the Plan Treats It as One of Six Migration Pages

**Responding to: "'Coming From...' Section" (lines 432–457)**

The plan lists six "Coming From..." pages: JavaScript/TypeScript, Python, Go, Haskell, CRUD, and Event Sourcing. They're presented as equally weighted translation layers.

**My critique:** "Coming From CRUD" is not a migration page. It's the core teaching document for the primary audience.

Look at the audience model (lines 22–31). The Developer audience "Brings: CRUD mental model, ORM habits, REST/GraphQL assumptions." That's not one segment — that's nearly every developer who will ever encounter NeoHaskell. The JavaScript developer coming to NeoHaskell is a CRUD developer. The Python developer is a CRUD developer. The Go developer is a CRUD developer. The only audience members who AREN'T CRUD developers are the Haskell audience (who need the Haskell page) and the event sourcing audience (who need the event sourcing page).

Putting "Coming From CRUD" in the same sidebar section as "Coming From Python" dramatically undersells its importance. It's like if react.dev had put "Thinking in React" (our most important page) inside a "Coming From..." section alongside "Coming from Angular" and "Coming from Vue."

The plan's Rosetta Stone translation for CRUD is:

> `UPDATE balance` → record `MoneyDeposited`; `SELECT balance` → fold over transactions; REST endpoint → command handler

This is two lines. The mental model shift from CRUD to event sourcing deserves an entire standalone page — or even a small section — not a two-line summary in a table.

**My specific recommendation:**

Elevate "Coming From CRUD" to a standalone page in the Core Concepts section. Rename it something like "From CRUD to Events" or "Why Not Just Update the Record?" Make it a first-class concept page, not a migration guide. It should be the page that the Quick Start links to when a reader says "wait, why can't I just update the balance?"

Structure it using the revised concept page template I proposed in Section II:

1. **What You Might Expect:** "Here's how you'd build a bank account in a CRUD system. You'd have an `accounts` table with a `balance` column. Deposits UPDATE the balance. Withdrawals UPDATE the balance."
2. **Why That Breaks:** "Now try to answer these questions about your system: What was the balance at 3pm yesterday? Was this withdrawal legitimate? How many deposits happened last month? You can't answer any of them. The history is gone."
3. **The Mental Model Shift:** "Event sourcing keeps the history. Every deposit, every withdrawal, every transfer is a permanent, immutable record. Your balance isn't stored — it's calculated."
4. **Side-by-Side Code:** Full CRUD implementation vs. full event-sourced implementation of the same feature. Let the reader see both and compare.
5. **When CRUD Is Fine:** Honesty about when event sourcing is unnecessary (settings pages, static content, simple lookups). This prevents cargo cult adoption.

This page should be linked from the first tutorial part, from the Quick Start, and from every "Coming From [Language]" page. It's the gravitational center of the conceptual documentation.

---

## IX. The Plan Doesn't Teach Readers How to Design Events — It Only Shows Them Pre-Designed Events

**Responding to: The tutorial progression and the concept page schedule**

Every tutorial part gives the reader pre-designed events: `AccountOpened`, `MoneyDeposited`, `MoneyWithdrawn`, `WithdrawalDeclined`, `TransferInitiated`, `TransferCompleted`. The reader types these event types, but they never design an event type from scratch.

This is the tutorial equivalent of teaching someone to cook by having them follow a recipe but never explaining how to choose ingredients. The reader can reproduce NeoBank. They cannot design their own event-sourced system.

Event design is arguably the hardest part of event sourcing. It requires asking: What are the meaningful things that happen in this domain? What's the difference between a command and an event? What granularity should events have? Should `AccountOpened` include the opening deposit, or should that be a separate `MoneyDeposited` event?

The plan's concept page for "commands-and-handlers.mdx" (scheduled for Tutorial Part 1) could teach this, but the concept page template doesn't include a "how to design" section — it's focused on explanation, not skill building.

On react.dev, we don't just show people how to use `useState`. We teach them how to *decide* what should be state and what shouldn't. The "Choosing the State Structure" page has five principles: group related state, avoid contradictions, avoid redundancy, avoid duplication, avoid deeply nested state. These aren't about `useState` syntax. They're about *design judgment.* They're the knowledge that separates someone who can follow a tutorial from someone who can build their own application.

**My specific recommendation:**

Add a concept page (or a substantial section of the commands-and-handlers page) titled "Designing Your Events" that teaches the judgment of event sourcing:

1. **Events describe what happened, not what changed.** `MoneyDeposited` not `BalanceUpdated`. `ItemShipped` not `InventoryDecremented`.
2. **Events should be past tense.** They're facts, not commands. `Deposited`, not `Deposit`.
3. **Events should contain enough information to replay.** `MoneyDeposited { amount: 100, accountId: "..." }` — enough to reconstruct state without additional lookups.
4. **Events should be as granular as business decisions.** If the business distinguishes between "wire transfer deposit" and "cash deposit," those should be different events, not the same event with a flag.
5. **Not everything needs to be an event.** Changing display preferences, toggling dark mode, saving a draft — these might not need event sourcing. Here's how to decide.

Link this from Tutorial Part 1 (after the reader has seen events in action) and from the "Coming From CRUD" page. Then, in the Tutorial Part 4 exercise ("Multiple Accounts"), don't just say "add a `SavingsAccountOpened` event." Say "you're adding savings accounts. Before writing code, list the events you think you'll need. Then compare with the implementation below."

This is the "Think First" challenge from Section III applied to event design specifically. It's where the reader transitions from "I can follow event-sourcing tutorials" to "I can design event-sourced systems."

---

## X. Steve Is Right About the Layer System, but for a Different Reason Than He States

**Responding to: Steve's Section I (Tutorial Layer System) and the plan's Layer System (lines 179–232)**

Steve argues the layer system should be dropped because it's an authoring and maintenance burden. He's right about that. But I want to add a deeper reason: **the layer system fights the implicit-first philosophy.**

The plan's Guiding Principle #7 says: "The tutorial teaches the language implicitly. Concept pages teach it explicitly." This is a deliberate pedagogical choice — and a good one. The reader learns `|>` by using it, not by reading a sidebar that says "the pipe operator passes the left side as the last argument."

But Layer 2 (the syntax sidebar) IS explicit teaching. It's a grammar reference attached to every tutorial page. Its presence says to the reader: "There are language constructs on this page that you need to pay attention to as language constructs." This shifts the reader's attention from "I'm building a bank" to "I'm learning a language" — exactly the frame the implicit-first strategy is designed to avoid.

When we taught Hooks on react.dev, we didn't have a sidebar on the tic-tac-toe tutorial that said "New hooks on this page: `useState` — stores a value between renders." The tutorial just used `useState` and said "this lets us remember the current square." The reader thought about squares, not about hooks. The concept of hooks emerged later, when they had enough experience to appreciate the abstraction.

The NeoHaskell plan's Layer 2 would be like us adding "New React APIs on this page: `useState` — a Hook that lets your component remember information" to the tic-tac-toe tutorial. Technically helpful. Pedagogically counterproductive. It pulls focus from the domain to the language at exactly the wrong moment.

**My specific recommendation (reinforcing Steve's):**

Drop Layer 2. But unlike Steve, I wouldn't put the syntax explanations inline in the tutorial prose either. I'd put them nowhere near the tutorial.

Instead: build the standalone "NeoHaskell Syntax Quick Reference" page that Steve recommends, and build the concept pages that the just-in-time strategy already calls for. The tutorial is for learning by doing. The syntax reference is for looking things up. The concept pages are for understanding deeply. These are three different needs served by three different pages, not three layers on one page.

The only inline language annotation the tutorial needs is the bold-on-first-use convention the plan already describes (line 205): "First occurrence of a construct is bold in the tutorial text itself." Bold it, explain it in one clause within the flow of the sentence, move on. That's implicit teaching done right.

---

## XI. The Plan Needs a "Thinking in Event Sourcing" Page — The Equivalent of react.dev's "Thinking in React"

**Responding to: The concept page schedule and the react.dev research analysis**

The react.dev analysis (line 174) recommends this:

> **Create "Thinking in Event Sourcing" Page** — Mirror React's "Thinking in React" five-step process. NeoHaskell equivalent: Design → Events → Commands → Projections → Queries.

The plan doesn't include this page. It should.

"Thinking in React" is, by traffic and by reader feedback, the most important page on react.dev. It's not a tutorial (it doesn't build a project step by step). It's not a concept page (it doesn't explain one concept in depth). It's a *process page* — it teaches you how to approach a problem the React way.

The five-step process (decompose the UI → build a static version → identify the state → determine where state lives → add data flow) is a *thinking tool.* When a reader faces a new problem, they don't need to remember `useState` syntax. They need to remember the five steps. The syntax is the easy part; the process is what makes them effective.

NeoHaskell needs an equivalent. When a reader faces a new domain (not banking), they need a process:

1. **Identify the domain events.** What are the things that happen in this business? ("Order placed." "Item shipped." "Payment received." "Refund requested.")
2. **Define the commands.** What are the actions users can take? ("Place an order." "Ship an item." "Process a payment." "Request a refund.")
3. **Design the aggregates.** What are the consistency boundaries? ("Order" is an aggregate — its events must be consistent. "Inventory" is a separate aggregate.)
4. **Build the projections.** What views do you need? ("Current order status." "Sales report." "Inventory levels.")
5. **Connect the bounded contexts.** Where do different domains interact? ("Order fulfillment triggers inventory update.")

This five-step process should be a standalone page in Core Concepts, linked prominently from the tutorial's conclusion (Part 6). It's the page that transitions the reader from Stage 4 ("I think in commands, events, and projections") to Stage 5 ("I can model any domain this way"). Without it, that transition relies on the reader independently generalizing from the banking example. Some will. Most won't.

**My specific recommendation:**

Add `thinking-in-events.mdx` to the concept pages. Schedule it to be written alongside Tutorial Part 6 (when the reader has the most context). Use a non-banking domain for the worked example — this is critical, because the whole point is showing that the process works outside banking. The logistics domain from the second example would be perfect.

Structure it exactly like "Thinking in React": walk through the five steps using a new domain, showing the thought process at each step. Include decision points where the reader might choose differently ("Should 'payment received' and 'order confirmed' be the same event? Here's how to decide...").

---

## XII. The Plan's Success Metrics Are About Behavior, Not Understanding — And That's a Problem

**Responding to: "Success Metrics" (lines 769–780)**

The metrics are:

1. Time to first deposit (<10 minutes)
2. Tutorial completion rate (>50% through Part 4)
3. Discord question reduction
4. Contributor PRs
5. Search satisfaction
6. "I built a bank" effect

**My critique:** Every one of these metrics measures behavior (did they do the thing?) rather than understanding (did they learn the concept?). You can optimize for all six and still produce readers who complete the tutorial without understanding event sourcing.

When we measured react.dev's success, we cared about completion rates, sure. But we also cared about: Can readers solve a problem they haven't seen before? Do readers correctly answer "what does this code output?" challenges? Do readers build projects that use patterns from the docs, not just replicate tutorial code?

The hardest thing to measure is whether the reader's mental model actually shifted. But there are proxies:

**My specific recommendation:**

Add two understanding-focused metrics:

1. **Challenge success rate.** If you implement the "Think First" and predict-and-reveal challenges I recommended in Sections III and VI, measure how often readers get them right on the first try. A low success rate on a challenge means the preceding content isn't building the right model. A high success rate means the reader has genuinely internalized the concept. (This requires client-side telemetry, even if anonymous.)

2. **"Build your own" completion rate.** At the end of the tutorial, offer an optional challenge: "Build a simple library book-checkout system using event sourcing. No guidance — just the five-step process from 'Thinking in Event Sourcing.'" Track how many readers attempt it and whether they produce something that uses events correctly. This is the ultimate test of whether the tutorial produced understanding or just procedural memory.

These are harder to measure than "time to first deposit." But they're the metrics that tell you whether the documentation is actually doing its job.

---

## XIII. The Disclaimer Is in the Wrong Place and Sends the Wrong Signal

**Responding to: "Disclaimer (appears on tutorial page 1)" (lines 156–159)**

> NeoBank is a teaching example, not a production banking system. It doesn't handle multi-currency, regulatory compliance, concurrent access, or interest calculation.

**My critique:** Leading with "this isn't real" undermines the plan's own emotional strategy.

The plan's "wow" moments rely on the reader feeling like they built something impressive. "It'll feel like enterprise software. It was 200 lines of code." But a disclaimer at the top of the tutorial says "don't take this too seriously — it's fake." The reader starts the tutorial already discounting the experience.

When we built the tic-tac-toe tutorial on react.dev, we didn't start with "This is a teaching example, not a production game engine. It doesn't handle AI opponents, online multiplayer, or 3D rendering." We just built a tic-tac-toe game. The reader felt the satisfaction of building a real thing. The limitations were obvious from context; we didn't need to enumerate them.

**My specific recommendation:**

Move the disclaimer to the END of Tutorial Part 6 (the conclusion), where it serves a different purpose: "You've built something impressive. Here's what you'd add for production." At that point, the list of missing features becomes a roadmap for learning, not a credibility hedge.

If you must address expectations early, do it with confidence rather than defensiveness:

> NeoBank is focused: accounts, transactions, transfers, and a full audit trail. We'll skip multi-currency, regulatory compliance, and interest calculation — those are real engineering problems covered in the advanced guides. What we WILL build is genuinely event-sourced, and every line of code works.

That sets scope without undercutting the experience.

---

## XIV. Steve's Points I Want to Reinforce, Extend, or Push Back On

**Reinforcing Steve's Section IV (Coming from Haskell ordering):** Yes. Lead with what's the same. The principle is identical to what we learned about Hooks: class component developers needed to hear "you already know most of this — state, effects, lifecycle, it's all still here, just expressed differently" before they could absorb the differences.

**Reinforcing Steve's Section VIII (Alex character):** Agree completely. Second person is more immersive than third person. The reader should be the protagonist, not the observer. On react.dev, every tutorial uses "you" — "You'll build a tic-tac-toe game," not "Alex builds a tic-tac-toe game."

**Extending Steve's Section V (Visual aids):** Steve recommends one architectural diagram, event timelines in Parts 3 and 6, and a "Visual" section in the concept template. I'd add: **the most valuable diagram is the one that shows how a single event flows through the entire system.** Not an architecture diagram — a data-flow diagram. "MoneyDeposited starts here (command handler), becomes this (event), gets stored here (event store), gets consumed here (projection), produces this (read model)." One event, traced through the whole system. This is the event-sourcing equivalent of React's "one-way data flow" diagram, and it's what makes the architecture comprehensible.

**Pushing back slightly on Steve's Section III (JIT concept pages):** Steve recommends 2-3 foundational concept pages (immutability, type inference) available before the tutorial. I agree with the goal but not the specific recommendations. Immutability and type inference are language features, not NeoHaskell-specific concepts. A reader from Haskell already understands both. A reader from JavaScript needs immutability explained, but they need it explained *in context* — not as a 500-word standalone page that uses NeoBank examples they haven't seen yet. I'd instead recommend that the Quick Start page (which already exists in the plan) include brief, contextual explanations of these prerequisites. "NeoHaskell values can't be reassigned — here's what that looks like and why it matters." Three sentences. In context. Not a standalone page.

**Pushing back on Steve's "drop Layer 2 but keep syntax inline" recommendation:** As I argued in Section X, I think syntax should be neither in a sidebar NOR inline in the tutorial. It should be in the tutorial only to the extent that the narrative naturally explains it: "We'll pipe the amount through the handler using `|>`." The syntax reference belongs in its own page. The tutorial should teach event sourcing; the syntax is incidental. If you put too much syntax explanation inline, you get the same frame-shift problem Layer 2 creates, just distributed across the prose.

---

## XV. The Biggest Risk Nobody Has Named

**Responding to: The plan as a whole**

The plan assumes that event sourcing is the right abstraction for the readers it's targeting, and that the main barrier to adoption is understanding. But there's a risk neither the plan nor Steve's review addresses: **what if the reader understands event sourcing perfectly and still doesn't want to use it?**

A CRUD developer might go through the entire tutorial, understand events, commands, projections, and bounded contexts, and think: "This is elegant, but my team would never adopt this. There's no ORM. There's no standard REST framework. I can't hire for this. My existing database doesn't support it. The ecosystem isn't there."

These are not misconceptions. They're legitimate pragmatic concerns. And the documentation plan doesn't address them anywhere.

The plan's Audience Model includes the Architect ("justified skepticism") and the Decision-Maker ("proof points, risk assessment"). But the content plan for these audiences is thin. The "Coming From Event Sourcing" page addresses people who already do event sourcing. The architecture guide addresses technical trade-offs. But nobody addresses the developer who says "I understood everything. I still can't adopt this at work."

**My specific recommendation:**

Add a page — maybe in the Guides section, maybe standalone — called "Adopting NeoHaskell at Work" or "The Pragmatic Case." Address:

1. **"My team doesn't know Haskell."** — What does the learning curve actually look like? How long until productivity?
2. **"We already have a database."** — How does NeoHaskell's event store interact with existing PostgreSQL/MySQL? Can you use both?
3. **"We can't rewrite everything."** — How to introduce event sourcing in one bounded context while keeping CRUD elsewhere. (The plan mentions this in Misconception #18 but doesn't give it a page.)
4. **"Who else uses this?"** — Case studies, production deployments, community size. Be honest about where things stand.

This page exists at the intersection of technical documentation and marketing, which is uncomfortable. But it's where adoption decisions actually happen. React adopted this approach explicitly: the "Add React to an Existing Project" page addresses the reader who says "I can't rewrite my whole app." That page is one of the highest-traffic pages on react.dev, because it meets the reader where they actually are, not where you wish they were.

---

## Summary of Recommendations (Prioritized)

1. **Add "Why Not CRUD?" moment to Tutorial Part 1** — show the failure of mutation before teaching events. This is the foundation all other learning builds on.

2. **Redesign the concept page template** to start with the wrong mental model and break it, not just explain the right one. Add "What You Might Expect" and "Why That Breaks" sections.

3. **Add disambiguation challenges to the tutorial** — specifically the "events are not logs" challenge in Part 1 and "Think First" design challenges in Parts 2-6.

4. **Elevate "Coming From CRUD" from migration page to core concept page** — this is the central teaching document for the primary audience.

5. **Add a "Thinking in Event Sourcing" page** — a five-step design process for applying event sourcing to new domains. The generalization mechanism.

6. **Build predict-and-reveal challenges and side-by-side tabs now** — don't wait for Phase 3 interactivity. 70% of the value at 10% of the cost.

7. **Add "Designing Your Events" to concept pages** — teach the judgment of event sourcing, not just the mechanics.

8. **Address cargo cult risks** — show when NOT to use events, build multiple projections from the same stream, teach bounded contexts honestly.

9. **Add a "When NOT to Event-Source" section** and an "Adopting NeoHaskell at Work" guide — address pragmatic concerns, not just comprehension.

10. **Drop the tutorial Layer 2** (reinforcing Steve) — but keep syntax OUT of the tutorial prose too. Syntax reference belongs in its own page.

11. **Move the disclaimer from Tutorial Part 1 to the conclusion** — don't undercut the experience before it starts.

12. **Add understanding-focused success metrics** — challenge success rates and "build your own" completion rates, not just behavioral metrics.

13. **Teach the Rosetta Stone as a starting point, not a translation layer** — warn against CRUD events wearing event-sourcing costumes.

---

## Closing Thought

The parallel between NeoHaskell and react.dev runs deeper than the plan acknowledges. When we rewrote the React docs, we weren't just updating API references. We were trying to change how an entire community of developers *thought* about building user interfaces. The old docs taught people to use React. The new docs taught people to *think in React.* That's a different project. It's harder, it's slower, and the quality bar is different.

NeoHaskell's documentation isn't trying to teach people NeoHaskell syntax. It's trying to change how CRUD developers think about data. That's a cognitive shift comparable to what React asked of jQuery developers, or what Rust asked of C++ developers. It's not about information — it's about identity. A successful reader doesn't just know how to write event-sourced code. They *think of themselves* as someone who models the world in events.

The plan gets the ambition right. Where it falls short is in the mechanisms for actually producing cognitive change, as opposed to producing tutorial completion. Tutorials that people complete are necessary but not sufficient. What you need are moments of productive confusion, design judgment, and independent application — the moments where the reader's mental model actually shifts.

Ship Tutorial Part 1 with a "Why Not CRUD?" moment and a disambiguation challenge. Watch someone use it. See if they think differently afterward. That's how you'll know if the plan is working.

— Dan Abramov