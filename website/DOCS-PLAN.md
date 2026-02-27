# NeoHaskell Documentation Work Plan

> **Revision 1**: Full review synthesis from Steve Klabnik, Julia Evans, Dan Abramov, Martin Fowler, Sarah Drasner, Tania Rascia, and Kent C. Dodds. All 33 consensus changes incorporated.
> **Revision 2**: Second-pass deep review — 29 additional improvements from individual reviewer insights (landing page experience, design challenges, debugging scenarios, audience-specific content, mobile experience, lesson granularity, event design teaching, i18n quality, visual design system, and more). All three priority tiers applied.
> **Revision 3**: Testbed code review — 6 major corrections from actual NeoHaskell codebase analysis. Transport swap story, configuration-as-code, FP accessibility evidence, CLI-first tutorial strategy, reframed anti-recommendations, updated misconceptions.
> Domain: **NeoBank** (banking/fintech) — consensus from expert panel.
> Language teaching strategy: **Implicit-first** — NeoHaskell syntax taught through the tutorial, not before it.

---

## Guiding Principles

1. **Learner before structure.** Understand who you're teaching and what transformation they need before deciding on sidebars.
2. **The mental model IS the documentation.** Every page exists to move the reader from "thinks in CRUD" to "thinks in events."
3. **Ship value early, iterate constantly.** No 5-phase waterfall. Build the first real page before perfecting the outline.
4. **If it doesn't compile, it doesn't ship.** Code verification infrastructure comes before content, not after.
5. **The example IS the argument.** NeoBank isn't a decoration — it's the lived proof that event sourcing is how the world already works.
6. **Multiple doors, one house.** Different audiences enter differently, but they all arrive at understanding.
7. **The tutorial teaches the language implicitly. Concept pages teach it explicitly.** NeoHaskell syntax is introduced through what the reader is building, never in isolation. New syntax is bolded on first use with a brief inline explanation; concept pages provide the full treatment. No one reads a grammar chapter before cooking dinner.
8. **Delight is a teaching strategy.** Surprise and "wait, really?!" moments aren't decoration — they're how insights stick. The emotional arc of the tutorial (curiosity → confidence → pride) is designed, not accidental.
9. **Polyglot code examples.** Every NeoHaskell code example should include equivalent snippets in JavaScript/TypeScript, Python, and Go (at minimum). Readers learn faster when they see the NeoHaskell way next to what they already know. LLMs make generating these trivially cheap — there's no excuse to skip them.

---

## Audience Model

Three distinct entry points. Each reader arrives with different goals, different patience, and different prior knowledge.

### Developer ("I want to try this")
- Comes from: JavaScript/TypeScript, Python, Go, or Haskell
- Brings: CRUD mental model, ORM habits, REST/GraphQL assumptions
- Wants: working code in 15 minutes, then progressive depth
- Needs to unlearn: mutation-first thinking, "just update the record"
- Success state: can build and deploy an event-sourced app with confidence
- Note: Fintech developers (Stripe, Plaid, banking startups) are a high-value sub-audience. They already think in transactions and will progress fastest through the tutorial.

### Architect ("I want to evaluate this")
- Comes from: enterprise systems, microservices, existing event sourcing with Kafka/EventStoreDB
- Brings: DDD vocabulary, CQRS experience, justified skepticism
- Wants: technical depth, tradeoff analysis, transaction guarantees, comparison with existing event sourcing infrastructure (EventStoreDB, Kafka, Axon)
- Needs to learn: how NeoHaskell's compiler enforces what they currently enforce with discipline
- Success state: can make an informed adoption decision

### Decision-Maker ("I want to understand why this matters")
- Comes from: business context, team management, cost awareness
- Brings: frustration with engineering velocity decay
- Wants: the "why" without the "how", proof points, risk assessment
- Needs to learn: why event sourcing reduces long-term cost
- Success state: understands the value proposition well enough to greenlight a pilot

---

## Landing Page Experience Design

The landing page (`index.mdx`) is the highest-traffic, highest-stakes page in the entire site. It must serve all three audience types within 10 seconds of arrival — not with a wall of text, but with clear visual paths.

### Three-Path Entry (CardGrid)

Use Starlight's `<CardGrid>` component to present three distinct entry points, each speaking directly to one audience:

| Card | Audience | Headline | Subtext | Destination |
|------|----------|----------|---------|-------------|
| **Try It** | Developer | "Build something in 10 minutes" | "Go from zero to a working event-sourced app" | Quick Start → Tutorial |
| **Evaluate It** | Architect | "How it compares to what you know" | "Event sourcing with compiler guarantees — tradeoffs, performance, adoption path" | Why NeoHaskell? → Architecture Evaluation |
| **Understand It** | Decision-Maker | "Why this matters for your team" | "Event sourcing reduces long-term cost. Here's the proof." | Why NeoHaskell? (business framing) |

### Design Requirements

- **Above the fold**: The three cards must be visible without scrolling on a standard laptop (1440×900)
- **No paragraph-of-text intro**: The opening line + CardGrid IS the introduction. Save the philosophy for deeper pages.
- **One sentence of NeoHaskell identity** above the cards: *"NeoHaskell makes event sourcing the default — the compiler enforces what other languages leave to discipline."*
- **Social proof below the fold** (when available): GitHub stars, Discord member count, notable adopters
- **Mobile**: Cards stack vertically, each with a clear tap target (see Mobile Experience section)

> **Source**: Sarah Drasner review — "The landing page should route people, not lecture them. Three clear paths with zero ambiguity."

---

## The Mental Model Journey

This is the actual spine of the documentation. Every page must place the reader somewhere on this path and move them forward.

```
Stage 0: "I store data in rows and update them"                    (CRUD-thinker)
Stage 1: "Banks don't update balances — they record transactions"  (Event-curious)
Stage 2: "Events are immutable facts. State is derived"            (Event-thinker)
Stage 3: "The type system prevents me from cheating"               (NeoHaskell-aware)
Stage 4: "I think in commands, events, and projections"            (NeoHaskell-fluent)
Stage 5: "I can model any domain this way — not just banking"      (NeoHaskell-native)
```

Every page in the docs should have a metadata annotation (internal, not reader-facing):
- **Entry stage**: what the reader must already understand
- **Exit stage**: what the reader understands after this page
- **Key misconception addressed**: the specific wrong assumption this page corrects

---

## The 20 Misconceptions (Design Before Structure)

Before writing a single page, enumerate and validate these. They drive content decisions.

| # | Misconception | Truth | Where to address |
|---|--------------|-------|-----------------|
| 1 | "I need a database with tables and columns" | Events are stored in an append-only log; read models are derived | Getting Started, Concepts |
| 2 | "Event sourcing is a library I import" | It's the language's default — you'd have to work to avoid it | First tutorial page |
| 3 | "I can just update a field" | State changes are expressed as events; the compiler enforces this | Early tutorial |
| 4 | "This is just Haskell with different names" | NeoHaskell has different defaults, stdlib, and compiler behavior | "Coming from Haskell" |
| 5 | "I need to learn category theory" | NeoHaskell hides the math; you use it without naming it | Concepts intro |
| | | **Evidence**: The testbed `Cart/Commands/AddItem.hs` decide function uses `case..of`, `if/then/else`, and `Decider.reject` / `Decider.acceptExisting`. Zero type class constraints visible to the user. A JavaScript developer can read every line. | |
| 6 | "Event sourcing means eventual consistency everywhere" | Depends on the bounded context; NeoHaskell gives you choices | Architecture guide |
| 7 | "My events are my API" | Commands (deposit slips) are the API; events (transactions) are internal facts | Tutorial part 2 — Account Rules |
| 8 | "I'll need a message broker (Kafka, RabbitMQ)" | Built-in event store handles this at language level | Getting Started |
| 9 | "Schema evolution will be a nightmare" | Type system + versioning strategy (address honestly) | Core Concepts page (not Advanced — this is too important to defer) |
| 10 | "Functional programming means no state" | There is state — it's just derived from events, not mutated | Concepts |
| | | **Evidence**: The testbed's `EventCounter.hs` uses `ConcurrentVar` for mutable state. State is managed, not forbidden. The `update` function in `Cart/Core.hs` is a pure function that takes an event and entity and returns a new entity — exactly like a Redux reducer. | |
| 11 | "Haskell is slow" | NeoHaskell compiles to native code; concurrency is built-in | FAQ or Concepts |
| 12 | "I need to understand monads first" | NeoHaskell's stdlib is designed so you don't need to | Getting Started |
| | | **Evidence**: The testbed's entire user-facing API contains zero references to Monad, Functor, or Applicative. `do` blocks are used like async/await. `Task.yield` replaces `pure`/`return`. The word "monad" never appears in user code. | |
| 13 | "Event sourcing only works for banking/finance" | Banking is the natural fit, but it works for any domain — the logistics guide proves this | Second example domain (advanced guides), Coming From... pages |
| 14 | "I can't do simple CRUD-style operations" | Read models give you familiar query patterns — checking your balance IS a read model | Tutorial part 3 — Transaction History |
| 15 | "The compiler errors will be cryptic" | NeoHaskell explicitly focuses on friendly error messages | Getting Started |
| 16 | "I need Docker/Kubernetes to run this" | `nix develop` gives you everything | Installation |
| 17 | "This won't integrate with my existing system" | Integrations package, PostgreSQL event store | Guides |
| 18 | "I have to go all-in or not at all" | You can start with one bounded context | Architecture guide |
| 19 | "Testing event-sourced systems is hard" | It's actually easier — replay events, assert projections | Testing guide |
| 20 | "There's no community/support" | Discord, contributors, going full-time | Throughout |

> **Action**: Validate these with real developers before finalizing structure. Post in Discord, ask in the Haskell subreddit, talk to 5 people who looked at NeoHaskell and didn't adopt it.

---

## NeoBank: The Tutorial Spine

Design this FIRST. The tutorial structure emerges from the example's progression.

### The Domain

You're building NeoBank — a simple banking service. The domain is universally understood (everyone has a bank account), maps perfectly to event sourcing (ledgers are the original event store), and produces output that feels like real fintech with minimal complexity.

### The Character

The tutorial uses second person ("you") from Part 1 onward. Alex appears only on the tutorial index page as brief framing ("Meet Alex — a developer like you who wants to build a bank"). Inside the tutorial itself, product requirements are framed as "The requirement:" not "Alex wants to..." — this keeps the reader immersed as the builder, not an observer.

### The Scope

| Feature | Sounds like | Actually is |
|---------|------------|-------------|
| Multi-account management | Enterprise banking platform | A map of account IDs to event streams |
| Real-time transaction ledger | Bloomberg terminal | A list of events, rendered chronologically |
| Instant balance reconciliation | FinTech infrastructure | A fold over deposit/withdrawal events |
| Overdraft protection | Risk management system | A type-level constraint the compiler enforces |
| Inter-account transfers | Payment settlement network | Two commands across two aggregates |
| Complete audit trail | Regulatory compliance engine | The event store, which you already have |
| Historical replay | Time-travel debugging | Replaying events to a point in time |

> The magic: the left column sounds like it requires a team of 20. The right column is what the reader actually builds. NeoHaskell makes the right column produce the left column.

### The Progression

| Part | Title | Your goal | What the reader builds | Events introduced | The "wow" moment |
|------|-------|------------|----------------------|-------------------|-----------------|
| 1 | **Your First Transaction** | "I want to deposit money" | Event type, command handler, event store, balance as fold. Opens with brief CRUD example that fails ("what was the balance before the dispute?") — then introduces events as the solution. | `AccountOpened`, `MoneyDeposited` | "That's 12 lines of code?" |
| 2 | **Account Rules** | "Don't let me overdraw" | Validation, declined commands (return `Result.err`, not events), compiler catching illegal states. First Given-When-Then test introduced. | `MoneyWithdrawn` | "The compiler just prevented a real bug" |
| 3 | **Transaction History** | "Show me what happened" | TWO projections from the same event stream: formatted bank statement + monthly summary. This IS the CQRS insight — same events, different views. Projection test introduced. | (consuming existing events) | "Two completely different views from the same events?" |
| 4 | **Multiple Accounts** | "I need a savings account too" | Multiple aggregates (NOT bounded contexts — checking/savings are different aggregates in the same context), account-scoped events | `AccountOpened` (savings variant, same event with `accountType` field) | "Adding a new account type was trivial?" |
| 5 | **Transfers** | "Move money between accounts" | Cross-aggregate commands, saga/process manager with full state machine: happy path → failure → compensation. Cross-aggregate test. | `TransferRequested`, `TransferCompleted`, `TransferFailed` | "It handles the failure case too?" |
| 6 | **Audit Everything** | "Show me everything that ever happened" | Event replay, time-travel queries, full audit trail. Property-based test. | (replaying all events) | "I accidentally built an audit trail" |

> **Revision 3 note — CLI-first strategy**: Based on testbed evidence, the tutorial should begin with CLI transport for instant feedback (no web server, no PostgreSQL). The transport swap from CLI → Web happens in Part 4 or 5, creating a natural "wow" moment. PostgreSQL replaces in-memory event store in Part 6 or as a post-tutorial guide. Each transport/persistence change is a single-line configuration change — this demonstrates the architecture's power better than any explanation.

### Event Design Teaching

The tutorial must explicitly teach *how to design good events*, not just how to use them. This is the single most transferable skill — a reader who can design events for banking can design events for any domain.

**Core principle to teach**: "Good events describe what happened in the world, not what the system did."

| Good event name | Bad event name | Why |
|----------------|---------------|-----|
| `MoneyDeposited` | `BalanceUpdated` | The good name describes a real-world fact; the bad name describes a system operation |
| `AccountOpened` | `AccountCreated` | "Opened" is the domain language (you open a bank account); "Created" is system language |
| `WithdrawalDeclined` | `ValidationFailed` | The good name tells you what happened to the customer; the bad name is an implementation detail |
| `TransferCompleted` | `TransferProcessed` | "Completed" has domain meaning; "Processed" is vague system-speak |

**Where to teach this**: Tutorial Part 2 (Account Rules) is the natural place — the reader is defining their first business events beyond the initial deposit. Include a callout box: "Naming rule: if your event name would make sense in a conversation between bankers, it's a good name. If it only makes sense to programmers, rename it."

> **Source**: Dan Abramov review — "The most important thing a tutorial can teach about event sourcing isn't the mechanics — it's how to think about what constitutes a good event."

### The Rosetta Stone (appears in Concepts, referenced from Tutorial Part 1)

| What you'd say | Banking term | NeoHaskell term | What it actually is |
|---------------|-------------|-----------------|-------------------|
| "Something happened" | Transaction | Event | An immutable record of a fact |
| "Do something" | Deposit slip | Command | A request that may be accepted or rejected |
| "What's my balance?" | Account statement | Projection | State derived by folding over events |
| "My account" | Account | Aggregate | A consistency boundary (not the entity itself — the boundary that protects it) |
| "Show me everything" | Audit log | Event Store | The append-only source of truth |
| "Retail vs. Lending" | Business division | Bounded Context | An independent model with its own events |

### Disambiguation Exercise (Part 1)

Part 1 includes a multiple-choice "check your understanding" moment after the first event is stored. This catches the most dangerous early misconception — that events are just logging.

> **The question**: "Which of these best describes what an event is?"
> - (a) A log message for debugging
> - (b) An immutable fact about something that happened in the domain
> - (c) A notification sent to another service
> - (d) A database row that gets updated
>
> **Correct answer**: (b). The tutorial explains why each wrong answer is wrong:
> - (a) "Events aren't log messages — you can't delete them, and your entire system state derives from them"
> - (c) "Events might trigger notifications later, but the event itself is a recorded fact, not a message"
> - (d) "Events are append-only — they're never updated, which is the whole point"

This appears as an interactive `<details>` toggle ("Test your understanding") after the reader stores their first event and before moving on to commands. It's the earliest possible intervention against wrong mental models.

> **Source**: Dan Abramov review — "Catch the 'events are just logging' misconception in Part 1, or it calcifies and the reader builds on a broken foundation."

### Design Constraints

- Each part builds ONE concept on top of the previous
- Each part results in running code the reader wrote themselves
- Each part starts with a product requirement ("The requirement: ..."), not "Alex wants to..."
- Part 1 must produce visible output within 10 minutes of starting
- Part 6 deliberately echoes NeoHaskell.org's pitch: "Banks don't UPDATE your balance"
- **Version-locked**: The tutorial targets a specific NeoHaskell version (pinned in the companion repo's `flake.nix`). The installation page states "This tutorial uses NeoHaskell vX.Y.Z." When NeoHaskell releases a breaking change, the tutorial is updated as part of the release process — not left to drift. (Source: Tania Rascia — "A tutorial that doesn't specify its version is a tutorial with an expiration date stamped 'unknown.'")
- Each tutorial page includes estimated reading/coding time (e.g., "~20 minutes")
- Each tutorial page opens with "What you'll learn" (3-5 bullet points) and closes with a "Recap" section
- Progress indicator shows "Part N of 6" on every tutorial page — completion visibility is motivation
- Tests are introduced from Part 2 onward; by Part 6 the reader has written 10-15 tests
- **Lesson granularity**: Each "Part" may be split into 2-4 focused lessons if the content exceeds ~25 minutes of reading/coding time. Example: Part 1 might become "1a: Your First Event" (10 min) and "1b: Your First Command" (15 min). The key constraint: each lesson must end with a working, runnable state. Progress indicator updates to "Part 1, Lesson 2 of 3" if split. Target: ~15-20 minutes per lesson, ~20 lessons total across 6 parts. (Source: Julia Evans — "Shorter, more frequent completion moments keep motivation high.")

### Debugging Scenario (Part 3 or 4)

The tutorial must include at least one moment where the code compiles successfully but produces wrong behavior — and the reader has to debug it using events. This teaches a critical real-world skill that "it compiles" doesn't cover.

**Recommended placement**: Part 3 (Transaction History) or Part 4 (Multiple Accounts).

**Example scenario**: The reader's monthly summary projection shows the wrong total. The code compiles. The types are correct. But the projection logic has a subtle error (e.g., counting transfers as both withdrawals AND deposits, double-counting). The reader must:

1. Replay the event stream manually (or with a helper function)
2. Identify which event is being miscounted
3. Fix the projection logic
4. Verify with a test

**Why this matters**: Real event-sourced systems fail at the projection level, not the event level. A tutorial that only shows the happy path leaves readers unprepared for the most common class of bugs they'll encounter.

> **Source**: Kent C. Dodds review — "If everything always works on the first try, the reader hasn't actually learned debugging. Give them a real bug to solve."

### Production Scope Note (appears in Part 6 conclusion, NOT Part 1)

> Leading with "this isn't real" undermines the emotional strategy. Instead, the Part 6 conclusion reframes scope as forward-looking: "You've built accounts, transactions, transfers, and a complete audit trail. For production, you'd add: multi-currency support, regulatory compliance, concurrent access controls, and interest calculation. Those are covered in the advanced guides. What you've proven is that event sourcing makes complex-sounding features genuinely simple."
>
> If scope-setting is needed early, do it with confidence ("We'll keep this focused on the core patterns"), not defensiveness.

### The Opening Line of the Tutorial

> *"Every bank in the world runs on the same idea: don't change the number, record what
> happened. A deposit isn't 'set balance to $150' — it's 'recorded: $50 deposited.'
> NeoHaskell is built on this same idea. Let's prove it works by building a bank.*
>
> *By the end of this tutorial, your NeoBank will handle accounts, transactions, transfers,
> and a complete audit trail. It'll feel like enterprise software. It was 200 lines of code."*

### "What You Built" Reveal (Part 6 Conclusion)

The end of Part 6 includes a scope table reveal — showing the reader the full breadth of what they've built, mapping their "simple" code to the enterprise-sounding features from the Scope table at the top of this section.

```markdown
## What You Built

| What it sounds like | What you actually wrote | Lines of code |
|--------------------|-----------------------|---------------|
| Multi-account management | A map of account IDs to event streams | ~15 |
| Real-time transaction ledger | A list of events, rendered chronologically | ~20 |
| Instant balance reconciliation | A fold over deposit/withdrawal events | ~8 |
| Overdraft protection | A type-level constraint the compiler enforces | ~5 |
| Inter-account transfers | Two commands across two aggregates | ~30 |
| Complete audit trail | The event store, which you already had | 0 (free) |
| Historical replay | Replaying events to a point in time | ~10 |
| **Total** | | **~88 lines** |
```

This is the emotional climax of the tutorial — the moment the reader realizes the left column (which sounds like it requires a team of 20) maps to the right column (which they wrote in an afternoon).

### "What's Next" Transition Page

After Part 6, a dedicated transition page (`tutorial/whats-next.mdx`) bridges the gap between "I finished the tutorial" and "I'm building my own thing." Without this, tutorial completers churn.

**Structure**:
1. **Celebrate**: "You just built an event-sourced banking service. Seriously."
2. **Branch**: Three paths forward, matching the audience model:
   - *"I want to build something else"* → Second example domain (logistics), then Guides
   - *"I want to understand the theory deeper"* → Core Concepts reading order
   - *"I want to use this at work"* → Adopting NeoHaskell at Work guide, Coming From pages
3. **Community**: Discord link, contributor guide, "show us what you built"
4. **Reference bookmark**: "Now that you've used these patterns, the Reference section will make sense. Bookmark it."

> **Source**: Julia Evans review — "The moment after a tutorial ends is the highest-churn moment. Give them a clear next step or they leave." Sarah Drasner review — "The post-tutorial transition page is where documentation sites lose half their audience."

### Second Example Domain (for advanced guides)

**Logistics / shipment tracking.** A package moves through locations, status changes are events, delivery is a projection. Different enough from banking to prove the pattern generalizes. Universal enough that everyone understands it.

**TDD Challenge Format**: The second domain should be presented as a test-driven challenge — the reader is given a complete test suite (Given-When-Then tests for the logistics domain) and must implement the event types, command handlers, and projections to make the tests pass. This inverts the tutorial's teach→test flow into a test→implement flow, proving the reader has internalized the pattern. The test suite serves as both specification and verification. (Source: Kent C. Dodds — "Providing tests and asking the reader to make them pass is the strongest possible assessment of whether they've actually learned the material.")

### Installation Friction Mitigation

If Nix is the only installation path, this is the single highest-friction point in the entire documentation. Design the Nix experience as if it's the product.

- **Zero-install path if possible**: Offer a playground or GitHub Codespaces option so readers can start the tutorial without any local setup
- **If Nix is required**: The installation page must make Nix feel effortless — not "install Nix, then install NeoHaskell" but "run this one command"
- **Installation time estimates**: Tell the reader how long each step takes ("This downloads ~500MB and takes 2-5 minutes")
- **Platform-specific instructions**: Use Starlight Tabs for macOS / Linux / Windows (WSL) — no "figure out which one applies to you"
- **Troubleshooting section**: The 5 most common installation failures and their fixes, based on Discord reports

### Transport-Agnostic Architecture

NeoHaskell services are transport-agnostic by design. The same service code runs as a CLI tool or a web API. The only line that changes is the transport declaration.

**Evidence from testbed `App.hs`**:

```haskell
-- Run as a web API:
Application.withTransport WebTransport.server app

-- Run as a CLI tool:
Application.withTransport CliTransport.server app
```

That's it. One line. The CLI transport auto-generates subcommands from command schemas, giving you a fully functional command-line interface with zero additional code. The web transport exposes the same commands as HTTP endpoints.

**Implication for the tutorial**: Start with CLI transport for instant feedback. No web server to configure, no PostgreSQL to install, no Docker to run. The reader gets a working event-sourced app in minutes. The transport swap from CLI to Web happens in Part 4 or 5 as a deliberate "wow" moment. PostgreSQL replaces the in-memory event store in Part 6 or as a post-tutorial guide. Each change is a single line.

This is the architecture's most compelling demonstration. No explanation conveys it as well as watching the reader change one word and get a web API.

> **Source**: Dan Abramov and Kent C. Dodds council feedback — "The transport swap story is the single most powerful demonstration of NeoHaskell's design. It should be the tutorial's climactic moment, not a footnote in the architecture guide."

### Configuration as Code

NeoHaskell's `defineConfig` DSL generates CLI flags, environment variable parsing, documentation, defaults, and type validation from a single declarative definition.

**Evidence from testbed `Config.hs`**:

```haskell
defineConfig do
  field "port" Int 8080 "HTTP port to listen on"
  field "database" Text "postgres://localhost/app" "PostgreSQL connection string"
  field "logLevel" LogLevel Info "Logging verbosity"
```

This single definition gives you:
- `--port 9000` as a CLI flag
- `PORT=9000` as an environment variable
- Auto-generated `--help` documentation
- Type-safe parsing with friendly error messages
- Sensible defaults that work out of the box

**Implication for the 'Why NeoHaskell?' page**: The Architect audience evaluates tools partly on operational ergonomics. Configuration-as-code with auto-generated CLI flags, env vars, and docs is a concrete, demonstrable advantage over hand-rolled config parsing. This belongs prominently in the Architect section of the 'Why NeoHaskell?' page.

> **Source**: Steve Klabnik council feedback — "Configuration is where most frameworks make you write the same boilerplate three times. Show that NeoHaskell writes it zero times."

---

## Tutorial Layer System

Every tutorial page operates on three simultaneous layers. The reader engages with layer 1 (the narrative) and dips into layers 2 and 3 as curiosity demands.

### Layer 1: The NeoBank Narrative (mandatory, always visible)

This is the tutorial. You want to build a feature, you write the code, you see the result. NeoHaskell syntax is used but never formally introduced — the reader absorbs it by doing.

### Layer 2: Inline Syntax Explanation (replaces the dropped sidebar specification)

> The original plan specified a collapsible "New NeoHaskell on this page" sidebar. Reviewer consensus (5/7) found this overengineered and fighting the implicit-first philosophy. Julia Evans' annotated code blocks solve compound-unfamiliarity (Martin's concern) without shifting the reader's attention from "I'm building a bank" to "I'm learning a language."

The tutorial handles new syntax through three mechanisms:

1. **Bold on first use**: The first time a construct appears, it's **bolded** with a one-clause inline explanation. Example: "We use the **pipe operator (`|>`)** — it passes the left side as input to the right side, like Unix pipes for code."

2. **Annotated code blocks**: For the first major code example in Parts 1-3, use Julia Evans-style annotated code blocks where arrows or callouts point to unfamiliar syntax within the code itself. This teaches syntax IN the context of what the reader is building, not separate from it.

3. **Standalone syntax quick reference page**: `getting-started/cheat-sheet.mdx` (already planned) serves as the go-to reference for NeoHaskell syntax. Linked once from Part 1 ("bookmark this — it's your NeoHaskell phrasebook").

Rules:
- Maximum 3-5 new constructs per page (if more, the page is doing too much — split it)
- No separate syntax sidebar or collapsible aside — syntax lives in the narrative flow
- First occurrence of each construct is bold in the tutorial text with a brief explanation
- Subsequent occurrences are not explained — the reader has seen it before

### Layer 3: Concept Deep-Dive Links (inline, unobtrusive)

When the tutorial uses a concept that has a dedicated concept page, provide a non-disruptive link:

```markdown
The balance is a *projection* — state derived from events.
[→ Deep dive: What is a projection?](/concepts/projections)
```

Rules:
- Never interrupt the narrative flow — links go at the end of the paragraph, not mid-sentence
- Each concept is linked **once** (first meaningful use), not every time
- Links are optional — the tutorial MUST make sense without clicking any of them

### Layer Interaction Map

| Tutorial Part | Layer 2: Bold-on-first-use syntax | Layer 3: Concept Links |
|--------------|--------------------|-----------------------|
| 1. First Transaction | `do`, `\|>`, type declarations, `Task`, `Module.yield` (annotated code block) | Events, Commands, Event Store |
| 2. Account Rules | `case..of`, `Result`, pattern matching on types (annotated code block) | Aggregates, Validation, Testing |
| 3. Transaction History | Record syntax, `Array.map`, `Array.foldl` (annotated code block) | Projections, Read Models |
| 4. Multiple Accounts | Module structure, qualified imports, `Map` | Multiple Aggregates |
| 5. Transfers | `Task.andThen`, error propagation, `[fmt\|...\|]` | Sagas, Process Managers, Bounded Contexts |
| 6. Audit Everything | Event replay functions, time-based queries | Event Store internals |

> **Principle**: If a reader skips every link, they still complete the tutorial and have working code. The layers add depth, not dependencies.

---

## Code Verification Infrastructure

Build this BEFORE writing content. Non-negotiable.

### Requirements

1. Every code block in the docs must be extractable and compilable
2. CI runs on every PR that touches content files
3. Broken code blocks fail the build
4. Code blocks reference source files from the NeoHaskell repo with git hashes for drift detection

### Implementation

```
# In the website repo
scripts/
  extract-code-blocks.sh    # Extracts fenced code from .mdx files
  verify-code-blocks.sh     # Compiles each extracted block
  check-source-hashes.sh    # Verifies referenced source files haven't drifted

# In CI (GitHub Actions)
.github/workflows/
  verify-docs-code.yml      # Runs on PR, blocks merge if code is broken
```

### Approach Options (decide during implementation)

- **Option A**: Code blocks are inline in MDX, extracted and compiled by CI
- **Option B**: Code blocks live as separate `.hs` files, imported into MDX (Rust Book approach)
- **Option C**: Hybrid — tutorial code is standalone files, concept examples are inline

> Option B is recommended. It makes testing trivial and prevents code/prose from drifting apart.

---

## Site Structure (Diataxis-Informed)

Using the [Diataxis framework](https://diataxis.fr/) explicitly. Four documentation modes, three entry paths.

```
                    PRACTICAL
                       |
          Tutorials    |    How-to Guides
          (learning)   |    (goals)
                       |
  ACQUISITION ---------+--------- APPLICATION
                       |
          Explanation  |    Reference
          (understanding)   (information)
                       |
                   THEORETICAL
```

### Top-Level Sections

```yaml
sidebar:
  # Entry path: Developer
  - label: "Quick Start"
    purpose: "Zero to running code in 10 minutes"
    audience: Developer
    stage: 0 -> 1

  # Entry path: Developer (deep)
  - label: "Tutorial: Build NeoBank"
    purpose: "Build an event-sourced banking service, learn every concept"
    audience: Developer
    stage: 1 -> 5

  # Entry path: Everyone
  - label: "Core Concepts"
    purpose: "Mental models — why NeoHaskell works this way"
    audience: All (but especially Architect)
    stage: Varies (each page has its own entry/exit)

  # Entry path: Developer (practicing)
  - label: "Guides"
    purpose: "Solve a specific problem (standalone, non-linear)"
    audience: Developer building something
    stage: 3+

  # Entry path: Developer (practicing)
  - label: "Reference"
    purpose: "Look up API details (generated where possible)"
    audience: Developer building something
    stage: 3+

  # Entry path: Architect, Decision-Maker
  - label: "Coming From..."
    purpose: "Translation layers for specific backgrounds"
    audience: Anyone with existing mental models
    stage: 0 -> 2
```

### Boundary Definitions (Kent's Rule)

| Section | Linear? | Builds one project? | Standalone pages? | Reader goal |
|---------|---------|--------------------|--------------------|-------------|
| Quick Start | Yes | Minimal example | No | "Does this work on my machine?" |
| Tutorial | Yes | NeoBank | No | "Teach me everything in order" |
| Core Concepts | No | No | Yes | "Help me understand why" |
| Guides | No | No | Yes | "Help me do this specific thing" |
| Reference | No | No | Yes | "What are the exact details?" |
| Coming From... | No | No | Yes | "Map my existing knowledge" |

### Litmus Tests (for deciding where content belongs)

| Section | Litmus Test |
|---------|-------------|
| Tutorial | "Does this page require previous pages?" — if yes, it's tutorial. |
| Concept | "Could the reader understand this without a keyboard?" — if yes, it's explanation. |
| Guide | "Does this solve a problem the reader already knows they have?" — if yes, it's a guide. |
| Reference | "Would a reader come here to look up a forgotten detail?" — if yes, it's reference. |

---

## Reference Documentation Strategy

Reference docs should be **generated from source code** where possible, **hand-written** only for narrative explanation.

### Auto-Generated

- Module API docs (types, functions, signatures)
- Event store API
- Standard library function reference
- CLI tool reference (`neo` commands)

### Hand-Written

- "How to read this reference" guide
- Cross-cutting patterns (error handling conventions, naming conventions)
- Architecture decision records (ADRs — already in the sidebar)

### Source Linking

Every code example in the docs references the NeoHaskell source:

```markdown
<!-- source: neohaskell/neohaskell/core/src/Event/Store.hs@a1b2c3d -->
```

CI checks these hashes. When upstream changes, a bot opens a PR flagging stale examples.

---

## Just-in-Time Concept Page Strategy

Concept pages are NOT pre-written. They are written **when the tutorial first needs them**, ensuring every concept page has a concrete anchor in the reader's experience.

### The Rule

> No concept page exists until a tutorial page links to it. The tutorial drives concept page creation, not the other way around.

### Concept Page Schedule (driven by tutorial progression)

| Tutorial Part | Concept Pages to Write | Why now (the tutorial needs it) |
|--------------|----------------------|-------------------------------|
| 1. First Transaction | `events-not-state.mdx`, `commands-and-handlers.mdx`, `from-crud-to-events.mdx` | Reader just stored their first event, issued their first command, and saw CRUD fail |
| 2. Account Rules | `type-safety.mdx`, `testing-event-sourced-systems.mdx` | Reader saw the compiler reject an invalid withdrawal and wrote their first test |
| 3. Transaction History | `projections.mdx` | Reader just built two projections from the same events — the CQRS concept is fresh |
| 4. Multiple Accounts | `schema-evolution.mdx` | Reader has two account types — schema evolution is concrete now, not theoretical |
| 5. Transfers | `effects.mdx`, `bounded-contexts.mdx` | Cross-aggregate transfers genuinely need bounded context explanation; effects needed for saga |
| 6. Audit Everything | `concurrency.mdx`, `thinking-in-events.mdx`, `trade-offs.mdx` | Capstone: replay at scale needs concurrency; wrap-up needs "Thinking in Events" (the react.dev equivalent); honesty about trade-offs |

> **New pages from review consensus**: `from-crud-to-events.mdx` (elevated from Coming From subpage), `testing-event-sourced-systems.mdx`, `thinking-in-events.mdx` (Dan's "Thinking in React" equivalent — 5-step process: Identify events → Define commands → Design aggregates → Build projections → Connect bounded contexts, using non-banking domain), `trade-offs.mdx` (honest about when event sourcing adds unnecessary complexity), `schema-evolution.mdx` (moved from Advanced to Core — too important to defer).

### Concept Page Template (internal)

Every concept page follows this structure. **The template is flexible** — some concepts need 2 sections, some need 7. Required sections are marked; all others are optional based on what the concept demands.

```markdown
---
title: [Concept Name] (use concept-name headings for searchability)
description: [One sentence]
entry_stage: [Mental model stage required]
exit_stage: [Mental model stage after reading]
misconception: [Which misconception from the 20 this addresses]
tutorial_anchor: [Which tutorial part first links here]
---

## What You Might Expect
[Start from the reader's CRUD assumptions. "If you're used to SQL, you'd expect..."]

## Why That Breaks
[Show the specific failure. Make the reader feel the pain of the old model.]

## [Concept Name]: The Mental Model Shift (REQUIRED)
[The core explanation — what changes in how you think, not just what you do]

## What Happens When... (trace a concrete scenario)
[Make invisible runtime behavior visible. "When you deposit $50, here's what happens step by step..."]

## In NeoBank Terms (REQUIRED)
[Explain using the banking domain they already know from the tutorial]

## The Full Picture (REQUIRED, with code)
[Complete explanation with code examples]

## How NeoHaskell Enforces This
[What the compiler/linter does to keep you on the right path]

## Testing This Concept
[How to verify your understanding with a test — connects to testing-first philosophy]

## Going Deeper (optional)
[Academic references, advanced patterns, links to external resources]
```

> Required sections: **Mental Model Shift**, **In NeoBank Terms**, **The Full Picture**. All others are included when they serve the concept — flexibility over rigidity.

### Why Just-in-Time?

Pre-written concept pages suffer from three problems:
1. They use abstract examples because no tutorial context exists yet
2. They can't reference "remember when you built X?" — the reader hasn't
3. Writers guess what needs explaining instead of knowing from tutorial authoring

---

## "Coming From..." Section

Translation layers for specific backgrounds. These become the highest-traffic pages.

### Pages

| Page | Reader's background | Key translations |
|------|-------------------|-----------------|
| Coming from JavaScript/TypeScript | `let x = ...; x = newValue` | Immutability, type inference, algebraic data types |
| Coming from Python | Django ORM, Flask routes | Events vs. models, handlers vs. views |
| Coming from Go | Structs, interfaces, goroutines | Type classes, algebraic types, channels |
| Coming from Haskell | Monads, IO, cabal/stack | **Full Rosetta Stone page** — see expanded spec below |
| Coming from CRUD | REST endpoints, SQL updates | `UPDATE balance` → record `MoneyDeposited`; `SELECT balance` → fold over transactions; REST endpoint → command handler. **NOTE: This is elevated to a standalone Core Concepts page (`from-crud-to-events.mdx`), not just a Coming From subpage.** This is the core teaching document for the primary audience. Structure: What You Might Expect → Why That Breaks → The Mental Model Shift → Side-by-Side Code → When CRUD Is Fine. The Coming From section retains a short version that links to the full concept page. |
| Coming from Event Sourcing | Kafka, EventStoreDB, Axon | No schema registry needed (types ARE the schema), no consumer groups (projections are functions), event versioning via algebraic data types. **Honesty requirement**: This page must acknowledge what NeoHaskell DOESN'T yet have that mature ES platforms do — distributed event stores, consumer group equivalents, production battle-testing. Tone: "Here's what's genuinely better, here's what's different, and here's what's not ready yet." Experienced ES developers will spot overselling instantly and leave. (Source: Martin Fowler — "The Coming from Event Sourcing page is the most dangerous page to get wrong. Oversell and you lose the most valuable audience.") |

### Pattern for Each Page

```
## In [your language], you'd write...
[familiar code]

## In NeoHaskell, the equivalent is...
[NeoHaskell code]

## But here's what's actually different...
[the mental model shift — NOT just syntax translation]
```

### "Coming from Haskell" — Expanded Specification

This page is the most important "Coming From..." entry. Haskell developers will assume NeoHaskell is "just Haskell with different names" (Misconception #4). This page must disabuse them of that immediately and completely.

#### Structure

1. **Opening**: "NeoHaskell is a Haskell dialect, not a Haskell skin. It makes deliberate, opinionated choices that differ from GHC Haskell. These aren't bugs — they're the point."

2. **The Full Rosetta Stone** (every enforced divergence):

| Haskell | NeoHaskell | Why NeoHaskell chose differently | Enforced by |
|---------|-----------|--------------------------------|-------------|
| `IO a` | `Task err val` | Typed errors — every effectful computation declares its failure mode | Compiler |
| `Either a b` | `Result err val` with `Ok`/`Err` | Names describe intent, not position | Compiler |
| `pure` / `return` | `Module.yield` (e.g., `Task.yield`, `Result.ok`) | Says what it does, not what it satisfies | Linter (hlint) |
| `$` | `\|>` (pipe operator) | Left-to-right data flow, like reading English | Linter |
| `let..in` / `where` | `do` blocks for ALL bindings | One way to bind, always. Reduces cognitive load | Linter |
| Pattern matching in function defs | `case..of` only | Patterns are always visible at the use site | Linter |
| Point-free style `map f . filter g` | Explicit lambdas always | Readability over cleverness | Linter |
| `import Module (func)` | Qualified imports: `Array.map`, `Text.length` | Always know where a function comes from | Convention (enforced by linter) |
| `forall a b.` | `forall input output.` | Type variables describe their role | Convention |
| `Functor` / `Monad` / `Semigroup` | `Mappable` / `Thenable` / `Appendable` | Names describe behavior, not math heritage | Stdlib |
| Stack / Cabal | Nix + Hix | Reproducible builds by default | Tooling |
| `putStrLn` / string concatenation | `[fmt\|Hello {name}!\|]` | String interpolation via quasiquoters | Stdlib |

3. **"Why would you do this?"** — For each row, a one-paragraph rationale. Not defensive — explanatory. The tone is: "We know you know Haskell. Here's why we made a different choice."

4. **Common AI mistakes for Haskell developers** — When a Haskell dev uses AI (Copilot, ChatGPT, Claude), the AI will generate standard Haskell. This section lists the top 5 patterns the AI will get wrong and how to spot/fix them. Cross-references the "Using AI" guide (`getting-started/using-ai.mdx`).

5. **"What stayed the same"** — ADTs, type classes (called Traits), `deriving`, records, modules, GHC under the hood. Reassure the reader that their Haskell knowledge isn't wasted — it's redirected.

---

## Architect & Decision-Maker Content

The Audience Model defines three personas, but the plan's content is heavily developer-focused. Architects and Decision-Makers need dedicated pages that speak to their concerns — not repurposed developer content.

### "Why NeoHaskell?" Page (`concepts/why-neohaskell.mdx`)

**Audience**: Architect, Decision-Maker
**Purpose**: The page someone sends to their CTO or tech lead. Answers "why should we bet on this?"

**Structure**:
1. **The problem**: Why event sourcing is hard with current tools (discipline-dependent, schema drift, testing friction)
2. **NeoHaskell's answer**: Compiler-enforced event sourcing — what other tools leave to discipline, NeoHaskell makes mandatory
3. **Comparison table**: NeoHaskell vs. EventStoreDB vs. Kafka + manual ES vs. Axon (honest — include weaknesses)
4. **Ecosystem maturity**: Current state, roadmap, community size — no overselling
5. **When to adopt — and when to wait**: Honest assessment of maturity, ecosystem, and team readiness (strengthens credibility). Explicitly does NOT include "your team doesn't know FP" — the testbed proves NeoHaskell code is readable by any developer. Anti-recommendations focus on ecosystem maturity, community size, production battle-testing, and hiring perception.

### Architecture Evaluation Page (`concepts/architecture-evaluation.mdx`)

**Audience**: Architect
**Purpose**: Technical deep-dive for someone evaluating NeoHaskell for a real system

**Structure**:
1. **Transaction guarantees**: What consistency model does NeoHaskell's event store provide?
2. **Performance characteristics**: Event store throughput, projection rebuild times, memory profile
3. **Scaling patterns**: Single-node vs. distributed, sharding strategies
4. **Integration story**: How NeoHaskell services talk to non-NeoHaskell services
5. **Migration path**: How to introduce NeoHaskell into an existing system (one bounded context at a time)
6. **Operational concerns**: Monitoring, alerting, backup/restore for event stores

> **Source**: Sarah Drasner review — "The Audience Model promises content for Architects and Decision-Makers, but the plan delivers almost exclusively developer content. These audiences need their own pages, not 'also read the tutorial.'"

---

## Exercise Strategy

Every tutorial page and most concept pages include exercises. Passive reading doesn't build understanding.

### Exercise Types

1. **Modify**: "Change the event type to include X. What happens when you compile?"
2. **Predict**: "What will this code output? Think first, then run it."
3. **Extend**: "Add a `DailyWithdrawalLimitSet` event. Wire it through the handler so withdrawals are declined above the limit."
4. **Break**: "Remove the type annotation. What error do you get? Why?"
5. **Compare**: "Write this same feature in [language you know]. Which version is clearer?"
6. **Audit**: "Replay all events from scratch. Does the derived balance match the current balance? What would happen if an event was missing?"
7. **Verify**: "Write a test that proves this feature works." Creates a natural build→verify feedback loop. Event sourcing's Given-When-Then pattern IS the testing grammar — use it.
8. **What If**: "What if the event store lost the last 3 events? What would happen to the balance?" Thought experiments that build intuition about event sourcing properties (immutability, replay, ordering) without requiring code. Best used in concept pages and as tutorial "pause and think" moments. (Source: Julia Evans — "What-if questions build the deepest understanding because they force the reader to simulate the system in their head.")

### "Think First" Design Challenges

Before each tutorial part reveals its solution, the reader faces a design challenge that asks them to think through the problem themselves. This activates retrieval practice and forces the reader to engage their mental model before seeing the "right" answer.

| Tutorial Part | Design Challenge | What the reader designs before seeing the answer |
|--------------|-----------------|------------------------------------------------|
| 1. First Transaction | "What events would a deposit produce?" | List the events before seeing `AccountOpened`, `MoneyDeposited` |
| 2. Account Rules | "How would you prevent overdrafts with events?" | Sketch the validation logic before seeing the `Result.err` pattern |
| 3. Transaction History | "If you have deposit and withdrawal events, how would you build a bank statement?" | Describe the projection before seeing the implementation |
| 4. Multiple Accounts | "How would you add a savings account without changing existing events?" | Propose the aggregate separation before seeing the solution |
| 5. Transfers | "What could go wrong in a transfer between accounts?" | List failure modes before seeing the saga/compensation pattern |
| 6. Audit Everything | "How would you answer 'what was the balance on March 15th?'" | Propose the replay strategy before seeing the time-travel query |

#### Placement

- Each "Think First" challenge appears at the **start** of the tutorial part, immediately after "The Requirement"
- The challenge is a callout box (`<Aside type="tip">`) with the prompt
- Reader is encouraged to write pseudocode or bullet points before scrolling down
- The tutorial then reveals the NeoHaskell solution, allowing the reader to compare their design

> **Source**: Dan Abramov review — "The reader who designs the event schema before seeing it will understand it 10x better than the reader who just reads it."

### Exercise Placement

- **Tutorial pages**: 1 Modify + 1 Extend per page (mandatory), plus 1 Verify from Part 2 onward, plus 1 "Think First" design challenge per part
- **Concept pages**: 1 Predict or 1 Break per page (optional but encouraged)
- **Tutorial Parts 3-6**: Include at least 1 Predict exercise per part (in addition to Modify/Extend/Verify). Predict exercises are especially valuable from Part 3 onward because the reader has enough context to form expectations. Example (Part 3): "Before running this code, predict: what will the monthly summary show if there were 2 deposits and 1 withdrawal?" The reader writes their prediction, runs the code, and compares. Mismatch = learning moment. (Source: Julia Evans — "Predict exercises work best when the reader knows enough to have an expectation but not enough to be certain. Part 3 is that sweet spot.")
- **Guides**: 0 exercises (guides are task-oriented, not learning-oriented)

### Exercise Solutions (non-negotiable)

Every exercise must have:
- 2-3 progressive hints in `<details>` components (graduated difficulty)
- Full solution behind a "Show solution" toggle
- Link to the tutorial companion repo branch for that section

> Exercises without solutions are frustrating. "Stuck? Here's the full code" sections dramatically improve tutorial completion rates.

### Exercise Difficulty Progression

Exercises follow a deliberate progression — the reader moves from copying to creating across the tutorial.

- **Part 1** (Copy & tweak): Copy-paste the code, run it, see the output, change one value. "Change the deposit amount to $200. Run it. What's the new balance?" The reader proves the system works.
- **Part 2** (Modify within structure): Change behavior within existing patterns. "Add a `MinimumBalance` rule — withdrawals below $10 remaining are declined." First `Verify` exercise: write a test for the rule.
- **Part 3** (Replicate a pattern): Build a similar feature from scratch. "Create a `MonthlyStatement` projection that groups transactions by month." The reader demonstrates they can apply the projection pattern independently.
- **Part 4** (Decompose & decide): Identify which pattern to apply and argue why. "Should `AccountType` be an event or a field on `AccountOpened`? Argue both sides." The reader is thinking like a designer, not a copier.
- **Part 5** (Critique & fix): Judge a given solution and find the flaw. "This transfer handler doesn't check the source balance. What goes wrong? Fix it." Write a cross-aggregate test.
- **Part 6** (Compose something new): Build something that combines multiple concepts. "Build a `FraudAlert` projection that flags accounts with 3+ declined withdrawals in 24 hours." Write a property-based test.

> By Part 6, the reader isn't following instructions — they're making design decisions. This is how we know they've internalized the mental model.

### Checkpoint Progression

Verification grows with the reader's confidence:
- **Part 1**: Manual verification — "run it, see the output"
- **Part 2**: Semi-automated — introduce the test runner, write first Given-When-Then test
- **Part 4**: Automated tests as primary verification — reader runs test suite
- **Part 6**: Reader writes their own verification from scratch — property-based testing

---

## Stuck Reader Strategy

Every tutorial page must design for failure, not just success. The #1 reason people don't finish tutorials isn't that the content is bad — they got stuck and had no way forward.

### Per-Tutorial-Page Requirements

1. **"If You're Stuck" section**: Escape hatches at the bottom of every tutorial page with 3-5 "If you see X, check Y" troubleshooting items specific to that page's content
2. **Complete code for each section**: Collapsible `<details>` with the full working code at each checkpoint — not just the final version
3. **Tutorial companion repo**: Tagged branches per part (`part-1-start`, `part-1-complete`, `part-2-start`, etc.). Reader can `git checkout part-2-start` to get a clean starting point for any section.
4. **Progressive hints**: Every exercise has 2-3 hints before the full solution (see Exercise Solutions above)

> The emotional safety net: at no point should a reader feel they have no options. Even "start this section over from the companion repo" is better than "I guess I'll quit."

---

## "Adopting NeoHaskell at Work" Guide

A dedicated guide (`guides/adopting-at-work.mdx`) addresses the pragmatic concerns of developers who want to use NeoHaskell at their company. This is NOT a marketing page — it's an honest, practical guide for navigating organizational adoption.

### Structure

1. **"Start with one bounded context"** — Don't pitch a rewrite. Pick the smallest, most event-natural domain in your system (audit logging, transaction history, notification events). Build it in NeoHaskell. Show results.
2. **"The Pilot Proposal" template** — A fill-in-the-blanks document the reader can adapt for their team lead: problem statement, proposed scope, success criteria, rollback plan, timeline.
3. **"Answering the hard questions"** — Pre-written responses to common objections:
   - "What if the developer leaves?" (NeoHaskell compiles to GHC Haskell — any Haskell dev can maintain it)
   - "What about hiring?" (The Haskell talent pool, training time, pair programming strategy)
   - "What about the ecosystem maturity?" (Honest assessment: what's stable, what's in-progress, what's missing)
   - "What if we need to migrate away?" (Event stores are portable; projections can be rewritten in any language)
4. **"Running NeoHaskell alongside existing services"** — Technical integration patterns (HTTP APIs, message queues, shared databases)
5. **"Measuring success"** — What metrics to track during the pilot (bug rate, development velocity, onboarding time for new team members)

> **Source**: Dan Abramov review — "Every developer who finishes the tutorial and wants to use NeoHaskell at work will face organizational resistance. Give them the playbook, or they'll fail at adoption and blame the tool."

---

## Visual Design Requirements

Diagrams are not optional. They're a core teaching mechanism, designed FIRST with prose written to support them.

### Required Diagrams (Phase 0)

1. **Event flow diagram**: Command → Event → Store → Projection → Read Model. The single most important diagram — shows how one action flows through the entire system. Clean SVG, not hand-drawn.
2. **Event timeline visualization**: Static SVG that grows through the tutorial. Part 1 shows 2 events; by Part 6 it shows the full history.
3. **CRUD vs. Events comparison visual**: Side-by-side showing "UPDATE balance SET 150" vs. "record MoneyDeposited $50." Used in tutorial intro and `from-crud-to-events.mdx`.

### Per-Concept-Page Visuals

Each concept page should include a "Visual Design" consideration: what would a diagram of this concept look like? Not every concept needs a diagram, but the decision should be explicit.

### Visual Design System

The visual language must be consistent, documented, and reusable — not invented per-diagram.

**Color Language** (across all diagrams, code highlights, and section accents):
- Commands = blue (`#3B82F6` / `--color-command`)
- Events = green (`#22C55E` / `--color-event`)
- Projections = orange (`#F97316` / `--color-projection`)
- Aggregates = purple (`#A855F7` / `--color-aggregate`)
- Errors/failures = red (`#EF4444` / `--color-error`)

**Section Visual Identity**: Each top-level section has a subtle visual differentiator (accent color or icon) so the reader always knows where they are:
- Getting Started = blue accent
- Tutorial = green accent
- Core Concepts = purple accent
- Guides = orange accent
- Reference = gray accent

**Diagram Style Guide**:
- Clean SVG, no hand-drawn aesthetic
- Consistent arrow styles (solid for data flow, dashed for optional/async)
- Font: system font stack matching the site
- Maximum 7 elements per diagram (Miller's law) — split complex diagrams
- Every diagram has descriptive alt text (not just "diagram")
- Diagrams are created in a tool that exports clean SVG (Figma, Excalidraw → SVG export, or hand-coded)
- Source files for diagrams stored in `src/assets/diagrams/` alongside SVG exports

**Accessible**: Must work in both light and dark mode — use CSS custom properties for colors, not hardcoded hex values.

> **Source**: Sarah Drasner review — "A consistent visual design system isn't polish — it's wayfinding. The reader should know they're in the Tutorial section before reading a single word."

---

## Mobile Experience Requirements

Documentation is read on phones — on the bus, during lunch, in bed. Mobile isn't an afterthought; it's a first-class concern.

### Code Block Constraints

- **Maximum line length**: 72 characters for ALL code examples. Longer lines cause horizontal scrolling on mobile, which breaks reading flow.
- **Code block testing**: CI should validate that no code block exceeds 72 characters per line
- **If a line must be longer**: Use a line continuation comment (`-- continued`) and break it logically

### Responsive Design

- **Tables**: Use responsive table patterns — on mobile, wide tables should either scroll horizontally with a visual indicator (shadow/fade) or collapse to a stacked layout
- **Diagrams**: All SVGs must be legible at 375px width (iPhone SE). If a diagram can't be read at that size, provide a simplified mobile version or a "tap to enlarge" pattern
- **CardGrid**: Cards stack vertically on mobile with full-width tap targets (minimum 48px height)
- **Code blocks**: Horizontal scroll with visible scroll indicator (not hidden overflow)

### Testing

- **Manual test on a real phone** before shipping any tutorial part — not just browser DevTools responsive mode
- **Target devices**: iPhone SE (375px), standard Android (360px), iPad Mini (768px)
- **Test checklist per page**: Can the reader complete the tutorial on a phone? Can they copy code blocks? Can they read diagrams? Can they navigate between parts?

> **Source**: Sarah Drasner review — "If your code examples cause horizontal scrolling on a phone, 30% of your readers just had a degraded experience. Set a max line length and enforce it."

---

## Search Experience Design

Starlight provides built-in search, but good search requires deliberate content design — not just enabling the feature.

### Searchable Headings

- Every heading should contain the words a reader would search for. "Projections" is better than "Deriving Your Views." "Error Handling" is better than "When Things Go Wrong."
- Concept pages should use the concept name in the H1: "Projections" not "Building Read Models"
- Error messages should appear as exact searchable text in Common Errors headings

### Synonym Awareness

Readers from different backgrounds search for different terms. Each concept page's frontmatter should include `keywords` (used by Starlight's search index):

```yaml
---
title: Projections
description: Deriving state from events
keywords: [projection, read model, view, query, CQRS, derived state, materialized view]
---
```

### Search Quality Testing

- After each phase, test 10 common search queries manually: "how to test," "error handling," "deployment," "what is an event," "CRUD to events," "transfer between accounts," etc.
- If a search returns no results or irrelevant results, fix the content (add headings, keywords, or redirects)

> **Source**: Sarah Drasner review — "Good search is 80% content structure and 20% search engine. If your headings use clever names instead of searchable names, no search engine will save you."

---

## Starlight Component Strategy

Starlight provides components that are not decorations — they're the difference between documentation that works and documentation that merely exists. Adopt from Phase 0.

| Component | Purpose | Where to use |
|-----------|---------|-------------|
| **Tabs** | Platform-specific instructions, "Your Language → NeoHaskell" comparisons | Installation page (OS tabs), Coming From pages |
| **Asides** (note, tip, caution, danger) | Contextual callouts without breaking narrative flow | Tutorial: `tip` for shortcuts, `caution` for common mistakes, `note` for "you'll learn more about this in Part N" |
| **CardGrid** | Visual navigation for section index pages | Core Concepts index, Guides index |
| **Code block labels** | Name what the code IS, not just its language | **Every** code block in the tutorial MUST have a file path label: `title="src/Bank/Account.hs"`. No unlabeled code blocks — the reader must always know which file they're editing. Concept pages use descriptive labels ("Example: processing a deposit"). CI should warn on unlabeled code blocks in tutorial pages. (Source: Tania Rascia — "An unlabeled code block forces the reader to guess which file it belongs to. That's a guaranteed support question.") |
| **Steps** | Numbered sequential instructions | Installation, Getting Started |
| **Badges** | Status indicators | "New in Part 3", difficulty level on guides |
| **`<Term>`** (custom) | Tooltip for domain vocabulary | First use of any term from the Rosetta Stone table. Hover/tap shows a one-line definition without leaving the page. Implementation: custom Astro component wrapping a `<abbr>` with CSS tooltip. Example: `<Term def="An immutable record of a fact that happened">event</Term>`. Build in Phase 0 — it's used from the first tutorial page onward. (Source: Steve Klabnik — "A glossary page nobody visits is less useful than a tooltip that appears exactly when the reader is confused.") |

> Adopt these from the first page written. Retrofitting component usage is harder than starting with it.

---

## Tutorial Page Template

Every tutorial page follows this structure:

```markdown
---
title: "Part N: [Title]"
description: [One sentence]
time_estimate: "~20 minutes"
---

## What You'll Learn
- [3-5 bullet points — the reader knows what they're signing up for]

## The Requirement
[Product requirement framing — what you're building this part]

## [Main content sections...]

## If You're Stuck
- If you see [X], check [Y]
- If you see [Z], try [W]
- [Full code for this section in <details> toggle]
- [Link to companion repo branch]

## Recap
- [3-5 bullet points — what you just learned]
- [Link to next part]
```

---

## Missing Patterns (Scheduled)

Patterns flagged by Martin Fowler as critical — now placed in specific phases with concrete anchors.

| Pattern | Type | Schedule | Anchor / Trigger |
|---------|------|----------|-----------------|
| **Idempotency** | Concept page | Phase 2 (with Part 5: Transfers) | Reader encounters duplicate transfer commands — idempotency becomes concrete |
| **Snapshotting** | Guide | Phase 3 (after Part 6: Audit Everything) | Reader has replayed full event history — snapshotting is the obvious optimization |
| **GDPR / Tombstones** | Guide | Phase 3 (standalone guide) | Addressed alongside `trade-offs.mdx` — "what happens when you need to forget?" |
| **Event ordering / Causality** | Concept page | Phase 2 (with Part 5: Transfers) | Cross-aggregate transfers raise ordering questions naturally |

> **Source**: Martin Fowler review — "These patterns are too important to leave unscheduled. Each has a natural tutorial anchor point — schedule them there, not in the abstract."

---

## Cross-Reference Strategy

Documentation sections are not silos. Every page should connect the reader to related content.

### Cross-Reference Requirements

- Every tutorial page links to the concept pages it introduces (Layer 3)
- Every concept page links back to the tutorial part where the concept was first encountered
- Every "Coming From..." page links to the relevant concept pages for deeper understanding
- Every guide links to prerequisite concept pages
- The `from-crud-to-events.mdx` concept page is linked from: Tutorial Part 1, Getting Started, and the Coming From CRUD page

### Cross-Reference Matrix

A formal matrix tracking links between all Diataxis modes. Maintained as an internal document (not reader-facing) to ensure no section is isolated.

| From \ To | Tutorial | Concepts | Guides | Reference | Coming From |
|-----------|----------|----------|--------|-----------|-------------|
| **Tutorial** | Next/prev part | Layer 3 concept links | "For more on testing: Testing Guide" | Rare (link to API when introducing a function) | — |
| **Concepts** | "See this in action: Part N" | Related concepts | "How to apply this: [Guide]" | "API details: [Reference]" | — |
| **Guides** | "New to this? Start with Part N" | Prerequisite concept links | Related guides | API reference for functions used | — |
| **Reference** | — | "Understand this: [Concept]" | "Use this: [Guide]" | — | — |
| **Coming From** | "Try the tutorial" | Relevant concept pages | Relevant guides | — | — |

> **Audit rule**: After each phase, verify that no page in the matrix has zero outbound links to other sections. Isolated pages are a navigation dead-end.

### Implementation

Use consistent link patterns:
- Tutorial → Concept: `[→ Deep dive: What is a projection?](/concepts/projections)`
- Concept → Tutorial: `[See this in action: Tutorial Part 3](/tutorial/03-transaction-history)`
- Coming From → Concept: `[Learn more: Events, not state](/concepts/events-not-state)`

---

## Compiler-as-Teacher Pattern

NeoHaskell's compiler is an active teaching tool. Every convention the language enforces becomes a learning opportunity — the reader deliberately triggers the guard rail, reads the error, and internalizes the rule.

### The Pattern

For each enforced NeoHaskell convention, the tutorial includes a **"Break It"** micro-exercise:

1. **Show the correct code** (reader has already written it)
2. **Instruct the reader to break it** — introduce the forbidden pattern intentionally
3. **Show the compiler error** — the exact message they'll see
4. **Explain what the error means** — translate compiler-speak to concept understanding
5. **Reader fixes it** — restoring the correct pattern with understanding, not just compliance

### Break-It Exercises by Convention

| Convention | What to break | Expected error (paraphrased) | What the reader learns | Enforced by |
|-----------|--------------|-----------------------------|-----------------------|-------------|
| `do` blocks only (no `let..in`, no `where`) | Replace a `do` block with `let..in` | "NeoHaskell uses do blocks for all bindings" | Why uniform binding syntax reduces cognitive load | Linter |
| Qualified imports (`Array.map`, not `map`) | Use an unqualified function name | "Ambiguous occurrence — did you mean Array.map or Map.map?" | Why qualification prevents name collisions | Linter |
| `Result Ok/Err` (not `Either Left/Right`) | Write `Left "error"` | "Not in scope: Left. Did you mean Result.err?" | NeoHaskell's vocabulary is intentional, not arbitrary | Compiler |
| `\|>` pipe (not `$` or nested parens) | Use `$` for function application | "NeoHaskell uses \|> for function piping" | Data flows left-to-right, like reading English | Linter |
| No point-free style | Write `Array.map show` instead of `Array.map (\x -> show x)` | "Eta-reduce — NeoHaskell requires explicit lambdas" | Readability over cleverness | Linter |
| `Module.yield` (not `pure`/`return`) | Write `pure value` | "Not in scope: pure. Did you mean Task.yield?" | NeoHaskell names things for what they do, not what they are | Compiler |

> **Enforcement level matters**: The "Enforced by" column tells the reader whether they'll get a hard compiler error or a linter warning. Compiler-enforced conventions are non-negotiable; linter-enforced conventions can theoretically be disabled (but shouldn't be). This distinction helps the reader calibrate their mental model of NeoHaskell's strictness. (Source: Steve Klabnik — "If you don't tell them whether the guardrail is a wall or a suggestion, they'll test it at the worst possible time.")

### Placement Rules

- One Break-It exercise per tutorial part (not per page — don't overdo it)
- Always AFTER the correct pattern has been used successfully at least once
- The broken code should be something AI tools commonly generate (see "Using AI" guide)
- Break-It exercises are distinct from the Exercise Strategy exercises — they target language conventions, not domain concepts

> **Why this works**: Readers who've seen the error once will recognize AI-generated wrong patterns immediately. The compiler becomes their code reviewer.

---

## Interactive Elements (Future, But Design For Them Now)

Even if not built immediately, structure content so these can be added later.

### Phase 1 (with Starlight)
- Copy buttons on all code blocks (Starlight default)
- Syntax highlighting (Starlight default)
- Search (Starlight default)
- Dark mode (Starlight default)

### Phase 2 (enhancements)
- "Run this code" buttons linking to an online REPL or playground
- Expandable "deep dive" sections for advanced details (progressive disclosure)
- Side-by-side "CRUD vs. Event Sourced" comparisons with toggle
- Interactive transaction timeline — click a deposit/withdrawal, see balance recalculate in real-time (mirrors how real banking apps display transactions)
- Balance reconciliation demo — show that replaying events from scratch produces the same balance (the "audit" concept, visualized)

### Phase 3 (ambitious)
- Embedded playground (like Svelte's tutorial)

> Structure the MDX so that code blocks tagged with `interactive` can be wired up later without rewriting content.

---

## Work Phases (Iterative, Not Waterfall)

### Phase 0 — Foundation (Week 1)

**Goal**: Buildable infrastructure and Tutorial Part 1 shipped. Streamlined per reviewer consensus — ship fast, learn from real readers.

Tasks:
- [ ] Set up code block extraction + compilation CI
- [ ] Finalize NeoBank domain model (account types, events, commands)
- [ ] Update `astro.config.mjs` sidebar to match the new section plan
- [ ] Design core visuals: event flow diagram (command → event → store → projection → read model), CRUD vs. Events comparison visual
- [ ] Design annotated code block component — Julia Evans-style arrows/callouts pointing to unfamiliar syntax within code. This is Phase 0 infrastructure because Tutorial Parts 1-3 depend on it. Options: custom Starlight component, SVG overlays, or CSS-based annotations. Decide and build before writing Part 1. (Source: Julia Evans — "The annotated code block is the single most important teaching tool for compound unfamiliarity.")
- [ ] Set up tutorial companion repo with `part-1-start` branch
- [ ] Write `getting-started/installation.mdx` — Nix setup with installation friction mitigations (see below), first `neo` command, "Hello Transactions"
- [ ] Write Tutorial Part 1 (`01-first-transaction.mdx`) — this IS the priority deliverable
- [ ] Write `getting-started/cheat-sheet.mdx` — "I want to... / Write this..." quick reference. Two-column format. Printable. Linked from Part 1 as "your NeoHaskell phrasebook."
- [ ] Test Part 1 with someone who's never seen NeoHaskell (observe, don't help). Fix everything.

> Moved to Phase 1: "Validate 20 misconceptions" (happens during writing), "Study reference docs" (already done as part of review process), "Create ARCHITECTURE.md" (can happen alongside content). The priority is: **ship Tutorial Part 1, watch someone use it, fix everything.**

**Output**: CI pipeline, confirmed example domain, updated site config, Installation page, Tutorial Part 1, cheat sheet, core diagrams, companion repo.

### Phase 1 — Quick Start + First Concept Pages (Week 2)

**Goal**: Complete Quick Start flow and first concept pages driven by Tutorial Part 1.

Tasks:
- [ ] Write `getting-started/reading-neohaskell.mdx` — 5-minute annotated code walkthrough ("Reading NeoHaskell"). Julia Evans style — visual, friendly, zero prerequisites.
- [ ] Write `getting-started/syntax-warmup.mdx` — 5-minute hands-on warm-up where the reader types 3-5 small NeoHaskell expressions and sees output. NOT a language tutorial — just enough muscle memory that the tutorial's syntax doesn't feel alien. Covers: `do` blocks, `|>` pipe, type annotations, `case..of`. Each example is 1-3 lines with immediate REPL output. (Source: Tania Rascia — "Let the reader's fingers learn the syntax before their brain has to learn the concepts.")
- [ ] Write `getting-started/first-events.mdx` — Define an event type, deposit money, see balance derived from events
- [ ] Write `getting-started/using-ai.mdx` (moved from guides/) — Copy-paste NeoHaskell prompt for AI tools, the 5 most common AI mistakes, how to spot wrong patterns. Placed in Getting Started because readers using AI copilots need this BEFORE writing their first line of code, not after finishing the tutorial. (Source: Sarah Drasner — "If 70% of developers use AI while coding, the AI guide belongs in Getting Started, not buried in Guides.")
- [ ] Write first JIT concept pages: `events-not-state.mdx`, `commands-and-handlers.mdx`, `from-crud-to-events.mdx`
- [ ] Validate the 20 misconceptions list with 5+ real developers (moved from Phase 0)
- [ ] Create `ARCHITECTURE.md` documenting the Diataxis-based structure decision (moved from Phase 0)
- [ ] Every code block compiles in CI
- [ ] Verify the Layer System works — first-events.mdx must have all 3 layers (narrative, bold-on-first-use syntax, concept links)

**Output**: Complete Quick Start path, first concept pages, AI guide, validated misconceptions.

**Why this is Phase 1, not Phase 4**: You discover 80% of structural problems by writing the first real pages. Writing early is research.

### Phase 2 — Tutorial Spine (Weeks 3-5)

**Goal**: NeoBank tutorial, parts 1-4. The complete beginner journey.

Tasks:
- [ ] Write tutorial parts 1-4 following the NeoBank progression table
- [ ] Each part has: narrative, code examples, 2 exercises, checkpoint
- [ ] Each part's code builds on the previous (reader has a growing project)
- [ ] Test each part with a fresh reader after writing
- [ ] Write the "Core Concepts" pages that the tutorial pages reference (just-in-time)

**Output**: A developer can go from zero to "I built a working event-sourced bank with transaction history, overdraft protection, and transfers."

**Note on concept pages**: Write concept pages AS NEEDED by the tutorial, not ahead of time. If tutorial part 2 requires understanding "what is an aggregate," write the aggregate concept page then. The Account is the natural first aggregate — it's a concept every reader already intuitively understands. Don't pre-write concepts you haven't needed yet.

### Phase 3 — Breadth (Weeks 6-8)

**Goal**: Fill in the remaining sections so the docs feel complete.

Tasks:
- [ ] Write tutorial parts 5-6 (cross-account transfers, audit trail)
- [ ] Write "Coming From..." pages (JS/TS and Haskell first, others based on traffic)
- [ ] Write 3-5 standalone guides (testing, deployment, PostgreSQL event store, error handling, integrating with existing systems)
- [ ] Generate reference docs from source code
- [ ] Write "Common Errors" page — **decision tree format** (not just error→fix pairs). Structure: "What were you trying to do?" → "What error did you see?" → Fix. This matches how readers actually search for help — they know their intent, then see an error, then need a fix. Group by intent (e.g., "I was trying to handle a command," "I was trying to build a projection") rather than alphabetically by error message. Include the exact error text as searchable headings. (Source: Julia Evans — "Error pages organized by error message assume the reader understands the error. They don't. Organize by what they were trying to do.")
- [ ] Write `guides/reading-error-messages.mdx` — A micro-guide (2-3 pages) showing how to read NeoHaskell compiler errors. Takes 3-5 real error messages, annotates each part (what went wrong, where to look, how to fix), and teaches the general skill of parsing compiler output. Julia Evans annotated-diagram style. This is a standalone guide, not part of the tutorial — but the tutorial links to it from the first Break-It exercise. (Source: Julia Evans — "Reading error messages is a teachable skill that most tutorials assume readers already have. They don't.")
- [ ] Write "FAQ" addressing remaining misconceptions not covered elsewhere

**Output**: A documentation site that serves all three audience entry paths.

### Phase 4 — Polish and Feedback (Ongoing)

**Goal**: Continuous improvement based on real usage.

Tasks:
- [ ] Add analytics to identify high-bounce pages (people who land and leave)
- [ ] Add "Was this page helpful?" feedback mechanism
- [ ] Monitor Discord for recurring questions — each one is a documentation failure
- [ ] Write the second example domain (logistics/shipment tracking) for advanced guides
- [ ] Add interactive elements (Phase 2 of interactive strategy)

**Output**: Docs that measurably improve over time.

---

## Iteration Protocol

Documentation is never done. Build these feedback loops in from day one.

### After Every Page

1. Does the code compile? (CI verifies)
2. Can a fresh reader follow it without help? (test with 1 person)
3. Does it address at least one misconception from the list?
4. Is the entry/exit mental model stage clear?

### After Every Phase

1. What questions are people still asking in Discord?
2. Which pages have high bounce rates?
3. What did the test readers struggle with?
4. What structural assumptions turned out to be wrong? (update the plan)

### Monthly

1. Check source hash drift (are code examples still current?)
2. Review and update misconceptions list
3. Assess whether the "Coming From..." pages need new entries

---

## i18n Quality Plan

Translations are auto-generated by GitHub Action, but auto-generated doesn't mean quality-assured. Bad translations actively harm credibility in non-English markets.

### Quality Tiers

| Language | Tier | Quality Standard | Review Process |
|----------|------|-----------------|----------------|
| Spanish (es) | 1 — High priority | Native speaker review of tutorial + getting started | Community reviewer assigned; review before merge |
| Japanese (ja) | 1 — High priority | Native speaker review of tutorial + getting started | Community reviewer assigned; review before merge |
| Russian (ru) | 2 — Medium priority | Spot-check key pages | Community volunteer; review when available |
| French (fr) | 2 — Medium priority | Spot-check key pages | Community volunteer; review when available |
| Armenian (hy) | 3 — Best effort | Auto-generated only | No review process (too small a contributor base) |

### Code Example Handling

- Code examples are NOT translated (code is universal)
- Comments within code blocks ARE translated
- Error messages shown in the tutorial stay in English (they're what the reader will actually see)
- Surrounding prose explaining the error IS translated

### Translation Staleness Detection

- When English source changes, the GitHub Action should mark the corresponding translation as "needs update" (via a frontmatter field or a tracking file)
- Stale translations display a banner: "This page may be outdated. [View the English version](/en/...)"

> **Source**: Sarah Drasner review — "Auto-generated translations with no quality process are worse than no translations — they signal that you don't care about non-English speakers."

---

## File Structure

```
src/content/docs/
  index.mdx                          # Landing page (existing)

  getting-started/
    index.mdx                        # Section overview
    installation.mdx                 # Nix, tooling, "hello world"
    reading-neohaskell.mdx           # 5-min annotated code walkthrough ("Reading NeoHaskell")
    syntax-warmup.mdx                # 5-min hands-on syntax warm-up: type the code, see the output (before tutorial)
    cheat-sheet.mdx                  # "I want to... / Write this..." quick reference (printable)
    first-events.mdx                 # Define event, store it, read it
    using-ai.mdx                     # MOVED from guides/ — AI prompt, common AI mistakes, how to spot wrong patterns (readers need this before the tutorial, not after)

  tutorial/
    index.mdx                        # "Meet Alex — a developer like you." (Alex only here, "you" from Part 1)
    01-first-transaction.mdx         # CRUD failure moment, then events. Deposit money, derive balance.
    02-account-rules.mdx             # Overdraft protection, declined commands (Result.err), first test
    03-transaction-history.mdx       # TWO projections from same events (statement + monthly summary)
    04-multiple-accounts.mdx         # Multiple aggregates (NOT bounded contexts)
    05-transfers.mdx                 # Cross-aggregate transfers, saga with failure/compensation
    06-audit-everything.mdx          # Event replay, time-travel, audit trail, production scope note
    whats-next.mdx                   # Post-tutorial transition: celebrate, branch (3 paths), community, reference bookmark

  concepts/
    index.mdx                        # "How to read this section"
    why-neohaskell.mdx               # For Architects/Decision-Makers: comparison, tradeoffs, when NOT to use
    architecture-evaluation.mdx      # For Architects: performance, scaling, integration, migration, operations
    events-not-state.mdx             # Core mental model shift
    commands-and-handlers.mdx        # Input processing
    from-crud-to-events.mdx          # ELEVATED: standalone page (was Coming From subpage) — the core teaching doc
    projections.mdx                  # Deriving state from events
    type-safety.mdx                  # Why the compiler is your friend
    testing-event-sourced-systems.mdx # Given-When-Then, the testing trophy for ES. Includes a "Testing Trophy" diagram adapted for event sourcing: base = type safety (free from compiler), middle = unit tests (Given-When-Then for aggregates), upper = integration tests (projection verification), top = E2E (full event replay). The diagram makes the testing strategy visual and shows WHY event-sourced systems are easier to test than CRUD. (Source: Kent C. Dodds — "The Testing Trophy gives readers a mental model for which tests to write and why, preventing the common mistake of testing everything at the E2E level.")
    schema-evolution.mdx             # MOVED from Advanced to Core — too important to defer
    bounded-contexts.mdx             # System decomposition (linked from Part 5, not Part 4)
    effects.mdx                      # How NeoHaskell handles side effects
    thinking-in-events.mdx           # The "Thinking in React" equivalent — capstone mental model
    trade-offs.mdx                   # Honest: when NOT to use event sourcing
    concurrency.mdx                  # Channels, locks, async

  guides/
    index.mdx                        # "Pick what you need"
    # using-ai.mdx moved to getting-started/ — readers need AI guidance before the tutorial, not after
    testing.mdx                      # Testing event-sourced systems
    deployment.mdx                   # From code to running service
    postgresql-event-store.mdx       # Production event storage
    error-handling.mdx               # Error patterns in NeoHaskell
    integrating-existing-systems.mdx # Incremental adoption
    adopting-at-work.mdx             # Pragmatic guide for introducing NeoHaskell at your company
    reading-error-messages.mdx       # Micro-guide: how to read NeoHaskell compiler errors (annotated real examples)
    common-errors.mdx                # Error message -> fix lookup table (decision tree format)

  coming-from/
    index.mdx                        # "Find your background"
    javascript.mdx
    python.mdx
    go.mdx
    haskell.mdx
    crud.mdx                         # Short version — links to concepts/from-crud-to-events.mdx for full treatment
    event-sourcing.mdx

  reference/
    index.mdx                        # "How to read this reference"
    # (generated from source code, hand-written intro only)

  adrs/
    index.mdx                        # Architecture Decision Records
```

---

## What We're NOT Doing (And Why)

| Rejected approach | Why |
|-------------------|-----|
| Analyzing more documentation sites beyond the 12 already reviewed | We analyzed 12 sites during research. Diminishing returns from further analysis. Time is better spent on learner research and shipping content. |
| Separate `research/` and `structure/` directories | Intermediate artifacts that rot. Ship real content instead. |
| Writing all skeletons before any prose | Creates false sense of progress. Write real pages, discover structure. |
| One linear flow for all audiences | Decision-makers, architects, and developers have different needs. |
| Hand-writing all reference docs | Generate from source. Hand-write only what needs narrative. |
| Phase-gated approach (finish Phase N before starting N+1) | Iterate. Write → test → learn → restructure → write. |
| Separate "Learn NeoHaskell" section before the tutorial | Forces readers through a grammar chapter before they have context. Language is learned by using it, not by studying it. The tutorial teaches syntax implicitly; concept pages teach it explicitly. |
| Formal grammar reference page | NeoHaskell is a dialect, not a new language. A grammar spec signals "this is academic" instead of "this is practical." The cheat sheet and concept pages cover everything a developer needs. |
| Beginner / Intermediate / Advanced language tiers | Artificial divisions that create anxiety ("Am I intermediate yet?"). Instead, the tutorial progression naturally moves from simple to complex syntax. The exercise difficulty progression handles scaling without labeling readers. |

---

## Success Metrics

How we know the docs are working:

**Behavioral metrics:**
1. **Time to first deposit**: A new developer goes from zero to depositing $100 and seeing their balance derived from events — under 10 minutes.
2. **Tutorial completion rate**: >50% of people who start tutorial part 1 finish part 4. Part 4 is the "bank statement" moment — where projections click. This is the critical retention point.
3. **Discord question reduction**: Recurring questions decrease month-over-month.
4. **Contributor PRs**: At least 1 external docs PR per month (evidence that docs are contributor-friendly).
5. **Search satisfaction**: People who use site search find what they need (low "search → bounce" rate).
6. **"I built a bank" effect**: Tutorial readers describe NeoBank as "impressive" or "real" in feedback — evidence that the domain creates the right emotional response.

**Understanding metrics** (not just behavior):
7. **Mental model transfer**: After completing the tutorial, can the reader model a non-banking domain in events without guidance? (Test with 5 users post-tutorial.)
8. **Explanation ability**: Can the reader explain to someone else why events are stored instead of state? If they can teach it, they understand it.
9. **Pattern recognition**: When shown a new feature request, does the reader instinctively think "what events does this produce?" rather than "what table do I update?"

---

## Open Questions (To Resolve During Phase 0)

1. **Is Nix the only installation path?** If yes, the "Getting Started" page must make Nix feel effortless. If no, document alternatives.
2. **What's the CLI tool?** The tutorial assumes a `neo` CLI. What's the actual developer command?
3. **What's compilable today?** The tutorial can only teach what actually works. Scope NeoBank to current NeoHaskell capabilities.
4. **How mature is the PostgreSQL event store?** The website says "in progress." If it's not ready, the tutorial should use the in-memory store and the guide should say "coming soon" honestly.
5. **What does NeoHaskell's error output actually look like?** The claim is "friendly errors." The docs should showcase real error messages. Are they actually friendly yet?
6. **Decimal precision**: Does NeoHaskell have a Decimal/Money type, or will the tutorial use integers (cents)? Using floats for money in a banking tutorial would undermine credibility instantly.
7. **Concurrency in transfers**: Tutorial part 5 (transfers) implies two aggregates updating. Does NeoHaskell currently support sagas or process managers? If not, scope part 5 to what's available.
8. **The "compliance" claim**: Part 6 says "I accidentally built a compliance engine." Is this defensible? If NeoHaskell's event store doesn't have tamper-proof guarantees, soften this to "audit trail" rather than "compliance."
