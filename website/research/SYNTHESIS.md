# Documentation Research Synthesis

This synthesis identifies cross-cutting patterns from 12 world-class documentation sites (Stripe, Tailwind, React, Vue, Rust Book, Go, Svelte, Elixir/Phoenix, Deno, Laravel, FastAPI, Astro) and extracts concrete, prioritized recommendations for NeoHaskell's documentation.

## Cross-Cutting Patterns

### Pattern 1: Tutorial/Reference Separation (Universal)
**Sites using this:** React, Vue, Svelte, Go, FastAPI, Astro, Elixir/Phoenix (7/12)

**What it is:** Strict separation between learning-oriented content (tutorials) and lookup-oriented content (reference docs). Tutorials are linear, narrative-driven, and project-based. Reference docs are non-linear, comprehensive, and organized for random access.

**Why it works:**
- Different mental modes require different structures
- Tutorials teach through building; reference docs document through explaining
- Mixing them creates hybrid pages that fail at both purposes
- Learners can progress through tutorials without reference clutter
- Experienced users can find API details without tutorial narrative

**Evidence:**
- **Svelte:** Separate `/tutorial` (interactive, linear) and `/docs` (comprehensive, random access)
- **React:** "Learn" section (hub-and-spoke tutorials) vs "Reference" (API docs)
- **Go:** "Tour of Go" (interactive) vs "Effective Go" (style guide) vs pkg.go.dev (API reference)
- **FastAPI:** Linear tutorial (30+ pages) vs API reference vs "How To" recipes

**NeoHaskell application:** Create separate `/tutorial` path for NeoBank project (linear, hands-on) and `/docs` for reference (comprehensive, searchable). Link bidirectionally but maintain distinct hierarchies.

---

### Pattern 2: Minimal Working Example First (Universal)
**Sites using this:** All 12 sites

**What it is:** Every tutorial/guide page starts with the smallest possible working code (5-15 lines) that demonstrates the concept. Explanation comes AFTER the working example.

**Why it works:**
- Immediate success builds confidence
- Concrete code beats abstract explanation
- Readers can copy-paste and verify it works before understanding why
- Reduces "does this actually work?" doubt
- Creates feedback loop: see code → run code → see result → understand concept

**Evidence:**
- **FastAPI:** 5-line Hello World before explaining anything
- **Rust Book:** Chapter 2 builds guessing game incrementally, each stage compiles
- **Stripe:** "Set up development environment" shows working checkout in 3 steps
- **Svelte:** Tutorial starts with editable code + live preview, minimal text
- **Astro:** Tutorial Unit 1 deploys a site before explaining components

**NeoHaskell application:** NeoBank tutorial should start with 10-line account creation that compiles and runs. Show the event stream output immediately. Explain event sourcing AFTER readers see it working.

---

### Pattern 3: Progressive Complexity Through Incremental Building (Universal)
**Sites using this:** All 12 sites

**What it is:** Each tutorial step adds ONE new concept to existing code. The same example evolves across multiple pages, building complexity gradually.

**Why it works:**
- Cognitive load stays manageable (one new thing at a time)
- Readers see how features compose
- Context is preserved (not starting from scratch each time)
- Natural progression from simple to complex
- Reinforces previous concepts while introducing new ones

**Evidence:**
- **FastAPI:** Page 1 shows basic endpoint, Page 2 adds path parameters, Page 3 adds query parameters, Page 4 adds request body
- **Rust Book:** Chapter 2 guessing game adds features incrementally (input → random number → comparison → loop → error handling)
- **Stripe:** "Accept a payment" guide builds from basic checkout to prefilled data to webhook handling
- **React:** Quick Start introduces components, then JSX, then props, then state, each building on previous

**NeoHaskell application:** NeoBank tutorial progression:
1. Create account (basic command)
2. Deposit money (command with validation)
3. Withdraw money (business rule: sufficient funds)
4. Transfer money (multiple aggregates)
5. Account history (query events)
6. Time travel debugging (replay to specific point)

---

### Pattern 4: Checkpoint/Verification Sections (9/12 sites)
**Sites using this:** Stripe, Rust Book, FastAPI, Elixir/Phoenix, Astro, Go, Svelte, Laravel, Deno

**What it is:** After each major step, a "Verify your progress" or "Check it" section with exact commands and expected output. Readers confirm they're on track before proceeding.

**Why it works:**
- Prevents readers from getting lost in long tutorials
- Immediate feedback on correctness
- Builds confidence through frequent wins
- Catches errors early (before they compound)
- Reduces "am I doing this right?" anxiety

**Evidence:**
- **Stripe:** "Confirm your endpoint" sections with exact curl commands and expected HTTP responses
- **FastAPI:** "Check it" sections showing exact URL and JSON response
- **Rust Book:** Shows exact compiler output after each step
- **Elixir/Phoenix:** "Let's give it a try" with exact IEx commands and output

**NeoHaskell application:** After each NeoBank tutorial step:
```markdown
### Verify Your Progress

Run the REPL:
\`\`\`bash
neo repl
\`\`\`

Execute this command:
\`\`\`haskell
> deposit account1 100
\`\`\`

Expected output:
\`\`\`
Account balance: $100
Events: [AccountCreated, MoneyDeposited 100]
\`\`\`

If you see errors, check that:
- Your `deposit` function matches the example
- You've imported the `Money` type
```

---

### Pattern 5: Teaching Unfamiliar Concepts Through Familiar Tasks (8/12 sites)
**Sites using this:** FastAPI, Rust Book, Elixir/Phoenix, React, Vue, Svelte, Deno, Go

**What it is:** Don't explain the unfamiliar concept abstractly. Teach it by building something familiar, letting the concept emerge naturally through usage.

**Why it works:**
- Reduces cognitive load (familiar task + unfamiliar tool)
- Concept becomes "the natural way to solve this problem"
- Readers learn by doing, not by studying
- Benefits become concrete, not theoretical
- Avoids "why should I care?" resistance

**Evidence:**
- **FastAPI:** Teaches type hints (unfamiliar to Python devs) by building an API (familiar). Never says "let's learn type hints," just uses them naturally.
- **Rust Book:** Teaches ownership (unfamiliar) through building a guessing game (familiar). Chapter 2 uses ownership before explaining it.
- **Elixir/Phoenix:** Teaches processes/OTP (unfamiliar) by building a chat app (familiar). Processes emerge as the solution to real-time messaging.
- **React:** Teaches hooks (unfamiliar to class-component devs) by building a tic-tac-toe game (familiar).

**NeoHaskell application:** Teach event sourcing (unfamiliar) by building a bank account system (familiar). Don't start with "What is event sourcing?" Start with "Let's build a bank account." Event sourcing emerges as the natural way to track transactions, provide audit trails, and enable time-travel debugging.

---

### Pattern 6: Compiler/Error Messages as Teaching Moments (7/12 sites)
**Sites using this:** Rust Book, Svelte, Elixir/Phoenix, TypeScript (via React/Vue), Deno, FastAPI, Go

**What it is:** Show actual compiler errors, explain what they mean, and demonstrate the fix. Treat errors as learning opportunities, not failures.

**Why it works:**
- Builds error literacy (readers learn to read error messages)
- Reduces fear of compiler (it's a teacher, not an adversary)
- Teaches the "why" behind language rules
- Prepares readers for real development (they'll see these errors)
- Turns frustration into understanding

**Evidence:**
- **Rust Book:** Shows full error output with caret pointing to problem, includes help text, explains ownership rules
- **Svelte:** Dedicated pages for compiler errors, warnings, runtime errors with explanations and fixes
- **FastAPI:** Shows validation error JSON when you pass wrong type, teaches validation through error messages
- **Deno:** Shows error messages inline in docs before users encounter them

**NeoHaskell application:** Create "Break It" exercises where readers intentionally trigger compiler errors:
```markdown
## Exercise: What Happens Without Validation?

Try to withdraw more money than the account has:

\`\`\`haskell
> withdraw account1 1000  -- account only has $100
\`\`\`

You'll see this error:
\`\`\`
error[NH0042]: Insufficient funds
  --> src/Account.nh:23:5
   |
23 |     withdraw amount account
   |     ^^^^^^^^^^^^^^^^^^^^^^^ attempted to withdraw $1000 from account with balance $100
   |
   = help: Check account balance before withdrawal
   = note: Event sourcing prevents invalid state transitions
\`\`\`

This error teaches you that NeoHaskell enforces business rules at compile time. The type system prevents you from creating invalid events.
```

---

### Pattern 7: Quick Reference Tables (8/12 sites)
**Sites using this:** Tailwind, Stripe, Vue, Deno, Laravel, Go, FastAPI, Astro

**What it is:** Start reference pages with scannable tables mapping inputs to outputs (class names → CSS, commands → results, functions → descriptions). Detailed examples come after.

**Why it works:**
- Experts can scan and leave (no scrolling through prose)
- Beginners can see the full scope before diving in
- Tables are faster to scan than paragraphs
- Provides "cheat sheet" value
- Supports both learning and lookup use cases

**Evidence:**
- **Tailwind:** Every utility page starts with table: `p-<number>` → `padding: calc(var(--spacing) * <number>)`
- **Deno:** "Node to Deno Cheatsheet" table: `node file.js` → `deno file.js`
- **Vue:** API preference toggle shows both Options and Composition API in table format
- **Stripe:** Testing tables with test card numbers and expected outcomes

**NeoHaskell application:**
```markdown
## Account Functions

| Function | Type Signature | Description |
|----------|---------------|-------------|
| `createAccount` | `AccountId -> IO Account` | Creates a new bank account |
| `deposit` | `Amount -> Account -> IO Account` | Deposits funds into account |
| `withdraw` | `Amount -> Account -> IO (Maybe Account)` | Withdraws funds if sufficient balance |
| `transfer` | `Amount -> Account -> Account -> IO (Account, Account)` | Transfers funds between accounts |
| `getBalance` | `Account -> Amount` | Returns current account balance |
| `getHistory` | `Account -> [Event]` | Returns all events for account |
```

---

### Pattern 8: Multi-Language/Multi-Path Support (10/12 sites)
**Sites using this:** Stripe, Vue, Deno, Laravel, Astro, Go, Svelte, React, FastAPI, Tailwind

**What it is:** Show code examples in multiple languages/styles side-by-side OR provide multiple installation/setup paths. Let users choose their context.

**Why it works:**
- Respects developer preferences (tooling, background)
- Reduces "this doesn't apply to me" friction
- Shows equivalence across approaches
- Helps migration from other languages/frameworks
- Inclusive design (no "one true way")

**Evidence:**
- **Stripe:** Language switcher adapts entire page (Ruby/Python/Go/Java)
- **Vue:** API preference toggle (Options vs Composition API) persists across pages
- **Deno:** "Coming from Node.js" page with side-by-side comparisons and cheatsheet
- **Laravel:** Platform-specific tabs (macOS/Windows/Linux) for installation
- **Go:** Package manager tabs (npm/pnpm/yarn/bun)

**NeoHaskell application:**
1. **"Coming From..." pages:** JavaScript, Python, Go, Haskell with side-by-side code comparisons
2. **Installation paths:** GHCup (Haskell devs), Installer (new devs), Docker (ops teams)
3. **Build tool tabs:** Stack vs Cabal examples
4. **Syntax comparisons:** Show NeoHaskell alongside equivalent Haskell for migration

---

### Pattern 9: Visual Aids for Abstract Concepts (7/12 sites)
**Sites using this:** Rust Book, Elixir/Phoenix, React, Svelte, Astro, Tailwind, Stripe

**What it is:** Use diagrams, memory layouts, flow charts, or visual examples to explain abstract concepts before showing code.

**Why it works:**
- Visual processing is faster than text processing
- Diagrams show relationships that prose obscures
- Memory/architecture diagrams make invisible concepts visible
- Reduces cognitive load (picture worth 1000 words)
- Appeals to visual learners

**Evidence:**
- **Rust Book:** SVG diagrams showing stack vs heap, ownership moves, borrowing
- **Elixir/Phoenix:** ASCII diagrams of client-server-PubSub message flow
- **React:** Component tree diagrams, data flow animations
- **Tailwind:** Visual boxes showing padding/margin effects

**NeoHaskell application:** Create diagrams for event sourcing:
```
Command → Aggregate → Event → Event Store
                               ↓
                          Projection → Read Model → Query
```

Show multiple projections from same event stream to demonstrate CQRS power. Use visual timeline to show event replay and time-travel debugging.

---

### Pattern 10: Inline Glossary/Tooltips (5/12 sites)
**Sites using this:** Stripe, React, Vue, Elixir/Phoenix, Astro

**What it is:** Hover tooltips or inline definitions for domain-specific terms. Readers get definitions without leaving the page.

**Why it works:**
- Reduces cognitive load (no context switching)
- Prevents "what does that mean?" interruptions
- Supports both beginners (need definitions) and experts (can ignore)
- Keeps narrative flow intact
- Builds shared vocabulary

**Evidence:**
- **Stripe:** Hover tooltips for "sandbox," "fulfill," "webhook"
- **React:** `<DeepDive>` expandable sections for advanced explanations
- **Vue:** Inline links to concept explanations with "Learn more about X"
- **Elixir/Phoenix:** Inline definitions: "We call these modules contexts"

**NeoHaskell application:** Mark up event sourcing terms with tooltips:
```markdown
When a *command* (A command is a request to change application state, like "create account" or "deposit money") is processed, it produces one or more *events* (An event is an immutable record of something that happened, like "account created" or "money deposited").
```

Render as interactive tooltips in live docs.

---

### Pattern 11: "See Also" / Cross-Reference Density (Universal)
**Sites using this:** All 12 sites

**What it is:** Heavy cross-linking between related pages. Every page links to 5-10 related concepts, guides, or reference docs.

**Why it works:**
- Supports non-linear learning (readers jump based on interest)
- Connects tutorial → guide → reference → concept
- Prevents dead ends ("what do I read next?")
- Builds mental map of documentation structure
- Enables discovery of related features

**Evidence:**
- **Stripe:** Every guide ends with "See also" links to related guides
- **React:** Inline links to related concepts, "Read X to learn more" sections
- **Laravel:** Aggressive cross-linking (every page links to 5-10 related pages)
- **Astro:** Breadcrumb navigation + "On this page" sidebar + inline links

**NeoHaskell application:** End each tutorial section with:
```markdown
## See Also

- **Tutorial:** [Build the Transaction Module](/tutorial/transactions) - Continue the NeoBank tutorial
- **Concept:** [Event Sourcing Explained](/concepts/event-sourcing) - Deep dive into event sourcing
- **How-to:** [Implement Custom Validation](/guides/validation) - Add business rules
- **Reference:** [Account API](/reference/account) - Complete API documentation
```

---

### Pattern 12: Progressive Disclosure via "Optional" or Expandable Sections (9/12 sites)
**Sites using this:** Stripe, React, Vue, Svelte, Rust Book, FastAPI, Astro, Elixir/Phoenix, Go

**What it is:** Mark advanced features as "Optional:" or hide them in expandable sections. Readers can skip to get basic functionality working, then return for advanced features.

**Why it works:**
- Prevents overwhelming beginners with advanced content
- Lets readers get to "working code" faster
- Supports both "just make it work" and "understand deeply" learners
- Reduces perceived complexity
- Enables incremental learning

**Evidence:**
- **Stripe:** "Optional: Prefill customer data," "Optional: Save payment method details"
- **React:** `<DeepDive>` expandable sections for "How does React know which state to return?"
- **Vue:** "Advanced" subsections at end of pages
- **Rust Book:** Chapter 4 splits ownership into 4.1 (basics), 4.2 (references), 4.3 (slices)

**NeoHaskell application:** Mark advanced event sourcing topics as optional:
```markdown
## Optional: Optimize with Snapshots

For accounts with thousands of events, replaying the entire event stream can be slow. Snapshots let you cache current state and replay only recent events.

<details>
<summary>Show implementation</summary>

[Snapshot implementation details...]
</details>
```

---

## The "Best of" for Each NeoHaskell Section

### Quick Start / Getting Started

**Best practices from the 12 sites:**

1. **Multiple entry paths** (Laravel, Deno, Astro)
   - Haskell developers: "Install via GHCup"
   - New developers: "Download NeoHaskell installer"
   - Docker users: "Use official image"

2. **Platform-specific tabs** (Laravel, Astro, Go)
   - macOS, Linux, Windows installation commands
   - Use Starlight's tabbed code blocks

3. **Immediate success** (FastAPI, Svelte, Astro)
   - 5-minute "Hello World" that compiles and runs
   - No configuration required
   - Deploy to web in first tutorial

4. **"What you'll learn" upfront** (Stripe, React, Astro)
   - Set expectations before starting
   - Estimated time to complete
   - Prerequisites clearly stated

5. **Verification checkpoints** (Stripe, Rust Book, FastAPI)
   - "Run this command, see this output"
   - Confirms setup worked before proceeding

**NeoHaskell Quick Start structure:**
```markdown
# Quick Start

**What you'll learn:** Install NeoHaskell, create your first project, deploy to the web
**Time:** 10 minutes
**Prerequisites:** None (we'll install everything)

## Install NeoHaskell

[Tabs: macOS | Linux | Windows | Docker]

## Create Your First Project

\`\`\`bash
neo new my-first-app
cd my-first-app
neo run
\`\`\`

## Verify It Works

Open http://localhost:3000. You should see "Hello, NeoHaskell!"

## Deploy to the Web

\`\`\`bash
neo deploy
\`\`\`

Your app is now live at https://my-first-app.neo.app

## Next Steps

- [Build a REST API](/tutorial/rest-api) - Learn by building
- [Understand the Basics](/concepts/fundamentals) - Core concepts
```

---

### Tutorial (NeoBank)

**Best practices from the 12 sites:**

1. **Separate from reference docs** (Svelte, React, Go, FastAPI)
   - Dedicated `/tutorial` path
   - Linear progression (can't skip ahead)
   - Narrative-driven, project-based

2. **Interactive code examples** (Svelte, Astro, React)
   - Editable code with live preview
   - "Solve" button reveals solution
   - Immediate feedback loop

3. **Tiny incremental lessons** (Svelte, FastAPI, Astro)
   - 1 concept per lesson
   - 2-3 paragraphs of explanation
   - 1 exercise per lesson

4. **Progress tracking** (Astro, Svelte)
   - Visual progress indicator
   - Checkbox-based completion
   - "Unit 1 (3/6 Complete)"

5. **Checkpoint pages** (Astro, Rust Book)
   - "Check in: Unit 2" summary pages
   - Recap what was learned
   - Preview what's next

6. **Minimal working example first** (FastAPI, Rust Book, Stripe)
   - 5-10 lines of code that compiles
   - Run it immediately
   - Explain after it works

7. **Progressive complexity** (All sites)
   - Each lesson adds ONE feature
   - Same NeoBank project evolves
   - Reinforces previous concepts

8. **Real-world domain** (React, FastAPI, Stripe)
   - No "foo/bar" examples
   - Realistic use case (banking)
   - Consistent domain across all lessons

**NeoHaskell Tutorial structure:**

**Unit 0: Welcome** (2 lessons)
- What you'll build
- About this tutorial

**Unit 1: Setup** (4 lessons)
- Install NeoHaskell
- Create NeoBank project
- Run your first command
- Deploy to web

**Unit 2: Core Concepts** (6 lessons)
- Create an account (basic command)
- Deposit money (command with validation)
- Withdraw money (business rule)
- Check balance (query)
- View history (event log)
- Understand events (concept explanation)

**Unit 3: Advanced Features** (5 lessons)
- Transfer between accounts (multiple aggregates)
- Add overdraft protection (complex business rule)
- Build account summary (projection)
- Time-travel debugging (replay events)
- Test your code (property-based testing)

**Unit 4: Production** (4 lessons)
- Add authentication
- Connect to database
- Deploy to production
- Monitor and debug

---

### Core Concepts

**Best practices from the 12 sites:**

1. **Concept-first, code-second** (React, Elixir/Phoenix, Rust Book)
   - Explain the mental model
   - Use analogies and diagrams
   - Show code as illustration, not primary content

2. **"Why?" sections** (Vue, Elixir/Phoenix, React)
   - Explain motivation before mechanism
   - "Why event sourcing?" before "How to implement event sourcing"

3. **Visual aids** (Rust Book, Elixir/Phoenix, React)
   - Diagrams showing architecture
   - Flow charts for processes
   - Memory/state diagrams

4. **Explicit mental models** (React, Rust Book, Elixir/Phoenix)
   - Name the concepts: "Events as Facts," "Projections as Views"
   - Reinforce names across pages
   - Build shared vocabulary

5. **Contrast with familiar** (Rust Book, Deno, Elixir/Phoenix)
   - "In CRUD, you UPDATE. In event sourcing, you APPEND."
   - Side-by-side comparison tables
   - Explicit "This is different" callouts

6. **Deep dives as expandable** (React, Vue, Stripe)
   - Main content stays focused
   - Advanced details in expandable sections
   - Readers opt-in to complexity

**NeoHaskell Concepts structure:**

**Event Sourcing**
- What is event sourcing? (with banking analogy: ledger vs balance)
- Why event sourcing? (audit trail, time travel, CQRS)
- Events vs Commands (comparison table)
- Event flow diagram (visual)
- CRUD vs Event Sourcing (side-by-side code)
- Deep dive: Event versioning (expandable)

**Type System**
- Types as documentation
- Type inference in action
- Algebraic data types (with examples)
- Pattern matching
- Deep dive: GADTs (expandable)

**Effects**
- What are effects?
- Why explicit effects?
- Effect handlers
- Composing effects
- Deep dive: Effect internals (expandable)

---

### Guides (How-To)

**Best practices from the 12 sites:**

1. **Task-oriented organization** (Go, Laravel, Astro)
   - "How to add authentication"
   - "How to connect to PostgreSQL"
   - Not organized by difficulty, by task

2. **Complete, copy-paste-ready examples** (Tailwind, Stripe, FastAPI)
   - Full working code, not fragments
   - No "..." placeholders
   - Includes imports and setup

3. **One guide = one task** (Tailwind, Svelte, Go)
   - Flat hierarchy (no deep nesting)
   - Each guide is self-contained
   - Can be read in any order

4. **"I want to..." format** (Tailwind, Deno, Laravel)
   - Cheat sheet mapping goals to code
   - Quick lookup for common tasks

5. **Recipes section** (FastAPI, Astro, Go)
   - Short, focused solutions
   - Single-purpose guides
   - 5-10 minute reads

**NeoHaskell Guides structure:**

**Building Features**
- Add authentication
- Connect to PostgreSQL
- Handle file uploads
- Send emails
- Schedule background jobs

**Event Sourcing Patterns**
- Implement snapshots
- Handle event versioning
- Build custom projections
- Implement sagas
- Handle eventual consistency

**Deployment**
- Deploy to Fly.io
- Deploy to Railway
- Deploy to Heroku
- Deploy with Docker
- Set up CI/CD

**Integrations**
- Integrate with Stripe
- Integrate with SendGrid
- Integrate with S3
- Integrate with Redis

**Cheat Sheet**
```markdown
## I Want To...

| Goal | Code |
|------|------|
| Create an account | `createAccount :: AccountId -> IO Account` |
| Deposit funds | `deposit amount account` |
| Query balance | `getBalance account` |
| Replay events | `replayEvents events` |
```

---

### Reference (API Documentation)

**Best practices from the 12 sites:**

1. **Quick reference tables** (Tailwind, Stripe, Vue)
   - Function signatures at top
   - Scannable format
   - Experts can find and leave

2. **Comprehensive on each page** (Laravel, Svelte, React)
   - Don't assume readers read other pages
   - Self-contained documentation
   - Heavy cross-linking to related topics

3. **Progressive disclosure** (Tailwind, Stripe, React)
   - Table → Examples → Customization
   - Serves all skill levels on one page

4. **Type signatures and defaults** (Astro, Laravel, Vue)
   - Show exact types
   - Show default values
   - Show version added/deprecated

5. **Troubleshooting sections** (React, Svelte, Deno)
   - Organized by symptom
   - "I'm getting error X" → solution
   - Explains mental model error, not just fix

**NeoHaskell Reference structure:**

**Account Module**

Quick Reference:
```haskell
createAccount :: AccountId -> IO Account
deposit       :: Amount -> Account -> IO Account
withdraw      :: Amount -> Account -> IO (Maybe Account)
getBalance    :: Account -> Amount
getHistory    :: Account -> [Event]
```

Examples:
```haskell
-- Create account
account <- createAccount "ACC001"

-- Deposit funds
account' <- deposit 100 account

-- Withdraw funds
result <- withdraw 50 account'
case result of
  Just account'' -> print "Withdrawal successful"
  Nothing        -> print "Insufficient funds"
```

Troubleshooting:
- **"I'm getting 'Insufficient funds' error"** → Check balance before withdrawal
- **"Events aren't being stored"** → Ensure event store is configured

---

### Coming From... (Migration Guides)

**Best practices from the 12 sites:**

1. **Reassurance first** (Deno, Vue, Elixir/Phoenix)
   - "NeoHaskell is Haskell-compatible"
   - "Most concepts translate directly"
   - Reduce migration anxiety

2. **Quick start section** (Deno, Vue, Stripe)
   - 3 immediate examples that work
   - Prove compatibility before explaining differences

3. **Cheatsheet table** (Deno, Vue, Go)
   - Side-by-side command/syntax comparisons
   - Scannable format
   - Quick reference

4. **"What's the same" section** (Deno, Vue)
   - List familiar concepts that work identically
   - Build confidence

5. **"What's different" section** (Deno, Rust Book, Elixir/Phoenix)
   - Explain divergences factually, not defensively
   - Show why changes were made
   - Provide migration path

6. **Troubleshooting** (Deno, Vue, Elixir/Phoenix)
   - "If you're used to X, here's how to do it in Y"
   - Common migration pitfalls

**NeoHaskell "Coming From Haskell" structure:**

```markdown
# Coming from Haskell

NeoHaskell is designed for Haskell developers. Most Haskell concepts translate directly, with intentional simplifications for web development.

## Quick Start

### Familiar Syntax
[Show function that looks identical]

### Type Inference
[Show type inference working the same]

### Different Default
[Show strict evaluation example]

## What's the Same

- Pure functions
- Algebraic data types
- Pattern matching
- Type inference
- do notation
- Higher-order functions

## What's Different

### Strict by Default
[Explanation + code example + rationale]

### Simplified Type Classes
[Explanation + code example + rationale]

### No Language Extensions
[Explanation + code example + rationale]

## Haskell to NeoHaskell Cheatsheet

| Haskell | NeoHaskell |
|---------|------------|
| `data Maybe a = ...` | `type Maybe a = ...` |
| `class Functor f where ...` | `interface Functor f ...` |
| Lazy by default | Strict by default |
| Language extensions | Built-in features |

## Migration Guide

### Converting Pure Functions
[Step-by-step]

### Handling IO
[Step-by-step]

### Using Haskell Libraries
[Step-by-step]

## Common Questions

### Why is lazy evaluation not default?
[Factual explanation: performance, predictability for web apps]

### How do I use my favorite Haskell library?
[FFI or compatibility layer]

### What about Monad transformers?
[NeoHaskell's effect system alternative]
```

---

## Priority Recommendations for NeoHaskell

### Must Adopt (Day 1)

These patterns are non-negotiable. Every great docs site uses them.

#### 1. Tutorial/Reference Separation
**Pattern:** Separate `/tutorial` (linear, project-based) from `/docs` (comprehensive, random access)
**Sites:** Svelte, React, Go, FastAPI, Astro, Vue, Elixir/Phoenix (7/12)
**Why critical:** Mixing tutorial and reference creates hybrid pages that fail at both teaching and lookup. This is the #1 structural decision.
**Action:** Create `/tutorial` path for NeoBank project. Keep `/docs` for reference only. Link bidirectionally.

#### 2. Minimal Working Example First
**Pattern:** Every page starts with 5-15 lines of working code. Explanation comes after.
**Sites:** All 12 sites
**Why critical:** Immediate success builds confidence. Concrete code beats abstract explanation.
**Action:** NeoBank tutorial starts with 10-line account creation that compiles and runs. Show event stream output immediately.

#### 3. Progressive Complexity Through Incremental Building
**Pattern:** Each tutorial step adds ONE concept to existing code.
**Sites:** All 12 sites
**Why critical:** Cognitive load stays manageable. Readers see how features compose.
**Action:** NeoBank tutorial: Create account → Deposit → Withdraw → Transfer → History → Time travel (each adds one feature)

#### 4. Checkpoint/Verification Sections
**Pattern:** "Verify your progress" after each step with exact commands and expected output
**Sites:** Stripe, Rust Book, FastAPI, Elixir/Phoenix, Astro, Go, Svelte, Laravel, Deno (9/12)
**Why critical:** Prevents readers from getting lost. Immediate feedback on correctness.
**Action:** After each NeoBank step: "Run `neo repl`, execute `deposit account1 100`, expect `Account balance: $100`"

#### 5. Quick Reference Tables
**Pattern:** Start reference pages with scannable tables (function → signature → description)
**Sites:** Tailwind, Stripe, Vue, Deno, Laravel, Go, FastAPI, Astro (8/12)
**Why critical:** Experts can scan and leave. Supports both learning and lookup.
**Action:** Every API page starts with table of functions, types, and descriptions.

#### 6. Cross-Reference Density
**Pattern:** Every page links to 5-10 related pages (tutorial → guide → reference → concept)
**Sites:** All 12 sites
**Why critical:** Supports non-linear learning. Prevents dead ends.
**Action:** End each page with "See Also" linking to tutorial, concept, how-to, and reference.

---

### Should Adopt (Phase 1-2)

These patterns significantly improve quality. Most great sites use them.

#### 7. Teaching Unfamiliar Concepts Through Familiar Tasks
**Pattern:** Teach event sourcing by building a bank, not by explaining event sourcing
**Sites:** FastAPI, Rust Book, Elixir/Phoenix, React, Vue, Svelte, Deno, Go (8/12)
**Why important:** Reduces cognitive load. Concept emerges as "natural solution."
**Action:** NeoBank tutorial feels like "building a bank" not "learning event sourcing." Event sourcing emerges naturally.

#### 8. Compiler/Error Messages as Teaching Moments
**Pattern:** Show actual errors, explain meaning, demonstrate fix
**Sites:** Rust Book, Svelte, Elixir/Phoenix, Deno, FastAPI, Go, TypeScript (7/12)
**Why important:** Builds error literacy. Reduces fear of compiler.
**Action:** "Break It" exercises where readers trigger errors intentionally. Show full error output with explanation.

#### 9. Visual Aids for Abstract Concepts
**Pattern:** Diagrams, flow charts, memory layouts before code
**Sites:** Rust Book, Elixir/Phoenix, React, Svelte, Astro, Tailwind, Stripe (7/12)
**Why important:** Visual processing is faster. Makes invisible concepts visible.
**Action:** Event flow diagrams (Command → Aggregate → Event → Projection). Timeline showing event replay.

#### 10. Multi-Language/Multi-Path Support
**Pattern:** Show code in multiple languages/styles OR multiple installation paths
**Sites:** Stripe, Vue, Deno, Laravel, Astro, Go, Svelte, React, FastAPI, Tailwind (10/12)
**Why important:** Respects developer preferences. Reduces "doesn't apply to me" friction.
**Action:** "Coming From..." pages (JS/Python/Go/Haskell). Installation paths (GHCup/Installer/Docker). Build tool tabs (Stack/Cabal).

#### 11. Progressive Disclosure via "Optional" Sections
**Pattern:** Mark advanced features as "Optional:" or hide in expandable sections
**Sites:** Stripe, React, Vue, Svelte, Rust Book, FastAPI, Astro, Elixir/Phoenix, Go (9/12)
**Why important:** Prevents overwhelming beginners. Supports incremental learning.
**Action:** "Optional: Optimize with Snapshots" sections. Expandable `<details>` for advanced topics.

#### 12. Interactive Code Examples
**Pattern:** Editable code with live preview or runnable in browser
**Sites:** Svelte, React, Astro, Go, FastAPI (5/12)
**Why important:** Immediate feedback loop. Eliminates "does this work?" doubt.
**Action:** Build NeoHaskell playground (browser-based REPL). Link every example to playground with pre-loaded code.

---

### Could Adopt (Phase 3+)

Nice-to-have patterns. Some sites use them to great effect.

#### 13. Inline Glossary/Tooltips
**Pattern:** Hover tooltips for domain-specific terms
**Sites:** Stripe, React, Vue, Elixir/Phoenix, Astro (5/12)
**Why nice:** Reduces cognitive load. No context switching.
**Action:** Mark up event sourcing terms with tooltips: "command," "event," "aggregate," "projection"

#### 14. "See Also" / Related Content Boxes
**Pattern:** Dedicated boxes linking to related guides/concepts
**Sites:** Stripe, React, Laravel, Astro, Go (5/12)
**Why nice:** Explicit navigation suggestions. Helps discovery.
**Action:** Add "See Also" boxes at end of sections with 3-5 related links.

#### 15. Complexity Ratings
**Pattern:** Rate guides by complexity (1/5 to 5/5)
**Sites:** Stripe, Vue (2/12)
**Why nice:** Helps readers gauge time/effort required.
**Action:** Add complexity indicators to tutorial sections: "Complexity: 2/5"

#### 16. "What You'll Learn" Upfront
**Pattern:** Bullet list of outcomes at start of page
**Sites:** Stripe, React, Astro, FastAPI (4/12)
**Why nice:** Sets expectations. Helps readers decide if page is relevant.
**Action:** Add to tutorial pages: "What you'll learn: [bullets], Time: 15 minutes, Complexity: 2/5"

#### 17. Recipes/Cheat Sheet Section
**Pattern:** Short, focused how-to guides for common tasks
**Sites:** FastAPI, Astro, Go, Tailwind (4/12)
**Why nice:** Quick lookup for common patterns.
**Action:** Create "Recipes" section with 5-10 minute guides: "Add authentication," "Connect to PostgreSQL"

#### 18. Migration Guides
**Pattern:** Dedicated pages for migrating from other frameworks/languages
**Sites:** Deno, Vue, Astro, Laravel (4/12)
**Why nice:** Reduces switching friction. Helps adoption.
**Action:** "Coming From Haskell/JavaScript/Python/Go" pages with cheatsheets and side-by-side comparisons.

---

## Teaching an Unfamiliar Concept: Lessons from Rust, Elixir, and FastAPI

NeoHaskell's core challenge is teaching event sourcing (unfamiliar) alongside NeoHaskell syntax (unfamiliar) to developers from JS/TS, Python, Go, and Haskell backgrounds. This is analogous to:
- **Rust:** Teaching ownership/borrowing to GC developers
- **Elixir:** Teaching processes/OTP to OOP developers
- **FastAPI:** Teaching type hints to Python developers

Here's how each handles their unfamiliar concept, and what NeoHaskell should adopt:

### Rust's Ownership Teaching Strategy

**The Challenge:** Ownership is Rust's most unique and difficult concept. It requires unlearning GC mental models.

**How Rust Teaches It (Chapter 4):**

1. **Acknowledge difficulty upfront**
   > "Ownership is Rust's most unique feature... it does take some time to get used to."
   
   **NeoHaskell application:** Start event sourcing chapter with:
   > "Event sourcing is NeoHaskell's most distinctive feature. It's different from CRUD patterns you may know. Because event sourcing is new to many developers, it takes time to get used to. The good news: the more you practice, the more natural it becomes."

2. **State the rules explicitly**
   - Each value has an owner
   - Only one owner at a time
   - When owner goes out of scope, value is dropped
   
   **NeoHaskell application:** Give 3-5 simple rules for event sourcing:
   - Events are immutable facts about what happened
   - Current state is derived by replaying events
   - Never delete or update events, only append
   - Every state change must be an event
   - Event log is the source of truth

3. **Use physical analogies**
   - Stack = stack of plates
   - Heap = restaurant seating
   - Borrowing = borrowing a book (you give it back)
   
   **NeoHaskell application:** Event sourcing analogies:
   - Event log = bank ledger (never erase, only add corrections)
   - State reconstruction = reading a story (can't skip chapters)
   - CRUD = editing Wikipedia (history lost)
   - Event sourcing = Git commits (every change preserved)

4. **Show compiler errors as teaching**
   - Full error output with caret pointing to problem
   - Explain why error exists (ownership rules)
   - Show the fix
   
   **NeoHaskell application:** Show errors when trying to mutate state directly:
   ```
   error[NH0042]: cannot mutate account balance directly
    --> src/Account.nh:12:5
     |
   12|     account.balance = account.balance + 100
     |     ^^^^^^^^^^^^^^^ direct state mutation not allowed
     |
     = help: emit a DepositMade event instead
     = note: in event-sourced systems, state changes must be events
   ```

5. **Incremental tutorial with frequent wins**
   - Chapter 2 builds guessing game in stages
   - Each stage compiles and runs
   - Ownership used before explained
   
   **NeoHaskell application:** NeoBank tutorial stages:
   - Part 1: Create account (simple event, immediate success)
   - Part 2: Deposit (event application)
   - Part 3: Query balance (state reconstruction)
   - Part 4: Withdraw (validation)
   - Part 5: History (event log benefits)
   - Part 6: Overdraft (business rules)

6. **Memory diagrams → Event flow diagrams**
   - SVG diagrams showing stack/heap, moves, borrowing
   
   **NeoHaskell application:** Diagrams showing:
   - Event flow: Command → Event → State Update
   - Event log structure: Append-only sequence
   - State reconstruction: Events replayed
   - CRUD comparison: Side-by-side mutation vs events

7. **Reassurance and encouragement**
   > "The more experienced you become with Rust... the easier you'll find it to naturally develop safe code. Keep at it!"
   
   **NeoHaskell application:**
   > "If event sourcing feels unfamiliar, that's normal. It requires a mental shift from CRUD thinking. Stick with it—the benefits become clear as you build real features."

### Elixir's Process/OTP Teaching Strategy

**The Challenge:** Processes and OTP are unfamiliar to OOP developers. Requires understanding message-passing concurrency.

**How Elixir Teaches It:**

1. **Name the pattern explicitly**
   > "We call these modules contexts"
   > "All code runs inside processes"
   
   **NeoHaskell application:** Name event sourcing concepts:
   - "Events as Facts"
   - "Projections as Views"
   - "Commands as Intentions"
   - "Aggregates as State Machines"

2. **Show you've already used it**
   > "If you have used `mix phx.gen.html`, you have already used contexts"
   
   **NeoHaskell application:**
   > "If you've completed the NeoBank tutorial, you've already used event sourcing. Every account operation you wrote was an event."

3. **Explain the why**
   > "Contexts help you group related schemas, instead of having several dozens with no insights"
   
   **NeoHaskell application:**
   > "Event sourcing gives you audit trails, time-travel debugging, and CQRS with zero extra code. The event log is your database, your audit log, and your debugging tool."

4. **Show the primitive, then the abstraction**
   - Teach `spawn` (low-level)
   - Then show `Agent` (high-level)
   > "We won't implement those patterns manually... Elixir provides Agents"
   
   **NeoHaskell application:**
   - Show manual event replay (low-level)
   - Then show NeoHaskell's event sourcing library (high-level)
   > "You could replay events manually, but NeoHaskell's event sourcing library handles this for you"

5. **Generator-first workflow**
   - `mix phx.gen.html` creates working code
   - Read generated code to understand patterns
   
   **NeoHaskell application:**
   - `neo gen.aggregate Account` generates event-sourced aggregate
   - `neo gen.projection AccountSummary` generates projection
   - Read generated code to learn patterns

6. **ASCII diagrams for architecture**
   - Full message-flow diagram before any code
   - Shows client → channel → PubSub → transport
   
   **NeoHaskell application:**
   - Event flow diagram before code
   - Shows Command → Aggregate → Event → Store → Projection → Query

7. **"Let's give it a try" verification**
   > "Let's give it a try by running `iex kv.exs`"
   - Shows exact commands and output
   
   **NeoHaskell application:**
   > "Let's give it a try by running `neo repl`"
   - Show exact REPL commands and event stream output

8. **Demystification pattern**
   - "Contexts are just modules"
   - "Views are just modules"
   - "At the end of the day, they are just plain modules"
   
   **NeoHaskell application:**
   - "Aggregates are just modules with state"
   - "Events are just data structures"
   - "Projections are just functions that fold over events"
   - "At the end of the day, it's just functional programming"

### FastAPI's Type Hints Teaching Strategy

**The Challenge:** Type hints are unfamiliar to most Python developers. Requires learning new syntax and mental model.

**How FastAPI Teaches It:**

1. **Teach through building, not studying**
   - Never says "let's learn type hints"
   - Says "let's build an API"
   - Type hints emerge naturally
   
   **NeoHaskell application:**
   - Never say "let's learn event sourcing"
   - Say "let's build a bank"
   - Event sourcing emerges as natural way to track transactions

2. **Show benefits immediately**
   - Type hints give you: validation, editor support, auto-generated docs
   - Benefits are concrete, not theoretical
   
   **NeoHaskell application:**
   - Event sourcing gives you: audit trail, time travel, CQRS
   - Show these benefits in first tutorial lesson

3. **Minimal example first**
   - 5-line Hello World before explaining anything
   
   **NeoHaskell application:**
   - 10-line account creation before explaining event sourcing

4. **"Check it" sections**
   - Shows exact URL, exact JSON response
   - Points out what changed from previous example
   
   **NeoHaskell application:**
   - Shows exact REPL commands, exact event stream
   - Points out new events added

5. **Progressive enhancement**
   - Page 1: Basic endpoint
   - Page 2: Add path parameters
   - Page 3: Add query parameters
   - Page 4: Add request body
   - Each adds ONE concept
   
   **NeoHaskell application:**
   - Lesson 1: Create account
   - Lesson 2: Deposit (add validation)
   - Lesson 3: Withdraw (add business rule)
   - Lesson 4: Transfer (multiple aggregates)
   - Each adds ONE event sourcing concept

6. **Type system as teaching tool**
   - `item_id: int` → FastAPI validates it's an int
   - Teach validation through types, not rules
   
   **NeoHaskell application:**
   - `AccountId` type → Haskell ensures it's valid
   - Teach domain modeling through types

7. **Error messages as teaching**
   - Show validation error JSON
   - Teach what validation looks like in practice
   
   **NeoHaskell application:**
   - Show business rule violation errors
   - Teach what event sourcing constraints look like

8. **Interactive feedback loop**
   - Write code → run server → open browser → see Swagger UI → test API
   
   **NeoHaskell application:**
   - Write code → run app → see account state → replay events → see history
   - Could build "event inspector" UI similar to Swagger UI

### Composite Strategy for NeoHaskell

Combining the best from all three:

**From Rust:**
- Acknowledge difficulty upfront
- State rules explicitly (3-5 simple rules)
- Use physical analogies (ledger, Git commits)
- Show compiler errors as teaching
- Incremental tutorial with frequent wins
- Visual diagrams (event flow)
- Reassurance and encouragement

**From Elixir:**
- Name concepts explicitly ("Events as Facts")
- Show you've already used it
- Explain the why (audit, time travel, CQRS)
- Show primitive then abstraction
- Generator-first workflow (`neo gen.aggregate`)
- Architecture diagrams before code
- "Let's give it a try" verification
- Demystification ("just modules," "just functions")

**From FastAPI:**
- Teach through building, not studying
- Show benefits immediately
- Minimal example first (10 lines)
- "Check it" sections with exact output
- Progressive enhancement (one concept per lesson)
- Type system as teaching tool
- Error messages as teaching
- Interactive feedback loop (event inspector UI)

**The Core Lesson:**

Don't explain event sourcing abstractly. Teach it by building NeoBank. The tutorial should feel like "building a bank" not "learning event sourcing." Event sourcing should emerge as the natural way to:
- Track transactions (audit trail)
- Debug issues (replay events)
- Build reports (projections)
- Ensure correctness (immutable events)

By the end of the tutorial, a CRUD developer should think: "This is different, but I understand why, and I can do this."

---

## What NeoHaskell Should NOT Copy

### Anti-Pattern 1: Mixing Tutorial and Reference
**Sites that avoid this:** Svelte, React, Go, FastAPI
**Why it fails:** Hybrid pages that try to teach AND document fail at both. Tutorials need narrative flow. Reference needs comprehensive coverage.
**NeoHaskell action:** Keep `/tutorial` and `/docs` strictly separate.

### Anti-Pattern 2: Abstract Explanation Before Working Code
**Sites that avoid this:** FastAPI, Rust Book, Svelte
**Why it fails:** Readers lose interest before seeing results. Abstract concepts don't stick without concrete examples.
**NeoHaskell action:** Always show working code first. Explain after it runs.

### Anti-Pattern 3: Assuming Prior Knowledge
**Sites that avoid this:** React, FastAPI, Astro
**Why it fails:** Gatekeeping ("you must know X first") prevents newcomers from starting.
**NeoHaskell action:** State prerequisites clearly but don't enforce them. Provide links to background material but don't require reading it.

### Anti-Pattern 4: Generic Examples (foo/bar)
**Sites that avoid this:** React, FastAPI, Stripe
**Why it fails:** Generic examples don't build mental models. Readers can't relate to "foo" and "bar."
**NeoHaskell action:** Use NeoBank (accounts, transactions, balances) in every example. Consistency builds domain understanding.

### Anti-Pattern 5: Hiding the Ecosystem
**Sites that avoid this:** Laravel, Go, Astro
**Why it fails:** Pretending your framework exists in isolation confuses readers when they encounter third-party tools.
**NeoHaskell action:** Link to Haskell packages (aeson, servant) as first-class. Show how to use existing Haskell libraries. Be transparent about building on GHC.

### Anti-Pattern 6: Over-Versioning Early
**Sites that avoid this:** Most sites wait until 1.0
**Why it fails:** Maintaining multiple doc versions pre-1.0 wastes effort. Breaking changes are expected.
**NeoHaskell action:** Wait until 0.5 or 1.0 to maintain multiple doc versions. Use warning banners on old versions.

### Anti-Pattern 7: Defensive Explanations
**Sites that avoid this:** Deno, Rust Book, Elixir/Phoenix
**Why it fails:** Explaining why you're different from competitors sounds defensive. Readers want to know "what can I build?" not "why you're better than X."
**NeoHaskell action:** Explain divergences from Haskell factually, not defensively. "NeoHaskell uses strict evaluation for better performance in web apps" not "Haskell's lazy evaluation is bad."

### Anti-Pattern 8: Deep Nesting in Navigation
**Sites that avoid this:** Tailwind, Go, Astro
**Why it fails:** Deep hierarchies (4+ levels) make navigation hard. Readers get lost.
**NeoHaskell action:** Maximum 3 levels in sidebar. Use flat hierarchies where possible.

### Anti-Pattern 9: Incomplete Code Examples
**Sites that avoid this:** Tailwind, Stripe, FastAPI
**Why it fails:** "..." placeholders force readers to guess. Incomplete examples don't compile.
**NeoHaskell action:** Every code example is complete, runnable NeoHaskell code. Include imports and setup.

### Anti-Pattern 10: Ignoring Error Messages
**Sites that avoid this:** Rust Book, Svelte, Deno
**Why it fails:** Readers will encounter errors. If docs don't prepare them, they'll get stuck.
**NeoHaskell action:** Show actual compiler errors. Explain what they mean. Demonstrate fixes. Create "Break It" exercises.

---

## Immediate Action Items (Priority Order)

### Phase 0: Foundation (Week 1-2)

1. **Set up Starlight versioning**
   - Configure `astro.config.mjs` with version support
   - Even if only one version exists, structure for future

2. **Create sidebar structure**
   - Getting Started (linear)
   - Tutorial (separate path)
   - Core Concepts (non-linear)
   - Guides (task-oriented)
   - Reference (API docs)

3. **Enable Starlight features**
   - Search (enabled by default)
   - Theme switching (dark/light)
   - "On this page" right sidebar
   - "Edit page" links to GitHub

4. **Create placeholder pages**
   - `/start/installation`
   - `/start/quick-start`
   - `/tutorial/welcome`
   - `/concepts/event-sourcing`
   - `/guides/deploy`
   - `/reference/api`

### Phase 1: Tutorial (Week 3-6)

5. **Build NeoBank tutorial structure**
   - Unit 0: Welcome (2 lessons)
   - Unit 1: Setup (4 lessons)
   - Unit 2: Core Concepts (6 lessons)
   - Unit 3: Advanced (5 lessons)
   - Unit 4: Production (4 lessons)

6. **Write first 3 tutorial lessons**
   - Lesson 1: Install NeoHaskell
   - Lesson 2: Create NeoBank project
   - Lesson 3: Create first account
   - Each with minimal working example, checkpoint, verification

7. **Add tutorial features**
   - Progress tracking (checkboxes)
   - "What you'll learn" sections
   - "Verify your progress" checkpoints
   - "See Also" cross-references

### Phase 2: Core Content (Week 7-10)

8. **Write event sourcing concept page**
   - What is event sourcing? (with analogies)
   - Why event sourcing? (benefits)
   - Event flow diagram (visual)
   - CRUD vs Event Sourcing comparison
   - "Break It" exercise with compiler error

9. **Write "Coming From Haskell" page**
   - Reassurance first
   - Quick start (3 examples)
   - What's the same / What's different
   - Cheatsheet table
   - Migration guide

10. **Create quick reference tables**
    - Account API functions
    - Event types
    - Command types
    - Common patterns

### Phase 3: Polish (Week 11-12)

11. **Add visual aids**
    - Event flow diagrams
    - State reconstruction timeline
    - CQRS architecture diagram

12. **Create recipes section**
    - Add authentication
    - Connect to PostgreSQL
    - Deploy to Fly.io
    - 5-10 short, focused guides

13. **Add interactive elements**
    - Code examples with copy buttons
    - Tabbed code blocks (Stack/Cabal)
    - Platform-specific installation tabs

14. **Final polish**
    - Cross-reference audit (every page links to 5-10 related pages)
    - Callout boxes (notes, tips, warnings)
    - Code block labels (file paths)
    - Breadcrumb navigation

---

## Success Metrics

How to measure if NeoHaskell docs are working:

1. **Tutorial completion rate**
   - Track how many users complete Unit 1, Unit 2, etc.
   - Target: 60%+ complete Unit 1, 40%+ complete full tutorial

2. **Time to first working code**
   - How long from landing on docs to running NeoBank example?
   - Target: <10 minutes

3. **Search queries**
   - What are users searching for?
   - Are they finding it?
   - Target: <3 searches to find answer

4. **Bounce rate on tutorial pages**
   - Are users leaving after first lesson?
   - Target: <30% bounce rate

5. **GitHub issues about docs**
   - "I don't understand X"
   - "How do I Y?"
   - Target: Decreasing over time

6. **Community questions**
   - Discord/forum questions about basics
   - Target: Decreasing as docs improve

7. **External validation**
   - "NeoHaskell has great docs" mentions
   - Comparisons to Rust Book, FastAPI, Svelte
   - Target: Positive sentiment

---

## Conclusion

The 12 sites analyzed share universal patterns:
1. Tutorial/reference separation
2. Minimal working examples first
3. Progressive complexity
4. Checkpoint verification
5. Teaching through building
6. Compiler as teacher
7. Quick reference tables
8. Multi-path support
9. Visual aids
10. Cross-reference density
11. Progressive disclosure
12. Interactive feedback

NeoHaskell's challenge (teaching event sourcing to CRUD developers) is analogous to:
- Rust teaching ownership to GC developers
- Elixir teaching processes to OOP developers
- FastAPI teaching type hints to Python developers

The solution is the same: **Don't explain the unfamiliar concept abstractly. Teach it by building something familiar.** NeoBank tutorial should feel like "building a bank" not "learning event sourcing." Event sourcing emerges as the natural way to solve banking problems.

Priority 1 (Must Adopt Day 1):
- Tutorial/reference separation
- Minimal working examples
- Progressive complexity
- Checkpoints
- Quick reference tables
- Cross-references

Priority 2 (Should Adopt Phase 1-2):
- Teaching through building
- Compiler as teacher
- Visual aids
- Multi-path support
- Progressive disclosure
- Interactive examples

Priority 3 (Could Adopt Phase 3+):
- Inline glossary
- Complexity ratings
- Recipes section
- Migration guides

The goal: By the end of the tutorial, a CRUD developer should think, "This is different, but I understand why, and I can do this."
