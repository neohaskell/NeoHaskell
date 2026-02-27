# Stripe Documentation Analysis

## Navigation Structure

Stripe's docs use a **two-tier navigation system**:

**Top-level sections** (from https://stripe.com/docs):
1. **Get Started** — Use-case driven entry points ("Accept payments online", "Sell subscriptions", "Set up development environment")
2. **Browse by Product** — Organized into three business categories:
   - Payments (Payments, Terminal, Connect, Radar, Climate, Identity, Financial Connections, Crypto)
   - Revenue (Billing, Tax, Revenue Recognition, Sigma, Data Pipeline, Atlas)
   - Money Management (Issuing, Financial Accounts, Capital)
3. **Prebuilt Components** — Quick-start solutions (Payment Links, Checkout, Elements)

**Depth**: The navigation is **shallow but wide**. The landing page presents ~25 product areas, but each product area has its own deep hierarchy. For example, the "Payments" section contains subsections for different payment methods, integration types, and guides.

**Key pattern**: Stripe leads with **use cases** ("Accept payments online") before exposing the underlying product taxonomy. This lets developers start from their business goal rather than learning Stripe's product architecture first.

## Page Types

Stripe uses **five distinct page types**:

### 1. Use-Case Landing Pages
**Purpose**: Entry point for developers with a specific business goal  
**Example**: "Accept payments online" (https://docs.stripe.com/get-started/use-cases/startup.md)  
**Structure**: Problem statement → recommended solution path → links to relevant guides

### 2. Quickstart/Setup Guides
**Purpose**: Get developers to "hello world" as fast as possible  
**Example**: "Set up your development environment" (https://docs.stripe.com/get-started/development-environment)  
**Structure**:
- "What you learn" section upfront (sets expectations)
- Language-specific tabs (Ruby, Python, Go, Java, etc.) — **the entire page adapts to selected language**
- Step-by-step with inline code blocks
- "Confirm setup" checkpoints after each major step
- Ends with "See also" links to next logical steps

**Notable**: The quickstart page is **one continuous scroll** with embedded language switcher. When you select "Ruby", all code examples, installation instructions, and even prose change to Ruby-specific content. This is a **single-page, multi-language experience** rather than separate pages per language.

### 3. Integration Guides
**Purpose**: Complete implementation of a specific feature  
**Example**: "Accept a payment" (https://docs.stripe.com/payments/accept-a-payment)  
**Structure**:
- **Integration effort indicator** (Complexity: 2/5)
- **Integration type** (e.g., "Redirect to Stripe-hosted page")
- **UI customization** summary (what's configurable)
- **"Try it out" demo link** (live sandbox)
- Step-by-step implementation with [Client-side] and [Server-side] tags on each section
- **Testing section** with specific test card numbers and expected outcomes
- **Optional sections** clearly marked (e.g., "Optional: Prefill customer data")
- **"Handle post-payment events"** section explaining webhook integration
- **"See also"** links to related guides

**Key pattern**: Every integration guide includes a **comparison table** showing when to use this approach vs. alternatives. For example, the Checkout guide compares "Stripe-hosted page" vs. "Embedded form" with side-by-side feature matrices.

### 4. API Reference Pages
**Purpose**: Complete parameter documentation for a single API resource  
**Example**: "Charges" (https://docs.stripe.com/api/charges)  
**Structure**:
- Brief description of the resource and when to use it
- **Endpoints list** (Create, Update, Retrieve, List, etc.) with HTTP method and path
- Each endpoint links to a dedicated page with full parameter documentation

**Notable**: API reference pages are **minimal on the index level**. The real detail lives on the individual endpoint pages (e.g., `/api/charges/create`). This keeps the reference scannable.

### 5. Concept/Explanation Pages
**Purpose**: Explain how Stripe's systems work  
**Example**: Referenced but not fetched (e.g., "How products and prices work")  
**Structure**: (Inferred from links) Conceptual explanation without code, linking to relevant guides

## Content Patterns

### Multi-Language Code Switching
**Every code example** includes a language selector. When you switch languages, the **entire page updates**:
- Code blocks change syntax
- Installation commands change (gem vs. pip vs. go get)
- Import statements change
- Even prose changes (e.g., "Install with RubyGems" vs. "Install with pip")

This is **not** separate pages per language. It's a single page with conditional rendering based on language selection. The URL includes `?lang=ruby` but doesn't navigate away.

### Inline Glossary Tooltips
Stripe uses **hover tooltips** for domain-specific terms. For example:
- "sandbox" → tooltip: "A sandbox is an isolated test environment that allows you to test Stripe functionality in your account without affecting your live integration."
- "fulfill" → tooltip: "Fulfillment is the process of providing the goods or services purchased by a customer, typically after payment is collected."

These appear as **italic text with underline** in the markdown, suggesting they're rendered as interactive elements in the live docs.

### Code Block Annotations
Code blocks include:
- **Language label** (e.g., `#### Ruby`, `#### bash`)
- **Filename hints** when showing file content (e.g., `#### create_price.rb`)
- **Inline comments** explaining key values (e.g., `"id": "prod_LTenIrmp8Q67sa", // The identifier looks like this.`)

### Checkpoint Pattern
After each major step, Stripe includes a **"Confirm your endpoint"** or **"Testing"** section with:
- Exact curl command to run
- Expected output
- "If your integration isn't working" troubleshooting steps

Example from the quickstart:
```bash
curl -X POST -is "http://localhost:4242/create-checkout-session" -d ""
```
Expected response:
```bash
HTTP/1.1 303 See Other
Location: https://checkout.stripe.com/c/pay/cs_test_...
```

### Testing Tables
Integration guides include **comprehensive testing tables** with:
- Test card numbers (e.g., `4242 4242 4242 4242` for success, `4000 0000 0000 9995` for decline)
- Scenario descriptions
- "How to test" instructions

These tables are **categorized by payment method type** (Cards, Wallets, Bank redirects, Bank debits, Vouchers).

### Event-Driven Architecture Emphasis
Every integration guide includes a **"Handle post-payment events"** section explaining:
- Which webhook events to listen for
- Why you can't rely on redirect-based confirmation
- What actions to trigger on each event

Example table from "Accept a payment":
| Event | Description | Action |
|-------|-------------|--------|
| `checkout.session.completed` | Customer completes payment | Send confirmation, fulfill order |
| `checkout.session.async_payment_succeeded` | Delayed payment succeeds | Send confirmation, fulfill order |
| `checkout.session.async_payment_failed` | Delayed payment fails | Notify customer, retry payment |

### Progressive Disclosure via "Optional" Sections
Guides use **"Optional:"** prefixes for advanced features:
- "Optional: Create products and prices"
- "Optional: Prefill customer data"
- "Optional: Save payment method details"
- "Optional: Separate authorization and capture"

This lets developers **skip to the end** for a basic integration, then return to add features incrementally.

### Cross-Reference Density
Stripe docs are **heavily interlinked**:
- Every API object mention links to its reference page
- Every concept mention links to an explanation page
- Every guide ends with "See also" links to related guides
- Inline links to related features (e.g., "Learn more about testing your integration")

### Callout Boxes
Stripe uses **blockquote-style callouts** for:
- **Warnings** (e.g., "Windows anti-virus scanners occasionally flag the Stripe CLI as unsafe")
- **Important notes** (e.g., "Checkout Sessions expire 24 hours after creation")
- **Alternative approaches** (e.g., "Not a developer? Use Stripe's no-code options")
- **Privacy/compliance guidance** (e.g., "Global privacy laws are complicated... contact your legal team")

### "See Also" Pattern
Every guide ends with a **"See also"** section linking to:
- Next logical steps (e.g., after basic payment, link to "Add discounts", "Collect taxes")
- Alternative approaches (e.g., "Create a payment link" as no-code alternative)
- Related features (e.g., "Customer account management")

## Progression Model

Stripe uses a **hub-and-spoke model** rather than a linear tutorial:

### Entry Points (The Hub)
The docs landing page offers **three entry strategies**:
1. **Use-case driven** ("Accept payments online", "Sell subscriptions") — for developers who know their business goal
2. **Product-driven** ("Browse by product") — for developers who know which Stripe product they need
3. **Quick-start driven** ("Set up development environment", "Integration quickstarts") — for developers who want to start coding immediately

### Spokes (Non-Linear Guides)
Once you enter via any hub, you're in a **web of interconnected guides**. There's no enforced sequence. Instead:
- Each guide is **self-contained** (includes all prerequisites)
- Guides link to **related guides** via "See also"
- Guides link to **deeper explanations** via inline links
- Guides link to **API reference** for parameter details

### Prerequisite Handling
Stripe **embeds prerequisites** rather than requiring separate setup:
- The "Accept a payment" guide includes a "Set up the Stripe CLI" section
- The "Set up development environment" guide includes SDK installation
- No "Before you begin, complete Tutorial 1" gatekeeping

If you've already completed a prerequisite, you can **skip that section**. If you haven't, you can complete it inline.

### Skill Level Indicators
Stripe uses **complexity ratings** (e.g., "Complexity: 2/5") on integration guides to help developers choose the right approach. Lower complexity = faster to implement but less customizable.

### Learning Paths (Implicit)
While there's no explicit "Tutorial 1 → Tutorial 2 → Tutorial 3" sequence, Stripe creates **implicit learning paths** via:
- **"See also"** links at the end of guides
- **Comparison tables** showing when to use each integration type
- **"Optional:"** sections that introduce advanced features

Example implicit path:
1. Start: "Accept payments online" (use-case landing)
2. Choose: "Stripe-hosted page" vs. "Embedded form" (comparison table)
3. Implement: "Accept a payment" guide (step-by-step)
4. Extend: "Add discounts", "Collect taxes" (see also links)
5. Optimize: "Save payment method details" (optional section)

### No "Beginner/Intermediate/Advanced" Labels
Stripe **doesn't categorize docs by skill level**. Instead, they use:
- **Complexity ratings** (objective: how many steps)
- **Integration type** (objective: redirect vs. embedded vs. custom)
- **Optional sections** (skip if you don't need it)

This avoids the "am I ready for intermediate?" question and focuses on **what you're trying to build**.

## Standout Features

### 1. Live Language Switching (Single-Page Multi-Language)
**What it is**: The entire page adapts to your selected programming language without navigation.  
**Why it's exceptional**: Most docs use separate pages per language (e.g., `/docs/ruby/quickstart`, `/docs/python/quickstart`). Stripe uses **conditional rendering** on a single page. This means:
- No context loss when switching languages
- Consistent URL structure (`?lang=ruby` query param)
- Easier to maintain (one source file, not N files)

**Implementation hint**: The markdown includes language-specific blocks like:
```markdown
# Ruby
> This is a Ruby for when lang is ruby. View the full page at https://docs.stripe.com/get-started/development-environment?lang=ruby.
```

This suggests a **build-time or runtime templating system** that shows/hides sections based on the `lang` parameter.

### 2. Inline Glossary Tooltips
**What it is**: Hover over domain-specific terms to see definitions without leaving the page.  
**Why it's exceptional**: Reduces cognitive load for new developers. You don't need to open a separate glossary page or remember what "sandbox" means.  
**Example**: "sandbox" → "A sandbox is an isolated test environment..."

**Implementation hint**: The markdown shows these as italic text, suggesting they're marked up with a special syntax (e.g., `*sandbox* (tooltip text)`) and rendered as interactive elements.

### 3. Checkpoint-Driven Guides
**What it is**: After each major step, a "Confirm your endpoint" section with exact commands and expected output.  
**Why it's exceptional**: Prevents developers from getting lost. You know **immediately** if you're on the right track.  
**Example**:
```bash
curl -X POST -is "http://localhost:4242/create-checkout-session" -d ""
```
Expected:
```bash
HTTP/1.1 303 See Other
Location: https://checkout.stripe.com/c/pay/cs_test_...
```

### 4. Comprehensive Testing Tables
**What it is**: Tables of test card numbers, scenarios, and expected outcomes.  
**Why it's exceptional**: Testing is **first-class**, not an afterthought. Developers can test edge cases (decline, 3D Secure, async payment) without needing real cards.  
**Example**:
| Card number | Scenario | How to test |
|-------------|----------|-------------|
| 4242424242424242 | Success | Fill form with this number |
| 4000002500003155 | Requires 3D Secure | Fill form, complete auth |
| 4000000000009995 | Declined | Fill form, see decline error |

### 5. Event-Driven Architecture Emphasis
**What it is**: Every integration guide includes a "Handle post-payment events" section explaining webhooks.  
**Why it's exceptional**: Stripe **doesn't let you build a brittle integration**. They explicitly warn against relying on redirects and show you the right way (webhooks) upfront.  
**Example**: "Listen for these events rather than waiting for your customer to be redirected back to your website."

### 6. Complexity Ratings
**What it is**: Each integration approach has a "Complexity: X/5" rating.  
**Why it's exceptional**: Helps developers **choose the right tool** for their skill level and timeline. A startup MVP might choose "Complexity: 1/5" (Payment Links), while a custom checkout might be "Complexity: 4/5".

### 7. "Try it out" Demo Links
**What it is**: Live sandbox demos embedded in the docs.  
**Why it's exceptional**: You can **see the end result** before writing code. Example: https://checkout.stripe.dev/ (linked from the Checkout guide).

### 8. Progressive Disclosure via "Optional" Sections
**What it is**: Advanced features are clearly marked as "Optional:" and placed after the core implementation.  
**Why it's exceptional**: Developers can **get to a working integration fast**, then return to add features. No "you must read all 10 sections" gatekeeping.

### 9. Installation Alternatives
**What it is**: Every SDK installation guide includes **multiple installation methods** (package manager, manual, Docker).  
**Why it's exceptional**: Accommodates different developer workflows. Example from Ruby guide:
- Install with bundler (recommended)
- Add as dependency (Gemfile)
- Global installation (gem install)
- Manual installation (build from source)

### 10. Cross-Platform CLI Installation
**What it is**: The Stripe CLI installation guide includes **platform-specific tabs** (homebrew, apt, yum, Scoop, macOS, Linux, Windows, Docker).  
**Why it's exceptional**: No "figure out how to install on your OS" friction. Every platform is a first-class citizen.

## Applicable to NeoHaskell

### 1. Adopt Use-Case Entry Points for the Tutorial Layer
**Pattern**: Stripe's landing page leads with "Accept payments online", "Sell subscriptions" (business goals) before exposing product taxonomy.  
**Apply to NeoHaskell**: The tutorial landing should lead with:
- "Build a REST API" (maps to NeoBank account creation endpoint)
- "Handle events" (maps to event sourcing concepts)
- "Store data" (maps to event store + read models)

**Why**: Developers from JS/Python/Go backgrounds think in terms of **what they're building**, not "what is event sourcing". Start with the familiar goal, introduce the unfamiliar concept as the solution.

**Concrete action**: Create a `tutorials/index.md` with use-case cards:
```markdown
## What do you want to build?

- **REST API** — Create endpoints that handle commands and return results
- **Event-Driven System** — React to changes in your application state
- **Data Persistence** — Store and query application data
```

Each card links to the relevant NeoBank tutorial section.

### 2. Adopt Single-Page Multi-Language Code Examples
**Pattern**: Stripe's guides show code in Ruby, Python, Go, Java, etc. on a single page with a language switcher.  
**Apply to NeoHaskell**: Show **equivalent patterns** from JS/TS, Python, Go, and Haskell side-by-side.  
**Why**: NeoHaskell's audience includes developers from other languages. Showing "here's how you'd do this in Python, here's the NeoHaskell equivalent" reduces cognitive load.

**Concrete action**: In the NeoBank tutorial, when introducing a concept like "defining a command", show:
```markdown
#### JavaScript
\`\`\`javascript
class CreateAccount {
  constructor(accountId, initialBalance) {
    this.accountId = accountId;
    this.initialBalance = initialBalance;
  }
}
\`\`\`

#### NeoHaskell
\`\`\`haskell
data CreateAccount = CreateAccount
  { accountId :: AccountId
  , initialBalance :: Money
  }
\`\`\`
```

Use a language switcher to show/hide based on the reader's background.

### 3. Adopt Checkpoint Pattern for Tutorial Steps
**Pattern**: Stripe's guides include "Confirm your endpoint" sections with exact commands and expected output after each step.  
**Apply to NeoHaskell**: After each tutorial step, include a "Verify your progress" section with:
- Exact command to run (e.g., `neo run`, `neo test`)
- Expected output (e.g., "Server listening on port 3000")
- Troubleshooting steps if output doesn't match

**Why**: Prevents developers from getting lost in a long tutorial. They know immediately if they're on track.

**Concrete action**: In the NeoBank tutorial, after "Define your first command", add:
```markdown
### Verify your progress

Run the type checker:
\`\`\`bash
neo check
\`\`\`

Expected output:
\`\`\`
✓ All types valid
✓ No errors found
\`\`\`

If you see errors, check that:
- Your `CreateAccount` type matches the example above
- You've imported the `Money` and `AccountId` types
```

### 4. Adopt Inline Glossary Tooltips for Event Sourcing Terms
**Pattern**: Stripe uses hover tooltips for domain-specific terms like "sandbox", "fulfill".  
**Apply to NeoHaskell**: Use tooltips for event sourcing terms like "aggregate", "event", "command", "projection".  
**Why**: Event sourcing is unfamiliar to most developers. Inline definitions reduce the need to jump to a separate glossary.

**Concrete action**: Mark up event sourcing terms in the tutorial:
```markdown
When a *command* (A command is a request to change application state, like "create account" or "transfer money") is processed, it produces one or more *events* (An event is a record of something that happened, like "account created" or "money transferred").
```

Render these as tooltips in the live docs.

### 5. Adopt "Optional:" Sections for Advanced Event Sourcing Concepts
**Pattern**: Stripe uses "Optional:" prefixes for advanced features like "Save payment method details".  
**Apply to NeoHaskell**: Use "Optional:" for advanced event sourcing concepts like:
- "Optional: Implement snapshots for performance"
- "Optional: Handle event versioning"
- "Optional: Build custom projections"

**Why**: Developers can get to a working NeoBank app without understanding every event sourcing optimization. They can return later to add these features.

**Concrete action**: In the NeoBank tutorial, after the core "create account, deposit, withdraw" flow, add:
```markdown
## Optional: Optimize with Snapshots

For accounts with thousands of events, replaying the entire event stream can be slow. Snapshots let you cache the current state and replay only recent events.

[Implementation details...]
```

### 6. Adopt Complexity Ratings for Tutorial Sections
**Pattern**: Stripe rates integration approaches as "Complexity: 2/5".  
**Apply to NeoHaskell**: Rate tutorial sections by complexity:
- "Complexity: 1/5" — Define a command (just a data type)
- "Complexity: 2/5" — Handle a command (pattern matching + validation)
- "Complexity: 3/5" — Build a projection (fold over events)
- "Complexity: 4/5" — Implement snapshots (caching + replay)

**Why**: Helps developers gauge how much time/effort a section requires. They can skip high-complexity sections if they're just exploring.

**Concrete action**: Add complexity indicators to tutorial section headers:
```markdown
## Handle Commands (Complexity: 2/5)

Learn how to validate and process commands to produce events.
```

### 7. Adopt Event-Driven Architecture Emphasis
**Pattern**: Stripe's guides emphasize webhooks and warn against relying on redirects.  
**Apply to NeoHaskell**: Emphasize **event-driven thinking** throughout the tutorial. Show how events enable:
- Audit logs (replay events to see what happened)
- Time travel debugging (replay events up to a point)
- Projections (build multiple read models from the same events)

**Why**: Event sourcing is the **core concept** NeoHaskell teaches. Make it central, not a side note.

**Concrete action**: In the NeoBank tutorial, after implementing the first command handler, add a section:
```markdown
## Why Events Matter

Notice that we didn't update a database directly. Instead, we produced an `AccountCreated` event. This event is:
- **Immutable** — Once written, it never changes
- **Auditable** — You can see exactly when and why the account was created
- **Replayable** — You can rebuild the account state by replaying all events

This is the foundation of event sourcing.
```

### 8. Adopt Testing Tables for Event Sourcing Scenarios
**Pattern**: Stripe's testing tables show test card numbers and expected outcomes.  
**Apply to NeoHaskell**: Create testing tables for event sourcing scenarios:
| Scenario | Given Events | When Command | Then Events | Then State |
|----------|--------------|--------------|-------------|------------|
| Create account | (none) | CreateAccount | AccountCreated | Balance = 0 |
| Deposit money | AccountCreated | Deposit 100 | MoneyDeposited | Balance = 100 |
| Withdraw (sufficient funds) | AccountCreated, Deposit 100 | Withdraw 50 | MoneyWithdrawn | Balance = 50 |
| Withdraw (insufficient funds) | AccountCreated | Withdraw 50 | (none) | Error: Insufficient funds |

**Why**: Event sourcing is **state machine logic**. Tables make the state transitions explicit.

**Concrete action**: In the NeoBank tutorial, after implementing command handlers, add:
```markdown
## Test Your Command Handlers

Use these scenarios to verify your implementation:

[Table as above]
```

### 9. Adopt "See Also" Links to Diataxis Sections
**Pattern**: Stripe's guides end with "See also" links to related guides.  
**Apply to NeoHaskell**: Tutorial sections should link to:
- **How-to Guides** (e.g., "How to implement custom validation")
- **Explanation** (e.g., "Why event sourcing?")
- **Reference** (e.g., "Command type reference")

**Why**: Diataxis separates learning (tutorials) from reference (API docs) from explanation (concepts). "See also" links help developers navigate between these modes.

**Concrete action**: At the end of each tutorial section, add:
```markdown
## See Also

- **How-to**: [Implement custom validation](../guides/custom-validation.md)
- **Explanation**: [Why event sourcing?](../concepts/event-sourcing.md)
- **Reference**: [Command types](../reference/commands.md)
```

### 10. Adopt Progressive Disclosure for Syntax Asides
**Pattern**: Stripe uses "Optional:" sections and collapsible details.  
**Apply to NeoHaskell**: Use **expandable syntax asides** in the tutorial. The main narrative focuses on **what you're building** (NeoBank), with syntax details hidden in expandable sections.

**Why**: Prevents syntax from overwhelming the narrative. Developers can expand asides when they're ready to learn the syntax details.

**Concrete action**: In the NeoBank tutorial, when introducing a new syntax feature, use:
```markdown
We define the `CreateAccount` command like this:

\`\`\`haskell
data CreateAccount = CreateAccount
  { accountId :: AccountId
  , initialBalance :: Money
  }
\`\`\`

<details>
<summary>Syntax aside: Record syntax</summary>

The `{ ... }` syntax defines a record type. Each field has a name and a type. This is similar to:
- JavaScript: `{ accountId: string, initialBalance: number }`
- Python: `@dataclass` with typed fields
- Go: `struct { AccountId string; InitialBalance int }`

[More syntax details...]
</details>
```

### 11. Adopt Cross-Platform Installation Instructions
**Pattern**: Stripe's CLI installation guide includes platform-specific tabs (homebrew, apt, Windows, Docker).  
**Apply to NeoHaskell**: The "Getting Started" guide should include platform-specific installation for:
- macOS (homebrew)
- Linux (apt, yum, manual)
- Windows (Scoop, manual)
- Docker

**Why**: Reduces installation friction. Developers shouldn't need to "figure out" how to install on their OS.

**Concrete action**: Create a `getting-started/installation.md` with platform tabs:
```markdown
## Install NeoHaskell

### macOS
\`\`\`bash
brew install neohaskell/tap/neo
\`\`\`

### Linux (apt)
\`\`\`bash
curl -s https://get.neohaskell.org/gpg | sudo apt-key add -
echo "deb https://get.neohaskell.org/apt stable main" | sudo tee /etc/apt/sources.list.d/neohaskell.list
sudo apt update && sudo apt install neo
\`\`\`

### Windows (Scoop)
\`\`\`powershell
scoop bucket add neohaskell https://github.com/neohaskell/scoop-neo
scoop install neo
\`\`\`

### Docker
\`\`\`bash
docker pull neohaskell/neo:latest
\`\`\`
```

### 12. Adopt "What You Learn" Upfront in Tutorials
**Pattern**: Stripe's quickstart includes a "What you learn" section at the top.  
**Apply to NeoHaskell**: Each tutorial section should start with:
```markdown
## Build the Account Aggregate

**What you'll learn**:
- How to define commands and events
- How to validate business rules
- How to produce events from commands
- How to rebuild state from events

**Time**: 15 minutes  
**Complexity**: 2/5
```

**Why**: Sets expectations. Developers know what they'll learn and how long it will take before they start.

### Summary of Concrete Actions

1. **Create use-case entry points** on the tutorial landing page
2. **Add language switcher** to show JS/Python/Go/Haskell equivalents side-by-side
3. **Add "Verify your progress" checkpoints** after each tutorial step
4. **Add inline glossary tooltips** for event sourcing terms
5. **Mark advanced sections as "Optional:"** (snapshots, versioning, custom projections)
6. **Add complexity ratings** to tutorial section headers
7. **Emphasize event-driven thinking** throughout the tutorial
8. **Create testing tables** for event sourcing scenarios
9. **Add "See also" links** to How-to/Explanation/Reference sections
10. **Use expandable syntax asides** to separate narrative from syntax details
11. **Add platform-specific installation instructions** to Getting Started
12. **Add "What you'll learn" sections** at the start of each tutorial section

These patterns directly address NeoHaskell's challenge: **teaching event sourcing (unfamiliar concept) alongside NeoHaskell syntax (unfamiliar language)** to developers from JS/TS, Python, Go, and Haskell backgrounds.
