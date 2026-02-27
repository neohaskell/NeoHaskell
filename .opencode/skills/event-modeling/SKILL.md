---
name: event-modeling
description: "Use when planning a new application, feature, or system with a user. Facilitates the Event Modeling process (Adam Dymitruk / Martin Dilger) to produce implementation-ready blueprints with testable Given/When/Then specifications. Do NOT use for implementation details, code review, debugging, or refactoring existing code."
tags:
  - planning
  - architecture
  - event-modeling
  - design
  - specification
---

# Event Modeling — Planning Skill

Facilitate Event Modeling sessions with users — from brainstorming to complete specification. Based on Adam Dymitruk's 7-step process and Martin Dilger's practical refinements. Produces implementation-ready blueprints with testable specifications.

## Trigger Conditions

- User says "let's plan an app", "design a system", "event model this"
- User describes a new feature or application they want to build
- User wants to break down a system into implementable slices
- User is starting a NeoHaskell project and needs to define entities, commands, events, queries

## Do NOT Use When

- User wants to implement code (use normal development workflow)
- User wants a code review or refactor
- User is debugging an existing system
- User is asking about NeoHaskell syntax or API usage

## What is Event Modeling?

Event Modeling is a visual specification technique that documents how an information system works by showing the flow of information through time. Created by Adam Dymitruk (2019), refined by Martin Dilger.

**Core idea**: The system is described as a timeline of events (facts) connected by user actions (commands) and queries (called "read models" in Event Modeling). The model IS the specification — no separate requirements document.

**Three building blocks**: Commands (blue), Events (orange), Queries (green)
**Four patterns**: State Change, State View, Integration, Translation

## The Four Patterns

Every information system uses exactly four patterns:

### Pattern 1: State Change
```
[Screen] → [Command] → [Event]
```
User intent causes a recorded fact. Example: User clicks "Place Order" → PlaceOrder command → OrderPlaced event.

**Specification format (Given/When/Then):**
```
Given: Customer has items in cart AND valid payment
When: Customer submits order
Then: OrderPlaced event is stored with orderId, items, total, timestamp
```

### Pattern 2: State View (Query)
```
[Event(s)] → [Query] → [Screen]
```
Accumulated facts project into a queryable view. Example: OrderPlaced + OrderShipped events → Order Status query.

**Specification format (Given/Then — no "When"):**
```
Given: OrderPlaced event with orderId "ORD-123"
  AND OrderShipped event with tracking "TRK-456"
Then: Order Status query shows status "shipped", tracking "TRK-456"
```

### Pattern 3: Integration (called "Automation" or "Policy" in Event Modeling)
```
[Event] → [Integration] → [Command]
```
A fact triggers an automated action. Example: OrderPlaced event → PaymentIntegration → ProcessPayment command.

**Specification format:**
```
Given: OrderPlaced event is stored
Then: ProcessPayment command is issued to payment gateway
  AND PaymentProcessed or PaymentFailed event is stored
```

### Pattern 4: Translation (External Input)
```
[External System] → [Translation] → [Event]
```
External information is translated into domain-meaningful events. Example: GPS coordinates → "GuestLeftHotel" event.

---

## The 7-Step Process

Follow these steps IN ORDER when planning with a user. Each step builds on the previous.

### Step 1: Brain Dump Events

**Goal**: Capture every possible event (fact) in the system.

**How to facilitate**:
- Ask: "What things HAPPEN in this system? What facts get recorded?"
- Ask: "If an auditor looked at this system a year from now, what entries would they see?"
- Ask: "What triggers notifications?"
- Capture everything — don't filter yet
- Gently correct non-events: "User viewed page" is NOT an event (nothing changed). "User registered" IS an event.

**Rules**:
- Events are past tense: "OrderPlaced", "UserRegistered", "PaymentProcessed"
- Events are facts — they happened, they're immutable
- Don't worry about order yet
- Don't worry about who triggered them yet

**Output**: Unordered list of domain events

**Martin Dilger's technique — "Intentional Chaos"**: Start with chaos, not structure. Ask the user to throw everything out there. The chaos reveals architectural flaws that structured thinking misses. Look for:
- Manual processes ("Larry always checks that") — single points of failure
- Triple redundancy — same process exists three ways
- Ghost processes — invisible dependencies

### Step 2: The Plot (Timeline)

**Goal**: Arrange events into a plausible chronological story.

**How to facilitate**:
- Ask: "If we watched this system run for a day/week/year, in what order would these events happen?"
- Arrange events left-to-right on a timeline
- Ask: "Does this sequence make sense? Can Event B happen before Event A?"
- Identify missing events: "Wait, how do we get from A to C? Something must happen in between."

**Rules**:
- One single timeline — no branching
- The story must be plausible from start to finish
- Missing events will become obvious

**Output**: Ordered timeline of events

### Step 3: The Storyboard (Screens)

**Goal**: Add wireframes/mockups showing what users see at each point.

**How to facilitate**:
- For each event, ask: "What screen was the user looking at when this happened?"
- Ask: "What screen shows the RESULT of this event?"
- Use rough wireframes — just enough to show data fields and actions
- Organize screens into entities (called "swimlanes" in Event Modeling) by user type (Customer, Admin, System)

**Rules**:
- Every field in a screen must have a source (where does the data come from?) and a destination (where does it go?)
- No screens stacked vertically — each state change is a separate column
- Different users get different entities
- Integrations get an entity too (gear icon)

**Output**: Wireframes linked to events on timeline

**Text-based format** (for terminal-based planning):
```
Entity: Customer
  Screen: Registration Form [name, email, password]
  Screen: Dashboard [order list, account info]
  Screen: Order Confirmation [orderId, items, total, eta]

Entity: Admin
  Screen: Order Management [orders table, status filters]
  Screen: Customer Support [customer details, order history]

Entity: System
  Integration: Payment Processing [payment queue]
  Integration: Email Notifications [notification queue]
```

### Step 4: Identify Inputs (Commands)

**Goal**: For each event, identify the command (user intent) that triggers it.

**How to facilitate**:
- For each event, ask: "What action caused this? Who or what triggered it?"
- Ask: "What data does the user provide when they do this?"
- Name commands in imperative mood: "PlaceOrder", "RegisterUser", "CancelSubscription"

**Rules**:
- One command may produce one event (most common)
- One command should NOT produce many events (anti-pattern: "Left Chair")
- Commands capture intent — what the user WANTS to happen
- Commands may be rejected (validation fails)

**Output**: Commands linked to events, with input fields defined

### Step 5: Identify Outputs (Queries)

**Goal**: For each screen, identify which events feed the data into it.

**How to facilitate**:
- For each screen field, ask: "Which event(s) provide this data?"
- If a field has no source event → you found a missing event
- Multiple events can feed one query
- One event can feed multiple queries

**Rules**:
- Queries are optimized for reads (denormalized)
- Queries can be rebuilt from events at any time
- Queries are passive — they cannot reject events

**Output**: Queries linked to events, with field-level traceability

### Step 6: Organize into Entities (Conway's Law)

**Goal**: Group events into domains/modules that separate teams can own.

**How to facilitate**:
- Ask: "Which events belong together? Which ones are about the same 'thing'?"
- Each group becomes an entity = bounded context = module
- Cross-entity communication happens via events + integrations (Pattern 3/4)

**Rules**:
- Never Command → Command across entities. Always: Event → Integration → Command
- Each entity should be ownable by one team
- Events are the only coupling between entities
- Entities map to code modules: `src/EntityName/`

**Output**: Events organized into entities with clear boundaries

### Step 7: Elaborate Scenarios (Given/When/Then)

**Goal**: Write precise specifications for each command and query.

**How to facilitate**:
- For each command: "What are the happy path conditions? What can go wrong?"
- For each query: "Given these events have happened, what should the view show?"
- Each specification ties to EXACTLY one command or one query

**Rules**:
- Use concrete examples, not abstract rules
- Cover: happy path, validation errors, edge cases, idempotency
- Every field in every element must have a documented source

**Specification templates**:

For State Change (Command):
```
SLICE: [Name]

COMMAND: [CommandName]
  Fields: { field1: Type, field2: Type }
  Preconditions: [list]

EVENT: [EventName]
  Fields: { field1: Type (from command.field1), field2: Type (calculated) }

SCENARIO: Happy Path
  Given: [preconditions — expressed as prior events or system state]
  When: [command is issued with specific data]
  Then: [event is stored with specific fields]

SCENARIO: Validation Error — [Name]
  Given: [conditions that make the command invalid]
  When: [command is issued]
  Then: [error returned, no event stored]
```

For State View (Query):
```
QUERY: [QueryName]
  Fields:
    - field1 (from EventA.field1)
    - field2 (from EventB.field2)
    - field3 (calculated: EventA.x + EventB.y)

SCENARIO: [Name]
  Given: [list of events that have occurred]
  Then: [query shows specific data]
```

### Completeness Check (MANDATORY after Step 7)

Run this checklist before declaring the model complete:

- [ ] Every screen field has a source event
- [ ] Every event has a source command (or external system)
- [ ] Every command has a source UI action (or integration)
- [ ] Every query field can be traced to source event(s)
- [ ] No "magic data" — all data is explicitly sourced
- [ ] Events are past tense, specific (not generic "DataChanged")
- [ ] Commands are imperative, express intent
- [ ] Each slice has at least one Given/When/Then scenario
- [ ] Cross-entity communication uses integrations (never direct command→command)

---

## Slices: The Unit of Work

A **slice** is a complete vertical loop:
```
Screen → Command → Event → Query → Screen
```

### Why Slices Matter

- **Estimable**: Each slice has roughly constant complexity
- **Testable**: Clear Given/When/Then scenarios
- **Implementable in isolation**: No dependencies on other slices
- **Parallelizable**: Different teams/agents can work different slices simultaneously
- **Flat cost curve**: Adding feature N+1 costs the same as feature 1

### Slice Size Guide

Too big: "Shopping Cart" (multiple actions)
Right size: "Add Item to Cart" (one action, one outcome)
Too small: "Validate Cart Item Price" (implementation detail, not user action)

**Rule of thumb**: One button click and the process behind it = one slice.

---

## Anti-Patterns to Watch For

These visual patterns (from Martin Dilger) indicate overcomplicated models:

### The Left Chair
One command → many events (3+). Someone's cramming too much business logic into one command.
**Fix**: Break into separate commands — one decision per command.

### The Right Chair (mirrored)
Many events → one query consuming everything. High coupling.
**Fix**: Distribute into focused queries with clear responsibility.

### The Bed
One UI action → many commands in sequence. UI is orchestrating what the system should handle.
**Fix**: Let the system orchestrate via events and integrations.

### The Book Shelf
One slice has all the logic; every other slice is anemic. A god-object in visual form.
**Fix**: Distribute business rules across slices.

### Detection Questions
- "Is this command doing too many things?" (Left Chair)
- "Is this query responsible for too much?" (Right Chair)
- "Is the UI orchestrating what the system should handle?" (Bed)
- "Is all the logic concentrated in one place?" (Book Shelf)

---

## Common Mistakes to Catch

### Mistake 1: Modeling Technology Instead of Information
Wrong: "PlaceOrder (via REST) → OrderPlaced (published to Kafka)"
Right: "PlaceOrder → OrderPlaced" — technology decisions come later.

### Mistake 2: Generic Events
Wrong: `DataChanged { entityType, fieldName, oldValue, newValue }`
Right: `CustomerNameChanged { customerId, newName }` — specific events are self-documenting.

### Mistake 3: Queries Without Event Sources
Every field in a query must trace back to a source event. If a field has no source, you found a missing event.

### Mistake 4: Architecture First
Event Modeling creates the vision. Don't start with an architectural assumption and use the model to "confirm" it.

### Mistake 5: One Giant Model
Don't cram everything into one massive model. Model one business capability at a time.

---

## Breaking "The Wall" (Martin Dilger)

When the user gets stuck or says "I'm not sure about this part":

1. Ask: "What would you ASSUME the answer is? We can validate it later."
2. Ask: "If we skip this, what's the next thing you DO know?"
3. Document assumptions explicitly — they become validation items later.
4. Mark unknowns with a hotspot (red/❓) and continue.
5. Never let uncertainty stop progress.

**The Two-Question Technique** (when user says "I need to ask someone"):
- "If that person were here right now, what do you THINK they would say?"
- "Can we write that as an assumption and verify it later?"

---

## When Things Go Wrong During a Session

### User contradicts an earlier decision
- Don't silently override. Say: "Earlier we said [X]. You're now suggesting [Y]. Which one should we go with?"
- Update all affected slices if the decision changes.

### User can't identify events
- Switch to commands first: "What can a user DO in this system?" Then derive events from commands.
- Or switch to screens: "What screens exist?" Then work backwards to events.

### Model feels too complex
- Check for anti-patterns (Left Chair, Right Chair, Bed, Book Shelf).
- Split into separate models per business capability.
- Ask: "If we launched with ONLY [subset], would it be useful?" — find the MVP boundary.

### User wants to skip steps
- Steps 1-2 (brain dump + timeline) are non-negotiable — they catch missing events.
- Steps 3 (screens) can be simplified to just field lists if wireframes feel heavy.
- Steps 4-5 (commands + queries) can be done together if the user is experienced.
- Step 7 (scenarios) can start with happy paths only, adding error cases later.

---

## Facilitation Questions by Phase

**Brain Dump**:
- "What would an auditor want to see?"
- "What triggers notifications or emails?"
- "What can go wrong in this system?"
- "What does the user see after each action?"

**Timeline**:
- "Can this happen before that? What if it does?"
- "What's missing between these two events?"
- "Is this the only path, or are there alternatives?"

**Commands**:
- "What data does the user provide here?"
- "What validation should happen before accepting this?"
- "What if the user submits this twice? (idempotency)"

**Queries**:
- "Who needs to see this information?"
- "What fields appear on this screen?"
- "Where does each field come from — which event?"

---

## Estimation from Event Models

- Count slices in the completed model
- Each slice ≈ constant effort (the flat cost curve principle)
- Measure actual cycle time on first few slices → forecast the rest
- No story points needed — just: (slice count) × (measured cycle time per slice)
- Re-prioritizing slices doesn't change total cost (slices are independent)

---

## NeoHaskell Code Structure

Each entity in the model maps directly to code:

| Model Element | Code Location |
|---|---|
| Entity | `src/EntityName/` |
| Command | `src/EntityName/Commands/CommandName.hs` |
| Event | `src/EntityName/Events/CommandName.hs` |
| Query | `src/EntityName/Queries/QueryName.hs` |
| Integration (outbound) | `src/EntityName/Integrations.hs` |
| Integration (inbound) | `src/EntityName/Integrations.hs` |
| Entity State | `src/EntityName/Entity.hs` |
| Service | `src/EntityName/Service.hs` |

### NeoHaskell Style in Specifications
- Use `|>` pipes, not `$`
- Use `do` blocks, not `let..in` or `where`
- Use `case..of` for pattern matching, not function definition patterns
- Use `Task` not `IO`, `Result` not `Either`
- Use qualified imports: `EventStore.new`, `Service.command`
- Use `[fmt|Hello {name}!|]` for string interpolation, not `<>` or `++`

---

## Text-Based Event Model Format

When working in a terminal (no whiteboard), use this structured format:

```markdown
# Event Model: [System Name]

## Entities

- [Entity 1]: [description]
- [Entity 2]: [description]

## Timeline

### Phase 1: [Phase Name]

#### Slice: [SliceName]
- **Screen**: [description of what user sees, key fields]
- **Command**: [CommandName] { field1: Type, field2: Type }
- **Event**: [EventName] { field1: Type, field2: Type, timestamp: UTCTime }
- **Query**: [QueryName] { field1 (from EventName.field1), field2 (calculated) }
- **Scenarios**:
  - Happy: Given [precondition]. When [action]. Then [outcome].
  - Error: Given [bad state]. When [action]. Then [error message].

#### Slice: [SliceName]
...

### Phase 2: [Phase Name]
...

## Integrations
- [IntegrationName]: Triggered by [EventName] → Issues [CommandName] to [Entity]
- [TranslationName]: External [SystemName] → Translates to [EventName]

## Completeness Check
- [ ] All screen fields traced to events
- [ ] All events traced to commands
- [ ] All commands traced to UI actions
- [ ] Cross-entity uses integrations only
```

---

## References

- Adam Dymitruk, "Event Modeling: What is it?" — https://eventmodeling.org/posts/what-is-event-modeling/
- Martin Dilger, "Understanding Eventsourcing" — https://leanpub.com/eventmodeling-and-eventsourcing
- Event Modeling & Event Sourcing Podcast — https://podcast.eventmodeling.org/
- Confluent "Practical Event Modeling" course — https://developer.confluent.io/courses/event-modeling/
- NeoHaskell AGENTS.md — `cli/design/AGENTS.md` (NeoHaskell-specific conventions)
