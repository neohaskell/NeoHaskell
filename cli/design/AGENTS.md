# AGENTS.md — Neo CLI Design

## Overview

This folder contains the design documents for the Neo CLI tool. The CLI is the primary interface for NeoHaskell developers — and the first thing they touch.

Every decision in this document serves one person: Jess.

---

## Who We're Designing For

**Jess** is a junior software developer working full-time at a TypeScript shop. Evenings and weekends, she builds side projects to grow her portfolio — maybe one becomes a startup. She's comfortable with statically typed mainstream languages (TypeScript, Java), interested in learning new things, but only as steps toward her goal. She has maybe 15 minutes at the end of the day.

**What Jess wants:**
- Build a project as fast as possible to test an idea
- Start writing code in hours, not days
- A CLI that does everything — no researching and configuring separate tools
- To understand the code she's looking at

**What Jess does NOT want:**
- To learn a language in-depth before being productive
- To learn software architecture theory before shipping
- To spend time on thorough testing before she has a working prototype
- To deal with tools that have pitfalls that waste more time than they save

**What Jess doesn't know yet:**
- She values working in a domain-driven way (she just doesn't have the vocabulary)
- Event Modeling will feel natural to her once she sees it in action (Command → Event → Query)

**Who Jess is NOT:** Experienced Haskell developers. Type theory enthusiasts. FP veterans looking for advanced abstractions. Those users are welcome, but we don't design for them.

Every design question has one test: **would Jess figure this out in 15 minutes?**

---

## Design Principles

Three principles guide every decision. When they conflict, resolve in this order: Least Astonishment > Least Effort > Developer Happiness.

### Principle of Least Astonishment

The system must behave the way Jess expects, based on what she already knows.

- Use conventions from her world: JSON for config (not TOML/Dhall), Git (not Darcs), GitHub, VS Code
- Command names should do what they sound like: `neo run` runs, `neo build` builds
- No surprising side effects — if a command creates files, say which ones
- Error messages should confirm what she tried and say what went wrong
- When in doubt: do what TypeScript/npm/Cargo would do

### Principle of Least Effort

Minimize steps, decisions, and cognitive load. Jess is learning a new language AND a new architecture — remove everything else.

- One command to create a project, one to run it, one to build it
- No flags when there's one obvious default
- No configuration files to write before getting started
- Standard library handles common tasks — don't make her find packages for basics
- Error messages tell her what to do, not just what failed
- If she needs to learn a concept (Entity, Command, Event), teach it through the generated code — not through docs she has to read first

### Principle of Developer Happiness

Jess should feel good using NeoHaskell, not frustrated or stupid.

- Clear documentation with code examples (not academic explanations)
- The generated Counter example is her first teacher — it must be immediately understandable
- Errors should never blame the user or use jargon she doesn't know
- The CLI should feel responsive and respectful of her time
- Working code on first run — `neo init my-project && cd my-project && neo run` must work, always

---

## Design Process: Event Modeling for NeoHaskell

We use Event Modeling (Adam Dymitruk / Martin Dilger) adapted to text-based specification for NeoHaskell projects.

### Why Event Modeling?

- **Visual clarity** — Both technical and non-technical stakeholders understand it
- **Complete specification** — Commands, Events, Queries, Integrations all defined upfront
- **Vertical slices** — Features are independently deliverable
- **NeoHaskell native** — Maps 1:1 to NeoHaskell's architecture
- **Jess-friendly** — The vocabulary is concrete ("place an order", "order was placed", "show me the order") not abstract

---

## Core Concepts

### The 4 Patterns

Every Event Model uses exactly four patterns:

```
Pattern 1: State Change
[UI] → [Command] → [Event]
User intent causes a recorded fact.

Pattern 2: View
[Event] → [Query] → [UI]
Facts project into queryable views.

Pattern 3: Automation (Policy)
[Event] → [Integration] → [Command]
A fact triggers automated action.

Pattern 4: External Input
[External System] → [Integration] → [Command]
External trigger causes internal action.
```

### Mapping to NeoHaskell

| Event Modeling | NeoHaskell | Description |
|----------------|------------|-------------|
| Swim Lane | **Entity** | State reduced/projected from events. Transaction boundary. |
| Command | **Command** | User/system intent. Imperative mood. May be rejected. |
| Event | **Event** | Immutable fact. Past tense. Append-only. |
| Read Model | **Query** | Projection for reading. **Can access multiple Entities.** |
| Integration | **Integration** | Automation. Outbound (event→effect/command), Inbound (external trigger→command). |

### NeoHaskell-Specific Rules

1. **Queries can read multiple Entities** — Unlike traditional Event Sourcing where read models belong to one aggregate, NeoHaskell Queries can join across Entities.

2. **Cross-entity communication always via Integration** — Never Command → Command directly. Always: Event → Integration → Command.

3. **Events are immutable and complete** — Must contain all fields needed for replay. No external lookups during replay.

4. **Folder structure per Entity:**
   ```
   src/
   ├── Main.nh                # Thin launcher — calls Application.run
   ├── App.nh                 # Application composition — all wiring
   ├── Config.nh              # Project configuration (env vars, CLI args, defaults)
   └── MyEntity/
       ├── Core.nh             # Domain types, helpers shared across the module
       ├── Entity.nh           # Entity definition, event ADT, update function
       ├── Service.nh          # Command registration
       ├── Commands/
       │   └── MyCommand.nh    # Command type, decide function
       ├── Events/
       │   └── MyCommand.nh    # Event type produced by MyCommand
       ├── Queries/
       │   └── MyQuery.nh      # Query projection
       └── Integrations.nh     # Outbound (event→effect) + Inbound (trigger→command)
   tests/
   ├── Main.nh                # Auto-discovers *Spec.nh files (hspec-discover)
   ├── MyEntity/
   │   ├── EntitySpec.nh      # Unit tests for entity update function
   │   └── Commands/
   │       └── MyCommandSpec.nh  # Unit tests for command decide function
   ├── commands/
   │   └── my-command.hurl    # Integration test: POST /commands/my-command
   ├── queries/
   │   └── my-query.hurl      # Integration test: GET /queries/my-query
   └── scenarios/
       └── my-workflow.hurl   # End-to-end: command → query → verify
   ```

---

## Text-Based Event Model Format

When designing a feature, use this format:

### Entity Specification

```markdown
# Entity: EntityName

## State
- field1: Type
- field2: Type

## Commands → Events

### Command: CommandName
- **Fields:** { field1: Type, field2: Type }
- **Produces:** EventName
- **Validation:**
  - Rule 1
  - Rule 2
- **Errors:** [Error1, Error2]

## Events

### Event: EventName
- **Fields:** { entityId: EntityId, field1: Type, timestamp: UTCTime, ... }
- **Caused by:** CommandName

## Queries

### Query: QueryName
- **Input:** { param1: Type }
- **Output:** { field1: Type, field2: Type }
- **Fed by:** [Event1, Event2]
- **Reads from:** [Entity1, Entity2]  # If cross-entity

## Integrations

### Integration: PolicyName
- **Trigger:** EventX
- **Action:** CommandY (on EntityZ)
- **Error handling:** ...
```

---

## Design Process (Step by Step)

### Phase 1: Brain Dump Events
1. List all things that "happen" in the system
2. Use past tense: "ProjectCreated", "DependencyAdded"
3. Don't filter — capture everything

**Prompts:**
- "What facts does this system record?"
- "What would an auditor want to see?"
- "What triggers notifications?"

### Phase 2: Organize by Entity
1. Group events by the noun they belong to (Project, Dependency, Config)
2. Each group = one Entity (swim lane)
3. Events that span entities → need Integration

### Phase 3: Add Commands
1. For each event, ask: "What action caused this?"
2. Write in imperative mood: "CreateProject", "AddDependency"
3. One command can produce multiple events (but watch for cross-entity smell)

### Phase 4: Add Queries
1. For each event, ask: "Who needs to see this data?"
2. Define the view shape
3. Note which events feed each query

### Phase 5: Add Integrations
1. Identify cross-entity flows
2. Identify external triggers (webhooks, cron, file watchers)
3. Define: Trigger → Action

### Phase 6: Slice into Features
1. Draw vertical lines grouping related elements
2. Each slice = independently deployable feature
3. Order by priority (leftmost = first)

### Phase 7: Detail Fields
1. For each Command: all input fields, validation rules, error cases
2. For each Event: all fields needed for replay
3. For each Query: input params, output shape

### Phase 8: Validate
- [ ] Every Command produces ≥1 Event
- [ ] Every Event feeds ≥1 Query or Integration
- [ ] Cross-entity communication uses Integrations
- [ ] No orphan events (events with no producer)
- [ ] No dead-end events (events with no consumer)

---

## Example: Counter (from neo init template)

```markdown
# Entity: Counter

## State
- counterId: CounterId
- value: Int

## Event ADT (in Entity.nh)
type CounterEvent
  = Increment Increment.Event

## Update Function (in Entity.nh)
update : CounterEvent -> Counter -> Counter
-- Pattern matches on each event branch, applies state change

## Service (in Service.nh)
service = Service.new |> Service.command @Increment

## Commands → Events

### Command: Increment (in Commands/Increment.nh)
- **Command type:** Increment { counterId: CounterId, amount: Int }
- **Validation:**
  - amount > 0
  - Counter must exist
- **Errors:** [CounterNotFound, InvalidAmount]

### Event: Incremented (in Events/Increment.nh)
- **Event type:** Incremented { counterId: CounterId, amount: Int, newValue: Int, timestamp: UTCTime }
- **Produced by:** Increment command

## Queries

### Query: CurrentCount (in Queries/CurrentCount.nh)
- **Input:** { counterId: CounterId }
- **Output:** { counterId: CounterId, value: Int }
- **Fed by:** [CounterIncremented]

## Integrations (in Integrations.nh)

### Outbound: LogIncrement
- **Trigger:** Incremented event
- **Action:** Log the new count to console
- Demonstrates: reacting to domain events with side effects

### Inbound: AutoIncrementer
- **Trigger:** Timer (every 1 second)
- **Action:** Emit Increment { amount = 1 } command
- Demonstrates: external trigger → command (Pattern 4)
```

---

## Files in This Folder

- **user-flows.md** — CLI command chapters (neo init, neo add, neo build, etc.)
- **AGENTS.md** — This file. Process documentation for agents.

---

## Working on CLI Chapters

Each CLI command is a "chapter" in `user-flows.md`. When defining a chapter:

1. **Define the user flow** — What does the user type? What happens?
2. **Define generated structure** — What files/folders are created?
3. **Define the example** — What working code is generated?
4. **Define errors** — What can go wrong? How do we communicate it?

Use Event Modeling when the CLI command involves domain logic (e.g., `neo add command` creates domain structures).

### Checklist (apply to every chapter)

- [ ] **Jess test**: Would Jess understand this command with no documentation? Would she succeed on her first try?
- [ ] **Least Astonishment**: Does it behave like she'd expect from npm/cargo/mix?
- [ ] **Least Effort**: Is every flag/option truly necessary? Can we remove it and pick a default?
- [ ] **Developer Happiness**: Are error messages helpful, not hostile? Does success output feel good?
- [ ] **Event Modeling**: If the command involves domain concepts, does the generated code follow the patterns (Entity, Command, Event, Query)?

---

## References

- [NHEP 2 — Target and User Persona](https://github.com/neohaskell/NHEP/blob/main/nhep/0002-project-target/index.mdx)
- [NHEP 3 — Principles of Design](https://github.com/neohaskell/NHEP/blob/main/nhep/0003-principles-of-design/index.mdx)
- [Event Modeling — Adam Dymitruk](https://eventmodeling.org/)
- [Martin Dilger's Event Modeling talks](https://www.youtube.com/@martindilger)
- NeoHaskell docs (internal)
