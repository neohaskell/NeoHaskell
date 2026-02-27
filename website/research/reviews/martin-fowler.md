# Review of the NeoHaskell Documentation Plan

**Reviewer: Martin Fowler**
**Date: February 2026**

---

## Preface

I've spent thirty years writing about enterprise architecture, fifteen of those specifically about patterns that include Event Sourcing, CQRS, and Domain-Driven Design. I popularized these patterns not because they're universally superior, but because they solve specific problems that conventional architectures handle poorly. That distinction — *when* to use a pattern, not just *how* — is the thing most documentation gets wrong, and it's the lens through which I'll evaluate this plan.

I've read Steve Klabnik's review. His perspective is pedagogical — how do you teach effectively? Mine is architectural — are you teaching the right things? We'll intersect in places, particularly around the tutorial's pacing and the treatment of cross-aggregate coordination. But my concerns are substantially different from his, because the failure modes I worry about are not confused learners but *confidently wrong* practitioners: people who finish the tutorial thinking they understand event sourcing when they've actually learned a simplified model that will mislead them in production.

The plan is ambitious and thoughtful. The banking domain is excellent. The "implicit-first" teaching philosophy is sound. What follows are the places where the plan's treatment of event sourcing, DDD, and CQRS is imprecise, incomplete, or actively misleading — and what to do about it.

---

## I. The Rosetta Stone Conflates Domain Entities with Architectural Patterns

**Responding to: "The Rosetta Stone" (lines 138–146)**

The Rosetta Stone maps developer language to banking terms to NeoHaskell terms:

| What Alex says | Banking term | NeoHaskell term | What it actually is |
|---|---|---|---|
| "My account" | Account | Aggregate | A consistency boundary for related events |
| "Something happened" | Transaction | Event | An immutable record of a fact |

**My critique:** This table makes a category error that will damage every reader's understanding of aggregates.

An aggregate is not "my account." An aggregate is a *consistency boundary* — a cluster of domain objects treated as a unit for the purpose of data changes. The Account *is modeled as* an aggregate, but the Rosetta Stone presents them as synonyms. This is like saying "a Car is a Vehicle" and then expecting readers to understand what makes something a Vehicle when they encounter Trucks and Motorcycles.

**Why this matters deeply:**

In Domain-Driven Design, the concept of an aggregate is one of the most misunderstood patterns in the entire catalogue. I've seen this confusion in dozens of enterprise projects. The confusion has a specific shape: developers learn that "aggregate = domain entity" and then they create one aggregate per database table, one aggregate per UI screen, or one aggregate per "important noun." This produces aggregates that are too large (because they include everything related to the noun) or too small (because they include only the noun's fields).

The *actual* insight of the aggregate pattern is about transactional consistency. An aggregate defines the boundary within which invariants must be maintained. When the plan says an aggregate is "a consistency boundary for related events," that's correct — but the Rosetta Stone row contradicts this by mapping it to "my account," which teaches the reader to think "aggregate = the thing I'm modeling" rather than "aggregate = the boundary within which business rules must hold atomically."

This matters concretely in Part 5 (Transfers). When the reader encounters cross-aggregate coordination, they need to understand *why* a transfer can't be handled within a single aggregate. If their mental model is "aggregate = account," the answer seems arbitrary ("why can't I just update both accounts?"). If their mental model is "aggregate = consistency boundary," the answer is structural ("each account maintains its own invariants independently, so coordinating between them requires a different mechanism").

**My specific recommendation:**

Revise the Rosetta Stone to separate the domain entity from the architectural pattern:

| What Alex says | Banking term | NeoHaskell term | What it actually is |
|---|---|---|---|
| "My account" | Account | Aggregate Root | The entry point to a cluster of related objects |
| "My account can't go negative" | Business rule | Aggregate Invariant | A rule enforced within a consistency boundary |
| "These events belong together" | Account ledger | Aggregate | A consistency boundary — events within it are always consistent |

Better yet, don't try to compress aggregates into a single Rosetta Stone row. Introduce the term in the tutorial narrative when it becomes necessary (Part 2, when the reader first encounters a business rule that must be enforced atomically), and create the concept page then. The Rosetta Stone should map *simple* concepts (event, command, projection). Aggregates are not simple.

Additionally, the row mapping "Transaction" to "Event" is imprecise. In banking, a transaction often encompasses multiple events — a transfer involves a debit event on one account and a credit event on another. The Rosetta Stone should say:

| What Alex says | Banking term | NeoHaskell term | What it actually is |
|---|---|---|---|
| "Something happened" | Ledger entry | Event | An immutable record of a single fact |

"Ledger entry" is more precise than "Transaction" because a ledger entry is always atomic and singular, while a transaction can be compound.

---

## II. Bounded Contexts Are Introduced at the Wrong Time and for the Wrong Reason

**Responding to: Tutorial Part 4 ("Multiple Accounts") and the Concept Page Schedule (lines 128–136, 383–389)**

The plan introduces bounded contexts in Part 4 ("Multiple Accounts"), with the rationale: "Reader has two account types — explain why they're separate."

**My critique:** Having a checking account and a savings account is NOT a bounded context separation. It's two instances of the same aggregate type, or at most two different aggregate types, within a single bounded context. The plan is teaching bounded contexts using an example that isn't one.

**Why this is a significant architectural error:**

A bounded context, as Eric Evans defined it and as I've written about extensively, is a *linguistic* and *model* boundary. Within a bounded context, every term has a precise, unambiguous meaning. The word "Account" means one thing in the Retail Banking context and a different thing in the Lending context. The word "Customer" means one thing in the Sales context and a different thing in the Support context.

Two account types within a banking service — checking and savings — share the same ubiquitous language. They share the same model. A deposit means the same thing regardless of account type. A balance means the same thing. These are not separate bounded contexts; they are variations within a single context.

The plan's "Retail vs. Lending" row in the Rosetta Stone (line 146) correctly identifies bounded contexts as "business divisions" with "independent models and their own events." But Part 4 doesn't introduce business divisions — it introduces multiple account types within the same division. This teaches the reader that "multiple things of different types" = "bounded contexts," which is precisely the wrong definition.

This confusion has downstream consequences. When Part 5 introduces transfers and claims "Two bounded contexts, and it just works," the reader will think bounded contexts are just "different kinds of things" rather than "independent models with different languages." They won't understand why cross-context communication is architecturally significant, because the plan never showed them contexts that actually have different *languages*.

**My specific recommendation:**

Delay bounded contexts until Part 5 or later. Part 4 should teach *multiple aggregates of the same type* (multiple accounts) and *multiple aggregate types* (checking vs. savings), but explicitly say: "These all live within the same bounded context — the Account Management context. They share the same language: deposit, withdraw, balance, and transaction all mean the same thing regardless of account type."

Then, in Part 5 or a new Part 6, introduce a *genuinely different* bounded context. The plan's disclaimer says NeoBank doesn't handle interest calculation — but interest calculation is actually a perfect second bounded context. In an Interest Calculation context, "Account" means something different (it's a balance snapshot with an interest rate, not a stream of deposits and withdrawals). "Transaction" means something different (it's an interest accrual, not a customer-initiated transfer). The *same domain* (banking) produces different bounded contexts because different sub-domains have different models.

If interest calculation is too complex, consider a Notification context. When a transfer completes, the Account Management context emits a `TransferCompleted` event. A Notification context consumes this event and sends an email. The Notification context has its own model of "Account" (just an email address and notification preferences, not a ledger). This demonstrates the key bounded context insight: the same real-world entity is modeled differently in different contexts.

---

## III. The Plan Undersells the Complexity of Projections and Misses the Core CQRS Insight

**Responding to: Tutorial Part 3 ("Transaction History") and the Rosetta Stone's projection row (lines 128–136, 138–146)**

The plan presents projections as "a bank statement" — state derived by folding over events. Part 3's "wow" moment is "A bank statement IS a projection."

**My critique:** This teaches projections as *formatting* (presenting events in a readable way) rather than as *transformation* (deriving materially different views from the same event stream). The reader learns that a projection shows you events in a pretty format. They don't learn that projections can *reshape* your data into structures that look nothing like the original events.

**Why this matters for CQRS understanding:**

The entire point of Command Query Responsibility Segregation — the pattern that projections enable — is that your write model (optimized for enforcing business rules) and your read model (optimized for answering queries) can be *radically different*. A bank statement that lists transactions chronologically is the most boring possible projection. It's essentially `SELECT * FROM events ORDER BY timestamp`. It doesn't demonstrate CQRS at all.

Here's what a projection should teach:

1. **The same events produce different read models.** From the same stream of `MoneyDeposited` and `MoneyWithdrawn` events, you can produce:
   - A current balance (a single number)
   - A monthly spending summary (aggregated by month)
   - A fraud risk score (counting declined transactions in a window)
   - A customer lifetime value (total deposits minus total withdrawals)

2. **Read models can denormalize aggressively.** A read model for "customer dashboard" might combine events from the Account aggregate with events from a Profile aggregate. This is impossible in the write model (different aggregates can't share a transaction boundary) but trivial in a read model (just subscribe to both event streams).

3. **Read models are disposable.** You can delete a projection and rebuild it from the event stream. This is the superpower of event sourcing that the plan's Part 6 ("Audit Everything") touches on, but it should be foreshadowed in Part 3.

The plan's Part 3 captures none of this. A chronological transaction list is something a CRUD system produces with `SELECT * FROM transactions WHERE account_id = ? ORDER BY created_at`. The reader's reaction should not be "a bank statement is a projection" (underwhelming — they already know how to list transactions) but rather "I can build *any* view from the same events, and I don't need to plan those views in advance" (the actual insight).

**My specific recommendation:**

Part 3 should build *two* projections from the same event stream:

1. **The transaction statement** (chronological list — the simple one, the one that maps to their CRUD experience)
2. **A monthly summary** (total deposits, total withdrawals, net change per month — a *different shape* from the underlying events)

Then the tutorial should say: "You just built two completely different views from the same events, without changing anything about how events are stored. In a CRUD system, you'd need to either query the transactions table differently or create a second table. Here, the data already contains everything — you just fold it differently."

This is the "wow" moment for CQRS. Not "I can see my events" but "I can see my events *any way I want*, and adding a new view doesn't require changing anything else."

Also: the concept page for `projections.mdx` should be explicit about eventual consistency. When you build a projection, it processes events asynchronously (in most real implementations). This means the read model can be *stale*. The plan's Misconception #6 says this "depends on the bounded context," which is vague to the point of being unhelpful. The concept page should say clearly: "Projections are eventually consistent by default. After writing an event, a query against the read model may not immediately reflect the change. For most use cases this is fine (bank statements update within milliseconds). For use cases that require immediate consistency, NeoHaskell provides [whatever the mechanism is]."

---

## IV. The Saga/Process Manager Treatment in Part 5 Is Dangerously Oversimplified

**Responding to: Tutorial Part 5 ("Transfers") (lines 128–136)**

Steve identified the Part 4 → Part 5 cognitive gap and recommended splitting Part 5 or adding a concept interlude. I agree with his diagnosis but want to go further, because the problem isn't just pacing — it's *conceptual honesty*.

**My critique:** The plan's "wow" moment for Part 5 is "Two bounded contexts, and it just works." This is the most dangerous sentence in the entire documentation plan. Cross-aggregate coordination in event-sourced systems is where things *stop* just working. Pretending otherwise sets the reader up for production failures.

**Why I feel so strongly about this:**

I have written extensively about the saga pattern (which I prefer to call a *process manager*, following the terminology Gregor Hohpe and I established in "Enterprise Integration Patterns"). This pattern exists *because* distributed coordination is hard. A transfer between two accounts in an event-sourced system involves:

1. **Initiating the transfer** (creating a `TransferInitiated` event)
2. **Debiting the source account** (creating a `MoneyDebited` event — what happens if the source account doesn't have sufficient funds? The transfer must be declined.)
3. **Crediting the destination account** (creating a `MoneyCredited` event — what happens if the destination account has been closed? The debit must be compensated.)
4. **Handling failure at any step** (what happens if the system crashes between the debit and the credit? You have money that left one account but didn't arrive at the other.)

This is the classic saga pattern: a sequence of local transactions, each of which can fail, with compensating transactions to undo partial progress. It's one of the hardest patterns in distributed systems, and it's the reason I've always been careful to describe it with appropriate caveats.

The plan says the events introduced in Part 5 are `TransferInitiated` and `TransferCompleted`. Where is `TransferFailed`? Where is the *compensation* event (`MoneyRefunded` or `DebitReversed`)? Where is the explanation of what happens when things go wrong between initiation and completion?

If the tutorial says "it just works," the reader will believe that event-sourced transfers are simple. Then they'll build a system where money disappears when a process crashes mid-transfer, or where a transfer to a closed account debits the source but never credits the destination. They'll blame NeoHaskell, or worse, they'll blame event sourcing.

**My specific recommendation:**

Part 5 needs to be honest about complexity while remaining teachable. Here's a progression I'd recommend:

**Part 5a: The Happy Path**
Build the transfer with `TransferInitiated`, `SourceDebited`, `DestinationCredited`, `TransferCompleted`. Show it working when everything succeeds. The reader sees the full event sequence.

**Part 5b: What Goes Wrong**
Introduce failure scenarios:
- Source account has insufficient funds → `TransferDeclined` (this is a command rejection, similar to Part 2's overdraft protection)
- Destination account doesn't exist → need to refund the source → `SourceCredited` (compensation)
- Show the reader that the event store captures the *entire story*, including the failure and recovery

**Part 5c: The Process Manager**
Introduce the process manager as the coordinator that handles the state machine: Initiated → SourceDebited → DestinationCredited → Completed (or → Failed → Compensated). Show this as a state machine diagram.

This transforms the "wow" moment from the misleading "it just works" to the genuinely impressive "the event store captured the entire saga, including failures, and I can see exactly what happened." That's a *real* selling point of event sourcing — not that it eliminates complexity, but that it makes complexity *visible and auditable*.

Steve recommended splitting Part 5 or adding a concept interlude. I'm recommending the specific split: happy path → failure cases → coordinator pattern. This mirrors how I teach sagas in my workshops: simple sequence first, then ask "what if step 2 fails?", then show the pattern that handles failure.

---

## V. Event Versioning / Schema Evolution Is Dangerously Underweighted

**Responding to: Misconception #9 (line 83) and the placement of schema evolution in "Advanced guide"**

The plan's misconception table says:

> **Misconception #9:** "Schema evolution will be a nightmare"
> **Truth:** Type system + versioning strategy (address honestly)
> **Where to address:** Advanced guide

**My critique:** Relegating schema evolution to an advanced guide is architectural negligence. Schema evolution is not an advanced topic — it's a *fundamental operational reality* of every event-sourced system that lives long enough to need a second version.

**Why this is one of the top three challenges of event sourcing:**

In a CRUD system, changing a database schema is hard but well-understood: you write a migration, it runs, and your data now conforms to the new schema. The old schema is gone.

In an event-sourced system, you *never delete old events*. If you change the structure of `MoneyDeposited` from `{ amount: Int }` to `{ amount: Decimal, currency: Currency }`, you still have millions of events in the store with the old shape. Every projection, every replay, every audit query must handle both versions. This is not an edge case — it's the normal state of any event-sourced system older than six months.

The plan's Misconception #9 says the truth is "Type system + versioning strategy (address honestly)" with the parenthetical "(address honestly)" doing an enormous amount of work. What does "address honestly" mean? Here's what it should mean:

1. **Event upcasting**: Old events are transformed to the current version at read time. This is the most common approach and NeoHaskell's type system can help here — but the reader needs to understand the pattern.

2. **Weak schema**: Events are versioned, and projections handle multiple versions. This is operationally simpler but makes projections more complex.

3. **Copy-and-replace event store**: You replay all events through an upcaster and create a new store. This works but requires downtime or careful orchestration.

4. **The types-ARE-the-schema claim**: The "Coming from Event Sourcing" page (line 443) says "No schema registry needed (types ARE the schema)." This is a half-truth that will alienate every experienced event sourcing practitioner. Types define the current schema. They don't define the mapping from version 1 to version 7. A schema registry (like Confluent's, or EventStoreDB's) provides that mapping. If NeoHaskell's type system provides an equivalent mechanism, the docs need to explain how. If it doesn't, the claim is misleading.

**My specific recommendation:**

1. **Move schema evolution from "Advanced guide" to a concept page introduced alongside Part 4 or Part 5.** By the time the reader has multiple event types, they should understand that event types will change over time and what that means.

2. **Add a Misconception #9 exercise in the tutorial.** After the reader has built their NeoBank, have them add a `currency` field to `MoneyDeposited`. What happens to the old events? How do you handle both versions? This is a "Break It" exercise for architecture, not syntax.

3. **Revise the "Coming from Event Sourcing" page's claims about schema.** Replace "No schema registry needed (types ARE the schema)" with an honest comparison:

| Concern | EventStoreDB/Kafka | NeoHaskell |
|---|---|---|
| Current event structure | Schema registry | Type definitions |
| Version compatibility | Schema compatibility checks | [whatever NeoHaskell provides] |
| Upcasting old events | Custom deserializers | [whatever NeoHaskell provides] |
| Breaking changes | Schema evolution policies | [whatever NeoHaskell provides] |

If NeoHaskell doesn't yet have answers to these questions, the docs should say so. Honest gaps are credible. False confidence is not.

---

## VI. The Plan Is Missing Critical Event Sourcing Patterns

**Responding to: The plan as a whole**

Several architectural patterns that are essential to any event-sourcing documentation are absent from the plan. These aren't nice-to-haves — they're patterns that every team building an event-sourced system will encounter within months.

### A. Idempotency

Nowhere in the plan — not in the tutorial, not in the concepts, not in the guides — does the word "idempotent" appear. In event-sourced systems, idempotency is not optional. Commands can be retried (network failures, timeouts, duplicated messages). If a `Deposit` command is processed twice, the account is credited twice. The system needs a mechanism to detect and reject duplicate commands.

This is especially critical for Part 5 (Transfers), where a retry could cause a double-debit. The plan's process manager discussion should include idempotency as a first-class concern.

**Recommendation:** Add idempotency to the Misconception list or to the Concepts section. Introduce it in Part 2 (Account Rules) alongside validation: "What happens if the same deposit command is sent twice?" This is a natural extension of the "the compiler prevents bugs" narrative.

### B. Snapshotting

The plan mentions snapshots exactly once, in the synthesis's "Optional: Optimize with Snapshots" sidebar. But snapshots are not merely an optimization — they're a fundamental concept that affects how you think about event streams.

The question "what is the current state of this aggregate?" has two answers in event-sourced systems: "replay all events from the beginning" or "start from a snapshot and replay events since the snapshot." Without snapshots, the plan's implicit model is "always replay everything," which works for a tutorial with 10 events and fails catastrophically for a production system with 10 million.

**Recommendation:** Introduce snapshots in the concept page for projections (Part 3) as a brief aside: "For large event streams, replaying from the beginning can be slow. Snapshots cache a point-in-time state so you only need to replay recent events. We'll cover this in the [Performance guide]." This foreshadows the concept without derailing the tutorial.

### C. Tombstones and Soft Deletes

The plan says events are immutable and append-only. But what happens when GDPR requires you to delete a customer's data? You can't delete events — that violates the append-only constraint. But you *must* delete personal data — that's a legal requirement.

This is one of the most common questions experienced developers ask about event sourcing, and the plan doesn't address it anywhere. Misconception #3 says "State changes are expressed as events; the compiler enforces this." But data deletion is a state change that *can't* be an event in the traditional sense — it must actually remove data, not just record that deletion happened.

**Recommendation:** Add this to the FAQ or to a guide: "Event Sourcing and Data Privacy." The honest answer involves crypto-shredding (encrypting personal data in events with a per-user key, then deleting the key when deletion is required) or event tombstoning (replacing events with anonymized versions). Neither is simple, and pretending the problem doesn't exist will be noticed by every reader working in a jurisdiction with data protection laws.

### D. Event Ordering and Causality

The plan never discusses event ordering guarantees. Within a single aggregate, events are ordered (that's the append-only log). But across aggregates, events may arrive out of order. In Part 5 (Transfers), what happens if the `DestinationCredited` event is processed by a projection before the `SourceDebited` event? The projection might briefly show an impossible state (destination has money that hasn't left the source yet).

**Recommendation:** Address this in the concept page for bounded contexts, or in the transfers tutorial. Show the reader that event ordering across aggregates is not guaranteed, and that projections must be designed to handle out-of-order events. This is a natural extension of the eventual consistency discussion.

---

## VII. The "Coming from Event Sourcing" Page Makes Claims That Will Alienate Its Audience

**Responding to: "Coming from Event Sourcing" (line 443)**

The plan says this page should translate for developers coming from Kafka, EventStoreDB, or Axon. The key translations listed are:

> No schema registry needed (types ARE the schema), no consumer groups (projections are functions), event versioning via algebraic data types

**My critique:** Every one of these claims is a simplification that an experienced event sourcing practitioner will immediately challenge, and two of them are arguably wrong.

**Claim 1: "No schema registry needed (types ARE the schema)"**

I addressed this in Section V. Types define the *current* schema. A schema registry manages *compatibility* across versions. These are different problems. If NeoHaskell's type system provides compatibility checking across event versions, that's genuinely impressive and should be explained in detail. If it doesn't, this claim is misleading.

**Claim 2: "No consumer groups (projections are functions)"**

Consumer groups in Kafka serve two purposes: load balancing (distributing partitions across consumers) and position tracking (remembering where each consumer last read). Saying "projections are functions" addresses neither concern. What happens when you have a projection that's slow? Can you run multiple instances? What tracks their position in the event stream? If NeoHaskell handles this automatically, explain how. If it doesn't, this claim is comparing apples to architecture.

**Claim 3: "Event versioning via algebraic data types"**

ADTs can represent event variants elegantly. But event versioning is about more than representation — it's about *migration*. When you add a new field to an event, existing events in the store don't have that field. ADTs don't solve this automatically. You still need upcasting logic. The claim implies that ADTs eliminate the versioning problem when they actually just provide a cleaner representation of the solution.

**My specific recommendation:**

Rewrite the "Coming from Event Sourcing" page with intellectual honesty as its organizing principle. The audience for this page already understands event sourcing. They don't need to be sold on it — they need to understand what NeoHaskell does differently and where the trade-offs lie.

Structure it as:

1. **What NeoHaskell provides at the language level** (built-in event store, type-safe events and commands, projections as first-class functions)
2. **What you no longer need to configure yourself** (with honest caveats about what's traded away)
3. **What's different and requires adjustment** (no Kafka, no separate schema registry — here's why, and here's what you lose)
4. **What's still your responsibility** (schema evolution strategy, scaling projections, GDPR compliance)

The tone should be: "You already know this domain. Here's how NeoHaskell makes specific parts easier, and here's where you'll need to adapt your existing knowledge." Not: "Everything you used to need is now unnecessary."

---

## VIII. The Domain Model Needs Explicit Command/Event Design

**Responding to: "The Progression" (lines 128–136) and the event list**

The plan lists events for each tutorial part:
- Part 1: `AccountOpened`, `MoneyDeposited`
- Part 2: `MoneyWithdrawn`, `WithdrawalDeclined`
- Part 3: (consuming existing events)
- Part 4: `AccountOpened` (savings variant)
- Part 5: `TransferInitiated`, `TransferCompleted`

**My critique:** This event list has several design problems that will confuse readers who later apply these patterns to their own domains.

**Problem 1: `WithdrawalDeclined` should not be an event.**

In event sourcing, events record facts about things that *happened* — state changes that were accepted. A declined withdrawal is a command that was *rejected*. No state change occurred. The account's event stream should not contain `WithdrawalDeclined` because the account's state didn't change.

This distinction between "command rejected" and "event recorded" is fundamental. If rejected commands are stored as events, the event stream becomes a log of *attempts* rather than a log of *facts*. The balance fold would need to filter out declined events. The audit trail becomes noisy. And the reader learns the wrong lesson: that every interaction produces an event.

Now — there *is* a legitimate case for recording declined commands, but it's in a *separate* stream (an audit log, or a "command log" that's distinct from the event log). Some systems do this for compliance or analytics. But it should be taught as a distinct pattern, not conflated with the primary event stream.

**Problem 2: `TransferInitiated` and `TransferCompleted` are under-specified.**

A transfer is a multi-step process. The events should reflect the actual state machine:

- `TransferRequested` (command accepted, process started)
- `SourceAccountDebited` (money left source)
- `DestinationAccountCredited` (money arrived at destination)
- `TransferCompleted` (process finished successfully)
- `TransferFailed` (process failed, compensation may be needed)
- `SourceAccountCredited` (compensation: money returned to source)

Each of these events occurs in a specific aggregate or process manager. `TransferRequested` and `TransferCompleted`/`TransferFailed` are process manager events. `SourceAccountDebited` and `DestinationAccountCredited` are account aggregate events. This is the architectural detail that makes Part 5 educational rather than misleading.

**Problem 3: `AccountOpened` (savings variant) should be the same event type with different data, not a different event.**

The plan's Part 4 introduces "AccountOpened (savings variant)." If this is a different event type from the checking account's `AccountOpened`, it implies that savings and checking accounts have fundamentally different event models. They don't — they have different *data* (account type, maybe different business rules) but the same event structure. The event should be `AccountOpened { accountType: Checking | Savings, ... }`, not two separate event types.

This is a domain modeling best practice: parameterize events by type rather than duplicating event types. It keeps projections simple (one handler for `AccountOpened`, with a pattern match on `accountType`) rather than requiring separate handlers for each variant.

**My specific recommendation:**

1. Remove `WithdrawalDeclined` from the event list. Part 2 should teach that declined commands *don't produce events*. The command handler returns a `Result.err` value. If the plan wants to show a declined withdrawal in the audit trail, introduce a separate audit log concept.

2. Expand Part 5's event list to show the full transfer state machine. This is the place to be comprehensive, because the reader needs to see how event sourcing handles multi-step processes.

3. Use a single `AccountOpened` event with an `accountType` field. Part 4 should teach parameterization, not type proliferation.

---

## IX. The Plan Oversells Event Sourcing's Benefits Without Honest Trade-Off Discussion

**Responding to: The plan's overall tone, the "wow" moments, and the Scope table (lines 113–124)**

The Scope table maps impressive-sounding features to simple implementations:

> **Complete audit trail** → "The event store, which you already have"
> **Historical replay** → "Replaying events to a point in time"

The plan's framing is: event sourcing gives you these things "for free." The "wow" moments reinforce this: "I accidentally built a compliance engine," "Adding a new account type was trivial."

**My critique:** This framing is dishonest about trade-offs in a way that will damage NeoHaskell's credibility with architects (the plan's second audience) and create unrealistic expectations for developers (the first audience).

**What event sourcing gives you and what it costs:**

| You get... | But it costs... |
|---|---|
| Complete audit trail | Larger storage requirements, need for event archiving |
| Historical replay | Replay performance degrades as event count grows (hence snapshots) |
| Multiple read models | Eventual consistency between write and read |
| Schema preserved forever | Schema evolution complexity |
| Temporal queries | Query complexity (can't just `SELECT WHERE date = ...`) |
| Immutability | GDPR/privacy compliance challenges |

The plan's Open Question #8 asks: "Is the 'compliance' claim defensible?" This is the right instinct. But the question should be broader: is the "you get this for free" framing defensible across the board?

I've spent years pushing back against the overly enthusiastic adoption of event sourcing. When I wrote about it on my bliki, I was careful to frame it as a pattern with specific applicability — not a silver bullet. The plan's tone risks creating the impression that NeoHaskell has eliminated the trade-offs of event sourcing, when in reality it has (at best) reduced some of the *accidental complexity* while the *essential complexity* remains.

**My specific recommendation:**

1. **Add a "Trade-offs" concept page.** This should be one of the first concept pages written (linked from Part 1), and it should be honest:

   > Event sourcing gives you audit trails, temporal queries, and multiple read models without extra infrastructure. But it also means: your storage grows without bound, your read models are eventually consistent, your event schemas must be versioned carefully, and some operations that are simple in CRUD (like deleting data) require extra thought.
   >
   > NeoHaskell makes the trade-offs more manageable — not absent.

2. **Rewrite the "wow" moments to be honest.** Instead of "I accidentally built a compliance engine," try "I have a complete audit trail — and with some additional work on tamper-proofing and access controls, this could form the basis of a compliance system." Instead of "Adding a new account type was trivial," try "Adding a new account type required no schema migration — I just added a new variant to my event type and updated my projections."

3. **Add an "Is Event Sourcing Right for You?" section** in the Core Concepts. Not everything benefits from event sourcing. A simple configuration store, a user preferences service, or a static content CMS would be overengineered with event sourcing. The plan's second example domain (logistics) is good for showing where event sourcing generalizes. The docs also need an example of where it *doesn't* — where CRUD is genuinely the simpler and better choice.

---

## X. Agreements and Disagreements with Steve's Review

Steve's review is excellent on pedagogy. I want to note specific points where our perspectives intersect or diverge, because the plan's authors should understand where two reviewers independently converge (high-confidence issues) and where we bring different lenses.

### Where I Strongly Agree with Steve

**The Part 4 → Part 5 gap (his Section XI):** Steve identifies this as a pacing problem. I identify it as both a pacing problem AND a conceptual integrity problem. We converge on the recommendation to split Part 5. His recommendation to add "same-account transfers" as a warm-up is architecturally sound — it introduces the *mechanics* of moving money before introducing the *coordination* problem.

**Visual aids (his Section V):** His point about the event flow diagram being the event-sourcing equivalent of the Rust Book's memory diagrams is exactly right. I'd add that the diagram should show the CQRS split explicitly — command side and query side — because that's the architectural pattern the plan implicitly teaches but never names.

**Checkpoint sections (his Section VI):** Completely agree. The one thing I'd add is that checkpoints for event-sourced systems should show the *event stream*, not just the final state. "You should see these events in this order" is the event-sourcing equivalent of Stripe's "you should see this HTTP response."

### Where I Disagree with Steve

**Tutorial Layer 2 — the syntax sidebar (his Section I):** Steve recommends dropping Layer 2 entirely. I think he's wrong for this specific project, and the reason is the compound unfamiliarity problem.

The Rust Book teaches one unfamiliar thing: ownership. Rust's syntax is C-like and broadly familiar to the target audience. So inline parenthetical syntax explanations work — the reader encounters `match` and a one-sentence explanation is sufficient because the surrounding syntax is readable.

NeoHaskell teaches *two* unfamiliar things simultaneously: event sourcing AND Haskell-derived syntax. A reader from JavaScript encountering `MoneyDeposited { accountId = "alex-checking", amount = 100 }` for the first time is confused by the *syntax* (what is this `{ }` notation in a function call?) while also being confused by the *concept* (what is an event and why am I "depositing" by creating a record?). An inline parenthetical can address one of these confusions but not both simultaneously without overwhelming the narrative.

The collapsible syntax aside serves a genuine purpose here: it separates "what does this code syntax mean?" from "what is this code doing architecturally?" The reader who understands Haskell-like syntax ignores it. The reader who doesn't can expand it without breaking the architectural narrative. I'd keep Layer 2, but make it shorter and more aggressively minimal than the plan proposes — one line per construct, no paragraphs.

**Bloom's Taxonomy (his Section II):** Steve says to demote it to a retrospective validation tool. He's right that it shouldn't be a design *constraint*, but I think there's a middle ground he's missing. For exercises about *domain modeling* (as opposed to syntax), Bloom's taxonomy is actually a useful design heuristic. "Should AccountType be an event or a field?" is an Analyze-level question because it requires the reader to reason about aggregate design. "Add a DailyWithdrawalLimit event" is an Apply-level question because it requires applying an established pattern. Using the taxonomy to *check* that Part 1 isn't asking Analyze questions and Part 6 isn't asking Remember questions is valuable. Using it to *prescribe* that Part 3 must be exactly "Apply" is harmful. The plan should adopt Steve's recommendation in spirit while keeping the taxonomy as a review checklist for exercise design.

### Where Steve's Review Has a Blind Spot

Steve's review doesn't address domain modeling correctness, event sourcing vocabulary, or CQRS patterns. That's appropriate — it's not his area. But the plan's authors should note that his approval of the Rosetta Stone, the event list, and the bounded context introduction doesn't constitute architectural validation. Those elements need the scrutiny I've provided above.

---

## XI. Summary of Recommendations

Ranked by architectural impact:

1. **Fix the Rosetta Stone's aggregate mapping.** An aggregate is a consistency boundary, not a synonym for "account." This misconception will poison every subsequent concept.

2. **Redesign Part 5 (Transfers) with honest complexity.** Show the full saga state machine, including failure and compensation. Remove "it just works" as the wow moment.

3. **Delay bounded contexts until you have a real example.** Multiple account types within the same context is not a bounded context separation. Introduce a genuinely different context (Notifications, Interest Calculation, or Reporting).

4. **Teach projections as transformation, not formatting.** Build two materially different read models from the same event stream in Part 3. This is the CQRS insight.

5. **Remove `WithdrawalDeclined` from the event stream.** Teach the distinction between rejected commands and recorded events. This is fundamental to correct event sourcing.

6. **Add a Trade-offs concept page.** Be honest about what event sourcing costs, not just what it gives. This is what will win the Architect audience.

7. **Move schema evolution from "Advanced" to "Core."** Every event-sourced system that survives past its first month will encounter schema evolution. It's not optional.

8. **Revise the "Coming from Event Sourcing" page** to be architecturally honest. Don't claim that NeoHaskell eliminates problems that it actually just mitigates.

9. **Add missing patterns:** Idempotency, snapshotting, GDPR/tombstones, event ordering. These are not edge cases — they're the normal concerns of production event sourcing.

10. **Add a "When NOT to use Event Sourcing" section.** This demonstrates maturity and builds credibility with architects who have seen event sourcing applied where it shouldn't be.

---

## Closing Thought

The plan has a sentence I keep coming back to: "NeoBank isn't a decoration — it's the lived proof that event sourcing is how the world already works." This is a beautiful aspiration. But event sourcing is *not* how the world already works for most developers, and claiming it is will feel dismissive to readers who are making a genuine effort to learn something new.

What the banking domain *actually* proves is something more nuanced and more powerful: that event sourcing is the natural model for domains where *history matters* — where knowing what happened is as important as knowing the current state. Banks understood this centuries ago. Accountants understood it millennia ago. The double-entry ledger is arguably humanity's first event store.

NeoHaskell's opportunity is not to claim that all developers should think this way. It's to show that *some domains* are naturally event-sourced, that NeoHaskell makes these domains a joy to implement, and that the patterns generalize further than most developers expect. That's an honest pitch, and it's a compelling one.

The documentation should teach event sourcing correctly, acknowledge its trade-offs candidly, and let the pattern's genuine strengths make the argument. If it does that, the Architect audience will be won. If it oversells and underspecifies, they'll walk away — and they'll tell others to do the same.

— Martin Fowler
