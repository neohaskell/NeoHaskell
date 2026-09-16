---
title: History gives change its meaning
description: Understand why current values are summaries, how corrections preserve context, and what meaningful history lets an application explain.
sidebar:
  order: 2
---

You open an application and see that a request was refused. That tells you its current status, but leaves the questions you actually care about unanswered. What was requested? Which rule applied? Was information missing? Did somebody later correct the decision?

The same gap appears in a balance, a delivery address, or a project's progress indicator. A current value can be perfectly accurate and still tell you very little about how it came to be. When people depend on an application to explain decisions, change its rules, or resolve mistakes, that missing context matters.

NeoHaskell builds around meaningful events: the facts an application accepts and retains about what happened. To understand why, start with a familiar number.

## The balance is the answer to a question

Imagine a simplified bank account record. It starts with €100, and its owner withdraws €30. The current balance is €70.

If the application keeps only the current balance, it can answer “How much is there now?” It cannot answer “Why is that the balance?” from that number alone. The same €70 could result from many different histories.

Instead, imagine keeping the opening amount and the withdrawal. The application calculates the current balance from those entries: €100 minus €30 equals €70. The useful number is still there, but the facts that explain it remain available too.

[![An opening amount of €100 and a withdrawal of €30 remain in history and produce a current balance of €70.](/diagrams/history-and-summary.svg)](/diagrams/history-and-summary.svg "Open diagram at full size")

The history retains the two meaningful entries. The balance summarises their effect; displaying €70 does not require replacing either entry.

This is a conceptual example, not a banking feature supplied by NeoHaskell. Its lesson applies wherever a present value summarises a sequence: a membership's status, the number of available places, or whether a document has been approved.

**Event sourcing** makes that retained sequence the basis for reconstructing application state. The current answer remains useful. It is also explainable in terms of the facts used to produce it.

## A correction can tell the truth about the mistake

Now suppose the €30 withdrawal was accidentally recorded twice. The calculated balance becomes €40, although only one withdrawal should count.

Changing the number directly to €70 repairs the display. It does not, by itself, explain which entry was wrong or why the correction was justified. A later reader sees the right answer without the reasoning needed to trust it.

In the retained history, the application can record an explicit reversal of the duplicate withdrawal. The original entries remain, and the correction contributes €30 back to the calculated balance.

[![The duplicate withdrawal reduces the recorded balance to €40; an explicit reversal adds €30 and restores €70 while retaining the mistaken entry.](/diagrams/correction-history.svg)](/diagrams/correction-history.svg "Open diagram at full size")

The reversal identifies the duplicate entry it corrects. The resulting balance is €70, and the history explains both the error and its repair.

This requires a designed operation. Someone requests the reversal; the application checks whether that entry can be reversed and whether it has already been corrected. If accepted, the reversal becomes another fact. Duplicate detection and those rules are responsibilities of the application model.

For an AI agent, that distinction is important. “Make the number right” is an incomplete instruction. “Correct this duplicate through the permitted reversal operation, then show the resulting history” describes an action whose intent and effect can be checked.

## A new preference should not rewrite an old agreement

Consider the ecommerce practice project used throughout these docs. Imagine adding orders and customer address preferences to it.

A customer places an order for delivery to Yerevan. Later, they change their preferred address to Lisbon. Both statements can remain true: the earlier order was placed for Yerevan, and the current preference is Lisbon.

If the old order displays whatever address happens to be in the customer's profile today, it may appear that Lisbon was always the agreed destination. Updating a preference has accidentally changed the meaning of an earlier transaction.

An explicit model gives these facts separate homes. The order records its agreed delivery destination. The profile records the preference for future requests. If changing an existing order is allowed, that is another operation with its own rules—perhaps delivery can change before dispatch but requires a different process afterwards.

The framework cannot infer this distinction from a field called “address.” You and your agent must identify what the value means and when it becomes part of an agreement. The same reasoning applies to an approved budget, an accepted document version, or eligibility assessed under an earlier policy.

## History makes new questions possible

An application rarely knows every question its users will eventually ask. Today they may need a current status. Later they may want to understand how long a process takes, which steps are often corrected, or where work becomes stuck.

Retained events can support new views of the same activity. One view presents current state; another explains the sequence for a person investigating a problem. In NeoHaskell, **queries** prepare information for readers, while entities reconstruct the state used when deciding what may happen next. You will explore both in the [build chapters](/build/).

The available questions depend on the information actually recorded. A new report cannot recover a refusal reason that was never retained. A timestamp on a stored event does not automatically establish when something happened outside the application. If that distinction matters, model the external occurrence and its time explicitly.

This is a useful conversation before implementation: “Which question would we regret being unable to answer?” The answer helps select meaningful facts and context without attempting to preserve everything.

## Events, audit logs, and backups have different jobs

A backup helps recover stored information after loss. Operational logs help diagnose execution. An audit mechanism can retain changes and their actors. These are valuable tools, and applications that store current state can maintain excellent histories through deliberate audit design.

Event sourcing places accepted domain facts in the path that produces state. “The order was cancelled” has a meaning the application understands and applies. A technical trace such as “field changed from 2 to 3” needs additional interpretation to explain the same outcome.

That does not make an event history a complete audit record automatically. A cancellation event may establish cancellation while omitting the reason, evidence, or authority behind it. Those details must be modeled where they matter. NeoHaskell's event metadata provides places for identifiers and relationships; optional fields do not populate themselves with every explanation a future reviewer needs.

Backups remain necessary for history you promise to retain. Reconstructing state works from the events available to the application; it cannot reconstruct lost facts from an empty store.

## Preserve meaning deliberately

Choosing history also means choosing what belongs in it. A useful event captures enough context to preserve its meaning. It does not need to copy every field from a request or retain every sensitive detail indefinitely.

Decide what must remain explainable, who may inspect it, and what may be removed or stored separately. If details have a shorter lifetime than the fact they support, design that relationship explicitly. Retention changes can affect reconstruction and reporting, so test what remains understandable afterwards.

Future code also needs to interpret earlier events faithfully. Changing tomorrow's quantity limit should not make yesterday's accepted request disappear. [Evolving an application](/operate/evolution/) develops the compatibility work behind that principle.

## What you can now ask your agent

Choose a value in the application you want to build. Ask your agent to explain the question it answers, the facts that produce it, and how a mistaken change would be corrected. Then ask which context would still be available to somebody reviewing the decision later.

For a short exercise, use the address example: a preference changes after an order is placed, and a separate request attempts to change that order after dispatch. Decide what each operation may do before asking for code.

<details>
<summary>Suggested reasoning</summary>

The new preference can apply to future orders while the earlier order retains its agreed destination. Changing that order requires a separate rule. Test a permitted change, a refused change after the chosen boundary, and the same order-change request submitted twice. The history should explain accepted changes without silently turning a refusal into success.

</details>

NeoHaskell gives these distinctions executable structure. Its guarantees are narrower than knowing the right policy: a well-typed, traceable decision can still be wrong. The benefit is a clearer basis for recognising, questioning, and correcting it while preserving the meaning of what came before.

Next, explore [growing by slices](/start/growing-by-slices/) to see how these facts connect new capabilities. For the practical modeling method, continue to [event modeling](/start/event-modeling/).

Public implementation foundations: [entity reconstruction](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Entity/Core.hs), [query definitions](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Query/Core.hs), [event metadata](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Event/EventMetadata.hs), and [event-store operations](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/EventStore/Core.hs).
