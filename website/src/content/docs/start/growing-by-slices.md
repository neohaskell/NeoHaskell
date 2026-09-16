---
title: "Grow an application by useful slices"
description: "Build one understandable behaviour at a time, with explicit connections between features."
sidebar:
  order: 3
---

An application rarely arrives as one complete idea. You solve an immediate problem,
learn from people using it, and discover the next useful change. The challenge is
keeping earlier work understandable as those changes accumulate.

NeoHaskell gives you a way to describe growth in small, connected pieces. A request,
the rule that evaluates it, the fact it produces, and the information someone sees
can form a **feature slice**: one behaviour you can explain, implement, and check.
You do not need to understand every part of the application to discuss that slice.
You do need to understand the promises it makes to its neighbours.

This approach suits membership services, booking tools, approval processes, and
ecommerce applications. The domain changes; the questions remain recognisable.

## Start with an outcome someone can recognise

“Build the database” describes technical work. “Let a member request a place and
see whether the request was accepted” describes an outcome. The second gives you
something to discuss before choosing how to implement it.

For each slice, answer four questions:

| Part | Question | Ecommerce example |
| --- | --- | --- |
| Trigger | What starts this step? | Someone asks to add two mugs to a cart. |
| Decision | What must be true for the request to succeed? | The cart exists and the quantity is positive. |
| Fact | What do we record if it succeeds? | An item was added with the requested quantity. |
| View | What should someone be able to see? | The cart summary reflects the accepted addition. |

A refusal belongs in this description too. A quantity of zero should not produce
an item-added fact. Writing that down gives the person, agent, and implementation
a shared boundary: they can disagree about the proposed code while still agreeing
on the outcome they are checking.

[![A slice accepts a request and records a fact. Two further slices use that shared event contract: one prepares a view and the other requests a follow-up action.](/diagrams/growing-by-slices.svg)](/diagrams/growing-by-slices.svg "Open diagram at full size")

*Each connection carries an explicit meaning. Accepting one step does not mean
that every later step has succeeded.*

A slice is a way to divide behaviour, rather than a rule about folder size. Some
slices accept requests. Others prepare a new view from existing information or
react to a fact by requesting another action. Their useful size is the size at
which you can explain both success and failure without hiding a consequential
step.

## Let later features use earlier promises

Suppose the application already records accepted membership applications. Adding
a reviewer dashboard should not require the dashboard to understand how the
application form was laid out. It needs the accepted information and a clear
meaning for “awaiting review.”

That is an **explicit contract**: agreement about what information is available
and what it means. In NeoHaskell, commands, events, and queries give those agreements
named places in the program. A command expresses a request; an event records an
accepted fact; a query prepares information for reading.

Stable contracts let a new feature use an existing capability without reaching
into its implementation. Tests can check each rule near the decision that owns it.
Tests of the connection then check whether the pieces work together. This makes
change easier to reason about; it does not make the connections disappear.

## Grow the practice project in deliberate steps

The ecommerce project gives you a familiar place to practise this way of growing:

1. **Create a cart.** Establish an identifiable cart before adding anything to it.
2. **Add an item.** Accept a positive quantity and explain a refusal clearly.
3. **Show the result.** Prepare a cart summary, including the possibility that a
   recently accepted change has not reached the view yet.
4. **Connect stock.** Give stock its own decision about whether a quantity can be
   reserved, then connect the cart action to that request.
5. **Design order acceptance.** Decide what must be confirmed before an order is
   accepted, and what the person sees while confirmation is pending.
6. **Add outside services.** Decide how payment outcomes, notifications, and
   failures affect the process.

The public reference application contains the cart, summary, and stock examples.
Order acceptance and payment complete a design you will develop; they are not
already supplied by those examples. The [build journey](/build/) introduces the
working pieces before [the practice project milestone](/build/your-shop/) asks
you to make more of the decisions yourself.

Notice how each step introduces a reason for another concept. Stock matters when
availability matters. An integration matters when an accepted action needs a
response elsewhere. You learn the machinery when there is a useful question for
it to answer.

## Keep relationships visible

Small slices can still participate in a large process. In the reference example,
adding an item to a cart triggers a separate stock reservation request. The cart
accepts its change before that later request finishes. The stock decision can
refuse an unavailable quantity.

This means “item added” and “stock reserved” are different promises. A complete
application must decide how to communicate and handle the gap. Should the item
remain pending? Should a failed reservation remove it? Who may try again? Drawing
the connection reveals those questions; it does not choose the policy for you.

The same issue appears when accepting a booking before a payment finishes, or
approving a project before a supplier confirms delivery. A useful boundary keeps
each responsibility clear while leaving the overall process visible. Later,
[cross-domain workflows](/connect/workflows/) explain how to implement those
connections and check failure cases.

## Give your agent a boundary it can work within

A bounded task might be: “Allow adding a positive quantity to an existing cart.
Refuse zero and negative quantities. Show evidence for acceptance, refusal, and
a missing cart. Explain which existing contracts you changed.”

Review whether the chosen rule is right, then ask for the next slice with the
previous decisions available as context.

As your confidence grows, delegate larger changes whose boundaries you can still
explain. The [visual IDE](/getting-started/visual-ide/) helps connect the vocabulary
to discovered code. Tests make expected outcomes inspectable. Neither substitutes
for deciding what the application ought to do.

## Plan for contracts that eventually change

A new field or a changed event meaning can affect several slices. The compiler
helps identify incompatible uses in code built together. It cannot establish
that old stored records still mean the same thing, that a remote consumer has
been updated, or that a new rule is appropriate for earlier decisions.

Treat such a change as coordinated work: identify consumers, retain examples of
old history, and check how the new version reads and explains it. The
[evolution chapter](/operate/evolution/) develops that responsibility.

Over time, meaningful history can also preserve institutional memory: what was
requested, what happened, and how later actions responded. Reasons, evidence,
and authority remain available only when you choose to record them.

Choose one process from your own domain and draw its smallest complete slice,
including a refusal. Then name the next slice and the promise that connects them.

Next: [weigh adoption value and tradeoffs](/start/fit-and-tradeoffs/).
