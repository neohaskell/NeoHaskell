---
title: "Describe the application with events"
description: "Use a shared model to connect requests, decisions, recorded facts, and what people see."
sidebar:
  order: 6
---

Before an application can do the right thing, people need to agree on what “right”
means. A request such as “let people cancel” sounds simple until someone asks:
cancel what, until when, and what happens to work already started?

Event modeling gives those questions a visible place. You describe what someone
wants to do, the rules that decide whether it may happen, the facts worth
remembering, and the information people need afterward. You can discuss this
without reading implementation code.

For someone evaluating NeoHaskell, the benefit is continuity: the words used to
explain a process have counterparts in the application. For someone building with
a coding agent, the model gives you a shared agreement to implement and challenge.
The work of choosing the rules remains; it becomes easier to see.

## Learn to read the picture

An Event Model follows a concrete example through time. Read it from left to
right: the screen someone uses, their request, the accepted fact, and the
information prepared for the next screen. Later steps use information that has
already appeared. Screens sit above the application behaviour; separate lanes
can distinguish actors or parts of a system. This layout follows [Adam Dymitruk's
introduction to Event Modeling](https://eventmodeling.org/posts/what-is-event-modeling/).

Three colours help you recognize the roles. We also label each role so you can
read the diagrams without relying on colour:

| Colour and element | Question it answers | Example |
| --- | --- | --- |
| Blue command | What change is requested? | CreateCart |
| Orange event | What happened? | CartCreated |
| Green read model | What information can someone read? | CartSummary |
| Plain screen sketch | What can the person see or do here? | A button to create a cart |

These are the conventions described in [Martin Dilger's introduction](https://eventmodelers.ai/docs/blog/documenting-software-with-event-modeling/).
An event uses past tense because it describes a fact. A read model gives those
facts a useful presentation. The screen is a rough sketch of that presentation,
not an extra kind of domain event.

Two other words help explain what happens behind a command. **Entity state** is
the current knowledge about the thing being changed. A **decision** uses that
knowledge to accept or refuse the request. We describe the rules beside our
examples; you do not need to add implementation machinery to the picture.

## First example: create an empty cart

Start smaller than adding a product. Someone wants an empty cart they can use.
This diagram uses the names and behaviour of NeoHaskell's public testbed, with
illustrative screens drawn for this explanation.

[![A Create cart screen triggers the blue CreateCart command, producing the orange CartCreated event; the green CartSummary read model supplies an empty-cart screen.](/diagrams/event-model-first-cart.svg)](/diagrams/event-model-first-cart.svg "Open first-cart event model at full size")

*Follow the information from a person's action to a recorded fact and back to
what they see. [Download the editable diagram](/diagrams/event-model-first-cart.drawio).*

Read the example in four steps:

1. The person asks to create a cart. **CreateCart** needs no input fields.
2. The application chooses a new cart identifier and an owner identifier. An
   authenticated user's identifier supplies the owner; an anonymous request gets
   a generated owner identifier.
3. **CartCreated** records those identifiers as `entityId` and `ownerId`.
4. **CartSummary** identifies the cart through `cartSummaryId` and reports
   `itemCount: 0` and `isEmpty: true`, allowing the screen to show an empty cart.

The owner field records a relationship; its presence alone does not establish
an access policy. What matters here is tracing information: the cart identifier
originates during creation and lets later requests refer to that same cart.
The testbed's [CreateCart command](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Commands/CreateCart.hs)
and [CartSummary](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Queries/CartSummary.hs)
provide the implementation behind this example.

## Second example: continue the same timeline

Now the person selects a product and requests two units. The model grows to the
right, retaining the creation step that makes the next action possible.

[![The cart timeline continues from CreateCart and CartCreated to AddItem with quantity two, then ItemAdded and an updated CartSummary showing one entry and isEmpty false.](/diagrams/event-model-cart-journey.svg)](/diagrams/event-model-cart-journey.svg "Open the cart-journey event model at full size")

*The second action uses the cart created earlier. Its result supplies the next
view. [Download the editable diagram](/diagrams/event-model-cart-journey.drawio).*

**AddItem** receives `cartId`, `stockId`, and `quantity`. Here the quantity is two.
If the cart exists and the quantity is positive, **ItemAdded** records the cart
as `entityId`, together with `stockId` and `quantity`.

The summary then shows `itemCount: 1` and `isEmpty: false`. Why one rather than
two? This particular read model counts entries in the cart, not the sum of their
quantities. One accepted addition creates one entry, even when that entry has two
units. Its screen should say “1 entry,” not “1 unit.”

This is precisely the kind of misunderstanding a concrete model can expose. If
you want a total-unit count, that is a different query requirement to implement
and check. Trace the values in the public [AddItem command](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Commands/AddItem.hs),
[cart state](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Core.hs),
and [summary definition](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Queries/CartSummary.hs).

## A request is not a fact

“Add two mugs” and “two mugs were added” have different meanings. The first could
fail: perhaps the cart does not exist. The second says the application accepted
that change. Keeping these separate prevents a hopeful request from being treated
as completed work.

An accepted addition does not mean a payment succeeded or a parcel shipped. Each
of those steps would need its own rules and evidence.

This distinction is useful outside ecommerce. “Book the room” is a request;
“the room was reserved” is a fact. “Send the document for review” is different
from “the reviewer approved it.” Naming those steps reveals promises that a
single “done” status might hide.

A refusal does not automatically create a domain event. If your application must
retain rejected attempts and their reasons, decide how to represent that
requirement explicitly. A useful model describes both acceptance and refusal.

## Make the rule testable with examples

A successful timeline leaves room for misunderstandings about other requests.
**Given–When–Then** adds specific scenarios: the history already known, the
command requested now, and the event or refusal expected afterward. Read-model
scenarios describe the information expected from a given history. These
conventions appear in the [Event Modelers cheat sheet](https://eventmodelers.ai/cheatsheet/).

For the second diagram, use these scenarios:

| Given | When | Then |
| --- | --- | --- |
| An empty cart has been created | AddItem requests two units | ItemAdded records quantity two |
| The same cart exists | AddItem requests zero units | The request is refused; no ItemAdded is recorded |
| No cart exists for the requested identifier | AddItem requests two units | The request is refused because the cart is missing |

Then check the view separately: given the creation and the accepted addition,
CartSummary should eventually report one entry and a nonempty cart.

These examples distinguish a request from a fact and a fact from its
presentation. They also give your coding agent a clear implementation target.
The compiler can check structural relationships; these scenarios help check
whether the chosen behaviour matches your intention.

## What people see can follow a moment later

Accepting a command and updating the information on a screen are separate steps.
NeoHaskell's read models update asynchronously: they consume changes after those
changes have been accepted. A person can therefore receive an acknowledgment
before a query shows the new result.

This is a design question you can discuss before implementation. Should the screen
show “updating”? What result confirms the requested change is visible? If the
summary is briefly unchanged, should the person wait or send another request?
Repeatedly submitting the command could request more work; it is not the same as
refreshing the view.

Separating the views also lets different readers ask different questions about
the same activity. A cart summary and a stock overview serve different needs.
Their existence does not make cart and stock changes one indivisible action;
[coordination between them](/build/stock-and-checkout/) needs its own design.

## When the next actor is the application

Some steps start without a person pressing a button. Event Modeling uses a gear
for an **automation**: information becomes available, a process reacts, and it
issues the next command. The [Event Modelers automation pattern](https://eventmodelers.ai/cheatsheet/)
shows this as events feeding a read model, then automation, command, and a new event.

The two cart pictures focus on the person's journey. You could later add a
separate stock-reservation step and explain its trigger and outcome. That new
step needs its own rules and failure scenarios; an arrow does not promise that
all steps complete together or that external work happens exactly once.

## Draw a small, complete slice

A **slice** is one useful piece of behaviour you can describe and check together.
Start with the screen and request. Add the rule and resulting event, then the
read model needed afterward. Include scenarios for acceptance and refusal. The
first diagram gives you a small working shape to adapt.

Ask your agent to explain the slice back to you. Challenge missing decisions:
“Where is the quantity checked?” “Does this fact mean requested or completed?”
“What happens if the next step fails?” These are meaningful engineering questions,
even when you cannot yet read the implementation.

Small slices give an agent a bounded task. They still share contracts: changing
an event's meaning may affect several readers. The model exposes that dependency.
The [Neo IDE graph](/getting-started/visual-ide/) later connects these concepts to
the codebase so you can explore their relationships visually.

## Try the model in your own words

Choose a small action from an application you want to build. Describe one
accepted request, one refused request, and the information someone needs afterward.
Then ask what you would need to know a month later to explain the outcome.

<details>
<summary>A way to check your reasoning</summary>

Use past tense for the accepted fact and a verb for the request. Identify the
rule that separates your successful and refused examples. Check that the view
answers a real question and that your explanation permits it to update later.
Finally, look for important context you assumed would be remembered but never
included in the model.

</details>

Next, use this shared vocabulary to [delegate confidently to your coding
agent](/start/trusting-your-agent/).
