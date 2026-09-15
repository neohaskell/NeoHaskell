---
title: "Describe the application with events"
description: "Connect requests, facts, state, and views using the shop you already understand."
sidebar:
  order: 4
---

An order means different things to different people. The customer wants a promise,
the merchant needs work to fulfil, and support needs to explain what happened.
An event model helps you connect those perspectives before code makes the choices
harder to see.

Start with a short story in time order. A customer requests an action, the business
accepts or refuses it, and people see the resulting information.

## Four useful words

| Concept | Plain meaning | Shop example |
| --- | --- | --- |
| Command | A request to do something | Add two mugs to this cart |
| Event | A fact the application accepted | Two mugs were added |
| Entity state | What is currently known about one business object | This cart's items and quantities |
| Query or read model | Information prepared for a reader | The cart summary shown on a screen |

A command is not guaranteed to become an event. The quantity may be invalid or the
cart may not exist. A view can take a short time to reflect an accepted change;
accepting the request and updating every reader are separate steps.

## Model a useful slice

A **slice** is a small piece of behaviour you can discuss and verify together.
For “add an item,” write down:

1. What the customer needs to see before acting: the product and current cart.
2. What they request: a product identifier and quantity for this cart.
3. What rules apply: the cart exists and the quantity is allowed.
4. What is recorded if accepted: the item-added fact.
5. What changes for the reader: the cart summary eventually reflects it.

Keep the example small enough that you can ask what happens when the request is
refused. A diagram showing only success hides decisions the implementation still
has to make.

## Capture state from accepted history

Imagine a cart starts empty, then receives two mugs, then another item. Its current
state comes from applying those accepted changes in order. Reconstructing state is
how the application understands the cart before deciding on the next request.

Do not make reconstruction send an email or ask a payment provider for today's
answer. Reading old history should not perform a new business action. Effects
belong in explicitly wired [integrations](/connect/).

## Let the model reveal missing decisions

If two customers request the last mug, which part of the system decides who gets
it? The cart's contents and the stock available are different concerns. Drawing
the transition between them makes the coordination question visible. The
[stock and checkout chapter](/build/stock-and-checkout/) develops it later.

Before moving on, describe a refusal in your own shop. What remains unchanged?
What message does the customer need? What fact, if any, should be recorded?
Your agent can propose answers, but the policy belongs to you.

Next: [work with your agent](/start/trusting-your-agent/) and then use the
[Neo IDE graph](/getting-started/visual-ide/) to connect the model with the project.
