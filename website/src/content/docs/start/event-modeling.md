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

## Follow one change through the application

Start with a familiar practice example: adding two mugs to a cart. There is a
person making a request, an existing cart, a decision about whether the request is
allowed, and a summary that should eventually show the result.

[![A request reaches a decision informed by the entity's history; accepted events update the history and feed the view people read.](/diagrams/shared-model.svg)](/diagrams/shared-model.svg "Open diagram at full size")

*The shared model connects intention, decision, fact, and information. The view
updates after an accepted change; it is not the decision itself.*

These words make the different responsibilities easier to discuss:

| Concept | Plain meaning | Practice example |
| --- | --- | --- |
| Actor | Whoever or whatever starts an action | A person using the cart screen |
| Command | A request to do something | Add two mugs to this cart |
| Entity state | The current knowledge used to decide about one thing | The cart and its items |
| Decision | The rule that accepts or refuses the request | The cart exists and the quantity is positive |
| Event | A fact the application has accepted | Two mugs were added |
| Query or read model | Information prepared to answer a question | The cart summary shown on a screen |

You do not have to memorize the vocabulary before using it. Start with ordinary
sentences, then identify which sentence asks for a change and which describes
something that happened.

## A request is not a fact

“Add two mugs” and “two mugs were added” have different meanings. The first could
fail: perhaps the cart does not exist. The second says the application accepted
that change. Keeping these separate prevents a hopeful request from being treated
as completed work.

NeoHaskell's public cart example refuses a missing cart or a nonpositive quantity.
An accepted request produces an item-added event. It does not follow that a
payment succeeded, a parcel shipped, or every other process finished. Each of
those would need its own rules and evidence.

This distinction is useful outside ecommerce. “Book the room” is a request;
“the room was reserved” is a fact. “Send the document for review” is different
from “the reviewer approved it.” Naming those steps reveals promises that a
single “done” status might hide.

A refusal does not automatically create a domain event. If your application must
retain rejected attempts and their reasons, decide how to represent that
requirement explicitly. A useful model describes both acceptance and refusal.

## Current state has a history

A cart can start empty, receive two mugs, and later receive another item. Its
current state comes from applying those accepted changes in order. The state is
the answer to “what do we know now?” The events explain how it became that way.

That separation matters when rules change. Suppose a future version allows at
most three units in a new request. An earlier accepted request for five units does
not become a different historical fact. The new decision rule governs new
requests; the application still needs to understand its older history.

Corrections deserve the same care. In an application with cancellation, a
cancellation can be another recorded change instead of making the original action
disappear. You must design that command, its rules, and its event; event sourcing
does not invent a cancellation policy for you.

History also preserves only what you put into it. If a reason, supporting document,
or approving person matters later, model that information deliberately. An event
name alone does not explain every motive behind a decision.

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

## Draw a small, complete slice

A **slice** is one useful piece of behaviour you can describe and check together.
Draw or write five things:

1. What the person needs to know before acting.
2. What change they request, with one concrete example.
3. What the application must know and which rules it applies.
4. What fact is recorded on acceptance, and what refusal means.
5. What the person should see afterward, including any waiting period.

Ask your agent to explain the slice back to you. Challenge missing decisions:
“Where is the quantity checked?” “Does this fact mean requested or completed?”
“What happens if the next step fails?” These are meaningful engineering questions,
even when you cannot yet read the implementation.

Small slices give an agent a bounded task and give you an understandable result to
review. They still share contracts: changing the meaning of an event may affect
several readers. The model helps expose that dependency; it does not remove it.
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
