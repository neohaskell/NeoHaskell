---
title: "Learn to trust your coding agent"
description: "Build confidence through a shared model, explicit examples, and evidence you can explain."
sidebar:
  order: 7
---

Working with a coding agent should let you build more than you could comfortably
implement alone. You bring an idea, an understanding of the people who will use
it, and judgment about what should happen. The agent helps turn that understanding
into working software.

You should not need to review every generated line to make progress. You do need
a way to recognize whether the result means what you intended. NeoHaskell's
shared model, conventions, compiler, and tests give you several ways to ask that
question. Each contributes evidence; together, they support confident delegation.

Trust grows when you can explain the application's promises and how you checked
them, then confidently hand over the next piece of work.

## Keep ownership of meaning

An agent can fill gaps with plausible choices. That is useful when choosing a
helper name; it is consequential when deciding who may cancel a booking or
whether a failed payment releases reserved stock.

Before implementation, describe the outcome you want and give a few examples.
Ask the agent to identify the affected commands, decisions, events, and views.
A short explanation often exposes a disagreement while it is still cheap
to correct.

For a new rule, agree on at least three cases: an ordinary accepted request, a
refused request, and a value right at the boundary. When changing an existing
rule, also ask what should happen to previously accepted history. These examples
turn “make it work” into a result you can recognize.

## Catch a small misunderstanding early

Imagine the ecommerce practice application is gaining a limit of three units per
new request. An earlier version accepted five. Here is a useful disagreement to
find before the agent changes anything:

> **Jess:** Limit new requests to three units. Older accepted requests should still
> be understood as they happened.
>
> **Agent:** I'll cap quantities at three while rebuilding the cart, so every cart
> respects the limit.
>
> **Jess:** That would change what the old request means. Check the limit when
> deciding on a new request. Rebuilding old history should preserve the five units
> we already accepted.
>
> **Agent:** I'll put the rule in the new-request decision and check both new
> requests and reconstruction of the older accepted event.

The misunderstanding is recoverable because the model gives Jess a clear question:
is this code deciding what may happen next, or interpreting what already happened?
She can challenge that distinction without writing the implementation herself.

The correction needs evidence. Ask for requests of two, three, and four units,
plus an older accepted five-unit event. Two and three should be accepted under
this proposed policy; four should be refused; the historical five should remain
five. These are practice requirements, not a built-in NeoHaskell quantity limit.

## Make the feedback loop visible

![A human defines intent and examples, an agent proposes and implements a model, checks produce evidence, and the human reviews the outcome before the next change.](/diagrams/trust-loop.svg)

*Confidence grows through a repeatable loop: explain, model, implement, check, and
review. Each pass gives the next delegation a clearer starting point.*

For a feature, bug fix, or policy change:

1. Explain the situation and the rule in ordinary language.
2. Ask the agent to show its understanding and name unresolved choices.
3. Agree on examples that distinguish a correct result from a plausible mistake.
4. Delegate implementation and the relevant checks.
5. Inspect the observable result, including one variation you choose.
6. Ask what the evidence covers and what still depends on an untested condition.

When a check fails, establish whether the behaviour or the expectation is wrong.
Changing an expectation can be appropriate when you deliberately change a rule.
Removing a failing check without resolving the disagreement only removes evidence.

## Know what each layer establishes

Different checks answer different questions:

| Evidence | What it helps establish | What it cannot decide for you |
| --- | --- | --- |
| Shared model and IDE graph | The concepts and relationships can be inspected | Whether you chose the right policy |
| Successful compilation | The implementation satisfies the compiler's checked type relationships | Whether the rule matches your intention |
| Passing tests | The asserted examples behave as expected in the tested environment | Whether important examples are missing |
| A check with an external provider | The tested interaction works with that setup | How every live failure or repeated request behaves |
| Deployment checks and observation | The deployed revision exhibits the checked behaviour | Whether every future condition will work |

The compiler can reject structural mistakes that would otherwise be easy to miss.
It cannot know whether three is the right limit, whether cancellation should be
allowed, or whether a rule treats people fairly. Those are policy decisions.
Tests can express your answers, but passing tests only supports the cases they
actually assert.

Ask for the scenarios and outcomes, not just “everything is green.” For a view
that updates asynchronously, distinguish “the command was accepted” from “the
view now reflects the event.” For an integration, distinguish “we asked the
provider” from “the provider completed the work.” The
[testing chapter](/build/testing/) develops the mechanics later.

## Two different roles for AI

The coding agent beside you changes the implementation: it writes source, runs
checks, and explains its work. You review a proposed change before relying on the
resulting application.

AI inside an application has a different role. It might suggest a category,
summarize a document, or propose an action. When you design it to act through
commands, it requests a domain change that the application's decision rules can
accept or refuse. A confident model response is not itself an accepted event.

Neither role removes your responsibility to define permitted behaviour. You must
design the available actions, access rules, and any human approval needed. A
coding agent can implement those boundaries; runtime AI must operate within the
boundaries you actually implemented. Later chapters explain
[AI assistance](/connect/ai/) and [constrained tools](/connect/ai-tools/).

## Let confidence grow with the evidence

Start with small changes whose outcomes you can easily inspect. As you learn the
model and see the agent handle your examples reliably, delegate larger slices.
Spend more attention where a mistake has greater consequences: irreversible
external actions, access changes, or changes to how old history is interpreted.

Trust can be specific. You may confidently delegate a familiar query change while
asking for a closer explanation of a new payment workflow. Let the evidence guide how closely you review.

Choose one rule in your own application. Write an example that would expose a
plausible but wrong implementation, then identify the evidence you would need.

<details>
<summary>A useful self-check</summary>

Can you state the expected outcome before seeing the implementation? Does your
example exercise a refusal, a boundary, or older history? Could the agent show you
the result without requiring you to accept its explanation on faith? If so, you
have a concrete basis for delegation.

</details>

Continue to [set up your first project](/getting-started/). You will use this loop
with small working examples before applying it to more complex features.
