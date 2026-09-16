---
title: "Try modeling without code"
description: "Use a tiny ecommerce exercise to describe a rule and catch an agent's misunderstanding."
sidebar:
  order: 5
---

Start a practice application with one product: a mug. A customer requests two mugs.
The merchant receives the request and decides how to fulfil it. There is no payment
provider, stock reservation, or shipping integration yet. Ecommerce gives us a
familiar example; the skill you are practicing is turning a rule into behaviour
you can check in any application.

For this first sketch, choose a simple rule: **a request may contain between one
and five mugs**. This is a fictional policy for the exercise, not a built-in
NeoHaskell rule.

## Draw three moments

| Before the request | What the customer asks | What we know afterwards |
| --- | --- | --- |
| No order exists | Place an order for two mugs | An order for two mugs was placed |

The request can be refused. The recorded fact describes something accepted. That
is why “Place order” and “Order placed” have different jobs, even though the words
look similar.

A merchant's order list then answers a different question: “What do I need to
fulfil?” The list is a view of the accepted facts. It should not turn a refused
request into a new order.

## Work with your agent

> **Jess:** Customers can order one to five mugs. Show me the rule and examples
> before implementing it.
>
> **Agent:** I will accept any positive quantity. Two mugs succeeds; zero fails.
>
> **Jess:** That misses the maximum. What happens for five mugs and six mugs?
>
> **Agent:** Five must succeed. Six must be refused, and no order should be recorded.

Jess did not need to review a function to spot the misunderstanding. She understood
the business rule and asked for evidence at its boundary. Later, a test will make
that same check repeatable.

## Decide what counts as success

| Request | Expected result |
| --- | --- |
| Two mugs | An accepted order containing two mugs |
| Zero mugs | A refusal, with no new order |
| Five mugs | An accepted order at the maximum |
| Six mugs | A refusal, with no new order |

Also ask what the customer sees after a refusal. A useful explanation helps them
correct the request. A silent failure leaves them guessing whether an order exists.

## Your first variation

Change the exercise's rule to accept up to twelve mugs. Tell your agent what changes and what
must remain true. Choose the examples you would inspect before accepting its work.

<details>
<summary>Suggested reasoning</summary>

Twelve should succeed and thirteen should fail. Zero should still fail. Two should
still succeed. Previously accepted orders should keep their original quantities;
a new policy must not rewrite what customers already ordered.

</details>

You have already practiced modeling, boundary testing, and correcting an agent.
Try naming a limit in your own application: who sets it, which requests does it
affect, and how would you show that the boundary is respected?
Next, [give those ideas names](/start/event-modeling/). When you start coding, the
[public cart example](/build/first-cart/) supplies executable building blocks; the
order policy above remains an explicit design exercise.
