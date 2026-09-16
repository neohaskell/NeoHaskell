---
title: "Why NeoHaskell?"
description: "Keep business intent understandable while people and coding agents change the software."
sidebar:
  order: 1
---

You can explain the rules your application should follow. Keeping those rules
understandable as the software changes is harder, especially when several people,
services, and coding agents contribute to the same process. A change that looks
reasonable in one place can contradict a decision made elsewhere.

NeoHaskell organises an application around requested actions, recorded facts,
current state, and the information people need. That structure gives you and your
coding agent a common language for discussing behaviour.

## Start with the question the business asks

Take an ecommerce example: a customer asks, “Why was my order cancelled?” Knowing that the current
status is cancelled may not answer them. Knowing that the order was placed, stock
could not be reserved, and cancellation followed gives support a useful history.

In an event-sourced application, accepted facts are stored as events. The current
state is reconstructed from them. Views for screens and reports are built from
that state. Later chapters show the exact boundaries and failure cases behind
this simple explanation.

That history has a cost: you must preserve its meaning when changing the software.
You also decide which facts to record and which information should stay outside a
long-lived history. The same question arises when explaining a cancelled booking
or a rejected approval request: what happened, and which rule led to the outcome?
[Fit and tradeoffs](/start/fit-and-tradeoffs/) explores that choice.

## Why this helps when an agent writes code

Shared conventions give the agent fewer arbitrary architectural decisions to make.
A request to change a rule can be discussed in terms of the command, the
facts it may produce, and the view the user will see. You can inspect the
connections in the Neo IDE graph and verify behaviour through tests.

The compiler checks relationships expressed in types. Runtime rules decide whether
a particular request is allowed. Tests check examples of the behaviour you chose.
None of these determines your business policy for you. A perfectly well-typed rule
can still allow cancellation at the wrong time.

## The business value to evaluate

The potential benefit is a more understandable change process: clearer discussions
with an agent, explicit rules, and a history that helps explain outcomes. Measure
that benefit on a representative workflow in your own team. Do not assume a
particular reduction in cost, development time, or incidents.

NeoHaskell brings a language, framework, CLI, and visual modeling surface together.
It is a commitment to a structured way of building applications. It also brings a
Haskell/Nix toolchain and event-sourcing responsibilities that your team needs to
learn and operate.

A useful first evaluation is small: choose one business rule, ask an agent to
implement it, inspect the model, and deliberately test a request that must be
refused. [Try that conversation without setup](/start/a-shop-on-paper/).
