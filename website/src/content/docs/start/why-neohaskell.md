---
title: "Why NeoHaskell?"
description: "Understand why preserving meaning matters when people and AI agents can change software quickly."
sidebar:
  order: 1
---

You know what you want an application to do. Perhaps it should organise bookings,
help people make decisions, manage a membership, or keep track of work. You can
explain who uses it, what they are allowed to do, and what a successful result
looks like. Turning that understanding into software is one challenge. Keeping
it intact as the software changes is another.

NeoHaskell starts with the second challenge. It brings together a Haskell dialect,
an application framework, and development tools around a simple ambition:
**you should be able to understand what your application means while people and
AI agents build and change it.**

That ambition affects how you describe a feature, how the application remembers
what happened, and how you decide whether an agent's work is ready to use. You can
understand the idea before learning a programming language or installing anything.

## What becomes important when code is easier to produce?

A coding agent can turn a short request into a substantial amount of code. It can
write a screen, connect a service, and propose a change while you are still
clarifying what you meant. That makes experimentation more accessible. It also
makes an unclear instruction consequential very quickly.

Imagine asking for a change of delivery address. There are several reasonable
interpretations: change the default for future orders, change one order that has
not shipped, or update every order associated with that person. Each can produce
code that runs. They express different promises.

The difficult question is therefore more specific than “Does the code work?” It
is “Which decision did we make, where does it apply, and how can we tell that the
application still respects it?” A large amount of plausible code cannot answer
that on its own.

This is what *meaning* refers to throughout these docs. A number represents
something. A status makes a claim about a process. A permission grants a particular
person authority. A change is correct only in relation to those intentions.

> Faster implementation makes a clear model more valuable: there is less time
> between a misunderstanding and the software built from it.

## Give the important ideas a shared shape

NeoHaskell organises application behaviour around a few concepts that can also be
discussed in ordinary language:

- A **command** asks for something to happen.
- A **decision** checks whether that request is allowed in the current situation.
- An **event** records an accepted fact.
- An **entity** is the thing whose current state informs the decision.
- A **query** prepares information for someone who needs to read it.

You do not need to memorise those terms yet. Picture a request reaching a clear
point of decision. If accepted, it becomes a recorded fact. If refused, the caller
gets a reason and the requested change does not become a fact.

[![A request passes through access checks and a decision based on current state. Acceptance records an event; refusal returns a reason without that event.](/diagrams/request-decision-event.svg)](/diagrams/request-decision-event.svg "Open diagram at full size")

*The application evaluates intent before recording a change. The diagram describes
normal command handling; access policies and business rules must be wired and
implemented by the application.*

For the ecommerce practice project, “reserve one item” is a request. “One item was
reserved” is an accepted fact. Confusing them would let a screen promise stock
before the application had actually secured it. The same distinction matters for
“approve this application,” “book this appointment,” or “publish this document.”

The vocabulary helps you locate a disagreement. Is the request unclear? Is the
rule wrong? Is a fact missing? Is the screen showing the wrong information? Those
are smaller, more useful questions than asking an agent to fix “the system.”

## Remember how the present came about

A current value is often a summary. A balance summarises money movements. A
booking status summarises a sequence of requests and decisions. A membership's
current tier does not, by itself, explain which terms applied last year.

An application can keep only the latest value, or it can preserve the meaningful
facts from which that value is derived. NeoHaskell's framework uses the latter
approach, called **event sourcing**. Accepted events form the history from which
entity state is reconstructed. Read models use events to prepare useful views.

This gives change a visible shape. A correction can record what was corrected;
a cancellation can retain the fact that something was previously accepted. A new
report can interpret the facts already available, instead of depending entirely
on what one screen happened to show at the time.

There is a limit to what history can tell you. If you never recorded the relevant
reason, price, identity, or evidence, replay cannot invent it. Choosing which facts
matter is part of designing the application. The next chapter,
[history and change](/start/history-and-change/), works through this with small
examples you can check by hand.

## Put the model where people can discuss it

**Event Modeling** is a way to describe the sequence of requests, decisions,
accepted facts, and information that people need. It lets a person who understands
the process participate before the implementation is buried in code.

For example, you can point to a step and ask, “What happens if this request arrives
twice?” or “Who is allowed to reverse this decision?” You can notice that an email
may fail after an order was accepted. You can require the interface to distinguish
“requested” from “confirmed.” These are useful contributions even if you cannot
write the implementation yourself.

NeoHaskell gives those ideas corresponding structures in the program. The Neo IDE
also provides a graph for exploring the model alongside the source. Today, source
synchronisation updates the model from the code; drawing a box does not generate
a complete application. You can use the graph to explain the intended change to
an agent and inspect how its implementation connects.

A useful model remains close enough to the program that the conversation can
continue after the first version ships. It becomes something you return to when
making decisions, investigating a problem, or introducing someone to the project.

## Make the scope of a change understandable

A feature often becomes easier to reason about when you can follow one complete
piece of behaviour: what triggers it, which rule applies, what it records, and what
someone can see afterwards. We call that a **slice**.

A slice gives an agent a bounded task and gives you something concrete to accept.
“Let a member request a renewal and show whether it succeeded” has a clearer scope
than “build membership management.” In the practice project, creating a cart is a
first slice; preparing its summary and connecting stock are further steps.

Stable boundaries let new behaviour build on existing facts without depending on
every detail of the earlier implementation. They can make changes easier to review
and divide between people or agents. A shared event still creates a real dependency:
changing its meaning may affect several readers. [Growing by slices](/start/growing-by-slices/)
explains both the benefit and that responsibility.

## What the language and framework contribute

The approach could be built in other languages. NeoHaskell brings the vocabulary,
conventions, and runtime machinery together so you can work within a consistent
structure.

The language's types describe relationships between values and operations. The
compiler can reject incompatible pieces before the program runs. The framework
provides command execution, event storage, entity reconstruction, queries, and
integration mechanisms. The CLI provides a common route through creating,
building, testing, and inspecting a project.

Those parts contribute different forms of evidence. A program that compiles can
still implement an inappropriate quantity limit. A test can assert the wrong
answer. A model can leave out an important refusal. The value is that you have
specific places to express and examine each concern, together with tools that
check particular kinds of mistake.

You remain the person who decides what the application is for. Your agent can do
much of the implementation work, while you learn to ask for observable evidence
that it followed the intended rules. [Working with an agent](/start/trusting-your-agent/)
shows how that relationship develops.

## The practical value to look for

For someone with an application idea, the benefit is a clearer path from an
intention to a feature they can explain and check. For a team, it is a shared way
to discuss changes, preserve important history, and bring another contributor into
the work. For a person evaluating adoption, it is a concrete basis for asking how
an application will remain understandable after its first release.

These are benefits to evaluate on your own work. They do not imply a universal
speed improvement or that every application needs event sourcing. Recording useful
history, preserving its meaning, and operating durable storage take effort. A
small static site may have little reason to pay that cost; a long-lived process
with revisions, disputes, and several views of the same activity may have much more.

You can continue without writing code. First, see [how a history explains the
present](/start/history-and-change/). Then explore [how features grow](/start/growing-by-slices/)
and [whether the tradeoffs fit your project](/start/fit-and-tradeoffs/). When you
want to try the method yourself, the [first modeling exercise](/start/a-shop-on-paper/)
starts with one rule and a few examples.
