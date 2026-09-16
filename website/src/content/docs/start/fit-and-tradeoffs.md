---
title: "Is NeoHaskell a fit for your team?"
description: "Assess adoption value, costs, and responsibilities through a representative workflow."
sidebar:
  order: 4
---

A framework earns its place by helping your team handle the work that matters.
For some applications, that means explaining how a decision was reached. For
others, it means changing rules without losing the meaning of existing records,
or coordinating actions with services that can fail independently.

NeoHaskell is worth evaluating when these concerns shape your application. Its
approach connects business requests, explicit decisions, accepted facts, and the
views people use. That gives humans and coding agents a shared structure for
building and discussing behaviour. The adoption question is whether that
structure solves enough of your problems to justify learning and operating it.

Start with a representative workflow and decide what evidence would make you
confident enough to continue.

## Look for value from several perspectives

**For the person building the application**, named commands, events, and queries
provide places to put different responsibilities. You can ask an agent to
implement a bounded behaviour, inspect its decisions, and test its outcomes.
This is useful when you understand the process better than you understand every
line of code. You still need enough understanding to recognise a wrong rule and
ask for meaningful evidence.

**For product and domain specialists**, an event model makes missing steps easier
to discuss before implementation settles them accidentally. “A request was
accepted” and “the work was completed” can appear as different facts. You can ask
what happens between them, who has authority, and what someone sees when progress
stops. The model becomes material for a conversation, rather than a specification
that only its author can interpret.

**For an evaluator**, explicit boundaries offer something concrete to inspect.
Choose a rule, follow it into its implementation and tests, and ask for a change.
Assess whether the team can explain the consequences and maintain the result.
Adaptability is an outcome to demonstrate in your context, not a productivity
promise to accept without evidence.

**For operators**, separating accepted history from the views built from it helps
frame recovery questions. What has been durably accepted? Which views are still
catching up? What happened outside this application? Those distinctions can help
diagnosis, but operating the system still requires storage, backups, monitoring,
and rehearsed recovery procedures.

## Match the approach to the domain

NeoHaskell's event-based application model is especially worth examining where
things have meaningful lifecycles:

- A membership is requested, approved, renewed, suspended, or ended.
- A booking is requested, confirmed, changed, or cancelled.
- A grant application is submitted, assessed, approved, and followed through.
- An order is assembled, accepted, fulfilled, or corrected.

In each case, the latest status answers only part of the question. People may
also need to know what happened before it, or build several views of the same
activity. An explicit history can support those needs when you model the relevant
facts.

A static content site, disposable script, or small lookup tool may gain little
from this structure. A specialised product may need mature domain packages more
than a new way of modelling change. Someone needing a turnkey commerce platform,
for example, must assess the work of supplying payment, shipping, tax, and other
required behaviour. The ecommerce practice project teaches framework concepts;
it does not supply a complete commercial product.

Use the [capability guide](/reference/capabilities/) to distinguish available
building blocks from application-specific work. Evaluate each required provider
and deployment environment directly.

## Treat history as a design commitment

Preserving accepted facts lets you distinguish an earlier action from a later
correction. That can help explain disputes, reconstruct state, and build new
views. It also commits future versions of the application to understanding the
history you retain.

A compiler can help detect incompatible changes in code. Historical data needs
its own compatibility checks. A renamed field, a changed interpretation of an
amount, or a new requirement for older records can require deliberate migration
or interpretation rules. Include that work in the cost of change.

History also contains only what you chose to record. A fact that a request
was refused does not explain the reason unless you preserve it. Recording a
reason does not establish that it was fair or accurate. Distinguish an observation,
a person's judgment, and an automated recommendation when that distinction
matters to later review.

## Decide who may act, question, and correct

Explicit decisions make governance questions visible. Who may change a rule? Who
may request an exception? Which actions may an automated system perform, and
which need a person's approval? How can someone challenge an outcome?

Answer these as application decisions. Permission checks, supporting evidence,
review steps, and correction commands need to be designed and tested. Event
history does not automatically preserve the responsible person, the rule version,
or the evidence considered.

Choose what to retain with equal care. Copying entire requests into retained
history can preserve unnecessary personal information. Decide what context is
needed, where sensitive details belong, who can read them, and how retention and
deletion requirements affect the design. These decisions belong near the start
of a pilot, while changing the model is still manageable.

## Account for learning and operational costs

Your team will learn a language, a toolchain, and a way of modelling application
behaviour. Try the supported [setup](/getting-started/) on the machines the team
uses, and allow time to understand the first build and error messages. Assess
whether someone besides the initial author can maintain the result.

Queries update separately from accepted commands, so a screen may briefly show
an older view. External actions introduce partial failure and retries. Durable
operation needs database and deployment choices, secret management, and restore
tests. These are concrete responsibilities to explore in [operating an
application](/operate/), rather than details to postpone until launch.

Also consider the ecosystem you depend on: integration coverage, upgrade work,
troubleshooting resources, and who can help when a problem crosses framework and
application boundaries. Use the pilot to estimate these responsibilities.

## Run a pilot that can change your mind

Choose one workflow with a real rule, a possible refusal, and a visible result.
For a booking tool, reserve a place only while capacity remains. For the practice
project, add a positive quantity to an existing cart. Write the expected success,
rejection, and boundary outcomes before implementation.

Then collect evidence that addresses your adoption questions:

1. Ask another person to explain the behaviour from the model and find its checks.
2. Introduce a deliberately wrong rule and see whether the checks expose it.
3. Request a small variation and examine which contracts and tests must change.
4. Read a representative older history with the changed application.
5. If durability matters, restart and rehearse restoration using the chosen stores.
6. If integrations matter, exercise a provider failure and explain the resulting state.

Record confusion and missing capability as carefully as success. Compare the
result with a familiar alternative against the same workflow and expectations.
You may proceed, narrow the intended use, or decide another approach fits better.

A useful decision names the evidence, unresolved responsibilities, and next
experiment. Continue to [the practice project on paper](/start/a-shop-on-paper/)
when you are ready to try the approach.
