---
title: "Is it a fit for your company?"
description: "Assess the benefits and costs with a representative workflow before committing."
sidebar:
  order: 2
---

A framework is useful when its choices help your team handle the work that matters.
For our shop, that work includes explaining orders, making safe changes to rules,
and coordinating with services that can fail independently.

NeoHaskell is worth evaluating when those concerns are central to your application.
A short experiment with one real business rule is stronger evidence than a broad
promise about productivity.

## Questions to take into an evaluation

| Your need | What to examine | Cost or responsibility |
| --- | --- | --- |
| Explain how an order reached its state | Events and reconstructed state | Choose meaningful facts and preserve their history |
| Change software with an agent | Shared conventions, model inspection, tests | Review intent and evidence; do not delegate business ownership |
| Serve several views of the same process | Query projections | Account for the delay between accepting a command and updating a view |
| Coordinate payment, stock, and email | Explicit integrations and outcomes | Handle partial failure, duplicates, and provider-specific rules |
| Run a durable service | Postgres-backed components and readiness | Operate backups, restore tests, secrets, and rollout checks |

The ecosystem includes concrete integrations and lower-level building blocks.
It does not provide a complete ecommerce platform. Check the [capability guide](/reference/capabilities/)
for what is present and what your application must supply.

## Where to look especially carefully

A tiny disposable tool may not benefit from a durable event history. A team needing
a turnkey storefront, tax engine, or payment package must assess the work of building
those pieces. A team unable to operate the chosen database and deployment environment
needs a plan for those responsibilities before promising a launch.

Event history is also a data-design commitment. Recording unnecessary personal
information creates future retention and access problems. Decide what facts the
business needs and how sensitive details are stored before copying a full request
into an event.

The language and toolchain are additional learning. Start with the supported
[Nix-based setup](/getting-started/), allow time for the first build, and test on
the platforms your team actually uses. Example applications deliberately simplify
permissions and persistence; their defaults are not production approval.

## Run a useful pilot

Choose a workflow small enough to finish: adding an item to a cart, refusing an
invalid quantity, and showing the resulting cart. Before implementation, write
three examples: one that succeeds, one that must fail, and one at the allowed limit.

After building, ask someone other than the author to explain the rule from the
model, find its checks, and request a small variation. Record what confused them,
which checks caught mistakes, and how long the change took. Include a restart and
an external-service failure when evaluating durability and integrations.

The decision is whether your team can understand and operate this particular
application with confidence. Continue with [the shop on paper](/start/a-shop-on-paper/)
or the deeper [deployment responsibilities](/operate/deployment/).
