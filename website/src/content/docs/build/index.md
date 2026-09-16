---
title: Build an application
description: Turn application rules into behaviour you can inspect, test, and confidently delegate.
sidebar:
  order: 0
---

An application accepts requests, applies rules, remembers what happened, and presents useful information. NeoHaskell gives those responsibilities explicit places in the code. Understanding how they fit together helps you build applications whose behaviour you can explain and verify.

This section teaches you to direct your agent and assess its work. You will follow business decisions into the model, inspect the implementation, and check the resulting behaviour. You do not need to review every line to retain ownership of those decisions.

We practise with a small ecommerce project that begins with one mug and a cart. It gives the concepts a familiar, connected example to grow through the docs. Apply the same reasoning to the requests, rules, and workflows in the application you want to build.

## Choose your starting point

If you are evaluating NeoHaskell, read the opening and decision sections of each page. Together they explain what the approach offers and what responsibility remains with your team. Start with [why NeoHaskell](/start/why-neohaskell/) if you want the broader case first.

If you are building, complete [getting started](/getting-started/), then follow the sequence below. Keep the [visual IDE](/getting-started/visual-ide/) available to connect the business model with the code. The graph helps you locate relationships; tests establish what those relationships actually do.

| Milestone | What you can understand or decide afterwards |
| --- | --- |
| [First working slice: a cart](/build/first-cart/) | Identify an accepted request and read its result. |
| [Commands and events](/build/commands-and-events/) | State a rule and recognise the difference between a request and a recorded fact. |
| [Entities and state](/build/entities-and-state/) | Explain how history produces the state used for the next decision. |
| [Queries](/build/queries/) | Choose what each reader needs to see. |
| [Coordinating changes: stock and checkout](/build/stock-and-checkout/) | Identify where two business processes need coordination. |
| [HTTP and frontends](/build/http-and-frontend/) | Connect a screen to the real application contract. |
| [Testing behaviour](/build/testing/) | Ask for evidence at the right boundary. |
| [Access control](/build/access-control/) | Decide who can act and whose information they can see. |
| [Configuration](/build/configuration/) | Separate deployment settings from business rules. |
| [Assemble a practice application](/build/your-shop/) | Bring Cart and Stock into a generated project and verify the connection. |
| [Language essentials](/build/language-essentials/) | Read the vocabulary your agent uses without a separate language course. |

The language page is a companion you can consult whenever a symbol gets in the way. It is not a prerequisite exam.

## What you will actually run

There are two public examples, with different purposes:

- The **generated starter** contains a Counter. It is a small working orientation to commands, events, and queries.
- The **repository testbed** contains Cart and Stock. These are the executable anchors for the ecommerce exercises.

The testbed is a feature demonstration with deliberately permissive policies. A cart is a customer's selection of goods; it is not an accepted, paid, or fulfilled order. The docs identify where you are reading existing behaviour and where you are designing an extension to the practice project. There is no hidden complete ecommerce application to install.

That distinction is useful when working with an agent. Ask it to name which behaviour already exists, which policy you must decide, and which change it proposes. A plausible function name is not evidence that the feature exists.

## A first decision you own

For the first exercise, call the product **Everyday Mug**. Decide whether “two mugs” means one line with quantity two or two separate selections. Both representations can work, but they affect the count displayed on the cart icon. This is a small example of a general responsibility: define what a displayed value means before asking an agent to calculate it.

<details>
<summary>Suggested reasoning and checks</summary>

The current testbed appends an entry for every accepted `AddItem`. Its summary counts entries, even when an entry has quantity greater than one. One addition of two mugs therefore produces `itemCount = 1`. Test a second addition, a rejected zero quantity, and the first valid quantity of one. Decide what the screen label should mean before changing the implementation.

</details>

When the core behaviour is understandable, continue to [integrations](/connect/) and [running and evolving applications](/operate/). Use the [glossary](/reference/glossary/) whenever a term needs a quick reminder.

Public anchors: [starter application](https://github.com/neohaskell/NeoHaskell/blob/main/neo/starter/src/App.hs), [Cart model](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Core.hs), [Cart summary](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Queries/CartSummary.hs).
