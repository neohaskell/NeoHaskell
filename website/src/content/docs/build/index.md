---
title: "Build your application"
description: Grow one project from its first request to decisions, views, tests, and integrations.
sidebar:
  order: 0
---

An application accepts requests, applies rules, remembers what happened, and presents useful information. NeoHaskell gives those responsibilities explicit places in the code. Understanding those connections helps you build behaviour you can explain and verify.

This journey stays inside **your own project**, created with `neo new mug-shop`. You will author its modules, build with `neo build`, run with `neo run`, check it with `neo test`, and explore it with `neo ide`. Each lesson grows that same application.

Our recurring practice project sells a mug. Ecommerce gives quantities, availability, and external work familiar meanings. Your actual application may manage appointments, documents, logistics, or something else; carry the method across to its requests and promises.

## Choose your depth

If you are evaluating NeoHaskell, read the opening and decision sections. They explain the benefits and the responsibilities your team retains. Start with [why NeoHaskell](/start/why-neohaskell/) for the broader case.

If you are building, complete [getting started](/getting-started/) and follow these milestones. Your coding agent can enter and adapt the files with you. The docs still show the implementation and its evidence, so you can question what it means.

| Milestone | What you will understand or build |
| --- | --- |
| [Your first working slice](/build/first-cart/) | Create Cart modules and read your own application's first result. |
| [Explore your cart visually](/getting-started/visual-ide/) | Connect the command, event, and summary in your project's IDE. |
| [Commands and events](/build/commands-and-events/) | Add an action with an explicit positive-quantity rule. |
| [Entities and state](/build/entities-and-state/) | Explain how accepted history informs the next decision. |
| [Queries](/build/queries/) | Shape information around a reader's question. |
| [Stock and checkout](/build/stock-and-checkout/) | Add a second domain and identify the coordination it needs. |
| [HTTP and frontends](/build/http-and-frontend/) | Connect an interface to real application outcomes. |
| [Testing behaviour](/build/testing/) | Write decision, replay, and HTTP checks. |
| [Access control](/build/access-control/) | Decide who may act and whose records they may see. |
| [Configuration](/build/configuration/) | Connect settings to their actual consumers. |
| [Review your application](/build/your-shop/) | Establish what works and choose the next useful slice. |
| [Language essentials](/build/language-essentials/) | Read unfamiliar syntax as it becomes useful. |

The language page is a companion, not an entrance exam. You can understand an action's meaning before memorising every declaration that supports it.

## Grow one promise at a time

The first application creates empty carts. We then add selections, a summary, and stock decisions. The [integration section](/connect/) joins those decisions and introduces external providers. [Run and evolve](/operate/) takes the same project into persistence, deployment, and change.

Each step has boundaries. A cart addition establishes a selection; an accepted order would require additional policy. A reservation rule can be tested before it has a trigger. A successful provider reply needs to be matched to the operation that requested it. Keep those meanings explicit as the project grows.

The introductory app uses an in-memory store and public local-development policies. Later chapters deliberately introduce durable storage and authentication rather than silently assuming they were already configured.

## A first decision you own

Does “two mugs” mean one cart entry with quantity two, or two separate selections? We will use one entry per accepted addition. That makes one addition of two mugs count as one entry in the summary.

<details>
<summary>Suggested reasoning and checks</summary>

State what the interface's count means before implementing it. Check an empty cart, one addition of two units, a second addition, and a rejected zero quantity. If your application should merge repeated products or display total units, design and test that change explicitly.

</details>

Start with [your first working slice](/build/first-cart/). Consult the [glossary](/reference/glossary/) whenever a term needs a quick reminder.
