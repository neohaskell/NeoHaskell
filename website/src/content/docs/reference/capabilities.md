---
title: Find the capability you need
description: Map a shop requirement to the appropriate NeoHaskell concept, documentation, and implementation boundary.
sidebar:
  order: 0
---

You do not need to learn every module before building the shop. Start with a business need, find the relevant concept, and go deeper when the current task requires it. This map also helps an evaluator distinguish framework building blocks from application features that a team must implement.

## Model and expose business behaviour

| Need | Capability | Where to continue |
| --- | --- | --- |
| Decide whether an order change is allowed | Commands and `Decider` | [Build the shop](/build/) |
| Preserve accepted facts | Events and event stores | [Persistence](/operate/persistence/) |
| Reconstruct current order state | Entities and snapshot-cache interfaces | [Build the shop](/build/) |
| Show customer and merchant views | Queries, query object stores, pagination | [Build the shop](/build/) |
| Serve commands and queries | HTTP transport, JSON, schema/OpenAPI | [Build the shop](/build/) |
| Wire the application | `Application` and service definitions | [Getting started](/getting-started/) |
| Restrict actions and reads | Authentication, command policies, query access controls | [Security](/operate/security/) |

Commands, events, and queries have derivation markers that generate framework plumbing. Entities supply their initial state and update behaviour explicitly. Learn the conceptual role before exploring how that generation works.

## Connect the shop to its surroundings

| Need | Capability | Boundary |
| --- | --- | --- |
| React to an accepted fact | Outbound integration runtime | Define the business follow-up and failure outcome |
| Receive external triggers or timed work | Inbound integrations | Translate input into an explicit command |
| Call a provider | HTTP client and integration request/response types | Provider-specific contracts still need implementation and tests |
| Connect a merchant’s account | OAuth2 consent, callbacks, and secret storage | [Provider accounts](/connect/provider-accounts/) |
| Send email | Brevo and Azure Communication Services integrations | Requires provider configuration and delivery verification |
| Add language-model features | OpenRouter, AzureAI, and agent/tool integrations | Define allowed actions and how results are checked |
| Process documents | File uploads, PDF, OCR, and related provider integrations | Persist bytes, protect access, and handle extraction failures |
| Process audio | Audio/transcription integrations | Treat external output as fallible input |

Follow [Connect the shop](/connect/) for these subjects. A generic HTTP client is not a ready-made payment or shipping adapter. Commerce policies, taxation, payment reconciliation, and fulfilment workflows remain application work unless a specific implemented adapter says otherwise.

## Use the language vocabulary when needed

The core library includes `Text`, `Array`, `Map`, `Maybe`, `Result`, `Task`, identifiers, dates, logging, and related primitives. Traits describe reusable operations across types. JSON/schema modules connect typed data to external representations.

System modules provide files, paths, directories, environment access, time, and subprocesses. Concurrency modules provide tasks, channels, locks, and shared variables. Those are mechanisms for a particular need; introducing concurrency does not automatically make a business operation atomic across services.

Language-adjacent modules include decimal arithmetic, parsing, and NeoQL tooling. Their existence is not a promise of a complete money/tax model or an unrestricted business reporting language. Choose the supported operation, representation, and boundary tests for the shop's actual requirement.

Consult [language essentials](/build/language-essentials/) and the [glossary](/reference/glossary/) as unfamiliar vocabulary appears.

## Build, inspect, verify, and operate

| Need | Capability | Documentation |
| --- | --- | --- |
| Create and work on a project | Neo CLI | [CLI reference](/reference/cli/) |
| Understand the model visually | Bundled Neo IDE | [Visual IDE](/getting-started/visual-ide/) |
| Establish behavioural evidence | Test DSL, application scenarios, Hurl acceptance tests | [Testing](/build/testing/) |
| Connect persistent stores | Postgres infrastructure | [Persistence](/operate/persistence/) |
| Run and change the application | Probes, logging, replay and compatibility practices | [Run and evolve](/operate/) |
| Improve the framework | Public reference app, repository tools, architecture decisions | [Contributing](/operate/contributing/) |

For source-level detail, the repository's [capability map](https://github.com/neohaskell/NeoHaskell/blob/main/codemap/capabilities.yaml) identifies implementation ownership and tests. A listed capability tells you where to investigate; a specific example and test establish the behaviour you can rely on.

For focused recipes, browse the [practical guides](/guides/).
