---
title: Find the capability you need
description: Map an application requirement to the appropriate NeoHaskell concept, documentation, and implementation boundary.
sidebar:
  order: 0
---

You do not need to learn every module before using NeoHaskell. Start with the task your program needs to perform, find the relevant concept, and go deeper when the current task requires it. This map also helps an evaluator distinguish framework building blocks from application features that a team must implement.

Continue in the application created with `neo new`. In the guided project, Cart and Stock code lives under `src/Shop/`. The links below teach how to use each capability there; public implementation links provide supporting evidence.

## Model and expose application behaviour

| Need | Capability | Where to continue |
| --- | --- | --- |
| Decide whether a requested change is allowed | Commands and `Decider` | [Build an application](/build/) |
| Preserve accepted facts | Events and event stores | [Persistence](/operate/persistence/) |
| Reconstruct an entity’s current state | Entities and snapshot-cache interfaces | [Build an application](/build/) |
| Prepare views for different readers | Queries, query object stores, pagination | [Build an application](/build/) |
| Serve commands and queries | HTTP transport, JSON, schema/OpenAPI | [Build an application](/build/) |
| Wire the application | `Application` and service definitions | [Getting started](/getting-started/) |
| Restrict actions and reads | Authentication, command policies, query access controls | [Security](/operate/security/) |

Commands, events, and queries have derivation markers that generate framework plumbing. Entities supply their initial state and update behaviour explicitly. Learn the conceptual role before exploring how that generation works.

## Connect an application to other systems

| Need | Capability | Boundary |
| --- | --- | --- |
| React to an accepted fact | Outbound integration runtime | Define the business follow-up and failure outcome |
| Receive external triggers or timed work | Inbound integrations | Translate input into an explicit command |
| Call a provider | HTTP client and integration request/response types | Provider-specific contracts still need implementation and tests |
| Connect a user’s provider account | OAuth2 consent, callbacks, and secret storage | [Provider accounts](/connect/provider-accounts/) |
| Send email | Brevo and Azure Communication Services integrations | Requires provider configuration and delivery verification |
| Add language-model features | OpenRouter, AzureAI, and agent/tool integrations | Define allowed actions and how results are checked |
| Process documents | File uploads, PDF, OCR, and related provider integrations | Persist bytes, protect access, and handle extraction failures |
| Process audio | Audio/transcription integrations | Treat external output as fallible input |

Follow [integrations](/connect/) for these subjects. The ecommerce practice project illustrates how they combine, but the capabilities apply across domains. A generic HTTP client still needs a provider contract: for example, it is not a ready-made payment or shipping adapter. Domain-specific rules and workflows remain application work unless a specific implementation supplies them.

## Use the language vocabulary when needed

The core library includes `Text`, `Array`, `Map`, `Maybe`, `Result`, `Task`, identifiers, dates, logging, and related primitives. Traits describe reusable operations across types. JSON/schema modules connect typed data to external representations.

System modules provide files, paths, directories, environment access, time, and subprocesses. Concurrency modules provide tasks, channels, locks, and shared variables. Those are mechanisms for a particular need; introducing concurrency does not automatically make a business operation atomic across services.

Language-adjacent modules include decimal arithmetic, parsing, and NeoQL tooling. Their existence is not a promise of a complete money/tax model or an unrestricted business reporting language. Choose the supported operation, representation, and boundary tests for your application's actual requirement.

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
