---
title: Contribute from what you have learned
description: Move from improving an example to changing NeoHaskell's public behaviour with reproducible evidence.
sidebar:
  order: 8
---

You do not need to understand the whole framework to make it better. A confusing installation step, a missing boundary case, or an example that no longer compiles is a useful starting point. Your experience learning NeoHaskell or building an application is evidence about where the next reader may struggle.

Contribution branches from the application journey. Running your application confidently does not require becoming a framework maintainer.

## Start with a bounded improvement

A good first report includes what you were trying to accomplish, the smallest reproduction, the expected behaviour, the actual result, and the relevant version. Remove credentials and private data before sharing it.

For a documentation change, preserve the gradual learning path: explain the situation first, provide an accurate example, and say how a reader can verify the outcome. For a bug, add a regression case that fails for the reported reason before changing implementation.

## Find the owning component

| Concern | Repository area |
| --- | --- |
| Language vocabulary and service framework | `core/` |
| Executable cart/stock examples and HTTP acceptance tests | `testbed/` |
| Provider integrations | `integrations/` |
| Rust CLI and bundled visual IDE | `neo/` |
| Human documentation | `website/` |
| Architectural decisions | `docs/decisions/` |

Read the [contributor README](https://github.com/neohaskell/NeoHaskell/blob/main/README.md) and [capability map](https://github.com/neohaskell/NeoHaskell/blob/main/codemap/README.md). The map connects a concept to its implementation and tests, so a small fix need not become an unbounded repository investigation.

## Work with the repository's toolchain

From a NeoHaskell repository checkout, the `./dev` commands enter the pinned environment as needed:

```sh
./dev watch
```

Keep that watcher running while editing. In another terminal:

```sh
./dev check
./dev test "EventStore" nhcore-test-service
./dev lint
```

`./dev test "EventStore" nhcore-test-service` is an example of a focused test selection. Choose the tests that establish your change; a selection unrelated to the fix supplies no evidence. Service tests can require PostgreSQL, and HTTP acceptance tests require their real fixtures. The contributor README describes the setup.

These repository commands differ from `neo build` and `neo test`, which operate on generated applications. Rust CLI and IDE changes have their own scoped instructions and test layers under `neo/`.

## Propose public behaviour before broad implementation

The repository uses change specifications and draft pull requests for governed changes. A specification explains the promised API difference and names the tests that prove each criterion. Maintainer review of that proposal comes before the larger implementation; final review considers the implemented result and verification.

Read the current [repository contribution contract](https://github.com/neohaskell/NeoHaskell/blob/main/AGENTS.md) for the exact scope, branch/stack workflow, review gates, and exceptions. Do not change existing test expectations just to turn a failure green. Explain what changed in user-visible behaviour and obtain the required maintainer review.

## Go deeper when the change requires it

The core service architecture separates command decisions, event persistence, entity reconstruction, queries, transports, and integrations. A change to one public event or derivation can affect several of these. Follow the owning tests and architecture decisions, then verify the public reference application as well as the local unit.

A CLI change can also change every newly generated application. The embedded starter and the starter-to-framework compatibility checks are part of that responsibility.

**Try a first contribution:** choose one confusing moment from the learning journey or your own project. Write the explanation you needed, identify the public source that supports it, and ask another reader to follow it. That is a concrete improvement even before you touch framework internals.
