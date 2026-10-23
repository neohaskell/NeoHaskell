---
name: principle-name-for-the-reader
description: Choose NeoHaskell module, type, and operation names that a junior TypeScript developer can understand without knowing GHC implementation terms.
---

# Name for the reader

Name the observable domain operation before its implementation mechanism. A junior TypeScript developer must be able to predict what a call does from its module, operation, arguments, and result.

Design the qualifier and operation together. A type-centered module supplies the subject. Prefer `IntegrationOutcome.text outcome` in `Service.Integration.Dispatcher.IntegrationOutcome` over `integrationOutcomeText outcome` in a large dispatcher. Keep helper names specific to their domain responsibility.

Before naming a concurrency operation, read its implementation and callers. Describe its cancellation and ownership contract in ordinary words. `AsyncTask.runUnmasked` exposes GHC terminology. It starts a child that can receive cancellation even when its parent is protecting an ownership handoff. Choose its eventual public name from that contract, then test the contract. A synonym that conceals the cancellation behavior is not an improvement.

Use established domain words consistently. Reserve GHC vocabulary for the module that implements the mechanism and documentation that defines the precise contract. Keep public Haddock when ordinary words need a precise qualification.

For an API rename, inventory imports, generated consumers, and documentation before changing it. Follow [consumer verification](../neohaskell-consumer-verification/SKILL.md) for exported APIs. A naming review alone does not authorize a production rename.
