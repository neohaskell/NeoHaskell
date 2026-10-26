---
name: principle-name-for-the-reader
description: Choose NeoHaskell module, type, and operation names that a junior TypeScript developer can understand without knowing GHC implementation terms.
---

# Name for the reader

Name the observable domain operation before its implementation mechanism. A junior TypeScript developer must be able to predict what a call does from its module, operation, arguments, and result.

Test a name without its doc comment. The module qualifier, operation, arguments, and result must communicate what the call does. If the comment is needed to decode that basic action, rename the operation. Keep public-contract documentation for precise guarantees, edge cases, and examples after the name is clear.

Design the qualifier and operation together. A type-centered module supplies the subject. In an illustrative mail service, `DeliveryOutcome.text outcome` converts an outcome to display text. The qualifier names the subject, and the operation names the conversion. Keep helper names specific to their domain responsibility.

Before naming a concurrency operation, establish its cancellation and ownership contract. An illustrative `startCancellableWorker` starts a child that can receive cancellation while its parent protects an ownership handoff. The name exposes the caller-visible guarantee. Choose the public name from the complete contract, then test that contract. A synonym that conceals cancellation behavior is not an improvement.

Use established domain words consistently. Reserve GHC vocabulary for the module that implements the mechanism and documentation that defines the precise contract. Keep public Haddock when ordinary words need a precise qualification.

For an API rename, inventory imports, generated consumers, and documentation before changing it. Follow [consumer verification](../neohaskell-consumer-verification/SKILL.md) for exported APIs. A naming review alone does not authorize a production rename.
