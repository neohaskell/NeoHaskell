---
name: principle-test-behavior-not-implementation
description: "Apply when you write, change, or keep a test. Assert what the code's users can observe, so that the test fails when that behavior breaks and keeps passing when only the internals change. Use property-based testing for contracts over many inputs."
---

# Test Behavior, Not Implementation

A test is a claim about what the code does for its users: a caller, an end user, or another service. Behavior is what those users can observe through the interface they use. That includes return values, errors, state they can read back, and effects on systems outside the code, such as a request sent, a row written, or a message emitted. Implementation is what they cannot observe: private helpers, internal call order, intermediate data, and which internal collaborator did the work.

Judge each test against two imagined edits:

- **Break the behavior.** Make the code return a wrong value, skip an effect, or do the effect twice. If the test still passes, it proves nothing. The usual causes are an assertion looser than the contract, an expected value computed by the code under test, a restated constant or prompt string that never runs the code that reads it, and an assertion on data that the test built itself.
- **Change the internals.** Keep every observable result the same and restructure the code underneath. If the test now fails, it is pinned to the implementation. It will need a rewrite on every refactor and still catches no bug.

Keep a test that catches the first edit and survives the second. Rewrite or delete the rest. To write a test that does both, drive the code through its users' interface with a concrete input. Derive the expected value from the requirement, not from a run of the code. Assert what the contract promises, no less and no more.

The contract decides, not the shape of the assertion. When "an empty cart never charges" is the promise, an assertion that the payment client got no call is behavior. Pair it with a case where the charge happens, so that the test shows it can see a call. The payload that a mocked external service receives is behavior, because that service observes it. A call to an internal helper is implementation, because no user can see it. An empty list is the exact answer when no record matches, and a hollow check when the test only needs some result. An exact error message pins wording that no caller reads. The error type that callers branch on is the contract.

A test can pin an invariant instead of a call, such as a key that must exist in two tables or a compile-time type check. The invariant is the behavior.

## Property-based testing

- **Express the contract as a property.** Use generated inputs for collection laws, codec round trips under documented normalization, or invariant preservation across operation sequences. State the law through the caller's interface, without copying the implementation into the expected result.
- **Generate meaningful cases and shrink failures.** Cover empty, singleton, boundary, and varied inputs relevant to the contract. Keep generators and shrinkers within required preconditions; generate invalid inputs deliberately when testing rejection. Avoid properties that pass trivially or discard most cases.
- **Use QuickCheck in the owning Hspec suite.** QuickCheck is already a core dependency. Register properties through Hspec's property integration, such as `Test.Hspec.QuickCheck.prop`, so failures fail the suite. The current `Test` facade has no property helper. Calling `QuickCheck.generate` to produce a fixture alone does not run a property check.
- **Make counterexamples reproducible.** Keep the shrunk failing input and QuickCheck replay information, including seed and size. Turn a discovered bug into a focused regression test.
- **Keep independent examples and type checks where they establish the contract.** A round-trip property can miss matching bugs in both directions. Pair it with independently specified examples or a simple reference model rather than relying on agreement between two implementations under test.
