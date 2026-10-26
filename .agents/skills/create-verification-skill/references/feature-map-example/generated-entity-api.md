# Generated entity API

A framework caller can derive event/entity APIs in a consuming module and rely on their initial state, update, routing and serialization contracts. Verify that the consumer compiles and that the generated behavior actually executes.

## Sub-features

- `derive-consumer` compiles declarations and generated instances used by the consumer.
- `derive-initial` uses the supplied initial state through the generated API.
- `derive-update-route` executes the supplied update and preserves the routed identifier.
- `derive-serialization` exercises the generated serialization contract asserted by the fixture.

## How to get to it (user POV)

- Consume `deriveEvent` and `deriveEntity` as demonstrated by `core/test/Service/Entity/TH/FreshFixture.hs`.
- Drive the registered `Service.Entity.TH` group in `core/test/Service/Entity/THSpec.hs` through the existing service test component.

## Driving it with Cabal and Hspec

Preconditions:

- The selected toolchain/dependencies and declared service test target are available. Any database required by the selected suite uses run-owned state.
- The generated skill has a literal build/test and evidence recipe; it records the selected fixture and establishes that the intended compiler and assertions ran.

- **Compile the consumer.** The maintained fixture invokes event/entity derivation, repeats entity derivation and consumes the generated event injection API. The declared service suite is driven with `cabal test nhcore-test-service`; discover any narrower recipe from the actual runner rather than inventing flags. Retain compiler command/output and fixture identity separately from test results.
- **Execute initial-state behavior.** Exercise the registered assertions for generated Entity and Default behavior. The fixture's expected initial count is 10.
- **Execute update and routing.** The maintained test applies delta 4 to count 5 and expects count 9 plus the exact routed identifier. Retain those observed assertions, not just a suite label.
- **Exercise serialization.** Run the fixture's generated JSON behavior assertions and report their actual scope. A roundtrip assertion establishes that roundtrip, not every external schema compatibility claim.
- **Proof.** Capture the commands, selected tests and their results under the run's retained evidence directory; confirm they remain after cleanup.

## Gotchas

- A stale compiled artifact or a constructed deferred action does not prove the intended compiler/runtime work occurred.
- Positive compilation does not prove invalid inputs are rejected; negative compile claims need their own fixture and expected diagnostic evidence.
- Runtime behavior must be asserted independently of how the fixture was constructed. Do not add tautological tests to fill the map.
- This example identifies maintained sources; a generated skill must still prove its selected literal recipe end to end.
