# Event-triggered internal command and downstream query state

A Cart command causes an event that a typed outbound integration consumes. The integration reconstructs its entity, emits an internal Stock command and causes a second event and query update. Verify the caller-visible effects across both independently updated projections.

## Sub-features

- `indirect-routing` routes the Cart event to the registered typed integration and the emitted command to the correct Stock entity.
- `indirect-state` reconstructs entity state and executes the second decision through the real command/storage path.
- `indirect-projections` observes both Cart and Stock outcomes at each checkpoint.
- `indirect-control` checks an untouched Stock record as a wrong-routing control at the observation boundary.

## How to get to it (user POV)

- Compose the fixture through the framework APIs used by `testbed/src/App.hs`, including the typed outbound integration and both queries.
- Create Stock records and a Cart through `/commands/initialize-stock` and `/commands/create-cart`.
- Submit `/commands/add-item` and observe `/queries/cart-summary` plus `/queries/stock-level`.
- Start from `testbed/tests/scenarios/stock-reservation.hurl`; keep `ReserveStock` on its actual internal transport path instead of inventing an HTTP route for it.

## Driving it with Hurl and correlated query evidence

Preconditions:

- Launch/Doctor has proved an owned testbed and database, usable query readiness and exclusive ownership of the fixture's port 8080 or a proven supported alternative.
- Capture target Stock S, control Stock U and Cart C from their creation responses, using distinct product identities. Baseline predicates require exactly one matching row each: S at 100 available/0 reserved, U at 37/0, and C at itemCount 0/isEmpty true.
- The generated harness includes the additional Cart/control checkpoints below. The existing Stock-only recipe does not supply those stronger assertions.

- **First addition.** POST `{cartId:C, stockId:S, quantity:5}` to `/commands/add-item` using the captured identifiers. Retain the actual request and response, then wait for both required query predicates.
- **First checkpoint.** Require C at itemCount 1/isEmpty false, S at 95 available/5 reserved, and U unchanged at 37/0. Select exact IDs and require exactly one matching row for each.
- **Second addition and checkpoint.** After that complete checkpoint, submit quantity 10 through the same route. Require C at itemCount 2/isEmpty false, S at 85/15 and U still 37/0. Cart itemCount counts entries, not the summed quantity 15.
- **Capture proof.** Retain both actions and both projections at each checkpoint, exact IDs/product identities, build/environment identity, bounded wait results and last observed values on failure. The baseline command is `hurl --test testbed/tests/scenarios/stock-reservation.hurl`; the generated recipe must add, document and execute the strengthened assertions. For exact event linkage or append counts, supply read-only stream evidence proving reservation quantities and `cartId=C`; that capture helper is additional harness work.
- **Clean owned state.** After retaining diagnostics, stop only the owned process and remove owned scratch/database/upload state. Verify proof remains in its named `.agents-state` evidence directory.

## Gotchas

- The Cart projection and integration are independent event consumers. Completion requires the specified Cart and Stock predicates; command acceptance or one query alone cannot substitute for the other. Use bounded predicate retries and do not automatically retry mutations.
- The integration reconstructs current stored Cart state, not an event-bounded snapshot. This fixture relies on stable Cart identity. Exact state checks can detect observed duplicate effects but do not establish exactly-once delivery, exclude later duplicates or prove restart replay/durability.
- `/ready` concerns query rebuild readiness, not completion of each later integration. Declared configuration is not proof of actual transport wiring; establish the real port, database and upload-directory ownership before driving.
- A later insufficient-stock variant requires correlated terminal evidence for the downstream rejection. Unchanged Stock after a timeout alone does not prove rejection or nonexecution, and this interaction defines no cross-service rollback guarantee.
- Cart/control checkpoints and stream-correlation capture extend the maintained recipe. Report an unimplemented or unproved extension as a harness gap rather than claiming the stronger feature passed.
