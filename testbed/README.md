# NeoHaskell Testbed

Welcome to the NeoHaskell testbed! This is where we put NeoHaskell through its paces to make sure everything works as expected.

## What is this?

The testbed is a real NeoHaskell application that serves two purposes:

1. **Feature Showcase** - It demonstrates how to use every major NeoHaskell feature in a practical context
2. **Acceptance Testing** - It runs blackbox tests to verify NeoHaskell works correctly from a user's perspective

Think of it as both an example app and a quality gate. If something works here, it should work in your projects too.

## What's Inside

This testbed includes:

- **Real-world implementations** of NeoHaskell features
- **Blackbox tests** using [Hurl](https://hurl.dev/) to test from outside the app
- **End-to-end scenarios** that verify the entire NeoHaskell toolchain
- **Documentation examples** that we reference in tutorials

## Running the Testbed

### Generated Event Model rules

The `nhtestbed-spec-demo` suite runs each direct rule in `Testbed.EventModel` as a separate Hspec example. It registers an `Int` generator for the cart quantity rule and uses a fixed seed, so a failed sample can be replayed. The same suite checks intentionally failing ordered-event, rejection, and query-diff fixtures, plus concurrent samples with overlapping cart IDs.

From the repository root, run the focused generated rules without PostgreSQL:

```sh
nix develop --command .agents/skills/tdd/scripts/test-match 'Cart flow / Manage carts' nhtestbed-spec-demo
```

Run the full executable fixture suite with:

```sh
nix develop --command cabal test nhtestbed:nhtestbed-spec-demo --project-file=cabal.project.dev -v0
```

The fixture declarations live in `testbed/spec-demo/SpecDemo.hs`. `Service.EventModel.RuleRunner.specFor` creates the named examples from the model, and `runRules` returns per-sample reports for failure checks.

### Protected PostgreSQL round trip

From the repository root, start PostgreSQL with a test user that can create databases. Run the maintained testbed against private ports, then create another fresh database for the service fixture:

```bash
DB_HOST=127.0.0.1 TESTBED_HTTP_PORT=19150 TESTBED_OAUTH_PORT=19151 TESTBED_FIXTURE_PORT=19152 nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict
```

```bash
private_db="nh_cryptoerasure_$(python3 -c 'import uuid; print(uuid.uuid4().hex[:12])')"
PGPASSWORD=neohaskell nix develop --command createdb -h 127.0.0.1 -U neohaskell "$private_db"
CRYPTOERASURE_TEST_DB="$private_db" nix develop --command .agents/skills/tdd/scripts/test-match 'Erasable key store' nhcore-test-service
```

The Hurl scenario asserts a command-generated event, a reconstructed query, two independently protected values, a mapped copy, nested optional values, and Unicode `Available` content. The service fixture uses real PostgreSQL to assert encrypted event storage, restart, key/event rollback, and a private tombstone yielding `Erased`. It rejects equivalent and conflicting protected event ID retries, mixed batches, and concurrent retries through separate pools. Create the named private database before running the fixture. The fixture rejects names without the `nh_cryptoerasure_` prefix.

The service fixture also submits two protected events with one fresh key and one key that already exists. PostgreSQL must reject the batch, leave the fresh key and both events absent, and retain the earlier key and event. Set `private_db` and `db_port` to the private database and port created for the fixture, then run:

```sh
CRYPTOERASURE_TEST_DB="$private_db" CRYPTOERASURE_TEST_DB_PORT="$db_port" nix develop --command .agents/skills/tdd/scripts/test-match 'rolls back a fresh key and events when another key in the batch already exists' nhcore-test-service
```

To check transaction faults against an isolated PostgreSQL instance, set its port in `db_port` and reserve another loopback port for the commit-response proxy. Start the proxy before the fixture. It drops one response only after PostgreSQL reports a completed key and event statement. The fixture also terminates a writer while that statement is blocked, forces two separate-pool writers to overlap at the SQL insert, and reads the durable event and key rows before retrying.

```sh
db_port=19105
proxy_port=19106
private_db="nh_cryptoerasure_$(python3 -c 'import uuid; print(uuid.uuid4().hex[:12])')"
PGPASSWORD=neohaskell nix develop --command createdb -h 127.0.0.1 -p "$db_port" -U neohaskell "$private_db"
(
  python3 -u testbed/scripts/cryptoerasure-commit-proxy.py --listen "$proxy_port" --upstream "$db_port" --evidence .agents-state/cryptoerasure-commit-proxy.json &
  proxy_pid=$!
  trap 'kill "$proxy_pid" 2>/dev/null || true' EXIT
  CRYPTOERASURE_TEST_DB="$private_db" CRYPTOERASURE_TEST_DB_PORT="$db_port" CRYPTOERASURE_COMMIT_PROXY_PORT="$proxy_port" nix develop --command .agents/skills/tdd/scripts/test-match 'Erasable key store' nhcore-test-service
)
```

The fixture requires 12 passing examples. The proxy evidence file must contain `"dropped_after_commit": true`. Use a fresh proxy process for each run because it drops only the first completed statement response.

### Application-owned erasure

The `OwnedProtectedEntity` example in `testbed/src/Testbed/Erasable/Owned/` shows an application accepting a request from the owner of a protected record. Its `RequestErasure` command checks the owner and reference, then records a pending request. `OwnedErasureOutbound` erases one reference through `Integration.Erasable.Erase` and sends a result to the application's internal `RecordErasureResult` command. The query exposes completion separately from the protected value's `Erased` state. A second protected value stays available. The event records a case status of `open` as an illustrative retained business fact; the query rebuilds it from events after restart without reading the erased name. The example also retains the owner ID to authorize retries. Applications must review whether their own owner IDs and other metadata may be retained.

The `nhtestbed-erasure` suite builds a `RecordErasureResult` command with a protected value. It checks that displaying the integration command payload hides its reference and encrypted envelope while the JSON command still decodes. Run that check from the repository root without PostgreSQL:

```sh
nix develop --command .agents/skills/tdd/scripts/test-match 'keeps its protected payload out of Show' nhtestbed-erasure
```

Run the maintained suite from the repository root with PostgreSQL running and ports 8080, 8081, and 8181 free:

```sh
nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict
```

To use private loopback ports, set all three testbed ports before the same command. The runner passes them to both applications, the OAuth fixture, and its Hurl requests. Unset variables keep the default ports above.
The fixture's signed JWT retains its fixed `http://127.0.0.1:8181/oidc` issuer claim; the discovery and key requests use the selected fixture port.

```sh
TESTBED_HTTP_PORT=18880 TESTBED_OAUTH_PORT=18881 TESTBED_FIXTURE_PORT=8181 TESTBED_OAUTH_LOG_DIR=.agents-state/testbed nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict
```

The runner creates private databases, runs `testbed/tests/integrations/cryptoerasure-m2.hurl`, and restarts the OAuth testbed for `testbed/scripts/verify-cryptoerasure-restart.py`. One check rejects the result command after key deletion. A separate check pauses after the key transaction commits and before the result callback runs, stops the owned OAuth process, and restarts it. Both checks read the pending application state after restart, retry explicitly, and require `AlreadyErased` completion. The crash check restarts once more and reads back the completed event through the authorized query. A further check stops the process before the key action, verifies that the key remains active and the request remains pending after restart, then requires an explicit retry to record `ErasedNow`. The application must retain and retry pending requests; the framework does not promise automatic replay after a crash. The OAuth-enabled and disabled application setups both register the outbound handler; the authenticated scenario uses port 8081.

The owned example creates two protected values with identical Unicode content in one command. The strict runner compares both authorized responses, reads their distinct references from the persisted event, erases the first, and restarts the application. It then checks that the first key is tombstoned while the second key and authorized `Available` value survive. A passing run prints `Equal protected values kept distinct references and only one was erased across restart`.

The owned example also creates a separately protected derivative with a third reference. After erasure, `MapErasedSecret` copies the erased first value through `Erasable.map` with a transform that fails if evaluated. The runner restarts the application, reads the rebuilt owner query, and checks the complete persisted event sequence. The first value, its copied reference, and the mapped copy remain `Erased`. The second value and derivative remain `Available`. The case status remains `open` under this example's explicit retained-state policy.

To run this reconstruction check with a private PostgreSQL server already listening on port 19540, use a role named `neohaskell` that can create databases and run the maintained suite from the repository root:

```sh
mkdir -p .agents-state/nh140-cell10
DB_HOST=127.0.0.1 DB_PORT=19540 DB_USER=neohaskell DB_PASSWORD=neohaskell PGPASSWORD=neohaskell TESTBED_HTTP_PORT=19550 TESTBED_OAUTH_PORT=19551 TESTBED_FIXTURE_PORT=19552 TESTBED_OAUTH_LOG_DIR=.agents-state/nh140-cell10 TESTBED_CABAL_BUILDDIR=.agents-state/nh140-cell10/testbed-build CABAL_BUILD_JOBS=1 nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict --timeout 1200 --transcript .agents-state/nh140-cell10/reconstruction.log
```

The strict runner also issues a signed token for a second user. That user cannot read or erase the owner's case even when it submits the known reference. The runner checks the owner response and active PostgreSQL key after denial. The private PostgreSQL command in the callback-death section below runs this assertion. A passing run prints `A valid other user could neither read nor erase the owner's protected case`.

The same strict command checks that a rejected result command records a callback dispatch failure while the application request stays pending. It reads the private PostgreSQL tombstone, then checks that the authorized `Erased` response contains only `tag` and null `contents` for the erased value. The maintained runner also checks that its application log contains neither the protected value nor its storage envelope. The second protected value remains `Available`, and an explicit retry records completion without restoring the first value. Set `TESTBED_OAUTH_LOG_DIR` to retain the application log in a chosen directory; otherwise the runner uses the system temporary directory. A passing run prints `Rejected result command was logged as callback dispatch failure without protected data` and ends with `testbed: PASS`.

The strict runner also kills its owned OAuth application while PostgreSQL is inserting the result callback's `ErasureCompleted` event. A private trigger pauses that insert after the key deletion commits. The runner checks that the key is tombstoned and the application request is still pending, kills the application, clears the trigger, and restarts it. The completed event must be absent after the crash. An explicit retry then records `AlreadyErased` without restoring the key. The private trigger is scoped to one case in the runner's fresh database and is removed before retry. To run this cell against a PostgreSQL instance already listening on private port 19440, use:

```sh
DB_HOST=127.0.0.1 DB_PORT=19440 DB_USER=neohaskell DB_PASSWORD=neohaskell TESTBED_HTTP_PORT=19450 TESTBED_OAUTH_PORT=19451 TESTBED_FIXTURE_PORT=19452 TESTBED_OAUTH_LOG_DIR=.agents-state/nh140-matrix TESTBED_CABAL_BUILDDIR=.agents-state/nh140-matrix/testbed-build CABAL_BUILD_JOBS=1 nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict --timeout 1200 --transcript .agents-state/nh140-matrix/callback-death-green.log
```

The strict runner also injects a typed failure into the selected erasure action before key deletion. It requires a `RunnerProcessingFailure` record, an owner-visible pending request, and one active key in PostgreSQL. It restarts the application, retries the request, and checks that the key is tombstoned and the completion event persists. A passing run prints `A typed action failure left the key active, then explicit retry erased it`. Use the same strict command above with a private PostgreSQL instance and private testbed ports.

To check that an integration command handler cannot return success without calling its response callback, run the focused dispatcher regression:

```sh
nix develop --command .agents/skills/tdd/scripts/test-match 'reports a handler that omits its callback' nhcore-test-integration
nix develop --command .agents/skills/tdd/scripts/test-match 'callback handler' nhcore-test-integration
```

The first focused run reports one example and zero failures. The second checks a handler exception and cancellation before its response callback. The private-port strict testbed command above also selects one application erasure action that deletes its key but emits no result command. The runner checks the exact outbound request log, the PostgreSQL tombstone, the absence of `ErasureCompleted`, and the owner's pending response. It restarts the application, retries the request, and requires `AlreadyErased` completion. A passing run prints `Omitted result callback left a tombstone and pending application request` and ends with `testbed: PASS`.

To run the persisted fault cases with the maintained testbed, reserve two more private ports and set `CRYPTOERASURE_FAULT_TESTS=1`:

```sh
DB_HOST=127.0.0.1 DB_PASSWORD=neohaskell TESTBED_HTTP_PORT=19150 TESTBED_OAUTH_PORT=19151 TESTBED_FIXTURE_PORT=19152 TESTBED_FAULT_HTTP_PORT=19153 TESTBED_FAULT_FIXTURE_PORT=19154 CRYPTOERASURE_FAULT_TESTS=1 CRYPTOERASURE_FAULT_REPORT_DIR=.agents-state/cryptoerasure-faults CRYPTOERASURE_REPLAY_REPORT_DIR=.agents-state/cryptoerasure-replay nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict
```

The fault checks create a fresh `nh_cryptoerasure_fault_` database for each case. They require an authorized `Available` read, then inject a missing key, malformed envelope, unavailable key table, a swap of two stored protected envelopes, or a copy of the second field's storage context to the first. All nine faults make the owner query return HTTP 500 without the protected value. An absent envelope or copied storage context makes replay report `failed` through `/ready` with HTTP 503.

The envelope swap keeps each field's reference and storage context in place. The context copy keeps both envelopes and references in place. Both cases preserve two active keys and save the event JSON before and after the mutation in the private report directory. The checks reject anonymous and invalid-token reads, inspect the page shape, and confirm that stored event JSON omits the Unicode value. The databases remain available for inspection after the run. With `CRYPTOERASURE_FAULT_REPORT_DIR` set as above, logs and database names are under `.agents-state/cryptoerasure-faults/`.

The same strict run seeds one owned case in a fresh PostgreSQL database, then restarts the OAuth application. A testbed query-store gate holds replay before the case reaches the in-memory projection and holds an authenticated HTTP read after it captures an empty page. The runner releases replay first and waits for `/ready` to report `ready`. The held request must return a generic HTTP 500, and a fresh owner request must return the protected `Available` case. Anonymous and invalid-token requests remain denied. The gate is active only when the runner sets `CRYPTOERASURE_REPLAY_GATE_DIR`. The replay report directory keeps the database name and application logs.

The same runner sends four owner-authorized erasure requests concurrently through one OAuth application instance. Each request records a copy of the first protected value before erasure. After the key commit, the result command appends that copy to the same entity, and the query projection reads it. The runner checks the persisted key tombstone and event, then the authorized `Erased` response. It also checks that the second value remains available and explicit retries report `AlreadyErased`. The restart checks read the query rebuilt from persisted events and compare the retained owner, case status, and protected state. These are example retention choices, not a policy for other applications.

The crash pause reads a case ID from the runner's local selector file. Public `RequestErasure` JSON has no pause setting. The runner writes the selector only for its crash case, removes it before restart, and checks that a normal owner command with an extra `pauseAfterCommit` field completes without pausing.

Run `nix develop --command cabal test nhtestbed:nhtestbed-erasure --test-show-details=direct` to check that another authenticated user and a userless no-OAuth transport cannot authorize the application-owned erasure request.

For the scoped PostgreSQL service fixture, create a fresh database owned by the configured test user and run:

```sh
private_db="nh_cryptoerasure_$(python3 -c 'import uuid; print(uuid.uuid4().hex[:12])')"
PGPASSWORD=neohaskell nix develop --command createdb -h 127.0.0.1 -U neohaskell "$private_db"
CRYPTOERASURE_TEST_DB="$private_db" nix develop --command .agents/skills/tdd/scripts/test-match 'erases only a matching application and stream' nhcore-test-service
CRYPTOERASURE_TEST_DB="$private_db" nix develop --command .agents/skills/tdd/scripts/test-match 'keeps a stale protected reference erased' nhcore-test-service
```

The second fixture uses independent PostgreSQL pools and concurrent tasks. It races deletion with an append and query update made from an earlier loaded wrapper. It then appends and updates another query row after confirmed erasure. It checks the persisted key tombstone, event count, and both query rows before reconnecting. The authorized response must render the first value as `Erased` while the second value and the example's retained fact survive.

### Run the maintained suite

The maintained runner builds and starts both testbed applications and the fake
external services. It requires a PostgreSQL user that can create databases.
Each run creates separate fresh databases for the two applications and prints
their names for inspection. The runner retains those databases after the run.
Ports 8080 and 8081 must be free.

```bash
# From the repository root, with PostgreSQL running locally.
nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict
```

The runner builds and starts both testbed applications and the fake external services. It requires a PostgreSQL user that can create databases. Each run creates separate fresh databases for the two applications and prints their names for inspection. It retains those databases, runs every Hurl file, and stops the processes it started. Ports 8080 and 8081 must be free. The `save-iban.hurl` scenario posts to `/commands/save-iban` and reads the saved IBAN through the cart summary query.

### Run the typed inbound example

`Testbed.Integrations.TypedInboundExample` uses `Integration.typedInbound` to
emit a cart command and a stock command from one worker. The cart Feature
declares both command targets in its inbound Slice, and the model registers the
worker. Both commands use `InternalTransport`. The Hurl scenario waits for new
records in the cart and stock queries.

The maintained suite runs `testbed/tests/integrations/typed-inbound.hurl`. To run
that scenario alone, start the application as shown in the SaveIBAN instructions
below. In another terminal, run:

```sh
nix develop --command hurl --test testbed/tests/integrations/typed-inbound.hurl
```

### Run the source-local Event Model example

`Testbed.Cart.Feature.cartFlow` and `Testbed.Stock.Feature.stockFlow` are exported Feature actions. `Testbed.EventModel.appEventModel` composes them with the Document, ExternalEffect, Erasable, and Owned Features in one named `eventModel do` value. The value also declares configuration, PostgreSQL, transport, API metadata, and file upload before the Features. `Launcher` starts it with `EventModel.run`. Every declared command, query, outbound handler, and typed inbound worker is registered by its Slice; there is no separate authored `App.hs` registration list.

The Owned Feature declares `MapErasedSecret` and its `ErasedCopyRecorded` event. The strict restart check observes the rebuilt query after a fresh process starts.

The cart Feature also declares three direct state-change rules and two direct state-view rules. The first command rule requests a typed quantity, makes it positive, uses it in `when AddItem`, and expects the same concrete quantity in its emitted `ItemAddedPayload`. The spec supplies a value of -3 and checks the normalized quantity of 8, ordered historical fixtures, emitted event, rejection reason, regex pattern, query actions, and full snapshot differences. Rule generation and isolated case execution belong to the separate generated-test delivery; the maintained Hurl scenario runs the cart and stock behavior now.

From the repository root, run the maintained public API assertions:

```sh
nix develop --command cabal test --project-file=cabal.project.dev nhtestbed-spec-demo --test-show-details=direct
```

The source-local Event Model spec asserts that `toApplication` contains every declared command, query, outbound handler, and inbound worker. It checks explicit query payload subscriptions, a query with two entity sources, the two distinct outbound handlers subscribed to `ExternalEffectRequestedPayload`, and each translation command's target entity. The compiler fixture runner accepts the complete testbed model and the remaining typed outcome forms. It rejects missing implementations, wrong ownership, invalid rule contexts, mixed command outcomes, and rules in translation Slices:

```sh
nix develop --command python3 core/test/Service/EventModel/check_compiler_rejections.py
```

The same spec verifies that the named model entry point rejects an inbound Slice that omits one typed worker output. It also checks that multiple assembled errors include their Feature, Chapter, and Slice paths before startup can start workers. The testbed retains both explicit inbound targets in `cartFlow`; removing either makes `appEventModel` return `Err`.

The maintained Hurl testbed exercises the declared cart and stock commands, queries, outbound handler, and typed inbound worker through the named model. `stock-reservation.hurl` asserts that `AddItem` updates `CartSummary` from one item to two and that the declared outbound handler runs `ReserveStock`, changing `StockLevel` from 100 available and 0 reserved to 95 available and 5 reserved after the first item. `generic-http-side-effect.hurl` requires one request from each distinct outbound handler for each event. Run it with:

`cart-stock-activity.hurl` checks the shared `CartStockActivity` projection. It observes cart creation and stock initialization in one row, then waits for the cart addition and stock reservation before checking that their undeclared payloads left the shared row unchanged. Run the maintained suite below, or run `nix develop --command hurl --test testbed/tests/scenarios/cart-stock-activity.hurl` while the testbed is running on the default HTTP port.

```sh
TESTBED_HTTP_PORT=18880 TESTBED_OAUTH_PORT=18881 TESTBED_FIXTURE_PORT=8181 nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict
```

### Run the public matcher example

From the repository root, run the Haskell test suite for the `Test` matchers:

```sh
nix develop --command cabal test --project-file=cabal.project.dev nhtestbed-spec-demo --test-show-details=direct
```

The suite checks `actual |> shouldContain expected` and `actual |> shouldNotContain expected` with arrays of different lengths. It needs no running app or PostgreSQL service. A passing run reports `2 examples, 0 failures`.

### Run the SaveIBAN scenario alone

```bash
# In one terminal, from the repository root, with PostgreSQL running locally.
nix develop --command cabal run exe:nhtestbed
```

```bash
# In another terminal, from the repository root.
nix develop --command hurl --test testbed/tests/commands/save-iban.hurl
```

### Run the S3 file upload example

`Testbed.Infrastructure.makeS3FileUploadConfig` builds a `FileUploadConfig S3Config` from `TestbedConfig`, and `Testbed.EventModel.s3AppEventModel` wires it through `EventModel.withFileUpload`. When `S3_ENDPOINT` is non-empty, `Launcher` starts `s3AppEventModel`; otherwise `Launcher` starts `appEventModel` with `LocalBlobStoreConfig`.

`testbed/scripts/fake_external_services.py` serves a path-style S3 bucket on `/testbed-s3-bucket/` and verifies AWS Signature Version 4 headers independently. `testbed/tests/s3/s3-file-upload.hurl` checks that unsigned requests return HTTP 403, uploads a binary PNG through `/files/upload`, verifies the signed `PUT` and `GET` counts on `/s3/observations`, downloads the exact bytes from `/files/{fileRef}`, and checks content deduplication.

The maintained runner executes both the default local upload scenarios and the S3 upload scenario automatically:

```bash
nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict
```

To run the S3 upload scenario alone with PostgreSQL running locally, start the fake services and the S3-configured testbed in two terminals, then run Hurl in a third terminal:

```bash
python3 testbed/scripts/fake_external_services.py
```

```bash
S3_ENDPOINT=http://127.0.0.1:8181 nix develop --command cabal run exe:nhtestbed
```

```bash
nix develop --command hurl --test testbed/tests/s3/s3-file-upload.hurl
```

### Check timestamped records

The `nhtestbed-datetime-schema` suite defines a `ShipmentTimeline` record with a required `DateTime` and an optional `Maybe DateTime`. It checks that both fields lower to JSON Schema strings and that a real shipment encodes its timestamp as a JSON string.

From the repository root, run:

```sh
nix develop --command cabal test nhtestbed:nhtestbed-datetime-schema --test-show-details=direct
```

The suite passes with `1 example, 0 failures`. Its source is `testbed/test-datetime-schema/Main.hs`.

## Project Structure

```text
testbed/
├── src/
│   ├── Cart/
│   │   ├── Core.hs              # Entity & Event definitions
│   │   ├── Commands/
│   │   │   ├── CreateCart.hs    # Command + decision logic
│   │   │   ├── AddItem.hs
│   │   │   └── RemoveItem.hs
│   │   └── Queries/             # Read model queries
│   ├── Order/                   # Another bounded context
│   │   ├── Core.hs
│   │   └── Commands/...
│   └── Main.hs                  # Service composition & startup
├── tests/
│   ├── commands/     # Command execution tests (Hurl)
│   ├── queries/      # Query tests (Hurl)
│   └── scenarios/    # End-to-end user workflows
├── scripts/
│   └── run-tests.sh  # Test runner
└── README.md         # You are here!
```

## Architecture

NeoHaskell applications are built using:

- **Entities & Events** - Core business objects and the events that change them
- **Commands** - Business operations with decision logic that generate events
- **Services** - Self-contained modules (Cart, Order, etc.) that work together

## What Gets Tested

All tests are run from outside the application using [Hurl](https://hurl.dev/), just like a real user would interact with it.

Each test file focuses on a specific area:

- **`tests/commands/`** - Command execution, event generation, business rules
- **`tests/queries/`** - Read model queries and projections
- **`tests/scenarios/`** - Complete user workflows across multiple bounded contexts

## Contributing

### Adding New Features to Test

When NeoHaskell adds a new feature, it should be demonstrated here:

1. **Implement** the feature in `src/` (as you would in a real app)
2. **Add tests** in `tests/` to verify it works correctly
3. **Update this README** if the feature changes how the testbed works

### Writing Good Tests

Our tests follow these principles:

- **User-focused** - Test what users actually do, not implementation details
- **Self-contained** - Each test should work independently
- **Clear failures** - When a test fails, it should be obvious what broke

Example of a good test:

```hurl
# Create a cart
POST http://localhost:8080/cart/commands/CreateCart
{}

HTTP/1.1 200
[Asserts]
jsonpath "$.events[0].type" == "CartCreated"
jsonpath "$.events[0].entityId" exists

# Capture the cart ID for next command
[Captures]
cartId: jsonpath "$.events[0].entityId"

# Add item to cart
POST http://localhost:8080/cart/commands/AddItem
{
  "cartId": "{{cartId}}",
  "productId": "550e8400-e29b-41d4-a716-446655440000",
  "amount": 2
}

HTTP/1.1 200
[Asserts]
jsonpath "$.events[0].type" == "ItemAdded"
```

## When Tests Fail

If tests are failing:

1. **Check the logs** - The app outputs helpful error messages
2. **Run tests individually** - Isolate which feature is broken
3. **Ask for help** - Jump in our [Discord](https://discord.gg/neohaskell) if you're stuck > 15 minutes

A failing test here means something broke in NeoHaskell itself, not your code.

## Why We Need This

The testbed ensures that:

- ✅ All documented features actually work
- ✅ The developer experience matches our promises
- ✅ Breaking changes are caught before release
- ✅ Examples in tutorials stay up-to-date

If it works in the testbed, it works in NeoHaskell.

## Questions?

- **"Can I use this as a template for my project?"** - Absolutely! Copy what you need.
- **"Why Hurl instead of traditional tests?"** - Hurl tests the actual user experience, not just the code.
- **"Do I need to run these locally?"** - No, CI runs them automatically. But it's helpful for development!

---

**Built with NeoHaskell** 💜

Need help? Join us on [Discord](https://discord.gg/neohaskell) or [open an issue](https://github.com/neohaskell/neohaskell/issues).
