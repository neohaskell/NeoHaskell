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

### Protected PostgreSQL round trip

From the repository root, start PostgreSQL with the testbed defaults (`neohaskell` database and user, password `neohaskell`, port `5432`). Then run these commands in separate terminals:

```bash
nix develop --command cabal run exe:nhtestbed
```

```bash
nix develop --command hurl --test testbed/tests/scenarios/cryptoerasure-m1.hurl
POSTGRES_AVAILABLE=true nix develop --command .agents/skills/tdd/scripts/test-match 'Erasable key store' nhcore-test-service
```

The Hurl scenario asserts a command-generated event, a reconstructed query, two independently protected values, a mapped copy, nested optional values, and Unicode `Available` content. The service fixture uses real PostgreSQL to assert encrypted event storage, restart, key/event rollback, and a private tombstone yielding `Erased`. Erasure is deliberately not exposed as an application command or HTTP success response in this slice.

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

### Run the SaveIBAN scenario alone

```bash
# In one terminal, from the repository root, with PostgreSQL running locally.
nix develop --command cabal run exe:nhtestbed
```

```bash
# In another terminal, from the repository root.
nix develop --command hurl --test testbed/tests/commands/save-iban.hurl
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
