# neohaskell-starter

An opinionated starter template for event-sourced [NeoHaskell](https://neohaskell.org) applications. Clone it, rename the `Counter` domain to whatever you're building, and you're off.

## What you get

- Event-sourced CQRS skeleton with a working **Counter** example — 3 events, 3 commands, 1 read model.
- Nix-managed toolchain: GHC, Cabal, HLS, fourmolu, hlint, hurl — pinned and reproducible.
- In-memory event store by default (zero setup). PostgreSQL is fully scaffolded and one uncomment away.
- HTTP transport on `:8080` with auto-generated routes (`POST /commands/kebab-case-name`).
- Hurl acceptance tests demonstrating the full create → increment → query flow.
- AI onramp: `AGENTS.md` + `.skills/` so coding assistants pick up the conventions immediately.

## Requirements

- [Determinate Nix](https://determinate.systems/nix) — the only thing you install yourself.
- [direnv](https://direnv.net/) (optional) — auto-loads the Nix shell when you `cd` into the directory.

Everything else (GHC, Cabal, HLS, fourmolu, hlint, hurl) comes from the Nix shell.

## Quickstart

```bash
# Clone and enter
git clone <your-fork-url> my-app && cd my-app

# Enter the dev shell. --accept-flake-config opts into the IOG + NeoHaskell
# binary caches (see "Binary caches" below) so GHC + deps are downloaded,
# not compiled. Takes a few minutes first time; seconds afterwards.
nix develop --accept-flake-config

# Build and run
cabal build
cabal run neohaskell-starter
```

In another terminal:

```bash
# Create a counter
curl -X POST http://localhost:8080/commands/create-counter \
  -H 'Content-Type: application/json' \
  -d '{"label": "downloads"}'
# → {"entityId": "…"}

# Increment it
curl -X POST http://localhost:8080/commands/increment-counter \
  -H 'Content-Type: application/json' \
  -d '{"entityId": "PASTE-UUID", "amount": 3}'

# Read the projection
curl http://localhost:8080/queries/counter-view
```

Or run the scripted end-to-end flow:

```bash
hurl tests/scenarios/counter-flow.hurl
```

## Binary caches

Building GHC + haskell.nix from source takes hours. `flake.nix` declares two public binary caches that ship prebuilt artifacts:

| Cache | Contents |
|---|---|
| `https://cache.iog.io` | haskell.nix / IOG artifacts (GHC, libraries) |
| `https://neohaskell.cachix.org` | NeoHaskell core + integrations |

Nix will only use these if you opt in. Pick one:

**Option A — per command (zero config).** Pass the flag on every `nix develop`:

```bash
nix develop --accept-flake-config
```

**Option B — accept once, forever.** Add `accept-flake-config = true` to your user Nix config:

```bash
mkdir -p ~/.config/nix
echo 'accept-flake-config = true' >> ~/.config/nix/nix.conf
```

After this, `nix develop` (no flag) transparently uses the caches for this flake and any other that declares `nixConfig.extra-substituters`. If you'd rather scope it tightly, you can also add yourself to `trusted-users` in `/etc/nix/nix.conf` instead — see the [NeoHaskell installation guide](https://neohaskell.org/getting-started/installation).

> If you skip both, Nix prompts you the first time and falls back to building from source if you decline. Expect a very long first build.

## Directory map

```
.
├── launcher/Launcher.hs          # Thin entry — buffers stdout/stderr, runs App
├── src/
│   ├── App.hs                    # Application wiring (event store, transports, services)
│   ├── Starter/Config.hs         # Typed config (port, upload dir, optional Postgres)
│   └── Starter/Counter/          # Example bounded context — rename or delete
│       ├── Core.hs               # Re-exports Entity and Event
│       ├── Entity.hs             # CounterEntity + update function
│       ├── Event.hs              # Sum type of all counter events
│       ├── Service.hs            # Registers commands with the framework
│       ├── Events/               # One file per event type
│       ├── Commands/             # One file per command type
│       └── Queries/              # One file per read model
├── tests/
│   ├── integration/smoke.hurl    # Server-is-up check
│   └── scenarios/counter-flow.hurl # End-to-end workflow
├── .skills/                       # AI assistant knowledge bases (domain-neutral)
├── AGENTS.md                      # Patterns + conventions for humans and AIs
├── cabal.project                  # Pins NeoHaskell at a specific commit
├── flake.nix                      # Nix shell definition
├── docker-compose.yml             # Postgres — only when you switch event stores
└── neohaskell-starter.cabal       # Package definition + extensions
```

## Rename the domain to your own

The starter's namespace is `Starter.Counter.*`. When you're building "Books" for a "Library" app, follow this 4-step checklist:

1. **Rename the directory.** `mv src/Starter src/Library`, then `mv src/Library/Counter src/Library/Book`.
2. **Fix the module names** in every `.hs` file under `src/Library/Book/` — change `module Starter.Counter.X` to `module Library.Book.X` and every `import Starter.Counter.X` to `import Library.Book.X`.
3. **Update `neohaskell-starter.cabal`** — rewrite the `exposed-modules` list, and change `Starter.Config` to `Library.Config`. Optionally rename the package itself (`name: library`, `executable library`).
4. **Update `src/App.hs`** — change `Starter.*` imports to `Library.*`.

Then:

```bash
cabal build
```

If it compiles, you're done.

## Add a new event

Say you want to record when a counter is reset to zero. Four steps:

1. **Create the event file** `src/Starter/Counter/Events/CounterReset.hs`:

   ```haskell
   module Starter.Counter.Events.CounterReset (Event (..)) where

   import Core
   import Json qualified

   data Event = Event { entityId :: Uuid }
     deriving (Generic, Show)

   instance Json.FromJSON Event
   instance Json.ToJSON Event
   ```

2. **Wire it into the sum type** in `src/Starter/Counter/Event.hs` — add a `CounterReset` variant and a branch in `getEventEntityId`.

3. **Handle it in `update`** in `src/Starter/Counter/Entity.hs`:

   ```haskell
   CounterReset _ ->
     entity { value = 0 }
   ```

4. **Expose the module** — add `Starter.Counter.Events.CounterReset` to `exposed-modules` in the `.cabal` file, then `cabal build`.

(Add a matching `Commands/ResetCounter.hs` and register it in `Service.hs` if you want an HTTP endpoint too.)

## Switch to Postgres

The starter uses an in-memory event store so it runs out of the box. When you're ready for durability:

1. Open `src/App.hs` and follow the `SWITCH TO POSTGRES` comment block.
2. Open `src/Starter/Config.hs` and uncomment the five Postgres fields.
3. Copy `.env.example` to `.env` (the defaults match `docker-compose.yml`).
4. Start Postgres: `docker compose up -d`.
5. Rebuild: `cabal build`. The event-store schema is created automatically on first run.

## Testing

```bash
# Start the server in one terminal
cabal run neohaskell-starter

# Run tests in another
hurl tests/integration/smoke.hurl
hurl tests/scenarios/counter-flow.hurl
```

Write new tests as `.hurl` files under `tests/commands/` (single-command) or `tests/scenarios/` (multi-step). Use `[Options] retry: 10 retry-interval: 200` on GETs to wait for projections.

## Formatting and linting

Inside the Nix shell:

```bash
fourmolu -i src/ launcher/       # Format
hlint src/ launcher/             # Lint
```

## Working with AI assistants

`AGENTS.md` at the root spells out the NeoHaskell conventions this project enforces — share it with any AI assistant you use (Claude Code, Codex, Cursor, etc.). Deeper reference material lives under `.skills/`.

## Links

- NeoHaskell docs: https://neohaskell.org
- Event Modeling: https://eventmodeling.org
- Hurl (tests): https://hurl.dev
