# Counter

Run the application from this directory.

```sh
neo run
```

Open [the local API documentation](http://localhost:8080/docs) to create a counter, change its value and read the Counter view. Set `PORT` in the environment or in a local `.env` file to choose another port.

## Application code

`src/App.hs` configures the application. The Counter feature in `src/Starter/Counter/Feature.hs` connects commands, events and the query. Each command owns its decision. The entity owns its initial state, event list and update function. The query owns its access rules and projection.

Neo discovers modules under `src` and manages the Cabal manifest.

## Storage

The starter uses an in-memory event store. Its history lasts until the process stops. To keep new events in local JSON Lines files, change the store selection in `src/App.hs` to `withEventStore (jsonLines ".neo/events")`.

## Tests

Run `neo test` for the Haskell tests. To exercise the running server on its default port, run the Counter scenario from the development shell.

```sh
nix develop --command hurl --test tests/scenarios/counter-flow.hurl
```
