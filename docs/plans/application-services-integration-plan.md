# Application Services Integration Plan

## Goal

Integrate Services into Application so that:
1. Services share a single `EventStore Json.Value` created by the Application
2. Query subscriber processes events and updates read models
3. Application orchestrates the startup sequence

## Current State

- `Application` holds only a `QueryRegistry`
- `Service` creates its own `EventStore` via `EventStoreConfig`
- Services run independently via `__internal_runServiceMain`

## Design Decision

Rather than deeply refactoring the existing `Service` type (which has complex type-level machinery), we'll:
1. Create a `ServiceRunner` that wraps a service's run function
2. Application receives `ServiceRunner`s and an `EventStore Json.Value`
3. Application's `run` function orchestrates startup

## Test Specification

### ApplicationSpec.hs - Extended Tests

```haskell
describe "Service.Application" do
  -- Existing tests remain...

  describe "withServiceRunner" do
    it "adds a service runner to the application" \_ -> do
      let runner = ServiceRunner
            { runWithEventStore = \_ -> Task.yield unit
            }
      let app = Application.new
            |> Application.withServiceRunner runner
      Application.hasServiceRunners app |> shouldBe True

    it "accumulates multiple service runners" \_ -> do
      let runner1 = ServiceRunner { runWithEventStore = \_ -> Task.yield unit }
      let runner2 = ServiceRunner { runWithEventStore = \_ -> Task.yield unit }
      let app = Application.new
            |> Application.withServiceRunner runner1
            |> Application.withServiceRunner runner2
      Application.serviceRunnerCount app |> shouldBe 2

  describe "run" do
    it "creates event store and passes it to service runners" \_ -> do
      -- Track whether service runner was called with event store
      calledRef <- ConcurrentVar.containing False

      let runner = ServiceRunner
            { runWithEventStore = \eventStore -> do
                -- Verify we received a working event store
                -- by inserting an event
                ConcurrentVar.set True calledRef
                Task.yield unit
            }

      eventStore <- InMemory.new |> Task.mapError toText

      let app = Application.new
            |> Application.withServiceRunner runner

      Application.runWith eventStore app

      called <- ConcurrentVar.read calledRef
      called |> shouldBe True

    it "rebuilds queries before starting services" \_ -> do
      -- Create event store with existing events
      eventStore <- InMemory.new |> Task.mapError toText

      -- Insert an event BEFORE running the app
      insertTestEvent eventStore

      -- Track query rebuild
      rebuildCalled <- ConcurrentVar.containing False

      let updater = QueryUpdater
            { queryName = "TestQuery",
              updateQuery = \_ -> do
                ConcurrentVar.set True rebuildCalled
                Task.yield unit
            }

      let registry = Registry.empty
            |> Registry.register (EntityName "TestEntity") updater

      let app = Application.new
            |> Application.withQueryRegistry registry

      Application.runWith eventStore app

      rebuilt <- ConcurrentVar.read rebuildCalled
      rebuilt |> shouldBe True

    it "starts query subscriber after rebuild" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText

      -- Track events processed by subscriber (live events after startup)
      liveEventCount <- ConcurrentVar.containing 0

      let updater = QueryUpdater
            { queryName = "TestQuery",
              updateQuery = \_ -> do
                ConcurrentVar.modify (\n -> n + 1) liveEventCount
                Task.yield unit
            }

      let registry = Registry.empty
            |> Registry.register (EntityName "TestEntity") updater

      let app = Application.new
            |> Application.withQueryRegistry registry

      -- Run app in background
      Application.runWithAsync eventStore app

      -- Give subscriber time to start
      AsyncTask.sleep 50

      -- Insert event AFTER app started
      insertTestEvent eventStore

      -- Give subscriber time to process
      AsyncTask.sleep 50

      count <- ConcurrentVar.read liveEventCount
      count |> shouldBe 1
```

## Implementation Steps

### Step 1: Add ServiceRunner type to Application

```haskell
-- | A function that runs a service given a shared EventStore.
data ServiceRunner = ServiceRunner
  { runWithEventStore :: EventStore Json.Value -> Task Text Unit
  }
```

### Step 2: Extend Application data type

```haskell
data Application = Application
  { queryRegistry :: QueryRegistry,
    serviceRunners :: Array ServiceRunner
  }
```

### Step 3: Add builder functions

```haskell
withServiceRunner :: ServiceRunner -> Application -> Application
withServiceRunner runner app =
  app { serviceRunners = app.serviceRunners |> Array.push runner }

hasServiceRunners :: Application -> Bool
hasServiceRunners app = not (Array.isEmpty app.serviceRunners)

serviceRunnerCount :: Application -> Int
serviceRunnerCount app = Array.length app.serviceRunners
```

### Step 4: Add run functions

```haskell
-- | Run application with a provided EventStore (for testing)
runWith :: EventStore Json.Value -> Application -> Task Text Unit
runWith eventStore app = do
  -- 1. Create query subscriber
  subscriber <- Subscriber.new eventStore app.queryRegistry

  -- 2. Rebuild all queries from historical events
  Subscriber.rebuildAll subscriber

  -- 3. Start live subscription
  Subscriber.start subscriber

  -- 4. Run all services with shared event store
  app.serviceRunners
    |> Task.forEach \runner ->
        runner.runWithEventStore eventStore

-- | Run application with provided EventStore, non-blocking
runWithAsync :: EventStore Json.Value -> Application -> Task Text Unit
runWithAsync eventStore app = do
  AsyncTask.run (runWith eventStore app)
    |> Task.mapError toText
  Task.yield unit
```

## Integration with Existing Service Type

To integrate existing Services:

```haskell
-- Helper to convert a Service to a ServiceRunner
-- (This would live in Service.ServiceDefinition or a new module)
toServiceRunner ::
  forall cmds ... .
  (constraints...) =>
  Service cmds ... ->
  ServiceRunner
toServiceRunner service = ServiceRunner
  { runWithEventStore = \rawEventStore -> do
      let eventStore = rawEventStore |> EventStore.castEventStore @event
      -- ... run service logic with shared eventStore
  }
```

This is a larger refactor of `runService` that we'll tackle separately.

## Verification Checklist

- [ ] `withServiceRunner` adds runners to application
- [ ] `hasServiceRunners` returns correct status
- [ ] `serviceRunnerCount` returns correct count
- [ ] `runWith` passes EventStore to service runners
- [ ] `runWith` rebuilds queries before starting services
- [ ] `runWithAsync` starts subscriber for live events
- [ ] `isEmpty` updated to consider service runners
