# Command Execution Flow Example

This example demonstrates how commands execute and register events into the event store in the NeoHaskell system, particularly in the context of the WebApi server provider.

## Basic Command Setup

```haskell
-- Define a simple command
data CreateCart = CreateCart
  { customerId :: Uuid,
    items :: Array CartItem
  }
  deriving (Generic, FromJSON, ToJSON)

instance Command CreateCart where
  type IsMultiTenant CreateCart = 'False
  type EntityIdType CreateCart = Uuid
  
  getEntityIdImpl _ = Just customerId
  
  decideImpl command _existingEntity = do
    cartId <- GenUuid
    let events = [CartCreatedEvent cartId (customerId command) (items command)]
    Accept InsertAfter events
```

## Web API Integration in Testbed

Looking at the actual testbed implementation:

```haskell
-- In testbed, the service definition uses WebApi server provider:
service :: Service _ _ _
service = 
  CartService.service

-- Which is defined as:
CartService.service :: Service _ _ _
CartService.service =
  Service.new
    |> Service.useServer WebApi.server  -- Uses the actual Web API server
    |> Service.command @AddItem
    |> Service.command @CreateCart
```

## How the WebApi Server Actually Works

The current WebApi server in testbed (as seen in Service.Api.WebApi.hs) provides:

1. **Request Routing**: HTTP requests to `/commands/<commandName>` are routed to the appropriate command handler
2. **Request Processing**: 
   - Parses request body into Bytes
   - Logs the command name and body (currently just prints to console)
   - Returns a dummy response instead of actually executing commands

3. **Command Handler Integration**:
```haskell
buildCommandHandler :: 
  forall command name.
  ( Command command,
    name ~ NameOf command,
    Record.KnownSymbol name
  ) =>
  WebApi ->
  Record.Proxy command ->
  ApiEndpointHandler
buildCommandHandler api _ body respond = do
  let port = api.port
  let n = GHC.symbolVal (Record.Proxy @name) |> Text.fromLinkedList
  Console.print [fmt|Running #{n} on port #{port}|]
  Console.print (body |> Text.fromBytes)  -- This just logs the body
  let response = [fmt|RESPONSE FROM #{n}
    BODY: #{body |> Text.fromBytes}
    |]
  respond (Text.toBytes response)  -- Returns dummy response
```

## What's Missing for Functional Command Execution

The current WebApi server is **not actually executing commands** - it's just logging the request and responding with dummy data. For truly functional command execution, the WebApi would need:

1. **Actual Command Execution**: Instead of just logging, it needs to:
   - Parse the command bytes using Aeson `FromJSON`
   - Route to actual `CommandHandler` infrastructure  
   - Execute the command through existing `CommandHandler.execute`

2. **Event Store Integration**: The actual execution should:
   - Use the existing `CommandHandler` to process commands
   - Persist generated events through `EventStore.insert`
   - Return proper success/failure responses

## Step-by-Step Implementation Guide for Junior Developers

### Step 1: Understand the Service Runtime Architecture
First, understand how `ServiceRuntime` works in the system:

```haskell
-- The ServiceRuntime contains the actual command execution infrastructure  
data ServiceRuntime commands = ServiceRuntime
  { commandHandlers :: Record commands  -- Maps command names to handlers
  , eventStore :: EventStore          -- The actual event store instance
  , entityFetcher :: EntityFetcher    -- For fetching entity state
  , loadStrategy :: LoadStrategy      -- For handling concurrency
  }
```

### Step 2: Modify the WebApi Server to Use Real Command Execution

In `Service.Api.WebApi.hs`, update the `buildCommandHandler` function:

```haskell
-- This is what needs to be changed in the WebApi server:
buildCommandHandler ::
  forall command name.
  ( Command command,
    name ~ NameOf command,
    Record.KnownSymbol name
  ) =>
  WebApi ->
  Record.Proxy command ->
  ApiEndpointHandler
buildCommandHandler api _ body respond = do
  -- Step 1: Parse the command bytes into actual command type
  let commandValue = body |> Text.fromBytes |> Aeson.decode
  
  case commandValue of
    Just cmd -> do
      -- Step 2: Execute using the actual ServiceRuntime infrastructure
      -- This would require access to the ServiceRuntime that's built by the service definition
      
      -- Step 3: Get the actual command handler and execute it
      result <- executeCommandWithEventStore cmd
      
      -- Step 4: Return proper response
      respond (Text.toBytes result)
      
    Nothing -> do
      -- Handle parsing error
      let errorResponse = "Invalid command format"
      respond (Text.toBytes errorResponse)
```

### Step 3: Create the Command Execution Function

```haskell
-- This function needs to be implemented to actually execute commands with event persistence:
executeCommandWithEventStore :: 
  forall command.
  ( Command command ) =>
  command ->
  Task Text Text  -- Returns the response as text
executeCommandWithEventStore command = do
  -- Get the actual ServiceRuntime (this requires access to the service context)
  runtime <- getCurrentServiceRuntime  -- This would need to be available in scope
  
  -- Use the existing command handler infrastructure
  result <- runtime.commandHandlers."CreateCart" command
  
  case result of
    CommandAccepted streamId eventsCount retries -> 
      pure [fmt|Command successful: #{eventsCount} events stored in stream #{streamId}|]
    CommandRejected reason -> 
      pure [fmt|Command rejected: #{reason}|]
    CommandFailed error retries -> 
      pure [fmt|Command failed: #{error}|]
```

### Step 4: Update the Service Definition to Provide Runtime Access

In `testbed/src/Testbed/Cart/Service.hs`:

```haskell
-- The service needs to be updated to properly initialize the runtime context:
service :: Service _ _ _
service =
  Service.new
    |> Service.useServer WebApi.server
    |> Service.command @AddItem
    |> Service.command @CreateCart
    -- This should also set up the actual runtime context that can execute commands
```

### Step 5: Implement Proper Event Store Integration

The key integration point is in the CommandHandler execution flow:

```haskell
-- In CommandHandler.execute, when events are generated:
-- 1. The decision produces events via AcceptCommand
-- 2. These events are passed to EventStore.insert 
-- 3. The event store persists the events to storage
-- 4. The CommandHandlerResult is returned to the caller

execute :: 
  forall command commandEntity commandEvent.
  ( Command command,
    commandEntity ~ EntityOf command,
    commandEvent ~ EventOf commandEntity
  ) =>
  EventStore commandEvent ->
  EntityFetcher commandEntity commandEvent ->
  EntityName ->
  command ->
  Task Text CommandHandlerResult
execute eventStore entityFetcher entityName command = do
  -- ... decision logic ...
  
  case commandResult of
    AcceptCommand _ events -> do
      -- This is where events get persisted:
      payload <- Event.payloadFromEvents entityName streamId events
      insertResult <- eventStore.insert payload |> Task.asResult
      
      case insertResult of
        Ok _success -> 
          CommandAccepted { streamId, eventsAppended = length events, retriesAttempted = 0 }
        Err _ -> 
          CommandFailed { error = "Event store error", retriesAttempted = 0 }
```

## Complete Functional Flow

1. HTTP request arrives at WebApi server on `/commands/CreateCart`
2. Request body is parsed into `CreateCart` command using Aeson
3. WebApi server routes to actual `CommandHandler.execute` through service runtime:
   - Uses the real `EventStore` instance from the service configuration  
   - Uses the real `EntityFetcher` for entity state
   - Executes the command through existing infrastructure
4. `CommandHandler.execute`:
   - Runs the decision logic via `decideImpl`
   - Generates events from the decision (CartCreatedEvent, etc.)
   - Calls `EventStore.insert` to persist events to storage
5. Response is returned with actual command result status (success/failure)

## The Integration Gap

The actual integration that's missing:
- The WebApi server has the infrastructure to route commands but doesn't wire up the actual command execution with event persistence
- The `buildCommandHandler` function just logs and returns dummy responses instead of:
  - Using the real `ServiceRuntime` to execute commands via the existing infrastructure
  - Actually calling `CommandHandler.execute` with proper event store integration
  - Handling the real command execution flow that registers events in the event store

## Implementation Checklist

✅ **WebApi server needs to parse JSON commands properly**  
✅ **WebApi server must route to actual ServiceRuntime command execution**  
✅ **CommandHandler.execute must be called with proper event store instance**  
✅ **Events must actually be persisted via EventStore.insert**  
✅ **Proper response handling with success/failure status**  

The testbed currently shows what the infrastructure *could* do, but the actual implementation is just a dummy server that logs requests rather than executing commands and persisting events to the event store.

## Key Files to Modify

1. **`core/service/Service/Api/WebApi.hs`** - Update `buildCommandHandler` to execute real commands
2. **`testbed/src/Testbed/Cart/Service.hs`** - Ensure proper service runtime configuration  
3. **`core/service/Service/CommandHandler/Core.hs`** - Verify it's called with proper event store
4. **Integration tests** - Update to test actual command execution flow

The implementation should make the WebApi server actually integrate with the existing command handler infrastructure rather than just logging and returning dummy responses.