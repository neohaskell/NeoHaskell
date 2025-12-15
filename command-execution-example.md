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

## Complete Functional Flow

1. HTTP request arrives at WebApi server on `/commands/CreateCart`
2. Request body is parsed into `CreateCart` command
3. WebApi server routes to `CommandHandler.execute` with:
   - The actual `EventStore` instance  
   - The `EntityFetcher` for entity state
   - The command to execute
4. `CommandHandler.execute`:
   - Runs the decision logic via `decideImpl`
   - Generates events from the decision
   - Calls `EventStore.insert` to persist events to storage
5. Response is returned with actual command result status

## The Integration Gap

The actual integration that's missing:
- The WebApi server has the infrastructure to route commands but doesn't wire up the actual command execution with event persistence
- The `buildCommandHandler` function just logs and returns dummy responses instead of:
  - Using the real `ServiceRuntime` to execute commands
  - Actually calling `CommandHandler.execute` with proper event store integration
  - Handling the real command execution flow that registers events in the event store

The testbed currently shows what the infrastructure *could* do, but the actual implementation is just a dummy server that logs requests rather than executing commands and persisting events to the event store.