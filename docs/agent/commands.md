# Commands

A command carries an intent for one target entity type. One `EntityOf` equation selects that type. One `decide` function returns its events or a rejection. A service registers commands whose entity and event types agree.

## Command declarations

The single tenant shape is below. Add imports and the `IncrementCounter` data declaration in your command module.

```haskell ignore
getEntityId :: IncrementCounter -> Maybe Uuid
getEntityId cmd = Just cmd.entityId

decide :: IncrementCounter -> Maybe CounterEntity -> RequestContext -> Decision CounterEvent

type instance EntityOf IncrementCounter = CounterEntity
type instance TransportsOf IncrementCounter = '[WebTransport]

command ''IncrementCounter
```

Define `getEntityId`, `decide`, and the type equations before `command`. Define a custom `canAccess` before the marker too. The marker derives the command instance and name from those declarations.

`getEntityId` returns `Just id` to load an existing target. For creation, return `Nothing` and generate an identifier with `Decider.generateUuid`, or derive a stable identifier from the request when duplicate submissions must address the same entity.

## Decisions and execution

`decide` receives the command, `Maybe entity`, and `RequestContext`. Reject invalid transitions with `Decider.reject`. Use `Decider.acceptNew` for a new stream and `Decider.acceptExisting` for an existing stream. The executor loads the target history, runs the decision, and inserts accepted events.

Do not use a read model as the authority for a command invariant. Queries update asynchronously. The command already receives its target's reconstructed state.

For a second entity or a remote effect, emit an event and react through an integration. A command dispatched by that integration is a separate operation. It does not share a transaction with the first command.

`IsMultiTenant command = True` adds a tenant `Uuid` argument to both routing and decision functions. Authorization rules remain explicit.

Register a command with `Service.command @CommandType`, then register the service with `Application.withService`.
