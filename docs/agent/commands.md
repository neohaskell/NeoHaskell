# Commands

A command carries an intent for one target entity type. One `EntityOf` equation selects that type. One `decide` function returns its events or a rejection. A service registers commands whose entity and event types agree. See Command in `core/service/Service/Command/Core.hs` and service registration constraints in `core/service/Service/ServiceDefinition/Core.hs`.

## Command declarations

The single tenant shape is below. This fragment uses the starter increment command in `neo/starter/src/Starter/Counter/Commands/IncrementCounter.hs` and needs its imports and data declaration.

```haskell ignore
getEntityId :: IncrementCounter -> Maybe Uuid
getEntityId cmd = Just cmd.entityId

decide :: IncrementCounter -> Maybe CounterEntity -> RequestContext -> Decision CounterEvent

type instance EntityOf IncrementCounter = CounterEntity
type instance TransportsOf IncrementCounter = '[WebTransport]

command ''IncrementCounter
```

Define `getEntityId`, `decide`, and the type equations before `command`. Define a custom `canAccess` before the marker too. The command marker in `core/service/Service/CommandExecutor/TH.hs` derives the command instance and naming from those declarations.

`getEntityId` returns `Just id` to load an existing target. The create command in `neo/starter/src/Starter/Counter/Commands/CreateCounter.hs` returns `Nothing` and generates a new identifier through `Decider.generateUuid`. Creation can also use a deterministic identifier, as RegisterCartByKey in `testbed/src/Testbed/Cart/Commands/RegisterCartByKey.hs` demonstrates.

## Decisions and execution

`decide` receives the command, `Maybe entity`, and `RequestContext`. Reject invalid transitions with `Decider.reject`. Use `Decider.acceptNew` for a new stream and `Decider.acceptExisting` for an existing stream. The decider in `core/service/Decider.hs` owns these constructors and the executor in `core/service/Service/CommandExecutor/Core.hs` owns loading, decision execution, and event insertion.

Do not use a read model as the authority for a command invariant. Queries update asynchronously. The command already receives its target's reconstructed state. See query subscription in `core/service/Service/Query/Subscriber.hs`.

For a second entity or a remote effect, emit an event and react through an integration. The Cart-to-Stock handler in `testbed/src/Testbed/Cart/Integrations/ReserveStockOnItemAdded.hs` emits a separate command. That sequence does not establish a transaction spanning both commands.

`IsMultiTenant command = True` adds a tenant `Uuid` argument to both routing and decision functions. The signature families in `core/service/Service/Command/Core.hs` define the exact shapes. Authorization rules remain explicit. See [authorization](authorization.md).

Register a command with `Service.command @CommandType`, then register the service with `Application.withService`. See the Counter service in `neo/starter/src/Starter/Counter/Service.hs` and application in `neo/starter/src/App.hs`.
