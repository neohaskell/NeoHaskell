# Events and entities

An entity is the current state of one stream. Its event type records accepted changes. `EntityOf command` chooses the command's entity type. `EventOf entity` chooses that entity's event type. `EntityOf event` connects event routing to the entity. See entity classes and families in `core/service/Service/Entity/Core.hs`.

## State reconstruction

`initialStateImpl` supplies the reducer's starting state. `updateImpl` has the shape `event -> entity -> entity`. The framework reconstructs an entity by applying its events in order. Keep the reducer pure and deterministic. See Entity in `core/service/Service/Entity/Core.hs` and the entity fetcher in `core/service/Service/EntityFetcher/Core.hs`.

The Counter entity in `neo/starter/src/Starter/Counter/Entity.hs` owns `counterId`, `label`, and `value`. The Counter event sum in `neo/starter/src/Starter/Counter/Event.hs` distinguishes creation, increment, and decrement. Each event payload carries the target entity identifier.

This fragment describes the reducer shape. It needs the entity and event declarations.

```haskell ignore
update :: CounterEvent -> CounterEntity -> CounterEntity
update event entity = case event of
	CounterIncremented payload ->
		entity {value = entity.value + payload.amount}
	CounterDecremented payload ->
		entity {value = entity.value - payload.amount}
	CounterCreated payload ->
		CounterEntity {counterId = payload.entityId, label = payload.label, value = 0}
```

`getEventEntityIdImpl` routes an event to its entity stream. `EntityIdType` defaults to `Uuid`. A custom identifier type belongs in the `Entity` instance. The entity contract in `core/service/Service/Entity/Core.hs` owns both rules.

## Event changes

For a new event, add its payload, add a constructor to the event sum, add its routing branch, and add its reducer branch. The starter payload modules in `neo/starter/src/Starter/Counter/Events` and event sum in `neo/starter/src/Starter/Counter/Event.hs` show this arrangement.

`deriveEvent` supplies serialization and stock instances that are missing. `deriveEntity` supplies missing entity plumbing, while application code still defines state transitions and event routing. Read [marker order](template-haskell-order.md) before using either marker.

Existing stored events are inputs to future replays. A change to their JSON representation or reducer semantics affects reconstruction. The event store contract in `core/service/Service/EventStore/Core.hs` and fetcher in `core/service/Service/EntityFetcher/Core.hs` show the read and replay path. Verify old histories when changing that contract.
