# Events and entities

An entity is the current state of one stream. Its event type records accepted changes. `EntityOf command` chooses the command's entity type. `EventOf entity` chooses that entity's event type. `EntityOf event` connects event routing to the entity.

## State reconstruction

`initialStateImpl` supplies the reducer's starting state. `updateImpl` has the shape `event -> entity -> entity`. The framework reconstructs an entity by applying its events in order. Keep the reducer pure and deterministic.

In this example, `CounterEntity` has `counterId`, `label`, and `value` fields. `CounterEvent` distinguishes creation, increment, and decrement. Each event payload carries the target entity identifier.

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

`getEventEntityIdImpl` routes an event to its entity stream. `EntityIdType` defaults to `Uuid`. Define a custom identifier type in the `Entity` instance when needed.

## Event changes

For a handwritten event sum, add the new constructor, its routing branch, and its reducer branch.

`deriveEvent` supplies serialization and stock instances that are missing. `deriveEntity` supplies missing entity plumbing, while application code still defines state transitions and event routing. Read [marker order](template-haskell-order.md) before using either marker.

For a new entity whose events are separate payload types, call `deriveEntityEvents ''Proposal [''ProposalCreated, ''PdfUploaded]` after declaring or importing both payloads and their `EntityOf` mappings. The marker generates `ProposalEvent`, one constructor per listed payload, `EventOf Proposal`, payload injection and extraction, routing through each payload's `Event` instance, and JSON instances for the parent event. Define the entity reducer and each payload's `Event` routing instance after the marker so the reducer can match the generated constructors. A command can pass one payload type directly to `Decider.acceptNew` or `Decider.acceptExisting`; use `event` around each payload in a mixed list. The marker does not read or evaluate an Event Model.

Generated parent events use tagged JSON even when the list has one payload. Adding another payload does not remove that tag from earlier events.

Existing stored events are inputs to future replays. A change to their JSON representation or reducer semantics affects reconstruction. Verify old histories when changing that contract.
