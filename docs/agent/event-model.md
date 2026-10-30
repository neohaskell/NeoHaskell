# Source-local Event Models

An Event Model groups executable application behavior as Feature, Chapter, and Slice. Export a Feature action from the module that owns the behavior. Keep Chapters and Slices inline. Compose Feature actions in one named `eventModel do` value with one `appName`.

## Declare a Feature

This fragment belongs in a Feature module. It assumes that the named entity, command, payload, and query already have implementations.

```haskell ignore
cartFlow :: FeatureAction
cartFlow = feature "Cart flow" do
	chapter "Manage carts" do
		slice "Add an item" do
			entity @CartEntity do
				command @AddItem do
					produces @ItemAddedPayload

		slice "Show cart summary" do
			query @CartSummary do
				readsFrom @CartEntity do
					onEvent @ItemAddedPayload
```

## Compose the application Event Model

Import the Feature action into `src/App.hs`, then compose it as a bare action.

```haskell ignore
appEventModel = eventModel do
	appName "Cart"
	features do
		cartFlow
```

Import `Service.EventModel` for these builders. Each Slice has one primary declaration. Use `entity` for one initiating `command`, `query` for a view, `outboundIntegration` for one payload trigger, or `inboundIntegration` for a worker. `readsFrom` may repeat for each entity listed in the query's `EntitiesOf` type family. A query source lists each subscribed payload with `onEvent`.

The compiler requires an `Entity` instance, a `Command` instance, and a `Query` instance for declarations that use them. It checks `EntityOf` for commands and payloads, `EntitiesOf` and `QueryOf` for query sources, and `EventVariantOf` for event payloads. Define the real behavior before adding its Slice. A missing implementation or ownership mismatch fails compilation. There is no topology-only declaration.

Generate an entity's Event ADT separately with `deriveEntityEvents`. Evaluating `eventModel` does not generate types or start workers. Until Event Model application wiring is available, register the same implemented services, queries, and integrations in `Application` as described in [configuration and wiring](configuration-and-wiring.md). Evaluate the `eventModel` result and handle an `Err` before startup. It reports residual shape errors, including an empty Feature or Chapter and an outbound Slice with the wrong number of triggers.
