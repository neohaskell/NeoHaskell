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
			rule "Adding an item records its quantity" do
				cartId <- requestValue @Uuid
				stockId <- requestValue @Uuid
				quantitySeed <- requestValue @Int
				let quantity = modBy 10 quantitySeed + 1
				given do
					event CartCreatedPayload {cartCreatedEntityId = cartId, cartCreatedOwnerId = "owner"}
				when AddItem {cartId, stockId, quantity}
				therefore do
					event ItemAddedPayload {itemAddedEntityId = cartId, itemAddedStockId = stockId, itemAddedQuantity = quantity}

		slice "Show cart summary" do
			query @CartSummary do
				readsFrom @CartEntity do
					onEvent @CartCreatedPayload
					onEvent @ItemAddedPayload
			rule "An item changes the summary" do
				cartId <- requestValue @Uuid
				stockId <- requestValue @Uuid
				given do
					event CartCreatedPayload {cartCreatedEntityId = cartId, cartCreatedOwnerId = "owner"}
				when ItemAddedPayload {itemAddedEntityId = cartId, itemAddedStockId = stockId, itemAddedQuantity = 1}
				therefore do
					updatedTo CartSummary {cartSummaryId = cartId, ownerId = "owner", itemCount = 1, isEmpty = False, iban = Nothing}
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

State-change and state-view Slices accept zero or more direct `rule "..." do` children. Translation Slices reject rules at compilation. In a rule, `givenNothing` starts with no history. `given do` lists concrete historical events in order. `when` accepts the Slice's command or a payload subscribed by its query. Use `requestValue @Type` to bind a typed value for the rule runner to supply, then reuse it in later statements. The Event Model does not choose a generator or run generated cases yet. Keep application tests for the declared behavior.

For a command, `therefore do` contains the complete ordered event list, `rejected`, `rejectedWith "reason"`, `rejectedMatches [regex|pattern|]`, or `acceptedWithNoEvents`. The regex searches within the reason unless you add `^` and `$` anchors. These alternatives cannot share one `therefore` block. For a view, use `updatedTo value`, `deleted @Query`, or `noOp @Query`. A view check compares the observed `QueryAction` and the complete before and after snapshots for that query type. A `Delete` check requires a row to be removed. `givenNothing` says only that the case has no prior events.

If the Feature module imports `Core`, use `import Core hiding (event)` before importing `Service.EventModel`. `Core.event` wraps a payload in an Event ADT; the rule's `event` records a concrete fixture or expected payload.

The compiler requires an `Entity` instance, a `Command` instance, and a `Query` instance for declarations that use them. It checks `EntityOf` for commands and payloads, `EntitiesOf` and `QueryOf` for query sources, and `EventVariantOf` for event payloads. Define the real behavior before adding its Slice. A missing implementation or ownership mismatch fails compilation. There is no topology-only declaration.

Generate an entity's Event ADT separately with `deriveEntityEvents`. Evaluating `eventModel` does not generate types or start workers. Until Event Model application wiring is available, register the same implemented services, queries, and integrations in `Application` as described in [configuration and wiring](configuration-and-wiring.md). Evaluate the `eventModel` result and handle an `Err` before startup. It reports residual shape errors, including an empty Feature or Chapter and an outbound Slice with the wrong number of triggers.
