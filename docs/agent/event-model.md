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
					updatedTo cartId CartSummary {cartSummaryId = cartId, ownerId = "owner", itemCount = 1, isEmpty = False, iban = Nothing}
```

## Compose the application Event Model

Import the Feature action into a named model module, then compose it as a bare action. Register shared configuration before the Features so each integration receives the loaded configuration at startup.

```haskell ignore
appEventModel = eventModel do
	appName "Cart"
	withConfig @AppConfig
	withEventStore makeEventStoreConfig
	withTransport WebTransport.server
	withHost "127.0.0.1"
	withPort 8080
	features do
		cartFlow

main = EventModel.run appEventModel |> Task.runOrPanic
```

Import `Service.EventModel` for these builders. Each Slice has one primary declaration. Use `entity` for one initiating `command`, `query` for a view, `outboundIntegration` for one payload trigger, or `inboundIntegration` for a worker. An inbound Slice lists every command and target entity pair in the worker's `InboundOutputsOf` type exactly once. `readsFrom` may repeat for each entity listed in the query's `EntitiesOf` type family. A query source lists each subscribed payload with `onEvent`.

State-change and state-view Slices accept zero or more direct `rule "..." do` children. Translation Slices reject rules at compilation. In a rule, `givenNothing` starts with no history. `given do` lists concrete historical events in order. `when` accepts the Slice's command or a payload subscribed by its query. Use `requestValue @Type` to bind a typed value for the rule runner to supply, then reuse it in later statements.

In an Hspec suite, import `Service.EventModel.RuleRunner`. Start with `emptyGenerators` and register each requested type through `withGenerator @Type gen render`. Pass the validated model, a base seed, and a positive sample count to `specFor`. It creates one named example per direct rule and runs each generated case against a fresh in-memory event store. View cases also get a fresh query projection. `runRules` returns `RuleReport` values when a test needs to inspect outcomes directly. Failed reports include the Feature, Chapter, Slice, rule, generated bindings, seed, expected and actual values, and framework errors. Translation Slices create no GWT examples. Keep integration tests for their own runtime behavior.

For a command, `therefore do` contains the complete ordered event list, `rejected`, `rejectedWith "reason"`, `rejectedMatches [regex|pattern|]`, or `acceptedWithNoEvents`. The regex searches within the reason unless you add `^` and `$` anchors. These alternatives cannot share one `therefore` block. For a view, use `updatedTo expectedRowId value`, `deleted @Query expectedRowId`, or `noOp @Query expectedRowId`. Supply the expected query row ID from the rule's authored or generated inputs. It may differ from the event entity ID. A view check compares that ID, the observed `QueryAction`, and the complete before and after snapshots for that query type. A `Delete` check requires the named row to be removed. `givenNothing` says only that the case has no prior events.

If the Feature module imports `Core`, use `import Core hiding (event)` before importing `Service.EventModel`. `Core.event` wraps a payload in an Event ADT; the rule's `event` records a concrete fixture or expected payload.

The compiler requires an `Entity` instance, a `Command` instance, and a `Query` instance for declarations that use them. It checks `EntityOf` for commands and payloads, `EntitiesOf` and `QueryOf` for query sources, and `EventVariantOf` for event payloads. Define the real behavior before adding its Slice. A missing implementation or ownership mismatch fails compilation. There is no topology-only declaration.

Generate an entity's Event ADT separately with `deriveEntityEvents`. Evaluating `eventModel` validates the assembled model without starting workers. `EventModel.toApplication` lowers a validated model to `Application`; `EventModel.run` rejects an `Err` before starting the application. The model registers every declared command, query, outbound handler, and typed inbound worker. A query subscribes only to its declared `readsFrom` entities and `onEvent` payloads. A query with multiple sources receives updates from each declared entity. Distinct outbound handler types may subscribe to the same payload and both run.

The model also accepts `withHost`, `withPort`, `withQueryObjectStore`, `withApiInfo`, `withFileUpload`, `withAuth`, `withAuthOverrides`, `withSecretStore`, `withCors`, `withHealthCheck`, `withoutHealthCheck`, `useReadinessEndpoint`, `withIntegration @Request`, `withOAuth2StateKey`, `withOAuth2Provider`, and `withDispatcherConfig`. Use the named builders for configuration. The Event Model exposes no arbitrary `Application -> Application` callback because it could register a command or bypass the duplicate `withConfig` check. Put `withQueryObjectStore` before query Features and `withOAuth2StateKey` before `withOAuth2Provider`. Attach a lifecycle worker to its outbound Slice with `outboundLifecycle`.

Direct `Application` assembly remains a supported legacy path. Its manual registration builders, including `withService`, `withQuery`, and `withOutbound`, are not deprecated and have no announced removal schedule. A model-backed executable needs no separate authored registration list.

Evaluate the `eventModel` result and handle an `Err` before startup. Validation reports every residual Slice failure with its Feature, Chapter, and Slice path. It rejects empty Features or Chapters, outbound Slices with the wrong number of triggers, reuse of one outbound handler type, and inbound Slices that omit a typed worker output. Two distinct outbound handler types may subscribe to one payload.
