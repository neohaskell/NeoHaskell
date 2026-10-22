# Template Haskell order

Derivation markers inspect declarations already visible to Template Haskell. Place the application functions and custom instances a marker needs before that marker. Each marker owns a different contract.

## Events

`deriveEvent ''EventType` supplies missing `Show`, `Generic`, `Json.FromJSON`, and `Json.ToJSON` instances. Define a custom instance before the marker. Defining it afterwards can duplicate an instance the marker already generated. See event derivation in `core/service/Service/Event/TH.hs`.

## Entities

`deriveEntity ''EntityType ''EventType` supplies missing naming, JSON, type relationships, `Default`, `Entity`, and event routing. Define `initialState`, `update`, and `getEventEntityId` first when the marker must generate their corresponding instances. Existing instances take precedence. Conflicting entity and event mappings fail compilation. See entity derivation in `core/service/Service/Entity/TH.hs`.

The starter entity in `neo/starter/src/Starter/Counter/Entity.hs` uses explicit instances. Both approaches still require application-owned transition and routing logic.

## Commands

Declare the command type, `getEntityId`, `decide`, and `EntityOf` before `command ''CommandType`. Declare `TransportsOf` for its allowed interfaces. Put a custom `canAccess` before the marker. The command generator in `core/service/Service/CommandExecutor/TH.hs` looks up those companion functions and validates their shapes.

The starter increment module in `neo/starter/src/Starter/Counter/Commands/IncrementCounter.hs` is the maintained ordering example. `deriveCommand` is an alias for `command` in the same generator in `core/service/Service/CommandExecutor/TH.hs`.

## Queries

Declare the row type, serialization and schema instances, `canAccess`, and `canView` before `deriveQuery`. Put a custom `maxResults` before the marker too. Write manual `QueryOf` instances after the marker so the generated `Query` instance exists. See query derivation in `core/service/Service/Query/TH.hs` and CounterView in `neo/starter/src/Starter/Counter/Queries/CounterView.hs`.

## Configuration and outbound handlers

`defineConfig` creates the config type itself from field declarations. Do not add a duplicate data declaration. See configuration derivation in `core/config/Config/TH.hs` and StarterConfig in `neo/starter/src/Starter/Config.hs`.

`outboundIntegration ''HandlerType` follows the handler type, its `EntityOf` equation, and `handleEvent`. The Cart handler in `testbed/src/Testbed/Cart/Integrations/ReserveStockOnItemAdded.hs` and outbound generator in `core/service/Service/OutboundIntegration/TH.hs` show that contract.
