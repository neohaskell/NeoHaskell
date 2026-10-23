# Template Haskell order

Derivation markers inspect declarations already visible to Template Haskell. Place the application functions and custom instances a marker needs before that marker. Each marker owns a different contract.

## Events

`deriveEvent ''EventType` supplies missing `Show`, `Generic`, `Json.FromJSON`, and `Json.ToJSON` instances. Define a custom instance before the marker. Defining it afterwards can duplicate an instance the marker already generated.

## Entities

`deriveEntity ''EntityType ''EventType` supplies missing naming, JSON, type relationships, `Default`, `Entity`, and event routing. Define `initialState`, `update`, and `getEventEntityId` first when the marker must generate their corresponding instances. Existing instances take precedence. Conflicting entity and event mappings fail compilation.

You can define the instances explicitly instead. Both approaches still require application-owned transition and routing logic.

## Commands

Declare the command type, `getEntityId`, `decide`, and `EntityOf` before `command ''CommandType`. Declare `TransportsOf` for its allowed interfaces. Put a custom `canAccess` before the marker. The generator looks up those companion functions and validates their shapes.

`deriveCommand` is an alias for `command`. Apply either marker after its companion declarations.

## Queries

Declare the row type, serialization and schema instances, `canAccess`, and `canView` before `deriveQuery`. Put a custom `maxResults` before the marker too. Write manual `QueryOf` instances after the marker so the generated `Query` instance exists.

## Configuration and outbound handlers

`defineConfig` creates the config type itself from field declarations. Do not add a duplicate data declaration.

`outboundIntegration ''HandlerType` follows the handler type, its `EntityOf` equation, and `handleEvent`.
