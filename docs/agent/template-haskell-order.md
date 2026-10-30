# Template Haskell order

Derivation markers inspect declarations already visible to Template Haskell. Place the application functions and custom instances a marker needs before that marker. Each marker owns a different contract.

## Events

`deriveEvent ''EventType` supplies missing `Show`, `Generic`, `Json.FromJSON`, and `Json.ToJSON` instances. Define a custom instance before the marker. Defining it afterwards can duplicate an instance the marker already generated.

`deriveEntityEvents ''EntityType [''FirstPayload, ''SecondPayload]` creates a new parent event sum from the listed payload types. Declare or import the payload types and map each with `EntityOf` before the marker. Define the entity reducer and each payload's `Event` routing instance after the marker. Define any custom payload JSON instances before `deriveEvent` on those payloads. The generated parent receives ordinary JSON instances and cannot replace a handwritten stored event sum without checking historical event bytes.

## Entities

`deriveEntity ''EntityType ''EventType` supplies missing naming, JSON, type relationships, `Default`, `Entity`, and event routing. Define `initialState`, `update`, and `getEventEntityId` first when the marker must generate their corresponding instances. Existing instances take precedence. Conflicting entity and event mappings fail compilation.

You can define the instances explicitly instead. Both approaches still require application-owned transition and routing logic.

## Commands

Declare the command type, `getEntityId`, `decide`, and `EntityOf` before `command ''CommandType`. Declare `TransportsOf` for its allowed interfaces. Put a custom `canAccess` before the marker. The generator looks up those companion functions and validates their shapes.

`deriveCommand` is an alias for `command`. Apply either marker after its companion declarations.

## Queries

Declare the row type, `canAccess`, and `canView` before `deriveQuery`. The marker supplies missing `Show`, `Generic`, `Json.FromJSON`, `Json.ToJSON`, and `ToSchema` instances. Define custom instances and a custom `maxResults` before the marker. Write manual `QueryOf` instances after the marker so the generated `Query` instance exists.

## Configuration and outbound handlers

`defineConfig` creates the config type itself from field declarations. Do not add a duplicate data declaration.

`outboundIntegration ''HandlerType` follows the handler type, its `EntityOf` equation, a visible `EventVariantOf` mapping, and `handleEvent :: Entity -> Payload -> Integration.Outbound`. The marker infers the payload from that signature. For an existing handler that still accepts the full event type, add `import Service.OutboundIntegration.TH (outboundIntegrationFullEvent)` to its module and use `outboundIntegrationFullEvent`.

`inboundIntegration ''HandlerType` follows the handler type and `workerImpl :: Config -> Integration.TypedInbound Outputs` in the same module. The marker records the config type and the complete command and target-entity output list. List each command's actual `EntityOf` target. Register the handler with `Application.withInboundHandler @HandlerType`.
