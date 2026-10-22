## Problem

Root commands already carry `RequestContext`, while `Event.payloadFromEvents` receives no context and `EventMetadata.new` writes three null lineage fields. Event runners still have their triggering `Event` when dispatching callback commands, but `dispatchCommand` currently creates a fresh trusted context with no source distinction. The design must connect these existing boundaries without turning historical attribution into verified identity or authority. The source flow is in `core/service/Service/Auth.hs`, `core/service/Service/Event.hs`, `core/service/Service/Event/EventMetadata.hs`, `core/service/Service/CommandExecutor/Core.hs`, and `core/service/Service/Integration/Dispatcher.hs`.

## Usage (caller's view)

Root command callers keep the existing `decide` signature. The normal context constructor establishes root attribution, and the executor passes its lineage into event construction.

```haskell
context <- Auth.authenticatedContext claims
Executor.execute store fetcher entityName context command

payload <- Event.payloadFromEvents context.lineage entityName streamId events
```

An event callback supplies its source at the dispatcher boundary. The dispatcher derives lineage, then creates a trusted context with a fresh operation ID.

```haskell
dispatchCommand (EventTriggeredBy source.metadata) endpoints payload
```

An autonomous worker names its origin explicitly. It receives a new root correlation and no actor or cause.

```haskell
dispatchCommand Autonomous endpoints payload
```

## Shape

Add `Service.Event.Lineage` as the owner of immutable attribution. `RequestContext.lineage` uses this type, separate from `RequestContext.user`. Its public constructor stays hidden. `empty` supports contextless event fixtures. `root` uses a request ID and optional verified subject. `fromSource` accepts the source event ID, stored correlation and stored actor. Accessors expose only the three persisted values.

```haskell
data EventLineage = EventLineage
  { correlationId :: Maybe Text
  , causationId :: Maybe Text
  , relatedUserSub :: Maybe Text
  }

empty :: EventLineage
root :: Uuid -> Maybe Text -> EventLineage
fromSource :: Uuid -> Maybe Text -> Maybe Text -> EventLineage

-- Service.Auth
RequestContext.lineage :: EventLineage
withLineage :: EventLineage -> RequestContext -> RequestContext

-- Service.Event
payloadFromEvents :: EventLineage -> EntityName -> StreamId -> Array eventType
                  -> Task err (InsertionPayload eventType)
```

`root` maps a non-nil `requestId` to the root `correlationId`; it maps `Uuid.nil` to empty lineage. The root `requestId` is the current operation ID. Its value starts the correlation chain, but the meanings remain distinct. A callback gets a new `requestId` while retaining the source correlation. The web transport generates root IDs and ignores any inbound correlation header or command field. Normal authenticated roots copy only validated `claims.sub` into `relatedUserSub`. Anonymous and trusted roots use no subject.

`fromSource` copies a non-nil stored correlation. If it is absent or the nil UUID, it uses the source event's `metadata.eventId` as the correlation. It always uses that same event ID as immediate `causationId`, and copies its `relatedUserSub`. Thus an old event without correlation gets a deterministic chain rooted at its own persisted event ID, including on redelivery. A nil source event ID normalizes correlation and cause to absent. Historical metadata is read as stored and never rewritten.

`Event.payloadFromEvents` applies one lineage value to each insertion's metadata. Multi-event outputs are siblings with the same correlation and immediate cause. Their generated `eventId` values and `createdAt` values retain current behavior. Each conflict retry reuses the captured request context, so lineage stays stable while existing event ID generation and retry behavior remain unchanged.

A dispatcher-owned `DispatchOrigin = Autonomous | EventTriggeredBy EventMetadata` distinguishes event callbacks from inbound or OAuth work. Stateless and lifecycle runners pass their source metadata. Autonomous callers pass `Autonomous`. Both paths still use `Auth.trustedContext`; only the event path overlays lineage. Neither path serializes lineage into `CommandPayload.commandData`. The dispatcher retry loop remains unchanged, and command dispatch remains outside it.

`relatedUserSub`, `correlationId` and `causationId` can enter structured log scope from the in-memory context. Do not log email, tokens or other claims. No log path reads event ancestry from storage. This follows `principle-model-the-domain` by encoding root, empty and source-derived attribution in one value. It follows `principle-security-by-construction` because root subjects come only from verified claims, child subjects copy stored attribution without filling `user`, and `trustedBypass` plus tenant checks keep their current owners and behavior. It follows `principle-design-for-throughput` by carrying one lineage value per execution with no ancestor collection, shared registry, synchronization or storage round trip. The 50,000 requests per second workload is a design target, not a measurement.

The interface keeps one `RequestContext` across command decisions, matching the accepted ADR-0011 request-scoped extension precedent. It adds one required lineage argument to the public event builder, so contextless callers state `EventLineage.empty`. This concentrates metadata mapping in the event owner and keeps lineage out of application command data.

## Synthesis decision

Candidate A's provisional choice is to extend `RequestContext` with a typed lineage field. This candidate favors the existing request-scoped extension precedent and the shallowest propagation path. It does not decide the arena synthesis. Candidate B's separate execution envelope remains for comparison.

## Tradeoffs accepted

- We accept a source change for callers that construct `RequestContext` records directly in exchange for keeping every `decide` signature uniform. The exported constructor makes this migration visible. Existing Auth constructors absorb it for ordinary callers.
- We accept a changed `Event.payloadFromEvents` signature in exchange for making event attribution explicit at the owning creation boundary. Existing fixture and seed callers pass `EventLineage.empty`.
- We accept that old events without correlation begin a new deterministic correlation at their own event ID because no earlier root is stored.

## Alternatives considered

- A separate execution envelope preserves the authorization context shape, but executor, transport and dispatcher call chains must carry both values and adapt them at each boundary. It hides less complexity from framework maintainers than the single context field.
- Putting lineage in `CommandPayload` lets callback code transport it, but mixes framework metadata into application serialized command data and gives payload producers a spoofable path.

## Open questions and risks

- Does the release policy require a deprecation window for direct `RequestContext` record construction and the exported event builder?
- The existing persisted IDs are typed as `Maybe Text`; implementation must normalize nil UUID text at the lineage boundary without changing the stored event's original metadata.

## Next implementation step

Prototype `EventLineage.root`, `fromSource` and `payloadFromEvents` against the executor and dispatcher caller shapes, including nil contexts and historical events without correlation.
