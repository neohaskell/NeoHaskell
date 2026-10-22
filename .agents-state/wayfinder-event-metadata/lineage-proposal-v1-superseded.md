# Extend RequestContext with explicit lineage

Proposal only. The user's decision is pending in this tab. This resolves only "Choose metadata lineage and context ownership". Production implementation, the log-coverage contract and compatibility policy remain outside this session.

## Problem

Framework consumers need newly emitted events to retain actor attribution and a correlation chain. Maintainers need one owner for lineage rules that never supplies authorization. The real command executor currently drops context before constructing metadata. The integration dispatcher drops the source distinction before constructing a new command context. See [current flow](lineage-how.md) and [historical constraints](lineage-why.md).

## Usage

Command decisions and endpoint handlers retain their existing `RequestContext` parameter. Auth constructors and executor ingress prepare root context. Outbound processors pass source metadata explicitly at the existing dispatch boundary.

```haskell
context <- Auth.authenticatedContext claims
Executor.execute store fetcher entityName context command

dispatchCommand (EventTriggeredBy source.metadata) endpoints payload
dispatchCommand Autonomous endpoints payload
```

Event construction requires a resolved lineage value. The following signatures describe the proposed boundary. They are sketches, not implemented APIs.

```haskell
prepare :: RequestContext -> Task err RequestContext
fromSource :: EventMetadata -> Lineage
payloadFromEvents :: Lineage -> EntityName -> StreamId -> Array event
                  -> Task err (InsertionPayload event)
```

## Shape

`Service.Event.Lineage` owns a small immutable value with optional `relatedUserSub`, a present `correlationId`, and optional `causationId`. The constructor is private. Request IDs remain `Uuid`. Stored correlation and cause remain compatible with the existing optional text representation. Naming and source-compatible adaptation remain for the compatibility decision and final execution task.

`RequestContext` carries that lineage separately from verified `user`, tenant claims, files, timestamp, and `trustedBypass`. An unprepared root context may lack resolved lineage. Executor ingress resolves it once before the conflict retry loop. Event construction receives resolved lineage rather than independently reconstructing it.

Model the Domain chose one type and owner for actor, correlation and immediate cause. Security by Construction keeps lineage out of verified claims, access checks and bypass construction. A historical actor can be copied into attribution only. It cannot supply `user`, permissions, tenant IDs or trust. Existing trusted framework execution and multi-tenant rejection remain unchanged.

## Proposed contract

1. A root command uses its server-generated request UUID as correlation. Nil request IDs normalize to a fresh UUID once per command invocation. Verified actor attribution survives this normalization. Anonymous and autonomous roots have no invented actor. External request headers and command JSON do not select framework lineage IDs.
2. An event callback gets a fresh command request ID. It copies source actor and any present source correlation unchanged. If correlation is absent, it uses the source's persisted `metadata.eventId` text as correlation. Immediate cause is always that same source `metadata.eventId`, never `Insertion.id`, the source's own cause, or a sibling event. Present historical text stays opaque. This proposal adds no blank-text or nil-source-ID reinterpretation policy.
3. All events emitted by one accepted decision are siblings. They share actor, correlation and immediate cause. Each retains current event ID and creation-time generation. Normalizing context does not replace event timestamps with request timestamps. Historical stored records, positions and identities remain unchanged.
4. Capture source-derived lineage once per delivered event before runner retries and fan-out. Each resulting command has a fresh request ID and uses the captured lineage. Conflict retries reuse prepared lineage while preserving existing generation of unpersisted event IDs and timestamps. Runner command dispatch remains outside its retry boundary. This adds no deduplication or exactly-once promise.
5. Each autonomous command emission starts a fresh root correlation from its command request ID, with no actor or event cause. A timer tick does not reuse the lifetime ID of its long-lived worker. No context or claims are retained across unrelated deliveries. Logger coverage and async scope restoration are for the separate logging decision.

## Synthesis decision

Candidate A is the proposed base. Its existing `RequestContext` carrier follows the documented extension precedent and preserves endpoint and command-decision parameter types. Candidate B's once-per-invocation nil-root normalization and explicit source-context lifetime are grafted into A. This rejects A's proposed empty nil-root result.

The independent cross-judge also recommended A with these grafts. Both written candidates proposed additional normalization of stored correlation text or nil source identities. Those rules are rejected because no demonstrated historical-data defect requires reinterpreting them. The prototype preserves every present source correlation and falls back only on absence.

The separate execution envelope remains viable. It separates historical attribution structurally, but changes `EndpointHandler`, transport `buildHandler`, registration callbacks and executor callers. Its larger public migration does not establish an additional authorization guarantee for this accepted scope. The authorization interpreter must read only verified claims in either design.

## Tradeoffs

- We accept adding a field for direct `RequestContext` record constructors in exchange for keeping existing handler and `decide` signatures. Ordinary Auth constructors absorb root setup.
- We accept explicit lineage at event construction and explicit origin at integration dispatch in exchange for preventing silent context loss. The compatibility decision must inventory direct event-builder callers.
- We accept starting a missing historical chain at its triggering event because no earlier correlation was stored. Deterministic fallback groups fan-out and redelivery without changing history.

## Throughput checkpoint

Design for Throughput chose fixed-size immutable values and bounded invocation or delivery lifetimes. The hypothetical aggregate workload is 50,000 requests per second with variable fan-out and existing event persistence. Added work scales with commands and emitted events. There are no ancestry storage reads, shared lineage registries, new queues or synchronization. Existing replay, storage and dispatcher resources remain the baseline. No achieved throughput or speedup is claimed.

## Evidence and limits

The real baseline executes `AuthenticatedAddItem` with an authenticated context and reads the accepted event back from the in-memory store. All three lineage values are null. The required actor assertion fails with the expected present subject versus `Nothing`. The source-bound receipt is `.agents-state/regression-evidence/runtime-4g4iax4g/receipt.json`.

`Baseline.hs` has separate characterization and required-behavior selections. `Contracts.hs` compares two throwaway carrier representations with shared proposed normalization and propagation helpers. Its checks demand real `EventMetadata` construction and in-memory persistence. Its property follows arbitrary callback depth. Shared helpers mean this is evidence for the proposed semantics in two representations, not two independent production implementations.

The HTML walkthrough illustrates five scenarios. Its browser probe checks exact rows, architecture selection, absence of reconstructed claims, mobile width and page errors. All assets live under `.agents-state/prototypes/metadata-lineage/`; browser captures live under `.agents-state/control-ui/metadata-lineage/`.

The baseline is real production command execution. The proposed treatment is throwaway logic. Full command treatment, callback execution, actual conflict retries, multi-tenant execution, PostgreSQL, subscriptions, replay, cancellation and logger isolation remain implementation evidence obligations. Prove It Works caused this distinction to stay explicit.

## Remaining decision

Does the user accept this context owner and lineage contract? The ticket remains started until their actual response here. After acceptance, record its resolution and refresh the remaining map. The logging and compatibility tickets must resolve before creating the final implementation task.
