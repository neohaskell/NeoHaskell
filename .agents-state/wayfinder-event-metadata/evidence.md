# Event metadata planning evidence

## Agreed outcome

Plan a framework fix for newly emitted events and their logs. Do not implement during Wayfinder charting.

1. Authenticated commands record the verified user subject and correlation ID in events and logs.
2. Event-triggered background work preserves the initiating subject and correlation ID. New events identify the immediate triggering event through causationId.
3. Scheduled work without a user gets a correlation ID and leaves relatedUserSub absent.
4. Historical stored events remain unchanged.
5. Logs include the exact relatedUserSub alongside correlationId and causationId when available. Exclude tokens and other claim contents.

The user explicitly accepted all four scenarios and exact subject logging in this session.

## Baseline

Repository is github.com/neohaskell/neohaskell. Source SHA is cbdc45f1bb89c9c7bb8da9fbdb25343771d924ca. Initial tracked worktree status was clean. Findings below are source inspection, not runtime reproduction.

| Boundary | Evidence | Finding |
| --- | --- | --- |
| Event metadata construction | core/service/Service/Event/EventMetadata.hs, lines 19 to 49 | EventMetadata has eventId, relatedUserSub, correlationId, causationId, createdAt, localPosition and globalPosition. Its constructor sets the three optional tracing fields to Nothing. |
| Request identity | core/service/Service/Auth.hs, lines 66 to 152 | RequestContext contains user, requestId and timestamp. Normal constructors generate IDs. emptyContext uses a nil UUID. trustedBypass controls authorization separately. |
| Accepted command events | core/service/Service/CommandExecutor/Core.hs, lines 255 to 368; core/service/Service/Event.hs, lines 68 to 89 | The executor receives RequestContext but calls payloadFromEvents without it. Each insertion creates metadata without caller context. |
| HTTP entrypoint | core/service/Service/Transport/Web.hs, lines 547 to 644 | The transport resolves authenticated, anonymous or trusted context and passes it to command handlers. Debug scopes contain component and command only and end before handler execution. |
| Log serialization | core/core/Log.hs, lines 45 to 162 | Log writes JSON to stdout. Scoped string fields live in a map keyed by thread ID. Existing scopes do not add the tracing fields. |
| Async execution | core/concurrency/AsyncTask.hs, lines 36 to 41; core/service/Service/Integration/Dispatcher.hs, lines 125 to 134 | Child async tasks and timeout races do not explicitly copy logger context. Scope inheritance cannot be assumed. |
| Background command dispatch | core/service/Service/Integration/Dispatcher.hs, lines 878 to 906 | Dispatch creates a new trusted context and receives only a CommandPayload, so the callback cannot carry the source event metadata through this signature. |

## Principles applied

Fix Root Causes places context construction and propagation before log formatting. Security by Construction keeps historical actor metadata separate from authorization and limits log fields to the agreed identifiers. Design for Throughput rules out per-log event-store lookups and requires bounded context lifetime.

The hypothetical workload is an aggregate 50,000 requests per second with independent requests, fixed-size metadata, existing event persistence and variable downstream fan-out. This is design reasoning. No measured performance result is available.

## Decision frontier

1. Choose metadata lineage and context ownership. Settle root ID generation, request versus correlation semantics, immediate cause identity, old-event fallback and callback propagation. Compare candidate shapes with a local prototype before selecting an API.
2. Specify structured log coverage and isolation. Preserve exact subject logging as agreed. Specify optional fields, event versus operation records, failure coverage and async cleanup. Do not assume automatic inheritance across long-lived workers.
3. Select compatibility and verification boundaries after those contracts settle. Inventory public and generated consumers, storage decoding, retry semantics, maintained test drivers and evidence required before rollout.

No decision ticket has been resolved. The final execution task is due after all in-scope decisions and remaining fog are resolved.

## Verification obligations

Reproduce the missing fields through actual command execution and captured JSON logs. Exercise an authenticated root command, an event-triggered command chain, a timer, an anonymous command, failures, concurrent requests, retries and worker reuse. Assert that actor attribution cannot create authority.

Verify metadata after persistence, readback, subscriptions and replay on both in-memory and PostgreSQL stores. Preserve historical null fields, event IDs, timestamps, stream positions and append preconditions. Define behavior when an old triggering event has no correlation ID.

Use .agents/skills/neohaskell-test/scripts/run-full-suite for broad coverage and .agents/skills/tdd/scripts/test-match for focused selections. Discover supported focused invocation and fixture setup before execution. No tests or performance probes ran during charting.

## Lineage decision prototype on 2026-10-22

The selected ticket was verified open, unassigned and unblocked, then claimed and entered the started state. Its decision is pending the user response in the current agent tab. No decision has been resolved and no production source was changed.

Actual CommandExecutor.execute with AuthenticatedAddItem, Auth.authenticatedContext and real in-memory persistence reproduced all three lineage fields as null. Baseline characterization passed with one example. The required actor assertion failed as expected. Source-bound failing receipt is `.agents-state/regression-evidence/runtime-4g4iax4g/receipt.json`.

The proposed contract in two throwaway carrier representations passed 22 Hspec examples and 100 property cases per representation. These share helper semantics and do not prove the written candidates end to end or a production treatment. Passing source-bound receipt is `.agents-state/regression-evidence/runtime-6l55qubz/receipt.json`. The browser probe passed five exact walkthroughs, both architecture selectors, no reconstructed user claims, mobile width and no page errors.

Provisional recommendation is candidate A, adding typed lineage to RequestContext, with candidate B's nil-root normalization and source-scoped lifetime. Preserve all present source correlations opaquely. Missing correlation falls back to canonical source metadata.eventId. Capture source lineage explicitly at dispatch without putting it in CommandPayload or verified claims. The separate execution envelope is viable but requires broader transport API migration.

See `lineage-proposal.md`, `lineage-comparison.md`, `lineage-how.md` and `lineage-why.md` alongside this file. Runnable artifacts are under `.agents-state/prototypes/metadata-lineage/`. No next decision tab may launch before this decision receives the user's actual response here and is resolved.

## User correction and revised decision

The user rejected the duplicated Lineage model and explicitly preferred filling the existing EventMetadata. A fresh independent design comparison selected existing optional source EventMetadata through internal dispatch and command execution. RequestContext, public EndpointHandler, decide and payloadFromEvents remain unchanged. The executor populates only the three tracing fields on fresh per-event metadata. The prior typed-Lineage proposal is superseded.

DirectMetadata.hs passed eight examples and 100 property cases. Its source-bound passing receipt is `.agents-state/regression-evidence/runtime-nmwvbk21/receipt.json`. This is a throwaway updater and in-memory persistence proof, not production callback or command treatment. Root GLOSSARY.md records the settled attribution vocabulary.
