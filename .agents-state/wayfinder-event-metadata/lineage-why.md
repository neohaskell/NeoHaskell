# Historical context for lineage ownership

## The question

Why does execution context currently combine request data and authorization, while persisted event lineage remains empty? Which historical constraints should shape the ownership decision?

## The code in question

`Service/Auth.hs:66-152` defines `RequestContext`. `Event/EventMetadata.hs:19-49` initializes lineage. `Service/Event.hs:68-90` constructs insertions.

`CommandExecutor/Core.hs:179-220,292-416` authorizes and appends. `Integration.hs:198-203` defines command payloads. `Integration/Dispatcher.hs:781-816,878-905` dispatches callbacks. Paths are under `core/service/`.

## What we found

- **[Direct]** `RequestContext` preserves a uniform `decide` signature and supports domain authorization. [PR 261](https://github.com/neohaskell/NeoHaskell/pull/261) introduced that contract. ADR0011:87-103 explicitly extends it for files to avoid another parameter or concept. ADR0011:298-303 acknowledges that context growth can confuse newcomers.
- **[Direct]** The timestamp migration supports stamping events without exposing `Data.Time`. [PR 632](https://github.com/neohaskell/NeoHaskell/pull/632) supplies this rationale. It does not settle lineage ownership.
- **[Direct]** Trusted execution preserves no-auth reachability and framework-triggered work. [PR 642](https://github.com/neohaskell/NeoHaskell/pull/642) records anonymous internal commands failing Hurl checks and the subsequent trusted-context change. External callers retain their authorization gate.
- **[Direct]** Dispatch stays outside runner retries to avoid duplicate command sends. [PR 572](https://github.com/neohaskell/NeoHaskell/pull/572) states this reason.
- **[Direct]** [Plan complete event metadata and correlated logs](https://plane.p.egregore.io/egregore/projects/5535680e-e3f6-4023-b400-2b27544130e2/issues/f38013da-e4b2-40f0-a177-0a0bf38939ef/) accepts authenticated root attribution, background continuity, immediate event causation, autonomous correlation without invented users, unchanged history, and attribution without authority. [Choose metadata lineage and context ownership](https://plane.p.egregore.io/egregore/projects/5535680e-e3f6-4023-b400-2b27544130e2/issues/bbe0198b-e347-4d5e-be1d-a8092546893f/) leaves architecture open.

## What we can reasonably infer

**[Inferred]** Extending `RequestContext` appears consistent with ADR0011's established extension pattern. That precedent does not establish that historical actors belong beside verified claims.

**[Inferred]** Separating attribution from authorization appears compatible with trusted internal execution. PR642 explicitly preserves system execution, while the accepted ticket requires actor continuity without transferred authority.

## Competing hypotheses

**[Speculative]** Empty lineage may reflect unfinished propagation. PR216 introduced optional metadata before PR261 introduced `RequestContext`. PR255's command payload also predates that context. No explicit statement confirms this explanation.

**[Speculative]** Empty lineage may reflect an intentional boundary. No investigated PR, ticket, or ADR documents such a policy. Absence alone cannot establish it.

## What we don't know

Why PR216 left lineage absent and why PR255 excluded lineage remain unknown. The searches found no explicit rationale.

ADR0050:171-180 describes anonymous internal execution, whereas current dispatch uses trusted execution. ADR0011 credits ADR0009 with `RequestContext`, but ADR0009's example uses `Maybe UserClaims`. These documents cannot establish the current contract unaided.

## Sources consulted

Source control investigators searched blame and `log -S`, inspecting PRs 216, 220, 255, 261, 404, 431, 572, 610, 632, 642, and 740.

Tracker investigators searched 116 current project items for metadata, correlation, authentication, integration, and exact symbols. Selected and sibling comment lists were empty.

Document investigators searched Markdown under `docs/decisions` and source Markdown. They reviewed accepted ADRs 0003, 0004, 0008, 0009, 0011, 0028, 0032, 0034, 0050 and 0054, and proposed ADRs 0056 and 0058. This synthesis spot-checked ADR0011, ADR0050, and current code.

Real-time chat, infrastructure observability, error tracking, and product analytics were each unavailable through usable read interfaces. No author interview occurred.

## Confidence summary

Authorization and retry constraints have direct evidence. Historical reasons for absent lineage and the correct new owner remain unresolved.

## Planning constraints

1. **Preserve.** Keep authorization, tenant checks, persisted identities, timestamps, and dispatch retry boundaries.
2. **Change.** Populate accepted lineage across root, callback, and autonomous paths.
3. **Avoid.** Never reconstruct claims or authority from historical actors. Exact `relatedUserSub` logging is accepted. Tokens and other claims remain excluded.
4. **Risk.** Compare ownership candidates against both context growth and accidental attribution-to-authority conversion.
