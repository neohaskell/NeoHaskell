---
title: "Words you will meet"
description: "Plain-language meanings with links to the deeper shop examples."
sidebar:
  order: 2
---

Use this page when a term gets between you and the business question you are
trying to answer. The examples all refer to the same fictional shop, and the
links take you to the practical details.

| Term | Meaning in the shop | Read more |
| --- | --- | --- |
| Command | A request such as adding two mugs to a cart; it can be refused | [Commands and events](/build/commands-and-events/) |
| Event | An accepted fact such as an item having been added | [Commands and events](/build/commands-and-events/) |
| Entity | One business object whose state informs decisions, such as a particular cart | [Entities and state](/build/entities-and-state/) |
| Event stream | The ordered history belonging to an entity | [Persistence](/operate/persistence/) |
| Event sourcing | Keeping accepted facts and reconstructing state from them | [Event modeling](/start/event-modeling/) |
| Event modeling | Describing how requests, facts, views, and external actions connect | [Model the shop](/start/event-modeling/) |
| Decider | The logic that accepts or refuses a request using current state | [Commands and events](/build/commands-and-events/) |
| Query / read model / projection | Information prepared for reading, such as a cart summary | [Queries](/build/queries/) |
| CQRS | Separating requests that change the system from reads of prepared information | [Queries](/build/queries/) |
| Eventual consistency | A read view may briefly lag behind an accepted change | [Queries](/build/queries/) |
| Replay | Applying stored events again to reconstruct state or views | [Recovery](/operate/recovery/) |
| Snapshot | A cached state that reduces how much history must be reread | [Performance](/operate/performance/) |
| Integration | An explicit connection to another part of the application or an external service | [Connect the shop](/connect/) |
| Outbound | Work triggered by the application's accepted events, such as preparing an email | [Integration lifecycle](/connect/) |
| Inbound | A trigger that submits work to the application, such as a timer | [Timers](/connect/timers/) |
| Idempotency | Repeating a request has the same intended business effect as performing it once | [HTTP and payments](/connect/http-and-payments/) |
| Correlation identifier | A value connecting a request to later provider responses or business facts | [Workflows](/connect/workflows/) |
| Optimistic concurrency | Detecting that state changed while a decision was being made and handling the conflict | [Stock and checkout](/build/stock-and-checkout/) |
| Authentication | Establishing who made a request | [Access control](/build/access-control/) |
| Authorization | Deciding what that person may do or see | [Access control](/build/access-control/) |
| Schema | A description of the expected shape of data | [HTTP and frontend](/build/http-and-frontend/) |
| Transport | The way requests and responses cross the application's boundary, such as HTTP | [HTTP and frontend](/build/http-and-frontend/) |
| Liveness | Whether the running HTTP process responds | [Deployment](/operate/deployment/) |
| Readiness | Whether registered query projections have caught up so this revision can serve traffic | [Deployment](/operate/deployment/) |
| Business invariant | A rule that must remain true, such as refusing a negative quantity | [Testing](/build/testing/) |

## Three distinctions worth keeping

**A request is not a fact.** “Charge the customer” is a request. A confirmed payment
is evidence of something that happened. A timeout leaves uncertainty; it does not
establish that no charge occurred.

**State is not a view for every reader.** A cart's state supports decisions. A
customer's summary and a merchant's report may present different information and
have different access rules.

**A passing check has a scope.** Compilation, model validation, tests, and provider
sandbox checks answer different questions. The [trust chapter](/start/trusting-your-agent/)
explains how to combine them when accepting your agent's work.
