---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

Build an app around its recorded history: define commands such as “add an item,”
events such as “item added,” and queries that build the lists users see. The new
service framework connects these parts, checks their types, and supplies the
following capabilities. Existing 0.9 apps can adopt them after applying the
core/event-store migration guide.

- **Commands and saved state.** Declare entities, events, commands and queries
  with the framework's derivation helpers instead of writing their supporting
  instances by hand. Command execution checks access, rejects invalid decisions
  and retries conflicting updates. Entity fetching rebuilds state from events;
  optional snapshots avoid replaying the same old history on every fetch.
  `Uuid.generateV5` and `Decider.generateDeterministicUuid` let an app derive a
  repeatable ID from a namespace and a natural key, such as an external order ID.
  Verify accepted, rejected and repeated commands with the app's domain tests.

- **Storage that survives restarts.** Use the PostgreSQL event store for shared
  database storage, or SimpleEventStore's optional JSONL files for local
  persistence. Subscriptions catch up after reconnecting; fixes prevent missing
  events after a PostgreSQL listener reconnect and release subscription
  connections on unsubscribe. PostgreSQL settings include explicit pool sizes
  and TLS modes. Configure storage and restart a test instance to check that its
  history and subscriptions recover. Database event listeners require a direct,
  session-preserving connection.

- **Lists, filters and pages.** Queries maintain read models: saved views of
  events used to answer requests. They can use memory or PostgreSQL storage.
  HTTP responses include `items`, `total`, `hasMore` and `effectiveLimit`, so a
  screen can request one page at a time. The default page contains at most 100
  items, with a maximum of 1,000 per request. Results and counts respect query
  authorization. NeoQL supplies field access and equality filtering. Test
  first/next/empty pages and access restrictions in your app before connecting
  the UI.

- **Sign-in and access rules.** JWT authentication validates signed access
  tokens; OAuth2 support connects external accounts, stores tokens and refreshes
  them. Typed configuration loads fields from flags, environment variables and
  `.env` files, reports missing or invalid values, and supports secret fields.
  Commands can declare public or authenticated access. Multi-tenant commands
  and the `tenantOnly` query filter let apps separate each customer's data when
  explicitly configured. Test both allowed and denied requests, including users
  from different tenants; adding sign-in alone does not select your access rules.

- **HTTP, command-line and agent access.** Expose service commands and queries
  through HTTP, an app-specific CLI, or MCP (the protocol coding assistants use
  to call tools). Internal commands can serve integrations without becoming
  public endpoints. Generated JSON schemas and OpenAPI descriptions reflect
  command inputs and paginated query responses. WebTransport supports CORS for
  browser clients. Select the transports your app needs and exercise their
  requests, including invalid inputs and denied access.

- **Uploads and downloads.** Apps can validate uploaded file sizes and types,
  retain an opaque file reference, confirm or delete files, and control download
  access. Local blob storage keeps bytes; the optional PostgreSQL file-state
  store keeps lifecycle information. Deduplication reuses identical content for
  the same owner and restores a missing blob on re-upload. Configure both stores
  for your deployment, then test upload, authorized download, rejection and
  deletion with real files.

- **Startup and diagnostics.** HTTP can start accepting health checks while
  queries rebuild from history. With readiness enabled, `/ready` stays at 503
  until the read models are usable; route traffic after it reaches 200.
  PostgreSQL query checkpoints support resuming rebuilds. Structured logging
  adds command and event context, and line-buffered output reaches container
  logs promptly. Follow the [deployment guide](https://github.com/neohaskell/NeoHaskell/blob/d2201cc555d55cc558d751ff67d04569a42eda18/website/src/content/docs/guides/deployment.mdx)
  for separate startup, liveness and readiness probes; test a cold restart,
  rather than only an already-warm process.

- **Everyday data and concurrency helpers.** Fixed-point `Decimal` provides
  money-oriented arithmetic and formatting; `Crypto` signs and verifies
  HMAC-SHA256 messages with an opaque key type. Arrays gain trimming, searching,
  chunking and zipping helpers; Bytes gains binary conversions, slicing and
  Base64 support. Map and Maybe gain convenience operations, and `Text.escapeHtml`
  escapes text for HTML. Concurrent maps, streams and atomic variables support
  shared work; asynchronous tasks gain cancellation and racing. `Parser` and
  `Layout` help parse and format text, with comment/function syntax primitives.
  These primitives do not constitute a complete new language compiler. Adopt
  the helpers where needed and retain boundary tests for your data formats,
  rounding, authentication and cancellation behavior.
