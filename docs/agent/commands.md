# Commands

A command carries an intent for one target entity type. The command descriptor names that entity. One `decide` function returns its events or a rejection. A service registers commands whose entity and event types agree.

## Command declarations

The single tenant shape is below. Add imports and the `IncrementCounter` data declaration in your command module.

```haskell ignore
getEntityId :: IncrementCounter -> Maybe Uuid
getEntityId cmd = Just cmd.entityId

decide :: IncrementCounter -> Maybe CounterEntity -> RequestContext -> Decision CounterEvent

derive ''IncrementCounter command { entity = ''CounterEntity }
```

Import `Core` and `Derive`. Write `getEntityId`, `decide` and any custom `canAccess` before the declaration. The marker generates the command instance, ownership, name, schema and missing JSON instances. Existing commands using explicit type equations and `deriveCommand` remain supported.

With one registered public transport, the command uses that interface. With more than one, set the descriptor's `transports` field, for example `transports = [''WebTransport]`. Use `[''InternalTransport]` for internal dispatch only.

`getEntityId` returns `Just id` to load an existing target. For creation, return `Nothing` and generate an identifier with `Decider.generateUuid`, or derive a stable identifier from the request when duplicate submissions must address the same entity.

## Decisions and execution

`decide` receives the command, `Maybe entity`, and `RequestContext`. Reject invalid transitions with `Decider.reject`. Use `Decider.acceptNew` for a new stream and `Decider.acceptExisting` for an existing stream. The executor loads the target history, runs the decision, and inserts accepted events. An accepted empty event list on an identified stream succeeds without an event-store insert.

Do not use a read model as the authority for a command invariant. Queries update asynchronously. The command already receives its target's reconstructed state.

For a second entity or a remote effect, emit an event and react through an integration. A command dispatched by that integration is a separate operation. It does not share a transaction with the first command.

`IsMultiTenant command = True` adds a tenant `Uuid` argument to both routing and decision functions. Authorization rules remain explicit.

## Web request origin

Web commands receive optional origin metadata in `RequestContext.origin`. `clientAddress` comes from the socket peer by default, and `userAgent` comes from the `User-Agent` header. A Web transport considers only `X-Forwarded-For` for proxy forwarding, and only when the socket peer matches an exact IPv4 or IPv6 address in `WebTransport.trustedProxyAddresses`. Parse configured entries with `trustedProxyAddressFromText`; the set is empty by default. For a trusted peer, the transport validates the full chain and scans from the right, skipping configured trusted addresses. A duplicate, malformed, oversized, or overlong chain falls back to the socket peer. CLI, MCP, and integration-created contexts leave `origin` empty.

Treat the address and User-Agent as untrusted metadata that can contain personal information. They are available to `decide`, but NeoHaskell does not add them to events or logs automatically. Avoid persisting or logging their raw values unless the application has a reviewed privacy and retention purpose. The maintained request-origin testbed example checks that both values reach a Web command and records only presence markers, never the raw values.

Run the public request-origin example against the testbed application. Start `exe:nhtestbed` from the testbed project, then run this command from the directory containing the example. The maintained testbed suite also runs it. PostgreSQL must be running and the testbed HTTP port must be free.

```sh
nix develop --command hurl --test request-origin.hurl
```

For a model-backed application, declare a source-local Slice with `entity @Entity do { command @CommandType do { produces @Payload } }`. The Event Model registers the command from that Slice. See [source-local Event Models](event-model.md) for the Feature and application entry point.

For a legacy application assembled directly with `Application`, keep `Service.command @CommandType` in the owning service and register that service with `Application.withService`. This manual registration path remains supported and is not deprecated.
