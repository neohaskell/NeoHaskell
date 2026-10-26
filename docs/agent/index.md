# NeoHaskell consumer guidance

Use these pages when building an application with NeoHaskell. Read the guidance published for the framework revision your project pins. The pages describe the APIs, required order, and observable behavior without requiring a framework checkout.

Start with [project and CLI](project-and-cli.md), then [syntax and Core](syntax-and-core.md). Follow the topic that matches the change.

| Application task | Guidance |
| --- | --- |
| Model state and its history | [Events and entities](events-and-entities.md) |
| Change one entity | [Commands](commands.md) |
| Expose a read model | [Queries](queries.md) |
| Assemble an application | [Configuration and wiring](configuration-and-wiring.md) |
| Choose an interface | [Transports](transports.md) |
| Choose durable storage | [Storage](storage.md) |
| Restrict access | [Authorization](authorization.md) |
| Verify a change | [Testing](testing.md) |
| Place derivation markers | [Template Haskell order](template-haskell-order.md) |

The generated Counter application is a working example of a command, reducer, and query. Advanced integrations belong in applications that need external effects.

- [Reusable side effects](advanced-side-effects.md) covers `ToAction`, HTTP requests, callbacks, and the separate adapter API.
- [External OAuth2](advanced-oauth2.md) covers account connections, persistent token storage, and callback commands.

A `haskell` code fence is a complete module. A `haskell ignore` fence is a fragment that needs the surrounding application definitions.
