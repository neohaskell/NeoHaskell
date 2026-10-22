# NeoHaskell consumer guidance

Use these pages when building an application with NeoHaskell. Read the guidance published for the framework revision your project pins. Source paths identify the implementation that owns each contract in the matching NeoHaskell checkout.

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

The generated Counter application in `neo/starter/src/App.hs` is the basic reference. Its commands, reducer, and query form one working example. Advanced integrations belong in applications that need external effects.

- [Reusable side effects](advanced-side-effects.md) covers `ToAction`, HTTP requests, callbacks, and the separate adapter API.
- [External OAuth2](advanced-oauth2.md) covers account connections, persistent token storage, and callback commands.

A `haskell` code fence is a complete module for compilation. A `haskell ignore` fence is an explanatory fragment that needs the surrounding application definitions. Cited tests describe maintained evidence. A source citation alone does not mean that a runtime scenario ran.
