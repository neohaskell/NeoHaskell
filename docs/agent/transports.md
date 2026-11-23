# Transports

A transport exposes commands through an interface. Register a concrete adapter with `withTransport` in the Event Model. A command derived with the default transport setting uses the sole registered adapter. Startup reports the command name when no adapter is registered or when multiple adapters make that choice ambiguous.

Set `transports` on the command declaration to select specific public adapters. Register each selected adapter in the Event Model. Include `InternalTransport` to allow integration dispatch. A declaration containing only `InternalTransport` needs no registered public adapter and creates no public endpoint. An explicit list can contain both public adapters and `InternalTransport`. The configured authorization policy still applies to each caller's request context.

## Web

`Service.Transport.Web.server` supplies the default Web configuration with `port = 8080` and `host = "*4"` (IPv4 preferred with IPv6 fallback). Commands use `POST /commands/{kebab-case-name}`. Registered queries provide collection reads at `GET /queries/{kebab-case-name}` and keyed reads at `GET /queries/{kebab-case-name}/{id}`. See [queries](queries.md) for authorization and response behavior.

Use `EventModel.withHost` and `EventModel.withPort`, or `Application.withHost` and `Application.withPort` in direct `Application` assembly, to override the registered Web listener. Overrides apply whether they appear before or after `withTransport`. Omitting an override preserves the `WebTransport` record's own `host` or `port`.

`WebTransport.host` accepts `"*"`, the family preferences `"*4"` and `"*6"`, strict-family `"!4"` and `"!6"`, a bare address or hostname such as `"127.0.0.1"`, `"::1"`, or `"localhost"`, or a bracketed IPv6 literal such as `"[::1]"`. Loopback (`"127.0.0.1"` or `"::1"`) accepts connections only from the same network namespace. Keep a non-loopback bind when a reverse proxy runs in a separate container or host.

Startup validates bind settings before opening sockets. Empty hosts, whitespace, URLs or paths, `host:port` pairs, malformed or non-IPv6 brackets, port `0`, and ports outside `1..65535` fail with `Application.withHost: ...` or `Application.withPort: ...`. A port already in use or an unowned address fails with `WebTransport could not bind <host>:<port>: ...`.

The Web transport also exposes configured authentication, OAuth2 connection routes, upload routes, and generated API descriptions. Adding a transport alone does not configure those features. Use their application builders.

## CLI

`Service.Transport.Cli.cli` is the default configuration for an application's command line interface. `CliTransport` carries `programName`, `version`, and `description`. It groups commands by entity and builds arguments from their schemas.

This application transport is separate from the `neo` development tool. [Project and CLI](project-and-cli.md) covers `neo build` and related development commands.

## MCP

`McpTransport` carries `serverName` and `serverVersion`. It serves registered command tools using JSON-RPC over standard input and output.

This fragment declares three allowed transport types. Import `Derive`, the listed transport types, and `CounterEntity` in the command module.

```haskell ignore
derive ''ChangeCounter command {
  entity = ''CounterEntity,
  transports = [''WebTransport, ''CliTransport, ''McpTransport]
}
```

## Verify each interface

Exercise the interface your application exposes. An HTTP scenario checks Web behavior. Test CLI argument parsing through the application executable and MCP tools through a JSON-RPC client over standard input and output.
