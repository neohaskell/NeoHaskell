# Transports

A transport exposes registered commands through an interface. `TransportsOf command` declares the allowed transport types. `Application.withTransport` installs each concrete transport configuration. Both the command declaration and application registration participate in the endpoint setup.

## Web

`Service.Transport.Web.server` supplies the default Web configuration with `port = 8080` and `host = "*4"` (IPv4 preferred with IPv6 fallback). Commands use `POST /commands/{kebab-case-name}`. Registered queries use `GET /queries/{kebab-case-name}`.

Use `EventModel.withHost` and `EventModel.withPort`, or `Application.withHost` and `Application.withPort` in direct `Application` assembly, to override the registered Web listener. Overrides apply whether they appear before or after `withTransport`. Omitting an override preserves the `WebTransport` record's own `host` or `port`.

`WebTransport.host` accepts `"*"`, the family preferences `"*4"` and `"*6"`, strict-family `"!4"` and `"!6"`, a bare address or hostname such as `"127.0.0.1"`, `"::1"`, or `"localhost"`, or a bracketed IPv6 literal such as `"[::1]"`. Loopback (`"127.0.0.1"` or `"::1"`) accepts connections only from the same network namespace. Keep a non-loopback bind when a reverse proxy runs in a separate container or host.

Startup validates bind settings before opening sockets. Empty hosts, whitespace, URLs or paths, `host:port` pairs, malformed or non-IPv6 brackets, port `0`, and ports outside `1..65535` fail with `Application.withHost: ...` or `Application.withPort: ...`. A port already in use or an unowned address fails with `WebTransport could not bind <host>:<port>: ...`.

The Web transport also exposes configured authentication, OAuth2 connection routes, upload routes, and generated API descriptions. Adding a transport alone does not configure those features. Use their application builders.

## CLI

`Service.Transport.Cli.cli` is the default configuration for an application's command line interface. `CliTransport` carries `programName`, `version`, and `description`. It groups commands by entity and builds arguments from their schemas.

This application transport is separate from the `neo` development tool. [Project and CLI](project-and-cli.md) covers `neo build` and related development commands.

## MCP

`McpTransport` carries `serverName` and `serverVersion`. It serves registered command tools using JSON-RPC over standard input and output.

This fragment declares three allowed transport types. It needs the transport imports and command definition.

```haskell ignore
type instance TransportsOf ChangeCounter = '[WebTransport, CliTransport, McpTransport]
```

## Verify each interface

Exercise the interface your application exposes. An HTTP scenario checks Web behavior. Test CLI argument parsing through the application executable and MCP tools through a JSON-RPC client over standard input and output.
