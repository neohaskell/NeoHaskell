# Transports

A transport exposes registered commands through an interface. `TransportsOf command` declares the allowed transport types. `Application.withTransport` installs each concrete transport configuration. Both the command declaration and application registration participate in the endpoint setup. See command transport families in `core/service/Service/Command/Core.hs`, transport assembly in `core/service/Service/Application/Transports.hs`, and Application in `core/service/Service/Application.hs`.

## Web

`Service.Transport.Web.server` supplies the Web configuration. Commands use `POST /commands/{kebab-case-name}`. Registered queries use `GET /queries/{kebab-case-name}`. The starter app in `neo/starter/src/App.hs` and Counter flow in `neo/starter/tests/scenarios/counter-flow.hurl` show this interface.

The Web transport in `core/service/Service/Transport/Web.hs` also implements configured authentication, OAuth2 connection routes, upload routes, and generated API descriptions. Adding a transport alone does not configure those features. Use their application builders.

## CLI

`Service.Transport.Cli.cli` is the default configuration for an application's command line interface. `CliTransport` carries `programName`, `version`, and `description`. The CLI transport in `core/service/Service/Transport/Cli.hs` groups commands by entity and builds arguments from their schemas.

This application transport is separate from the `neo` development tool. [Project and CLI](project-and-cli.md) covers `neo build` and related development commands.

## MCP

`McpTransport` carries `serverName` and `serverVersion`. The MCP transport in `core/service/Service/Transport/Mcp.hs` serves the registered command tools using its JSON-RPC protocol over standard input and output. The protocol implementation in `core/service/Service/Transport/Mcp/Protocol.hs` owns message handling.

This fragment declares three allowed transport types. It needs the transport imports and command definition.

```haskell ignore
type instance TransportsOf ChangeCounter = '[WebTransport, CliTransport, McpTransport]
```

## Verification

Exercise the interface that consumers call. The maintained Web specs in `core/test/Service/Transport/WebSpec.hs`, CLI specs in `core/test/Service/Transport/CliSpec.hs`, and MCP specs in `core/test/Service/Transport/McpSpec.hs` cover different transports. The testbed app in `testbed/src/App.hs` installs only Web, so its Hurl scenarios do not establish CLI or MCP behavior.
