---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

You can now choose which network address and port your web server listens on. Add `EventModel.withHost "127.0.0.1"` and `EventModel.withPort 9090` in a named Event Model, or `Application.withHost "127.0.0.1"` and `Application.withPort 9090` in direct `Application` assembly. `WebTransport.host` also accepts `"*"`, the family preferences `"*4"` and `"*6"`, strict-family `"!4"` and `"!6"`, a bare address or hostname, or a bracketed IPv6 literal such as `"[::1]"`. Family preferences may fall back to the other family and do not guarantee strict isolation.

Loopback (`"127.0.0.1"` or `"::1"`) is the right choice when your reverse proxy runs on the same machine or in the same container as the app. If the proxy runs in its own container network or on another host, it cannot reach a loopback listener. Keep a non-loopback bind and restrict access with your firewall or network policy instead.

Invalid bind settings fail at startup with a clear message before opening sockets. An empty host, whitespace, a URL or path like `"http://127.0.0.1"`, a `host:port` pair, or malformed or non-IPv6 brackets (`"[]"`, `"[localhost]"`, `"[::1"`) fails with `Application.withHost: ...`. Port `0` and values outside `1..65535` fail with `Application.withPort: ...`. If the port is already in use or the address does not belong to this machine, startup stops with `WebTransport could not bind <host>:<port>: ...`.

The default remains port 8080 and Warp's existing `"*4"` IPv4 preference, including its fallback behavior. A custom transport host or port is preserved when the corresponding override is omitted.

This change is marked breaking for full-field record construction and prior ephemeral-port usage. `WebTransport` and `Application` each gained fields, so code that builds either record by naming every field must add the new ones. Apps using `WebTransport.server`, `EventModel.eventModel`, or `Application.new` need no change. A custom transport that previously used port `0` must choose a fixed port in `1..65535`.

## Migration

Search your code for a `WebTransport { ... }` or `Application { ... }` record written with every field. Also search for custom port `0` settings and change them to a fixed port in `1..65535`. Default-only apps need no migration.

- In a `WebTransport { ... }` record, add `host = "*4"` to keep the previous behavior, or `host = "127.0.0.1"` to accept local connections only.
- In an `Application { ... }` record, add `bindHost = Nothing` and `bindPort = Nothing`.

Prefer `EventModel.withHost` and `EventModel.withPort`, `Application.withHost` and `Application.withPort`, or `WebTransport.server { port = 9090 }` over naming every field.

### Verify

Build your app, start it, and request `/health` on the address you chose. To check the framework change itself, run `nix develop --command .agents/skills/tdd/scripts/test-match "WebTransport bind host and port" nhcore-test-service`. The suite checks `EventModel` and `Application` assembly, owned-instance identity with a competing-server negative control, loopback exclusion, early validation, bracketed IPv6 binding, and returned bind-failure cleanup.

## Agent prompt

```text
My NeoHaskell app fails to compile after upgrading nhcore. `WebTransport` gained a `host :: Text` field and `Application` gained `bindHost :: Maybe Text` and `bindPort :: Maybe Int`. Find every place that builds a `WebTransport { ... }` or `Application { ... }` record by naming all fields (search for `WebTransport {`, `WebTransport$` followed by `{`, and `Application {`). In each `WebTransport` record add `host = "*4"` so it keeps listening on all IPv4 addresses as before. In each `Application` record add `bindHost = Nothing` and `bindPort = Nothing`. Preserve existing nonzero ports and other fields. If a custom transport uses port 0, explain that this feature rejects ephemeral ports and ask which fixed port in 1..65535 to use. Default-only `WebTransport.server`, `EventModel.eventModel`, and `Application.new` usage needs no edit. Then build the project and run its tests. Report which files you edited and whether the build and tests pass. If my app sits behind a reverse proxy that runs on the same machine or in the same container, tell me I can use `EventModel.withHost "127.0.0.1"` or `Application.withHost "127.0.0.1"` for local-only access. If the proxy runs elsewhere, tell me to keep the default bind. Do not change the bind host without asking.
```
