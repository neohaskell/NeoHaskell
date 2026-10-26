---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

You can now choose which network address and port your web server listens on. Add `Application.withHost "127.0.0.1"` to accept connections from the same machine only, and `Application.withPort 9090` to change the port. `host` also accepts `"*"`, the family preferences `"*4"`/`"*6"`, strict-family `"!4"`/`"!6"`, or a literal address. Family preferences may fall back to the other family; they are not strict isolation guarantees.

Loopback (`"127.0.0.1"`) is the right choice when your reverse proxy runs on the same machine or in the same container as the app. If the proxy runs somewhere else (its own container with its own network, another host), it cannot reach a loopback listener; keep the default bind and restrict access with your firewall or network policy instead.

A mistake in these settings fails at startup with a clear message instead of starting something unintended. The host must be a bare address or hostname (an empty host, a URL like `"http://127.0.0.1"`, a `host:port` pair or whitespace is rejected with `Application.withHost: ...`), and the port must be between 1 and 65535 (`0` and out-of-range values are rejected with `Application.withPort: ...`). If the port is already in use or the address does not belong to this machine, the app stops with `WebTransport could not bind <host>:<port>: ...`.

The default remains port 8080 and Warp’s existing `"*4"` IPv4 preference, including its fallback behavior. A custom transport port is preserved when no application override is supplied.

This is marked breaking for record construction and prior ephemeral-port usage. `WebTransport` and `Application` each gained fields, so code that builds either record by naming every field must add the new ones. Apps using the defaults need no change. A custom transport that previously used port `0` must now choose a fixed port in `1..65535`; this was the feature’s existing validation decision, not a new restriction added by the remediation.

## Migration

Search your code for a `WebTransport { ... }` or `Application { ... }` record written with every field. Also search for custom port `0` settings; change these to a fixed port in `1..65535`. Default-only apps need no migration.

- In a `WebTransport { ... }` record, add `host = "*4"` to keep the old behaviour, or `host = "127.0.0.1"` to accept local connections only.
- In an `Application { ... }` record, add `bindHost = Nothing` and `bindPort = Nothing`.

Prefer `WebTransport.server { port = 9090 }` or `Application.withHost` and `Application.withPort` over naming every field.

### Verify

Build your app, then start it and request `/health` on the address you chose. To check the framework change itself, run `.agents/skills/tdd/scripts/test-match "WebTransport bind host and port" nhcore-test-service`. The suite checks real application assembly, owned-instance identity (including a competing-server negative control), loopback exclusion, early validation, and returned bind-failure cleanup. Consult the current test receipt for results; these checks do not replace the full suite or final review.

## Agent prompt

```text
My NeoHaskell app fails to compile after upgrading nhcore. `WebTransport` gained a `host :: Text` field and `Application` gained `bindHost :: Maybe Text` and `bindPort :: Maybe Int`. Find every place that builds a `WebTransport { ... }` or `Application { ... }` record by naming all fields (search for `WebTransport {`, `WebTransport$` followed by `{`, and `Application {`). In each `WebTransport` record add `host = "*4"` so it keeps listening on all IPv4 addresses exactly as before. In each `Application` record add `bindHost = Nothing` and `bindPort = Nothing`. Preserve existing nonzero ports and other fields. If a custom transport uses port 0, explain that this feature rejects ephemeral ports and ask which fixed port in 1..65535 to use. Default-only `WebTransport.server` and `Application.new` usage needs no edit. Then build the project and run its tests. Report which files you edited and whether the build and tests pass. If my app sits behind a reverse proxy that runs on the same machine or in the same container, tell me I can use `Application.withHost "127.0.0.1"` for local-only access; if the proxy runs elsewhere, tell me to keep the default bind. Do not change the bind host without asking.
```
